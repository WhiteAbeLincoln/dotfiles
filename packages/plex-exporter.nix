{
  lib,
  buildGoModule,
  dockerTools,
  writeTextDir,
  symlinkJoin,
  cacert,
}: let
  version = "1.0.0";
  source = symlinkJoin {
    name = "plex-exporter-source";
    paths = [
      (writeTextDir "go.mod" ''
        module plex-exporter

        go 1.24
      '')
      (writeTextDir "main.go" ''
        package main

        import (
            "encoding/xml"
            "errors"
            "fmt"
            "io"
            "log"
            "net/http"
            "net/url"
            "os"
            "strconv"
            "strings"
            "time"
        )

        const maxResponseBytes = 16 << 20

        type plexClient struct {
            baseURL string
            token   string
            client  *http.Client
        }

        type mediaContainer struct {
            Size      int       `xml:"size,attr"`
            TotalSize int       `xml:"totalSize,attr"`
            Videos    []video   `xml:"Video"`
            Sections  []section `xml:"Directory"`
        }

        type video struct {
            Transcodes []struct{} `xml:"TranscodeSession"`
        }

        type section struct {
            Key string `xml:"key,attr"`
        }

        type metrics struct {
            up          float64
            duration    float64
            sessions    int
            transcoding int
            library     int
        }

        func (c *plexClient) get(path string, query url.Values) (mediaContainer, error) {
            endpoint, err := url.Parse(c.baseURL + path)
            if err != nil {
                return mediaContainer{}, fmt.Errorf("parse endpoint: %w", err)
            }
            endpoint.RawQuery = query.Encode()
            req, err := http.NewRequest(http.MethodGet, endpoint.String(), nil)
            if err != nil {
                return mediaContainer{}, fmt.Errorf("create request: %w", err)
            }
            req.Header.Set("Accept", "application/xml")
            req.Header.Set("X-Plex-Token", c.token)
            resp, err := c.client.Do(req)
            if err != nil {
                return mediaContainer{}, fmt.Errorf("request Plex API: %w", err)
            }
            defer resp.Body.Close()
            if resp.StatusCode != http.StatusOK {
                _, _ = io.Copy(io.Discard, io.LimitReader(resp.Body, 4096))
                return mediaContainer{}, fmt.Errorf("Plex API returned %s", resp.Status)
            }
            var result mediaContainer
            decoder := xml.NewDecoder(io.LimitReader(resp.Body, maxResponseBytes))
            if err := decoder.Decode(&result); err != nil {
                return mediaContainer{}, fmt.Errorf("decode Plex response: %w", err)
            }
            return result, nil
        }

        func (c *plexClient) collect() (metrics, error) {
            started := time.Now()
            result := metrics{}
            sessions, err := c.get("/status/sessions", nil)
            if err != nil {
                result.duration = time.Since(started).Seconds()
                return result, err
            }
            result.sessions = sessions.Size
            for _, item := range sessions.Videos {
                if len(item.Transcodes) > 0 {
                    result.transcoding++
                }
            }
            sections, err := c.get("/library/sections", nil)
            if err != nil {
                result.duration = time.Since(started).Seconds()
                return result, err
            }
            for _, item := range sections.Sections {
                if item.Key == "" || strings.Contains(item.Key, "/") {
                    return result, errors.New("Plex returned an invalid library section key")
                }
                values := url.Values{
                    "X-Plex-Container-Start": {"0"},
                    "X-Plex-Container-Size":  {"0"},
                }
                library, err := c.get("/library/sections/"+item.Key+"/all", values)
                if err != nil {
                    result.duration = time.Since(started).Seconds()
                    return result, err
                }
                result.library += library.TotalSize
            }
            result.up = 1
            result.duration = time.Since(started).Seconds()
            return result, nil
        }

        func writeMetric(w io.Writer, name, help, kind string, value float64) {
            fmt.Fprintf(w, "# HELP %s %s\n# TYPE %s %s\n%s %s\n",
                name, help, name, kind, name, strconv.FormatFloat(value, 'g', -1, 64))
        }

        func metricsHandler(client *plexClient, logger *log.Logger) http.Handler {
            return http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
                result, err := client.collect()
                if err != nil {
                    logger.Printf("collect Plex metrics: %v", err)
                    http.Error(w, "Plex metrics collection failed", http.StatusInternalServerError)
                    return
                }
                w.Header().Set("Content-Type", "text/plain; version=0.0.4")
                writeMetric(w, "service_up", "Whether the Plex API collection succeeded.", "gauge", result.up)
                writeMetric(w, "service_api_request_duration_seconds", "Duration of the Plex API collection.", "gauge", result.duration)
                writeMetric(w, "plex_active_sessions", "Current active Plex sessions.", "gauge", float64(result.sessions))
                writeMetric(w, "plex_transcoding_sessions", "Current Plex sessions that are transcoding.", "gauge", float64(result.transcoding))
                writeMetric(w, "plex_library_items", "Total items across Plex libraries.", "gauge", float64(result.library))
            })
        }

        func run() error {
            baseURL := strings.TrimRight(os.Getenv("PLEX_URL"), "/")
            tokenFile := os.Getenv("PLEX_TOKEN_FILE")
            if baseURL == "" || tokenFile == "" {
                return errors.New("PLEX_URL and PLEX_TOKEN_FILE are required")
            }
            tokenBytes, err := os.ReadFile(tokenFile)
            if err != nil {
                return fmt.Errorf("read Plex token: %w", err)
            }
            token := strings.TrimSpace(string(tokenBytes))
            if token == "" {
                return errors.New("Plex token is empty")
            }
            client := &plexClient{
                baseURL: baseURL,
                token:   token,
                client:  &http.Client{Timeout: 15 * time.Second},
            }
            mux := http.NewServeMux()
            mux.Handle("/metrics", metricsHandler(client, log.Default()))
            server := &http.Server{
                Addr:              ":9100",
                Handler:           mux,
                ReadHeaderTimeout: 5 * time.Second,
                ReadTimeout:       10 * time.Second,
                WriteTimeout:      30 * time.Second,
                IdleTimeout:       60 * time.Second,
            }
            return server.ListenAndServe()
        }

        func main() {
            if err := run(); err != nil {
                log.Printf("Plex exporter stopped: %v", err)
                os.Exit(1)
            }
        }
      '')
      (writeTextDir "main_test.go" ''
        package main

        import (
            "io"
            "log"
            "net/http"
            "net/http/httptest"
            "strings"
            "testing"
        )

        func TestMetricsAreAggregateAndComplete(t *testing.T) {
            api := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
                switch r.URL.Path {
                case "/status/sessions":
                    io.WriteString(w, `<MediaContainer size="2"><Video title="private"><TranscodeSession/></Video><Video title="also-private"/></MediaContainer>`)
                case "/library/sections":
                    io.WriteString(w, `<MediaContainer><Directory key="1" title="private"/></MediaContainer>`)
                case "/library/sections/1/all":
                    io.WriteString(w, `<MediaContainer totalSize="42"/>`)
                default:
                    http.NotFound(w, r)
                }
            }))
            defer api.Close()
            client := &plexClient{baseURL: api.URL, token: "secret", client: api.Client()}
            recorder := httptest.NewRecorder()
            metricsHandler(client, log.New(io.Discard, "", 0)).ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/metrics", nil))
            body := recorder.Body.String()
            if recorder.Code != http.StatusOK {
                t.Fatalf("unexpected status %d: %s", recorder.Code, body)
            }
            for _, expected := range []string{"service_up 1", "plex_active_sessions 2", "plex_transcoding_sessions 1", "plex_library_items 42"} {
                if !strings.Contains(body, expected) {
                    t.Errorf("missing %q in metrics", expected)
                }
            }
            for _, private := range []string{"private", "title=", "user=", "session="} {
                if strings.Contains(body, private) {
                    t.Errorf("metrics leaked %q", private)
                }
            }
        }

        func TestMetricsReturns500WhenAPIUnavailable(t *testing.T) {
            api := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
                http.Error(w, "no", http.StatusUnauthorized)
            }))
            defer api.Close()
            client := &plexClient{baseURL: api.URL, token: "secret", client: api.Client()}
            recorder := httptest.NewRecorder()
            metricsHandler(client, log.New(io.Discard, "", 0)).ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/metrics", nil))
            if recorder.Code != http.StatusInternalServerError {
                t.Fatalf("got status %d, want 500", recorder.Code)
            }
        }
      '')
    ];
  };
  exporter = buildGoModule {
    pname = "plex-exporter";
    inherit version;
    src = source;
    vendorHash = null;
    ldflags = ["-s" "-w"];
    meta = {
      description = "Privacy-preserving aggregate Plex metrics exporter";
      license = lib.licenses.mit;
      mainProgram = "plex-exporter";
    };
  };
in
  dockerTools.buildLayeredImage {
    name = "localhost/plex-exporter";
    tag = version;
    contents = [cacert];
    config = {
      Entrypoint = ["${lib.getExe exporter}"];
      User = "65532:65532";
      ExposedPorts."9100/tcp" = {};
    };
  }
