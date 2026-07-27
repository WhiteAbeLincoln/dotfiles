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
    name = "adguard-exporter-source";
    paths = [
      (writeTextDir "go.mod" ''
        module adguard-exporter

        go 1.24
      '')
      (writeTextDir "main.go" ''
        package main

        import (
            "encoding/json"
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

        const maxResponseBytes = 1 << 20

        type adguardClient struct {
            baseURL string
            username string
            password string
            client   *http.Client
        }

        type statsResponse struct {
            Queries           float64 `json:"num_dns_queries"`
            Blocked           float64 `json:"num_blocked_filtering"`
            AverageDuration   float64 `json:"avg_processing_time"`
        }

        type statusResponse struct {
            ProtectionEnabled bool `json:"protection_enabled"`
        }

        type filteringResponse struct {
            Enabled bool `json:"enabled"`
        }

        type metrics struct {
            up                float64
            duration          float64
            queries           float64
            blocked           float64
            queryDuration     float64
            protectionEnabled float64
            filterEnabled     float64
        }

        func (c *adguardClient) get(path string, target any) error {
            endpoint, err := url.Parse(c.baseURL + path)
            if err != nil {
                return fmt.Errorf("parse endpoint: %w", err)
            }
            req, err := http.NewRequest(http.MethodGet, endpoint.String(), nil)
            if err != nil {
                return fmt.Errorf("create request: %w", err)
            }
            req.SetBasicAuth(c.username, c.password)
            resp, err := c.client.Do(req)
            if err != nil {
                return fmt.Errorf("request AdGuard API: %w", err)
            }
            defer resp.Body.Close()
            if resp.StatusCode != http.StatusOK {
                _, _ = io.Copy(io.Discard, io.LimitReader(resp.Body, 4096))
                return fmt.Errorf("AdGuard API returned %s", resp.Status)
            }
            decoder := json.NewDecoder(io.LimitReader(resp.Body, maxResponseBytes))
            if err := decoder.Decode(target); err != nil {
                return fmt.Errorf("decode AdGuard response: %w", err)
            }
            return nil
        }

        func (c *adguardClient) collect() (metrics, error) {
            started := time.Now()
            var stats statsResponse
            var status statusResponse
            var filtering filteringResponse
            if err := c.get("/control/stats", &stats); err != nil {
                return metrics{duration: time.Since(started).Seconds()}, err
            }
            if err := c.get("/control/status", &status); err != nil {
                return metrics{duration: time.Since(started).Seconds()}, err
            }
            if err := c.get("/control/filtering/status", &filtering); err != nil {
                return metrics{duration: time.Since(started).Seconds()}, err
            }
            result := metrics{
                up:            1,
                duration:      time.Since(started).Seconds(),
                queries:       stats.Queries,
                blocked:       stats.Blocked,
                queryDuration: stats.AverageDuration,
            }
            if status.ProtectionEnabled {
                result.protectionEnabled = 1
            }
            if filtering.Enabled {
                result.filterEnabled = 1
            }
            return result, nil
        }

        func writeMetric(w io.Writer, name, help, kind string, value float64) {
            fmt.Fprintf(w, "# HELP %s %s\n# TYPE %s %s\n%s %s\n",
                name, help, name, kind, name, strconv.FormatFloat(value, 'g', -1, 64))
        }

        func metricsHandler(client *adguardClient, logger *log.Logger) http.Handler {
            return http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
                result, err := client.collect()
                if err != nil {
                    logger.Printf("collect AdGuard metrics: %v", err)
                    http.Error(w, "AdGuard metrics collection failed", http.StatusInternalServerError)
                    return
                }
                w.Header().Set("Content-Type", "text/plain; version=0.0.4")
                writeMetric(w, "service_up", "Whether the AdGuard API collection succeeded.", "gauge", result.up)
                writeMetric(w, "service_api_request_duration_seconds", "Duration of the AdGuard API collection.", "gauge", result.duration)
                writeMetric(w, "adguard_queries_total", "Total DNS queries handled by AdGuard.", "counter", result.queries)
                writeMetric(w, "adguard_blocked_queries_total", "Total DNS queries blocked by AdGuard.", "counter", result.blocked)
                writeMetric(w, "adguard_query_duration_seconds", "Average AdGuard DNS query duration.", "gauge", result.queryDuration)
                writeMetric(w, "adguard_protection_enabled", "Whether AdGuard protection is enabled.", "gauge", result.protectionEnabled)
                writeMetric(w, "adguard_filter_enabled", "Whether AdGuard filtering is enabled.", "gauge", result.filterEnabled)
            })
        }

        func run() error {
            baseURL := strings.TrimRight(os.Getenv("ADGUARD_URL"), "/")
            username := os.Getenv("ADGUARD_USERNAME")
            passwordFile := os.Getenv("ADGUARD_PASSWORD_FILE")
            if baseURL == "" || username == "" || passwordFile == "" {
                return errors.New("ADGUARD_URL, ADGUARD_USERNAME, and ADGUARD_PASSWORD_FILE are required")
            }
            passwordBytes, err := os.ReadFile(passwordFile)
            if err != nil {
                return fmt.Errorf("read AdGuard password: %w", err)
            }
            password := strings.TrimSpace(string(passwordBytes))
            if password == "" {
                return errors.New("AdGuard password is empty")
            }
            client := &adguardClient{
                baseURL: baseURL,
                username: username,
                password: password,
                client: &http.Client{Timeout: 15 * time.Second},
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
                log.Printf("AdGuard exporter stopped: %v", err)
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

        func TestMetricsExcludeQueryDetails(t *testing.T) {
            api := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
                switch r.URL.Path {
                case "/control/stats":
                    io.WriteString(w, `{"num_dns_queries":12,"num_blocked_filtering":5,"avg_processing_time":0.003,"top_clients":[{"private-client":10}],"top_queried_domains":[{"private.example":8}]}`)
                case "/control/status":
                    io.WriteString(w, `{"protection_enabled":true}`)
                case "/control/filtering/status":
                    io.WriteString(w, `{"enabled":true,"filters":[{"name":"private-list"}]}`)
                default:
                    http.NotFound(w, r)
                }
            }))
            defer api.Close()
            client := &adguardClient{baseURL: api.URL, username: "admin", password: "secret", client: api.Client()}
            recorder := httptest.NewRecorder()
            metricsHandler(client, log.New(io.Discard, "", 0)).ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/metrics", nil))
            body := recorder.Body.String()
            if recorder.Code != http.StatusOK {
                t.Fatalf("unexpected status %d: %s", recorder.Code, body)
            }
            for _, expected := range []string{"service_up 1", "adguard_queries_total 12", "adguard_blocked_queries_total 5", "adguard_query_duration_seconds 0.003", "adguard_protection_enabled 1", "adguard_filter_enabled 1"} {
                if !strings.Contains(body, expected) {
                    t.Errorf("missing %q in metrics", expected)
                }
            }
            for _, private := range []string{"private-client", "private.example", "private-list", "client=", "domain="} {
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
            client := &adguardClient{baseURL: api.URL, username: "admin", password: "secret", client: api.Client()}
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
    pname = "adguard-exporter";
    inherit version;
    src = source;
    vendorHash = null;
    ldflags = ["-s" "-w"];
    meta = {
      description = "Privacy-preserving aggregate AdGuard Home metrics exporter";
      license = lib.licenses.mit;
      mainProgram = "adguard-exporter";
    };
  };
in
  dockerTools.buildLayeredImage {
    name = "localhost/adguard-exporter";
    tag = version;
    contents = [cacert];
    config = {
      Entrypoint = ["${lib.getExe exporter}"];
      User = "65532:65532";
      ExposedPorts."9100/tcp" = {};
    };
  }
