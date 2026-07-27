package main

import (
	"bytes"
	"context"
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
	Size      *int      `xml:"size,attr"`
	TotalSize *int      `xml:"totalSize,attr"`
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

func (c *plexClient) get(ctx context.Context, path string, query url.Values) (mediaContainer, error) {
	endpoint, err := url.Parse(c.baseURL + path)
	if err != nil {
		return mediaContainer{}, fmt.Errorf("parse endpoint: %w", err)
	}
	endpoint.RawQuery = query.Encode()
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, endpoint.String(), nil)
	if err != nil {
		return mediaContainer{}, fmt.Errorf("create request: %w", err)
	}
	req.Header.Set("Accept", "application/xml")
	req.Header.Set("X-Plex-Token", c.token)
	resp, err := c.client.Do(req)
	if err != nil {
		return mediaContainer{}, fmt.Errorf("request Plex API: %w", err)
	}
	defer func() {
		_ = resp.Body.Close()
	}()
	if resp.StatusCode != http.StatusOK {
		_, _ = io.Copy(io.Discard, io.LimitReader(resp.Body, 4096))
		return mediaContainer{}, fmt.Errorf("plex API returned %s", resp.Status)
	}
	var result mediaContainer
	decoder := xml.NewDecoder(io.LimitReader(resp.Body, maxResponseBytes))
	if err := decoder.Decode(&result); err != nil {
		return mediaContainer{}, fmt.Errorf("decode Plex response: %w", err)
	}
	return result, nil
}

func (c *plexClient) collect(ctx context.Context) (metrics, error) {
	started := time.Now()
	result := metrics{}
	sessions, err := c.get(ctx, "/status/sessions", nil)
	if err != nil {
		result.duration = time.Since(started).Seconds()
		return result, err
	}
	if sessions.Size == nil || *sessions.Size < 0 {
		return result, errors.New("plex sessions response omitted a valid size")
	}
	result.sessions = *sessions.Size
	for _, item := range sessions.Videos {
		if len(item.Transcodes) > 0 {
			result.transcoding++
		}
	}
	sections, err := c.get(ctx, "/library/sections", nil)
	if err != nil {
		result.duration = time.Since(started).Seconds()
		return result, err
	}
	for _, item := range sections.Sections {
		if item.Key == "" || strings.Contains(item.Key, "/") {
			return result, errors.New("plex returned an invalid library section key")
		}
		values := url.Values{
			"X-Plex-Container-Start": {"0"},
			"X-Plex-Container-Size":  {"0"},
		}
		library, err := c.get(ctx, "/library/sections/"+item.Key+"/all", values)
		if err != nil {
			result.duration = time.Since(started).Seconds()
			return result, err
		}
		if library.TotalSize == nil || *library.TotalSize < 0 {
			return result, errors.New("plex library response omitted a valid totalSize")
		}
		result.library += *library.TotalSize
	}
	result.up = 1
	result.duration = time.Since(started).Seconds()
	return result, nil
}

func writeMetric(w io.Writer, name, help, kind string, value float64) error {
	_, err := fmt.Fprintf(w, "# HELP %s %s\n# TYPE %s %s\n%s %s\n",
		name, help, name, kind, name, strconv.FormatFloat(value, 'g', -1, 64))
	return err
}

func writeMetrics(w io.Writer, result metrics) error {
	values := []struct {
		name  string
		help  string
		kind  string
		value float64
	}{
		{"service_up", "Whether the Plex API collection succeeded.", "gauge", result.up},
		{"service_api_request_duration_seconds", "Duration of the Plex API collection.", "gauge", result.duration},
		{"plex_active_sessions", "Current active Plex sessions.", "gauge", float64(result.sessions)},
		{"plex_transcoding_sessions", "Current Plex sessions that are transcoding.", "gauge", float64(result.transcoding)},
		{"plex_library_items", "Total items across Plex libraries.", "gauge", float64(result.library)},
	}
	for _, value := range values {
		if err := writeMetric(w, value.name, value.help, value.kind, value.value); err != nil {
			return fmt.Errorf("write %s: %w", value.name, err)
		}
	}
	return nil
}

func metricsHandler(client *plexClient, logger *log.Logger, scrapeTimeout time.Duration) http.Handler {
	gate := make(chan struct{}, 1)
	return http.HandlerFunc(func(w http.ResponseWriter, request *http.Request) {
		select {
		case gate <- struct{}{}:
			defer func() { <-gate }()
		default:
			http.Error(w, "Plex metrics collection already in progress", http.StatusServiceUnavailable)
			return
		}
		ctx, cancel := context.WithTimeout(request.Context(), scrapeTimeout)
		defer cancel()
		result, err := client.collect(ctx)
		if err != nil {
			logger.Printf("collect Plex metrics: %v", err)
			http.Error(w, "Plex metrics collection failed", http.StatusInternalServerError)
			return
		}
		var output bytes.Buffer
		if err := writeMetrics(&output, result); err != nil {
			logger.Printf("render Plex metrics: %v", err)
			http.Error(w, "Plex metrics rendering failed", http.StatusInternalServerError)
			return
		}
		w.Header().Set("Content-Type", "text/plain; version=0.0.4")
		if _, err := io.Copy(w, &output); err != nil {
			logger.Printf("write Plex metrics response: %v", err)
		}
	})
}

func run() error {
	baseURL := strings.TrimRight(os.Getenv("PLEX_URL"), "/")
	tokenFile := os.Getenv("PLEX_TOKEN_FILE")
	if baseURL == "" || tokenFile == "" {
		return errors.New("required environment variables are missing: PLEX_URL and PLEX_TOKEN_FILE")
	}
	tokenBytes, err := os.ReadFile(tokenFile)
	if err != nil {
		return fmt.Errorf("read Plex token: %w", err)
	}
	token := strings.TrimSpace(string(tokenBytes))
	if token == "" {
		return errors.New("plex token is empty")
	}
	client := &plexClient{
		baseURL: baseURL,
		token:   token,
		client:  &http.Client{Timeout: 15 * time.Second},
	}
	mux := http.NewServeMux()
	mux.Handle("/metrics", metricsHandler(client, log.Default(), 25*time.Second))
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
