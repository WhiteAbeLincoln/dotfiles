package main

import (
	"bytes"
	"context"
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
	baseURL  string
	username string
	password string
	client   *http.Client
}

type statsResponse struct {
	Queries         *float64 `json:"num_dns_queries"`
	Blocked         *float64 `json:"num_blocked_filtering"`
	AverageDuration *float64 `json:"avg_processing_time"`
}

type statusResponse struct {
	ProtectionEnabled *bool `json:"protection_enabled"`
}

type filteringResponse struct {
	Enabled *bool `json:"enabled"`
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

func (c *adguardClient) get(ctx context.Context, path string, target any) error {
	endpoint, err := url.Parse(c.baseURL + path)
	if err != nil {
		return fmt.Errorf("parse endpoint: %w", err)
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, endpoint.String(), nil)
	if err != nil {
		return fmt.Errorf("create request: %w", err)
	}
	req.SetBasicAuth(c.username, c.password)
	resp, err := c.client.Do(req)
	if err != nil {
		return fmt.Errorf("request AdGuard API: %w", err)
	}
	defer func() {
		_ = resp.Body.Close()
	}()
	if resp.StatusCode != http.StatusOK {
		_, _ = io.Copy(io.Discard, io.LimitReader(resp.Body, 4096))
		return fmt.Errorf("adguard API returned %s", resp.Status)
	}
	decoder := json.NewDecoder(io.LimitReader(resp.Body, maxResponseBytes))
	if err := decoder.Decode(target); err != nil {
		return fmt.Errorf("decode AdGuard response: %w", err)
	}
	return nil
}

func (c *adguardClient) collect(ctx context.Context) (metrics, error) {
	started := time.Now()
	var stats statsResponse
	var status statusResponse
	var filtering filteringResponse
	if err := c.get(ctx, "/control/stats", &stats); err != nil {
		return metrics{duration: time.Since(started).Seconds()}, err
	}
	if stats.Queries == nil || stats.Blocked == nil || stats.AverageDuration == nil {
		return metrics{duration: time.Since(started).Seconds()}, errors.New("adguard stats response omitted required fields")
	}
	if *stats.Queries < 0 || *stats.Blocked < 0 || *stats.AverageDuration < 0 {
		return metrics{duration: time.Since(started).Seconds()}, errors.New("adguard stats response contained negative values")
	}
	if err := c.get(ctx, "/control/status", &status); err != nil {
		return metrics{duration: time.Since(started).Seconds()}, err
	}
	if status.ProtectionEnabled == nil {
		return metrics{duration: time.Since(started).Seconds()}, errors.New("adguard status response omitted protection_enabled")
	}
	if err := c.get(ctx, "/control/filtering/status", &filtering); err != nil {
		return metrics{duration: time.Since(started).Seconds()}, err
	}
	if filtering.Enabled == nil {
		return metrics{duration: time.Since(started).Seconds()}, errors.New("adguard filtering response omitted enabled")
	}
	result := metrics{
		up:            1,
		duration:      time.Since(started).Seconds(),
		queries:       *stats.Queries,
		blocked:       *stats.Blocked,
		queryDuration: *stats.AverageDuration,
	}
	if *status.ProtectionEnabled {
		result.protectionEnabled = 1
	}
	if *filtering.Enabled {
		result.filterEnabled = 1
	}
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
		{"service_up", "Whether the AdGuard API collection succeeded.", "gauge", result.up},
		{"service_api_request_duration_seconds", "Duration of the AdGuard API collection.", "gauge", result.duration},
		{"adguard_queries_total", "Total DNS queries handled by AdGuard.", "counter", result.queries},
		{"adguard_blocked_queries_total", "Total DNS queries blocked by AdGuard.", "counter", result.blocked},
		{"adguard_query_duration_seconds", "Average AdGuard DNS query duration.", "gauge", result.queryDuration},
		{"adguard_protection_enabled", "Whether AdGuard protection is enabled.", "gauge", result.protectionEnabled},
		{"adguard_filter_enabled", "Whether AdGuard filtering is enabled.", "gauge", result.filterEnabled},
	}
	for _, value := range values {
		if err := writeMetric(w, value.name, value.help, value.kind, value.value); err != nil {
			return fmt.Errorf("write %s: %w", value.name, err)
		}
	}
	return nil
}

func metricsHandler(client *adguardClient, logger *log.Logger, scrapeTimeout time.Duration) http.Handler {
	gate := make(chan struct{}, 1)
	return http.HandlerFunc(func(w http.ResponseWriter, request *http.Request) {
		select {
		case gate <- struct{}{}:
			defer func() { <-gate }()
		default:
			http.Error(w, "AdGuard metrics collection already in progress", http.StatusServiceUnavailable)
			return
		}
		ctx, cancel := context.WithTimeout(request.Context(), scrapeTimeout)
		defer cancel()
		result, err := client.collect(ctx)
		if err != nil {
			logger.Printf("collect AdGuard metrics: %v", err)
			http.Error(w, "AdGuard metrics collection failed", http.StatusInternalServerError)
			return
		}
		var output bytes.Buffer
		if err := writeMetrics(&output, result); err != nil {
			logger.Printf("render AdGuard metrics: %v", err)
			http.Error(w, "AdGuard metrics rendering failed", http.StatusInternalServerError)
			return
		}
		w.Header().Set("Content-Type", "text/plain; version=0.0.4")
		if _, err := io.Copy(w, &output); err != nil {
			logger.Printf("write AdGuard metrics response: %v", err)
		}
	})
}

func run() error {
	baseURL := strings.TrimRight(os.Getenv("ADGUARD_URL"), "/")
	username := os.Getenv("ADGUARD_USERNAME")
	passwordFile := os.Getenv("ADGUARD_PASSWORD_FILE")
	if baseURL == "" || username == "" || passwordFile == "" {
		return errors.New("required environment variables are missing: ADGUARD_URL, ADGUARD_USERNAME, and ADGUARD_PASSWORD_FILE")
	}
	passwordBytes, err := os.ReadFile(passwordFile)
	if err != nil {
		return fmt.Errorf("read AdGuard password: %w", err)
	}
	password := strings.TrimSpace(string(passwordBytes))
	if password == "" {
		return errors.New("adguard password is empty")
	}
	client := &adguardClient{
		baseURL:  baseURL,
		username: username,
		password: password,
		client:   &http.Client{Timeout: 15 * time.Second},
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
		log.Printf("AdGuard exporter stopped: %v", err)
		os.Exit(1)
	}
}
