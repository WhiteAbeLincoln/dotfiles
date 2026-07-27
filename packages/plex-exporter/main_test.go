package main

import (
	"context"
	"errors"
	"io"
	"log"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
	"time"
)

func TestMetricsAreAggregateAndComplete(t *testing.T) {
	api := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/status/sessions":
			_, _ = io.WriteString(w, `<MediaContainer size="2"><Video title="private"><TranscodeSession/></Video><Video title="also-private"/></MediaContainer>`)
		case "/library/sections":
			_, _ = io.WriteString(w, `<MediaContainer><Directory key="1" title="private"/></MediaContainer>`)
		case "/library/sections/1/all":
			_, _ = io.WriteString(w, `<MediaContainer totalSize="42"/>`)
		default:
			http.NotFound(w, r)
		}
	}))
	defer api.Close()
	client := &plexClient{baseURL: api.URL, token: "secret", client: api.Client()}
	recorder := httptest.NewRecorder()
	metricsHandler(client, log.New(io.Discard, "", 0), time.Second).ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/metrics", nil))
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
	metricsHandler(client, log.New(io.Discard, "", 0), time.Second).ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/metrics", nil))
	if recorder.Code != http.StatusInternalServerError {
		t.Fatalf("got status %d, want 500", recorder.Code)
	}
}

func TestMetricsRejectMissingRequiredField(t *testing.T) {
	api := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/status/sessions":
			_, _ = io.WriteString(w, `<MediaContainer><Video/></MediaContainer>`)
		default:
			_, _ = io.WriteString(w, `<MediaContainer/>`)
		}
	}))
	defer api.Close()
	client := &plexClient{baseURL: api.URL, token: "secret", client: api.Client()}
	recorder := httptest.NewRecorder()
	metricsHandler(client, log.New(io.Discard, "", 0), time.Second).ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/metrics", nil))
	if recorder.Code != http.StatusInternalServerError {
		t.Fatalf("got status %d, want 500 for a partial API response", recorder.Code)
	}
}

func TestScrapeDeadlineCancelsUpstreamRequests(t *testing.T) {
	canceled := make(chan struct{})
	api := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		<-r.Context().Done()
		close(canceled)
	}))
	defer api.Close()
	client := &plexClient{baseURL: api.URL, token: "secret", client: api.Client()}
	recorder := httptest.NewRecorder()
	metricsHandler(client, log.New(io.Discard, "", 0), 20*time.Millisecond).ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/metrics", nil))
	if recorder.Code != http.StatusInternalServerError {
		t.Fatalf("got status %d, want 500 after scrape deadline", recorder.Code)
	}
	select {
	case <-canceled:
	case <-time.After(time.Second):
		t.Fatal("upstream request continued after scrape deadline")
	}
}

func TestRequestCancellationStopsCollection(t *testing.T) {
	started := make(chan struct{})
	canceled := make(chan struct{})
	api := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		close(started)
		<-r.Context().Done()
		close(canceled)
	}))
	defer api.Close()
	client := &plexClient{baseURL: api.URL, token: "secret", client: api.Client()}
	requestContext, cancel := context.WithCancel(context.Background())
	request := httptest.NewRequest(http.MethodGet, "/metrics", nil).WithContext(requestContext)
	done := make(chan struct{})
	go func() {
		metricsHandler(client, log.New(io.Discard, "", 0), time.Second).ServeHTTP(httptest.NewRecorder(), request)
		close(done)
	}()
	<-started
	cancel()
	select {
	case <-canceled:
	case <-time.After(time.Second):
		t.Fatal("upstream request continued after client cancellation")
	}
	<-done
}

func TestOverlappingScrapeIsRejected(t *testing.T) {
	started := make(chan struct{})
	release := make(chan struct{})
	api := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/status/sessions" {
			close(started)
			<-release
		}
		_, _ = io.WriteString(w, `<MediaContainer size="0"/>`)
	}))
	defer api.Close()
	client := &plexClient{baseURL: api.URL, token: "secret", client: api.Client()}
	handler := metricsHandler(client, log.New(io.Discard, "", 0), time.Second)
	var group sync.WaitGroup
	group.Add(1)
	go func() {
		defer group.Done()
		handler.ServeHTTP(httptest.NewRecorder(), httptest.NewRequest(http.MethodGet, "/metrics", nil))
	}()
	<-started
	overlapping := httptest.NewRecorder()
	handler.ServeHTTP(overlapping, httptest.NewRequest(http.MethodGet, "/metrics", nil))
	close(release)
	group.Wait()
	if overlapping.Code != http.StatusServiceUnavailable {
		t.Fatalf("overlapping scrape got status %d, want 503", overlapping.Code)
	}
}

type failingWriter struct{}

func (failingWriter) Write([]byte) (int, error) {
	return 0, errors.New("write failed")
}

func TestMetricsWriterReturnsErrors(t *testing.T) {
	err := writeMetrics(failingWriter{}, metrics{})
	if err == nil {
		t.Fatal("writeMetrics ignored a writer error")
	}
}
