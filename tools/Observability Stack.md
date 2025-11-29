---
title: Observability Stack
aliases:
  - Monitoring Stack
  - O11y Stack
tags:
  - tool
  - observability
  - monitoring
  - comparison
type: reference
status: complete
created: 2025-11-28
---

# Observability Stack

The three pillars of observability and how to implement them.

## Three Pillars

```
┌─────────────────────────────────────────────────────────────┐
│                    OBSERVABILITY                             │
├───────────────────┬───────────────────┬─────────────────────┤
│      METRICS      │       LOGS        │       TRACES        │
├───────────────────┼───────────────────┼─────────────────────┤
│ What's happening? │ Why did it happen?│ How did it flow?    │
│ Aggregated values │ Discrete events   │ Request journey     │
│ Time-series       │ Unstructured/JSON │ Spans & context     │
├───────────────────┼───────────────────┼─────────────────────┤
│ Prometheus        │ Loki              │ Tempo               │
│ InfluxDB          │ Elasticsearch     │ Jaeger              │
│ Datadog           │ Splunk            │ Zipkin              │
└───────────────────┴───────────────────┴─────────────────────┘
```

---

## Common Stacks

### Grafana Stack (LGTM)

**Free, open-source, cloud-native.**

| Component | Role |
|-----------|------|
| Loki | Logs |
| Grafana | Visualization |
| Tempo | Traces |
| Mimir | Metrics (long-term) |
| Prometheus | Metrics (collection) |
| Alloy/Agent | Collection |

```
┌─────────────────────────────────────────────────────────────┐
│                    Applications                              │
└──────────────────────────┬──────────────────────────────────┘
                           │ OTLP / Prometheus / logs
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                   Grafana Alloy                              │
│              (collect, transform, forward)                   │
└────────┬─────────────────┬──────────────────┬───────────────┘
         │                 │                  │
         ▼                 ▼                  ▼
    ┌─────────┐      ┌──────────┐      ┌──────────┐
    │  Mimir  │      │   Loki   │      │  Tempo   │
    │(metrics)│      │  (logs)  │      │ (traces) │
    └────┬────┘      └────┬─────┘      └────┬─────┘
         │                │                 │
         └────────────────┴─────────────────┘
                          │
                          ▼
                   ┌────────────┐
                   │  Grafana   │
                   └────────────┘
```

### ELK Stack (Elastic)

**Powerful search, log-centric.**

| Component | Role |
|-----------|------|
| Elasticsearch | Storage, search |
| Logstash | Ingestion, transformation |
| Kibana | Visualization |
| Beats | Collection agents |
| APM | Traces |

### Commercial Solutions

| Platform | Strengths |
|----------|-----------|
| Datadog | All-in-one, easy setup |
| New Relic | APM-focused |
| Splunk | Log search, enterprise |
| Dynatrace | AI-powered, auto-discovery |
| Honeycomb | High cardinality, debugging |

---

## Stack Comparison

| Aspect | Grafana Stack | ELK | Datadog |
|--------|---------------|-----|---------|
| Cost | Free (OSS) | Free (OSS) or paid | Per host + metrics |
| Metrics | Excellent | Adequate | Excellent |
| Logs | Good | Excellent | Excellent |
| Traces | Good | Good | Excellent |
| Setup complexity | Medium | High | Low |
| Scalability | High | High | Managed |
| Vendor lock-in | Low | Medium | High |

---

## Metrics Deep Dive

### What to Measure

| Category | Examples |
|----------|----------|
| RED (services) | Rate, Errors, Duration |
| USE (resources) | Utilization, Saturation, Errors |
| Golden Signals | Latency, Traffic, Errors, Saturation |

### Tools Comparison

| Tool | Type | Best For |
|------|------|----------|
| [[Prometheus]] | Pull-based | Cloud-native, Kubernetes |
| InfluxDB | Push-based | IoT, high cardinality |
| VictoriaMetrics | Prometheus-compatible | Better performance |
| Datadog Metrics | SaaS | Easy setup |
| CloudWatch | AWS native | AWS workloads |

### Prometheus vs InfluxDB

| Aspect | Prometheus | InfluxDB |
|--------|------------|----------|
| Collection | Pull (scrape) | Push |
| Query | PromQL | InfluxQL/Flux |
| Cardinality | Lower (label limits) | Higher tolerance |
| Ecosystem | Huge (exporters) | Smaller |
| Long-term | Need Thanos/Mimir | Built-in |

---

## Logs Deep Dive

### What to Log

| Level | When |
|-------|------|
| ERROR | Something failed |
| WARN | Potential problem |
| INFO | Significant events |
| DEBUG | Development detail |

### Structured Logging

```json
{
  "timestamp": "2024-01-15T10:30:00Z",
  "level": "ERROR",
  "service": "api",
  "trace_id": "abc123",
  "message": "Database connection failed",
  "error": "connection refused",
  "host": "api-pod-xyz"
}
```

### Tools Comparison

| Tool | Best For |
|------|----------|
| Loki | Cost-effective, Grafana users |
| Elasticsearch | Full-text search |
| Splunk | Enterprise, compliance |
| CloudWatch Logs | AWS native |
| Papertrail | Simple SaaS |

### Loki vs Elasticsearch

| Aspect | Loki | Elasticsearch |
|--------|------|---------------|
| Indexing | Labels only | Full content |
| Cost | Lower | Higher |
| Query speed | Label-fast, content-slow | Fast everywhere |
| Setup | Simple | Complex |
| Scale | Easier | Harder |

---

## Traces Deep Dive

### When Traces Help

- Request flows through multiple services
- Debugging latency issues
- Understanding dependencies
- Finding bottlenecks

### Trace Structure

```
Trace (trace_id: abc123)
├── Span: API Gateway (100ms)
│   ├── Span: Auth Service (20ms)
│   └── Span: User Service (70ms)
│       ├── Span: Database Query (30ms)
│       └── Span: Cache Lookup (5ms)
```

### Tools Comparison

| Tool | Best For |
|------|----------|
| Tempo | Grafana users, cost-effective |
| Jaeger | CNCF standard |
| Zipkin | Simple, mature |
| Datadog APM | Full APM features |
| AWS X-Ray | AWS native |

### Sampling Strategies

| Strategy | Description |
|----------|-------------|
| Head-based | Decide at trace start |
| Tail-based | Decide after trace complete |
| Probabilistic | Random percentage |
| Rate-limited | Max traces per second |

---

## Unified Collection

### OpenTelemetry

Standard for telemetry collection.

```
┌─────────────────────────────────────────────────────────────┐
│                    Your Application                          │
│              (OTel SDK instrumentation)                      │
└──────────────────────────┬──────────────────────────────────┘
                           │ OTLP (metrics, logs, traces)
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                  OTel Collector                              │
│         (receive, process, export)                           │
└────────┬─────────────────┬──────────────────┬───────────────┘
         │                 │                  │
         ▼                 ▼                  ▼
    Prometheus          Loki              Jaeger
```

See [[OpenTelemetry]] for details.

### Grafana Alloy

Grafana's distribution of OTel Collector with extras.

| Feature | OTel Collector | Grafana Alloy |
|---------|----------------|---------------|
| Core | Same | Same |
| Config | YAML | River (HCL-like) |
| Prometheus scraping | Extension | Native |
| Log collection | Extension | Native |

---

## Correlation

### Connecting Signals

```
Metric spike detected
    │
    ├──▶ Exemplar links to trace_id
    │         │
    │         ▼
    │    Trace shows slow span
    │         │
    │         ├──▶ span_id in logs
    │         │         │
    │         │         ▼
    │         │    Log shows error detail
    │         │
    └─────────┴──▶ Root cause found
```

### Implementation

```json
// Log with trace context
{
  "message": "Request failed",
  "trace_id": "abc123",
  "span_id": "def456"
}
```

```promql
// Prometheus exemplar linking to trace
http_request_duration_seconds_bucket{le="0.5"} 100 # trace_id=abc123
```

---

## Alerting Strategy

### Alert Hierarchy

| Level | Response | Example |
|-------|----------|---------|
| Page | Immediate | Service down |
| Ticket | Next business day | Disk 80% |
| Log | Investigate when time | Unusual pattern |

### Alert Quality

| Good Alert | Bad Alert |
|------------|-----------|
| Actionable | "CPU high" with no context |
| Rare | Fires constantly (alert fatigue) |
| Timely | Too late to fix |
| Accurate | False positives |

### SLO-Based Alerting

```
Error Budget: 0.1% (99.9% availability)

Monthly budget: 43 minutes downtime

Alert when: Burn rate exceeds threshold
  - 14.4x burn rate for 1 hour → page
  - 6x burn rate for 6 hours → ticket
```

---

## Quick Start Recommendations

### Small Team / Startup

```
Prometheus → Grafana
     ↑
Application (metrics endpoint)
```

- Prometheus for metrics
- Grafana for visualization
- Structured logs to stdout
- Add tracing when needed

### Growing Team

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ Prometheus  │    │    Loki     │    │   Tempo     │
└──────┬──────┘    └──────┬──────┘    └──────┬──────┘
       └──────────────────┼──────────────────┘
                          │
                    ┌─────┴─────┐
                    │  Grafana  │
                    └───────────┘
```

- Full Grafana stack
- OpenTelemetry for instrumentation
- Alertmanager for routing

### Enterprise / Large Scale

- Consider commercial (Datadog, Splunk)
- Or: Grafana stack with Mimir/Loki/Tempo at scale
- Dedicated observability team
- SLO-based alerting

---

## Related

- [[Prometheus]]
- [[Grafana]]
- [[OpenTelemetry]]
- [[Distributed Tracing]]
- [[Logging Libraries]]
