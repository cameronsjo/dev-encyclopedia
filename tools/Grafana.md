---
title: Grafana
aliases:
  - Grafana Labs
tags:
  - tool
  - observability
  - visualization
  - dashboards
  - monitoring
type: reference
status: complete
created: '2025-11-28'
---

# Grafana

Open-source visualization and observability platform.

## Overview

| Aspect | Details |
|--------|---------|
| Type | Visualization, dashboards, alerting |
| Data Sources | 100+ integrations |
| License | AGPL v3 (OSS) / Commercial |
| Backing | Grafana Labs |
| Deployment | Self-hosted or Grafana Cloud |

---

## The Grafana Stack (LGTM)

```
┌─────────────────────────────────────────────────────────────┐
│                      Grafana Stack                           │
├────────────────┬────────────────┬────────────────┬──────────┤
│     Loki       │    Grafana     │    Tempo       │  Mimir   │
│    (Logs)      │   (Visualize)  │   (Traces)     │ (Metrics)│
├────────────────┴────────────────┴────────────────┴──────────┤
│                     Grafana Agent                            │
│              (Collection & Forwarding)                       │
└─────────────────────────────────────────────────────────────┘
```

| Component | Purpose | Alternative |
|-----------|---------|-------------|
| Grafana | Visualization | - |
| Loki | Log aggregation | Elasticsearch |
| Tempo | Distributed tracing | Jaeger |
| Mimir | Long-term metrics | Thanos, Cortex |
| Alloy/Agent | Collection | OTel Collector |

---

## Data Sources

### Metrics

| Source | Notes |
|--------|-------|
| Prometheus | Native, most common |
| InfluxDB | Time-series |
| Graphite | Legacy metrics |
| CloudWatch | AWS |
| Azure Monitor | Azure |
| Google Cloud Monitoring | GCP |

### Logs

| Source | Notes |
|--------|-------|
| Loki | Grafana-native |
| Elasticsearch | ELK stack |
| CloudWatch Logs | AWS |

### Traces

| Source | Notes |
|--------|-------|
| Tempo | Grafana-native |
| Jaeger | CNCF tracing |
| Zipkin | Older tracing |

### Databases

| Source | Notes |
|--------|-------|
| PostgreSQL | Direct SQL |
| MySQL | Direct SQL |
| ClickHouse | Analytics |

---

## Dashboards

### Panel Types

| Panel | Use Case |
|-------|----------|
| Time series | Metrics over time |
| Stat | Single value |
| Gauge | Value with thresholds |
| Bar chart | Comparisons |
| Table | Tabular data |
| Heatmap | Distribution over time |
| Logs | Log viewer |
| Traces | Trace viewer |
| Geomap | Geographic data |

### Dashboard Structure

```
Dashboard
├── Variables (dropdowns for filtering)
├── Rows (grouping)
│   ├── Panel 1 (time series)
│   ├── Panel 2 (stat)
│   └── Panel 3 (table)
└── Annotations (events overlay)
```

### Variables (Templating)

Dynamic dashboards with dropdowns.

```
# Query variable from Prometheus labels
label_values(up, instance)

# Custom values
production, staging, development

# Chained variables
label_values(up{environment="$environment"}, service)
```

---

## Query Examples

### Prometheus Panels

```promql
# Request rate
rate(http_requests_total{job="api"}[5m])

# Error percentage
sum(rate(http_requests_total{status=~"5.."}[5m]))
/ sum(rate(http_requests_total[5m])) * 100

# P95 latency
histogram_quantile(0.95,
  sum(rate(http_request_duration_seconds_bucket[5m])) by (le)
)

# Memory usage %
(1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes) * 100
```

### Loki Panels

```logql
# Error logs
{job="api"} |= "error"

# JSON parsing
{job="api"} | json | level="error"

# Rate of errors
sum(rate({job="api"} |= "error" [5m]))

# Log context around errors
{job="api"} |= "error" | line_format "{{.message}}"
```

---

## Alerting

### Alert Rules

```yaml
# Grafana Alerting (Unified Alerting)
apiVersion: 1
groups:
  - orgId: 1
    name: app_alerts
    folder: Alerts
    interval: 1m
    rules:
      - uid: high-error-rate
        title: High Error Rate
        condition: C
        data:
          - refId: A
            datasourceUid: prometheus
            model:
              expr: sum(rate(http_requests_total{status=~"5.."}[5m]))
          - refId: B
            datasourceUid: prometheus
            model:
              expr: sum(rate(http_requests_total[5m]))
          - refId: C
            datasourceUid: __expr__
            model:
              type: math
              expression: $A / $B > 0.05
        for: 5m
        annotations:
          summary: Error rate above 5%
        labels:
          severity: critical
```

### Contact Points

| Channel | Notes |
|---------|-------|
| Slack | Webhooks |
| PagerDuty | Incident management |
| Email | SMTP |
| Webhook | Custom integrations |
| Microsoft Teams | Webhooks |
| Opsgenie | Atlassian alerting |

### Notification Policies

Route alerts based on labels.

```yaml
# Route critical alerts to PagerDuty, others to Slack
policies:
  - receiver: slack-general
    matchers:
      - severity != critical
  - receiver: pagerduty
    matchers:
      - severity = critical
```

---

## Loki (Logs)

### Overview

Log aggregation inspired by Prometheus.

| Feature | Description |
|---------|-------------|
| Labels | Index by metadata, not content |
| LogQL | Query language |
| Cost-effective | Doesn't index log content |
| Scales | Horizontally |

### LogQL Basics

```logql
# Stream selector
{job="api", environment="production"}

# Filter expressions
{job="api"} |= "error"           # contains
{job="api"} != "debug"           # doesn't contain
{job="api"} |~ "error|warn"      # regex

# Parser
{job="api"} | json
{job="api"} | logfmt
{job="api"} | pattern "<ip> - <user>"

# Metric queries
count_over_time({job="api"} |= "error" [5m])
rate({job="api"} |= "error" [5m])
```

### Loki vs Elasticsearch

| Aspect | Loki | Elasticsearch |
|--------|------|---------------|
| Indexing | Labels only | Full text |
| Storage cost | Lower | Higher |
| Query speed | Slower for unindexed | Fast full-text |
| Complexity | Simpler | Complex |
| Best for | Cloud-native, cost-sensitive | Full-text search needs |

---

## Tempo (Traces)

### Overview

Distributed tracing backend.

| Feature | Description |
|---------|-------------|
| Storage | Object storage (S3, GCS) |
| Protocol | OTLP, Jaeger, Zipkin |
| Sampling | Tail-based |
| Cost | Storage-only indexing |

### TraceQL

```traceql
# Find traces by service
{ resource.service.name = "api" }

# By span attributes
{ span.http.status_code >= 500 }

# Duration filter
{ span.http.url = "/api/users" } | duration > 1s

# Combine conditions
{ resource.service.name = "api" && span.http.status_code = 500 }
```

### Correlating Signals

```
┌─────────────────────────────────────────┐
│                Metrics                   │
│    ───────────────────────────────      │
│         ↑                               │
│         │ exemplars                     │
│         │ (link metrics → traces)       │
│         ▼                               │
│    ───────────────────────────────      │
│                Traces                    │
│         ↑                               │
│         │ trace_id in logs              │
│         ▼                               │
│    ───────────────────────────────      │
│                 Logs                     │
└─────────────────────────────────────────┘
```

---

## Grafana Agent / Alloy

Collection agent for all signals.

```yaml
# agent.yaml (simplified)
metrics:
  configs:
    - name: default
      scrape_configs:
        - job_name: 'app'
          static_configs:
            - targets: ['app:8080']
      remote_write:
        - url: http://mimir:9009/api/v1/push

logs:
  configs:
    - name: default
      clients:
        - url: http://loki:3100/loki/api/v1/push
      positions:
        filename: /tmp/positions.yaml
      scrape_configs:
        - job_name: containers
          docker_sd_configs:
            - host: unix:///var/run/docker.sock

traces:
  configs:
    - name: default
      receivers:
        otlp:
          protocols:
            grpc:
      remote_write:
        - endpoint: tempo:4317
```

---

## Best Practices

### Dashboard Design

| Practice | Why |
|----------|-----|
| Use variables | Reusable across environments |
| Group related panels | Easier navigation |
| Add descriptions | Self-documenting |
| Set thresholds | Visual status |
| Use consistent units | No confusion |
| Link related dashboards | Navigation |

### Performance

| Practice | Why |
|----------|-----|
| Limit time range | Less data to query |
| Use recording rules | Pre-computed queries |
| Avoid regex when possible | Expensive |
| Set refresh intervals | Don't over-query |

### Organization

```
Folders/
├── Infrastructure/
│   ├── Node Metrics
│   └── Kubernetes
├── Applications/
│   ├── API Service
│   └── Worker Service
└── Business/
    ├── Revenue
    └── User Activity
```

---

## Deployment Options

| Option | Best For |
|--------|----------|
| Docker/Podman | Local, simple |
| Kubernetes (Helm) | Production |
| Grafana Cloud | Managed, SaaS |
| Grafana Enterprise | On-prem, features |

### Docker Compose (Local)

```yaml
services:
  grafana:
    image: grafana/grafana:latest
    ports:
      - "3000:3000"
    volumes:
      - grafana-data:/var/lib/grafana
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin

  prometheus:
    image: prom/prometheus:latest
    ports:
      - "9090:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml

  loki:
    image: grafana/loki:latest
    ports:
      - "3100:3100"
```

---

## Grafana vs Alternatives

| Aspect | Grafana | Datadog | New Relic |
|--------|---------|---------|-----------|
| Cost | Free (OSS) | Per host/metric | Per host |
| Flexibility | High | Medium | Medium |
| Ease of use | Medium | Easy | Easy |
| Data sources | 100+ | Limited | Limited |
| Self-hosted | Yes | No | No |
| APM | Via Tempo | Built-in | Built-in |

---

## Related

- [[Prometheus]]
- [[OpenTelemetry]]
- [[Distributed Tracing]]
- [[Logging Libraries]]
