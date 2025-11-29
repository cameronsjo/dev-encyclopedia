---
title: Prometheus
aliases:
  - Prom
tags:
  - tool
  - observability
  - metrics
  - monitoring
  - cncf
type: reference
status: complete
created: 2025-11-28
---

# Prometheus

Open-source metrics monitoring and alerting system.

## Overview

| Aspect | Details |
|--------|---------|
| Type | Time-series metrics database |
| Model | Pull-based scraping |
| Query Language | PromQL |
| Storage | Local TSDB |
| License | Apache 2.0 |
| Backing | CNCF (graduated) |

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Prometheus Server                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │  Retrieval  │  │    TSDB     │  │    HTTP Server      │  │
│  │  (Scraping) │  │  (Storage)  │  │  (PromQL, API)      │  │
│  └──────┬──────┘  └─────────────┘  └──────────┬──────────┘  │
└─────────┼──────────────────────────────────────┼────────────┘
          │ scrape                               │ query
          ▼                                      ▼
┌─────────────────┐                    ┌─────────────────┐
│   Targets       │                    │    Grafana      │
│  (exporters,    │                    │   Alertmanager  │
│   apps, etc.)   │                    │                 │
└─────────────────┘                    └─────────────────┘
```

### Pull vs Push

| Pull (Prometheus) | Push (StatsD, etc.) |
|-------------------|---------------------|
| Prometheus scrapes targets | Targets push to collector |
| Easier to debug (hit /metrics) | Firewall-friendly |
| Service discovery | No discovery needed |
| Targets must be reachable | Works behind NAT |

---

## Metric Types

### Counter

Cumulative value that only goes up (resets on restart).

```
# HELP http_requests_total Total HTTP requests
# TYPE http_requests_total counter
http_requests_total{method="GET", path="/api"} 12345
```

**Use for:** Request counts, errors, completed tasks.

### Gauge

Value that can go up and down.

```
# HELP temperature_celsius Current temperature
# TYPE temperature_celsius gauge
temperature_celsius{location="server_room"} 23.5
```

**Use for:** Temperature, memory usage, queue size.

### Histogram

Observations in configurable buckets.

```
# HELP http_request_duration_seconds Request latency
# TYPE http_request_duration_seconds histogram
http_request_duration_seconds_bucket{le="0.1"} 24054
http_request_duration_seconds_bucket{le="0.5"} 33444
http_request_duration_seconds_bucket{le="1"} 34055
http_request_duration_seconds_bucket{le="+Inf"} 34298
http_request_duration_seconds_sum 53423.5
http_request_duration_seconds_count 34298
```

**Use for:** Request latency, response sizes.

### Summary

Similar to histogram but calculates quantiles client-side.

```
http_request_duration_seconds{quantile="0.5"} 0.052
http_request_duration_seconds{quantile="0.9"} 0.089
http_request_duration_seconds{quantile="0.99"} 0.145
```

**Histogram vs Summary:** Prefer histogram. Aggregatable across instances.

---

## PromQL

### Basic Queries

```promql
# Instant vector - current value
http_requests_total

# With label filter
http_requests_total{method="GET"}

# Regex match
http_requests_total{path=~"/api/.*"}

# Negative match
http_requests_total{status!="200"}
```

### Range Vectors

```promql
# Last 5 minutes of data
http_requests_total[5m]

# Rate of increase per second
rate(http_requests_total[5m])

# Increase over time period
increase(http_requests_total[1h])
```

### Aggregations

```promql
# Sum across all instances
sum(http_requests_total)

# Sum by label
sum by (method) (http_requests_total)

# Average
avg(node_cpu_seconds_total)

# Top 5
topk(5, http_requests_total)
```

### Common Patterns

```promql
# Request rate per second
rate(http_requests_total[5m])

# Error rate percentage
sum(rate(http_requests_total{status=~"5.."}[5m]))
/ sum(rate(http_requests_total[5m])) * 100

# 95th percentile latency (histogram)
histogram_quantile(0.95,
  sum(rate(http_request_duration_seconds_bucket[5m])) by (le)
)

# Memory usage percentage
(node_memory_MemTotal_bytes - node_memory_MemAvailable_bytes)
/ node_memory_MemTotal_bytes * 100
```

---

## Configuration

### prometheus.yml

```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

alerting:
  alertmanagers:
    - static_configs:
        - targets: ['alertmanager:9093']

rule_files:
  - 'alerts/*.yml'

scrape_configs:
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']

  - job_name: 'node'
    static_configs:
      - targets: ['node-exporter:9100']

  - job_name: 'kubernetes-pods'
    kubernetes_sd_configs:
      - role: pod
    relabel_configs:
      - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_scrape]
        action: keep
        regex: true
```

### Service Discovery

| Type | Use Case |
|------|----------|
| static_configs | Fixed targets |
| kubernetes_sd_configs | Kubernetes pods/services |
| ec2_sd_configs | AWS EC2 instances |
| consul_sd_configs | Consul services |
| file_sd_configs | JSON/YAML files |
| dns_sd_configs | DNS SRV records |

---

## Exporters

Expose metrics in Prometheus format.

### Common Exporters

| Exporter | Metrics |
|----------|---------|
| node_exporter | Linux host metrics |
| windows_exporter | Windows metrics |
| blackbox_exporter | Probe endpoints (HTTP, TCP, ICMP) |
| mysqld_exporter | MySQL metrics |
| postgres_exporter | PostgreSQL metrics |
| redis_exporter | Redis metrics |
| nginx_exporter | NGINX metrics |
| cadvisor | Container metrics |

### Custom Metrics (Application)

```python
# Python with prometheus_client
from prometheus_client import Counter, Histogram, start_http_server

REQUEST_COUNT = Counter('app_requests_total', 'Total requests', ['method', 'endpoint'])
REQUEST_LATENCY = Histogram('app_request_latency_seconds', 'Request latency')

@REQUEST_LATENCY.time()
def handle_request():
    REQUEST_COUNT.labels(method='GET', endpoint='/api').inc()
    # ... handle request
```

```go
// Go with prometheus/client_golang
var requestCount = prometheus.NewCounterVec(
    prometheus.CounterOpts{
        Name: "app_requests_total",
        Help: "Total requests",
    },
    []string{"method", "endpoint"},
)

func handleRequest(w http.ResponseWriter, r *http.Request) {
    requestCount.WithLabelValues(r.Method, r.URL.Path).Inc()
    // ...
}
```

---

## Alerting

### Alert Rules

```yaml
# alerts/app.yml
groups:
  - name: app
    rules:
      - alert: HighErrorRate
        expr: |
          sum(rate(http_requests_total{status=~"5.."}[5m]))
          / sum(rate(http_requests_total[5m])) > 0.05
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "High error rate detected"
          description: "Error rate is {{ $value | humanizePercentage }}"

      - alert: HighLatency
        expr: |
          histogram_quantile(0.95,
            sum(rate(http_request_duration_seconds_bucket[5m])) by (le)
          ) > 0.5
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "High latency detected"
```

### Alertmanager

Routes alerts to receivers.

```yaml
# alertmanager.yml
route:
  receiver: 'slack-notifications'
  group_by: ['alertname', 'severity']
  group_wait: 30s
  group_interval: 5m
  repeat_interval: 4h
  routes:
    - match:
        severity: critical
      receiver: 'pagerduty'

receivers:
  - name: 'slack-notifications'
    slack_configs:
      - api_url: 'https://hooks.slack.com/...'
        channel: '#alerts'

  - name: 'pagerduty'
    pagerduty_configs:
      - service_key: '...'
```

---

## Storage & Scaling

### Local Storage

- Default: 15 days retention
- Not designed for long-term
- Single node

### Remote Storage

For long-term and HA:

| Solution | Type |
|----------|------|
| Thanos | Sidecar + object storage |
| Cortex | Horizontally scalable |
| VictoriaMetrics | Drop-in replacement, better perf |
| Mimir | Grafana's Cortex fork |
| M3 | Uber's solution |

### Federation

Pull aggregated metrics from other Prometheus servers.

```yaml
scrape_configs:
  - job_name: 'federate'
    honor_labels: true
    metrics_path: '/federate'
    params:
      'match[]':
        - '{job=~".+"}'
    static_configs:
      - targets:
          - 'prometheus-1:9090'
          - 'prometheus-2:9090'
```

---

## Best Practices

### Naming Conventions

```
# Format: namespace_subsystem_name_unit
myapp_http_requests_total
myapp_http_request_duration_seconds
myapp_database_connections_current
```

| Suffix | Meaning |
|--------|---------|
| _total | Counter |
| _seconds | Duration |
| _bytes | Size |
| _info | Metadata (gauge of 1) |

### Label Cardinality

**Danger:** High cardinality kills Prometheus.

```promql
# BAD - user_id has millions of values
http_requests_total{user_id="..."}

# GOOD - bounded values
http_requests_total{method="GET", status="200"}
```

### Recording Rules

Pre-compute expensive queries.

```yaml
groups:
  - name: example
    rules:
      - record: job:http_requests:rate5m
        expr: sum by (job) (rate(http_requests_total[5m]))
```

---

## Prometheus Ecosystem

```
┌─────────────────────────────────────────────────────────────┐
│                    Prometheus Ecosystem                      │
├─────────────────────────────────────────────────────────────┤
│  Collection    │  Storage      │  Visualization │  Alerting │
├────────────────┼───────────────┼────────────────┼───────────┤
│  Prometheus    │  Prometheus   │  Grafana       │  Alertmgr │
│  Exporters     │  Thanos       │  Console       │  PagerDuty│
│  Pushgateway   │  Cortex/Mimir │                │  Slack    │
│  OpenTelemetry │  VictoriaM.   │                │  Opsgenie │
└────────────────┴───────────────┴────────────────┴───────────┘
```

---

## Related

- [[Grafana]]
- [[OpenTelemetry]]
- [[Database Engines]]
- [[Distributed Tracing]]
