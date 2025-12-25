---
title: Local Observability
aliases:
  - Local Monitoring
  - Local Grafana
  - Local Prometheus
tags:
  - tool
  - observability
  - local-dev
  - monitoring
type: reference
status: complete
created: "2025-12-18"
---

# Local Observability

Running monitoring and observability tools on your development machine.

## Overview

Why run observability locally?

- Test dashboards before production
- Develop metrics/traces in your app
- Learn the tools without cloud costs
- Debug performance issues locally

## Quick Start Options

| Method | Best For | Complexity |
|--------|----------|------------|
| **Docker Compose** | Simple, standalone | Low |
| **Kubernetes (KIND/k3d)** | Production-like | Medium |
| **Binary/brew** | Single tool testing | Low |
| **All-in-one (Grafana LGTM)** | Full stack quickly | Low |

## Docker Compose Stack

### Full Observability Stack

```yaml
# docker-compose.yaml
version: "3.8"

services:
  # Metrics
  prometheus:
    image: prom/prometheus:latest
    ports:
      - "9090:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml
      - prometheus-data:/prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'

  # Visualization
  grafana:
    image: grafana/grafana:latest
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin
      - GF_USERS_ALLOW_SIGN_UP=false
    volumes:
      - grafana-data:/var/lib/grafana
      - ./grafana/provisioning:/etc/grafana/provisioning
    depends_on:
      - prometheus
      - loki

  # Logs
  loki:
    image: grafana/loki:latest
    ports:
      - "3100:3100"
    command: -config.file=/etc/loki/local-config.yaml

  # Log shipper
  promtail:
    image: grafana/promtail:latest
    volumes:
      - ./promtail.yml:/etc/promtail/config.yml
      - /var/log:/var/log:ro
    command: -config.file=/etc/promtail/config.yml

  # Traces
  tempo:
    image: grafana/tempo:latest
    ports:
      - "3200:3200"   # tempo
      - "4317:4317"   # otlp grpc
      - "4318:4318"   # otlp http
    command: -config.file=/etc/tempo/tempo.yml
    volumes:
      - ./tempo.yml:/etc/tempo/tempo.yml

volumes:
  prometheus-data:
  grafana-data:
```

### Prometheus Config

```yaml
# prometheus.yml
global:
  scrape_interval: 15s

scrape_configs:
  # Prometheus itself
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']

  # Your application
  - job_name: 'myapp'
    static_configs:
      - targets: ['host.docker.internal:8080']
    metrics_path: /metrics

  # Node exporter (if running)
  - job_name: 'node'
    static_configs:
      - targets: ['node-exporter:9100']
```

### Start Stack

```bash
docker-compose up -d

# Access:
# Grafana:    http://localhost:3000 (admin/admin)
# Prometheus: http://localhost:9090
# Loki:       http://localhost:3100
```

## Prometheus Only

### Docker

```bash
# Simple prometheus
docker run -d \
  --name prometheus \
  -p 9090:9090 \
  -v $(pwd)/prometheus.yml:/etc/prometheus/prometheus.yml \
  prom/prometheus
```

### Binary

```bash
# Download
wget https://github.com/prometheus/prometheus/releases/download/v2.47.0/prometheus-2.47.0.linux-amd64.tar.gz
tar xvf prometheus-*.tar.gz
cd prometheus-*

# Run
./prometheus --config.file=prometheus.yml

# macOS
brew install prometheus
prometheus --config.file=/usr/local/etc/prometheus.yml
```

### Test Your Metrics

```bash
# Check Prometheus is scraping
curl http://localhost:9090/api/v1/targets

# Query metrics
curl 'http://localhost:9090/api/v1/query?query=up'

# Your app metrics
curl http://localhost:8080/metrics
```

## Grafana Only

### Docker

```bash
docker run -d \
  --name grafana \
  -p 3000:3000 \
  grafana/grafana

# With persistent storage
docker run -d \
  --name grafana \
  -p 3000:3000 \
  -v grafana-storage:/var/lib/grafana \
  grafana/grafana
```

### Binary

```bash
# macOS
brew install grafana
brew services start grafana

# Linux
wget https://dl.grafana.com/oss/release/grafana-10.0.0.linux-amd64.tar.gz
tar -zxvf grafana-*.tar.gz
cd grafana-*
./bin/grafana-server
```

### Provisioning Dashboards

```yaml
# grafana/provisioning/datasources/datasources.yml
apiVersion: 1
datasources:
  - name: Prometheus
    type: prometheus
    url: http://prometheus:9090
    isDefault: true

  - name: Loki
    type: loki
    url: http://loki:3100

  - name: Tempo
    type: tempo
    url: http://tempo:3200
```

```yaml
# grafana/provisioning/dashboards/dashboards.yml
apiVersion: 1
providers:
  - name: 'default'
    folder: ''
    type: file
    options:
      path: /etc/grafana/provisioning/dashboards
```

## On Kubernetes (KIND/K3s)

### Using Helm

```bash
# Add repos
helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
helm repo add grafana https://grafana.github.io/helm-charts
helm repo update

# Install prometheus-stack (includes Grafana)
helm install monitoring prometheus-community/kube-prometheus-stack \
  --namespace monitoring \
  --create-namespace

# Port forward
kubectl port-forward -n monitoring svc/monitoring-grafana 3000:80
kubectl port-forward -n monitoring svc/monitoring-kube-prometheus-prometheus 9090:9090

# Get Grafana password
kubectl get secret -n monitoring monitoring-grafana -o jsonpath="{.data.admin-password}" | base64 -d
```

### Minimal Setup

```yaml
# prometheus-k8s.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: prometheus-config
data:
  prometheus.yml: |
    global:
      scrape_interval: 15s
    scrape_configs:
      - job_name: 'kubernetes-pods'
        kubernetes_sd_configs:
          - role: pod
        relabel_configs:
          - source_labels: [__meta_kubernetes_pod_annotation_prometheus_io_scrape]
            action: keep
            regex: true
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: prometheus
spec:
  replicas: 1
  selector:
    matchLabels:
      app: prometheus
  template:
    metadata:
      labels:
        app: prometheus
    spec:
      containers:
        - name: prometheus
          image: prom/prometheus:latest
          ports:
            - containerPort: 9090
          volumeMounts:
            - name: config
              mountPath: /etc/prometheus
      volumes:
        - name: config
          configMap:
            name: prometheus-config
---
apiVersion: v1
kind: Service
metadata:
  name: prometheus
spec:
  selector:
    app: prometheus
  ports:
    - port: 9090
```

## Grafana LGTM Stack

**All-in-one: Loki, Grafana, Tempo, Mimir**

```bash
# Single container with everything
docker run -d \
  --name lgtm \
  -p 3000:3000 \
  -p 4317:4317 \
  -p 4318:4318 \
  grafana/otel-lgtm

# Grafana at http://localhost:3000
# OTLP gRPC at localhost:4317
# OTLP HTTP at localhost:4318
```

## Application Instrumentation

### Send Metrics

```python
# Python with prometheus_client
from prometheus_client import Counter, Histogram, start_http_server

# Define metrics
REQUEST_COUNT = Counter('http_requests_total', 'Total requests', ['method', 'endpoint'])
REQUEST_LATENCY = Histogram('http_request_duration_seconds', 'Request latency')

# Use in code
REQUEST_COUNT.labels(method='GET', endpoint='/api').inc()
with REQUEST_LATENCY.time():
    # do work
    pass

# Start metrics server
start_http_server(8080)  # /metrics endpoint
```

```go
// Go with prometheus/client_golang
import (
    "github.com/prometheus/client_golang/prometheus"
    "github.com/prometheus/client_golang/prometheus/promhttp"
)

var requestCount = prometheus.NewCounterVec(
    prometheus.CounterOpts{
        Name: "http_requests_total",
    },
    []string{"method", "endpoint"},
)

func init() {
    prometheus.MustRegister(requestCount)
}

// Expose metrics
http.Handle("/metrics", promhttp.Handler())
```

### Send Traces (OpenTelemetry)

```python
# Python with OpenTelemetry
from opentelemetry import trace
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter

# Setup
provider = TracerProvider()
processor = BatchSpanProcessor(OTLPSpanExporter(endpoint="localhost:4317"))
provider.add_span_processor(processor)
trace.set_tracer_provider(provider)

# Use
tracer = trace.get_tracer(__name__)
with tracer.start_as_current_span("my-operation"):
    # do work
    pass
```

### Send Logs (to Loki)

```python
# Python with logging to Loki
import logging
import logging_loki

handler = logging_loki.LokiHandler(
    url="http://localhost:3100/loki/api/v1/push",
    tags={"application": "myapp"},
    version="1",
)
logger = logging.getLogger("myapp")
logger.addHandler(handler)

logger.info("Application started", extra={"user": "john"})
```

## Useful Dashboards

### Import Pre-built

| Dashboard | ID | For |
|-----------|-----|-----|
| Node Exporter Full | 1860 | System metrics |
| Go Metrics | 6671 | Go applications |
| JVM Micrometer | 4701 | Java/Spring |
| NGINX | 9614 | NGINX |
| PostgreSQL | 9628 | PostgreSQL |
| Kubernetes Cluster | 7249 | K8s overview |

```bash
# Import via API
curl -X POST http://admin:admin@localhost:3000/api/dashboards/import \
  -H "Content-Type: application/json" \
  -d '{"dashboard":{"id":null},"overwrite":true,"inputs":[],"folderId":0,"gnetId":1860}'
```

## Cleanup

```bash
# Docker Compose
docker-compose down -v  # -v removes volumes

# Kubernetes
helm uninstall monitoring -n monitoring
kubectl delete namespace monitoring

# Docker
docker rm -f prometheus grafana loki
docker volume rm grafana-storage
```

## Tips

| Tip | Details |
|-----|---------|
| **Persist data** | Use volumes to keep metrics across restarts |
| **Network access** | Use `host.docker.internal` to reach host apps |
| **Resource limits** | Set memory limits for Prometheus |
| **Retention** | Configure `--storage.tsdb.retention.time=15d` |
| **Provisioning** | Pre-configure dashboards via files |

## Related

- [[Prometheus]] — Prometheus in depth
- [[Grafana]] — Grafana in depth
- [[OpenTelemetry]] — Instrumentation
- [[Local Kubernetes]] — K8s locally
- [[Tools MOC]] — All tools
