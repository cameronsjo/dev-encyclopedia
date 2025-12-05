---
title: Distributed Tracing
aliases:
  - Tracing
  - APM Tracing
tags:
  - tool
  - comparison
  - observability
  - tracing
type: comparison
status: complete
created: "2025-12-04"
---

# Distributed Tracing

Tools for tracking requests across distributed systems.

## Overview

| Tool | Type | License | Storage | Best For |
|------|------|---------|---------|----------|
| Jaeger | Tracing backend | OSS (Apache 2) | Cassandra, ES, Memory | CNCF standard |
| Zipkin | Tracing backend | OSS (Apache 2) | Cassandra, ES, MySQL | Simple setup |
| Tempo | Tracing backend | OSS (AGPLv3) | Object storage | Grafana users |
| SigNoz | Full observability | OSS (MIT) | ClickHouse | All-in-one OSS |
| AWS X-Ray | Managed tracing | Commercial | AWS | AWS workloads |
| Datadog APM | Managed APM | Commercial | Datadog | Full APM |

---

## Tracing Concepts

### Trace Structure

```
Trace: A complete request journey
├── trace_id: unique identifier for entire request
│
├── Span: API Gateway
│   ├── span_id: abc001
│   ├── parent_span_id: null (root)
│   ├── operation: "HTTP GET /users"
│   ├── duration: 150ms
│   ├── tags: {http.method: GET, http.status: 200}
│   │
│   ├── Span: Auth Service
│   │   ├── span_id: abc002
│   │   ├── parent_span_id: abc001
│   │   ├── operation: "validate_token"
│   │   └── duration: 20ms
│   │
│   └── Span: User Service
│       ├── span_id: abc003
│       ├── parent_span_id: abc001
│       ├── operation: "get_user"
│       ├── duration: 100ms
│       │
│       ├── Span: Cache Lookup
│       │   ├── span_id: abc004
│       │   ├── parent_span_id: abc003
│       │   └── duration: 5ms (cache miss)
│       │
│       └── Span: Database Query
│           ├── span_id: abc005
│           ├── parent_span_id: abc003
│           └── duration: 80ms
```

### Context Propagation

```
Service A                    Service B                    Service C
    │                            │                            │
    │  traceparent: 00-xxx-yyy   │  traceparent: 00-xxx-zzz   │
    │ ─────────────────────────▶ │ ─────────────────────────▶ │
    │                            │                            │
    │  W3C Trace Context or      │  Same trace_id,            │
    │  B3 Headers                │  new span_id               │
```

### Propagation Formats

| Format | Header Example | Used By |
|--------|----------------|---------|
| W3C Trace Context | `traceparent: 00-{trace_id}-{span_id}-{flags}` | OpenTelemetry (default) |
| B3 Single | `b3: {trace_id}-{span_id}-{sampled}` | Zipkin |
| B3 Multi | `X-B3-TraceId`, `X-B3-SpanId`, etc. | Zipkin (legacy) |
| Jaeger | `uber-trace-id: {trace_id}:{span_id}:{parent}:{flags}` | Jaeger |

---

## Jaeger

CNCF graduated project. Production-ready distributed tracing.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Applications                            │
│              (instrumented with OpenTelemetry)               │
└──────────────────────────┬──────────────────────────────────┘
                           │ OTLP / Jaeger protocol
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                    Jaeger Collector                          │
│              (receives, validates, transforms)               │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                       Storage                                │
│     Cassandra │ Elasticsearch │ Kafka │ Badger │ Memory     │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                     Jaeger Query                             │
│                    (UI and API)                              │
└─────────────────────────────────────────────────────────────┘
```

### All-in-One Deployment

```yaml
# docker-compose.yaml
services:
  jaeger:
    image: jaegertracing/all-in-one:latest
    ports:
      - "16686:16686"   # UI
      - "4317:4317"     # OTLP gRPC
      - "4318:4318"     # OTLP HTTP
      - "14268:14268"   # Jaeger HTTP
      - "6831:6831/udp" # Jaeger compact (legacy)
    environment:
      - COLLECTOR_OTLP_ENABLED=true
```

### Production Deployment

```yaml
# docker-compose.yaml
services:
  jaeger-collector:
    image: jaegertracing/jaeger-collector:latest
    environment:
      - SPAN_STORAGE_TYPE=elasticsearch
      - ES_SERVER_URLS=http://elasticsearch:9200
    ports:
      - "4317:4317"
      - "4318:4318"

  jaeger-query:
    image: jaegertracing/jaeger-query:latest
    environment:
      - SPAN_STORAGE_TYPE=elasticsearch
      - ES_SERVER_URLS=http://elasticsearch:9200
    ports:
      - "16686:16686"

  elasticsearch:
    image: elasticsearch:8.11.0
    environment:
      - discovery.type=single-node
      - xpack.security.enabled=false
```

### Sampling Configuration

```yaml
# sampling.json
{
  "service_strategies": [
    {
      "service": "api-gateway",
      "type": "probabilistic",
      "param": 0.5
    },
    {
      "service": "critical-service",
      "type": "const",
      "param": 1
    }
  ],
  "default_strategy": {
    "type": "probabilistic",
    "param": 0.1
  }
}
```

---

## Zipkin

Simple, mature distributed tracing system.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Applications                            │
│           (instrumented with Zipkin/OpenTelemetry)           │
└──────────────────────────┬──────────────────────────────────┘
                           │ HTTP POST /api/v2/spans
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                        Zipkin                                │
│   ┌───────────┐   ┌───────────┐   ┌───────────────────┐    │
│   │ Collector │──▶│  Storage  │◀──│    Query API      │    │
│   └───────────┘   └───────────┘   └─────────┬─────────┘    │
│                                              │              │
│                                              ▼              │
│                                        ┌──────────┐        │
│                                        │    UI    │        │
│                                        └──────────┘        │
└─────────────────────────────────────────────────────────────┘
```

### Deployment

```yaml
# docker-compose.yaml
services:
  zipkin:
    image: openzipkin/zipkin:latest
    ports:
      - "9411:9411"
    environment:
      - STORAGE_TYPE=elasticsearch
      - ES_HOSTS=http://elasticsearch:9200

  # Or simple in-memory
  zipkin-slim:
    image: openzipkin/zipkin-slim:latest
    ports:
      - "9411:9411"
```

### Storage Options

| Storage | Use Case |
|---------|----------|
| Memory | Development, testing |
| Elasticsearch | Production, search |
| Cassandra | High-volume production |
| MySQL | Small deployments |

---

## Tempo

Grafana's distributed tracing backend. Cost-effective at scale.

### Key Concepts

- **Object storage** — Uses S3/GCS/Azure for traces
- **No indexing** — Relies on trace IDs
- **TraceQL** — Query language
- **Grafana native** — Deep integration

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Applications                            │
│              (instrumented with OpenTelemetry)               │
└──────────────────────────┬──────────────────────────────────┘
                           │ OTLP
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                         Tempo                                │
│   ┌───────────────┐   ┌───────────────┐                     │
│   │  Distributor  │──▶│   Ingester    │                     │
│   └───────────────┘   └───────┬───────┘                     │
│                               │                              │
│                               ▼                              │
│                       ┌───────────────┐                      │
│                       │Object Storage │                      │
│                       │  (S3/GCS)     │                      │
│                       └───────┬───────┘                      │
│                               │                              │
│                               ▼                              │
│                       ┌───────────────┐                      │
│                       │    Querier    │                      │
│                       └───────────────┘                      │
└─────────────────────────────────────────────────────────────┘
```

### TraceQL Examples

```traceql
// Find traces by service
{ resource.service.name = "api" }

// Find slow spans
{ span.http.status_code >= 500 }

// Duration filter
{ span.http.url = "/api/users" } | duration > 500ms

// Structural queries
{ span.http.method = "POST" } >> { span.db.system = "postgresql" }

// Aggregate
{ resource.service.name = "api" } | count() > 100
```

### Deployment

```yaml
# docker-compose.yaml
services:
  tempo:
    image: grafana/tempo:latest
    command: ["-config.file=/etc/tempo.yaml"]
    volumes:
      - ./tempo.yaml:/etc/tempo.yaml
      - tempo-data:/var/tempo
    ports:
      - "3200:3200"   # Tempo query
      - "4317:4317"   # OTLP gRPC
      - "4318:4318"   # OTLP HTTP
```

```yaml
# tempo.yaml
server:
  http_listen_port: 3200

distributor:
  receivers:
    otlp:
      protocols:
        grpc:
        http:

storage:
  trace:
    backend: local
    local:
      path: /var/tempo/traces
    # Or for production:
    # backend: s3
    # s3:
    #   bucket: tempo-traces
    #   endpoint: s3.amazonaws.com
```

---

## OpenTelemetry Integration

All modern tracing backends support OpenTelemetry.

### SDK Setup (Node.js)

```javascript
// tracing.js
const { NodeSDK } = require('@opentelemetry/sdk-node');
const { OTLPTraceExporter } = require('@opentelemetry/exporter-trace-otlp-grpc');
const { getNodeAutoInstrumentations } = require('@opentelemetry/auto-instrumentations-node');

const sdk = new NodeSDK({
  traceExporter: new OTLPTraceExporter({
    url: 'http://jaeger:4317', // or tempo, or collector
  }),
  instrumentations: [getNodeAutoInstrumentations()],
});

sdk.start();
```

### SDK Setup (Python)

```python
# tracing.py
from opentelemetry import trace
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.trace.export import BatchSpanProcessor
from opentelemetry.exporter.otlp.proto.grpc.trace_exporter import OTLPSpanExporter
from opentelemetry.instrumentation.flask import FlaskInstrumentor

trace.set_tracer_provider(TracerProvider())
tracer = trace.get_tracer(__name__)

otlp_exporter = OTLPSpanExporter(endpoint="http://jaeger:4317")
trace.get_tracer_provider().add_span_processor(
    BatchSpanProcessor(otlp_exporter)
)

# Auto-instrument Flask
FlaskInstrumentor().instrument()
```

### SDK Setup (Go)

```go
package main

import (
    "go.opentelemetry.io/otel"
    "go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracegrpc"
    "go.opentelemetry.io/otel/sdk/trace"
)

func initTracer() (*trace.TracerProvider, error) {
    exporter, err := otlptracegrpc.New(ctx,
        otlptracegrpc.WithEndpoint("jaeger:4317"),
        otlptracegrpc.WithInsecure(),
    )
    if err != nil {
        return nil, err
    }

    tp := trace.NewTracerProvider(
        trace.WithBatcher(exporter),
    )
    otel.SetTracerProvider(tp)
    return tp, nil
}
```

---

## Sampling Strategies

### Head-Based Sampling

Decision made at trace start.

```yaml
# OpenTelemetry Collector config
processors:
  probabilistic_sampler:
    sampling_percentage: 10  # Sample 10%
```

### Tail-Based Sampling

Decision made after trace completes.

```yaml
# OpenTelemetry Collector config
processors:
  tail_sampling:
    decision_wait: 10s
    policies:
      - name: errors
        type: status_code
        status_code: {status_codes: [ERROR]}
      - name: slow
        type: latency
        latency: {threshold_ms: 1000}
      - name: default
        type: probabilistic
        probabilistic: {sampling_percentage: 5}
```

### Sampling Comparison

| Strategy | Pros | Cons |
|----------|------|------|
| Head-based | Simple, low overhead | Miss interesting traces |
| Tail-based | Keep important traces | Higher resource usage |
| Probabilistic | Predictable cost | Random, might miss errors |
| Rate-limited | Controlled volume | Might miss bursts |

---

## Comparison Matrix

| Feature | Jaeger | Zipkin | Tempo |
|---------|:------:|:------:|:-----:|
| Query by trace ID | ✅ | ✅ | ✅ |
| Search by tags | ✅ | ✅ | TraceQL |
| Service dependency graph | ✅ | ✅ | Via metrics |
| Storage backends | Many | Many | Object storage |
| UI | Built-in | Built-in | Grafana |
| Scalability | High | High | Very high |
| Cost at scale | $$$ | $$$ | $ |
| Retention policies | Manual | Manual | Built-in |
| CNCF status | Graduated | Incubating | — |

---

## Decision Guide

| Scenario | Recommendation |
|----------|----------------|
| Grafana stack | Tempo |
| CNCF standard | Jaeger |
| Simple setup | Zipkin |
| Cost-sensitive, high volume | Tempo |
| Elasticsearch already in use | Jaeger |
| AWS workloads | X-Ray or Jaeger |
| Full APM needed | Datadog or SigNoz |

---

## Related

- [[Observability Stack]]
- [[Log Aggregation]]
- [[OpenTelemetry]]
- [[Prometheus]]
- [[Grafana]]
