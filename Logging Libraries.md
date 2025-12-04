---
title: Logging Libraries
aliases:
  - Structured Logging
  - Observability
tags:
  - comparison
  - languages
  - tools
  - logging
  - observability
  - csharp
  - go
  - python
  - typescript
  - rust
type: comparison
status: complete
created: '2025-11-28'
---

# Logging Libraries

Cross-language comparison of logging libraries, structured logging patterns, and observability integration.

## Why Structured Logging?

Traditional text logs are human-readable but machine-hostile. Structured logging outputs logs as key-value pairs (usually JSON), enabling:

- **Searchability** — Query by field (`user_id=123`, `level=error`)
- **Aggregation** — Count errors by service, latency percentiles
- **Correlation** — Trace requests across services via trace IDs
- **Alerting** — Trigger on specific field patterns

**Text log:**

```
2024-01-15 10:23:45 ERROR Failed to process order 12345 for user john@example.com
```

**Structured log:**

```json
{"timestamp":"2024-01-15T10:23:45Z","level":"error","message":"Failed to process order","order_id":12345,"user_email":"john@example.com","service":"orders"}
```

---

## Library Overview

| Language | Library | Style | Structured | Async |
|----------|---------|-------|------------|-------|
| C# | Serilog | Fluent | ✅ Native | ✅ |
| C# | Microsoft.Extensions.Logging | DI-based | ✅ | ✅ |
| Go | slog (stdlib) | Structured-first | ✅ Native | ❌ |
| Go | zerolog | Zero-alloc | ✅ Native | ❌ |
| Go | zap | High-performance | ✅ Native | ❌ |
| Python | structlog | Processors | ✅ Native | ❌ |
| Python | loguru | Modern API | ✅ | ❌ |
| TypeScript | pino | JSON-first | ✅ Native | ✅ |
| TypeScript | winston | Traditional | ✅ | ✅ |
| Rust | tracing | Spans + events | ✅ Native | ✅ |

---

## Concepts

### Log Levels

Standard severity hierarchy (lowest to highest):

| Level | Use Case |
|-------|----------|
| TRACE | Fine-grained debugging (function entry/exit) |
| DEBUG | Diagnostic information for developers |
| INFO | Normal operation milestones |
| WARN | Unexpected but recoverable situations |
| ERROR | Failures that need attention |
| FATAL | Application cannot continue |

**Best practice:** Default to INFO in production, DEBUG in development. TRACE should be opt-in per component.

### Structured Fields

Common fields to include:

| Field | Purpose |
|-------|---------|
| `timestamp` | When (ISO 8601 UTC) |
| `level` | Severity |
| `message` | Human description |
| `service` | Which service |
| `trace_id` | Request correlation |
| `span_id` | Operation within trace |
| `user_id` | Who triggered |
| `duration_ms` | How long |
| `error` | Error details if applicable |

### Context Propagation

Automatically attach context to all logs within a scope:

- **Request ID** — Correlate all logs from a single HTTP request
- **User ID** — Track who initiated the action
- **Trace/Span** — OpenTelemetry integration for distributed tracing

---

## Language-Specific Notes

### C# — Serilog

The de facto standard. Message templates with semantic properties:

```csharp
Log.Information("Order {OrderId} processed for {UserId}", orderId, userId);
```

**Key features:**

- Sinks (outputs): Console, File, Seq, Elasticsearch, Application Insights
- Enrichers: Add properties globally (machine name, environment)
- Destructuring: Complex objects logged as structured data
- `ILogger<T>` integration with DI

**When to use:** Any C# application. Serilog + Seq is an excellent combo for searching logs.

### Go — slog (stdlib)

Added in Go 1.21, the stdlib now has structured logging built-in.

**Key features:**

- Zero dependencies
- Handler interface for custom outputs
- Attribute groups for nested structure
- Context-aware logging

**When to use:** New Go projects. For high-performance needs, zerolog (zero allocations) or zap.

**zerolog vs zap:** zerolog is simpler and faster. zap has more features (sampling, hooks). Both are excellent.

### Python — structlog

Processor-based pipeline that transforms log events:

**Key features:**

- Processors add/modify fields in chain
- Integrates with stdlib logging
- Context variables for request-scoped data
- Render to JSON, colored console, or custom

**When to use:** Production Python apps. loguru is simpler but structlog is more powerful for structured output.

### TypeScript — pino

Fastest Node.js logger, JSON by default:

**Key features:**

- JSON output (pipe to `pino-pretty` for dev)
- Child loggers with inherited context
- Async mode for high throughput
- Redaction of sensitive fields

**When to use:** Any Node.js backend. winston is more configurable but slower.

### Rust — tracing

More than logging—a full instrumentation framework:

**Key features:**

- Spans track operation duration automatically
- Events are logs within spans
- Subscribers process the data (console, JSON, OTLP)
- Async-aware (spans cross `.await` boundaries)

**When to use:** Any Rust application. The ecosystem standard. Pairs with `tracing-subscriber` for output and `tracing-opentelemetry` for distributed tracing.

---

## Feature Comparison

| Feature | Serilog | slog | structlog | pino | tracing |
|---------|---------|------|-----------|------|---------|
| Message templates | ✅ | ❌ | ❌ | ❌ | ✅ |
| Zero-alloc option | ❌ | ✅ | ❌ | ✅ | ✅ |
| Span tracking | ❌ | ❌ | ❌ | ❌ | ✅ Native |
| OpenTelemetry | Plugin | Plugin | Plugin | Plugin | ✅ Native |
| Log sampling | ✅ | ❌ | ❌ | ✅ | ✅ |
| Async writes | ✅ | ❌ | ❌ | ✅ | ✅ |
| Sensitive data redaction | Plugin | Manual | Processor | ✅ | Subscriber |

---

## Output Destinations

### Common Sinks/Handlers

| Destination | Use Case |
|-------------|----------|
| Console | Development, containerized apps (stdout to orchestrator) |
| File | Traditional apps, log rotation with logrotate |
| Seq | .NET ecosystem log aggregation (excellent UI) |
| Elasticsearch | Full-text search, Kibana dashboards |
| Loki | Prometheus-ecosystem log aggregation |
| OTLP | OpenTelemetry collector (vendor-agnostic) |
| Cloud | CloudWatch, Application Insights, Cloud Logging |

### Production Pattern

```
App → structured JSON to stdout → Container runtime → Log aggregator → Query UI
```

Don't write to files in containers. Log to stdout, let the orchestrator collect.

---

## OpenTelemetry Integration

Modern observability connects three signals:

| Signal | Purpose | Format |
|--------|---------|--------|
| Logs | Events and errors | JSON with trace context |
| Traces | Request flow across services | Spans with parent/child |
| Metrics | Aggregated measurements | Counters, histograms |

**Correlation:** Include `trace_id` and `span_id` in logs to link them to traces.

All five languages have OTLP exporters:

- C#: `OpenTelemetry.Exporter.OpenTelemetryProtocol`
- Go: `go.opentelemetry.io/otel`
- Python: `opentelemetry-sdk`
- TypeScript: `@opentelemetry/sdk-node`
- Rust: `tracing-opentelemetry`

---

## Anti-Patterns

| Anti-Pattern | Problem | Fix |
|--------------|---------|-----|
| Logging sensitive data | PII in logs = compliance violation | Redact or exclude |
| String interpolation | `$"User {user}"` loses structure | Use templates with named params |
| Logging in hot paths | Performance degradation | Sample or use async |
| No correlation IDs | Can't trace requests | Add trace_id to context |
| Excessive DEBUG in prod | Log volume explodes | Filter by level, sample |
| Catching and logging only | Error disappears | Log AND propagate |

---

## Decision Guide

| Priority | Recommendation |
|----------|----------------|
| Performance-critical | zerolog (Go), pino (TS), tracing (Rust) |
| Best DX | Serilog (C#), loguru (Python) |
| OpenTelemetry native | tracing (Rust) |
| Zero dependencies | slog (Go) |
| Most flexible | Serilog (C#), structlog (Python) |

---

## Related

- [[Terminal UI & Language Features]]
- [[Testing Frameworks]]
- [[Debugging Tools]]
- [[Deployment]]
