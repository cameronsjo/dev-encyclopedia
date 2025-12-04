---
title: Distributed Tracing
aliases:
  - Request Tracing
  - APM
tags:
  - concept
  - observability
  - distributed-systems
  - tracing
type: concept
status: complete
difficulty: intermediate
created: '2025-11-28'
---

# Distributed Tracing

Tracking requests as they flow through distributed systems, capturing timing, dependencies, and context across service boundaries.

## The Problem

In a monolith, debugging is straightforward—stack traces show the full call path. In microservices:

- Request touches 5+ services
- Each service has separate logs
- No correlation between them
- "Where did this request slow down?" is unanswerable

**Distributed tracing solves this** by assigning a unique ID to each request and propagating it across all services.

---

## Core Concepts

### Trace

A complete request journey from start to finish.

- **Trace ID** — Unique identifier (usually 128-bit hex)
- **Contains** — All spans from all services
- **Represents** — One user action or API call

### Span

A single operation within a trace.

| Field | Purpose |
|-------|---------|
| `trace_id` | Which trace this belongs to |
| `span_id` | Unique ID for this span |
| `parent_span_id` | The span that called this one |
| `operation_name` | What this span represents |
| `start_time` | When it began |
| `duration` | How long it took |
| `status` | Success, error, etc. |
| `attributes` | Key-value metadata |
| `events` | Timestamped logs within the span |

### Span Relationships

```
[Frontend Request] ─ trace_id: abc
    └── [API Gateway] ─ span_id: 1, parent: null
        ├── [User Service] ─ span_id: 2, parent: 1
        │   └── [User DB Query] ─ span_id: 3, parent: 2
        └── [Order Service] ─ span_id: 4, parent: 1
            ├── [Inventory Check] ─ span_id: 5, parent: 4
            └── [Payment Service] ─ span_id: 6, parent: 4
```

**Root span:** No parent, represents the entry point.

**Child spans:** Operations initiated by parent. Show dependencies.

---

## Context Propagation

How trace context moves between services.

### The Challenge

Service A calls Service B over HTTP. How does B know the trace ID?

### The Solution

**Inject** context into outgoing requests (HTTP headers, message metadata).
**Extract** context from incoming requests.

**W3C Trace Context (standard):**

```
traceparent: 00-{trace_id}-{parent_span_id}-{flags}
```

Example:

```
traceparent: 00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01
```

### Propagation Protocols

| Protocol | Format | Use |
|----------|--------|-----|
| HTTP | Headers | REST APIs |
| gRPC | Metadata | gRPC calls |
| Message queues | Message attributes | Kafka, RabbitMQ |
| GraphQL | Extensions or headers | GraphQL APIs |

**Key insight:** Auto-instrumentation handles this. You rarely write propagation code manually.

---

## What to Trace

### Always Trace

| Boundary | Why |
|----------|-----|
| Incoming HTTP/gRPC | Entry points |
| Outgoing HTTP/gRPC | External calls |
| Database queries | Common bottleneck |
| Cache operations | Hit/miss visibility |
| Message queue pub/sub | Async flow |
| External API calls | Third-party latency |

### Sometimes Trace

| Operation | When |
|-----------|------|
| Internal function calls | Only if expensive |
| Loops | Only aggregate, not per-iteration |
| File I/O | If significant |

### Don't Trace

- Health checks (pollute data)
- Every line of code (noise)
- Static asset serving

---

## Attributes & Events

### Span Attributes

Metadata attached to spans for filtering and analysis.

**Semantic conventions (standard names):**

| Attribute | Example |
|-----------|---------|
| `http.method` | GET, POST |
| `http.url` | /api/users/123 |
| `http.status_code` | 200, 500 |
| `db.system` | postgresql, redis |
| `db.statement` | SELECT * FROM... (careful with PII) |
| `user.id` | 12345 |
| `error` | true |

### Span Events

Point-in-time logs within a span.

Use for:

- Exceptions (with stack trace)
- State changes
- Retry attempts
- Cache hits/misses

---

## Sampling Strategies

Recording every span is expensive. Sampling reduces volume.

| Strategy | How It Works | Trade-off |
|----------|--------------|-----------|
| **Head-based** | Decide at trace start | Cheap, but may miss errors |
| **Tail-based** | Decide after trace completes | Keeps errors, more complex |
| **Probability** | Random percentage | Simple, representative |
| **Rate-limiting** | Max N/sec | Predictable cost |
| **Error-biased** | Always keep errors | Never miss failures |

**Recommendation:**

1. Head-based probability (10-20%) for baseline
2. Tail-based at collector to capture all errors and slow traces

---

## Analyzing Traces

### What to Look For

| Question | How to Find |
|----------|-------------|
| Where is latency? | Look at span durations in waterfall |
| What failed? | Filter by error status |
| What's the critical path? | Longest sequential chain |
| Are services healthy? | Aggregate error rates |
| Is caching working? | Cache hit/miss events |

### Trace Visualization

**Waterfall view:** Shows spans as horizontal bars on timeline. Width = duration.

**Service map:** Shows which services call which. Derived from traces.

**Latency histogram:** P50, P95, P99 across traces.

---

## Distributed Tracing vs Logging

| Aspect | Tracing | Logging |
|--------|---------|---------|
| Scope | Request journey | Individual events |
| Correlation | Built-in (trace_id) | Manual |
| Structure | Hierarchical spans | Flat records |
| Aggregation | Latency percentiles, service maps | Text search |
| Cost | Higher (more data) | Lower |
| Use case | Performance, dependencies | Debugging, audit |

**Best practice:** Use both. Include `trace_id` in logs to correlate.

---

## Common Pitfalls

| Pitfall | Impact | Fix |
|---------|--------|-----|
| Missing context propagation | Broken traces | Use auto-instrumentation |
| No sampling | Massive storage costs | Sample 10-20% |
| Tracing health checks | Noise | Exclude from instrumentation |
| High-cardinality attributes | Backend explosion | Avoid unique IDs in metric tags |
| Not setting service.name | Can't identify source | Always configure |
| Async without context | Lost trace | Explicitly pass context |

---

## Related

- [[OpenTelemetry]]
- [[Logging Libraries]]
- [[Debugging Tools]]
- [[System Design]]
