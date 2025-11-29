---
title: Tools MOC
tags:
  - moc
  - tools
type: moc
created: 2025-11-28
---

# Tools MOC

Development tools, libraries, and infrastructure across languages.

## Categories

### Development

- [[Testing Frameworks]] — Unit, integration, E2E testing
- [[Logging Libraries]] — Structured logging, observability
- [[HTTP Clients]] — REST clients, request handling
- [[Debugging Tools]] — Debuggers, profilers, tracers

### Build & Deploy

- [[Build Systems]] — Compilers, bundlers, task runners
- [[Cross-Compilation]] — Building for multiple targets
- [[Runtimes]] — Execution models, VMs, interpreters
- [[Deployment]] — Containers, serverless, platforms

### Data

- [[Database Engines]] — SQL, document, KV, graph, time-series, vector
- [[ORMs & Database Access]] — Data access patterns
- [[Serialization]] — JSON, YAML, binary formats

### Observability

- [[Observability Stack]] — Metrics, logs, traces comparison
- [[OpenTelemetry]] — Unified telemetry collection
- [[Prometheus]] — Metrics monitoring
- [[Grafana]] — Visualization and dashboards
- [[Distributed Tracing]] — Request flow tracking

### UI

- [[Terminal UI & Language Features]] — CLI and TUI libraries
- [[Web Frameworks]] — Backend and full-stack

---

## All Tools Pages

```dataview
TABLE status, join(tags, ", ") as "Tags"
FROM #tools
WHERE type != "moc"
SORT file.name ASC
```

## By Language

### C\#

```dataview
LIST
FROM #tools AND #csharp
```

### Go

```dataview
LIST
FROM #tools AND #go
```

### Python

```dataview
LIST
FROM #tools AND #python
```

### TypeScript

```dataview
LIST
FROM #tools AND #typescript
```

### Rust

```dataview
LIST
FROM #tools AND #rust
```
