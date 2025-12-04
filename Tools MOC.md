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
- [[Deployment]] — Containers, serverless, platforms

### Shells & CLI

- [[Shells]] — Bash vs Zsh vs Fish vs PowerShell
- [[Terminal Emulators]] — Ghostty, Alacritty, Kitty, Warp, iTerm2
- [[WSL]] — Windows Subsystem for Linux

### Runtimes

- [[JavaScript Runtimes]] — Node.js vs Deno vs Bun
- [[WebAssembly Runtimes]] — Wasmtime, Wasmer, WasmEdge

### Containers

- [[Container Runtimes]] — Docker, Podman, Colima, Rancher Desktop

### Infrastructure & Security

- [[API Gateways]] — Traffic routing, rate limiting, auth
- [[Load Balancing]] — Traffic distribution, high availability
- [[Web Security]] — WAF, DDoS protection, CAPTCHA

### Data

- [[Database Engines]] — SQL, document, KV, graph, time-series, vector
- [[File Storage]] — Object, block, file, distributed storage
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
