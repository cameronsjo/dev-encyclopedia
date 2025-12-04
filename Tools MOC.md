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
- [[Version Managers]] — nvm, pyenv, mise, asdf, rustup
- [[Environment Management]] — direnv, dotenv, mise
- [[Remote Development]] — SSH, VS Code Remote, Codespaces

### Build & Deploy

- [[Build Systems]] — Compilers, bundlers, task runners
- [[Cross-Compilation]] — Building for multiple targets
- [[Deployment]] — Containers, serverless, platforms

### Shells & CLI

- [[Shells]] — Bash vs Zsh vs Fish vs PowerShell
- [[Terminal Emulators]] — Ghostty, Alacritty, Kitty, Warp, iTerm2
- [[Terminal Multiplexers]] — tmux vs screen vs Zellij
- [[WSL]] — Windows Subsystem for Linux

### Operating Systems

- [[Linux Distributions]] — Debian, Fedora, Arch, Alpine, NixOS
- [[Package Managers]] — apt, dnf, pacman, brew, winget
- [[Process Managers]] — systemd, launchd, supervisord, PM2

### Runtimes

- [[JavaScript Runtimes]] — Node.js vs Deno vs Bun
- [[Language Runtimes]] — Python, Ruby, JVM, .NET, PHP runtimes
- [[WebAssembly Runtimes]] — Wasmtime, Wasmer, WasmEdge

### Containers

- [[Container Runtimes]] — Docker, Podman, Colima, Rancher Desktop
- [[Container Tools]] — Portainer, Traefik, Watchtower, registries
- [[Kubernetes]] — K8s, K3s, kind, minikube, Helm

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
- [[Log Aggregation]] — Loki, Elasticsearch, Splunk, Fluentd
- [[Distributed Tracing]] — Jaeger, Zipkin, Tempo
- [[OpenTelemetry]] — Unified telemetry collection
- [[Prometheus]] — Metrics monitoring
- [[Grafana]] — Visualization and dashboards

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
