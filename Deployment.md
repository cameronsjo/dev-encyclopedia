---
title: Deployment
aliases:
  - Deployment Strategies
  - Production
tags:
  - comparison
  - tools
  - deployment
  - devops
  - containers
  - serverless
type: comparison
status: complete
created: '2025-11-28'
---

# Deployment

Strategies for deploying applications to production environments.

## Deployment Models

| Model | What | Best For |
|-------|------|----------|
| **Bare metal** | Physical servers | Maximum control, specific hardware |
| **VMs** | Virtual machines | Traditional infra, isolation |
| **Containers** | Docker, Podman | Consistency, microservices |
| **Kubernetes** | Container orchestration | Scale, self-healing |
| **Serverless** | Functions as a Service | Event-driven, variable load |
| **PaaS** | Managed platforms | Simple apps, rapid deployment |
| **Edge** | CDN edge locations | Low latency, global |

---

## Containers

### Why Containers?

- **Consistency:** Same image runs everywhere
- **Isolation:** Dependencies don't conflict
- **Efficiency:** Share kernel, lighter than VMs
- **Portability:** Build once, deploy anywhere

### Dockerfile Best Practices

| Practice | Why |
|----------|-----|
| Multi-stage builds | Smaller final images |
| Minimize layers | Smaller images, faster pulls |
| Use specific tags | Reproducible builds |
| Non-root user | Security |
| .dockerignore | Exclude unnecessary files |
| Order by change frequency | Layer caching |

### Base Images by Language

| Language | Production Image | Size |
|----------|-----------------|------|
| Go | `scratch` or `distroless` | ~5-15 MB |
| Rust | `scratch` or `distroless` | ~5-15 MB |
| C# (AOT) | `mcr.microsoft.com/dotnet/runtime-deps` | ~15-30 MB |
| C# (JIT) | `mcr.microsoft.com/dotnet/aspnet` | ~100+ MB |
| Python | `python:3.x-slim` | ~50-100 MB |
| Node.js | `node:xx-alpine` | ~50-100 MB |

**`scratch`:** Empty base. Only for fully static binaries.

**`distroless`:** Google's minimal images. No shell, just runtime.

### Multi-Stage Example Pattern

```dockerfile
# Build stage
FROM golang:1.22 AS builder
WORKDIR /app
COPY . .
RUN go build -o myapp

# Production stage
FROM gcr.io/distroless/static
COPY --from=builder /app/myapp /
CMD ["/myapp"]
```

Compiles in full environment, runs in minimal image.

---

## Kubernetes

### When to Use K8s

**Use when:**

- Multiple services to orchestrate
- Need auto-scaling
- Want self-healing
- Running on multiple clouds
- Team has K8s expertise

**Don't use when:**

- Single service
- Small team
- Simple deployment needs
- Serverless fits better

### Key Concepts

| Concept | Purpose |
|---------|---------|
| Pod | Smallest deployable unit |
| Deployment | Manages pod replicas |
| Service | Network access to pods |
| Ingress | External HTTP routing |
| ConfigMap | Configuration data |
| Secret | Sensitive data |
| HPA | Horizontal pod autoscaling |

### Deployment Strategies

| Strategy | How | Use Case |
|----------|-----|----------|
| Rolling update | Replace pods gradually | Default, zero downtime |
| Blue-green | Switch traffic all at once | Quick rollback |
| Canary | Route % to new version | Gradual validation |

---

## Serverless

### What It Means

- No server management
- Pay per execution
- Auto-scales to zero
- Event-driven

### Platforms

| Platform | Provider | Cold Start |
|----------|----------|------------|
| Lambda | AWS | 100ms-1s+ |
| Cloud Functions | GCP | 100ms-500ms |
| Azure Functions | Azure | 100ms-1s+ |
| Cloudflare Workers | Cloudflare | < 5ms (V8 isolates) |
| Vercel Functions | Vercel | Varies |
| Deno Deploy | Deno | < 10ms |

### Cold Starts

**Problem:** First request to idle function is slow.

**Mitigations:**

| Strategy | Trade-off |
|----------|-----------|
| Provisioned concurrency | Costs money, always warm |
| Keep-warm pings | Hacky, still has limits |
| Faster runtimes | Use Go, Rust over Python, Node |
| Edge runtimes | V8 isolates start fast |

### When Serverless Fits

| Good Fit | Poor Fit |
|----------|----------|
| Variable/spiky load | Consistent high load |
| Event processing | Long-running processes |
| API endpoints | WebSocket servers |
| Scheduled tasks | Stateful applications |
| Prototypes | Sub-10ms latency requirements |

---

## Platform as a Service (PaaS)

### Options

| Platform | Strengths |
|----------|-----------|
| Heroku | Original PaaS, easy |
| Railway | Modern Heroku alternative |
| Render | Good free tier |
| Fly.io | Global edge deployment |
| Vercel | Frontend + serverless |
| Netlify | JAMstack, frontend |

### When PaaS Fits

- Small teams
- MVPs and prototypes
- Don't want to manage infrastructure
- Standard app patterns
- Cost at scale is acceptable

### When PaaS Doesn't Fit

- Complex infrastructure needs
- Cost optimization critical
- Specific compliance requirements
- Need full control

---

## CI/CD Integration

### Deployment Pipeline

```mermaid
graph LR
    A[Push] --> B[Build]
    B --> C[Test]
    C --> D[Security Scan]
    D --> E[Build Image]
    E --> F[Push Registry]
    F --> G[Deploy Staging]
    G --> H[Integration Tests]
    H --> I[Deploy Production]
```

### Key Practices

| Practice | Why |
|----------|-----|
| Immutable deployments | Rollback = deploy old image |
| Environment parity | Staging mirrors production |
| Feature flags | Deploy != release |
| Health checks | Detect failed deployments |
| Automated rollback | On failed health checks |
| Gradual rollout | Canary or % traffic |

---

## Configuration Management

### 12-Factor App Principles

| Principle | What |
|-----------|------|
| Config in environment | No hardcoded values |
| Logs to stdout | Let platform handle aggregation |
| Stateless processes | Scale horizontally |
| Port binding | Self-contained services |
| Disposability | Fast startup, graceful shutdown |

### Config Sources

| Source | Use Case |
|--------|----------|
| Environment variables | Simple config, secrets |
| Config files | Complex structured config |
| Secret managers | Vault, AWS Secrets Manager |
| ConfigMaps (K8s) | Kubernetes-native |

---

## Zero-Downtime Deployment

### Requirements

1. **Rolling updates:** Replace instances gradually
2. **Health checks:** Only route to healthy instances
3. **Graceful shutdown:** Drain connections before exit
4. **Database migrations:** Backward-compatible changes
5. **Session handling:** External session store

### Database Migration Strategy

| Approach | How |
|----------|-----|
| Expand/contract | Add new schema, migrate data, remove old |
| Feature flags | Toggle between old/new code paths |
| Shadow writes | Write to both schemas during transition |

---

## Observability in Production

| What to Monitor | Why |
|-----------------|-----|
| Request latency (p50, p95, p99) | User experience |
| Error rates | Reliability |
| Resource usage (CPU, memory) | Capacity planning |
| Business metrics | Actual impact |

**Essential stack:**

- Logs → Aggregator (Loki, ELK, CloudWatch)
- Traces → Jaeger, Tempo, vendor APM
- Metrics → Prometheus + Grafana

---

## Decision Matrix

| Scenario | Recommendation |
|----------|----------------|
| Single app, small team | PaaS (Railway, Render) |
| Multiple services | Kubernetes or managed containers |
| Event-driven, variable load | Serverless |
| Global low latency | Edge (Cloudflare Workers, Fly.io) |
| Maximum control | VMs or bare metal |
| Cost-sensitive at scale | Kubernetes with spot instances |

---

## Related

- [[Build Systems]]
- [[Cross-Compilation]]
- [[Runtimes]]
- [[OpenTelemetry]]
- [[Logging Libraries]]
