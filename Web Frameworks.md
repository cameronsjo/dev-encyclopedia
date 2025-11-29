---
title: Web Frameworks
aliases:
  - Backend Frameworks
  - API Frameworks
tags:
  - comparison
  - languages
  - tools
  - web
  - api
  - backend
  - csharp
  - go
  - python
  - typescript
  - rust
type: comparison
status: complete
created: 2025-11-28
---

# Web Frameworks

Cross-language comparison of backend web frameworks for APIs and web applications.

## Overview

| Language | Framework | Style | Use Case |
|----------|-----------|-------|----------|
| C# | ASP.NET Core | Full-featured | Enterprise APIs, full-stack |
| C# | Minimal APIs | Lightweight | Simple APIs |
| Go | net/http | Stdlib | Any |
| Go | Gin | Fast router | APIs |
| Go | Echo | Feature-rich | APIs with middleware |
| Python | FastAPI | Modern async | APIs |
| Python | Django | Batteries-included | Full-stack apps |
| Python | Flask | Minimal | Simple APIs |
| TypeScript | Express | Minimal | APIs, middleware |
| TypeScript | Fastify | Performance | APIs |
| TypeScript | NestJS | Enterprise | Large apps, DI |
| TypeScript | Hono | Edge-first | Serverless, Cloudflare |
| Rust | Axum | Tokio ecosystem | APIs |
| Rust | Actix-web | Actor model | High-performance |

---

## Framework Philosophies

### Batteries-Included vs Minimal

| Approach | Examples | Trade-offs |
|----------|----------|------------|
| **Batteries-included** | Django, ASP.NET Core, NestJS | Opinionated, faster start, heavier |
| **Minimal** | Flask, Express, Gin | Flexible, assemble yourself |

**Batteries-included gives you:**
- ORM/database integration
- Authentication/authorization
- Admin interfaces
- Form handling
- Templating

**Minimal gives you:**
- Routing
- Request/response handling
- Middleware
- You add the rest

### Sync vs Async

| Model | Frameworks | Trade-off |
|-------|------------|-----------|
| **Sync** | Django, Flask, Express | Simpler, but blocks on I/O |
| **Async** | FastAPI, Fastify, Axum | Higher concurrency, more complex |

**When async matters:** High I/O (database, external APIs, file systems). Less important for CPU-bound work.

---

## Language-Specific Notes

### C# — ASP.NET Core

The most mature enterprise framework. Excellent performance, full feature set.

**Minimal APIs vs Controllers:**

| Style | When to Use |
|-------|-------------|
| Minimal APIs | Simple APIs, microservices, quick prototypes |
| Controllers | Large apps, complex routing, full MVC |

**Key features:**
- Dependency injection built-in
- Middleware pipeline
- Model binding and validation
- OpenAPI/Swagger generation
- SignalR for real-time

**Performance:** Among the fastest. Competes with Go and Rust in benchmarks.

### Go — Standard Library is Enough

Go's `net/http` is production-ready. Many teams use it directly.

**When to add a framework:**

| Need | Solution |
|------|----------|
| Faster routing | Gin, Echo, Chi |
| Middleware ecosystem | Echo |
| Stay minimal | Chi (just a router) |

**Gin vs Echo:**
- Gin: Fastest, minimal API
- Echo: More features (binding, validation), slightly slower

**Key insight:** Go frameworks are thin. Most add routing and middleware only.

### Python — FastAPI for APIs, Django for Apps

**FastAPI:**
- Modern, async-first
- Type hints drive validation and docs
- Automatic OpenAPI generation
- Pydantic for data validation

**Django:**
- ORM, admin, auth, forms
- Sync by default (async improving)
- Best for full web apps with databases

**Flask:**
- Minimal, sync
- Good for simple APIs
- Less relevant now that FastAPI exists

**Recommendation:** FastAPI for APIs, Django for traditional web apps.

### TypeScript — Fragmented but Powerful

**Express:** The default. Minimal, huge ecosystem, but showing age.

**Fastify:** Express successor. Faster, better TypeScript support, schema validation.

**NestJS:** Angular-inspired. DI, modules, decorators. Best for large teams.

**Hono:** Edge-first. Works on Cloudflare Workers, Deno, Bun. Tiny, fast.

| Framework | Best For |
|-----------|----------|
| Express | Legacy, huge middleware ecosystem |
| Fastify | Performance-focused APIs |
| NestJS | Enterprise, large teams, structure |
| Hono | Edge/serverless, multi-runtime |

### Rust — Axum or Actix

**Axum:**
- Built on Tokio + Tower
- Composable, type-safe extractors
- Growing ecosystem
- Recommended for new projects

**Actix-web:**
- Actor model
- Extremely fast
- More complex

**Recommendation:** Axum unless you need actor model or max performance.

---

## Feature Comparison

| Feature | ASP.NET | Gin | FastAPI | Fastify | Axum |
|---------|---------|-----|---------|---------|------|
| Routing | ✅ | ✅ | ✅ | ✅ | ✅ |
| Middleware | ✅ | ✅ | ✅ Depends | ✅ Hooks | ✅ Tower |
| Validation | ✅ | ✅ binding | ✅ Pydantic | ✅ JSON Schema | ✅ extractors |
| OpenAPI | ✅ Swashbuckle | ✅ swag | ✅ Native | ✅ Plugin | ✅ utoipa |
| WebSockets | ✅ SignalR | ✅ gorilla | ✅ | ✅ | ✅ |
| Auth | ✅ Identity | Manual | ✅ OAuth2 | ✅ Plugin | Manual |
| ORM | ✅ EF Core | Manual | Manual | Manual | Manual |

---

## Performance Tiers

Based on TechEmpower benchmarks (rough ordering):

| Tier | Frameworks |
|------|------------|
| **Fastest** | Actix-web, Drogon (C++), may-minihttp |
| **Very Fast** | Axum, ASP.NET Core, Gin, Fastify |
| **Fast** | Echo, Express, FastAPI |
| **Moderate** | Django, Flask, NestJS |

**Reality check:** Framework performance rarely matters. Database queries, external APIs, and business logic dominate latency.

---

## Choosing a Framework

### By Team Size

| Size | Recommendation |
|------|----------------|
| Solo / small | Minimal (Gin, FastAPI, Express) |
| Medium | Feature-rich (ASP.NET Core, NestJS) |
| Large enterprise | Batteries-included (ASP.NET Core, Django, NestJS) |

### By Use Case

| Use Case | Recommendation |
|----------|----------------|
| REST API | FastAPI, ASP.NET Core Minimal, Gin |
| GraphQL | NestJS, Strawberry (Python), async-graphql (Rust) |
| Full-stack web app | Django, ASP.NET Core MVC |
| Microservices | Go (stdlib), FastAPI, ASP.NET Core Minimal |
| Serverless/Edge | Hono, ASP.NET Core (isolated) |
| Real-time | ASP.NET Core SignalR, Socket.io (Node) |

### By Existing Skills

| Background | Natural Fit |
|------------|-------------|
| .NET developer | ASP.NET Core |
| Systems programmer | Go stdlib, Rust Axum |
| Data scientist | FastAPI |
| Frontend developer | NestJS, Express |

---

## Anti-Patterns

| Anti-Pattern | Problem | Fix |
|--------------|---------|-----|
| Blocking in async | Defeats concurrency benefits | Use async DB drivers |
| Fat controllers | Hard to test, maintain | Extract services |
| No input validation | Security vulnerabilities | Validate at boundaries |
| Catching all exceptions | Hides bugs | Let unexpected errors propagate |
| Manual auth implementation | Security risks | Use framework auth |

---

## Related

- [[HTTP Clients]]
- [[ORMs & Database Access]]
- [[Testing Frameworks]]
- [[OpenTelemetry]]
- [[Deployment]]
