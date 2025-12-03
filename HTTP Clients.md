---
title: HTTP Clients
aliases:
  - REST Clients
  - API Clients
tags:
  - comparison
  - languages
  - tools
  - http
  - networking
  - csharp
  - go
  - python
  - typescript
  - rust
type: comparison
status: complete
created: '2025-11-28'
---

# HTTP Clients

Cross-language comparison of HTTP client libraries, patterns, and best practices.

## Overview

| Language | Library | Style | Async | Connection Pooling |
|----------|---------|-------|-------|-------------------|
| C# | HttpClient | Fluent | ✅ Native | ✅ via handler |
| Go | net/http | Stdlib | ❌ Sync | ✅ Built-in |
| Go | resty | Fluent | ❌ | ✅ |
| Python | httpx | Modern | ✅ Both | ✅ |
| Python | requests | Traditional | ❌ | ✅ via session |
| TypeScript | fetch | Native | ✅ | ✅ (runtime) |
| TypeScript | axios | Promise | ✅ | ❌ |
| Rust | reqwest | Async-first | ✅ | ✅ |

---

## Key Concepts

### Connection Pooling

**What:** Reuse TCP connections across multiple requests.

**Why:** TCP handshake + TLS negotiation is expensive. Pooling amortizes this cost.

**How:**

- C#: Use single `HttpClient` instance (or `IHttpClientFactory`)
- Go: Default `http.Client` pools connections
- Python: Use `httpx.Client()` context or `requests.Session()`
- TypeScript: Varies by runtime (Node's agent, browser handles it)
- Rust: `reqwest::Client` pools internally

**Anti-pattern:** Creating new client per request. Exhausts sockets, causes connection churn.

### Timeout Configuration

| Timeout Type | What It Limits |
|--------------|----------------|
| Connect | Time to establish TCP connection |
| Read | Time waiting for response data |
| Write | Time to send request body |
| Total | End-to-end request duration |

**Recommendation:** Always set timeouts. Default "infinite" will hang forever on network issues.

### Retry Strategies

When to retry:

- 5xx errors (server problems)
- Timeouts
- Connection failures

When NOT to retry:

- 4xx errors (client problems)
- Non-idempotent requests (POST without idempotency key)

**Retry patterns:**

| Pattern | Description |
|---------|-------------|
| Fixed delay | Wait N seconds between retries |
| Exponential backoff | 1s, 2s, 4s, 8s... |
| Jitter | Add randomness to avoid thundering herd |
| Circuit breaker | Stop retrying after threshold |

---

## Language-Specific Notes

### C# — HttpClient

The standard. Use `IHttpClientFactory` in DI scenarios to manage client lifecycle.

**Key points:**

- Single `HttpClient` instance is thread-safe
- `IHttpClientFactory` handles DNS changes, pooling
- `HttpRequestMessage` for fine-grained control
- Polly for retry/circuit breaker policies

**Common mistakes:**

- Instantiating per request (socket exhaustion)
- Not disposing `HttpResponseMessage` (memory leaks)
- Ignoring DNS TTL (stale endpoints)

### Go — net/http

The stdlib is production-ready. No need for third-party libraries for basic use.

**Key points:**

- Default client pools connections
- Must close response body
- Context for cancellation and timeouts
- `http.Transport` for connection tuning

**resty:** Adds fluent API, retries, but hides some control. Good for quick scripting.

### Python — httpx

Modern replacement for `requests`. Supports both sync and async.

**Key points:**

- `httpx.Client()` for connection pooling
- `httpx.AsyncClient()` for async
- HTTP/2 support built-in
- Timeout configuration is explicit

**requests:** Still widely used, but sync-only and no HTTP/2. Use httpx for new projects.

### TypeScript — fetch vs axios

**fetch:** Native in browsers and modern Node (18+). Minimal, but requires more boilerplate.

**axios:** More features (interceptors, transforms), but additional dependency.

| Feature | fetch | axios |
|---------|-------|-------|
| Native | ✅ | ❌ |
| Interceptors | ❌ | ✅ |
| Automatic JSON | ❌ | ✅ |
| Cancel requests | AbortController | CancelToken |
| Browser + Node | ✅ (Node 18+) | ✅ |

**Recommendation:** fetch for simple use, axios if you need interceptors.

### Rust — reqwest

The de facto standard. Async by default with tokio.

**Key points:**

- `reqwest::Client` pools connections
- Blocking API available via feature flag
- Good TLS support (native-tls or rustls)
- Builder pattern for configuration

---

## Feature Comparison

| Feature | HttpClient | net/http | httpx | fetch | reqwest |
|---------|------------|----------|-------|-------|---------|
| HTTP/2 | ✅ | ✅ | ✅ | Runtime | ✅ |
| Streaming | ✅ | ✅ | ✅ | ✅ | ✅ |
| Multipart | ✅ | ✅ | ✅ | ✅ | ✅ |
| Cookies | ✅ | CookieJar | ✅ | ❌ | ✅ |
| Proxy | ✅ | ✅ | ✅ | ❌ | ✅ |
| Interceptors | DelegatingHandler | RoundTripper | Event hooks | ❌ | Middleware |
| Retry built-in | ❌ (Polly) | ❌ | ❌ | ❌ | ❌ |

---

## Common Patterns

### Retry with Exponential Backoff

All languages need external logic or libraries for robust retries:

| Language | Library |
|----------|---------|
| C# | Polly |
| Go | hashicorp/go-retryablehttp |
| Python | tenacity, httpx hooks |
| TypeScript | axios-retry, custom |
| Rust | reqwest-retry, backoff |

### Request Interceptors

Add headers, logging, auth to all requests:

| Language | Mechanism |
|----------|-----------|
| C# | `DelegatingHandler` |
| Go | Custom `RoundTripper` |
| Python | httpx event hooks |
| TypeScript | axios interceptors |
| Rust | reqwest-middleware |

### Circuit Breaker

Stop calling failing services:

| Language | Library |
|----------|---------|
| C# | Polly |
| Go | sony/gobreaker |
| Python | pybreaker |
| TypeScript | opossum |
| Rust | failsafe-rs |

---

## Best Practices

| Practice | Why |
|----------|-----|
| Reuse client instances | Connection pooling |
| Set timeouts | Prevent hanging requests |
| Use exponential backoff | Avoid overwhelming failed services |
| Log request/response | Debugging |
| Handle all status codes | Don't assume success |
| Use idempotency keys | Safe retries for POST |
| Validate SSL certs | Security (don't disable in prod) |

---

## Decision Guide

| Scenario | Recommendation |
|----------|----------------|
| C# web app | `IHttpClientFactory` + Polly |
| Go CLI tool | net/http (stdlib) |
| Go with retries | go-retryablehttp |
| Python async | httpx.AsyncClient |
| Python simple scripts | httpx or requests |
| TypeScript browser | fetch |
| TypeScript Node with interceptors | axios |
| Rust | reqwest |

---

## Related

- [[Web Frameworks]]
- [[Testing Frameworks]]
- [[OpenTelemetry]]
- [[Logging Libraries]]
