---
title: API Gateways
aliases:
  - API Gateway
  - API Management
  - API Proxy
tags:
  - tools
  - infrastructure
  - networking
  - api
  - security
type: reference
status: complete
created: 2025-11-30
---

# API Gateways

Centralized entry point for managing, securing, and routing API traffic.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Single entry point for API traffic |
| Core functions | Routing, auth, rate limiting, transformation |
| Deployment | Edge, service mesh, sidecar |
| Key benefit | Decouples clients from backend complexity |

---

## What is an API Gateway?

```
                         ┌─────────────────────────────────────┐
                         │           API Gateway               │
  Clients                │                                     │        Backend Services
 ─────────              │  • Authentication                   │        ────────────────
                         │  • Rate Limiting                    │
┌──────────┐            │  • Request Routing                  │       ┌────────────────┐
│ Web App  │────────────┤  • Load Balancing                   ├──────►│ User Service   │
└──────────┘            │  • Caching                          │       └────────────────┘
                         │  • Logging/Metrics                  │
┌──────────┐            │  • Request/Response Transform       │       ┌────────────────┐
│ Mobile   │────────────┤  • SSL Termination                  ├──────►│ Order Service  │
└──────────┘            │                                     │       └────────────────┘
                         │                                     │
┌──────────┐            │                                     │       ┌────────────────┐
│ Partner  │────────────┤                                     ├──────►│ Payment Service│
└──────────┘            │                                     │       └────────────────┘
                         └─────────────────────────────────────┘
```

---

## Core Capabilities

### Request Routing

Route requests to appropriate services.

```yaml
# Kong declarative config
routes:
  - name: users-route
    paths: ["/api/users"]
    service: users-service

  - name: orders-route
    paths: ["/api/orders"]
    service: orders-service
    methods: [GET, POST]
```

### Path-Based Routing

```
/api/v1/users/*     → users-service
/api/v1/orders/*    → orders-service
/api/v2/users/*     → users-service-v2
```

### Header-Based Routing

```yaml
routes:
  - name: mobile-route
    headers:
      X-Client-Type: mobile
    service: mobile-optimized-service

  - name: default-route
    service: default-service
```

---

## Authentication & Authorization

### Supported Methods

| Method | Description |
|--------|-------------|
| API Key | Simple token in header |
| JWT | Stateless token validation |
| OAuth 2.0 | Delegated authorization |
| mTLS | Mutual TLS certificates |
| OIDC | OpenID Connect |

### JWT Validation

```yaml
# Kong JWT plugin
plugins:
  - name: jwt
    config:
      claims_to_verify:
        - exp
        - nbf
      key_claim_name: iss
      secret_is_base64: false
```

### API Key Authentication

```yaml
plugins:
  - name: key-auth
    config:
      key_names: [apikey, x-api-key]
      hide_credentials: true
```

---

## Rate Limiting

Protect services from overload.

### Rate Limit Strategies

| Strategy | Description |
|----------|-------------|
| Fixed window | N requests per time window |
| Sliding window | Smoothed fixed window |
| Token bucket | Burst + sustained rate |
| Leaky bucket | Smooth output rate |

### Configuration Examples

```yaml
# Kong rate limiting
plugins:
  - name: rate-limiting
    config:
      minute: 100
      hour: 1000
      policy: redis
      redis_host: redis.example.com
```

```yaml
# NGINX rate limiting
limit_req_zone $binary_remote_addr zone=api:10m rate=10r/s;

location /api/ {
    limit_req zone=api burst=20 nodelay;
    proxy_pass http://backend;
}
```

### Rate Limit Headers

```http
HTTP/1.1 200 OK
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1640000000
```

---

## Request/Response Transformation

### Request Transformation

```yaml
# Add headers
plugins:
  - name: request-transformer
    config:
      add:
        headers:
          - X-Request-ID:$(uuid)
          - X-Forwarded-For:$(client_ip)
```

### Response Transformation

```yaml
# Modify response
plugins:
  - name: response-transformer
    config:
      remove:
        headers: [Server, X-Powered-By]
      add:
        headers:
          - X-Response-Time:$(latency)
```

### Body Transformation

```lua
-- Kong plugin: transform JSON response
function plugin:body_filter(conf)
    local body = kong.response.get_raw_body()
    local json = cjson.decode(body)

    -- Add metadata wrapper
    local wrapped = {
        data = json,
        meta = {
            timestamp = os.time(),
            version = "v1"
        }
    }

    kong.response.set_raw_body(cjson.encode(wrapped))
end
```

---

## Caching

Reduce backend load with response caching.

```yaml
plugins:
  - name: proxy-cache
    config:
      response_code: [200]
      request_method: [GET, HEAD]
      content_type: [application/json]
      cache_ttl: 300
      storage_ttl: 600
```

### Cache Keys

```
Default: method + path + query_string
Custom: + headers + body hash
```

### Cache Headers

```http
Cache-Control: public, max-age=300
ETag: "abc123"
Vary: Accept-Encoding, Authorization
```

---

## Load Balancing

Distribute traffic across backends.

### Algorithms

| Algorithm | Description |
|-----------|-------------|
| Round-robin | Rotate through servers |
| Weighted | Based on server capacity |
| Least connections | Route to least busy |
| IP hash | Sticky sessions by IP |
| Random | Random selection |

```yaml
# Kong upstream
upstreams:
  - name: users-upstream
    algorithm: round-robin
    targets:
      - target: users-1:8080
        weight: 100
      - target: users-2:8080
        weight: 100
      - target: users-3:8080
        weight: 50  # Less powerful server
```

### Health Checks

```yaml
upstreams:
  - name: api-upstream
    healthchecks:
      active:
        http_path: /health
        healthy:
          interval: 5
          successes: 2
        unhealthy:
          interval: 5
          http_failures: 3
```

---

## Gateway Solutions Comparison

### Open Source

| Gateway | Language | Strengths |
|---------|----------|-----------|
| Kong | Lua/NGINX | Extensive plugins, proven |
| APISIX | Lua/NGINX | High performance, dynamic |
| Envoy | C++ | Service mesh, gRPC |
| Traefik | Go | Kubernetes-native, auto-discovery |
| Tyk | Go | GraphQL, developer portal |
| KrakenD | Go | Ultra-fast, stateless |

### Cloud/Managed

| Service | Platform | Best For |
|---------|----------|----------|
| AWS API Gateway | AWS | Lambda, AWS services |
| Azure API Management | Azure | Enterprise, .NET |
| GCP Cloud Endpoints | GCP | GKE, Cloud Run |
| Cloudflare API Shield | Cloudflare | Edge, security |
| Apigee | Google | Enterprise API management |

---

## Kong Deep Dive

### Architecture

```
┌────────────────────────────────────────────────────────────┐
│                         Kong                                │
│                                                            │
│  ┌──────────────────────────────────────────────────────┐ │
│  │                    Control Plane                      │ │
│  │   Admin API   │   Kong Manager   │   DB (Postgres)   │ │
│  └──────────────────────────────────────────────────────┘ │
│                           │                                │
│                           ▼                                │
│  ┌──────────────────────────────────────────────────────┐ │
│  │                    Data Plane                         │ │
│  │   NGINX + OpenResty   │   Plugins   │   Lua Runtime  │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

### Plugin Architecture

```lua
-- Custom Kong plugin
local MyPlugin = {
    VERSION = "1.0.0",
    PRIORITY = 1000,
}

function MyPlugin:access(conf)
    -- Before proxying to upstream
    local api_key = kong.request.get_header("X-API-Key")
    if not api_key then
        return kong.response.exit(401, { message = "Unauthorized" })
    end
end

function MyPlugin:header_filter(conf)
    -- Modify response headers
    kong.response.add_header("X-Plugin", "MyPlugin")
end

return MyPlugin
```

### Essential Plugins

| Category | Plugins |
|----------|---------|
| Auth | key-auth, jwt, oauth2, basic-auth |
| Security | rate-limiting, ip-restriction, cors |
| Traffic | proxy-cache, request-size-limiting |
| Transform | request-transformer, response-transformer |
| Logging | file-log, http-log, datadog |
| Analytics | prometheus, statsd, zipkin |

---

## Envoy Deep Dive

### Architecture

```yaml
# Envoy config
static_resources:
  listeners:
    - name: listener_0
      address:
        socket_address:
          address: 0.0.0.0
          port_value: 8080
      filter_chains:
        - filters:
            - name: envoy.filters.network.http_connection_manager
              typed_config:
                route_config:
                  virtual_hosts:
                    - name: backend
                      domains: ["*"]
                      routes:
                        - match:
                            prefix: "/api"
                          route:
                            cluster: api_cluster

  clusters:
    - name: api_cluster
      type: STRICT_DNS
      load_assignment:
        cluster_name: api_cluster
        endpoints:
          - lb_endpoints:
              - endpoint:
                  address:
                    socket_address:
                      address: api-service
                      port_value: 80
```

### Envoy Strengths

| Feature | Description |
|---------|-------------|
| L4/L7 proxy | TCP, HTTP, gRPC |
| xDS APIs | Dynamic configuration |
| Extensibility | Wasm filters |
| Observability | Built-in tracing, metrics |
| Service mesh | Istio data plane |

---

## AWS API Gateway

### Types

| Type | Use Case |
|------|----------|
| HTTP API | Simple, low-latency |
| REST API | Full features, WAF |
| WebSocket API | Real-time |

### Lambda Integration

```yaml
# SAM template
Resources:
  ApiGateway:
    Type: AWS::Serverless::HttpApi
    Properties:
      StageName: prod

  HelloFunction:
    Type: AWS::Serverless::Function
    Properties:
      Handler: index.handler
      Runtime: nodejs18.x
      Events:
        Api:
          Type: HttpApi
          Properties:
            ApiId: !Ref ApiGateway
            Path: /hello
            Method: GET
```

### Usage Plans & API Keys

```yaml
UsagePlan:
  Type: AWS::ApiGateway::UsagePlan
  Properties:
    Quota:
      Limit: 10000
      Period: MONTH
    Throttle:
      BurstLimit: 100
      RateLimit: 50
```

---

## Gateway Patterns

### Backend for Frontend (BFF)

Separate gateways for different clients.

```
┌─────────┐     ┌────────────┐
│ Web App │────►│ Web BFF    │──┐
└─────────┘     └────────────┘  │    ┌────────────┐
                                 ├───►│ Services   │
┌─────────┐     ┌────────────┐  │    └────────────┘
│ Mobile  │────►│ Mobile BFF │──┘
└─────────┘     └────────────┘
```

### API Composition

Gateway aggregates multiple services.

```
GET /api/dashboard

Gateway calls:
  - /users/current
  - /orders/recent
  - /notifications/count

Returns: Combined response
```

### Strangler Fig

Gradually migrate from monolith.

```
Phase 1: Gateway routes all to monolith
Phase 2: New features to microservices
Phase 3: Migrate old features gradually
Phase 4: Decommission monolith
```

---

## Security Best Practices

| Practice | Implementation |
|----------|----------------|
| TLS everywhere | Terminate at gateway, mTLS to services |
| Input validation | Schema validation, size limits |
| Rate limiting | Per client, per endpoint |
| CORS | Strict origin policies |
| Security headers | HSTS, CSP, X-Frame-Options |
| API versioning | URL or header-based |
| Audit logging | All requests logged |

---

## Observability

### Metrics to Collect

```
# Gateway metrics
request_count{service, route, status}
request_latency_ms{service, route, quantile}
upstream_latency_ms{upstream}
cache_hit_rate{route}
rate_limit_exceeded{consumer}
error_rate{service, error_type}
```

### Distributed Tracing

```
Request → Gateway → Service A → Service B
   │          │          │          │
   └──────────┴──────────┴──────────┴── Trace ID: abc123
```

---

## When to Use API Gateway

**Strengths:**
- Centralized cross-cutting concerns
- Client abstraction
- Security enforcement
- Traffic management
- Observability

**Considerations:**
- Single point of failure (need HA)
- Added latency
- Complexity
- Gateway can become bottleneck

**Best for:**
- Microservices architectures
- Multi-client applications
- Public APIs
- Service mesh ingress

---

## Related

- [[Load Balancing]]
- [[Web Security]]
- [[MCP Gateway]]
- [[Agent Gateway]]
- [[Deployment]]
