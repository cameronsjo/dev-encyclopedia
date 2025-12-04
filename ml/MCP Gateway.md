---
title: MCP Gateway
aliases:
  - MCP Proxy
  - MCP Hub
  - MCP Router
tags:
  - ai
  - infrastructure
  - protocol
  - agents
  - gateway
type: reference
status: complete
created: 2025-11-30
---

# MCP Gateway

Centralized routing and management layer for Model Context Protocol servers.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Route, secure, and manage MCP server access |
| Pattern | Reverse proxy for MCP |
| Use case | Multi-tenant, enterprise, production MCP |
| Transport | HTTP/SSE, WebSocket |

---

## Why MCP Gateway?

### Direct MCP Limitations

| Challenge | Gateway Solution |
|-----------|------------------|
| Per-client config | Centralized server registry |
| No auth layer | Unified authentication |
| No rate limiting | Throttling and quotas |
| No visibility | Centralized logging/metrics |
| Scaling issues | Load balancing across instances |

### Architecture

```
                         ┌─────────────────────────────┐
                         │        MCP Gateway          │
                         │                             │
┌─────────┐              │  ┌─────────────────────┐   │
│ Claude  │──────────────┼─►│  Auth / Rate Limit  │   │
└─────────┘              │  └──────────┬──────────┘   │
                         │             │              │
┌─────────┐              │  ┌──────────▼──────────┐   │
│ Agent   │──────────────┼─►│      Router         │   │
└─────────┘              │  └──────────┬──────────┘   │
                         │             │              │
                         │  ┌──────────▼──────────┐   │
                         │  │  Load Balancer      │   │
                         │  └──────────┬──────────┘   │
                         │             │              │
                         └─────────────┼──────────────┘
                                       │
           ┌───────────────┬───────────┴───────────┬───────────────┐
           ▼               ▼                       ▼               ▼
    ┌──────────┐    ┌──────────┐           ┌──────────┐    ┌──────────┐
    │ Database │    │  Search  │           │   Git    │    │  Custom  │
    │   MCP    │    │   MCP    │           │   MCP    │    │   MCP    │
    └──────────┘    └──────────┘           └──────────┘    └──────────┘
```

---

## Core Capabilities

### Server Registry

Centralized catalog of available MCP servers.

```yaml
# gateway-config.yaml
servers:
  database:
    url: http://mcp-db:3000
    capabilities: [tools]
    auth_required: true
    rate_limit: 100/min

  search:
    url: http://mcp-search:3000
    capabilities: [tools, resources]
    auth_required: true

  filesystem:
    url: http://mcp-fs:3000
    capabilities: [tools, resources]
    allowed_clients: [internal]
```

### Request Routing

Route requests to appropriate MCP servers.

```
Client Request                    Gateway Action
────────────────────────────────────────────────────
tools/list                   →    Aggregate from all servers
tools/call (search_*)        →    Route to search MCP
resources/read (file://*)    →    Route to filesystem MCP
```

### Authentication

| Method | Use Case |
|--------|----------|
| API Keys | Simple service auth |
| OAuth 2.0 | User-based access |
| mTLS | Service-to-service |
| JWT | Stateless tokens |

```typescript
// Gateway auth middleware
async function authenticateRequest(req: Request): Promise<Client> {
  const token = req.headers.get('Authorization');
  const client = await validateToken(token);

  if (!client) throw new UnauthorizedError();

  // Check server access
  const targetServer = resolveServer(req);
  if (!client.canAccess(targetServer)) {
    throw new ForbiddenError();
  }

  return client;
}
```

### Rate Limiting

```typescript
interface RateLimitConfig {
  requests_per_minute: number;
  burst_size: number;
  by: 'client' | 'ip' | 'user';
}

// Per-server limits
const limits: Record<string, RateLimitConfig> = {
  'database': { requests_per_minute: 100, burst_size: 20, by: 'client' },
  'search': { requests_per_minute: 60, burst_size: 10, by: 'client' },
  'expensive-ops': { requests_per_minute: 10, burst_size: 2, by: 'user' }
};
```

---

## Gateway Patterns

### 1. Aggregation Gateway

Combine multiple MCP servers into single interface.

```
┌──────────────────────────────────────┐
│          Aggregation Gateway         │
│                                      │
│   tools/list → Merge all servers     │
│   Unified capability discovery       │
│                                      │
└──────────────────────────────────────┘
```

### 2. Security Gateway

Add auth and access control.

```
┌──────────────────────────────────────┐
│           Security Gateway           │
│                                      │
│   - Authentication                   │
│   - Authorization (RBAC)             │
│   - Audit logging                    │
│   - PII filtering                    │
│                                      │
└──────────────────────────────────────┘
```

### 3. Transformation Gateway

Modify requests/responses in transit.

```typescript
// Add context to all tool calls
async function transformRequest(req: ToolCallRequest): Promise<ToolCallRequest> {
  return {
    ...req,
    arguments: {
      ...req.arguments,
      _context: {
        client_id: req.client.id,
        timestamp: Date.now(),
        trace_id: req.traceId
      }
    }
  };
}
```

### 4. Caching Gateway

Cache frequent requests.

```typescript
const cacheConfig = {
  'resources/read': { ttl: 300, vary: ['uri'] },
  'tools/list': { ttl: 60 },
  'prompts/list': { ttl: 60 }
  // tools/call typically not cached
};
```

---

## Multi-Tenancy

### Tenant Isolation

| Isolation Level | Description |
|-----------------|-------------|
| Logical | Same servers, filtered access |
| Namespace | Prefixed resources/tools |
| Instance | Dedicated server instances |

```yaml
tenants:
  acme-corp:
    servers: [database, search, git]
    namespace: acme
    quotas:
      requests_per_day: 10000

  startup-inc:
    servers: [database, search]
    namespace: startup
    quotas:
      requests_per_day: 1000
```

### Access Control

```typescript
interface AccessPolicy {
  tenant: string;
  servers: string[];
  tools: {
    allowed: string[];  // glob patterns
    denied: string[];
  };
  resources: {
    allowed: string[];  // URI patterns
  };
}

const policies: AccessPolicy[] = [
  {
    tenant: 'acme-corp',
    servers: ['database', 'search'],
    tools: {
      allowed: ['search_*', 'query_*'],
      denied: ['delete_*', 'drop_*']
    },
    resources: {
      allowed: ['file:///shared/*', 'db://acme/*']
    }
  }
];
```

---

## Observability

### Metrics

```typescript
// Key gateway metrics
const metrics = {
  // Request metrics
  'mcp_gateway_requests_total': Counter,
  'mcp_gateway_request_duration_seconds': Histogram,
  'mcp_gateway_request_size_bytes': Histogram,

  // Server health
  'mcp_server_up': Gauge,
  'mcp_server_latency_seconds': Gauge,

  // Rate limiting
  'mcp_gateway_rate_limited_total': Counter,

  // Auth
  'mcp_gateway_auth_failures_total': Counter
};
```

### Logging

```json
{
  "timestamp": "2025-01-15T10:30:00Z",
  "level": "info",
  "event": "tool_call",
  "client_id": "client-123",
  "tenant": "acme-corp",
  "server": "database",
  "tool": "query_users",
  "duration_ms": 45,
  "status": "success",
  "trace_id": "abc-123"
}
```

### Distributed Tracing

```
Gateway                  MCP Server              Database
   │                         │                      │
   │──── tool_call ─────────►│                      │
   │     (trace: abc)        │                      │
   │                         │──── query ──────────►│
   │                         │     (trace: abc)     │
   │                         │◄─── result ──────────│
   │◄─── response ───────────│                      │
   │                         │                      │
```

---

## Health & Resilience

### Health Checks

```typescript
interface ServerHealth {
  server: string;
  status: 'healthy' | 'degraded' | 'unhealthy';
  latency_ms: number;
  last_check: Date;
  consecutive_failures: number;
}

async function healthCheck(server: ServerConfig): Promise<ServerHealth> {
  const start = Date.now();
  try {
    await server.ping();
    return {
      server: server.name,
      status: 'healthy',
      latency_ms: Date.now() - start,
      last_check: new Date(),
      consecutive_failures: 0
    };
  } catch (error) {
    return {
      server: server.name,
      status: 'unhealthy',
      latency_ms: -1,
      last_check: new Date(),
      consecutive_failures: server.failures + 1
    };
  }
}
```

### Circuit Breaker

```typescript
class CircuitBreaker {
  private state: 'closed' | 'open' | 'half-open' = 'closed';
  private failures = 0;
  private lastFailure?: Date;

  async call<T>(fn: () => Promise<T>): Promise<T> {
    if (this.state === 'open') {
      if (this.shouldAttemptReset()) {
        this.state = 'half-open';
      } else {
        throw new CircuitOpenError();
      }
    }

    try {
      const result = await fn();
      this.onSuccess();
      return result;
    } catch (error) {
      this.onFailure();
      throw error;
    }
  }
}
```

### Failover

```yaml
servers:
  database:
    primary: http://mcp-db-1:3000
    fallback:
      - http://mcp-db-2:3000
      - http://mcp-db-3:3000
    strategy: failover  # or round-robin, weighted
```

---

## Implementation Options

### Build vs Buy

| Approach | Pros | Cons |
|----------|------|------|
| Custom gateway | Full control | Development cost |
| API gateway + custom | Reuse infra | MCP-specific logic needed |
| Managed service | No ops | Vendor lock-in, cost |

### Using Existing API Gateways

Adapt general API gateways for MCP:

| Gateway | MCP Suitability |
|---------|-----------------|
| Kong | Good - plugin system |
| Envoy | Good - programmable |
| NGINX | Basic - config only |
| AWS API Gateway | Limited - less flexible |

### Example: Kong Plugin

```lua
-- MCP routing plugin
local MCPRouter = {
  VERSION = "1.0.0",
  PRIORITY = 1000,
}

function MCPRouter:access(conf)
  local body = kong.request.get_body()
  local method = body.method

  -- Route based on MCP method
  if method == "tools/call" then
    local tool_name = body.params.name
    local target = resolve_server(tool_name, conf.routes)
    kong.service.set_target(target.host, target.port)
  end
end
```

---

## Security Considerations

### Best Practices

| Practice | Implementation |
|----------|----------------|
| Input validation | Validate all MCP messages |
| Tool allowlisting | Only expose approved tools |
| Resource restrictions | Limit accessible URIs |
| Audit logging | Log all operations |
| Secrets management | Never log credentials |

### Dangerous Tool Blocking

```typescript
const blockedTools = [
  'eval',           // Arbitrary code execution
  'shell_execute',  // Shell access
  'file_delete',    // Destructive operations
  'sql_execute',    // Raw SQL (use parameterized instead)
];

function validateToolCall(tool: string): void {
  if (blockedTools.includes(tool)) {
    throw new ForbiddenError(`Tool '${tool}' is not allowed`);
  }
}
```

---

## MCP Gateway vs API Gateway

| Aspect | API Gateway | MCP Gateway |
|--------|-------------|-------------|
| Protocol | HTTP/REST | JSON-RPC 2.0 |
| Discovery | OpenAPI/Swagger | MCP capability negotiation |
| Routing | Path-based | Method + param based |
| Streaming | Optional | SSE native |
| Semantics | REST verbs | tools, resources, prompts |

---

## When to Use MCP Gateway

**Strengths:**
- Centralized MCP management
- Unified auth and observability
- Multi-tenant isolation
- Production-ready scaling

**Considerations:**
- Additional infrastructure
- Latency overhead
- Complexity for simple setups

**Best for:**
- Enterprise deployments
- Multi-tenant platforms
- Production AI applications
- Regulated environments

---

## Related

- [[MCP Servers]]
- [[Agent Gateway]]
- [[API Gateways]]
- [[Load Balancing]]
