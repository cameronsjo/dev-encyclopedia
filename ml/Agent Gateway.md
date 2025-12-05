---
title: Agent Gateway
aliases:
  - AI Gateway
  - LLM Gateway
  - Agent Router
  - AI Proxy
tags:
  - ai
  - infrastructure
  - agents
  - gateway
  - llm
type: reference
status: complete
created: "2025-11-30"
---

# Agent Gateway

Infrastructure layer for routing, managing, and securing AI agent traffic.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Route, secure, and observe AI agent requests |
| Pattern | Reverse proxy for LLM/agent traffic |
| Use case | Production AI, multi-model, cost control |
| Key features | Model routing, fallback, caching, guardrails |

---

## Why Agent Gateway?

### Direct LLM API Challenges

| Challenge | Gateway Solution |
|-----------|------------------|
| Vendor lock-in | Abstract multiple providers |
| No fallback | Automatic failover |
| Cost overruns | Budget limits, caching |
| No visibility | Centralized metrics/logs |
| Security gaps | Input/output guardrails |
| Rate limits | Request queuing, retry |

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                       Agent Gateway                          │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │                    Ingress Layer                      │   │
│  │   • Authentication     • Rate Limiting                │   │
│  │   • Request Validation • Input Guardrails            │   │
│  └───────────────────────────┬──────────────────────────┘   │
│                              │                              │
│  ┌───────────────────────────▼──────────────────────────┐   │
│  │                    Routing Layer                      │   │
│  │   • Model Selection    • Load Balancing               │   │
│  │   • Fallback Logic     • A/B Testing                  │   │
│  └───────────────────────────┬──────────────────────────┘   │
│                              │                              │
│  ┌───────────────────────────▼──────────────────────────┐   │
│  │                    Egress Layer                       │   │
│  │   • Response Caching   • Output Guardrails            │   │
│  │   • Cost Tracking      • Logging                      │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
└─────────────────────────────────┬───────────────────────────┘
                                  │
        ┌─────────────────────────┼─────────────────────────┐
        ▼                         ▼                         ▼
   ┌─────────┐              ┌─────────┐              ┌─────────┐
   │ OpenAI  │              │ Claude  │              │  Local  │
   │   API   │              │   API   │              │  LLM    │
   └─────────┘              └─────────┘              └─────────┘
```

---

## Core Capabilities

### Model Routing

Route requests to appropriate models based on criteria.

```yaml
# routing-config.yaml
routes:
  - name: coding
    match:
      intent: code_generation
      # or: prompt contains "write code", "implement"
    target:
      model: claude-sonnet-4-20250514
      provider: anthropic

  - name: simple-qa
    match:
      max_tokens: 100
      complexity: low
    target:
      model: gpt-4o-mini
      provider: openai

  - name: default
    target:
      model: claude-sonnet-4-20250514
      provider: anthropic
```

### Routing Strategies

| Strategy | Description | Use Case |
|----------|-------------|----------|
| Intent-based | Route by detected intent | Different models for code vs chat |
| Cost-based | Cheapest viable model | Budget optimization |
| Latency-based | Fastest responding | Real-time applications |
| Capability-based | Model features | Vision, long context |
| A/B testing | Random split | Evaluation |

```typescript
interface RoutingDecision {
  model: string;
  provider: string;
  reason: string;
}

function routeRequest(request: AgentRequest): RoutingDecision {
  // Intent detection
  if (isCodeRequest(request.prompt)) {
    return { model: 'claude-sonnet-4-20250514', provider: 'anthropic', reason: 'code_intent' };
  }

  // Context length
  if (request.context_tokens > 100000) {
    return { model: 'claude-3-opus', provider: 'anthropic', reason: 'long_context' };
  }

  // Cost optimization for simple queries
  if (estimateComplexity(request) === 'low') {
    return { model: 'gpt-4o-mini', provider: 'openai', reason: 'cost_optimization' };
  }

  return { model: 'gpt-4o', provider: 'openai', reason: 'default' };
}
```

---

## Provider Abstraction

### Unified Interface

```typescript
interface UnifiedRequest {
  messages: Message[];
  model?: string;         // Optional - gateway can decide
  max_tokens?: number;
  temperature?: number;
  tools?: Tool[];
  stream?: boolean;
}

interface UnifiedResponse {
  content: string;
  model: string;
  provider: string;
  usage: {
    input_tokens: number;
    output_tokens: number;
    cost_usd: number;
  };
  latency_ms: number;
}
```

### Provider Adapters

```typescript
interface ProviderAdapter {
  name: string;
  transformRequest(req: UnifiedRequest): ProviderRequest;
  transformResponse(res: ProviderResponse): UnifiedResponse;
  estimateCost(usage: TokenUsage): number;
}

const adapters: Record<string, ProviderAdapter> = {
  openai: new OpenAIAdapter(),
  anthropic: new AnthropicAdapter(),
  google: new GoogleAdapter(),
  local: new LocalLLMAdapter()
};
```

---

## Resilience Patterns

### Automatic Fallback

```yaml
fallback:
  primary:
    provider: anthropic
    model: claude-sonnet-4-20250514
  fallbacks:
    - provider: openai
      model: gpt-4o
      conditions:
        - error_codes: [500, 502, 503, 529]
        - timeout_ms: 30000
    - provider: local
      model: llama-3-70b
      conditions:
        - all_remote_failed: true
```

### Circuit Breaker

```typescript
class ProviderCircuitBreaker {
  private state: 'closed' | 'open' | 'half-open' = 'closed';
  private failures = 0;
  private threshold = 5;
  private resetTimeout = 60000; // 1 minute

  async call(provider: string, request: Request): Promise<Response> {
    if (this.isOpen(provider)) {
      throw new CircuitOpenError(provider);
    }

    try {
      const response = await this.providers[provider].call(request);
      this.recordSuccess(provider);
      return response;
    } catch (error) {
      this.recordFailure(provider);
      throw error;
    }
  }
}
```

### Retry with Backoff

```typescript
const retryConfig = {
  maxRetries: 3,
  initialDelay: 1000,
  maxDelay: 10000,
  backoffMultiplier: 2,
  retryableErrors: [429, 500, 502, 503, 529]
};

async function withRetry<T>(
  fn: () => Promise<T>,
  config = retryConfig
): Promise<T> {
  let lastError: Error;
  let delay = config.initialDelay;

  for (let attempt = 0; attempt <= config.maxRetries; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error;
      if (!isRetryable(error, config)) throw error;
      await sleep(delay);
      delay = Math.min(delay * config.backoffMultiplier, config.maxDelay);
    }
  }

  throw lastError;
}
```

---

## Cost Management

### Budget Controls

```yaml
budgets:
  organization:
    daily_limit_usd: 1000
    monthly_limit_usd: 25000
    alert_threshold: 0.8

  teams:
    engineering:
      daily_limit_usd: 500
    research:
      daily_limit_usd: 300

  users:
    default_daily_usd: 50
```

### Cost Tracking

```typescript
interface CostTracker {
  recordUsage(request: CompletedRequest): void;
  getCurrentSpend(scope: 'org' | 'team' | 'user', id: string): number;
  checkBudget(scope: string, id: string): BudgetStatus;
}

// Per-request cost calculation
function calculateCost(usage: TokenUsage, model: string): number {
  const pricing = MODEL_PRICING[model];
  return (
    (usage.input_tokens / 1000) * pricing.input_per_1k +
    (usage.output_tokens / 1000) * pricing.output_per_1k
  );
}
```

### Model Pricing Reference

| Model | Input ($/1M) | Output ($/1M) |
|-------|--------------|---------------|
| GPT-4o | $2.50 | $10.00 |
| GPT-4o-mini | $0.15 | $0.60 |
| Claude Sonnet | $3.00 | $15.00 |
| Claude Haiku | $0.25 | $1.25 |

---

## Caching

### Semantic Caching

Cache based on meaning, not exact match.

```typescript
interface SemanticCache {
  // Store response with embedding
  set(prompt: string, response: string, embedding: number[]): void;

  // Find similar cached responses
  get(prompt: string, embedding: number[], threshold: number): CacheHit | null;
}

async function handleRequest(request: Request): Promise<Response> {
  const embedding = await embed(request.prompt);
  const cached = await cache.get(request.prompt, embedding, 0.95);

  if (cached) {
    return { ...cached.response, cached: true };
  }

  const response = await llm.complete(request);
  await cache.set(request.prompt, response, embedding);
  return response;
}
```

### Cache Strategies

| Strategy | How | Trade-off |
|----------|-----|-----------|
| Exact match | Hash prompt | Fast, low hit rate |
| Semantic | Embedding similarity | Higher hit rate, cost |
| Prefix | Match prompt prefix | Good for templates |
| TTL-based | Expire after time | Freshness vs cost |

---

## Guardrails

### Input Guardrails

```typescript
interface InputGuardrail {
  name: string;
  check(input: string): GuardrailResult;
}

const inputGuardrails: InputGuardrail[] = [
  {
    name: 'pii_detection',
    check: (input) => detectPII(input) ? { block: true, reason: 'PII detected' } : { block: false }
  },
  {
    name: 'prompt_injection',
    check: (input) => detectInjection(input) ? { block: true, reason: 'Injection attempt' } : { block: false }
  },
  {
    name: 'content_policy',
    check: (input) => violatesPolicy(input) ? { block: true, reason: 'Policy violation' } : { block: false }
  }
];
```

### Output Guardrails

```typescript
const outputGuardrails = [
  {
    name: 'pii_redaction',
    transform: (output) => redactPII(output)
  },
  {
    name: 'toxicity_filter',
    check: (output) => isToxic(output) ? { block: true } : { block: false }
  },
  {
    name: 'hallucination_detection',
    check: (output, context) => isGrounded(output, context)
  }
];
```

### Guardrail Pipeline

```
Input → [PII Check] → [Injection Check] → [Policy Check] → LLM
                                                            ↓
Output ← [PII Redact] ← [Toxicity Filter] ← [Format Check] ←
```

---

## Observability

### Metrics

```typescript
const gatewayMetrics = {
  // Latency
  'llm_request_duration_seconds': Histogram,
  'llm_time_to_first_token_seconds': Histogram,

  // Volume
  'llm_requests_total': Counter,  // labels: model, provider, status
  'llm_tokens_total': Counter,    // labels: direction (input/output)

  // Cost
  'llm_cost_usd_total': Counter,

  // Errors
  'llm_errors_total': Counter,    // labels: error_type, provider

  // Cache
  'llm_cache_hits_total': Counter,
  'llm_cache_hit_ratio': Gauge,

  // Guardrails
  'llm_guardrail_blocks_total': Counter,
};
```

### Logging

```json
{
  "timestamp": "2025-01-15T10:30:00Z",
  "request_id": "req-abc123",
  "user_id": "user-456",
  "model": "claude-sonnet-4-20250514",
  "provider": "anthropic",
  "input_tokens": 1500,
  "output_tokens": 500,
  "latency_ms": 2340,
  "cost_usd": 0.012,
  "cache_hit": false,
  "guardrails_triggered": [],
  "status": "success"
}
```

---

## Agent Gateway Solutions

### Open Source

| Solution | Language | Features |
|----------|----------|----------|
| LiteLLM | Python | 100+ providers, fallback |
| Portkey | TypeScript | Caching, guardrails |
| AI Gateway (Cloudflare) | Rust | Edge, caching |
| Kong AI Gateway | Lua | Enterprise, plugins |

### Managed Services

| Service | Best For |
|---------|----------|
| Portkey.ai | Observability, reliability |
| Helicone | Analytics, caching |
| Braintrust | Eval, logging |
| LangSmith | LangChain ecosystem |

### LiteLLM Example

```python
from litellm import completion, Router

# Define model list with fallbacks
model_list = [
    {
        "model_name": "gpt-4",
        "litellm_params": {
            "model": "azure/gpt-4",
            "api_key": os.environ["AZURE_API_KEY"],
        },
    },
    {
        "model_name": "gpt-4",
        "litellm_params": {
            "model": "openai/gpt-4",
            "api_key": os.environ["OPENAI_API_KEY"],
        },
    },
]

router = Router(model_list=model_list)

# Automatic failover
response = router.completion(
    model="gpt-4",
    messages=[{"role": "user", "content": "Hello"}]
)
```

---

## Implementation Patterns

### Proxy Pattern

Gateway sits between client and providers.

```
Client → Gateway → Provider
```

### Sidecar Pattern

Gateway runs alongside each agent.

```
┌────────────────────┐
│  Pod               │
│  ┌──────┐ ┌──────┐│
│  │Agent │→│Sidecar││
│  └──────┘ └──────┘│
└────────────────────┘
```

### Service Mesh

Integrate with Istio/Linkerd for traffic management.

---

## When to Use Agent Gateway

**Strengths:**

- Multi-provider flexibility
- Cost control and visibility
- Production resilience
- Security guardrails
- Centralized observability

**Considerations:**

- Added latency (minimal)
- Additional infrastructure
- Complexity for simple apps

**Best for:**

- Production AI applications
- Multi-model strategies
- Cost-sensitive deployments
- Regulated industries
- Enterprise AI platforms

---

## Related

- [[MCP Gateway]]
- [[API Gateways]]
- [[Load Balancing]]
- [[Agent Frameworks]]
- [[LLMs & Transformers]]
