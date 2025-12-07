---
title: AI Observability
aliases:
  - LLM Observability
  - AI Monitoring
tags:
  - ai
  - ml
  - observability
  - llm
  - monitoring
type: reference
status: complete
created: "2025-11-30"
---

# AI Observability

Specialized observability for LLM applications: trace generation calls, track token usage and costs, debug multi-step agent reasoning, version prompts, and evaluate output quality.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Monitor, debug, and evaluate LLM application behavior |
| **Key Metrics** | Latency, token usage, cost, output quality, retrieval accuracy |
| **Trace Targets** | LLM calls, agent steps, RAG pipelines, tool invocations |
| **Standards** | OpenTelemetry extensions, vendor-specific SDKs |
| **Challenges** | Non-determinism, prompt injection detection, PII leakage, cost explosion |

## Core Capabilities

### Trace Visualization

**Track execution flow through LLM pipelines:**

- Individual LLM API calls with request/response payloads
- Multi-step agent reasoning chains and decision points
- RAG pipeline stages: retrieval, reranking, generation
- Tool/function calling sequences
- Parallel execution paths in agent orchestration

### Token and Cost Tracking

**Monitor resource consumption:**

- Per-request token counts (prompt + completion)
- Cumulative usage per user, session, or feature
- Real-time cost calculation with model-specific pricing
- Budget alerts and rate limiting triggers
- Historical trends for capacity planning

### Latency Analysis

**Identify performance bottlenecks:**

- Time-to-first-token (TTFT) metrics
- Total generation latency per request
- Streaming chunk intervals
- External API call overhead (vector DB, tools)
- Queue wait times and throttling events

### Prompt Management

**Version control for prompts:**

- Prompt template versioning with git-like diffs
- A/B testing support with traffic splitting
- Rollback capabilities for bad deployments
- Environment-specific overrides (dev/staging/prod)
- Audit trail for prompt changes

### Agent Debugging

**Inspect multi-step reasoning:**

- Step-by-step agent decision logs
- Intermediate outputs and internal monologue
- Tool selection rationale and parameters
- Error propagation through agent chains
- Loop detection and termination analysis

### Quality Evaluation

**Assess output correctness:**

- Human feedback collection (thumbs up/down)
- Automated evaluations (similarity, faithfulness, hallucination detection)
- Custom evaluators with LLM-as-judge patterns
- Ground truth comparisons for regression testing
- Output classification (toxicity, PII, off-topic)

## Platform Comparison

| Platform | Open Source | Hosted | Self-Hosted | Strengths | Considerations |
|----------|-------------|--------|-------------|-----------|----------------|
| **LangSmith** | ❌ | ✅ | ✅ (Enterprise) | Deep LangChain integration, datasets, evaluators | LangChain ecosystem lock-in |
| **LangFuse** | ✅ | ✅ | ✅ | Framework-agnostic, PostgreSQL backend, cost tracking | Smaller community vs LangSmith |
| **Helicone** | ✅ | ✅ | ✅ | Gateway proxy pattern, zero code changes, caching | Adds network hop to LLM calls |
| **Arize Phoenix** | ✅ | ✅ | ✅ | ML model monitoring heritage, embedding visualization | Heavier weight setup |
| **Weights & Biases** | ❌ | ✅ | ❌ | Experiment tracking, artifact management, collaboration | General ML tool, not LLM-first |
| **OpenLLMetry** | ✅ | ❌ | ✅ | OpenTelemetry-native, vendor-neutral, extensible | Requires separate backend (Jaeger, etc.) |

## Integration Patterns

### SDK Instrumentation

**Direct code integration:**

```python
# LangSmith example
from langsmith import trace

@trace(name="generate_response")
def generate_response(prompt: str) -> str:
    return llm.invoke(prompt)
```

**Pros:** Fine-grained control, custom metadata, framework-specific features
**Cons:** Code changes required, framework lock-in risk

### Gateway Proxy

**Intercept LLM API calls:**

```bash
# Route OpenAI calls through Helicone
export OPENAI_API_BASE=https://oai.hconeai.com/v1
export HELICONE_API_KEY=your_key
```

**Pros:** Zero code changes, universal compatibility, centralized caching
**Cons:** Additional network latency, single point of failure

### OpenTelemetry Extension

**Standards-based tracing:**

```python
from opentelemetry import trace
from openllmetry.instrumentation.openai import OpenAIInstrumentor

OpenAIInstrumentor().instrument()
tracer = trace.get_tracer(__name__)

with tracer.start_as_current_span("llm_call"):
    response = openai.chat.completions.create(...)
```

**Pros:** Vendor-neutral, existing observability pipeline reuse
**Cons:** Fewer LLM-specific features, manual setup complexity

## Key Features by Use Case

### Production Monitoring

**Essential capabilities:**

- Real-time dashboards for latency and error rates
- Cost tracking with budget alerts
- PII detection and redaction
- Rate limiting enforcement
- Incident correlation with traces

### Development & Debugging

**Essential capabilities:**

- Detailed trace inspection with payload viewing
- Prompt playground for testing variations
- Agent step-by-step debugging
- Evaluation runs against test datasets
- Diff comparison between prompt versions

### Evaluation & Testing

**Essential capabilities:**

- Dataset management for regression tests
- Batch evaluation runs
- Custom evaluator definitions
- Ground truth labeling workflows
- A/B test result analysis

### Security & Compliance

**Essential capabilities:**

- Audit logs for all LLM interactions
- PII detection and anonymization
- Prompt injection attempt logging
- Output toxicity filtering
- Data retention policies

## Decision Guide

| Choose | If You Need | Consider |
|--------|-------------|----------|
| **LangSmith** | Deep LangChain integration, comprehensive datasets | LangChain dependency acceptable |
| **LangFuse** | Self-hosted, open source, PostgreSQL backend | Managing own infrastructure |
| **Helicone** | Zero code changes, drop-in proxy pattern | Additional network latency acceptable |
| **Arize Phoenix** | Embedding visualization, ML model monitoring | Heavier infrastructure investment |
| **Weights & Biases** | Existing W&B usage, experiment tracking | General ML workflows beyond LLMs |
| **OpenLLMetry** | OpenTelemetry standardization, vendor neutrality | DIY backend setup (Jaeger, Tempo) |

## Common Metrics

### Latency Metrics

- **Time-to-First-Token (TTFT):** User-perceived responsiveness
- **Total Latency:** End-to-end request duration
- **Tokens per Second:** Streaming generation speed
- **P95/P99 Latency:** Tail latency for SLA monitoring

### Cost Metrics

- **Cost per Request:** Average spend per LLM call
- **Daily/Monthly Budget Burn:** Cumulative spending trends
- **Cost by Model:** Compare pricing across GPT-4, Claude, etc.
- **Cost by Feature:** Attribute spending to product areas

### Quality Metrics

- **Hallucination Rate:** % of factually incorrect outputs
- **Retrieval Accuracy:** Relevance of RAG-retrieved documents
- **User Feedback Score:** Thumbs up/down ratios
- **Task Success Rate:** % of successful completions

### Usage Metrics

- **Requests per Minute (RPM):** Traffic volume
- **Token Throughput:** Total tokens processed
- **Unique Users:** Active user counts
- **Retry Rate:** % of failed requests retried

## Best Practices

### Instrumentation

- Instrument at agent/pipeline boundaries, not individual LLM calls
- Capture prompt templates separately from runtime variables
- Include user ID and session ID for request correlation
- Tag traces with feature flags for A/B test analysis
- Set sampling rates to balance cost and coverage

### Cost Management

- Set per-user and per-feature budget limits
- Monitor unexpected cost spikes with alerting
- Use cheaper models for development/testing
- Cache expensive LLM calls when deterministic
- Track cost attribution to product teams

### Security

- Redact PII before sending to external observability platforms
- Implement rate limiting per user/API key
- Log prompt injection attempts for security analysis
- Validate outputs before displaying to users
- Encrypt trace payloads in transit and at rest

### Evaluation

- Maintain golden datasets for regression testing
- Version evaluators alongside prompt changes
- Run evaluations in CI/CD pipelines
- Combine automated and human evaluations
- Track evaluation metric trends over time

## Related

- [[LLM Evaluation]]
- [[Agent Frameworks]]
- [[OpenTelemetry]]
- [[Prompt Engineering]]
- [[RAG Systems]]
