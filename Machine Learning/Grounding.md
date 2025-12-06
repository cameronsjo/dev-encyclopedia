---
title: Grounding
aliases:
  - LLM Grounding
  - Factual Grounding
  - Groundedness
tags:
  - ai
  - ml
  - llm
  - reliability
  - rag
type: concept
status: complete
created: "2025-11-30"
---

# Grounding

Anchoring LLM outputs to verifiable sources and factual information.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Reduce hallucinations, ensure factual accuracy |
| Methods | RAG, citations, knowledge bases, real-time data |
| Trade-off | Accuracy vs latency/cost |
| Key metric | Groundedness score |

---

## Why Grounding Matters

### The Hallucination Problem

LLMs generate plausible-sounding but potentially false information.

| Type | Example |
|------|---------|
| Factual error | "The Eiffel Tower was built in 1920" |
| Citation fabrication | Inventing non-existent papers/sources |
| Outdated information | Using training data from years ago |
| Confident uncertainty | Stating guesses as facts |

### Grounding Benefits

| Benefit | Description |
|---------|-------------|
| Accuracy | Responses backed by sources |
| Verifiability | Users can check citations |
| Recency | Access to current information |
| Trust | Builds user confidence |
| Compliance | Audit trail for regulated industries |

---

## Grounding Techniques

### 1. Retrieval-Augmented Generation (RAG)

Inject relevant documents into context.

```
Query → Retrieve Documents → [Query + Documents] → LLM → Response
```

See [[RAG]] for detailed implementation.

### 2. Real-Time Search

Ground in live web/API data.

```python
async def grounded_response(query: str) -> str:
    # Search for current information
    search_results = await web_search(query)

    # Construct grounded prompt
    prompt = f"""Answer based ONLY on these sources:

{format_sources(search_results)}

Question: {query}

Cite sources using [1], [2], etc."""

    response = await llm.complete(prompt)
    return response
```

### 3. Knowledge Base Lookup

Query structured knowledge.

```python
def ground_with_kb(query: str, kb: KnowledgeBase) -> str:
    # Extract entities
    entities = extract_entities(query)

    # Lookup facts
    facts = []
    for entity in entities:
        facts.extend(kb.lookup(entity))

    # Generate with facts
    return llm.complete(f"Facts: {facts}\nQuery: {query}")
```

### 4. Tool Use / Function Calling

Let LLM query authoritative APIs.

```typescript
const tools = [
  {
    name: "get_stock_price",
    description: "Get current stock price",
    execute: async (symbol: string) => await stockAPI.getPrice(symbol)
  },
  {
    name: "get_weather",
    description: "Get current weather",
    execute: async (location: string) => await weatherAPI.get(location)
  }
];

// LLM decides when to call tools for grounding
```

### 5. Self-Verification

LLM checks its own outputs.

```
Generate Response → Extract Claims → Verify Each Claim → Revise if Needed
```

---

## Grounding Architecture

### Hybrid Grounding Pipeline

```
┌──────────────────────────────────────────────────────────────┐
│                     Grounding Pipeline                        │
│                                                              │
│  Query                                                       │
│    │                                                         │
│    ▼                                                         │
│  ┌─────────────────────────────────────────────────────┐    │
│  │              Intent Classification                   │    │
│  │   • Factual query → needs grounding                 │    │
│  │   • Creative task → less grounding needed           │    │
│  │   • Opinion request → acknowledge subjectivity      │    │
│  └───────────────────────────┬─────────────────────────┘    │
│                              │                              │
│    ┌─────────────────────────┼─────────────────────────┐    │
│    ▼                         ▼                         ▼    │
│  ┌────────┐            ┌──────────┐            ┌─────────┐ │
│  │  RAG   │            │ Web/API  │            │   KB    │ │
│  │ Vector │            │  Search  │            │ Lookup  │ │
│  └────┬───┘            └────┬─────┘            └────┬────┘ │
│       │                     │                       │       │
│       └─────────────────────┼───────────────────────┘       │
│                             ▼                               │
│                    ┌────────────────┐                       │
│                    │ Context Fusion │                       │
│                    └───────┬────────┘                       │
│                            │                                │
│                            ▼                                │
│                    ┌────────────────┐                       │
│                    │   LLM + Cite   │                       │
│                    └───────┬────────┘                       │
│                            │                                │
│                            ▼                                │
│                    ┌────────────────┐                       │
│                    │   Verify &     │                       │
│                    │   Validate     │                       │
│                    └────────────────┘                       │
│                                                              │
└──────────────────────────────────────────────────────────────┘
```

---

## Citation Patterns

### Inline Citations

```
The population of Tokyo is approximately 14 million [1]. The city
has experienced growth since the 2020 census [2].

Sources:
[1] Japanese Statistics Bureau, 2024
[2] Tokyo Metropolitan Government Report
```

### Structured Citations

```json
{
  "answer": "The population of Tokyo is approximately 14 million.",
  "citations": [
    {
      "text": "population of Tokyo is approximately 14 million",
      "source": "Japanese Statistics Bureau",
      "url": "https://...",
      "retrieved": "2025-01-15"
    }
  ],
  "confidence": 0.95
}
```

### Citation Prompt Pattern

```
You are a research assistant. For every factual claim:
1. Only state information from the provided sources
2. Cite using [Source Name] after each claim
3. If unsure, say "I don't have information about this"
4. Never invent sources

Sources:
{sources}

Question: {query}
```

---

## Verification Techniques

### Claim Extraction

```python
def extract_claims(text: str) -> list[str]:
    """Extract verifiable claims from text."""
    prompt = f"""Extract factual claims from this text.
Return each claim on a new line.
Only include verifiable facts, not opinions.

Text: {text}"""

    claims = llm.complete(prompt).split('\n')
    return [c.strip() for c in claims if c.strip()]
```

### Claim Verification

```python
async def verify_claim(claim: str, sources: list[str]) -> VerificationResult:
    """Check if claim is supported by sources."""

    prompt = f"""Determine if this claim is supported by the sources.

Claim: {claim}

Sources:
{format_sources(sources)}

Answer with:
- SUPPORTED: Claim is directly supported
- PARTIALLY_SUPPORTED: Some support, missing details
- NOT_SUPPORTED: No evidence found
- CONTRADICTED: Sources contradict this claim

Also quote the relevant source text."""

    result = await llm.complete(prompt)
    return parse_verification(result)
```

### Verification Pipeline

```
Response Claims                     Verification
──────────────────                 ──────────────
"Tokyo has 14M people"       →     SUPPORTED [Source 1]
"Founded in 1457"            →     SUPPORTED [Source 2]
"Largest city in world"      →     PARTIALLY (by metro area)
"10,000 restaurants"         →     NOT_SUPPORTED (no source)
```

---

## Groundedness Metrics

### Faithfulness Score

How well does the response follow the sources?

```python
def faithfulness_score(response: str, sources: list[str]) -> float:
    """Score 0-1 for how well response follows sources."""
    claims = extract_claims(response)
    supported = 0

    for claim in claims:
        if is_supported_by_sources(claim, sources):
            supported += 1

    return supported / len(claims) if claims else 1.0
```

### Attribution Rate

What percentage of claims have citations?

```python
def attribution_rate(response: str) -> float:
    """Percentage of claims with citations."""
    claims = extract_claims(response)
    cited = count_cited_claims(response)
    return cited / len(claims) if claims else 1.0
```

### RAGAS Metrics

| Metric | Measures |
|--------|----------|
| Faithfulness | Response derived from context |
| Answer relevancy | Response addresses query |
| Context precision | Retrieved context is relevant |
| Context recall | All needed info retrieved |

```python
from ragas import evaluate
from ragas.metrics import faithfulness, context_precision

results = evaluate(
    dataset,
    metrics=[faithfulness, context_precision]
)
```

---

## Handling Uncertainty

### Confidence Calibration

```python
class GroundedResponse:
    answer: str
    confidence: float  # 0.0 - 1.0
    sources: list[Source]
    caveats: list[str]

def generate_with_confidence(query: str, sources: list) -> GroundedResponse:
    # Generate response
    response = llm.complete(query, sources)

    # Assess confidence
    coverage = assess_source_coverage(query, sources)
    recency = assess_source_recency(sources)
    agreement = assess_source_agreement(sources)

    confidence = (coverage + recency + agreement) / 3

    caveats = []
    if confidence < 0.7:
        caveats.append("Limited source coverage")
    if recency < 0.5:
        caveats.append("Sources may be outdated")

    return GroundedResponse(
        answer=response,
        confidence=confidence,
        sources=sources,
        caveats=caveats
    )
```

### Epistemic Markers

Train models to express uncertainty appropriately:

| High Confidence | Low Confidence |
|-----------------|----------------|
| "The capital is Paris" | "The population is estimated to be..." |
| "According to [source]" | "Sources suggest..." |
| Direct statements | "It appears that..." |

---

## Common Pitfalls

| Pitfall | Solution |
|---------|----------|
| Outdated sources | Check source dates, prefer recent |
| Source conflicts | Acknowledge disagreement |
| Missing citations | Enforce citation in prompts |
| Over-reliance on single source | Require multiple sources |
| Citing without understanding | Verify claim-source alignment |
| Confident hallucinations | Add verification step |

---

## Grounding by Domain

### Legal/Compliance

```yaml
requirements:
  - Cite specific laws/regulations
  - Include jurisdiction
  - Note effective dates
  - Disclaimer for non-legal advice
```

### Medical/Health

```yaml
requirements:
  - Cite peer-reviewed sources
  - Include study dates
  - Note limitations
  - Strong disclaimers
```

### Technical Documentation

```yaml
requirements:
  - Cite version numbers
  - Link to official docs
  - Note deprecations
```

---

## Grounding vs Fine-Tuning

| Aspect | Grounding (RAG) | Fine-Tuning |
|--------|-----------------|-------------|
| Update knowledge | Easy (re-index) | Retrain needed |
| Source attribution | Yes | No |
| Hallucination risk | Lower | Can increase |
| Cost | Per-query retrieval | Training cost |
| Best for | Facts, recency | Style, format |

**Use both:** Fine-tune for style, ground for facts.

---

## Implementation Checklist

```
□ Intent detection (does query need grounding?)
□ Source retrieval (RAG, search, API)
□ Citation enforcement in prompts
□ Claim extraction pipeline
□ Verification system
□ Confidence scoring
□ Uncertainty expression
□ Source freshness checks
□ Metrics and monitoring
```

---

## When to Ground

**Always ground:**

- Factual queries (dates, numbers, events)
- Medical/legal/financial advice
- Technical specifications
- Current events

**Grounding optional:**

- Creative writing
- Brainstorming
- Code generation (unless specs matter)
- General explanations

**Skip grounding:**

- Pure opinion questions
- Hypotheticals
- Casual conversation

---

## Related

- [[RAG]]
- [[LLMs & Transformers]]
- [[LLM Internals]]
- [[Agent Frameworks]]
