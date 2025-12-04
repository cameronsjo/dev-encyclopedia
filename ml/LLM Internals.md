---
title: LLM Internals
aliases:
  - LLM Architecture
  - Model Internals
  - LLM Parameters
tags:
  - ai
  - ml
  - llm
  - deep-learning
  - internals
type: concept
status: complete
created: 2025-11-30
---

# LLM Internals

Understanding what's inside large language models: weights, parameters, and configuration.

## Overview

| Aspect | Details |
|--------|---------|
| Core components | Weights, embeddings, attention layers |
| Configuration | Hyperparameters, inference settings |
| Control surfaces | System prompts, sampling parameters |
| Output shaping | Temperature, top-p, stop sequences |

---

## Model Components

### Weights

The learned parameters that define model behavior.

```
┌────────────────────────────────────────────────────────────┐
│                      LLM Weights                            │
│                                                            │
│  ┌──────────────────────────────────────────────────────┐ │
│  │              Token Embeddings                         │ │
│  │   Maps vocabulary (~100K tokens) to vectors          │ │
│  │   Shape: [vocab_size × embedding_dim]                │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
│  ┌──────────────────────────────────────────────────────┐ │
│  │              Transformer Layers (×N)                  │ │
│  │                                                       │ │
│  │   ┌─────────────────┐  ┌─────────────────┐          │ │
│  │   │ Self-Attention  │  │ Feed-Forward    │          │ │
│  │   │ Wq, Wk, Wv, Wo  │  │ W1, W2          │          │ │
│  │   └─────────────────┘  └─────────────────┘          │ │
│  │                                                       │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
│  ┌──────────────────────────────────────────────────────┐ │
│  │              Output Layer                             │ │
│  │   Projects to vocabulary for next token prediction   │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

### Weight Dimensions

| Component | Typical Shape | Notes |
|-----------|---------------|-------|
| Token embedding | [vocab × d_model] | ~100K × 4096 |
| Position embedding | [max_seq × d_model] | Or RoPE |
| Query/Key/Value | [d_model × d_head × n_heads] | Per layer |
| Feed-forward | [d_model × d_ff] | d_ff ≈ 4× d_model |
| Output projection | [d_model × vocab] | Often tied to embedding |

### Parameter Counts

| Model | Parameters | Layers | d_model | Heads |
|-------|------------|--------|---------|-------|
| GPT-2 | 1.5B | 48 | 1600 | 25 |
| LLaMA 7B | 7B | 32 | 4096 | 32 |
| LLaMA 70B | 70B | 80 | 8192 | 64 |
| GPT-4* | ~1T+ | ? | ? | ? |

*Estimated/rumored

---

## Inference Parameters

### Temperature

Controls randomness in token selection.

```
Temperature Effect:
──────────────────
T = 0.0  →  Greedy (always pick highest prob)
T = 0.5  →  More focused, less random
T = 1.0  →  Standard sampling
T = 2.0  →  Very random, creative/chaotic
```

```python
# Temperature scaling
logits = model_output / temperature
probabilities = softmax(logits)
next_token = sample(probabilities)
```

| Temperature | Use Case |
|-------------|----------|
| 0.0 - 0.3 | Factual, deterministic |
| 0.5 - 0.7 | Balanced |
| 0.8 - 1.0 | Creative writing |
| > 1.0 | Experimental, brainstorming |

### Top-K Sampling

Only consider the K most likely tokens.

```python
# Top-K: only sample from top K tokens
logits = model_output
top_k_logits = keep_top_k(logits, k=50)
probabilities = softmax(top_k_logits)
```

### Top-P (Nucleus) Sampling

Sample from smallest set of tokens whose cumulative probability ≥ P.

```python
# Top-P: dynamic cutoff based on cumulative probability
sorted_probs = sort_descending(softmax(logits))
cumulative = cumsum(sorted_probs)
cutoff_idx = first_index_where(cumulative >= p)
nucleus = sorted_probs[:cutoff_idx + 1]
```

| Top-P | Effect |
|-------|--------|
| 0.1 | Very focused |
| 0.5 | Moderate diversity |
| 0.9 | Standard |
| 1.0 | No filtering |

### Combined Sampling

```python
def sample_next_token(logits, temperature=1.0, top_k=50, top_p=0.9):
    # 1. Apply temperature
    logits = logits / temperature

    # 2. Top-K filtering
    if top_k > 0:
        logits = top_k_filter(logits, k=top_k)

    # 3. Top-P filtering
    if top_p < 1.0:
        logits = top_p_filter(logits, p=top_p)

    # 4. Sample
    probs = softmax(logits)
    return multinomial_sample(probs)
```

---

## System Prompts

Instructions that shape model behavior across the conversation.

### System Prompt Anatomy

```
┌────────────────────────────────────────────────────────────┐
│                     System Prompt                           │
│                                                            │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ Role Definition                                       │ │
│  │ "You are a helpful coding assistant..."              │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ Behavioral Guidelines                                 │ │
│  │ "Always cite sources. Never make up information."    │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ Output Format                                         │ │
│  │ "Respond in JSON format with fields: ..."            │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ Constraints                                           │ │
│  │ "Keep responses under 200 words. Use formal tone."   │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
│  ┌──────────────────────────────────────────────────────┐ │
│  │ Context/Knowledge                                     │ │
│  │ "Today's date is 2025-01-15. User timezone: PST."    │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

### System Prompt Patterns

**Role Pattern:**
```
You are [role] with expertise in [domain].
Your goal is to [objective].
```

**Rules Pattern:**
```
Follow these rules:
1. Always [do X]
2. Never [do Y]
3. When [condition], [action]
```

**Format Pattern:**
```
Respond in the following format:
{
  "answer": "...",
  "confidence": 0.0-1.0,
  "sources": [...]
}
```

**Few-Shot Pattern:**
```
Here are examples of good responses:

User: [example input 1]
Assistant: [example output 1]

User: [example input 2]
Assistant: [example output 2]
```

### System Prompt Best Practices

| Do | Don't |
|-----|-------|
| Be specific and clear | Use vague instructions |
| Provide examples | Assume understanding |
| Set explicit constraints | Leave behavior implicit |
| Test edge cases | Trust single tests |
| Version control prompts | Modify ad-hoc |

---

## Output Styles

Controlling how the model formats and structures responses.

### Style Dimensions

| Dimension | Options |
|-----------|---------|
| Length | Concise, detailed, exhaustive |
| Formality | Casual, professional, academic |
| Structure | Prose, lists, tables, code |
| Persona | Neutral, friendly, expert |
| Explanation depth | ELI5, technical, expert |

### Style Control Methods

**1. Explicit Instructions:**
```
Respond in a casual, friendly tone.
Use bullet points for lists.
Keep response under 100 words.
```

**2. Examples (Few-Shot):**
```
Example response style:
Q: What is X?
A: Great question! X is basically... [casual explanation]
```

**3. Format Enforcement:**
```
Always structure your response as:
## Summary
[1-2 sentences]

## Details
[Bullet points]

## Next Steps
[If applicable]
```

**4. Personas:**
```
Respond as a senior software engineer reviewing code.
Be direct, point out issues clearly, suggest improvements.
```

### Structured Output

Force specific output formats:

```python
# JSON mode (OpenAI)
response = client.chat.completions.create(
    model="gpt-4o",
    response_format={"type": "json_object"},
    messages=[{"role": "user", "content": "List 3 colors as JSON"}]
)

# Structured output (Anthropic)
response = client.messages.create(
    model="claude-sonnet-4-20250514",
    messages=[...],
    # Use tool use for structured output
)
```

---

## Token Limits

### Context Window

| Model | Context Length |
|-------|----------------|
| GPT-4o | 128K tokens |
| Claude 3.5 | 200K tokens |
| Claude Opus 4 | 200K tokens |
| Gemini 1.5 | 1M+ tokens |
| Llama 3 | 8K-128K |

### Token Counting

```python
# OpenAI tiktoken
import tiktoken
enc = tiktoken.encoding_for_model("gpt-4o")
tokens = enc.encode("Hello, world!")
print(len(tokens))  # 4

# Anthropic
from anthropic import Anthropic
client = Anthropic()
count = client.count_tokens("Hello, world!")
```

### Token Budget Management

```
┌────────────────────────────────────────────────────────────┐
│                  Context Window Budget                      │
│                                                            │
│  ┌─────────────────────────────────────────────────────┐  │
│  │ System Prompt                          ~500 tokens  │  │
│  └─────────────────────────────────────────────────────┘  │
│  ┌─────────────────────────────────────────────────────┐  │
│  │ Retrieved Context (RAG)              ~4000 tokens   │  │
│  └─────────────────────────────────────────────────────┘  │
│  ┌─────────────────────────────────────────────────────┐  │
│  │ Conversation History                 ~2000 tokens   │  │
│  └─────────────────────────────────────────────────────┘  │
│  ┌─────────────────────────────────────────────────────┐  │
│  │ Current User Message                  ~500 tokens   │  │
│  └─────────────────────────────────────────────────────┘  │
│  ┌─────────────────────────────────────────────────────┐  │
│  │ Reserved for Response               ~1000 tokens    │  │
│  └─────────────────────────────────────────────────────┘  │
│                                                            │
│  Total: 8000 tokens                                        │
└────────────────────────────────────────────────────────────┘
```

---

## Stop Sequences

Tokens that signal generation should stop.

```python
response = client.chat.completions.create(
    model="gpt-4o",
    messages=[...],
    stop=["\n\n", "END", "```"]  # Stop at any of these
)
```

| Use Case | Stop Sequences |
|----------|----------------|
| Single paragraph | `["\n\n"]` |
| Code block | `["```"]` |
| JSON object | `["}"]` (be careful) |
| List item | `["\n-", "\n*"]` |

---

## Logit Bias

Adjust probability of specific tokens.

```python
# Increase/decrease token probability
response = client.chat.completions.create(
    model="gpt-4o",
    messages=[...],
    logit_bias={
        12345: 100,   # Strongly prefer this token
        67890: -100,  # Strongly avoid this token
    }
)
```

| Bias Value | Effect |
|------------|--------|
| -100 | Token never selected |
| -10 | Significantly less likely |
| 0 | No change |
| +10 | Significantly more likely |
| +100 | Almost always selected |

---

## Frequency & Presence Penalties

Reduce repetition in outputs.

```python
response = client.chat.completions.create(
    model="gpt-4o",
    messages=[...],
    frequency_penalty=0.5,  # Penalize based on frequency
    presence_penalty=0.5,   # Penalize if token appeared at all
)
```

| Parameter | Effect |
|-----------|--------|
| frequency_penalty | Higher = less repetition of frequent words |
| presence_penalty | Higher = more topic diversity |

---

## Model Internals Deep Dive

### Attention Patterns

What the model "pays attention to":

```
Query: "The capital of France is"

Attention weights (simplified):
─────────────────────────────
"capital" → "France" (0.8)    # Strong connection
"is" → "capital" (0.6)        # Verb-noun link
"The" → "capital" (0.2)       # Weak
```

### Layer Functions

| Layer Type | What It Learns |
|------------|----------------|
| Early layers | Syntax, grammar, simple patterns |
| Middle layers | Semantic meaning, relationships |
| Late layers | Task-specific, output formatting |

### Residual Stream

Information flows through residual connections:

```
Input
  │
  ▼
┌─────────────────────────────────┐
│ Layer 1: Attention              │
└─────────────────────────────────┘
  │
  ▼ (add residual)
┌─────────────────────────────────┐
│ Layer 1: FFN                    │
└─────────────────────────────────┘
  │
  ▼ (add residual)
  ...
  │
  ▼
Output
```

---

## Inference Optimization

### KV Cache

Store computed key-value pairs to avoid recomputation.

```
Without KV Cache: O(n²) per token
With KV Cache: O(n) per token

Trade-off: Memory for speed
```

### Quantization

Reduce precision to save memory/compute.

| Precision | Memory | Quality |
|-----------|--------|---------|
| FP32 | 4 bytes/param | Best |
| FP16 | 2 bytes/param | Near-best |
| INT8 | 1 byte/param | Good |
| INT4 | 0.5 bytes/param | Acceptable |

```python
# GPTQ quantization
from transformers import AutoModelForCausalLM
model = AutoModelForCausalLM.from_pretrained(
    "TheBloke/Llama-2-7B-GPTQ",
    device_map="auto"
)
```

### Speculative Decoding

Use smaller model to draft, larger model to verify.

```
Draft (fast, small model): ["The", "cat", "sat", "on", "mat"]
Verify (slow, big model):  ["The", "cat", "sat", "on", "the", ...]
Accept: 4 tokens, reject: "mat"
```

---

## Configuration Summary

```yaml
# Complete inference config
model:
  name: gpt-4o
  max_tokens: 4096

sampling:
  temperature: 0.7
  top_p: 0.9
  top_k: 50
  frequency_penalty: 0.5
  presence_penalty: 0.0

control:
  stop_sequences: ["\n\n", "END"]
  logit_bias: {}

system_prompt: |
  You are a helpful assistant.
  Be concise and accurate.
```

---

## Related

- [[LLMs & Transformers]]
- [[Neural Networks]]
- [[Agent Frameworks]]
- [[Grounding]]
