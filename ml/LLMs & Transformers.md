---
title: LLMs & Transformers
aliases:
  - Large Language Models
  - Transformer Architecture
  - GPT
tags:
  - ml
  - deep-learning
  - nlp
  - transformers
  - llms
type: concept
status: complete
difficulty: intermediate
created: 2025-11-28
---

# LLMs & Transformers

The architecture powering modern AI: ChatGPT, Claude, and beyond.

## Transformers Overview

### What Changed

**Before (2017):** RNNs/LSTMs processed sequences one token at a time.

**After:** Transformers process all tokens in parallel using attention.

**Result:** Better parallelization, longer context, superior performance.

### Key Innovation: Attention

"Attention is All You Need" (Vaswani et al., 2017)

**Core idea:** Every token can directly attend to every other token.

---

## The Attention Mechanism

### Self-Attention

For each position, compute weighted sum of all positions based on relevance.

**Steps:**
1. Project input to Query (Q), Key (K), Value (V)
2. Compute attention scores: Q · Kᵀ
3. Scale and softmax: scores / √d_k
4. Weight values: softmax(scores) · V

**Intuition:** "What should I pay attention to?"

### Multi-Head Attention

Run multiple attention operations in parallel with different projections.

**Why:** Different heads learn different types of relationships.

- Head 1: Syntactic relationships
- Head 2: Semantic similarity
- Head 3: Positional patterns
- etc.

### Scaled Dot-Product Attention

Attention(Q, K, V) = softmax(QKᵀ/√d_k)V

**Scaling by √d_k:** Prevents softmax from becoming too peaky with large dimensions.

---

## Transformer Architecture

### Encoder-Decoder (Original)

```
[Encoder] → Hidden States → [Decoder] → Output
```

**Used for:** Translation, summarization.

### Encoder-Only (BERT)

Bidirectional context—sees all tokens at once.

**Used for:** Classification, NER, embeddings.

### Decoder-Only (GPT, Claude)

Autoregressive—only sees previous tokens.

**Used for:** Text generation, chat, coding.

### Key Components

| Component | Purpose |
|-----------|---------|
| Token Embedding | Convert tokens to vectors |
| Position Encoding | Add position information |
| Multi-Head Attention | Contextual mixing |
| Feed-Forward Network | Per-position processing |
| Layer Normalization | Stabilize training |
| Residual Connections | Enable gradient flow |

---

## Position Encoding

Transformers have no inherent notion of order.

### Sinusoidal (Original)

PE(pos, 2i) = sin(pos/10000^(2i/d))
PE(pos, 2i+1) = cos(pos/10000^(2i/d))

**Advantages:** Extrapolates to longer sequences, no learned parameters.

### Learned Positional Embeddings

Train position embeddings like any other embedding.

**Used by:** GPT-2, BERT.

### Rotary Position Embedding (RoPE)

Encode position in rotation of Q/K vectors.

**Used by:** LLaMA, GPT-NeoX. Better length generalization.

---

## Training LLMs

### Pretraining Objectives

| Objective | How | Model Type |
|-----------|-----|------------|
| Causal LM | Predict next token | GPT, Claude |
| Masked LM | Predict masked tokens | BERT |
| Prefix LM | Some bidirectional, some causal | T5, UL2 |

### Scale Matters

| Parameter | GPT-2 | GPT-3 | GPT-4* |
|-----------|-------|-------|--------|
| Parameters | 1.5B | 175B | ~1T+ |
| Training data | 40GB | 570GB | ??? |
| Compute | ? | ~3.6M GPU hours | ??? |

*Estimated/rumored

**Scaling laws:** Performance improves predictably with compute, data, parameters.

### Training Data

| Source | Example |
|--------|---------|
| Web text | Common Crawl |
| Books | BookCorpus, Books3 |
| Code | GitHub, StackOverflow |
| Conversation | Reddit, forums |
| Academic | ArXiv, PubMed |

---

## Making LLMs Useful

### Supervised Fine-Tuning (SFT)

Train on (prompt, response) pairs.

**Result:** Model follows instructions.

### RLHF (Reinforcement Learning from Human Feedback)

1. Collect human preference data
2. Train reward model
3. Optimize policy with RL (PPO)

**Result:** Model produces preferred outputs.

### Constitutional AI

Define principles, have model self-critique.

**Result:** Model aligns with specified values.

---

## Inference

### Autoregressive Generation

1. Input prompt
2. Predict next token
3. Append token to input
4. Repeat until stop

**Problem:** O(n²) attention at each step.

### Sampling Strategies

| Method | How | Trade-off |
|--------|-----|-----------|
| Greedy | Take highest prob | Deterministic, repetitive |
| Temperature | Divide logits by T | Higher T = more random |
| Top-k | Sample from top k | Cuts off unlikely |
| Top-p (nucleus) | Sample until cumulative p | Adaptive cutoff |

### KV Cache

Store computed K, V from previous tokens.

**Result:** Only compute attention for new token.

---

## Context Length

### The Challenge

Attention is O(n²) in sequence length.

| Model | Context |
|-------|---------|
| Original Transformer | 512 |
| GPT-3 | 2K |
| GPT-4 | 8K-128K |
| Claude | 100K-200K |

### Solutions

| Approach | How |
|----------|-----|
| Sparse attention | Attend to subset |
| Linear attention | O(n) approximations |
| Chunking | Process in windows |
| RoPE + fine-tuning | Extend position encoding |
| Ring attention | Distribute across devices |

---

## Emergent Abilities

Capabilities that appear suddenly at scale:

| Ability | Emerges At |
|---------|------------|
| Few-shot learning | ~1B params |
| Chain-of-thought | ~10B params |
| Instruction following | ~10B + RLHF |
| Code generation | ~10B params |
| Complex reasoning | ~100B+ params |

**Controversial:** Are these truly emergent or artifacts of metrics?

---

## Practical Considerations

### Prompt Engineering

| Technique | How |
|-----------|-----|
| Few-shot | Provide examples |
| Chain-of-thought | "Think step by step" |
| System prompts | Set context and constraints |
| Structured output | Request specific format |

### RAG (Retrieval-Augmented Generation)

1. Query retrieves relevant documents
2. Documents added to context
3. Model generates with retrieved info

**Use when:** Need current/specific information not in training data.

### Fine-Tuning vs Prompting

| Approach | When |
|----------|------|
| Prompting | Quick iteration, no compute |
| Few-shot | Some examples help |
| Fine-tuning | Specific domain, consistent style |
| Full training | Massive data, new capabilities |

---

## Limitations

| Limitation | Description |
|------------|-------------|
| Hallucination | Generates plausible-sounding false info |
| Knowledge cutoff | Doesn't know recent events |
| Context length | Can't process very long documents |
| Reasoning | Struggles with novel logic |
| Consistency | May contradict itself |
| Computation | Can't execute code/math reliably |

---

## The Ecosystem

### Foundation Models

| Model | Organization | Notes |
|-------|--------------|-------|
| GPT-4 | OpenAI | Proprietary |
| Claude | Anthropic | Constitutional AI |
| Gemini | Google | Multimodal |
| LLaMA | Meta | Open weights |
| Mistral | Mistral AI | Open, efficient |

### Tools & Frameworks

| Tool | Purpose |
|------|---------|
| Hugging Face | Model hub, Transformers library |
| LangChain | LLM application framework |
| LlamaIndex | RAG framework |
| vLLM | Fast inference |
| Ollama | Local model running |

---

## Related

- [[Neural Networks]]
- [[Attention Mechanisms]]
- [[NLP]]
- [[Embeddings]]
