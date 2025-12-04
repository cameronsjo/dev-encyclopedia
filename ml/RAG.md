---
title: RAG
aliases:
  - Retrieval Augmented Generation
  - Retrieval-Augmented Generation
tags:
  - ai
  - ml
  - llm
  - architecture
type: reference
status: complete
created: '2025-11-28'
---

# RAG

Retrieval-Augmented Generation - grounding LLM responses with external knowledge.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Reduce hallucinations, add current knowledge |
| Components | Retriever + Generator (LLM) |
| Key tech | Vector embeddings, semantic search |
| Trade-off | Latency vs accuracy |

---

## Why RAG?

### LLM Limitations

| Problem | RAG Solution |
|---------|--------------|
| Knowledge cutoff | Retrieve current data |
| Hallucinations | Ground in real documents |
| No private data | Search your own corpus |
| Generic answers | Domain-specific context |

### RAG vs Fine-tuning

| Aspect | RAG | Fine-tuning |
|--------|-----|-------------|
| Update data | Easy (re-index) | Retrain model |
| Cost | Cheaper | Expensive |
| Latency | Higher (retrieval) | Lower |
| Accuracy | Good with quality data | Can overfit |
| Transparency | Can cite sources | Black box |

**Rule of thumb:** RAG for knowledge, fine-tuning for style/format.

---

## Architecture

### Basic RAG Pipeline

```
┌──────────────────────────────────────────────────────────────┐
│                        INDEXING (Offline)                     │
├──────────────────────────────────────────────────────────────┤
│                                                              │
│  Documents ──► Chunking ──► Embedding ──► Vector Store       │
│                                                              │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│                        QUERYING (Online)                      │
├──────────────────────────────────────────────────────────────┤
│                                                              │
│  Query ──► Embed ──► Search ──► Retrieved Chunks             │
│                                      │                       │
│                                      ▼                       │
│                    [Query + Chunks] ──► LLM ──► Response     │
│                                                              │
└──────────────────────────────────────────────────────────────┘
```

### Components

| Component | Purpose | Examples |
|-----------|---------|----------|
| Chunker | Split documents | Fixed size, semantic, recursive |
| Embedder | Text → vectors | OpenAI, Cohere, local models |
| Vector store | Store & search | Pinecone, Weaviate, pgvector |
| Retriever | Find relevant chunks | Similarity search, hybrid |
| Generator | Produce answer | GPT-4, Claude, Llama |

---

## Chunking Strategies

### Fixed Size

```python
# Simple but can break context
chunks = [text[i:i+512] for i in range(0, len(text), 512)]
```

### Recursive Character

Split by hierarchy: paragraphs → sentences → words.

```python
from langchain.text_splitter import RecursiveCharacterTextSplitter

splitter = RecursiveCharacterTextSplitter(
    chunk_size=1000,
    chunk_overlap=200,
    separators=["\n\n", "\n", " ", ""]
)
chunks = splitter.split_text(document)
```

### Semantic Chunking

Split by meaning, not characters.

| Strategy | Pros | Cons |
|----------|------|------|
| Fixed size | Simple, predictable | Breaks context |
| Recursive | Respects structure | May still split badly |
| Semantic | Best quality | Slower, more complex |

### Chunk Size Trade-offs

| Smaller Chunks | Larger Chunks |
|----------------|---------------|
| More precise retrieval | More context per chunk |
| More chunks to search | Fewer chunks |
| May lose context | May include noise |

**Sweet spot:** 256-1024 tokens with 10-20% overlap.

---

## Embedding Models

### Popular Models

| Model | Dimensions | Notes |
|-------|------------|-------|
| OpenAI text-embedding-3-small | 1536 | Good balance |
| OpenAI text-embedding-3-large | 3072 | Best quality |
| Cohere embed-v3 | 1024 | Multilingual |
| Voyage-2 | 1024 | Code-optimized |
| BGE-large | 1024 | Open source |
| E5-mistral-7b | 4096 | Open, high quality |
| all-MiniLM-L6-v2 | 384 | Fast, local |

### Local vs API

| Local | API |
|-------|-----|
| Free, private | Pay per token |
| Slower | Fast |
| Your hardware | Their hardware |
| sentence-transformers | OpenAI, Cohere |

---

## Vector Stores

### Comparison

| Store | Type | Best For |
|-------|------|----------|
| Pinecone | Managed | Production, scale |
| Weaviate | Managed/Self | Hybrid search |
| Qdrant | Self-hosted | Performance |
| Chroma | Embedded | Local dev |
| pgvector | PostgreSQL ext | Existing Postgres |
| Milvus | Self-hosted | Large scale |
| FAISS | Library | In-memory, research |

### pgvector Example

```sql
-- Enable extension
CREATE EXTENSION vector;

-- Create table
CREATE TABLE documents (
    id SERIAL PRIMARY KEY,
    content TEXT,
    embedding vector(1536)
);

-- Create index
CREATE INDEX ON documents
USING ivfflat (embedding vector_cosine_ops)
WITH (lists = 100);

-- Search
SELECT content, 1 - (embedding <=> $1) AS similarity
FROM documents
ORDER BY embedding <=> $1
LIMIT 5;
```

---

## Retrieval Strategies

### Basic Similarity

```python
results = vector_store.similarity_search(query, k=5)
```

### Hybrid Search

Combine semantic + keyword (BM25).

```
Score = α × semantic_score + (1-α) × keyword_score
```

### Multi-Query

Generate multiple query variations.

```python
# Original: "What is RAG?"
# Variations:
# - "Explain retrieval augmented generation"
# - "How does RAG work in LLMs?"
# - "RAG architecture overview"
```

### Reranking

Two-stage: fast retrieval → accurate reranking.

```python
# Stage 1: Get 100 candidates (fast)
candidates = vector_store.search(query, k=100)

# Stage 2: Rerank to top 5 (accurate)
reranked = reranker.rerank(query, candidates, top_k=5)
```

| Reranker | Notes |
|----------|-------|
| Cohere Rerank | API, high quality |
| BGE Reranker | Open source |
| Cross-encoder | Accurate, slow |

---

## Advanced Patterns

### Parent Document Retrieval

Embed small chunks, return parent document.

```
Embed: [chunk1, chunk2, chunk3]  (small, precise)
Return: [parent_document]        (full context)
```

### Contextual Compression

Compress retrieved docs to relevant parts only.

### Self-Query

LLM generates structured query from natural language.

```
Input: "Papers about RAG from 2024"
Generated: {
  query: "RAG retrieval augmented generation",
  filter: { year: 2024, type: "paper" }
}
```

### HyDE (Hypothetical Document Embeddings)

Generate hypothetical answer, embed that instead of query.

```
Query: "How does photosynthesis work?"
HyDE: [LLM generates hypothetical answer]
Search: Embed hypothetical answer, find similar real docs
```

---

## Evaluation

### Metrics

| Metric | Measures |
|--------|----------|
| Recall@K | Relevant docs in top K |
| MRR | Rank of first relevant doc |
| NDCG | Ranking quality |
| Faithfulness | Answer grounded in context |
| Relevance | Answer addresses query |

### RAGAS Framework

```python
from ragas import evaluate
from ragas.metrics import faithfulness, answer_relevancy, context_precision

results = evaluate(
    dataset,
    metrics=[faithfulness, answer_relevancy, context_precision]
)
```

---

## Common Pitfalls

| Pitfall | Solution |
|---------|----------|
| Chunks too large | Smaller chunks, overlap |
| Poor embeddings | Better model, domain-specific |
| Wrong K value | Tune based on evaluation |
| No reranking | Add reranker for quality |
| Ignoring metadata | Filter by date, source, type |
| Prompt doesn't use context | Explicit instructions |

---

## RAG Stack Examples

### Simple (Local)

```
Chroma + all-MiniLM + Ollama
```

### Production

```
Pinecone + OpenAI embeddings + GPT-4 + Cohere Rerank
```

### Open Source

```
Qdrant + BGE embeddings + Llama 3 + BGE Reranker
```

---

## When to Use RAG

**Strengths:**

- Current, updateable knowledge
- Cites sources
- Works with any LLM
- Private data stays private
- Cost-effective vs fine-tuning

**Considerations:**

- Adds latency
- Quality depends on retrieval
- Chunk strategy matters
- Needs maintenance (re-indexing)

**Best for:**

- Q&A over documents
- Customer support bots
- Code assistants
- Research assistants
- Any domain-specific knowledge

---

## Related

- [[LLMs & Transformers]]
- [[Agent Frameworks]]
- [[MCP Servers]]
- [[Database Engines]]
