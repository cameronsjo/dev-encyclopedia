---
title: Vector Databases
aliases:
  - Vector DB
  - Embedding Database
  - Similarity Search Database
tags:
  - ai
  - ml
  - database
  - vector
  - embeddings
  - similarity-search
type: reference
status: complete
created: 2025-11-30
---

# Vector Databases

Specialized databases optimized for storing, indexing, and querying high-dimensional vector embeddings with similarity search capabilities.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Store and retrieve vectors based on semantic similarity rather than exact matches |
| **Use Cases** | RAG systems, recommendation engines, image/audio search, anomaly detection |
| **Key Feature** | Approximate Nearest Neighbor (ANN) search with sub-millisecond latency |
| **Typical Dimensions** | 384-4096 dimensions (text embeddings: 384-1536, multimodal: 512-4096) |
| **Query Type** | k-nearest neighbors (kNN) with optional metadata filtering |
| **Scaling** | Billions of vectors with horizontal partitioning and replication |

## Core Concepts

### Vector Embeddings

Numerical representations of unstructured data (text, images, audio) in continuous vector space where semantic similarity corresponds to geometric proximity.

### Similarity Metrics

**Cosine Similarity**

- Measures angle between vectors (direction, not magnitude)
- Range: -1 to 1 (1 = identical direction)
- Best for: Normalized embeddings, text similarity
- Formula: `cos(θ) = (A·B) / (||A|| ||B||)`

**Euclidean Distance (L2)**

- Measures straight-line distance between points
- Range: 0 to ∞ (0 = identical)
- Best for: Spatial data, when magnitude matters
- Formula: `√Σ(Ai - Bi)²`

**Dot Product**

- Measures alignment and magnitude
- Range: -∞ to ∞ (higher = more similar)
- Best for: Pre-normalized vectors, recommendation scores
- Formula: `Σ(Ai × Bi)`

### Indexing Algorithms

**HNSW (Hierarchical Navigable Small World)**

- Graph-based index with multiple layers
- Fast queries (log complexity) with high recall
- Memory-intensive (stores graph in RAM)
- Best for: High-accuracy requirements

**IVF (Inverted File Index)**

- Partitions space into clusters (Voronoi cells)
- Search only relevant clusters
- Lower memory than HNSW
- Best for: Large-scale datasets with memory constraints

**PQ (Product Quantization)**

- Compresses vectors into compact codes
- Reduces memory 10-100x with minimal accuracy loss
- Combines with IVF (IVFPQ) for scalable search
- Best for: Billion-scale deployments

**Flat Index**

- Brute-force exact search
- Guaranteed 100% recall
- Best for: Small datasets (<100K vectors), baseline accuracy

### Hybrid Search

Combines vector similarity with traditional keyword/metadata filtering:

- **Pre-filtering**: Apply filters before vector search
- **Post-filtering**: Search first, filter results after
- **Sparse-Dense**: Combine BM25/keyword scores with vector similarity

## Vector Database Options

### Managed Cloud Services

| Database | Backing | Languages | Key Features | Pricing Model |
|----------|---------|-----------|--------------|---------------|
| **Pinecone** | Purpose-built | Python, JS/TS, Go, Java | Serverless scaling, namespaces, hybrid search | Usage-based (storage + queries) |
| **Weaviate Cloud** | Open-source Weaviate | Python, JS/TS, Go, Java | GraphQL API, named vectors, multi-tenancy | Usage-based (pod-hours) |
| **Qdrant Cloud** | Open-source Qdrant | Python, JS/TS, Rust, Go | Payload indexing, quantization, snapshots | Usage-based (pod-hours) |

### Self-Hosted / Open Source

| Database | Type | Languages | Key Features | Scaling |
|----------|------|-----------|--------------|---------|
| **Weaviate** | Standalone | Python, JS/TS, Go, Java | Modular vectorizers, CRUD, HNSW index | Horizontal (sharding) |
| **Qdrant** | Standalone | Python, JS/TS, Rust, Go | Quantization, filtering, gRPC/REST | Horizontal (sharding) |
| **Milvus** | Standalone | Python, Go, Java, Node | GPU support, disk-based index, Attu UI | Horizontal (distributed) |
| **Chroma** | Embedded/Server | Python, JS/TS | Embedded mode, collections, SQLite backend | Single-node (embedded) |

### Extensions to Traditional Databases

| Extension | Base Database | Languages | Key Features | When to Use |
|-----------|---------------|-----------|--------------|-------------|
| **pgvector** | PostgreSQL | SQL + client libs | Native SQL queries, ACID, joins with relational data | Existing PostgreSQL stack |
| **Redis Vector Search** | Redis | Any (Redis clients) | In-memory speed, hybrid queries, JSON integration | Real-time low-latency apps |
| **Elasticsearch** | Elasticsearch | Any (REST API) | Full-text + vector, aggregations, mature ecosystem | Existing ELK stack |

## Feature Comparison

### Similarity Search Performance

| Database | Index Types | Max Dimensions | Recall@10 | Queries/sec (1M vectors) |
|----------|-------------|----------------|-----------|--------------------------|
| Pinecone | HNSW variant | 20,000 | 99%+ | 10,000+ |
| Weaviate | HNSW | 65,536 | 95-99% | 5,000-10,000 |
| Qdrant | HNSW + quantization | 65,536 | 95-99% | 5,000-10,000 |
| Milvus | HNSW, IVF, DiskANN | 32,768 | 90-99% | 3,000-8,000 |
| Chroma | HNSW (HNSWlib) | Unlimited | 90-95% | 1,000-3,000 |
| pgvector | IVF, HNSW | 16,000 | 85-95% | 100-1,000 |
| FAISS | Flat, IVF, HNSW, PQ | Unlimited | 80-99% | 1,000-5,000 (in-memory) |

### Filtering & Hybrid Search

| Database | Metadata Filtering | Hybrid Search | Full-Text Search | Payload Indexing |
|----------|-------------------|---------------|------------------|------------------|
| Pinecone | ✅ Pre/post-filter | ✅ Sparse-dense | ❌ | ✅ |
| Weaviate | ✅ Pre-filter | ✅ BM25 + vector | ✅ | ✅ |
| Qdrant | ✅ Pre/post-filter | ✅ Custom scoring | ❌ | ✅ Advanced |
| Milvus | ✅ Scalar filtering | ✅ Hybrid search | ❌ | ✅ |
| Chroma | ✅ Where filters | ❌ | ❌ | ✅ Basic |
| pgvector | ✅ SQL WHERE | ✅ SQL + vector | ✅ (PostgreSQL) | ✅ (PostgreSQL) |

### Scaling & Operations

| Database | Sharding | Replication | Backup/Restore | Multi-Tenancy | GPU Support |
|----------|----------|-------------|----------------|---------------|-------------|
| Pinecone | ✅ Automatic | ✅ Automatic | ✅ Point-in-time | ✅ Namespaces | ❌ |
| Weaviate | ✅ Manual | ✅ | ✅ Backups | ✅ Tenants | ❌ |
| Qdrant | ✅ Manual | ✅ | ✅ Snapshots | ✅ Collections | ❌ |
| Milvus | ✅ Manual | ✅ | ✅ Backups | ✅ Partitions | ✅ |
| Chroma | ❌ | ❌ | ✅ Export | ❌ | ❌ |
| pgvector | ✅ (PostgreSQL) | ✅ (PostgreSQL) | ✅ (PostgreSQL) | ✅ (schemas) | ❌ |
| FAISS | ❌ (library) | ❌ | ✅ Save/load | ❌ | ✅ |

## When to Use

### Pinecone

**Strengths:**

- Zero infrastructure management (fully serverless)
- Predictable low latency at any scale
- Best-in-class query performance
- Strong hybrid search capabilities

**Considerations:**

- Highest cost at scale
- Vendor lock-in (proprietary)
- Limited control over infrastructure

**Best for:** Production RAG systems, enterprise applications requiring SLA guarantees, teams without DevOps capacity

### Weaviate

**Strengths:**

- Rich GraphQL API with complex queries
- Strong ecosystem and integrations
- Excellent documentation and community
- Flexible deployment (cloud or self-hosted)

**Considerations:**

- Higher learning curve for configuration
- Resource-intensive for large datasets
- Query performance varies with complexity

**Best for:** Complex knowledge graphs, multi-modal search, teams wanting open-source with managed option

### Qdrant

**Strengths:**

- Advanced filtering with payload indexing
- Efficient quantization (reduced memory)
- Rust-based performance and reliability
- Clean REST and gRPC APIs

**Considerations:**

- Smaller ecosystem than Pinecone/Weaviate
- Less mature cloud offering
- Documentation gaps for advanced features

**Best for:** Filtering-heavy applications, cost-conscious deployments, teams comfortable with Rust tooling

### Milvus

**Strengths:**

- Handles billion-scale datasets
- GPU acceleration support
- Mature distributed architecture
- LF AI & Data Foundation backing

**Considerations:**

- Complex deployment and operations
- Steep learning curve
- Higher resource requirements

**Best for:** Massive-scale deployments (10M+ vectors), GPU-accelerated workloads, research/academic use

### Chroma

**Strengths:**

- Embedded mode (no server required)
- Simplest developer experience
- Lightweight and fast iteration
- Perfect for prototyping

**Considerations:**

- Limited scalability (single-node)
- Basic filtering capabilities
- Not production-ready for large datasets

**Best for:** Prototypes, local development, small applications (<1M vectors), Jupyter notebooks

### pgvector

**Strengths:**

- Native PostgreSQL integration
- ACID transactions with vectors
- Join vectors with relational data
- Leverage existing PostgreSQL expertise

**Considerations:**

- Lower query performance than specialized DBs
- Limited to 16K dimensions
- Requires PostgreSQL tuning for scale

**Best for:** Existing PostgreSQL infrastructure, applications requiring ACID, moderate scale (<10M vectors)

### FAISS (Library, Not Database)

**Strengths:**

- Meta-backed, battle-tested library
- Fastest in-memory search
- Flexible index composition
- No network overhead

**Considerations:**

- Requires custom persistence layer
- No built-in filtering or metadata
- Single-machine memory limits
- Not a database (build your own)

**Best for:** Research, custom vector search services, batch processing, when building from scratch

## Decision Guide

| Requirement | Recommended Options |
|-------------|-------------------|
| **Prototype/MVP** | Chroma (embedded), Qdrant (Docker), pgvector (if using PostgreSQL) |
| **Production RAG (<1M vectors)** | Pinecone, Weaviate Cloud, Qdrant Cloud |
| **Production RAG (1-100M vectors)** | Pinecone, Weaviate, Qdrant, pgvector (optimized) |
| **Production RAG (100M+ vectors)** | Pinecone, Milvus, Weaviate (sharded) |
| **Complex filtering requirements** | Qdrant, Weaviate, pgvector |
| **Hybrid search (keyword + vector)** | Weaviate, Pinecone, pgvector + PostgreSQL FTS |
| **Existing PostgreSQL stack** | pgvector |
| **Cost-sensitive** | Self-hosted Qdrant, Weaviate, Chroma, pgvector |
| **Zero ops overhead** | Pinecone, Weaviate Cloud, Qdrant Cloud |
| **GPU acceleration** | Milvus, FAISS |
| **Multi-modal embeddings** | Weaviate, Milvus |
| **Real-time low latency (<10ms)** | Pinecone, Redis Vector Search |
| **ACID transactions** | pgvector |
| **Research/experimentation** | FAISS, Chroma, Qdrant |

## Implementation Considerations

### Embedding Dimensions

- **Small (384-512)**: All-MiniLM, smaller BERT models - faster, lower memory
- **Medium (768-1024)**: BERT-base, sentence-transformers - balanced performance
- **Large (1536-2048)**: OpenAI ada-002, Cohere embed - highest quality
- **Multimodal (512-4096)**: CLIP, ImageBind - cross-modal search

Higher dimensions = better semantic capture but slower queries and more memory.

### Index Tuning

**HNSW Parameters:**

- `ef_construction`: Build-time accuracy (64-512, higher = better recall)
- `M`: Graph connections per node (16-64, higher = better recall, more memory)
- `ef_search`: Query-time accuracy (10-500, higher = better recall, slower)

**IVF Parameters:**

- `nlist`: Number of clusters (√N to 4√N where N = dataset size)
- `nprobe`: Clusters to search (1-nlist, higher = better recall, slower)

**PQ Parameters:**

- `m`: Subvector count (8-64, must divide dimension count)
- `nbits`: Bits per code (8 is standard, 4 for extreme compression)

### Query Optimization

- **Batch queries**: Send multiple queries together (10-100x throughput)
- **Caching**: Cache frequent queries and embeddings
- **Filtering strategies**: Pre-filter reduces search space, post-filter guarantees recall
- **Quantization**: Reduce memory and speed up queries (5-10x memory reduction, minimal accuracy loss)
- **Partitioning**: Separate hot/cold data or multi-tenant isolation

### Cost Management

- **Vector dimensions**: Use smallest model that meets quality requirements
- **Quantization**: Compress vectors to reduce storage costs
- **Tiered storage**: Archive old vectors to cheaper storage
- **Batch operations**: Upsert and delete in batches (100-1000 vectors)
- **Monitoring**: Track query volume, storage growth, and compute usage

## Related

- [[RAG]] - Retrieval-Augmented Generation patterns using vector search
- [[Embeddings]] - Creating vector representations of data
- [[Database Engines]] - Comparison of relational and NoSQL databases
- [[Similarity Search]] - Algorithms and techniques for nearest neighbor search
- [[OpenTelemetry]] - Observability for vector database operations
