---
title: Database Engines
aliases:
  - Databases
  - DB Engines
tags:
  - tool
  - database
  - comparison
  - infrastructure
type: reference
status: complete
created: '2025-11-28'
---

# Database Engines

Choosing the right database for your data model and access patterns.

## Database Categories

| Type | Data Model | Best For |
|------|------------|----------|
| Relational (SQL) | Tables, rows, relations | Structured data, ACID, complex queries |
| Document | JSON/BSON documents | Flexible schemas, nested data |
| Key-Value | Simple key→value pairs | Caching, sessions, high throughput |
| Graph | Nodes and edges | Relationships, social networks |
| Time-Series | Timestamped data points | Metrics, IoT, monitoring |
| Vector | High-dimensional embeddings | AI/ML, semantic search |
| Wide-Column | Column families | Analytics, large-scale writes |

---

## Relational (SQL) Databases

### Overview

Tables with rows and columns. Schema-enforced. ACID transactions.

```
┌─────────────────────────────────────────┐
│  users                                  │
├────────┬──────────┬────────────────────┤
│ id     │ name     │ email              │
├────────┼──────────┼────────────────────┤
│ 1      │ Alice    │ alice@example.com  │
│ 2      │ Bob      │ bob@example.com    │
└────────┴──────────┴────────────────────┘
         │
         │ foreign key
         ▼
┌─────────────────────────────────────────┐
│  orders                                 │
├────────┬──────────┬────────────────────┤
│ id     │ user_id  │ total              │
└────────┴──────────┴────────────────────┘
```

### Major Engines

| Database | Best For | Notes |
|----------|----------|-------|
| PostgreSQL | General purpose | Most feature-rich, extensible |
| MySQL | Web applications | Widely deployed, MariaDB fork |
| SQLite | Embedded, local | Single file, no server |
| SQL Server | Enterprise/.NET | Microsoft ecosystem |
| Oracle | Enterprise | Legacy, expensive |
| CockroachDB | Distributed SQL | Postgres-compatible, scales horizontally |
| PlanetScale | Serverless MySQL | Vitess-based, branching |

### PostgreSQL vs MySQL

| Aspect | PostgreSQL | MySQL |
|--------|------------|-------|
| Standards compliance | High | Medium |
| JSON support | Excellent (JSONB) | Good |
| Full-text search | Built-in | Basic |
| Extensions | Rich (PostGIS, pgvector) | Limited |
| Replication | Logical + physical | Statement + row |
| Performance | Complex queries | Simple reads |
| Learning curve | Steeper | Easier |

### When to Use SQL

- Complex relationships between entities
- ACID transactions required
- Ad-hoc queries and reporting
- Data integrity is critical
- Well-defined schema

---

## Document Databases

### Overview

Store JSON-like documents. Flexible schemas. Nested data natural.

```json
{
  "_id": "user_123",
  "name": "Alice",
  "email": "alice@example.com",
  "addresses": [
    { "type": "home", "city": "Seattle" },
    { "type": "work", "city": "Portland" }
  ],
  "preferences": {
    "theme": "dark",
    "notifications": true
  }
}
```

### Major Engines

| Database | Best For | Notes |
|----------|----------|-------|
| MongoDB | General document store | Most popular, Atlas cloud |
| Couchbase | Mobile sync, caching | N1QL query language |
| Amazon DocumentDB | MongoDB-compatible on AWS | Managed, not true MongoDB |
| Azure Cosmos DB | Multi-model global | Multiple APIs |
| Firebase Firestore | Mobile/web apps | Real-time sync |

### Document vs Relational

| Aspect | Document | Relational |
|--------|----------|------------|
| Schema | Flexible | Fixed |
| Joins | Embedded or manual | Native |
| Scaling | Horizontal (sharding) | Vertical (usually) |
| Transactions | Limited (improving) | Full ACID |
| Query language | Varies | SQL standard |

### When to Use Documents

- Rapidly evolving schemas
- Nested/hierarchical data
- Content management
- Catalogs with varying attributes
- Prototyping

---

## Key-Value Stores

### Overview

Simplest model: key → value. Blazing fast for known-key access.

```
key: "session:abc123"  →  value: "{user_id: 1, expires: ...}"
key: "cache:user:1"    →  value: "{name: 'Alice', ...}"
key: "counter:visits"  →  value: 42
```

### Major Engines

| Database | Best For | Notes |
|----------|----------|-------|
| Redis | Caching, real-time | Data structures, pub/sub, Lua |
| Memcached | Simple caching | Pure cache, multi-threaded |
| Amazon DynamoDB | Serverless scale | AWS native, single-digit ms |
| etcd | Configuration, coordination | Kubernetes uses this |
| Valkey | Redis fork | Post-license-change community fork |

### Redis vs Memcached

| Aspect | Redis | Memcached |
|--------|-------|-----------|
| Data structures | Rich (lists, sets, hashes) | Strings only |
| Persistence | Optional | None |
| Pub/sub | Yes | No |
| Clustering | Yes | Client-side |
| Memory efficiency | Lower | Higher |
| Use case | Feature-rich cache | Simple cache |

### When to Use Key-Value

- Session storage
- Caching (query results, API responses)
- Rate limiting
- Leaderboards, counters
- Message queues (Redis)

---

## Graph Databases

### Overview

Nodes (entities) connected by edges (relationships). Traverse relationships efficiently.

```
     ┌─────────┐
     │  Alice  │
     └────┬────┘
          │ FOLLOWS
          ▼
     ┌─────────┐     WORKS_AT     ┌─────────┐
     │   Bob   │─────────────────▶│  Acme   │
     └────┬────┘                  └─────────┘
          │ LIKES
          ▼
     ┌─────────┐
     │ Post:42 │
     └─────────┘
```

### Major Engines

| Database | Best For | Notes |
|----------|----------|-------|
| Neo4j | General graph | Cypher query language, mature |
| Amazon Neptune | AWS managed | RDF and property graphs |
| ArangoDB | Multi-model | Graph + document + KV |
| TigerGraph | Enterprise analytics | Parallel processing |
| Memgraph | Real-time | In-memory, Cypher-compatible |

### Graph vs Relational

| Aspect | Graph | Relational |
|--------|-------|------------|
| Relationship traversal | O(1) per hop | JOIN overhead |
| Schema | Flexible | Fixed |
| Query complexity | Path queries easy | Many JOINs |
| Best for | Connected data | Tabular data |

### When to Use Graphs

- Social networks
- Recommendation engines
- Fraud detection
- Knowledge graphs
- Network/infrastructure mapping
- Shortest path problems

---

## Time-Series Databases

### Overview

Optimized for timestamped data points. High write throughput, time-range queries.

```
┌─────────────────────────────────────────────────────┐
│ metric: cpu_usage                                   │
│ tags: {host: "server1", region: "us-west"}          │
├─────────────────────┬───────────────────────────────┤
│ timestamp           │ value                         │
├─────────────────────┼───────────────────────────────┤
│ 2024-01-15 10:00:00 │ 45.2                          │
│ 2024-01-15 10:00:15 │ 47.8                          │
│ 2024-01-15 10:00:30 │ 52.1                          │
└─────────────────────┴───────────────────────────────┘
```

### Major Engines

| Database | Best For | Notes |
|----------|----------|-------|
| InfluxDB | General time-series | InfluxQL/Flux, popular |
| TimescaleDB | SQL + time-series | PostgreSQL extension |
| Prometheus | Metrics/monitoring | Pull-based, PromQL |
| ClickHouse | Analytics | Column-oriented, fast |
| QuestDB | High performance | SQL, low latency |
| Amazon Timestream | AWS serverless | Managed, auto-scaling |
| VictoriaMetrics | Prometheus-compatible | Better performance |

### Key Features

| Feature | Purpose |
|---------|---------|
| Time-based partitioning | Efficient range queries |
| Automatic downsampling | Reduce old data resolution |
| Retention policies | Auto-delete old data |
| Compression | Time-series specific algorithms |
| Continuous queries | Real-time aggregations |

### InfluxDB vs TimescaleDB

| Aspect | InfluxDB | TimescaleDB |
|--------|----------|-------------|
| Query language | InfluxQL/Flux | SQL |
| Underlying | Custom | PostgreSQL |
| Ecosystem | InfluxDB-specific | Postgres tools |
| JOINs | Limited | Full SQL |
| Learning curve | New language | Familiar SQL |

### When to Use Time-Series

- Application metrics
- IoT sensor data
- Financial tick data
- Log analytics
- Monitoring/observability
- Event tracking

---

## Vector Databases

### Overview

Store and search high-dimensional vectors (embeddings). Enable semantic/similarity search.

```
┌──────────────────────────────────────────────────────┐
│ document: "The quick brown fox"                      │
│ embedding: [0.12, -0.45, 0.78, ..., 0.33] (1536 dim) │
└──────────────────────────────────────────────────────┘

Query: "fast red animal" → embedding → nearest neighbors
```

### Major Engines

| Database | Best For | Notes |
|----------|----------|-------|
| Pinecone | Managed vector search | Serverless, easy |
| Weaviate | Hybrid search | GraphQL, modules |
| Milvus | Large scale | Open source, distributed |
| Qdrant | Performance | Rust-based, filtering |
| Chroma | Local/embedded | Python-native, simple |
| pgvector | PostgreSQL extension | Add vectors to Postgres |

### Vector Search Algorithms

| Algorithm | Trade-off |
|-----------|-----------|
| Flat/brute force | Exact, slow |
| IVF | Fast, approximate |
| HNSW | Fast, memory-heavy |
| PQ (Product Quantization) | Compressed, less accurate |

### When to Use Vectors

- Semantic search
- RAG (Retrieval Augmented Generation)
- Image/audio similarity
- Recommendation systems
- Anomaly detection
- Deduplication

---

## Wide-Column Stores

### Overview

Column families instead of rows. Optimized for writes and horizontal scale.

```
Row Key: "user:alice"
├── Column Family: profile
│   ├── name: "Alice"
│   └── email: "alice@example.com"
└── Column Family: activity
    ├── last_login: "2024-01-15"
    └── login_count: 42
```

### Major Engines

| Database | Best For | Notes |
|----------|----------|-------|
| Apache Cassandra | Write-heavy, global | No single point of failure |
| ScyllaDB | Cassandra-compatible | C++ rewrite, faster |
| HBase | Hadoop ecosystem | HDFS-based |
| Google Bigtable | GCP managed | Original wide-column |
| Amazon Keyspaces | Cassandra-compatible | AWS managed |

### When to Use Wide-Column

- Write-heavy workloads
- Time-series at scale
- Global distribution
- When you can't afford downtime
- IoT data ingestion

---

## Choosing a Database

### Decision Flow

```
┌─────────────────────────────────────────┐
│        What's your data shape?          │
└────────────────┬────────────────────────┘
                 │
    ┌────────────┼────────────┬────────────┬────────────┐
    ▼            ▼            ▼            ▼            ▼
 Tabular    Documents    Key→Value    Connected    Time-based
    │            │            │            │            │
    ▼            ▼            ▼            ▼            ▼
PostgreSQL   MongoDB      Redis       Neo4j      InfluxDB
  MySQL                  DynamoDB               TimescaleDB
```

### Quick Reference

| Need | Consider |
|------|----------|
| General purpose, don't know yet | PostgreSQL |
| Maximum flexibility | MongoDB or PostgreSQL (JSONB) |
| Caching layer | Redis |
| Relationships are key | Neo4j |
| Metrics/monitoring | Prometheus + InfluxDB |
| AI/semantic search | Pinecone or pgvector |
| Global scale, writes | Cassandra or DynamoDB |
| Embedded/local | SQLite |
| Analytics | ClickHouse |

### Multi-Model Trend

Modern databases blur boundaries:

| Database | Models |
|----------|--------|
| PostgreSQL | Relational + JSON + Vector (pgvector) |
| MongoDB | Document + Time-series |
| Azure Cosmos DB | Document + Graph + KV + Column |
| ArangoDB | Document + Graph + KV |
| Redis | KV + Streams + Search |

---

## Managed vs Self-Hosted

| Managed (DBaaS) | Self-Hosted |
|-----------------|-------------|
| Less ops burden | Full control |
| Auto-scaling | Cost savings at scale |
| Built-in backups | Customization |
| Vendor lock-in risk | Ops expertise needed |

### Major Managed Offerings

| Provider | Databases |
|----------|-----------|
| AWS | RDS, DynamoDB, DocumentDB, Neptune, Timestream |
| GCP | Cloud SQL, Firestore, Bigtable, Spanner |
| Azure | SQL Database, Cosmos DB, PostgreSQL |
| PlanetScale | MySQL (Vitess) |
| Neon | PostgreSQL (serverless) |
| Supabase | PostgreSQL + realtime |
| MongoDB Atlas | MongoDB |

---

## Related

- [[ORMs & Database Access]]
- [[Deployment]]
- [[Domains/Web Development|Web Development]]
