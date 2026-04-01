---
title: PostgreSQL
aliases:
  - Postgres
  - PG
tags:
  - tools
  - database
  - sql
  - relational
type: reference
status: complete
created: "2025-12-16"
---

# PostgreSQL

Advanced open-source relational database known for standards compliance, extensibility, and robust feature set.

## Overview

| Aspect | Details |
|--------|---------|
| **Type** | Relational (ORDBMS) |
| **License** | PostgreSQL License (permissive) |
| **Current Version** | 16.x (2024) |
| **Default Port** | 5432 |
| **Written In** | C |
| **Strengths** | Standards, extensibility, data integrity |

## Key Features

| Feature | Description |
|---------|-------------|
| **ACID Compliant** | Full transaction support |
| **MVCC** | Multi-Version Concurrency Control |
| **Extensible** | Custom types, operators, functions |
| **JSON/JSONB** | Native JSON with indexing |
| **Full-Text Search** | Built-in FTS capabilities |
| **PostGIS** | Geospatial extension |
| **Partitioning** | Table partitioning |
| **Logical Replication** | Fine-grained replication |

## Data Types

### Common Types

| Type | Description |
|------|-------------|
| `INTEGER` / `BIGINT` | Whole numbers |
| `NUMERIC(p,s)` | Exact decimal |
| `TEXT` / `VARCHAR(n)` | Strings |
| `BOOLEAN` | True/false |
| `TIMESTAMP[TZ]` | Date and time |
| `UUID` | Universally unique ID |
| `JSONB` | Binary JSON |
| `ARRAY` | Array of any type |

### PostgreSQL-Specific Types

```sql
-- Arrays
CREATE TABLE tags (
    id SERIAL PRIMARY KEY,
    name TEXT,
    keywords TEXT[]
);
INSERT INTO tags VALUES (1, 'tech', ARRAY['postgres', 'database']);
SELECT * FROM tags WHERE 'postgres' = ANY(keywords);

-- JSONB
CREATE TABLE events (
    id SERIAL PRIMARY KEY,
    data JSONB
);
INSERT INTO events (data) VALUES ('{"type": "click", "page": "/home"}');
SELECT data->>'type' FROM events;
SELECT * FROM events WHERE data @> '{"type": "click"}';

-- Range types
CREATE TABLE reservations (
    id SERIAL PRIMARY KEY,
    room_id INT,
    during TSRANGE
);
INSERT INTO reservations VALUES (1, 101, '[2024-01-01 14:00, 2024-01-01 16:00)');
SELECT * FROM reservations WHERE during && '[2024-01-01 15:00, 2024-01-01 17:00)';

-- UUID
CREATE TABLE users (
    id UUID DEFAULT gen_random_uuid() PRIMARY KEY,
    email TEXT UNIQUE
);
```

## Indexing

### Index Types

| Type | Use Case |
|------|----------|
| **B-tree** | Default, equality and range |
| **Hash** | Equality only |
| **GiST** | Geometric, full-text search |
| **GIN** | Arrays, JSONB, full-text |
| **BRIN** | Large sorted tables |

### Index Examples

```sql
-- B-tree (default)
CREATE INDEX idx_users_email ON users(email);

-- Partial index
CREATE INDEX idx_active_users ON users(email)
    WHERE active = true;

-- Expression index
CREATE INDEX idx_users_lower_email ON users(LOWER(email));

-- GIN for JSONB
CREATE INDEX idx_events_data ON events USING GIN(data);

-- GIN for array
CREATE INDEX idx_tags_keywords ON tags USING GIN(keywords);

-- BRIN for time-series
CREATE INDEX idx_logs_created ON logs USING BRIN(created_at);

-- Covering index (INCLUDE)
CREATE INDEX idx_orders_customer ON orders(customer_id)
    INCLUDE (total, status);
```

## Performance

### EXPLAIN ANALYZE

```sql
EXPLAIN (ANALYZE, BUFFERS, FORMAT TEXT)
SELECT * FROM orders WHERE customer_id = 123;

-- Output:
-- Index Scan using idx_orders_customer on orders
--   Index Cond: (customer_id = 123)
--   Buffers: shared hit=3
--   Planning Time: 0.1 ms
--   Execution Time: 0.05 ms
```

### Query Optimization Tips

| Technique | When to Use |
|-----------|-------------|
| **Use indexes** | Filter/sort columns |
| **Partial indexes** | Frequently filtered subset |
| **Covering indexes** | Avoid table lookups |
| **LIMIT** | Pagination |
| **Materialized views** | Complex aggregations |
| **Connection pooling** | High connection count |

### Configuration Tuning

```ini
# postgresql.conf

# Memory
shared_buffers = 4GB            # 25% of RAM
effective_cache_size = 12GB     # 75% of RAM
work_mem = 256MB                # Per operation
maintenance_work_mem = 1GB      # For VACUUM, CREATE INDEX

# WAL
wal_buffers = 64MB
checkpoint_completion_target = 0.9

# Planner
random_page_cost = 1.1          # For SSD
effective_io_concurrency = 200   # For SSD

# Connections
max_connections = 200
```

## Advanced Features

### CTEs and Window Functions

```sql
-- CTE (Common Table Expression)
WITH monthly_sales AS (
    SELECT DATE_TRUNC('month', created_at) AS month,
           SUM(total) AS revenue
    FROM orders
    GROUP BY 1
)
SELECT month, revenue,
       LAG(revenue) OVER (ORDER BY month) AS prev_month
FROM monthly_sales;

-- Window functions
SELECT customer_id, total,
       ROW_NUMBER() OVER (PARTITION BY customer_id ORDER BY total DESC) AS rank,
       SUM(total) OVER (PARTITION BY customer_id) AS customer_total,
       AVG(total) OVER () AS overall_avg
FROM orders;
```

### Full-Text Search

```sql
-- Create search column
ALTER TABLE articles ADD COLUMN search_vector tsvector;

UPDATE articles SET search_vector =
    to_tsvector('english', title || ' ' || body);

CREATE INDEX idx_articles_search ON articles USING GIN(search_vector);

-- Search
SELECT title, ts_rank(search_vector, query) AS rank
FROM articles, to_tsquery('english', 'postgres & performance') query
WHERE search_vector @@ query
ORDER BY rank DESC;
```

### Partitioning

```sql
-- Range partitioning
CREATE TABLE logs (
    id BIGSERIAL,
    created_at TIMESTAMP NOT NULL,
    message TEXT
) PARTITION BY RANGE (created_at);

CREATE TABLE logs_2024_01 PARTITION OF logs
    FOR VALUES FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE logs_2024_02 PARTITION OF logs
    FOR VALUES FROM ('2024-02-01') TO ('2024-03-01');

-- Automatic partition creation (pg_partman extension)
```

### LISTEN/NOTIFY

```sql
-- Publisher
NOTIFY order_created, '{"order_id": 123}';

-- Subscriber (application code)
LISTEN order_created;
-- Receives: {"order_id": 123}
```

## Replication

### Streaming Replication

```
Primary ──WAL──► Standby (sync/async)
                    │
                    ├──► Standby 2
                    └──► Standby 3
```

### Logical Replication

```sql
-- Publisher
CREATE PUBLICATION my_pub FOR TABLE orders, customers;

-- Subscriber
CREATE SUBSCRIPTION my_sub
    CONNECTION 'host=primary dbname=mydb'
    PUBLICATION my_pub;
```

## Extensions

| Extension | Purpose |
|-----------|---------|
| **PostGIS** | Geospatial data |
| **pg_trgm** | Fuzzy string matching |
| **pgcrypto** | Cryptographic functions |
| **pg_stat_statements** | Query statistics |
| **timescaledb** | Time-series |
| **pgvector** | Vector similarity search |
| **pg_partman** | Partition management |

```sql
-- Enable extension
CREATE EXTENSION pg_stat_statements;
CREATE EXTENSION pgvector;

-- Vector search example
CREATE TABLE items (
    id SERIAL PRIMARY KEY,
    embedding vector(1536)
);

SELECT * FROM items
ORDER BY embedding <-> '[0.1, 0.2, ...]'::vector
LIMIT 10;
```

## Comparison

| Feature | PostgreSQL | MySQL |
|---------|------------|-------|
| **Standards** | ✅ Excellent | ⚠️ Good |
| **JSON** | ✅ JSONB with indexes | ✅ JSON |
| **Full-text** | ✅ Built-in | ✅ Built-in |
| **Replication** | ✅ Logical + Physical | ✅ Multiple options |
| **Extensions** | ✅ Rich ecosystem | ⚠️ Limited |
| **Performance** | ✅ Complex queries | ✅ Simple queries |

## When to Use

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Complex queries** | Advanced SQL, CTEs, window functions |
| **Data integrity** | Strong constraints, transactions |
| **Extensibility** | Custom types, PostGIS, vectors |
| **Standards** | Most SQL-compliant |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Connection overhead** | Need connection pooling |
| **Replication complexity** | More setup than MySQL |

### Best For

- Complex analytical queries
- Geospatial applications (PostGIS)
- JSON document storage with SQL
- Applications requiring strong data integrity
- Vector/AI applications (pgvector)

## Related

- [[MySQL]] — Alternative RDBMS
- [[Database Engines]] — Database comparison
- [[Database Indexing]] — Indexing strategies
- [[Tools MOC]] — All tools
