---
title: Database Indexing
aliases:
  - Indexing Strategies
  - Index Types
  - Query Optimization
tags:
  - tools
  - database
  - performance
  - optimization
type: reference
status: complete
created: "2025-12-16"
---

# Database Indexing

Strategies for creating and using indexes to optimize database query performance.

## Overview

| Aspect | Description |
|--------|-------------|
| **What** | Data structures for fast lookups |
| **Trade-off** | Faster reads vs slower writes + storage |
| **Goal** | Minimize disk I/O and row scans |

## How Indexes Work

### Without Index (Full Table Scan)

```
Query: WHERE email = 'alice@example.com'

Table: users (1M rows)
┌────┬────────────────────┬─────────┐
│ id │ email              │ name    │  ← Must scan ALL rows
├────┼────────────────────┼─────────┤
│ 1  │ bob@example.com    │ Bob     │  Check
│ 2  │ alice@example.com  │ Alice   │  ✓ Found!
│ 3  │ carol@example.com  │ Carol   │  Check (continues...)
│... │ ...                │ ...     │
│ 1M │ zack@example.com   │ Zack    │
└────┴────────────────────┴─────────┘
```

### With B-tree Index

```
Query: WHERE email = 'alice@example.com'

B-tree Index on email:
                    ┌─────────────┐
                    │   m...      │
                    └──────┬──────┘
              ┌────────────┼────────────┐
              ▼            ▼            ▼
        ┌─────────┐  ┌─────────┐  ┌─────────┐
        │ a-f     │  │ g-l     │  │ m-z     │
        └────┬────┘  └─────────┘  └─────────┘
             │
        ┌────▼────┐
        │ alice@  │ → Row pointer → Directly to row
        │ bob@    │
        └─────────┘

O(log n) lookups instead of O(n)
```

## Index Types

### B-tree Index

**Default in most databases. Good for:**

- Equality: `WHERE x = 5`
- Range: `WHERE x > 5`
- Sorting: `ORDER BY x`
- Prefix: `WHERE x LIKE 'abc%'`

```sql
-- PostgreSQL, MySQL, SQL Server
CREATE INDEX idx_email ON users(email);

-- Works with:
SELECT * FROM users WHERE email = 'test@example.com';
SELECT * FROM users WHERE email > 'a' AND email < 'b';
SELECT * FROM users ORDER BY email;
SELECT * FROM users WHERE email LIKE 'test%';
```

### Hash Index

**Fast equality lookups only.**

```sql
-- PostgreSQL
CREATE INDEX idx_email_hash ON users USING HASH(email);

-- Works with:
SELECT * FROM users WHERE email = 'test@example.com';

-- Does NOT work with:
SELECT * FROM users WHERE email > 'a';  -- No range
SELECT * FROM users ORDER BY email;      -- No sorting
```

### GIN (Generalized Inverted Index)

**For values containing multiple elements.**

```sql
-- PostgreSQL: Arrays, JSONB, full-text
CREATE INDEX idx_tags ON posts USING GIN(tags);
CREATE INDEX idx_data ON events USING GIN(data jsonb_path_ops);
CREATE INDEX idx_search ON articles USING GIN(to_tsvector('english', body));

-- Works with:
SELECT * FROM posts WHERE tags @> ARRAY['redis'];
SELECT * FROM events WHERE data @> '{"type": "click"}';
SELECT * FROM articles WHERE to_tsvector('english', body) @@ to_tsquery('database');
```

### GiST (Generalized Search Tree)

**For geometric and range data.**

```sql
-- PostgreSQL: PostGIS, ranges, full-text
CREATE INDEX idx_location ON places USING GIST(location);
CREATE INDEX idx_schedule ON events USING GIST(time_range);

-- Works with:
SELECT * FROM places WHERE location <@ box '((0,0),(10,10))';
SELECT * FROM events WHERE time_range && '[2024-01-01, 2024-01-31]';
```

### BRIN (Block Range Index)

**For large, naturally ordered data.**

```sql
-- PostgreSQL: Time-series, logs
CREATE INDEX idx_created ON logs USING BRIN(created_at);

-- Very small index size
-- Best when data is inserted in order
```

### Full-Text Index

```sql
-- PostgreSQL
CREATE INDEX idx_search ON articles USING GIN(to_tsvector('english', title || ' ' || body));

-- MySQL
CREATE FULLTEXT INDEX idx_search ON articles(title, body);

-- Query
SELECT * FROM articles WHERE MATCH(title, body) AGAINST('database performance');
```

## Composite Indexes

### Index Column Order Matters

```sql
CREATE INDEX idx_composite ON orders(customer_id, status, created_at);
```

**Leftmost Prefix Rule:**

| Query | Uses Index? |
|-------|-------------|
| `WHERE customer_id = 1` | ✅ Yes |
| `WHERE customer_id = 1 AND status = 'pending'` | ✅ Yes |
| `WHERE customer_id = 1 AND status = 'pending' AND created_at > '2024-01-01'` | ✅ Yes |
| `WHERE status = 'pending'` | ❌ No (missing leftmost) |
| `WHERE customer_id = 1 AND created_at > '2024-01-01'` | ⚠️ Partial (uses customer_id only) |

### Choosing Column Order

```sql
-- Put equality conditions first, range conditions last
-- High cardinality columns first (usually)

-- Good for: WHERE customer_id = ? AND status = ? ORDER BY created_at
CREATE INDEX idx_orders ON orders(customer_id, status, created_at);

-- Good for: WHERE customer_id = ? ORDER BY created_at DESC
CREATE INDEX idx_orders_recent ON orders(customer_id, created_at DESC);
```

## Covering Indexes

**Index contains all columns needed—no table lookup required.**

```sql
-- PostgreSQL (INCLUDE)
CREATE INDEX idx_orders_covering ON orders(customer_id)
    INCLUDE (total, status);

-- MySQL (columns in index)
CREATE INDEX idx_orders_covering ON orders(customer_id, total, status);

-- Query satisfied entirely from index:
SELECT total, status FROM orders WHERE customer_id = 123;
```

## Partial Indexes

**Index only a subset of rows.**

```sql
-- PostgreSQL
CREATE INDEX idx_active_users ON users(email)
    WHERE active = true;

-- Only 10% of users are active = much smaller index
-- Query must match the WHERE clause to use it:
SELECT * FROM users WHERE email = 'test@example.com' AND active = true;
```

## Expression Indexes

**Index on a computed value.**

```sql
-- PostgreSQL
CREATE INDEX idx_lower_email ON users(LOWER(email));

-- MySQL
CREATE INDEX idx_lower_email ON users((LOWER(email)));

-- Query must use same expression:
SELECT * FROM users WHERE LOWER(email) = 'test@example.com';
```

## Index Anti-Patterns

### Functions on Indexed Columns

```sql
-- ❌ Bad: Index not used
SELECT * FROM orders WHERE YEAR(created_at) = 2024;

-- ✅ Good: Index can be used
SELECT * FROM orders
WHERE created_at >= '2024-01-01' AND created_at < '2025-01-01';
```

### Leading Wildcards

```sql
-- ❌ Bad: Full scan
SELECT * FROM users WHERE email LIKE '%@gmail.com';

-- ✅ Good: Uses index
SELECT * FROM users WHERE email LIKE 'john%';

-- Alternative: Full-text search or reverse index
```

### OR Conditions

```sql
-- ❌ Potentially slow: May not use index
SELECT * FROM orders WHERE customer_id = 1 OR status = 'pending';

-- ✅ Better: UNION (if both columns indexed)
SELECT * FROM orders WHERE customer_id = 1
UNION
SELECT * FROM orders WHERE status = 'pending';
```

### Over-Indexing

```sql
-- ❌ Too many indexes:
CREATE INDEX idx1 ON users(email);
CREATE INDEX idx2 ON users(email, name);
CREATE INDEX idx3 ON users(email, name, created_at);
CREATE INDEX idx4 ON users(name);
CREATE INDEX idx5 ON users(name, email);

-- Problems:
-- - Slow INSERT/UPDATE/DELETE
-- - Increased storage
-- - Optimizer confusion
```

## Analyzing Index Usage

### PostgreSQL

```sql
-- Explain query
EXPLAIN (ANALYZE, BUFFERS) SELECT * FROM users WHERE email = 'test@example.com';

-- Index usage stats
SELECT relname, indexrelname, idx_scan, idx_tup_read, idx_tup_fetch
FROM pg_stat_user_indexes
ORDER BY idx_scan DESC;

-- Unused indexes
SELECT * FROM pg_stat_user_indexes WHERE idx_scan = 0;

-- Table statistics
ANALYZE users;
SELECT * FROM pg_stats WHERE tablename = 'users';
```

### MySQL

```sql
-- Explain query
EXPLAIN SELECT * FROM users WHERE email = 'test@example.com';

-- Index usage
SELECT * FROM sys.schema_index_statistics
WHERE table_schema = 'mydb';

-- Unused indexes
SELECT * FROM sys.schema_unused_indexes;

-- Update statistics
ANALYZE TABLE users;
```

## Index Maintenance

### When to Rebuild

| Scenario | Action |
|----------|--------|
| **Large deletes** | Rebuild to reclaim space |
| **Statistics stale** | ANALYZE table |
| **Index bloat (PostgreSQL)** | REINDEX or pg_repack |
| **Fragmentation (SQL Server)** | REORGANIZE or REBUILD |

### Commands

```sql
-- PostgreSQL
REINDEX INDEX idx_email;
REINDEX TABLE users;
VACUUM ANALYZE users;

-- MySQL
OPTIMIZE TABLE users;
ALTER TABLE users ENGINE=InnoDB;  -- Rebuild

-- SQL Server
ALTER INDEX idx_email ON users REORGANIZE;
ALTER INDEX idx_email ON users REBUILD;
```

## Decision Guide

| Scenario | Index Type |
|----------|------------|
| **Equality lookups** | B-tree or Hash |
| **Range queries** | B-tree |
| **Full-text search** | GIN/Full-text |
| **JSON/Array contains** | GIN |
| **Geospatial** | GiST |
| **Time-series (ordered)** | BRIN |
| **Frequently filtered subset** | Partial index |
| **Case-insensitive search** | Expression index |
| **Query needs specific columns** | Covering index |

## Related

- [[PostgreSQL]] — PostgreSQL specifics
- [[MySQL]] — MySQL specifics
- [[Database Engines]] — Database comparison
- [[Tools MOC]] — All tools
