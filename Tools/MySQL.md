---
title: MySQL
aliases:
  - MariaDB
tags:
  - tools
  - database
  - sql
  - relational
type: reference
status: complete
created: "2025-12-16"
---

# MySQL

World's most popular open-source relational database, known for simplicity, speed, and web application dominance.

## Overview

| Aspect | Details |
|--------|---------|
| **Type** | Relational (RDBMS) |
| **License** | GPL (Community) / Commercial |
| **Owner** | Oracle Corporation |
| **Fork** | MariaDB (community-driven) |
| **Default Port** | 3306 |
| **Strengths** | Simplicity, read performance, web apps |

## Storage Engines

| Engine | Use Case |
|--------|----------|
| **InnoDB** | Default, ACID, transactions |
| **MyISAM** | Read-heavy, full-text (legacy) |
| **Memory** | Temporary tables, caching |
| **Archive** | Compressed historical data |

### InnoDB Features

| Feature | Description |
|---------|-------------|
| **ACID** | Full transaction support |
| **Row-level locking** | High concurrency |
| **Foreign keys** | Referential integrity |
| **MVCC** | Non-blocking reads |
| **Crash recovery** | Automatic recovery |

## Data Types

```sql
-- Numeric
TINYINT, SMALLINT, MEDIUMINT, INT, BIGINT
DECIMAL(10,2)  -- Exact
FLOAT, DOUBLE  -- Approximate

-- String
CHAR(50)       -- Fixed length
VARCHAR(255)   -- Variable length
TEXT, MEDIUMTEXT, LONGTEXT

-- Date/Time
DATE, TIME, DATETIME, TIMESTAMP
YEAR

-- Binary
BLOB, MEDIUMBLOB, LONGBLOB
BINARY, VARBINARY

-- JSON (MySQL 5.7+)
JSON
```

## Indexing

### Index Types

| Type | Description |
|------|-------------|
| **B-tree** | Default, range queries |
| **Hash** | Memory engine only |
| **Full-text** | Text search |
| **Spatial** | Geographic data |

### Index Examples

```sql
-- Primary key
CREATE TABLE users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    email VARCHAR(255) UNIQUE,
    name VARCHAR(100)
);

-- Composite index
CREATE INDEX idx_name_email ON users(name, email);

-- Prefix index (for long strings)
CREATE INDEX idx_email_prefix ON users(email(20));

-- Full-text index
CREATE FULLTEXT INDEX idx_content ON articles(title, body);

-- Using EXPLAIN
EXPLAIN SELECT * FROM users WHERE email = 'test@example.com';
```

### Index Best Practices

| Practice | Rationale |
|----------|-----------|
| **Index WHERE columns** | Speed up filtering |
| **Leftmost prefix rule** | Composite index usage |
| **Avoid over-indexing** | Slow writes |
| **Use covering indexes** | Avoid table lookups |

## Query Optimization

### EXPLAIN Output

```sql
EXPLAIN FORMAT=JSON SELECT * FROM orders
WHERE customer_id = 123 AND status = 'pending';

-- Key fields:
-- type: ALL (bad) → index → range → ref → eq_ref → const (best)
-- key: Which index used
-- rows: Estimated rows scanned
-- Extra: Using where, Using index, Using filesort
```

### Common Optimizations

```sql
-- Use indexes for ORDER BY
SELECT * FROM orders ORDER BY created_at DESC LIMIT 10;
-- Needs: INDEX(created_at)

-- Avoid SELECT *
SELECT id, name FROM users WHERE id = 123;

-- Use LIMIT with OFFSET carefully
-- Bad for large offsets:
SELECT * FROM logs ORDER BY id LIMIT 10 OFFSET 1000000;
-- Better (keyset pagination):
SELECT * FROM logs WHERE id > 1000000 ORDER BY id LIMIT 10;

-- Avoid functions on indexed columns
-- Bad:
SELECT * FROM users WHERE YEAR(created_at) = 2024;
-- Good:
SELECT * FROM users WHERE created_at >= '2024-01-01'
                      AND created_at < '2025-01-01';
```

## Replication

### Master-Slave (Source-Replica)

```
Source (Write) ──binlog──► Replica (Read)
                               │
                               ├──► Replica 2
                               └──► Replica 3
```

### Replication Types

| Type | Description |
|------|-------------|
| **Async** | Default, some lag possible |
| **Semi-sync** | At least one replica confirms |
| **Group Replication** | Multi-primary, Paxos-based |

### Setup Example

```sql
-- On source
CREATE USER 'repl'@'%' IDENTIFIED BY 'password';
GRANT REPLICATION SLAVE ON *.* TO 'repl'@'%';
SHOW MASTER STATUS;

-- On replica
CHANGE MASTER TO
    MASTER_HOST='source.example.com',
    MASTER_USER='repl',
    MASTER_PASSWORD='password',
    MASTER_LOG_FILE='mysql-bin.000001',
    MASTER_LOG_POS=123;
START SLAVE;
```

## JSON Support

```sql
-- Create table with JSON
CREATE TABLE events (
    id INT AUTO_INCREMENT PRIMARY KEY,
    data JSON
);

-- Insert JSON
INSERT INTO events (data) VALUES ('{"type": "click", "page": "/home"}');

-- Query JSON
SELECT data->>'$.type' AS event_type FROM events;
SELECT * FROM events WHERE data->>'$.type' = 'click';

-- JSON functions
SELECT JSON_EXTRACT(data, '$.type') FROM events;
SELECT JSON_KEYS(data) FROM events;
SELECT JSON_ARRAY_LENGTH(data->'$.items') FROM events;

-- Generated columns for indexing
ALTER TABLE events ADD COLUMN event_type VARCHAR(50)
    GENERATED ALWAYS AS (data->>'$.type') STORED;
CREATE INDEX idx_event_type ON events(event_type);
```

## Performance Tuning

### Key Configuration

```ini
# my.cnf

[mysqld]
# InnoDB Buffer Pool (70-80% of RAM for dedicated server)
innodb_buffer_pool_size = 12G
innodb_buffer_pool_instances = 8

# Logging
innodb_log_file_size = 2G
innodb_flush_log_at_trx_commit = 1  # ACID (2 for performance)

# Connections
max_connections = 500
thread_cache_size = 50

# Query cache (disabled in 8.0+)
# query_cache_type = 0

# Temp tables
tmp_table_size = 256M
max_heap_table_size = 256M
```

### Monitoring Queries

```sql
-- Slow query log
SET GLOBAL slow_query_log = 'ON';
SET GLOBAL long_query_time = 1;

-- Process list
SHOW PROCESSLIST;
SHOW FULL PROCESSLIST;

-- InnoDB status
SHOW ENGINE INNODB STATUS;

-- Table statistics
SHOW TABLE STATUS LIKE 'orders';

-- Index usage
SELECT * FROM sys.schema_unused_indexes;
SELECT * FROM sys.schema_redundant_indexes;
```

## MySQL vs MariaDB

| Aspect | MySQL | MariaDB |
|--------|-------|---------|
| **Owner** | Oracle | Community |
| **Compatibility** | N/A | Drop-in (mostly) |
| **Features** | Conservative | More experimental |
| **Storage Engines** | InnoDB focus | More options |
| **JSON** | Native | Native |

## Comparison with PostgreSQL

| Feature | MySQL | PostgreSQL |
|---------|-------|------------|
| **Simplicity** | ✅ Easier | ⚠️ More complex |
| **Read Performance** | ✅ Excellent | ✅ Excellent |
| **Write Performance** | ✅ Good | ✅ Good |
| **SQL Standards** | ⚠️ Deviations | ✅ Excellent |
| **JSON** | ✅ Good | ✅ Better (JSONB) |
| **Replication** | ✅ Many options | ✅ Logical + Physical |
| **Extensions** | ⚠️ Limited | ✅ Rich |
| **Window Functions** | ✅ 8.0+ | ✅ Long-standing |

## When to Use

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Web applications** | LAMP stack, WordPress, etc. |
| **Read-heavy workloads** | Efficient reads |
| **Simplicity** | Easy to set up and manage |
| **Ecosystem** | Massive tooling support |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Complex queries** | PostgreSQL may be better |
| **Oracle ownership** | Some prefer MariaDB |

### Best For

- Web applications (WordPress, Drupal)
- Read-heavy workloads
- Simple CRUD applications
- Teams familiar with MySQL

## Related

- [[PostgreSQL]] — Alternative RDBMS
- [[Database Engines]] — Database comparison
- [[Database Indexing]] — Indexing strategies
- [[Tools MOC]] — All tools
