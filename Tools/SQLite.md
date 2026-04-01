---
title: SQLite
aliases:
  - SQLite3
  - SQLite Database
tags:
  - tool
  - database
  - sql
  - embedded
type: reference
status: complete
created: "2025-12-18"
---

# SQLite

Self-contained, serverless, zero-configuration SQL database engine.

## Overview

| Aspect | Details |
|--------|---------|
| **Type** | Embedded relational database |
| **Storage** | Single file |
| **License** | Public domain |
| **Written In** | C |
| **Size** | ~700 KB library |
| **Use Cases** | Mobile apps, browsers, IoT, testing |

## Key Characteristics

| Feature | Description |
|---------|-------------|
| **Serverless** | No separate server process |
| **Zero-config** | No setup or administration |
| **Self-contained** | Single library, no dependencies |
| **Transactional** | ACID compliant |
| **Cross-platform** | Runs everywhere |

## Architecture

```
┌─────────────────────────────────────┐
│           Application               │
├─────────────────────────────────────┤
│         SQLite Library              │
│  ┌────────────────────────────────┐ │
│  │ SQL Parser → Bytecode Compiler │ │
│  │              ↓                 │ │
│  │     Virtual Machine (VDBE)     │ │
│  │              ↓                 │ │
│  │      B-Tree Module             │ │
│  │              ↓                 │ │
│  │       Pager Module             │ │
│  │              ↓                 │ │
│  │      OS Interface (VFS)        │ │
│  └────────────────────────────────┘ │
├─────────────────────────────────────┤
│         Database File (.db)         │
└─────────────────────────────────────┘
```

## Data Types

### Type Affinity

| Affinity | Description |
|----------|-------------|
| `TEXT` | Character strings |
| `NUMERIC` | Integer or real based on value |
| `INTEGER` | Signed integer |
| `REAL` | Floating point |
| `BLOB` | Binary data as-is |

```sql
-- Dynamic typing
CREATE TABLE flexible (
    id INTEGER PRIMARY KEY,
    data ANY  -- Can store any type
);

INSERT INTO flexible VALUES (1, 'text');
INSERT INTO flexible VALUES (2, 42);
INSERT INTO flexible VALUES (3, 3.14);
```

## Common Operations

### Creating Database

```bash
# Create or open database
sqlite3 mydb.db

# In-memory database
sqlite3 :memory:
```

### Basic SQL

```sql
-- Create table
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    email TEXT UNIQUE,
    created_at TEXT DEFAULT CURRENT_TIMESTAMP
);

-- Insert
INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com');

-- Query
SELECT * FROM users WHERE name LIKE 'A%';

-- Update
UPDATE users SET email = 'new@example.com' WHERE id = 1;

-- Delete
DELETE FROM users WHERE id = 1;
```

### Indexes

```sql
-- Create index
CREATE INDEX idx_users_email ON users(email);

-- Unique index
CREATE UNIQUE INDEX idx_users_name ON users(name);

-- Composite index
CREATE INDEX idx_users_name_email ON users(name, email);

-- Partial index
CREATE INDEX idx_active_users ON users(name) WHERE active = 1;

-- Check query plan
EXPLAIN QUERY PLAN SELECT * FROM users WHERE email = 'test@example.com';
```

## Advanced Features

### JSON Support

```sql
-- Store JSON
CREATE TABLE events (
    id INTEGER PRIMARY KEY,
    data JSON
);

INSERT INTO events VALUES (1, '{"type": "click", "x": 100, "y": 200}');

-- Query JSON
SELECT json_extract(data, '$.type') FROM events;
SELECT data->>'$.type' FROM events;  -- SQLite 3.38+

-- JSON functions
SELECT json_array(1, 2, 3);           -- [1,2,3]
SELECT json_object('a', 1, 'b', 2);   -- {"a":1,"b":2}
```

### Full-Text Search (FTS5)

```sql
-- Create FTS table
CREATE VIRTUAL TABLE articles_fts USING fts5(
    title,
    content,
    tokenize='porter'
);

-- Populate
INSERT INTO articles_fts VALUES ('Hello World', 'This is the content...');

-- Search
SELECT * FROM articles_fts WHERE articles_fts MATCH 'hello';
SELECT * FROM articles_fts WHERE articles_fts MATCH 'title:hello';

-- Ranking
SELECT *, rank FROM articles_fts WHERE articles_fts MATCH 'search term'
ORDER BY rank;
```

### Window Functions

```sql
SELECT
    name,
    department,
    salary,
    ROW_NUMBER() OVER (PARTITION BY department ORDER BY salary DESC) as rank,
    SUM(salary) OVER (PARTITION BY department) as dept_total,
    AVG(salary) OVER () as company_avg
FROM employees;
```

### Common Table Expressions

```sql
-- Recursive CTE for hierarchical data
WITH RECURSIVE subordinates AS (
    SELECT id, name, manager_id, 0 as level
    FROM employees
    WHERE id = 1  -- Start with CEO

    UNION ALL

    SELECT e.id, e.name, e.manager_id, s.level + 1
    FROM employees e
    JOIN subordinates s ON e.manager_id = s.id
)
SELECT * FROM subordinates;
```

## Transactions & Locking

### Transaction Modes

```sql
-- Deferred (default) - lock acquired on first read/write
BEGIN DEFERRED TRANSACTION;

-- Immediate - write lock acquired immediately
BEGIN IMMEDIATE TRANSACTION;

-- Exclusive - exclusive lock immediately
BEGIN EXCLUSIVE TRANSACTION;

COMMIT;
-- or
ROLLBACK;
```

### Write-Ahead Logging (WAL)

```sql
-- Enable WAL mode (recommended for concurrency)
PRAGMA journal_mode = WAL;

-- Check mode
PRAGMA journal_mode;

-- Checkpoint
PRAGMA wal_checkpoint(TRUNCATE);
```

| Mode | Readers During Write | Writers Concurrent | Crash Recovery |
|------|---------------------|-------------------|----------------|
| **DELETE** | Blocked | No | Rollback journal |
| **WAL** | Allowed | No (but non-blocking) | WAL file |

## Performance Tuning

### Pragmas

```sql
-- Memory settings
PRAGMA cache_size = -64000;     -- 64MB cache
PRAGMA temp_store = MEMORY;      -- Temp tables in RAM

-- Synchronization (trade durability for speed)
PRAGMA synchronous = NORMAL;     -- or OFF for max speed

-- Enable foreign keys
PRAGMA foreign_keys = ON;

-- Analyze for query planner
ANALYZE;

-- Optimize database
VACUUM;
```

### Bulk Operations

```sql
-- Wrap in transaction (much faster)
BEGIN TRANSACTION;
INSERT INTO users VALUES (...);
INSERT INTO users VALUES (...);
-- ... thousands more
COMMIT;

-- Use prepared statements (in code)
-- Disable sync for bulk loads
PRAGMA synchronous = OFF;
-- Re-enable after
PRAGMA synchronous = NORMAL;
```

## Language Bindings

### Python

```python
import sqlite3

# Connect
conn = sqlite3.connect('mydb.db')
conn.row_factory = sqlite3.Row  # Dict-like access

# Execute
cursor = conn.cursor()
cursor.execute('SELECT * FROM users WHERE id = ?', (1,))
user = cursor.fetchone()

# Context manager
with conn:
    conn.execute('INSERT INTO users (name) VALUES (?)', ('Bob',))
    # Auto-commits on success, rollback on exception

conn.close()
```

### Node.js (better-sqlite3)

```javascript
const Database = require('better-sqlite3');
const db = new Database('mydb.db');

// Synchronous API (better-sqlite3)
const user = db.prepare('SELECT * FROM users WHERE id = ?').get(1);
const users = db.prepare('SELECT * FROM users').all();

// Transaction
const insertMany = db.transaction((users) => {
  const stmt = db.prepare('INSERT INTO users (name) VALUES (?)');
  for (const user of users) stmt.run(user.name);
});
insertMany([{ name: 'Alice' }, { name: 'Bob' }]);

db.close();
```

### Go

```go
import (
    "database/sql"
    _ "github.com/mattn/go-sqlite3"
)

db, err := sql.Open("sqlite3", "./mydb.db")
defer db.Close()

// Query
rows, _ := db.Query("SELECT id, name FROM users")
defer rows.Close()

for rows.Next() {
    var id int
    var name string
    rows.Scan(&id, &name)
}

// Exec
db.Exec("INSERT INTO users (name) VALUES (?)", "Alice")
```

## Limitations

| Limitation | Details |
|------------|---------|
| **Concurrent writes** | Single writer at a time |
| **Network access** | Local file only (no client-server) |
| **Database size** | Theoretical 281 TB, practical ~1 TB |
| **Row size** | Default 1 GB max |
| **Columns** | 2000 max per table |

## Use Cases

| Use Case | Why SQLite |
|----------|------------|
| **Mobile apps** | iOS/Android default, offline-first |
| **Desktop apps** | Browsers, editors, games |
| **Embedded systems** | Small footprint, zero-config |
| **Testing** | Fast, in-memory, disposable |
| **Data analysis** | Import CSV, query with SQL |
| **Application cache** | Structured data caching |
| **File format** | Application file format |

## SQLite vs Server Databases

| Aspect | SQLite | PostgreSQL/MySQL |
|--------|--------|------------------|
| **Setup** | None | Server installation |
| **Concurrency** | Limited | High |
| **Network** | Local only | Client-server |
| **Scalability** | Single machine | Distributed possible |
| **Best for** | Embedded, edge | Multi-user, web |

## Related

- [[PostgreSQL]] — Enterprise SQL database
- [[MySQL]] — Popular web database
- [[Database Engines]] — Database comparison
- [[Tools MOC]] — All tools
