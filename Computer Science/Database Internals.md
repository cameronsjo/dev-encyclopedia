---
title: Database Internals
aliases:
  - DBMS Internals
  - Database Architecture
  - Storage Engines
tags:
  - cs
  - database
  - fundamentals
  - systems
type: reference
status: complete
created: 2025-11-30
---

# Database Internals

Core architectural components and algorithms that power relational and NoSQL database systems.

## System Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    SQL/Query Interface                   │
├─────────────────────────────────────────────────────────┤
│  Parser → Optimizer → Executor                          │
│  (Query Planning & Execution)                           │
├─────────────────────────────────────────────────────────┤
│  Transaction Manager  │  Lock Manager                   │
│  (ACID, MVCC)        │  (Concurrency Control)          │
├─────────────────────────────────────────────────────────┤
│  Buffer Pool Manager  │  WAL (Write-Ahead Log)          │
│  (Page Cache)        │  (Durability)                   │
├─────────────────────────────────────────────────────────┤
│  Storage Engine                                         │
│  (B-tree, LSM-tree, Heap Files)                        │
├─────────────────────────────────────────────────────────┤
│  Disk I/O Subsystem                                     │
└─────────────────────────────────────────────────────────┘
```

## Storage Engines

### B-Tree Storage

**Architecture:**
- Pages organized in balanced tree structure
- Each page: fixed size (typically 4KB, 8KB, or 16KB)
- Internal nodes: keys + pointers to child pages
- Leaf nodes: keys + values (or row pointers)

**Characteristics:**

| Aspect | Details |
|--------|---------|
| Read Performance | O(log n) - excellent for point lookups and range scans |
| Write Performance | O(log n) - requires page splits/merges |
| Space Amplification | Low - 50-70% page fill factor typical |
| Write Amplification | High - updates require rewriting entire pages |
| Use Cases | OLTP, transactional workloads, random reads |

**Page Structure:**
```
Page Header (metadata, LSN, free space pointer)
├─ Slot Array (offsets to tuples)
├─ Free Space
└─ Tuple Data (grows upward from bottom)
```

**Optimizations:**
- **Clustered Index:** Table data stored in B-tree leaf nodes (primary key order)
- **Non-Clustered Index:** Leaf nodes contain pointers to heap tuples
- **Covering Index:** Include extra columns to avoid heap lookups
- **Prefix Compression:** Store common prefixes once per page

### LSM-Tree Storage

**Architecture:**
- **MemTable:** In-memory sorted structure (skip list or red-black tree)
- **Immutable MemTable:** Frozen for flushing to disk
- **SSTables:** Sorted String Tables on disk (immutable)
- **Compaction:** Merge SSTables to remove obsoletes

**Write Path:**
```
Write → WAL → MemTable → (flush) → L0 SSTable → (compact) → L1-Ln
```

**Characteristics:**

| Aspect | Details |
|--------|---------|
| Read Performance | O(log n × k) - must check multiple levels, Bloom filters help |
| Write Performance | O(log n) - append-only, sequential writes |
| Space Amplification | High - multiple versions until compaction |
| Write Amplification | Very high - data rewritten multiple times during compaction |
| Use Cases | Write-heavy workloads, time-series, append-only logs |

**Compaction Strategies:**
- **Leveled:** Merge adjacent levels (PostgreSQL, RocksDB default)
- **Size-Tiered:** Merge SSTables of similar size (Cassandra default)
- **Time-Window:** Compact by time buckets (time-series optimized)

### Comparison

| Feature | B-Tree | LSM-Tree |
|---------|--------|----------|
| Read Latency | ✅ Predictable | ❌ Variable (multiple levels) |
| Write Latency | ❌ Variable (page splits) | ✅ Consistent (append-only) |
| Write Throughput | ❌ Lower | ✅ Higher |
| Point Queries | ✅ Excellent | ❌ Good (Bloom filters required) |
| Range Scans | ✅ Excellent | ✅ Good |
| Space Efficiency | ✅ Better | ❌ Worse (temporary duplicates) |
| Update-in-Place | ✅ Yes | ❌ No (append new version) |

## Write-Ahead Log (WAL)

**Purpose:** Guarantee durability (the D in ACID) without synchronous disk writes on every transaction.

**Protocol:**
1. Transaction modifies pages in buffer pool (in-memory)
2. Log records written to WAL **before** dirty pages flushed to disk
3. Transaction commits only after WAL fsync completes
4. Dirty pages lazily flushed by checkpointer

**Log Sequence Number (LSN):**
- Monotonically increasing identifier for each log record
- Each page stores LSN of last modification
- Recovery: replay WAL records with LSN > page LSN

**WAL Record Types:**
- **INSERT/UPDATE/DELETE:** Physical or logical record of change
- **CHECKPOINT:** Marker for recovery starting point
- **COMMIT/ABORT:** Transaction outcome

**Optimization:**
- **Group Commit:** Batch multiple transactions into single fsync
- **WAL Compression:** Reduce log volume for network replication
- **WAL Archiving:** Ship logs to replicas or backup systems

## Buffer Pool

**Purpose:** Cache disk pages in memory to minimize expensive I/O operations.

**Architecture:**
```
Buffer Pool (fixed-size array of frames)
├─ Frame 0: [Page ID | Dirty Bit | Pin Count | Data (8KB)]
├─ Frame 1: [Page ID | Dirty Bit | Pin Count | Data (8KB)]
├─ ...
└─ Frame N

Hash Table: Page ID → Frame Number (fast lookup)
Replacement Policy: LRU, Clock, LRU-K
```

**Key Mechanisms:**

| Component | Purpose |
|-----------|---------|
| Pin Count | Prevent eviction of pages in use by queries |
| Dirty Bit | Track pages modified but not yet flushed to disk |
| Eviction Policy | Choose victim page when buffer pool full (LRU, Clock, 2Q) |
| Page Table | Map page IDs to buffer pool frames |

**Eviction Policies:**
- **LRU (Least Recently Used):** Simple but vulnerable to sequential scans
- **Clock:** Approximates LRU with lower overhead (single pass)
- **LRU-K:** Track K most recent accesses (PostgreSQL uses this)
- **2Q:** Separate queues for first-time and frequently accessed pages

**Challenges:**
- **Buffer Pool Pollution:** Large scans evict hot pages → use ring buffers for scans
- **Priority Inversion:** Low-priority queries evict high-priority data → weighted eviction

## Checkpoints

**Purpose:** Establish recovery points to minimize WAL replay time.

**Process:**
1. Write all dirty pages in buffer pool to disk
2. Write checkpoint record to WAL (includes oldest active transaction LSN)
3. Update control file with checkpoint LSN

**Recovery Algorithm (ARIES):**
1. **Analysis Phase:** Scan WAL from last checkpoint to identify dirty pages and active transactions
2. **Redo Phase:** Replay WAL to reconstruct buffer pool state at crash
3. **Undo Phase:** Roll back uncommitted transactions

**Checkpoint Strategies:**
- **Full Checkpoint:** Flush all dirty pages (blocks writes during flush)
- **Incremental Checkpoint:** Spread flushing over time (PostgreSQL)
- **Fuzzy Checkpoint:** Allow concurrent modifications (most modern systems)

## ACID Properties

### Atomicity

**Mechanism:** Write-Ahead Logging + Undo Logs

- **Commit:** Write COMMIT record to WAL, all changes become durable
- **Abort:** Apply undo log entries to roll back changes
- **Shadow Paging:** Copy-on-write alternative (SQLite uses this)

### Consistency

**Mechanism:** Constraint Enforcement + Triggers

- **Primary Key:** Uniqueness enforced via B-tree index
- **Foreign Key:** Check referential integrity on insert/update/delete
- **Check Constraints:** Validate expressions (e.g., `age >= 0`)
- **Triggers:** Execute code before/after modifications

### Isolation

**Mechanism:** Locking, MVCC, or Optimistic Concurrency Control

See Isolation Levels section below.

### Durability

**Mechanism:** Write-Ahead Log + fsync

- **WAL First:** Log records persisted before commit acknowledged
- **fsync:** Force OS to flush write cache to disk platters/SSD
- **Replication:** Sync replicas before acknowledging commit (quorum-based durability)

## Isolation Levels

| Level | Dirty Read | Non-Repeatable Read | Phantom Read | Implementation |
|-------|------------|---------------------|--------------|----------------|
| Read Uncommitted | ❌ Allowed | ❌ Allowed | ❌ Allowed | No read locks |
| Read Committed | ✅ Prevented | ❌ Allowed | ❌ Allowed | Short read locks or MVCC snapshots per statement |
| Repeatable Read | ✅ Prevented | ✅ Prevented | ❌ Allowed (varies) | Long read locks or MVCC snapshot per transaction |
| Serializable | ✅ Prevented | ✅ Prevented | ✅ Prevented | Strict 2PL or SSI (Serializable Snapshot Isolation) |

### Isolation Anomalies

**Dirty Read:** Read uncommitted changes from another transaction
```sql
-- T1: UPDATE accounts SET balance = 500 WHERE id = 1;
-- T2: SELECT balance FROM accounts WHERE id = 1; -- sees 500
-- T1: ROLLBACK; -- T2 saw data that never existed
```

**Non-Repeatable Read:** Same query returns different results within transaction
```sql
-- T1: SELECT balance FROM accounts WHERE id = 1; -- returns 1000
-- T2: UPDATE accounts SET balance = 500 WHERE id = 1; COMMIT;
-- T1: SELECT balance FROM accounts WHERE id = 1; -- returns 500 (different!)
```

**Phantom Read:** Range query returns different rows within transaction
```sql
-- T1: SELECT COUNT(*) FROM orders WHERE status = 'pending'; -- returns 10
-- T2: INSERT INTO orders (..., status = 'pending') ...; COMMIT;
-- T1: SELECT COUNT(*) FROM orders WHERE status = 'pending'; -- returns 11
```

## Multi-Version Concurrency Control (MVCC)

**Core Idea:** Keep multiple versions of each tuple to allow readers to access consistent snapshots without blocking writers.

**Tuple Versioning:**
```
Tuple Header
├─ xmin: Transaction ID that created this version
├─ xmax: Transaction ID that deleted/updated this version (or NULL)
├─ cmin/cmax: Command IDs within transaction
└─ ctid: Pointer to newer version (if updated)
```

**Snapshot Isolation:**
- Each transaction gets snapshot at start: `(xmin, xmax, active_xids)`
- Tuple visible if:
  - Created by committed transaction < snapshot xmin, OR
  - Created by current transaction, AND
  - Not deleted, OR deleted by uncommitted/future transaction

**Visibility Rules (PostgreSQL-style):**
1. If `xmin` is current transaction → visible
2. If `xmin` is aborted or in-progress → not visible
3. If `xmax` is NULL → visible
4. If `xmax` is current transaction → not visible
5. If `xmax` is committed and < snapshot → not visible
6. Otherwise → visible

**Trade-offs:**

| Aspect | MVCC | Locking |
|--------|------|---------|
| Read Performance | ✅ No blocking | ❌ Readers block writers |
| Write Performance | ❌ Version overhead | ✅ Less metadata |
| Space Overhead | ❌ High (old versions) | ✅ Low |
| Implementation | ❌ Complex visibility | ✅ Simpler |

## Lock Types

### Lock Granularity

| Level | Scope | Conflict | Use Case |
|-------|-------|----------|----------|
| Table | Entire table | High | DDL operations (ALTER, DROP) |
| Page | Storage page (8KB) | Medium | Bulk operations |
| Row | Single tuple | Low | OLTP transactions |
| Predicate | Query result set | Complex | Serializable isolation (phantom prevention) |

### Lock Modes

**Shared (S) Lock:** Read access, multiple transactions can hold simultaneously
**Exclusive (X) Lock:** Write access, blocks all other locks

**Intent Locks:** Signal intention to acquire finer-grained locks
- **IS (Intent Shared):** Plan to acquire S locks on rows
- **IX (Intent Exclusive):** Plan to acquire X locks on rows
- **SIX (Shared + Intent Exclusive):** Read entire table + update specific rows

**Compatibility Matrix:**

|     | S  | X  | IS | IX | SIX |
|-----|----|----|----|----|-----|
| S   | ✅ | ❌ | ✅ | ❌ | ❌  |
| X   | ❌ | ❌ | ❌ | ❌ | ❌  |
| IS  | ✅ | ❌ | ✅ | ✅ | ✅  |
| IX  | ❌ | ❌ | ✅ | ✅ | ❌  |
| SIX | ❌ | ❌ | ✅ | ❌ | ❌  |

### Two-Phase Locking (2PL)

**Protocol:**
1. **Growing Phase:** Acquire locks, cannot release any lock
2. **Shrinking Phase:** Release locks, cannot acquire any new lock

**Guarantees:** Conflict-serializable schedules (equivalent to some serial execution)

**Variants:**
- **Strict 2PL:** Hold all locks until commit/abort (prevents cascading aborts)
- **Strong Strict 2PL:** Strict 2PL + release in reverse acquisition order

## Deadlock Detection

**Scenario:**
```
T1: LOCK TABLE accounts (X)    | T2: LOCK TABLE orders (X)
T1: LOCK TABLE orders (X) ...  | T2: LOCK TABLE accounts (X) ...
    [WAITING]                  |     [WAITING]
                    ↓
              DEADLOCK!
```

**Detection Methods:**

| Approach | Mechanism | Trade-off |
|----------|-----------|-----------|
| Timeout | Abort transaction after N seconds waiting | ✅ Simple, ❌ Inefficient (wastes work) |
| Wait-For Graph | Build directed graph T1 → T2 (T1 waits for T2), detect cycles | ✅ Precise, ❌ Overhead |
| Deadlock Prevention | Impose ordering (e.g., always lock tables alphabetically) | ✅ No detection needed, ❌ Limits concurrency |

**Victim Selection:**
- Transaction with least work done (minimize wasted effort)
- Transaction with fewest locks held
- Youngest transaction (least time invested)

## Indexes

### B-Tree Index

**Default index type in most databases.**

**Structure:**
- Height-balanced tree, all leaves at same depth
- Internal nodes: keys + child pointers
- Leaf nodes: keys + row pointers (heap TID or clustered data)

**Operations:**
- **Search:** O(log n) - traverse from root to leaf
- **Insert:** O(log n) - find leaf, insert, split if full
- **Delete:** O(log n) - find leaf, remove, merge if underfull

**Range Scans:** Efficient via leaf-level linked list
```sql
SELECT * FROM users WHERE age BETWEEN 20 AND 30;
-- 1. Binary search for age = 20 in B-tree
-- 2. Scan linked list of leaves until age > 30
```

### Hash Index

**Structure:** Hash table mapping keys to row pointers

**Characteristics:**
- **Point Queries:** O(1) average case
- **Range Scans:** Not supported (hash destroys ordering)
- **Equality Only:** Cannot use for `<`, `>`, `BETWEEN`, `LIKE`

**Use Cases:** Unique constraints, exact-match lookups on high-cardinality columns

### GIN (Generalized Inverted Index)

**Purpose:** Index multi-value columns (arrays, JSONB, full-text search)

**Structure:**
- Maps each element to list of rows containing it
- Example: `tags = ['postgres', 'database']` creates entries:
  - `postgres → [row1, row2, ...]`
  - `database → [row1, row3, ...]`

**Queries:**
```sql
-- Find rows where tags contain 'postgres'
SELECT * FROM articles WHERE tags @> ARRAY['postgres'];

-- Full-text search
SELECT * FROM documents WHERE content @@ to_tsquery('database & performance');
```

### GiST (Generalized Search Tree)

**Purpose:** Index geometric data, ranges, nearest-neighbor searches

**Structure:** Balanced tree with predicate-based search (not just equality)

**Use Cases:**
- **Geometric:** PostGIS (points, polygons, spatial queries)
- **Range Types:** `daterange`, `int4range` overlaps
- **Nearest Neighbor:** Find K closest points

```sql
-- Find polygons overlapping a point
SELECT * FROM regions WHERE bounds @> point(10.5, 20.3);

-- K-nearest neighbors
SELECT * FROM stores ORDER BY location <-> point(user_lat, user_lon) LIMIT 5;
```

### Index Comparison

| Index Type | Point Query | Range Scan | Multi-Value | Space | Maintenance |
|------------|-------------|------------|-------------|-------|-------------|
| B-Tree | O(log n) | ✅ Excellent | ❌ | Low | Medium |
| Hash | O(1) | ❌ | ❌ | Low | Low |
| GIN | O(log n) | ❌ | ✅ Arrays, JSON, FTS | High | High (WAL volume) |
| GiST | O(log n) | ✅ | ✅ Geometric, ranges | Medium | Medium |

## Query Planning

### Planner Architecture

```
SQL Query
  ↓
Parser (syntax tree)
  ↓
Rewriter (view expansion, rules)
  ↓
Planner
  ├─ Generate candidate plans (scan methods, join orders)
  ├─ Cost estimation (CPU, I/O, memory)
  └─ Select cheapest plan
  ↓
Executor (row-by-row or vectorized)
  ↓
Result Set
```

### Scan Methods

| Method | Description | Cost | Use Case |
|--------|-------------|------|----------|
| Sequential Scan | Read entire table page-by-page | O(n) | Small tables, no index, large % of rows needed |
| Index Scan | Use index to find rows, fetch from heap | O(log n + k) | Selective queries (< 5-10% of rows) |
| Index-Only Scan | Read from index without heap access | O(log n + k) | Covering index (all columns in index) |
| Bitmap Scan | Build bitmap of matching TIDs, fetch in heap order | O(log n + k) | OR conditions, avoid random I/O |

### Join Algorithms

**Nested Loop Join:**
```
for each row r1 in R:
    for each row r2 in S where r2.key = r1.key:
        output (r1, r2)
```
- **Cost:** O(n × m) without index, O(n × log m) with index on S
- **Best for:** Small outer table, index on inner table

**Hash Join:**
```
1. Build phase: Create hash table from smaller table R
2. Probe phase: For each row in S, lookup in hash table
```
- **Cost:** O(n + m)
- **Best for:** Equi-joins, medium-to-large tables, no indexes

**Merge Join:**
```
1. Sort R and S by join key (if not already sorted)
2. Merge sorted runs
```
- **Cost:** O(n log n + m log m) if unsorted, O(n + m) if sorted
- **Best for:** Already sorted inputs (clustered index), range joins

### Cost Model

**Simplified Formula:**
```
Total Cost = (seq_page_cost × pages_read) +
             (random_page_cost × random_reads) +
             (cpu_tuple_cost × tuples_processed) +
             (cpu_operator_cost × operations)
```

**Statistics Used:**
- **reltuples:** Estimated row count
- **relpages:** Estimated page count
- **Histograms:** Distribution of values (for selectivity estimation)
- **Correlation:** Physical vs logical ordering (affects random I/O)

### Optimization Techniques

**Predicate Pushdown:** Move filters closer to data source
```sql
-- Before: Filter after join
SELECT * FROM (SELECT * FROM orders JOIN customers ...) WHERE status = 'shipped'

-- After: Filter before join (fewer rows joined)
SELECT * FROM (SELECT * FROM orders WHERE status = 'shipped') JOIN customers ...
```

**Projection Pushdown:** Select only needed columns early
```sql
-- Push projection down to scan
SELECT name FROM users; -- only read 'name' column, not entire row
```

**Join Reordering:** Choose optimal join order using dynamic programming
```sql
-- Given: A JOIN B JOIN C
-- Candidates: (A ⋈ B) ⋈ C, (A ⋈ C) ⋈ B, (B ⋈ C) ⋈ A, ...
-- Choose based on cardinality estimates and available indexes
```

**Subquery Unnesting:** Convert correlated subqueries to joins
```sql
-- Correlated (runs subquery per row)
SELECT * FROM orders o
WHERE EXISTS (SELECT 1 FROM customers c WHERE c.id = o.customer_id);

-- Unnested (single join)
SELECT o.* FROM orders o JOIN customers c ON c.id = o.customer_id;
```

## Vacuum and Compaction

### VACUUM (PostgreSQL)

**Problem:** MVCC creates dead tuples (old versions no longer visible to any transaction)

**VACUUM Process:**
1. Scan table to identify dead tuples
2. Mark dead tuple space as reusable (update free space map)
3. Update visibility map (tracks pages with no dead tuples)
4. Truncate empty pages at end of table (if possible)

**VACUUM FULL:**
- Rewrites entire table, reclaiming all dead space
- Requires exclusive lock (blocks reads/writes)
- Use sparingly (high I/O cost)

**AUTOVACUUM:** Background process that runs VACUUM automatically when:
- `dead_tuples > autovacuum_vacuum_threshold + (autovacuum_vacuum_scale_factor × reltuples)`

### Compaction (LSM-Trees)

**Problem:** Overlapping SSTables waste space and slow reads

**Leveled Compaction:**
1. Level 0: Flush MemTable to SSTables (overlapping ranges)
2. Level 1+: Non-overlapping SSTables within level
3. When level exceeds size threshold, compact into next level
4. Merge sort SSTables, discard tombstones and old versions

**Size-Tiered Compaction:**
- Group SSTables by similar size
- Merge when N SSTables accumulate in a tier
- Less write amplification, more space amplification

**Tombstones:** Deletion markers persisted until compaction (cannot delete immediately in immutable SSTables)

## Performance Tuning Strategies

### Buffer Pool Sizing

**Rule of Thumb:** Allocate 25-75% of system RAM
- Too small: High disk I/O (thrashing)
- Too large: OS page cache starved (double-buffering inefficiency)

### WAL Tuning

**Checkpoint Frequency:**
- More frequent: Faster recovery, higher I/O overhead
- Less frequent: Slower recovery, less overhead

**WAL Archiving:** Balance between RPO (Recovery Point Objective) and disk/network bandwidth

### Index Strategy

**When to Index:**
- ✅ Columns in WHERE clauses (high selectivity)
- ✅ Foreign keys (join performance)
- ✅ Columns in ORDER BY / GROUP BY
- ❌ Low-cardinality columns (e.g., boolean flags)
- ❌ Frequently updated columns (index maintenance overhead)

**Covering Indexes:** Include extra columns to avoid heap lookups
```sql
CREATE INDEX idx_users_email_name ON users(email) INCLUDE (name);
-- Query can satisfy: SELECT name FROM users WHERE email = '...'
```

### Query Optimization

**Use EXPLAIN ANALYZE:** Identify bottlenecks (seq scans, high-cost nodes, poor cardinality estimates)

**Cardinality Misestimates:** Update statistics with `ANALYZE` or increase `default_statistics_target`

**Avoid Implicit Conversions:**
```sql
-- BAD: Index on int column cannot be used
WHERE id = '123'  -- implicit cast: CAST(id AS text) = '123'

-- GOOD: Explicit type match
WHERE id = 123
```

## Related

- [[Database Engines]] - Specific database implementations (PostgreSQL, MySQL, MongoDB)
- [[Data Structures]] - Fundamental structures (B-trees, hash tables, skip lists)
- [[Computer Science MOC]] - Broader CS concepts

