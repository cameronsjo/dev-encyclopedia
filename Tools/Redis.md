---
title: Redis
aliases:
  - Redis Cache
  - Redis Database
tags:
  - tools
  - database
  - cache
  - nosql
  - key-value
type: reference
status: complete
created: "2025-12-16"
---

# Redis

In-memory data structure store used as cache, database, message broker, and streaming engine.

## Overview

| Aspect | Details |
|--------|---------|
| **Type** | Key-value / Data structure store |
| **Storage** | In-memory (with persistence options) |
| **License** | BSD (Redis 7.2+: RSALv2/SSPLv1) |
| **Default Port** | 6379 |
| **Protocol** | RESP (Redis Serialization Protocol) |
| **Strengths** | Speed, data structures, simplicity |

## Data Structures

### Strings

```redis
SET user:1:name "Alice"
GET user:1:name            # "Alice"

INCR page:views            # Atomic increment
INCRBY page:views 10       # Increment by 10

SETEX session:abc 3600 "data"  # Expires in 1 hour
SETNX lock:resource "holder"   # Set if not exists

MSET key1 "v1" key2 "v2"   # Multiple set
MGET key1 key2             # Multiple get
```

### Lists

```redis
LPUSH queue:jobs "job1"    # Push left
RPUSH queue:jobs "job2"    # Push right
LPOP queue:jobs            # Pop left
RPOP queue:jobs            # Pop right
BRPOP queue:jobs 30        # Blocking pop (30s timeout)

LRANGE queue:jobs 0 -1     # Get all
LLEN queue:jobs            # Length
LINDEX queue:jobs 0        # Get by index
```

### Sets

```redis
SADD tags:post:1 "redis" "database" "cache"
SMEMBERS tags:post:1       # Get all members
SISMEMBER tags:post:1 "redis"  # Check membership

SINTER tags:post:1 tags:post:2  # Intersection
SUNION tags:post:1 tags:post:2  # Union
SDIFF tags:post:1 tags:post:2   # Difference

SCARD tags:post:1          # Cardinality (count)
SRANDMEMBER tags:post:1    # Random member
```

### Sorted Sets

```redis
ZADD leaderboard 100 "alice" 85 "bob" 92 "charlie"
ZRANGE leaderboard 0 -1 WITHSCORES    # Ascending
ZREVRANGE leaderboard 0 2 WITHSCORES  # Top 3

ZSCORE leaderboard "alice"   # Get score
ZINCRBY leaderboard 5 "bob"  # Increment score
ZRANK leaderboard "alice"    # Rank (0-indexed)

ZRANGEBYSCORE leaderboard 80 100  # By score range
ZCOUNT leaderboard 80 100         # Count in range
```

### Hashes

```redis
HSET user:1 name "Alice" email "alice@example.com" age 30
HGET user:1 name           # "Alice"
HGETALL user:1             # All fields and values
HMGET user:1 name email    # Multiple fields

HINCRBY user:1 age 1       # Increment field
HDEL user:1 email          # Delete field
HEXISTS user:1 name        # Check field exists
HKEYS user:1               # All field names
```

### Streams

```redis
XADD events * type "click" page "/home"
XADD events * type "view" page "/products"

XREAD COUNT 10 STREAMS events 0  # Read from beginning
XREAD BLOCK 5000 STREAMS events $  # Block for new

# Consumer groups
XGROUP CREATE events mygroup $ MKSTREAM
XREADGROUP GROUP mygroup consumer1 COUNT 1 STREAMS events >
XACK events mygroup 1234567890-0
```

## Pub/Sub

```redis
# Subscriber
SUBSCRIBE news:tech news:sports
PSUBSCRIBE news:*          # Pattern subscribe

# Publisher
PUBLISH news:tech "Redis 8.0 released!"
```

## Caching Patterns

### Cache-Aside (Lazy Loading)

```python
def get_user(user_id):
    # Try cache first
    cached = redis.get(f"user:{user_id}")
    if cached:
        return json.loads(cached)

    # Cache miss - load from DB
    user = db.query("SELECT * FROM users WHERE id = ?", user_id)
    redis.setex(f"user:{user_id}", 3600, json.dumps(user))
    return user
```

### Write-Through

```python
def update_user(user_id, data):
    # Update DB
    db.execute("UPDATE users SET ... WHERE id = ?", user_id)

    # Update cache
    redis.setex(f"user:{user_id}", 3600, json.dumps(data))
```

### Write-Behind (Write-Back)

```python
def update_user(user_id, data):
    # Update cache immediately
    redis.setex(f"user:{user_id}", 3600, json.dumps(data))

    # Queue DB write for later
    redis.rpush("db:write:queue", json.dumps({
        "table": "users",
        "id": user_id,
        "data": data
    }))
```

## Distributed Locking

### Basic Lock

```redis
SET lock:resource "owner" NX EX 30
# NX = only if not exists
# EX = expire in 30 seconds

# Release (only if owner)
# Use Lua script for atomicity
```

### Redlock Algorithm

```python
# Acquire lock on N/2+1 instances
# Use if: Distributed system, need strong guarantees
# Libraries: redlock-py, redisson
```

## Persistence

### RDB (Snapshots)

```redis
# redis.conf
save 900 1      # Save if 1 key changed in 900s
save 300 10     # Save if 10 keys changed in 300s
save 60 10000   # Save if 10000 keys changed in 60s

dbfilename dump.rdb
```

### AOF (Append-Only File)

```redis
# redis.conf
appendonly yes
appendfsync everysec  # Options: always, everysec, no
```

### Comparison

| Aspect | RDB | AOF |
|--------|-----|-----|
| **Recovery Speed** | ✅ Fast | ⚠️ Slower |
| **Data Safety** | ⚠️ May lose recent | ✅ More durable |
| **File Size** | ✅ Compact | ⚠️ Larger |
| **Recommended** | Backups | Durability |

## High Availability

### Replication

```redis
# On replica
REPLICAOF master.example.com 6379
```

### Redis Sentinel

```
┌─────────┐     ┌─────────┐     ┌─────────┐
│Sentinel │     │Sentinel │     │Sentinel │
└────┬────┘     └────┬────┘     └────┬────┘
     │               │               │
     └───────────────┼───────────────┘
                     │
              ┌──────┴──────┐
              │   Master    │
              └──────┬──────┘
         ┌───────────┼───────────┐
         ▼           ▼           ▼
    ┌────────┐  ┌────────┐  ┌────────┐
    │Replica │  │Replica │  │Replica │
    └────────┘  └────────┘  └────────┘
```

### Redis Cluster

```
┌─────────────────────────────────────────┐
│           Redis Cluster                  │
│                                          │
│  Slots 0-5460    5461-10922   10923-16383│
│  ┌────────┐     ┌────────┐    ┌────────┐│
│  │Master 1│     │Master 2│    │Master 3││
│  └────┬───┘     └────┬───┘    └────┬───┘│
│       │              │             │     │
│  ┌────▼───┐     ┌────▼───┐    ┌────▼───┐│
│  │Replica │     │Replica │    │Replica ││
│  └────────┘     └────────┘    └────────┘│
└─────────────────────────────────────────┘
```

## Lua Scripting

```redis
-- Atomic increment if below limit
local current = redis.call('GET', KEYS[1])
if current and tonumber(current) >= tonumber(ARGV[1]) then
    return 0
else
    return redis.call('INCR', KEYS[1])
end
```

```python
# Python client
script = """
local current = redis.call('GET', KEYS[1])
...
"""
rate_limit = redis.register_script(script)
result = rate_limit(keys=['rate:user:1'], args=[100])
```

## Use Cases

| Use Case | Data Structure |
|----------|----------------|
| **Session storage** | Strings + TTL |
| **Caching** | Strings, Hashes |
| **Rate limiting** | Strings + INCR |
| **Leaderboards** | Sorted Sets |
| **Queues** | Lists (BRPOP) |
| **Pub/Sub** | Pub/Sub |
| **Real-time analytics** | HyperLogLog, Streams |
| **Geospatial** | Geo commands |

## Performance Tips

| Tip | Rationale |
|-----|-----------|
| **Use pipelining** | Reduce round trips |
| **Avoid large keys** | Memory + network |
| **Use SCAN over KEYS** | Non-blocking iteration |
| **Set TTLs** | Prevent memory bloat |
| **Use connection pooling** | Reduce connection overhead |

## Monitoring

```redis
INFO                    # Server info
INFO memory             # Memory stats
INFO replication        # Replication info

MONITOR                 # Real-time commands (debug only)
SLOWLOG GET 10          # Slow queries

CLIENT LIST             # Connected clients
DBSIZE                  # Key count
```

## Related

- [[Database Engines]] — Database comparison
- [[Caching Strategies]] — Caching patterns
- [[Message Queues]] — Queue comparison
- [[Tools MOC]] — All tools
