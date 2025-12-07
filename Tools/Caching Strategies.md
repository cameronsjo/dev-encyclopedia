---
title: Caching Strategies
aliases:
  - Caching
  - Cache Patterns
tags:
  - infrastructure
  - performance
  - architecture
  - tool
type: reference
status: complete
created: "2025-12-07"
---

# Caching Strategies

Techniques for storing and retrieving frequently accessed data to improve performance and reduce backend load.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Reduce latency, lower database load, improve scalability |
| **Layers** | Application, Database, CDN, Browser, DNS |
| **Key Concerns** | Consistency, invalidation, memory limits, cold starts |
| **Common Tools** | Redis, Memcached, Varnish, CDN providers |

## Cache Layers

### Application-Level Caching

**In-Memory Cache:** Store data within application process (e.g., LRU cache, dictionary/map).

- **Pros:** Fastest access, no network latency
- **Cons:** Limited by process memory, lost on restart, not shared across instances

**Distributed Cache:** External cache service shared across application instances.

- **Tools:** Redis, Memcached, Hazelcast
- **Pros:** Shared state, survives restarts, scalable
- **Cons:** Network latency, additional infrastructure

### Database-Level Caching

**Query Result Cache:** Store results of expensive queries.

**Materialized Views:** Precomputed query results stored as tables.

**Connection Pooling:** Reuse database connections to reduce overhead.

### CDN Caching

Cache static assets (images, CSS, JS) and dynamic content at edge locations.

- **Geographic distribution:** Serve from nearest POP (Point of Presence)
- **Cache headers:** `Cache-Control`, `ETag`, `Last-Modified`
- **Invalidation:** Purge API, cache versioning, URL fingerprinting

### Browser Caching

Client-side storage controlled via HTTP headers.

- **HTTP Cache:** Browser stores responses based on headers
- **Service Workers:** Programmable cache with offline support
- **Local Storage/Session Storage:** Key-value storage for application data

## Caching Patterns

| Pattern | Description | Read Flow | Write Flow | Use Case |
|---------|-------------|-----------|------------|----------|
| **Cache-Aside** (Lazy Loading) | Application manages cache explicitly | Check cache → miss → fetch DB → populate cache | Write to DB → invalidate/update cache | Most flexible, general purpose |
| **Read-Through** | Cache handles DB reads transparently | Check cache → miss → cache fetches from DB | Write directly to DB | Simplify read logic |
| **Write-Through** | All writes go through cache to DB | Read from cache | Write to cache → cache writes to DB (sync) | Strong consistency needed |
| **Write-Behind** (Write-Back) | Cache writes asynchronously to DB | Read from cache | Write to cache → async batch to DB | High write throughput, can tolerate data loss |
| **Refresh-Ahead** | Proactively refresh before expiration | Read from cache | Background job refreshes hot keys | Predictable access patterns |

### Cache-Aside Example Flow

```
READ:
1. Check cache for key
2. If HIT: return cached value
3. If MISS: fetch from database
4. Store in cache with TTL
5. Return value

WRITE:
1. Write to database
2. Invalidate cache key (or update)
```

### Write-Through vs Write-Behind

| Aspect | Write-Through | Write-Behind |
|--------|---------------|--------------|
| **Consistency** | Strong (sync writes) | Eventual (async writes) |
| **Latency** | Higher write latency | Lower write latency |
| **Durability** | Durable immediately | Risk of data loss on crash |
| **Write Load** | DB writes on every update | Batched DB writes |
| **Best For** | Financial data, critical updates | Metrics, logs, high-volume writes |

## Cache Invalidation Strategies

**"There are only two hard things in Computer Science: cache invalidation and naming things." — Phil Karlton**

### Time-Based (TTL)

Set expiration time for each cache entry.

- **Fixed TTL:** Same duration for all entries (e.g., 5 minutes)
- **Sliding TTL:** Reset expiration on each access
- **Adaptive TTL:** Adjust based on data change frequency

### Event-Based Invalidation

Invalidate cache when underlying data changes.

- **Explicit invalidation:** Delete cache key on write operations
- **Pub/Sub notifications:** Broadcast invalidation events
- **Change Data Capture (CDC):** React to database change logs

### Version-Based Invalidation

Include version identifier in cache keys.

```
user:123:v2
```

On data change, increment version. Old keys expire naturally via TTL.

### Tag-Based Invalidation

Group related cache entries with tags for bulk invalidation.

```
Tags: ["user:123", "org:456"]
Invalidate all entries with tag "org:456"
```

## Eviction Policies

When cache reaches memory limit, remove entries using:

| Policy | Description | Use Case |
|--------|-------------|----------|
| **LRU** (Least Recently Used) | Evict oldest accessed item | General purpose, temporal locality |
| **LFU** (Least Frequently Used) | Evict least accessed item | Frequency matters more than recency |
| **FIFO** (First In First Out) | Evict oldest inserted item | Simple, predictable |
| **Random** | Evict random item | Fast, minimal overhead |
| **TTL** | Evict expired items first | Time-sensitive data |

**Redis supports:** LRU, LFU, TTL, Random, volatile variants (only evict keys with TTL).

## Common Challenges

### Cache Stampede (Thundering Herd)

**Problem:** Many requests simultaneously miss cache, overload database.

**Solutions:**

- **Locking:** First requester fetches, others wait
- **Probabilistic early expiration:** Refresh before TTL expires
- **Request coalescing:** Deduplicate concurrent identical requests
- **Stale-while-revalidate:** Serve stale data while refreshing

### Cold Start

**Problem:** Empty cache after restart causes high database load.

**Solutions:**

- **Cache warming:** Preload critical data on startup
- **Gradual traffic ramp:** Slowly increase traffic to new instances
- **Persistent cache:** Use Redis with persistence (RDB/AOF)

### Cache Consistency

**Problem:** Cache and database get out of sync.

**Solutions:**

- **TTL:** Accept eventual consistency with bounded staleness
- **Write-through:** Ensure writes update both cache and DB
- **Event-driven invalidation:** React to database changes
- **Read-repair:** Check version on read, refresh if stale

### Hot Keys

**Problem:** Single key receives disproportionate traffic, becomes bottleneck.

**Solutions:**

- **Replication:** Shard hot key across multiple cache instances
- **Local cache:** Add application-level cache in front of Redis
- **Randomized TTL:** Prevent synchronized expiration

## Cache Technologies Comparison

| Tool | Type | Protocol | Persistence | Use Case |
|------|------|----------|-------------|----------|
| **Redis** | In-memory data structure store | RESP | ✅ Optional (RDB, AOF) | General cache, session store, queues |
| **Memcached** | In-memory key-value cache | Binary/ASCII | ❌ None | Simple distributed cache, high throughput |
| **Varnish** | HTTP reverse proxy cache | HTTP | ❌ None | CDN, web acceleration |
| **Hazelcast** | Distributed in-memory data grid | Java API | ✅ Optional | Java apps, distributed computing |
| **Apache Ignite** | Distributed database + cache | SQL, Key-Value | ✅ Yes | ACID transactions, compute grid |

### Redis vs Memcached

| Aspect | Redis | Memcached |
|--------|-------|-----------|
| **Data Types** | Strings, Lists, Sets, Hashes, Sorted Sets, Streams | Strings only |
| **Persistence** | ✅ RDB snapshots, AOF logs | ❌ None |
| **Replication** | ✅ Primary-replica | ❌ None (client-side) |
| **Clustering** | ✅ Native sharding | Client-side consistent hashing |
| **Eviction** | 8+ policies (LRU, LFU, etc.) | LRU only |
| **Pub/Sub** | ✅ Yes | ❌ No |
| **Lua Scripting** | ✅ Yes | ❌ No |
| **Multithreading** | Mostly single-threaded (I/O threads in 6+) | Multithreaded |
| **Use Case** | Rich data structures, persistence needed | Simple, high-throughput caching |

## HTTP Caching Headers

### Response Headers

```http
Cache-Control: public, max-age=3600, s-maxage=7200
ETag: "33a64df551425fcc55e4d42a148795d9f25f89d4"
Last-Modified: Wed, 21 Oct 2025 07:28:00 GMT
Vary: Accept-Encoding, Accept-Language
```

### Cache-Control Directives

| Directive | Meaning |
|-----------|---------|
| `public` | Cacheable by any cache (CDN, browser) |
| `private` | Cacheable only by browser (not CDN) |
| `no-cache` | Must revalidate with origin before use |
| `no-store` | Do not cache at all (sensitive data) |
| `max-age=N` | Fresh for N seconds (browser) |
| `s-maxage=N` | Fresh for N seconds (shared cache/CDN) |
| `must-revalidate` | Validate stale cache before use |
| `immutable` | Never changes (fingerprinted assets) |

### Conditional Requests

**ETag (Entity Tag):** Hash or version identifier for cache validation.

```http
Request: If-None-Match: "abc123"
Response: 304 Not Modified (if ETag matches)
Response: 200 OK + new content (if ETag differs)
```

**Last-Modified:** Timestamp-based validation.

```http
Request: If-Modified-Since: Wed, 21 Oct 2025 07:28:00 GMT
Response: 304 Not Modified (if not modified)
Response: 200 OK + new content (if modified)
```

## Distributed Caching Patterns

### Cache Cluster Topologies

**Primary-Replica:**

- Single primary for writes, multiple replicas for reads
- Eventual consistency between replicas
- Failover to replica on primary failure

**Sharded:**

- Data partitioned across multiple nodes
- Consistent hashing for key distribution
- Scale horizontally by adding shards

**Replicated:**

- Full copy of data on each node
- High availability, low read latency
- High memory overhead, complex write coordination

### Consistency Models

| Model | Description | Use Case |
|-------|-------------|----------|
| **Strong** | All nodes see same data immediately | Critical data, low concurrency |
| **Eventual** | Nodes converge to same state over time | High availability, partition tolerance |
| **Bounded Staleness** | Data stale for max duration (TTL) | Acceptable lag, balance consistency/performance |

## Decision Guide

### Choose Cache-Aside When

✅ Need flexibility in caching logic
✅ Cache misses are acceptable
✅ Complex invalidation requirements
✅ Multiple data sources

### Choose Write-Through When

✅ Strong consistency required
✅ Simple read/write patterns
✅ Can tolerate write latency

### Choose Write-Behind When

✅ High write throughput needed
✅ Can tolerate data loss risk
✅ Batching improves DB performance
✅ Asynchronous processing acceptable

### Choose Redis When

✅ Need data structures (lists, sets, sorted sets)
✅ Persistence required
✅ Pub/Sub messaging needed
✅ Complex operations (Lua scripts)
✅ Replication/clustering needed

### Choose Memcached When

✅ Simple key-value caching
✅ No persistence needed
✅ Maximum throughput priority
✅ Multithreaded performance critical

### Choose CDN Caching When

✅ Serving static assets globally
✅ Reducing origin load
✅ Improving geographic latency
✅ DDoS protection needed

## Best Practices

**DO:**

- Set appropriate TTLs based on data change frequency
- Monitor cache hit rates and eviction rates
- Use separate cache namespaces for different data types
- Implement fallback logic for cache failures
- Use connection pooling for cache clients
- Cache immutable data aggressively
- Version cache keys when schema changes

**DON'T:**

- Cache sensitive data without encryption
- Set TTLs longer than data staleness tolerance
- Ignore cache stampede on high-traffic keys
- Cache large objects without compression
- Share cache instances across environments (dev/prod)
- Assume cache is always available (fail gracefully)

## Monitoring Metrics

| Metric | Description | Target |
|--------|-------------|--------|
| **Hit Rate** | `hits / (hits + misses)` | >80% for hot data |
| **Miss Rate** | `misses / (hits + misses)` | <20% |
| **Eviction Rate** | Items removed due to memory limits | Low, stable |
| **Memory Usage** | Current / max memory | <80% |
| **Latency (p99)** | 99th percentile response time | <5ms (Redis) |
| **Throughput** | Operations per second | Depends on workload |
| **Replication Lag** | Time behind primary | <100ms |

## Related

- [[Database Engines]] — Persistent storage layer that caching optimizes
- [[CDN]] — Edge caching for global content delivery
- [[Redis]] — Primary distributed cache and data structure store
- [[HTTP]] — Protocol-level caching with headers and status codes
- [[Load Balancing]] — Distribute cache load across instances
- [[API Design]] — Cache-friendly API patterns and headers
