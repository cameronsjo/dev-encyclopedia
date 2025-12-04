---
title: System Design
aliases:
  - Systems Design
  - Architecture
tags:
  - concept
  - architecture
  - distributed
type: concept
status: complete
created: '2025-12-03'
---

# System Design

Designing scalable, reliable, and maintainable software systems.

## Overview

| Aspect | Details |
|--------|---------|
| Focus | Architecture, scalability, reliability |
| Scope | Distributed systems, data storage, networking |
| Key skills | Trade-off analysis, capacity planning |

---

## Core Concepts

### Scalability

| Type | Description | When to Use |
|------|-------------|-------------|
| Vertical | Add more CPU/RAM to existing machines | Simple, limited ceiling |
| Horizontal | Add more machines | Unlimited scale, complex |

### Reliability

| Measure | Definition |
|---------|------------|
| Availability | % of time system is operational |
| Durability | Data survives failures |
| Fault tolerance | System operates despite failures |

**Nines of availability:**

| Nines | Downtime/year |
|-------|---------------|
| 99% | 3.65 days |
| 99.9% | 8.76 hours |
| 99.99% | 52.6 minutes |
| 99.999% | 5.26 minutes |

### CAP Theorem

Distributed systems can only guarantee 2 of 3:

| Property | Description |
|----------|-------------|
| **C**onsistency | All nodes see same data |
| **A**vailability | Every request gets response |
| **P**artition tolerance | System works despite network failures |

**In practice:** P is required, choose between C and A.

---

## Load Balancing

### Algorithms

| Algorithm | Description |
|-----------|-------------|
| Round Robin | Rotate through servers |
| Least Connections | Send to least busy |
| IP Hash | Consistent routing by client |
| Weighted | Account for server capacity |

### Layers

| Layer | Examples |
|-------|----------|
| L4 (Transport) | HAProxy, AWS NLB |
| L7 (Application) | Nginx, AWS ALB |
| DNS | Route 53, Cloudflare |

---

## Caching

### Cache Strategies

| Strategy | Write | Read |
|----------|-------|------|
| Cache-aside | App manages | Check cache, then DB |
| Write-through | Cache + DB sync | Cache |
| Write-back | Cache only, async DB | Cache |
| Write-around | DB only | Check cache, then DB |

### Cache Invalidation

| Method | Description |
|--------|-------------|
| TTL | Expire after time |
| Event-driven | Invalidate on update |
| Versioning | Cache key includes version |

### Technologies

| Tool | Use Case |
|------|----------|
| Redis | In-memory, versatile |
| Memcached | Simple key-value |
| CDN | Static content |
| Browser cache | Client-side |

---

## Databases

### SQL vs NoSQL

| Aspect | SQL | NoSQL |
|--------|-----|-------|
| Schema | Fixed | Flexible |
| Scaling | Vertical | Horizontal |
| Joins | Native | Limited/none |
| Transactions | ACID | Often eventual |
| Use case | Complex queries | High scale, simple access |

### Scaling Patterns

| Pattern | Description |
|---------|-------------|
| Read replicas | Scale reads, single writer |
| Sharding | Partition data across nodes |
| Denormalization | Duplicate data for read speed |

### Sharding Strategies

| Strategy | Description |
|----------|-------------|
| Hash-based | Hash key to shard |
| Range-based | Split by key range |
| Geographic | Split by region |
| Directory | Lookup table |

---

## Message Queues

Decouple producers and consumers.

### Use Cases

- Async processing
- Load leveling
- Event-driven architecture
- Microservice communication

### Technologies

| Tool | Model | Use Case |
|------|-------|----------|
| Kafka | Log-based | High throughput, streaming |
| RabbitMQ | Traditional queue | Task queues, RPC |
| SQS | Cloud queue | AWS integration |
| Redis Pub/Sub | Simple pub/sub | Real-time, ephemeral |

### Patterns

| Pattern | Description |
|---------|-------------|
| Point-to-point | One consumer per message |
| Pub/Sub | Multiple consumers |
| Request/Reply | Async RPC |
| Dead letter queue | Failed message handling |

---

## Microservices

### Communication

| Style | Use Case |
|-------|----------|
| REST | Simple, synchronous |
| gRPC | High performance |
| GraphQL | Flexible queries |
| Events | Async, decoupled |

### Service Discovery

| Type | Examples |
|------|----------|
| Client-side | Eureka, Consul |
| Server-side | Kubernetes, AWS ALB |

### Challenges

- Network latency
- Partial failures
- Data consistency
- Distributed tracing
- Service versioning

---

## Common Patterns

### Rate Limiting

| Algorithm | Description |
|-----------|-------------|
| Token bucket | Smooth bursts |
| Leaky bucket | Fixed rate |
| Fixed window | Count per window |
| Sliding window | Rolling count |

### Circuit Breaker

Prevent cascading failures.

```
CLOSED -> (failures) -> OPEN -> (timeout) -> HALF-OPEN -> (success) -> CLOSED
                                          -> (failure) -> OPEN
```

### API Gateway

- Authentication
- Rate limiting
- Request routing
- Response caching
- Protocol translation

---

## Data Consistency

### Consistency Models

| Model | Description |
|-------|-------------|
| Strong | Read sees latest write |
| Eventual | Eventually consistent |
| Causal | Respects causality |
| Read-your-writes | See own writes |

### Distributed Transactions

| Pattern | Description |
|---------|-------------|
| Two-phase commit | Coordinator-based |
| Saga | Sequence of local transactions |
| Outbox | Event sourcing pattern |

---

## Estimation

### Back-of-envelope calculations

| Metric | Value |
|--------|-------|
| Read latency (SSD) | ~100μs |
| Read latency (HDD) | ~10ms |
| Network round trip (same DC) | ~0.5ms |
| Network round trip (cross-continent) | ~150ms |

### Traffic estimation

1. Daily active users (DAU)
2. Actions per user per day
3. Peak multiplier (2-3x average)
4. Data size per action

---

## Design Process

1. **Clarify requirements** — Functional & non-functional
2. **Estimate scale** — Users, traffic, storage
3. **Define API** — Endpoints, data models
4. **High-level design** — Core components
5. **Deep dive** — Critical components
6. **Address bottlenecks** — Scale, reliability

---

## Related

- [[Design Patterns]]
- [[API Design Patterns]]
- [[Database Engines]]
- [[Distributed Tracing]]
- [[Networking Fundamentals]]
