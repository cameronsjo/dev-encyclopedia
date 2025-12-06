---
title: Message Queues
aliases:
  - Message Queue
  - Message Broker
  - Event Streaming
tags:
  - infrastructure
  - messaging
  - distributed-systems
  - tool
type: reference
status: complete
created: 2025-11-30
---

# Message Queues

Systems for asynchronous communication between services using queues, topics, and streams.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Decouple services, handle async workloads, buffer traffic spikes |
| **Patterns** | Point-to-point queues, pub/sub topics, event streaming |
| **Key Concepts** | Producers, consumers, topics/queues, partitions, consumer groups |
| **Guarantees** | At-most-once, at-least-once, exactly-once delivery |
| **Use Cases** | Background jobs, event-driven architecture, log aggregation, microservices communication |

## Core Concepts

### Messaging Patterns

**Point-to-Point (Queue)**
- Single consumer receives each message
- Work distribution across workers
- Message deleted after consumption
- Example: Job processing, task distribution

**Publish-Subscribe (Topic)**
- Multiple subscribers receive each message
- Broadcasting events to multiple services
- Each subscriber gets independent copy
- Example: Notifications, event distribution

**Event Streaming**
- Persistent, ordered log of events
- Consumers replay from any offset
- Long-term retention
- Example: Event sourcing, analytics pipelines

### Delivery Guarantees

| Guarantee | Description | Trade-offs |
|-----------|-------------|------------|
| **At-Most-Once** | Message delivered zero or one time | Fast, may lose messages |
| **At-Least-Once** | Message delivered one or more times | Duplicates possible, idempotency required |
| **Exactly-Once** | Message delivered exactly one time | Complex, performance overhead |

### Ordering Guarantees

**Total Ordering**
- All messages globally ordered
- Single partition/queue only
- Limits throughput

**Partition Ordering**
- Messages within partition ordered
- Different partitions independent
- Scales horizontally

**No Ordering**
- Maximum parallelism
- Application handles ordering
- Highest throughput

## Message Queue Systems

### Kafka

| Aspect | Details |
|--------|---------|
| **Type** | Distributed event streaming platform |
| **Model** | Append-only log with partitions |
| **Persistence** | Disk-based, configurable retention |
| **Ordering** | Per-partition ordering guaranteed |
| **Delivery** | At-least-once (default), exactly-once (with transactions) |
| **Throughput** | Very high (millions msg/sec) |
| **Latency** | Medium (2-10ms) |
| **Best For** | Event sourcing, stream processing, high-volume data pipelines |

**Key Features:**
- Consumer groups with automatic rebalancing
- Log compaction for state storage
- Kafka Streams for processing
- Strong durability guarantees
- Time-based and log retention

**Considerations:**
- Complex operational overhead
- Requires ZooKeeper (pre-3.0) or KRaft
- Not ideal for low-latency messaging
- Large cluster recommended for HA

### RabbitMQ

| Aspect | Details |
|--------|---------|
| **Type** | Traditional message broker |
| **Model** | Queues with exchanges and routing |
| **Persistence** | Optional disk persistence |
| **Ordering** | Per-queue ordering |
| **Delivery** | At-most-once, at-least-once |
| **Throughput** | Medium-high (tens of thousands msg/sec) |
| **Latency** | Low (sub-millisecond) |
| **Best For** | Task queues, RPC, complex routing patterns |

**Key Features:**
- Flexible routing (direct, topic, fanout, headers)
- Dead letter exchanges
- Message TTL and priority
- Built-in clustering
- Management UI

**Considerations:**
- Messages deleted after consumption
- Not designed for replay
- Lower throughput than Kafka
- Memory-based by default

### Redis Streams

| Aspect | Details |
|--------|---------|
| **Type** | In-memory append-only log |
| **Model** | Stream with consumer groups |
| **Persistence** | Optional RDB/AOF persistence |
| **Ordering** | Total ordering per stream |
| **Delivery** | At-least-once |
| **Throughput** | Very high (in-memory) |
| **Latency** | Very low (sub-millisecond) |
| **Best For** | Real-time messaging, lightweight event streaming, caching + messaging |

**Key Features:**
- Consumer groups with pending entries
- Time-based queries
- Auto-trimming by length/time
- Simple Kafka-like API
- Integrated with Redis ecosystem

**Considerations:**
- Limited retention (memory-based)
- Single-node bottleneck
- No built-in partitioning
- Requires Redis Cluster for HA

### Amazon SQS

| Aspect | Details |
|--------|---------|
| **Type** | Fully managed queue service |
| **Model** | Standard (at-least-once) and FIFO queues |
| **Persistence** | Managed by AWS |
| **Ordering** | FIFO queues guarantee ordering |
| **Delivery** | At-least-once (Standard), exactly-once (FIFO) |
| **Throughput** | Unlimited (Standard), 3000 msg/sec (FIFO) |
| **Latency** | Medium (tens of milliseconds) |
| **Best For** | AWS-native apps, serverless, decoupling microservices |

**Key Features:**
- Zero operational overhead
- Auto-scaling
- Dead letter queues
- Message delay and visibility timeout
- Integration with Lambda, SNS, EventBridge

**Considerations:**
- AWS vendor lock-in
- Limited retention (14 days max)
- No message replay
- FIFO has throughput limits

### NATS

| Aspect | Details |
|--------|---------|
| **Type** | Cloud-native messaging system |
| **Model** | Subject-based pub/sub with JetStream for persistence |
| **Persistence** | JetStream for durable messaging |
| **Ordering** | Per-stream ordering (JetStream) |
| **Delivery** | At-most-once (core), at-least-once (JetStream) |
| **Throughput** | Very high (millions msg/sec) |
| **Latency** | Very low (sub-millisecond) |
| **Best For** | Microservices mesh, IoT, edge computing, request-reply |

**Key Features:**
- Minimal dependencies
- Subject-based addressing
- Request-reply pattern
- Clustering and supercluster
- Leaf nodes for edge

**Considerations:**
- JetStream required for persistence
- Smaller ecosystem than Kafka
- Limited third-party integrations
- Best with NATS-native clients

### Apache Pulsar

| Aspect | Details |
|--------|---------|
| **Type** | Multi-tenant event streaming platform |
| **Model** | Topics with partitions, separate compute/storage |
| **Persistence** | Apache BookKeeper for storage |
| **Ordering** | Per-partition ordering |
| **Delivery** | At-least-once, at-most-once, exactly-once |
| **Throughput** | Very high (millions msg/sec) |
| **Latency** | Low-medium |
| **Best For** | Multi-tenancy, geo-replication, unified messaging |

**Key Features:**
- Tiered storage (hot/warm/cold)
- Native multi-tenancy
- Geo-replication built-in
- Schema registry
- Functions for stream processing

**Considerations:**
- More complex than Kafka
- Smaller community
- Operationally heavy
- Requires BookKeeper knowledge

## Comparison Matrix

| Feature | Kafka | RabbitMQ | Redis Streams | SQS | NATS | Pulsar |
|---------|-------|----------|---------------|-----|------|--------|
| **Throughput** | ✅ Very High | Medium | ✅ Very High | High | ✅ Very High | ✅ Very High |
| **Latency** | Medium | ✅ Low | ✅ Very Low | Medium | ✅ Very Low | Low |
| **Persistence** | ✅ Disk | Optional | Optional | ✅ Managed | Optional | ✅ Disk |
| **Message Replay** | ✅ Yes | ❌ No | ✅ Yes | ❌ No | ✅ JetStream | ✅ Yes |
| **Ordering** | ✅ Partition | Queue | ✅ Stream | FIFO only | ✅ Stream | ✅ Partition |
| **Ops Complexity** | High | Medium | ✅ Low | ✅ None | ✅ Low | High |
| **Multi-tenancy** | Basic | ✅ Native | ❌ No | ✅ AWS | Basic | ✅ Native |
| **Exactly-Once** | ✅ Yes | ❌ No | ❌ No | FIFO only | ❌ No | ✅ Yes |
| **Schema Registry** | ✅ Yes | ❌ No | ❌ No | ❌ No | ❌ No | ✅ Yes |

## Key Patterns

### Consumer Groups

**Purpose:** Scale message consumption across multiple instances.

**Kafka/Pulsar:**
- Automatic partition assignment
- Rebalancing on consumer join/leave
- Each partition consumed by one consumer in group

**RabbitMQ:**
- Competing consumers on same queue
- Round-robin distribution
- Manual or auto-ack

**Redis Streams:**
- Consumer groups with pending entries list
- Claim stale messages
- ACK-based tracking

### Dead Letter Queues

**Purpose:** Handle messages that fail processing.

**Common Uses:**
- Poison message isolation
- Manual inspection and reprocessing
- Alerting on recurring failures
- Archival of bad data

**Implementation:**
- Maximum retry count exceeded
- Processing exception
- Message expiration
- Explicit rejection

### Backpressure Handling

**Producer-Side:**
- Block on full buffer
- Drop messages
- Return error to caller
- Batch and compress

**Consumer-Side:**
- Limit prefetch/batch size
- Manual acknowledgment
- Pause/resume consumption
- Scale consumer instances

### Partitioning Strategies

| Strategy | Description | Use Case |
|----------|-------------|----------|
| **Key-Based** | Hash of message key determines partition | Maintain order per entity (user ID, order ID) |
| **Round-Robin** | Distribute evenly across partitions | Maximum throughput, no ordering needed |
| **Custom** | Application-defined logic | Complex routing requirements |
| **Random** | Random partition selection | Load balancing without keys |

## When to Use

### Use Kafka When

**Strengths:**
- Need event replay and reprocessing
- High-volume data pipelines
- Event sourcing architecture
- Stream processing with Kafka Streams
- Long-term event retention
- Strong ordering within partitions

**Best For:**
- Analytics and data lakes
- Change data capture (CDC)
- Activity tracking
- Log aggregation
- Microservices event bus

**Considerations:**
- Operational complexity
- Resource-intensive
- Overkill for simple queues
- Learning curve

### Use RabbitMQ When

**Strengths:**
- Need complex routing patterns
- Request-reply messaging
- Task distribution with priorities
- Low-latency messaging
- Battle-tested reliability
- Rich management UI

**Best For:**
- Background job processing
- RPC communication
- Workflow orchestration
- Legacy system integration
- Moderate throughput workloads

**Considerations:**
- No message replay
- Scaling limits vs Kafka
- Memory management
- Cluster complexity

### Use Redis Streams When

**Strengths:**
- Already using Redis
- Need very low latency
- Simple event streaming
- Lightweight messaging
- Real-time notifications
- Combined caching + messaging

**Best For:**
- Chat applications
- Real-time dashboards
- Lightweight event sourcing
- Session management
- Rate limiting with messaging

**Considerations:**
- Memory constraints
- Single-stream bottleneck
- Limited retention
- Requires Redis expertise

### Use SQS When

**Strengths:**
- Running on AWS
- Zero operational overhead
- Serverless architecture
- Auto-scaling needed
- Unpredictable traffic
- AWS service integration

**Best For:**
- Lambda triggers
- Decoupling AWS services
- Simple queue workloads
- Cost-sensitive projects
- Teams without ops resources

**Considerations:**
- AWS vendor lock-in
- FIFO throughput limits
- No replay capability
- Higher latency
- Limited visibility

### Use NATS When

**Strengths:**
- Need extreme simplicity
- Sub-millisecond latency critical
- Request-reply pattern
- Edge/IoT deployment
- Microservices mesh
- Minimal resource footprint

**Best For:**
- Service mesh communication
- Edge computing
- IoT messaging
- Gaming backends
- Telemetry collection

**Considerations:**
- JetStream needed for durability
- Smaller ecosystem
- Less mature streaming features
- Limited enterprise tooling

### Use Pulsar When

**Strengths:**
- Multi-tenant requirements
- Geo-replication needed
- Tiered storage critical
- Unified queue + streaming
- Schema evolution important
- Horizontal scaling priority

**Best For:**
- Multi-tenant SaaS
- Global deployments
- Financial services
- Unified messaging platform
- Large-scale enterprises

**Considerations:**
- High complexity
- Smaller community
- BookKeeper dependency
- Steep learning curve

## Decision Guide

| Requirement | Recommendation |
|-------------|----------------|
| **Event replay and reprocessing** | Kafka, Pulsar, Redis Streams |
| **Exactly-once semantics** | Kafka, Pulsar, SQS FIFO |
| **Sub-millisecond latency** | NATS, Redis Streams |
| **Complex routing** | RabbitMQ |
| **Zero ops overhead** | SQS |
| **Multi-tenancy** | Pulsar, SQS |
| **Stream processing** | Kafka (Streams), Pulsar (Functions) |
| **Simple task queues** | RabbitMQ, SQS |
| **Geo-replication** | Pulsar, Kafka (MirrorMaker) |
| **Edge/IoT** | NATS |
| **High throughput (millions/sec)** | Kafka, Pulsar, NATS |
| **AWS-native** | SQS, SNS, EventBridge |
| **Request-reply pattern** | NATS, RabbitMQ |
| **Long retention (months/years)** | Kafka, Pulsar (tiered) |
| **Minimal infrastructure** | Redis Streams, NATS |

## Common Pitfalls

**Poison Messages**
- One bad message blocks queue
- Solution: Dead letter queues, message validation, retry limits

**Consumer Lag**
- Consumers falling behind producers
- Solution: Scale consumers, increase batch size, optimize processing

**Message Duplication**
- At-least-once delivery causes duplicates
- Solution: Idempotent consumers, deduplication logic, exactly-once semantics

**Partition Skew**
- Uneven load across partitions
- Solution: Better key distribution, more partitions, monitoring

**Lost Messages**
- Fire-and-forget without confirmation
- Solution: Producer acknowledgments, replication, persistence

**Tight Coupling**
- Shared message schemas create dependencies
- Solution: Schema evolution, versioning, backward compatibility

## Related

- [[Apache Kafka]] - Detailed Kafka coverage
- [[RabbitMQ]] - RabbitMQ deep dive
- [[Redis]] - Redis and Redis Streams
- [[Event-Driven Architecture]] - Architectural patterns
- [[Microservices]] - Service communication patterns
- [[Distributed Systems]] - Consensus, replication, partitioning
- [[Observability]] - Monitoring message systems
- [[Backpressure]] - Handling flow control
