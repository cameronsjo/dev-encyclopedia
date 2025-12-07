---
title: Service Registry
aliases:
  - Service Registry Pattern
  - Service Catalog
tags:
  - infrastructure
  - microservices
  - architecture
  - tool
type: reference
status: complete
created: 2025-11-30
---

# Service Registry

Centralized database storing service instance locations, health status, and metadata for dynamic service-to-service communication in distributed systems.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Maintain dynamic inventory of service instances and their network locations |
| **Key Function** | Enable service discovery by providing queryable registry of available services |
| **Primary Use** | Microservices architectures, container orchestration, cloud-native systems |
| **Relationship** | Registry = storage layer, Discovery = lookup/query layer |
| **Pattern Type** | Infrastructure pattern for distributed systems |

## Core Concepts

**Registration** — Process of adding service instance information to the registry when service starts.

**Deregistration** — Removing service instance from registry when service stops or becomes unavailable.

**Health Checking** — Continuous verification that registered instances are alive and accepting requests.

**Metadata** — Additional service information beyond location (version, tags, capabilities, routing weights).

**TTL (Time-To-Live)** — Expiration time for registry entries requiring periodic renewal to prevent stale entries.

**Service Instance** — Single running copy of a service with unique network address (IP:port or DNS name).

## Registration Patterns

| Pattern | Description | Pros | Cons | Best For |
|---------|-------------|------|------|----------|
| **Self-Registration** | Service instance registers itself on startup | Simple, no external dependencies | Tight coupling to registry, registration logic in each service | Small deployments, single registry |
| **Third-Party Registration** | Separate registrar component manages registration | Decouples services from registry, centralized logic | Additional component to manage | Large deployments, multiple registries |
| **Sidecar Registration** | Sidecar proxy handles registration for service | Service completely decoupled, transparent | Resource overhead per instance | Service mesh architectures |
| **Platform Registration** | Orchestrator automatically registers services | Zero service code changes, platform-managed | Platform lock-in | Kubernetes, cloud platforms |

## Health Check Mechanisms

| Mechanism | How It Works | Update Frequency | Failure Detection |
|-----------|--------------|------------------|-------------------|
| **Heartbeat** | Service sends periodic "alive" signal | Every 5-30 seconds | Miss 2-3 consecutive beats |
| **TTL Renewal** | Service renews registry entry before expiration | Before TTL expires | Entry expires if not renewed |
| **Active Polling** | Registry actively checks service health endpoint | Registry-controlled interval | HTTP/TCP check failure |
| **Event-Based** | Service emits events on state changes | On change only | Timeout if no event received |

## Implementation Comparison

| Tool | Type | Consistency Model | Health Checks | Metadata Support | Best For |
|------|------|-------------------|---------------|------------------|----------|
| **Consul** | Dedicated registry | Raft consensus (CP) | Active polling + TTL | ✅ Rich KV store | Multi-DC, service mesh |
| **etcd** | Distributed KV store | Raft consensus (CP) | TTL-based | ✅ Hierarchical keys | Kubernetes, config storage |
| **Eureka** | Netflix registry | AP (eventual) | Heartbeat + renewal | ✅ Custom metadata | AWS, Spring ecosystem |
| **Kubernetes** | Platform registry | etcd-backed (CP) | Liveness/readiness probes | ✅ Labels + annotations | Container orchestration |
| **Zookeeper** | Coordination service | ZAB consensus (CP) | Ephemeral nodes | ❌ Limited | Legacy systems |

### Consul

**Architecture:**

- Agent on each node (client mode)
- Server cluster (3-5 nodes) for consensus
- Gossip protocol for membership and failure detection
- DNS and HTTP interfaces for queries

**Registration:**

- Service definition files (JSON/HCL)
- HTTP API registration
- Automatic deregistration on health check failure

**Health Checks:**

- HTTP endpoint checks
- TCP connection checks
- Script-based checks
- Docker container checks
- TTL-based checks

**Key Features:**

- Multi-datacenter support
- Service mesh (Consul Connect)
- KV store for configuration
- DNS-based service discovery

```hcl
# Example Consul service definition
service {
  name = "web"
  port = 8080
  tags = ["v1.0", "production"]

  check {
    http     = "http://localhost:8080/health"
    interval = "10s"
    timeout  = "2s"
  }

  meta {
    version = "1.0.0"
    region  = "us-west"
  }
}
```

### etcd

**Architecture:**

- Raft consensus for strong consistency
- Hierarchical key-value store
- Watch API for real-time updates
- gRPC and HTTP interfaces

**Registration:**

- Keys with TTL (lease-based)
- Automatic cleanup on lease expiration
- Directory structure for service organization

**Health Checks:**

- TTL/lease renewal required
- Service must refresh lease periodically
- No built-in active health checking

**Key Features:**

- Strong consistency guarantees
- Efficient watch mechanism
- Transaction support
- Kubernetes backing store

```bash
# Example etcd registration with lease
etcdctl lease grant 30
# lease 694d7a4e5c1a7c0d granted with TTL(30s)

etcdctl put --lease=694d7a4e5c1a7c0d \
  /services/web/instance-1 \
  '{"host":"10.0.1.5","port":8080}'

# Keep-alive maintains the lease
etcdctl lease keep-alive 694d7a4e5c1a7c0d
```

### Eureka

**Architecture:**

- Server cluster with peer replication (AP model)
- Client library handles registration
- REST API for all operations
- Self-preservation mode prevents mass deregistration

**Registration:**

- Client sends heartbeat every 30s (default)
- Full registration info on first heartbeat
- Renewal-only on subsequent heartbeats

**Health Checks:**

- Heartbeat-based (no active polling)
- Client-side health check integration
- Configurable eviction timeout

**Key Features:**

- AWS region/zone awareness
- Spring Cloud integration
- Client-side caching
- Gradual degradation under network partition

```java
// Example Spring Boot Eureka client
@SpringBootApplication
@EnableEurekaClient
public class WebService {
    public static void main(String[] args) {
        SpringApplication.run(WebService.class, args);
    }
}

// application.yml
eureka:
  client:
    serviceUrl:
      defaultZone: http://localhost:8761/eureka/
  instance:
    leaseRenewalIntervalInSeconds: 30
    metadata-map:
      version: "1.0.0"
      region: "us-west"
```

### Kubernetes Service Registry

**Architecture:**

- Built on etcd backend
- API server provides registry interface
- kube-proxy or service mesh handles discovery
- Labels and selectors for service targeting

**Registration:**

- Automatic via Service resources
- Endpoints controller tracks Pod IPs
- EndpointSlice for scalability (1000+ endpoints)

**Health Checks:**

- Liveness probes (restart unhealthy pods)
- Readiness probes (remove from endpoints)
- Startup probes (delayed initialization)

**Key Features:**

- Native to Kubernetes platform
- Integrated with network policies
- Supports headless services
- External service integration

```yaml
# Kubernetes Service and Deployment
apiVersion: v1
kind: Service
metadata:
  name: web
  labels:
    app: web
    version: v1
spec:
  selector:
    app: web
  ports:
    - port: 80
      targetPort: 8080
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: web
spec:
  replicas: 3
  selector:
    matchLabels:
      app: web
  template:
    metadata:
      labels:
        app: web
        version: v1
    spec:
      containers:
      - name: web
        image: web:1.0
        ports:
        - containerPort: 8080
        readinessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 10
```

## Registry vs Discovery

| Aspect | Service Registry | Service Discovery |
|--------|------------------|-------------------|
| **Role** | Storage/database of service instances | Lookup/query mechanism for finding services |
| **Operation** | Register, deregister, update health | Query, resolve, load balance |
| **Component** | Central database or cluster | Client library or proxy |
| **Examples** | Consul server, etcd cluster, Eureka server | Consul DNS, Ribbon, Envoy |
| **Responsibility** | Maintain current service inventory | Find and connect to services |

**Relationship:** Discovery requires a registry. Registry is the data layer, discovery is the access layer.

## Cloud-Native Patterns

### DNS-Based Discovery

**Pattern:** Service registry provides DNS interface for lookups.

**Example:** Consul DNS, Kubernetes CoreDNS

**Advantages:**

- No client library required
- Works with any language/framework
- Standardized interface

**Limitations:**

- TTL caching can cause stale data
- Limited metadata in DNS records
- No advanced load balancing

### Client-Side Discovery

**Pattern:** Client queries registry directly and performs load balancing.

**Examples:** Eureka + Ribbon, Consul + custom client

**Advantages:**

- Client controls load balancing algorithm
- No single point of failure proxy
- Rich metadata available

**Disadvantages:**

- Client library required for each language
- Client logic duplicated across services
- Registry address must be known

### Server-Side Discovery (Proxy)

**Pattern:** Load balancer queries registry and routes requests.

**Examples:** Kubernetes kube-proxy, Envoy + Consul

**Advantages:**

- Services unaware of registry
- Centralized routing logic
- Language-agnostic

**Disadvantages:**

- Load balancer is critical component
- Additional network hop
- Proxy must scale with traffic

### Service Mesh Integration

**Pattern:** Sidecar proxies handle registration and discovery.

**Examples:** Istio + Kubernetes, Consul Connect

**Advantages:**

- Complete service decoupling
- Advanced traffic management
- Security and observability built-in

**Disadvantages:**

- Operational complexity
- Resource overhead (sidecar per instance)
- Learning curve

## Metadata Strategies

| Metadata Type | Purpose | Example |
|---------------|---------|---------|
| **Version** | API versioning, canary routing | `version: "2.1.0"` |
| **Environment** | Stage-specific routing | `env: "production"` |
| **Region/Zone** | Geographic routing, latency optimization | `region: "us-west-2a"` |
| **Capabilities** | Feature-based routing | `features: ["auth", "payments"]` |
| **Weight** | Gradual rollout, traffic splitting | `weight: 10` (10% of traffic) |
| **Protocol** | Communication method | `protocol: "grpc"` |
| **Tags** | Arbitrary categorization | `tags: ["experimental", "high-memory"]` |

## Consistency Considerations

| Model | Description | Tools | Trade-offs |
|-------|-------------|-------|------------|
| **CP (Consistent)** | Strong consistency, partition tolerance | Consul, etcd, Zookeeper | May become unavailable during partition |
| **AP (Available)** | Availability, partition tolerance | Eureka | May serve stale data during partition |
| **Eventually Consistent** | Asynchronous replication | Multi-region Consul | Temporary inconsistency across regions |

**CAP Theorem:** Can't have all three (Consistency, Availability, Partition tolerance). Choose based on requirements.

## When to Use

### Service Registry Strengths

| Scenario | Why Registry Needed |
|----------|---------------------|
| **Dynamic Scaling** | Instances come and go frequently, IPs change |
| **Multi-Instance Services** | Need to track and load balance across replicas |
| **Microservices** | Many services discovering each other |
| **Cloud Deployments** | Ephemeral infrastructure, dynamic IPs |
| **Container Orchestration** | Automated deployment and scaling |
| **Health-Based Routing** | Only route to healthy instances |

### Considerations

**Operational Overhead:**

- Registry must be highly available
- Requires monitoring and maintenance
- Adds complexity to deployment

**Network Dependency:**

- Services depend on registry availability
- Network partitions can cause issues
- Need fallback/caching strategies

**Consistency Requirements:**

- Choose CP or AP based on needs
- Understand failure modes
- Plan for split-brain scenarios

**Scale:**

- Registry must handle query load
- Watch/notification mechanisms at scale
- Metadata storage growth

### Alternatives

**Static Configuration:**

- Works for small, stable deployments
- No registry overhead
- Limited to fixed infrastructure

**DNS Only:**

- Simple, no special tooling
- Limited metadata and health checking
- Sufficient for some use cases

**Platform-Managed:**

- Kubernetes, cloud platforms handle it
- No need for separate registry
- Platform lock-in

## Best Practices

**Registration:**

- Register on successful startup, not deployment
- Include all necessary metadata upfront
- Deregister gracefully on shutdown

**Health Checks:**

- Check actual service health, not just process
- Include dependency health in checks
- Use appropriate intervals (not too frequent)
- Implement graceful degradation

**Metadata:**

- Keep metadata minimal and relevant
- Use consistent naming conventions
- Version metadata schema

**Client Behavior:**

- Cache registry responses
- Handle registry unavailability
- Refresh cache periodically
- Implement circuit breakers

**Operations:**

- Monitor registry health and latency
- Set up alerts for registration failures
- Test failure scenarios
- Document registry architecture

## Related

- [[Service Discovery]] — Lookup and resolution layer built on registry
- [[Microservices]] — Architecture pattern requiring service registry
- [[Kubernetes]] — Platform with built-in service registry
- [[Load Balancing]] — Distribution of requests across registered instances
- [[Health Checks]] — Mechanisms for validating service availability
- [[DNS]] — Alternative discovery mechanism
