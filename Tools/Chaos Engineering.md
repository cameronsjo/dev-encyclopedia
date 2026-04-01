---
title: Chaos Engineering
aliases:
  - Chaos Monkey
  - Failure Injection
  - Resilience Testing
  - Game Days
tags:
  - tools
  - devops
  - sre
  - testing
  - reliability
type: reference
status: complete
created: "2025-12-16"
---

# Chaos Engineering

The discipline of experimenting on distributed systems to build confidence in their ability to withstand turbulent conditions in production.

## Overview

| Aspect | Description |
|--------|-------------|
| **Definition** | Proactive failure injection to find weaknesses |
| **Origin** | Netflix (2011) - Chaos Monkey |
| **Goal** | Build resilient systems, not fragile ones |
| **Principle** | Better to fail controlled than unexpectedly |

## Core Principles

### The Chaos Engineering Manifesto

1. **Build a hypothesis around steady state**
2. **Vary real-world events**
3. **Run experiments in production**
4. **Automate to run continuously**
5. **Minimize blast radius**

### Steady State Hypothesis

```
"Under normal conditions, our payment service maintains
99.9% success rate and p99 latency under 200ms.

Hypothesis: If we terminate 1 database replica, the
service will maintain these SLOs."
```

## Experiment Design

### Experiment Structure

```
┌─────────────────────────────────────────────────────┐
│              Chaos Experiment                        │
├─────────────────────────────────────────────────────┤
│                                                      │
│  1. STEADY STATE                                    │
│     Define normal behavior metrics                  │
│     Example: Error rate < 1%, Latency p99 < 200ms  │
│                                                      │
│  2. HYPOTHESIS                                       │
│     What we expect to happen                        │
│     Example: "System will auto-failover"           │
│                                                      │
│  3. INTRODUCE CHAOS                                 │
│     Inject the failure                              │
│     Example: Terminate database primary            │
│                                                      │
│  4. OBSERVE                                          │
│     Monitor system behavior                         │
│     Example: Watch metrics, logs, alerts           │
│                                                      │
│  5. ANALYZE                                          │
│     Did hypothesis hold?                            │
│     Example: Failover took 45s, SLO breached       │
│                                                      │
│  6. IMPROVE                                          │
│     Fix weaknesses found                            │
│     Example: Tune failover timeout                 │
│                                                      │
└─────────────────────────────────────────────────────┘
```

### Experiment Template

```yaml
name: Database Failover Test
description: Verify system handles database primary failure
owner: platform-team
environment: staging  # Start here before production

steady_state:
  metrics:
    - name: success_rate
      query: sum(rate(requests_success[5m])) / sum(rate(requests_total[5m]))
      threshold: "> 0.999"
    - name: latency_p99
      query: histogram_quantile(0.99, rate(request_duration_bucket[5m]))
      threshold: "< 0.2"  # 200ms

hypothesis: >
  When the database primary is terminated, the system will
  failover to replica within 30 seconds and maintain SLOs.

method:
  - action: terminate_instance
    target: database-primary

rollback:
  - action: restore_instance
    target: database-primary

abort_conditions:
  - metric: error_rate
    threshold: "> 0.1"  # Abort if >10% errors
    duration: 60s
```

## Types of Chaos Experiments

### Infrastructure Failures

| Experiment | Description |
|------------|-------------|
| **Instance termination** | Kill VMs/containers randomly |
| **Network partition** | Isolate services from each other |
| **Latency injection** | Add artificial delay |
| **Disk failure** | Fill disk, corrupt data |
| **CPU stress** | Exhaust compute resources |
| **Memory pressure** | Consume available memory |
| **DNS failure** | Break name resolution |

### Application Failures

| Experiment | Description |
|------------|-------------|
| **Exception injection** | Force error paths |
| **Dependency failure** | Mock failing upstream |
| **Resource exhaustion** | Connection pool, thread pool |
| **State corruption** | Invalid cache data |

### Network Chaos

| Experiment | Description |
|------------|-------------|
| **Packet loss** | Drop % of packets |
| **Latency** | Add delay to requests |
| **Bandwidth** | Limit throughput |
| **DNS manipulation** | Wrong/slow DNS |
| **TLS failures** | Certificate issues |

## Tools

### Chaos Monkey (Netflix)

**The original chaos tool.**

```yaml
# Simian Army configuration
simianarmy.chaos.enabled = true
simianarmy.chaos.probability = 1.0
simianarmy.chaos.leashed = false
simianarmy.chaos.ASG.enabled = true
```

### Gremlin

**Commercial chaos platform.**

| Feature | Description |
|---------|-------------|
| **Attack Library** | Pre-built failure scenarios |
| **Scenarios** | Multi-step experiments |
| **Targets** | Hosts, containers, K8s |
| **Safety** | Automatic rollback |

### Chaos Mesh (Kubernetes)

**Cloud-native chaos for K8s.**

```yaml
apiVersion: chaos-mesh.org/v1alpha1
kind: PodChaos
metadata:
  name: pod-failure-example
spec:
  action: pod-failure
  mode: one
  selector:
    namespaces:
      - production
    labelSelectors:
      app: payment-service
  duration: "60s"
```

### Litmus (Kubernetes)

**CNCF chaos engineering project.**

```yaml
apiVersion: litmuschaos.io/v1alpha1
kind: ChaosEngine
metadata:
  name: nginx-chaos
spec:
  appinfo:
    appns: default
    applabel: "app=nginx"
  chaosServiceAccount: litmus-admin
  experiments:
    - name: pod-delete
      spec:
        components:
          env:
            - name: TOTAL_CHAOS_DURATION
              value: "30"
            - name: CHAOS_INTERVAL
              value: "10"
```

### AWS Fault Injection Simulator

**AWS-native chaos service.**

```json
{
  "description": "Terminate random EC2 instances",
  "targets": {
    "instances": {
      "resourceType": "aws:ec2:instance",
      "selectionMode": "COUNT(1)",
      "resourceTags": {
        "Environment": "production"
      }
    }
  },
  "actions": {
    "terminateInstances": {
      "actionId": "aws:ec2:terminate-instances",
      "targets": {
        "Instances": "instances"
      }
    }
  }
}
```

### tc (Traffic Control) - Linux

**Network chaos with Linux tools.**

```bash
# Add 100ms latency to eth0
tc qdisc add dev eth0 root netem delay 100ms

# Add 10% packet loss
tc qdisc add dev eth0 root netem loss 10%

# Limit bandwidth to 1mbit
tc qdisc add dev eth0 root tbf rate 1mbit burst 32kbit latency 400ms

# Remove chaos
tc qdisc del dev eth0 root
```

### Toxiproxy (Shopify)

**Proxy for simulating network conditions.**

```bash
# Create proxy
toxiproxy-cli create redis -l localhost:26379 -u localhost:6379

# Add latency
toxiproxy-cli toxic add redis -t latency -a latency=1000

# Add timeout (connection drop)
toxiproxy-cli toxic add redis -t timeout -a timeout=5000
```

## Tool Comparison

| Tool | Platform | OSS | Key Feature |
|------|----------|-----|-------------|
| **Chaos Monkey** | AWS | ✅ | Original, simple |
| **Gremlin** | Multi | ❌ | Enterprise features |
| **Chaos Mesh** | K8s | ✅ | K8s native |
| **Litmus** | K8s | ✅ | CNCF project |
| **AWS FIS** | AWS | ❌ | AWS native |
| **Toxiproxy** | Any | ✅ | Network simulation |
| **Pumba** | Docker | ✅ | Container chaos |

## Game Days

### What is a Game Day?

**Scheduled chaos exercise** with the team.

```
┌─────────────────────────────────────────────────────┐
│               Game Day Structure                     │
├─────────────────────────────────────────────────────┤
│                                                      │
│  BEFORE (1 week prior)                              │
│  • Define scenarios                                  │
│  • Notify stakeholders                              │
│  • Prepare rollback procedures                      │
│  • Brief participants                               │
│                                                      │
│  DURING (2-4 hours)                                 │
│  • Run experiments                                   │
│  • Observe team response                            │
│  • Document findings                                │
│  • Practice incident response                       │
│                                                      │
│  AFTER (1 week following)                           │
│  • Review results                                    │
│  • Create action items                              │
│  • Update runbooks                                   │
│  • Share learnings                                   │
│                                                      │
└─────────────────────────────────────────────────────┘
```

### Game Day Scenarios

| Scenario | Tests |
|----------|-------|
| **Zone failure** | Multi-AZ resilience |
| **Database failover** | HA configuration |
| **Dependency outage** | Circuit breakers |
| **Traffic spike** | Auto-scaling |
| **On-call handoff** | Process and documentation |
| **Secret rotation** | Credential management |

## Best Practices

### Start Small

```
Progression:
1. Development environment
2. Staging environment
3. Production (canary)
4. Production (wider)
```

### Safety Mechanisms

| Mechanism | Purpose |
|-----------|---------|
| **Abort conditions** | Stop if impact exceeds threshold |
| **Blast radius limits** | Contain experiment scope |
| **Time limits** | Maximum experiment duration |
| **Rollback automation** | Quick recovery |
| **Runbook ready** | Manual recovery documented |

### What Not to Do

| Anti-Pattern | Risk |
|--------------|------|
| **No hypothesis** | Can't evaluate results |
| **No monitoring** | Can't observe impact |
| **Skip staging** | Unknown production risk |
| **No communication** | Surprise outages |
| **No rollback plan** | Extended incidents |

## Maturity Model

| Level | Description |
|-------|-------------|
| **1. Ad-hoc** | Manual, occasional experiments |
| **2. Defined** | Documented experiments, game days |
| **3. Automated** | Continuous chaos in staging |
| **4. Advanced** | Continuous chaos in production |
| **5. Optimized** | AI-driven chaos, self-healing |

## Metrics

### Experiment Metrics

| Metric | Measures |
|--------|----------|
| **Experiments run** | Volume of testing |
| **Weaknesses found** | Effectiveness |
| **MTTR during chaos** | Recovery capability |
| **False positives** | Alert quality |

### Program Metrics

| Metric | Measures |
|--------|----------|
| **Coverage** | % of services tested |
| **Frequency** | Experiments per month |
| **Fix rate** | Issues resolved after finding |
| **Production incidents** | Should decrease over time |

## Related

- [[Incident Management]] — When experiments reveal issues
- [[SLOs and SLIs]] — Defining steady state
- [[Runbooks]] — Recovery procedures
- [[Testing Strategies]] — Testing approaches
- [[Tools MOC]] — All tools
