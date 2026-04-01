---
title: SLOs and SLIs
aliases:
  - Service Level Objectives
  - Service Level Indicators
  - SLAs
  - Error Budgets
tags:
  - tools
  - devops
  - sre
  - observability
  - reliability
type: reference
status: complete
created: "2025-12-16"
---

# SLOs and SLIs

Quantitative approach to defining and measuring service reliability, forming the foundation of Site Reliability Engineering (SRE).

## Overview

| Term | Full Name | Definition |
|------|-----------|------------|
| **SLI** | Service Level Indicator | Metric measuring service behavior |
| **SLO** | Service Level Objective | Target value for an SLI |
| **SLA** | Service Level Agreement | Contract with consequences |
| **Error Budget** | 100% - SLO | Allowable unreliability |

## The Hierarchy

```
┌─────────────────────────────────────────────┐
│               SLA (Contract)                │
│  "99.9% uptime or service credits"          │
│                                             │
│  ┌───────────────────────────────────────┐  │
│  │           SLO (Target)                │  │
│  │  "99.95% of requests < 200ms"         │  │
│  │                                       │  │
│  │  ┌─────────────────────────────────┐  │  │
│  │  │         SLI (Measurement)       │  │  │
│  │  │  "Request latency at p99"       │  │  │
│  │  └─────────────────────────────────┘  │  │
│  └───────────────────────────────────────┘  │
└─────────────────────────────────────────────┘
```

**Key Insight:** SLOs should be stricter than SLAs to provide a safety buffer.

## Service Level Indicators (SLIs)

### Common SLI Categories

| Category | Measures | Examples |
|----------|----------|----------|
| **Availability** | Is it up? | Success rate, uptime |
| **Latency** | How fast? | Response time (p50, p99) |
| **Throughput** | How much? | Requests per second |
| **Error Rate** | How often failing? | 5xx errors / total requests |
| **Correctness** | Is it right? | Data integrity, accuracy |
| **Freshness** | How recent? | Data staleness |

### Defining Good SLIs

**Good SLI Properties:**

| Property | Description |
|----------|-------------|
| **Measurable** | Can be quantified objectively |
| **User-centric** | Reflects user experience |
| **Actionable** | Team can influence it |
| **Understandable** | Clear to all stakeholders |

### SLI Specifications

```
SLI: Request Latency
────────────────────────────────────────
Measurement: Time from request received to response sent
Source: Application metrics (Prometheus)
Aggregation: 99th percentile over 5-minute windows
Good event: Response time < 200ms
Valid event: All HTTP requests (excluding health checks)
```

### Calculating SLIs

**Availability SLI:**

```
Availability = (Successful Requests / Total Requests) × 100

Example: (9,950 / 10,000) × 100 = 99.5%
```

**Latency SLI:**

```
Latency SLI = (Requests < Threshold / Total Requests) × 100

Example: (9,800 requests < 200ms / 10,000 total) × 100 = 98%
```

## Service Level Objectives (SLOs)

### Setting SLOs

| Consideration | Guidance |
|---------------|----------|
| **User expectations** | What do users actually need? |
| **Historical data** | What are you achieving now? |
| **Business requirements** | What can you afford to provide? |
| **Technical constraints** | What's realistically achievable? |

### SLO Examples

| Service | SLI | SLO |
|---------|-----|-----|
| **API Gateway** | Availability | 99.95% over 30 days |
| **API Gateway** | Latency (p99) | < 100ms for 99% of requests |
| **Database** | Availability | 99.99% over 30 days |
| **Search** | Latency (p50) | < 50ms for 95% of requests |
| **Batch Job** | Success rate | 99.9% of jobs complete |
| **Data Pipeline** | Freshness | < 5 minutes old 99.9% of time |

### SLO Document Template

```yaml
service: payment-api
owner: payments-team
slos:
  - name: Availability
    description: Payment API responds successfully
    sli:
      type: availability
      good_event: HTTP status 2xx or 4xx
      valid_event: All requests (excluding /health)
    objective: 99.95%
    window: 30 days

  - name: Latency
    description: Payment API responds quickly
    sli:
      type: latency
      threshold: 200ms
      percentile: 99
    objective: 99%
    window: 30 days
```

## Error Budgets

### The Concept

```
Error Budget = 100% - SLO

If SLO = 99.9%
Then Error Budget = 0.1%
```

### Error Budget Calculation

| SLO | Error Budget | Monthly Downtime Allowed |
|-----|--------------|--------------------------|
| 99% | 1% | 7 hours 18 minutes |
| 99.9% | 0.1% | 43 minutes 50 seconds |
| 99.95% | 0.05% | 21 minutes 55 seconds |
| 99.99% | 0.01% | 4 minutes 23 seconds |
| 99.999% | 0.001% | 26 seconds |

### Using Error Budgets

```
┌─────────────────────────────────────────────────────┐
│              Error Budget Status                     │
├─────────────────────────────────────────────────────┤
│                                                      │
│  Budget: 0.1% (43 min/month)                        │
│  Used:   0.03% (13 min)                             │
│  Remaining: 0.07% (30 min)                          │
│                                                      │
│  ██████████░░░░░░░░░░░░░░░░░░░░  30% consumed       │
│                                                      │
│  Status: ✅ HEALTHY - Ship features!                │
│                                                      │
└─────────────────────────────────────────────────────┘
```

### Error Budget Policies

| Budget Status | Action |
|---------------|--------|
| **> 50% remaining** | Ship features, experiment |
| **25-50% remaining** | Normal development, monitor |
| **< 25% remaining** | Slow down, focus on reliability |
| **Exhausted** | Freeze features, fix reliability |

### Error Budget Policy Example

```markdown
## Error Budget Policy

### When budget is healthy (>50% remaining):
- Normal feature development proceeds
- Risky changes allowed with appropriate review

### When budget is concerning (25-50% remaining):
- Feature development continues with caution
- Extra scrutiny on changes that might affect reliability

### When budget is critical (<25% remaining):
- Feature freeze except for reliability improvements
- Post-incident reviews required for all incidents

### When budget is exhausted (0% remaining):
- Hard feature freeze
- All engineering effort on reliability
- Executive escalation required for exceptions
```

## SLAs vs SLOs

### Key Differences

| Aspect | SLO | SLA |
|--------|-----|-----|
| **Audience** | Internal teams | External customers |
| **Consequence** | Policy-based | Contractual (financial) |
| **Target** | What we aim for | What we guarantee |
| **Flexibility** | Can be adjusted | Legally binding |

### Relationship

```
SLA: 99.9% availability (contractual)
        ↑
        │ Buffer (0.05%)
        │
SLO: 99.95% availability (internal target)
        ↑
        │ Buffer (0.04%)
        │
Actual: 99.99% availability (current performance)
```

## Implementation

### Prometheus SLO Recording Rules

```yaml
groups:
  - name: slo_availability
    rules:
      # Total requests
      - record: sli:http_requests:rate5m
        expr: sum(rate(http_requests_total[5m]))

      # Successful requests
      - record: sli:http_requests_success:rate5m
        expr: sum(rate(http_requests_total{status=~"2.."}[5m]))

      # Availability SLI
      - record: sli:availability:ratio
        expr: sli:http_requests_success:rate5m / sli:http_requests:rate5m

      # Error budget consumption (30 day window)
      - record: slo:error_budget:remaining
        expr: |
          1 - (
            (1 - avg_over_time(sli:availability:ratio[30d]))
            /
            (1 - 0.999)  # SLO target
          )
```

### Alerting on Error Budget

```yaml
groups:
  - name: slo_alerts
    rules:
      # Alert when burning budget too fast (will exhaust in 2 hours)
      - alert: HighErrorBudgetBurn
        expr: |
          slo:error_budget:burn_rate:1h > 14.4
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "High error budget burn rate"
          description: "At current rate, error budget will be exhausted in 2 hours"

      # Alert when budget is low
      - alert: ErrorBudgetLow
        expr: slo:error_budget:remaining < 0.25
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Error budget below 25%"
```

### Multi-Window, Multi-Burn-Rate Alerts

| Window | Burn Rate | Exhaustion Time | Alert Type |
|--------|-----------|-----------------|------------|
| 5m | 14.4× | 2 hours | Page |
| 30m | 6× | 5 hours | Page |
| 6h | 1× | 30 days | Ticket |

## Tools for SLOs

| Tool | Type | Features |
|------|------|----------|
| **Prometheus** | Metrics | Recording rules, alerts |
| **Grafana** | Visualization | SLO dashboards |
| **Nobl9** | SLO Platform | Error budgets, burn rates |
| **Datadog** | Observability | SLO tracking |
| **Google SLO Generator** | OSS | Automated SLO from metrics |
| **Sloth** | OSS | Prometheus SLO generator |

## Best Practices

### Do

| Practice | Rationale |
|----------|-----------|
| **Start simple** | 1-2 SLOs per service initially |
| **Focus on user experience** | External behavior, not internals |
| **Review regularly** | Adjust as service evolves |
| **Make SLOs visible** | Dashboards, status pages |
| **Act on error budgets** | Actually slow down when exhausted |

### Don't

| Anti-Pattern | Problem |
|--------------|---------|
| **100% SLO** | Impossible, blocks all change |
| **Too many SLOs** | Dilutes focus |
| **Internal metrics as SLIs** | CPU usage ≠ user experience |
| **Ignoring error budgets** | Defeats the purpose |
| **SLO without consequences** | Just becomes a vanity metric |

## Related

- [[Incident Management]] — When SLOs are breached
- [[Observability Stack]] — Measuring SLIs
- [[Prometheus]] — SLO implementation
- [[Tools MOC]] — All tools
