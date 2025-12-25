---
title: Load Testing
aliases:
  - Performance Testing
  - Stress Testing
  - Capacity Testing
tags:
  - cs
  - testing
  - performance
  - sre
type: reference
status: complete
created: "2025-12-18"
---

# Load Testing

Testing system behavior under expected and peak load conditions.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Validate performance under load |
| **Metrics** | Response time, throughput, errors |
| **When** | Before releases, capacity planning |
| **Types** | Load, stress, spike, soak, scalability |

## Types of Performance Tests

### Test Type Comparison

| Type | Load Level | Duration | Goal |
|------|------------|----------|------|
| **Load Test** | Expected | Minutes-hours | Validate normal performance |
| **Stress Test** | Above capacity | Until failure | Find breaking point |
| **Spike Test** | Sudden increase | Short bursts | Handle traffic spikes |
| **Soak Test** | Normal-high | Hours-days | Find memory leaks, degradation |
| **Scalability Test** | Incremental | Varies | Measure scaling behavior |

### Load Patterns

```
Load Test:          Stress Test:         Spike Test:
   Users               Users               Users
     │                   │                   │
  100├────────────     │      ╱╲          │    ╱╲
     │            │    │     ╱  ╲         │   ╱  ╲
   50├            │    │    ╱    ╲        │  ╱    ╲
     │   ramp    hold  │   ╱      ╲       │ ╱      ╲
     │  ╱             │  ╱        ╲      │╱        ╲
     └───────────     └───────────      └───────────
        Time              Time              Time

Soak Test:          Scalability Test:
   Users               Users
     │                   │
  100├────────────────   │       ╱
     │                │  │      ╱
     │                │  │     ╱ step
     │                │  │    ╱  increase
     │   sustained    │  │   ╱
     └───────────────    └───────────
        Time (hours)        Time
```

## Key Metrics

### Response Time

| Metric | Description |
|--------|-------------|
| **Average** | Mean response time |
| **Median (p50)** | 50th percentile |
| **p90** | 90% of requests faster than this |
| **p95** | 95th percentile |
| **p99** | 99th percentile |
| **Max** | Slowest request |

```
Response Time Distribution:
                                  p99
                             p95  │
                        p90  │    │
             Median     │    │    │
Average      │          │    │    │
  │          │          │    │    │
  ▼          ▼          ▼    ▼    ▼
──────────────────────────────────────►
0ms        100ms      200ms    500ms
```

### Throughput

| Metric | Description |
|--------|-------------|
| **RPS** | Requests per second |
| **TPS** | Transactions per second |
| **Concurrent Users** | Simultaneous active users |

### Error Rates

| Metric | Target |
|--------|--------|
| **Error Rate** | < 1% typically |
| **HTTP 5xx** | 0% ideally |
| **Timeouts** | Minimal |

## Load Testing Tools

### k6

```javascript
// k6 script
import http from 'k6/http';
import { check, sleep } from 'k6';

export const options = {
  stages: [
    { duration: '2m', target: 100 },  // Ramp up
    { duration: '5m', target: 100 },  // Hold
    { duration: '2m', target: 0 },    // Ramp down
  ],
  thresholds: {
    http_req_duration: ['p(95)<500'],  // 95% under 500ms
    http_req_failed: ['rate<0.01'],    // Error rate < 1%
  },
};

export default function() {
  const res = http.get('https://api.example.com/users');

  check(res, {
    'status is 200': (r) => r.status === 200,
    'response time < 500ms': (r) => r.timings.duration < 500,
  });

  sleep(1);  // Think time
}
```

### Locust (Python)

```python
from locust import HttpUser, task, between

class WebsiteUser(HttpUser):
    wait_time = between(1, 5)  # Think time

    @task(3)  # Weight 3
    def view_homepage(self):
        self.client.get("/")

    @task(1)  # Weight 1
    def view_product(self):
        self.client.get("/product/123")

    def on_start(self):
        """Login on start"""
        self.client.post("/login", {
            "username": "testuser",
            "password": "password"
        })
```

### JMeter

```xml
<!-- JMeter test plan excerpt -->
<ThreadGroup>
  <intProp name="ThreadGroup.num_threads">100</intProp>
  <intProp name="ThreadGroup.ramp_time">60</intProp>
  <boolProp name="ThreadGroup.scheduler">true</boolProp>
  <stringProp name="ThreadGroup.duration">300</stringProp>
</ThreadGroup>

<HTTPSamplerProxy>
  <stringProp name="HTTPSampler.domain">api.example.com</stringProp>
  <stringProp name="HTTPSampler.path">/users</stringProp>
  <stringProp name="HTTPSampler.method">GET</stringProp>
</HTTPSamplerProxy>
```

### Artillery

```yaml
# artillery.yml
config:
  target: "https://api.example.com"
  phases:
    - duration: 60
      arrivalRate: 5
      name: "Warm up"
    - duration: 300
      arrivalRate: 50
      name: "Sustained load"

scenarios:
  - name: "Browse and purchase"
    flow:
      - get:
          url: "/products"
      - think: 2
      - get:
          url: "/products/{{ $randomNumber(1, 100) }}"
      - post:
          url: "/cart"
          json:
            productId: "{{ $randomNumber(1, 100) }}"
```

### Tool Comparison

| Tool | Language | GUI | Cloud | Strengths |
|------|----------|-----|-------|-----------|
| **k6** | JavaScript | No | Yes | Modern, scriptable, Grafana |
| **Locust** | Python | Web UI | Yes | Easy scripting, distributed |
| **JMeter** | Java/XML | Yes | Plugins | Feature-rich, enterprise |
| **Artillery** | YAML/JS | No | Yes | Easy config, serverless |
| **Gatling** | Scala | Yes | Yes | High performance |
| **wrk** | Lua | No | No | Simple, fast |

## Test Design

### Workload Modeling

```
Real User Flow:
1. Homepage (30% of traffic)
2. Search (25%)
3. Product Page (25%)
4. Add to Cart (15%)
5. Checkout (5%)

Translate to test weights:
- homepage: weight 6
- search: weight 5
- product: weight 5
- add_to_cart: weight 3
- checkout: weight 1
```

### Think Time

```javascript
// Realistic user behavior
export default function() {
  http.get('/');
  sleep(randomIntBetween(2, 5));  // Read page

  http.get('/products');
  sleep(randomIntBetween(3, 10)); // Browse

  http.post('/cart', { productId: 1 });
  sleep(randomIntBetween(1, 3));  // Quick action
}
```

### Data Parameterization

```javascript
// k6 with CSV data
import papaparse from 'papaparse';
import { SharedArray } from 'k6/data';

const users = new SharedArray('users', function() {
  return papaparse.parse(open('./users.csv'), { header: true }).data;
});

export default function() {
  const user = users[Math.floor(Math.random() * users.length)];
  http.post('/login', {
    username: user.username,
    password: user.password,
  });
}
```

## Running Tests

### Pre-Test Checklist

| Item | Description |
|------|-------------|
| **Baseline** | Measure current performance |
| **Environment** | Production-like, isolated |
| **Data** | Realistic test data |
| **Monitoring** | APM, logs, metrics ready |
| **Stakeholders** | Notify relevant teams |
| **Rollback** | Plan if tests cause issues |

### During Test

```bash
# Monitor during test
# CPU, memory, network, disk I/O
top
vmstat 1
iostat -x 1
netstat -s

# Application metrics
# - Response times
# - Error rates
# - Database connections
# - Cache hit rates
# - Queue depths
```

### Post-Test Analysis

| Analyze | Questions |
|---------|-----------|
| **Response times** | Meet SLOs? Degraded over time? |
| **Throughput** | Hit target RPS? |
| **Errors** | What failed? When? |
| **Resources** | CPU/memory bottlenecks? |
| **Database** | Slow queries? Connection pool? |
| **External deps** | Third-party latency? |

## Results Interpretation

### Healthy vs Unhealthy

```
Healthy Load Test:
Response Time ────────────────────────
                stable, within SLO
                                   time →

Unhealthy Load Test:
Response Time        ╱
                    ╱
                   ╱  degradation under load
                  ╱
              ───╱─────────────────────
                                   time →

Resource Exhaustion:
Response Time                 │
                              │ cliff
                    ─────────┘
                                   time →
```

### Identifying Bottlenecks

| Symptom | Possible Cause |
|---------|----------------|
| High CPU | Inefficient code, missing caching |
| High memory | Memory leaks, large caches |
| High DB latency | Missing indexes, N+1 queries |
| Connection errors | Pool exhausted, timeouts |
| Increasing latency | Queue buildup, GC pressure |

## CI/CD Integration

```yaml
# GitHub Actions example
name: Performance Tests
on:
  push:
    branches: [main]

jobs:
  load-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Run k6 test
        uses: grafana/k6-action@v0.3.0
        with:
          filename: tests/load-test.js

      - name: Upload results
        uses: actions/upload-artifact@v3
        with:
          name: k6-results
          path: results.json
```

### Performance Budgets

```javascript
// k6 thresholds as gates
export const options = {
  thresholds: {
    // Fail if any threshold breached
    http_req_duration: ['p(95)<500', 'p(99)<1000'],
    http_req_failed: ['rate<0.01'],
    http_reqs: ['rate>100'],  // Minimum throughput
  },
};
```

## Best Practices

| Practice | Description |
|----------|-------------|
| **Test in production-like env** | Same infra, data volume |
| **Use realistic data** | Not just "test user" |
| **Include think time** | Real users pause |
| **Ramp gradually** | Don't spike immediately |
| **Monitor everything** | APM, logs, metrics |
| **Test regularly** | Catch regressions |
| **Set baselines** | Compare against known good |
| **Test dependencies** | Don't mock everything |

## Common Mistakes

| Mistake | Issue |
|---------|-------|
| **Testing against mocks** | Miss real bottlenecks |
| **No think time** | Unrealistic load pattern |
| **Ignoring ramp-up** | Miss warm-up issues |
| **Single endpoint** | Miss interaction issues |
| **Insufficient duration** | Miss memory leaks |
| **Wrong environment** | Results don't transfer |

## Related

- [[SLOs and SLIs]] — Performance targets
- [[Chaos Engineering]] — Failure testing
- [[Testing Frameworks]] — Testing tools
- [[Computer Science MOC]] — CS topics
