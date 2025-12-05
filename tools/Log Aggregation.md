---
title: Log Aggregation
aliases:
  - Logging Platforms
  - Centralized Logging
tags:
  - tool
  - comparison
  - observability
  - logging
type: comparison
status: complete
created: 2025-12-04
---

# Log Aggregation

Centralized logging platforms for collecting, storing, and searching logs.

## Overview

| Tool | Type | License | Query Language | Best For |
|------|------|---------|----------------|----------|
| Loki | Log aggregation | OSS (AGPLv3) | LogQL | Grafana users, cost-effective |
| Elasticsearch | Search engine | OSS/Commercial | KQL, Lucene | Full-text search |
| Splunk | SIEM/Log platform | Commercial | SPL | Enterprise, security |
| Fluentd | Log collector | OSS (Apache 2) | — | Collection/routing |
| Fluent Bit | Log collector | OSS (Apache 2) | — | Lightweight collection |
| Vector | Log collector | OSS (MPL 2) | VRL | High-performance |
| Graylog | Log management | OSS/Commercial | — | Self-hosted alternative |

---

## Architecture Patterns

### Collection → Aggregation → Storage

```
┌─────────────────────────────────────────────────────────────┐
│                       Applications                           │
│   stdout/stderr │ files │ syslog │ API calls                │
└────────┬────────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────┐
│                    Log Collectors                            │
│   Fluent Bit │ Fluentd │ Vector │ Promtail │ Beats          │
│   (parse, transform, buffer, forward)                        │
└────────┬────────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────┐
│                  Log Aggregation/Storage                     │
│   Loki │ Elasticsearch │ Splunk │ ClickHouse                │
└────────┬────────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────┐
│                     Visualization                            │
│   Grafana │ Kibana │ Splunk UI │ Custom                     │
└─────────────────────────────────────────────────────────────┘
```

---

## Loki

Grafana's log aggregation system. "Like Prometheus, but for logs."

### Key Concepts

- **Labels only** — Indexes labels, not log content
- **LogQL** — PromQL-like query language
- **Cost-effective** — Store in object storage (S3, GCS)
- **Grafana native** — Perfect integration

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Applications                            │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                       Promtail                               │
│              (or Grafana Alloy, Fluent Bit)                 │
│   - Discovers log files                                      │
│   - Attaches labels                                          │
│   - Pushes to Loki                                           │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                         Loki                                 │
│   ┌───────────┐   ┌───────────┐   ┌───────────┐            │
│   │Distributor│──▶│  Ingester │──▶│  Querier  │            │
│   └───────────┘   └─────┬─────┘   └───────────┘            │
│                         │                                    │
│                         ▼                                    │
│                   ┌───────────┐                              │
│                   │  Storage  │                              │
│                   │ (S3/GCS)  │                              │
│                   └───────────┘                              │
└─────────────────────────────────────────────────────────────┘
```

### LogQL Examples

```logql
# Basic stream selection
{job="nginx"}

# Filter by content
{job="nginx"} |= "error"
{job="nginx"} != "healthcheck"
{job="nginx"} |~ "status=[45].."

# Parse and filter
{job="nginx"} | json | status >= 400

# Pattern parsing
{job="nginx"} | pattern `<ip> - - [<_>] "<method> <path> <_>" <status>`
  | status = "500"

# Aggregations
count_over_time({job="nginx"} |= "error" [5m])
rate({job="nginx"}[1m])
sum by (status) (count_over_time({job="nginx"} | json [5m]))

# Top errors
topk(10, sum by (error) (count_over_time({job="api"} | json [1h])))
```

### Promtail Config

```yaml
# promtail-config.yaml
server:
  http_listen_port: 9080

positions:
  filename: /tmp/positions.yaml

clients:
  - url: http://loki:3100/loki/api/v1/push

scrape_configs:
  - job_name: containers
    docker_sd_configs:
      - host: unix:///var/run/docker.sock
    relabel_configs:
      - source_labels: ['__meta_docker_container_name']
        target_label: container

  - job_name: system
    static_configs:
      - targets:
          - localhost
        labels:
          job: syslog
          __path__: /var/log/syslog
```

### Deployment

```yaml
# docker-compose.yaml
services:
  loki:
    image: grafana/loki:latest
    ports:
      - "3100:3100"
    command: -config.file=/etc/loki/local-config.yaml
    volumes:
      - loki-data:/loki

  promtail:
    image: grafana/promtail:latest
    volumes:
      - /var/log:/var/log:ro
      - ./promtail-config.yaml:/etc/promtail/config.yaml
    command: -config.file=/etc/promtail/config.yaml
```

---

## Elasticsearch (ELK/Elastic Stack)

Full-text search engine, powerful for log analysis.

### Key Concepts

- **Full-text indexing** — Indexes all log content
- **Lucene-based** — Powerful search capabilities
- **ELK Stack** — Elasticsearch + Logstash + Kibana
- **Beats** — Lightweight data shippers

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Applications                            │
└────────┬───────────────────────────────────────┬────────────┘
         │                                       │
         ▼                                       ▼
┌─────────────────┐                   ┌─────────────────────┐
│      Beats      │                   │      Logstash       │
│  (Filebeat,     │                   │   (transform,       │
│   Metricbeat)   │                   │    enrich)          │
└────────┬────────┘                   └──────────┬──────────┘
         │                                       │
         └───────────────────┬───────────────────┘
                             │
                             ▼
                   ┌─────────────────┐
                   │  Elasticsearch  │
                   │   (cluster)     │
                   └────────┬────────┘
                            │
                            ▼
                   ┌─────────────────┐
                   │     Kibana      │
                   │ (visualization) │
                   └─────────────────┘
```

### Query Examples

```json
// KQL (Kibana Query Language)
status:error AND service:api
response_time:>1000
message:"connection refused"

// Lucene syntax
status:5* AND NOT path:/healthcheck
@timestamp:[2024-01-01 TO 2024-01-31]

// Elasticsearch Query DSL
{
  "query": {
    "bool": {
      "must": [
        { "match": { "level": "error" } },
        { "range": { "@timestamp": { "gte": "now-1h" } } }
      ],
      "filter": [
        { "term": { "service": "api" } }
      ]
    }
  },
  "aggs": {
    "errors_by_type": {
      "terms": { "field": "error_type.keyword" }
    }
  }
}
```

### Filebeat Config

```yaml
# filebeat.yml
filebeat.inputs:
  - type: container
    paths:
      - /var/lib/docker/containers/*/*.log
    processors:
      - add_docker_metadata: ~

  - type: log
    paths:
      - /var/log/nginx/*.log
    fields:
      service: nginx

output.elasticsearch:
  hosts: ["elasticsearch:9200"]
  index: "logs-%{+yyyy.MM.dd}"

setup.kibana:
  host: "kibana:5601"
```

### Index Lifecycle Management

```json
// ILM Policy
{
  "policy": {
    "phases": {
      "hot": {
        "actions": {
          "rollover": {
            "max_size": "50GB",
            "max_age": "1d"
          }
        }
      },
      "warm": {
        "min_age": "7d",
        "actions": {
          "shrink": { "number_of_shards": 1 },
          "forcemerge": { "max_num_segments": 1 }
        }
      },
      "delete": {
        "min_age": "30d",
        "actions": { "delete": {} }
      }
    }
  }
}
```

---

## Splunk

Enterprise log management and SIEM platform.

### Key Concepts

- **SPL** — Search Processing Language
- **Indexers** — Store and index data
- **Forwarders** — Collect and send data
- **Search Heads** — Query interface

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Data Sources                            │
└────────┬───────────────────────────────────────┬────────────┘
         │                                       │
         ▼                                       ▼
┌─────────────────┐                   ┌─────────────────────┐
│    Universal    │                   │      Heavy         │
│    Forwarder    │                   │    Forwarder       │
│   (lightweight) │                   │ (parsing/routing)  │
└────────┬────────┘                   └──────────┬──────────┘
         │                                       │
         └───────────────────┬───────────────────┘
                             │
                             ▼
                   ┌─────────────────┐
                   │    Indexers     │
                   │  (cluster)      │
                   └────────┬────────┘
                            │
                            ▼
                   ┌─────────────────┐
                   │  Search Heads   │
                   │  (Web UI/API)   │
                   └─────────────────┘
```

### SPL Examples

```spl
// Basic search
index=main sourcetype=nginx status>=400

// Stats
index=main sourcetype=nginx
| stats count by status

// Time chart
index=main sourcetype=api
| timechart span=5m count by level

// Top errors
index=main level=error
| top 10 error_message

// Transaction tracking
index=main
| transaction request_id
| where duration > 5

// Join
index=main sourcetype=api
| join user_id [search index=users]

// Complex analysis
index=main sourcetype=api
| eval response_time_ms = response_time * 1000
| where response_time_ms > 1000
| stats avg(response_time_ms) as avg_latency,
        count as slow_requests
        by endpoint
| sort - slow_requests
```

### Deployment Options

| Option | Use Case |
|--------|----------|
| Splunk Cloud | SaaS, managed |
| Splunk Enterprise | On-premises |
| Splunk Free | <500MB/day, single user |

---

## Log Collectors

### Fluentd

Plugin-based log collector. Ruby-based.

```xml
<!-- fluent.conf -->
<source>
  @type tail
  path /var/log/nginx/access.log
  pos_file /var/log/td-agent/nginx-access.log.pos
  tag nginx.access
  <parse>
    @type nginx
  </parse>
</source>

<filter nginx.access>
  @type record_transformer
  <record>
    hostname "#{Socket.gethostname}"
  </record>
</filter>

<match nginx.**>
  @type elasticsearch
  host elasticsearch
  port 9200
  logstash_format true
  <buffer>
    @type file
    path /var/log/td-agent/buffer/elasticsearch
    flush_interval 5s
  </buffer>
</match>
```

### Fluent Bit

Lightweight version of Fluentd. C-based.

```ini
# fluent-bit.conf
[SERVICE]
    Flush         5
    Daemon        Off
    Log_Level     info

[INPUT]
    Name          tail
    Path          /var/log/containers/*.log
    Parser        docker
    Tag           kube.*
    Mem_Buf_Limit 5MB

[FILTER]
    Name          kubernetes
    Match         kube.*
    Merge_Log     On
    K8S-Logging.Parser On

[OUTPUT]
    Name          loki
    Match         *
    Host          loki
    Port          3100
    Labels        job=fluent-bit
```

### Vector

High-performance observability data pipeline.

```toml
# vector.toml
[sources.logs]
type = "file"
include = ["/var/log/**/*.log"]

[transforms.parse]
type = "remap"
inputs = ["logs"]
source = '''
. = parse_json!(.message)
.timestamp = now()
'''

[transforms.filter_errors]
type = "filter"
inputs = ["parse"]
condition = '.level == "error"'

[sinks.loki]
type = "loki"
inputs = ["parse"]
endpoint = "http://loki:3100"
labels.service = "{{ service }}"

[sinks.elasticsearch]
type = "elasticsearch"
inputs = ["filter_errors"]
endpoints = ["http://elasticsearch:9200"]
```

### Collector Comparison

| Aspect | Fluentd | Fluent Bit | Vector |
|--------|---------|------------|--------|
| Language | Ruby | C | Rust |
| Memory | ~40MB | ~1MB | ~10MB |
| Plugins | 1000+ | 70+ | 100+ |
| Performance | Good | Excellent | Excellent |
| Use case | Feature-rich | Resource-constrained | High-throughput |

---

## Comparison Matrix

| Feature | Loki | Elasticsearch | Splunk |
|---------|:----:|:-------------:|:------:|
| Full-text search | Slow | Fast | Fast |
| Label/field queries | Fast | Fast | Fast |
| Storage cost | Low | High | High |
| Query language | LogQL | KQL/Lucene | SPL |
| Learning curve | Low | Medium | Medium |
| Self-hosted | ✅ | ✅ | ✅ |
| Cloud hosted | Grafana Cloud | Elastic Cloud | Splunk Cloud |
| SIEM features | ❌ | ✅ | ✅ |
| Visualization | Grafana | Kibana | Splunk UI |

### Cost Comparison

| Storage Approach | Relative Cost |
|------------------|---------------|
| Loki (object storage) | $ |
| Elasticsearch (SSD) | $$$ |
| Splunk (indexed) | $$$$ |

---

## Decision Guide

| Scenario | Recommendation |
|----------|----------------|
| Already use Grafana | Loki |
| Full-text search critical | Elasticsearch |
| Enterprise/compliance | Splunk |
| Cost-sensitive | Loki |
| SIEM/Security | Splunk or Elastic SIEM |
| Kubernetes native | Loki + Promtail |
| High-cardinality labels | Loki |
| Complex log parsing | Elasticsearch |

---

## Related

- [[Observability Stack]]
- [[Distributed Tracing]]
- [[Prometheus]]
- [[Grafana]]
- [[OpenTelemetry]]
