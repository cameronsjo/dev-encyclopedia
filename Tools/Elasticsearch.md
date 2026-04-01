---
title: Elasticsearch
aliases:
  - ES
  - Elastic
  - ELK Stack
tags:
  - tool
  - database
  - search
  - analytics
type: reference
status: complete
created: "2025-12-18"
---

# Elasticsearch

Distributed search and analytics engine built on Apache Lucene.

## Overview

| Aspect | Details |
|--------|---------|
| **Type** | Search engine / Document store |
| **Based On** | Apache Lucene |
| **Protocol** | REST API (JSON) |
| **License** | SSPL / Elastic License |
| **Written In** | Java |
| **Use Cases** | Full-text search, log analytics, APM |

## Core Concepts

| Concept | Description |
|---------|-------------|
| **Index** | Collection of documents (like a database) |
| **Document** | JSON record (like a row) |
| **Field** | Key-value pair in document |
| **Mapping** | Schema definition for fields |
| **Shard** | Horizontal partition of index |
| **Replica** | Copy of shard for redundancy |
| **Node** | Single Elasticsearch instance |
| **Cluster** | Group of nodes |

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                     Cluster                          │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  │
│  │   Node 1    │  │   Node 2    │  │   Node 3    │  │
│  │  (Master)   │  │   (Data)    │  │   (Data)    │  │
│  │ ┌─────────┐ │  │ ┌─────────┐ │  │ ┌─────────┐ │  │
│  │ │Shard P0 │ │  │ │Shard P1 │ │  │ │Shard P2 │ │  │
│  │ │Shard R1 │ │  │ │Shard R2 │ │  │ │Shard R0 │ │  │
│  │ └─────────┘ │  │ └─────────┘ │  │ └─────────┘ │  │
│  └─────────────┘  └─────────────┘  └─────────────┘  │
└─────────────────────────────────────────────────────┘

P = Primary shard, R = Replica shard
```

## Basic Operations

### Index Management

```bash
# Create index
PUT /products
{
  "settings": {
    "number_of_shards": 3,
    "number_of_replicas": 1
  }
}

# Check index exists
HEAD /products

# Get index info
GET /products

# Delete index
DELETE /products

# List all indices
GET /_cat/indices?v
```

### Document CRUD

```bash
# Create document (auto ID)
POST /products/_doc
{
  "name": "Laptop",
  "price": 999.99,
  "category": "electronics"
}

# Create document (specific ID)
PUT /products/_doc/1
{
  "name": "Laptop",
  "price": 999.99,
  "category": "electronics"
}

# Get document
GET /products/_doc/1

# Update document (partial)
POST /products/_update/1
{
  "doc": {
    "price": 899.99
  }
}

# Delete document
DELETE /products/_doc/1

# Bulk operations
POST /_bulk
{"index": {"_index": "products", "_id": "1"}}
{"name": "Laptop", "price": 999}
{"index": {"_index": "products", "_id": "2"}}
{"name": "Mouse", "price": 29}
{"delete": {"_index": "products", "_id": "3"}}
```

## Mappings

### Define Schema

```bash
PUT /products
{
  "mappings": {
    "properties": {
      "name": { "type": "text" },
      "sku": { "type": "keyword" },
      "price": { "type": "float" },
      "quantity": { "type": "integer" },
      "created": { "type": "date" },
      "in_stock": { "type": "boolean" },
      "tags": { "type": "keyword" },
      "description": {
        "type": "text",
        "analyzer": "english"
      },
      "location": { "type": "geo_point" }
    }
  }
}
```

### Field Types

| Type | Use Case |
|------|----------|
| `text` | Full-text search (analyzed) |
| `keyword` | Exact match, aggregations |
| `long/integer/short/byte` | Numeric |
| `float/double` | Floating point |
| `boolean` | True/false |
| `date` | Datetime |
| `geo_point` | Lat/lon coordinates |
| `nested` | Arrays of objects |
| `object` | JSON objects |

## Search Queries

### Match Queries (Full-Text)

```bash
# Simple match
GET /products/_search
{
  "query": {
    "match": {
      "description": "wireless bluetooth"
    }
  }
}

# Match phrase
GET /products/_search
{
  "query": {
    "match_phrase": {
      "description": "noise cancelling"
    }
  }
}

# Multi-match
GET /products/_search
{
  "query": {
    "multi_match": {
      "query": "laptop",
      "fields": ["name^3", "description"]
    }
  }
}
```

### Term Queries (Exact Match)

```bash
# Term (exact match on keyword)
GET /products/_search
{
  "query": {
    "term": {
      "category": "electronics"
    }
  }
}

# Terms (multiple values)
GET /products/_search
{
  "query": {
    "terms": {
      "tags": ["sale", "new"]
    }
  }
}

# Range
GET /products/_search
{
  "query": {
    "range": {
      "price": {
        "gte": 100,
        "lte": 500
      }
    }
  }
}

# Exists
GET /products/_search
{
  "query": {
    "exists": {
      "field": "discount"
    }
  }
}
```

### Compound Queries

```bash
# Bool query
GET /products/_search
{
  "query": {
    "bool": {
      "must": [
        { "match": { "description": "laptop" } }
      ],
      "filter": [
        { "term": { "in_stock": true } },
        { "range": { "price": { "lte": 1000 } } }
      ],
      "should": [
        { "term": { "brand": "apple" } }
      ],
      "must_not": [
        { "term": { "category": "refurbished" } }
      ],
      "minimum_should_match": 1
    }
  }
}
```

| Clause | Behavior | Affects Score |
|--------|----------|---------------|
| `must` | Required | ✅ Yes |
| `filter` | Required | ❌ No (cached) |
| `should` | Optional boost | ✅ Yes |
| `must_not` | Excluded | ❌ No |

## Aggregations

### Metric Aggregations

```bash
GET /products/_search
{
  "size": 0,
  "aggs": {
    "avg_price": { "avg": { "field": "price" } },
    "max_price": { "max": { "field": "price" } },
    "min_price": { "min": { "field": "price" } },
    "total_value": { "sum": { "field": "price" } },
    "price_stats": { "stats": { "field": "price" } }
  }
}
```

### Bucket Aggregations

```bash
GET /products/_search
{
  "size": 0,
  "aggs": {
    "by_category": {
      "terms": {
        "field": "category",
        "size": 10
      },
      "aggs": {
        "avg_price": { "avg": { "field": "price" } }
      }
    },
    "price_ranges": {
      "range": {
        "field": "price",
        "ranges": [
          { "to": 100 },
          { "from": 100, "to": 500 },
          { "from": 500 }
        ]
      }
    },
    "by_date": {
      "date_histogram": {
        "field": "created",
        "calendar_interval": "month"
      }
    }
  }
}
```

## Analyzers

### Built-in Analyzers

| Analyzer | Description |
|----------|-------------|
| `standard` | Default, Unicode text |
| `simple` | Lowercase, split on non-letters |
| `whitespace` | Split on whitespace |
| `english` | English stemming, stopwords |
| `keyword` | No analysis (exact) |

### Test Analyzer

```bash
GET /_analyze
{
  "analyzer": "standard",
  "text": "The Quick Brown Fox"
}
# Result: ["the", "quick", "brown", "fox"]

GET /_analyze
{
  "analyzer": "english",
  "text": "running runners ran"
}
# Result: ["run", "runner", "ran"]
```

### Custom Analyzer

```bash
PUT /my_index
{
  "settings": {
    "analysis": {
      "analyzer": {
        "my_analyzer": {
          "type": "custom",
          "tokenizer": "standard",
          "filter": ["lowercase", "asciifolding", "my_stemmer"]
        }
      },
      "filter": {
        "my_stemmer": {
          "type": "stemmer",
          "language": "english"
        }
      }
    }
  }
}
```

## Index Templates

```bash
PUT /_index_template/logs_template
{
  "index_patterns": ["logs-*"],
  "template": {
    "settings": {
      "number_of_shards": 1
    },
    "mappings": {
      "properties": {
        "@timestamp": { "type": "date" },
        "message": { "type": "text" },
        "level": { "type": "keyword" },
        "service": { "type": "keyword" }
      }
    }
  }
}
```

## ELK Stack

```
┌───────────┐    ┌───────────────┐    ┌─────────────┐
│  Beats    │───▶│  Logstash     │───▶│Elasticsearch│
│(Filebeat) │    │ (Processing)  │    │  (Storage)  │
└───────────┘    └───────────────┘    └──────┬──────┘
                                             │
                                      ┌──────▼──────┐
                                      │   Kibana    │
                                      │(Visualize)  │
                                      └─────────────┘
```

| Component | Purpose |
|-----------|---------|
| **Elasticsearch** | Store and search data |
| **Logstash** | Ingest and transform data |
| **Kibana** | Visualize and explore data |
| **Beats** | Lightweight data shippers |

## Client Libraries

### Node.js

```javascript
const { Client } = require('@elastic/elasticsearch');

const client = new Client({ node: 'http://localhost:9200' });

// Index document
await client.index({
  index: 'products',
  id: '1',
  document: { name: 'Laptop', price: 999 }
});

// Search
const result = await client.search({
  index: 'products',
  query: { match: { name: 'laptop' } }
});

console.log(result.hits.hits);
```

### Python

```python
from elasticsearch import Elasticsearch

es = Elasticsearch(['http://localhost:9200'])

# Index document
es.index(index='products', id=1, document={'name': 'Laptop', 'price': 999})

# Search
result = es.search(index='products', query={'match': {'name': 'laptop'}})
print(result['hits']['hits'])
```

## Performance Tips

| Tip | Rationale |
|-----|-----------|
| **Use filter context** | Cached, faster than query |
| **Avoid wildcard prefix** | `*foo` scans entire index |
| **Use keyword for exact** | Don't analyze when unnecessary |
| **Tune shard size** | 10-50GB per shard ideal |
| **Use bulk API** | Much faster than individual requests |
| **Set refresh_interval** | Increase for write-heavy loads |

## When to Use

| Good For | Not Ideal For |
|----------|---------------|
| ✅ Full-text search | ❌ Primary data store |
| ✅ Log analytics | ❌ Transactions |
| ✅ Real-time dashboards | ❌ Frequent updates |
| ✅ Autocomplete | ❌ Small datasets |
| ✅ Geospatial queries | ❌ Simple key-value |

## Related

- [[PostgreSQL]] — SQL full-text search alternative
- [[Redis]] — Caching layer
- [[Prometheus]] — Metrics (not logs)
- [[Tools MOC]] — All tools
