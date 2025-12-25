---
title: REST vs GraphQL vs gRPC
aliases:
  - API Styles
  - API Comparison
tags:
  - cs
  - api
  - architecture
  - comparison
type: comparison
status: complete
created: "2025-12-16"
---

# REST vs GraphQL vs gRPC

Comparison of major API architectural styles for building web services.

## Overview

| Aspect | REST | GraphQL | gRPC |
|--------|------|---------|------|
| **Type** | Architectural style | Query language | RPC framework |
| **Protocol** | HTTP | HTTP | HTTP/2 |
| **Data Format** | JSON (typically) | JSON | Protocol Buffers |
| **Schema** | Optional (OpenAPI) | Required (SDL) | Required (Protobuf) |
| **Transport** | Text | Text | Binary |

## REST

### Characteristics

```
GET    /users/123        → Read user
POST   /users            → Create user
PUT    /users/123        → Update user
DELETE /users/123        → Delete user
```

| Principle | Description |
|-----------|-------------|
| **Stateless** | Each request contains all needed info |
| **Resource-based** | URLs represent resources |
| **HTTP Verbs** | GET, POST, PUT, PATCH, DELETE |
| **Cacheable** | HTTP caching mechanisms |
| **Uniform Interface** | Standard conventions |

### Example

```bash
# Get user
GET /api/users/123
Accept: application/json

Response:
{
  "id": 123,
  "name": "Alice",
  "email": "alice@example.com",
  "posts": [
    {"id": 1, "title": "Hello"},
    {"id": 2, "title": "World"}
  ]
}
```

### Pros & Cons

| Pros | Cons |
|------|------|
| ✅ Simple, well-understood | ❌ Over-fetching (too much data) |
| ✅ HTTP caching | ❌ Under-fetching (multiple requests) |
| ✅ Stateless, scalable | ❌ No standard query language |
| ✅ Wide tooling support | ❌ Versioning challenges |
| ✅ Browser-friendly | ❌ Multiple round trips |

## GraphQL

### Characteristics

```graphql
# Single endpoint, query what you need
POST /graphql

query {
  user(id: 123) {
    name
    email
    posts(first: 5) {
      title
    }
  }
}
```

| Feature | Description |
|---------|-------------|
| **Single Endpoint** | All queries to /graphql |
| **Client-Specified** | Client asks for exact fields |
| **Strongly Typed** | Schema defines all types |
| **Introspection** | Schema is queryable |
| **Real-time** | Subscriptions for live data |

### Schema Definition

```graphql
type User {
  id: ID!
  name: String!
  email: String!
  posts: [Post!]!
}

type Post {
  id: ID!
  title: String!
  content: String
  author: User!
}

type Query {
  user(id: ID!): User
  users(limit: Int): [User!]!
}

type Mutation {
  createUser(name: String!, email: String!): User!
  updateUser(id: ID!, name: String): User!
}

type Subscription {
  postCreated: Post!
}
```

### Example Query

```graphql
# Query
query GetUserWithPosts($userId: ID!) {
  user(id: $userId) {
    name
    posts(first: 5) {
      title
      createdAt
    }
  }
}

# Variables
{ "userId": "123" }

# Response
{
  "data": {
    "user": {
      "name": "Alice",
      "posts": [
        { "title": "Hello", "createdAt": "2024-01-15" },
        { "title": "World", "createdAt": "2024-01-16" }
      ]
    }
  }
}
```

### Pros & Cons

| Pros | Cons |
|------|------|
| ✅ No over/under-fetching | ❌ Complexity (N+1, caching) |
| ✅ Single request | ❌ HTTP caching harder |
| ✅ Strong typing | ❌ File uploads awkward |
| ✅ Great for mobile | ❌ Learning curve |
| ✅ Introspection | ❌ Rate limiting complex |

## gRPC

### Characteristics

```protobuf
// Define service in .proto file
service UserService {
  rpc GetUser(GetUserRequest) returns (User);
  rpc CreateUser(CreateUserRequest) returns (User);
  rpc StreamUsers(StreamRequest) returns (stream User);
}

message User {
  int64 id = 1;
  string name = 2;
  string email = 3;
}

message GetUserRequest {
  int64 id = 1;
}
```

| Feature | Description |
|---------|-------------|
| **Binary Protocol** | Protocol Buffers (efficient) |
| **HTTP/2** | Multiplexing, streaming |
| **Code Generation** | Clients generated from .proto |
| **Streaming** | Client, server, bidirectional |
| **Strongly Typed** | Compiled schema |

### Example Usage

```go
// Generated Go client
client := pb.NewUserServiceClient(conn)

// Unary call
user, err := client.GetUser(ctx, &pb.GetUserRequest{Id: 123})

// Server streaming
stream, err := client.StreamUsers(ctx, &pb.StreamRequest{})
for {
    user, err := stream.Recv()
    if err == io.EOF {
        break
    }
    fmt.Println(user)
}
```

### Streaming Types

| Type | Description |
|------|-------------|
| **Unary** | Single request, single response |
| **Server Streaming** | Single request, stream of responses |
| **Client Streaming** | Stream of requests, single response |
| **Bidirectional** | Stream both ways |

### Pros & Cons

| Pros | Cons |
|------|------|
| ✅ Very fast (binary) | ❌ Not browser-native |
| ✅ Strong typing | ❌ Harder to debug (binary) |
| ✅ Streaming built-in | ❌ Learning curve |
| ✅ Code generation | ❌ Requires HTTP/2 |
| ✅ Bi-directional | ❌ Less tooling than REST |

## Comparison Matrix

| Feature | REST | GraphQL | gRPC |
|---------|------|---------|------|
| **Protocol** | HTTP/1.1+ | HTTP/1.1+ | HTTP/2 |
| **Format** | JSON | JSON | Protobuf |
| **Schema** | Optional | Required | Required |
| **Browser Support** | ✅ Native | ✅ Via HTTP | ⚠️ gRPC-web |
| **Caching** | ✅ HTTP caching | ⚠️ Custom | ❌ No standard |
| **Real-time** | ⚠️ WebSocket/SSE | ✅ Subscriptions | ✅ Streaming |
| **File Upload** | ✅ Easy | ⚠️ Workarounds | ⚠️ Chunked |
| **Tooling** | ✅ Excellent | ✅ Good | ⚠️ Growing |
| **Learning Curve** | Low | Medium | Medium-High |

## Performance

| Aspect | REST | GraphQL | gRPC |
|--------|------|---------|------|
| **Payload Size** | Medium | Small-Medium | Small |
| **Serialization** | Slow (JSON) | Slow (JSON) | Fast (binary) |
| **Latency** | Higher | Medium | Lowest |
| **Throughput** | Good | Good | Excellent |

**Typical Latency:**

```
REST:    ~100ms
GraphQL: ~80ms
gRPC:    ~10-30ms
```

## Use Case Decision Guide

### Choose REST When

| Scenario | Reason |
|----------|--------|
| **Public APIs** | Universal understanding |
| **Simple CRUD** | Resources map well |
| **Caching important** | HTTP caching works |
| **Browser-first** | Native support |
| **Team new to APIs** | Lowest learning curve |

### Choose GraphQL When

| Scenario | Reason |
|----------|--------|
| **Mobile apps** | Minimize data transfer |
| **Multiple frontends** | Each gets what it needs |
| **Rapid iteration** | No endpoint changes |
| **Complex data needs** | Nested resources |
| **API aggregation** | Combine multiple sources |

### Choose gRPC When

| Scenario | Reason |
|----------|--------|
| **Microservices** | Service-to-service calls |
| **High performance** | Binary protocol |
| **Streaming needed** | Built-in support |
| **Polyglot systems** | Code generation |
| **Internal APIs** | Not browser-facing |

## Hybrid Approaches

### Common Patterns

```
┌─────────────────────────────────────────────────────┐
│                    Frontend                          │
└─────────────────────────┬───────────────────────────┘
                          │
                    GraphQL / REST
                          │
                          ▼
┌─────────────────────────────────────────────────────┐
│                  API Gateway                         │
└─────────────────────────┬───────────────────────────┘
                          │
                         gRPC
                          │
      ┌───────────────────┼───────────────────┐
      ▼                   ▼                   ▼
┌──────────┐       ┌──────────┐       ┌──────────┐
│ Service A│       │ Service B│       │ Service C│
└──────────┘       └──────────┘       └──────────┘
```

### REST + GraphQL

- REST for simple resources
- GraphQL for complex queries
- GraphQL wrapping REST endpoints

### gRPC + REST

- gRPC for internal microservices
- REST/GraphQL gateway for external APIs
- gRPC-web for browser support

## Related

- [[API Design Patterns]] — API best practices
- [[OAuth and OIDC]] — API authentication
- [[Networking Fundamentals]] — HTTP basics
- [[Computer Science MOC]] — All CS topics
