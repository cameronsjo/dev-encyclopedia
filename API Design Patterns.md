---
title: API Design Patterns
aliases:
  - API Design
  - Web APIs
tags:
  - api
  - architecture
  - web
  - backend
type: reference
status: complete
created: 2025-11-28
---

# API Design Patterns

Designing APIs that developers love.

## Overview

| Style | Use Case | Format |
|-------|----------|--------|
| REST | CRUD, resources | JSON over HTTP |
| GraphQL | Flexible queries | Single endpoint |
| gRPC | Service-to-service | Protobuf over HTTP/2 |
| tRPC | TypeScript full-stack | End-to-end types |
| WebSocket | Real-time, bidirectional | Persistent connection |
| Webhooks | Event notifications | HTTP callbacks |

---

## REST

### Core Principles

| Principle | Meaning |
|-----------|---------|
| Stateless | No server-side session |
| Resource-based | URLs represent resources |
| HTTP verbs | GET, POST, PUT, PATCH, DELETE |
| Uniform interface | Consistent patterns |

### URL Design

```
# Resources (nouns, plural)
GET    /users              # List users
GET    /users/123          # Get user
POST   /users              # Create user
PUT    /users/123          # Replace user
PATCH  /users/123          # Update user
DELETE /users/123          # Delete user

# Nested resources
GET    /users/123/posts    # User's posts
POST   /users/123/posts    # Create post for user

# Actions (when needed)
POST   /users/123/activate # Non-CRUD action
```

### HTTP Methods

| Method | Idempotent | Safe | Use |
|--------|------------|------|-----|
| GET | ✅ | ✅ | Read |
| POST | ❌ | ❌ | Create |
| PUT | ✅ | ❌ | Replace |
| PATCH | ❌ | ❌ | Partial update |
| DELETE | ✅ | ❌ | Delete |

### Status Codes

| Code | Meaning | When |
|------|---------|------|
| 200 | OK | Success with body |
| 201 | Created | Resource created |
| 204 | No Content | Success, no body |
| 400 | Bad Request | Invalid input |
| 401 | Unauthorized | No/invalid auth |
| 403 | Forbidden | Authenticated but not allowed |
| 404 | Not Found | Resource doesn't exist |
| 409 | Conflict | State conflict |
| 422 | Unprocessable | Validation failed |
| 429 | Too Many Requests | Rate limited |
| 500 | Internal Error | Server bug |

### Query Parameters

```
# Pagination
GET /users?page=2&per_page=20
GET /users?cursor=abc123&limit=20

# Filtering
GET /users?status=active&role=admin

# Sorting
GET /users?sort=created_at&order=desc

# Field selection
GET /users?fields=id,name,email

# Search
GET /users?q=john
```

### Response Format

```json
{
  "data": {
    "id": "123",
    "type": "user",
    "attributes": {
      "name": "John",
      "email": "john@example.com"
    }
  },
  "meta": {
    "total": 100,
    "page": 1
  }
}
```

### Error Format

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid input",
    "details": [
      {
        "field": "email",
        "message": "Invalid email format"
      }
    ]
  }
}
```

---

## GraphQL

### Why GraphQL?

| REST Problem | GraphQL Solution |
|--------------|------------------|
| Over-fetching | Request only needed fields |
| Under-fetching | Get related data in one query |
| Multiple endpoints | Single endpoint |
| Versioning | Schema evolution |

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
  author: User!
}

type Query {
  user(id: ID!): User
  users(limit: Int, offset: Int): [User!]!
}

type Mutation {
  createUser(input: CreateUserInput!): User!
  updateUser(id: ID!, input: UpdateUserInput!): User!
}

input CreateUserInput {
  name: String!
  email: String!
}
```

### Queries

```graphql
# Get exactly what you need
query {
  user(id: "123") {
    name
    email
    posts {
      title
    }
  }
}

# Variables
query GetUser($id: ID!) {
  user(id: $id) {
    name
  }
}
```

### Mutations

```graphql
mutation {
  createUser(input: { name: "John", email: "john@example.com" }) {
    id
    name
  }
}
```

### Subscriptions

```graphql
subscription {
  messageAdded(channelId: "123") {
    id
    content
    author {
      name
    }
  }
}
```

### GraphQL Trade-offs

| Pros | Cons |
|------|------|
| Flexible queries | Complexity |
| Single endpoint | Caching harder |
| Strong typing | N+1 queries |
| Great tooling | File uploads tricky |

---

## gRPC

### Why gRPC?

| Feature | Benefit |
|---------|---------|
| HTTP/2 | Multiplexing, streaming |
| Protobuf | Binary, efficient |
| Streaming | Bidirectional |
| Code gen | Type-safe clients |

### Protocol Buffers

```protobuf
syntax = "proto3";

package users;

service UserService {
  rpc GetUser(GetUserRequest) returns (User);
  rpc ListUsers(ListUsersRequest) returns (stream User);
  rpc CreateUser(CreateUserRequest) returns (User);
}

message User {
  string id = 1;
  string name = 2;
  string email = 3;
}

message GetUserRequest {
  string id = 1;
}

message ListUsersRequest {
  int32 page_size = 1;
  string page_token = 2;
}

message CreateUserRequest {
  string name = 1;
  string email = 2;
}
```

### Streaming Types

| Type | Description |
|------|-------------|
| Unary | Single request, single response |
| Server streaming | Single request, stream of responses |
| Client streaming | Stream of requests, single response |
| Bidirectional | Stream both directions |

### gRPC vs REST

| Aspect | gRPC | REST |
|--------|------|------|
| Format | Protobuf (binary) | JSON (text) |
| Speed | Faster | Slower |
| Browser support | Limited | Native |
| Tooling | Code gen required | Curl works |
| Streaming | Native | Workarounds |
| Human readable | No | Yes |

### When to Use gRPC

- Microservices communication
- Low latency requirements
- Streaming data
- Polyglot environments

---

## tRPC

### Why tRPC?

End-to-end type safety with zero code generation.

```
┌─────────────────────────────────────────────────────────┐
│                    TypeScript                            │
│  ┌─────────────┐                  ┌─────────────────┐   │
│  │   Client    │◄────────────────►│     Server      │   │
│  │  (React)    │  Types inferred  │   (Node.js)     │   │
│  └─────────────┘                  └─────────────────┘   │
└─────────────────────────────────────────────────────────┘
```

### Server Definition

```typescript
import { initTRPC } from '@trpc/server';
import { z } from 'zod';

const t = initTRPC.create();

const appRouter = t.router({
  user: t.router({
    get: t.procedure
      .input(z.object({ id: z.string() }))
      .query(async ({ input }) => {
        return await db.user.findUnique({ where: { id: input.id } });
      }),

    create: t.procedure
      .input(z.object({
        name: z.string(),
        email: z.string().email(),
      }))
      .mutation(async ({ input }) => {
        return await db.user.create({ data: input });
      }),
  }),
});

export type AppRouter = typeof appRouter;
```

### Client Usage

```typescript
import { createTRPCReact } from '@trpc/react-query';
import type { AppRouter } from '../server';

const trpc = createTRPCReact<AppRouter>();

function UserProfile({ id }: { id: string }) {
  // Fully typed - autocomplete works!
  const { data: user } = trpc.user.get.useQuery({ id });

  return <div>{user?.name}</div>;
}
```

### tRPC vs GraphQL vs REST

| Aspect | tRPC | GraphQL | REST |
|--------|------|---------|------|
| Type safety | Full | Schema-based | Manual |
| Code gen | None | Required | Optional |
| Learning curve | Low | Medium | Low |
| Flexibility | Procedures | Queries | Endpoints |
| Best for | Full-stack TS | Multi-client | General |

---

## Webhooks

### Pattern

```
┌──────────┐         ┌──────────┐
│  Source  │         │  Target  │
│ (GitHub) │────────►│(Your App)│
└──────────┘  HTTP   └──────────┘
              POST
```

### Best Practices

| Practice | Why |
|----------|-----|
| Verify signatures | Prevent spoofing |
| Respond quickly | Return 200, process async |
| Handle retries | Idempotency keys |
| Log everything | Debugging |

### Signature Verification

```typescript
import crypto from 'crypto';

function verifyWebhook(payload: string, signature: string, secret: string): boolean {
  const expected = crypto
    .createHmac('sha256', secret)
    .update(payload)
    .digest('hex');

  return crypto.timingSafeEqual(
    Buffer.from(signature),
    Buffer.from(`sha256=${expected}`)
  );
}
```

---

## API Versioning

### Strategies

| Strategy | Example | Pros | Cons |
|----------|---------|------|------|
| URL path | `/v1/users` | Clear, cacheable | URL pollution |
| Header | `Accept: application/vnd.api+json;v=1` | Clean URLs | Hidden |
| Query param | `/users?version=1` | Easy | Cache issues |

### Recommendation

URL path (`/v1/`) for simplicity. Only version when breaking changes.

---

## Rate Limiting

### Headers

```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1640000000
Retry-After: 60
```

### Algorithms

| Algorithm | Description |
|-----------|-------------|
| Fixed window | X requests per minute |
| Sliding window | Smoother limiting |
| Token bucket | Burst allowed |
| Leaky bucket | Constant rate |

---

## Pagination

### Offset-based

```
GET /users?page=2&per_page=20
```

| Pros | Cons |
|------|------|
| Simple | Inconsistent with changes |
| Jump to page | Slow on large datasets |

### Cursor-based

```
GET /users?cursor=abc123&limit=20
```

| Pros | Cons |
|------|------|
| Consistent | No page jumping |
| Fast | More complex |

### Response

```json
{
  "data": [...],
  "pagination": {
    "next_cursor": "xyz789",
    "has_more": true
  }
}
```

---

## Authentication

| Method | Use Case |
|--------|----------|
| API Keys | Server-to-server |
| JWT | Stateless auth |
| OAuth 2.0 | Third-party access |
| Session cookies | Web apps |

See [[Security Concepts]] for details.

---

## API Documentation

| Tool | Type |
|------|------|
| OpenAPI/Swagger | REST specification |
| GraphQL Playground | GraphQL explorer |
| Postman | Testing + docs |
| Redoc | OpenAPI renderer |

### OpenAPI Example

```yaml
openapi: 3.0.0
info:
  title: Users API
  version: 1.0.0
paths:
  /users:
    get:
      summary: List users
      parameters:
        - name: limit
          in: query
          schema:
            type: integer
      responses:
        '200':
          description: Success
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/User'
```

---

## Decision Guide

| Scenario | Recommendation |
|----------|----------------|
| Public API | REST + OpenAPI |
| Mobile app | REST or GraphQL |
| Microservices | gRPC |
| Full-stack TypeScript | tRPC |
| Complex queries, multiple clients | GraphQL |
| Real-time | WebSocket or gRPC streaming |
| Simple CRUD | REST |

---

## Related

- [[Security Concepts]]
- [[Web Frameworks]]
- [[domains/Web Development|Web Development]]
