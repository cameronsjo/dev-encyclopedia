---
title: ORMs & Database Access
aliases:
  - Database Libraries
  - Data Access
  - ORM
tags:
  - comparison
  - languages
  - tools
  - database
  - orm
  - sql
  - csharp
  - go
  - python
  - typescript
  - rust
type: comparison
status: complete
created: 2025-11-28
---

# ORMs & Database Access

Cross-language comparison of database access patterns, ORMs, and query builders.

## Overview

| Language | ORM | Query Builder | Raw SQL |
|----------|-----|---------------|---------|
| C# | Entity Framework Core | Dapper | ADO.NET |
| Go | GORM | sqlx | database/sql |
| Python | SQLAlchemy, Django ORM | SQLAlchemy Core | psycopg, asyncpg |
| TypeScript | Prisma, TypeORM | Kysely, Knex | pg, mysql2 |
| Rust | Diesel, SeaORM | sqlx | tokio-postgres |

---

## Approaches

### Full ORM

**What:** Objects map to tables. ORM generates SQL from method calls.

**Pros:**
- Abstracts SQL completely
- Type-safe queries
- Automatic migrations
- Relationship handling

**Cons:**
- N+1 query problems
- Complex queries are awkward
- Performance overhead
- Learning the ORM, not SQL

**Best for:** CRUD-heavy apps, rapid development, teams unfamiliar with SQL.

### Query Builder

**What:** Programmatic SQL construction with type safety.

**Pros:**
- Close to SQL
- Composable queries
- Less magic than ORM
- Better performance control

**Cons:**
- More verbose than ORM
- Still not raw SQL performance
- Manual relationship handling

**Best for:** Complex queries, performance-sensitive code, SQL-comfortable teams.

### Raw SQL

**What:** Write SQL strings, map results manually.

**Pros:**
- Full control
- Best performance
- Use all database features
- No abstraction leaks

**Cons:**
- SQL injection risk if not careful
- Manual mapping
- No compile-time checks (usually)

**Best for:** Complex analytics, stored procedures, performance-critical paths.

---

## Language-Specific Notes

### C# — Entity Framework Core

The dominant ORM. Mature, feature-rich, well-integrated.

**Key features:**
- LINQ queries compile to SQL
- Code-first or database-first
- Migrations
- Change tracking
- Lazy/eager loading

**Dapper:** Micro-ORM. Raw SQL with object mapping. Use when EF Core is too heavy.

**When to use what:**

| Scenario | Choice |
|----------|--------|
| Standard CRUD app | EF Core |
| Complex reporting queries | Dapper or raw SQL |
| High-performance hot paths | Dapper |
| Stored procedures | Dapper |

### Go — Lean Toward Raw SQL

Go culture favors simplicity. Many teams use `database/sql` directly.

**Options:**

| Tool | Style | Use Case |
|------|-------|----------|
| database/sql | Raw | Maximum control |
| sqlx | Enhanced raw | Struct scanning, named params |
| GORM | Full ORM | Rapid development |
| sqlc | Generated | Type-safe from SQL |

**sqlc:** Write SQL, generate Go code. Best of both worlds—real SQL with type safety.

**Recommendation:** sqlx for most cases. sqlc if you want generated code. GORM if you need ORM features.

### Python — SQLAlchemy Dominates

SQLAlchemy has two APIs:
- **Core:** Query builder
- **ORM:** Full object mapping

**Django ORM:** Tightly coupled to Django. Great within Django, awkward outside.

**Async options:**
- SQLAlchemy 2.0 (async support)
- Tortoise ORM (async-first)
- databases (async query runner)

**Recommendation:** SQLAlchemy for standalone apps, Django ORM within Django.

### TypeScript — Prisma Leading

**Prisma:**
- Schema-first
- Generated type-safe client
- Excellent DX
- Migrations

**TypeORM:**
- Decorator-based
- More traditional ORM
- Older, more issues

**Kysely:**
- Query builder only
- Excellent TypeScript types
- No ORM features

**Drizzle:**
- SQL-like syntax
- Type-safe
- Growing fast

| Tool | Style | Best For |
|------|-------|----------|
| Prisma | Schema-first ORM | Most projects |
| Kysely | Query builder | Complex queries |
| Drizzle | SQL-like | SQL lovers |
| TypeORM | Traditional ORM | Legacy |

### Rust — Type Safety First

**Diesel:**
- Compile-time query checking
- Schema-aware types
- Sync only

**SeaORM:**
- Async-first
- More flexible
- Less strict checking

**sqlx:**
- Compile-time checked raw SQL
- Async
- No ORM features

**Recommendation:** sqlx for query builder approach, SeaORM for async ORM. Diesel for max type safety.

---

## Common Patterns

### Repository Pattern

Abstract database access behind an interface.

**Pros:** Testable, swappable backends.
**Cons:** Can become thin wrapper over ORM, adding indirection without value.

**When to use:** When you need to mock database access in tests, or might switch databases.

### Unit of Work

Group operations into a transaction.

**ORMs handle this:** EF Core's DbContext, SQLAlchemy's Session.

**Key:** One unit of work per request/operation.

### Connection Pooling

Reuse database connections.

**Every production app needs this.** All major libraries support it.

| Language | Pool |
|----------|------|
| C# | Built into providers |
| Go | Built into database/sql |
| Python | SQLAlchemy pool, asyncpg pool |
| TypeScript | Prisma managed, pg pool |
| Rust | sqlx pool, deadpool |

---

## N+1 Problem

The most common ORM performance issue.

**What happens:**
1. Fetch 10 users (1 query)
2. For each user, fetch their posts (10 queries)
3. Total: 11 queries instead of 2

**Solutions:**

| Strategy | How |
|----------|-----|
| Eager loading | `Include()` in EF, `select_related` in Django |
| Batch loading | DataLoader pattern |
| Query manually | Write the join yourself |
| Monitor | Log queries, catch in dev |

---

## Migrations

Schema changes over time.

| Language | Tool |
|----------|------|
| C# | EF Core Migrations |
| Go | golang-migrate, goose |
| Python | Alembic, Django migrations |
| TypeScript | Prisma Migrate, Knex migrations |
| Rust | Diesel migrations, sqlx-migrate |

**Best practices:**
- Version control migrations
- Test migrations on copy of prod data
- Make migrations reversible
- Small, incremental changes

---

## Feature Comparison

| Feature | EF Core | GORM | SQLAlchemy | Prisma | Diesel |
|---------|---------|------|------------|--------|--------|
| Async | ✅ | ❌ | ✅ 2.0 | ✅ | ❌ |
| Compile-time checks | ❌ | ❌ | ❌ | Generated | ✅ |
| Migrations | ✅ | ✅ | Alembic | ✅ | ✅ |
| Raw SQL escape | ✅ | ✅ | ✅ | ✅ | ✅ |
| Transactions | ✅ | ✅ | ✅ | ✅ | ✅ |
| Connection pool | Provider | ✅ | ✅ | ✅ | ✅ |

---

## Decision Guide

| Priority | Recommendation |
|----------|----------------|
| Rapid development | Full ORM (EF Core, Prisma, GORM) |
| Type safety | Prisma, Diesel, sqlx |
| Performance | Query builder or raw SQL |
| Complex queries | Query builder (Kysely, SQLAlchemy Core) |
| Max control | Raw SQL with mapping (Dapper, sqlx) |
| Async Python | SQLAlchemy 2.0 or Tortoise |

---

## Anti-Patterns

| Anti-Pattern | Problem | Fix |
|--------------|---------|-----|
| N+1 queries | Slow performance | Eager load or batch |
| ORM for analytics | Wrong tool | Use raw SQL |
| No connection pooling | Connection exhaustion | Always pool |
| Exposing ORM entities | Couples API to schema | Use DTOs |
| Long transactions | Lock contention | Keep transactions short |
| String concatenation for SQL | SQL injection | Use parameters |

---

## Related

- [[Web Frameworks]]
- [[Testing Frameworks]]
- [[Logging Libraries]]
- [[System Design]]
