---
title: Go
aliases:
  - Golang
tags:
  - language
  - backend
  - systems
type: reference
status: draft
created: '2025-12-03'
---

# Go

Statically typed, compiled language designed for simplicity and efficiency.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Procedural, concurrent |
| Typing | Static, strong, inferred |
| Compiled | Yes (fast compilation) |
| First release | 2009 (1.0 in 2012) |
| Designed by | Google (Rob Pike, Ken Thompson, Robert Griesemer) |

---

## Key Features

- Simple, minimal syntax
- Fast compilation
- Built-in concurrency (goroutines, channels)
- Garbage collected
- Static linking (single binary)
- Standard formatting (gofmt)

---

## Concurrency Model

| Concept | Description |
|---------|-------------|
| Goroutines | Lightweight threads managed by Go runtime |
| Channels | Type-safe communication between goroutines |
| Select | Multiplexing channel operations |
| sync package | Mutexes, wait groups, etc. |

---

## When to Use Go

**Strengths:**

- Simple to learn and read
- Excellent concurrency support
- Fast compilation and execution
- Single binary deployment
- Strong standard library

**Considerations:**

- No generics until Go 1.18
- Verbose error handling
- No exceptions

**Best for:**

- Microservices
- CLI tools
- DevOps tooling (Docker, Kubernetes)
- Network services
- Cloud infrastructure

---

## Related

- [[Build Systems]]
- [[Deployment]]
- [[API Design Patterns]]
