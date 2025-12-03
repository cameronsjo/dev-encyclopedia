---
title: Rust
aliases:
  - rs
tags:
  - language
  - systems
  - embedded
type: reference
status: draft
created: '2025-12-03'
---

# Rust

Systems programming language focused on safety, speed, and concurrency.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Multi-paradigm (functional, imperative, OOP) |
| Typing | Static, strong, inferred |
| Memory | Ownership system (no GC) |
| First release | 2010 (1.0 in 2015) |
| Designed by | Mozilla (now Rust Foundation) |

---

## Key Features

- Memory safety without garbage collection
- Ownership and borrowing system
- Zero-cost abstractions
- Pattern matching
- Traits (similar to interfaces)
- Fearless concurrency
- Cargo package manager

---

## Memory Model

| Concept | Description |
|---------|-------------|
| Ownership | Each value has exactly one owner |
| Borrowing | References without taking ownership |
| Lifetimes | Compiler-tracked reference validity |
| Drop | Automatic cleanup when owner goes out of scope |

---

## When to Use Rust

**Strengths:**

- Performance comparable to C/C++
- Memory safety guaranteed at compile time
- Excellent tooling (Cargo, rustfmt, clippy)
- Growing ecosystem

**Considerations:**

- Steep learning curve
- Longer compile times
- Smaller talent pool than C++

**Best for:**

- Systems programming
- WebAssembly
- CLI tools
- Performance-critical applications
- Embedded systems

---

## Related

- [[C++]]
- [[Bevy]]
- [[Tauri]]
- [[domains/Embedded & Systems|Embedded & Systems]]
