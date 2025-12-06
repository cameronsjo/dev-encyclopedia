---
title: Systems Language Performance
aliases:
  - Language Performance
  - C vs Rust vs Go
  - Native Performance
tags:
  - concept
  - performance
  - systems
  - comparison
type: comparison
status: complete
created: "2025-12-04"
---

# Systems Language Performance

Performance characteristics of C, C++, Rust, Go, and WebAssembly.

## Overview

| Aspect | C | C++ | Rust | Go | WASM |
|--------|---|-----|------|-----|------|
| Memory management | Manual | Manual/RAII | Ownership | GC | Linear memory |
| Runtime | None | Minimal | None | GC + scheduler | VM/JIT |
| Zero-cost abstractions | ✅ | ✅ | ✅ | ❌ | ✅ |
| Compile time | Fast | Slow | Slow | Fast | Varies |
| Binary size | Tiny | Medium | Medium | Large | Small |
| Startup time | Instant | Instant | Instant | Fast | Fast-Instant |

---

## Performance Model

### C

The baseline. Direct hardware mapping.

```
Source → Compiler → Machine Code → CPU

┌─────────────────────────────────────────┐
│              Your Code                   │
├─────────────────────────────────────────┤
│              C Runtime                   │
│         (minimal: crt0, libc)           │
├─────────────────────────────────────────┤
│            Operating System              │
├─────────────────────────────────────────┤
│              Hardware                    │
└─────────────────────────────────────────┘
```

**Characteristics:**

- No hidden costs
- Predictable memory layout
- Direct syscalls possible
- Undefined behavior risks

### C++

C with abstractions. Zero-cost when done right.

```
Source → Compiler → Machine Code → CPU
         │
         ├── Templates (compile-time)
         ├── RAII (no runtime cost)
         └── Virtual dispatch (vtable indirection)
```

**Characteristics:**

- Zero-cost abstractions (templates, RAII, constexpr)
- Costs when using: virtual, RTTI, exceptions
- More optimization opportunities than C
- Complexity can hide performance issues

### Rust

Zero-cost abstractions with safety guarantees.

```
Source → Compiler → LLVM IR → Machine Code → CPU
         │
         ├── Borrow checker (compile-time only)
         ├── Monomorphization (like C++ templates)
         └── No GC, no runtime
```

**Characteristics:**

- Same performance ceiling as C/C++
- Safety checks at compile time, not runtime
- Predictable: no hidden allocations
- LLVM backend (same optimizations as Clang)

### Go

Simplicity over raw performance. GC + runtime.

```
Source → Compiler → Machine Code → CPU
                         │
              ┌──────────┴──────────┐
              │      Go Runtime      │
              ├─────────────────────┤
              │  Goroutine scheduler │
              │  Garbage collector   │
              │  Stack management    │
              └─────────────────────┘
```

**Characteristics:**

- GC pauses (sub-millisecond, but present)
- Goroutine overhead (~2KB initial stack)
- Escape analysis reduces allocations
- No generics until 1.18 (some runtime costs before)

### WebAssembly

Portable bytecode. Performance depends on runtime.

```
Source → Compiler → .wasm → Runtime → (JIT/AOT) → CPU
                              │
                    ┌─────────┴─────────┐
                    │    WASM Runtime    │
                    ├───────────────────┤
                    │  Linear memory     │
                    │  Bounds checking   │
                    │  Sandboxing        │
                    └───────────────────┘
```

**Characteristics:**

- ~0.8-1.0x native speed (JIT/AOT)
- Mandatory bounds checking
- No direct syscalls (WASI abstraction)
- Portable but not free

---

## Memory Management

| Approach | Languages | Overhead | Predictability |
|----------|-----------|----------|----------------|
| Manual | C | None | High (if correct) |
| RAII | C++, Rust | None | High |
| Ownership | Rust | None | High |
| Ref counting | Swift, Rust (Arc) | Counter updates | Medium |
| Tracing GC | Go, Java, C# | Pause + scan | Lower |

### Memory Layout Control

```
C/C++/Rust:
┌─────────────────────────────────────┐
│ struct Point { float x, y, z; }     │
│ Memory: [x][y][z] (12 bytes, packed)│
└─────────────────────────────────────┘

Go:
┌─────────────────────────────────────┐
│ type Point struct { X, Y, Z float32 }│
│ Memory: [X][Y][Z] (12 bytes)        │
│ But slices: pointer + len + cap     │
└─────────────────────────────────────┘
```

### Allocation Patterns

| Pattern | C/C++ | Rust | Go |
|---------|-------|------|-----|
| Stack allocation | Default | Default | Escape analysis |
| Heap allocation | malloc/new | Box, Vec | make, new, literals |
| Arena/pool | Manual | bumpalo | sync.Pool |
| Zero-copy | Possible | Possible | Harder (GC moves) |

---

## Runtime Overhead

### Startup Time

| Language | Cold Start | Notes |
|----------|------------|-------|
| C | ~1ms | Minimal init |
| C++ | ~1-5ms | Static constructors |
| Rust | ~1ms | No runtime init |
| Go | ~5-10ms | Runtime, GC init |
| WASM (JIT) | ~5-50ms | Parse + compile |
| WASM (AOT) | ~1-5ms | Pre-compiled |

### Memory Footprint

| Language | Minimal Binary | Hello World | Runtime Overhead |
|----------|----------------|-------------|------------------|
| C | ~10KB | ~20KB | libc only |
| C++ | ~50KB | ~100KB+ | libstdc++ |
| Rust | ~200KB | ~300KB | std library |
| Go | ~1.2MB | ~1.8MB | Runtime + GC |
| WASM | ~10KB | ~50KB | + runtime memory |

_Static linking increases binary size but reduces dependencies_

---

## Benchmark Patterns

### Compute-Bound

Raw number crunching, SIMD-friendly.

| Task | C | C++ | Rust | Go | WASM |
|------|:-:|:---:|:----:|:--:|:----:|
| Matrix multiply | 1.0x | 1.0x | 1.0x | 1.3-1.5x | 1.0-1.2x |
| Image processing | 1.0x | 1.0x | 1.0x | 1.2-1.5x | 1.0-1.3x |
| Crypto | 1.0x | 1.0x | 1.0x | 1.1-1.3x | 1.0-1.2x |

_SIMD support: C/C++/Rust (intrinsics), Go (limited), WASM (SIMD proposal)_

### Memory-Bound

Cache efficiency, allocation patterns.

| Task | C | C++ | Rust | Go |
|------|:-:|:---:|:----:|:--:|
| Large array traversal | 1.0x | 1.0x | 1.0x | 1.0-1.1x |
| Pointer chasing | 1.0x | 1.0x | 1.0x | 1.0x |
| Many small allocations | 1.0x | 1.0-1.2x | 1.0-1.1x | 1.5-3.0x |

_Go's GC adds overhead for allocation-heavy workloads_

### I/O-Bound

Syscalls, async, concurrency.

| Task | C | C++ | Rust | Go |
|------|:-:|:---:|:----:|:--:|
| File I/O | 1.0x | 1.0x | 1.0x | 1.0-1.1x |
| Network (sync) | 1.0x | 1.0x | 1.0x | 1.0x |
| Network (async) | Complex | Complex | 1.0x (tokio) | 1.0x (goroutines) |
| 10K connections | Hard | Hard | Easy | Easy |

_Go and Rust async excel at concurrent I/O_

---

## Latency vs Throughput

### GC Impact (Go)

```
Go GC characteristics:
├── Sub-millisecond pauses (typically <500μs)
├── Concurrent marking
├── Write barriers during GC
└── GOGC controls frequency vs memory

Tuning:
GOGC=100 (default) — GC when heap doubles
GOGC=200 — Less frequent, more memory
GOGC=off — Disable GC (memory grows forever)
```

### Predictability Spectrum

```
Most predictable ◄──────────────────────► Least predictable

    C          Rust        C++         Go
    │           │           │           │
    │           │           │           └── GC pauses
    │           │           └── Exceptions, virtual
    │           └── Safe, but deterministic
    └── Manual, undefined behavior risk
```

---

## Concurrency Performance

### Threading Models

| Language | Model | Overhead per Thread/Task |
|----------|-------|-------------------------|
| C | pthreads | ~8MB stack (configurable) |
| C++ | std::thread | ~8MB stack |
| Rust | std::thread | ~8MB stack |
| Rust | tokio task | ~few KB |
| Go | goroutine | ~2KB initial, grows |

### Scaling

```
10,000 concurrent connections:

C/C++ (threads):     10,000 × 8MB = 80GB (impossible)
C/C++ (epoll/io_uring): Works, complex code
Rust (tokio):        Works, ~20-50MB
Go (goroutines):     Works, ~20-50MB
```

---

## Compiler Optimizations

### Optimization Comparison

| Optimization | C (GCC/Clang) | C++ | Rust | Go |
|--------------|:-------------:|:---:|:----:|:--:|
| Inlining | ✅ | ✅ | ✅ | ✅ (limited) |
| LTO | ✅ | ✅ | ✅ | ❌ |
| PGO | ✅ | ✅ | ✅ | ✅ |
| Auto-vectorization | ✅ | ✅ | ✅ | Limited |
| Devirtualization | N/A | ✅ | ✅ (traits) | ✅ (interfaces) |
| Escape analysis | N/A | N/A | N/A | ✅ |

### Build Time vs Runtime

| Language | -O0 | -O2/-O3 | LTO | PGO |
|----------|-----|---------|-----|-----|
| C | Fast | Medium | Slow | Slowest |
| C++ | Medium | Slow | Very slow | Slowest |
| Rust | Slow | Slower | Very slow | Slowest |
| Go | Fast | Fast | N/A | Slow |

---

## WASM Performance Details

### Overhead Sources

| Source | Cost | Mitigation |
|--------|------|------------|
| Bounds checking | 1-5% | Sometimes elided |
| Indirect calls | 2-10% | Speculative |
| Memory model | 0-5% | Linear memory helps |
| 32-bit pointers | Can help | Less memory traffic |
| No SIMD (old) | 2-4x slower | SIMD proposal |

### WASM vs Native

```
Typical performance (compute-bound):

Native (C/Rust):  ████████████████████ 100%
WASM (AOT):       ██████████████████   90%
WASM (JIT):       ████████████████     80%
WASM (interpret): ████████             40%
```

### Best WASM Performance

1. Use Rust or C/C++ as source
2. AOT compile if possible
3. Minimize host calls
4. Use SIMD when available
5. Batch operations

---

## Decision Guide

| Priority | Best Choice |
|----------|-------------|
| Maximum performance, control | C or Rust |
| Performance + safety | Rust |
| Performance + simplicity | Go |
| Team knows C++ | C++ |
| Portability + sandbox | WASM (from Rust/C) |
| Fast compilation | Go or C |
| Predictable latency | Rust or C |
| High concurrency | Go or Rust (async) |
| Embedded/minimal | C or Rust (no_std) |

### Trade-offs Summary

| Language | Gives Up | Gets |
|----------|----------|------|
| C | Safety | Maximum control |
| C++ | Simplicity | Abstractions + control |
| Rust | Compile time | Safety + control |
| Go | Raw speed | Simplicity + productivity |
| WASM | Some speed | Portability + sandbox |

---

## Benchmarking Resources

- [Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)
- [TechEmpower Web Frameworks](https://www.techempower.com/benchmarks/)
- [Are We Fast Yet](https://arewefastyet.com/) (JS/WASM)

---

## Related

- [[WebAssembly Runtimes]]
- [[JavaScript Runtimes]]
- [[Big O Notation]]
- [[Rust]]
- [[Go]]
- [[C++]]
