---
title: Computer Science MOC
aliases:
  - CS MOC
tags:
  - moc
  - cs
  - computer-science
type: moc
created: '2025-11-28'
status: complete
---
# Computer Science MOC

Fundamental CS concepts, data structures, algorithms, and system design.

## Core Topics

### Data Structures

- [[Arrays & Lists]]
- [[Hash Tables]]
- [[Trees]]
- [[Graphs]]
- [[Heaps & Priority Queues]]
- [[Stacks & Queues]]

### Algorithms

- [[Sorting Algorithms]]
- [[Searching Algorithms]]
- [[Graph Algorithms]]
- [[Dynamic Programming]]
- [[Greedy Algorithms]]
- [[Divide & Conquer]]
- [[Computer Science/Compression|Compression]] — gzip, zstd, LZ4, Brotli, how they work

### Complexity

- [[Big O Notation]]
- [[Time Complexity]]
- [[Space Complexity]]
- [[Amortized Analysis]]

### Systems

- [[Memory Management]]
- [[Concurrency Patterns]]
- [[Distributed Systems]]
- [[System Design]]
- [[Systems Language Performance]] — C, C++, Rust, Go, WASM

### Execution

- [[WebAssembly]] — WASM, WASI, Component Model

### Networking

- [[Networking Fundamentals]]
- [[Sockets & Low-Level Networking]]
- [[DNS & Service Discovery]]
- [[Network Performance]]

### Patterns

- [[Design Patterns]]
- [[Architectural Patterns]]
- [[API Design]]

### Software Engineering

- [[Testing Strategies]] — Unit, integration, E2E, BDD, property-based

### Testing

- [[Computer Science/TDD and BDD|TDD and BDD]] — Test-driven and behavior-driven development
- [[Computer Science/Property-Based Testing|Property-Based Testing]] — QuickCheck, Hypothesis, generative testing
- [[Computer Science/Load Testing|Load Testing]] — Performance and stress testing

### Developer Practices

- [[Computer Science/Git Internals|Git Internals]] — Objects, refs, plumbing commands
- [[Computer Science/Code Review|Code Review]] — PR reviews, feedback, best practices
- [[Computer Science/Technical Writing|Technical Writing]] — Documentation, READMEs, API docs
- [[Computer Science/Regex Reference|Regex Reference]] — Regular expression patterns

### API Styles

- [[Computer Science/REST vs GraphQL vs gRPC|REST vs GraphQL vs gRPC]] — API architectural comparison

### OS Development Basics

- [[Computer Science/XDG Base Directory|XDG Base Directory]] — Linux config/data/cache paths
- [[Computer Science/Windows Development Basics|Windows Development Basics]] — AppData, Registry, paths
- [[Computer Science/macOS Development Basics|macOS Development Basics]] — Library folders, plist, Homebrew
- [[Computer Science/Cross-Platform Development Paths|Cross-Platform Development Paths]] — Where to store files on any OS

### Reference

- [[Technical Measurements]]

---

## All CS Pages

```dataview
TABLE status, join(tags, ", ") as "Tags"
FROM #cs
WHERE type != "moc"
SORT file.name ASC
```

## By Difficulty

### Fundamentals

```dataview
LIST
FROM #cs AND #fundamentals
```

### Intermediate

```dataview
LIST
FROM #cs AND #intermediate
```

### Advanced

```dataview
LIST
FROM #cs AND #advanced
```
