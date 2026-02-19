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

### File Formats & Media

- [[File Formats]] — Binary structure, magic bytes, headers, and trailers
- [[Image Formats]] — JPEG, PNG, GIF, WebP, AVIF internals
- [[File Metadata]] — EXIF, GPS, XMP, ID3 metadata systems
- [[Audio and Video Formats]] — Codecs, containers, streaming
- [[Archive and Compression Formats]] — ZIP, tar, gzip, zstd, brotli
- [[Document Formats]] — PDF, DOCX, EPUB internals

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
