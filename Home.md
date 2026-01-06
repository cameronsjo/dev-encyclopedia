---
title: Developer's Encyclopedia
tags:
  - moc
  - index
type: moc
created: '2025-11-28'
status: complete
---

# Developer's Encyclopedia

A comprehensive reference covering programming languages, frameworks, tools, and foundational concepts.

## Quick Navigation

| Category | Content |
|----------|---------|
| [[Languages MOC]] | 12 languages, ecosystems, comparisons |
| [[Domains MOC]] | Web, Mobile, Desktop, Game, Embedded |
| [[Tools MOC]] | Testing, logging, HTTP, databases, deployment |
| [[Computer Science MOC]] | Data structures, algorithms, design patterns |
| [[Math MOC]] | Calculus, linear algebra, statistics |
| [[Machine Learning MOC]] | Machine learning, neural networks, LLMs |

---

## Languages

### By Category

| Category | Languages |
|----------|-----------|
| Systems | [[Languages/C++]], [[Rust]], [[Go]] |
| Enterprise/JVM | [[Languages/Java]], [[Languages/Kotlin]], [[C Sharp]] |
| Web/Scripting | [[TypeScript]], [[Python]], [[PHP]], [[Languages/Ruby]] |
| Mobile/UI | [[Languages/Swift]], [[Languages/Dart]] |

---

## Domains

| Domain | What's There |
|--------|--------------|
| [[Domains/Web Development]] | Frontend frameworks, backend, full-stack |
| [[Domains/Mobile Development]] | iOS, Android, cross-platform (Flutter, RN) |
| [[Domains/Desktop Development]] | WPF, SwiftUI, Electron, Tauri, Qt |
| [[Domains/Game Development]] | Unity, Unreal, Godot, Bevy |
| [[Embedded & Systems]] | Bare metal, RTOS, systems programming |

---

## Tools & Development

| Page | Topics |
|------|--------|
| [[Terminal UI & Language Features]] | CLI/TUI libraries, async, types |
| [[Testing Frameworks]] | xUnit, pytest, Jest, go test |
| [[Logging Libraries]] | Structured logging, observability |
| [[OpenTelemetry]] | Tracing, metrics, distributed systems |
| [[Web Frameworks]] | ASP.NET, Gin, FastAPI, Express |
| [[ORMs & Database Access]] | EF Core, SQLAlchemy, Prisma |
| [[Build Systems]] | Compilers, bundlers, toolchains |
| [[Deployment]] | Containers, K8s, serverless |

---

## Computer Science

| Page | Topics |
|------|--------|
| [[Computer Science/Big O Notation]] | Complexity analysis |
| [[Computer Science/Data Structures]] | Arrays, trees, graphs |
| [[Computer Science/Sorting Algorithms]] | QuickSort, MergeSort |
| [[Computer Science/Graph Algorithms]] | BFS, DFS, Dijkstra |
| [[Computer Science/Design Patterns]] | GoF, modern patterns |

---

## Mathematics & ML

| Page | Topics |
|------|--------|
| [[Math/Calculus]] | Derivatives, optimization |
| [[Math/Linear Algebra]] | Vectors, matrices |
| [[Math/Probability & Statistics]] | Distributions, inference |
| [[Machine Learning/ML Fundamentals]] | Supervised, unsupervised |
| [[Machine Learning/Neural Networks]] | Architecture, training |
| [[Machine Learning/LLMs & Transformers]] | Attention, GPT, Claude |

---

## Stats

```dataview
TABLE length(rows) as "Count"
FROM ""
WHERE type != "template" AND type != "home"
GROUP BY type
SORT length(rows) DESC
```

---

## Recently Updated

```dataview
TABLE file.mtime as "Modified", type as "Type"
FROM ""
WHERE type != "home" AND type != "template"
SORT file.mtime DESC
LIMIT 10
```
