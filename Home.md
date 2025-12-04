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
| [[CS MOC]] | Data structures, algorithms, design patterns |
| [[Math MOC]] | Calculus, linear algebra, statistics |
| [[ML MOC]] | Machine learning, neural networks, LLMs |

---

## Languages

### By Category

| Category | Languages |
|----------|-----------|
| Systems | [[languages/C++|C++]], [[Rust]], [[Go]] |
| Enterprise/JVM | [[languages/Java|Java]], [[languages/Kotlin|Kotlin]], [[C Sharp|C#]] |
| Web/Scripting | [[TypeScript]], [[Python]], [[languages/PHP|PHP]], [[languages/Ruby|Ruby]] |
| Mobile/UI | [[languages/Swift|Swift]], [[languages/Dart|Dart]] |

---

## Domains

| Domain | What's There |
|--------|--------------|
| [[domains/Web Development|Web]] | Frontend frameworks, backend, full-stack |
| [[domains/Mobile Development|Mobile]] | iOS, Android, cross-platform (Flutter, RN) |
| [[domains/Desktop Development|Desktop]] | WPF, SwiftUI, Electron, Tauri, Qt |
| [[domains/Game Development|Game]] | Unity, Unreal, Godot, Bevy |
| [[domains/Embedded & Systems|Embedded]] | Bare metal, RTOS, systems programming |

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
| [[cs/Big O Notation]] | Complexity analysis |
| [[cs/Data Structures]] | Arrays, trees, graphs |
| [[cs/Sorting Algorithms]] | QuickSort, MergeSort |
| [[cs/Graph Algorithms]] | BFS, DFS, Dijkstra |
| [[cs/Design Patterns]] | GoF, modern patterns |

---

## Mathematics & ML

| Page | Topics |
|------|--------|
| [[math/Calculus]] | Derivatives, optimization |
| [[math/Linear Algebra]] | Vectors, matrices |
| [[math/Probability & Statistics]] | Distributions, inference |
| [[ml/ML Fundamentals]] | Supervised, unsupervised |
| [[ml/Neural Networks]] | Architecture, training |
| [[ml/LLMs & Transformers]] | Attention, GPT, Claude |

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
