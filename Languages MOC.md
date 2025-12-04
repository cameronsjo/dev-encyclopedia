---
title: Languages MOC
tags:
  - moc
  - languages
type: moc
created: '2025-11-28'
status: complete
---

# Languages MOC

Programming languages and their ecosystems.

## All Languages

### Systems & Performance

| Language | Paradigm | Use Cases |
|----------|----------|-----------|
| [[languages/C++|C++]] | Multi-paradigm | Games, systems, performance |
| [[Rust]] | Ownership-based | Systems, CLI, safe code |
| [[Go]] | CSP, imperative | Cloud, CLI, services |

### Enterprise & JVM

| Language | Paradigm | Use Cases |
|----------|----------|-----------|
| [[languages/Java|Java]] | OOP | Enterprise, Android (legacy) |
| [[languages/Kotlin|Kotlin]] | OOP + Functional | Android, backend |
| [[C Sharp|C#]] | OOP + Functional | .NET, games, enterprise |

### Web & Scripting

| Language | Paradigm | Use Cases |
|----------|----------|-----------|
| [[TypeScript]] | Multi-paradigm | Frontend, Node.js |
| [[Python]] | Multi-paradigm | ML, scripting, web |
| [[languages/PHP|PHP]] | Multi-paradigm | Web, WordPress |
| [[languages/Ruby|Ruby]] | OOP | Rails, web |

### Mobile & UI

| Language | Paradigm | Use Cases |
|----------|----------|-----------|
| [[languages/Swift|Swift]] | Multi-paradigm | Apple platforms |
| [[languages/Dart|Dart]] | OOP | Flutter |

---

## By Domain

| Domain | Primary Languages |
|--------|-------------------|
| [[domains/Web Development|Web]] | TypeScript, Python, Go, C#, PHP, Ruby |
| [[domains/Mobile Development|Mobile]] | Swift, Kotlin, Dart, TypeScript, C# |
| [[domains/Desktop Development|Desktop]] | C#, C++, Swift, Rust, TypeScript |
| [[domains/Game Development|Game]] | C#, C++, Rust |
| [[domains/Embedded & Systems|Systems]] | C++, Rust, C |

---

## Comparison Topics

### Language Features

- [[Terminal UI & Language Features]] — TUI, async, types, errors
- [[Runtimes]] — JVM, CLR, native, interpreters
- [[Build Systems]] — Compilers, bundlers

### Development

- [[Testing Frameworks]]
- [[Logging Libraries]]
- [[HTTP Clients]]
- [[Web Frameworks]]
- [[ORMs & Database Access]]

---

## All Language Pages

```dataview
TABLE file.folder as "Category"
FROM #language
SORT file.name ASC
```

## Comparisons Mentioning Languages

```dataview
LIST
FROM #comparison AND (#csharp OR #go OR #python OR #typescript OR #rust OR #java OR #kotlin OR #swift OR #cpp OR #dart OR #php OR #ruby)
SORT file.name ASC
```
