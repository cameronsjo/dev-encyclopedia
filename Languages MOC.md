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
| [[Languages/C++|C++]] | Multi-paradigm | Games, systems, performance |
| [[Rust]] | Ownership-based | Systems, CLI, safe code |
| [[Go]] | CSP, imperative | Cloud, CLI, services |

### Enterprise & JVM

| Language | Paradigm | Use Cases |
|----------|----------|-----------|
| [[Languages/Java|Java]] | OOP | Enterprise, Android (legacy) |
| [[Languages/Kotlin|Kotlin]] | OOP + Functional | Android, backend |
| [[C Sharp|C#]] | OOP + Functional | .NET, games, enterprise |

### Web & Scripting

| Language | Paradigm | Use Cases |
|----------|----------|-----------|
| [[TypeScript]] | Multi-paradigm | Frontend, Node.js |
| [[Python]] | Multi-paradigm | ML, scripting, web |
| [[Languages/PHP|PHP]] | Multi-paradigm | Web, WordPress |
| [[Languages/Ruby|Ruby]] | OOP | Rails, web |

### Mobile & UI

| Language | Paradigm | Use Cases |
|----------|----------|-----------|
| [[Languages/Swift|Swift]] | Multi-paradigm | Apple platforms |
| [[Languages/Dart|Dart]] | OOP | Flutter |

### Legacy & Mainframe

| Language | Paradigm | Use Cases |
|----------|----------|-----------|
| [[Languages/COBOL|COBOL]] | Procedural | Banking, insurance, government |
| [[Languages/Fortran|Fortran]] | Array-oriented | Scientific computing, HPC |
| [[Languages/JCL|JCL]] | Job control | Mainframe batch processing |
| [[Languages/BASIC|BASIC]] | Procedural | Education, VBA, legacy apps |
| [[Languages/Pascal|Pascal]] | Structured | Education, Delphi desktop |
| [[Languages/Ada|Ada]] | Contract-based | Aerospace, defense, safety-critical |

---

## By Domain

| Domain | Primary Languages |
|--------|-------------------|
| [[Domains/Web Development|Web]] | TypeScript, Python, Go, C#, PHP, Ruby |
| [[Domains/Mobile Development|Mobile]] | Swift, Kotlin, Dart, TypeScript, C# |
| [[Domains/Desktop Development|Desktop]] | C#, C++, Swift, Rust, TypeScript |
| [[Domains/Game Development|Game]] | C#, C++, Rust |
| [[Domains/Embedded & Systems|Systems]] | C++, Rust, C |
| Legacy & Mainframe | COBOL, Fortran, JCL, Ada |
| Scientific & HPC | Fortran, Python, C++ |

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
FROM #comparison AND (#csharp OR #go OR #python OR #typescript OR #rust OR #java OR #kotlin OR #swift OR #cpp OR #dart OR #php OR #ruby OR #legacy OR #mainframe OR #scientific)
SORT file.name ASC
```
