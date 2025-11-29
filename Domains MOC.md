---
title: Domains MOC
aliases:
  - Application Domains
  - Development Domains
tags:
  - moc
  - domains
type: moc
created: 2025-11-28
---

# Domains MOC

Application development domains and their ecosystems.

## Overview

| Domain | Description | Key Languages |
|--------|-------------|---------------|
| [[Web Development]] | Frontend, backend, full-stack | TypeScript, Python, Go, C#, PHP, Ruby |
| [[Mobile Development]] | iOS, Android, cross-platform | Swift, Kotlin, Dart, TypeScript, C# |
| [[Desktop Development]] | Native and cross-platform apps | C#, C++, Swift, Rust, TypeScript |
| [[Game Development]] | Games and real-time graphics | C#, C++, Rust, GDScript |
| [[Embedded & Systems]] | Low-level, hardware-adjacent | C, C++, Rust |
| [[CLI Development]] | Command-line tools | Go, Rust, Python, C# |

---

## Cross-Domain Technologies

| Technology | Domains |
|------------|---------|
| React/React Native | Web, Mobile |
| .NET MAUI | Mobile, Desktop |
| Flutter | Mobile, Desktop, Web |
| Electron | Desktop (Web tech) |
| Tauri | Desktop (Web + Rust) |
| Unity | Game, Mobile, Desktop |

---

## By Framework Type

### Frontend Web

```dataview
LIST
FROM #web AND #frontend
SORT file.name ASC
```

### Backend

```dataview
LIST
FROM #backend OR #api
SORT file.name ASC
```

### Mobile

```dataview
LIST
FROM #mobile
SORT file.name ASC
```

### Desktop

```dataview
LIST
FROM #desktop
SORT file.name ASC
```

### Game

```dataview
LIST
FROM #game
SORT file.name ASC
```
