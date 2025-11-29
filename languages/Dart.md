---
title: Dart
aliases:
  - Dart Language
tags:
  - language
  - dart
  - flutter
  - mobile
type: reference
status: complete
created: 2025-11-28
---

# Dart

Google's language powering Flutter. Cross-platform UI development.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | OOP, some functional |
| Typing | Static, strong (sound null safety) |
| Runtime | JIT (dev), AOT (production) |
| Memory | Garbage collected |
| First appeared | 2011 (Google) |
| Current | Dart 3.x |

---

## Key Features

### Sound Null Safety

Null safety enforced at compile time.

```dart
String name = 'Alice';     // Non-nullable
String? name = null;       // Nullable

// Null-aware operators
name?.length;              // Safe access
name ?? 'Unknown';         // Default value
name!;                     // Assert non-null (dangerous)
```

### Classes

```dart
class User {
  final String name;
  final int age;

  User(this.name, this.age);  // Concise constructor

  // Named constructor
  User.guest() : name = 'Guest', age = 0;
}
```

### Extension Methods

Add methods to existing types.

```dart
extension StringExtensions on String {
  bool get isPalindrome => this == split('').reversed.join();
}

'radar'.isPalindrome;  // true
```

### Async/Await

```dart
Future<String> fetchData() async {
  final response = await http.get(url);
  return response.body;
}

// Streams
Stream<int> countStream() async* {
  for (var i = 0; i < 10; i++) {
    yield i;
  }
}
```

### Records (Dart 3)

Lightweight tuples with named fields.

```dart
(int, String) pair = (1, 'hello');

// Named fields
({int x, int y}) point = (x: 1, y: 2);
```

### Patterns (Dart 3)

Pattern matching and destructuring.

```dart
switch (obj) {
  case (int x, int y):
    print('Point: $x, $y');
  case [var first, ...var rest]:
    print('List with $first');
}
```

---

## Flutter

**The primary reason Dart exists today.**

### Why Dart for Flutter?

- JIT for hot reload
- AOT for production performance
- Designed for UI (single-threaded, GC)
- Google controls both

### Flutter Architecture

```
┌─────────────────────────┐
│      Your Dart Code     │
├─────────────────────────┤
│   Flutter Framework     │
│   (Widgets, Material)   │
├─────────────────────────┤
│    Flutter Engine       │
│    (Skia, Text)         │
├─────────────────────────┤
│   Platform (iOS/Android)│
└─────────────────────────┘
```

### State Management

| Approach | Complexity |
|----------|------------|
| setState | Simple, local |
| Provider | Moderate, popular |
| Riverpod | Modern Provider |
| Bloc/Cubit | Enterprise, events |
| GetX | Lightweight, opinionated |

---

## Ecosystem

### Packages

**pub.dev** — Central package repository.

| Package | Purpose |
|---------|---------|
| flutter | UI framework |
| http | HTTP client |
| dio | Advanced HTTP |
| provider | State management |
| freezed | Immutable classes |

### Testing

| Tool | Purpose |
|------|---------|
| test | Unit testing |
| flutter_test | Widget testing |
| integration_test | E2E testing |
| mockito | Mocking |

### Tooling

| Tool | Purpose |
|------|---------|
| dart | CLI tool |
| flutter | Flutter CLI |
| dartfmt | Formatter |
| dart analyze | Static analysis |

---

## Beyond Flutter

### Server-Side

Dart can run on server, but ecosystem is small.

| Framework | Notes |
|-----------|-------|
| Shelf | Low-level HTTP |
| Dart Frog | VGV's backend |
| Serverpod | Full-stack |

**Reality:** Most use Dart only for Flutter.

### CLI Tools

```dart
void main(List<String> args) {
  print('Hello, ${args.first}!');
}
```

Compile to native: `dart compile exe`

---

## Dart vs Other Languages

| Aspect | Dart | TypeScript | Kotlin |
|--------|------|------------|--------|
| Null safety | Sound | Optional | Built-in |
| Compilation | JIT + AOT | Transpile | JIT (JVM) |
| Primary use | Flutter | Web | Android/JVM |
| Ecosystem | Flutter-focused | Huge | Large |

---

## When to Use Dart

**Strengths:**
- Flutter development
- Hot reload
- Sound null safety
- Single language for multi-platform

**Considerations:**
- Mainly used with Flutter
- Smaller ecosystem outside Flutter
- Less general-purpose adoption

**Best for:**
- Flutter apps
- Cross-platform mobile/desktop
- Projects needing single codebase

---

## Related

- [[Mobile Development]]
- [[Desktop Development]]
- [[Flutter]]
- [[TypeScript]]
