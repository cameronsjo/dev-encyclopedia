---
title: Swift
aliases:
  - Swift Language
tags:
  - language
  - swift
  - apple
  - ios
  - macos
type: reference
status: complete
created: '2025-11-28'
---

# Swift

Apple's modern language for iOS, macOS, and beyond.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Multi-paradigm (OOP, functional, protocol-oriented) |
| Typing | Static, strong, inferred |
| Runtime | Native (LLVM) |
| Memory | ARC (Automatic Reference Counting) |
| First appeared | 2014 (Apple) |
| Current | Swift 5.9+ |

---

## Key Features

### Optionals

Explicit null handling.

```swift
var name: String? = nil  // Optional
var name: String = "Bob" // Non-optional

// Safe unwrap
if let n = name {
    print(n)
}

// Guard
guard let n = name else { return }

// Nil coalescing
let n = name ?? "Unknown"
```

### Value Types

Structs are first-class, preferred over classes.

```swift
struct Point {
    var x: Double
    var y: Double
}
// Copied on assignment, not referenced
```

### Protocols

Like interfaces, but more powerful.

```swift
protocol Drawable {
    func draw()
}

extension Drawable {
    func draw() { /* default implementation */ }
}
```

### Protocol Extensions

Add functionality to protocols (and conforming types).

```swift
extension Collection where Element: Numeric {
    var sum: Element {
        reduce(0, +)
    }
}
```

### Enums with Associated Values

Powerful algebraic data types.

```swift
enum Result<T> {
    case success(T)
    case failure(Error)
}

switch result {
case .success(let value):
    print(value)
case .failure(let error):
    print(error)
}
```

### Concurrency (Swift 5.5+)

Modern async/await with actors.

```swift
func fetchData() async throws -> Data {
    let (data, _) = try await URLSession.shared.data(from: url)
    return data
}

// Actor for safe shared state
actor Counter {
    var count = 0
    func increment() { count += 1 }
}
```

---

## Ecosystem

### Apple Platforms

| Platform | UI Framework |
|----------|--------------|
| iOS | SwiftUI, UIKit |
| macOS | SwiftUI, AppKit |
| watchOS | SwiftUI, WatchKit |
| tvOS | SwiftUI, UIKit |
| visionOS | SwiftUI |

### SwiftUI

Declarative UI framework.

```swift
struct ContentView: View {
    @State private var count = 0

    var body: some View {
        VStack {
            Text("Count: \(count)")
            Button("Increment") { count += 1 }
        }
    }
}
```

### Server-Side Swift

| Framework | Notes |
|-----------|-------|
| Vapor | Most popular |
| Hummingbird | Lightweight |
| Kitura | IBM (deprecated) |

### Package Manager

**Swift Package Manager (SPM)** — built into Xcode.

```swift
// Package.swift
dependencies: [
    .package(url: "https://github.com/...", from: "1.0.0")
]
```

---

## Memory Management

### ARC (Automatic Reference Counting)

Not garbage collection. Reference counts at compile time.

**Retain cycles:**

```swift
class Parent {
    var child: Child?
}
class Child {
    weak var parent: Parent?  // Break cycle
}
```

| Keyword | Use |
|---------|-----|
| `strong` | Default, increments count |
| `weak` | Optional, doesn't increment |
| `unowned` | Non-optional, doesn't increment |

---

## Swift vs Objective-C

| Aspect | Swift | Objective-C |
|--------|-------|-------------|
| Syntax | Modern, clean | C with objects |
| Null safety | Optionals | Nullable annotations |
| Memory | ARC | ARC (or manual) |
| Interop | Can call Obj-C | Can call Swift |
| Performance | Similar | Similar |
| New APIs | Swift-first | Bridged |

**Reality:** Most new Apple development is Swift. Objective-C for legacy.

---

## Swift on Non-Apple Platforms

### Linux

Official support. Server-side Swift works.

### Windows

Experimental support. Growing.

### Android

Not supported. Use Kotlin Multiplatform for cross-platform.

---

## When to Use Swift

**Strengths:**

- Modern, safe language
- Best for Apple platforms
- Growing server-side
- Protocol-oriented design
- Great tooling (Xcode)

**Considerations:**

- Apple ecosystem lock-in
- Smaller server ecosystem
- ABI stability (recent)

**Best for:**

- iOS/macOS apps
- Apple ecosystem
- Server-side (if already Swift shop)

---

## Related

- [[Mobile Development]]
- [[Desktop Development]]
- [[SwiftUI]]
- [[Kotlin]]
