---
title: C++
aliases:
  - CPP
  - C Plus Plus
tags:
  - language
  - cpp
  - systems
  - performance
type: reference
status: complete
created: 2025-11-28
---

# C++

High-performance systems language. Games, browsers, databases.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Multi-paradigm (OOP, generic, functional) |
| Typing | Static, strong |
| Runtime | Native |
| Memory | Manual (RAII encouraged) |
| First appeared | 1985 |
| Current | C++23 |

---

## Modern C++ (11-23)

### Evolution

| Version | Key Features |
|---------|--------------|
| C++11 | auto, lambdas, smart pointers, move semantics |
| C++14 | Generic lambdas, relaxed constexpr |
| C++17 | std::optional, std::variant, if constexpr |
| C++20 | Concepts, ranges, coroutines, modules |
| C++23 | std::expected, more constexpr |

### Smart Pointers

RAII for memory management.

```cpp
// Unique ownership
std::unique_ptr<Widget> w = std::make_unique<Widget>();

// Shared ownership
std::shared_ptr<Widget> s = std::make_shared<Widget>();

// Non-owning reference
std::weak_ptr<Widget> weak = s;
```

### Move Semantics

Transfer resources without copying.

```cpp
std::vector<int> v1 = {1, 2, 3};
std::vector<int> v2 = std::move(v1);  // v1 is now empty
```

### Lambda Expressions

```cpp
auto add = [](int a, int b) { return a + b; };
auto captureAll = [=]() { /* capture by value */ };
auto captureRef = [&]() { /* capture by reference */ };
```

### Concepts (C++20)

Constraints on template parameters.

```cpp
template<typename T>
concept Numeric = std::integral<T> || std::floating_point<T>;

template<Numeric T>
T add(T a, T b) { return a + b; }
```

### Ranges (C++20)

Composable, lazy operations.

```cpp
auto result = numbers
    | std::views::filter([](int n) { return n > 0; })
    | std::views::transform([](int n) { return n * 2; });
```

---

## Ecosystem

### Build Systems

| Tool | Notes |
|------|-------|
| CMake | De facto standard |
| Meson | Modern, fast |
| Make | Legacy, manual |
| Bazel | Large scale, hermetic |

### Package Managers

| Tool | Notes |
|------|-------|
| vcpkg | Microsoft, large registry |
| Conan | Decentralized |
| CPM | CMake-based |

**State of packaging:** Fragmented compared to other languages.

### Compilers

| Compiler | Platform |
|----------|----------|
| GCC | Linux, cross-platform |
| Clang | macOS, cross-platform |
| MSVC | Windows |

### Testing

| Framework | Style |
|-----------|-------|
| Google Test | xUnit style |
| Catch2 | Modern, header-only |
| doctest | Lightweight |

---

## Use Cases

### Games

- Unreal Engine (C++)
- Custom engines
- Performance-critical gameplay

### Systems

- Operating systems
- Databases (MySQL, MongoDB)
- Browsers (Chrome, Firefox)

### Embedded

- Automotive
- Aerospace
- Real-time systems

### Finance

- High-frequency trading
- Risk calculations
- Low-latency systems

---

## Memory Safety

### The Problem

C++ has all of C's memory issues:
- Buffer overflows
- Use after free
- Dangling pointers
- Memory leaks

### Mitigations

| Approach | How |
|----------|-----|
| Smart pointers | RAII, automatic cleanup |
| Sanitizers | ASan, MSan, UBSan |
| Static analysis | Clang-Tidy, PVS-Studio |
| Coding standards | C++ Core Guidelines |

### The Future

**Safe C++ proposals** being discussed. Rust influence.

---

## C++ vs Rust

| Aspect | C++ | Rust |
|--------|-----|------|
| Memory safety | Manual (RAII helps) | Compile-time enforced |
| Learning curve | Steep | Steeper initially |
| Ecosystem | Massive, legacy | Growing |
| Build system | Fragmented | Cargo (unified) |
| Compile time | Varies | Slower |
| Job market | Huge | Growing |

---

## When to Use C++

**Strengths:**
- Maximum performance
- Huge existing codebase
- Game engines
- Low-level control
- Mature tooling

**Considerations:**
- Memory safety burden
- Complexity
- Build system pain
- Long compile times

**Best for:**
- Game development
- Performance-critical systems
- Existing C++ codebases
- Embedded (complex)

---

## Related

- [[Rust]]
- [[Game Development]]
- [[Embedded & Systems]]
- [[Build Systems]]
