---
title: Java
aliases:
  - Java Ecosystem
tags:
  - language
  - java
  - jvm
  - enterprise
type: reference
status: complete
created: 2025-11-28
---

# Java

Enterprise workhorse, Android foundation, massive ecosystem.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | OOP, some functional (8+) |
| Typing | Static, strong |
| Runtime | JVM (JIT compiled) |
| Memory | Garbage collected |
| First appeared | 1995 |
| Current (LTS) | Java 21 |

---

## Modern Java (17+)

### Key Features

| Version | Feature |
|---------|---------|
| 8 | Lambdas, streams, Optional |
| 11 | Local var `var`, HTTP client |
| 14 | Records (preview) |
| 16 | Records (final) |
| 17 | Sealed classes, pattern matching |
| 21 | Virtual threads, pattern matching |

### Records

Immutable data classes with less boilerplate.

```java
public record User(String name, int age) {}
// Generates constructor, getters, equals, hashCode, toString
```

### Sealed Classes

Control inheritance hierarchy.

```java
public sealed interface Shape permits Circle, Rectangle {}
public final class Circle implements Shape {}
public final class Rectangle implements Shape {}
```

### Virtual Threads (Project Loom)

Lightweight threads for high concurrency.

```java
try (var executor = Executors.newVirtualThreadPerTaskExecutor()) {
    executor.submit(() -> handleRequest());
}
```

**Impact:** Millions of concurrent tasks without thread pool tuning.

### Pattern Matching

```java
if (obj instanceof String s) {
    System.out.println(s.length());
}

switch (shape) {
    case Circle c -> System.out.println(c.radius());
    case Rectangle r -> System.out.println(r.area());
}
```

---

## Ecosystem

### Build Tools

| Tool | Style |
|------|-------|
| Maven | XML, convention over config |
| Gradle | Groovy/Kotlin DSL, flexible |

**Maven:** Enterprise standard, verbose but predictable.

**Gradle:** Faster, more flexible, Android default.

### Frameworks

| Framework | Use Case |
|-----------|----------|
| Spring Boot | Web, microservices, enterprise |
| Quarkus | Cloud-native, fast startup |
| Micronaut | Cloud-native, low memory |
| Jakarta EE | Enterprise standards |

### Spring Boot

**The dominant framework.**

- Dependency injection
- Auto-configuration
- Embedded servers
- Huge ecosystem

### Testing

| Tool | Purpose |
|------|---------|
| JUnit 5 | Unit testing |
| Mockito | Mocking |
| AssertJ | Fluent assertions |
| Testcontainers | Integration testing |

### ORM/Data

| Tool | Type |
|------|------|
| Hibernate/JPA | Full ORM |
| jOOQ | Type-safe SQL |
| Spring Data | Repository pattern |

---

## Concurrency

### Traditional

- `Thread`, `Runnable`
- `ExecutorService`
- `synchronized`, locks

### Modern

- `CompletableFuture` (Java 8+)
- Virtual threads (Java 21+)
- Reactive (Project Reactor)

---

## JVM Languages

Java's runtime hosts other languages:

| Language | Notes |
|----------|-------|
| Kotlin | Modern, Android preferred |
| Scala | Functional, Spark |
| Groovy | Scripting, Gradle |
| Clojure | Lisp on JVM |

---

## When to Use Java

**Strengths:**
- Enterprise adoption
- Mature ecosystem
- Strong backwards compatibility
- Excellent tooling
- Large talent pool

**Considerations:**
- Verbose compared to Kotlin
- Startup time (improving with GraalVM)
- Memory footprint

**Best for:**
- Enterprise applications
- Large teams
- Long-lived projects
- Android (legacy)

---

## Related

- [[Kotlin]]
- [[Spring Boot]]
- [[Web Frameworks]]
- [[Testing Frameworks]]
