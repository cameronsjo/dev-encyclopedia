---
title: Kotlin
aliases:
  - Kotlin Language
tags:
  - language
  - kotlin
  - jvm
  - android
type: reference
status: complete
created: '2025-11-28'
---

# Kotlin

Modern JVM language. Android's preferred language. Java interop.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | OOP + Functional |
| Typing | Static, strong, inferred |
| Runtime | JVM, Native, JS, WASM |
| Memory | Garbage collected (JVM) |
| First appeared | 2011 (JetBrains) |
| Current | 2.0+ |

---

## Key Features

### Null Safety

Null handled at type system level.

```kotlin
var name: String = "Alice"   // Non-null
var name: String? = null     // Nullable

// Safe call
name?.length

// Elvis operator
val len = name?.length ?: 0
```

### Data Classes

Like Java records, but more features.

```kotlin
data class User(val name: String, val age: Int)
// copy(), equals(), hashCode(), toString(), component functions
```

### Extension Functions

Add methods to existing types.

```kotlin
fun String.isPalindrome(): Boolean = this == this.reversed()
"radar".isPalindrome() // true
```

### Coroutines

Lightweight concurrency, not threads.

```kotlin
suspend fun fetchData(): Data {
    return withContext(Dispatchers.IO) {
        api.fetch()
    }
}

// Launch concurrently
coroutineScope {
    val a = async { fetchA() }
    val b = async { fetchB() }
    process(a.await(), b.await())
}
```

### Smart Casts

Automatic type narrowing after checks.

```kotlin
if (x is String) {
    println(x.length)  // x is String here
}
```

### Sealed Classes

Exhaustive when expressions.

```kotlin
sealed class Result<out T>
data class Success<T>(val data: T) : Result<T>()
data class Error(val message: String) : Result<Nothing>()

when (result) {
    is Success -> println(result.data)
    is Error -> println(result.message)
}
```

---

## Platforms

### Kotlin/JVM

- Full Java interop
- Spring Boot support
- Android development

### Kotlin/Native

- Compiles to native binaries
- No JVM required
- iOS via Kotlin Multiplatform

### Kotlin/JS

- Compiles to JavaScript
- React wrappers available

### Kotlin Multiplatform (KMP)

Share code across platforms:

- Common (shared logic)
- JVM (Android, backend)
- Native (iOS, desktop)
- JS (browser)

---

## Ecosystem

### Android

**Preferred language since 2019.**

| Component | Kotlin Support |
|-----------|----------------|
| Jetpack Compose | Kotlin-first |
| Room | Coroutines support |
| Retrofit | Coroutines adapters |
| ViewModel | Kotlin extensions |

### Backend

| Framework | Notes |
|-----------|-------|
| Spring Boot | Full support |
| Ktor | Kotlin-native framework |
| http4k | Functional style |
| Exposed | Kotlin SQL DSL |

### Build

| Tool | Status |
|------|--------|
| Gradle Kotlin DSL | Recommended |
| Maven | Supported |

---

## Kotlin vs Java

| Aspect | Kotlin | Java |
|--------|--------|------|
| Null safety | Built-in | Optional class |
| Conciseness | Less boilerplate | More verbose |
| Coroutines | Native | Virtual threads (21+) |
| Data classes | Full-featured | Records (limited) |
| Extension functions | Yes | No |
| Interop | Calls Java seamlessly | - |
| Compile time | Slower | Faster |
| Ecosystem maturity | Good | Excellent |

---

## When to Use Kotlin

**Strengths:**

- Concise, expressive
- Null safety by default
- Coroutines for async
- Multiplatform capability
- Java interop

**Considerations:**

- Slower compilation
- Smaller (but growing) community
- Learning curve for Java devs

**Best for:**

- Android development
- Spring Boot (modern projects)
- Multiplatform (shared logic)
- Greenfield JVM projects

---

## Related

- [[Java]]
- [[Mobile Development]]
- [[Web Frameworks]]
- [[Coroutines]]
