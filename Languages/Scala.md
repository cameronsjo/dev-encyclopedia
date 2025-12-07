# Scala

A multi-paradigm language combining object-oriented and functional programming on the JVM with a powerful type system and expressive syntax.

## Overview

| Aspect | Details |
|--------|---------|
| **Paradigm** | Object-oriented, Functional, Imperative |
| **Typing** | Static, Strong, Inferred |
| **Platform** | JVM (primary), JavaScript (Scala.js), Native (Scala Native) |
| **First Appeared** | 2004 (Scala 2), 2021 (Scala 3) |
| **Designed By** | Martin Odersky |
| **Influenced By** | Java, Haskell, ML, Erlang |
| **Latest Version** | Scala 3.x (dotty) |
| **Key Features** | Type inference, Pattern matching, Traits, Implicits/Givens, For-comprehensions |
| **Build Tools** | sbt, Mill, Maven, Gradle |
| **Ecosystem** | Akka, Spark, Play, Cats, ZIO, http4s |

## Scala 2 vs Scala 3

| Feature | Scala 2 | Scala 3 |
|---------|---------|---------|
| **Compiler** | scalac | dotty |
| **Syntax** | Braces required | Significant whitespace optional |
| **Implicits** | `implicit` keyword | Redesigned as `given`/`using` |
| **Union Types** | ❌ | ✅ `String | Int` |
| **Intersection Types** | Limited (`with`) | ✅ Full support `A & B` |
| **Enums** | Sealed traits + case objects | ✅ Native `enum` keyword |
| **Extension Methods** | Implicit classes | ✅ Direct `extension` syntax |
| **Type Lambdas** | Complex syntax | ✅ Simplified `[X] =>> F[X]` |
| **Opaque Types** | ❌ | ✅ Zero-cost type aliases |
| **Metaprogramming** | Macros (complex) | ✅ Inline, quotes, splices |
| **Migration** | Legacy codebase | Interoperable with Scala 2 |

## Core Concepts

### Type System

**Variance:**

- Covariance (`+T`): `List[Dog]` is subtype of `List[Animal]`
- Contravariance (`-T`): `Function1[-T, +R]`
- Invariance (default): `Array[T]`

**Advanced Types:**

- Path-dependent types
- Higher-kinded types (`F[_]`)
- Existential types (Scala 2) / Wildcard types (Scala 3)
- Structural types (duck typing)
- Dependent function types (Scala 3)

### Traits

Mixins combining interface and implementation, support multiple inheritance.

**Key features:**

- Abstract and concrete members
- Self-type annotations
- Linearization (deterministic MRO)
- Can have parameters (Scala 3)

### Case Classes and Pattern Matching

**Case classes:** Immutable data containers with automatic `equals`, `hashCode`, `toString`, `copy`, and pattern matching support.

**Pattern matching:**

- Destructuring
- Guards (`if` conditions)
- Type patterns
- Variable binding (`@`)
- Exhaustiveness checking
- Sealed hierarchies

### Implicits (Scala 2) vs Givens (Scala 3)

| Concept | Scala 2 | Scala 3 |
|---------|---------|---------|
| **Implicit Parameters** | `def f(implicit x: T)` | `def f(using x: T)` |
| **Implicit Values** | `implicit val x: T = ...` | `given T = ...` |
| **Implicit Conversions** | `implicit def toX(y: Y): X` | `given Conversion[Y, X] = ...` |
| **Extension Methods** | `implicit class RichInt(i: Int)` | `extension (i: Int) def ...` |
| **Type Classes** | `implicit val ord: Ordering[T]` | `given Ordering[T] = ...` |
| **Context Bounds** | `def f[T: Ordering]` | Same syntax |
| **Summoning** | `implicitly[T]` | `summon[T]` |

**Scala 3 improvements:**

- Intent-driven syntax (explicit purpose)
- Reduced ambiguity
- Better error messages
- Clearer import semantics

### For-Comprehensions

Syntactic sugar for `map`, `flatMap`, `withFilter` chains.

**Desugars to:**

- `for (x <- xs) yield f(x)` → `xs.map(f)`
- `for (x <- xs; y <- ys) yield (x, y)` → `xs.flatMap(x => ys.map(y => (x, y)))`
- `for (x <- xs if pred) yield x` → `xs.withFilter(pred).map(identity)`

**Works with:** Options, Lists, Futures, Try, Either, custom monadic types.

## Key Ecosystem Libraries

| Library | Purpose | Paradigm |
|---------|---------|----------|
| **Akka** | Actor model concurrency, distributed systems | Reactive |
| **Spark** | Big data processing, distributed computing | Batch/Stream |
| **Cats** | Functional programming abstractions (Monads, Functors) | Pure FP |
| **ZIO** | Effect system, resource management, concurrency | Effect-based FP |
| **http4s** | Functional HTTP server/client | Pure FP |
| **Play** | Web framework (MVC) | Pragmatic |
| **Slick** | Functional relational mapping | FP |
| **Circe** | JSON parsing/encoding | FP |
| **ScalaTest** | Testing framework | Multi-style |
| **Scala.js** | Compile to JavaScript | Cross-platform |

## Build Tools

| Tool | Description | Strengths |
|------|-------------|-----------|
| **sbt** | Scala Build Tool (standard) | Incremental compilation, REPL integration, plugin ecosystem |
| **Mill** | Modern build tool | Fast, simple, scriptable |
| **Maven** | JVM standard | Java interop, enterprise |
| **Gradle** | JVM multi-language | Kotlin DSL, flexibility |

## Comparison with Other JVM Languages

| Feature | Scala | Java | Kotlin |
|---------|-------|------|--------|
| **Type Inference** | ✅ Extensive | ❌ Limited (var) | ✅ Good |
| **Functional Programming** | ✅ First-class | ⚠️ Limited (Streams) | ✅ Good |
| **Immutability** | ✅ Default collections | ❌ Explicit | ✅ `val` default |
| **Pattern Matching** | ✅ Powerful | ⚠️ Switch expressions | ✅ `when` |
| **Extension Methods** | ✅ Implicits/extensions | ❌ | ✅ Extension functions |
| **Higher-Kinded Types** | ✅ | ❌ | ❌ |
| **Null Safety** | ⚠️ Option type | ❌ | ✅ Built-in |
| **Learning Curve** | ⚠️ Steep | ✅ Gentle | ✅ Gentle |
| **Compilation Speed** | ❌ Slow | ✅ Fast | ⚠️ Moderate |
| **Tooling** | ⚠️ Improving | ✅ Excellent | ✅ Excellent |
| **Enterprise Adoption** | ⚠️ Niche | ✅ Dominant | ✅ Growing |
| **Concurrency** | ✅ Akka, ZIO | ⚠️ Threads, CompletableFuture | ✅ Coroutines |

## When to Use

### Strengths

- **Functional programming:** First-class FP with powerful abstractions (monads, functors, type classes)
- **Type safety:** Sophisticated type system catches errors at compile time
- **Expressiveness:** Concise syntax reduces boilerplate significantly
- **Big data:** De facto standard for Spark development
- **Concurrency:** Akka for actor-based systems, ZIO for effect management
- **DSL creation:** Flexible syntax enables internal DSLs
- **JVM interop:** Seamless Java library integration

### Considerations

- **Learning curve:** Steep for developers new to FP or advanced type systems
- **Compilation speed:** Slower than Java/Kotlin, especially cold builds
- **Tooling:** IDE support improving but still behind Java/Kotlin
- **Team adoption:** Requires team buy-in for FP paradigm
- **Multiple paradigms:** Easy to mix styles inconsistently
- **Binary compatibility:** Scala 2/3 transition, library version conflicts
- **Community split:** Scala 2 vs Scala 3, pragmatic vs pure FP camps

### Best For

| Use Case | Why |
|----------|-----|
| **Data engineering** | Spark ecosystem, type-safe data pipelines |
| **Distributed systems** | Akka actors, event sourcing, CQRS |
| **Functional backends** | Pure FP with Cats/ZIO, http4s |
| **DSL-heavy applications** | Financial modeling, rules engines |
| **Teams valuing type safety** | Complex domain modeling, compile-time guarantees |
| **Existing Scala codebases** | Leveraging ecosystem, maintaining legacy |

### Avoid For

- Greenfield projects with mixed-experience teams (consider Kotlin)
- Time-sensitive prototypes (compilation overhead)
- Projects requiring fastest build times
- Teams without FP experience (without training investment)
- Heavy reliance on cutting-edge IDE features

## Migration Paths

**Java → Scala:**

- Start with Scala 2 syntax (familiar)
- Gradually adopt FP patterns
- Leverage existing Java libraries
- Consider Scala 3 for new modules

**Scala 2 → Scala 3:**

- Use compatibility mode
- Migrate implicits to givens incrementally
- Adopt new syntax gradually
- Rewrite macros last

**Scala → Kotlin:**

- Similar syntax for basics
- Different concurrency models (Akka → Coroutines)
- Loss of higher-kinded types
- Gain simpler tooling, faster compilation

## Related

- [[Java]] - JVM foundation, interoperability
- [[Kotlin]] - Alternative modern JVM language
- [[Languages MOC]] - All language references
