---
title: Haskell
aliases:
  - GHC
  - Haskell Language
tags:
  - language
  - functional
  - pure
  - statically-typed
type: reference
status: complete
created: 2025-11-30
---

# Haskell

A purely functional, statically-typed language with lazy evaluation, strong type inference, and advanced type system features.

## Overview

| Aspect | Details |
|--------|---------|
| **Paradigm** | Pure functional |
| **Typing** | Static, strong, Hindley-Milner type inference |
| **Evaluation** | Lazy (non-strict) by default |
| **Compiler** | GHC (Glasgow Haskell Compiler) |
| **First Release** | 1990 |
| **Package Manager** | Cabal, Stack |
| **Primary Use Cases** | Compilers, formal verification, fintech, research |
| **Learning Curve** | Steep (requires paradigm shift) |

## Core Concepts

### Pure Functions

All functions are pure by default - same inputs always produce same outputs, no side effects.

```haskell
-- Pure function
add :: Int -> Int -> Int
add x y = x + y

-- Side effects are explicitly tracked in types
readFile :: FilePath -> IO String
```

**Implications:**

- Referential transparency: expressions can be replaced with their values
- Easier reasoning about code behavior
- Natural parallelization opportunities
- Side effects isolated to IO types

### Lazy Evaluation

Expressions are not evaluated until their values are needed.

```haskell
-- Infinite list (only computes what's needed)
naturals = [1..]
take 5 naturals  -- [1,2,3,4,5]

-- Conditional evaluation
expensiveComputation = if condition
  then cheapValue
  else veryExpensiveCalculation  -- Only runs if condition is False
```

**Benefits:** Infinite data structures, efficient short-circuiting, composable programs
**Drawbacks:** Space leaks, harder performance reasoning

### Type Classes

Interfaces defining behavior for types, enabling ad-hoc polymorphism.

```haskell
-- Type class definition
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

-- Instance for custom type
data Color = Red | Green | Blue

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False
```

**Common Type Classes:**

- `Eq` - Equality comparison
- `Ord` - Ordering
- `Show` - String conversion
- `Functor` - Mappable containers
- `Applicative` - Independent effects
- `Monad` - Sequenced computations

### Algebraic Data Types

Types built from sums (variants) and products (records).

```haskell
-- Sum type (like enums/unions)
data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double

-- Product type (like structs)
data Person = Person
  { name :: String
  , age :: Int
  , email :: String
  }

-- Recursive type
data Tree a = Leaf a
            | Node (Tree a) (Tree a)
```

### Pattern Matching

Deconstructing data structures in function definitions.

```haskell
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) = sqrt (s * (s - a) * (s - b) * (s - c))
  where s = (a + b + c) / 2

-- Guards for conditional logic
classify :: Int -> String
classify n
  | n < 0 = "negative"
  | n == 0 = "zero"
  | otherwise = "positive"
```

## Monads

Monads sequence computations while managing context (state, failure, effects, etc.).

### Common Monads

| Monad | Purpose | Example Use |
|-------|---------|-------------|
| **Maybe** | Optional values, failure without error messages | Safe division, lookup operations |
| **Either** | Failure with error information | Validation, error handling |
| **IO** | Side effects (I/O, randomness, mutation) | File operations, network, user input |
| **State** | Stateful computations | Random generation, counters |
| **List** | Non-deterministic computation | Generating combinations |
| **Reader** | Shared read-only environment | Configuration passing |
| **Writer** | Logging/accumulation | Audit trails, debug output |

### Do-Notation

Syntactic sugar for monadic sequencing.

```haskell
-- Without do-notation
getFullName :: IO String
getFullName =
  putStrLn "First name:" >>
  getLine >>= \first ->
  putStrLn "Last name:" >>
  getLine >>= \last ->
  return (first ++ " " ++ last)

-- With do-notation
getFullName :: IO String
getFullName = do
  putStrLn "First name:"
  first <- getLine
  putStrLn "Last name:"
  last <- getLine
  return (first ++ " " ++ last)
```

### Example: Maybe Monad

```haskell
-- Chain operations that might fail
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

calculate :: Double -> Double -> Double -> Maybe Double
calculate a b c = do
  x <- safeDivide a b
  y <- safeDivide x c
  return (y * 2)
```

## Build Tools & Package Management

### Cabal

Traditional Haskell build system and package manager.

```cabal
-- myproject.cabal
name:                myproject
version:             0.1.0.0
build-type:          Simple
cabal-version:       >=1.10

library
  exposed-modules:     MyLib
  build-depends:       base >=4.7 && <5
                     , text
                     , aeson
  hs-source-dirs:      src
  default-language:    Haskell2010
```

**Commands:**

- `cabal build` - Compile project
- `cabal repl` - Interactive REPL
- `cabal test` - Run tests
- `cabal install <package>` - Install package

### Stack

Alternative build tool with reproducible builds and curated package sets.

```yaml
# stack.yaml
resolver: lts-21.17  # Curated package set
packages:
  - .
extra-deps: []
```

**Commands:**

- `stack build` - Build project
- `stack test` - Run tests
- `stack ghci` - Load REPL
- `stack exec <app>` - Run executable

**Stack vs Cabal:**

- Stack: Reproducible builds, curated snapshots, simpler for beginners
- Cabal: More flexible, direct control, better for library authors

## Popular Libraries & Ecosystems

| Domain | Libraries |
|--------|-----------|
| **Web** | Servant, Yesod, Scotty, Spock |
| **Parsing** | Parsec, Megaparsec, Attoparsec |
| **JSON** | Aeson |
| **Concurrency** | async, stm (Software Transactional Memory) |
| **Testing** | QuickCheck, HUnit, Hspec, Tasty |
| **Data Structures** | containers, vector, unordered-containers |
| **Text** | text, bytestring |
| **HTTP** | http-client, wreq, req |
| **Database** | persistent, esqueleto, postgresql-simple |
| **Lenses** | lens, microlens, optics |

## GHC (Glasgow Haskell Compiler)

The de facto standard Haskell compiler.

**Key Features:**

- Advanced optimizations (strictness analysis, inlining, fusion)
- Language extensions via pragmas
- Profiling tools
- Foreign Function Interface (FFI)
- Interactive REPL (GHCi)

**Common Language Extensions:**

```haskell
{-# LANGUAGE OverloadedStrings #-}  -- String literals as Text
{-# LANGUAGE DeriveGeneric #-}      -- Auto-derive Generic instances
{-# LANGUAGE LambdaCase #-}         -- \case syntax
{-# LANGUAGE RecordWildCards #-}    -- Pattern match records
```

## Comparison with Other Functional Languages

| Feature | Haskell | Scala | Elixir | OCaml | F# |
|---------|---------|-------|--------|-------|-----|
| **Purity** | Pure (IO monad for effects) | Impure (allows side effects) | Impure | Impure | Impure |
| **Evaluation** | Lazy | Strict (lazy available) | Strict | Strict (lazy available) | Strict |
| **Type System** | Hindley-Milner, type classes | Scala 3 types, implicits | Dynamic with specs | ML-style, modules | ML-style, .NET types |
| **Concurrency Model** | STM, async, lightweight threads | Akka, futures, Java threads | Actor model (OTP) | Async, Lwt, Eio | Async workflows, TPL |
| **Platform** | Native (GHC) | JVM, Native (Scala Native) | BEAM VM | Native, JavaScript | .NET CLR |
| **Learning Curve** | Steep | Moderate-Steep | Moderate | Moderate | Moderate |
| **Industry Adoption** | Niche (fintech, compilers) | High (backend, data) | Growing (web, telecom) | Niche (formal verification) | Growing (.NET shops) |
| **Main Paradigm** | Pure functional | Multi-paradigm (OOP + FP) | Functional + Erlang | Functional + imperative | Functional-first |

## When to Use Haskell

### Strengths

| Aspect | Benefit |
|--------|---------|
| **Correctness** | Type system catches many bugs at compile time |
| **Refactoring** | Pure functions make large-scale changes safer |
| **Concurrency** | STM provides composable concurrent programming |
| **Mathematical Code** | Natural expression of algorithms and proofs |
| **DSLs** | Excellent for domain-specific languages |
| **Long-term Maintenance** | Types serve as machine-checked documentation |

### Considerations

| Aspect | Challenge |
|--------|-----------|
| **Learning Curve** | Requires understanding monads, type system, lazy evaluation |
| **Performance** | Lazy evaluation can cause space leaks; requires profiling |
| **Ecosystem** | Smaller library ecosystem than mainstream languages |
| **Hiring** | Fewer Haskell developers available |
| **Tooling** | IDE support improving but still behind mainstream languages |
| **Debugging** | Lazy evaluation makes stack traces less intuitive |

### Best For

| Use Case | Why |
|----------|-----|
| ✅ Compilers & interpreters | Strong parsing libraries, ADTs model ASTs well |
| ✅ Financial systems | Correctness-critical, complex business logic |
| ✅ Formal verification | Mathematical foundations, theorem proving |
| ✅ Research & prototyping | Express ideas concisely, type system helps exploration |
| ✅ Backend services | Strong concurrency, type safety |
| ❌ Performance-critical systems | GC overhead, lazy evaluation complexity |
| ❌ Rapid prototyping (for beginners) | Steep learning curve slows initial development |
| ❌ Mobile/embedded | Limited platform support, runtime overhead |
| ❌ Teams new to FP | Requires significant training investment |

## Related

- [[Scala]] - JVM functional language with OOP, more industry adoption
- [[Elixir]] - Functional language on BEAM VM, actor model concurrency
- [[OCaml]] - ML-family language with practical focus
- [[F Sharp]] - Functional-first language on .NET platform
- [[Functional Programming]] - Core paradigm and concepts
- [[Type Systems]] - Advanced type theory concepts
