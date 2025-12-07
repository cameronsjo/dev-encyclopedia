---
title: CS Fundamentals Glossary
aliases:
  - CS Glossary
  - Computer Science Terms
  - CS Terminology
tags:
  - cs
  - fundamentals
  - reference
  - glossary
type: reference
status: complete
created: "2025-12-07"
---

# CS Fundamentals Glossary

Quick-reference definitions for foundational computer science terminology.

## Computation & Automata

| Term | Definition |
|------|------------|
| **Deterministic** | Given the same input, always produces the same output. No randomness or ambiguity in execution path. |
| **Non-deterministic** | May have multiple possible execution paths or outputs for the same input. (Theory: can "guess" correct path; practice: randomized or concurrent) |
| **Turing Complete** | A system capable of simulating any Turing machine. Can compute anything that's computable (given enough time/memory). |
| **Turing Machine** | Theoretical model of computation: infinite tape + read/write head + state machine. Foundation of computability theory. |
| **Finite Automaton** | Simplest computational model: fixed states, no memory beyond current state. Recognizes regular languages. |
| **Pushdown Automaton** | Finite automaton + stack. Recognizes context-free languages (e.g., balanced parentheses). |
| **Halting Problem** | Undecidable problem: no algorithm can determine if an arbitrary program will halt or run forever. |
| **Decidable** | A problem for which an algorithm exists that always terminates with correct yes/no answer. |
| **Undecidable** | A problem for which no algorithm can exist that always terminates with correct answer. |
| **Semi-decidable** | Algorithm exists that terminates with "yes" if answer is yes, but may run forever if "no." |
| **Computable** | A function that can be calculated by some algorithm (Turing machine). |

## Complexity Theory

| Term | Definition |
|------|------------|
| **P** | Problems solvable in polynomial time. Considered "efficiently solvable." |
| **NP** | Problems whose solutions can be *verified* in polynomial time. (Non-deterministic Polynomial) |
| **NP-Complete** | Hardest problems in NP. If any NP-complete problem is in P, then P=NP. |
| **NP-Hard** | At least as hard as NP-complete, but not necessarily in NP (may not be decision problems). |
| **PSPACE** | Problems solvable with polynomial space (may take exponential time). |
| **EXPTIME** | Problems requiring exponential time. |
| **Tractable** | Solvable in polynomial time; practical to compute. |
| **Intractable** | Requires super-polynomial time; impractical for large inputs. |
| **Reduction** | Transforming one problem into another to prove relative difficulty. |

### Complexity Relationships

```
P ⊆ NP ⊆ PSPACE ⊆ EXPTIME

NP-Complete: hardest in NP
NP-Hard: ≥ NP-Complete (includes non-decision problems)

Open question: P = NP?
```

## Algorithm Analysis

| Term | Definition |
|------|------------|
| **Asymptotic** | Behavior as input size approaches infinity. Ignores constants and lower-order terms. |
| **Big O (O)** | Upper bound. f(n) = O(g(n)) means f grows no faster than g. |
| **Big Omega (Ω)** | Lower bound. f(n) = Ω(g(n)) means f grows at least as fast as g. |
| **Big Theta (Θ)** | Tight bound. f(n) = Θ(g(n)) means f grows at same rate as g. |
| **Amortized** | Average cost per operation over a sequence, even if individual ops vary. |
| **Worst-case** | Maximum cost for any input of size n. |
| **Average-case** | Expected cost over all possible inputs (requires probability distribution). |
| **Best-case** | Minimum cost; often not useful for analysis. |

### Common Complexities

```
O(1)        Constant      Hash lookup, array index
O(log n)    Logarithmic   Binary search
O(n)        Linear        Linear search, single loop
O(n log n)  Linearithmic  Efficient sorting (merge, heap)
O(n²)       Quadratic     Nested loops, naive sorting
O(2ⁿ)       Exponential   Brute-force subsets
O(n!)       Factorial     Brute-force permutations
```

## Mathematics Foundations

| Term | Definition |
|------|------------|
| **Discrete** | Countable, separate values (integers, graphs). Opposite of continuous. |
| **Continuous** | Uncountably infinite values (real numbers). Opposite of discrete. |
| **Combinatorial** | Counting and arranging discrete structures. Permutations, combinations. |
| **Recurrence Relation** | Defines sequence terms using previous terms. Used to analyze recursive algorithms. |
| **Invariant** | Property that remains true throughout algorithm execution. Used for correctness proofs. |
| **Induction** | Proof technique: prove base case, prove if true for n then true for n+1. |

## State & Behavior

| Term | Definition |
|------|------------|
| **Stateful** | Maintains state between operations. Behavior depends on history. |
| **Stateless** | No retained state. Same input always produces same output, regardless of history. |
| **Idempotent** | Applying operation multiple times has same effect as applying once. `f(f(x)) = f(x)` |
| **Pure Function** | No side effects, depends only on inputs. Same input → same output, always. |
| **Side Effect** | Observable change beyond return value: I/O, mutation, global state. |
| **Referential Transparency** | Expression can be replaced with its value without changing behavior. |
| **Memoization** | Caching function results to avoid redundant computation. |

### Examples

```
Idempotent:     DELETE /resource/123  (deleting twice = deleting once)
                Math.abs(-5)          (abs(abs(x)) = abs(x))

Not idempotent: counter++             (each call changes state)
                POST /orders          (creates new resource each time)

Pure:           function add(a, b) { return a + b; }
Impure:         function log(x) { console.log(x); return x; }
```

## Data Properties

| Term | Definition |
|------|------------|
| **Mutable** | Can be changed after creation. |
| **Immutable** | Cannot be changed after creation. Modifications create new copies. |
| **Persistent** | Data structure that preserves previous versions when modified. |
| **Ephemeral** | Opposite of persistent; modifications destroy previous state. |
| **Copy-on-Write** | Share data until modification, then copy. Lazy immutability optimization. |

## Concurrency & Systems

| Term | Definition |
|------|------------|
| **Synchronous** | Operations execute sequentially; caller waits for completion. |
| **Asynchronous** | Operations can execute without blocking; caller continues immediately. |
| **Blocking** | Operation holds thread/process until complete. |
| **Non-blocking** | Operation returns immediately; completion signaled later. |
| **Concurrent** | Multiple computations in overlapping time periods (not necessarily simultaneous). |
| **Parallel** | Multiple computations at the exact same time (requires multiple cores/machines). |
| **Race Condition** | Behavior depends on timing/ordering of events. Bug when unintended. |
| **Deadlock** | Circular wait: A waits for B, B waits for A. Neither can proceed. |
| **Livelock** | Processes continuously change state in response to each other but make no progress. |
| **Starvation** | Process never gets resources it needs due to scheduling/priority. |
| **Atomic** | Operation completes entirely or not at all; no observable intermediate state. |
| **Thread-safe** | Correct behavior when accessed from multiple threads simultaneously. |

### Concurrency vs Parallelism

```
Concurrent (single core):   Parallel (multi-core):

Task A ──┐    ┌── Task A    Task A ─────────────
         │    │             Task B ─────────────
Task B ──┴────┴── Task B
         ↑ time-slicing     ↑ simultaneous
```

## Type Theory

| Term | Definition |
|------|------------|
| **Static Typing** | Types checked at compile time. Errors caught before runtime. |
| **Dynamic Typing** | Types checked at runtime. More flexible, errors caught during execution. |
| **Strong Typing** | Strict type rules; implicit conversions restricted. |
| **Weak Typing** | Lenient type rules; implicit conversions allowed. |
| **Type Inference** | Compiler deduces types without explicit annotations. |
| **Nominal Typing** | Types distinguished by declared name. (Java, C#) |
| **Structural Typing** | Types distinguished by structure/shape. (TypeScript, Go interfaces) |
| **Duck Typing** | "If it walks like a duck..." Type based on available methods/properties at runtime. |
| **Covariance** | Subtype relationship preserved: `List<Dog>` is subtype of `List<Animal>`. |
| **Contravariance** | Subtype relationship reversed. Function expecting `Animal` accepts `Dog` handler. |
| **Invariance** | No subtype relationship. `List<Dog>` is NOT related to `List<Animal>`. |

## Memory & Storage

| Term | Definition |
|------|------------|
| **Stack** | LIFO memory for function calls, local variables. Fast, automatic management. |
| **Heap** | Dynamic memory allocation. Manual or garbage-collected. |
| **Garbage Collection** | Automatic memory reclamation of unreachable objects. |
| **Reference Counting** | Track number of references to object; free when count reaches zero. |
| **Memory Leak** | Allocated memory never freed; accumulates over time. |
| **Dangling Pointer** | Pointer to freed memory. Undefined behavior if accessed. |
| **Buffer Overflow** | Writing beyond allocated memory bounds. Security vulnerability. |
| **Cache** | Fast storage for frequently accessed data. Trading space for time. |
| **Locality** | Tendency to access nearby memory (spatial) or recently used data (temporal). |

## Distributed Systems

| Term | Definition |
|------|------------|
| **CAP Theorem** | Distributed system can have at most 2 of: Consistency, Availability, Partition tolerance. |
| **Consistency** | All nodes see same data at same time. |
| **Availability** | Every request receives response (success or failure). |
| **Partition Tolerance** | System continues despite network failures between nodes. |
| **Eventual Consistency** | Given no new updates, all nodes will eventually converge to same value. |
| **Strong Consistency** | Read always returns most recent write. |
| **Consensus** | Agreement among distributed nodes on a single value. (Paxos, Raft) |
| **Byzantine Fault** | Node behaves arbitrarily/maliciously, not just crash. |
| **Quorum** | Minimum nodes that must agree for operation to proceed. |

## Encoding & Representation

| Term | Definition |
|------|------------|
| **Serialization** | Converting object to byte stream for storage/transmission. |
| **Deserialization** | Reconstructing object from byte stream. |
| **Marshalling** | Transforming data for transmission (includes serialization + metadata). |
| **Endianness** | Byte order: big-endian (MSB first) vs little-endian (LSB first). |
| **Two's Complement** | Standard signed integer representation. Negation: invert bits + 1. |
| **IEEE 754** | Floating-point standard. Sign bit + exponent + mantissa. |
| **Unicode** | Character encoding standard. UTF-8, UTF-16, UTF-32 are encodings. |

## Logic & Proofs

| Term | Definition |
|------|------------|
| **Soundness** | If provable, then true. No false positives. |
| **Completeness** | If true, then provable. No false negatives. |
| **Satisfiable** | At least one assignment makes formula true. |
| **Valid** | True under all possible assignments (tautology). |
| **Contradiction** | False under all possible assignments. |
| **NP-Complete (SAT)** | Boolean satisfiability: first proven NP-complete problem. |

## Related

- [[Big O Notation]]
- [[Data Structures]]
- [[Concurrency Patterns]]
- [[Distributed Systems]]
- [[Memory Management]]
- [[Type Systems]]

## References

- Sipser, M. "Introduction to the Theory of Computation"
- Cormen et al. "Introduction to Algorithms" (CLRS)
- Tanenbaum, A. "Distributed Systems"
