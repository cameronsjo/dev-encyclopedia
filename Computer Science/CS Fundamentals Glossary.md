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

## Graph Theory

| Term | Definition |
|------|------------|
| **Graph** | Set of vertices (nodes) connected by edges. G = (V, E). |
| **Directed Graph (Digraph)** | Edges have direction: A → B ≠ B → A. |
| **Undirected Graph** | Edges have no direction: A — B means both directions. |
| **Weighted Graph** | Edges have associated values (costs, distances). |
| **DAG** | Directed Acyclic Graph. No cycles; enables topological ordering. |
| **Cycle** | Path that starts and ends at same vertex. |
| **Connected** | Path exists between every pair of vertices (undirected). |
| **Strongly Connected** | Path exists in both directions between every pair (directed). |
| **Tree** | Connected acyclic graph. n vertices, n-1 edges. |
| **Spanning Tree** | Tree that includes all vertices of a graph. |
| **Bipartite** | Vertices can be split into two sets with edges only between sets. |
| **Topological Sort** | Linear ordering where for every edge u→v, u comes before v. Only possible for DAGs. |
| **Adjacency Matrix** | 2D array where matrix[i][j] = 1 if edge exists. O(V²) space. |
| **Adjacency List** | Array of lists; each vertex stores its neighbors. O(V+E) space. |
| **In-degree** | Number of edges coming into a vertex. |
| **Out-degree** | Number of edges leaving a vertex. |
| **Path** | Sequence of vertices connected by edges. |
| **Shortest Path** | Path with minimum total weight (Dijkstra, Bellman-Ford). |
| **MST** | Minimum Spanning Tree. Spans all vertices with minimum total edge weight. |

## Parsing & Formal Languages

| Term | Definition |
|------|------------|
| **Grammar** | Rules defining valid strings in a language. Production rules. |
| **BNF** | Backus-Naur Form. Notation for context-free grammars. |
| **Terminal** | Literal symbol in grammar (actual characters/tokens). |
| **Non-terminal** | Symbol that can be expanded by grammar rules. |
| **Parse Tree** | Tree showing how input derives from grammar rules. |
| **AST** | Abstract Syntax Tree. Simplified parse tree; semantic structure without syntax noise. |
| **Lexer/Tokenizer** | Converts character stream to token stream. |
| **Parser** | Converts token stream to parse tree/AST. |
| **Recursive Descent** | Top-down parser using mutual recursion. One function per grammar rule. |
| **LL Parser** | Left-to-right, Leftmost derivation. Top-down. LL(k) looks ahead k tokens. |
| **LR Parser** | Left-to-right, Rightmost derivation. Bottom-up. More powerful than LL. |
| **Ambiguous Grammar** | Grammar where some strings have multiple parse trees. |
| **Left Recursion** | Rule like A → Aα. Problematic for recursive descent parsers. |

### Chomsky Hierarchy

```
Type 0: Recursively enumerable    (Turing machine)
Type 1: Context-sensitive         (Linear bounded automaton)
Type 2: Context-free              (Pushdown automaton) ← Most programming languages
Type 3: Regular                   (Finite automaton) ← Regex
```

## Databases

| Term | Definition |
|------|------------|
| **ACID** | Atomicity, Consistency, Isolation, Durability. Transaction guarantees. |
| **Atomicity** | Transaction fully completes or fully rolls back. No partial state. |
| **Consistency** | Transaction brings database from one valid state to another. |
| **Isolation** | Concurrent transactions don't interfere with each other. |
| **Durability** | Committed transactions survive system failures. |
| **BASE** | Basically Available, Soft state, Eventual consistency. NoSQL alternative to ACID. |
| **Transaction** | Logical unit of work; sequence of operations treated as single unit. |
| **Commit** | Make transaction's changes permanent. |
| **Rollback** | Undo transaction's changes. |
| **Normalization** | Organizing data to reduce redundancy. 1NF, 2NF, 3NF, BCNF. |
| **Denormalization** | Intentionally adding redundancy for read performance. |
| **Index** | Data structure for fast lookups. Trade write speed for read speed. |
| **B-Tree** | Balanced tree used for database indexes. O(log n) operations. |
| **Primary Key** | Unique identifier for a row. |
| **Foreign Key** | Reference to primary key in another table. Enforces relationships. |
| **Join** | Combine rows from multiple tables based on related columns. |
| **Sharding** | Horizontal partitioning across multiple databases. Scale-out strategy. |
| **Replication** | Copying data to multiple nodes. Availability and read scaling. |
| **Write-Ahead Log (WAL)** | Log changes before applying. Enables crash recovery. |
| **MVCC** | Multi-Version Concurrency Control. Readers don't block writers. |

### Isolation Levels

```
Read Uncommitted  → Dirty reads possible
Read Committed    → No dirty reads
Repeatable Read   → No dirty reads, no non-repeatable reads
Serializable      → Full isolation (slowest)
```

## Functional Programming

| Term | Definition |
|------|------------|
| **First-Class Function** | Functions can be assigned to variables, passed as arguments, returned. |
| **Higher-Order Function** | Function that takes or returns other functions. |
| **Lambda/Anonymous Function** | Function without a name. `x => x * 2` |
| **Closure** | Function that captures variables from enclosing scope. |
| **Currying** | Transform f(a, b, c) into f(a)(b)(c). Partial application enabler. |
| **Partial Application** | Fix some arguments, return function taking the rest. |
| **Map** | Apply function to each element: [1,2,3].map(x => x*2) → [2,4,6] |
| **Filter** | Keep elements matching predicate: [1,2,3].filter(x => x>1) → [2,3] |
| **Reduce/Fold** | Combine elements into single value: [1,2,3].reduce((a,b) => a+b) → 6 |
| **Functor** | Type with map operation. Applies function inside container. |
| **Monad** | Functor with flatMap/bind. Chains computations that return wrapped values. |
| **Option/Maybe** | Container for value that might be absent. Avoid null. |
| **Either** | Container for value that's one of two types. Often error handling. |
| **Lazy Evaluation** | Delay computation until value needed. Enables infinite structures. |
| **Eager Evaluation** | Compute immediately. Default in most languages. |
| **Tail Recursion** | Recursive call is last operation. Can be optimized to loop. |
| **Pattern Matching** | Destructure data and branch based on structure. |

### Example: Monad Chaining

```
// Without monad (null checks everywhere)
if (user != null) {
  if (user.address != null) {
    return user.address.city;
  }
}

// With Option monad
user.flatMap(u => u.address).map(a => a.city)
```

## Object-Oriented Programming

| Term | Definition |
|------|------------|
| **Class** | Blueprint for creating objects. Defines properties and methods. |
| **Object/Instance** | Concrete realization of a class. |
| **Encapsulation** | Bundle data with methods that operate on it. Hide internal state. |
| **Inheritance** | Create new class from existing class. "is-a" relationship. |
| **Polymorphism** | Same interface, different implementations. Method behaves differently based on type. |
| **Abstraction** | Hide complexity behind simple interface. |
| **Interface** | Contract specifying methods a class must implement. |
| **Abstract Class** | Class that can't be instantiated; meant to be subclassed. |
| **Composition** | Build complex objects from simpler ones. "has-a" relationship. |
| **Aggregation** | Weak composition; contained object can exist independently. |
| **Method Overriding** | Subclass provides different implementation of inherited method. |
| **Method Overloading** | Same method name, different parameter types/counts. |
| **Constructor** | Special method called when creating new instance. |
| **Destructor/Finalizer** | Called when object is destroyed. Cleanup resources. |
| **this/self** | Reference to current object instance. |
| **super** | Reference to parent class. Access overridden methods. |
| **Virtual Method** | Method that can be overridden. Default in some languages. |
| **SOLID** | Single responsibility, Open-closed, Liskov substitution, Interface segregation, Dependency inversion. |

### Composition vs Inheritance

```
Inheritance (is-a):          Composition (has-a):
class Dog extends Animal     class Car {
                               engine: Engine
                               wheels: Wheel[]
                             }

Prefer composition: more flexible, avoids deep hierarchies
```

## Networking

| Term | Definition |
|------|------------|
| **Latency** | Time for data to travel from source to destination. Measured in ms. |
| **Throughput** | Amount of data transferred per unit time. Measured in Mbps, Gbps. |
| **Bandwidth** | Maximum theoretical throughput of a connection. |
| **Jitter** | Variation in latency. Bad for real-time applications. |
| **RTT** | Round-Trip Time. Latency for request + response. |
| **Packet** | Unit of data transmitted over network. Header + payload. |
| **Protocol** | Rules for communication. HTTP, TCP, UDP, etc. |
| **TCP** | Transmission Control Protocol. Reliable, ordered, connection-oriented. |
| **UDP** | User Datagram Protocol. Unreliable, unordered, connectionless. Fast. |
| **IP** | Internet Protocol. Addressing and routing packets. IPv4, IPv6. |
| **DNS** | Domain Name System. Translates domain names to IP addresses. |
| **Port** | Logical endpoint for network communication. 0-65535. |
| **Socket** | Endpoint for sending/receiving data. IP address + port. |
| **Handshake** | Initial exchange to establish connection. TCP uses 3-way handshake. |
| **TLS/SSL** | Transport Layer Security. Encryption for data in transit. |
| **HTTP** | Hypertext Transfer Protocol. Request-response, stateless. |
| **WebSocket** | Full-duplex communication over single TCP connection. |
| **Load Balancer** | Distributes traffic across multiple servers. |
| **Proxy** | Intermediary between client and server. Forward or reverse. |
| **NAT** | Network Address Translation. Maps private IPs to public IP. |

### OSI Model (Simplified)

```
7. Application   HTTP, FTP, SMTP
6. Presentation  Encryption, compression
5. Session       Connections, sessions
4. Transport     TCP, UDP (ports)
3. Network       IP (routing, addressing)
2. Data Link     Ethernet, MAC addresses
1. Physical      Cables, signals
```

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
