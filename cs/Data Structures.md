---
title: Data Structures
aliases:
  - Data Structure Overview
tags:
  - cs
  - data-structures
  - fundamentals
type: concept
status: complete
difficulty: fundamentals
created: 2025-11-28
---

# Data Structures

Organized ways to store and access data, each with different performance trade-offs.

## Choosing a Data Structure

**Key questions:**
1. What operations are most frequent?
2. How large is the data?
3. Is ordering important?
4. Are duplicates allowed?
5. Is the data changing or static?

---

## Linear Structures

### Arrays

**What:** Contiguous block of memory with indexed access.

| Operation | Complexity |
|-----------|------------|
| Access by index | O(1) |
| Search | O(n) |
| Insert at end | O(1) amortized |
| Insert at position | O(n) |
| Delete | O(n) |

**Use when:**
- Need fast random access
- Know size upfront (or growth is acceptable)
- Cache locality matters

**Language implementations:**
| Language | Fixed | Dynamic |
|----------|-------|---------|
| C# | `T[]` | `List<T>` |
| Go | `[n]T` | `[]T` slice |
| Python | — | `list` |
| TypeScript | — | `Array` |
| Rust | `[T; N]` | `Vec<T>` |

### Linked Lists

**What:** Nodes connected by pointers.

| Operation | Singly | Doubly |
|-----------|--------|--------|
| Access by index | O(n) | O(n) |
| Insert at head | O(1) | O(1) |
| Insert at tail | O(n) / O(1)* | O(1) |
| Insert after node | O(1) | O(1) |
| Delete node | O(n) | O(1) |

*O(1) if tail pointer maintained

**Use when:**
- Frequent insertions/deletions at known positions
- Don't need random access
- Building queues or stacks

**Reality check:** Arrays are usually faster due to cache locality. Linked lists have overhead per node.

### Stacks

**What:** LIFO (Last In, First Out).

| Operation | Complexity |
|-----------|------------|
| Push | O(1) |
| Pop | O(1) |
| Peek | O(1) |

**Use when:**
- Function call tracking
- Undo mechanisms
- Expression evaluation
- DFS traversal

**Implementation:** Usually backed by array or linked list.

### Queues

**What:** FIFO (First In, First Out).

| Operation | Complexity |
|-----------|------------|
| Enqueue | O(1) |
| Dequeue | O(1) |
| Peek | O(1) |

**Use when:**
- BFS traversal
- Task scheduling
- Message passing
- Rate limiting

**Variants:**
| Variant | Ordering |
|---------|----------|
| Queue | FIFO |
| Deque | Both ends |
| Priority Queue | By priority |
| Circular Queue | Fixed-size ring |

---

## Hash-Based Structures

### Hash Tables

**What:** Key-value store using hash function for O(1) average access.

| Operation | Average | Worst |
|-----------|---------|-------|
| Search | O(1) | O(n) |
| Insert | O(1) | O(n) |
| Delete | O(1) | O(n) |

**Collision resolution:**
| Method | How |
|--------|-----|
| Chaining | Linked list at each bucket |
| Open addressing | Probe for next empty slot |

**Use when:**
- Need fast lookup by key
- Key equality is well-defined
- Ordering doesn't matter

**Language implementations:**
| Language | Map | Set |
|----------|-----|-----|
| C# | `Dictionary<K,V>` | `HashSet<T>` |
| Go | `map[K]V` | — |
| Python | `dict` | `set` |
| TypeScript | `Map`, `Object` | `Set` |
| Rust | `HashMap<K,V>` | `HashSet<T>` |

### Hash Functions

**Good hash function:**
- Deterministic (same input → same output)
- Uniform distribution
- Fast to compute
- Avalanche effect (small input change → big output change)

**For custom types:** Override `GetHashCode()`, `Hash` trait, `__hash__()`, etc.

---

## Tree Structures

### Binary Trees

**What:** Each node has at most two children.

**Traversals:**
| Order | Visit Sequence |
|-------|----------------|
| Pre-order | Root, Left, Right |
| In-order | Left, Root, Right |
| Post-order | Left, Right, Root |
| Level-order | Breadth-first |

### Binary Search Trees (BST)

**Property:** Left < Node < Right

| Operation | Average | Worst (unbalanced) |
|-----------|---------|-------------------|
| Search | O(log n) | O(n) |
| Insert | O(log n) | O(n) |
| Delete | O(log n) | O(n) |

**Problem:** Can degrade to linked list if insertions are ordered.

**Solution:** Self-balancing trees.

### Self-Balancing Trees

| Tree | Balance Method | Use Case |
|------|----------------|----------|
| AVL | Height difference ≤ 1 | Read-heavy, strict balance |
| Red-Black | Color properties | General purpose |
| B-Tree | Multi-way, disk-optimized | Databases, filesystems |
| B+ Tree | Leaves linked | Range queries |

**Language implementations:**
| Language | Sorted Map |
|----------|------------|
| C# | `SortedDictionary<K,V>` (RB-tree) |
| C++ | `std::map` (usually RB-tree) |
| Java | `TreeMap` (RB-tree) |
| Rust | `BTreeMap` (B-tree) |

### Heaps

**What:** Complete binary tree with heap property.

| Type | Property |
|------|----------|
| Min-heap | Parent ≤ children |
| Max-heap | Parent ≥ children |

| Operation | Complexity |
|-----------|------------|
| Find min/max | O(1) |
| Insert | O(log n) |
| Extract min/max | O(log n) |
| Build heap | O(n) |

**Use when:**
- Need quick access to min/max
- Priority queues
- Heap sort
- Top-K problems

**Implementation:** Usually array-backed. Children of node i at 2i+1 and 2i+2.

### Tries (Prefix Trees)

**What:** Tree for storing strings by character.

| Operation | Complexity |
|-----------|------------|
| Search | O(m) where m = key length |
| Insert | O(m) |
| Prefix search | O(m) |

**Use when:**
- Autocomplete
- Spell checking
- IP routing tables
- Dictionary implementations

---

## Graph Structures

### Representations

| Representation | Space | Add Edge | Check Edge | Iterate Neighbors |
|----------------|-------|----------|------------|-------------------|
| Adjacency Matrix | O(V²) | O(1) | O(1) | O(V) |
| Adjacency List | O(V+E) | O(1) | O(degree) | O(degree) |
| Edge List | O(E) | O(1) | O(E) | O(E) |

**Adjacency list** is most common for sparse graphs.

**Adjacency matrix** when graph is dense or need fast edge checks.

### Common Graph Types

| Type | Property |
|------|----------|
| Directed | Edges have direction |
| Undirected | Edges are bidirectional |
| Weighted | Edges have values |
| DAG | Directed, no cycles |
| Tree | Connected, no cycles |

---

## Specialized Structures

### Bloom Filters

**What:** Probabilistic set membership. May have false positives, never false negatives.

**Use when:**
- Checking if item might be in set
- Cache lookup optimization
- Duplicate detection at scale

### Skip Lists

**What:** Layered linked lists for O(log n) search.

**Use when:**
- Need sorted operations
- Simpler than balanced trees
- Concurrent access (lock-free variants)

### Union-Find (Disjoint Set)

**What:** Track connected components with near-O(1) operations.

| Operation | Complexity |
|-----------|------------|
| Union | O(α(n)) ≈ O(1) |
| Find | O(α(n)) ≈ O(1) |

**Use when:**
- Kruskal's MST algorithm
- Cycle detection
- Connected components
- Percolation problems

---

## Decision Guide

| Need | Data Structure |
|------|----------------|
| Fast access by index | Array |
| Fast access by key | Hash table |
| Sorted access | BST, B-tree, sorted array |
| LIFO | Stack |
| FIFO | Queue |
| Priority access | Heap |
| String prefixes | Trie |
| Graph traversal | Adjacency list |
| Set membership (probabilistic) | Bloom filter |

---

## Related

- [[Big O Notation]]
- [[Sorting Algorithms]]
- [[Graph Algorithms]]
- [[Hash Tables]]
- [[Trees]]
