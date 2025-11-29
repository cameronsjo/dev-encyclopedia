---
title: Big O Notation
aliases:
  - Time Complexity
  - Space Complexity
  - Asymptotic Analysis
tags:
  - cs
  - algorithms
  - complexity
  - fundamentals
type: concept
status: complete
difficulty: fundamentals
created: 2025-11-28
---

# Big O Notation

A mathematical notation describing the upper bound of an algorithm's growth rate as input size increases.

## Why It Matters

**The question:** "How does performance change as data grows?"

- 100 items: Any algorithm feels fast
- 1 million items: Algorithm choice matters enormously
- 1 billion items: Only certain algorithms are feasible

Big O describes scaling behavior, not absolute speed.

---

## Common Complexities

| Notation | Name | Example | 1K items | 1M items |
|----------|------|---------|----------|----------|
| O(1) | Constant | Array index lookup | 1 | 1 |
| O(log n) | Logarithmic | Binary search | 10 | 20 |
| O(n) | Linear | Array scan | 1,000 | 1,000,000 |
| O(n log n) | Linearithmic | Merge sort | 10,000 | 20,000,000 |
| O(n²) | Quadratic | Nested loops | 1,000,000 | 10¹² |
| O(2ⁿ) | Exponential | Naive recursion | 10³⁰⁰ | Infinite |

```mermaid
graph LR
    A[O(1)] --> B[O(log n)]
    B --> C[O(n)]
    C --> D[O(n log n)]
    D --> E[O(n²)]
    E --> F[O(2ⁿ)]

    style A fill:#90EE90
    style B fill:#90EE90
    style C fill:#FFFF99
    style D fill:#FFFF99
    style E fill:#FFB6C1
    style F fill:#FF6B6B
```

---

## Reading Big O

### What It Describes

- **Worst case** (usually): Maximum operations for any input
- **Growth rate:** How operations scale with input size
- **Dominant term:** Drop constants and lower-order terms

### Constants Don't Matter (Asymptotically)

O(2n) = O(n)
O(n + 1000) = O(n)
O(n² + n) = O(n²)

**Why:** As n approaches infinity, constants become irrelevant.

**Caveat:** Constants matter in practice for reasonable n. O(100n) can be slower than O(n²) for small inputs.

---

## Recognizing Complexity

### O(1) — Constant

Operations don't depend on input size.

**Examples:**
- Array index access
- Hash table lookup (average)
- Stack push/pop
- Linked list head access

### O(log n) — Logarithmic

Halving the problem each step.

**Examples:**
- Binary search
- Balanced tree operations
- Finding in sorted array
- Exponentiation by squaring

**Key insight:** log₂(1,000,000) ≈ 20. Logarithmic is nearly constant for practical sizes.

### O(n) — Linear

Touch each element once.

**Examples:**
- Array traversal
- Linear search
- Counting elements
- Finding max/min

### O(n log n) — Linearithmic

Typically: divide and conquer with linear work at each level.

**Examples:**
- Merge sort
- Heap sort
- Quick sort (average)
- Many divide-and-conquer algorithms

**Why it's good:** Lower bound for comparison-based sorting. You can't do better.

### O(n²) — Quadratic

Nested iteration over input.

**Examples:**
- Bubble sort
- Insertion sort
- Naive string matching
- Comparing all pairs

**Warning sign:** Nested loops where both depend on n.

### O(2ⁿ) — Exponential

Combinatorial explosion.

**Examples:**
- Naive Fibonacci
- Power set generation
- Traveling salesman (brute force)
- Many NP-hard problems

**Reality:** Only feasible for n < 30 or so.

---

## Space Complexity

Same notation, but for memory usage.

| Algorithm | Time | Space |
|-----------|------|-------|
| Merge sort | O(n log n) | O(n) |
| Quick sort | O(n log n) | O(log n) |
| Heap sort | O(n log n) | O(1) |

**Trade-off:** Sometimes you trade space for time (caching, memoization) or vice versa.

### In-Place Algorithms

O(1) extra space beyond input.

**Examples:**
- Heap sort
- Quick sort (with careful implementation)
- Two-pointer techniques

---

## Amortized Analysis

Average cost over a sequence of operations.

**Classic example: Dynamic array (ArrayList, Vec)**

| Operation | Worst Case | Amortized |
|-----------|------------|-----------|
| Append | O(n) resize | O(1) |

Resize happens rarely. Most appends are O(1). Overall: O(1) amortized.

---

## Practical Considerations

### When Big O Misleads

| Situation | Reality |
|-----------|---------|
| Small n | Constants dominate |
| Cache locality | O(n) array can beat O(log n) tree |
| Hidden constants | Hash table "O(1)" has overhead |
| Worst vs average | Quick sort O(n²) worst, O(n log n) average |

### Rule of Thumb

| Input Size | Acceptable Complexity |
|------------|----------------------|
| n ≤ 20 | O(2ⁿ), O(n!) |
| n ≤ 1,000 | O(n²) |
| n ≤ 100,000 | O(n log n) |
| n ≤ 10,000,000 | O(n) |
| n > 10,000,000 | O(log n), O(1) |

---

## Common Operations by Data Structure

| Structure | Access | Search | Insert | Delete |
|-----------|--------|--------|--------|--------|
| Array | O(1) | O(n) | O(n) | O(n) |
| Linked List | O(n) | O(n) | O(1)* | O(1)* |
| Hash Table | — | O(1) avg | O(1) avg | O(1) avg |
| Binary Search Tree | — | O(log n) | O(log n) | O(log n) |
| Heap | — | O(n) | O(log n) | O(log n) |

*At known position

---

## Analyzing Code

### Loop Analysis

**Single loop over n elements:** O(n)

**Nested loops:**
- Both iterate n times: O(n²)
- Inner loop constant: O(n)
- Inner loop halves: O(n log n)

**Sequential loops:** O(n) + O(n) = O(n)

### Recursion Analysis

**Recurrence relation → Master theorem**

| Recurrence | Complexity | Algorithm |
|------------|------------|-----------|
| T(n) = T(n/2) + O(1) | O(log n) | Binary search |
| T(n) = 2T(n/2) + O(n) | O(n log n) | Merge sort |
| T(n) = 2T(n/2) + O(1) | O(n) | Tree traversal |
| T(n) = T(n-1) + O(n) | O(n²) | Selection sort |
| T(n) = 2T(n-1) + O(1) | O(2ⁿ) | Naive Fibonacci |

---

## Beyond Big O

| Notation | Meaning |
|----------|---------|
| O (Big O) | Upper bound (worst case) |
| Ω (Omega) | Lower bound (best case) |
| Θ (Theta) | Tight bound (average case) |

**In practice:** Big O is most commonly used and discussed.

---

## Related

- [[Sorting Algorithms]]
- [[Searching Algorithms]]
- [[Data Structures]]
- [[Dynamic Programming]]
