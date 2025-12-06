---
title: Sorting Algorithms
aliases:
  - Sorting
  - Sort Algorithms
tags:
  - cs
  - algorithms
  - sorting
  - fundamentals
type: concept
status: complete
difficulty: fundamentals
created: '2025-11-28'
---

# Sorting Algorithms

Algorithms for arranging elements in order.

## Overview

| Algorithm | Time (Avg) | Time (Worst) | Space | Stable | In-Place |
|-----------|------------|--------------|-------|--------|----------|
| Quick Sort | O(n log n) | O(n²) | O(log n) | No | Yes |
| Merge Sort | O(n log n) | O(n log n) | O(n) | Yes | No |
| Heap Sort | O(n log n) | O(n log n) | O(1) | No | Yes |
| Tim Sort | O(n log n) | O(n log n) | O(n) | Yes | No |
| Insertion Sort | O(n²) | O(n²) | O(1) | Yes | Yes |
| Bubble Sort | O(n²) | O(n²) | O(1) | Yes | Yes |
| Selection Sort | O(n²) | O(n²) | O(1) | No | Yes |
| Counting Sort | O(n + k) | O(n + k) | O(k) | Yes | No |
| Radix Sort | O(nk) | O(nk) | O(n + k) | Yes | No |

**Stable:** Equal elements maintain relative order.

**In-place:** Constant extra space.

---

## Comparison-Based Sorts

### Quick Sort

**How it works:**

1. Pick a pivot
2. Partition: elements < pivot on left, > pivot on right
3. Recursively sort partitions

**Characteristics:**

- Average O(n log n), worst O(n²) with bad pivots
- In-place with O(log n) stack space
- Not stable
- Excellent cache locality

**Pivot selection matters:**

| Strategy | Risk |
|----------|------|
| First/last element | O(n²) on sorted input |
| Random | Good average |
| Median-of-three | Better for practical data |

**Use when:** General-purpose, large datasets, cache performance matters.

### Merge Sort

**How it works:**

1. Divide array in half
2. Recursively sort halves
3. Merge sorted halves

**Characteristics:**

- Always O(n log n)
- Stable
- Requires O(n) extra space
- Good for linked lists (no extra space)

**Use when:** Stability required, worst-case guarantee needed, sorting linked lists.

### Heap Sort

**How it works:**

1. Build max heap from array
2. Repeatedly extract max, place at end

**Characteristics:**

- Always O(n log n)
- In-place O(1) extra
- Not stable
- Poor cache locality

**Use when:** Need O(1) space with guaranteed O(n log n).

### Tim Sort

**How it works:**

- Hybrid of merge sort and insertion sort
- Finds natural "runs" (sorted subsequences)
- Merges runs intelligently

**Characteristics:**

- O(n) on nearly sorted data
- Stable
- O(n) extra space

**Use when:** Real-world data (often partially sorted). Default in Python, Java, JavaScript.

### Insertion Sort

**How it works:**

- Build sorted portion one element at a time
- Insert each element in correct position

**Characteristics:**

- O(n) on nearly sorted data
- O(n²) average/worst
- Stable, in-place
- Very low overhead

**Use when:** Small datasets (< 50 elements), nearly sorted data, simple implementation needed.

---

## Non-Comparison Sorts

Can beat O(n log n) by not comparing elements.

### Counting Sort

**How it works:**

1. Count occurrences of each value
2. Calculate positions from counts
3. Place elements in output array

**Characteristics:**

- O(n + k) where k = range of values
- Only works for integers in known range
- Stable

**Use when:** Small integer range, many duplicates.

### Radix Sort

**How it works:**

- Sort by each digit (or character), least to most significant
- Use stable sort (counting sort) for each digit

**Characteristics:**

- O(nk) where k = number of digits
- Only for integers or fixed-length strings
- Stable

**Use when:** Sorting integers, strings of uniform length.

### Bucket Sort

**How it works:**

1. Distribute elements into buckets by range
2. Sort each bucket
3. Concatenate buckets

**Characteristics:**

- O(n) average when uniform distribution
- O(n²) worst when all elements in one bucket

**Use when:** Uniform distribution known, floating point numbers.

---

## Special Cases

### Sorting Small Arrays

For n < 10-50, simple algorithms win:

- Insertion sort has lowest overhead
- No recursion, no complex logic
- Standard library sorts often switch to insertion for small partitions

### Nearly Sorted Data

| Algorithm | Performance |
|-----------|-------------|
| Tim Sort | O(n) |
| Insertion Sort | O(n) |
| Quick Sort | O(n²) with bad pivot |

### Sorting with Expensive Comparisons

If comparing elements is costly:

- Minimize comparisons (merge sort = n log n comparisons)
- Consider Schwartzian transform: compute keys once, sort by keys

### External Sorting

Data doesn't fit in memory:

- External merge sort
- Sort chunks that fit in memory
- Merge sorted chunks from disk

---

## Stability Matters When

**Stable sort preserves original order for equal elements.**

Important for:

- Multi-key sorting (sort by last name, then first name)
- Maintaining previous sort order
- Database operations

**Stable sorts:** Merge, Insertion, Tim, Counting, Radix
**Unstable sorts:** Quick, Heap, Selection

---

## Standard Library Sorts

| Language | Algorithm | Notes |
|----------|-----------|-------|
| C# | Introsort | Quicksort → Heapsort fallback |
| Go | Pattern-defeating quicksort | Hybrid approach |
| Python | Timsort | Adaptive merge sort |
| JavaScript | Varies | V8 uses Timsort |
| Rust | Pattern-defeating quicksort | Similar to Go |
| C++ | Introsort | std::sort |

**Recommendation:** Use standard library. They're heavily optimized and handle edge cases.

---

## Decision Guide

| Scenario | Recommendation |
|----------|----------------|
| General purpose | Standard library sort |
| Need stability | Merge sort or Tim sort |
| Memory constrained | Heap sort |
| Small arrays | Insertion sort |
| Nearly sorted | Insertion sort or Tim sort |
| Integers in range | Counting sort |
| Strings/integers | Radix sort |
| Guaranteed O(n log n) | Merge sort or Heap sort |

---

## Related

- [[Big O Notation]]
- [[Data Structures]]
- [[Searching Algorithms]]
