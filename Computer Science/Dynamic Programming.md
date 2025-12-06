---
title: Dynamic Programming
aliases:
  - DP
  - Dynamic Programming
tags:
  - cs
  - algorithms
  - fundamentals
type: reference
status: complete
created: 2025-11-30
---

# Dynamic Programming

An algorithmic optimization technique that solves complex problems by breaking them down into simpler subproblems, storing solutions to avoid redundant computation.

## Overview

| Aspect | Details |
|--------|---------|
| **Core Principle** | Store solutions to subproblems and reuse them |
| **When to Use** | Problems with optimal substructure and overlapping subproblems |
| **Time Complexity** | Typically O(n) to O(n³) depending on problem dimensions |
| **Space Complexity** | O(n) to O(n²), often reducible with optimization |
| **Key Benefit** | Reduces exponential time to polynomial time |

## Core Concepts

### Optimal Substructure

A problem has optimal substructure if its optimal solution can be constructed from optimal solutions of its subproblems.

**Example:** Shortest path from A to C through B = shortest path from A to B + shortest path from B to C.

### Overlapping Subproblems

The problem can be broken down into subproblems that are reused multiple times. Without memoization, these would be recalculated repeatedly.

**Example:** Computing Fibonacci(5) requires Fibonacci(3) twice when calculated recursively.

## Approaches

| Approach | Description | When to Use | Space | Complexity |
|----------|-------------|-------------|-------|------------|
| **Memoization** | Top-down recursive with caching | Intuitive, doesn't compute all states | ✅ Can skip unneeded states | O(n) to O(n²) |
| **Tabulation** | Bottom-up iterative | All states needed, iterative preference | ❌ Computes all states | O(n) to O(n²) |

### Memoization (Top-Down)

Recursive approach with caching of computed results.

```python
def fib_memo(n: int, memo: dict[int, int] = None) -> int:
    if memo is None:
        memo = {}
    if n in memo:
        return memo[n]
    if n <= 1:
        return n
    memo[n] = fib_memo(n - 1, memo) + fib_memo(n - 2, memo)
    return memo[n]

# Time: O(n), Space: O(n)
```

### Tabulation (Bottom-Up)

Iterative approach building from base cases.

```python
def fib_tab(n: int) -> int:
    if n <= 1:
        return n
    dp = [0] * (n + 1)
    dp[1] = 1
    for i in range(2, n + 1):
        dp[i] = dp[i - 1] + dp[i - 2]
    return dp[n]

# Time: O(n), Space: O(n)
```

### Space Optimization

Many DP problems can reduce space by keeping only needed previous states.

```python
def fib_optimized(n: int) -> int:
    if n <= 1:
        return n
    prev2, prev1 = 0, 1
    for _ in range(2, n + 1):
        current = prev1 + prev2
        prev2, prev1 = prev1, current
    return prev1

# Time: O(n), Space: O(1)
```

## Classic Problems

### 1. Fibonacci Sequence

**State:** `dp[i]` = ith Fibonacci number

**Recurrence:** `dp[i] = dp[i-1] + dp[i-2]`

**Base Cases:** `dp[0] = 0`, `dp[1] = 1`

**Complexity:** O(n) time, O(1) space optimized

### 2. Coin Change

**Problem:** Minimum coins to make amount using given denominations.

**State:** `dp[amount]` = minimum coins to make amount

**Recurrence:** `dp[i] = min(dp[i - coin] + 1)` for each coin

**Base Case:** `dp[0] = 0`

```python
def coin_change(coins: list[int], amount: int) -> int:
    dp = [float('inf')] * (amount + 1)
    dp[0] = 0

    for i in range(1, amount + 1):
        for coin in coins:
            if i >= coin:
                dp[i] = min(dp[i], dp[i - coin] + 1)

    return dp[amount] if dp[amount] != float('inf') else -1

# Time: O(amount * len(coins)), Space: O(amount)
```

### 3. 0/1 Knapsack

**Problem:** Maximize value with weight constraint, items used once.

**State:** `dp[i][w]` = max value using first i items with weight limit w

**Recurrence:**
```
dp[i][w] = max(
    dp[i-1][w],              # don't take item i
    dp[i-1][w-weight[i]] + value[i]  # take item i
)
```

**Complexity:** O(n × capacity) time, O(capacity) space optimized

```python
def knapsack(weights: list[int], values: list[int], capacity: int) -> int:
    n = len(weights)
    dp = [[0] * (capacity + 1) for _ in range(n + 1)]

    for i in range(1, n + 1):
        for w in range(capacity + 1):
            if weights[i-1] <= w:
                dp[i][w] = max(
                    dp[i-1][w],
                    dp[i-1][w - weights[i-1]] + values[i-1]
                )
            else:
                dp[i][w] = dp[i-1][w]

    return dp[n][capacity]

# Time: O(n * capacity), Space: O(n * capacity)
# Space optimizable to O(capacity) with 1D array
```

### 4. Longest Common Subsequence (LCS)

**Problem:** Find length of longest subsequence common to two strings.

**State:** `dp[i][j]` = LCS length of text1[0:i] and text2[0:j]

**Recurrence:**
```
if text1[i-1] == text2[j-1]:
    dp[i][j] = dp[i-1][j-1] + 1
else:
    dp[i][j] = max(dp[i-1][j], dp[i][j-1])
```

**Complexity:** O(m × n) time, O(min(m, n)) space optimized

### 5. Longest Increasing Subsequence (LIS)

**Problem:** Find length of longest strictly increasing subsequence.

**State:** `dp[i]` = length of LIS ending at index i

**Recurrence:** `dp[i] = max(dp[j] + 1)` for all j < i where nums[j] < nums[i]

**Complexity:** O(n²) time with DP, O(n log n) with binary search optimization

```python
def longest_increasing_subsequence(nums: list[int]) -> int:
    if not nums:
        return 0

    dp = [1] * len(nums)

    for i in range(1, len(nums)):
        for j in range(i):
            if nums[j] < nums[i]:
                dp[i] = max(dp[i], dp[j] + 1)

    return max(dp)

# Time: O(n²), Space: O(n)
```

### 6. Edit Distance (Levenshtein Distance)

**Problem:** Minimum operations (insert, delete, replace) to convert word1 to word2.

**State:** `dp[i][j]` = edit distance for word1[0:i] to word2[0:j]

**Recurrence:**
```
if word1[i-1] == word2[j-1]:
    dp[i][j] = dp[i-1][j-1]
else:
    dp[i][j] = 1 + min(
        dp[i-1][j],    # delete
        dp[i][j-1],    # insert
        dp[i-1][j-1]   # replace
    )
```

**Complexity:** O(m × n) time, O(min(m, n)) space optimized

### 7. Matrix Chain Multiplication

**Problem:** Find optimal order to multiply sequence of matrices.

**State:** `dp[i][j]` = minimum operations to multiply matrices from i to j

**Recurrence:**
```
dp[i][j] = min(
    dp[i][k] + dp[k+1][j] + dimensions[i-1] * dimensions[k] * dimensions[j]
)
for all k in range(i, j)
```

**Complexity:** O(n³) time, O(n²) space

## DP Patterns

### Linear DP (1D)

Problems where state depends on previous elements in a sequence.

**Examples:** Fibonacci, House Robber, Climbing Stairs, Decode Ways

**Pattern:**
```python
dp = [base_case] * n
for i in range(start, n):
    dp[i] = function(dp[i-1], dp[i-2], ...)
```

### Grid DP (2D)

Problems involving paths, grids, or two sequences.

**Examples:** Unique Paths, Minimum Path Sum, LCS, Edit Distance

**Pattern:**
```python
dp = [[0] * cols for _ in range(rows)]
for i in range(rows):
    for j in range(cols):
        dp[i][j] = function(dp[i-1][j], dp[i][j-1], ...)
```

### Interval DP

Problems involving ranges or intervals.

**Examples:** Matrix Chain Multiplication, Palindrome Partitioning, Burst Balloons

**Pattern:**
```python
# Process by increasing interval length
for length in range(2, n + 1):
    for i in range(n - length + 1):
        j = i + length - 1
        for k in range(i, j):
            dp[i][j] = optimize(dp[i][k], dp[k+1][j])
```

### State Machine DP

Problems with discrete states and transitions.

**Examples:** Best Time to Buy/Sell Stock (with cooldown/fees), Paint House

**Pattern:**
```python
# Multiple states per position
state1, state2 = initial_values
for element in sequence:
    new_state1 = function(state1, state2, element)
    new_state2 = function(state1, state2, element)
    state1, state2 = new_state1, new_state2
```

## Complexity Analysis by Pattern

| Pattern | Typical Time | Typical Space | Space Optimizable |
|---------|--------------|---------------|-------------------|
| Linear 1D | O(n) | O(n) | ✅ Often to O(1) |
| Grid 2D | O(m × n) | O(m × n) | ✅ To O(min(m,n)) |
| Interval DP | O(n³) | O(n²) | ❌ Usually not |
| Knapsack | O(n × W) | O(n × W) | ✅ To O(W) |
| LIS (DP) | O(n²) | O(n) | ❌ Already minimal |
| String DP | O(m × n) | O(m × n) | ✅ To O(min(m,n)) |

## Problem-Solving Framework

1. **Identify DP applicability:**
   - Does it have optimal substructure?
   - Are there overlapping subproblems?

2. **Define the state:**
   - What information is needed to represent a subproblem?
   - How many dimensions are required?

3. **Find the recurrence relation:**
   - How does the current state relate to previous states?
   - What are the transition options?

4. **Determine base cases:**
   - What are the simplest subproblems with known answers?

5. **Choose approach:**
   - Memoization: intuitive, handles sparse states
   - Tabulation: iterative, potentially more efficient

6. **Optimize space:**
   - Can you reduce dimensions?
   - Do you only need the last k states?

## When to Use Dynamic Programming

| Use When | Avoid When |
|----------|------------|
| ✅ Optimization problems (min/max) | ❌ Need to find all solutions (use backtracking) |
| ✅ Counting problems (number of ways) | ❌ Problem requires actual path/solution construction only |
| ✅ Overlapping subproblems exist | ❌ Subproblems are independent (use divide & conquer) |
| ✅ Optimal substructure present | ❌ Greedy approach suffices |
| ✅ Decision problems (yes/no) | ❌ Real-time constraints (DP has setup cost) |

## Common Mistakes

1. **Off-by-one errors:** Carefully handle array indices and loop bounds
2. **Incorrect base cases:** Ensure base cases cover all edge scenarios
3. **Wrong state definition:** State must capture all information needed for transitions
4. **Space optimization errors:** Ensure you don't overwrite needed values
5. **Integer overflow:** Use appropriate data types for large values

## Related

- [[Big O Notation]] - Understanding time and space complexity
- [[Sorting Algorithms]] - Foundational algorithmic techniques
- [[Graph Algorithms]] - Advanced algorithmic problem solving
- [[Recursion]] - Foundation for memoization approach
- [[Greedy Algorithms]] - Alternative optimization technique
