---
title: Graph Algorithms
aliases:
  - Graph Theory
  - Graph Traversal
tags:
  - cs
  - algorithms
  - graphs
  - intermediate
type: concept
status: complete
difficulty: intermediate
created: '2025-11-28'
---

# Graph Algorithms

Algorithms for traversing, searching, and analyzing graphs.

## Graph Basics

### Terminology

| Term | Definition |
|------|------------|
| Vertex (Node) | Entity in graph |
| Edge | Connection between vertices |
| Directed | Edges have direction |
| Undirected | Edges go both ways |
| Weighted | Edges have values |
| Degree | Number of edges on vertex |
| Path | Sequence of connected vertices |
| Cycle | Path that returns to start |
| Connected | Path exists between all vertices |
| DAG | Directed Acyclic Graph |

### Representations

| Type | Space | Add Edge | Check Edge | Neighbors |
|------|-------|----------|------------|-----------|
| Adjacency Matrix | O(V²) | O(1) | O(1) | O(V) |
| Adjacency List | O(V+E) | O(1) | O(degree) | O(degree) |

**Use adjacency list** for most cases (sparse graphs).

**Use adjacency matrix** when graph is dense or need fast edge lookup.

---

## Traversal

### BFS (Breadth-First Search)

**What:** Visit all neighbors before going deeper.

**Uses:**

- Shortest path in unweighted graph
- Level-order traversal
- Finding connected components
- Web crawling

**Complexity:** O(V + E)

**Key insight:** Uses queue. Explores layer by layer.

### DFS (Depth-First Search)

**What:** Go as deep as possible, then backtrack.

**Uses:**

- Topological sort
- Cycle detection
- Path finding
- Maze solving
- Connected components

**Complexity:** O(V + E)

**Key insight:** Uses stack (or recursion). Explores branches fully.

### When to Use Which

| Problem | Algorithm |
|---------|-----------|
| Shortest path (unweighted) | BFS |
| Shortest path (weighted) | Dijkstra/Bellman-Ford |
| All paths exist? | DFS |
| Cycle detection | DFS |
| Topological sort | DFS |
| Level by level | BFS |
| Maze with one solution | DFS |
| Social network degrees | BFS |

---

## Shortest Path

### Dijkstra's Algorithm

**What:** Shortest path from source to all vertices (non-negative weights).

**Complexity:**

- O(V²) with array
- O((V + E) log V) with priority queue

**Limitation:** No negative edge weights.

**Use when:** GPS navigation, network routing, game pathfinding.

### Bellman-Ford

**What:** Shortest path with possible negative weights.

**Complexity:** O(VE)

**Advantage:** Handles negative weights, detects negative cycles.

**Use when:** Currency arbitrage detection, graphs with negative weights.

### Floyd-Warshall

**What:** All-pairs shortest path.

**Complexity:** O(V³)

**Use when:** Small graphs, need all pairs, can precompute.

### A* Search

**What:** Dijkstra with heuristic guidance.

**Complexity:** Depends on heuristic quality.

**Use when:** Pathfinding with known goal (games, maps). Heuristic estimates distance to goal.

---

## Minimum Spanning Tree

**What:** Connect all vertices with minimum total edge weight.

### Kruskal's Algorithm

**How:** Sort edges, add smallest that doesn't create cycle.

**Complexity:** O(E log E)

**Uses Union-Find** for cycle detection.

### Prim's Algorithm

**How:** Grow tree from starting vertex, always add cheapest edge.

**Complexity:** O(E log V) with priority queue.

**Use when:** Dense graphs, start vertex matters.

---

## Topological Sort

**What:** Linear ordering of DAG vertices such that for every edge u→v, u comes before v.

**Use when:**

- Task scheduling with dependencies
- Build systems
- Course prerequisites
- Package installation order

**Algorithms:**

1. DFS-based: Post-order reverse
2. Kahn's: Remove vertices with no incoming edges

**Complexity:** O(V + E)

**Note:** Only possible for DAGs. Cycle = no valid ordering.

---

## Cycle Detection

### In Directed Graphs

**DFS with coloring:**

- White: unvisited
- Gray: in current path
- Black: finished

Gray → Gray edge = cycle.

### In Undirected Graphs

**DFS:** Back edge to non-parent = cycle.

**Union-Find:** Edge connects already-connected nodes = cycle.

---

## Strongly Connected Components

**What:** Maximal sets of vertices where every vertex is reachable from every other.

**Algorithms:**

- Kosaraju's: Two DFS passes
- Tarjan's: Single DFS with low-link values

**Use when:**

- Social network analysis
- Compiler optimization (finding loops)
- 2-SAT problem solving

---

## Common Problems

### Finding Shortest Path

| Graph Type | Algorithm |
|------------|-----------|
| Unweighted | BFS |
| Non-negative weights | Dijkstra |
| Negative weights | Bellman-Ford |
| All pairs | Floyd-Warshall |
| Known goal | A* |

### Is Graph Connected?

**Undirected:** Run BFS/DFS from any node. Visit all nodes = connected.

**Directed:** Check if strongly connected (every node reachable from every other).

### Find All Connected Components

Run BFS/DFS. Mark visited. Repeat from unvisited nodes.

### Detect Cycle

**Directed:** DFS with gray/black coloring.

**Undirected:** DFS checking for back edges to non-parent.

### Bipartite Check

**BFS/DFS with two colors.** If neighbor has same color = not bipartite.

---

## Complexity Summary

| Algorithm | Time | Space |
|-----------|------|-------|
| BFS | O(V + E) | O(V) |
| DFS | O(V + E) | O(V) |
| Dijkstra | O((V + E) log V) | O(V) |
| Bellman-Ford | O(VE) | O(V) |
| Floyd-Warshall | O(V³) | O(V²) |
| Kruskal | O(E log E) | O(V) |
| Prim | O(E log V) | O(V) |
| Topological Sort | O(V + E) | O(V) |
| Tarjan SCC | O(V + E) | O(V) |

---

## Related

- [[Big O Notation]]
- [[Data Structures]]
- [[Dynamic Programming]]
