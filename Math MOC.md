---
title: Math MOC
tags:
  - moc
  - math
  - mathematics
type: moc
created: 2025-11-28
---

# Mathematics MOC

Mathematical foundations for software engineering and machine learning.

## Core Topics

### Calculus

- [[Limits & Continuity]]
- [[Derivatives]]
- [[Integrals]]
- [[Multivariable Calculus]]
- [[Differential Equations]]

### Linear Algebra

- [[Vectors & Matrices]]
- [[Matrix Operations]]
- [[Eigenvalues & Eigenvectors]]
- [[Linear Transformations]]
- [[Vector Spaces]]

### Probability & Statistics

- [[Probability Fundamentals]]
- [[Distributions]]
- [[Statistical Inference]]
- [[Hypothesis Testing]]
- [[Bayesian Statistics]]
- [[Regression Analysis]]

### Discrete Math

- [[Set Theory]]
- [[Graph Theory]]
- [[Combinatorics]]
- [[Logic & Proofs]]
- [[Number Theory]]

### Numerical Methods

- [[Numerical Integration]]
- [[Root Finding]]
- [[Optimization Methods]]
- [[Interpolation]]
- [[Numerical Linear Algebra]]

---

## All Math Pages

```dataview
TABLE status, difficulty as "Level"
FROM #math
WHERE type != "moc"
SORT file.name ASC
```

## By Application

### For ML/AI

```dataview
LIST
FROM #math AND #ml-prerequisite
```

### For Graphics

```dataview
LIST
FROM #math AND #graphics
```

### For Cryptography

```dataview
LIST
FROM #math AND #crypto
```
