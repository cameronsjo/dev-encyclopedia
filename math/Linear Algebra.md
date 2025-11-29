---
title: Linear Algebra
aliases:
  - Matrices
  - Vectors
tags:
  - math
  - linear-algebra
  - ml-prerequisite
type: concept
status: complete
difficulty: fundamentals
created: 2025-11-28
---

# Linear Algebra

The mathematics of vectors and matrices. Foundation for ML, graphics, and data analysis.

## Why It Matters for CS/ML

- **Neural networks:** Matrix multiplications everywhere
- **Computer graphics:** Transformations, projections
- **Data science:** PCA, SVD for dimensionality reduction
- **Recommendation systems:** Matrix factorization
- **Search:** PageRank uses eigenvectors

---

## Vectors

### The Concept

Ordered list of numbers representing a point or direction in space.

**Notation:** **v** = [v₁, v₂, ..., vₙ]

### Key Operations

| Operation | Formula | Result |
|-----------|---------|--------|
| Addition | **u** + **v** | [u₁+v₁, u₂+v₂, ...] |
| Scalar mult | c**v** | [cv₁, cv₂, ...] |
| Dot product | **u** · **v** | u₁v₁ + u₂v₂ + ... |
| Norm (length) | \|\|**v**\|\| | √(v₁² + v₂² + ...) |

### Dot Product Intuition

**u** · **v** = \|\|**u**\|\| \|\|**v**\|\| cos(θ)

- Measures similarity between directions
- = 0 when perpendicular (orthogonal)
- Positive when pointing same direction
- Negative when opposite

**ML application:** Similarity between embeddings.

### Unit Vectors

Vector with length 1. Direction without magnitude.

**Normalize:** **v** / \|\|**v**\|\|

---

## Matrices

### The Concept

2D array of numbers. Can represent:
- Linear transformations
- Systems of equations
- Data tables (rows = samples, columns = features)
- Weights in neural network layers

**Notation:** Capital letters (A, B, M). Element aᵢⱼ = row i, column j.

### Key Properties

| Property | Notation | Definition |
|----------|----------|------------|
| Shape | m × n | m rows, n columns |
| Transpose | Aᵀ | Swap rows and columns |
| Square | n × n | Same rows and columns |
| Identity | I | 1s on diagonal, 0s elsewhere |
| Symmetric | A = Aᵀ | |

### Matrix Multiplication

(A × B)ᵢⱼ = row i of A · column j of B

**Dimensions:** (m × n) · (n × p) = (m × p)

**Not commutative:** AB ≠ BA generally.

**ML context:** Layer output = activation(W · input + b)

### Matrix Operations

| Operation | What |
|-----------|------|
| A + B | Element-wise addition |
| cA | Scalar multiplication |
| AB | Matrix multiplication |
| Aᵀ | Transpose |
| A⁻¹ | Inverse (AA⁻¹ = I) |

---

## Linear Transformations

### Matrices as Transformations

Matrix multiplication transforms vectors:
- Rotation
- Scaling
- Reflection
- Shearing
- Projection

**Key insight:** Every linear transformation is a matrix. Every matrix is a linear transformation.

### Common Transformations (2D)

| Transform | Matrix |
|-----------|--------|
| Scale by k | [[k, 0], [0, k]] |
| Rotate by θ | [[cos θ, -sin θ], [sin θ, cos θ]] |
| Reflect x-axis | [[1, 0], [0, -1]] |

---

## Systems of Linear Equations

A**x** = **b**

- A = coefficient matrix
- **x** = unknown vector
- **b** = result vector

**Solutions:**
- Unique: A is invertible
- None: Inconsistent system
- Infinite: Underdetermined

---

## Eigenvalues and Eigenvectors

### The Concept

For matrix A, eigenvector **v** satisfies:

A**v** = λ**v**

- **v** = eigenvector (direction unchanged by transformation)
- λ = eigenvalue (scaling factor)

### Intuition

Eigenvectors are "special directions" where the transformation only stretches/shrinks, doesn't rotate.

### Applications

| Application | Use |
|-------------|-----|
| PCA | Eigenvectors of covariance matrix = principal components |
| PageRank | Dominant eigenvector of link matrix |
| Stability analysis | Eigenvalue signs determine stability |
| Vibration modes | Natural frequencies |

---

## Matrix Decompositions

### Eigendecomposition

A = VΛV⁻¹

- V = matrix of eigenvectors
- Λ = diagonal matrix of eigenvalues

Only works for square matrices with n independent eigenvectors.

### Singular Value Decomposition (SVD)

A = UΣVᵀ

- U = left singular vectors
- Σ = diagonal singular values
- V = right singular vectors

**Works for any matrix.** More general than eigendecomposition.

**Applications:**
- Dimensionality reduction
- Matrix completion (recommendations)
- Image compression
- Noise reduction

### QR Decomposition

A = QR

- Q = orthogonal matrix
- R = upper triangular

**Use:** Solving least squares, numerical stability.

---

## Key Concepts for ML

### Broadcasting

Extend operations to mismatched shapes (NumPy, PyTorch convention).

### Batched Operations

Process multiple samples simultaneously:
- Input: (batch_size, features)
- Weights: (features, outputs)
- Result: (batch_size, outputs)

### Norm Types

| Norm | Formula | Use |
|------|---------|-----|
| L1 | Σ\|xᵢ\| | Sparsity (Lasso) |
| L2 | √Σxᵢ² | Smoothness (Ridge) |
| Frobenius | √ΣΣaᵢⱼ² | Matrix "size" |

### Positive Definite

Matrix A is positive definite if **x**ᵀA**x** > 0 for all **x** ≠ 0.

**Why it matters:** Covariance matrices are positive semi-definite. Hessians determine convexity.

---

## Computational Considerations

### Efficiency

| Operation | Naive | Optimized |
|-----------|-------|-----------|
| Matrix multiply (n×n) | O(n³) | O(n^2.37) Strassen |
| Eigenvalues | O(n³) | |
| SVD | O(min(mn², m²n)) | |

### GPU Acceleration

Matrix operations parallelize well → GPUs excel at them → deep learning uses GPUs.

### Numerical Stability

- Avoid computing inverses explicitly
- Use decompositions (QR, SVD) instead
- Watch for near-singular matrices

---

## Related

- [[Calculus]]
- [[Probability & Statistics]]
- [[Neural Networks]]
- [[Dimensionality Reduction]]
