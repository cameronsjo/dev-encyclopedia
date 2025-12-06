---
title: Calculus
aliases:
  - Differential Calculus
  - Integral Calculus
tags:
  - math
  - calculus
  - ml-prerequisite
type: concept
status: complete
difficulty: fundamentals
created: '2025-11-28'
---

# Calculus

The mathematics of change. Foundation for optimization, physics, and machine learning.

## Why It Matters for CS/ML

- **Optimization:** Gradient descent minimizes loss functions
- **Neural networks:** Backpropagation is chain rule applied
- **Physics simulations:** Motion, forces, transformations
- **Signal processing:** Continuous transformations

---

## Derivatives

### The Concept

The derivative measures **instantaneous rate of change**.

**Notation:**

| Form | Meaning |
|------|---------|
| f'(x) | Derivative of f |
| dy/dx | Change in y per change in x |
| ∂f/∂x | Partial derivative (multivariable) |
| ∇f | Gradient (vector of partials) |

### Intuition

- Slope of tangent line at a point
- How fast output changes as input changes
- Direction of steepest increase

### Key Rules

| Rule | Formula |
|------|---------|
| Power | d/dx(xⁿ) = nxⁿ⁻¹ |
| Constant | d/dx(c) = 0 |
| Sum | d/dx(f + g) = f' + g' |
| Product | d/dx(fg) = f'g + fg' |
| Quotient | d/dx(f/g) = (f'g - fg')/g² |
| Chain | d/dx(f(g(x))) = f'(g(x)) · g'(x) |

### Chain Rule (Critical for ML)

**When functions are composed:**
d/dx[f(g(x))] = f'(g(x)) · g'(x)

**Why it matters:** Backpropagation in neural networks is repeated chain rule.

If y = f(u) and u = g(x):
dy/dx = (dy/du) · (du/dx)

### Common Derivatives

| Function | Derivative |
|----------|------------|
| xⁿ | nxⁿ⁻¹ |
| eˣ | eˣ |
| ln(x) | 1/x |
| sin(x) | cos(x) |
| cos(x) | -sin(x) |
| aˣ | aˣ ln(a) |

---

## Partial Derivatives

### The Concept

For functions of multiple variables, partial derivative measures change in one variable while others held constant.

**Example:** f(x, y) = x² + xy + y²

∂f/∂x = 2x + y (treat y as constant)
∂f/∂y = x + 2y (treat x as constant)

### Gradient

**The gradient is the vector of all partial derivatives:**

∇f = [∂f/∂x₁, ∂f/∂x₂, ..., ∂f/∂xₙ]

**Key property:** Gradient points in direction of steepest increase.

**Gradient descent:** Move opposite to gradient to minimize function.

---

## Integrals

### The Concept

Integration is the inverse of differentiation. Also measures area under curve.

**Indefinite integral:** ∫f(x)dx = F(x) + C where F'(x) = f(x)

**Definite integral:** ∫ₐᵇf(x)dx = F(b) - F(a)

### Intuition

- Area under the curve
- Accumulation over interval
- Sum of infinitesimal pieces

### Key Rules

| Rule | Formula |
|------|---------|
| Power | ∫xⁿdx = xⁿ⁺¹/(n+1) + C |
| Constant | ∫c dx = cx + C |
| Sum | ∫(f + g)dx = ∫f dx + ∫g dx |
| Constant multiple | ∫cf dx = c∫f dx |

### Common Integrals

| Function | Integral |
|----------|----------|
| xⁿ | xⁿ⁺¹/(n+1) + C |
| eˣ | eˣ + C |
| 1/x | ln\|x\| + C |
| sin(x) | -cos(x) + C |
| cos(x) | sin(x) + C |

---

## Optimization

### Finding Extrema

**Critical points:** Where f'(x) = 0 or undefined.

**Second derivative test:**

- f''(x) > 0 → local minimum
- f''(x) < 0 → local maximum
- f''(x) = 0 → inconclusive

### Gradient Descent

**The ML workhorse:**

x_new = x_old - α · ∇f(x_old)

Where α = learning rate.

**Intuition:** Step opposite to gradient to decrease function value.

### Convexity

**Convex function:** "Bowl-shaped." Any local minimum is global minimum.

**Test:** f''(x) ≥ 0 everywhere (1D), or Hessian is positive semi-definite (multi-D).

**Why it matters:** Convex optimization has nice guarantees. Neural networks are non-convex (harder).

---

## Multivariable Calculus

### Hessian Matrix

Matrix of second partial derivatives:

```
H = | ∂²f/∂x² ∂²f/∂x∂y |
    | ∂²f/∂y∂x ∂²f/∂y² |
```

**Uses:**

- Second-order optimization (Newton's method)
- Characterizing critical points
- Curvature information

### Jacobian Matrix

Matrix of first partial derivatives for vector functions:

If f: Rⁿ → Rᵐ, Jacobian is m×n matrix.

**Uses:**

- Chain rule for vector functions
- Change of variables
- Backpropagation through layers

---

## Applications in ML

### Loss Function Minimization

Training = minimizing loss function.

L(θ) = loss as function of parameters θ

∇L(θ) tells us how to update parameters.

### Backpropagation

Chain rule through computation graph:

Output → Loss → ∂Loss/∂Output → ... → ∂Loss/∂Weights

Each step applies chain rule.

### Activation Functions

Need differentiable functions:

| Activation | Derivative |
|------------|------------|
| ReLU: max(0, x) | 1 if x > 0, else 0 |
| Sigmoid: σ(x) | σ(x)(1 - σ(x)) |
| Tanh: tanh(x) | 1 - tanh²(x) |

---

## Common Pitfalls

| Issue | Problem | Solution |
|-------|---------|----------|
| Vanishing gradients | Derivatives → 0 in deep networks | ReLU, skip connections |
| Exploding gradients | Derivatives → ∞ | Gradient clipping |
| Saddle points | Gradient = 0 but not extremum | Momentum, adaptive learning |
| Local minima | Stuck at non-global solution | Multiple initializations |

---

## Related

- [[Linear Algebra]]
- [[Probability & Statistics]]
- [[Neural Networks]]
- [[Backpropagation]]
