---
title: Probability & Statistics
aliases:
  - Statistics
  - Probability Theory
tags:
  - math
  - probability
  - statistics
  - ml-prerequisite
type: concept
status: complete
difficulty: fundamentals
created: 2025-11-28
---

# Probability & Statistics

The mathematics of uncertainty. Foundation for machine learning, inference, and data analysis.

## Why It Matters for CS/ML

- **ML models:** Probabilistic interpretation of predictions
- **Generative models:** Sampling from distributions
- **Bayesian methods:** Prior knowledge + data → posterior
- **A/B testing:** Statistical significance
- **Data analysis:** Understanding patterns, variance

---

## Probability Fundamentals

### Basic Concepts

| Term | Definition |
|------|------------|
| Sample space (Ω) | All possible outcomes |
| Event | Subset of sample space |
| P(A) | Probability of event A (0 to 1) |
| P(A\|B) | Probability of A given B |

### Axioms

1. P(A) ≥ 0
2. P(Ω) = 1
3. P(A ∪ B) = P(A) + P(B) if A and B are mutually exclusive

### Key Formulas

| Formula | Equation |
|---------|----------|
| Complement | P(A') = 1 - P(A) |
| Union | P(A ∪ B) = P(A) + P(B) - P(A ∩ B) |
| Conditional | P(A\|B) = P(A ∩ B) / P(B) |
| Independence | P(A ∩ B) = P(A) · P(B) |

### Bayes' Theorem

P(A|B) = P(B|A) · P(A) / P(B)

**Interpretation:**
- P(A) = prior (belief before evidence)
- P(B|A) = likelihood (probability of evidence given hypothesis)
- P(A|B) = posterior (updated belief after evidence)

**ML application:** Naive Bayes, Bayesian inference, probabilistic models.

---

## Random Variables

### Types

| Type | Values | Examples |
|------|--------|----------|
| Discrete | Countable | Coin flips, dice rolls |
| Continuous | Any real number | Height, temperature |

### Probability Distributions

**Discrete:** Probability mass function (PMF)
P(X = x)

**Continuous:** Probability density function (PDF)
f(x), where P(a ≤ X ≤ b) = ∫ₐᵇ f(x)dx

### Cumulative Distribution Function (CDF)

F(x) = P(X ≤ x)

Works for both discrete and continuous.

---

## Common Distributions

### Discrete

| Distribution | Parameters | Use Case |
|--------------|------------|----------|
| Bernoulli | p (success prob) | Single yes/no |
| Binomial | n, p | Count of successes in n trials |
| Poisson | λ (rate) | Count of rare events |
| Categorical | p₁...pₖ | One of k outcomes |

### Continuous

| Distribution | Parameters | Use Case |
|--------------|------------|----------|
| Uniform | a, b (bounds) | Equal probability in range |
| Normal (Gaussian) | μ, σ² | Natural phenomena, errors |
| Exponential | λ (rate) | Time between events |
| Beta | α, β | Probabilities |

### The Normal Distribution

**The most important distribution.**

f(x) = (1/σ√2π) exp(-(x-μ)²/2σ²)

**Properties:**
- Bell-shaped, symmetric
- Mean = median = mode = μ
- 68% within 1σ, 95% within 2σ, 99.7% within 3σ

**Why it's everywhere:** Central Limit Theorem—averages of many things become normal.

---

## Descriptive Statistics

### Central Tendency

| Measure | What | When to Use |
|---------|------|-------------|
| Mean | Average | Symmetric data |
| Median | Middle value | Skewed data, outliers |
| Mode | Most frequent | Categorical data |

### Spread

| Measure | Formula | What |
|---------|---------|------|
| Variance | E[(X - μ)²] | Average squared deviation |
| Std Dev | √Variance | Same units as data |
| Range | Max - Min | Simplest measure |
| IQR | Q3 - Q1 | Robust to outliers |

### Shape

| Property | What |
|----------|------|
| Skewness | Asymmetry (positive = right tail) |
| Kurtosis | Tail heaviness vs. normal |

---

## Expected Value and Variance

### Expected Value

E[X] = Σ x · P(X = x) (discrete)
E[X] = ∫ x · f(x) dx (continuous)

**Properties:**
- E[aX + b] = aE[X] + b
- E[X + Y] = E[X] + E[Y]

### Variance

Var(X) = E[(X - μ)²] = E[X²] - (E[X])²

**Properties:**
- Var(aX + b) = a²Var(X)
- Var(X + Y) = Var(X) + Var(Y) (if independent)

### Covariance and Correlation

**Covariance:** Measures how two variables vary together.
Cov(X, Y) = E[(X - μₓ)(Y - μᵧ)]

**Correlation:** Normalized covariance (-1 to 1).
ρ = Cov(X, Y) / (σₓσᵧ)

- ρ = 1: Perfect positive relationship
- ρ = 0: No linear relationship
- ρ = -1: Perfect negative relationship

---

## Statistical Inference

### Point Estimation

Estimate population parameter from sample.

| Parameter | Estimator |
|-----------|-----------|
| Mean μ | Sample mean x̄ |
| Variance σ² | Sample variance s² |
| Proportion p | Sample proportion p̂ |

### Confidence Intervals

Range likely to contain true parameter.

**95% CI for mean:** x̄ ± 1.96 · (s/√n)

**Interpretation:** 95% of such intervals contain the true mean. (Not: 95% chance this interval contains the mean.)

### Hypothesis Testing

1. State null hypothesis H₀
2. Choose significance level α (typically 0.05)
3. Calculate test statistic
4. Find p-value
5. Reject H₀ if p < α

**p-value:** Probability of seeing data this extreme if H₀ is true.

### Common Tests

| Test | Use Case |
|------|----------|
| t-test | Compare means |
| Chi-squared | Categorical data, goodness of fit |
| ANOVA | Compare multiple means |
| A/B test | Compare two treatments |

---

## ML Applications

### Maximum Likelihood Estimation (MLE)

Find parameters that maximize probability of observed data.

θ_MLE = argmax P(data | θ)

**Example:** For normal distribution, MLE for μ is sample mean.

### Cross-Entropy Loss

Measures difference between predicted and true distributions.

H(p, q) = -Σ p(x) log q(x)

Used in classification: true distribution is one-hot, predicted is softmax.

### Information Theory

| Concept | Formula | Meaning |
|---------|---------|---------|
| Entropy | -Σ p log p | Uncertainty/randomness |
| KL Divergence | Σ p log(p/q) | Distance between distributions |
| Mutual Information | I(X;Y) | Shared information |

### Regularization as Prior

L2 regularization = Gaussian prior on weights.
L1 regularization = Laplace prior on weights.

---

## Common Pitfalls

| Pitfall | Problem | Fix |
|---------|---------|-----|
| Correlation ≠ causation | Spurious relationships | Controlled experiments |
| p-hacking | Testing until significant | Pre-register hypotheses |
| Survivorship bias | Only see successes | Consider missing data |
| Simpson's paradox | Aggregation reverses trend | Stratify analysis |
| Base rate neglect | Ignoring prior probability | Use Bayes' theorem |

---

## Related

- [[Linear Algebra]]
- [[Calculus]]
- [[ML Fundamentals]]
- [[Bayesian Statistics]]
