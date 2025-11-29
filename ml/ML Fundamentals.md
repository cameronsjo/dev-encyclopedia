---
title: ML Fundamentals
aliases:
  - Machine Learning Basics
  - ML Basics
tags:
  - ml
  - fundamentals
type: concept
status: complete
difficulty: fundamentals
created: 2025-11-28
---

# ML Fundamentals

Core concepts in machine learning: how machines learn from data.

## What is Machine Learning?

**Traditional programming:** Rules + Data → Answers
**Machine learning:** Data + Answers → Rules

ML systems learn patterns from examples rather than being explicitly programmed.

---

## Learning Paradigms

| Paradigm | Data | Goal | Examples |
|----------|------|------|----------|
| **Supervised** | Labeled (X, y) | Predict y from X | Classification, regression |
| **Unsupervised** | Unlabeled (X) | Find structure | Clustering, dimensionality reduction |
| **Reinforcement** | Actions + rewards | Maximize reward | Games, robotics |
| **Self-supervised** | Unlabeled, create labels | Learn representations | LLMs, BERT |

---

## Supervised Learning

### Classification

**Goal:** Predict discrete categories.

| Type | Output | Example |
|------|--------|---------|
| Binary | 0 or 1 | Spam detection |
| Multi-class | One of k | Image classification |
| Multi-label | Multiple of k | Article tags |

### Regression

**Goal:** Predict continuous values.

Examples: House prices, temperature, stock prices.

### The Process

1. **Collect data** — Features (X) and labels (y)
2. **Split data** — Train/validation/test
3. **Choose model** — Based on problem type
4. **Train** — Minimize loss on training data
5. **Validate** — Tune hyperparameters
6. **Test** — Final evaluation on held-out data
7. **Deploy** — Serve predictions

---

## Key Concepts

### Features and Labels

- **Features (X):** Input variables (predictors, attributes)
- **Labels (y):** Output variable (target, response)
- **Sample:** One data point (row)
- **Feature vector:** All features for one sample

### Loss Functions

Measure how wrong predictions are.

| Loss | Use | Formula |
|------|-----|---------|
| MSE | Regression | (1/n)Σ(y - ŷ)² |
| MAE | Regression (robust) | (1/n)Σ\|y - ŷ\| |
| Cross-entropy | Classification | -Σy log(ŷ) |
| Hinge | SVM | max(0, 1 - y·ŷ) |

### Optimization

**Goal:** Find parameters that minimize loss.

**Gradient descent:** Move opposite to gradient.
θ = θ - α · ∇L(θ)

| Variant | Batch Size | Use |
|---------|------------|-----|
| Batch GD | All data | Small datasets |
| Stochastic GD | 1 sample | Online learning |
| Mini-batch GD | n samples | Standard practice |

---

## Bias-Variance Tradeoff

### The Problem

| Error Type | Cause | Symptom |
|------------|-------|---------|
| **Bias** | Model too simple | Underfitting |
| **Variance** | Model too complex | Overfitting |

**Total Error = Bias² + Variance + Irreducible Error**

### Underfitting

- High training error
- High validation error
- Model can't capture patterns

**Fixes:** More features, more complex model, less regularization.

### Overfitting

- Low training error
- High validation error
- Model memorized training data

**Fixes:** More data, regularization, simpler model, dropout.

```
Error
  ^
  |    \
  |     \    Validation Error
  |      \  /
  |       \/
  |       /\
  |      /  \
  |     /    Training Error
  +-----|----|----|--------> Model Complexity
    Underfit  |  Overfit
              |
           Sweet spot
```

---

## Data Splitting

| Set | Purpose | Typical % |
|-----|---------|-----------|
| Training | Learn parameters | 60-80% |
| Validation | Tune hyperparameters | 10-20% |
| Test | Final evaluation | 10-20% |

**Golden rule:** Never touch test set during development.

### Cross-Validation

When data is limited:

| Method | How |
|--------|-----|
| K-fold | Split into k parts, rotate test set |
| Leave-one-out | K-fold where k = n |
| Stratified | Preserve class distribution |

---

## Regularization

Prevent overfitting by penalizing model complexity.

### L1 (Lasso)

Loss + λΣ|wᵢ|

- Produces sparse weights (some → 0)
- Feature selection

### L2 (Ridge)

Loss + λΣwᵢ²

- Shrinks all weights toward 0
- More stable than L1

### Dropout (Neural Networks)

Randomly zero out neurons during training.

- Forces redundant representations
- Approximates ensemble

---

## Evaluation Metrics

### Classification

| Metric | Formula | When |
|--------|---------|------|
| Accuracy | Correct / Total | Balanced classes |
| Precision | TP / (TP + FP) | Cost of false positives |
| Recall | TP / (TP + FN) | Cost of false negatives |
| F1 | 2·(P·R)/(P+R) | Balance P and R |
| AUC-ROC | Area under ROC | Ranking quality |

### Confusion Matrix

```
              Predicted
              Neg    Pos
Actual  Neg   TN     FP
        Pos   FN     TP
```

### Regression

| Metric | Formula | Notes |
|--------|---------|-------|
| MSE | Mean squared error | Penalizes large errors |
| RMSE | √MSE | Same units as target |
| MAE | Mean absolute error | Robust to outliers |
| R² | 1 - (SS_res/SS_tot) | Explained variance |

---

## Hyperparameters

Parameters set before training:

| Type | Examples |
|------|----------|
| Model architecture | # layers, # neurons |
| Training | Learning rate, batch size |
| Regularization | λ, dropout rate |

### Tuning Methods

| Method | How |
|--------|-----|
| Grid search | Try all combinations |
| Random search | Random combinations |
| Bayesian optimization | Model hyperparameter space |

**Random search usually beats grid search** (Bergstra & Bengio).

---

## Feature Engineering

Transforming raw data into useful inputs.

### Common Techniques

| Technique | What | When |
|-----------|------|------|
| Normalization | Scale to [0,1] | Different scales |
| Standardization | Zero mean, unit variance | Many algorithms |
| One-hot encoding | Binary columns for categories | Categorical features |
| Binning | Discretize continuous | Non-linear relationships |
| Log transform | log(x) | Skewed distributions |
| Polynomial features | x², x·y | Non-linear patterns |

### Missing Data

| Strategy | When |
|----------|------|
| Remove rows | Few missing |
| Impute (mean/median) | Random missing |
| Indicator variable | Missingness is informative |
| Model-based imputation | Complex relationships |

---

## Common Pitfalls

| Pitfall | Problem | Fix |
|---------|---------|-----|
| Data leakage | Test info in training | Careful feature engineering |
| Target leakage | Future info in features | Temporal validation |
| Imbalanced classes | Model ignores minority | Resampling, class weights |
| Distribution shift | Train ≠ production data | Monitor, retrain |
| Wrong metric | Optimizing wrong thing | Align metric with goal |

---

## Related

- [[Supervised Learning]]
- [[Neural Networks]]
- [[Feature Engineering]]
- [[Model Evaluation]]
