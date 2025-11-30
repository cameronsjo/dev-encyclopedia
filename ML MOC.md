---
title: ML MOC
tags:
  - moc
  - ml
  - ai
  - machine-learning
type: moc
created: 2025-11-28
---

# Machine Learning & AI MOC

Machine learning, deep learning, and artificial intelligence concepts.

## Foundations

- [[ML Fundamentals]]
- [[Feature Engineering]]
- [[Model Evaluation]]
- [[Bias-Variance Tradeoff]]
- [[Cross-Validation]]

## Learning Paradigms

### Supervised Learning

- [[Linear Regression]]
- [[Logistic Regression]]
- [[Decision Trees]]
- [[Random Forests]]
- [[Support Vector Machines]]
- [[Gradient Boosting]]

### Unsupervised Learning

- [[Clustering]]
- [[Dimensionality Reduction]]
- [[Anomaly Detection]]
- [[Association Rules]]

### Reinforcement Learning

- [[RL Fundamentals]]
- [[Q-Learning]]
- [[Policy Gradients]]
- [[Actor-Critic Methods]]

## Deep Learning

### Architectures

- [[Neural Network Basics]]
- [[Convolutional Neural Networks]]
- [[Recurrent Neural Networks]]
- [[Transformers]]
- [[Autoencoders]]
- [[GANs]]

### Training

- [[Backpropagation]]
- [[Optimizers]]
- [[Regularization]]
- [[Batch Normalization]]
- [[Transfer Learning]]

## LLMs & Agents

- [[LLMs & Transformers]]
- [[LLM Internals]]
- [[RAG]]
- [[Grounding]]
- [[Agent Frameworks]]
- [[MCP Servers]]
- [[MCP Gateway]]
- [[Agent Gateway]]
- [[Prompt Engineering]]

## Applications

### NLP

- [[Text Preprocessing]]
- [[Word Embeddings]]
- [[Sequence Models]]
- [[Attention Mechanisms]]

### Computer Vision

- [[Image Classification]]
- [[Object Detection]]
- [[Semantic Segmentation]]
- [[Image Generation]]

---

## All ML Pages

```dataview
TABLE status, difficulty as "Level", join(tags, ", ") as "Tags"
FROM #ml OR #ai
WHERE type != "moc"
SORT file.name ASC
```

## Prerequisites

```dataview
LIST
FROM #ml-prerequisite
```
