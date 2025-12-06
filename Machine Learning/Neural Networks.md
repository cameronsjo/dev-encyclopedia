---
title: Neural Networks
aliases:
  - Deep Learning Basics
  - ANNs
tags:
  - ml
  - deep-learning
  - neural-networks
type: concept
status: complete
difficulty: intermediate
created: '2025-11-28'
---

# Neural Networks

Computational models loosely inspired by biological neurons. Foundation of deep learning.

## The Basics

### What is a Neural Network?

Layers of connected nodes that transform inputs into outputs through learned weights.

```
Input → [Hidden Layers] → Output
  X    →   W₁, W₂, ...  →   ŷ
```

### Single Neuron

output = activation(Σ(wᵢxᵢ) + b)

- **Inputs (x):** Feature values
- **Weights (w):** Learned importance
- **Bias (b):** Learned offset
- **Activation:** Non-linear function

### Why Non-Linearity?

Without activation functions, stacked linear layers = one linear layer.

Non-linearity enables learning complex patterns.

---

## Architecture

### Layers

| Layer Type | What It Does |
|------------|--------------|
| Dense (Fully Connected) | Every input → every output |
| Convolutional | Local patterns, weight sharing |
| Recurrent | Sequential data, memory |
| Attention | Weighted combinations |
| Normalization | Stabilize activations |
| Dropout | Regularization |

### Network Depth

| Depth | Example | Capacity |
|-------|---------|----------|
| Shallow | 1-2 hidden layers | Simple patterns |
| Deep | 10+ layers | Complex hierarchies |
| Very deep | 100+ layers | State-of-the-art |

**Deeper = more expressive** but harder to train.

---

## Activation Functions

### Common Activations

| Function | Formula | Range | Use |
|----------|---------|-------|-----|
| ReLU | max(0, x) | [0, ∞) | Hidden layers (default) |
| Sigmoid | 1/(1+e⁻ˣ) | (0, 1) | Binary output |
| Tanh | (eˣ-e⁻ˣ)/(eˣ+e⁻ˣ) | (-1, 1) | Centered output |
| Softmax | eˣⁱ/Σeˣʲ | (0, 1), sums to 1 | Multi-class output |
| GELU | x·Φ(x) | (-0.17, ∞) | Transformers |

### ReLU Variants

| Variant | Fix | Formula |
|---------|-----|---------|
| Leaky ReLU | Dying neurons | max(0.01x, x) |
| ELU | Negative values | x if x>0, α(eˣ-1) otherwise |
| GELU | Smooth | x·Φ(x) |
| Swish | Smooth | x·sigmoid(x) |

---

## Training

### Forward Pass

Input flows through network → prediction.

### Loss Calculation

Compare prediction to ground truth.

| Task | Loss Function |
|------|---------------|
| Regression | MSE, MAE |
| Binary classification | Binary cross-entropy |
| Multi-class | Categorical cross-entropy |

### Backpropagation

Chain rule through computational graph.

1. Compute loss
2. Calculate gradients: ∂L/∂w for all weights
3. Update weights: w = w - α·∂L/∂w

**Key insight:** Backprop is efficient—O(n) not O(n²).

### Optimizers

| Optimizer | Idea | Use |
|-----------|------|-----|
| SGD | Basic gradient descent | Baseline |
| SGD + Momentum | Accumulate velocity | Faster convergence |
| Adam | Adaptive learning rates | Default choice |
| AdamW | Adam + weight decay | Transformers |
| LAMB | Layer-wise adaptive | Large batches |

**Adam is usually the safe choice** for getting started.

---

## Regularization

### Dropout

Randomly zero out neurons during training.

- Typical rate: 0.1-0.5
- Applied after activation
- Disabled during inference

### Batch Normalization

Normalize activations within mini-batch.

- Stabilizes training
- Allows higher learning rates
- Slight regularization effect

### Layer Normalization

Normalize across features (not batch).

- Used in transformers
- Works with small batches

### Weight Decay

Add penalty for large weights to loss.

L_total = L_task + λ·||w||²

---

## Common Architectures

### Feedforward (MLP)

Stack of dense layers.

Use: Tabular data, simple problems.

### Convolutional (CNN)

Spatial hierarchies via convolutions.

Use: Images, spatial data.

### Recurrent (RNN/LSTM/GRU)

Sequential processing with memory.

Use: Time series (mostly replaced by transformers).

### Transformer

Attention-based, parallel processing.

Use: NLP, vision, audio, almost everything now.

### Autoencoder

Encoder → Bottleneck → Decoder.

Use: Compression, denoising, representation learning.

---

## Practical Considerations

### Weight Initialization

| Method | Distribution | Use |
|--------|--------------|-----|
| Xavier/Glorot | N(0, 2/(n_in + n_out)) | Sigmoid, tanh |
| He | N(0, 2/n_in) | ReLU |
| Orthogonal | Orthogonal matrix | RNNs |

Bad initialization → vanishing/exploding gradients.

### Learning Rate

Most important hyperparameter.

| Issue | Symptom | Fix |
|-------|---------|-----|
| Too high | Loss explodes | Reduce LR |
| Too low | Very slow progress | Increase LR |

**Learning rate schedules:**

- Step decay
- Cosine annealing
- Warmup + decay

### Batch Size

| Size | Trade-off |
|------|-----------|
| Small (16-64) | More noise, better generalization |
| Large (256-4096) | Faster, may need LR scaling |

### Gradient Issues

| Problem | Symptom | Solutions |
|---------|---------|-----------|
| Vanishing | Early layers don't learn | Skip connections, better init, LSTM |
| Exploding | NaN loss | Gradient clipping, lower LR |

---

## Debugging Neural Networks

### Training Not Progressing

1. Check data pipeline (visualize samples)
2. Verify shapes match
3. Try overfitting single batch
4. Reduce model size
5. Check learning rate

### Loss Exploding

1. Lower learning rate
2. Add gradient clipping
3. Check for NaN in data
4. Better weight initialization

### Loss Stuck

1. Increase learning rate
2. Add more capacity
3. Check for data issues
4. Different initialization

---

## Frameworks

| Framework | Strengths |
|-----------|-----------|
| PyTorch | Research, flexibility |
| TensorFlow | Production, deployment |
| JAX | Functional, fast |
| Keras | Simplicity |

**PyTorch is dominant in research.** TensorFlow for production at scale.

---

## Related

- [[Backpropagation]]
- [[Convolutional Neural Networks]]
- [[LLMs & Transformers|Transformers]]
- [[Deep Learning]]
