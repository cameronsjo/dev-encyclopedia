---
title: Fine-tuning
aliases:
  - Model Fine-tuning
  - LLM Fine-tuning
  - Parameter-Efficient Fine-Tuning
  - PEFT
tags:
  - ai
  - ml
  - llm
  - training
type: reference
status: complete
created: "2025-11-30"
---

# Fine-tuning

Adapting pre-trained models to specific tasks or domains by continuing training on targeted datasets.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Specialize general models for specific tasks, domains, or behaviors |
| **Scope** | Full model retraining to parameter-efficient methods (LoRA, adapters) |
| **Data Requirements** | 100s to 10,000s of examples (task-dependent) |
| **Compute** | Ranges from consumer GPU (PEFT) to datacenter clusters (full) |
| **Key Tradeoff** | Model specialization vs generalization retention |
| **Main Risk** | Catastrophic forgetting of original capabilities |

## When to Fine-tune

### Decision Framework

| Approach | Best For | Limitations |
|----------|----------|-------------|
| **Prompting** | ✅ General tasks<br>✅ Quick iterations<br>✅ No training data | ❌ Inconsistent formatting<br>❌ Limited context window<br>❌ Per-request cost |
| **RAG** | ✅ Knowledge retrieval<br>✅ Dynamic information<br>✅ Explainable sources | ❌ Not for behavior/style<br>❌ Retrieval overhead<br>❌ Quality depends on chunks |
| **Fine-tuning** | ✅ Consistent behavior<br>✅ Domain expertise<br>✅ Format compliance | ❌ Requires training data<br>❌ Compute intensive<br>❌ Static knowledge cutoff |

### Use Cases for Fine-tuning

**Strong candidates:**

- Instruction following in specific formats (JSON, SQL, code)
- Domain-specific terminology and reasoning (medical, legal, technical)
- Tone and style consistency (customer service, brand voice)
- Task-specific behavior (summarization, classification, extraction)
- Reducing hallucinations on known domains

**Poor candidates:**

- Adding recent factual knowledge (use RAG)
- One-off tasks or experiments (use prompting)
- Tasks with <100 quality examples
- General capabilities improvement

## Fine-tuning Methods

### Full Fine-tuning

**Description:** Update all model parameters during training.

**Characteristics:**

- Highest quality potential
- Maximum memory requirements (model weights + gradients + optimizer states)
- Risk of catastrophic forgetting
- Requires largest datasets for stability

**Typical use:** Domain adaptation for production systems with significant resources.

### LoRA (Low-Rank Adaptation)

**Description:** Train small rank decomposition matrices injected into each layer while freezing original weights.

**Mathematics:** For weight matrix W, learn ΔW = BA where B and A are low-rank (r << d).

**Characteristics:**

- 90%+ memory reduction vs full fine-tuning
- Trainable parameters: typically 0.1-1% of model size
- Minimal quality degradation
- Fast training and inference
- Multiple adapters can be swapped at runtime

**Typical use:** Most common choice for LLM fine-tuning on consumer hardware.

### QLoRA (Quantized LoRA)

**Description:** LoRA with base model quantized to 4-bit precision.

**Characteristics:**

- Further 75% memory reduction vs LoRA
- Enables fine-tuning 70B models on single 48GB GPU
- Minimal quality loss vs full-precision LoRA
- Slight training speed decrease

**Typical use:** Fine-tuning large models on limited hardware.

### Adapter Layers

**Description:** Insert small trainable modules between frozen transformer layers.

**Characteristics:**

- Similar memory savings to LoRA
- Slightly more inference overhead
- Can be stacked or composed
- Earlier approach, less popular than LoRA

**Typical use:** Legacy systems, research on composable modules.

### Prefix Tuning / Soft Prompts

**Description:** Learn continuous task-specific vectors prepended to input embeddings.

**Characteristics:**

- Smallest number of parameters (10K-100K)
- Fastest training
- No model architecture changes
- Limited expressiveness vs LoRA

**Typical use:** Simple task adaptation, multi-task scenarios.

## Method Comparison

| Method | Trainable Params | Memory (7B model) | Quality | Inference Speed | Use Case |
|--------|------------------|-------------------|---------|-----------------|----------|
| **Full Fine-tune** | 100% (7B) | ~80GB | ✅ Best | ✅ Fast | Production, unlimited resources |
| **LoRA** | 0.1-1% (7-70M) | ~12GB | ✅ Excellent | ✅ Fast | Most common choice |
| **QLoRA** | 0.1-1% (7-70M) | ~6GB | ✅ Excellent | ⚠️ Moderate | Limited GPU memory |
| **Adapters** | 1-5% (70-350M) | ~15GB | ✅ Very Good | ⚠️ Moderate | Multi-adapter scenarios |
| **Prefix Tuning** | <0.01% (<1M) | ~10GB | ⚠️ Good | ✅ Fast | Simple tasks, experiments |

_Memory estimates for training 7B parameter model (FP16/BF16)_

## Dataset Preparation

### Data Quality Principles

**Priority:** Quality over quantity. 500 excellent examples > 5,000 mediocre ones.

**Required elements:**

- Consistent format across all examples
- Representative of target task distribution
- Diverse enough to prevent overfitting
- Clean, human-validated outputs
- Balanced classes/categories if applicable

### Dataset Size Guidelines

| Task Complexity | Minimum Examples | Recommended |
|----------------|------------------|-------------|
| Simple classification | 100-500 | 1,000+ |
| Instruction following | 500-1,000 | 5,000+ |
| Complex reasoning | 1,000-5,000 | 10,000+ |
| Domain adaptation | 5,000-10,000 | 50,000+ |

### Data Format

**Instruction format (most common):**

```json
{
  "instruction": "Classify the sentiment of this review",
  "input": "This product exceeded my expectations!",
  "output": "Positive"
}
```

**Chat format:**

```json
{
  "messages": [
    {"role": "system", "content": "You are a helpful assistant"},
    {"role": "user", "content": "What is the capital of France?"},
    {"role": "assistant", "content": "The capital of France is Paris."}
  ]
}
```

### Train/Validation Split

- **Standard:** 80% train, 20% validation
- **Large datasets:** 90% train, 10% validation
- **Small datasets:** Consider k-fold cross-validation
- **Ensure:** Splits are stratified for classification tasks

## Training Configuration

### Critical Hyperparameters

| Parameter | Typical Range | Impact | Tuning Priority |
|-----------|--------------|--------|-----------------|
| **Learning Rate** | 1e-5 to 5e-4 | Convergence speed, stability | ✅ High |
| **Batch Size** | 4-32 (effective) | Training stability, memory | ✅ High |
| **Epochs** | 3-10 | Overfitting vs underfitting | ✅ High |
| **LoRA Rank (r)** | 8-64 | Expressiveness, memory | ⚠️ Medium |
| **LoRA Alpha** | 16-128 | Scaling of adapter weights | ⚠️ Medium |
| **Weight Decay** | 0-0.1 | Regularization | ⚠️ Low |
| **Warmup Steps** | 10-500 | Training stability | ⚠️ Low |

### Learning Rate Strategies

**Conservative (recommended starting point):**

- Full fine-tune: 1e-5 to 5e-5
- LoRA: 1e-4 to 3e-4
- Use linear warmup (10% of total steps)
- Cosine decay to 10% of peak

**Aggressive (for quick experiments):**

- 2-5x higher learning rates
- Shorter warmup
- Higher risk of instability

### Batch Size Considerations

**Effective batch size** = per_device_batch_size × gradient_accumulation_steps × num_gpus

- Larger batches: More stable, better generalization, slower iteration
- Smaller batches: Faster iteration, more noise, regularization effect
- Memory-limited: Use gradient accumulation to simulate larger batches

## Catastrophic Forgetting

**Problem:** Fine-tuned models lose general capabilities from pre-training.

### Mitigation Strategies

| Strategy | Description | Effectiveness | Cost |
|----------|-------------|---------------|------|
| **Replay Buffer** | Mix 10-20% pre-training data into fine-tuning | ✅ Excellent | High (requires pre-train data) |
| **Regularization** | L2 penalty on weight changes from base model | ⚠️ Moderate | Low |
| **LoRA/PEFT** | Train adapters, keep base frozen | ✅ Excellent | Low |
| **Smaller Learning Rate** | Reduce magnitude of weight updates | ⚠️ Moderate | Low |
| **Early Stopping** | Stop before overfitting to task | ⚠️ Moderate | Low |
| **Curriculum Learning** | Gradually increase task-specific ratio | ✅ Good | Medium |

**Best practice:** Use LoRA/PEFT methods by default to preserve base model capabilities.

## Evaluation

### During Training

**Monitor these metrics:**

- Training loss (should decrease smoothly)
- Validation loss (should track training, watch for divergence)
- Learning rate schedule
- Gradient norms (watch for explosions)

**Warning signs:**

- Validation loss increasing while training decreases (overfitting)
- Loss plateaus early (learning rate too low, data insufficient)
- NaN or exploding gradients (learning rate too high, numerical instability)

### Post-Training

**Task-specific metrics:**

- Classification: Accuracy, F1, precision, recall
- Generation: BLEU, ROUGE, perplexity
- Instruction following: Human evaluation, GPT-4 as judge

**General capability preservation:**

- Run base model benchmarks (MMLU, HellaSwag, etc.)
- Compare pre/post fine-tuning scores
- Test edge cases and out-of-distribution inputs

**A/B testing:**

- Deploy alongside base model
- Measure task success rates
- Monitor hallucination rates
- Track user satisfaction metrics

## Common Pitfalls

| Issue | Symptoms | Solution |
|-------|----------|----------|
| **Overfitting** | Perfect train, poor validation | More data, regularization, early stopping |
| **Underfitting** | Poor train and validation | More epochs, higher LR, larger LoRA rank |
| **Data Leakage** | Unrealistic validation scores | Check train/val split, remove duplicates |
| **Format Issues** | Model ignores instructions | Validate data format, check tokenization |
| **OOM Errors** | Training crashes | Reduce batch size, use gradient accumulation, QLoRA |
| **Slow Convergence** | Loss decreases too slowly | Increase learning rate, check data quality |
| **Catastrophic Forgetting** | Task works, general ability lost | Use PEFT, add replay data, reduce epochs |

## Tools and Frameworks

### Training Frameworks

**Hugging Face Transformers + PEFT:**

- Most popular, extensive model support
- Easy LoRA/QLoRA integration
- SFTTrainer for supervised fine-tuning

**Axolotl:**

- Configuration-driven fine-tuning
- Optimized for common workflows
- Built on Transformers/PEFT

**LLaMA Factory:**

- Web UI for fine-tuning
- Supports multiple methods
- Good for beginners

**OpenAI Fine-tuning API:**

- Managed service for GPT models
- No infrastructure management
- Limited customization

### Quantization Libraries

**bitsandbytes:** 4-bit/8-bit quantization for QLoRA
**GPTQ:** Post-training quantization for inference
**AWQ:** Activation-aware weight quantization

## Advanced Topics

### Multi-Task Fine-tuning

Train on multiple tasks simultaneously to improve generalization.

**Approaches:**

- Task-specific prefixes or adapters
- Weighted loss across tasks
- Curriculum learning (simple → complex)

### RLHF (Reinforcement Learning from Human Feedback)

Further align models with human preferences after supervised fine-tuning.

**Process:**

1. Supervised fine-tuning (SFT) on demonstrations
2. Train reward model on preference rankings
3. Optimize policy with PPO/DPO

**Use case:** Chatbots, alignment, safety

### Continual Learning

Sequentially fine-tune on new tasks without forgetting previous ones.

**Techniques:**

- Elastic Weight Consolidation (EWC)
- Progressive adapter addition
- Memory replay buffers

## Related

- [[RAG]] - Alternative to fine-tuning for knowledge injection
- [[LLMs & Transformers]] - Foundation models that are fine-tuned
- [[Model Serving]] - Deploying fine-tuned models in production
- [[Prompt Engineering]] - Lighter-weight alternative to fine-tuning
- [[Neural Networks]] - Underlying architecture being fine-tuned
