---
title: GPUs
aliases:
  - Graphics Processing Unit
  - Graphics Cards
  - Accelerators
  - GPU Computing
tags:
  - hardware
  - gpu
  - compute
  - ml
  - gaming
type: reference
status: complete
created: "2025-12-16"
---

# GPUs

Massively parallel processors originally designed for graphics, now essential for machine learning, scientific computing, and high-performance workloads.

## Overview

| Aspect | Details |
|--------|---------|
| **Primary Vendors** | NVIDIA, AMD, Intel |
| **Core Architecture** | Thousands of small cores (SIMT) |
| **Primary Use Cases** | Graphics, ML/AI, HPC, crypto |
| **Memory Type** | GDDR6, HBM2e, HBM3 |
| **Key Advantage** | Parallel computation |

## Architecture

### GPU vs CPU

| Aspect | CPU | GPU |
|--------|-----|-----|
| **Cores** | Few (4-64) powerful cores | Thousands of simple cores |
| **Optimization** | Low latency | High throughput |
| **Task Type** | Complex, sequential | Simple, parallel |
| **Cache** | Large | Smaller |
| **Control** | Complex branch prediction | Simpler control flow |

### Execution Model

**SIMT (Single Instruction, Multiple Threads):**

```
┌────────────────────────────────────────┐
│           Streaming Multiprocessor     │
│  ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐   │
│  │Core│ │Core│ │Core│ │Core│ │Core│   │
│  └────┘ └────┘ └────┘ └────┘ └────┘   │
│  ┌────┐ ┌────┐ ┌────┐ ┌────┐ ┌────┐   │
│  │Core│ │Core│ │Core│ │Core│ │Core│   │
│  └────┘ └────┘ └────┘ └────┘ └────┘   │
│           ... (32-128 cores)          │
│  ┌─────────────────────────────────┐  │
│  │        Shared Memory/L1        │   │
│  └─────────────────────────────────┘  │
└────────────────────────────────────────┘
        × 80-144 SMs per GPU
```

### Memory Hierarchy

| Level | Size | Latency |
|-------|------|---------|
| **Registers** | Per-thread | 1 cycle |
| **Shared Memory** | Per SM (64-228 KB) | ~20 cycles |
| **L2 Cache** | Shared (6-96 MB) | ~200 cycles |
| **Global Memory (VRAM)** | 8-80 GB | ~400 cycles |

## NVIDIA Ecosystem

### Current Architectures

| Architecture | Year | Key Features |
|--------------|------|--------------|
| **Hopper** | 2022 | H100, Transformer Engine, HBM3 |
| **Ada Lovelace** | 2022 | RTX 40 series, DLSS 3 |
| **Ampere** | 2020 | A100, 3rd gen Tensor Cores |
| **Blackwell** | 2024 | B100/B200, next-gen AI |

### Product Lines

| Line | Target | Examples |
|------|--------|----------|
| **GeForce** | Gaming/Consumer | RTX 4090, 4080, 4070 |
| **RTX Professional** | Workstation | RTX 6000, RTX 5000 |
| **Data Center** | AI/HPC | H100, A100, H200 |
| **Jetson** | Edge AI | Orin, Xavier |

### Key Technologies

| Technology | Purpose |
|------------|---------|
| **CUDA Cores** | General-purpose compute |
| **Tensor Cores** | Matrix operations (AI) |
| **RT Cores** | Ray tracing acceleration |
| **NVLink** | GPU-to-GPU interconnect |
| **NVSwitch** | Multi-GPU fabric |

### CUDA Programming

**Dominant GPU computing platform.**

```cuda
__global__ void vectorAdd(float *a, float *b, float *c, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) {
        c[i] = a[i] + b[i];
    }
}

// Launch kernel
vectorAdd<<<numBlocks, threadsPerBlock>>>(a, b, c, n);
```

**CUDA Concepts:**

| Concept | Description |
|---------|-------------|
| **Kernel** | Function running on GPU |
| **Thread** | Single execution unit |
| **Block** | Group of threads (share memory) |
| **Grid** | Collection of blocks |
| **Warp** | 32 threads executing together |

## AMD Ecosystem

### Current Architectures

| Architecture | Year | Key Features |
|--------------|------|--------------|
| **CDNA 3** | 2023 | MI300X, unified memory |
| **RDNA 3** | 2022 | RX 7000 series, chiplets |
| **CDNA 2** | 2021 | MI250X, exascale HPC |

### Product Lines

| Line | Target | Examples |
|------|--------|----------|
| **Radeon RX** | Gaming | RX 7900 XTX, 7800 XT |
| **Radeon PRO** | Workstation | PRO W7900, W7800 |
| **Instinct** | Data Center | MI300X, MI300A |

### ROCm Platform

**AMD's open-source GPU compute stack.**

```cpp
// HIP (CUDA-like API)
__global__ void vectorAdd(float *a, float *b, float *c, int n) {
    int i = hipBlockIdx_x * hipBlockDim_x + hipThreadIdx_x;
    if (i < n) {
        c[i] = a[i] + b[i];
    }
}

hipLaunchKernelGGL(vectorAdd, numBlocks, threadsPerBlock, 0, 0, a, b, c, n);
```

| Component | Purpose |
|-----------|---------|
| **HIP** | CUDA-like programming API |
| **rocBLAS** | Linear algebra |
| **MIOpen** | Deep learning primitives |
| **rocFFT** | FFT library |

## Intel GPUs

### Arc Graphics

**Discrete gaming GPUs:**

| Architecture | Year | Products |
|--------------|------|----------|
| **Battlemage** | 2024 | Arc B-series |
| **Alchemist** | 2022 | Arc A770, A750, A380 |

### Data Center GPUs

| Product | Target |
|---------|--------|
| **Max Series (Ponte Vecchio)** | HPC/AI (Aurora supercomputer) |
| **Flex Series** | Cloud gaming, media |

### oneAPI

**Cross-architecture programming model.**

```cpp
// SYCL (oneAPI)
q.submit([&](handler &h) {
    h.parallel_for(range<1>(n), [=](id<1> i) {
        c[i] = a[i] + b[i];
    });
});
```

## Memory Technologies

| Type | Bandwidth | Capacity | Use Case |
|------|-----------|----------|----------|
| **GDDR6** | ~900 GB/s | 8-24 GB | Gaming |
| **GDDR6X** | ~1 TB/s | 16-24 GB | High-end gaming |
| **HBM2e** | ~2 TB/s | 40-80 GB | Data center |
| **HBM3** | ~3+ TB/s | 80+ GB | AI accelerators |

### Memory Bandwidth Importance

**AI/ML is memory-bound:**

- Large model weights
- Batch processing
- Tensor operations

Higher bandwidth = faster training/inference.

## AI/ML Hardware

### Training GPUs

| GPU | Memory | FP16 TFLOPs | Use Case |
|-----|--------|-------------|----------|
| **H100 SXM** | 80 GB HBM3 | 1,979 | LLM training |
| **H200** | 141 GB HBM3e | 1,979 | Large models |
| **MI300X** | 192 GB HBM3 | 1,307 | AMD alternative |
| **A100** | 40/80 GB HBM2e | 312 | Previous gen |

### Inference Accelerators

| Accelerator | Vendor | Notes |
|-------------|--------|-------|
| **L4** | NVIDIA | Efficient inference |
| **L40S** | NVIDIA | Generative AI |
| **Inferentia** | AWS | Custom ASIC |
| **TPU** | Google | Tensor Processing Unit |
| **Trainium** | AWS | Training ASIC |

### Multi-GPU Systems

| Configuration | Description |
|---------------|-------------|
| **NVLink** | GPU-to-GPU (900 GB/s on H100) |
| **NVSwitch** | All-to-all GPU fabric |
| **DGX H100** | 8x H100 system |
| **HGX** | Reference multi-GPU platform |

## Software Ecosystem

### Frameworks

| Framework | GPU Support |
|-----------|-------------|
| **PyTorch** | CUDA, ROCm, MPS |
| **TensorFlow** | CUDA, ROCm |
| **JAX** | CUDA, TPU |
| **ONNX Runtime** | CUDA, DirectML, ROCm |

### Libraries

| Library | Purpose |
|---------|---------|
| **cuDNN** | Deep learning primitives |
| **cuBLAS** | Linear algebra |
| **TensorRT** | Inference optimization |
| **Triton** | Inference serving |
| **FlashAttention** | Efficient attention |

### Containers

| Tool | Purpose |
|------|---------|
| **NGC** | NVIDIA GPU Cloud containers |
| **nvidia-docker** | Docker GPU support |
| **NVIDIA Driver Container** | Driver in container |

## Comparison

### Gaming vs Compute GPUs

| Aspect | Gaming (GeForce) | Compute (A100/H100) |
|--------|------------------|---------------------|
| **Memory** | GDDR6/6X (up to 24 GB) | HBM (40-80 GB) |
| **ECC** | No | Yes |
| **FP64** | Limited | Full speed |
| **NVLink** | No | Yes |
| **MIG** | No | Yes (Multi-Instance GPU) |
| **Price** | $500-$2000 | $10,000-$40,000 |

### NVIDIA vs AMD vs Intel

| Aspect | NVIDIA | AMD | Intel |
|--------|--------|-----|-------|
| **Market Share** | ✅ Dominant | ⚠️ Growing | ⚠️ New entrant |
| **Software (AI)** | ✅ CUDA ecosystem | ⚠️ ROCm catching up | ⚠️ oneAPI developing |
| **Gaming** | ✅ Strong | ✅ Strong | ⚠️ Improving |
| **Ray Tracing** | ✅ Mature | ⚠️ Capable | ⚠️ Present |
| **Open Source** | ❌ Proprietary CUDA | ✅ ROCm is open | ✅ oneAPI is open |
| **Data Center** | ✅ Dominant | ✅ Growing (MI300X) | ⚠️ Limited |

## When to Use GPUs

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Parallel Workloads** | Matrix operations, simulations |
| **ML Training** | Essential for deep learning |
| **Graphics Rendering** | Original purpose |
| **Scientific Computing** | Molecular dynamics, CFD |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Memory Limits** | Model size constrained by VRAM |
| **Power Consumption** | 300-700W per GPU |
| **Programming Model** | CUDA lock-in concerns |
| **Cost** | High-end AI GPUs $10K+ |

### Best For

- **Deep Learning** — Training and inference
- **Scientific Simulation** — Physics, chemistry, weather
- **Graphics/Video** — Rendering, encoding
- **Cryptocurrency** — Mining (Ethereum PoS changed this)

### Not Ideal For

- Sequential algorithms
- Low-latency, small-batch inference
- Memory-bound tasks exceeding VRAM
- Cost-sensitive applications

## Related

- [[CPU Architectures]] — Comparison with CPUs
- [[Machine Learning MOC]] — ML/AI concepts
- [[Neural Networks]] — GPU-accelerated models
- [[Mainframes]] — Enterprise computing alternative
