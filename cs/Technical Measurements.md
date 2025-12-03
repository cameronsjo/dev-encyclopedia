---
title: Technical Measurements
aliases:
  - FLOPS
  - IOPS
  - Computing Units
  - Performance Metrics
tags:
  - cs
  - fundamentals
  - performance
  - hardware
type: concept
status: complete
difficulty: fundamentals
created: 2025-12-03
---

# Technical Measurements

Units and metrics used to measure computing performance, storage, and data transfer.

## Data Size Units

### Decimal (SI) vs Binary (IEC)

| SI Unit | Value | IEC Unit | Value |
|---------|-------|----------|-------|
| Kilobyte (KB) | 10³ = 1,000 | Kibibyte (KiB) | 2¹⁰ = 1,024 |
| Megabyte (MB) | 10⁶ = 1,000,000 | Mebibyte (MiB) | 2²⁰ = 1,048,576 |
| Gigabyte (GB) | 10⁹ | Gibibyte (GiB) | 2³⁰ |
| Terabyte (TB) | 10¹² | Tebibyte (TiB) | 2⁴⁰ |
| Petabyte (PB) | 10¹⁵ | Pebibyte (PiB) | 2⁵⁰ |
| Exabyte (EB) | 10¹⁸ | Exbibyte (EiB) | 2⁶⁰ |

**Why it matters:** A "500 GB" drive is ~465 GiB. Marketing uses SI, OS often uses IEC.

### Scale Reference

| Size | Real-World Example |
|------|-------------------|
| 1 KB | Short text file |
| 1 MB | 1 minute MP3, high-res photo |
| 1 GB | 1 hour SD video, ~200 songs |
| 1 TB | ~500 hours HD video |
| 1 PB | 3.4 years of 24/7 HD video |
| 1 EB | All words ever spoken by humans (~5 EB) |

---

## Computing Performance

### FLOPS (Floating Point Operations Per Second)

Measures computational throughput for scientific/ML workloads.

| Unit | Value | Notation |
|------|-------|----------|
| FLOPS | 1 | - |
| KFLOPS | 10³ | Kilo |
| MFLOPS | 10⁶ | Mega |
| GFLOPS | 10⁹ | Giga |
| TFLOPS | 10¹² | Tera |
| PFLOPS | 10¹⁵ | Peta |
| EFLOPS | 10¹⁸ | Exa |

### FLOPS in Context

| System | Performance | Year |
|--------|-------------|------|
| Intel 8087 coprocessor | 50 KFLOPS | 1980 |
| PlayStation 2 | 6.2 GFLOPS | 2000 |
| iPhone 15 Pro (A17) | ~2 TFLOPS | 2023 |
| NVIDIA RTX 4090 | 83 TFLOPS (FP32) | 2022 |
| NVIDIA H100 | 2,000 TFLOPS (FP8) | 2022 |
| Frontier (supercomputer) | 1.2 EFLOPS | 2022 |

**Note:** FLOPS varies by precision (FP64, FP32, FP16, FP8, INT8).

### TOPS (Tera Operations Per Second)

Used for AI/ML inference, often INT8 operations.

| Chip | TOPS | Use Case |
|------|------|----------|
| Apple M3 Neural Engine | 18 | On-device ML |
| Google TPU v4 | 275 | Cloud ML |
| NVIDIA H100 | 3,958 (INT8) | Data center AI |

### OPS vs FLOPS

| Metric | Measures | Use Case |
|--------|----------|----------|
| FLOPS | Floating-point ops | Scientific computing, training |
| OPS/TOPS | All operations (often INT) | Inference, edge AI |
| MIPS | Instructions | General CPU comparison (dated) |

---

## Storage Performance

### IOPS (I/O Operations Per Second)

Measures storage transaction speed.

| Storage Type | Random Read IOPS | Random Write IOPS |
|--------------|------------------|-------------------|
| HDD (7200 RPM) | 75-100 | 75-100 |
| SATA SSD | 50,000-100,000 | 30,000-90,000 |
| NVMe SSD | 500,000-1,000,000+ | 300,000-800,000 |
| Intel Optane | 500,000+ | 500,000+ |
| RAM disk | Millions | Millions |

### Throughput vs IOPS

| Workload | Metric That Matters |
|----------|-------------------|
| Database transactions | IOPS |
| Video editing | Throughput (MB/s) |
| Boot time | IOPS |
| File copy | Throughput |
| VM hosting | Both |

### Storage Throughput

| Interface | Max Throughput |
|-----------|---------------|
| SATA III | 600 MB/s |
| PCIe 3.0 x4 | 4 GB/s |
| PCIe 4.0 x4 | 8 GB/s |
| PCIe 5.0 x4 | 16 GB/s |

---

## Network Measurements

### Bandwidth Units

| Unit | Value | Common Use |
|------|-------|------------|
| bps | 1 bit/second | Base unit |
| Kbps | 10³ bps | Legacy modems |
| Mbps | 10⁶ bps | Home internet |
| Gbps | 10⁹ bps | Enterprise, data centers |
| Tbps | 10¹² bps | Internet backbone |

**Bits vs Bytes:** Network = bits (Mbps), storage = bytes (MB/s). Divide by 8.

```
100 Mbps ≈ 12.5 MB/s theoretical max
```

### Latency Units

| Unit | Value | Use |
|------|-------|-----|
| Second (s) | 1 | Human-scale |
| Millisecond (ms) | 10⁻³ | Network latency |
| Microsecond (μs) | 10⁻⁶ | Memory, SSD |
| Nanosecond (ns) | 10⁻⁹ | CPU cache, RAM |
| Picosecond (ps) | 10⁻¹² | Chip internals |

### Latency Reference

| Operation | Latency |
|-----------|---------|
| L1 cache reference | 1 ns |
| L2 cache reference | 4 ns |
| RAM reference | 100 ns |
| NVMe SSD read | 10-100 μs |
| HDD seek | 5-10 ms |
| Same datacenter RTT | 0.5 ms |
| Cross-country RTT | 30-50 ms |
| Intercontinental RTT | 100-200 ms |

---

## CPU Measurements

### Clock Speed

| Unit | Value |
|------|-------|
| Hz | 1 cycle/second |
| MHz | 10⁶ Hz |
| GHz | 10⁹ Hz |

**Modern CPUs:** 3-5 GHz typical, up to 6 GHz boost.

### IPC (Instructions Per Cycle)

Measures CPU efficiency. Higher is better.

```
Performance = Clock Speed × IPC × Cores
```

**Why it matters:** A 3 GHz CPU with 2 IPC beats a 4 GHz CPU with 1 IPC.

### TDP (Thermal Design Power)

Power consumption and heat output in watts.

| Category | TDP Range |
|----------|-----------|
| Mobile (ultrabook) | 15-28W |
| Mobile (gaming) | 45-65W |
| Desktop | 65-125W |
| High-end desktop | 125-250W |
| Server | 200-400W |

---

## Memory Measurements

### Bandwidth

| Memory Type | Bandwidth |
|-------------|-----------|
| DDR4-3200 | 25.6 GB/s per channel |
| DDR5-5600 | 44.8 GB/s per channel |
| HBM2e | 460 GB/s per stack |
| HBM3 | 819 GB/s per stack |

### Capacity Scaling

| RAM | Typical Use |
|-----|------------|
| 8 GB | Basic desktop |
| 16 GB | Development, gaming |
| 32-64 GB | Professional workstation |
| 128-256 GB | Servers, VMs |
| 1-2 TB | Large databases, ML |
| 4+ TB | In-memory computing |

---

## GPU Measurements

### Memory Bandwidth

| GPU | Memory Bandwidth |
|-----|-----------------|
| RTX 3060 | 360 GB/s |
| RTX 4090 | 1,008 GB/s |
| A100 (80GB) | 2,039 GB/s |
| H100 (SXM) | 3,350 GB/s |

### VRAM

| Use Case | VRAM Needed |
|----------|-------------|
| 1080p gaming | 6-8 GB |
| 4K gaming | 10-12 GB |
| Stable Diffusion | 8-12 GB |
| LLM inference (7B) | 8-16 GB |
| LLM inference (70B) | 80-140 GB |
| LLM training | 100s of GB |

---

## Request Rates

### RPS/QPS (Requests/Queries Per Second)

| Scale | Context |
|-------|---------|
| 10 RPS | Small application |
| 1,000 RPS | Medium website |
| 10,000 RPS | Large service |
| 100,000+ RPS | Major platform |
| 1M+ RPS | Google-scale |

### TPS (Transactions Per Second)

| System | TPS |
|--------|-----|
| Bitcoin | ~7 |
| Ethereum (pre-sharding) | ~15-30 |
| Visa network | ~65,000 peak |
| Single PostgreSQL | 10,000-50,000 |

---

## Reliability Measurements

### Availability (Nines)

| Nines | Uptime % | Downtime/Year |
|-------|----------|---------------|
| 2 nines | 99% | 3.65 days |
| 3 nines | 99.9% | 8.76 hours |
| 4 nines | 99.99% | 52.6 minutes |
| 5 nines | 99.999% | 5.26 minutes |
| 6 nines | 99.9999% | 31.5 seconds |

### MTBF/MTTR

| Metric | Definition |
|--------|------------|
| MTBF | Mean Time Between Failures |
| MTTR | Mean Time To Recovery |
| MTTF | Mean Time To Failure (non-repairable) |

```
Availability = MTBF / (MTBF + MTTR)
```

---

## Energy Efficiency

### PUE (Power Usage Effectiveness)

Data center efficiency metric.

```
PUE = Total Facility Power / IT Equipment Power
```

| PUE | Rating |
|-----|--------|
| 2.0+ | Inefficient |
| 1.5-2.0 | Average |
| 1.2-1.5 | Efficient |
| 1.1-1.2 | Very efficient |
| <1.1 | State of the art |

### FLOPS per Watt

ML training efficiency metric.

| System | FLOPS/Watt |
|--------|------------|
| CPU (general) | ~10 GFLOPS/W |
| GPU (gaming) | ~100 GFLOPS/W |
| GPU (datacenter) | ~300 GFLOPS/W |
| TPU | ~400+ GFLOPS/W |

---

## Prefix Reference

| Prefix | Symbol | Value | Name |
|--------|--------|-------|------|
| Kilo | K | 10³ | Thousand |
| Mega | M | 10⁶ | Million |
| Giga | G | 10⁹ | Billion |
| Tera | T | 10¹² | Trillion |
| Peta | P | 10¹⁵ | Quadrillion |
| Exa | E | 10¹⁸ | Quintillion |
| Zetta | Z | 10²¹ | Sextillion |
| Yotta | Y | 10²⁴ | Septillion |

### Small Prefixes

| Prefix | Symbol | Value |
|--------|--------|-------|
| Milli | m | 10⁻³ |
| Micro | μ | 10⁻⁶ |
| Nano | n | 10⁻⁹ |
| Pico | p | 10⁻¹² |
| Femto | f | 10⁻¹⁵ |

---

## Related

- [[Big O Notation]]
- [[Network Performance]]
- [[System Design]]
- [[Memory Management]]
