---
title: CPU Architectures
aliases:
  - Processor Architectures
  - ISA
  - Instruction Set Architecture
tags:
  - hardware
  - cpu
  - architecture
  - systems
type: reference
status: complete
created: "2025-12-16"
---

# CPU Architectures

Instruction Set Architectures (ISAs) defining how software interfaces with processors, from desktop to embedded systems.

## Overview

| Architecture | Type | Primary Use Cases |
|--------------|------|-------------------|
| **x86-64** | CISC | Desktop, server, workstation |
| **ARM** | RISC | Mobile, embedded, servers, Apple Silicon |
| **RISC-V** | RISC | Embedded, emerging server/desktop |
| **z/Architecture** | CISC | IBM mainframes |
| **POWER** | RISC | IBM servers, HPC |
| **MIPS** | RISC | Embedded, networking (legacy) |
| **SPARC** | RISC | Oracle servers (legacy) |

## x86-64 (AMD64)

### Overview

| Aspect | Details |
|--------|---------|
| **Origin** | AMD (2003), backward-compatible with Intel x86 |
| **Type** | CISC (Complex Instruction Set Computer) |
| **Register Size** | 64-bit |
| **Vendors** | Intel, AMD |
| **Market** | 95%+ of servers and desktops |

### Key Features

| Feature | Description |
|---------|-------------|
| **Backward Compatibility** | Runs 16-bit, 32-bit, and 64-bit code |
| **Virtual Memory** | 48-bit virtual (256 TB), 52-bit physical |
| **Registers** | 16 general-purpose (RAX-R15) |
| **Extensions** | SSE, AVX, AVX-512, AMX |

### Intel vs AMD

| Aspect | Intel | AMD |
|--------|-------|-----|
| **Current Desktop** | Core Ultra (Meteor Lake) | Ryzen 9000 (Zen 5) |
| **Current Server** | Xeon (Sapphire Rapids) | EPYC (Genoa/Bergamo) |
| **Manufacturing** | Intel 4/3 | TSMC 4/5nm |
| **Chiplet Design** | E-cores + P-cores | Multi-CCD architecture |
| **Memory** | DDR5, HBM | DDR5, HBM |

### Extensions

| Extension | Purpose | Operations |
|-----------|---------|------------|
| **SSE** | SIMD (128-bit) | Floating-point |
| **AVX2** | SIMD (256-bit) | FP, integer |
| **AVX-512** | SIMD (512-bit) | AI, HPC |
| **AMX** | Matrix operations | AI acceleration |
| **AES-NI** | Encryption | Hardware AES |

## ARM

### Overview

| Aspect | Details |
|--------|---------|
| **Origin** | Acorn (1985), ARM Holdings (1990) |
| **Type** | RISC (Reduced Instruction Set Computer) |
| **Licensing** | Architecture license, core license |
| **Market** | 99% of mobile, growing in servers |

### Architecture Versions

| Version | Year | Key Features |
|---------|------|--------------|
| **ARMv7** | 2005 | 32-bit, Cortex-A series |
| **ARMv8** | 2011 | 64-bit (AArch64), Cortex-A53/A57 |
| **ARMv9** | 2021 | SVE2, security improvements |

### Core Families

| Family | Target | Examples |
|--------|--------|----------|
| **Cortex-A** | Application (phones, servers) | A78, X4, Neoverse |
| **Cortex-R** | Real-time | Automotive, storage |
| **Cortex-M** | Microcontroller | IoT, embedded |
| **Neoverse** | Infrastructure | N2, V2 (servers) |

### Apple Silicon

**ARM-based chips for Mac, iPad, iPhone.**

| Chip | Year | Cores | Target |
|------|------|-------|--------|
| **M1** | 2020 | 8 (4P+4E) | MacBook Air, Mac mini |
| **M2** | 2022 | 8 (4P+4E) | MacBook Air/Pro |
| **M3** | 2023 | 8 (4P+4E) | MacBook Pro, iMac |
| **M4** | 2024 | Up to 10 | iPad Pro, Mac |
| **M1/M2/M3 Ultra** | — | Up to 24 | Mac Studio, Mac Pro |

**Advantages:**

- Unified memory architecture
- Excellent performance per watt
- Tight hardware/software integration
- Neural Engine for ML

### ARM in Servers

| Product | Vendor | Notes |
|---------|--------|-------|
| **Graviton4** | AWS | Neoverse V2-based |
| **Ampere Altra** | Ampere | Up to 128 cores |
| **Grace** | NVIDIA | CPU for AI workloads |
| **Cobalt 100** | Microsoft | Azure ARM server |

### ARM vs x86 Trade-offs

| Aspect | ARM | x86 |
|--------|-----|-----|
| **Power Efficiency** | ✅ Excellent | ⚠️ Improving |
| **Single-Thread** | ⚠️ Catching up | ✅ Historically stronger |
| **Software Ecosystem** | ⚠️ Growing | ✅ Mature |
| **Virtualization** | ✅ Mature (ARMv8) | ✅ Mature (VT-x) |
| **Price/Performance** | ✅ Often better | ⚠️ Varies |

## RISC-V

### Overview

| Aspect | Details |
|--------|---------|
| **Origin** | UC Berkeley (2010) |
| **Type** | RISC, open-source ISA |
| **Licensing** | Free, no royalties |
| **Status** | Emerging in embedded, growing elsewhere |

### Key Characteristics

| Feature | Description |
|---------|-------------|
| **Open Standard** | Free to implement |
| **Modular** | Base ISA + optional extensions |
| **Customizable** | Custom instructions allowed |
| **No Royalties** | Unlike ARM |

### Extensions

| Extension | Description |
|-----------|-------------|
| **I** | Integer base (mandatory) |
| **M** | Integer multiply/divide |
| **A** | Atomic operations |
| **F** | Single-precision FP |
| **D** | Double-precision FP |
| **C** | Compressed instructions |
| **V** | Vector operations |

**Common Profiles:**

- **RV32IMAC** — 32-bit embedded
- **RV64GC** — 64-bit general-purpose (G = IMAFD)

### Current Products

| Product | Vendor | Target |
|---------|--------|--------|
| **SiFive U74** | SiFive | Linux-capable SoC |
| **ESP32-C3** | Espressif | IoT (replacing Xtensa) |
| **StarFive JH7110** | StarFive | Single-board computers |
| **Kendryte K210** | Canaan | AI edge |

### RISC-V Adoption

| Domain | Status |
|--------|--------|
| **Embedded/IoT** | ✅ Growing rapidly |
| **Desktop** | ⚠️ Very early (VisionFive) |
| **Server** | ⚠️ In development |
| **HPC** | ⚠️ European initiatives |

## Other Architectures

### z/Architecture (IBM Mainframe)

| Aspect | Details |
|--------|---------|
| **Type** | CISC |
| **Register Size** | 64-bit |
| **Current** | z16 (Telum processor) |
| **Features** | On-chip AI, extreme reliability |

See [[Mainframes]] for details.

### IBM POWER

| Aspect | Details |
|--------|---------|
| **Type** | RISC |
| **Current** | POWER10 |
| **Target** | Enterprise servers, HPC |
| **Features** | SMT8, high memory bandwidth |

### MIPS (Legacy)

| Aspect | Details |
|--------|---------|
| **Status** | Largely replaced by ARM/RISC-V |
| **Historical Use** | Networking, embedded, consoles |
| **Modern** | Open-sourced, limited new development |

## Architecture Comparison

### Instruction Set Philosophy

| Aspect | CISC (x86) | RISC (ARM, RISC-V) |
|--------|------------|---------------------|
| **Instructions** | Many, complex | Fewer, simpler |
| **Instruction Length** | Variable | Fixed (mostly) |
| **Decode Complexity** | Higher | Lower |
| **Memory Access** | In most instructions | Load/store only |
| **Code Density** | Good | Requires more instructions |

### Modern Reality

The CISC/RISC distinction is blurred:

- x86 CPUs internally decode to RISC-like micro-ops
- ARM has grown more complex over time
- Performance depends more on implementation than ISA

### Performance Comparison

| Workload | x86 | ARM | RISC-V |
|----------|-----|-----|--------|
| **Single-Thread** | ✅ Strong | ✅ Apple leads | ⚠️ Developing |
| **Multi-Thread** | ✅ Strong | ✅ High core counts | ⚠️ Developing |
| **Power Efficiency** | ⚠️ Improving | ✅ Excellent | ✅ Good |
| **Embedded** | ❌ Too power-hungry | ✅ Dominant | ✅ Growing |
| **Server** | ✅ Dominant | ✅ Growing (AWS, Azure) | ⚠️ Emerging |

## Memory and Virtualization

### Memory Models

| Feature | x86-64 | ARM | RISC-V |
|---------|--------|-----|--------|
| **Ordering** | Strong (TSO) | Relaxed | Relaxed (RVWMO) |
| **Virtual Memory** | 48-bit+ | 48-bit (ARMv8.2: 52-bit) | Sv39/Sv48/Sv57 |
| **Page Sizes** | 4K, 2M, 1G | 4K, 16K, 64K, + huge | 4K, 2M, 1G |

### Virtualization Support

| Feature | x86 (Intel) | x86 (AMD) | ARM | RISC-V |
|---------|-------------|-----------|-----|--------|
| **Technology** | VT-x, VT-d | AMD-V, IOMMU | EL2/VHE | H extension |
| **Maturity** | ✅ Mature | ✅ Mature | ✅ Mature | ⚠️ Developing |
| **Nested** | ✅ Yes | ✅ Yes | ✅ Yes | ⚠️ Planned |

## Development Considerations

### Cross-Compilation

| Host | Target | Toolchain |
|------|--------|-----------|
| x86 | ARM | `aarch64-linux-gnu-gcc` |
| x86 | RISC-V | `riscv64-unknown-linux-gnu-gcc` |
| ARM | x86 | `x86_64-linux-gnu-gcc` |

### Emulation

| Tool | Purpose |
|------|---------|
| **QEMU** | Full system/user-mode emulation |
| **Rosetta 2** | x86 on Apple Silicon |
| **Box64** | x86 on ARM Linux |
| **FEX-Emu** | x86 on ARM Linux |

### Containerization

| Platform | Multi-arch Support |
|----------|-------------------|
| **Docker** | `--platform linux/amd64,linux/arm64` |
| **Podman** | QEMU-based cross-arch |
| **Kubernetes** | Node selectors, taints |

## When to Choose

| Scenario | Recommendation |
|----------|----------------|
| **Desktop/Laptop** | x86-64 (most software) or Apple Silicon (Mac) |
| **Cloud Server** | x86-64 or ARM (Graviton for cost) |
| **Mobile** | ARM (no real alternative) |
| **Embedded (low power)** | ARM Cortex-M or RISC-V |
| **Embedded (Linux)** | ARM Cortex-A or RISC-V |
| **HPC** | x86-64 (AMD EPYC) or ARM (emerging) |
| **Mainframe** | z/Architecture (IBM only) |
| **Custom chip (no royalties)** | RISC-V |

## Related

- [[Mainframes]] — IBM z/Architecture details
- [[GPUs]] — Parallel compute accelerators
- [[Domains/Embedded & Systems]] — Low-level development
- [[Languages/C|C]] — Systems programming language
- [[Rust]] — Modern systems language
