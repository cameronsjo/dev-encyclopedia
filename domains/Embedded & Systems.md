---
title: Embedded & Systems
aliases:
  - Embedded Development
  - Systems Programming
tags:
  - domain
  - embedded
  - systems
  - low-level
type: moc
status: complete
created: '2025-11-28'
---

# Embedded & Systems

Low-level programming for hardware, operating systems, and resource-constrained environments.

## What is Systems Programming?

Code that interacts closely with hardware or provides services to other software:

- Operating systems
- Device drivers
- Embedded firmware
- Compilers and interpreters
- Database engines
- Game engines

---

## Language Landscape

| Language | Memory | Safety | Use Case |
|----------|--------|--------|----------|
| C | Manual | Unsafe | OS, drivers, legacy embedded |
| C++ | Manual | Unsafe | Games, performance-critical |
| Rust | Ownership | Safe | New systems, embedded |
| Zig | Manual | Manual | C replacement |
| Assembly | N/A | N/A | Critical hot paths |

### C

**The lingua franca of systems.**

**Where it dominates:**

- Linux kernel
- Most embedded systems
- Legacy systems
- Maximum portability

**Strengths:**

- Minimal runtime
- Close to hardware
- Universal toolchain support
- Maximum control

**Weaknesses:**

- Memory safety errors
- Undefined behavior
- No modern abstractions

### C++

**C with objects (and much more).**

**Where it's used:**

- Game engines
- Browsers
- Databases
- Performance-critical applications

**Modern C++ (C++17, C++20):**

- Smart pointers
- RAII
- Concepts
- Coroutines

### Rust

**Systems programming with safety.**

**Where it's growing:**

- Linux kernel (modules)
- Firefox components
- AWS infrastructure
- New embedded projects

**Key features:**

- Ownership system (compile-time memory safety)
- No garbage collector
- Zero-cost abstractions
- Strong type system

**Trade-offs:**

- Steep learning curve
- Longer compile times
- Smaller ecosystem than C/C++

---

## Embedded Development

### Categories

| Type | Resources | OS | Example |
|------|-----------|----|---------|
| Bare metal | Minimal | None | Arduino, STM32 |
| RTOS | Limited | Yes | FreeRTOS, Zephyr |
| Embedded Linux | More | Yes | Raspberry Pi |

### Microcontrollers (MCU)

| Family | Architecture | Vendor |
|--------|--------------|--------|
| STM32 | ARM Cortex-M | ST |
| ESP32 | Xtensa/RISC-V | Espressif |
| nRF52 | ARM Cortex-M | Nordic |
| RP2040 | ARM Cortex-M | Raspberry Pi |
| AVR | AVR | Microchip |

### Development Approaches

| Approach | Description |
|----------|-------------|
| Arduino | Simplified C++, libraries |
| PlatformIO | Multi-platform tooling |
| Manufacturer SDK | HAL, CMSIS |
| Rust embedded | `no_std`, `embedded-hal` |

### Real-Time Operating Systems

| RTOS | Notes |
|------|-------|
| FreeRTOS | Most popular, AWS backing |
| Zephyr | Linux Foundation, modern |
| RIOT | IoT-focused |
| Embassy | Rust async RTOS |
| ThreadX | Commercial, now open |

### Rust Embedded

**Growing ecosystem for safe embedded.**

```rust
#![no_std]
#![no_main]

use cortex_m_rt::entry;

#[entry]
fn main() -> ! {
    // Bare metal code
    loop {}
}
```

**Key crates:**

- `embedded-hal` - Hardware abstraction
- `cortex-m` - ARM Cortex-M support
- `svd2rust` - Peripheral access from SVD
- `embassy` - Async runtime

---

## Operating System Development

### Components

| Component | Purpose |
|-----------|---------|
| Bootloader | Initial loading |
| Kernel | Core services |
| Drivers | Hardware interface |
| Scheduler | Process management |
| Memory manager | Virtual memory |
| Filesystem | Storage abstraction |

### Kernel Types

| Type | Description | Examples |
|------|-------------|----------|
| Monolithic | All in kernel space | Linux, FreeBSD |
| Microkernel | Minimal kernel | Minix, seL4 |
| Hybrid | Mix of both | Windows NT, macOS |
| Unikernel | Single-purpose | MirageOS, Unikraft |

### Notable OS Projects

| Project | Language | Notes |
|---------|----------|-------|
| Linux | C | The dominant kernel |
| Redox | Rust | Unix-like, pure Rust |
| SerenityOS | C++ | Desktop OS from scratch |
| Fuchsia | C++, Rust | Google, microkernel |

---

## Memory Management

### Stack vs Heap

| Location | Allocation | Speed | Lifetime |
|----------|------------|-------|----------|
| Stack | Automatic | Fast | Scope-based |
| Heap | Manual | Slower | Explicit |

### Manual Management

**C/C++:** `malloc`/`free`, `new`/`delete`

**Common errors:**

- Use after free
- Double free
- Memory leaks
- Buffer overflows

### Rust Ownership

**Compile-time memory safety:**

- Each value has one owner
- Borrowing rules enforced
- No garbage collector

---

## Toolchains

### Cross-Compilation

Build on one platform, run on another.

| Target | Toolchain |
|--------|-----------|
| ARM Cortex-M | `arm-none-eabi-gcc` |
| ARM Linux | `aarch64-linux-gnu-gcc` |
| RISC-V | `riscv64-unknown-elf-gcc` |

### Build Systems

| System | Languages |
|--------|-----------|
| Make | C, C++ |
| CMake | C, C++ |
| Meson | C, C++ |
| Cargo | Rust |

### Debugging

| Tool | Use |
|------|-----|
| GDB | Standard debugger |
| LLDB | LLVM debugger |
| OpenOCD | On-chip debugging |
| J-Link | Debug probe |
| Logic analyzer | Signal debugging |

---

## Concepts

### Memory-Mapped I/O

Hardware registers accessed as memory addresses.

### Interrupts

Hardware signals that pause normal execution.

| Type | Trigger |
|------|---------|
| IRQ | Hardware event |
| NMI | Critical, non-maskable |
| Software | Explicit instruction |

### DMA (Direct Memory Access)

Hardware copies data without CPU involvement.

### Volatile

Tells compiler: this memory can change unexpectedly.

```c
volatile uint32_t* reg = (volatile uint32_t*)0x40000000;
```

---

## Safety & Security

### Memory Safety

| Approach | Language |
|----------|----------|
| Static analysis | All |
| Fuzzing | All |
| Sanitizers | C, C++, Rust |
| Safe language | Rust |

### Secure Boot

Verify firmware authenticity before execution.

### Formal Verification

Mathematically prove correctness. Used in seL4, CompCert.

---

## Decision Guide

| Scenario | Recommendation |
|----------|----------------|
| New embedded project | Rust if possible, C if ecosystem requires |
| Existing C codebase | Stay C, consider Rust for new modules |
| Game engine | C++ |
| Maximum performance | C or C++ |
| Safety-critical | Rust or formally verified |
| Quick prototype | Arduino, MicroPython |

---

## Related

- [[Runtimes]]
- [[Cross-Compilation]]
- [[Build Systems]]
