---
title: Embedded Frameworks
aliases:
  - Embedded Development Frameworks
  - IoT Frameworks
  - RTOS Frameworks
tags:
  - framework
  - embedded
  - hardware
  - iot
  - rtos
type: reference
status: complete
created: "2025-11-30"
---

# Embedded Frameworks

Software frameworks and operating systems for resource-constrained embedded systems, microcontrollers, and IoT devices.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Abstracts hardware, provides drivers, RTOS, and development tools for embedded systems |
| **Scope** | Bare-metal to full RTOS, single-core MCUs to multi-core SoCs |
| **Key Features** | HAL, peripheral drivers, task scheduling, power management, connectivity |
| **Primary Languages** | C, C++, Rust |
| **Target Hardware** | ARM Cortex-M, RISC-V, ESP32, AVR, STM32, Nordic nRF |

## Framework Comparison

| Framework | Type | RTOS | Languages | License | Best For |
|-----------|------|------|-----------|---------|----------|
| **Arduino** | Development Platform | No (loop-based) | C++ | LGPL | Rapid prototyping, beginners, hobby projects |
| **ESP-IDF** | SDK + RTOS | FreeRTOS | C, C++ | Apache 2.0 | ESP32/ESP8266, Wi-Fi/BLE IoT devices |
| **Zephyr RTOS** | Full RTOS | Yes | C | Apache 2.0 | Multi-vendor hardware, commercial IoT |
| **FreeRTOS** | Lightweight RTOS | Yes | C | MIT | Real-time tasks, AWS IoT integration |
| **mbed OS** | IoT RTOS | Yes | C++ | Apache 2.0 | ARM Cortex-M, cloud-connected devices |
| **RIOT** | Microkernel RTOS | Yes | C, C++, Rust | LGPL | IoT research, resource-constrained devices |

## Core Concepts

### Hardware Abstraction Layer (HAL)

Provides consistent API across different microcontrollers and hardware platforms.

- **Purpose**: Write code once, run on multiple hardware targets
- **Abstracts**: GPIO, timers, ADC, DAC, memory, clocks
- **Trade-off**: Slight overhead vs direct register manipulation
- **Examples**: STM32 HAL, Nordic SDK, ESP-IDF HAL

### Peripheral Communication Protocols

**GPIO (General Purpose Input/Output)**

- Digital pin control (high/low, input/output)
- Interrupt-driven event handling
- Common for LEDs, buttons, relays

**I2C (Inter-Integrated Circuit)**

- Multi-device bus (master/slave)
- Two-wire: SDA (data) + SCL (clock)
- Typical use: sensors, displays, EEPROMs
- Speed: 100 kHz (standard) to 3.4 MHz (high-speed)

**SPI (Serial Peripheral Interface)**

- Full-duplex, high-speed (MHz range)
- Four-wire: MISO, MOSI, SCK, CS
- Typical use: SD cards, displays, high-speed sensors
- Supports multiple slaves with chip select

**UART (Universal Asynchronous Receiver/Transmitter)**

- Two-wire: TX, RX
- Asynchronous serial communication
- Common for debugging, GPS modules, serial consoles
- Configurable baud rate, parity, stop bits

### Real-Time Operating System (RTOS)

**Task Scheduling**

- Preemptive multitasking with priority levels
- Deterministic response times for real-time requirements
- Round-robin or priority-based scheduling

**Inter-Task Communication**

- Queues: Pass data between tasks
- Semaphores: Resource locking and signaling
- Mutexes: Mutual exclusion for shared resources
- Event flags: Synchronize task execution

**Memory Management**

- Static allocation (compile-time)
- Dynamic allocation with heap management
- Stack overflow protection
- Memory pools for deterministic allocation

### Interrupts

Hardware events that preempt normal execution flow.

- **Types**: External (GPIO), peripheral (UART, timer), software
- **Priority Levels**: Nested interrupts based on urgency
- **ISR Best Practices**: Keep short, defer work to tasks, avoid blocking calls
- **Context Switching**: Save/restore CPU state when servicing interrupts

### Power Management

Critical for battery-powered IoT devices.

**Sleep Modes**

- Light sleep: CPU halted, peripherals active
- Deep sleep: Only RTC and wake sources active
- Hibernation: Minimal power, wake via external trigger

**Power Optimization**

- Dynamic frequency scaling
- Peripheral power gating
- Wake-on-interrupt
- Duty cycling for periodic tasks

**Typical Battery Life Strategies**

- Sleep between sensor readings
- Batch network transmissions
- Use low-power peripherals (RTC vs timers)

### Over-The-Air (OTA) Updates

Remote firmware updates without physical access.

**Process**

1. Download new firmware to secondary partition
2. Verify integrity (checksum, signature)
3. Swap partitions and reboot
4. Rollback on boot failure

**Considerations**

- Dual-partition requirement (A/B scheme)
- Secure boot and signed images
- Network reliability and resume capability
- Atomic updates to prevent bricking

## Framework Deep Dive

### Arduino

**Strengths**

- Enormous library ecosystem (sensors, displays, actuators)
- Unified API across hundreds of boards
- Visual IDE with serial monitor and plotter
- Massive community and tutorials

**Architecture**

- `setup()` runs once at boot
- `loop()` runs continuously
- No multitasking (cooperative only via yield)
- Direct access to underlying platform (ESP-IDF, mbed, etc.)

**Considerations**

- Not suitable for hard real-time requirements
- Limited low-power optimization
- Abstraction overhead on resource-constrained MCUs

### ESP-IDF

**Strengths**

- First-class Wi-Fi, Bluetooth, BLE support
- FreeRTOS for multitasking
- Comprehensive peripheral drivers (SPI, I2C, UART, ADC, DAC)
- OTA updates, secure boot, flash encryption

**Architecture**

- Component-based build system (CMake)
- Event loop for asynchronous operations
- Non-volatile storage (NVS) for configuration
- Wi-Fi provisioning (SoftAP, BLE, SmartConfig)

**Best For**

- ESP32-based IoT products
- Connected devices requiring Wi-Fi/BLE
- Battery-powered devices with deep sleep

### Zephyr RTOS

**Strengths**

- 500+ supported boards (ARM, RISC-V, x86, Xtensa)
- Unified device tree and Kconfig configuration
- Comprehensive networking stack (IPv4/6, 6LoWPAN, Thread, Bluetooth Mesh)
- Professional-grade security (TLS, secure boot, MPU)

**Architecture**

- Microkernel design with modular subsystems
- Device drivers follow consistent API patterns
- Logging, shell, file systems included
- West tool for multi-repo management

**Best For**

- Multi-vendor commercial products
- Standards-compliant IoT (Thread, Matter)
- Long-term support requirements

### FreeRTOS

**Strengths**

- Tiny footprint (4-9 KB)
- Preemptive, deterministic scheduler
- AWS IoT integration (FreeRTOS LTS)
- Decades of production use

**Architecture**

- Tasks, queues, semaphores, timers
- Portable to 40+ architectures
- Tick-based scheduling (configurable rate)
- Optional memory protection (MPU support)

**Considerations**

- Minimal features (no networking, file system by default)
- Requires integration work for peripherals
- Best used with vendor SDKs (STM32Cube, ESP-IDF)

### mbed OS

**Strengths**

- C++ RTOS for ARM Cortex-M
- Built-in connectivity (BLE, LoRaWAN, cellular)
- Pelion Device Management integration
- Hardware security (TrustZone, secure storage)

**Architecture**

- RTOS with C++ threading
- HAL for uniform peripheral access
- CMSIS-RTOS2 API compliance
- Online compiler and CLI tools

**Considerations**

- ARM-only (Cortex-M series)
- Larger footprint than bare-metal solutions
- Pelion cloud services require subscription

### RIOT

**Strengths**

- Microkernel architecture (modular, small)
- Energy efficiency focus
- IoT protocol support (CoAP, MQTT, 6LoWPAN)
- Rust support alongside C/C++

**Architecture**

- Tickless scheduler (power-efficient)
- Native port runs on Linux (rapid testing)
- Network stack with IPv6 emphasis
- Module-based build system

**Best For**

- Academic and research projects
- Ultra-low-power wireless sensor networks
- Rust-based embedded development

## Decision Guide

| If You Need | Choose |
|-------------|--------|
| Fastest prototyping, huge library ecosystem | **Arduino** |
| ESP32 with Wi-Fi/BLE, cloud connectivity | **ESP-IDF** |
| Multi-vendor support, commercial IoT product | **Zephyr RTOS** |
| Minimal RTOS for real-time tasks, AWS IoT | **FreeRTOS** |
| ARM Cortex-M with cloud management | **mbed OS** |
| Research, ultra-low-power, Rust support | **RIOT** |

## Feature Matrix

| Feature | Arduino | ESP-IDF | Zephyr | FreeRTOS | mbed OS | RIOT |
|---------|---------|---------|--------|----------|---------|------|
| **RTOS** | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **Multi-Board** | ✅ | ❌ | ✅ | ✅ | Partial | ✅ |
| **Wi-Fi Built-in** | Partial | ✅ | ✅ | ❌ | ✅ | ✅ |
| **BLE Built-in** | Partial | ✅ | ✅ | ❌ | ✅ | ✅ |
| **OTA Updates** | Partial | ✅ | ✅ | ❌ | ✅ | ✅ |
| **Power Management** | Basic | ✅ | ✅ | Basic | ✅ | ✅ |
| **Secure Boot** | ❌ | ✅ | ✅ | ❌ | ✅ | ❌ |
| **File System** | ✅ | ✅ | ✅ | ❌ | ✅ | ✅ |
| **USB Device** | ✅ | ✅ | ✅ | ❌ | ✅ | ✅ |
| **Debugger Support** | Basic | ✅ | ✅ | ✅ | ✅ | ✅ |

## Common Peripheral Patterns

### GPIO Interrupt Handling

**Pattern**: Detect events without polling

- Configure pin as input with pull-up/pull-down
- Attach ISR to rising/falling/change edge
- Debounce in software (timers or delays)
- Signal task via semaphore or queue from ISR

### I2C Sensor Reading

**Pattern**: Read temperature/humidity/pressure sensors

1. Initialize I2C bus (SDA/SCL pins, frequency)
2. Send device address + register address
3. Read response bytes
4. Parse data per sensor datasheet
5. Handle timeouts and NACK errors

### SPI Display Update

**Pattern**: High-speed graphics updates

1. Initialize SPI (MOSI, MISO, SCK, CS)
2. Assert chip select (CS low)
3. Send command/data via SPI transactions
4. De-assert chip select (CS high)
5. Use DMA for large frame buffers

### UART Debugging

**Pattern**: Serial console for logs and commands

- Configure baud rate (9600, 115200 common)
- Transmit formatted strings (printf-style)
- Receive commands via interrupt or DMA
- Implement shell with command parsing

## Best Practices

**Hardware Abstraction**

- Use HAL for portability across vendors
- Abstract board-specific code into separate files
- Use device tree or configuration files for pin mappings

**Task Design**

- One task per logical function (sensor read, network, UI)
- Use queues for inter-task data flow
- Avoid busy-waiting; use semaphores or event flags

**Interrupt Service Routines**

- Keep ISRs under 10 microseconds
- Defer processing to tasks via queues
- Never call blocking functions in ISRs
- Use ISR-safe RTOS calls only

**Power Optimization**

- Profile current consumption per mode
- Use lowest acceptable sleep mode
- Disable unused peripherals and clocks
- Batch network operations to minimize radio on-time

**Debugging**

- Use JTAG/SWD debuggers (J-Link, ST-Link)
- Enable watchdog timers to recover from hangs
- Implement panic handlers with stack dumps
- Log to persistent storage for post-mortem analysis

**Security**

- Enable secure boot and flash encryption
- Validate firmware signatures before OTA
- Use hardware crypto accelerators
- Isolate sensitive data with memory protection units

## When to Use Each Framework

### Arduino

**Best For**

- Hobbyist projects and rapid prototyping
- Educational environments
- Projects with extensive library dependencies
- Non-critical timing requirements

**Avoid When**

- Hard real-time deadlines required
- Battery life is critical (sub-1mA average)
- Security is paramount (no secure boot)

### ESP-IDF

**Best For**

- ESP32/ESP8266 commercial products
- Wi-Fi/BLE IoT devices
- OTA-enabled consumer electronics
- AWS IoT or cloud-connected devices

**Avoid When**

- Non-ESP hardware required
- Ultra-low-power (nA range) needed
- Multi-vendor hardware flexibility desired

### Zephyr RTOS

**Best For**

- Multi-vendor product lines
- Standards-based IoT (Matter, Thread)
- Long-term support and security updates
- Enterprise-grade reliability

**Avoid When**

- Rapid prototyping (steeper learning curve)
- Minimal footprint required (<64 KB flash)
- Arduino library ecosystem needed

### FreeRTOS

**Best For**

- Real-time control systems
- Minimal footprint RTOS
- AWS IoT integration
- Existing vendor SDK integration (STM32, Nordic)

**Avoid When**

- Networking stack required out-of-box
- USB, file systems, or graphics needed
- Prefer higher-level abstractions

### mbed OS

**Best For**

- ARM Cortex-M product development
- Cloud-managed device fleets (Pelion)
- C++ embedded development
- Cellular/LoRaWAN IoT devices

**Avoid When**

- Non-ARM hardware required
- Open-source cloud solution preferred
- Minimal footprint critical

### RIOT

**Best For**

- Wireless sensor networks
- Academic research and education
- Rust embedded development
- Energy-efficient IoT protocols (6LoWPAN, CoAP)

**Avoid When**

- Commercial support required
- Extensive production deployments
- Mainstream hardware vendors preferred

## Related

- [[Domains/Embedded & Systems]]
- [[Languages/C]]
- [[Languages/Rust]]
- [[Languages/C++]]
- [[Tools/Build Systems]]
