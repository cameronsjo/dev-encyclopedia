---
title: WebAssembly
aliases:
  - WASM
  - Wasm
tags:
  - concept
  - wasm
  - runtime
  - portable
type: reference
status: complete
created: 2025-12-04
---

# WebAssembly

Portable binary instruction format for a stack-based virtual machine.

## Overview

| Aspect | Details |
|--------|---------|
| Type | Binary instruction format |
| Execution | Stack machine |
| Memory | Linear (flat array) |
| Created | 2015 (W3C standard 2019) |
| Use cases | Browser, server, edge, embedded, plugins |

---

## What is WASM?

```
┌─────────────────────────────────────────────────────────┐
│                   Source Languages                       │
│        Rust, C, C++, Go, Zig, AssemblyScript            │
└──────────────────────┬──────────────────────────────────┘
                       │ Compile
                       ▼
              ┌─────────────────┐
              │   .wasm binary  │ ← Portable bytecode
              └────────┬────────┘
                       │ Execute
                       ▼
┌─────────────────────────────────────────────────────────┐
│                      Runtimes                            │
│   Browser │ Wasmtime │ Wasmer │ WasmEdge │ Node/Deno   │
└─────────────────────────────────────────────────────────┘
```

### Key Properties

| Property | Description |
|----------|-------------|
| **Portable** | Same binary runs on any conforming runtime |
| **Fast** | Near-native speed (JIT/AOT compiled) |
| **Safe** | Sandboxed, memory-safe by default |
| **Compact** | Binary format smaller than text |
| **Language-agnostic** | Compile from many source languages |
| **Deterministic** | Same inputs → same outputs |

---

## Binary Format

### Module Structure

```
WASM Module (.wasm):
┌─────────────────────────────────────────┐
│ Magic number: 0x00 0x61 0x73 0x6D       │ ← "\0asm"
│ Version: 0x01 0x00 0x00 0x00            │ ← version 1
├─────────────────────────────────────────┤
│ Section: Types     (function signatures)│
│ Section: Imports   (from host/other)    │
│ Section: Functions (indices to types)   │
│ Section: Tables    (indirect calls)     │
│ Section: Memory    (linear memory)      │
│ Section: Globals   (mutable/const)      │
│ Section: Exports   (exposed to host)    │
│ Section: Start     (initialization)     │
│ Section: Elements  (table init)         │
│ Section: Code      (function bodies)    │
│ Section: Data      (memory init)        │
└─────────────────────────────────────────┘
```

### Text Format (WAT)

```wasm
;; add.wat - Human-readable WebAssembly text format
(module
  ;; Function that adds two i32 values
  (func $add (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add
  )

  ;; Export the function
  (export "add" (func $add))
)
```

```bash
# Convert WAT to WASM
wat2wasm add.wat -o add.wasm

# Convert WASM to WAT
wasm2wat add.wasm -o add.wat
```

### Value Types

| Type | Description |
|------|-------------|
| `i32` | 32-bit integer |
| `i64` | 64-bit integer |
| `f32` | 32-bit float |
| `f64` | 64-bit float |
| `v128` | 128-bit SIMD vector |
| `funcref` | Function reference |
| `externref` | External (host) reference |

---

## Memory Model

### Linear Memory

```
WASM Linear Memory:
┌─────────────────────────────────────────────────────────┐
│ 0x0000  │ 0x0004  │ 0x0008  │ ... │ 0xFFFF  │           │
├─────────┴─────────┴─────────┴─────┴─────────┴───────────┤
│                    Flat byte array                       │
│                   (grows in 64KB pages)                  │
└─────────────────────────────────────────────────────────┘
         ▲                                     ▲
         │                                     │
      Heap start                          Current size
```

### Memory Operations

```wasm
;; Load 32-bit int from address
(i32.load (i32.const 0))

;; Store 32-bit int at address
(i32.store (i32.const 0) (i32.const 42))

;; Grow memory by 1 page (64KB)
(memory.grow (i32.const 1))

;; Get current memory size in pages
(memory.size)
```

### Bounds Checking

```
Every memory access is bounds-checked:

load(address) {
  if (address + size > memory.length) {
    trap("out of bounds memory access")
  }
  return memory[address:address+size]
}
```

---

## Execution Model

### Stack Machine

```
Expression: (i32.add (i32.const 1) (i32.const 2))

Stack evolution:
┌───┐     ┌───┐     ┌───┐     ┌───┐
│   │     │ 1 │     │ 2 │     │ 3 │
│   │ →   │   │ →   │ 1 │ →   │   │
│   │     │   │     │   │     │   │
└───┘     └───┘     └───┘     └───┘
 init    push 1    push 2    add → pop 2, push result
```

### Control Flow

```wasm
;; Blocks and branches
(block $outer
  (block $inner
    (br_if $outer (i32.eq (local.get $x) (i32.const 0)))
    ;; code if x != 0
  )
  ;; code after inner block
)

;; Loops
(loop $continue
  ;; loop body
  (br_if $continue (local.get $condition))
)

;; If/else
(if (result i32) (local.get $cond)
  (then (i32.const 1))
  (else (i32.const 0))
)
```

### Traps

Execution stops on:
- Out of bounds memory access
- Divide by zero
- Invalid indirect call
- Stack overflow
- Unreachable instruction

---

## WASI

WebAssembly System Interface — syscalls for WASM outside browsers.

### Why WASI?

```
Problem: WASM has no built-in I/O

Browser:          WASM ← → JavaScript ← → Web APIs
                          (import/export)

Server (WASI):    WASM ← → WASI ← → Operating System
                          (standardized interface)
```

### WASI Preview 1

Original WASI. POSIX-like syscalls.

```wasm
;; WASI imports
(import "wasi_snapshot_preview1" "fd_write"
  (func $fd_write (param i32 i32 i32 i32) (result i32)))

(import "wasi_snapshot_preview1" "fd_read"
  (func $fd_read (param i32 i32 i32 i32) (result i32)))

(import "wasi_snapshot_preview1" "path_open"
  (func $path_open ...))
```

### Capabilities

```bash
# WASI is capability-based (explicit permissions)

# No permissions (sandboxed)
wasmtime app.wasm

# Grant file access
wasmtime --dir=./data app.wasm

# Grant specific paths
wasmtime --dir=/input::/app/input --dir=/output::/app/output app.wasm

# Environment variables
wasmtime --env KEY=value app.wasm

# Network (preview2)
wasmtime --wasi tcp app.wasm
```

### WASI Preview 2

New version based on Component Model.

| Preview 1 | Preview 2 |
|-----------|-----------|
| POSIX-like | Higher-level |
| Functions | Interface types |
| Linear memory passing | Rich types |
| Single module | Components |

---

## Component Model

The future of WASM composition and interop.

### Core Concepts

```
Component Model:
┌─────────────────────────────────────────────────────────┐
│                      Component                           │
│  ┌─────────────────────────────────────────────────┐    │
│  │                   Core Module                    │    │
│  │                   (.wasm)                        │    │
│  └─────────────────────────────────────────────────┘    │
│                         │                               │
│              ┌──────────┴──────────┐                   │
│              ▼                      ▼                   │
│  ┌─────────────────┐    ┌─────────────────┐            │
│  │   WIT Imports   │    │   WIT Exports   │            │
│  │  (interfaces)   │    │  (interfaces)   │            │
│  └─────────────────┘    └─────────────────┘            │
└─────────────────────────────────────────────────────────┘
```

### WIT (WASM Interface Types)

```wit
// http.wit - Define an HTTP interface

package example:http;

interface types {
  record request {
    method: string,
    uri: string,
    headers: list<tuple<string, string>>,
    body: option<list<u8>>,
  }

  record response {
    status: u16,
    headers: list<tuple<string, string>>,
    body: option<list<u8>>,
  }
}

interface handler {
  use types.{request, response};

  handle: func(req: request) -> response;
}

world http-handler {
  export handler;
}
```

### Component Composition

```
┌─────────────────────────────────────────────────────────┐
│                  Composed Component                      │
│                                                          │
│  ┌──────────────┐        ┌──────────────┐              │
│  │  Component A  │───────▶│  Component B  │              │
│  │   (logging)   │        │   (business)  │              │
│  └──────────────┘        └──────┬───────┘              │
│                                  │                       │
│                          ┌──────┴───────┐              │
│                          │  Component C  │              │
│                          │  (database)   │              │
│                          └──────────────┘              │
└─────────────────────────────────────────────────────────┘
```

```bash
# Compose components
wasm-tools compose -o composed.wasm \
  --config config.yaml \
  main.wasm
```

---

## Compiling to WASM

### Rust

```bash
# Add target
rustup target add wasm32-wasip1      # WASI
rustup target add wasm32-unknown-unknown  # Browser

# Build for WASI
cargo build --target wasm32-wasip1 --release

# Build for browser
cargo build --target wasm32-unknown-unknown --release

# With wasm-bindgen (browser interop)
wasm-pack build --target web
```

```rust
// src/lib.rs - Browser WASM with wasm-bindgen
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}
```

### C/C++

```bash
# With Emscripten (browser-focused)
emcc main.c -o main.js -s WASM=1

# With WASI SDK (standalone)
$WASI_SDK/bin/clang main.c -o main.wasm

# Optimization
emcc -O3 -s WASM=1 main.c -o main.wasm
```

### Go

```bash
# Standard Go (larger output)
GOOS=wasip1 GOARCH=wasm go build -o main.wasm

# TinyGo (smaller, better for WASM)
tinygo build -o main.wasm -target=wasi main.go

# For browser
GOOS=js GOARCH=wasm go build -o main.wasm
```

### Zig

```bash
# Build for WASI
zig build-exe main.zig -target wasm32-wasi

# Optimize
zig build-exe main.zig -target wasm32-wasi -O ReleaseFast
```

### Other Languages

| Language | Tool | Notes |
|----------|------|-------|
| AssemblyScript | `asc` | TypeScript-like, WASM-first |
| Kotlin | Kotlin/Wasm | Experimental |
| Swift | SwiftWasm | Community toolchain |
| C# | Blazor, NativeAOT | Browser or WASI |
| Python | Pyodide, py2wasm | Interpreter or compile |
| Ruby | ruby.wasm | Interpreter in WASM |

---

## Toolchain

### Core Tools

| Tool | Purpose |
|------|---------|
| `wasm-tools` | Parse, validate, compose |
| `wasm-opt` | Optimize WASM binaries |
| `wasm2wat` | Binary → text |
| `wat2wasm` | Text → binary |
| `wit-bindgen` | Generate bindings from WIT |
| `wasm-pack` | Rust → npm package |

### wasm-tools

```bash
# Install
cargo install wasm-tools

# Validate
wasm-tools validate app.wasm

# Print info
wasm-tools print app.wasm

# Parse to JSON
wasm-tools dump app.wasm

# Component operations
wasm-tools component new core.wasm -o component.wasm
wasm-tools component wit component.wasm
```

### wasm-opt (Binaryen)

```bash
# Install (via binaryen)
# macOS: brew install binaryen
# Linux: apt install binaryen

# Optimize
wasm-opt -O3 input.wasm -o output.wasm

# Aggressive size optimization
wasm-opt -Oz input.wasm -o output.wasm

# Strip debug info
wasm-opt --strip-debug input.wasm -o output.wasm
```

---

## Browser Usage

### Loading WASM

```javascript
// Modern async approach
const response = await fetch('app.wasm');
const { instance } = await WebAssembly.instantiateStreaming(response);

// Call exported function
const result = instance.exports.add(1, 2);
```

### With JavaScript Glue

```javascript
// Import object provides functions to WASM
const imports = {
  env: {
    log: (ptr, len) => {
      const bytes = new Uint8Array(memory.buffer, ptr, len);
      console.log(new TextDecoder().decode(bytes));
    },
    memory: new WebAssembly.Memory({ initial: 1 })
  }
};

const { instance } = await WebAssembly.instantiateStreaming(
  fetch('app.wasm'),
  imports
);
```

### With wasm-bindgen (Rust)

```javascript
import init, { greet } from './pkg/my_wasm.js';

await init();  // Load and initialize WASM
console.log(greet("World"));  // "Hello, World!"
```

---

## Use Cases

### Plugin Systems

```
Host Application
┌─────────────────────────────────────────────┐
│  ┌─────────────┐  ┌─────────────┐          │
│  │  Plugin A   │  │  Plugin B   │  ...     │
│  │   (.wasm)   │  │   (.wasm)   │          │
│  └─────────────┘  └─────────────┘          │
│         │                │                  │
│         └────────┬───────┘                  │
│                  ▼                          │
│         ┌─────────────────┐                 │
│         │   WASM Runtime  │                 │
│         │  (sandboxed)    │                 │
│         └─────────────────┘                 │
└─────────────────────────────────────────────┘

Benefits:
- Language agnostic plugins
- Sandboxed (can't crash host)
- Portable across platforms
```

**Examples:** Envoy filters, Figma plugins, VS Code extensions

### Edge Computing

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Cloudflare │     │   Fastly    │     │   Fermyon   │
│   Workers    │     │   Compute   │     │    Spin     │
└──────┬──────┘     └──────┬──────┘     └──────┬──────┘
       │                   │                   │
       └───────────────────┴───────────────────┘
                           │
                    ┌──────▼──────┐
                    │  Your WASM  │
                    │   Module    │
                    └─────────────┘

Benefits:
- Cold start < 1ms
- Memory isolation per request
- Same code everywhere
```

### Serverless / FaaS

```rust
// Spin serverless function
use spin_sdk::http::{IntoResponse, Request, Response};
use spin_sdk::http_component;

#[http_component]
fn handle(req: Request) -> anyhow::Result<impl IntoResponse> {
    Ok(Response::builder()
        .status(200)
        .body("Hello from WASM!")
        .build())
}
```

### Embedded / IoT

- Scripting layer in constrained devices
- OTA-updatable logic
- Hardware-agnostic business logic

### Browser Applications

- High-performance web apps
- Porting native apps (Figma, AutoCAD, Photoshop)
- Games (Unity, Unreal)
- Video/audio processing

---

## Performance Tips

### Minimize Host Calls

```
❌ Slow: Many small calls
   for item in items:
       host.process(item)  # Cross-boundary call

✅ Fast: Batch operations
   host.process_all(items)  # Single call
```

### Use Linear Memory Efficiently

```rust
// Pre-allocate in WASM, pass pointer to host
let buffer = vec![0u8; 1024];
let ptr = buffer.as_ptr();
let len = buffer.len();
unsafe { host_fill_buffer(ptr, len); }
```

### Enable SIMD

```rust
// Rust with SIMD
#[target_feature(enable = "simd128")]
fn sum_simd(a: &[f32]) -> f32 {
    // SIMD implementation
}
```

### AOT Compile

```bash
# Wasmtime AOT
wasmtime compile app.wasm -o app.cwasm

# Run pre-compiled
wasmtime run app.cwasm
```

---

## Related

- [[WebAssembly Runtimes]]
- [[Systems Language Performance]]
- [[JavaScript Runtimes]]
- [[Rust]]
