---
title: WebAssembly Runtimes
aliases:
  - WASM Runtimes
  - Wasm
tags:
  - tool
  - comparison
  - wasm
  - runtime
type: comparison
status: complete
created: 2025-12-04
---

# WebAssembly Runtimes

Standalone runtimes for executing WebAssembly outside the browser.

## Overview

| Runtime | Written In | WASI | Component Model | Focus |
|---------|------------|:----:|:---------------:|-------|
| Wasmtime | Rust | ✅ | ✅ | Reference, production |
| Wasmer | Rust | ✅ | ✅ | Universal, ecosystem |
| WasmEdge | C++ | ✅ | ✅ | Cloud-native, AI |
| WAMR | C | ✅ | Partial | Embedded, IoT |
| wazero | Go | ✅ | ❌ | Zero dependencies |
| Spin | Rust | ✅ | ✅ | Serverless apps |

---

## What is WASM?

WebAssembly (WASM) is a binary instruction format designed as a portable compilation target.

```
Source Code → Compiler → .wasm → Runtime → Execution

┌─────────────────────────────────────────────────────┐
│                    Languages                         │
│   Rust, C, C++, Go, Zig, AssemblyScript, etc.       │
└──────────────────────┬──────────────────────────────┘
                       ▼
              ┌─────────────────┐
              │   .wasm binary  │
              └────────┬────────┘
                       ▼
┌─────────────────────────────────────────────────────┐
│                    Runtimes                          │
│   Browser, Wasmtime, Wasmer, WasmEdge, etc.         │
└─────────────────────────────────────────────────────┘
```

### Key Properties

| Property | Description |
|----------|-------------|
| Portable | Same binary runs anywhere |
| Sandboxed | No direct access to host by default |
| Fast | Near-native execution speed |
| Language-agnostic | Compile from many languages |
| Compact | Small binary size |

---

## WASI

WebAssembly System Interface — standardized syscall interface for WASM outside browsers.

```
┌─────────────────────────────────────────────────┐
│               WASM Module                        │
│  ┌───────────────────────────────────────────┐  │
│  │           Application Code                 │  │
│  └───────────────────────────────────────────┘  │
│                      │                          │
│                      ▼                          │
│  ┌───────────────────────────────────────────┐  │
│  │              WASI Imports                  │  │
│  │   fd_read, fd_write, path_open, etc.      │  │
│  └───────────────────────────────────────────┘  │
└──────────────────────┬──────────────────────────┘
                       ▼
┌─────────────────────────────────────────────────┐
│              WASI Implementation                 │
│            (provided by runtime)                 │
└─────────────────────────────────────────────────┘
```

### WASI Capabilities

```bash
# Grant file system access
wasmtime --dir=/data app.wasm

# Grant network access (preview2)
wasmtime --wasi tcp app.wasm

# Environment variables
wasmtime --env KEY=value app.wasm
```

---

## Wasmtime

Bytecode Alliance reference runtime. Production-grade.

### Key Characteristics

- **Standards-first** — Reference WASI implementation
- **Component Model** — Full support
- **Cranelift** — Fast JIT/AOT compiler
- **Embeddings** — Rust, C, Python, Go, .NET, etc.
- **Production** — Used by Shopify, Fastly, Fermyon

### Usage

```bash
# Install
curl https://wasmtime.dev/install.sh -sSf | bash

# Run WASM module
wasmtime hello.wasm

# With WASI capabilities
wasmtime --dir=. --env NAME=World app.wasm

# AOT compilation
wasmtime compile app.wasm -o app.cwasm
wasmtime run app.cwasm
```

### Embedding (Rust)

```rust
use wasmtime::*;

fn main() -> Result<()> {
    let engine = Engine::default();
    let module = Module::from_file(&engine, "hello.wasm")?;

    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[])?;

    let hello = instance.get_typed_func::<(), ()>(&mut store, "hello")?;
    hello.call(&mut store, ())?;

    Ok(())
}
```

### Strengths

- Most spec-compliant
- Excellent documentation
- Component Model leader
- Strong embedding story
- Bytecode Alliance backing

---

## Wasmer

Universal runtime with focus on developer experience.

### Key Characteristics

- **Multiple compilers** — Singlepass, Cranelift, LLVM
- **Package registry** — wasmer.io/registry (WAPM)
- **Wasmer Edge** — Deployment platform
- **Broad language support** — Many embedding options

### Usage

```bash
# Install
curl https://get.wasmer.io -sSfL | sh

# Run WASM module
wasmer run hello.wasm

# Run from registry
wasmer run python/python

# Create package
wasmer init
wasmer publish
```

### Compiler Options

| Compiler | Speed | Optimization | Use Case |
|----------|-------|--------------|----------|
| Singlepass | Fastest compile | Minimal | JIT, quick iteration |
| Cranelift | Fast compile | Good | General purpose |
| LLVM | Slow compile | Best | Maximum performance |

```bash
# Choose compiler
wasmer run --singlepass app.wasm
wasmer run --cranelift app.wasm
wasmer run --llvm app.wasm
```

### Embedding (Python)

```python
from wasmer import engine, Store, Module, Instance

store = Store(engine.JIT())
module = Module(store, open('hello.wasm', 'rb').read())
instance = Instance(module)

result = instance.exports.add(5, 37)
print(result)  # 42
```

### Strengths

- Great developer experience
- Package registry
- Multiple compiler backends
- Edge deployment platform
- Extensive language bindings

---

## WasmEdge

Cloud-native WebAssembly runtime optimized for edge and AI.

### Key Characteristics

- **Cloud-native** — Kubernetes, Docker integration
- **AI inference** — TensorFlow, PyTorch, llama.cpp
- **WASI-NN** — Neural network extension
- **Networking** — Async I/O, HTTP, sockets
- **Languages** — Rust, C, Go, JS, Python

### Usage

```bash
# Install
curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash

# Run WASM
wasmedge hello.wasm

# With reactor mode (library)
wasmedge --reactor lib.wasm function_name arg1 arg2

# With AI extension
wasmedge --dir .:. wasmedge-nn.wasm
```

### Docker/Kubernetes Integration

```dockerfile
# Use WasmEdge in Docker
FROM scratch
COPY app.wasm /app.wasm
ENTRYPOINT ["/app.wasm"]
```

```bash
# Run WASM container with Docker
docker run --runtime=io.containerd.wasmedge.v1 myapp:latest
```

### AI Workloads

```rust
// Using WASI-NN for inference
use wasi_nn::{Graph, GraphExecutionContext, TensorType};

let graph = Graph::load(&model_bytes, GraphEncoding::Ggml)?;
let context = graph.init_execution_context()?;
context.set_input(0, TensorType::F32, &input_dims, &input_data)?;
context.compute()?;
```

### Strengths

- Best container/K8s integration
- AI/ML focus with WASI-NN
- Async networking
- Active development
- Edge computing focus

---

## WAMR

WebAssembly Micro Runtime — for embedded and IoT.

### Key Characteristics

- **Tiny footprint** — ~100KB runtime
- **Interpreter + JIT + AOT** — Multiple modes
- **Real-time** — Deterministic execution
- **Embedded** — MCUs, IoT devices
- **Intel backing** — Production tested

### Execution Modes

| Mode | Memory | Speed | Use Case |
|------|--------|-------|----------|
| Interpreter | Tiny | Slower | Very constrained devices |
| Fast JIT | Small | Fast | Resource-limited |
| LLVM JIT | Larger | Fastest | Performance critical |
| AOT | Tiny runtime | Fastest | Production deployment |

### Usage

```bash
# Run with interpreter
iwasm app.wasm

# AOT compile
wamrc -o app.aot app.wasm

# Run AOT
iwasm app.aot
```

### Strengths

- Smallest footprint
- Real-time capable
- Multiple execution modes
- Battle-tested at Intel
- Ideal for embedded

---

## wazero

Pure Go WebAssembly runtime. Zero dependencies.

### Key Characteristics

- **Pure Go** — No CGO, no dependencies
- **Portable** — Anywhere Go runs
- **Embedder-friendly** — Simple Go API
- **Compliant** — WASI preview1

### Usage (Go)

```go
import (
    "context"
    "github.com/tetratelabs/wazero"
    "github.com/tetratelabs/wazero/imports/wasi_snapshot_preview1"
)

func main() {
    ctx := context.Background()

    // Create runtime
    r := wazero.NewRuntime(ctx)
    defer r.Close(ctx)

    // Instantiate WASI
    wasi_snapshot_preview1.MustInstantiate(ctx, r)

    // Load and run module
    wasm, _ := os.ReadFile("app.wasm")
    mod, _ := r.Instantiate(ctx, wasm)

    // Call function
    fn := mod.ExportedFunction("hello")
    fn.Call(ctx)
}
```

### Strengths

- Zero dependencies
- Go-native experience
- Cross-compilation friendly
- Simple embedding
- Compliant implementation

---

## Spin

Serverless application framework built on WASM.

### Key Characteristics

- **Framework** — Not just runtime
- **Components** — WASM component model
- **Triggers** — HTTP, Redis, timers
- **Multi-language** — Rust, Go, JS, Python, etc.
- **Fermyon Cloud** — Deployment platform

### Usage

```bash
# Install
curl -fsSL https://developer.fermyon.com/downloads/install.sh | bash

# Create new app
spin new -t http-rust hello-spin
cd hello-spin

# Build
spin build

# Run locally
spin up

# Deploy to Fermyon Cloud
spin deploy
```

### Application Structure

```rust
// Rust component
use spin_sdk::http::{IntoResponse, Request, Response};
use spin_sdk::http_component;

#[http_component]
fn handle_request(req: Request) -> anyhow::Result<impl IntoResponse> {
    Ok(Response::builder()
        .status(200)
        .body("Hello from Spin!")
        .build())
}
```

### Strengths

- Full framework, not just runtime
- Component Model native
- Sub-millisecond cold starts
- Multi-language support
- Serverless platform included

---

## Feature Comparison

| Feature | Wasmtime | Wasmer | WasmEdge | WAMR | wazero |
|---------|:--------:|:------:|:--------:|:----:|:------:|
| WASI Preview 1 | ✅ | ✅ | ✅ | ✅ | ✅ |
| WASI Preview 2 | ✅ | ✅ | ✅ | Partial | ❌ |
| Component Model | ✅ | ✅ | ✅ | ❌ | ❌ |
| JIT compilation | ✅ | ✅ | ✅ | ✅ | ❌ |
| AOT compilation | ✅ | ✅ | ✅ | ✅ | ❌ |
| Interpreter | ❌ | ❌ | ❌ | ✅ | ✅ |
| Async I/O | ✅ | ✅ | ✅ | ❌ | ❌ |
| Docker/K8s | Via containerd | Wasmer Edge | ✅ Native | ❌ | ❌ |
| AI/NN extensions | ❌ | ❌ | ✅ | ❌ | ❌ |

---

## Use Cases

| Use Case | Best Runtimes |
|----------|---------------|
| Reference/standards | Wasmtime |
| Developer experience | Wasmer |
| Cloud/Kubernetes | WasmEdge |
| Embedded/IoT | WAMR |
| Go applications | wazero |
| Serverless apps | Spin |
| AI inference | WasmEdge |
| Browser + Server | All support |
| Plugin systems | Wasmtime, Wasmer, wazero |

---

## Compiling to WASM

### Rust

```bash
# Add target
rustup target add wasm32-wasip1

# Build
cargo build --target wasm32-wasip1 --release

# Output: target/wasm32-wasip1/release/app.wasm
```

### Go

```bash
# Build with TinyGo (recommended for WASM)
tinygo build -o app.wasm -target=wasi main.go

# Or standard Go (larger output)
GOOS=wasip1 GOARCH=wasm go build -o app.wasm
```

### C/C++

```bash
# With Emscripten
emcc app.c -o app.wasm

# With WASI SDK
$WASI_SDK/bin/clang app.c -o app.wasm
```

### Other Languages

| Language | Compiler/Tool |
|----------|---------------|
| AssemblyScript | `asc` (TypeScript-like) |
| Zig | `zig build-exe -target wasm32-wasi` |
| Swift | SwiftWasm |
| Kotlin | Kotlin/Wasm |
| C# | Blazor, NativeAOT-LLVM |

---

## Decision Guide

| Need | Recommendation |
|------|----------------|
| Standards compliance | Wasmtime |
| Best developer UX | Wasmer |
| Kubernetes/containers | WasmEdge |
| Embedded systems | WAMR |
| Go project, simple embedding | wazero |
| Serverless applications | Spin |
| AI inference at edge | WasmEdge |
| Production, battle-tested | Wasmtime or Wasmer |
| Smallest footprint | WAMR |

---

## Related

- [[JavaScript Runtimes]]
- [[Container Runtimes]]
- [[Deployment]]
