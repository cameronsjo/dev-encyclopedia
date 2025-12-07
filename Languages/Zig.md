---
title: Zig
aliases:
  - Zig Language
  - ziglang
tags:
  - language
  - systems
  - low-level
  - compiled
type: reference
status: complete
created: 2025-11-30
---

# Zig

A general-purpose programming language emphasizing robustness, optimality, and maintainability through compile-time execution and explicit control flow.

## Overview

| Aspect | Details |
|--------|---------|
| **Paradigm** | Imperative, procedural, with compile-time metaprogramming |
| **Typing** | Static, strong, with compile-time type inference |
| **Memory Model** | Manual with explicit allocators, no garbage collection |
| **First Release** | 2016 (Andrew Kelley) |
| **License** | MIT |
| **Platform Support** | Linux, macOS, Windows, BSD, embedded systems |
| **Compilation** | LLVM-based with self-hosted compiler in development |
| **Key Features** | Comptime, no hidden control flow, optional types, error unions, allocator awareness |

## Core Philosophy

**Explicit over Implicit**
No hidden control flow, allocations, or exceptions. Every operation is visible in the code.

**Compile-Time Execution**
`comptime` allows arbitrary code execution during compilation for zero-cost abstractions.

**Incremental Improvement over C**
Modern language features while maintaining C-level control and ABI compatibility.

**Robustness**
Built-in safety checks, explicit error handling, and optional types prevent common bugs.

## Language Features

### Comptime

Compile-time code execution for generics, code generation, and zero-cost abstractions:

```zig
fn max(comptime T: type, a: T, b: T) T {
    return if (a > b) a else b;
}

comptime {
    // Runs at compile time
    const size = calculateOptimalBufferSize();
}
```

### No Hidden Control Flow

- No operator overloading
- No exceptions (use error unions)
- No implicit type coercion
- No hidden memory allocations
- No preprocessor macros

### Optional Types

Built-in null safety using `?T` syntax:

```zig
const maybe_value: ?i32 = null;

if (maybe_value) |value| {
    // value is i32, not ?i32
} else {
    // handle null case
}
```

### Error Handling

Error unions (`!T`) for explicit, type-safe error propagation:

```zig
const FileOpenError = error{
    AccessDenied,
    FileNotFound,
};

fn openFile(path: []const u8) FileOpenError!File {
    return error.FileNotFound;
}

// Usage
const file = try openFile("data.txt"); // propagate error
const file = openFile("data.txt") catch |err| {
    // handle error
};
```

### Explicit Allocators

All allocations require an explicit allocator parameter:

```zig
const allocator = std.heap.page_allocator;
const list = try std.ArrayList(i32).init(allocator);
defer list.deinit(); // manual cleanup

// No hidden allocations - always visible
```

### C Interop

First-class C integration without FFI overhead:

```zig
const c = @cImport({
    @cInclude("stdio.h");
});

pub fn main() void {
    c.printf("Hello from Zig\n");
}
```

## Build System

**build.zig**
Native build system using Zig code instead of external tools:

```zig
const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "myapp",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = b.standardTargetOptions(.{}),
        .optimize = b.standardOptimizeOption(.{}),
    });
    b.installArtifact(exe);
}
```

**Cross-Compilation**
Built-in cross-compilation to any supported target without additional toolchains:

```bash
zig build-exe main.zig -target x86_64-windows
zig build-exe main.zig -target aarch64-linux
zig build-exe main.zig -target wasm32-freestanding
```

**C/C++ Compilation**
Can compile C/C++ code with better error messages and cross-compilation:

```bash
zig cc -o myapp main.c  # Drop-in Clang replacement
zig c++ -o myapp main.cpp
```

## Safety Features

| Feature | Description |
|---------|-------------|
| **Bounds Checking** | Array/slice access validated at runtime (Debug/ReleaseSafe) |
| **Integer Overflow** | Detected in Debug/ReleaseSafe modes |
| **Use-After-Free** | Prevented through explicit ownership and defer |
| **Null Safety** | Optional types (`?T`) make null explicit |
| **Error Handling** | Errors cannot be ignored without explicit `try` or `catch` |
| **Undefined Behavior** | Detected in safe builds, explicit `@setRuntimeSafety(false)` to disable |

**Build Modes:**

- `Debug` - All safety checks, no optimizations
- `ReleaseSafe` - Optimized with safety checks
- `ReleaseFast` - Maximum speed, minimal safety
- `ReleaseSmall` - Optimize for binary size

## Standard Library

**Key Modules:**

- `std.mem` - Memory operations, allocators
- `std.fs` - Filesystem operations
- `std.io` - Input/output
- `std.json` - JSON parsing/serialization
- `std.ArrayList` - Dynamic arrays
- `std.HashMap` - Hash maps
- `std.Thread` - Threading primitives
- `std.crypto` - Cryptographic primitives
- `std.testing` - Testing framework

**Testing:**

```zig
test "example test" {
    const result = add(2, 3);
    try std.testing.expectEqual(5, result);
}
```

## Comparison with Other Languages

| Aspect | Zig | C | Rust | Go |
|--------|-----|---|------|-----|
| **Memory Safety** | Manual + optional runtime checks | Manual | Borrow checker (compile-time) | GC |
| **Error Handling** | Error unions | Return codes | Result type | Multiple returns + panic |
| **Generics** | Comptime | Macros/void* | Traits | Interfaces |
| **C Interop** | ✅ Native, zero-cost | ✅ N/A | ⚠️ Requires FFI | ⚠️ Requires cgo |
| **Build System** | ✅ Built-in (build.zig) | ❌ External (Make, CMake) | ✅ Cargo | ✅ go build |
| **Cross-Compilation** | ✅ Trivial | ⚠️ Complex | ⚠️ Requires setup | ✅ Built-in |
| **Runtime** | ❌ None | ❌ None | ❌ Minimal | ✅ Large (GC, scheduler) |
| **Learning Curve** | Medium | Low | High | Low |
| **Compile Speed** | Fast | Very Fast | Slow | Very Fast |
| **Binary Size** | Small | Very Small | Medium | Large |
| **Maturity** | ⚠️ Pre-1.0 | ✅ Mature | ✅ Mature | ✅ Mature |

## When to Use Zig

### Strengths

- **Systems programming** requiring fine control without C footguns
- **C integration** projects benefiting from modern language features
- **Cross-platform** development with trivial cross-compilation
- **Embedded systems** needing explicit resource control
- **Performance-critical** code with predictable behavior
- **Replacing build systems** (Zig as C/C++ compiler)

### Considerations

- Pre-1.0 status means breaking changes between releases
- Smaller ecosystem compared to C/C++/Rust
- Manual memory management requires discipline
- No async/await (event loop patterns required)
- Limited IDE tooling compared to mature languages

### Best For

| Use Case | Why Zig |
|----------|---------|
| **Systems Programming** | C-level control with modern safety features |
| **Embedded Development** | Explicit allocations, no runtime, tiny binaries |
| **Game Engines** | Fine-grained performance control, comptime optimizations |
| **CLI Tools** | Fast compilation, static binaries, cross-compilation |
| **C Codebases** | Gradual migration path, excellent C interop |
| **Performance Libraries** | Zero-cost abstractions via comptime |

### Not Ideal For

- Production systems requiring long-term stability guarantees (pre-1.0)
- Projects needing extensive third-party libraries
- Teams unfamiliar with manual memory management
- Rapid prototyping where GC languages excel
- Applications requiring mature async/await ecosystems

## Related

- [[C]] - Spiritual predecessor, excellent interop
- [[Rust]] - Alternative systems language with different safety approach
- [[Go]] - Alternative with runtime/GC tradeoffs
- [[Domains/Embedded & Systems]] - Primary domain for Zig
- [[C++]] - Language Zig aims to replace in many contexts
