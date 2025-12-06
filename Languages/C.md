---
title: C
aliases:
  - C Programming Language
  - ANSI C
  - ISO C
tags:
  - language
  - systems
  - low-level
  - procedural
type: reference
status: complete
created: 2025-11-30
---

# C

A low-level, procedural programming language providing direct memory access and minimal abstraction over hardware, designed for systems programming and portable assembly.

## Overview

| Aspect | Details |
|--------|---------|
| **Paradigm** | Procedural, imperative |
| **Typing** | Static, weak, manifest |
| **Memory Model** | Manual (malloc/free) |
| **First Appeared** | 1972 (Dennis Ritchie) |
| **Current Standard** | C23 (ISO/IEC 9899:2023) |
| **Primary Use Cases** | Operating systems, embedded systems, drivers, performance-critical code |
| **Toolchains** | GCC, Clang/LLVM, MSVC, ICC |
| **Standard Library** | libc (glibc, musl, uclibc, newlib) |

## Core Concepts

### Memory Management

**Manual Allocation** — Programmer controls all memory operations.

- `malloc(size)` — Allocates heap memory, returns pointer or NULL
- `calloc(count, size)` — Allocates zeroed memory
- `realloc(ptr, size)` — Resizes allocation
- `free(ptr)` — Deallocates memory (MUST match every malloc)
- Stack allocation — Automatic variables, freed on scope exit

**Ownership** — No language-level enforcement. Programmer tracks:
- Who owns allocated memory
- When to free (double-free is undefined behavior)
- Avoiding use-after-free

### Pointers

**Direct Memory Addresses** — Core abstraction for indirection and dynamic structures.

- `int *ptr` — Pointer to int
- `*ptr` — Dereference (access value)
- `&var` — Address-of operator
- `ptr[i]` — Array indexing (equivalent to `*(ptr + i)`)
- `void *` — Generic pointer, requires casting
- Pointer arithmetic — Adding/subtracting moves by sizeof(type)
- Function pointers — `int (*func)(char *)` for callbacks

**Null Pointers** — `NULL` (0) indicates invalid pointer. Dereferencing causes undefined behavior.

### Structs and Unions

**Structs** — Aggregate data types, members laid out in memory order.

```c
struct Point {
    int x;
    int y;
};

struct Point p = {10, 20};
struct Point *ptr = &p;
ptr->x = 30;  // Arrow operator for pointer access
```

**Unions** — Multiple members share same memory location.

```c
union Value {
    int i;
    float f;
    char c;
};  // Size = max(sizeof members)
```

**Memory Layout** — Padding inserted for alignment. Use `sizeof()` to get actual size, not sum of members.

### Preprocessor

**Text Substitution** — Runs before compilation, operates on source text.

- `#include <header.h>` — Insert file contents
- `#define NAME value` — Macro definition
- `#ifdef`, `#ifndef`, `#endif` — Conditional compilation
- `#pragma` — Compiler-specific directives
- Macro functions — `#define MAX(a,b) ((a) > (b) ? (a) : (b))`

**Include Guards** — Prevent multiple inclusion:

```c
#ifndef HEADER_H
#define HEADER_H
// declarations
#endif
```

### Undefined Behavior

**Actions with No Specified Outcome** — Compiler may assume UB never occurs, leading to surprising optimizations.

**Common Sources:**
- Dereferencing null/invalid pointers
- Out-of-bounds array access
- Signed integer overflow
- Uninitialized variable use
- Use-after-free
- Double-free
- Data races (without synchronization)
- Violating aliasing rules (strict aliasing)
- Modifying string literals

**Detection:** Use sanitizers (ASan, UBSan, MSan) during development.

## C Standards

| Standard | Year | Key Features |
|----------|------|--------------|
| **K&R C** | 1978 | Original "The C Programming Language" book |
| **C89/C90** | 1989 | First ANSI/ISO standard, function prototypes |
| **C99** | 1999 | `//` comments, `bool`, VLAs, inline, designated initializers, `long long` |
| **C11** | 2011 | Threads, atomics, `_Static_assert`, anonymous structs/unions, `_Noreturn` |
| **C17/C18** | 2018 | Bug fixes, no new features |
| **C23** | 2023 | `typeof`, `constexpr`, `nullptr`, `bool`/`true`/`false` keywords, `#embed`, `[[attributes]]` |

**Portability** — Older standards (C89, C99) have wider compiler support. Embedded systems often use C99 or earlier.

## Common Pitfalls

| Issue | Description | Prevention |
|-------|-------------|------------|
| **Buffer Overflows** | Writing past array bounds | Bounds checking, `strncpy` over `strcpy`, AddressSanitizer |
| **Memory Leaks** | Allocating without freeing | Valgrind, ownership discipline, RAII patterns |
| **Dangling Pointers** | Accessing freed memory | Set to NULL after free, avoid returning stack addresses |
| **Uninitialized Variables** | Reading before writing | Initialize at declaration, MemorySanitizer |
| **Integer Overflow** | Signed overflow is UB | Use unsigned, check before operations, `-ftrapv` |
| **Implicit Conversions** | Silent type changes | Enable warnings (`-Wall -Wextra -Wconversion`) |
| **Null Termination** | Forgetting `\0` in strings | Use `strlen`, `strnlen`, allocate `n+1` for n chars |
| **Macro Side Effects** | `MAX(x++, y++)` evaluates args multiple times | Use functions or `_Generic` macros |

## Tooling

### Compilers

| Tool | Description |
|------|-------------|
| **GCC** | GNU Compiler Collection, industry standard, excellent optimization |
| **Clang** | LLVM-based, faster compilation, better diagnostics than GCC |
| **MSVC** | Microsoft Visual C++, Windows-focused, non-standard extensions |
| **ICC** | Intel C Compiler, optimized for Intel CPUs |
| **TCC** | Tiny C Compiler, extremely fast compilation, minimal optimization |

**Recommended Flags:**
- `-Wall -Wextra -Wpedantic` — Comprehensive warnings
- `-Werror` — Treat warnings as errors
- `-std=c99` / `-std=c11` — Specify standard
- `-O2` / `-O3` — Optimization levels
- `-g` — Debug symbols

### Debugging and Analysis

| Tool | Purpose |
|------|---------|
| **Valgrind** | Memory error detection, leak checking (slow but thorough) |
| **AddressSanitizer (ASan)** | Fast memory error detection (`-fsanitize=address`) |
| **MemorySanitizer (MSan)** | Uninitialized memory reads (`-fsanitize=memory`) |
| **UndefinedBehaviorSanitizer (UBSan)** | Detects UB at runtime (`-fsanitize=undefined`) |
| **ThreadSanitizer (TSan)** | Data race detection (`-fsanitize=thread`) |
| **GDB / LLDB** | Interactive debuggers |
| **Static Analyzers** | Clang Static Analyzer, cppcheck, Coverity |

**Best Practice:** Develop with sanitizers enabled, run Valgrind before releases.

### Build Systems

- **Make** — Classic, ubiquitous, declarative dependencies
- **CMake** — Cross-platform, generates native build files
- **Meson** — Modern, fast, Python-based
- **Autotools** — Portable configuration (configure, make, make install)

## Comparison with Alternatives

| Feature | C | C++ | Rust |
|---------|---|-----|------|
| **Memory Safety** | ❌ Manual, UB common | ❌ Manual (RAII helps) | ✅ Borrow checker enforced |
| **Abstraction Cost** | ✅ Minimal | ⚠️ Can be zero-cost | ✅ Zero-cost abstractions |
| **Simplicity** | ✅ Small language | ❌ Extremely complex | ⚠️ Moderate complexity |
| **Compile Times** | ✅ Very fast | ❌ Slow (templates) | ⚠️ Moderate to slow |
| **Learning Curve** | ✅ Shallow syntax | ❌ Steep | ⚠️ Steep (ownership) |
| **Portability** | ✅ Runs everywhere | ✅ Very wide | ⚠️ Growing support |
| **ABI Stability** | ✅ Stable, simple | ⚠️ Complex mangling | ⚠️ No stable ABI yet |
| **OOP Support** | ❌ None (manual vtables) | ✅ Native classes | ⚠️ Traits, no inheritance |
| **Standard Library** | ⚠️ Minimal | ✅ Extensive (STL) | ✅ Modern, safe |
| **Tooling Maturity** | ✅ Decades of tools | ✅ Excellent | ✅ Cargo ecosystem |
| **Interop** | ✅ De facto FFI standard | ✅ C-compatible | ✅ C FFI built-in |

### Key Distinctions

**C vs C++:**
- C is not a subset of C++ (subtle incompatibilities)
- C lacks classes, exceptions, templates, namespaces, references
- C++ has RAII (destructors), reducing manual cleanup
- C code often compiles as C++ but semantics may differ

**C vs Rust:**
- Both aim for zero-cost abstractions
- Rust prevents memory errors at compile time (borrow checker)
- C requires external tools (sanitizers, Valgrind) to catch errors
- Rust has no garbage collection overhead (like C)
- C has simpler build process and faster compilation

## When to Use C

### Strengths

| Strength | Rationale |
|----------|-----------|
| **System-Level Control** | Direct hardware access, inline assembly, memory-mapped I/O |
| **Predictable Performance** | No hidden allocations, no runtime, deterministic |
| **Minimal Runtime** | Tiny footprint, suitable for resource-constrained embedded systems |
| **Universal ABI** | FFI lingua franca for all languages |
| **Portability** | Compilers for every architecture, from microcontrollers to supercomputers |
| **Longevity** | Code from 1980s still compiles and runs |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Manual Memory Management** | Requires discipline, prone to leaks and corruption |
| **No Safety Guarantees** | UB is easy to trigger, hard to debug |
| **Limited Abstractions** | Verbose data structures, no generics, manual polymorphism |
| **String Handling** | Cumbersome, error-prone (null termination) |
| **Concurrency** | No built-in safety (pre-C11), data races are UB |

### Best For

- **Operating System Kernels** — Linux, BSD, Windows kernel components
- **Embedded Systems** — Microcontrollers, firmware, real-time systems
- **Device Drivers** — Hardware abstraction layers
- **Language Runtimes** — Interpreters (CPython, Ruby MRI), VMs (JVM hotspots)
- **Performance Libraries** — NumPy core, image processing, compression
- **Legacy Codebases** — Maintaining existing C projects
- **FFI Layers** — Exposing libraries to other languages

**Avoid For:**
- Application-level software (prefer C++, Rust, higher-level languages)
- Projects prioritizing safety over control (use Rust)
- Rapid prototyping (Python, Go, JavaScript better suited)

## Related

- [[C++]] — Object-oriented extension with RAII and STL
- [[Rust]] — Memory-safe systems language with borrow checker
- [[Embedded & Systems]] — Primary domain for C development
- [[Languages MOC]] — Overview of all language references
