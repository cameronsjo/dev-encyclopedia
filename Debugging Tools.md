---
title: Debugging Tools
aliases:
  - Debuggers
  - Profilers
tags:
  - comparison
  - languages
  - tools
  - debugging
  - profiling
  - csharp
  - go
  - python
  - typescript
  - rust
type: comparison
status: complete
created: 2025-11-28
---

# Debugging Tools

Cross-language comparison of debuggers, profilers, and diagnostic tools.

## Overview

| Language | Debugger | Profiler | Memory |
|----------|----------|----------|--------|
| C# | Visual Studio, VS Code | dotnet-trace, PerfView | dotnet-dump, dotnet-gcdump |
| Go | Delve | pprof | pprof (heap) |
| Python | pdb, debugpy | cProfile, py-spy | memory_profiler, tracemalloc |
| TypeScript | Chrome DevTools, VS Code | Chrome DevTools, clinic.js | Chrome DevTools |
| Rust | lldb, gdb, VS Code | perf, flamegraph | valgrind, heaptrack |

---

## Debugging Approaches

### Print Debugging

The universal technique. Add log statements, observe output.

**When to use:** Quick checks, production debugging (via logging).

**Limitations:** Modifies code, no interactivity, hard to inspect complex state.

### Interactive Debugging

Pause execution, inspect state, step through code.

**When to use:** Complex bugs, understanding unfamiliar code, inspecting data structures.

**Key features:**
- Breakpoints (conditional, hit count)
- Step in/over/out
- Watch expressions
- Call stack inspection
- Variable modification

### Time-Travel Debugging

Record execution, replay backwards.

**Tools:**
- C#: VS Enterprise IntelliTrace
- Rust: rr (Linux)
- JS: Replay.io

**When to use:** Intermittent bugs, race conditions, understanding complex flows.

---

## Language-Specific Notes

### C# — Best-in-Class Tooling

Visual Studio's debugger is exceptional.

**Tools:**
| Tool | Purpose |
|------|---------|
| Visual Studio Debugger | Full-featured IDE debugging |
| VS Code + C# Dev Kit | Cross-platform debugging |
| dotnet-trace | Collect traces |
| dotnet-dump | Analyze crash dumps |
| dotnet-counters | Real-time metrics |
| PerfView | Deep performance analysis |

**Hot reload:** Edit code while debugging. Works for many scenarios.

**Diagnostics APIs:** Built-in EventSource, Activity for custom tracing.

### Go — Delve

The standard Go debugger. Works with VS Code, GoLand, CLI.

**Key features:**
- Goroutine-aware
- Conditional breakpoints
- Core dump analysis
- Remote debugging

**pprof:** Built-in profiling. CPU, memory, goroutine, block profiles.

**Flame graphs:** `go tool pprof -http=:8080 profile.pb.gz`

**Tracing:** `go tool trace` for execution tracer. Shows goroutine scheduling.

### Python — pdb and Beyond

**pdb:** Built-in, basic. Works anywhere.

**debugpy:** VS Code debugger backend. Better UX.

**ipdb:** pdb with IPython features.

**Profiling:**
| Tool | What |
|------|------|
| cProfile | Built-in CPU profiler |
| py-spy | Sampling profiler (no code change) |
| line_profiler | Line-by-line timing |
| memory_profiler | Memory by line |
| tracemalloc | Built-in memory tracking |

**py-spy is excellent:** Attach to running process, low overhead, flame graphs.

### TypeScript/JavaScript — Browser DevTools

Chrome DevTools is remarkably powerful.

**Features:**
- Sources panel: Breakpoints, stepping
- Performance panel: CPU profiling, flame charts
- Memory panel: Heap snapshots, allocation timeline
- Network panel: Request inspection

**Node.js:**
- `--inspect` flag enables debugging
- VS Code attaches seamlessly
- `node --prof` for V8 profiler

**clinic.js:** Node performance toolkit (doctor, flame, bubbleprof).

### Rust — LLVM Toolchain

Uses lldb (macOS/Linux) or gdb (Linux).

**VS Code:** CodeLLDB extension provides good experience.

**Challenges:**
- Optimized code hard to debug
- Async code complex to step through
- Consider `cargo build` without release for debugging

**Profiling:**
| Tool | Platform | Use |
|------|----------|-----|
| perf | Linux | CPU profiling |
| flamegraph | Cross | Visualization |
| cargo-instruments | macOS | Instruments integration |
| heaptrack | Linux | Memory profiling |
| valgrind | Linux | Memory errors, leaks |

---

## Profiling Types

### CPU Profiling

**What:** Where is time spent?

**Sampling vs Instrumentation:**
| Method | How | Overhead |
|--------|-----|----------|
| Sampling | Periodically check stack | Low |
| Instrumentation | Measure every function | High |

**Output:** Flame graphs show call hierarchy with time.

### Memory Profiling

**What:** Where is memory allocated? Are there leaks?

**Key metrics:**
- Heap size over time
- Allocation rate
- Object counts by type
- Retained size

### Concurrency Profiling

**What:** Lock contention, goroutine/thread issues.

**Tools:**
- Go: Block profiler, mutex profiler
- C#: Concurrency Visualizer (VS Enterprise)
- Rust: tracing + Tokio Console

---

## Common Debugging Scenarios

### Memory Leaks

**Symptoms:** Growing memory over time.

**Approach:**
1. Take heap snapshots at intervals
2. Compare object counts
3. Find objects that shouldn't be retained
4. Trace retention path to root

### Performance Regression

**Approach:**
1. Profile before/after change
2. Compare flame graphs
3. Identify new hot paths
4. Benchmark specific functions

### Race Conditions

**Hard to reproduce.** Strategies:
- Add delays to increase timing window
- Use thread sanitizers (TSAN)
- Time-travel debugging
- Structured logging with timestamps

### Production Issues

**Can't attach debugger.** Rely on:
- Structured logging
- Distributed tracing
- Metrics
- Core dumps for post-mortem

---

## IDE Integration

| IDE | C# | Go | Python | TypeScript | Rust |
|-----|----|----|--------|------------|------|
| VS Code | ✅ | ✅ | ✅ | ✅ | ✅ |
| Visual Studio | ✅ Best | ❌ | ✅ | ✅ | ❌ |
| JetBrains | ✅ Rider | ✅ GoLand | ✅ PyCharm | ✅ WebStorm | ✅ RustRover |

**VS Code:** Universal, good for all languages with extensions.

**JetBrains:** Best experience per language, but multiple IDEs.

---

## Remote Debugging

Debug processes running elsewhere.

| Language | Method |
|----------|--------|
| C# | VS Remote Debugger, SSH |
| Go | Delve headless mode |
| Python | debugpy remote |
| Node.js | `--inspect=0.0.0.0:9229` |
| Rust | gdbserver, lldb-server |

**Security:** Never expose debug ports in production.

---

## Best Practices

| Practice | Why |
|----------|-----|
| Debug builds for debugging | Optimizations break debuggers |
| Reproduce locally first | Easier than remote |
| Minimize reproduction case | Faster iteration |
| Check logs before debugging | Often faster |
| Profile before optimizing | Don't guess |
| Use conditional breakpoints | Avoid stepping through loops |
| Sanitizers in CI | Catch memory/thread issues early |

---

## Decision Guide

| Scenario | Approach |
|----------|----------|
| Quick check | Print/log debugging |
| Complex bug | Interactive debugger |
| Performance issue | CPU profiler + flame graph |
| Memory leak | Heap snapshots |
| Production issue | Logs + traces + metrics |
| Intermittent bug | Increased logging, time-travel if available |

---

## Related

- [[Testing Frameworks]]
- [[Logging Libraries]]
- [[OpenTelemetry]]
- [[Distributed Tracing]]
