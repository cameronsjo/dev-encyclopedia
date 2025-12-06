---
title: Build Systems
aliases:
  - Build Tools
  - Toolchains
  - Compilers
tags:
  - comparison
  - languages
  - tools
  - build
  - compilation
  - csharp
  - go
  - python
  - typescript
  - rust
type: comparison
status: complete
created: '2025-11-28'
---

# Build Systems

Cross-language comparison of build systems, compilers, bundlers, and toolchains.

## Overview

| Language | Build Tool | Package Manager | Compiler/Runtime |
|----------|------------|-----------------|------------------|
| C# | MSBuild / dotnet CLI | NuGet | Roslyn → CLR |
| Go | go build | go mod | gc compiler |
| Python | setuptools / hatch | pip / uv | CPython interpreter |
| TypeScript | tsc + bundler | npm / pnpm | V8 (Node) or browser |
| Rust | Cargo | Cargo (integrated) | rustc → LLVM |

---

## Concepts

### Compilation Models

| Model | Languages | How It Works |
|-------|-----------|--------------|
| **AOT (Ahead of Time)** | Rust, Go, C# (Native AOT) | Compile to native binary before execution |
| **JIT (Just in Time)** | C# (.NET), Java | Compile to IL, then to native at runtime |
| **Transpilation** | TypeScript | Convert to another language (JS), then run |
| **Interpretation** | Python | Execute source directly (with bytecode cache) |

**AOT advantages:** Fast startup, small footprint, no runtime needed.

**JIT advantages:** Runtime optimization, cross-platform IL, faster compilation.

### Build Phases

```mermaid
graph LR
    A[Source] --> B[Parse/Lint]
    B --> C[Type Check]
    C --> D[Compile/Transpile]
    D --> E[Link/Bundle]
    E --> F[Output]
```

| Phase | What Happens |
|-------|--------------|
| Parse | Read source, build AST |
| Lint | Style and quality checks |
| Type Check | Verify type correctness |
| Compile | Generate intermediate or machine code |
| Link | Combine modules, resolve dependencies |
| Bundle | Package for distribution |

### Incremental Builds

**What:** Only rebuild what changed.

**Why:** Full rebuilds are slow. Incremental saves time.

| Language | Incremental Support |
|----------|---------------------|
| C# | ✅ MSBuild caches |
| Go | ✅ Package-level |
| Python | N/A (interpreted) |
| TypeScript | ✅ tsc --incremental |
| Rust | ✅ Cargo caches |

---

## Language-Specific Toolchains

### C# — dotnet CLI + MSBuild

**Components:**

- **Roslyn** — Compiler, produces IL
- **MSBuild** — Build orchestration
- **NuGet** — Package management
- **dotnet CLI** — Unified interface

**Build modes:**

| Mode | Output | Use Case |
|------|--------|----------|
| Debug | IL + PDB symbols | Development |
| Release | Optimized IL | Production |
| Native AOT | Native binary | CLI tools, containers |

**Key files:**

- `.csproj` — Project definition (MSBuild XML)
- `Directory.Build.props` — Shared settings across projects
- `global.json` — SDK version pinning

### Go — go build

**Philosophy:** Simple, fast, no configuration files.

**Components:**

- **gc** — Official Go compiler
- **go mod** — Dependency management
- **go build** — Compilation
- **go test** — Testing

**Key features:**

- Cross-compilation via `GOOS` and `GOARCH`
- Static linking by default
- Fast compilation (designed for it)
- No makefile needed

**Key files:**

- `go.mod` — Module definition
- `go.sum` — Dependency checksums

### Python — Various

**The fragmentation:** Python has multiple build/packaging tools.

| Tool | Purpose | Status |
|------|---------|--------|
| setuptools | Traditional packaging | Legacy |
| pip | Package installer | Standard |
| poetry | Modern dependency management | Popular |
| uv | Fast pip replacement | Emerging |
| hatch | Modern project management | Growing |
| pyproject.toml | Unified config | Standard |

**Recommendation:** Use `uv` for speed, `pyproject.toml` for config.

**Key files:**

- `pyproject.toml` — Modern config standard
- `requirements.txt` — Legacy dependency list
- `setup.py` — Legacy build script

### TypeScript — tsc + Bundlers

**Compilation:** TypeScript → JavaScript (transpilation).

**Components:**

- **tsc** — TypeScript compiler
- **Bundler** — Combines JS modules for distribution

**Bundler landscape:**

| Bundler | Speed | Config | Use Case |
|---------|-------|--------|----------|
| esbuild | ⚡ Fastest | Minimal | Simple builds, libraries |
| Vite | ⚡ Fast | Moderate | Frontend apps |
| Webpack | 🐢 Slow | Complex | Legacy, complex needs |
| Rollup | Medium | Moderate | Libraries |
| Turbopack | ⚡ Fast | Minimal | Next.js (emerging) |

**Recommendation:** Vite for apps, esbuild for libraries.

**Key files:**

- `tsconfig.json` — TypeScript config
- `package.json` — Dependencies and scripts
- `vite.config.ts` / `webpack.config.js` — Bundler config

### Rust — Cargo

**The gold standard.** Unified build, test, package, publish.

**Components:**

- **rustc** — Compiler (LLVM backend)
- **Cargo** — Build system + package manager
- **crates.io** — Package registry

**Key features:**

- Incremental compilation
- Parallel compilation
- Built-in test runner
- Built-in doc generator
- Workspaces for monorepos

**Build profiles:**

| Profile | Optimization | Debug Info | Use |
|---------|--------------|------------|-----|
| dev | Low | Full | Development |
| release | High | None | Production |

**Key files:**

- `Cargo.toml` — Project definition
- `Cargo.lock` — Exact dependency versions

---

## Build Speed Comparison

| Language | Cold Build | Incremental | Why |
|----------|------------|-------------|-----|
| Go | ⚡ Fast | ⚡ Fast | Designed for speed |
| Rust | 🐢 Slow | Medium | Deep optimization |
| C# | Medium | ⚡ Fast | Good caching |
| TypeScript | Depends on bundler | Fast | tsc is fast, bundlers vary |
| Python | N/A | N/A | Interpreted |

**Rust build times:** The common complaint. Mitigations:

- Use `cargo check` for type checking without full build
- Enable incremental compilation (default in dev)
- Use `sccache` for shared compilation cache
- Consider `mold` linker for faster linking

---

## Monorepo Support

| Language | Tool | How |
|----------|------|-----|
| C# | MSBuild | `Directory.Build.props`, solution files |
| Go | Go workspaces | `go.work` file |
| TypeScript | pnpm / nx / turborepo | Workspace configs |
| Rust | Cargo workspaces | `[workspace]` in Cargo.toml |
| Python | Weak | Manual or poetry plugins |

---

## Task Runners

Beyond compilation—running scripts, tests, deployments.

| Tool | Language | Notes |
|------|----------|-------|
| Make | Any | Universal, but arcane syntax |
| Just | Any | Modern make alternative |
| Task | Any | YAML-based, Go-written |
| npm scripts | TS/JS | Built into package.json |
| cargo-make | Rust | Extends Cargo |

---

## CI/CD Considerations

| Factor | Recommendation |
|--------|----------------|
| Caching | Cache dependencies and build artifacts |
| Parallelism | Use language-native parallel builds |
| Docker | Multi-stage builds for smaller images |
| Matrix builds | Test across OS/version combinations |
| Incremental | CI systems often start fresh—expect cold builds |

---

## Decision Guide

| Priority | Recommendation |
|----------|----------------|
| Fastest builds | Go |
| Best tooling UX | Rust (Cargo) |
| Most flexible | TypeScript (pick your bundler) |
| Enterprise ecosystem | C# (MSBuild) |
| Simplest setup | Go (zero config) |

---

## Related

- [[Cross-Compilation]]
- [[Runtimes]]
- [[Deployment]]
- [[Testing Frameworks]]
