---
title: Cross-Compilation
aliases:
  - Cross-Platform Builds
  - Multi-Architecture
tags:
  - comparison
  - languages
  - tools
  - build
  - deployment
  - csharp
  - go
  - python
  - typescript
  - rust
type: comparison
status: complete
created: '2025-11-28'
---

# Cross-Compilation

Building software on one platform to run on another.

## Why Cross-Compile?

- **Distribution:** Build for Windows, macOS, Linux from single CI
- **ARM support:** Build for Raspberry Pi, Apple Silicon, AWS Graviton
- **Embedded:** Target embedded systems from development machine
- **Mobile:** Build native libraries for iOS/Android

---

## Terminology

| Term | Meaning |
|------|---------|
| **Host** | Machine where compilation runs |
| **Target** | Machine where binary will run |
| **Triple** | `arch-vendor-os[-abi]` identifier |
| **Sysroot** | Target system's headers and libraries |
| **Toolchain** | Compiler, linker, and tools for a target |

**Example triples:**

- `x86_64-unknown-linux-gnu` — 64-bit Linux with glibc
- `aarch64-apple-darwin` — Apple Silicon macOS
- `x86_64-pc-windows-msvc` — 64-bit Windows MSVC

---

## Language Comparison

| Language | Cross-Compile | Difficulty | Native Deps |
|----------|---------------|------------|-------------|
| Go | ✅ Built-in | Easy | CGO complicates |
| Rust | ✅ Via targets | Medium | Need linker |
| C# | ✅ RID-based | Easy | .NET handles it |
| Python | N/A | N/A | Interpreted |
| TypeScript | N/A | N/A | Interpreted |

---

## Go — The Easiest

Go cross-compilation is trivial:

```bash
GOOS=linux GOARCH=amd64 go build
GOOS=darwin GOARCH=arm64 go build
GOOS=windows GOARCH=amd64 go build
```

**Supported targets:**

- `GOOS`: linux, darwin, windows, freebsd, android, ios, etc.
- `GOARCH`: amd64, arm64, arm, 386, wasm, etc.

**The catch: CGO**

Pure Go cross-compiles trivially. CGO (C bindings) requires:

- Cross-compiler for target
- Target's C libraries
- `CGO_ENABLED=1` + `CC=target-gcc`

**Recommendation:** Avoid CGO if cross-compilation matters. Pure Go alternatives exist for most needs.

---

## Rust — Powerful but More Setup

**Add target:**

```bash
rustup target add x86_64-unknown-linux-gnu
rustup target add aarch64-apple-darwin
```

**Build:**

```bash
cargo build --target x86_64-unknown-linux-gnu
```

**The catch: Linker**

Rust compiles to object files easily. Linking needs target's linker.

**Solutions:**

| Approach | How |
|----------|-----|
| Install cross-linker | `apt install gcc-aarch64-linux-gnu` |
| Use `cross` tool | Docker-based, handles everything |
| Use `cargo-zigbuild` | Zig as cross-linker |
| CI per platform | Build natively on each OS |

**`cross` (recommended for Linux targets):**

```bash
cargo install cross
cross build --target x86_64-unknown-linux-gnu
```

Uses Docker images with pre-configured toolchains.

**Zig as linker (elegant):**

```bash
cargo install cargo-zigbuild
cargo zigbuild --target x86_64-unknown-linux-gnu
```

Zig bundles cross-compilation toolchains.

---

## C# — RID System

.NET uses Runtime Identifiers (RIDs).

**Common RIDs:**

- `linux-x64`, `linux-arm64`
- `osx-x64`, `osx-arm64`
- `win-x64`, `win-arm64`

**Publish for target:**

```bash
dotnet publish -r linux-x64 --self-contained
dotnet publish -r osx-arm64 --self-contained
```

**Native AOT cross-compilation:**

Limited. Generally need to build on target OS.

- Linux → Linux (different arch): Works
- macOS → macOS (different arch): Works
- Cross-OS: Usually needs target OS

**Recommendation:** Use CI matrix to build on each OS natively.

---

## Python & TypeScript

Interpreted languages don't cross-compile. The interpreter runs on the target.

**Distribution challenges:**

| Language | Challenge | Solution |
|----------|-----------|----------|
| Python | Interpreter + deps | Docker, pyinstaller, nuitka |
| TypeScript | Node.js runtime | Docker, pkg, nexe, Deno compile |

**Deno compile (TypeScript):**

```bash
deno compile --target x86_64-unknown-linux-gnu app.ts
```

Creates standalone binary with runtime embedded.

---

## Native Dependencies

The hard part of cross-compilation.

**Problem:** Your code links to C libraries (OpenSSL, SQLite, etc.). These must be compiled for target.

**Solutions:**

| Approach | Trade-off |
|----------|-----------|
| Static linking | Larger binary, but no runtime deps |
| Vendored deps | Include source, compile for target |
| Docker images | Pre-built environments |
| Build on target | Use CI matrix |
| Avoid native deps | Use pure-language alternatives |

**Go example:** Use `crypto/tls` (pure Go) instead of OpenSSL.

**Rust example:** Use `rustls` instead of `openssl-sys`.

---

## CI/CD Strategies

### Matrix Builds

Build natively on each platform:

```yaml
# GitHub Actions
jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
    runs-on: ${{ matrix.os }}
    steps:
      - run: cargo build --release
```

**Pros:** Simplest, most reliable.
**Cons:** Slower, more CI minutes.

### Cross-Compilation in CI

Build all targets from one runner:

```yaml
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - run: |
          rustup target add x86_64-pc-windows-gnu
          rustup target add aarch64-unknown-linux-gnu
          cargo build --target x86_64-pc-windows-gnu
          cargo build --target aarch64-unknown-linux-gnu
```

**Pros:** Faster, single environment.
**Cons:** More complex setup.

### Docker Multiplatform

Build images for multiple architectures:

```bash
docker buildx build --platform linux/amd64,linux/arm64 -t myapp:latest .
```

Uses QEMU emulation or native nodes.

---

## Common Targets

### Desktop

| Target | Triple (Rust) | RID (C#) | GOOS/GOARCH |
|--------|---------------|----------|-------------|
| Linux x64 | x86_64-unknown-linux-gnu | linux-x64 | linux/amd64 |
| Linux ARM64 | aarch64-unknown-linux-gnu | linux-arm64 | linux/arm64 |
| macOS x64 | x86_64-apple-darwin | osx-x64 | darwin/amd64 |
| macOS ARM64 | aarch64-apple-darwin | osx-arm64 | darwin/arm64 |
| Windows x64 | x86_64-pc-windows-msvc | win-x64 | windows/amd64 |

### Embedded / IoT

| Target | Notes |
|--------|-------|
| ARM Cortex-M | `thumbv7em-none-eabihf` (Rust) |
| Raspberry Pi | `arm-unknown-linux-gnueabihf` or `aarch64` |
| ESP32 | Requires specific toolchain |

### WebAssembly

| Target | Use |
|--------|-----|
| `wasm32-unknown-unknown` | Browser, raw WASM |
| `wasm32-wasi` | WASI runtimes |

---

## Troubleshooting

| Problem | Cause | Fix |
|---------|-------|-----|
| Missing linker | No cross-toolchain | Install or use `cross`/zigbuild |
| Missing headers | No sysroot | Use Docker or install SDK |
| ABI mismatch | Wrong libc | Match musl vs glibc |
| Dynamic lib issues | Runtime deps missing | Static link or ship libs |
| Code signing (macOS) | Need Apple cert | Sign after build or use CI mac |

---

## Best Practices

| Practice | Why |
|----------|-----|
| Test on real target | Emulation can hide issues |
| Prefer static linking | Fewer runtime surprises |
| Use CI matrix for releases | Native builds are reliable |
| Pin toolchain versions | Reproducible builds |
| Minimize native deps | Simplifies cross-compilation |

---

## Related

- [[Build Systems]]
- [[Runtimes]]
- [[Deployment]]
