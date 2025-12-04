---
title: JavaScript Runtimes
aliases:
  - JS Runtimes
  - Node vs Deno vs Bun
tags:
  - tool
  - comparison
  - javascript
  - typescript
  - runtime
type: comparison
status: complete
created: 2025-12-04
---

# JavaScript Runtimes

Server-side JavaScript/TypeScript execution environments: Node.js, Deno, and Bun.

## Overview

| Aspect | Node.js | Deno | Bun |
|--------|---------|------|-----|
| First release | 2009 | 2020 | 2022 |
| Created by | Ryan Dahl | Ryan Dahl | Jarred Sumner |
| Written in | C++ | Rust | Zig |
| Engine | V8 | V8 | JavaScriptCore |
| TypeScript | Via transpiler | Native | Native |
| Package manager | npm/yarn/pnpm | Built-in (+ npm) | Built-in (+ npm) |
| Security model | Full access | Permissions-based | Full access |
| Config files | package.json, tsconfig, etc. | deno.json (optional) | bunfig.toml (optional) |

---

## Node.js

The original server-side JavaScript runtime. Massive ecosystem.

### Key Characteristics

- **Mature ecosystem** — npm has 2M+ packages
- **CommonJS + ESM** — Supports both module systems
- **Event loop** — libuv-based async I/O
- **Wide adoption** — Most tutorials, SO answers, production deployments

### Package Management

```bash
# npm (default)
npm install express

# yarn
yarn add express

# pnpm (disk-efficient)
pnpm add express
```

### Module Systems

```javascript
// CommonJS (traditional)
const express = require('express');
module.exports = { handler };

// ESM (modern, add "type": "module" to package.json)
import express from 'express';
export { handler };
```

### Considerations

- Fragmented tooling (bundlers, transpilers, test runners)
- `node_modules` can be massive
- TypeScript requires build step
- Permission model added in v20+ but not default

---

## Deno

"A secure runtime for JavaScript and TypeScript" — addresses Node.js design regrets.

### Key Characteristics

- **Secure by default** — Explicit permissions for fs, net, env
- **TypeScript native** — No config needed
- **Standard library** — Audited, versioned std lib
- **URL imports** — Import directly from URLs
- **Web-compatible** — Uses web platform APIs (fetch, Web Crypto)

### Permissions Model

```bash
# No permissions (sandboxed)
deno run script.ts

# Explicit permissions
deno run --allow-read --allow-net script.ts

# Allow specific paths/hosts
deno run --allow-read=/data --allow-net=api.example.com script.ts

# All permissions (not recommended)
deno run --allow-all script.ts
```

### Import Styles

```typescript
// URL imports (original style)
import { serve } from "https://deno.land/std@0.208.0/http/server.ts";

// Import maps (deno.json)
import { serve } from "@std/http";

// npm compatibility
import express from "npm:express@4";
```

### Built-in Tools

```bash
deno fmt          # Formatter
deno lint         # Linter
deno test         # Test runner
deno bench        # Benchmarking
deno compile      # Create executable
deno doc          # Documentation generator
deno jupyter      # Jupyter kernel
```

### Considerations

- Smaller ecosystem (but npm compat helps)
- Different mental model from Node
- Some npm packages don't work
- Less hosting/deployment support

---

## Bun

"All-in-one JavaScript runtime & toolkit" — focused on speed.

### Key Characteristics

- **Speed** — JavaScriptCore + Zig = fast startup, fast runtime
- **All-in-one** — Runtime + bundler + transpiler + package manager
- **Node compatible** — Drop-in replacement for many Node apps
- **Native TypeScript** — No build step
- **Fast package manager** — Installs packages 10-100x faster than npm

### Performance Focus

```
Bun optimizes for:
├── Startup time (faster than Node/Deno)
├── Runtime performance (JSC optimizations)
├── Package install speed
├── Bundling speed
└── Test execution speed
```

### Package Management

```bash
bun install              # Install from package.json (very fast)
bun add express          # Add package
bun remove lodash        # Remove package
bun update              # Update packages
```

### Built-in APIs

```typescript
// Fast file I/O
const file = Bun.file("./data.json");
const contents = await file.json();

// Built-in SQLite
import { Database } from "bun:sqlite";
const db = new Database("mydb.sqlite");

// Fast HTTP server
Bun.serve({
  port: 3000,
  fetch(req) {
    return new Response("Hello!");
  },
});
```

### Built-in Tools

```bash
bun run script.ts    # Run (no build needed)
bun test             # Test runner (Jest-compatible)
bun build            # Bundler
bunx                 # Like npx
```

### Considerations

- Youngest runtime, still maturing
- Some Node APIs not yet implemented
- Smaller community
- Less battle-tested in production

---

## Feature Comparison

| Feature | Node.js | Deno | Bun |
|---------|:-------:|:----:|:---:|
| TypeScript native | | ✅ | ✅ |
| Permissions system | Opt-in | ✅ Default | |
| npm compatibility | ✅ Native | ✅ Via npm: | ✅ Native |
| Built-in test runner | ✅ (v18+) | ✅ | ✅ |
| Built-in bundler | | | ✅ |
| Built-in formatter | | ✅ | |
| Web APIs (fetch, etc.) | ✅ (v18+) | ✅ | ✅ |
| Single executable | ✅ (experimental) | ✅ | ✅ |
| JSX support | Via bundler | ✅ | ✅ |
| Watch mode | ✅ --watch | ✅ --watch | ✅ --watch |

---

## Performance Comparison

| Benchmark | Node.js | Deno | Bun |
|-----------|---------|------|-----|
| Startup time | Baseline | ~Similar | Faster |
| HTTP throughput | Good | Good | Excellent |
| Package install | Slow (npm) | Medium | Very fast |
| Cold start | Medium | Medium | Fast |
| Memory usage | Medium | Medium | Lower |

*Note: Benchmarks vary by workload. Always test your specific use case.*

---

## Ecosystem & Tooling

### Node.js Ecosystem

```
Node.js requires external tools:
├── TypeScript → tsc, ts-node, tsx
├── Bundler → webpack, esbuild, rollup, vite
├── Test → jest, mocha, vitest
├── Lint → eslint
├── Format → prettier
└── Types → @types/* packages
```

### Deno Ecosystem

```
Deno includes:
├── TypeScript → built-in
├── Bundler → deno bundle (deprecated, use esbuild)
├── Test → deno test
├── Lint → deno lint
├── Format → deno fmt
└── Types → built-in
```

### Bun Ecosystem

```
Bun includes:
├── TypeScript → built-in
├── Bundler → bun build
├── Test → bun test
├── Lint → use external (eslint, biome)
├── Format → use external (prettier, biome)
└── Types → built-in
```

---

## Migration Paths

### Node → Deno

```typescript
// Change imports
// Before (Node)
import fs from 'fs';
import path from 'path';

// After (Deno)
import * as fs from "@std/fs";
import * as path from "@std/path";
// Or use Node compat
import fs from "node:fs";
```

### Node → Bun

```bash
# Often just works
bun run your-node-script.ts

# Replace npm scripts in package.json
bun run dev
bun test
```

---

## Decision Guide

| Use Case | Recommendation |
|----------|----------------|
| Enterprise, maximum stability | Node.js |
| Security-critical, sandboxing needed | Deno |
| Maximum performance, fast iteration | Bun |
| Existing Node.js codebase | Node.js or Bun |
| New TypeScript project | Deno or Bun |
| Edge/serverless functions | Bun or Deno |
| Scripts and tooling | Bun (fast startup) |
| Learning JavaScript server-side | Node.js (most resources) |

### Summary

| Runtime | Choose When |
|---------|-------------|
| **Node.js** | Need stability, ecosystem breadth, hiring, documentation |
| **Deno** | Want security-first, clean design, TypeScript-native, std lib |
| **Bun** | Want speed, all-in-one tooling, minimal config, Node compat |

---

## Related

- [[Build Systems]]
- [[TypeScript]]
- [[Deployment]]
- [[Testing Frameworks]]
