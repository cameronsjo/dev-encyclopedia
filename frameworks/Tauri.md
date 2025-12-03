---
title: Tauri
aliases:
  - Tauri Apps
tags:
  - framework
  - desktop
  - cross-platform
  - rust
  - typescript
  - javascript
  - web
type: reference
status: complete
created: '2025-11-28'
---

# Tauri

Lightweight cross-platform desktop apps with web frontend and Rust backend.

## Overview

| Aspect | Details |
|--------|---------|
| Frontend | Any (React, Vue, Svelte, etc.) |
| Backend | Rust |
| Webview | System (WebView2, WebKit) |
| Platforms | Windows, macOS, Linux, iOS, Android (v2) |
| First stable | 2022 |
| Backing | Tauri Foundation |

---

## Platform Support

| Platform | Status | Webview |
|----------|--------|---------|
| Windows | ✅ Stable | WebView2 (Edge) |
| macOS | ✅ Stable | WebKit |
| Linux | ✅ Stable | WebKitGTK |
| iOS | ✅ v2 | WKWebView |
| Android | ✅ v2 | WebView |

**Tauri 2.0:** Added mobile support.

---

## Why Tauri?

### Size Comparison

| Framework | Bundle Size |
|-----------|-------------|
| Electron | 150-200+ MB |
| Tauri | 1-10 MB |

### Memory Comparison

| Framework | Baseline |
|-----------|----------|
| Electron | 100-300 MB |
| Tauri | 20-50 MB |

**Key difference:** Tauri uses system webview, not bundled Chromium.

---

## Architecture

```
┌─────────────────────────────────────────┐
│              Rust Core                  │
│  (Commands, System APIs, Plugins)       │
└────────────────┬────────────────────────┘
                 │ IPC (JSON-RPC)
                 ▼
┌─────────────────────────────────────────┐
│           System Webview                │
│  (Your Web Frontend - React/Vue/etc)   │
└─────────────────────────────────────────┘
```

### Frontend

Any web framework:

- React, Vue, Svelte, Solid
- Vanilla JS/TS
- Any build tool (Vite recommended)

### Backend

Rust core providing:

- System APIs
- File system access
- Native functionality
- Plugin system

### IPC

Type-safe communication via `@tauri-apps/api`.

---

## Getting Started

### Create App

```bash
npm create tauri-app@latest
# or
cargo create-tauri-app
```

### Project Structure

```
my-app/
├── src/              # Frontend code
│   ├── main.ts
│   └── App.vue
├── src-tauri/        # Rust backend
│   ├── Cargo.toml
│   ├── src/
│   │   └── main.rs
│   └── tauri.conf.json
└── package.json
```

---

## Commands (IPC)

### Define in Rust

```rust
#[tauri::command]
fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

fn main() {
    tauri::Builder::default()
        .invoke_handler(tauri::generate_handler![greet])
        .run(tauri::generate_context!())
        .expect("error running app");
}
```

### Call from Frontend

```typescript
import { invoke } from '@tauri-apps/api/core';

const greeting = await invoke('greet', { name: 'World' });
```

### Async Commands

```rust
#[tauri::command]
async fn fetch_data(url: String) -> Result<String, String> {
    reqwest::get(&url)
        .await
        .map_err(|e| e.to_string())?
        .text()
        .await
        .map_err(|e| e.to_string())
}
```

---

## Core APIs

### File System

```typescript
import { readTextFile, writeTextFile } from '@tauri-apps/plugin-fs';

const content = await readTextFile('path/to/file.txt');
await writeTextFile('path/to/file.txt', 'Hello');
```

### Dialog

```typescript
import { open, save } from '@tauri-apps/plugin-dialog';

const selected = await open({
  multiple: true,
  filters: [{ name: 'Images', extensions: ['png', 'jpg'] }]
});
```

### Shell

```typescript
import { Command } from '@tauri-apps/plugin-shell';

const output = await Command.create('git', ['status']).execute();
```

### Window Management

```typescript
import { getCurrentWindow } from '@tauri-apps/api/window';

const win = getCurrentWindow();
await win.setTitle('New Title');
await win.minimize();
```

---

## Plugins (v2)

Official plugins:

| Plugin | Purpose |
|--------|---------|
| fs | File system access |
| dialog | Native dialogs |
| shell | Execute commands |
| http | HTTP client |
| notification | System notifications |
| clipboard | Copy/paste |
| os | OS information |
| updater | Auto-updates |
| store | Key-value storage |

### Installing Plugins

```bash
npm install @tauri-apps/plugin-fs
cargo add tauri-plugin-fs
```

```rust
// main.rs
fn main() {
    tauri::Builder::default()
        .plugin(tauri_plugin_fs::init())
        .run(tauri::generate_context!())
        .unwrap();
}
```

---

## Security

### Permissions (v2)

Fine-grained capability system.

```json
// capabilities/default.json
{
  "identifier": "default",
  "windows": ["main"],
  "permissions": [
    "fs:read",
    "dialog:open"
  ]
}
```

### Content Security Policy

```json
// tauri.conf.json
{
  "security": {
    "csp": "default-src 'self'; script-src 'self'"
  }
}
```

### No Node in Frontend

Frontend is pure web—no Node.js APIs exposed.

---

## Building

### Development

```bash
npm run tauri dev
```

Hot reload for frontend, rebuilds Rust on change.

### Production

```bash
npm run tauri build
```

### Output

| Platform | Format |
|----------|--------|
| Windows | .msi, .exe (NSIS) |
| macOS | .app, .dmg |
| Linux | .deb, .rpm, .AppImage |

### Cross-Compilation

Limited. Best to build on target platform.

---

## Auto-Updates

```rust
// Check for updates
use tauri_plugin_updater::UpdaterExt;

app.updater_builder().build()?.check().await?;
```

Requires hosting update files (GitHub Releases works).

---

## Tauri vs Electron

| Aspect | Tauri | Electron |
|--------|-------|----------|
| Size | 1-10 MB | 150+ MB |
| Memory | 20-50 MB | 100-300 MB |
| Backend | Rust | Node.js |
| Webview | System | Bundled Chromium |
| Consistency | Varies by webview | Identical everywhere |
| Rust required | Yes (backend) | No |
| Maturity | Growing | Proven |
| Mobile | ✅ v2 | ❌ |

### When Electron Wins

- Need consistent rendering everywhere
- Team doesn't know Rust
- Heavy Node.js ecosystem usage
- Complex native module needs

### When Tauri Wins

- Size/memory matters
- Rust expertise available
- Mobile + desktop needed
- Security-conscious

---

## Tauri 2.0 Features

- **Mobile support** (iOS, Android)
- **Plugin system** overhaul
- **Capabilities** (permission system)
- **Swift/Kotlin** plugins for mobile

---

## When to Use Tauri

**Strengths:**

- Tiny bundle size
- Low memory footprint
- Rust backend (performance, safety)
- Mobile support (v2)
- Strong security model

**Considerations:**

- Rust learning curve
- Webview varies by OS
- Younger ecosystem
- Fewer ready-made solutions

**Best for:**

- Size-conscious apps
- Teams with Rust experience
- Apps needing mobile + desktop
- Security-sensitive applications

---

## Related

- [[Electron]]
- [[Avalonia]]
- [[Rust]]
- [[domains/Desktop Development|Desktop Development]]
