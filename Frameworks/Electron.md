---
title: Electron
aliases:
  - Electron.js
tags:
  - framework
  - desktop
  - cross-platform
  - typescript
  - javascript
  - web
type: reference
status: complete
created: '2025-11-28'
---

# Electron

Build cross-platform desktop apps with web technologies.

## Overview

| Aspect | Details |
|--------|---------|
| Languages | JavaScript/TypeScript, HTML, CSS |
| Rendering | Chromium |
| Platforms | Windows, macOS, Linux |
| Architecture | Main + Renderer processes |
| First release | 2013 (Atom Shell) |
| Backing | OpenJS Foundation |

---

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| Windows | ✅ Stable | Primary target |
| macOS | ✅ Stable | Primary target |
| Linux | ✅ Stable | AppImage, deb, rpm |

---

## Notable Apps

- VS Code
- Slack
- Discord
- Figma (desktop)
- Notion
- Obsidian
- 1Password
- Microsoft Teams

---

## Architecture

### Process Model

```
┌─────────────────────────────────────────┐
│              Main Process               │
│  (Node.js - system access, windows)     │
└────────────────┬────────────────────────┘
                 │ IPC
    ┌────────────┼────────────┐
    ▼            ▼            ▼
┌────────┐  ┌────────┐  ┌────────┐
│Renderer│  │Renderer│  │Renderer│
│(Chrome)│  │(Chrome)│  │(Chrome)│
└────────┘  └────────┘  └────────┘
```

### Main Process

- One per app
- Node.js environment
- Creates windows
- System APIs (file, network)
- App lifecycle

### Renderer Process

- One per window
- Chromium web page
- Sandboxed (by default)
- Communicates via IPC

### Preload Scripts

Bridge between main and renderer (secure).

```javascript
// preload.js
const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('electronAPI', {
  sendMessage: (msg) => ipcRenderer.send('message', msg),
  onReply: (callback) => ipcRenderer.on('reply', callback)
});
```

---

## IPC Communication

### Renderer → Main

```javascript
// Renderer
window.electronAPI.sendMessage('Hello');

// Main
ipcMain.on('message', (event, msg) => {
  console.log(msg);
  event.reply('reply', 'Got it');
});
```

### Main → Renderer

```javascript
// Main
mainWindow.webContents.send('update', data);

// Renderer (preload exposed)
window.electronAPI.onUpdate((data) => { ... });
```

### Invoke Pattern (Request/Response)

```javascript
// Renderer
const result = await window.electronAPI.doSomething(args);

// Main
ipcMain.handle('doSomething', async (event, args) => {
  return await processArgs(args);
});
```

---

## Security

### Context Isolation

Separate JavaScript contexts for preload and renderer.

```javascript
new BrowserWindow({
  webPreferences: {
    contextIsolation: true,  // Default in Electron 12+
    nodeIntegration: false,  // Don't expose Node to renderer
    preload: path.join(__dirname, 'preload.js')
  }
});
```

### Security Checklist

| Practice | Why |
|----------|-----|
| Enable context isolation | Prevent prototype pollution |
| Disable nodeIntegration | Limit attack surface |
| Use preload scripts | Controlled API exposure |
| Validate IPC inputs | Prevent injection |
| Enable sandbox | Process isolation |
| CSP headers | Prevent XSS |

---

## Key APIs

### BrowserWindow

```javascript
const win = new BrowserWindow({
  width: 1200,
  height: 800,
  webPreferences: { preload: ... }
});

win.loadFile('index.html');
// or
win.loadURL('https://example.com');
```

### Dialog

```javascript
const { dialog } = require('electron');

const result = await dialog.showOpenDialog({
  properties: ['openFile', 'multiSelections'],
  filters: [{ name: 'Images', extensions: ['jpg', 'png'] }]
});
```

### Menu

```javascript
const template = [
  {
    label: 'File',
    submenu: [
      { label: 'New', accelerator: 'CmdOrCtrl+N', click: () => {} },
      { type: 'separator' },
      { role: 'quit' }
    ]
  }
];
Menu.setApplicationMenu(Menu.buildFromTemplate(template));
```

### Tray

```javascript
const tray = new Tray('/path/to/icon.png');
tray.setContextMenu(contextMenu);
tray.setToolTip('My App');
```

### Notification

```javascript
new Notification({
  title: 'Hello',
  body: 'World'
}).show();
```

---

## Building & Packaging

### Electron Forge

Official tooling.

```bash
npm init electron-app@latest my-app
npx electron-forge make
```

### Electron Builder

Community alternative, more options.

```bash
npm install electron-builder --save-dev
npx electron-builder
```

### Output Formats

| Platform | Formats |
|----------|---------|
| Windows | exe, msi, NSIS, portable |
| macOS | dmg, pkg, mas |
| Linux | AppImage, deb, rpm, snap |

### Auto-Updates

```javascript
const { autoUpdater } = require('electron-updater');

autoUpdater.checkForUpdatesAndNotify();

autoUpdater.on('update-downloaded', () => {
  autoUpdater.quitAndInstall();
});
```

---

## Performance

### The Elephant in the Room

| Concern | Reality |
|---------|---------|
| Bundle size | 150-200+ MB (includes Chromium) |
| Memory | 100-300+ MB base |
| Startup | Slower than native |
| CPU | Full browser engine |

### Mitigation

| Strategy | Benefit |
|----------|---------|
| Lazy loading | Faster perceived startup |
| Code splitting | Load only needed code |
| Minimize dependencies | Smaller bundle |
| Profile & optimize | Find actual bottlenecks |

---

## Frameworks on Electron

| Framework | Purpose |
|-----------|---------|
| Electron Forge | Scaffolding, building |
| electron-vite | Vite integration |
| electron-react-boilerplate | React template |
| Electron Fiddle | Experimentation |

### With Web Frameworks

Use any web framework:

- React
- Vue
- Angular
- Svelte
- Solid

---

## Electron vs Tauri

| Aspect | Electron | Tauri |
|--------|----------|-------|
| Size | 150+ MB | 1-10 MB |
| Memory | High | Low |
| Backend | Node.js | Rust |
| Webview | Bundled Chromium | System webview |
| Maturity | Proven (VS Code, Slack) | Growing |
| Node.js APIs | Full access | Via Rust |
| Community | Massive | Growing |

---

## When to Use Electron

**Strengths:**

- Web team can build desktop
- Consistent cross-platform
- Massive ecosystem
- Proven at scale (VS Code)
- Full Node.js access

**Considerations:**

- Large bundle size
- High memory usage
- Not truly native feel
- Security requires attention

**Best for:**

- Complex desktop apps
- Web app ports
- Teams with web expertise
- Apps where resources aren't critical

---

## Related

- [[Tauri]]
- [[Domains/Desktop Development|Desktop Development]]
- [[TypeScript]]
- [[React]]
