---
title: Desktop Development
aliases:
  - Desktop Apps
  - Native Desktop
tags:
  - domain
  - desktop
  - gui
type: moc
status: complete
created: '2025-11-28'
---

# Desktop Development

Building native and cross-platform desktop applications.

## Platform Overview

| Platform | Native Frameworks | Languages |
|----------|-------------------|-----------|
| Windows | WinUI 3, WPF, WinForms | C#, C++ |
| macOS | SwiftUI, AppKit | Swift, Obj-C |
| Linux | GTK, Qt | C, C++, Rust, Python |

---

## Development Approaches

| Approach | Examples | Trade-off |
|----------|----------|-----------|
| **Native** | WPF, SwiftUI, GTK | Best integration, platform-specific |
| **Cross-platform native** | Qt, .NET MAUI, Avalonia | Native feel, shared code |
| **Web-based** | Electron, Tauri | Web skills, larger size |

---

## Windows

### Framework Evolution

```
WinForms (2002) → WPF (2006) → UWP (2015) → WinUI 3 (2021)
```

### WinForms

**Legacy but alive.** Simple, drag-and-drop design.

**Use when:**

- Maintaining existing apps
- Simple internal tools
- Quick prototypes

**Limitations:**

- GDI+ rendering
- Poor scaling
- Dated look

### WPF (Windows Presentation Foundation)

**XAML-based, vector graphics, data binding.**

**Strengths:**

- Powerful data binding (MVVM)
- Custom styling and templates
- Hardware-accelerated (DirectX)
- Mature ecosystem

**Considerations:**

- Windows only
- Large runtime
- Steeper learning curve

### WinUI 3

**Modern Windows UI.** Fluent Design, latest controls.

**Strengths:**

- Modern look
- Decoupled from OS updates
- Part of Windows App SDK

**Considerations:**

- Windows 10 1809+ only
- Still maturing
- Migration from UWP/WPF needed

---

## macOS

### SwiftUI

**Declarative, modern.** Same framework as iOS.

**Strengths:**

- Code sharing with iOS
- Concise syntax
- Native look automatically

**Considerations:**

- macOS 10.15+
- Some AppKit features still needed

### AppKit

**Mature, full control.** Traditional Cocoa.

**Use when:**

- Complex custom UI
- Pre-10.15 support
- Document-based apps

### Catalyst

**Run iPad apps on Mac.** With modifications.

**Trade-offs:** Quick port, but not truly native feel.

---

## Linux

### GTK

**GNOME toolkit.** C-based, many bindings.

| Binding | Language |
|---------|----------|
| gtk-rs | Rust |
| PyGObject | Python |
| Vala | Vala |

### Qt

**Cross-platform C++ toolkit.** Also Python (PySide/PyQt).

**Strengths:**

- True cross-platform
- Comprehensive widgets
- Commercial support

**Considerations:**

- Licensing (GPL/LGPL/Commercial)
- C++ complexity
- Large footprint

---

## Cross-Platform Native

### .NET MAUI

**Successor to Xamarin.Forms.** C# + XAML.

| Platform | Support |
|----------|---------|
| Windows | WinUI 3 |
| macOS | Mac Catalyst |
| iOS | Native |
| Android | Native |

**Strengths:**

- Single codebase
- Native controls
- .NET ecosystem

**Considerations:**

- Linux not official
- macOS via Catalyst (limitations)

### Avalonia

**Cross-platform WPF.** True cross-platform XAML.

| Platform | Support |
|----------|---------|
| Windows | ✅ |
| macOS | ✅ |
| Linux | ✅ |
| Web (WASM) | ✅ |

**Strengths:**

- WPF developers feel at home
- True Linux support
- Active development

### Qt

See Linux section. Works on Windows, macOS, Linux.

### Compose Multiplatform

**JetBrains.** Kotlin Compose for desktop.

**Platforms:** Windows, macOS, Linux

**Strengths:**

- Kotlin, modern
- Share with Android Compose

---

## Web-Based Desktop

### Electron

**Chromium + Node.js.** Powers VS Code, Slack, Discord.

**Strengths:**

- Full web ecosystem
- Huge community
- Battle-tested

**Considerations:**

- Large bundle (100+ MB)
- High memory usage
- Not truly native

### Tauri

**Web frontend + Rust backend.** System webview.

**Strengths:**

- Small bundles (< 10 MB)
- Low memory
- Rust backend

**Considerations:**

- Webview varies by OS
- Rust learning curve
- Younger ecosystem

### Comparison

| Factor | Electron | Tauri |
|--------|----------|-------|
| Bundle size | 100+ MB | 1-10 MB |
| Memory | High | Low |
| Backend | Node.js | Rust |
| Webview | Bundled Chromium | System |
| Maturity | Proven | Growing |

---

## Architecture Patterns

### MVVM

**Model-View-ViewModel.** Standard for XAML apps.

- View: UI (XAML)
- ViewModel: UI logic, state
- Model: Business logic, data

Works with: WPF, WinUI, MAUI, Avalonia

### MVU / Elm

**Model-View-Update.** Unidirectional data flow.

Works with: Fabulous (.NET), Iced (Rust)

---

## Comparison Matrix

| Framework | Platforms | Language | Size | Native Feel |
|-----------|-----------|----------|------|-------------|
| WPF | Windows | C# | Medium | Excellent |
| WinUI 3 | Windows | C# | Medium | Excellent |
| SwiftUI | macOS/iOS | Swift | Small | Excellent |
| AppKit | macOS | Swift | Small | Excellent |
| GTK | Linux+ | C/Rust/Python | Small | Good |
| Qt | All | C++/Python | Large | Good |
| .NET MAUI | Win/Mac/Mobile | C# | Medium | Good |
| Avalonia | All | C# | Medium | Good |
| Electron | All | JS/TS | Large | Web |
| Tauri | All | Rust + Web | Small | Web |

---

## Decision Guide

| Scenario | Recommendation |
|----------|----------------|
| Windows-only, modern | WinUI 3 |
| Windows-only, existing WPF | Stay WPF |
| macOS-only | SwiftUI |
| True cross-platform, .NET | Avalonia |
| Cross-platform, native controls | .NET MAUI or Qt |
| Web team, cross-platform | Tauri (small) or Electron (mature) |
| Linux native | GTK or Qt |

---

## Related

- [[Mobile Development]]
- [[Web Development]]
- [[Build Systems]]
- [[Cross-Compilation]]
