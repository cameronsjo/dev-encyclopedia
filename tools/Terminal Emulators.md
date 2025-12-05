---
title: Terminal Emulators
aliases:
  - Terminals
  - Terminal Apps
tags:
  - tool
  - comparison
  - terminal
  - cli
type: comparison
status: complete
created: 2025-12-04
---

# Terminal Emulators

Modern terminal applications for running shells and CLI programs.

## Overview

| Terminal | Platform | GPU Accel | Written In | License |
|----------|----------|:---------:|------------|---------|
| Ghostty | macOS, Linux | ✅ | Zig | MIT |
| Alacritty | Cross-platform | ✅ | Rust | Apache 2.0 |
| Kitty | macOS, Linux | ✅ | Python/C | GPL-3.0 |
| Warp | macOS, Linux | ✅ | Rust | Proprietary |
| iTerm2 | macOS | ❌ | Objective-C | GPL-2.0 |
| Windows Terminal | Windows | ✅ | C++ | MIT |
| Hyper | Cross-platform | ❌ | Electron | MIT |
| WezTerm | Cross-platform | ✅ | Rust | MIT |

---

## Ghostty

New GPU-accelerated terminal by Mitchell Hashimoto (HashiCorp founder).

### Key Characteristics

- **Native** — Platform-native UI (macOS Cocoa, GTK on Linux)
- **Fast** — Zig + GPU acceleration
- **Zero config** — Works great out of box
- **Ligatures** — Full font ligature support
- **Minimal** — Focused feature set

### Configuration

```
# ~/.config/ghostty/config

font-family = JetBrains Mono
font-size = 14
theme = catppuccin-mocha

window-padding-x = 10
window-padding-y = 10

cursor-style = block
cursor-style-blink = false
```

### Strengths

- Extremely fast rendering
- Native look and feel
- Simple configuration
- Low resource usage
- Active development

### Considerations

- Newer, smaller ecosystem
- No Windows support yet
- Fewer features than Kitty/WezTerm

---

## Alacritty

"The fastest terminal emulator in existence" — minimal, GPU-accelerated.

### Key Characteristics

- **Speed** — OpenGL rendering, no bloat
- **Cross-platform** — macOS, Linux, Windows, BSD
- **Minimal** — No tabs, splits, ligatures (by design)
- **Vi mode** — Built-in vi keybindings

### Configuration

```yaml
# ~/.config/alacritty/alacritty.toml

[font]
normal = { family = "JetBrains Mono", style = "Regular" }
size = 14.0

[window]
padding = { x = 10, y = 10 }
decorations = "Buttonless"
opacity = 0.95

[cursor]
style = { shape = "Block", blinking = "Off" }
```

### Strengths

- Blazing fast
- Simple, predictable
- Cross-platform consistency
- Low memory usage

### Considerations

- No tabs/splits (use tmux)
- No ligatures
- TOML config only
- Minimal feature set by philosophy

---

## Kitty

Feature-rich, GPU-accelerated terminal with unique capabilities.

### Key Characteristics

- **GPU rendering** — OpenGL-based
- **Kitten plugins** — Extensible via Python
- **Graphics protocol** — Display images inline
- **Layouts** — Built-in tabs, splits, layouts
- **Remote control** — Control via CLI/scripts

### Configuration

```conf
# ~/.config/kitty/kitty.conf

font_family JetBrains Mono
font_size 14.0

background_opacity 0.95
window_padding_width 10

enable_audio_bell no
tab_bar_style powerline

# Keyboard shortcuts
map ctrl+shift+t new_tab
map ctrl+shift+enter new_window
```

### Kittens (Plugins)

```bash
# View images in terminal
kitty +kitten icat image.png

# SSH with automatic shell integration
kitty +kitten ssh user@host

# Show diffs with syntax highlighting
kitty +kitten diff file1 file2

# Unicode input
kitty +kitten unicode_input
```

### Strengths

- Image display in terminal
- Rich feature set
- Highly configurable
- Active development
- Good documentation

### Considerations

- Config syntax can be verbose
- Some features Linux-focused
- Heavier than Alacritty

---

## Warp

AI-powered terminal with modern UX. Reimagines the terminal experience.

### Key Characteristics

- **Block-based** — Commands grouped as blocks
- **AI integration** — Natural language to commands
- **Workflows** — Saved/shared command sequences
- **Modern input** — IDE-like text editing
- **Collaboration** — Share sessions

### Features

```
Warp innovations:
├── Command blocks (select, copy, share)
├── AI command search (describe what you want)
├── Completions (context-aware)
├── Workflows (parameterized snippets)
├── Warp Drive (cloud sync)
└── IDE-style editing (multi-cursor, etc.)
```

### Strengths

- Modern, intuitive UX
- AI assistance built-in
- Great for beginners
- Active development
- Beautiful default theme

### Considerations

- Requires account signup
- Proprietary/closed source
- Different mental model
- Cloud features may raise privacy concerns
- Resource heavier

---

## iTerm2

The classic macOS terminal. Feature-rich, battle-tested.

### Key Characteristics

- **macOS native** — Deep OS integration
- **Mature** — 15+ years of development
- **Feature-complete** — Everything you might need
- **Profiles** — Multiple configurations

### Key Features

```
iTerm2 features:
├── Split panes (horizontal, vertical)
├── Hotkey window (drop-down terminal)
├── Search with regex
├── Autocomplete
├── Shell integration
├── Triggers (pattern → action)
├── Password manager
├── tmux integration
└── Instant replay
```

### Configuration

Preferences GUI-based, but can export/import JSON.

### Strengths

- Rock solid stability
- Comprehensive features
- Great tmux integration
- Shell integration (marks, navigation)
- Mature ecosystem

### Considerations

- macOS only
- Not GPU-accelerated
- Can feel dated compared to newer terminals
- Memory usage higher with many tabs

---

## Windows Terminal

Microsoft's modern terminal for Windows.

### Key Characteristics

- **Modern Windows terminal** — Replaces cmd.exe, PowerShell windows
- **Multiple profiles** — PowerShell, WSL, cmd, Azure Cloud Shell
- **GPU-accelerated** — DirectX rendering
- **Customizable** — Themes, keybindings, appearance

### Configuration

```json
// settings.json
{
  "defaultProfile": "{574e775e-4f2a-5b96-ac1e-a2962a402336}",
  "profiles": {
    "defaults": {
      "font": { "face": "JetBrains Mono", "size": 14 },
      "opacity": 95,
      "useAcrylic": true
    },
    "list": [
      { "name": "PowerShell", "source": "Windows.Terminal.PowershellCore" },
      { "name": "Ubuntu", "source": "Windows.Terminal.Wsl" }
    ]
  },
  "schemes": [{ "name": "One Dark", ... }]
}
```

### Strengths

- Best Windows terminal experience
- WSL integration
- Multiple shell profiles
- Quake mode (dropdown)
- Active Microsoft support

### Considerations

- Windows only
- JSON config can be verbose
- Some features still maturing

---

## WezTerm

Cross-platform terminal with Lua configuration.

### Key Characteristics

- **Lua config** — Full programming language for config
- **Multiplexer built-in** — Tabs, panes, workspaces
- **Cross-platform** — Windows, macOS, Linux
- **GPU-accelerated** — Fast rendering
- **SSH integration** — Built-in multiplexing over SSH

### Configuration

```lua
-- ~/.config/wezterm/wezterm.lua
local wezterm = require 'wezterm'
local config = {}

config.font = wezterm.font 'JetBrains Mono'
config.font_size = 14.0
config.color_scheme = 'Catppuccin Mocha'

config.window_background_opacity = 0.95
config.enable_tab_bar = true

config.keys = {
  { key = 't', mods = 'CTRL|SHIFT', action = wezterm.action.SpawnTab 'CurrentPaneDomain' },
}

return config
```

### Strengths

- Lua = dynamic, programmable config
- Feature-rich like Kitty
- True cross-platform
- Built-in multiplexer
- Serial port support

### Considerations

- Lua config steeper learning curve
- Less mainstream than others
- Some platform differences

---

## Hyper

Electron-based terminal. Extensible via web technologies.

### Key Characteristics

- **Electron** — HTML/CSS/JS based
- **Plugins** — npm ecosystem
- **Themeable** — Web technologies
- **Cross-platform** — Anywhere Electron runs

### Configuration

```javascript
// ~/.hyper.js
module.exports = {
  config: {
    fontSize: 14,
    fontFamily: '"JetBrains Mono", monospace',
    cursorShape: 'BLOCK',
    shell: '/bin/zsh',
    plugins: [
      'hyper-one-dark',
      'hyper-search',
      'hypercwd',
    ],
  },
};
```

### Strengths

- Easy to theme (CSS)
- npm plugin ecosystem
- Familiar for web developers
- Cross-platform

### Considerations

- Electron = higher resource usage
- Slower than native terminals
- Can feel sluggish
- Battery impact on laptops

---

## Feature Matrix

| Feature | Ghostty | Alacritty | Kitty | Warp | iTerm2 | WinTerm | WezTerm |
|---------|:-------:|:---------:|:-----:|:----:|:------:|:-------:|:-------:|
| GPU accelerated | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ | ✅ |
| Tabs built-in | ✅ | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Splits built-in | ✅ | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Ligatures | ✅ | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Images in terminal | ❌ | ❌ | ✅ | ✅ | ✅ | ❌ | ✅ |
| Config language | Custom | TOML | Custom | GUI | GUI | JSON | Lua |
| Scrollback search | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| macOS | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ |
| Linux | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ | ✅ |
| Windows | ❌ | ✅ | ❌ | ❌ | ❌ | ✅ | ✅ |

---

## Performance Comparison

| Terminal | Startup | Latency | Memory | Battery |
|----------|---------|---------|--------|---------|
| Ghostty | Excellent | Excellent | Low | Good |
| Alacritty | Excellent | Excellent | Lowest | Good |
| Kitty | Good | Excellent | Medium | Good |
| Warp | Medium | Good | Higher | Medium |
| iTerm2 | Medium | Good | Higher | Medium |
| WezTerm | Good | Excellent | Medium | Good |
| Hyper | Slow | Medium | High | Poor |

---

## Decision Guide

| Use Case | Recommendation |
|----------|----------------|
| macOS, want native feel | Ghostty or iTerm2 |
| Absolute speed, minimalism | Alacritty + tmux |
| Feature-rich, images, extensible | Kitty or WezTerm |
| New to terminal, want guidance | Warp |
| Windows primary | Windows Terminal |
| Cross-platform consistency | Alacritty, WezTerm, or Kitty |
| Web developer, want plugins | Hyper |
| Programmable config | WezTerm (Lua) |

### Quick Picks

| Profile | Terminal |
|---------|----------|
| **Minimalist** | Alacritty |
| **Power user** | Kitty or WezTerm |
| **macOS native** | Ghostty or iTerm2 |
| **Modern UX** | Warp |
| **Windows** | Windows Terminal |

---

## Related

- [[Shells]]
- [[Terminal UI & Language Features]]
