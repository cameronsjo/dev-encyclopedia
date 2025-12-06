---
title: Terminal Multiplexers
aliases:
  - tmux vs screen
  - Multiplexers
tags:
  - tool
  - comparison
  - terminal
  - cli
type: comparison
status: complete
created: "2025-12-04"
---

# Terminal Multiplexers

Tools for managing multiple terminal sessions within a single window.

## Overview

| Tool | Written In | Config | Session Persist | Pane Splits |
|------|------------|--------|-----------------|-------------|
| tmux | C | `.tmux.conf` | ✅ | ✅ |
| screen | C | `.screenrc` | ✅ | Limited |
| Zellij | Rust | KDL | ✅ | ✅ |
| byobu | Shell (wrapper) | Multiple | ✅ | ✅ |

---

## Why Use a Multiplexer?

```
Without multiplexer:          With multiplexer:
┌─────────────────────┐       ┌─────────────────────┐
│     SSH Session     │       │     SSH Session     │
│                     │       │ ┌─────┬─────┬─────┐ │
│   Single terminal   │       │ │vim  │logs │build│ │
│                     │       │ ├─────┴─────┴─────┤ │
│ Disconnect = lose   │       │ │   shell         │ │
│ everything          │       │ └─────────────────┘ │
└─────────────────────┘       │ Persists on        │
                              │ disconnect!         │
                              └─────────────────────┘
```

**Key benefits:**

- Sessions persist when disconnected
- Multiple panes in one terminal
- Switch between workspaces
- Remote pair programming
- Consistent environment across machines

---

## tmux

Terminal MUltipleXer. The modern standard.

### Concepts

```
tmux hierarchy:
┌─────────────────────────────────────────────────────────┐
│ Server (background daemon)                              │
│ ├── Session: "dev"                                      │
│ │   ├── Window 0: "editor"                              │
│ │   │   ├── Pane 0: vim                                 │
│ │   │   └── Pane 1: terminal                            │
│ │   └── Window 1: "server"                              │
│ │       └── Pane 0: npm run dev                         │
│ └── Session: "work"                                     │
│     └── Window 0: "main"                                │
│         └── Pane 0: shell                               │
└─────────────────────────────────────────────────────────┘
```

### Essential Commands

```bash
# Sessions
tmux                          # New session
tmux new -s name              # New named session
tmux ls                       # List sessions
tmux attach -t name           # Attach to session
tmux kill-session -t name     # Kill session

# Prefix key: Ctrl-b (default)
# All shortcuts start with prefix

# After prefix:
d                             # Detach
:new                          # New session
$                             # Rename session
s                             # List sessions
(                             # Previous session
)                             # Next session
```

### Windows (Tabs)

```bash
# After prefix:
c                             # Create window
,                             # Rename window
n                             # Next window
p                             # Previous window
0-9                           # Switch to window N
&                             # Kill window
w                             # List windows
```

### Panes (Splits)

```bash
# After prefix:
%                             # Split vertical
"                             # Split horizontal
o                             # Switch pane
x                             # Kill pane
z                             # Toggle zoom (fullscreen)
{                             # Swap pane left
}                             # Swap pane right
Arrow keys                    # Navigate panes
Ctrl+Arrow                    # Resize pane
Space                         # Cycle layouts
```

### Copy Mode

```bash
# After prefix:
[                             # Enter copy mode
# In copy mode (vi keys):
Space                         # Start selection
Enter                         # Copy and exit
q                             # Exit copy mode

# Paste
]                             # Paste buffer
```

### Configuration

```bash
# ~/.tmux.conf

# Change prefix to Ctrl-a
unbind C-b
set-option -g prefix C-a
bind-key C-a send-prefix

# Vi mode
setw -g mode-keys vi

# Mouse support
set -g mouse on

# Start windows at 1
set -g base-index 1
setw -g pane-base-index 1

# Split shortcuts
bind | split-window -h
bind - split-window -v

# Vim-style pane navigation
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

# Reload config
bind r source-file ~/.tmux.conf \; display "Reloaded!"

# Status bar
set -g status-style 'bg=#333333 fg=#ffffff'
set -g status-right '%Y-%m-%d %H:%M'

# Increase scrollback
set -g history-limit 10000
```

---

## screen

GNU Screen. The original multiplexer.

### Basic Usage

```bash
# Sessions
screen                        # New session
screen -S name                # New named session
screen -ls                    # List sessions
screen -r name                # Reattach
screen -d -r name             # Detach elsewhere, attach here
screen -X -S name quit        # Kill session

# Prefix key: Ctrl-a (default)
```

### After Prefix (Ctrl-a)

```bash
# Windows
c                             # Create window
n                             # Next window
p                             # Previous window
0-9                           # Switch to window N
"                             # List windows
A                             # Rename window
k                             # Kill window

# Splits (limited)
S                             # Split horizontal
|                             # Split vertical (newer versions)
Tab                           # Switch region
X                             # Close region

# Other
d                             # Detach
?                             # Help
[                             # Copy mode
]                             # Paste
```

### Configuration

```bash
# ~/.screenrc

# Change escape key
escape ^Aa

# Visual bell
vbell on

# Scrollback
defscrollback 10000

# Status line
hardstatus alwayslastline
hardstatus string '%{= kG}[ %{G}%H %{g}][%= %{= kw}%?%-Lw%?%{r}(%{W}%n*%f%t%?(%u)%?%{r})%{w}%?%+Lw%?%?%= %{g}][%{B} %m-%d %{W}%c %{g}]'

# Start with multiple windows
screen -t shell 0
screen -t vim 1
screen -t logs 2
```

---

## Zellij

Modern, user-friendly multiplexer. Written in Rust.

### Key Features

- **Discoverable UI** — On-screen hints
- **Layouts** — Declarative, saveable
- **Floating panes** — Popup windows
- **Sessions** — Built-in session management
- **Plugins** — WebAssembly plugins

### Basic Usage

```bash
# Start
zellij                        # New session
zellij -s name                # Named session
zellij ls                     # List sessions
zellij attach name            # Attach
zellij kill-session name      # Kill

# Built-in help shown at bottom of screen
```

### Keybindings

```bash
# Modes (press key to enter mode)
Ctrl+g                        # Locked (disable zellij keys)
Ctrl+p                        # Pane mode
Ctrl+t                        # Tab mode
Ctrl+n                        # Resize mode
Ctrl+h                        # Move mode
Ctrl+s                        # Scroll mode
Ctrl+o                        # Session mode
Ctrl+q                        # Quit

# In Pane mode (Ctrl+p):
n                             # New pane (down)
d                             # New pane (down)
r                             # New pane (right)
x                             # Close pane
f                             # Toggle fullscreen
w                             # Toggle floating
hjkl / arrows                 # Navigate

# In Tab mode (Ctrl+t):
n                             # New tab
x                             # Close tab
r                             # Rename tab
1-9                           # Go to tab
```

### Layouts

```kdl
// ~/.config/zellij/layouts/dev.kdl
layout {
    pane size=1 borderless=true {
        plugin location="tab-bar"
    }
    pane split_direction="vertical" {
        pane command="nvim"
        pane split_direction="horizontal" {
            pane command="npm" args=["run", "dev"]
            pane  // empty shell
        }
    }
    pane size=2 borderless=true {
        plugin location="status-bar"
    }
}
```

```bash
# Use layout
zellij --layout dev
```

### Configuration

```kdl
// ~/.config/zellij/config.kdl

keybinds {
    normal {
        bind "Alt h" { MoveFocus "Left"; }
        bind "Alt l" { MoveFocus "Right"; }
        bind "Alt j" { MoveFocus "Down"; }
        bind "Alt k" { MoveFocus "Up"; }
    }
}

themes {
    catppuccin {
        fg "#CDD6F4"
        bg "#1E1E2E"
        // ...
    }
}

theme "catppuccin"

default_shell "zsh"
default_layout "compact"
```

---

## byobu

Wrapper around tmux or screen with better defaults.

```bash
# Install
sudo apt install byobu        # Ubuntu/Debian
brew install byobu             # macOS

# Start (uses tmux by default)
byobu

# Switch backend
byobu-select-backend          # Choose tmux or screen

# Key bindings (F-keys by default)
F2                            # New window
F3                            # Previous window
F4                            # Next window
F6                            # Detach
F7                            # Scrollback
F8                            # Rename window
F9                            # Config menu
```

---

## Comparison

### Feature Matrix

| Feature | tmux | screen | Zellij |
|---------|:----:|:------:|:------:|
| Vertical splits | ✅ | ✅ (newer) | ✅ |
| Horizontal splits | ✅ | ✅ | ✅ |
| Floating panes | ❌ | ❌ | ✅ |
| Mouse support | ✅ | Limited | ✅ |
| Scripting | ✅ | ✅ | Limited |
| Layouts | ✅ | ❌ | ✅ (KDL) |
| Session management | ✅ | ✅ | ✅ |
| Learning curve | Medium | Easy | Easy |
| Visual UI | Minimal | Minimal | Rich |
| Plugin system | Via scripts | ❌ | ✅ (WASM) |

### Resource Usage

| Tool | Memory | Startup |
|------|--------|---------|
| tmux | ~5MB | Fast |
| screen | ~3MB | Fast |
| Zellij | ~15MB | Fast |

---

## Common Workflows

### SSH + Multiplexer

```bash
# Start persistent session
ssh server
tmux new -s work

# Later, after disconnect
ssh server
tmux attach -t work           # Everything still there!
```

### Development Layout

```bash
# tmux script: ~/.local/bin/dev-session
#!/bin/bash
tmux new-session -d -s dev
tmux rename-window -t dev:0 'editor'
tmux send-keys -t dev:0 'nvim .' C-m
tmux new-window -t dev -n 'server'
tmux send-keys -t dev:1 'npm run dev' C-m
tmux new-window -t dev -n 'shell'
tmux select-window -t dev:0
tmux attach -t dev
```

### Pair Programming

```bash
# Host
tmux new -s pair

# Guest (same machine or via SSH)
tmux attach -t pair

# Both see and can type in same session
```

---

## Decision Guide

| Use Case | Recommendation |
|----------|----------------|
| Servers, minimal | tmux or screen |
| New user, want discoverability | Zellij |
| Heavy customization | tmux |
| Already know screen | screen or byobu |
| Modern features, plugins | Zellij |
| Remote pair programming | tmux |
| Quick start, good defaults | byobu or Zellij |

### Quick Picks

| Profile | Tool |
|---------|------|
| **Power user** | tmux |
| **Beginner** | Zellij or byobu |
| **Minimal/legacy** | screen |
| **Modern, discoverable** | Zellij |

---

## Related

- [[Shells]]
- [[Terminal Emulators]]
- [[WSL]]
