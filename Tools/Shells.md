---
title: Shells
aliases:
  - Command Line Shells
  - Bash vs Zsh vs Fish
  - PowerShell
tags:
  - tool
  - comparison
  - shell
  - cli
  - terminal
type: comparison
status: complete
created: "2025-12-04"
---

# Shells

Command-line interpreters for interacting with operating systems and scripting.

## Overview

| Aspect | Bash | Zsh | Fish | PowerShell |
|--------|------|-----|------|------------|
| Full name | Bourne Again Shell | Z Shell | Friendly Interactive Shell | PowerShell |
| First release | 1989 | 1990 | 2005 | 2006 |
| Default on | Linux, older macOS | macOS (10.15+) | — | Windows |
| Config file | `.bashrc`, `.bash_profile` | `.zshrc` | `config.fish` | `$PROFILE` |
| Scripting | POSIX-ish | POSIX + extensions | Own syntax | .NET-based |
| Philosophy | Compatibility | Power user features | User-friendly | Object pipeline |

---

## Bash

The standard shell on most Linux systems. POSIX-compatible.

### Key Characteristics

- **Ubiquitous** — Available on virtually all Unix-like systems
- **POSIX-compatible** — Scripts are portable
- **Mature** — Extensive documentation, huge community
- **Default** — What most tutorials and scripts assume

### Configuration

```bash
# ~/.bashrc (interactive non-login)
# ~/.bash_profile (login shells)

# Prompt customization
export PS1="\u@\h:\w\$ "

# Aliases
alias ll='ls -la'
alias gs='git status'

# Functions
mkcd() { mkdir -p "$1" && cd "$1"; }
```

### Scripting

```bash
#!/bin/bash

# Variables
name="world"
echo "Hello, $name"

# Arrays
arr=(one two three)
echo "${arr[1]}"  # "two"

# Conditionals
if [[ -f "$file" ]]; then
    echo "File exists"
fi

# Loops
for i in {1..5}; do
    echo "$i"
done
```

### Considerations

- Limited interactive features out of box
- No built-in autocomplete for commands
- Syntax quirks (quoting, spacing matters)
- `[[` vs `[` vs `test` confusion

---

## Zsh

Bash-compatible with powerful extensions. macOS default since Catalina.

### Key Characteristics

- **Bash-compatible** — Most bash scripts work
- **Better completion** — Tab completion for commands, flags, arguments
- **Glob patterns** — Extended globbing (`**/*.ts`)
- **Frameworks** — Oh My Zsh, Prezto ecosystems
- **Themes** — Powerlevel10k, etc.

### Configuration

```bash
# ~/.zshrc

# Enable extended globbing
setopt extended_glob

# History settings
HISTSIZE=10000
SAVEHIST=10000
setopt share_history

# Better completion
autoload -Uz compinit && compinit
```

### Oh My Zsh

```bash
# Install
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# ~/.zshrc with Oh My Zsh
plugins=(
  git
  docker
  kubectl
  zsh-autosuggestions
  zsh-syntax-highlighting
)
ZSH_THEME="robbyrussell"  # or "powerlevel10k/powerlevel10k"
```

### Zsh vs Bash

| Feature | Bash | Zsh |
|---------|------|-----|
| Tab completion | Basic | Advanced (context-aware) |
| Spelling correction | No | Yes (`setopt correct`) |
| Glob patterns | Basic | Extended (`**/*.ts`) |
| Prompt themes | DIY | Rich ecosystem |
| Right-side prompt | No | Yes (RPROMPT) |
| Array indexing | 0-based | 1-based (configurable) |
| Plugin ecosystem | Limited | Oh My Zsh, Prezto |

### Considerations

- Slightly slower startup (especially with plugins)
- 1-based arrays can confuse bash users
- Oh My Zsh can become bloated
- Minor incompatibilities with bash scripts

---

## Fish

User-friendly shell that works out of the box.

### Key Characteristics

- **Autosuggestions** — Built-in, based on history
- **Syntax highlighting** — Real-time, in the prompt
- **Web config** — `fish_config` opens browser UI
- **No configuration needed** — Great defaults
- **Not POSIX** — Own scripting syntax

### Configuration

```fish
# ~/.config/fish/config.fish

# Variables (no export keyword)
set -x PATH $HOME/bin $PATH

# Aliases (actually functions)
alias ll 'ls -la'

# Functions
function mkcd
    mkdir -p $argv[1] && cd $argv[1]
end
```

### Scripting Differences

```fish
# Fish syntax differs from bash/zsh

# Variables
set name "world"
echo "Hello, $name"

# Conditionals (no [[ ]])
if test -f $file
    echo "File exists"
end

# Loops
for i in (seq 1 5)
    echo $i
end

# Command substitution
set files (ls *.txt)

# No && or ||, use and/or
command1; and command2
command1; or command2
```

### Key Features

```fish
# Autosuggestions (accept with →)
$ git comm<gray>it -m "last message"</gray>

# Abbreviations (expand on space)
abbr -a gc 'git commit'
# Type "gc " → expands to "git commit "

# Universal variables (persist across sessions)
set -U fish_greeting ""
```

### Considerations

- Not POSIX — can't run bash scripts directly
- Fewer tutorials written for fish
- Some tools assume bash
- Abbreviations > aliases philosophy

---

## PowerShell

Object-oriented shell from Microsoft. Cross-platform since PowerShell Core.

### Key Characteristics

- **Object pipeline** — Pass .NET objects, not text
- **Verb-Noun commands** — `Get-Process`, `Set-Location`
- **Cross-platform** — Windows, macOS, Linux
- **.NET integration** — Full access to .NET libraries
- **Structured output** — Objects with properties

### Configuration

```powershell
# $PROFILE (create if doesn't exist)
# Usually: ~\Documents\PowerShell\Microsoft.PowerShell_profile.ps1

# Aliases
Set-Alias -Name g -Value git
Set-Alias -Name ll -Value Get-ChildItem

# Functions
function mkcd { param($path) New-Item -ItemType Directory $path; Set-Location $path }

# Prompt customization
function prompt {
    "$($executionContext.SessionState.Path.CurrentLocation)> "
}
```

### Object Pipeline

```powershell
# Objects, not text
Get-Process | Where-Object { $_.CPU -gt 10 } | Select-Object Name, CPU

# vs Unix (parsing text)
ps aux | awk '$3 > 10 {print $11, $3}'

# Properties are accessible
$procs = Get-Process
$procs[0].Name
$procs | ForEach-Object { $_.WorkingSet64 / 1MB }
```

### Common Commands

| Unix | PowerShell | Alias |
|------|------------|-------|
| `ls` | `Get-ChildItem` | `dir`, `ls`, `gci` |
| `cd` | `Set-Location` | `cd`, `sl` |
| `cat` | `Get-Content` | `cat`, `gc` |
| `rm` | `Remove-Item` | `rm`, `ri` |
| `cp` | `Copy-Item` | `cp`, `copy` |
| `mv` | `Move-Item` | `mv`, `move` |
| `grep` | `Select-String` | `sls` |
| `echo` | `Write-Output` | `echo` |

### PowerShell vs Unix Shells

| Aspect | PowerShell | Unix Shells |
|--------|------------|-------------|
| Pipeline | Objects | Text streams |
| Commands | Verb-Noun | Short names |
| Scripting | C#-like | POSIX/Bash |
| Error handling | Try/Catch | Exit codes |
| Variables | `$var` | `$var` |
| Cross-platform | Yes (Core) | Native |
| Windows integration | Excellent | WSL needed |

### Considerations

- Verbose command names (use aliases)
- Steeper learning curve for Unix users
- Different mental model (objects vs text)
- Less common on servers (except Windows)

---

## Feature Matrix

| Feature | Bash | Zsh | Fish | PowerShell |
|---------|:----:|:---:|:----:|:----------:|
| POSIX compatible | ✅ | ✅ | ❌ | ❌ |
| Autosuggestions | ❌ | Plugin | ✅ | Plugin |
| Syntax highlighting | ❌ | Plugin | ✅ | Plugin |
| Tab completion | Basic | Excellent | Excellent | Excellent |
| Spell correction | ❌ | ✅ | ✅ | ❌ |
| Plugin ecosystem | Limited | Rich | Growing | PSGallery |
| Web-based config | ❌ | ❌ | ✅ | ❌ |
| Object pipeline | ❌ | ❌ | ❌ | ✅ |
| Cross-platform | Unix | Unix | Unix | ✅ |
| Windows native | WSL/Git Bash | WSL | WSL | ✅ |

---

## Plugin Managers & Frameworks

### Zsh

| Tool | Purpose |
|------|---------|
| Oh My Zsh | Framework with plugins/themes |
| Prezto | Lighter alternative to OMZ |
| zinit | Fast plugin manager |
| zplug | Feature-rich plugin manager |

### Fish

| Tool | Purpose |
|------|---------|
| Fisher | Plugin manager |
| Oh My Fish | Framework (like OMZ) |
| Tide | Powerlevel10k-like prompt |

### PowerShell

| Tool | Purpose |
|------|---------|
| PSReadLine | Enhanced editing (built-in now) |
| oh-my-posh | Cross-shell prompt themes |
| Terminal-Icons | File icons in listing |
| z | Directory jumper |

---

## Prompt Customization

### Cross-Shell (Starship)

Works with all shells. Written in Rust.

```bash
# Install
curl -sS https://starship.rs/install.sh | sh

# Bash: add to .bashrc
eval "$(starship init bash)"

# Zsh: add to .zshrc
eval "$(starship init zsh)"

# Fish: add to config.fish
starship init fish | source

# PowerShell: add to $PROFILE
Invoke-Expression (&starship init powershell)
```

---

## Script Portability

```
Portability spectrum:

Most portable ←————————————————————→ Least portable

  sh/POSIX     Bash      Zsh      Fish      PowerShell
     │          │         │         │            │
     └──────────┴─────────┘         └────────────┘
           Works on Unix              Own ecosystem
```

### Writing Portable Scripts

```bash
#!/bin/sh
# Use /bin/sh for maximum portability
# Avoid bash-isms: [[ ]], arrays, <<<, etc.

# Instead of [[ ]]
if [ -f "$file" ]; then
    echo "exists"
fi

# Instead of $(())
result=$(expr 1 + 1)
```

---

## Decision Guide

| Use Case | Recommendation |
|----------|----------------|
| Maximum compatibility | Bash |
| Power user on macOS/Linux | Zsh + Oh My Zsh |
| Best out-of-box experience | Fish |
| Windows administration | PowerShell |
| Cross-platform scripts | Bash or PowerShell Core |
| Interactive daily use | Fish or Zsh |
| CI/CD pipelines | Bash |
| Minimal configuration | Fish |
| Windows + Unix hybrid | PowerShell Core |

### Summary

| Shell | Choose When |
|-------|-------------|
| **Bash** | Need portability, writing scripts, Linux servers, CI/CD |
| **Zsh** | Want bash compatibility + better UX, macOS users, customization |
| **Fish** | Want great defaults, minimal config, don't need POSIX |
| **PowerShell** | Windows environment, .NET integration, object manipulation |

---

## Related

- [[Terminal UI & Language Features]]
- [[Build Systems]]
- [[Deployment]]
