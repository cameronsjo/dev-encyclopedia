---
title: macOS Development Basics
aliases:
  - macOS Dev
  - macOS Paths
  - Library Folder
tags:
  - cs
  - macos
  - configuration
type: reference
status: complete
created: "2025-12-18"
---

# macOS Development Basics

Essential macOS concepts for developers from other platforms.

## Overview

| Aspect | Details |
|--------|---------|
| **Config Location** | `~/Library`, `~/.config` |
| **System Config** | `/Library`, `/System` |
| **Package Manager** | Homebrew, MacPorts |
| **Shell** | zsh (default), bash, fish |
| **Based On** | BSD Unix (Darwin) |

## Important Directories

### Library Folders

```
/                               # Root
├── Library/                    # System-wide (all users)
│   ├── Application Support/    # App data
│   ├── Preferences/            # System preferences
│   └── LaunchDaemons/          # System services
│
├── System/Library/             # Apple only (SIP protected)
│   └── (don't touch)
│
└── Users/username/
    └── Library/                # User-specific (hidden by default)
        ├── Application Support/  # App data
        ├── Preferences/          # .plist files
        ├── Caches/               # App caches
        ├── Logs/                 # App logs
        ├── LaunchAgents/         # User services
        └── Saved Application State/  # App state
```

### Mapping to XDG

| XDG (Linux) | macOS Equivalent |
|-------------|------------------|
| `XDG_CONFIG_HOME` | `~/Library/Application Support` or `~/.config` |
| `XDG_DATA_HOME` | `~/Library/Application Support` |
| `XDG_CACHE_HOME` | `~/Library/Caches` |
| `XDG_STATE_HOME` | `~/Library/Application Support` |
| `XDG_RUNTIME_DIR` | `$TMPDIR` (per-user temp) |

### Common Paths

| What | Location |
|------|----------|
| **Home** | `/Users/username` or `~` |
| **Applications** | `/Applications` or `~/Applications` |
| **Desktop** | `~/Desktop` |
| **Documents** | `~/Documents` |
| **Downloads** | `~/Downloads` |
| **Temp** | `$TMPDIR` (e.g., `/var/folders/...`) |
| **Homebrew (Intel)** | `/usr/local` |
| **Homebrew (Apple Silicon)** | `/opt/homebrew` |

## The Library Folder

### Accessing Library

```bash
# Library is hidden by default in Finder
# Option 1: Go menu → hold Option → Library appears

# Option 2: Terminal
open ~/Library

# Option 3: Show permanently
chflags nohidden ~/Library
```

### Application Support

**Where apps store data that isn't preferences.**

```
~/Library/Application Support/
├── Code/                    # VS Code
│   └── User/
│       ├── settings.json
│       └── keybindings.json
├── Firefox/                 # Firefox profiles
├── Slack/                   # Slack data
└── MyApp/                   # Your app
    ├── data.db
    └── plugins/
```

### Preferences (plist files)

**Property list files storing app settings.**

```
~/Library/Preferences/
├── com.apple.Terminal.plist
├── com.microsoft.VSCode.plist
├── com.googlecode.iterm2.plist
└── com.mycompany.myapp.plist
```

### Working with plists

```bash
# Read plist
defaults read com.apple.Terminal

# Read specific key
defaults read com.apple.finder ShowPathbar

# Write value
defaults write com.apple.finder ShowPathbar -bool true

# Delete key
defaults delete com.apple.finder ShowPathbar

# Convert plist to XML (readable)
plutil -convert xml1 ~/Library/Preferences/com.myapp.plist

# Convert to JSON
plutil -convert json ~/Library/Preferences/com.myapp.plist -o - | jq
```

```swift
// Swift - UserDefaults
let defaults = UserDefaults.standard

// Read
let value = defaults.string(forKey: "setting")

// Write
defaults.set("value", forKey: "setting")
defaults.synchronize()
```

```python
# Python - plistlib
import plistlib
from pathlib import Path

# Read
plist_path = Path.home() / "Library/Preferences/com.myapp.plist"
with open(plist_path, 'rb') as f:
    data = plistlib.load(f)

# Write
with open(plist_path, 'wb') as f:
    plistlib.dump(data, f)
```

### Caches

```
~/Library/Caches/
├── com.apple.Safari/
├── Homebrew/
├── pip/
└── com.mycompany.myapp/
```

```bash
# Clear specific app cache
rm -rf ~/Library/Caches/com.myapp

# Clear all caches (use with caution)
rm -rf ~/Library/Caches/*
```

## Launch Services

### LaunchAgents vs LaunchDaemons

| Type | Location | Runs As | When |
|------|----------|---------|------|
| **User Agent** | `~/Library/LaunchAgents/` | User | User login |
| **Global Agent** | `/Library/LaunchAgents/` | User | Any user login |
| **Daemon** | `/Library/LaunchDaemons/` | root | System boot |

### Creating a LaunchAgent

```xml
<!-- ~/Library/LaunchAgents/com.mycompany.myapp.plist -->
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.mycompany.myapp</string>

    <key>ProgramArguments</key>
    <array>
        <string>/usr/local/bin/myapp</string>
        <string>--daemon</string>
    </array>

    <key>RunAtLoad</key>
    <true/>

    <key>KeepAlive</key>
    <true/>

    <key>StandardOutPath</key>
    <string>/tmp/myapp.log</string>

    <key>StandardErrorPath</key>
    <string>/tmp/myapp.error.log</string>
</dict>
</plist>
```

### Managing Services

```bash
# Load (start at login)
launchctl load ~/Library/LaunchAgents/com.myapp.plist

# Unload (stop and disable)
launchctl unload ~/Library/LaunchAgents/com.myapp.plist

# Start now
launchctl start com.myapp

# Stop
launchctl stop com.myapp

# List loaded
launchctl list | grep myapp

# Modern syntax (macOS 10.10+)
launchctl bootstrap gui/$UID ~/Library/LaunchAgents/com.myapp.plist
launchctl bootout gui/$UID ~/Library/LaunchAgents/com.myapp.plist
```

### Homebrew Services

```bash
# Easier than raw launchctl
brew services list
brew services start postgresql
brew services stop postgresql
brew services restart postgresql
```

## Homebrew

### Installation

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Apple Silicon: Add to PATH
echo 'eval "$(/opt/homebrew/bin/brew shellenv)"' >> ~/.zprofile
```

### Basic Usage

```bash
# Search
brew search node

# Install
brew install node

# Install GUI app (cask)
brew install --cask visual-studio-code

# Upgrade
brew upgrade

# Cleanup old versions
brew cleanup

# List installed
brew list
brew list --cask

# Info
brew info node
```

### Brewfile (like package.json for system)

```ruby
# Brewfile
tap "homebrew/bundle"
tap "homebrew/cask-fonts"

# CLI tools
brew "git"
brew "node"
brew "python"
brew "ripgrep"
brew "fzf"

# Applications
cask "visual-studio-code"
cask "iterm2"
cask "docker"

# Fonts
cask "font-fira-code-nerd-font"

# App Store apps (requires mas)
mas "Xcode", id: 497799835
```

```bash
# Install from Brewfile
brew bundle

# Generate Brewfile from installed
brew bundle dump
```

## Security & Permissions

### System Integrity Protection (SIP)

```bash
# Check status
csrutil status

# Can't modify:
# /System, /usr (except /usr/local), /bin, /sbin
# Signed Apple apps
```

### Gatekeeper

```bash
# Allow app from unidentified developer
xattr -d com.apple.quarantine /Applications/MyApp.app

# Or: System Preferences → Security → Open Anyway
```

### Privacy Permissions

| Permission | Controls |
|------------|----------|
| **Full Disk Access** | Access all files |
| **Files and Folders** | Specific folder access |
| **Accessibility** | Control other apps |
| **Screen Recording** | Capture screen |
| **Automation** | AppleScript/control apps |
| **Developer Tools** | Debugging |

```bash
# Open privacy settings
open "x-apple.systempreferences:com.apple.preference.security?Privacy"
```

## Environment Variables

### Shell Config

```bash
# zsh (default)
~/.zshrc          # Interactive shells
~/.zprofile       # Login shells

# bash
~/.bashrc
~/.bash_profile

# Both
~/.profile        # Fallback
```

### Setting Variables

```bash
# ~/.zprofile or ~/.zshrc
export MY_VAR="value"
export PATH="/opt/homebrew/bin:$PATH"

# Or use launchctl for GUI apps
launchctl setenv MY_VAR "value"
```

### PATH on macOS

```bash
# System PATH construction:
# 1. /etc/paths (base paths)
# 2. /etc/paths.d/* (additional paths)
# 3. Shell profile additions

cat /etc/paths
# /usr/local/bin
# /usr/bin
# /bin
# /usr/sbin
# /sbin
```

## Common Developer Tools

### Xcode Command Line Tools

```bash
# Install (required for many dev tools)
xcode-select --install

# Location
xcode-select -p
# /Library/Developer/CommandLineTools

# Reset
sudo xcode-select --reset
```

### Developer Directory

```
~/Library/Developer/
├── CoreSimulator/       # iOS Simulators
├── Xcode/
│   └── DerivedData/     # Build artifacts (safe to delete)
└── CommandLineTools/    # If no full Xcode
```

## Keychain

```bash
# Store secret
security add-generic-password -a "$USER" -s "myapp" -w "secret123"

# Retrieve secret
security find-generic-password -a "$USER" -s "myapp" -w

# Delete
security delete-generic-password -a "$USER" -s "myapp"
```

```python
# Python with keyring
import keyring

keyring.set_password("myapp", "api_key", "secret123")
password = keyring.get_password("myapp", "api_key")
```

## App Bundles

### Structure

```
MyApp.app/
└── Contents/
    ├── Info.plist          # App metadata
    ├── MacOS/
    │   └── MyApp           # Main executable
    ├── Resources/          # Assets, localizations
    │   ├── en.lproj/
    │   └── icon.icns
    └── Frameworks/         # Bundled libraries
```

### Info.plist Keys

| Key | Description |
|-----|-------------|
| `CFBundleIdentifier` | Reverse-DNS ID (com.mycompany.myapp) |
| `CFBundleName` | Display name |
| `CFBundleVersion` | Build number |
| `CFBundleShortVersionString` | Version string |
| `LSMinimumSystemVersion` | Minimum macOS version |

## Related

- [[XDG Base Directory]] — Linux equivalent
- [[Windows Development Basics]] — Windows equivalent
- [[Computer Science MOC]] — CS topics
