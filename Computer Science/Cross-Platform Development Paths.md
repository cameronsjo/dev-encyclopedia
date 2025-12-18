---
title: Cross-Platform Development Paths
aliases:
  - OS Paths Comparison
  - Platform Directories
  - Config Locations
tags:
  - cs
  - cross-platform
  - comparison
type: comparison
status: complete
created: "2025-12-18"
---

# Cross-Platform Development Paths

Quick reference for where to store files across operating systems.

## Path Comparison

### Config Files (User Settings)

| Purpose | Linux | macOS | Windows |
|---------|-------|-------|---------|
| **Location** | `~/.config/app/` | `~/Library/Application Support/app/` | `%APPDATA%\app\` |
| **XDG Var** | `$XDG_CONFIG_HOME` | - | - |
| **Example** | `~/.config/git/config` | `~/Library/Application Support/Code/` | `C:\Users\name\AppData\Roaming\Code\` |

### Data Files (App Data)

| Purpose | Linux | macOS | Windows |
|---------|-------|-------|---------|
| **Location** | `~/.local/share/app/` | `~/Library/Application Support/app/` | `%APPDATA%\app\` |
| **XDG Var** | `$XDG_DATA_HOME` | - | - |
| **Example** | `~/.local/share/nvim/` | `~/Library/Application Support/` | `C:\Users\name\AppData\Roaming\` |

### Cache Files (Deletable)

| Purpose | Linux | macOS | Windows |
|---------|-------|-------|---------|
| **Location** | `~/.cache/app/` | `~/Library/Caches/app/` | `%LOCALAPPDATA%\app\cache\` |
| **XDG Var** | `$XDG_CACHE_HOME` | - | - |
| **Example** | `~/.cache/pip/` | `~/Library/Caches/pip/` | `C:\Users\name\AppData\Local\pip\Cache\` |

### Temp Files (Session)

| Purpose | Linux | macOS | Windows |
|---------|-------|-------|---------|
| **Location** | `/tmp/` or `$XDG_RUNTIME_DIR` | `$TMPDIR` | `%TEMP%` |
| **Example** | `/tmp/myapp.sock` | `/var/folders/.../T/` | `C:\Users\name\AppData\Local\Temp\` |

### System-Wide Config

| Purpose | Linux | macOS | Windows |
|---------|-------|-------|---------|
| **Location** | `/etc/app/` | `/Library/Application Support/` | `%PROGRAMDATA%\app\` |
| **Example** | `/etc/nginx/` | `/Library/Application Support/` | `C:\ProgramData\` |

### Executables (User)

| Purpose | Linux | macOS | Windows |
|---------|-------|-------|---------|
| **Location** | `~/.local/bin/` | `/usr/local/bin/` | `%LOCALAPPDATA%\Programs\` |
| **In PATH?** | Usually not | Yes | Usually not |

### Executables (System)

| Purpose | Linux | macOS | Windows |
|---------|-------|-------|---------|
| **Location** | `/usr/bin/`, `/usr/local/bin/` | `/usr/local/bin/`, `/opt/homebrew/bin/` | `C:\Program Files\` |

## Cross-Platform Code

### Using Libraries (Recommended)

```python
# Python - platformdirs (best option)
from platformdirs import user_config_dir, user_data_dir, user_cache_dir

config = user_config_dir("myapp")   # Correct path for each OS
data = user_data_dir("myapp")
cache = user_cache_dir("myapp")
```

```javascript
// Node.js - env-paths
const envPaths = require('env-paths');
const paths = envPaths('myapp');

console.log(paths.config);  // Config directory
console.log(paths.data);    // Data directory
console.log(paths.cache);   // Cache directory
```

```rust
// Rust - dirs crate
use dirs;

let config = dirs::config_dir().unwrap().join("myapp");
let data = dirs::data_dir().unwrap().join("myapp");
let cache = dirs::cache_dir().unwrap().join("myapp");
```

```go
// Go - os.UserConfigDir() (Go 1.13+)
import "os"

configDir, _ := os.UserConfigDir()
// Linux: ~/.config
// macOS: ~/Library/Application Support
// Windows: %APPDATA%

cacheDir, _ := os.UserCacheDir()
// Linux: ~/.cache
// macOS: ~/Library/Caches
// Windows: %LOCALAPPDATA%
```

### Manual Implementation

```python
import os
import sys
from pathlib import Path

def get_config_dir(app_name: str) -> Path:
    if sys.platform == 'win32':
        base = Path(os.environ.get('APPDATA', Path.home() / 'AppData/Roaming'))
    elif sys.platform == 'darwin':
        base = Path.home() / 'Library/Application Support'
    else:  # Linux/BSD
        base = Path(os.environ.get('XDG_CONFIG_HOME', Path.home() / '.config'))
    return base / app_name

def get_cache_dir(app_name: str) -> Path:
    if sys.platform == 'win32':
        base = Path(os.environ.get('LOCALAPPDATA', Path.home() / 'AppData/Local'))
        return base / app_name / 'cache'
    elif sys.platform == 'darwin':
        return Path.home() / 'Library/Caches' / app_name
    else:
        base = Path(os.environ.get('XDG_CACHE_HOME', Path.home() / '.cache'))
        return base / app_name
```

## Path Separators

| OS | Separator | Example |
|----|-----------|---------|
| **Linux/macOS** | `/` | `/home/user/file.txt` |
| **Windows** | `\` | `C:\Users\name\file.txt` |
| **Windows (also works)** | `/` | `C:/Users/name/file.txt` |

```python
# Always use pathlib or os.path - never hardcode separators
from pathlib import Path

# Good
config_file = Path.home() / ".config" / "myapp" / "config.json"

# Bad
config_file = os.environ['HOME'] + "/.config/myapp/config.json"
```

## Environment Variables

| Variable | Linux | macOS | Windows |
|----------|-------|-------|---------|
| **Home** | `$HOME` | `$HOME` | `%USERPROFILE%` |
| **User** | `$USER` | `$USER` | `%USERNAME%` |
| **Temp** | `$TMPDIR` or `/tmp` | `$TMPDIR` | `%TEMP%` |
| **Path** | `$PATH` | `$PATH` | `%PATH%` |
| **Shell** | `$SHELL` | `$SHELL` | N/A |

```python
# Cross-platform home directory
from pathlib import Path
home = Path.home()  # Works everywhere

# Or
import os
home = os.path.expanduser("~")
```

## Shell Config Files

| Shell | Linux | macOS | Windows |
|-------|-------|-------|---------|
| **bash** | `~/.bashrc` | `~/.bash_profile` | `~/.bashrc` (Git Bash) |
| **zsh** | `~/.zshrc` | `~/.zshrc` | N/A |
| **fish** | `~/.config/fish/config.fish` | Same | Same |
| **PowerShell** | `~/.config/powershell/profile.ps1` | Same | `$PROFILE` |

## Services/Daemons

| Concept | Linux | macOS | Windows |
|---------|-------|-------|---------|
| **Service Manager** | systemd | launchd | Services |
| **User Service Dir** | `~/.config/systemd/user/` | `~/Library/LaunchAgents/` | Task Scheduler |
| **System Service Dir** | `/etc/systemd/system/` | `/Library/LaunchDaemons/` | Registry/Services |
| **Start Command** | `systemctl start x` | `launchctl start x` | `Start-Service x` |
| **Auto-start** | `systemctl enable x` | `RunAtLoad` plist key | Service properties |

## Package Managers

| OS | Built-in | Popular Third-Party |
|----|----------|---------------------|
| **Linux (Debian)** | apt | snap, flatpak |
| **Linux (Fedora)** | dnf | snap, flatpak |
| **Linux (Arch)** | pacman | yay (AUR) |
| **macOS** | - | Homebrew, MacPorts |
| **Windows** | winget | Chocolatey, Scoop |

## Decision Guide

### Where Should My App Store Data?

```
Is it user-editable config?
├── Yes → Config dir (XDG_CONFIG_HOME / App Support / APPDATA)
└── No
    ├── Is it cacheable/regeneratable?
    │   ├── Yes → Cache dir (XDG_CACHE_HOME / Caches / LOCALAPPDATA)
    │   └── No
    │       ├── Is it important data?
    │       │   ├── Yes → Data dir (XDG_DATA_HOME / App Support / APPDATA)
    │       │   └── No → State dir (XDG_STATE_HOME / App Support / LOCALAPPDATA)
    │       └── Is it temporary (session only)?
    │           └── Yes → Temp dir ($TMPDIR / %TEMP%)
```

### Best Practices

| Practice | Why |
|----------|-----|
| **Use libraries** | They handle edge cases |
| **Create dirs on first use** | Don't assume they exist |
| **Respect platform conventions** | Users expect standard locations |
| **Support XDG overrides** | Power users customize paths |
| **Don't pollute home** | Use proper subdirectories |
| **Separate config/data/cache** | Different backup/sync needs |

## Related

- [[XDG Base Directory]] — Linux paths in detail
- [[macOS Development Basics]] — macOS paths in detail
- [[Windows Development Basics]] — Windows paths in detail
- [[Computer Science MOC]] — CS topics
