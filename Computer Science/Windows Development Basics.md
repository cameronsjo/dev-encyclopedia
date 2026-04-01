---
title: Windows Development Basics
aliases:
  - Windows Dev
  - Windows Paths
  - AppData
tags:
  - cs
  - windows
  - configuration
type: reference
status: complete
created: "2025-12-18"
---

# Windows Development Basics

Essential Windows concepts for developers from other platforms.

## Overview

| Aspect | Details |
|--------|---------|
| **Config Location** | `%APPDATA%`, `%LOCALAPPDATA%` |
| **System Config** | Registry |
| **Package Manager** | winget, Chocolatey, Scoop |
| **Shell** | PowerShell, CMD, WSL |

## Important Directories

### Environment Variables

| Variable | Typical Path | Purpose |
|----------|--------------|---------|
| `%USERPROFILE%` | `C:\Users\username` | Home directory |
| `%APPDATA%` | `C:\Users\username\AppData\Roaming` | Roaming app data |
| `%LOCALAPPDATA%` | `C:\Users\username\AppData\Local` | Local app data |
| `%PROGRAMDATA%` | `C:\ProgramData` | System-wide app data |
| `%PROGRAMFILES%` | `C:\Program Files` | 64-bit programs |
| `%PROGRAMFILES(X86)%` | `C:\Program Files (x86)` | 32-bit programs |
| `%TEMP%` / `%TMP%` | `C:\Users\username\AppData\Local\Temp` | Temporary files |
| `%SYSTEMROOT%` | `C:\Windows` | Windows directory |

### AppData Structure

```
C:\Users\username\AppData\
├── Local\                    # Machine-specific data
│   ├── Programs\             # User-installed programs
│   ├── Microsoft\
│   │   └── WindowsApps\      # Store apps
│   ├── Temp\                 # Temp files
│   ├── npm-cache\            # npm cache
│   └── myapp\                # Your app (cache, large data)
│
├── LocalLow\                 # Low-integrity apps (sandboxed)
│   └── (rarely used by devs)
│
└── Roaming\                  # Synced across domain machines
    ├── npm\                  # npm global packages
    ├── Code\                 # VS Code settings
    ├── .config\              # Some apps use XDG-style
    └── myapp\                # Your app config
```

### Local vs Roaming

| Location | Use For | Synced? |
|----------|---------|---------|
| **Roaming** | Settings, small config | Yes (domain) |
| **Local** | Cache, large files, machine-specific | No |
| **LocalLow** | Sandboxed/low-trust apps | No |

### Mapping to XDG

| XDG (Linux) | Windows Equivalent |
|-------------|-------------------|
| `XDG_CONFIG_HOME` | `%APPDATA%` |
| `XDG_DATA_HOME` | `%APPDATA%` |
| `XDG_CACHE_HOME` | `%LOCALAPPDATA%` |
| `XDG_STATE_HOME` | `%LOCALAPPDATA%` |
| `XDG_RUNTIME_DIR` | `%TEMP%` |

## Registry

### What Is It?

Hierarchical database storing system and application settings.

```
HKEY_CURRENT_USER (HKCU)     # Current user settings
├── Software\
│   ├── Microsoft\
│   └── MyCompany\
│       └── MyApp\
│           ├── Settings
│           └── WindowPosition
│
HKEY_LOCAL_MACHINE (HKLM)    # System-wide settings
├── SOFTWARE\
│   ├── Microsoft\
│   └── MyCompany\
│
HKEY_CLASSES_ROOT (HKCR)     # File associations
HKEY_USERS (HKU)             # All user profiles
HKEY_CURRENT_CONFIG (HKCC)   # Hardware config
```

### Registry vs Files

| Use Registry For | Use Files For |
|------------------|---------------|
| Simple settings | Complex config |
| Windows integration | Portability |
| File associations | Cross-platform |
| Admin-controlled settings | User-editable config |

### Registry Operations

```powershell
# PowerShell - Read
Get-ItemProperty -Path "HKCU:\Software\MyApp" -Name "Setting"

# PowerShell - Write
Set-ItemProperty -Path "HKCU:\Software\MyApp" -Name "Setting" -Value "NewValue"

# PowerShell - Create key
New-Item -Path "HKCU:\Software\MyApp" -Force

# PowerShell - Delete
Remove-ItemProperty -Path "HKCU:\Software\MyApp" -Name "Setting"
Remove-Item -Path "HKCU:\Software\MyApp" -Recurse
```

```csharp
// C# - Microsoft.Win32
using Microsoft.Win32;

// Read
using var key = Registry.CurrentUser.OpenSubKey(@"Software\MyApp");
var value = key?.GetValue("Setting") as string;

// Write
using var key = Registry.CurrentUser.CreateSubKey(@"Software\MyApp");
key.SetValue("Setting", "NewValue");
```

```python
# Python - winreg
import winreg

# Read
key = winreg.OpenKey(winreg.HKEY_CURRENT_USER, r"Software\MyApp")
value, _ = winreg.QueryValueEx(key, "Setting")
winreg.CloseKey(key)

# Write
key = winreg.CreateKey(winreg.HKEY_CURRENT_USER, r"Software\MyApp")
winreg.SetValueEx(key, "Setting", 0, winreg.REG_SZ, "NewValue")
winreg.CloseKey(key)
```

## Path Handling

### Path Separator

```python
# Wrong - hardcoded
path = "C:\\Users\\name\\file.txt"  # Windows only
path = "/home/name/file.txt"        # Unix only

# Right - use os.path or pathlib
from pathlib import Path
path = Path.home() / "Documents" / "file.txt"  # Works everywhere
```

### Drive Letters

```python
# Windows paths have drive letters
from pathlib import Path

p = Path("C:/Users/name/file.txt")
print(p.drive)   # 'C:'
print(p.root)    # '\\'
print(p.anchor)  # 'C:\\'

# UNC paths (network shares)
p = Path("//server/share/folder")
print(p.drive)   # '\\\\server\\share'
```

### Common Gotchas

```python
# Case insensitive (usually)
Path("C:/Users") == Path("c:/users")  # True on Windows

# But not always! WSL, some tools are case-sensitive

# Max path length (historically 260 chars)
# Modern Windows supports long paths if enabled

# Reserved names - can't create files named:
# CON, PRN, AUX, NUL, COM1-9, LPT1-9
```

## Services

### Windows Services

Long-running background processes (like systemd units on Linux).

```powershell
# List services
Get-Service

# Start/stop
Start-Service -Name "MyService"
Stop-Service -Name "MyService"
Restart-Service -Name "MyService"

# Status
Get-Service -Name "MyService" | Select-Object Status, StartType

# Create service (from elevated prompt)
New-Service -Name "MyService" -BinaryPathName "C:\path\to\app.exe"
```

### Task Scheduler

For scheduled/periodic tasks (like cron on Linux).

```powershell
# List tasks
Get-ScheduledTask

# Create scheduled task
$action = New-ScheduledTaskAction -Execute "C:\path\to\script.ps1"
$trigger = New-ScheduledTaskTrigger -Daily -At "3:00AM"
Register-ScheduledTask -TaskName "MyTask" -Action $action -Trigger $trigger
```

## Environment Variables

### Setting Variables

```powershell
# Session only
$env:MY_VAR = "value"

# User permanent (survives reboot)
[Environment]::SetEnvironmentVariable("MY_VAR", "value", "User")

# System permanent (requires admin)
[Environment]::SetEnvironmentVariable("MY_VAR", "value", "Machine")

# Add to PATH (user)
$path = [Environment]::GetEnvironmentVariable("PATH", "User")
[Environment]::SetEnvironmentVariable("PATH", "$path;C:\new\path", "User")
```

### Common Variables

| Variable | Example |
|----------|---------|
| `PATH` | Executable search path |
| `PATHEXT` | `.COM;.EXE;.BAT;.CMD;.PS1` |
| `COMPUTERNAME` | Machine name |
| `USERNAME` | Current user |
| `USERDOMAIN` | Domain name |

## Package Managers

### winget (Built-in)

```powershell
# Search
winget search vscode

# Install
winget install Microsoft.VisualStudioCode

# Upgrade all
winget upgrade --all

# List installed
winget list
```

### Chocolatey

```powershell
# Install Chocolatey (admin PowerShell)
Set-ExecutionPolicy Bypass -Scope Process -Force
iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))

# Install packages
choco install git nodejs python -y

# Upgrade
choco upgrade all -y
```

### Scoop (User-level)

```powershell
# Install Scoop (no admin needed)
irm get.scoop.sh | iex

# Install packages
scoop install git nodejs python

# Apps install to ~/scoop/apps/
```

## PowerShell Basics

### Essential Commands

| PowerShell | Bash Equivalent |
|------------|-----------------|
| `Get-ChildItem` / `ls` | `ls` |
| `Set-Location` / `cd` | `cd` |
| `Get-Content` / `cat` | `cat` |
| `Copy-Item` / `cp` | `cp` |
| `Move-Item` / `mv` | `mv` |
| `Remove-Item` / `rm` | `rm` |
| `Get-Process` | `ps` |
| `Stop-Process` | `kill` |
| `$env:VAR` | `$VAR` |

### PowerShell Profile

```powershell
# Location: $PROFILE
# Usually: C:\Users\name\Documents\PowerShell\Microsoft.PowerShell_profile.ps1

# Edit profile
notepad $PROFILE

# Example profile content
Set-Alias g git
Set-Alias k kubectl

function dev { Set-Location C:\dev }

# Oh-My-Posh prompt
oh-my-posh init pwsh | Invoke-Expression
```

## File Associations

### Query Association

```powershell
# What opens .txt files?
cmd /c assoc .txt
# .txt=txtfile

cmd /c ftype txtfile
# txtfile=%SystemRoot%\system32\NOTEPAD.EXE %1
```

### Register Application

```powershell
# Register file type (requires admin)
New-Item -Path "HKCR:\.myext" -Value "MyApp.Document" -Force
New-Item -Path "HKCR:\MyApp.Document\shell\open\command" `
         -Value '"C:\path\to\myapp.exe" "%1"' -Force
```

## Common Locations

| What | Location |
|------|----------|
| **User home** | `C:\Users\username` |
| **Desktop** | `%USERPROFILE%\Desktop` |
| **Documents** | `%USERPROFILE%\Documents` |
| **Downloads** | `%USERPROFILE%\Downloads` |
| **Start Menu** | `%APPDATA%\Microsoft\Windows\Start Menu` |
| **Startup** | `%APPDATA%\Microsoft\Windows\Start Menu\Programs\Startup` |
| **Fonts** | `C:\Windows\Fonts` or `%LOCALAPPDATA%\Microsoft\Windows\Fonts` |
| **Hosts file** | `C:\Windows\System32\drivers\etc\hosts` |

## Related

- [[WSL]] — Windows Subsystem for Linux
- [[XDG Base Directory]] — Linux equivalent
- [[macOS Development Basics]] — macOS equivalent
- [[Computer Science MOC]] — CS topics
