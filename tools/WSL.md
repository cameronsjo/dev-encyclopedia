---
title: WSL
aliases:
  - Windows Subsystem for Linux
  - WSL2
tags:
  - tool
  - windows
  - linux
  - virtualization
type: reference
status: complete
created: 2025-12-04
---

# WSL

Windows Subsystem for Linux — run Linux directly on Windows.

## Overview

| Aspect | WSL 1 | WSL 2 |
|--------|-------|-------|
| Architecture | Translation layer | Full Linux kernel |
| File system | NTFS via translation | ext4 in virtual disk |
| System call compatibility | ~70% | 100% |
| Memory usage | Lower | Higher (VM) |
| Cross-OS file access | Fast | Slower (9P protocol) |
| Docker support | Limited | Full native |
| GPU support | No | Yes (CUDA, DirectML) |
| Networking | Shared with Windows | Virtual adapter |

---

## Installation

### Windows 10/11 (Modern)

```powershell
# Install WSL with Ubuntu (default)
wsl --install

# Install specific distro
wsl --install -d Debian

# List available distros
wsl --list --online
```

### Available Distributions

| Distro | Command |
|--------|---------|
| Ubuntu | `wsl --install -d Ubuntu` |
| Ubuntu 22.04 | `wsl --install -d Ubuntu-22.04` |
| Debian | `wsl --install -d Debian` |
| Fedora | Via Fedora Remix or manual |
| Arch | Via ArchWSL or manual |
| Alpine | `wsl --install -d Alpine` |
| openSUSE | `wsl --install -d openSUSE-Leap-15.5` |

---

## WSL 1 vs WSL 2

### Architecture Comparison

```
WSL 1:
┌─────────────────────────────────────┐
│           Windows Kernel            │
├─────────────────────────────────────┤
│    Linux Syscall Translation Layer  │
├─────────────────────────────────────┤
│         Linux User Space            │
└─────────────────────────────────────┘

WSL 2:
┌─────────────────────────────────────┐
│           Windows Kernel            │
├─────────────────────────────────────┤
│    Lightweight Hyper-V VM           │
│  ┌───────────────────────────────┐  │
│  │      Real Linux Kernel        │  │
│  ├───────────────────────────────┤  │
│  │      Linux User Space         │  │
│  └───────────────────────────────┘  │
└─────────────────────────────────────┘
```

### When to Use Each

| Use Case | Recommendation |
|----------|----------------|
| Docker/containers | WSL 2 |
| Full syscall compatibility | WSL 2 |
| Working with Windows files | WSL 1 |
| Low memory usage | WSL 1 |
| GPU workloads | WSL 2 |
| Network tools (low-level) | WSL 2 |

### Switch Versions

```powershell
# Set WSL 2 as default
wsl --set-default-version 2

# Convert existing distro
wsl --set-version Ubuntu 2

# Check version of installed distros
wsl -l -v
```

---

## Configuration

### Per-Distro Config

```bash
# /etc/wsl.conf (inside Linux)

[boot]
systemd = true              # Enable systemd (WSL 2)

[automount]
enabled = true
root = /mnt/
options = "metadata,umask=22,fmask=11"

[network]
hostname = dev-machine
generateHosts = true
generateResolvConf = true

[interop]
enabled = true              # Run Windows executables
appendWindowsPath = true    # Include Windows PATH
```

### Global Config

```ini
# %USERPROFILE%\.wslconfig (Windows side)

[wsl2]
memory = 8GB                # Limit memory
processors = 4              # Limit CPUs
swap = 2GB
localhostForwarding = true

# Experimental features
[experimental]
sparseVhd = true           # Reclaim disk space
autoMemoryReclaim = gradual
```

---

## File System

### Path Translation

```bash
# Windows paths from WSL
/mnt/c/Users/username/Documents

# WSL paths from Windows
\\wsl$\Ubuntu\home\username

# Or via wsl command
wsl --cd ~
```

### Performance Considerations

```
File access speed:

WSL 2:
├── Linux files (ext4) ──────────▶ Fast
├── Windows files (/mnt/c) ──────▶ Slow (9P protocol)
└── Recommendation: Keep projects in Linux FS

WSL 1:
├── Linux files ─────────────────▶ Medium
├── Windows files (/mnt/c) ──────▶ Fast (native NTFS)
└── Good for cross-FS work
```

### Best Practice

```bash
# Clone repos to Linux filesystem, not /mnt/c
cd ~
git clone https://github.com/user/project.git

# NOT this (slow in WSL 2)
cd /mnt/c/Users/username/projects
git clone ...
```

---

## Docker Integration

### Docker Desktop with WSL 2

```
┌──────────────────────────────────────────┐
│              Docker Desktop              │
├──────────────────────────────────────────┤
│    WSL 2 Backend (docker-desktop-data)   │
├────────────────────┬─────────────────────┤
│      Ubuntu        │      Debian         │
│  docker CLI works  │  docker CLI works   │
└────────────────────┴─────────────────────┘
```

Settings:
1. Docker Desktop → Settings → General → Use WSL 2 based engine
2. Settings → Resources → WSL Integration → Enable for your distros

### Without Docker Desktop

```bash
# Install Docker in WSL 2 directly
sudo apt update
sudo apt install docker.io

# Start Docker (with systemd)
sudo systemctl start docker
sudo systemctl enable docker

# Add user to docker group
sudo usermod -aG docker $USER
```

---

## Networking

### WSL 2 Networking

```
WSL 2 has its own virtual network adapter:

Windows ◄───────► Virtual Switch ◄───────► WSL 2 VM
192.168.x.x                                172.x.x.x
```

```bash
# Get WSL IP
hostname -I

# Get Windows host IP (from WSL)
cat /etc/resolv.conf | grep nameserver

# Access Windows localhost from WSL
# Use host IP or localhost (with localhostForwarding)

# Access WSL server from Windows
# localhost:port works with port forwarding
```

### Port Forwarding

```powershell
# WSL 2 auto-forwards ports to localhost
# Access WSL service at localhost:3000 from Windows

# Manual port proxy (if needed)
netsh interface portproxy add v4tov4 `
  listenport=3000 listenaddress=0.0.0.0 `
  connectport=3000 connectaddress=172.x.x.x
```

---

## GUI Apps (WSLg)

Windows 11 / Windows 10 21H2+ supports Linux GUI apps.

```bash
# Install GUI app
sudo apt install gedit

# Just run it
gedit &

# Apps appear in Windows Start menu
# Audio and clipboard work automatically
```

### GPU Support

```bash
# NVIDIA CUDA in WSL 2
# Install NVIDIA driver on Windows (not in WSL)

# Verify
nvidia-smi

# TensorFlow/PyTorch work with GPU
pip install tensorflow
python -c "import tensorflow as tf; print(tf.config.list_physical_devices('GPU'))"
```

---

## Development Workflow

### VS Code Integration

```bash
# Open VS Code from WSL
code .

# VS Code Remote - WSL extension
# - Edit Linux files with Windows VS Code
# - Terminal runs in WSL
# - Extensions run in WSL
```

### JetBrains IDEs

```
Options:
├── JetBrains Gateway (remote dev)
├── IDE in Windows, interpreter in WSL
└── IDE running in WSL via WSLg
```

### Git Configuration

```bash
# Consistent line endings
git config --global core.autocrlf input

# Credential sharing with Windows
git config --global credential.helper "/mnt/c/Program\ Files/Git/mingw64/bin/git-credential-manager.exe"
```

---

## Common Commands

### WSL Management

```powershell
# List installed distros
wsl -l -v

# Run specific distro
wsl -d Ubuntu

# Run as specific user
wsl -u root

# Terminate distro
wsl -t Ubuntu

# Shutdown all WSL
wsl --shutdown

# Export distro
wsl --export Ubuntu ubuntu-backup.tar

# Import distro
wsl --import MyUbuntu C:\WSL\MyUbuntu ubuntu-backup.tar

# Unregister (delete) distro
wsl --unregister Ubuntu

# Update WSL
wsl --update
```

### From Inside WSL

```bash
# Run Windows commands
cmd.exe /c dir
powershell.exe Get-Process
explorer.exe .

# Open Windows apps
notepad.exe file.txt
code .

# Access Windows env vars
echo $WSLENV
cmd.exe /c "echo %USERNAME%"
```

---

## Troubleshooting

### Common Issues

| Issue | Solution |
|-------|----------|
| Slow file access | Move files to Linux FS (`~`) |
| High memory usage | Set `memory` limit in `.wslconfig` |
| Network issues | `wsl --shutdown` and restart |
| DNS not working | Check `/etc/resolv.conf`, regenerate |
| Docker not working | Enable WSL 2 integration in Docker Desktop |
| systemd not starting | Add `systemd=true` to `/etc/wsl.conf` |

### Reset Networking

```powershell
# Windows side
wsl --shutdown
netsh winsock reset
netsh int ip reset
# Restart computer
```

### Reclaim Disk Space

```powershell
# WSL 2 VHD doesn't shrink automatically
wsl --shutdown

# Compact the disk (PowerShell as admin)
Optimize-VHD -Path "C:\Users\<user>\AppData\Local\Packages\<distro>\LocalState\ext4.vhdx" -Mode Full

# Or enable sparse VHD in .wslconfig
# [experimental]
# sparseVhd = true
```

---

## Performance Tips

1. **Keep code in Linux FS** — `/home/user`, not `/mnt/c`
2. **Use WSL 2** — Better performance for most tasks
3. **Limit memory** — Set `.wslconfig` memory limit
4. **Exclude from antivirus** — Add WSL paths to Windows Defender exclusions
5. **Use VS Code Remote** — Better than editing `/mnt/c` files
6. **Enable sparse VHD** — Automatic disk space reclamation

---

## Related

- [[Shells]]
- [[Container Runtimes]]
- [[Terminal Emulators]]
