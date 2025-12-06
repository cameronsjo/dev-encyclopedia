---
title: Package Managers
aliases:
  - System Package Managers
  - OS Package Managers
tags:
  - tool
  - comparison
  - package-manager
type: comparison
status: complete
created: "2025-12-04"
---

# Package Managers

System-level package managers for Linux, macOS, and Windows.

## Overview by Platform

| Platform | Package Manager | Package Format |
|----------|-----------------|----------------|
| Debian/Ubuntu | apt | .deb |
| Fedora/RHEL | dnf | .rpm |
| Arch | pacman | .pkg.tar.zst |
| openSUSE | zypper | .rpm |
| Alpine | apk | .apk |
| NixOS/Nix | nix | .nar |
| macOS | Homebrew | bottles/formulae |
| macOS | MacPorts | ports |
| Windows | winget | .msix/.exe |
| Windows | Chocolatey | .nupkg |
| Windows | Scoop | manifests |

---

## Linux Package Managers

### apt (Debian/Ubuntu)

Advanced Package Tool. Standard on Debian-based systems.

```bash
# Update and upgrade
apt update                    # Refresh package lists
apt upgrade                   # Upgrade all packages
apt full-upgrade              # Upgrade, allowing removals

# Install/remove
apt install nginx             # Install
apt install nginx=1.18.0-1    # Specific version
apt remove nginx              # Remove (keep config)
apt purge nginx               # Remove with config
apt autoremove                # Remove unused dependencies

# Search/info
apt search nginx              # Search packages
apt show nginx                # Package details
apt list --installed          # List installed
apt list --upgradable         # List upgradable

# Cache
apt clean                     # Clear package cache
apt autoclean                 # Clear old package cache

# Low-level (dpkg)
dpkg -i package.deb           # Install local .deb
dpkg -l                       # List all installed
dpkg -L nginx                 # Files in package
dpkg -S /usr/bin/nginx        # Which package owns file
```

### dnf (Fedora/RHEL)

Dandified YUM. Modern Red Hat package manager.

```bash
# Update
dnf check-update              # Check for updates
dnf update                    # Update all
dnf update nginx              # Update specific

# Install/remove
dnf install nginx             # Install
dnf install nginx-1.20.0      # Specific version
dnf remove nginx              # Remove
dnf autoremove                # Remove unused deps

# Search/info
dnf search nginx              # Search
dnf info nginx                # Package info
dnf list installed            # List installed
dnf list available            # List available
dnf provides /usr/bin/nginx   # Find package by file

# Groups
dnf group list                # List groups
dnf group install "Development Tools"

# Modules (version streams)
dnf module list               # List modules
dnf module enable nodejs:18   # Enable stream
dnf module install nodejs:18  # Install from stream

# History/rollback
dnf history                   # Transaction history
dnf history undo 5            # Undo transaction 5

# Repository management
dnf repolist                  # List repos
dnf config-manager --add-repo URL
```

### pacman (Arch)

Simple, fast. Rolling release philosophy.

```bash
# Sync database
pacman -Sy                    # Sync repos
pacman -Syy                   # Force sync

# Install
pacman -S nginx               # Install
pacman -S nginx vim git       # Multiple packages
pacman -U package.pkg.tar.zst # Install local

# Remove
pacman -R nginx               # Remove
pacman -Rs nginx              # Remove with unused deps
pacman -Rns nginx             # Remove with deps and config

# Update
pacman -Syu                   # Full system upgrade
# NEVER do pacman -Sy package (partial upgrade)

# Search/info
pacman -Ss nginx              # Search repos
pacman -Qs nginx              # Search installed
pacman -Si nginx              # Remote info
pacman -Qi nginx              # Local info
pacman -Ql nginx              # List files in package
pacman -Qo /usr/bin/nginx     # Find owning package

# Cleanup
pacman -Sc                    # Clear cache (keep installed)
pacman -Scc                   # Clear all cache
paccache -r                   # Keep last 3 versions

# AUR (use helper)
yay -S package                # yay
paru -S package               # paru
```

### zypper (openSUSE)

SUSE's package manager. Rich feature set.

```bash
# Refresh/update
zypper refresh                # Refresh repos
zypper update                 # Update packages
zypper dup                    # Distribution upgrade

# Install/remove
zypper install nginx          # Install
zypper remove nginx           # Remove
zypper install -t pattern lamp_server  # Install pattern

# Search/info
zypper search nginx           # Search
zypper info nginx             # Info
zypper what-provides nginx    # What provides

# Repos
zypper repos                  # List repos
zypper addrepo URL alias      # Add repo
zypper removerepo alias       # Remove repo

# Locks
zypper addlock nginx          # Prevent updates
zypper removelock nginx       # Remove lock
```

### apk (Alpine)

Minimal, fast. Container-focused.

```bash
# Update
apk update                    # Update index
apk upgrade                   # Upgrade packages

# Install/remove
apk add nginx                 # Install
apk add --no-cache nginx      # Install without cache (Docker)
apk del nginx                 # Remove

# Search/info
apk search nginx              # Search
apk info nginx                # Info
apk info -L nginx             # List files

# Virtual packages (for build deps)
apk add --virtual .build-deps gcc musl-dev
# ... build ...
apk del .build-deps           # Remove all at once
```

### nix (NixOS and others)

Functional, declarative. Works on any Linux/macOS.

```bash
# Imperative commands
nix-env -iA nixpkgs.nginx     # Install
nix-env -e nginx              # Remove
nix-env -u                    # Upgrade all
nix-env -q                    # List installed

# Search
nix search nixpkgs nginx      # Search packages

# Generations (rollback)
nix-env --list-generations    # List generations
nix-env --rollback            # Rollback
nix-env -G 42                 # Switch to generation 42

# Garbage collection
nix-collect-garbage           # Remove unused
nix-collect-garbage -d        # Delete old generations too

# New nix command (experimental)
nix profile install nixpkgs#nginx
nix profile remove nginx
nix profile upgrade '.*'
```

---

## macOS Package Managers

### Homebrew

The missing package manager for macOS (also Linux).

```bash
# Install Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Update
brew update                   # Update Homebrew
brew upgrade                  # Upgrade all
brew upgrade nginx            # Upgrade specific

# Install
brew install nginx            # Install formula
brew install --cask firefox   # Install GUI app (cask)
brew install --formula nginx  # Explicitly formula

# Remove
brew uninstall nginx          # Remove
brew autoremove               # Remove unused deps

# Search/info
brew search nginx             # Search
brew info nginx               # Info
brew list                     # List installed
brew deps nginx               # Show dependencies
brew uses nginx --installed   # What depends on this

# Maintenance
brew cleanup                  # Remove old versions
brew doctor                   # Check for issues

# Taps (additional repos)
brew tap homebrew/cask-fonts  # Add tap
brew untap repo               # Remove tap

# Services
brew services list            # List services
brew services start nginx     # Start service
brew services stop nginx      # Stop service
```

### MacPorts

Traditional Unix ports. More Unix-like.

```bash
# Install MacPorts
# Download from macports.org

# Update
sudo port selfupdate          # Update MacPorts
sudo port upgrade outdated    # Upgrade packages

# Install
sudo port install nginx       # Install
sudo port install nginx +ssl  # With variant

# Remove
sudo port uninstall nginx     # Remove
sudo port uninstall --follow-dependents nginx

# Search/info
port search nginx             # Search
port info nginx               # Info
port installed                # List installed
port contents nginx           # List files

# Variants
port variants nginx           # Show variants
```

### Homebrew vs MacPorts

| Aspect | Homebrew | MacPorts |
|--------|----------|----------|
| Philosophy | Use system libs when possible | Self-contained |
| Install location | /opt/homebrew (M1) or /usr/local | /opt/local |
| Root required | No | Yes |
| Package count | ~6,000 formulae + casks | ~27,000 ports |
| GUI apps | Casks | Limited |
| Popularity | More popular | Traditional Unix users |

---

## Windows Package Managers

### winget (Windows Package Manager)

Microsoft's official package manager.

```powershell
# Search
winget search vscode          # Search
winget show Microsoft.VisualStudioCode  # Info

# Install
winget install Microsoft.VisualStudioCode
winget install -e --id Git.Git  # Exact ID match
winget install -h              # Silent install

# Update
winget upgrade                 # List upgradable
winget upgrade --all           # Upgrade all
winget upgrade Git.Git         # Upgrade specific

# Remove
winget uninstall Git.Git       # Uninstall

# List
winget list                    # List installed
winget list --source winget    # Only winget-installed

# Export/import
winget export -o packages.json # Export installed
winget import -i packages.json # Import/install
```

### Chocolatey

Community package manager. Large repository.

```powershell
# Install Chocolatey (admin PowerShell)
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Install
choco install git              # Install
choco install git -y           # Auto-confirm
choco install git --version=2.40.0  # Specific version

# Update
choco upgrade all              # Upgrade all
choco upgrade git              # Upgrade specific
choco outdated                 # List outdated

# Remove
choco uninstall git            # Uninstall

# Search/info
choco search git               # Search
choco info git                 # Info
choco list                     # List installed

# Features
choco feature enable -n allowGlobalConfirmation
```

### Scoop

Git-based, user-level installs. No admin required.

```powershell
# Install Scoop
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression

# Install
scoop install git              # Install
scoop install extras/vscode    # From bucket

# Update
scoop update                   # Update Scoop + buckets
scoop update *                 # Update all apps
scoop update git               # Update specific

# Remove
scoop uninstall git            # Uninstall

# Search
scoop search git               # Search
scoop info git                 # Info
scoop list                     # List installed

# Buckets (additional repos)
scoop bucket add extras        # Add bucket
scoop bucket list              # List buckets
scoop bucket known             # Known buckets

# Cleanup
scoop cleanup *                # Remove old versions
scoop cache rm *               # Clear download cache
```

### Windows: Which to Choose?

| Aspect | winget | Chocolatey | Scoop |
|--------|--------|------------|-------|
| Official | Yes | No | No |
| Packages | ~4,000 | ~10,000 | ~3,000+ |
| Admin required | Usually | Yes | No |
| GUI apps | Yes | Yes | Limited |
| Dev tools | Good | Excellent | Excellent |
| Scripting | Good | Mature | Good |
| Portable apps | No | Some | Yes (default) |

---

## Cross-Platform Comparison

### Common Operations

| Operation | apt | dnf | pacman | brew | winget |
|-----------|-----|-----|--------|------|--------|
| Update index | `apt update` | `dnf check-update` | `pacman -Sy` | `brew update` | — |
| Upgrade all | `apt upgrade` | `dnf update` | `pacman -Syu` | `brew upgrade` | `winget upgrade --all` |
| Install | `apt install X` | `dnf install X` | `pacman -S X` | `brew install X` | `winget install X` |
| Remove | `apt remove X` | `dnf remove X` | `pacman -R X` | `brew uninstall X` | `winget uninstall X` |
| Search | `apt search X` | `dnf search X` | `pacman -Ss X` | `brew search X` | `winget search X` |
| Info | `apt show X` | `dnf info X` | `pacman -Si X` | `brew info X` | `winget show X` |
| List installed | `apt list --installed` | `dnf list installed` | `pacman -Q` | `brew list` | `winget list` |
| Clean cache | `apt clean` | `dnf clean all` | `pacman -Sc` | `brew cleanup` | — |

---

## Related

- [[Linux Distributions]]
- [[Version Managers]]
- [[Shells]]
