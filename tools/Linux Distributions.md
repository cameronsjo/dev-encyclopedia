---
title: Linux Distributions
aliases:
  - Linux Distros
  - Distros
tags:
  - tool
  - comparison
  - linux
  - operating-system
type: comparison
status: complete
created: "2025-12-04"
---

# Linux Distributions

Major Linux distributions, their families, and use cases.

## Overview

| Distro | Family | Package Manager | Release Model | Best For |
|--------|--------|-----------------|---------------|----------|
| Ubuntu | Debian | apt/deb | Fixed (LTS) | Desktop, servers, beginners |
| Debian | Debian | apt/deb | Fixed (Stable) | Servers, stability |
| Fedora | Red Hat | dnf/rpm | Fixed (6mo) | Developers, new tech |
| RHEL | Red Hat | dnf/rpm | Fixed (LTS) | Enterprise servers |
| Arch | Independent | pacman | Rolling | Power users |
| openSUSE | SUSE | zypper/rpm | Leap (fixed) / Tumbleweed (rolling) | Enterprise, KDE |
| Alpine | Independent | apk | Fixed | Containers, minimal |
| NixOS | Independent | nix | Rolling | Reproducible, declarative |

---

## Distribution Families

```
                    ┌─────────────┐
                    │   Debian    │
                    └──────┬──────┘
           ┌───────────────┼───────────────┐
           ▼               ▼               ▼
      ┌─────────┐    ┌──────────┐    ┌─────────┐
      │ Ubuntu  │    │   Mint   │    │  Kali   │
      └────┬────┘    └──────────┘    └─────────┘
           │
     ┌─────┴─────┐
     ▼           ▼
┌─────────┐ ┌─────────┐
│ Pop!_OS │ │ Kubuntu │
└─────────┘ └─────────┘


      ┌─────────────┐
      │   Red Hat   │
      └──────┬──────┘
    ┌────────┼────────┐
    ▼        ▼        ▼
┌──────┐ ┌──────┐ ┌────────┐
│ RHEL │ │Fedora│ │CentOS* │
└──┬───┘ └──────┘ └────────┘
   │
   ├──▶ AlmaLinux
   ├──▶ Rocky Linux
   └──▶ Oracle Linux


      ┌─────────────┐
      │    SUSE     │
      └──────┬──────┘
    ┌────────┴────────┐
    ▼                 ▼
┌────────┐      ┌──────────┐
│  SLES  │      │ openSUSE │
└────────┘      └──────────┘


      ┌─────────────┐
      │    Arch     │
      └──────┬──────┘
    ┌────────┼────────┐
    ▼        ▼        ▼
┌───────┐ ┌───────┐ ┌─────────┐
│Manjaro│ │EndeavOS││SteamOS 3│
└───────┘ └───────┘ └─────────┘
```

_CentOS Stream continues, CentOS Linux ended_

---

## Package Managers by Family

| Family | Package Format | CLI Tool | GUI Tool |
|--------|---------------|----------|----------|
| Debian | .deb | apt, dpkg | Synaptic, GNOME Software |
| Red Hat | .rpm | dnf, yum, rpm | GNOME Software, dnfdragora |
| SUSE | .rpm | zypper, rpm | YaST |
| Arch | .pkg.tar.zst | pacman | pamac |
| Alpine | .apk | apk | — |
| NixOS | .nar (closures) | nix | — |

---

## Debian Family

### Debian

The universal operating system. Rock-solid stability.

| Aspect | Details |
|--------|---------|
| Release cycle | ~2 years, 5 year support |
| Current | Debian 12 "Bookworm" |
| Branches | Stable, Testing, Unstable (Sid) |
| Philosophy | Free software, stability |

```bash
# Package management
apt update                    # Update package lists
apt upgrade                   # Upgrade packages
apt install nginx             # Install package
apt remove nginx              # Remove package
apt search nginx              # Search packages
apt show nginx                # Package info

# Low-level
dpkg -i package.deb           # Install .deb file
dpkg -l                       # List installed
dpkg -L nginx                 # List package files
```

### Ubuntu

Debian-based, user-friendly, most popular desktop Linux.

| Aspect | Details |
|--------|---------|
| Release cycle | 6 months (April, October) |
| LTS releases | Every 2 years, 5 year support (10 with Pro) |
| Current LTS | Ubuntu 24.04 "Noble Numbat" |
| Flavors | Ubuntu, Kubuntu, Xubuntu, Ubuntu Server |

```bash
# Same as Debian plus:
snap install code             # Snap packages
add-apt-repository ppa:...    # PPAs (personal package archives)
do-release-upgrade            # Upgrade to next release
ubuntu-drivers autoinstall    # Install drivers
```

### Linux Mint

Ubuntu-based, traditional desktop experience.

| Aspect | Details |
|--------|---------|
| Based on | Ubuntu LTS |
| Editions | Cinnamon, MATE, Xfce |
| Focus | Desktop usability, Windows migrants |

---

## Red Hat Family

### Fedora

Cutting-edge features, developer-focused.

| Aspect | Details |
|--------|---------|
| Release cycle | ~6 months |
| Support | 13 months per release |
| Editions | Workstation, Server, Silverblue (immutable) |
| Upstream | RHEL derives from Fedora |

```bash
# Package management
dnf update                    # Update all
dnf install nginx             # Install
dnf remove nginx              # Remove
dnf search nginx              # Search
dnf info nginx                # Package info
dnf groupinstall "Development Tools"  # Install group

# Modules (versioned streams)
dnf module list nodejs
dnf module enable nodejs:18
```

### RHEL (Red Hat Enterprise Linux)

Enterprise Linux with commercial support.

| Aspect | Details |
|--------|---------|
| Release cycle | ~3 years major, minor every 6 months |
| Support | 10 years (full) + extended |
| Use cases | Enterprise servers, production |
| Cost | Subscription-based |

### AlmaLinux / Rocky Linux

RHEL-compatible, community-driven replacements for CentOS.

| Aspect | AlmaLinux | Rocky Linux |
|--------|-----------|-------------|
| Backed by | CloudLinux | Rocky Enterprise Software Foundation |
| Binary compat | 1:1 with RHEL | 1:1 with RHEL |
| Use case | CentOS replacement | CentOS replacement |

---

## SUSE Family

### openSUSE

Community SUSE. Two variants.

| Variant | Description |
|---------|-------------|
| Leap | Fixed release, SLES-based, stable |
| Tumbleweed | Rolling release, bleeding edge |

```bash
# Package management
zypper refresh                # Update repos
zypper update                 # Upgrade packages
zypper install nginx          # Install
zypper remove nginx           # Remove
zypper search nginx           # Search
zypper info nginx             # Info

# Patterns (package groups)
zypper install -t pattern devel_basis

# YaST (admin tool)
yast                          # TUI configuration tool
```

### SLES (SUSE Linux Enterprise Server)

Enterprise SUSE with commercial support.

---

## Arch Family

### Arch Linux

DIY, bleeding-edge, minimalist.

| Aspect | Details |
|--------|---------|
| Release | Rolling (continuous updates) |
| Philosophy | KISS, user-centric |
| Installation | Manual (archinstall helper available) |
| Wiki | Legendary documentation |

```bash
# Package management
pacman -Syu                   # Full system upgrade
pacman -S nginx               # Install
pacman -R nginx               # Remove
pacman -Rs nginx              # Remove with unused deps
pacman -Ss nginx              # Search
pacman -Si nginx              # Info
pacman -Qs nginx              # Search installed

# AUR (Arch User Repository)
# Use AUR helper like yay or paru
yay -S visual-studio-code-bin
paru -S spotify
```

### Manjaro

Arch-based, user-friendly, delayed updates.

| Aspect | Details |
|--------|---------|
| Based on | Arch Linux |
| Release | Rolling (with testing delay) |
| Editions | GNOME, KDE, Xfce |
| Difference | Easier install, curated repos |

### EndeavourOS

Arch-based, closer to pure Arch than Manjaro.

---

## Specialty Distributions

### Alpine Linux

Minimal, security-focused. Docker standard.

| Aspect | Details |
|--------|---------|
| Size | ~5MB base |
| libc | musl (not glibc) |
| Init | OpenRC |
| Use case | Containers, embedded |

```bash
# Package management
apk update                    # Update index
apk upgrade                   # Upgrade packages
apk add nginx                 # Install
apk del nginx                 # Remove
apk search nginx              # Search
apk info nginx                # Info

# Key difference: musl compatibility
# Some binaries compiled for glibc won't work
```

```dockerfile
# Docker - Alpine saves space
FROM alpine:3.19              # ~5MB
RUN apk add --no-cache nodejs # Minimal install

# vs Debian
FROM debian:12                # ~120MB
RUN apt-get update && apt-get install -y nodejs
```

### NixOS

Declarative, reproducible configuration.

| Aspect | Details |
|--------|---------|
| Package manager | Nix (also runs on other OSes) |
| Configuration | Declarative (configuration.nix) |
| Rollbacks | Atomic, any generation |
| Reproducibility | Exact same system anywhere |

```nix
# /etc/nixos/configuration.nix
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    vim
    git
    firefox
  ];

  services.nginx.enable = true;

  users.users.alice = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
}
```

```bash
# Apply configuration
sudo nixos-rebuild switch

# Rollback
sudo nixos-rebuild switch --rollback

# List generations
sudo nix-env --list-generations
```

---

## Release Models

### Fixed Release

```
Fixed Release (Ubuntu LTS, RHEL, Debian Stable):

Version 1.0 ─────────────────────▶ EOL
              security patches only

Version 2.0 ─────────────────────▶ EOL
              security patches only

Pros: Stability, predictability
Cons: Older packages
```

### Rolling Release

```
Rolling Release (Arch, openSUSE Tumbleweed, NixOS):

─────────────────────────────────────────▶
     ▲      ▲      ▲      ▲      ▲
     │      │      │      │      │
  update update update update update

Pros: Latest packages always
Cons: Potential breakage, constant updates
```

---

## Decision Guide

| Use Case | Recommendation |
|----------|----------------|
| Desktop beginner | Ubuntu, Linux Mint, Pop!_OS |
| Developer workstation | Fedora, Ubuntu |
| Server (stability) | Debian, Ubuntu LTS, AlmaLinux |
| Enterprise (support) | RHEL, SLES, Ubuntu Pro |
| Containers | Alpine, Debian slim |
| Power user | Arch, EndeavourOS |
| Gaming | Pop!_OS, Nobara, Arch |
| Reproducible systems | NixOS |
| Learning Linux internals | Arch, Gentoo |
| KDE desktop | openSUSE, Fedora KDE, Kubuntu |
| Raspberry Pi | Raspberry Pi OS, Ubuntu |

### Quick Picks

| Profile | Distro |
|---------|--------|
| **Just works desktop** | Ubuntu or Linux Mint |
| **Developer** | Fedora or Ubuntu |
| **Server** | Debian or AlmaLinux |
| **Docker base** | Alpine |
| **I want control** | Arch |
| **Enterprise** | RHEL |
| **Declarative/reproducible** | NixOS |

---

## Related

- [[Package Managers]]
- [[WSL]]
- [[Shells]]
- [[Container Runtimes]]
