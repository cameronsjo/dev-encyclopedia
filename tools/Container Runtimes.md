---
title: Container Runtimes
aliases:
  - Docker Alternatives
  - Container Engines
tags:
  - tool
  - comparison
  - containers
  - docker
  - devops
type: comparison
status: complete
created: 2025-12-04
---

# Container Runtimes

Tools for building, running, and managing containers.

## Overview

| Tool | Type | Daemon | Rootless | Kubernetes | Platform |
|------|------|:------:|:--------:|:----------:|----------|
| Docker | Engine + CLI | ✅ | ✅ | Via K8s | All |
| Podman | Engine + CLI | ❌ | ✅ | Via K8s | Linux, macOS, Windows |
| containerd | Low-level runtime | ✅ | ✅ | ✅ Native | Linux, Windows |
| Rancher Desktop | GUI + Engine | ✅ | ✅ | ✅ Built-in | macOS, Windows, Linux |
| Colima | CLI wrapper | ✅ | ❌ | ✅ Optional | macOS, Linux |
| Lima | VM manager | ✅ | ❌ | ❌ | macOS, Linux |
| OrbStack | GUI + Engine | ✅ | ❌ | ✅ Built-in | macOS |

---

## Architecture Layers

```
┌─────────────────────────────────────────────────────────┐
│                    User Interface                        │
│     Docker CLI / Podman / nerdctl / Rancher Desktop     │
├─────────────────────────────────────────────────────────┤
│                   Container Engine                       │
│           dockerd / Podman / containerd                 │
├─────────────────────────────────────────────────────────┤
│                  Low-Level Runtime                       │
│              runc / crun / kata / gVisor                │
├─────────────────────────────────────────────────────────┤
│                   Linux Kernel                           │
│            namespaces, cgroups, seccomp                 │
└─────────────────────────────────────────────────────────┘
```

---

## Docker

The original container platform. Industry standard.

### Key Characteristics

- **Docker Engine** — Daemon-based container runtime
- **Docker CLI** — Primary interface
- **Docker Compose** — Multi-container orchestration
- **Docker Hub** — Default registry
- **Docker Desktop** — GUI for macOS/Windows

### Architecture

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│  Docker CLI  │────▶│   dockerd    │────▶│  containerd  │
└──────────────┘     │   (daemon)   │     └──────┬───────┘
                     └──────────────┘            │
                                                 ▼
                                          ┌──────────────┐
                                          │     runc     │
                                          └──────────────┘
```

### Common Commands

```bash
# Images
docker pull nginx:latest
docker build -t myapp .
docker push myregistry/myapp:v1

# Containers
docker run -d -p 8080:80 nginx
docker ps
docker exec -it container_id /bin/sh
docker logs -f container_id
docker stop container_id

# Compose
docker compose up -d
docker compose down
docker compose logs -f
```

### Docker Desktop

- GUI for container management
- Built-in Kubernetes
- Volume management
- Extension marketplace
- **Licensing**: Free for personal/small business, paid for enterprise

### Considerations

- Requires daemon (security surface)
- Docker Desktop licensing for enterprises
- Resource usage on macOS/Windows (VM)

---

## Podman

Daemonless container engine. Docker-compatible CLI.

### Key Characteristics

- **Daemonless** — No background service required
- **Rootless** — Run containers without root
- **Docker-compatible** — Same CLI commands
- **Pod support** — Group containers like Kubernetes
- **Systemd integration** — Generate unit files

### Architecture

```
┌──────────────┐     ┌──────────────┐
│  Podman CLI  │────▶│  Container   │  (no daemon)
└──────────────┘     │   Process    │
                     └──────┬───────┘
                            │
                            ▼
                     ┌──────────────┐
                     │  crun/runc   │
                     └──────────────┘
```

### Common Commands

```bash
# Alias for Docker compatibility
alias docker=podman

# Same commands work
podman pull nginx
podman run -d -p 8080:80 nginx
podman ps
podman exec -it container_id /bin/sh

# Pods (Podman-specific)
podman pod create --name mypod -p 8080:80
podman run -d --pod mypod nginx
podman run -d --pod mypod redis

# Generate Kubernetes YAML
podman generate kube mypod > pod.yaml

# Generate systemd unit
podman generate systemd --new --name mycontainer
```

### Podman Desktop

GUI application similar to Docker Desktop:
- Container/pod management
- Image building
- Kubernetes integration
- Extension support

### Podman vs Docker

| Aspect | Docker | Podman |
|--------|--------|--------|
| Daemon | Required | None |
| Root required | Default (rootless available) | Rootless default |
| CLI compatibility | Native | `alias docker=podman` |
| Compose | Docker Compose | podman-compose or Compose v2 |
| Pods | No | Yes (like K8s) |
| Systemd integration | Limited | Native |
| macOS/Windows | Docker Desktop | Podman Desktop + VM |
| License | Docker Desktop paid for enterprise | Free |

### Considerations

- Some Docker Compose features may differ
- macOS/Windows requires VM (like Docker)
- Smaller ecosystem than Docker
- Some edge cases with Docker compatibility

---

## containerd

Industry-standard low-level container runtime.

### Key Characteristics

- **Low-level** — Used by Docker, Kubernetes, others
- **CNCF graduated** — Production-ready, widely adopted
- **Kubernetes native** — CRI implementation
- **Minimal** — Runtime only, no build tools

### Usage

```bash
# nerdctl = Docker-compatible CLI for containerd
nerdctl run -d nginx
nerdctl build -t myapp .
nerdctl compose up

# ctr = low-level containerd CLI
ctr images pull docker.io/library/nginx:latest
ctr run docker.io/library/nginx:latest nginx-container
```

### When to Use Directly

- Kubernetes nodes (containerd as CRI)
- Minimal container runtime needs
- Building custom container platforms
- Embedded systems

---

## Rancher Desktop

GUI application providing container runtime and Kubernetes.

### Key Characteristics

- **GUI-first** — Visual container/K8s management
- **Choice of runtime** — containerd or dockerd
- **Built-in Kubernetes** — K3s distribution
- **Cross-platform** — macOS, Windows, Linux
- **Free** — Open source, no licensing fees

### Features

```
Rancher Desktop provides:
├── Container runtime (dockerd or containerd)
├── Kubernetes (K3s)
├── nerdctl or docker CLI
├── Helm dashboard
├── Image management
└── Port forwarding UI
```

### Configuration

```yaml
# Settings via GUI or ~/.rd/settings.json
{
  "kubernetes": {
    "version": "1.28.3",
    "enabled": true
  },
  "containerEngine": {
    "name": "containerd"  # or "moby" for dockerd
  }
}
```

### Rancher Desktop vs Docker Desktop

| Aspect | Rancher Desktop | Docker Desktop |
|--------|-----------------|----------------|
| License | Free (open source) | Free personal, paid enterprise |
| Kubernetes | K3s (built-in) | Docker's K8s |
| Container engine | containerd or dockerd | dockerd |
| CLI | nerdctl or docker | docker |
| Extensions | Limited | Marketplace |
| Resource usage | Similar | Similar |

---

## Colima

Minimal container runtime for macOS (and Linux).

### Key Characteristics

- **Lightweight** — Minimal VM wrapper
- **CLI-focused** — No GUI, just works
- **Lima-based** — Uses Lima VMs
- **Docker-compatible** — Works with Docker CLI
- **Kubernetes optional** — Via K3s

### Installation & Usage

```bash
# Install
brew install colima docker

# Start (creates VM with Docker)
colima start

# With resource limits
colima start --cpu 4 --memory 8 --disk 100

# With Kubernetes
colima start --kubernetes

# Use Docker CLI normally
docker ps
docker run -d nginx

# Multiple profiles
colima start --profile dev
colima start --profile test --cpu 2 --memory 4
```

### Configuration

```yaml
# ~/.colima/default/colima.yaml
cpu: 4
memory: 8
disk: 100

# VM type: qemu or vz (Virtualization.framework on macOS 13+)
vmType: vz

# Mount type
mountType: virtiofs  # or 9p, sshfs

kubernetes:
  enabled: true
  version: v1.28.3
```

### Colima vs Docker Desktop

| Aspect | Colima | Docker Desktop |
|--------|--------|----------------|
| Interface | CLI only | GUI + CLI |
| Resource usage | Lower | Higher |
| Startup time | Fast | Slower |
| Features | Minimal | Full-featured |
| Price | Free | Paid for enterprise |
| Kubernetes | K3s (optional) | Docker K8s |
| File sharing | Configurable | Automatic |

---

## Comparison Summary

### Feature Matrix

| Feature | Docker | Podman | Rancher Desktop | Colima |
|---------|:------:|:------:|:---------------:|:------:|
| Daemon required | ✅ | ❌ | ✅ | ✅ |
| Rootless | ✅ | ✅ Default | ✅ | ❌ |
| GUI | Desktop | Desktop | ✅ | ❌ |
| Kubernetes | ✅ | Via tools | ✅ K3s | ✅ K3s |
| Compose | ✅ | ✅ | ✅ | ✅ |
| Free for enterprise | ❌ | ✅ | ✅ | ✅ |
| Linux native | ✅ | ✅ | ✅ | VM |
| macOS | Via VM | Via VM | Via VM | Via VM |
| Windows | Via VM | Via VM | Via VM | ❌ |

### Performance (macOS)

| Tool | Startup | Memory | Disk I/O |
|------|---------|--------|----------|
| Docker Desktop | Slower | Higher | Good (VirtioFS) |
| Colima (vz) | Fast | Lower | Good (VirtioFS) |
| Podman | Medium | Medium | Medium |
| Rancher Desktop | Medium | Medium | Good |

---

## Decision Guide

| Use Case | Recommendation |
|----------|----------------|
| Enterprise, need support | Docker (paid) or Rancher |
| Free Docker Desktop alternative | Colima or Rancher Desktop |
| Rootless, daemonless | Podman |
| Minimal, CLI-only (macOS) | Colima |
| Need GUI + Kubernetes | Rancher Desktop |
| Kubernetes development | Rancher Desktop or Colima + K3s |
| CI/CD pipelines | Docker or Podman |
| Security-focused | Podman (rootless, daemonless) |

### Quick Picks

| Profile | Tool |
|---------|------|
| **Docker user, want free** | Colima + Docker CLI |
| **Security conscious** | Podman |
| **Need GUI + K8s** | Rancher Desktop |
| **Enterprise standard** | Docker |
| **Minimal setup** | Colima |

---

## Related

- [[Deployment]]
- [[Kubernetes]]
- [[WSL]]
