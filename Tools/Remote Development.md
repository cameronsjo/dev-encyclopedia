---
title: Remote Development
aliases:
  - Remote Dev
  - Cloud Development
tags:
  - tool
  - comparison
  - development
type: comparison
status: complete
created: "2025-12-04"
---

# Remote Development

Tools and platforms for developing on remote machines and cloud environments.

## Overview

| Tool/Platform | Type | IDE Support | Cost |
|---------------|------|-------------|------|
| SSH + editor | Direct connection | Any | Free |
| VS Code Remote | IDE extension | VS Code | Free |
| JetBrains Gateway | IDE remote backend | JetBrains IDEs | License |
| GitHub Codespaces | Cloud environment | VS Code, JetBrains | Usage-based |
| Gitpod | Cloud environment | VS Code, JetBrains | Usage-based |
| Coder | Self-hosted platform | Any | Free/Enterprise |
| DevPod | Open-source devcontainers | Any | Free |

---

## SSH Configuration

The foundation of remote development.

### Basic Config

```bash
# ~/.ssh/config
Host dev-server
    HostName 192.168.1.100
    User developer
    Port 22
    IdentityFile ~/.ssh/id_ed25519

Host jump-host
    HostName bastion.example.com
    User admin

Host internal-server
    HostName 10.0.0.50
    User developer
    ProxyJump jump-host

Host *
    AddKeysToAgent yes
    IdentitiesOnly yes
    ServerAliveInterval 60
    ServerAliveCountMax 3
```

### Key Management

```bash
# Generate key
ssh-keygen -t ed25519 -C "user@example.com"

# Copy to server
ssh-copy-id dev-server

# Agent forwarding
ssh -A dev-server

# SSH agent
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
```

### Port Forwarding

```bash
# Local forward (access remote service locally)
ssh -L 8080:localhost:3000 dev-server
# localhost:8080 → remote:3000

# Remote forward (expose local service remotely)
ssh -R 9000:localhost:3000 dev-server
# remote:9000 → local:3000

# Dynamic (SOCKS proxy)
ssh -D 1080 dev-server

# In config
Host dev-server
    LocalForward 8080 localhost:3000
    RemoteForward 9000 localhost:3000
```

### Multiplexing

```bash
# ~/.ssh/config
Host *
    ControlMaster auto
    ControlPath ~/.ssh/sockets/%r@%h-%p
    ControlPersist 600

# First connection establishes socket
# Subsequent connections reuse it (instant)
```

---

## VS Code Remote

Extensions for remote development in VS Code.

### Remote Extensions

| Extension | Use Case |
|-----------|----------|
| Remote - SSH | Connect to any SSH host |
| Remote - Containers | Develop in containers |
| Remote - WSL | Develop in WSL |
| Remote - Tunnels | Connect via tunnel (no SSH) |
| Dev Containers | Devcontainer support |

### Remote - SSH

```bash
# Install extension
code --install-extension ms-vscode-remote.remote-ssh

# Connect
# Cmd/Ctrl+Shift+P → "Remote-SSH: Connect to Host"
# Or click green button bottom-left
```

Settings in VS Code:

```json
{
  "remote.SSH.remotePlatform": {
    "dev-server": "linux"
  },
  "remote.SSH.defaultExtensions": [
    "golang.go",
    "rust-lang.rust-analyzer"
  ]
}
```

### Dev Containers

```json
// .devcontainer/devcontainer.json
{
  "name": "Node.js",
  "image": "mcr.microsoft.com/devcontainers/javascript-node:18",
  "features": {
    "ghcr.io/devcontainers/features/docker-in-docker:2": {}
  },
  "forwardPorts": [3000],
  "postCreateCommand": "npm install",
  "customizations": {
    "vscode": {
      "extensions": [
        "dbaeumer.vscode-eslint",
        "esbenp.prettier-vscode"
      ],
      "settings": {
        "editor.formatOnSave": true
      }
    }
  }
}
```

```dockerfile
# .devcontainer/Dockerfile
FROM mcr.microsoft.com/devcontainers/base:ubuntu

RUN apt-get update && apt-get install -y \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install specific tools
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y nodejs
```

### Remote Tunnels

Connect without SSH, through Microsoft's tunnel service.

```bash
# On remote machine
code tunnel

# Creates a tunnel accessible via:
# - vscode.dev
# - VS Code desktop with Remote - Tunnels extension
```

---

## JetBrains Gateway

Remote development for JetBrains IDEs.

### How It Works

```
┌─────────────────┐          ┌─────────────────┐
│  Local Machine  │          │  Remote Server  │
│                 │          │                 │
│  ┌───────────┐  │   SSH    │  ┌───────────┐  │
│  │  Gateway  │◄─┼──────────┼─▶│  IDE      │  │
│  │  (thin)   │  │          │  │  Backend  │  │
│  └───────────┘  │          │  └───────────┘  │
└─────────────────┘          └─────────────────┘
```

### Setup

1. Download JetBrains Gateway
2. Configure SSH connection
3. Gateway installs IDE backend on remote
4. Develop with full IDE features

### Connection Options

| Method | Description |
|--------|-------------|
| SSH | Direct SSH connection |
| JetBrains Space | JetBrains cloud platform |
| Gitpod | Gitpod workspace |
| GitHub Codespaces | Codespace connection |
| Google Cloud | GCP workstations |

### Project Configuration

```yaml
# .idea/remote-dev.yaml
remoteProjectPath: /home/user/project
ide: IntelliJ IDEA Ultimate
```

---

## GitHub Codespaces

Cloud development environments from GitHub.

### Features

- **Instant environments** — Pre-built from repo
- **VS Code or JetBrains** — IDE choice
- **Dotfiles** — Personalization
- **Prebuilds** — Faster startup

### Configuration

```json
// .devcontainer/devcontainer.json
{
  "name": "My Project",
  "image": "mcr.microsoft.com/devcontainers/base:ubuntu",
  "features": {
    "ghcr.io/devcontainers/features/node:1": {
      "version": "20"
    },
    "ghcr.io/devcontainers/features/docker-in-docker:2": {}
  },
  "postCreateCommand": "npm install",
  "forwardPorts": [3000, 5432],
  "portsAttributes": {
    "3000": {
      "label": "Application",
      "onAutoForward": "openPreview"
    }
  }
}
```

### Prebuilds

```yaml
# .github/workflows/codespaces-prebuild.yml
# Configured in repo settings, not as workflow
# Settings → Codespaces → Prebuilds
```

### Usage

```bash
# CLI
gh codespace create
gh codespace list
gh codespace ssh
gh codespace code        # Open in VS Code
gh codespace delete

# Machine types
gh codespace create --machine largePremiumLinux
```

### Dotfiles

GitHub automatically clones your dotfiles repo:

```bash
# Create repo: github.com/username/dotfiles
# Contains:
# - .bashrc or .zshrc
# - .gitconfig
# - install.sh (runs automatically)
```

---

## Gitpod

Open-source cloud development platform.

### Features

- **Ephemeral environments** — Fresh every time
- **Prebuilds** — Pre-compile on push
- **VS Code and JetBrains** — IDE choice
- **Self-hostable** — Run on your infrastructure

### Configuration

```yaml
# .gitpod.yml
image:
  file: .gitpod.Dockerfile

tasks:
  - name: Setup
    init: |
      npm install
      npm run build
    command: npm run dev

  - name: Database
    command: docker-compose up db

ports:
  - port: 3000
    onOpen: open-preview
    visibility: public
  - port: 5432
    onOpen: ignore

vscode:
  extensions:
    - dbaeumer.vscode-eslint
    - esbenp.prettier-vscode

jetbrains:
  intellij:
    plugins:
      - com.intellij.plugins.vscodekeymap
```

```dockerfile
# .gitpod.Dockerfile
FROM gitpod/workspace-full

RUN npm install -g pnpm
```

### Prebuilds

```yaml
# .gitpod.yml
github:
  prebuilds:
    master: true
    branches: true
    pullRequests: true
    pullRequestsFromForks: false
    addComment: true
    addBadge: true
```

### Usage

```bash
# Open in Gitpod
# Prefix any GitHub URL with: gitpod.io/#
# https://gitpod.io/#https://github.com/user/repo

# CLI
gitpod workspace create
gitpod workspace list
```

---

## Coder

Self-hosted remote development platform.

### Features

- **Self-hosted** — Your infrastructure
- **Any IDE** — VS Code, JetBrains, Vim, etc.
- **Templates** — Terraform-based environments
- **Enterprise** — SSO, audit logs, quotas

### Template Example

```hcl
# template.tf
terraform {
  required_providers {
    coder = { source = "coder/coder" }
    docker = { source = "kreuzwerker/docker" }
  }
}

data "coder_workspace" "me" {}

resource "docker_container" "workspace" {
  name  = "coder-${data.coder_workspace.me.owner}-${data.coder_workspace.me.name}"
  image = "codercom/enterprise-base:ubuntu"

  env = [
    "CODER_AGENT_TOKEN=${coder_agent.main.token}"
  ]

  volumes {
    host_path      = "/home/coder"
    container_path = "/home/coder"
  }
}

resource "coder_agent" "main" {
  os   = "linux"
  arch = "amd64"
}

resource "coder_app" "code-server" {
  agent_id     = coder_agent.main.id
  slug         = "code-server"
  display_name = "VS Code"
  url          = "http://localhost:13337"
  icon         = "/icon/code.svg"
}
```

---

## DevPod

Open-source dev containers, anywhere.

### Features

- **Provider agnostic** — Local, SSH, cloud
- **Devcontainer spec** — Standard configuration
- **No vendor lock-in** — Open source
- **CLI and GUI** — Both available

### Usage

```bash
# Install
brew install loft-sh/tap/devpod

# Add provider
devpod provider add docker
devpod provider add ssh
devpod provider add aws

# Create workspace
devpod up github.com/user/repo
devpod up . --provider docker

# Open in IDE
devpod up . --ide vscode
devpod up . --ide goland
devpod up . --ide openvscode  # Browser

# SSH into workspace
devpod ssh my-workspace

# List
devpod list
devpod delete my-workspace
```

---

## Comparison

### Feature Matrix

| Feature | VS Code Remote | Gateway | Codespaces | Gitpod |
|---------|:--------------:|:-------:|:----------:|:------:|
| SSH | ✅ | ✅ | Via CLI | ❌ |
| Containers | ✅ | ✅ | ✅ | ✅ |
| Cloud hosted | Via tunnels | ❌ | ✅ | ✅ |
| Self-hostable | ❌ | ❌ | ❌ | ✅ |
| Prebuilds | ❌ | ❌ | ✅ | ✅ |
| Cost | Free | License | Usage | Usage |
| IDE | VS Code | JetBrains | Both | Both |

### Latency Considerations

| Approach | Latency | Best For |
|----------|---------|----------|
| SSH + terminal Vim/Neovim | Lowest | Experienced devs |
| VS Code Remote SSH | Low | General development |
| JetBrains Gateway | Low-Medium | JetBrains users |
| Cloud environments | Medium | Standardization |
| Browser-based | Higher | Quick access |

---

## Decision Guide

| Scenario | Recommendation |
|----------|----------------|
| Existing SSH server | VS Code Remote SSH |
| JetBrains user | JetBrains Gateway |
| Team standardization | Codespaces or Gitpod |
| Self-hosted requirement | Coder or Gitpod |
| Quick contribution | Codespaces (GitHub) |
| Open source, flexible | DevPod |
| Corporate firewall | Remote Tunnels |
| Low bandwidth | Terminal + Neovim |

---

## Related

- [[Terminal Emulators]]
- [[Terminal Multiplexers]]
- [[Container Runtimes]]
- [[WSL]]
