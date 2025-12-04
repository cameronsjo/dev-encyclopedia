---
title: Process Managers
aliases:
  - Service Managers
  - Init Systems
tags:
  - tool
  - comparison
  - devops
  - systems
type: comparison
status: complete
created: 2025-12-04
---

# Process Managers

Tools for managing, monitoring, and keeping processes running.

## Overview

| Tool | Type | Platform | Language | Use Case |
|------|------|----------|----------|----------|
| systemd | Init system | Linux | C | System services |
| launchd | Init system | macOS | C | System services |
| supervisord | Process manager | Cross-platform | Python | App processes |
| PM2 | Process manager | Cross-platform | Node.js | Node.js apps |
| runit | Init system | Linux/BSD | C | Simple services |
| s6 | Init system | Linux | C | Containers |
| Circus | Process manager | Cross-platform | Python | Multi-process apps |

---

## Use Case Comparison

```
System Init (boot services):
├── systemd (Linux standard)
├── launchd (macOS)
├── runit (lightweight Linux)
└── s6 (containers)

Application Process Manager:
├── PM2 (Node.js focus)
├── supervisord (Python apps, general)
└── Circus (multi-process)
```

---

## systemd

The standard Linux init system and service manager.

### Key Features

- **Unit files** — Declarative service definitions
- **Dependencies** — Service ordering and requirements
- **Socket activation** — Start on demand
- **Resource control** — cgroups integration
- **Journaling** — Centralized logging

### Unit File Example

```ini
# /etc/systemd/system/myapp.service
[Unit]
Description=My Application
After=network.target
Requires=postgresql.service

[Service]
Type=simple
User=myapp
WorkingDirectory=/opt/myapp
ExecStart=/opt/myapp/bin/server
ExecReload=/bin/kill -HUP $MAINPID
Restart=on-failure
RestartSec=5s
StandardOutput=journal
StandardError=journal
Environment=NODE_ENV=production

[Install]
WantedBy=multi-user.target
```

### Common Commands

```bash
# Service control
sudo systemctl start myapp
sudo systemctl stop myapp
sudo systemctl restart myapp
sudo systemctl reload myapp       # If supported
sudo systemctl status myapp

# Enable/disable on boot
sudo systemctl enable myapp
sudo systemctl disable myapp

# List services
systemctl list-units --type=service
systemctl list-units --state=failed

# View logs
journalctl -u myapp               # All logs
journalctl -u myapp -f            # Follow
journalctl -u myapp --since "1 hour ago"
journalctl -u myapp -n 100        # Last 100 lines

# Reload unit files after changes
sudo systemctl daemon-reload
```

### Service Types

| Type | Behavior |
|------|----------|
| `simple` | Process stays in foreground (default) |
| `exec` | Like simple, but started after exec() |
| `forking` | Traditional daemon (forks to background) |
| `oneshot` | Run once, then exit |
| `notify` | Like simple, but notifies when ready |

### Resource Limits

```ini
[Service]
# Memory limits
MemoryMax=500M
MemoryHigh=400M

# CPU limits
CPUQuota=50%
CPUWeight=100

# File limits
LimitNOFILE=65535

# Security
NoNewPrivileges=true
ProtectSystem=strict
ProtectHome=true
PrivateTmp=true
```

---

## launchd

macOS's init and service management system.

### Key Concepts

- **LaunchAgents** — Per-user services
- **LaunchDaemons** — System-wide services
- **Property lists** — XML/plist configuration
- **On-demand loading** — Start when needed

### Plist Locations

| Path | Scope | Runs As |
|------|-------|---------|
| `~/Library/LaunchAgents` | User | Current user |
| `/Library/LaunchAgents` | All users | Current user |
| `/Library/LaunchDaemons` | System | root |
| `/System/Library/Launch*` | System (Apple) | Various |

### Plist Example

```xml
<!-- ~/Library/LaunchAgents/com.myapp.server.plist -->
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.myapp.server</string>

    <key>ProgramArguments</key>
    <array>
        <string>/usr/local/bin/myapp</string>
        <string>--port=3000</string>
    </array>

    <key>WorkingDirectory</key>
    <string>/opt/myapp</string>

    <key>RunAtLoad</key>
    <true/>

    <key>KeepAlive</key>
    <true/>

    <key>StandardOutPath</key>
    <string>/var/log/myapp/stdout.log</string>

    <key>StandardErrorPath</key>
    <string>/var/log/myapp/stderr.log</string>

    <key>EnvironmentVariables</key>
    <dict>
        <key>NODE_ENV</key>
        <string>production</string>
    </dict>
</dict>
</plist>
```

### Common Commands

```bash
# Load/unload service
launchctl load ~/Library/LaunchAgents/com.myapp.server.plist
launchctl unload ~/Library/LaunchAgents/com.myapp.server.plist

# Modern commands (macOS 10.10+)
launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.myapp.plist
launchctl bootout gui/$(id -u) ~/Library/LaunchAgents/com.myapp.plist

# Start/stop
launchctl start com.myapp.server
launchctl stop com.myapp.server

# List services
launchctl list
launchctl list | grep myapp

# View service info
launchctl print gui/$(id -u)/com.myapp.server
```

### Homebrew Services

```bash
# Homebrew wraps launchd
brew services list
brew services start postgresql
brew services stop postgresql
brew services restart postgresql

# View generated plist
cat ~/Library/LaunchAgents/homebrew.mxcl.postgresql.plist
```

---

## supervisord

Python-based process manager. Cross-platform.

### Key Features

- **Simple config** — INI file format
- **Web UI** — Optional HTTP interface
- **Process groups** — Manage related processes
- **Event system** — Hook into process lifecycle
- **Cross-platform** — Linux, macOS, Windows

### Configuration

```ini
; /etc/supervisor/conf.d/myapp.conf
[program:myapp]
command=/opt/myapp/venv/bin/python app.py
directory=/opt/myapp
user=myapp
autostart=true
autorestart=true
startsecs=5
startretries=3
redirect_stderr=true
stdout_logfile=/var/log/myapp/app.log
stdout_logfile_maxbytes=10MB
stdout_logfile_backups=5
environment=PYTHONPATH="/opt/myapp",NODE_ENV="production"

[program:myapp-worker]
command=/opt/myapp/venv/bin/celery -A app worker
directory=/opt/myapp
user=myapp
numprocs=4
process_name=%(program_name)s_%(process_num)02d
autostart=true
autorestart=true
```

### Process Groups

```ini
[group:myapp]
programs=myapp,myapp-worker

; Control all at once:
; supervisorctl start myapp:*
; supervisorctl stop myapp:*
```

### Common Commands

```bash
# Service control
supervisorctl status
supervisorctl start myapp
supervisorctl stop myapp
supervisorctl restart myapp
supervisorctl start all
supervisorctl stop all

# Reload config
supervisorctl reread          # Check for changes
supervisorctl update          # Apply changes

# Tail logs
supervisorctl tail myapp
supervisorctl tail -f myapp

# Interactive shell
supervisorctl
supervisor> help
```

### Web Interface

```ini
; /etc/supervisor/supervisord.conf
[inet_http_server]
port=127.0.0.1:9001
username=admin
password=secret
```

---

## PM2

Node.js process manager with advanced features.

### Key Features

- **Cluster mode** — Multi-core load balancing
- **Zero-downtime reload** — Graceful restarts
- **Ecosystem file** — Declarative configuration
- **Monitoring** — Built-in metrics, web dashboard
- **Log management** — Rotation, viewing

### Basic Usage

```bash
# Start application
pm2 start app.js
pm2 start app.js --name myapp
pm2 start app.js -i max          # Cluster mode, all cores
pm2 start app.js -i 4            # 4 instances

# Control
pm2 stop myapp
pm2 restart myapp
pm2 reload myapp                  # Zero-downtime
pm2 delete myapp

# Status and logs
pm2 list
pm2 status
pm2 logs
pm2 logs myapp
pm2 monit                         # Real-time dashboard

# Save and restore
pm2 save                          # Save current processes
pm2 startup                       # Generate startup script
pm2 resurrect                     # Restore saved processes
```

### Ecosystem File

```javascript
// ecosystem.config.js
module.exports = {
  apps: [
    {
      name: 'myapp',
      script: './app.js',
      instances: 'max',
      exec_mode: 'cluster',
      watch: false,
      max_memory_restart: '1G',
      env: {
        NODE_ENV: 'development'
      },
      env_production: {
        NODE_ENV: 'production',
        PORT: 3000
      }
    },
    {
      name: 'worker',
      script: './worker.js',
      instances: 2,
      exec_mode: 'cluster'
    }
  ]
};
```

```bash
# Use ecosystem file
pm2 start ecosystem.config.js
pm2 start ecosystem.config.js --env production

# Deploy (if deploy section defined)
pm2 deploy production setup
pm2 deploy production
```

### Non-Node.js Apps

```bash
# Python
pm2 start app.py --interpreter python3

# Bash script
pm2 start script.sh --interpreter bash

# Binary
pm2 start ./myapp

# General
pm2 start "command to run" --name myprocess
```

### PM2 Plus (Monitoring)

```bash
# Link to PM2 Plus dashboard
pm2 plus

# Or use local monitoring
pm2 monit
```

---

## Comparison

### Feature Matrix

| Feature | systemd | launchd | supervisord | PM2 |
|---------|:-------:|:-------:|:-----------:|:---:|
| Init system | ✅ | ✅ | ❌ | ❌ |
| Process manager | ✅ | ✅ | ✅ | ✅ |
| Cluster mode | ❌ | ❌ | ❌ | ✅ |
| Zero-downtime | Manual | Manual | ❌ | ✅ |
| Web UI | ❌ | ❌ | ✅ | ✅ |
| Log rotation | journald | Manual | ✅ | ✅ |
| Resource limits | ✅ | ✅ | ❌ | ✅ |
| Cross-platform | Linux | macOS | ✅ | ✅ |
| Config format | INI | Plist | INI | JS/JSON |

### Performance Overhead

| Tool | Memory | Startup | Complexity |
|------|--------|---------|------------|
| systemd | Low | Fast | Medium |
| launchd | Low | Fast | Medium |
| supervisord | Medium | Medium | Low |
| PM2 | Higher | Medium | Low |

---

## Use Case Guide

| Scenario | Recommendation |
|----------|----------------|
| Linux system services | systemd |
| macOS services | launchd (or Homebrew services) |
| Python/Django apps | supervisord or systemd |
| Node.js production | PM2 |
| Multi-process apps | PM2 or supervisord |
| Docker containers | s6, runit, or direct |
| Dev environment | PM2 or direct |
| Enterprise Linux | systemd |

### Quick Setup by Language

```bash
# Node.js → PM2
npm install -g pm2
pm2 start app.js
pm2 startup

# Python → supervisord
pip install supervisor
# Create /etc/supervisor/conf.d/myapp.conf
supervisorctl update

# Any language → systemd
# Create /etc/systemd/system/myapp.service
sudo systemctl enable myapp
sudo systemctl start myapp
```

---

## Container Considerations

### Inside Containers

| Scenario | Recommendation |
|----------|----------------|
| Single process | Run directly (no manager) |
| Multiple processes | s6, runit, or supervisord |
| Process supervision | s6-overlay or dumb-init |

```dockerfile
# Single process (preferred)
CMD ["node", "app.js"]

# With s6-overlay (multiple processes)
FROM alpine
RUN apk add s6-overlay
COPY services.d /etc/services.d
ENTRYPOINT ["/init"]
```

### Outside Containers

Use systemd/launchd to manage Docker/Podman containers:

```ini
# /etc/systemd/system/myapp-container.service
[Service]
ExecStart=/usr/bin/docker run --rm --name myapp myimage
ExecStop=/usr/bin/docker stop myapp
Restart=always
```

---

## Related

- [[Container Runtimes]]
- [[Deployment]]
- [[Linux Distributions]]
