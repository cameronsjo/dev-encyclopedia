---
title: Container Tools
aliases:
  - Docker Tools
  - Container Ecosystem
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

# Container Tools

FOSS tools for managing, monitoring, and orchestrating containers.

## Overview

| Category | Tools |
|----------|-------|
| Management UI | Portainer, Dockge, Yacht |
| Reverse Proxy | Traefik, Caddy, nginx-proxy |
| Auto-Update | Watchtower, Diun |
| Orchestration | Docker Swarm, Nomad |
| Registry | Harbor, Distribution, Zot |
| Networking | Tailscale, Cloudflare Tunnel |
| Backups | Duplicati, Restic, Borgmatic |
| Secrets | Docker Secrets, Vault, SOPS |

---

## Container Management

### Portainer

Web UI for managing Docker/Kubernetes environments.

```yaml
# docker-compose.yaml
services:
  portainer:
    image: portainer/portainer-ce:latest
    ports:
      - "9443:9443"
      - "9000:9000"
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock
      - portainer_data:/data
    restart: unless-stopped

volumes:
  portainer_data:
```

**Features:**
- Container/stack management
- Docker Compose support
- Multi-environment (Docker, Swarm, K8s)
- User management and RBAC
- Templates and app store
- Edge agent for remote hosts

### Dockge

Lightweight Docker Compose stack manager.

```yaml
# docker-compose.yaml
services:
  dockge:
    image: louislam/dockge:1
    ports:
      - "5001:5001"
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock
      - ./data:/app/data
      - /opt/stacks:/opt/stacks  # Your compose files
    environment:
      - DOCKGE_STACKS_DIR=/opt/stacks
    restart: unless-stopped
```

**Features:**
- Simple, clean UI
- Edit compose files in browser
- Start/stop/restart stacks
- View logs
- Terminal access

### Yacht

Docker container management UI.

```yaml
services:
  yacht:
    image: selfhostedpro/yacht
    ports:
      - "8000:8000"
    volumes:
      - yacht:/config
      - /var/run/docker.sock:/var/run/docker.sock
    restart: unless-stopped
```

### Comparison

| Feature | Portainer | Dockge | Yacht |
|---------|:---------:|:------:|:-----:|
| Compose support | ✅ | ✅ | ✅ |
| Kubernetes | ✅ | ❌ | ❌ |
| Multi-host | ✅ | ❌ | ❌ |
| Resource usage | Medium | Low | Low |
| Complexity | Higher | Simple | Simple |

---

## Reverse Proxy / Ingress

### Traefik

Cloud-native reverse proxy with auto-discovery.

```yaml
# docker-compose.yaml
services:
  traefik:
    image: traefik:v3.0
    command:
      - "--api.dashboard=true"
      - "--providers.docker=true"
      - "--providers.docker.exposedbydefault=false"
      - "--entrypoints.web.address=:80"
      - "--entrypoints.websecure.address=:443"
      - "--certificatesresolvers.letsencrypt.acme.httpchallenge=true"
      - "--certificatesresolvers.letsencrypt.acme.email=admin@example.com"
      - "--certificatesresolvers.letsencrypt.acme.storage=/letsencrypt/acme.json"
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock:ro
      - letsencrypt:/letsencrypt
    labels:
      - "traefik.enable=true"
      - "traefik.http.routers.dashboard.rule=Host(`traefik.example.com`)"
      - "traefik.http.routers.dashboard.service=api@internal"

  whoami:
    image: traefik/whoami
    labels:
      - "traefik.enable=true"
      - "traefik.http.routers.whoami.rule=Host(`whoami.example.com`)"
      - "traefik.http.routers.whoami.entrypoints=websecure"
      - "traefik.http.routers.whoami.tls.certresolver=letsencrypt"
```

**Features:**
- Auto-discovery via Docker labels
- Let's Encrypt integration
- Middleware (auth, rate limit, etc.)
- Dashboard
- Metrics (Prometheus)
- Multiple backends (Docker, K8s, file, etc.)

### Caddy

Modern web server with automatic HTTPS.

```yaml
services:
  caddy:
    image: caddy:2
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./Caddyfile:/etc/caddy/Caddyfile
      - caddy_data:/data
      - caddy_config:/config
```

```caddyfile
# Caddyfile
app.example.com {
    reverse_proxy app:3000
}

api.example.com {
    reverse_proxy api:8080
}

# Automatic HTTPS by default
```

### nginx-proxy

Automated nginx reverse proxy for Docker.

```yaml
services:
  nginx-proxy:
    image: nginxproxy/nginx-proxy
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - /var/run/docker.sock:/tmp/docker.sock:ro
      - certs:/etc/nginx/certs
      - vhost:/etc/nginx/vhost.d
      - html:/usr/share/nginx/html

  acme-companion:
    image: nginxproxy/acme-companion
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock:ro
      - certs:/etc/nginx/certs
      - acme:/etc/acme.sh
    environment:
      - DEFAULT_EMAIL=admin@example.com
      - NGINX_PROXY_CONTAINER=nginx-proxy

  app:
    image: myapp
    environment:
      - VIRTUAL_HOST=app.example.com
      - LETSENCRYPT_HOST=app.example.com
```

### Comparison

| Feature | Traefik | Caddy | nginx-proxy |
|---------|:-------:|:-----:|:-----------:|
| Auto-discovery | ✅ | Config file | ✅ |
| Auto HTTPS | ✅ | ✅ | With companion |
| Dashboard | ✅ | ❌ | ❌ |
| Config style | Labels | Caddyfile | Env vars |
| Middleware | Extensive | Good | Basic |
| Performance | Good | Excellent | Excellent |

---

## Auto-Update

### Watchtower

Automatically update running containers.

```yaml
services:
  watchtower:
    image: containrrr/watchtower
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock
    environment:
      - WATCHTOWER_CLEANUP=true
      - WATCHTOWER_POLL_INTERVAL=86400  # Daily
      - WATCHTOWER_NOTIFICATIONS=email
      - WATCHTOWER_NOTIFICATION_EMAIL_TO=admin@example.com
    restart: unless-stopped
```

**Options:**
```yaml
environment:
  # Update schedule
  - WATCHTOWER_SCHEDULE=0 0 4 * * *  # 4 AM daily

  # Selective updates (label-based)
  - WATCHTOWER_LABEL_ENABLE=true

  # Rolling restarts
  - WATCHTOWER_ROLLING_RESTART=true

  # Monitor only (no updates)
  - WATCHTOWER_MONITOR_ONLY=true
```

```yaml
# Label containers to include/exclude
services:
  app:
    image: myapp
    labels:
      - "com.centurylinklabs.watchtower.enable=true"

  db:
    image: postgres
    labels:
      - "com.centurylinklabs.watchtower.enable=false"
```

### Diun (Docker Image Update Notifier)

Notify about available updates without auto-updating.

```yaml
services:
  diun:
    image: crazymax/diun:latest
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock
      - ./diun.yml:/diun.yml:ro
    environment:
      - TZ=America/New_York
      - DIUN_WATCH_SCHEDULE=0 */6 * * *
```

```yaml
# diun.yml
watch:
  workers: 20
  schedule: "0 */6 * * *"

notif:
  discord:
    webhookURL: https://discord.com/api/webhooks/xxx
  slack:
    webhookURL: https://hooks.slack.com/xxx

providers:
  docker:
    watchByDefault: true
```

---

## Container Registry

### Harbor

Enterprise container registry with security scanning.

```bash
# Install Harbor
wget https://github.com/goharbor/harbor/releases/download/v2.10.0/harbor-offline-installer-v2.10.0.tgz
tar xvf harbor-offline-installer-v2.10.0.tgz
cd harbor

# Configure
cp harbor.yml.tmpl harbor.yml
# Edit harbor.yml (hostname, https, passwords)

# Install
./install.sh --with-trivy  # Include vulnerability scanning
```

**Features:**
- Vulnerability scanning (Trivy)
- RBAC and projects
- Replication between registries
- Helm chart repository
- OCI artifact support

### Distribution (Docker Registry)

Official Docker registry.

```yaml
services:
  registry:
    image: registry:2
    ports:
      - "5000:5000"
    volumes:
      - registry-data:/var/lib/registry
    environment:
      - REGISTRY_STORAGE_DELETE_ENABLED=true
```

### Registry UI

```yaml
services:
  registry-ui:
    image: joxit/docker-registry-ui:latest
    ports:
      - "8080:80"
    environment:
      - REGISTRY_TITLE=My Registry
      - REGISTRY_URL=http://registry:5000
      - DELETE_IMAGES=true
    depends_on:
      - registry
```

---

## Networking

### Tailscale

Mesh VPN for containers.

```yaml
services:
  tailscale:
    image: tailscale/tailscale:latest
    hostname: docker-server
    environment:
      - TS_AUTHKEY=tskey-xxx
      - TS_STATE_DIR=/var/lib/tailscale
    volumes:
      - tailscale-state:/var/lib/tailscale
      - /dev/net/tun:/dev/net/tun
    cap_add:
      - NET_ADMIN
      - SYS_MODULE
    restart: unless-stopped

  # Use as sidecar
  app:
    network_mode: service:tailscale
    depends_on:
      - tailscale
```

### Cloudflare Tunnel

Expose services without opening ports.

```yaml
services:
  cloudflared:
    image: cloudflare/cloudflared:latest
    command: tunnel --no-autoupdate run
    environment:
      - TUNNEL_TOKEN=xxx
    restart: unless-stopped

  app:
    image: myapp
    # No ports exposed, accessed via tunnel
```

```yaml
# Tunnel config (Cloudflare dashboard or CLI)
ingress:
  - hostname: app.example.com
    service: http://app:3000
  - hostname: api.example.com
    service: http://api:8080
  - service: http_status:404
```

---

## Backup Tools

### Duplicati

Backup with web UI.

```yaml
services:
  duplicati:
    image: linuxserver/duplicati
    ports:
      - "8200:8200"
    volumes:
      - /opt/stacks:/source:ro  # What to backup
      - duplicati-config:/config
      - /backups:/backups
    environment:
      - PUID=1000
      - PGID=1000
```

### Restic + Docker

```yaml
services:
  restic-backup:
    image: mazzolino/restic
    environment:
      - BACKUP_CRON=0 3 * * *
      - RESTIC_REPOSITORY=s3:s3.amazonaws.com/my-bucket
      - AWS_ACCESS_KEY_ID=xxx
      - AWS_SECRET_ACCESS_KEY=xxx
      - RESTIC_PASSWORD=xxx
    volumes:
      - /opt/stacks:/data:ro
```

### docker-volume-backup

Backup Docker volumes directly.

```yaml
services:
  backup:
    image: offen/docker-volume-backup:latest
    environment:
      - BACKUP_CRON_EXPRESSION=0 4 * * *
      - AWS_S3_BUCKET_NAME=my-backups
      - AWS_ACCESS_KEY_ID=xxx
      - AWS_SECRET_ACCESS_KEY=xxx
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock:ro
      - app-data:/backup/app-data:ro

volumes:
  app-data:
    labels:
      - docker-volume-backup.stop-during-backup=true
```

---

## Monitoring

### cAdvisor

Container resource monitoring.

```yaml
services:
  cadvisor:
    image: gcr.io/cadvisor/cadvisor:latest
    ports:
      - "8080:8080"
    volumes:
      - /:/rootfs:ro
      - /var/run:/var/run:ro
      - /sys:/sys:ro
      - /var/lib/docker/:/var/lib/docker:ro
```

### Prometheus + cAdvisor

```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'cadvisor'
    static_configs:
      - targets: ['cadvisor:8080']
```

### Dozzle

Real-time log viewer.

```yaml
services:
  dozzle:
    image: amir20/dozzle:latest
    ports:
      - "8080:8080"
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock:ro
```

---

## Development Tools

### lazydocker

Terminal UI for Docker.

```bash
# Install
brew install lazydocker
# or
docker run --rm -it -v /var/run/docker.sock:/var/run/docker.sock lazyteam/lazydocker
```

**Features:**
- Container/image/volume management
- Log viewing
- Stats monitoring
- Keyboard-driven

### ctop

Top-like container metrics.

```bash
docker run --rm -it \
  -v /var/run/docker.sock:/var/run/docker.sock \
  quay.io/vektorlab/ctop:latest
```

### dive

Explore Docker image layers.

```bash
# Analyze image
dive nginx:latest

# Check Dockerfile efficiency
dive build -t myapp .
```

---

## Docker Compose Utilities

### docker-compose-viz

Visualize compose files.

```bash
docker run --rm -it --name dcv -v $(pwd):/input pmsipilot/docker-compose-viz render -m image docker-compose.yml
```

### Composerize

Convert `docker run` to compose.

```bash
# Web: composerize.com
# CLI
npx composerize docker run -d -p 80:80 nginx
```

---

## Quick Reference

### Essential Stack

```yaml
# docker-compose.yaml - Self-hosted essentials
services:
  traefik:
    image: traefik:v3.0
    # ... reverse proxy with auto SSL

  portainer:
    image: portainer/portainer-ce
    labels:
      - "traefik.enable=true"
      - "traefik.http.routers.portainer.rule=Host(`portainer.example.com`)"
    # ... management UI

  watchtower:
    image: containrrr/watchtower
    # ... auto-updates

  dozzle:
    image: amir20/dozzle
    labels:
      - "traefik.enable=true"
      - "traefik.http.routers.dozzle.rule=Host(`logs.example.com`)"
    # ... log viewer
```

---

## Decision Guide

| Need | Tool |
|------|------|
| Container management UI | Portainer (full) or Dockge (simple) |
| Reverse proxy + SSL | Traefik (auto-discovery) or Caddy (simple) |
| Auto-updates | Watchtower |
| Update notifications | Diun |
| Private registry | Harbor (enterprise) or Distribution (simple) |
| Remote access | Tailscale or Cloudflare Tunnel |
| Log viewing | Dozzle |
| Metrics | cAdvisor + Prometheus |
| Terminal management | lazydocker |

---

## Related

- [[Container Runtimes]]
- [[Observability Stack]]
- [[Deployment]]
- [[Distributed Tracing]]
