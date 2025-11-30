---
title: Load Balancing
aliases:
  - Load Balancer
  - LB
  - Traffic Distribution
tags:
  - tools
  - infrastructure
  - networking
  - scaling
type: reference
status: complete
created: 2025-11-30
---

# Load Balancing

Distributing traffic across multiple servers for reliability and performance.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Distribute traffic, ensure availability |
| Types | L4 (transport), L7 (application) |
| Key benefits | Scalability, fault tolerance, performance |
| Deployment | Hardware, software, cloud |

---

## How Load Balancing Works

```
                                    ┌──────────────┐
                                ┌──►│  Server 1    │
                                │   └──────────────┘
                                │
┌──────────┐    ┌────────────┐  │   ┌──────────────┐
│ Clients  │───►│    Load    │──┼──►│  Server 2    │
└──────────┘    │  Balancer  │  │   └──────────────┘
                └────────────┘  │
                                │   ┌──────────────┐
                                └──►│  Server 3    │
                                    └──────────────┘
```

---

## L4 vs L7 Load Balancing

### Layer 4 (Transport)

Routes based on IP and port. No inspection of content.

```
Client:54321 ──► LB:443 ──► Server1:8080
                       ──► Server2:8080
                       ──► Server3:8080

Decision based on: Source IP, Port, Protocol (TCP/UDP)
```

| Pros | Cons |
|------|------|
| Very fast | No content awareness |
| Low overhead | No HTTP features |
| Protocol agnostic | Limited routing options |

### Layer 7 (Application)

Routes based on HTTP content: URL, headers, cookies.

```
GET /api/users ──► API Service
GET /static/*   ──► CDN
POST /upload    ──► Upload Service

Decision based on: URL path, Headers, Cookies, Body
```

| Pros | Cons |
|------|------|
| Content-based routing | Higher latency |
| SSL termination | More resource intensive |
| Advanced features | HTTP only |

### Comparison

| Feature | L4 | L7 |
|---------|----|----|
| Speed | Faster | Slower |
| Routing | IP/Port | URL, Headers, Cookies |
| SSL | Pass-through | Termination |
| Content inspection | No | Yes |
| Session persistence | IP-based | Cookie-based |
| WebSocket | Pass-through | Protocol aware |

---

## Load Balancing Algorithms

### Round Robin

Rotate through servers sequentially.

```
Request 1 → Server A
Request 2 → Server B
Request 3 → Server C
Request 4 → Server A
...
```

| Best For | Limitations |
|----------|-------------|
| Homogeneous servers | Ignores server load |
| Stateless apps | Ignores capacity |

### Weighted Round Robin

Distribute based on server capacity.

```yaml
servers:
  - host: server-a
    weight: 5   # 50% of traffic
  - host: server-b
    weight: 3   # 30% of traffic
  - host: server-c
    weight: 2   # 20% of traffic
```

### Least Connections

Route to server with fewest active connections.

```
Server A: 10 connections
Server B: 5 connections   ← Next request goes here
Server C: 8 connections
```

| Best For | Limitations |
|----------|-------------|
| Long-lived connections | Doesn't account for request complexity |
| Varying request duration | |

### Weighted Least Connections

Combine connection count with server weights.

```
Score = Active Connections / Weight
Route to lowest score
```

### IP Hash

Route based on client IP for sticky sessions.

```python
server_index = hash(client_ip) % num_servers
```

| Best For | Limitations |
|----------|-------------|
| Session affinity | Uneven distribution |
| Caching | Server failure breaks stickiness |

### Least Response Time

Route to fastest responding server.

```
Server A: 50ms average
Server B: 30ms average  ← Next request goes here
Server C: 45ms average
```

### Random

Randomly select server.

| Best For | Limitations |
|----------|-------------|
| Simple stateless | Can cause hotspots |
| Testing | Not optimal distribution |

---

## Health Checks

### Active Health Checks

Load balancer probes servers.

```yaml
# HAProxy health check
backend api_servers
    option httpchk GET /health
    http-check expect status 200

    server api1 10.0.0.1:8080 check inter 5s fall 3 rise 2
    server api2 10.0.0.2:8080 check inter 5s fall 3 rise 2
```

| Parameter | Description |
|-----------|-------------|
| interval | How often to check |
| timeout | Max wait for response |
| fall | Failures before marking down |
| rise | Successes before marking up |

### Passive Health Checks

Monitor real traffic for failures.

```yaml
# NGINX passive checks
upstream backend {
    server 10.0.0.1:8080 max_fails=3 fail_timeout=30s;
    server 10.0.0.2:8080 max_fails=3 fail_timeout=30s;
}
```

### Health Check Endpoints

```python
# Comprehensive health check
@app.get("/health")
async def health_check():
    checks = {
        "database": check_database(),
        "cache": check_cache(),
        "disk": check_disk_space(),
    }

    all_healthy = all(checks.values())

    return JSONResponse(
        status_code=200 if all_healthy else 503,
        content={"status": "healthy" if all_healthy else "unhealthy", "checks": checks}
    )
```

---

## Session Persistence (Sticky Sessions)

Ensure user requests go to same server.

### Cookie-Based

```nginx
upstream backend {
    server 10.0.0.1:8080;
    server 10.0.0.2:8080;

    sticky cookie srv_id expires=1h domain=.example.com path=/;
}
```

### IP-Based

```yaml
# HAProxy
backend api
    balance source  # IP hash
    server api1 10.0.0.1:8080
    server api2 10.0.0.2:8080
```

### Session Persistence Trade-offs

| Approach | Pros | Cons |
|----------|------|------|
| No persistence | Best distribution | Session issues |
| Cookie-based | Precise | Cookie management |
| IP-based | Simple | Proxies cause issues |
| Application-level | Most flexible | More complex |

**Best practice:** Design stateless apps, use external session stores.

---

## SSL/TLS Handling

### SSL Termination

LB decrypts, sends plain HTTP to backend.

```
Client ──HTTPS──► LB ──HTTP──► Backend
```

| Pros | Cons |
|------|------|
| Centralized cert management | Traffic unencrypted internally |
| Reduces backend load | Requires secure network |

### SSL Passthrough

LB forwards encrypted traffic.

```
Client ──HTTPS──► LB ──HTTPS──► Backend
```

| Pros | Cons |
|------|------|
| End-to-end encryption | No content inspection |
| Backend controls certs | L7 features unavailable |

### SSL Re-encryption

LB decrypts, re-encrypts to backend.

```
Client ──HTTPS──► LB ──HTTPS──► Backend
         (cert A)     (cert B)
```

| Pros | Cons |
|------|------|
| Content inspection | Double encryption overhead |
| End-to-end encryption | Certificate management |

---

## Load Balancer Solutions

### Software

| Solution | Type | Best For |
|----------|------|----------|
| NGINX | L7 | Web, API, high performance |
| HAProxy | L4/L7 | TCP/HTTP, proven |
| Envoy | L7 | Service mesh, modern |
| Traefik | L7 | Kubernetes, auto-config |
| Caddy | L7 | Simple setup, auto HTTPS |

### Cloud

| Service | Provider | Notes |
|---------|----------|-------|
| ALB/NLB | AWS | Application/Network |
| Cloud Load Balancing | GCP | Global, anycast |
| Azure Load Balancer | Azure | L4/L7 options |
| DigitalOcean LB | DO | Simple, affordable |

### Hardware

| Vendor | Use Case |
|--------|----------|
| F5 BIG-IP | Enterprise, feature-rich |
| Citrix ADC | Enterprise, app delivery |
| A10 Networks | High performance |

---

## NGINX Configuration

### Basic Load Balancing

```nginx
http {
    upstream backend {
        server 10.0.0.1:8080;
        server 10.0.0.2:8080;
        server 10.0.0.3:8080;
    }

    server {
        listen 80;

        location / {
            proxy_pass http://backend;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
        }
    }
}
```

### Weighted + Health Checks

```nginx
upstream backend {
    least_conn;

    server 10.0.0.1:8080 weight=5;
    server 10.0.0.2:8080 weight=3;
    server 10.0.0.3:8080 weight=2 backup;

    # Health check (NGINX Plus)
    health_check interval=5s fails=3 passes=2;
}
```

---

## HAProxy Configuration

### Basic Setup

```haproxy
global
    log /dev/log local0
    maxconn 4096

defaults
    mode http
    timeout connect 5s
    timeout client  50s
    timeout server  50s

frontend http_front
    bind *:80
    default_backend http_back

backend http_back
    balance roundrobin
    option httpchk GET /health

    server web1 10.0.0.1:8080 check
    server web2 10.0.0.2:8080 check
    server web3 10.0.0.3:8080 check backup
```

### Advanced Features

```haproxy
frontend https_front
    bind *:443 ssl crt /etc/ssl/certs/site.pem
    http-request set-header X-Forwarded-Proto https

    # Route based on path
    acl is_api path_beg /api
    use_backend api_back if is_api
    default_backend web_back

backend api_back
    balance leastconn
    server api1 10.0.1.1:8080 check weight 100
    server api2 10.0.1.2:8080 check weight 100

backend web_back
    balance roundrobin
    cookie SERVERID insert indirect nocache
    server web1 10.0.2.1:8080 check cookie s1
    server web2 10.0.2.2:8080 check cookie s2
```

---

## Cloud Load Balancers

### AWS ALB (Application Load Balancer)

```yaml
# Terraform
resource "aws_lb" "main" {
  name               = "my-alb"
  internal           = false
  load_balancer_type = "application"
  subnets            = var.public_subnets
  security_groups    = [aws_security_group.alb.id]
}

resource "aws_lb_target_group" "main" {
  name     = "my-tg"
  port     = 80
  protocol = "HTTP"
  vpc_id   = var.vpc_id

  health_check {
    path                = "/health"
    healthy_threshold   = 2
    unhealthy_threshold = 3
  }
}

resource "aws_lb_listener" "http" {
  load_balancer_arn = aws_lb.main.arn
  port              = 80
  protocol          = "HTTP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.main.arn
  }
}
```

### AWS NLB (Network Load Balancer)

| Feature | ALB | NLB |
|---------|-----|-----|
| Layer | 7 | 4 |
| Latency | Higher | Ultra-low |
| Static IP | No | Yes |
| WebSocket | Yes | Yes (pass-through) |
| Path routing | Yes | No |

---

## High Availability

### Active-Passive

```
┌─────────────────┐     ┌─────────────────┐
│  Primary LB     │     │  Standby LB     │
│    (Active)     │     │   (Passive)     │
└────────┬────────┘     └────────┬────────┘
         │                       │
         │    Heartbeat          │
         │◄─────────────────────►│
         │                       │
         ▼                       ▼
    Virtual IP (failover if primary fails)
```

### Active-Active

```
         ┌──────────────────────────┐
         │     DNS / Anycast        │
         └─────────────┬────────────┘
                       │
           ┌───────────┴───────────┐
           ▼                       ▼
    ┌─────────────┐         ┌─────────────┐
    │    LB 1     │         │    LB 2     │
    │  (Active)   │         │  (Active)   │
    └─────────────┘         └─────────────┘
```

### Keepalived for VRRP

```conf
# /etc/keepalived/keepalived.conf
vrrp_instance VI_1 {
    state MASTER
    interface eth0
    virtual_router_id 51
    priority 100

    virtual_ipaddress {
        192.168.1.100
    }

    track_script {
        chk_haproxy
    }
}

vrrp_script chk_haproxy {
    script "pidof haproxy"
    interval 2
}
```

---

## Global Load Balancing

### DNS-Based

```
                    ┌───────────────┐
     Query          │     DNS       │
────────────────────►    GSLB      │
                    └───────┬───────┘
                            │
              ┌─────────────┼─────────────┐
              ▼             ▼             ▼
         US-East        EU-West        Asia
         10.0.0.1       10.0.1.1       10.0.2.1
```

| Provider | Feature |
|----------|---------|
| Route 53 | Geolocation, latency-based |
| Cloudflare | Anycast, load balancing |
| NS1 | Advanced traffic management |
| Azure Traffic Manager | DNS-based global LB |

### Anycast

Same IP advertised from multiple locations.

```
Client → Closest datacenter (by BGP routing)

IP: 1.2.3.4 → US datacenter (for US users)
IP: 1.2.3.4 → EU datacenter (for EU users)
```

---

## Monitoring

### Key Metrics

```yaml
# Essential LB metrics
- request_count_total
- request_latency_seconds
- active_connections
- backend_health_status
- error_rate
- bytes_transferred
- connection_errors
```

### HAProxy Stats

```haproxy
frontend stats
    bind *:8404
    stats enable
    stats uri /stats
    stats refresh 10s
```

### Prometheus + Grafana

```yaml
# NGINX exporter metrics
nginx_http_requests_total
nginx_http_upstream_request_duration_seconds
nginx_http_upstream_connect_time_seconds
nginx_up
```

---

## Best Practices

| Practice | Description |
|----------|-------------|
| Health checks | Always enable, tune thresholds |
| Graceful degradation | Backup servers, circuit breakers |
| Connection limits | Prevent backend overload |
| Keep-alive | Reduce connection overhead |
| Logging | Enable access logs for debugging |
| Monitoring | Track latency, errors, throughput |
| Security | Limit exposed ports, use TLS |
| Testing | Load test regularly |

---

## Related

- [[API Gateways]]
- [[Web Security]]
- [[Deployment]]
- [[Observability Stack]]
