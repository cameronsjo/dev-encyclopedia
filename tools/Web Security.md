---
title: Web Security
aliases:
  - WAF
  - Web Application Firewall
  - DDoS Protection
  - CAPTCHA
tags:
  - tools
  - security
  - infrastructure
  - networking
type: reference
status: complete
created: "2025-11-30"
---

# Web Security

Infrastructure and tools for protecting web applications from attacks.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Protect web apps from attacks |
| Layers | Edge, network, application |
| Key components | WAF, CDN, DDoS protection, CAPTCHA |
| Approach | Defense in depth |

---

## Security Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Security Layers                               │
│                                                                     │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │                    Edge Layer (CDN/WAF)                        │ │
│  │  • DDoS mitigation    • Bot detection    • Rate limiting      │ │
│  │  • TLS termination    • WAF rules        • Geo-blocking       │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                              │                                      │
│  ┌───────────────────────────▼───────────────────────────────────┐ │
│  │                    Network Layer                               │ │
│  │  • Firewall rules     • VPC/network isolation                 │ │
│  │  • IDS/IPS            • Port filtering                        │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                              │                                      │
│  ┌───────────────────────────▼───────────────────────────────────┐ │
│  │                    Application Layer                           │ │
│  │  • Authentication     • Input validation    • CSRF protection │ │
│  │  • Authorization      • Output encoding     • Session mgmt    │ │
│  └───────────────────────────────────────────────────────────────┘ │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Web Application Firewalls (WAF)

### What is a WAF?

Layer 7 firewall that inspects HTTP traffic.

```
Client → WAF → Origin Server
           │
           ├── Allow (normal request)
           ├── Block (malicious)
           ├── Challenge (suspicious)
           └── Log (monitoring)
```

### WAF Rule Types

| Type | Description | Example |
|------|-------------|---------|
| Signature-based | Known attack patterns | SQL injection strings |
| Anomaly-based | Deviation from baseline | Unusual request rate |
| Behavioral | User behavior analysis | Bot patterns |
| Custom | Application-specific | Business logic |

### OWASP Core Rule Set (CRS)

Industry-standard WAF rules.

| Category | Protects Against |
|----------|------------------|
| SQL Injection | `' OR '1'='1`, UNION attacks |
| XSS | `<script>`, event handlers |
| LFI/RFI | Path traversal, file inclusion |
| RCE | Command injection |
| Protocol | HTTP violations, smuggling |

### WAF Configuration Example

```yaml
# ModSecurity rules
SecRule REQUEST_URI "@contains ../"\
    "id:1001,\
    phase:2,\
    deny,\
    status:403,\
    msg:'Path traversal attempt'"

SecRule ARGS "@rx (?i)(union.*select|select.*from)"\
    "id:1002,\
    phase:2,\
    deny,\
    status:403,\
    msg:'SQL Injection attempt'"
```

### WAF Solutions

| Solution | Type | Best For |
|----------|------|----------|
| Cloudflare WAF | Managed | Easy setup, edge |
| AWS WAF | Managed | AWS integration |
| Azure WAF | Managed | Azure ecosystem |
| ModSecurity | Open source | Self-hosted, customizable |
| NGINX App Protect | Commercial | NGINX environments |
| Imperva | Enterprise | Compliance, enterprise |

---

## Cloudflare

### Core Services

| Service | Description |
|---------|-------------|
| CDN | Global content distribution |
| WAF | Web application firewall |
| DDoS Protection | Volumetric attack mitigation |
| Bot Management | Bot detection and mitigation |
| Rate Limiting | Request throttling |
| Workers | Edge compute |

### Cloudflare Architecture

```
                    ┌─────────────────────────────────────┐
                    │         Cloudflare Edge             │
                    │      (300+ data centers)            │
                    │                                     │
┌──────────┐       │  ┌─────────────────────────────┐   │       ┌──────────┐
│  Client  │───────┼─►│  DNS → CDN → WAF → Proxy   │───┼──────►│  Origin  │
└──────────┘       │  └─────────────────────────────┘   │       └──────────┘
                    │                                     │
                    │  • Cache static content            │
                    │  • Apply security rules            │
                    │  • Optimize delivery               │
                    │                                     │
                    └─────────────────────────────────────┘
```

### Cloudflare WAF Rules

```javascript
// Cloudflare Workers - Custom WAF logic
addEventListener('fetch', event => {
  event.respondWith(handleRequest(event.request))
})

async function handleRequest(request) {
  const url = new URL(request.url)

  // Block suspicious patterns
  if (url.pathname.includes('../')) {
    return new Response('Forbidden', { status: 403 })
  }

  // Rate limit by IP
  const ip = request.headers.get('CF-Connecting-IP')
  if (await isRateLimited(ip)) {
    return new Response('Too Many Requests', { status: 429 })
  }

  return fetch(request)
}
```

### Security Settings

| Setting | Options |
|---------|---------|
| SSL Mode | Off, Flexible, Full, Full (Strict) |
| Security Level | Off, Low, Medium, High, Under Attack |
| Bot Fight Mode | On/Off |
| Browser Integrity Check | On/Off |

---

## DDoS Protection

### Attack Types

| Type | Layer | Method |
|------|-------|--------|
| Volumetric | L3/L4 | Flood with traffic (UDP, ICMP) |
| Protocol | L4 | Exploit protocol (SYN flood, Ping of Death) |
| Application | L7 | HTTP floods, slow attacks |

### Mitigation Techniques

| Technique | Description |
|-----------|-------------|
| Anycast | Distribute traffic globally |
| Rate limiting | Cap requests per IP/client |
| CAPTCHA | Challenge suspicious traffic |
| Behavioral analysis | Detect bot patterns |
| Blackholing | Drop traffic to attacked IP |
| Scrubbing | Filter malicious traffic |

### DDoS Protection Services

| Service | Capacity | Notes |
|---------|----------|-------|
| Cloudflare | 209 Tbps | All plans |
| AWS Shield | Multi-Tbps | Standard free, Advanced paid |
| Azure DDoS | Multi-Tbps | Basic free, Standard paid |
| Akamai Prolexic | Tbps+ | Enterprise |
| GCP Cloud Armor | Google scale | GCP only |

---

## CAPTCHA & Bot Detection

### CAPTCHA Types

| Type | User Experience | Security |
|------|-----------------|----------|
| Text CAPTCHA | Poor | Low (OCR broken) |
| Image CAPTCHA | Medium | Medium |
| reCAPTCHA v2 | Poor (checkbox + challenge) | High |
| reCAPTCHA v3 | Invisible | High (score-based) |
| hCaptcha | Medium | High (privacy-focused) |
| Turnstile | Invisible/Managed | High (Cloudflare) |

### reCAPTCHA v3 Implementation

```html
<!-- Load script -->
<script src="https://www.google.com/recaptcha/api.js?render=SITE_KEY"></script>

<script>
  function submitForm() {
    grecaptcha.execute('SITE_KEY', { action: 'submit' })
      .then(token => {
        // Send token to backend
        fetch('/api/verify', {
          method: 'POST',
          body: JSON.stringify({ token, action: 'submit' })
        })
      })
  }
</script>
```

```python
# Backend verification
import requests

def verify_recaptcha(token: str, action: str) -> bool:
    response = requests.post(
        'https://www.google.com/recaptcha/api/siteverify',
        data={
            'secret': RECAPTCHA_SECRET,
            'response': token
        }
    ).json()

    return (
        response['success'] and
        response['action'] == action and
        response['score'] >= 0.5
    )
```

### Turnstile (Cloudflare)

```html
<!-- Managed challenge -->
<div class="cf-turnstile" data-sitekey="SITE_KEY"></div>
<script src="https://challenges.cloudflare.com/turnstile/v0/api.js" async defer></script>
```

```python
# Verify token
def verify_turnstile(token: str) -> bool:
    response = requests.post(
        'https://challenges.cloudflare.com/turnstile/v0/siteverify',
        data={
            'secret': TURNSTILE_SECRET,
            'response': token
        }
    ).json()
    return response['success']
```

### Bot Detection Signals

| Signal | What It Detects |
|--------|-----------------|
| JavaScript fingerprint | Headless browsers |
| Mouse/touch patterns | Automated behavior |
| Request timing | Inhuman speed |
| Header analysis | Missing/abnormal headers |
| TLS fingerprint | Known bot libraries |
| IP reputation | Known bad actors |

---

## Security Headers

### Essential Headers

```
# Prevent XSS
Content-Security-Policy: default-src 'self'; script-src 'self' 'unsafe-inline'

# Prevent clickjacking
X-Frame-Options: DENY

# Prevent MIME sniffing
X-Content-Type-Options: nosniff

# Force HTTPS
Strict-Transport-Security: max-age=31536000; includeSubDomains; preload

# Control referrer
Referrer-Policy: strict-origin-when-cross-origin

# Permissions policy
Permissions-Policy: geolocation=(), microphone=(), camera=()
```

### Header Configuration

```nginx
# NGINX security headers
add_header X-Frame-Options "SAMEORIGIN" always;
add_header X-Content-Type-Options "nosniff" always;
add_header X-XSS-Protection "1; mode=block" always;
add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
add_header Content-Security-Policy "default-src 'self'" always;
```

### Content Security Policy (CSP)

```
Content-Security-Policy:
    default-src 'self';
    script-src 'self' https://cdn.example.com;
    style-src 'self' 'unsafe-inline';
    img-src 'self' data: https:;
    font-src 'self' https://fonts.gstatic.com;
    connect-src 'self' https://api.example.com;
    frame-ancestors 'none';
    base-uri 'self';
    form-action 'self';
```

---

## Rate Limiting

### Implementation Patterns

```python
# Token bucket algorithm
class TokenBucket:
    def __init__(self, rate: float, capacity: int):
        self.rate = rate  # tokens per second
        self.capacity = capacity
        self.tokens = capacity
        self.last_update = time.time()

    def consume(self, tokens: int = 1) -> bool:
        now = time.time()
        elapsed = now - self.last_update
        self.tokens = min(
            self.capacity,
            self.tokens + elapsed * self.rate
        )
        self.last_update = now

        if self.tokens >= tokens:
            self.tokens -= tokens
            return True
        return False
```

### Redis-Based Rate Limiting

```python
import redis
import time

def rate_limit(key: str, limit: int, window: int) -> bool:
    """Sliding window rate limit."""
    now = time.time()
    window_start = now - window

    pipe = redis.pipeline()
    # Remove old entries
    pipe.zremrangebyscore(key, 0, window_start)
    # Add current request
    pipe.zadd(key, {str(now): now})
    # Count requests in window
    pipe.zcard(key)
    # Set expiry
    pipe.expire(key, window)

    results = pipe.execute()
    request_count = results[2]

    return request_count <= limit
```

---

## SSL/TLS Configuration

### Best Practices

```nginx
# NGINX SSL config
ssl_protocols TLSv1.2 TLSv1.3;
ssl_ciphers ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256;
ssl_prefer_server_ciphers off;

ssl_session_cache shared:SSL:10m;
ssl_session_timeout 1d;
ssl_session_tickets off;

ssl_stapling on;
ssl_stapling_verify on;
```

### Certificate Management

| Service | Type | Notes |
|---------|------|-------|
| Let's Encrypt | Free | Auto-renewal, 90 days |
| Cloudflare | Free | Edge certs included |
| AWS ACM | Free | AWS services only |
| DigiCert | Paid | Enterprise, EV certs |

---

## OWASP Top 10 Protection

| Vulnerability | Protection |
|---------------|------------|
| Injection | Parameterized queries, input validation |
| Broken Auth | MFA, session management |
| Sensitive Data | Encryption, access controls |
| XXE | Disable external entities |
| Broken Access | RBAC, principle of least privilege |
| Misconfig | Security hardening, audits |
| XSS | CSP, output encoding |
| Insecure Deserialize | Input validation, type checking |
| Vulnerable Components | Dependency scanning |
| Logging/Monitoring | Centralized logging, alerting |

---

## Security Testing

### Tools

| Tool | Purpose |
|------|---------|
| OWASP ZAP | DAST, vulnerability scanning |
| Burp Suite | Web app security testing |
| Nmap | Network scanning |
| SQLMap | SQL injection testing |
| Nikto | Web server scanning |
| Nuclei | Template-based scanning |

### Security Headers Check

```bash
# Test security headers
curl -I https://example.com | grep -E "^(X-|Content-Security|Strict)"

# Or use online tools
# - securityheaders.com
# - observatory.mozilla.org
```

---

## Monitoring & Alerting

### Security Metrics

```
# WAF metrics
waf_requests_total{action="block|allow|challenge"}
waf_rule_hits{rule_id, category}

# Rate limiting
rate_limit_exceeded_total{endpoint, client_type}

# DDoS
requests_per_second{origin}
bandwidth_bytes{direction}

# Auth
failed_login_attempts{reason}
suspicious_sessions{type}
```

### Alert Rules

```yaml
# Prometheus alert rules
groups:
  - name: security
    rules:
      - alert: HighWAFBlocks
        expr: rate(waf_requests_total{action="block"}[5m]) > 100
        for: 5m
        labels:
          severity: warning

      - alert: DDoSDetected
        expr: rate(requests_total[1m]) > 10000
        for: 2m
        labels:
          severity: critical
```

---

## When to Use What

| Threat | Solution |
|--------|----------|
| DDoS attacks | CDN + DDoS protection |
| Injection attacks | WAF + input validation |
| Bots/scrapers | Bot detection + CAPTCHA |
| Credential stuffing | Rate limiting + CAPTCHA |
| API abuse | API gateway + rate limiting |
| Data exfiltration | WAF + monitoring |

---

## Related

- [[API Gateways]]
- [[Load Balancing]]
- [[Deployment]]
- [[Observability Stack]]
