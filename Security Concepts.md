---
title: Security Concepts
aliases:
  - Application Security
  - Web Security
  - AppSec
tags:
  - security
  - web
  - backend
  - architecture
type: reference
status: complete
created: 2025-11-28
---

# Security Concepts

Essential security knowledge for developers.

## Overview

| Category | Focus |
|----------|-------|
| Authentication | Who are you? |
| Authorization | What can you do? |
| Cryptography | Protecting data |
| OWASP Top 10 | Common vulnerabilities |
| Defense in Depth | Layered security |

---

## Authentication

### Methods Comparison

| Method | Use Case | Stateless | Notes |
|--------|----------|-----------|-------|
| Session cookies | Web apps | ❌ | Server stores session |
| JWT | APIs, SPAs | ✅ | Token contains claims |
| API Keys | Server-to-server | ✅ | Simple, no user context |
| OAuth 2.0 | Third-party access | ✅ | Delegation protocol |
| OIDC | Identity + OAuth | ✅ | Auth layer on OAuth |
| Passkeys/WebAuthn | Passwordless | ✅ | Phishing-resistant |

### Session-Based Auth

```
┌──────────┐         ┌──────────┐         ┌──────────┐
│  Client  │         │  Server  │         │  Store   │
└────┬─────┘         └────┬─────┘         └────┬─────┘
     │   POST /login      │                    │
     │───────────────────►│                    │
     │                    │  Create session    │
     │                    │───────────────────►│
     │   Set-Cookie:      │                    │
     │   session_id=abc   │                    │
     │◄───────────────────│                    │
     │                    │                    │
     │   GET /api         │                    │
     │   Cookie: abc      │                    │
     │───────────────────►│                    │
     │                    │  Lookup session    │
     │                    │───────────────────►│
     │                    │◄───────────────────│
     │   Response         │                    │
     │◄───────────────────│                    │
```

**Pros:** Simple, easy revocation
**Cons:** Requires session store, not stateless

### JWT (JSON Web Tokens)

```
Header.Payload.Signature

eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxMjMiLCJleHAiOjE3MDAwMDAwMDB9.signature
```

**Structure:**

| Part | Contains |
|------|----------|
| Header | Algorithm, token type |
| Payload | Claims (sub, exp, iat, custom) |
| Signature | HMAC or RSA signature |

**Token Types:**

| Token | Purpose | Lifetime |
|-------|---------|----------|
| Access token | API authorization | Short (15m-1h) |
| Refresh token | Get new access token | Long (days-weeks) |
| ID token | User identity (OIDC) | Short |

**JWT Best Practices:**

| Practice | Why |
|----------|-----|
| Short expiration | Limit breach window |
| Use refresh tokens | Balance security/UX |
| Validate all claims | exp, iat, iss, aud |
| Use asymmetric keys | RS256 > HS256 for distributed |
| Don't store sensitive data | Payload is base64, not encrypted |

### OAuth 2.0 Flows

| Flow | Use Case |
|------|----------|
| Authorization Code | Web apps with backend |
| Authorization Code + PKCE | SPAs, mobile apps |
| Client Credentials | Machine-to-machine |
| Device Code | TVs, CLI tools |

**Authorization Code + PKCE:**

```
┌────────┐                              ┌────────┐
│  App   │                              │ Auth   │
│        │                              │ Server │
└───┬────┘                              └───┬────┘
    │  1. Generate code_verifier            │
    │     code_challenge = SHA256(verifier) │
    │                                       │
    │  2. /authorize?                       │
    │     response_type=code&               │
    │     code_challenge=...&               │
    │     code_challenge_method=S256        │
    │──────────────────────────────────────►│
    │                                       │
    │  3. Redirect with code                │
    │◄──────────────────────────────────────│
    │                                       │
    │  4. /token                            │
    │     code=...&code_verifier=...        │
    │──────────────────────────────────────►│
    │                                       │
    │  5. Access token                      │
    │◄──────────────────────────────────────│
```

### Multi-Factor Authentication (MFA)

| Factor | Type | Examples |
|--------|------|----------|
| Knowledge | Something you know | Password, PIN |
| Possession | Something you have | Phone, hardware key |
| Inherence | Something you are | Fingerprint, face |

**MFA Methods (strongest to weakest):**

1. Hardware keys (YubiKey, Passkeys)
2. Authenticator apps (TOTP)
3. Push notifications
4. SMS codes (vulnerable to SIM swap)

---

## Authorization

### Models

| Model | Description | Use Case |
|-------|-------------|----------|
| RBAC | Role-Based Access Control | Simple apps |
| ABAC | Attribute-Based Access Control | Complex policies |
| ReBAC | Relationship-Based Access Control | Social, hierarchies |
| ACL | Access Control Lists | File systems |

### RBAC Example

```
User ─── has ───► Role ─── has ───► Permission
                  │
            ┌─────┴─────┐
            ▼           ▼
         Admin       Editor
            │           │
    ┌───────┼───────┐   │
    ▼       ▼       ▼   ▼
  read   write   delete read
                       write
```

```typescript
// Check permission
function canEdit(user: User, resource: Resource): boolean {
  const permissions = user.roles.flatMap(r => r.permissions);
  return permissions.includes(`${resource.type}:write`);
}
```

### ABAC Example

Policy-based decisions using attributes.

```typescript
// Policy: Users can edit their own posts
// OR admins can edit any post
// AND post must not be archived

function canEditPost(user: User, post: Post): boolean {
  const isOwner = post.authorId === user.id;
  const isAdmin = user.roles.includes('admin');
  const notArchived = !post.archived;

  return (isOwner || isAdmin) && notArchived;
}
```

### Authorization Patterns

| Pattern | Description |
|---------|-------------|
| Centralized | Single authorization service |
| Embedded | Logic in each service |
| Policy engine | OPA, Cedar, Casbin |

---

## OWASP Top 10 (2021)

| # | Vulnerability | Prevention |
|---|---------------|------------|
| A01 | Broken Access Control | Deny by default, check on server |
| A02 | Cryptographic Failures | Use TLS, proper key management |
| A03 | Injection | Parameterized queries, input validation |
| A04 | Insecure Design | Threat modeling, secure patterns |
| A05 | Security Misconfiguration | Hardened defaults, remove defaults |
| A06 | Vulnerable Components | Update dependencies, audit |
| A07 | Auth Failures | MFA, rate limiting, secure sessions |
| A08 | Data Integrity Failures | Verify signatures, integrity checks |
| A09 | Logging Failures | Log security events, monitor |
| A10 | SSRF | Validate URLs, allowlist |

### Injection Prevention

**SQL Injection:**

```typescript
// ❌ Vulnerable
const query = `SELECT * FROM users WHERE id = ${userId}`;

// ✅ Parameterized query
const query = 'SELECT * FROM users WHERE id = $1';
await db.query(query, [userId]);
```

**Command Injection:**

```typescript
// ❌ Vulnerable
exec(`convert ${filename} output.png`);

// ✅ Use arrays, avoid shell
execFile('convert', [filename, 'output.png']);
```

**XSS Prevention:**

```typescript
// ❌ Vulnerable (React dangerouslySetInnerHTML, etc.)
element.innerHTML = userInput;

// ✅ Use framework's escaping
element.textContent = userInput;

// ✅ Content Security Policy
// Content-Security-Policy: default-src 'self'
```

### CSRF Prevention

| Method | How |
|--------|-----|
| CSRF tokens | Hidden form field, validate on server |
| SameSite cookies | `SameSite=Strict` or `Lax` |
| Check Origin/Referer | Validate request origin |

```typescript
// Set secure cookies
res.cookie('session', token, {
  httpOnly: true,     // No JS access
  secure: true,       // HTTPS only
  sameSite: 'strict', // No cross-site
  maxAge: 3600000     // 1 hour
});
```

---

## Cryptography Basics

### Hashing vs Encryption

| Hashing | Encryption |
|---------|------------|
| One-way | Two-way |
| Fixed output | Variable output |
| Passwords, integrity | Data confidentiality |
| SHA-256, bcrypt | AES, RSA |

### Password Hashing

| Algorithm | Use | Notes |
|-----------|-----|-------|
| bcrypt | ✅ Recommended | Adaptive, salted |
| Argon2id | ✅ Recommended | Memory-hard, modern |
| scrypt | ✅ Acceptable | Memory-hard |
| PBKDF2 | ✅ Acceptable | Widely supported |
| SHA-256 | ❌ Never | Too fast, no salt |
| MD5 | ❌ Never | Broken |

```typescript
import bcrypt from 'bcrypt';

// Hash password
const hash = await bcrypt.hash(password, 12); // 12 rounds

// Verify password
const valid = await bcrypt.compare(password, hash);
```

### Encryption Types

| Type | Use Case | Examples |
|------|----------|----------|
| Symmetric | Fast, same key | AES-256-GCM |
| Asymmetric | Key exchange, signatures | RSA, Ed25519 |
| Hybrid | Best of both | TLS handshake |

### TLS/HTTPS

| Version | Status |
|---------|--------|
| TLS 1.3 | ✅ Current, use this |
| TLS 1.2 | ✅ Acceptable |
| TLS 1.1 | ❌ Deprecated |
| TLS 1.0 | ❌ Deprecated |
| SSL | ❌ Broken |

---

## Security Headers

| Header | Purpose | Example |
|--------|---------|---------|
| `Content-Security-Policy` | XSS prevention | `default-src 'self'` |
| `Strict-Transport-Security` | Force HTTPS | `max-age=31536000; includeSubDomains` |
| `X-Content-Type-Options` | MIME sniffing | `nosniff` |
| `X-Frame-Options` | Clickjacking | `DENY` |
| `Referrer-Policy` | Control referer | `strict-origin-when-cross-origin` |
| `Permissions-Policy` | Feature control | `camera=(), microphone=()` |

```
# Example headers
Content-Security-Policy: default-src 'self'; script-src 'self'
Strict-Transport-Security: max-age=31536000; includeSubDomains
X-Content-Type-Options: nosniff
X-Frame-Options: DENY
```

---

## Secrets Management

### Hierarchy

```
┌─────────────────────────────────────────────┐
│              Secrets Manager                 │
│  (Vault, AWS Secrets Manager, 1Password)    │
└─────────────────────┬───────────────────────┘
                      │
          ┌───────────┼───────────┐
          ▼           ▼           ▼
    ┌──────────┐ ┌──────────┐ ┌──────────┐
    │   Dev    │ │ Staging  │ │   Prod   │
    │   Env    │ │   Env    │ │   Env    │
    └──────────┘ └──────────┘ └──────────┘
```

### Best Practices

| Practice | Why |
|----------|-----|
| Never commit secrets | Use .gitignore, pre-commit hooks |
| Rotate regularly | Limit breach window |
| Least privilege | Minimal access per secret |
| Audit access | Know who accessed what |
| Encrypt at rest | Protect stored secrets |

### Tools

| Tool | Type |
|------|------|
| HashiCorp Vault | Self-hosted |
| AWS Secrets Manager | AWS managed |
| Azure Key Vault | Azure managed |
| GCP Secret Manager | GCP managed |
| 1Password | Team secrets |
| Doppler | Developer-focused |

---

## Input Validation

### Validation Strategies

| Strategy | Example |
|----------|---------|
| Allowlist | Only accept known good |
| Type checking | Ensure correct type |
| Length limits | Max/min length |
| Format validation | Regex for patterns |
| Sanitization | Remove dangerous chars |

```typescript
import { z } from 'zod';

const UserSchema = z.object({
  email: z.string().email().max(255),
  username: z.string().min(3).max(30).regex(/^[a-zA-Z0-9_]+$/),
  age: z.number().int().min(13).max(120),
});

// Validate and parse
const user = UserSchema.parse(input);
```

### Validation Layers

```
Client ──► Server ──► Database
  │          │          │
  ▼          ▼          ▼
 UX       Security   Integrity
 only     critical   constraints
```

**Never trust client-side validation alone.**

---

## Rate Limiting

### Algorithms

| Algorithm | Description |
|-----------|-------------|
| Token bucket | Burst allowed, refills over time |
| Leaky bucket | Constant rate output |
| Fixed window | X requests per time window |
| Sliding window | Smoother than fixed |

### Implementation

```typescript
// Simple in-memory rate limiter
const rateLimit = new Map<string, { count: number; resetTime: number }>();

function checkRateLimit(ip: string, limit: number, windowMs: number): boolean {
  const now = Date.now();
  const record = rateLimit.get(ip);

  if (!record || now > record.resetTime) {
    rateLimit.set(ip, { count: 1, resetTime: now + windowMs });
    return true;
  }

  if (record.count >= limit) {
    return false;
  }

  record.count++;
  return true;
}
```

### Rate Limit Headers

```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1640000000
Retry-After: 60
```

---

## Security Checklist

### Authentication

- [ ] Secure password storage (bcrypt/Argon2id)
- [ ] MFA available/enforced
- [ ] Account lockout after failed attempts
- [ ] Secure session management
- [ ] Password requirements enforced

### Authorization

- [ ] Deny by default
- [ ] Check permissions server-side
- [ ] Validate object-level access
- [ ] Audit sensitive operations

### Data Protection

- [ ] TLS everywhere
- [ ] Encrypt sensitive data at rest
- [ ] Secure secrets management
- [ ] PII handling compliant

### Input/Output

- [ ] Validate all input server-side
- [ ] Parameterized queries
- [ ] Output encoding
- [ ] Security headers set

### Operations

- [ ] Security logging
- [ ] Dependency scanning
- [ ] Regular updates
- [ ] Incident response plan

---

## Related

- [[API Design Patterns]]
- [[Web Frameworks]]
- [[domains/Web Development|Web Development]]
