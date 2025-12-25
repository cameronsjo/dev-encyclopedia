---
title: JWT
aliases:
  - JSON Web Token
  - JSON Web Tokens
tags:
  - security
  - authentication
  - api
  - tokens
type: reference
status: complete
created: "2025-12-16"
---

# JWT

JSON Web Tokens — compact, URL-safe tokens for securely transmitting claims between parties.

## Overview

| Aspect | Details |
|--------|---------|
| **Standard** | RFC 7519 |
| **Format** | Base64URL encoded JSON |
| **Structure** | Header.Payload.Signature |
| **Use Cases** | Authentication, authorization, information exchange |

## Structure

```
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.
eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.
SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c

│                 Header                │
│                                       │
│                Payload                │
│                                       │
│               Signature               │
```

### Header

```json
{
  "alg": "HS256",
  "typ": "JWT"
}
```

| Field | Description |
|-------|-------------|
| `alg` | Signing algorithm (HS256, RS256, ES256) |
| `typ` | Token type (always "JWT") |
| `kid` | Key ID (for key rotation) |

### Payload (Claims)

```json
{
  "sub": "user123",
  "name": "Alice Smith",
  "email": "alice@example.com",
  "iat": 1704063600,
  "exp": 1704067200,
  "iss": "https://auth.example.com",
  "aud": "https://api.example.com",
  "roles": ["admin", "user"]
}
```

#### Registered Claims

| Claim | Name | Description |
|-------|------|-------------|
| `iss` | Issuer | Who created the token |
| `sub` | Subject | Who the token is about |
| `aud` | Audience | Intended recipient |
| `exp` | Expiration | When token expires (Unix timestamp) |
| `nbf` | Not Before | Token not valid before this time |
| `iat` | Issued At | When token was created |
| `jti` | JWT ID | Unique token identifier |

### Signature

```
HMACSHA256(
  base64UrlEncode(header) + "." + base64UrlEncode(payload),
  secret
)
```

## Signing Algorithms

### Symmetric (HMAC)

**Same key for signing and verification.**

| Algorithm | Description |
|-----------|-------------|
| `HS256` | HMAC with SHA-256 |
| `HS384` | HMAC with SHA-384 |
| `HS512` | HMAC with SHA-512 |

```javascript
// Node.js
const jwt = require('jsonwebtoken');

const token = jwt.sign(
  { sub: 'user123', name: 'Alice' },
  'your-256-bit-secret',
  { algorithm: 'HS256', expiresIn: '1h' }
);
```

### Asymmetric (RSA, ECDSA)

**Private key signs, public key verifies.**

| Algorithm | Description |
|-----------|-------------|
| `RS256` | RSA + SHA-256 |
| `RS384` | RSA + SHA-384 |
| `RS512` | RSA + SHA-512 |
| `ES256` | ECDSA + P-256 |
| `ES384` | ECDSA + P-384 |
| `ES512` | ECDSA + P-521 |

```javascript
const fs = require('fs');
const jwt = require('jsonwebtoken');

const privateKey = fs.readFileSync('private.pem');
const publicKey = fs.readFileSync('public.pem');

// Sign with private key
const token = jwt.sign(
  { sub: 'user123' },
  privateKey,
  { algorithm: 'RS256', expiresIn: '1h' }
);

// Verify with public key
const decoded = jwt.verify(token, publicKey, { algorithms: ['RS256'] });
```

### Algorithm Comparison

| Type | Algorithms | Key Distribution | Use Case |
|------|------------|------------------|----------|
| **Symmetric** | HS256/384/512 | Shared secret | Single service |
| **RSA** | RS256/384/512 | Public/private | Distributed systems |
| **ECDSA** | ES256/384/512 | Public/private | Smaller tokens |

## Validation

### Validation Steps

```javascript
function validateJWT(token, publicKey) {
  // 1. Split token
  const [headerB64, payloadB64, signature] = token.split('.');

  // 2. Decode header and payload
  const header = JSON.parse(base64UrlDecode(headerB64));
  const payload = JSON.parse(base64UrlDecode(payloadB64));

  // 3. Check algorithm (prevent alg:none attack)
  if (!['RS256', 'ES256'].includes(header.alg)) {
    throw new Error('Invalid algorithm');
  }

  // 4. Verify signature
  const valid = verifySignature(headerB64, payloadB64, signature, publicKey);
  if (!valid) throw new Error('Invalid signature');

  // 5. Check expiration
  if (payload.exp && Date.now() >= payload.exp * 1000) {
    throw new Error('Token expired');
  }

  // 6. Check not before
  if (payload.nbf && Date.now() < payload.nbf * 1000) {
    throw new Error('Token not yet valid');
  }

  // 7. Check issuer
  if (payload.iss !== 'https://auth.example.com') {
    throw new Error('Invalid issuer');
  }

  // 8. Check audience
  if (payload.aud !== 'https://api.example.com') {
    throw new Error('Invalid audience');
  }

  return payload;
}
```

### Library Validation

```javascript
// Node.js with jsonwebtoken
const jwt = require('jsonwebtoken');

try {
  const decoded = jwt.verify(token, publicKey, {
    algorithms: ['RS256'],
    issuer: 'https://auth.example.com',
    audience: 'https://api.example.com',
    clockTolerance: 30 // 30 seconds leeway
  });
  console.log(decoded);
} catch (err) {
  console.error('Invalid token:', err.message);
}
```

## Security Best Practices

### Do

| Practice | Rationale |
|----------|-----------|
| **Verify signature** | Prevent tampering |
| **Check expiration** | Limit token lifetime |
| **Validate issuer/audience** | Prevent token misuse |
| **Use asymmetric for distributed** | No shared secrets |
| **Keep tokens short-lived** | 5-15 minutes typical |
| **Use HTTPS** | Prevent interception |

### Don't

| Anti-Pattern | Risk |
|--------------|------|
| **Store in localStorage** | XSS can steal tokens |
| **Include sensitive data** | Payload is readable |
| **Trust without verifying** | Accept forged tokens |
| **Allow `alg: none`** | Signature bypass |
| **Long expiration** | Extended exposure window |
| **Expose in URLs** | Logged, bookmarked, cached |

## Common Attacks

### Algorithm Confusion

```json
// Attacker changes header
{ "alg": "none", "typ": "JWT" }
// or
{ "alg": "HS256", "typ": "JWT" }  // When server expects RS256
```

**Prevention:** Always specify allowed algorithms.

```javascript
jwt.verify(token, key, { algorithms: ['RS256'] }); // Only RS256
```

### Token Sidejacking

**Attack:** XSS steals token from localStorage.

**Prevention:** Use HttpOnly cookies or secure token binding.

### Brute Force (Weak Secrets)

**Attack:** Guess HMAC secret for HS256.

**Prevention:** Use strong secrets (256+ bits) or asymmetric algorithms.

## Storage Options

| Storage | XSS Safe | CSRF Safe | Best For |
|---------|----------|-----------|----------|
| **HttpOnly Cookie** | ✅ | ❌ (needs protection) | Web apps |
| **localStorage** | ❌ | ✅ | Not recommended |
| **sessionStorage** | ❌ | ✅ | Short sessions |
| **Memory** | ✅ | ✅ | SPAs (refresh from cookie) |

### Recommended Pattern (Web)

```javascript
// Server sets token in HttpOnly cookie
res.cookie('token', jwt, {
  httpOnly: true,
  secure: true,
  sameSite: 'strict',
  maxAge: 15 * 60 * 1000 // 15 minutes
});

// Client includes automatically (no JS access)
// Add CSRF token for state-changing requests
```

## JWK (JSON Web Key)

**Public key format for distribution.**

```json
{
  "keys": [
    {
      "kty": "RSA",
      "kid": "key-id-1",
      "use": "sig",
      "alg": "RS256",
      "n": "0vx7agoebGc...",
      "e": "AQAB"
    }
  ]
}
```

**JWKS Endpoint:** `/.well-known/jwks.json`

## Token Lifecycle

```
┌─────────┐    ┌─────────────┐    ┌──────────┐
│  Login  │───▶│ Access Token│───▶│  API     │
└─────────┘    │  (15 min)   │    │ Requests │
               └──────┬──────┘    └──────────┘
                      │
                      │ Expires
                      ▼
               ┌─────────────┐
               │Refresh Token│
               │  (7 days)   │
               └──────┬──────┘
                      │
                      ▼
               ┌─────────────┐
               │New Access   │
               │Token        │
               └─────────────┘
```

## Libraries

| Language | Library |
|----------|---------|
| **Node.js** | jsonwebtoken, jose |
| **Python** | PyJWT, python-jose |
| **Go** | golang-jwt |
| **Java** | jjwt, auth0-java-jwt |
| **C#** | System.IdentityModel.Tokens.Jwt |
| **Ruby** | ruby-jwt |

## Related

- [[OAuth and OIDC]] — Authentication protocols
- [[Identity and Access Management]] — IAM overview
- [[Cryptography]] — Signing algorithms
- [[Security MOC]] — Security topics
