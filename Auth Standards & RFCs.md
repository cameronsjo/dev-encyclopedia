---
title: Auth Standards & RFCs
aliases:
  - Authentication RFCs
  - Authorization Standards
  - OAuth RFCs
  - Identity Standards
tags:
  - security
  - authentication
  - authorization
  - rfc
  - standards
type: reference
status: complete
created: 2025-11-30
---

# Auth Standards & RFCs

Essential RFCs and standards for modern authentication and authorization.

## Overview

| Category | Key Standards |
|----------|---------------|
| Token formats | JWT, PASETO, Macaroons |
| Auth flows | OAuth 2.0, OAuth 2.1, OIDC |
| Security extensions | PKCE, DPoP, PAR, RAR |
| Token management | Token Exchange, Introspection, Revocation |
| Federation | SAML, SCIM |

---

## OAuth 2.0 Core RFCs

### RFC 6749 - OAuth 2.0 Framework

The foundation. Defines authorization grants and token endpoints.

| Grant Type | Use Case | Status |
|------------|----------|--------|
| Authorization Code | Web apps with backend | ✅ Use with PKCE |
| Implicit | SPAs (legacy) | ❌ Deprecated |
| Resource Owner Password | Trusted apps | ❌ Deprecated |
| Client Credentials | Machine-to-machine | ✅ Still valid |

### RFC 6750 - Bearer Token Usage

How to send access tokens in requests.

```http
# Authorization header (preferred)
GET /api/resource HTTP/1.1
Authorization: Bearer eyJhbGciOiJSUzI1NiIs...

# Form body (avoid)
POST /api/resource HTTP/1.1
Content-Type: application/x-www-form-urlencoded

access_token=eyJhbGciOiJSUzI1NiIs...

# Query parameter (never in production)
GET /api/resource?access_token=eyJ...
```

**Security:** Bearer tokens are like cash—anyone with the token can use it. See DPoP for sender-constrained tokens.

---

## OAuth 2.1 (Draft)

Consolidates OAuth 2.0 + security best practices into one spec.

| Change | Rationale |
|--------|-----------|
| PKCE required for all clients | Prevents auth code interception |
| Implicit grant removed | Tokens in URL fragments unsafe |
| Password grant removed | Credentials exposure risk |
| Refresh token rotation | Detect token theft |
| Exact redirect URI matching | Prevent open redirects |

**Status:** Draft, but follow these practices now.

---

## Security Extensions

### RFC 7636 - PKCE (Proof Key for Code Exchange)

Prevents authorization code interception attacks.

```
┌────────────┐                              ┌────────────┐
│   Client   │                              │   AuthZ    │
│            │                              │   Server   │
└─────┬──────┘                              └─────┬──────┘
      │                                           │
      │  1. Generate code_verifier (random)       │
      │     code_challenge = SHA256(verifier)     │
      │                                           │
      │  2. /authorize?                           │
      │       code_challenge=abc...               │
      │       code_challenge_method=S256          │
      │──────────────────────────────────────────►│
      │                                           │
      │  3. Authorization code                    │
      │◄──────────────────────────────────────────│
      │                                           │
      │  4. /token                                │
      │       code=xyz&code_verifier=original     │
      │──────────────────────────────────────────►│
      │                                           │
      │     Server verifies:                      │
      │     SHA256(verifier) == stored challenge  │
      │                                           │
      │  5. Access token                          │
      │◄──────────────────────────────────────────│
```

**Required for:** All public clients (SPAs, mobile, CLI). Recommended for confidential clients too.

### RFC 9449 - DPoP (Demonstrating Proof of Possession)

Sender-constrained tokens—tokens bound to a specific client.

```
┌────────────┐                              ┌────────────┐
│   Client   │                              │  Resource  │
│            │                              │   Server   │
└─────┬──────┘                              └─────┬──────┘
      │                                           │
      │  1. Generate ephemeral key pair           │
      │                                           │
      │  2. Create DPoP proof (signed JWT):       │
      │     {                                     │
      │       "typ": "dpop+jwt",                  │
      │       "htm": "POST",                      │
      │       "htu": "https://api.example/data",  │
      │       "iat": 1234567890,                  │
      │       "jti": "unique-id"                  │
      │     }                                     │
      │                                           │
      │  GET /api/data                            │
      │  Authorization: DPoP <access_token>       │
      │  DPoP: <dpop_proof_jwt>                   │
      │──────────────────────────────────────────►│
      │                                           │
      │     Server validates:                     │
      │     - DPoP proof signature                │
      │     - Token bound to this key             │
      │     - htm/htu match request               │
```

**Benefit:** Stolen tokens are useless without the private key.

### RFC 9126 - PAR (Pushed Authorization Requests)

Push authorization parameters to server before redirect.

```
┌────────────┐                              ┌────────────┐
│   Client   │                              │   AuthZ    │
│            │                              │   Server   │
└─────┬──────┘                              └─────┬──────┘
      │                                           │
      │  1. POST /par                             │
      │     client_id=...                         │
      │     redirect_uri=...                      │
      │     scope=openid profile                  │
      │     code_challenge=...                    │
      │──────────────────────────────────────────►│
      │                                           │
      │  2. { "request_uri": "urn:...",           │
      │       "expires_in": 60 }                  │
      │◄──────────────────────────────────────────│
      │                                           │
      │  3. Redirect to /authorize?               │
      │       request_uri=urn:...                 │
      │       client_id=...                       │
      │──────────────────────────────────────────►│
```

**Benefits:**
- Keeps sensitive params off URL (logged, cached, leaked via Referer)
- Enables pre-validation before user interaction
- Required for FAPI (Financial-grade API)

### RFC 9396 - RAR (Rich Authorization Requests)

Express fine-grained authorization beyond simple scopes.

```json
// Traditional scopes (coarse)
"scope": "read write"

// RAR (fine-grained)
"authorization_details": [
  {
    "type": "payment_initiation",
    "instructedAmount": {
      "currency": "EUR",
      "amount": "123.50"
    },
    "creditorAccount": {
      "iban": "DE89370400440532013000"
    }
  },
  {
    "type": "account_information",
    "actions": ["read"],
    "locations": ["https://api.bank.com/accounts"],
    "datatypes": ["balance", "transactions"]
  }
]
```

**Use cases:** Banking (PSD2), healthcare, any domain needing transaction-specific consent.

---

## Token Standards

### RFC 7519 - JWT (JSON Web Token)

Compact, self-contained tokens.

```
Header.Payload.Signature

eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.
eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4iLCJpYXQiOjE1MTYyMzkwMjJ9.
SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c
```

| Claim | Purpose |
|-------|---------|
| `iss` | Issuer |
| `sub` | Subject (user ID) |
| `aud` | Audience (intended recipient) |
| `exp` | Expiration time |
| `iat` | Issued at |
| `nbf` | Not before |
| `jti` | JWT ID (unique identifier) |

**Validation checklist:**
- [ ] Verify signature (RS256 preferred over HS256)
- [ ] Check `exp` not passed
- [ ] Check `nbf` if present
- [ ] Validate `iss` matches expected
- [ ] Validate `aud` includes your service
- [ ] Check `jti` for replay prevention (if needed)

### RFC 7515/7516/7517/7518 - JOSE

JSON Object Signing and Encryption suite.

| RFC | Name | Purpose |
|-----|------|---------|
| 7515 | JWS | JSON Web Signature |
| 7516 | JWE | JSON Web Encryption |
| 7517 | JWK | JSON Web Key |
| 7518 | JWA | JSON Web Algorithms |

**JWK Example:**

```json
{
  "kty": "RSA",
  "kid": "key-2024",
  "use": "sig",
  "alg": "RS256",
  "n": "0vx7agoebGcQSuu...",
  "e": "AQAB"
}
```

### PASETO (Platform-Agnostic Security Tokens)

Alternative to JWT with better defaults.

| Feature | JWT | PASETO |
|---------|-----|--------|
| Algorithm agility | Yes (risk) | No (safe defaults) |
| Encryption option | JWE (complex) | Built-in |
| None algorithm | Possible (CVE) | Impossible |
| Key confusion | Possible | Impossible |

**Versions:**

| Version | Purpose |
|---------|---------|
| v1 | Legacy compatibility |
| v2 | Modern: Ed25519, XChaCha20 |
| v3 | NIST-approved algorithms |
| v4 | Current recommended |

```
v4.public.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4ifQ...
```

**When to use:** New projects where you control both sides.

### Macaroons

Tokens with caveats (restrictions) that can be added by anyone.

```
Original macaroon:
  identifier: user-123-access
  location: https://api.example.com

Add caveat (anyone can do this):
  time < 2024-01-01
  ip = 192.168.1.0/24
  operation = read

Result: More restricted token, still verifiable by server
```

**Use cases:** Delegated credentials, capability-based security.

---

## Token Management

### RFC 7662 - Token Introspection

Query auth server about token validity.

```http
POST /introspect HTTP/1.1
Content-Type: application/x-www-form-urlencoded
Authorization: Basic czZCaGRSa3F0MzpnWDFmQmF0M2JW

token=eyJhbGciOiJSUzI1NiIs...
```

```json
{
  "active": true,
  "client_id": "app-123",
  "username": "john",
  "scope": "read write",
  "exp": 1640000000
}
```

**When to use:**
- Opaque tokens (not self-contained)
- Need real-time revocation check
- Token details not in token itself

### RFC 7009 - Token Revocation

Invalidate tokens before expiration.

```http
POST /revoke HTTP/1.1
Content-Type: application/x-www-form-urlencoded
Authorization: Basic czZCaGRSa3F0MzpnWDFmQmF0M2JW

token=eyJhbGciOiJSUzI1NiIs...
&token_type_hint=access_token
```

**Response:** Always 200 OK (don't reveal if token existed).

### RFC 8693 - Token Exchange

Exchange one token for another.

```http
POST /token HTTP/1.1
Content-Type: application/x-www-form-urlencoded

grant_type=urn:ietf:params:oauth:grant-type:token-exchange
&subject_token=eyJ...
&subject_token_type=urn:ietf:params:oauth:token-type:access_token
&requested_token_type=urn:ietf:params:oauth:token-type:access_token
&audience=https://backend-service
```

**Use cases:**
- Impersonation (admin acting as user)
- Delegation (service-to-service)
- Token downscoping (reduce privileges)

---

## OpenID Connect

### OIDC Core

Identity layer on OAuth 2.0.

| Component | Purpose |
|-----------|---------|
| ID Token | JWT with user identity claims |
| UserInfo endpoint | Additional user attributes |
| Discovery | `.well-known/openid-configuration` |

**Standard claims:**

| Claim | Description |
|-------|-------------|
| `sub` | Subject identifier |
| `name` | Full name |
| `email` | Email address |
| `email_verified` | Email verification status |
| `picture` | Profile picture URL |

### OIDC Discovery

Auto-configure clients via well-known endpoint.

```http
GET /.well-known/openid-configuration
```

```json
{
  "issuer": "https://auth.example.com",
  "authorization_endpoint": "https://auth.example.com/authorize",
  "token_endpoint": "https://auth.example.com/token",
  "userinfo_endpoint": "https://auth.example.com/userinfo",
  "jwks_uri": "https://auth.example.com/.well-known/jwks.json",
  "scopes_supported": ["openid", "profile", "email"],
  "response_types_supported": ["code"],
  "grant_types_supported": ["authorization_code", "refresh_token"],
  "subject_types_supported": ["public"],
  "id_token_signing_alg_values_supported": ["RS256"]
}
```

---

## Enterprise Standards

### SAML 2.0

XML-based federation (enterprise SSO).

| Component | Purpose |
|-----------|---------|
| IdP | Identity Provider (authenticates users) |
| SP | Service Provider (your app) |
| Assertion | XML document with auth claims |
| Metadata | XML config for IdP/SP |

**Flow:**

```
User ──► SP ──► IdP ──► User authenticates ──► IdP ──► SP (with assertion)
```

**When to use:** Enterprise SSO requirements, legacy systems.

### SCIM 2.0 (RFC 7643/7644)

System for Cross-domain Identity Management.

```http
# Create user
POST /scim/v2/Users
{
  "schemas": ["urn:ietf:params:scim:schemas:core:2.0:User"],
  "userName": "john@example.com",
  "name": {
    "givenName": "John",
    "familyName": "Doe"
  },
  "emails": [
    { "value": "john@example.com", "primary": true }
  ]
}

# List users
GET /scim/v2/Users?filter=userName eq "john@example.com"
```

**Use cases:** User provisioning/deprovisioning across systems.

---

## Emerging Standards

### GNAP (Grant Negotiation and Authorization Protocol)

Next-generation auth protocol (OAuth 3.0 spiritual successor).

| Feature | OAuth 2.0 | GNAP |
|---------|-----------|------|
| Request format | Form-encoded | JSON |
| Interaction modes | Redirect only | Multiple (redirect, app2app, CIBA) |
| Key binding | Extension (DPoP) | Built-in |
| Continuation | No | Yes (multi-step) |

**Status:** In development, watch for adoption.

### WebAuthn / Passkeys

Passwordless authentication using public key cryptography.

```
┌────────────┐    ┌────────────┐    ┌────────────┐
│  Relying   │    │  Browser/  │    │Authenticator│
│   Party    │    │    App     │    │ (TouchID,  │
│  (Server)  │    │            │    │  YubiKey)  │
└─────┬──────┘    └─────┬──────┘    └─────┬──────┘
      │                 │                 │
      │  Challenge      │                 │
      │────────────────►│                 │
      │                 │  Sign challenge │
      │                 │────────────────►│
      │                 │                 │
      │                 │  Signature      │
      │                 │◄────────────────│
      │  Assertion      │                 │
      │◄────────────────│                 │
      │                 │                 │
      │  Verify w/      │                 │
      │  stored pubkey  │                 │
```

**Benefits:**
- Phishing-resistant (bound to origin)
- No passwords to steal
- Built into OS/browsers

---

## Security Best Practices

### Token Lifetimes

| Token Type | Recommended Lifetime |
|------------|---------------------|
| Access token | 5-60 minutes |
| Refresh token | Hours to days (with rotation) |
| ID token | Minutes (one-time use) |
| Authorization code | 1-10 minutes |

### Algorithm Selection

| Use | Recommended | Avoid |
|-----|-------------|-------|
| Signing (asymmetric) | RS256, ES256, EdDSA | RS384/512 (slow) |
| Signing (symmetric) | HS256 (same trust domain only) | - |
| Encryption | A256GCM | A128CBC-HS256 |
| Key exchange | ECDH-ES+A256KW | RSA-OAEP (slow) |

### Common Vulnerabilities

| Vulnerability | Prevention |
|---------------|------------|
| Token in URL | Use Authorization header |
| Insecure storage | HttpOnly cookies, secure storage |
| Missing PKCE | Always use PKCE |
| Weak algorithms | Allowlist strong algorithms |
| Missing audience validation | Always check `aud` claim |
| Replay attacks | Use `jti`, short expiry |

---

## Quick Reference

### Which Standard When?

| Scenario | Use |
|----------|-----|
| SPA/mobile app | OAuth 2.0 + PKCE + OIDC |
| Machine-to-machine | Client Credentials |
| High security (banking) | OAuth 2.0 + PKCE + DPoP + PAR |
| Enterprise SSO | OIDC or SAML |
| User provisioning | SCIM |
| Passwordless | WebAuthn/Passkeys |
| New greenfield | Consider PASETO over JWT |

### RFC Quick List

| RFC | Name |
|-----|------|
| 6749 | OAuth 2.0 Framework |
| 6750 | Bearer Token Usage |
| 7519 | JWT |
| 7636 | PKCE |
| 7662 | Token Introspection |
| 7009 | Token Revocation |
| 8693 | Token Exchange |
| 9126 | PAR |
| 9396 | RAR |
| 9449 | DPoP |

---

## Related

- [[Security Concepts]]
- [[API Design Patterns]]
- [[Web Frameworks]]
