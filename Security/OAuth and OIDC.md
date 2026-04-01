---
title: OAuth and OIDC
aliases:
  - OAuth 2.0
  - OpenID Connect
  - OAuth
tags:
  - security
  - authentication
  - authorization
  - api
type: reference
status: complete
created: "2025-12-16"
---

# OAuth and OIDC

Industry-standard protocols for authorization (OAuth 2.0) and authentication (OpenID Connect).

## Overview

| Protocol | Purpose | Result |
|----------|---------|--------|
| **OAuth 2.0** | Authorization | Access Token |
| **OpenID Connect** | Authentication | ID Token + Access Token |

```
OAuth 2.0: "Can this app access my photos?"
OIDC: "Who is this user?" + OAuth 2.0
```

## Key Concepts

| Term | Definition |
|------|------------|
| **Resource Owner** | User who owns the data |
| **Client** | Application requesting access |
| **Authorization Server** | Issues tokens (Google, Auth0) |
| **Resource Server** | API hosting protected resources |
| **Access Token** | Credential to access resources |
| **Refresh Token** | Credential to get new access tokens |
| **ID Token** | JWT containing user identity (OIDC) |
| **Scope** | Permission being requested |

## OAuth 2.0 Flows

### Authorization Code Flow (Recommended)

**Best for:** Server-side web apps

```
┌──────────┐                              ┌───────────────┐
│  User    │                              │ Authorization │
│ (Browser)│                              │    Server     │
└────┬─────┘                              └───────┬───────┘
     │                                            │
     │  1. Click "Login with Google"              │
     │────────────────────────────────────────────▶
     │                                            │
     │  2. Redirect to Auth Server                │
     │◀────────────────────────────────────────────
     │                                            │
     │  3. User logs in & consents                │
     │────────────────────────────────────────────▶
     │                                            │
     │  4. Redirect back with code                │
     │◀────────────────────────────────────────────
     │                                            │
┌────┴─────┐                                      │
│  Client  │  5. Exchange code for tokens         │
│ (Server) │─────────────────────────────────────▶│
│          │                                      │
│          │  6. Access Token + Refresh Token     │
│          │◀─────────────────────────────────────│
└──────────┘                                      │
```

**Request:**

```
GET /authorize?
  response_type=code&
  client_id=CLIENT_ID&
  redirect_uri=https://app.com/callback&
  scope=openid profile email&
  state=RANDOM_STATE
```

**Token Exchange:**

```bash
POST /token
Content-Type: application/x-www-form-urlencoded

grant_type=authorization_code&
code=AUTH_CODE&
redirect_uri=https://app.com/callback&
client_id=CLIENT_ID&
client_secret=CLIENT_SECRET
```

### Authorization Code with PKCE

**Best for:** Mobile apps, SPAs (public clients)

```
1. Generate code_verifier (random string)
2. Create code_challenge = BASE64URL(SHA256(code_verifier))
3. Include code_challenge in authorize request
4. Include code_verifier in token request
```

```javascript
// Generate PKCE values
const codeVerifier = generateRandomString(64);
const codeChallenge = base64UrlEncode(sha256(codeVerifier));

// Authorization request
const authUrl = `https://auth.example.com/authorize?
  response_type=code&
  client_id=${clientId}&
  redirect_uri=${redirectUri}&
  scope=openid profile&
  code_challenge=${codeChallenge}&
  code_challenge_method=S256&
  state=${state}`;

// Token request (no client_secret needed)
const tokenResponse = await fetch('https://auth.example.com/token', {
  method: 'POST',
  body: new URLSearchParams({
    grant_type: 'authorization_code',
    code: authorizationCode,
    redirect_uri: redirectUri,
    client_id: clientId,
    code_verifier: codeVerifier
  })
});
```

### Client Credentials Flow

**Best for:** Machine-to-machine (no user)

```bash
POST /token
Content-Type: application/x-www-form-urlencoded

grant_type=client_credentials&
client_id=CLIENT_ID&
client_secret=CLIENT_SECRET&
scope=api:read api:write
```

### Implicit Flow (Deprecated)

**Don't use for new applications.** Replaced by Authorization Code + PKCE.

## OpenID Connect

**OIDC = OAuth 2.0 + Identity Layer**

### ID Token (JWT)

```json
{
  "iss": "https://auth.example.com",
  "sub": "user123",
  "aud": "client_id",
  "exp": 1704067200,
  "iat": 1704063600,
  "nonce": "random_nonce",
  "name": "Alice Smith",
  "email": "alice@example.com",
  "email_verified": true,
  "picture": "https://example.com/alice.jpg"
}
```

### Standard Scopes

| Scope | Claims Returned |
|-------|-----------------|
| `openid` | Required for OIDC (sub) |
| `profile` | name, family_name, picture, etc. |
| `email` | email, email_verified |
| `address` | address |
| `phone` | phone_number, phone_number_verified |

### UserInfo Endpoint

```bash
GET /userinfo
Authorization: Bearer ACCESS_TOKEN

Response:
{
  "sub": "user123",
  "name": "Alice Smith",
  "email": "alice@example.com"
}
```

## Token Types

### Access Token

```
Authorization: Bearer eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...
```

| Property | Typical Value |
|----------|---------------|
| **Lifetime** | 5-60 minutes |
| **Format** | JWT or opaque |
| **Use** | API authorization |

### Refresh Token

| Property | Typical Value |
|----------|---------------|
| **Lifetime** | Days to months |
| **Format** | Opaque |
| **Use** | Get new access tokens |

```bash
POST /token
Content-Type: application/x-www-form-urlencoded

grant_type=refresh_token&
refresh_token=REFRESH_TOKEN&
client_id=CLIENT_ID&
client_secret=CLIENT_SECRET
```

### ID Token

| Property | Value |
|----------|-------|
| **Format** | Always JWT |
| **Use** | User authentication |
| **Validate** | Signature, exp, aud, iss |

## Token Validation

### JWT Validation Steps

1. **Parse JWT** — Split header.payload.signature
2. **Verify signature** — Using issuer's public key
3. **Check `exp`** — Token not expired
4. **Check `iat`** — Issued at reasonable time
5. **Check `aud`** — Matches your client_id
6. **Check `iss`** — Matches expected issuer
7. **Check `nonce`** — Matches sent nonce (OIDC)

```javascript
const jwt = require('jsonwebtoken');
const jwksClient = require('jwks-rsa');

const client = jwksClient({
  jwksUri: 'https://auth.example.com/.well-known/jwks.json'
});

function getKey(header, callback) {
  client.getSigningKey(header.kid, (err, key) => {
    callback(null, key.getPublicKey());
  });
}

jwt.verify(token, getKey, {
  audience: 'CLIENT_ID',
  issuer: 'https://auth.example.com',
  algorithms: ['RS256']
}, (err, decoded) => {
  if (err) throw err;
  console.log(decoded);
});
```

## Security Best Practices

### Do

| Practice | Rationale |
|----------|-----------|
| **Use PKCE** | Prevents code interception |
| **Validate state** | Prevents CSRF |
| **Use short-lived tokens** | Limit exposure window |
| **Store tokens securely** | HttpOnly cookies or secure storage |
| **Use HTTPS** | Prevent token interception |
| **Validate all claims** | Prevent token misuse |

### Don't

| Anti-Pattern | Risk |
|--------------|------|
| **Store tokens in localStorage** | XSS can steal them |
| **Include secrets in SPAs** | Visible to users |
| **Use Implicit flow** | Token in URL, no refresh |
| **Skip state validation** | CSRF vulnerability |
| **Long-lived access tokens** | Extended exposure if stolen |

## Common Providers

| Provider | OIDC Discovery URL |
|----------|-------------------|
| **Google** | `https://accounts.google.com/.well-known/openid-configuration` |
| **Microsoft** | `https://login.microsoftonline.com/{tenant}/v2.0/.well-known/openid-configuration` |
| **Auth0** | `https://{domain}/.well-known/openid-configuration` |
| **Okta** | `https://{domain}/.well-known/openid-configuration` |
| **Keycloak** | `https://{host}/realms/{realm}/.well-known/openid-configuration` |

## Flow Decision Guide

| Scenario | Recommended Flow |
|----------|------------------|
| **Server-side web app** | Authorization Code |
| **SPA (Single Page App)** | Authorization Code + PKCE |
| **Mobile app** | Authorization Code + PKCE |
| **Machine-to-machine** | Client Credentials |
| **First-party highly trusted** | Resource Owner Password (rare) |

## Related

- [[JWT]] — Token format details
- [[Identity and Access Management]] — IAM overview
- [[API Design Patterns]] — API security
- [[Security MOC]] — Security topics
