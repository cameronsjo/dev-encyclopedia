---
title: Essential RFCs
aliases:
  - Important RFCs
  - Must-Know RFCs
  - RFC Reference
tags:
  - rfc
  - standards
  - web
  - networking
  - reference
type: reference
status: complete
created: 2025-11-30
---

# Essential RFCs

RFCs every developer should know.

## Overview

| Category | Key RFCs |
|----------|----------|
| HTTP | 9110-9114 (HTTP semantics, caching, HTTP/2, HTTP/3) |
| URLs & URIs | 3986, 6570 |
| Email | 5321, 5322, 6531 |
| DNS | 1035, 8484 |
| Security | 8446 (TLS), 5280 (X.509) |
| Data Formats | 8259 (JSON), 4180 (CSV) |
| Time | 3339 |
| Semantic Versioning | Not RFC but essential |

---

## HTTP

### RFC 9110 - HTTP Semantics

The foundation of web communication.

**Methods:**

| Method | Safe | Idempotent | Use |
|--------|------|------------|-----|
| GET | ✅ | ✅ | Retrieve resource |
| HEAD | ✅ | ✅ | GET without body |
| POST | ❌ | ❌ | Create/process |
| PUT | ❌ | ✅ | Replace resource |
| PATCH | ❌ | ❌ | Partial update |
| DELETE | ❌ | ✅ | Remove resource |
| OPTIONS | ✅ | ✅ | Supported methods |

**Status Code Classes:**

| Range | Meaning |
|-------|---------|
| 1xx | Informational |
| 2xx | Success |
| 3xx | Redirection |
| 4xx | Client error |
| 5xx | Server error |

**Key Status Codes:**

| Code | Name | When |
|------|------|------|
| 200 | OK | Success with body |
| 201 | Created | Resource created |
| 204 | No Content | Success, no body |
| 301 | Moved Permanently | Permanent redirect |
| 302 | Found | Temporary redirect |
| 304 | Not Modified | Cached response valid |
| 400 | Bad Request | Malformed request |
| 401 | Unauthorized | Auth required |
| 403 | Forbidden | Auth succeeded, not allowed |
| 404 | Not Found | Resource doesn't exist |
| 405 | Method Not Allowed | Wrong HTTP method |
| 409 | Conflict | State conflict |
| 422 | Unprocessable Entity | Validation failed |
| 429 | Too Many Requests | Rate limited |
| 500 | Internal Server Error | Server bug |
| 502 | Bad Gateway | Upstream error |
| 503 | Service Unavailable | Temporarily down |
| 504 | Gateway Timeout | Upstream timeout |

### RFC 9111 - HTTP Caching

Control how responses are cached.

**Cache-Control Directives:**

| Directive | Meaning |
|-----------|---------|
| `max-age=N` | Cache for N seconds |
| `no-cache` | Revalidate before use |
| `no-store` | Never cache |
| `private` | Only browser can cache |
| `public` | CDN/proxy can cache |
| `immutable` | Never changes |
| `stale-while-revalidate=N` | Serve stale, revalidate in background |

**Examples:**

```http
# Cache for 1 hour, CDN can cache
Cache-Control: public, max-age=3600

# Never cache (sensitive data)
Cache-Control: no-store

# Cache forever (versioned assets)
Cache-Control: public, max-age=31536000, immutable

# Cache 1 min, serve stale up to 1 hour while revalidating
Cache-Control: max-age=60, stale-while-revalidate=3600
```

**Validation Headers:**

| Header | Purpose |
|--------|---------|
| `ETag` | Resource version hash |
| `Last-Modified` | Resource modification time |
| `If-None-Match` | Conditional GET with ETag |
| `If-Modified-Since` | Conditional GET with time |

### RFC 9112/9113/9114 - HTTP/1.1, HTTP/2, HTTP/3

| Version | Transport | Key Feature |
|---------|-----------|-------------|
| HTTP/1.1 | TCP | Keep-alive, chunked |
| HTTP/2 | TCP | Multiplexing, server push, header compression |
| HTTP/3 | QUIC (UDP) | No head-of-line blocking, 0-RTT |

**HTTP/2 Benefits:**
- Multiple requests over single connection
- Header compression (HPACK)
- Stream prioritization
- Server push (rarely used)

**HTTP/3 Benefits:**
- Faster connection establishment
- Better on lossy networks
- Connection migration (Wi-Fi → cellular)

---

## URLs & URIs

### RFC 3986 - URI Syntax

```
  foo://example.com:8042/over/there?name=ferret#nose
  \_/   \______________/\_________/ \_________/ \__/
   |           |            |            |        |
scheme     authority       path        query   fragment
```

**Encoding:**

| Character | Encoded |
|-----------|---------|
| Space | `%20` or `+` (query only) |
| `/` | `%2F` |
| `?` | `%3F` |
| `&` | `%26` |
| `=` | `%3D` |
| `#` | `%23` |

**Safe characters (no encoding needed):**
```
A-Z a-z 0-9 - . _ ~
```

### RFC 6570 - URI Templates

```
# Simple
/users/{id}              → /users/123

# Multiple
/users/{id}/posts/{post} → /users/123/posts/456

# Query expansion
/search{?q,limit}        → /search?q=foo&limit=10

# Path segments
/files{/path*}           → /files/a/b/c

# Optional
/users/{id}{?fields}     → /users/123 or /users/123?fields=name
```

---

## Data Formats

### RFC 8259 - JSON

The ubiquitous data format.

**Valid JSON:**
- Objects: `{"key": "value"}`
- Arrays: `[1, 2, 3]`
- Strings: `"hello"` (double quotes only)
- Numbers: `42`, `3.14`, `-1`, `2.5e10`
- Booleans: `true`, `false`
- Null: `null`

**NOT valid JSON:**
- Single quotes: `'hello'`
- Trailing commas: `[1, 2, 3,]`
- Comments: `// comment`
- Undefined: `undefined`
- NaN/Infinity

**Content-Type:** `application/json`

### RFC 8949 - CBOR

Concise Binary Object Representation—binary JSON.

| Feature | JSON | CBOR |
|---------|------|------|
| Format | Text | Binary |
| Size | Larger | Smaller |
| Parse speed | Slower | Faster |
| Types | Limited | Extended (bytes, dates) |

**Use cases:** IoT, constrained environments, high-performance APIs.

### RFC 4180 - CSV

Comma-separated values.

```csv
name,email,age
"John Doe",john@example.com,30
"Jane, Jr.",jane@example.com,25
```

**Rules:**
- Fields with commas, quotes, or newlines must be quoted
- Escape quotes by doubling: `"He said ""hello"""`
- Line endings: CRLF (but LF common)

**Content-Type:** `text/csv`

---

## Date & Time

### RFC 3339 - Date/Time Format

ISO 8601 profile for internet timestamps.

```
# Full timestamp (preferred)
2024-01-15T14:30:00Z

# With timezone offset
2024-01-15T14:30:00+05:30
2024-01-15T09:00:00-05:00

# With fractional seconds
2024-01-15T14:30:00.123Z

# Date only
2024-01-15

# Time only (rarely used)
14:30:00Z
```

**Always:**
- Use UTC (`Z` suffix) for storage/APIs
- Include timezone for user-facing displays
- Use ISO 8601/RFC 3339 format

**Never:**
- Store local times without timezone
- Use ambiguous formats (`01/02/03`)

---

## Email

### RFC 5321 - SMTP

Simple Mail Transfer Protocol.

```
Client: EHLO client.example.com
Server: 250-mail.example.com Hello
        250 OK

Client: MAIL FROM:<sender@example.com>
Server: 250 OK

Client: RCPT TO:<recipient@example.com>
Server: 250 OK

Client: DATA
Server: 354 Start mail input

Client: Subject: Hello

        This is the message body.
        .
Server: 250 OK

Client: QUIT
Server: 221 Bye
```

### RFC 5322 - Email Format

```
From: sender@example.com
To: recipient@example.com
Subject: Meeting tomorrow
Date: Mon, 15 Jan 2024 14:30:00 +0000
Message-ID: <unique-id@example.com>
MIME-Version: 1.0
Content-Type: text/plain; charset=utf-8

Hello,

This is the message body.

Best regards
```

### Email Address Validation

**Don't use regex.** Valid addresses are surprisingly complex:

```
# All valid per RFC
simple@example.com
very.common@example.com
x@example.com
"john doe"@example.com
user+tag@example.com
user@[192.168.1.1]
```

**Best practice:** Basic format check + send verification email.

---

## DNS

### RFC 1035 - DNS

Domain Name System basics.

**Record Types:**

| Type | Purpose | Example |
|------|---------|---------|
| A | IPv4 address | `93.184.216.34` |
| AAAA | IPv6 address | `2606:2800:220:1:...` |
| CNAME | Alias | `www` → `example.com` |
| MX | Mail server | `10 mail.example.com` |
| TXT | Text data | SPF, DKIM, verification |
| NS | Name server | `ns1.example.com` |
| SOA | Start of authority | Zone metadata |
| SRV | Service location | `_http._tcp.example.com` |
| CAA | Certificate authority | `issue "letsencrypt.org"` |

### RFC 8484 - DNS over HTTPS (DoH)

DNS queries over HTTPS for privacy.

```http
GET /dns-query?dns=AAABAAABAAAAAAAAA3d3dwdleGFtcGxlA2NvbQAAAQAB
Accept: application/dns-message
```

**Providers:**
- Cloudflare: `https://cloudflare-dns.com/dns-query`
- Google: `https://dns.google/dns-query`

---

## Security

### RFC 8446 - TLS 1.3

Transport Layer Security—encryption for the web.

**Improvements over TLS 1.2:**
- 1-RTT handshake (vs 2-RTT)
- 0-RTT resumption (optional)
- Removed weak ciphers
- Forward secrecy mandatory
- Encrypted handshake

**Cipher Suites (TLS 1.3):**

| Suite | Notes |
|-------|-------|
| TLS_AES_256_GCM_SHA384 | Recommended |
| TLS_AES_128_GCM_SHA256 | Good |
| TLS_CHACHA20_POLY1305_SHA256 | Good for mobile |

### RFC 6797 - HSTS

HTTP Strict Transport Security.

```http
Strict-Transport-Security: max-age=31536000; includeSubDomains; preload
```

| Directive | Meaning |
|-----------|---------|
| `max-age` | Remember for N seconds |
| `includeSubDomains` | Apply to all subdomains |
| `preload` | Submit to browser preload list |

### RFC 7469 - HPKP (Deprecated)

HTTP Public Key Pinning—deprecated due to risk of bricking sites.

**Use instead:** Certificate Transparency (CT) logs.

### RFC 6066 - TLS Extensions

Notable extensions:

| Extension | Purpose |
|-----------|---------|
| SNI | Server Name Indication (virtual hosting) |
| ALPN | Application-Layer Protocol Negotiation |
| OCSP Stapling | Certificate status |

---

## Networking

### RFC 791/8200 - IP (IPv4/IPv6)

| Aspect | IPv4 | IPv6 |
|--------|------|------|
| Address size | 32 bits | 128 bits |
| Format | `192.168.1.1` | `2001:db8::1` |
| Addresses | ~4 billion | ~340 undecillion |
| Header | Variable | Fixed 40 bytes |

**Private IPv4 ranges:**

| Range | CIDR | Use |
|-------|------|-----|
| 10.0.0.0 – 10.255.255.255 | 10.0.0.0/8 | Large networks |
| 172.16.0.0 – 172.31.255.255 | 172.16.0.0/12 | Medium networks |
| 192.168.0.0 – 192.168.255.255 | 192.168.0.0/16 | Home/small office |

**Special addresses:**

| Address | Purpose |
|---------|---------|
| 127.0.0.1 | Localhost (IPv4) |
| ::1 | Localhost (IPv6) |
| 0.0.0.0 | All interfaces |
| 255.255.255.255 | Broadcast |

### RFC 793/9293 - TCP

Transmission Control Protocol.

**Three-way handshake:**
```
Client ──SYN──────► Server
Client ◄──SYN-ACK── Server
Client ──ACK──────► Server
```

**Connection termination:**
```
Client ──FIN──────► Server
Client ◄──ACK────── Server
Client ◄──FIN────── Server
Client ──ACK──────► Server
```

### RFC 768 - UDP

User Datagram Protocol—connectionless, unreliable, fast.

| TCP | UDP |
|-----|-----|
| Reliable | Unreliable |
| Ordered | Unordered |
| Connection-oriented | Connectionless |
| Slower | Faster |
| HTTP, SMTP, FTP | DNS, gaming, streaming |

---

## WebSocket

### RFC 6455 - WebSocket Protocol

Full-duplex communication over single TCP connection.

**Handshake:**

```http
GET /chat HTTP/1.1
Host: server.example.com
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
Sec-WebSocket-Version: 13
```

```http
HTTP/1.1 101 Switching Protocols
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
```

**Frame types:**

| Opcode | Type |
|--------|------|
| 0x1 | Text frame |
| 0x2 | Binary frame |
| 0x8 | Close |
| 0x9 | Ping |
| 0xA | Pong |

---

## Miscellaneous

### RFC 4122 - UUID

Universally Unique Identifier.

| Version | Generation Method |
|---------|-------------------|
| v1 | Timestamp + MAC address |
| v4 | Random (most common) |
| v5 | SHA-1 hash of namespace + name |
| v7 | Timestamp + random (sortable, new) |

```
550e8400-e29b-41d4-a716-446655440000
         │    │
         │    └── Version (4 = random)
         └─────── Variant (1 = RFC 4122)
```

**Prefer ULIDs** for sortable, URL-safe identifiers.

### RFC 7807 - Problem Details

Standard error format for HTTP APIs.

```json
{
  "type": "https://example.com/errors/insufficient-funds",
  "title": "Insufficient Funds",
  "status": 403,
  "detail": "Your account balance is $30, but the transfer requires $50.",
  "instance": "/accounts/12345/transfers/67890",
  "balance": 30,
  "required": 50
}
```

**Content-Type:** `application/problem+json`

### RFC 5988/8288 - Web Linking

Link headers for pagination, related resources.

```http
Link: <https://api.example.com/users?page=2>; rel="next",
      <https://api.example.com/users?page=10>; rel="last",
      <https://api.example.com/users?page=1>; rel="first"
```

---

## Quick Reference

### Must-Know RFCs

| RFC | Topic | Priority |
|-----|-------|----------|
| 9110 | HTTP Semantics | Essential |
| 9111 | HTTP Caching | Essential |
| 8259 | JSON | Essential |
| 3339 | Date/Time | Essential |
| 3986 | URI Syntax | Essential |
| 8446 | TLS 1.3 | Important |
| 6455 | WebSocket | Important |
| 7807 | Problem Details | Useful |
| 6570 | URI Templates | Useful |

### Content-Type Quick Reference

| Type | Content-Type |
|------|--------------|
| JSON | `application/json` |
| Form data | `application/x-www-form-urlencoded` |
| File upload | `multipart/form-data` |
| Plain text | `text/plain` |
| HTML | `text/html` |
| CSV | `text/csv` |
| Binary | `application/octet-stream` |
| PDF | `application/pdf` |
| Error | `application/problem+json` |

---

## Related

- [[Auth Standards & RFCs]]
- [[API Design Patterns]]
- [[Security Concepts]]
