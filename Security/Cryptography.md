---
title: Cryptography
aliases:
  - Crypto
  - Encryption
  - Cryptographic Primitives
tags:
  - security
  - cryptography
  - fundamentals
  - concept
type: reference
status: complete
created: 2025-11-30
---

# Cryptography

Mathematical techniques for securing information through encryption, authentication, and integrity verification.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Confidentiality, integrity, authentication, non-repudiation |
| **Core Primitives** | Symmetric encryption, asymmetric encryption, hashing, digital signatures |
| **Common Uses** | TLS/SSL, password storage, data encryption, authentication, key exchange |
| **Modern Standards** | AES-256, RSA-2048+, ECC, SHA-256, Ed25519 |
| **Emerging Threats** | Quantum computing (post-quantum crypto in development) |

## Symmetric Encryption

Same key for encryption and decryption. Fast, suitable for bulk data.

| Algorithm | Key Size | Block Size | Status | Use Cases |
|-----------|----------|------------|--------|-----------|
| **AES** | 128/192/256-bit | 128-bit | ✅ Recommended | Database encryption, file encryption, TLS |
| **ChaCha20** | 256-bit | Stream cipher | ✅ Recommended | Mobile devices, low-power systems |
| **3DES** | 168-bit | 64-bit | ❌ Deprecated | Legacy systems only |
| **Blowfish** | 32-448-bit | 64-bit | ❌ Superseded by AES | Legacy systems only |

**Modes of Operation:**

- **GCM (Galois/Counter Mode)**: Provides both encryption and authentication (AEAD)
- **CBC (Cipher Block Chaining)**: Requires separate MAC, older standard
- **CTR (Counter Mode)**: Converts block cipher to stream cipher
- **ECB**: ❌ Never use (deterministic, reveals patterns)

**Best Practice:** Use AES-256-GCM for new applications.

## Asymmetric Encryption

Key pairs (public/private). Slower, used for key exchange and digital signatures.

| Algorithm | Key Size | Speed | Status | Use Cases |
|-----------|----------|-------|--------|-----------|
| **RSA** | 2048/3072/4096-bit | Slow | ✅ Still standard | TLS, SSH, PGP, code signing |
| **ECC (Elliptic Curve)** | 256/384/521-bit | Fast | ✅ Preferred | Mobile, IoT, modern protocols |
| **Ed25519** | 256-bit | Very fast | ✅ Recommended | SSH keys, signatures, cryptocurrency |
| **X25519** | 256-bit | Very fast | ✅ Recommended | Key exchange (ECDH) |
| **DSA** | 1024-3072-bit | Moderate | ❌ Deprecated | Legacy only |

**ECC Advantages:**

- Smaller keys (256-bit ECC ≈ 3072-bit RSA security)
- Faster computation
- Lower power consumption
- Better for mobile and embedded systems

**Best Practice:** Use Ed25519 for signatures, X25519 for key exchange.

## Cryptographic Hashing

One-way functions producing fixed-size output. Used for integrity verification and password storage.

### General-Purpose Hashing

| Algorithm | Output Size | Status | Use Cases |
|-----------|-------------|--------|-----------|
| **SHA-256** | 256-bit | ✅ Recommended | File integrity, blockchain, certificates |
| **SHA-3** | 224/256/384/512-bit | ✅ Recommended | General hashing, NIST standard |
| **BLAKE3** | 256-bit | ✅ Modern choice | High-performance hashing |
| **SHA-1** | 160-bit | ❌ Broken | Legacy only (Git uses it) |
| **MD5** | 128-bit | ❌ Broken | Checksums only (not security) |

### Password Hashing

**NEVER** use general-purpose hashes for passwords. Use specialized algorithms designed to be slow.

| Algorithm | Type | Adjustable Cost | Status | Notes |
|-----------|------|-----------------|--------|-------|
| **Argon2id** | Memory-hard | ✅ | ✅ Winner of PHC 2015 | Best choice for new systems |
| **bcrypt** | CPU-hard | ✅ | ✅ Still good | Industry standard, max 72 chars |
| **scrypt** | Memory-hard | ✅ | ✅ Good | Used by some cryptocurrencies |
| **PBKDF2** | CPU-hard | ✅ | ⚠️ Acceptable | Slower to compute than Argon2 |
| **SHA-256** | Fast hash | ❌ | ❌ Never use | Vulnerable to brute force |

**Best Practice:** Use Argon2id for password hashing with appropriate memory/time parameters.

## Message Authentication Codes (MAC)

Verify message integrity and authenticity.

| Type | Example | Use Cases |
|------|---------|-----------|
| **HMAC** | HMAC-SHA256 | API authentication, JWT signatures |
| **CMAC** | AES-CMAC | Block cipher-based MAC |
| **Poly1305** | ChaCha20-Poly1305 | AEAD with ChaCha20 |

**HMAC** is the most common. Used with a secret key to produce a tag.

## Key Exchange

Securely establish shared secrets over insecure channels.

| Protocol | Based On | Status | Use Cases |
|----------|----------|--------|-----------|
| **ECDH** (X25519) | Elliptic Curve Diffie-Hellman | ✅ Recommended | TLS 1.3, Signal Protocol |
| **DHE** | Diffie-Hellman Ephemeral | ✅ Still used | TLS, VPNs |
| **RSA Key Exchange** | RSA encryption | ❌ Deprecated in TLS 1.3 | Legacy TLS 1.2 |

**Perfect Forward Secrecy (PFS):** Use ephemeral keys (DHE, ECDHE) so compromised long-term keys don't reveal past sessions.

## Digital Signatures

Prove authenticity and non-repudiation of messages.

| Algorithm | Type | Performance | Use Cases |
|-----------|------|-------------|-----------|
| **Ed25519** | EdDSA (Edwards Curve) | Very fast | SSH, Git commits, cryptocurrency |
| **ECDSA** | Elliptic Curve DSA | Fast | Bitcoin, TLS certificates |
| **RSA-PSS** | RSA with PSS padding | Slow | Code signing, document signing |
| **RSA-PKCS1** | Legacy RSA | Slow | Legacy systems only |

**Best Practice:** Use Ed25519 for new applications.

## TLS/SSL

Transport Layer Security — encrypts data in transit.

| TLS Version | Status | Key Features |
|-------------|--------|--------------|
| **TLS 1.3** | ✅ Use this | Faster handshake, only AEAD ciphers, mandatory PFS |
| **TLS 1.2** | ✅ Acceptable | Still widely supported, secure if configured properly |
| **TLS 1.1** | ❌ Deprecated | Vulnerable to attacks |
| **SSL 3.0** | ❌ Broken | POODLE attack |

**TLS 1.3 Cipher Suites (all AEAD):**

- `TLS_AES_256_GCM_SHA384`
- `TLS_CHACHA20_POLY1305_SHA256`
- `TLS_AES_128_GCM_SHA256`

**Certificate Chain:** Client ← Server Cert ← Intermediate CA ← Root CA

### Certificate Types

| Type | Validation | Use Cases |
|------|------------|-----------|
| **DV** (Domain Validated) | Domain ownership only | Personal sites, blogs |
| **OV** (Organization Validated) | Organization verified | Business websites |
| **EV** (Extended Validation) | Extensive verification | Banks, e-commerce (less common now) |
| **Wildcard** | `*.example.com` | Subdomains |
| **SAN** (Subject Alternative Name) | Multiple domains | Multi-domain certificates |

**Best Practice:** Use Let's Encrypt for free DV certificates with automated renewal.

## Key Management

| Aspect | Recommendations |
|--------|----------------|
| **Storage** | Hardware Security Module (HSM), Key Management Service (KMS), encrypted vaults |
| **Rotation** | Regular key rotation (every 90 days for high-security, annually otherwise) |
| **Access Control** | Principle of least privilege, audit logging |
| **Backup** | Encrypted backups in separate locations |
| **Destruction** | Secure deletion when keys expire |

**Common KMS:**

- AWS KMS
- Google Cloud KMS
- Azure Key Vault
- HashiCorp Vault
- PKCS#11 for HSMs

## Advanced Cryptography

### Zero-Knowledge Proofs

Prove knowledge of information without revealing the information itself.

| Type | Use Cases |
|------|-----------|
| **zk-SNARKs** | Privacy-preserving cryptocurrencies (Zcash), blockchain scalability |
| **zk-STARKs** | Quantum-resistant, transparent setup |

**Applications:** Anonymous credentials, private transactions, verifiable computation.

### Homomorphic Encryption

Perform computations on encrypted data without decrypting.

| Type | Capabilities | Maturity |
|------|--------------|----------|
| **Partially Homomorphic** | Addition OR multiplication | ✅ Production-ready |
| **Fully Homomorphic (FHE)** | Arbitrary computations | ⚠️ Research/early adoption |

**Use Cases:** Cloud computing on sensitive data, secure multi-party computation.

### Post-Quantum Cryptography

Algorithms resistant to quantum computer attacks.

| Algorithm | Type | Status |
|-----------|------|--------|
| **CRYSTALS-Kyber** | Key encapsulation | ✅ NIST selected (2024) |
| **CRYSTALS-Dilithium** | Digital signatures | ✅ NIST selected (2024) |
| **SPHINCS+** | Stateless signatures | ✅ NIST selected (2024) |

**Timeline:** Quantum computers capable of breaking RSA/ECC expected 2030-2040. Transition should begin now for long-lived systems.

## Common Cryptographic Protocols

| Protocol | Purpose | Cryptographic Components |
|----------|---------|--------------------------|
| **TLS 1.3** | Secure web traffic | ECDHE key exchange, AEAD ciphers, X.509 certificates |
| **SSH** | Secure remote access | Ed25519/RSA keys, ChaCha20 or AES encryption |
| **JWT** | Token-based auth | HMAC-SHA256 or RSA/ECDSA signatures |
| **OpenPGP** | Email encryption | RSA/ECC for key exchange, AES for data |
| **Signal Protocol** | E2E messaging | Double Ratchet (ECDH), AES-256-GCM |
| **WireGuard** | VPN | ChaCha20-Poly1305, X25519, BLAKE2s |

## When to Use: Algorithm Recommendations

### Data Encryption

| Scenario | Algorithm | Notes |
|----------|-----------|-------|
| **Database encryption at rest** | AES-256-GCM | Use database-native TDE or application-level encryption |
| **File encryption** | AES-256-GCM or ChaCha20-Poly1305 | Both provide AEAD |
| **Streaming data** | ChaCha20-Poly1305 | Better performance than AES on mobile |
| **API token encryption** | AES-256-GCM | Short-lived tokens |

### Authentication & Signatures

| Scenario | Algorithm | Notes |
|----------|-----------|-------|
| **SSH keys** | Ed25519 | Faster and smaller than RSA-4096 |
| **Code signing** | RSA-4096 + SHA-256 or Ed25519 | RSA more widely supported |
| **API signatures** | HMAC-SHA256 or Ed25519 | HMAC for symmetric, Ed25519 for asymmetric |
| **JWT signatures** | RS256 (RSA-SHA256) or ES256 (ECDSA) | ES256 smaller tokens |

### Password Storage

| Scenario | Algorithm | Configuration |
|----------|-----------|---------------|
| **New applications** | Argon2id | Memory: 64MB, iterations: 3, parallelism: 4 |
| **Existing systems (bcrypt)** | bcrypt | Work factor: 12-14 |
| **Constrained environments** | scrypt | N=16384, r=8, p=1 |

### Hashing & Integrity

| Scenario | Algorithm | Notes |
|----------|-----------|-------|
| **File integrity** | SHA-256 or BLAKE3 | BLAKE3 faster for large files |
| **Blockchain/cryptocurrency** | SHA-256 (Bitcoin) or Keccak (Ethereum) | Domain-specific |
| **Message authentication** | HMAC-SHA256 | Standard choice |

### Key Exchange

| Scenario | Algorithm | Notes |
|----------|-----------|-------|
| **TLS 1.3** | X25519 (ECDHE) | Default in modern browsers |
| **VPN** | X25519 or traditional DH | WireGuard uses X25519 |
| **Messaging** | X25519 (Signal Protocol) | Double Ratchet for forward secrecy |

## Anti-Patterns

**❌ NEVER:**

- Roll your own crypto
- Use ECB mode
- Use MD5 or SHA-1 for security purposes
- Hash passwords with fast algorithms (SHA-256, MD5)
- Reuse initialization vectors (IVs) with the same key
- Use RSA without padding (use OAEP for encryption, PSS for signatures)
- Store keys in source code
- Use weak random number generators (`Math.random()`)

**✅ ALWAYS:**

- Use well-vetted libraries (OpenSSL, libsodium, cryptography.io)
- Use authenticated encryption (AEAD: AES-GCM, ChaCha20-Poly1305)
- Use cryptographically secure random number generators
- Validate all inputs
- Keep libraries updated
- Use TLS 1.2+ with strong cipher suites
- Implement proper key rotation

## Common Libraries

| Language | Library | Notes |
|----------|---------|-------|
| **Python** | `cryptography` | Industry standard, comprehensive |
| **JavaScript** | `crypto` (Node.js), Web Crypto API | Native, avoid third-party for primitives |
| **Java** | JCA/JCE, Bouncy Castle | JCA built-in, BC for advanced features |
| **Go** | `crypto/*` | Standard library, well-designed |
| **Rust** | `ring`, `RustCrypto` | `ring` fast and audited, RustCrypto pure Rust |
| **C/C++** | libsodium, OpenSSL | libsodium simpler API, OpenSSL comprehensive |
| **.NET** | `System.Security.Cryptography` | Built-in, modern APIs in .NET 6+ |

## Compliance & Standards

| Standard | Focus | Key Requirements |
|----------|-------|------------------|
| **FIPS 140-2/3** | Cryptographic modules | Validated algorithms, physical security |
| **PCI DSS** | Payment card security | TLS 1.2+, strong encryption, key management |
| **HIPAA** | Healthcare data | Encryption at rest and in transit |
| **GDPR** | Personal data | Encryption as security measure, pseudonymization |
| **Common Criteria** | IT security | Certification framework (EAL1-EAL7) |

## Related

- [[Elliptic Curve Cryptography]] — Deep dive into ECC, curves, and EdDSA
- [[Post-Quantum Cryptography]] — NIST PQC standards, migration strategies
- [[Forward Secrecy]] — Perfect forward secrecy, future secrecy, Double Ratchet
- [[One-Time Pad]] — The only provably unbreakable cipher
- [[Cryptographic Algorithms Comparison]] — Side-by-side algorithm comparison
- [[Security Concepts]]
- [[Auth Standards & RFCs]]
- [[TLS Best Practices]]
- [[Zero Trust Architecture]]
- [[Secure Coding Practices]]
