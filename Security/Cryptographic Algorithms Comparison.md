---
title: Cryptographic Algorithms Comparison
aliases:
  - Crypto Algorithm Comparison
  - Encryption Algorithm Comparison
tags:
  - security
  - cryptography
  - comparison
type: comparison
status: complete
created: "2025-12-07"
---

# Cryptographic Algorithms Comparison

Comprehensive comparison of cryptographic algorithms across symmetric, asymmetric, hashing, and post-quantum categories.

## Quick Reference

### Recommended Algorithms by Use Case

| Use Case | Recommended | Alternative | Avoid |
|----------|-------------|-------------|-------|
| **Symmetric encryption** | AES-256-GCM | ChaCha20-Poly1305 | DES, 3DES, Blowfish |
| **Key exchange** | X25519 | ECDH P-256 | RSA key transport, static DH |
| **Digital signatures** | Ed25519 | ECDSA P-256 | RSA-1024, DSA |
| **Password hashing** | Argon2id | bcrypt | SHA-256, MD5 |
| **File integrity** | SHA-256, BLAKE3 | SHA-3 | MD5, SHA-1 |
| **MAC** | HMAC-SHA256 | Poly1305 | HMAC-MD5 |
| **Post-quantum KEM** | ML-KEM-768 | ML-KEM-1024 | — |
| **Post-quantum signatures** | ML-DSA-65 | SLH-DSA | — |

## Symmetric Encryption Algorithms

### Block Ciphers

| Algorithm | Key Sizes | Block Size | Status | Performance | Notes |
|-----------|-----------|------------|--------|-------------|-------|
| **AES** | 128/192/256 | 128 | ✅ Standard | Fast (HW accel) | NIST standard, ubiquitous |
| **ChaCha20** | 256 | Stream | ✅ Modern | Very fast (no HW) | Preferred on mobile |
| **Twofish** | 128-256 | 128 | ✅ Secure | Moderate | AES finalist |
| **Serpent** | 128-256 | 128 | ✅ Secure | Slow | Most conservative AES finalist |
| **Camellia** | 128-256 | 128 | ✅ Secure | Fast | ISO/NESSIE standard |
| **3DES** | 168 (112 effective) | 64 | ❌ Deprecated | Slow | Sweet32 vulnerability |
| **Blowfish** | 32-448 | 64 | ❌ Legacy | Fast | 64-bit block = vulnerable |
| **DES** | 56 | 64 | ❌ Broken | — | Brute-forceable since 1999 |

### Authenticated Encryption (AEAD)

| Mode | Base Cipher | Auth Tag | Parallel | Status | Use Case |
|------|-------------|----------|----------|--------|----------|
| **AES-GCM** | AES | 128-bit | ✅ | ✅ Standard | TLS, disk encryption |
| **ChaCha20-Poly1305** | ChaCha20 | 128-bit | ✅ | ✅ Modern | Mobile, WireGuard |
| **AES-CCM** | AES | Variable | ❌ | ✅ | IoT, constrained |
| **AES-OCB** | AES | Variable | ✅ | ✅ | Patent-free now |
| **AES-SIV** | AES | 128-bit | ❌ | ✅ | Deterministic encryption |
| **AES-CBC + HMAC** | AES | — | ❌ | ⚠️ | Legacy (encrypt-then-MAC) |

### Block Cipher Modes (Non-AEAD)

| Mode | Parallel Encrypt | Parallel Decrypt | Random Access | Status |
|------|------------------|------------------|---------------|--------|
| **CTR** | ✅ | ✅ | ✅ | ✅ Use with MAC |
| **CBC** | ❌ | ✅ | ❌ | ⚠️ Legacy |
| **CFB** | ❌ | ✅ | ❌ | ⚠️ Legacy |
| **OFB** | ❌ | ❌ | ❌ | ⚠️ Legacy |
| **ECB** | ✅ | ✅ | ✅ | ❌ Never use |

## Asymmetric Encryption & Key Exchange

### Key Exchange Algorithms

| Algorithm | Type | Key Size | Security | Performance | Status |
|-----------|------|----------|----------|-------------|--------|
| **X25519** | ECDH | 256-bit | 128-bit | Very fast | ✅ Preferred |
| **ECDH P-256** | ECDH | 256-bit | 128-bit | Fast | ✅ Standard |
| **ECDH P-384** | ECDH | 384-bit | 192-bit | Moderate | ✅ High security |
| **X448** | ECDH | 448-bit | 224-bit | Fast | ✅ Higher margin |
| **DH 2048** | Classical DH | 2048-bit | 112-bit | Slow | ⚠️ Acceptable |
| **DH 4096** | Classical DH | 4096-bit | 140-bit | Very slow | ⚠️ If required |
| **RSA-OAEP** | Encryption | 2048+ | 112+ | Very slow | ⚠️ Key transport only |
| **ML-KEM-768** | Lattice | 1,184 B | 192-bit | Moderate | ✅ Post-quantum |

### Key Size Equivalence

| Security Level | Symmetric | RSA/DH | ECC | Post-Quantum |
|---------------|-----------|--------|-----|--------------|
| 80-bit (legacy) | 80 | 1024 | 160 | — |
| 112-bit | 112 | 2048 | 224 | — |
| **128-bit** | **128** | **3072** | **256** | **ML-KEM-512** |
| 192-bit | 192 | 7680 | 384 | ML-KEM-768 |
| 256-bit | 256 | 15360 | 521 | ML-KEM-1024 |

## Digital Signature Algorithms

### Algorithm Comparison

| Algorithm | Type | Key Size | Sig Size | Sign Speed | Verify Speed | Status |
|-----------|------|----------|----------|------------|--------------|--------|
| **Ed25519** | EdDSA | 256-bit | 64 B | Very fast | Very fast | ✅ Preferred |
| **Ed448** | EdDSA | 448-bit | 114 B | Fast | Fast | ✅ Higher margin |
| **ECDSA P-256** | ECDSA | 256-bit | 64 B | Fast | Moderate | ✅ Standard |
| **ECDSA P-384** | ECDSA | 384-bit | 96 B | Moderate | Moderate | ✅ High security |
| **RSA-2048** | RSA-PSS | 2048-bit | 256 B | Slow | Fast | ✅ Widely supported |
| **RSA-4096** | RSA-PSS | 4096-bit | 512 B | Very slow | Fast | ✅ Long-term |
| **DSA** | DSA | 2048-bit | 64 B | Slow | Slow | ❌ Deprecated |
| **ML-DSA-65** | Lattice | 1,952 B | 3,293 B | Fast | Fast | ✅ Post-quantum |
| **SLH-DSA-128s** | Hash | 32 B | 7,856 B | Slow | Fast | ✅ Post-quantum |

### EdDSA vs ECDSA

| Aspect | EdDSA (Ed25519) | ECDSA (P-256) |
|--------|-----------------|---------------|
| **Nonce** | Deterministic | Random (dangerous) |
| **Side-channels** | Easier to resist | Harder |
| **Performance** | Faster | Slower |
| **Specification** | Fully specified | Ambiguous |
| **Key recovery risk** | None | Same k → key leak |
| **Adoption** | SSH, Signal, newer systems | TLS, Bitcoin, legacy |

## Hash Functions

### General-Purpose Hashes

| Algorithm | Output | Security | Performance | Status | Use Cases |
|-----------|--------|----------|-------------|--------|-----------|
| **SHA-256** | 256-bit | 128-bit collision | Fast | ✅ Standard | Certificates, blockchain |
| **SHA-384** | 384-bit | 192-bit collision | Moderate | ✅ | High security |
| **SHA-512** | 512-bit | 256-bit collision | Fast (64-bit) | ✅ | Large files |
| **SHA-3-256** | 256-bit | 128-bit collision | Moderate | ✅ | NIST backup |
| **BLAKE2b** | Up to 512 | 256-bit collision | Very fast | ✅ Modern | General purpose |
| **BLAKE3** | 256-bit | 128-bit collision | Extremely fast | ✅ Modern | High performance |
| **SHA-1** | 160-bit | Broken | Fast | ❌ Broken | Legacy (Git) |
| **MD5** | 128-bit | Broken | Fast | ❌ Broken | Non-security checksums |

### Hash Performance (Relative)

```
BLAKE3:     ████████████████████████████████  (fastest)
BLAKE2b:    ██████████████████████████
SHA-256:    ███████████████████
SHA-512:    █████████████████████ (on 64-bit)
SHA-3-256:  ████████████████
```

### Password Hashing

| Algorithm | Type | Memory-Hard | Tunable | Max Length | Status |
|-----------|------|-------------|---------|------------|--------|
| **Argon2id** | Memory-hard | ✅ | Time, memory, parallelism | Unlimited | ✅ Winner |
| **Argon2i** | Memory-hard | ✅ | Time, memory, parallelism | Unlimited | ✅ Side-channel resistant |
| **Argon2d** | Memory-hard | ✅ | Time, memory, parallelism | Unlimited | ⚠️ Fast but side-channel risk |
| **scrypt** | Memory-hard | ✅ | N, r, p | Unlimited | ✅ Good |
| **bcrypt** | CPU-hard | ❌ | Work factor | 72 chars | ✅ Standard |
| **PBKDF2** | CPU-hard | ❌ | Iterations | Unlimited | ⚠️ Acceptable |

### Recommended Password Hash Parameters

| Algorithm | Parameters | Notes |
|-----------|------------|-------|
| **Argon2id** | m=64MB, t=3, p=4 | OWASP recommendation |
| **bcrypt** | cost=12+ | ~250ms on modern CPU |
| **scrypt** | N=2^17, r=8, p=1 | ~100ms |
| **PBKDF2** | 600,000+ iterations | SHA-256 |

## Post-Quantum Algorithms

### NIST Standardized (2024)

| Algorithm | Type | Public Key | Private Key | Ciphertext/Sig | Security |
|-----------|------|------------|-------------|----------------|----------|
| **ML-KEM-512** | KEM | 800 B | 1,632 B | 768 B | Level 1 |
| **ML-KEM-768** | KEM | 1,184 B | 2,400 B | 1,088 B | Level 3 |
| **ML-KEM-1024** | KEM | 1,568 B | 3,168 B | 1,568 B | Level 5 |
| **ML-DSA-44** | Signature | 1,312 B | 2,560 B | 2,420 B | Level 2 |
| **ML-DSA-65** | Signature | 1,952 B | 4,032 B | 3,293 B | Level 3 |
| **ML-DSA-87** | Signature | 2,592 B | 4,896 B | 4,595 B | Level 5 |
| **SLH-DSA-128s** | Signature | 32 B | 64 B | 7,856 B | Level 1 |
| **SLH-DSA-256f** | Signature | 64 B | 128 B | 49,856 B | Level 5 |

### Size Comparison: Classical vs Post-Quantum

| Operation | Classical | Post-Quantum | Increase |
|-----------|-----------|--------------|----------|
| **Key exchange pubkey** | X25519: 32 B | ML-KEM-768: 1,184 B | 37x |
| **Signature** | Ed25519: 64 B | ML-DSA-65: 3,293 B | 51x |
| **Signature pubkey** | Ed25519: 32 B | ML-DSA-65: 1,952 B | 61x |

## Protocol Cipher Suites

### TLS 1.3 (All AEAD, All PFS)

| Cipher Suite | Encryption | Key Exchange | Status |
|--------------|------------|--------------|--------|
| `TLS_AES_256_GCM_SHA384` | AES-256-GCM | ECDHE | ✅ Preferred |
| `TLS_CHACHA20_POLY1305_SHA256` | ChaCha20-Poly1305 | ECDHE | ✅ Mobile |
| `TLS_AES_128_GCM_SHA256` | AES-128-GCM | ECDHE | ✅ Good |

### SSH Modern Recommendations

| Type | Algorithm | Status |
|------|-----------|--------|
| **Key exchange** | curve25519-sha256 | ✅ Preferred |
| **Host key** | ssh-ed25519 | ✅ Preferred |
| **Cipher** | <chacha20-poly1305@openssh.com> | ✅ Preferred |
| **MAC** | (implicit with AEAD) | — |

### WireGuard (Fixed Suite)

| Component | Algorithm |
|-----------|-----------|
| **Key exchange** | Curve25519 |
| **Encryption** | ChaCha20-Poly1305 |
| **Hashing** | BLAKE2s |
| **Key derivation** | HKDF |

## Decision Matrix

### Symmetric Encryption

```
Need HW acceleration? ────────────────────┐
         │                                │
         ▼                                ▼
    AES available?                   ChaCha20-Poly1305
         │
    Yes ─┴─ No
         │    │
         ▼    ▼
   AES-256-GCM  ChaCha20-Poly1305
```

### Asymmetric Cryptography

```
Post-quantum required? ─── Yes ──► ML-KEM / ML-DSA (hybrid)
         │
         No
         │
    Legacy compatibility? ─── Yes ──► RSA-2048+ or P-256
         │
         No
         │
    ▼
Ed25519 / X25519
```

### Password Storage

```
Memory constraints? ─── Severe ──► bcrypt (cost 12+)
         │
         No
         │
    Side-channel risk? ─── High ──► Argon2i
         │
         Normal
         │
    ▼
Argon2id (64MB, t=3, p=4)
```

## Migration Paths

### From Legacy to Modern

| Legacy | Modern Replacement | Migration Notes |
|--------|-------------------|-----------------|
| 3DES | AES-256-GCM | Direct replacement |
| RSA-1024 | Ed25519 or RSA-3072 | New key generation |
| SHA-1 | SHA-256 | Hash recalculation |
| MD5 (passwords) | Argon2id | Rehash on login |
| ECDSA | Ed25519 | New key generation |
| RSA key transport | ECDHE | Protocol update |
| TLS 1.0/1.1 | TLS 1.3 | Server configuration |

### Quantum Migration Timeline

| Phase | Timeframe | Actions |
|-------|-----------|---------|
| **Inventory** | Now | Catalog all crypto usage |
| **Hybrid prep** | 2024-2025 | Test PQC, update libraries |
| **Hybrid deploy** | 2025-2028 | Classical + PQC together |
| **Full PQC** | 2028-2035 | Phase out classical-only |

## Related

- [[Cryptography]]
- [[Elliptic Curve Cryptography]]
- [[Post-Quantum Cryptography]]
- [[Forward Secrecy]]
- [[One-Time Pad]]
- [[Security Concepts]]

## References

- [NIST Cryptographic Standards](https://csrc.nist.gov/projects/cryptographic-standards-and-guidelines)
- [Cryptographic Right Answers (2023)](https://latacora.micro.blog/2018/04/03/cryptographic-right-answers.html)
- [Mozilla TLS Configuration](https://wiki.mozilla.org/Security/Server_Side_TLS)
- [OWASP Password Storage Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html)
