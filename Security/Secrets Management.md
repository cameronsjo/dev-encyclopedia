---
title: Secrets Management
aliases:
  - Secret Storage
  - Credential Management
  - Vault
tags:
  - security
  - infrastructure
  - devops
  - concept
type: reference
status: complete
created: 2025-11-30
---

# Secrets Management

Secure storage, distribution, rotation, and auditing of sensitive credentials (API keys, passwords, certificates, tokens) used by applications and infrastructure.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Eliminate hardcoded secrets, centralize access control, enable rotation |
| **Scope** | Application secrets, database credentials, API keys, certificates, encryption keys |
| **Key Features** | Encryption at rest/transit, access policies, audit logs, secret rotation, versioning |
| **Integration** | CI/CD pipelines, Kubernetes, cloud services, application runtimes |
| **Alternatives to** | Environment variables, config files, hardcoded credentials |

## Core Concepts

### Secret Types

**Static Secrets** — Long-lived credentials stored and retrieved as-is.

- API keys, service account tokens, third-party credentials
- Manual or scheduled rotation
- Versioned history for rollback

**Dynamic Secrets** — Generated on-demand with limited TTL.

- Database credentials created per-session
- Cloud IAM roles with temporary tokens
- Automatic revocation after expiration

**Encryption Keys** — Used to encrypt application data.

- Master keys, data encryption keys (DEKs)
- Key rotation without data re-encryption (envelope encryption)
- Hardware Security Module (HSM) backing for compliance

### Access Patterns

**Pull Model** — Applications fetch secrets at runtime.

- Direct API calls to secret manager
- SDK/library integration
- Requires network access to secret service

**Push Model** — Secrets injected into application environment.

- Kubernetes secrets mounted as volumes/env vars
- CI/CD injects secrets during deployment
- Sidecar containers sync secrets to filesystem

**Operator Model** — Platform manages secret lifecycle.

- External Secrets Operator syncs to Kubernetes
- Cloud provider managed identities
- Service mesh handles certificate rotation

## Secret Management Solutions

### Comparison Matrix

| Solution | Type | Dynamic Secrets | Cloud Integration | Open Source | Best For |
|----------|------|-----------------|-------------------|-------------|----------|
| **HashiCorp Vault** | Self-hosted/Cloud | ✅ | Multi-cloud | ✅ | Enterprise multi-cloud |
| **AWS Secrets Manager** | Managed | Limited | AWS native | ❌ | AWS-centric apps |
| **Azure Key Vault** | Managed | Limited | Azure native | ❌ | Azure workloads |
| **GCP Secret Manager** | Managed | ❌ | GCP native | ❌ | GCP applications |
| **1Password** | SaaS | ❌ | CI/CD focus | ❌ | Developer workflows |
| **Doppler** | SaaS | ❌ | Multi-env sync | ❌ | Multi-environment apps |
| **SOPS** | File encryption | ❌ | GitOps friendly | ✅ | GitOps, config files |

### HashiCorp Vault

**Enterprise-grade secret management with dynamic secrets and multi-cloud support.**

**Key Features:**

- Dynamic secret generation for databases, cloud providers, SSH, PKI
- Encryption as a Service for application data
- Identity-based access with multiple auth methods (Kubernetes, AWS IAM, LDAP)
- Secret leasing and automatic revocation
- Audit logging of all operations
- Self-hosted or managed (HCP Vault)

**Use Cases:**

- Multi-cloud environments requiring unified secret management
- Dynamic database credentials for zero-trust applications
- Certificate authority for internal PKI
- Encryption key management with HSM backing

**Considerations:**

- Operational complexity (HA, unsealing, upgrades)
- Requires dedicated infrastructure
- Learning curve for policy language

### AWS Secrets Manager

**Managed secret storage with native AWS service integration.**

**Key Features:**

- Automatic rotation for RDS, Redshift, DocumentDB credentials
- Lambda-based custom rotation functions
- VPC endpoint support for private access
- Cross-account secret sharing
- Integration with IAM policies
- CloudFormation and CDK support

**Use Cases:**

- AWS-native applications
- Rotating RDS database credentials
- Secrets shared across AWS accounts
- Lambda functions requiring API keys

**Considerations:**

- AWS-only (not multi-cloud)
- Cost scales with secret count and API calls
- Limited dynamic secret types vs Vault

### Azure Key Vault

**Azure's managed service for secrets, keys, and certificates.**

**Key Features:**

- Hardware Security Module (HSM) backing
- Managed identities for Azure resources
- Certificate lifecycle management
- Private Link for network isolation
- RBAC with Azure AD integration
- Soft delete and purge protection

**Use Cases:**

- Azure-hosted applications and services
- Managed identity authentication
- Certificate management for App Service, AKS
- Compliance requiring FIPS 140-2 Level 2

**Considerations:**

- Azure-specific (limited multi-cloud)
- Throttling limits on high-traffic scenarios
- Premium tier required for HSM backing

### GCP Secret Manager

**Google Cloud's managed secret storage service.**

**Key Features:**

- Automatic replication across regions
- Secret versioning with enabled/disabled states
- IAM-based access control
- Audit logging via Cloud Audit Logs
- Customer-managed encryption keys (CMEK)
- Integration with Cloud Build, Cloud Run, GKE

**Use Cases:**

- GCP-native applications
- Secrets for Cloud Run, Cloud Functions
- Multi-region secret replication
- Integration with Workload Identity

**Considerations:**

- GCP-only ecosystem
- No dynamic secret generation
- Limited rotation automation vs AWS

### 1Password

**Developer-focused secret management with CLI and CI/CD integration.**

**Key Features:**

- CLI for local development and CI/CD
- Secret references in config files (`op://vault/item/field`)
- Browser extension for credential autofill
- Vaults with team access controls
- Audit logs and activity monitoring
- Integrations with GitHub Actions, GitLab CI, CircleCI

**Use Cases:**

- Developer local environment secrets
- CI/CD pipeline credentials
- Team password sharing
- Bridging personal and work credentials

**Considerations:**

- Not designed for production runtime secrets
- No dynamic secret generation
- Subscription-based pricing

### Doppler

**Universal secret management SaaS for multi-environment applications.**

**Key Features:**

- Environment-based secret organization (dev, staging, prod)
- Automatic syncing to cloud platforms, CI/CD, Kubernetes
- Secret referencing and composition
- Personal and shared configs
- Integrations with 40+ platforms
- Dynamic secret injection (no SDK required)

**Use Cases:**

- Multi-environment secret synchronization
- Replacing scattered .env files
- Syncing secrets to Vercel, Netlify, Heroku
- Teams managing secrets across many services

**Considerations:**

- SaaS-only (no self-hosted option)
- Not designed for dynamic secret generation
- Pricing scales with seats and projects

### SOPS (Secrets OPerationS)

**File encryption tool for GitOps workflows.**

**Key Features:**

- Encrypts values in YAML/JSON/INI files (keys remain plaintext)
- Multi-key support (AWS KMS, GCP KMS, Azure Key Vault, PGP, age)
- Editor integration for transparent encrypt/decrypt
- Git-friendly (only encrypted values change)
- Integrates with Flux, ArgoCD, Helm

**Use Cases:**

- GitOps secret management
- Encrypting Kubernetes manifests
- Config files in version control
- Multi-party secret access (PGP keys)

**Considerations:**

- File-based (not API-driven)
- No rotation automation
- Requires key management strategy
- Not suitable for runtime secret access

## Key Capabilities

### Secret Rotation

**Automatic Rotation** — Service rotates credentials on schedule.

- AWS Secrets Manager: RDS, Redshift, DocumentDB (Lambda-based)
- Azure Key Vault: Certificates via policies
- Vault: Dynamic secrets auto-expire

**Manual Rotation** — Operator triggers rotation.

- Update secret version
- Application fetches latest version
- Old version deprecated after grace period

**Zero-Downtime Rotation:**

1. Create new credential version
2. Dual-running period (old and new both valid)
3. Applications migrate to new version
4. Revoke old version after TTL

### Encryption at Rest

| Feature | Purpose |
|---------|---------|
| **Envelope Encryption** | Data encrypted with DEK, DEK encrypted with master key |
| **HSM Backing** | Hardware-backed key storage for compliance (FIPS 140-2) |
| **Customer Managed Keys** | Bring your own encryption keys (BYOK) |
| **Key Rotation** | Periodic master key rotation without data re-encryption |

### Access Policies

**Identity-Based Access:**

- Vault: Policies attached to authentication tokens
- AWS: IAM policies on roles/users
- Azure: RBAC with Azure AD principals
- GCP: IAM bindings to service accounts

**Attribute-Based Access:**

- Environment tags (production vs staging)
- IP allowlisting
- Time-based restrictions
- MFA requirements for sensitive secrets

**Least Privilege:**

- Separate policies per service
- Read-only vs read-write permissions
- Path-based restrictions (e.g., `/prod/*` vs `/dev/*`)

### Audit Logging

**What to Log:**

- Secret access (read operations)
- Secret modifications (create, update, delete)
- Policy changes
- Authentication attempts
- Rotation events

**Integration:**

- Vault: Audit devices (file, syslog, socket)
- AWS: CloudTrail logs
- Azure: Diagnostic logs to Log Analytics
- GCP: Cloud Audit Logs

**Compliance:**

- Immutable logs for SOC 2, PCI-DSS
- Long-term retention
- SIEM integration for alerting

## Integration Patterns

### Kubernetes Secrets

**Native Kubernetes Secrets:**

```yaml
# Base64-encoded in etcd (NOT encrypted by default)
apiVersion: v1
kind: Secret
metadata:
  name: db-credentials
type: Opaque
data:
  username: YWRtaW4=  # admin
  password: cGFzc3dvcmQ=  # password
```

**Problems:**

- Not encrypted in etcd by default (requires encryption provider)
- Stored in version control if committed
- No rotation automation
- Limited RBAC granularity

**External Secrets Operator:**

```yaml
# Syncs from external secret manager to Kubernetes
apiVersion: external-secrets.io/v1beta1
kind: ExternalSecret
metadata:
  name: db-secret
spec:
  secretStoreRef:
    name: vault-backend
  target:
    name: db-credentials
  data:
  - secretKey: password
    remoteRef:
      key: database/creds/app
      property: password
```

**Benefits:**

- Secrets live in external manager (Vault, AWS, Azure, GCP)
- Automatic sync and rotation
- Single source of truth
- Better audit trail

### Environment Variables vs Secret Managers

| Approach | Security | Rotation | Auditability | Runtime Overhead |
|----------|----------|----------|--------------|------------------|
| **Hardcoded** | ❌ | ❌ | ❌ | None |
| **Environment Variables** | Limited | Manual | ❌ | None |
| **Config Files** | Limited | Manual | Limited | None |
| **Secret Manager (Pull)** | ✅ | ✅ | ✅ | Network call |
| **Secret Manager (Push)** | ✅ | ✅ | ✅ | Startup only |
| **Dynamic Secrets** | ✅ | Automatic | ✅ | Per-request |

**Environment Variables:**

- Visible in process listings (`ps aux`)
- Leaked in logs, error reports
- No rotation without restart
- Acceptable for non-sensitive config

**Secret Managers:**

- Encrypted in transit and at rest
- Access logged and auditable
- Rotation without redeploy
- Credential scoped to application identity

### CI/CD Integration

**GitHub Actions:**

```yaml
# Using 1Password
- uses: 1password/load-secrets-action@v1
  with:
    export-env: true
  env:
    API_KEY: op://ci/service/api_key

# Using Vault
- uses: hashicorp/vault-action@v2
  with:
    url: https://vault.example.com
    role: github-actions
    secrets: |
      secret/data/api key | API_KEY
```

**Doppler:**

- Automatic sync to CI/CD secrets
- Reference secrets without API calls
- Audit CI/CD secret access

**Best Practices:**

- Use OIDC for authentication (no long-lived tokens)
- Scope secrets to specific workflows/branches
- Rotate CI/CD secrets regularly
- Mask secret values in logs

## When to Use

### HashiCorp Vault

**Strengths:**

- Multi-cloud and hybrid environments
- Dynamic secret generation for databases, cloud providers
- Complex access policies and compliance requirements
- Enterprise features (namespaces, replication, HSMs)

**Considerations:**

- Requires operational expertise
- Infrastructure overhead (HA, unsealing)
- Cost of self-hosting or HCP Vault

**Best For:**

- Large enterprises with multi-cloud strategy
- Zero-trust architectures requiring dynamic credentials
- Regulated industries (finance, healthcare)

### Cloud-Native Managed Services

**AWS Secrets Manager / Azure Key Vault / GCP Secret Manager**

**Strengths:**

- Fully managed (no infrastructure)
- Native integration with cloud services
- Automatic scaling and high availability
- Compliance certifications inherited from cloud provider

**Considerations:**

- Vendor lock-in
- Limited to single cloud ecosystem
- API costs at scale

**Best For:**

- Cloud-native applications
- Teams without secret management expertise
- Startups prioritizing speed over multi-cloud

### Developer-Focused Tools

**1Password / Doppler**

**Strengths:**

- Excellent developer experience
- Fast setup and CI/CD integration
- Team collaboration features
- No infrastructure to manage

**Considerations:**

- SaaS dependency
- Not designed for production runtime at scale
- Limited compliance features vs enterprise solutions

**Best For:**

- Developer environment secrets
- CI/CD pipelines
- Small to medium teams
- Replacing scattered .env files

### SOPS

**Strengths:**

- GitOps-native workflow
- No runtime dependency
- Multi-cloud key management
- Free and open source

**Considerations:**

- File-based (not API-driven)
- Manual rotation process
- Requires key management strategy

**Best For:**

- GitOps deployments (Flux, ArgoCD)
- Kubernetes manifests in Git
- Teams already using Git for config
- Multi-party secret access (PGP)

## Decision Guide

| Requirement | Recommended Solution |
|-------------|---------------------|
| Multi-cloud environment | HashiCorp Vault |
| AWS-only workloads | AWS Secrets Manager |
| Azure-only workloads | Azure Key Vault |
| GCP-only workloads | GCP Secret Manager |
| Dynamic database credentials | HashiCorp Vault |
| Developer local secrets | 1Password |
| CI/CD pipeline secrets | 1Password, Doppler, cloud-native |
| Multi-environment sync | Doppler |
| GitOps workflow | SOPS + External Secrets Operator |
| Kubernetes secrets | External Secrets Operator + any backend |
| Certificate management | HashiCorp Vault, Azure Key Vault |
| Compliance (FIPS, PCI-DSS) | HashiCorp Vault (HSM), cloud HSM tiers |
| Small team, fast setup | Doppler, 1Password |
| Enterprise, complex policies | HashiCorp Vault Enterprise |

## Best Practices

**Never Commit Secrets:**

- Use `.gitignore` for `.env`, `secrets.yaml`
- Scan commits with tools like `git-secrets`, `truffleHog`
- Revoke and rotate any committed secrets immediately

**Principle of Least Privilege:**

- One secret per service (no shared credentials)
- Scope access by environment, path, identity
- Use dynamic secrets where possible (short TTL)

**Rotation Strategy:**

- Automate rotation for databases, cloud credentials
- Define rotation schedule (30/60/90 days)
- Test rotation process in non-production first
- Monitor for failed rotations

**Audit and Monitor:**

- Enable audit logging on all secret access
- Alert on anomalies (unusual access patterns, failed auth)
- Integrate logs with SIEM for compliance
- Regular access reviews

**Encryption Everywhere:**

- Secrets encrypted at rest (storage layer)
- Secrets encrypted in transit (TLS)
- Kubernetes etcd encryption enabled
- Use HSMs for regulatory compliance

**Backup and Disaster Recovery:**

- Regular backups of secret manager state
- Test restore procedures
- Document unsealing process (Vault)
- Maintain break-glass credentials securely

**Developer Experience:**

- Provide CLI tools for local secret access
- Document secret retrieval process
- Automate secret injection in development
- Avoid manual secret copying (use secret references)

## Anti-Patterns

| Anti-Pattern | Problem | Solution |
|--------------|---------|----------|
| Hardcoded secrets | Version control leaks, no rotation | Secret manager with runtime fetch |
| Shared credentials | No auditability, blast radius | One credential per service |
| Long-lived tokens | Increased exposure window | Dynamic secrets or short TTL |
| Secrets in logs | Leaks via log aggregation | Mask secrets, use secret references |
| Unencrypted Kubernetes secrets | Readable in etcd | Enable etcd encryption + external secrets |
| Manual rotation | Forgotten, error-prone | Automated rotation schedules |
| Secrets in environment variables | Visible in process list | Fetch at runtime or use injectors |
| No audit logging | Compliance failures | Enable logging on all operations |

## Related

- [[Security Concepts]]
- [[Container Tools]]
- [[Kubernetes]]
- [[CI CD Tools]]
- [[Cloud Platforms]]
- [[Zero Trust Architecture]]
