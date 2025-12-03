---
title: File Storage
aliases:
  - Object Storage
  - Cloud Storage
  - Blob Storage
tags:
  - tool
  - storage
  - infrastructure
  - comparison
type: reference
status: complete
created: 2025-12-03
---

# File Storage

Choosing the right storage solution for files, objects, and binary data.

## Storage Types Overview

| Type | Model | Best For |
|------|-------|----------|
| Object Storage | Flat namespace, key→blob | Unstructured data, media, backups, data lakes |
| Block Storage | Raw disk volumes | Databases, VMs, high IOPS workloads |
| File Storage | Hierarchical filesystem | Shared access, legacy apps, home directories |
| Distributed FS | Clustered filesystem | Big data, analytics, parallel processing |

---

## Object Storage

### Overview

Store arbitrary blobs with metadata. Flat namespace (buckets/containers + keys). HTTP API access.

```
┌─────────────────────────────────────────────────────────┐
│  Bucket: my-app-assets                                  │
├─────────────────────────────────────────────────────────┤
│  images/logo.png         → blob (45KB)                  │
│  images/banner.jpg       → blob (1.2MB)                 │
│  videos/intro.mp4        → blob (50MB)                  │
│  backups/2024-01-15.tar  → blob (2GB)                   │
└─────────────────────────────────────────────────────────┘

Each object has:
  - Key (path-like identifier)
  - Data (binary blob)
  - Metadata (content-type, custom headers)
  - Version ID (if versioning enabled)
```

### Major Providers

| Provider | Service | Notes |
|----------|---------|-------|
| AWS | S3 | Industry standard, most integrations |
| Google Cloud | Cloud Storage | Strong analytics integration |
| Azure | Blob Storage | Enterprise, .NET-friendly |
| Cloudflare | R2 | S3-compatible, zero egress fees |
| Backblaze | B2 | Low cost, S3-compatible |
| MinIO | MinIO | Self-hosted, S3-compatible |
| DigitalOcean | Spaces | Simple, S3-compatible |
| Wasabi | Wasabi | Low cost, no egress fees |

### S3 vs GCS vs Azure Blob

| Aspect | AWS S3 | Google Cloud Storage | Azure Blob |
|--------|--------|---------------------|------------|
| API | S3 (de facto standard) | JSON/XML, S3 interop | REST, S3 adapter |
| Consistency | Strong (2020+) | Strong | Strong |
| Storage classes | 6 tiers | 4 tiers | Hot/Cool/Cold/Archive |
| Max object size | 5TB | 5TB | 190.7TB (block blob) |
| Multipart upload | Yes (required >5GB) | Resumable uploads | Block blobs |
| Versioning | Per-bucket | Per-bucket | Soft delete + versions |
| Analytics | S3 Analytics, Athena | BigQuery integration | Synapse integration |
| CDN | CloudFront | Cloud CDN | Azure CDN |

### S3-Compatible Services

Many services implement the S3 API:

| Service | Benefit |
|---------|---------|
| MinIO | Self-hosted, Kubernetes-native |
| Cloudflare R2 | No egress fees, edge locations |
| Backblaze B2 | 1/4 S3 price, S3-compatible API |
| DigitalOcean Spaces | Simple pricing, included CDN |
| Wasabi | No egress, no API fees |
| Ceph (RGW) | Open-source, on-premise |

### Storage Classes / Tiers

| Tier | Use Case | Access | Cost Pattern |
|------|----------|--------|--------------|
| Standard/Hot | Frequent access | Immediate | Higher storage, lower retrieval |
| Infrequent Access | Monthly access | Immediate | Lower storage, retrieval fee |
| Cold/Archive | Rare access | Minutes to hours | Lowest storage, high retrieval |
| Deep Archive | Compliance, long-term | Hours | Cheapest storage, expensive retrieval |

**AWS S3 Classes:**
```
Standard → Standard-IA → One Zone-IA → Glacier Instant
                                     → Glacier Flexible
                                     → Glacier Deep Archive
```

**Lifecycle Rules:** Automatically transition objects between tiers based on age.

### When to Use Object Storage

- Static assets (images, videos, documents)
- Backup and disaster recovery
- Data lakes and analytics
- Application logs
- User-generated content
- Software distribution
- ML training datasets

---

## Block Storage

### Overview

Raw storage volumes attached to compute instances. Appears as a disk device.

```
┌──────────────┐     attach      ┌──────────────┐
│   Compute    │◄───────────────►│ Block Volume │
│   Instance   │                 │   (100GB)    │
└──────────────┘                 │   /dev/sda   │
                                 └──────────────┘
```

### Major Providers

| Provider | Service | Notes |
|----------|---------|-------|
| AWS | EBS | GP3, io2, st1, sc1 types |
| Google Cloud | Persistent Disk | Standard, SSD, Extreme |
| Azure | Managed Disks | Standard, Premium, Ultra |
| DigitalOcean | Volumes | Simple block storage |

### EBS Volume Types

| Type | Use Case | IOPS | Throughput |
|------|----------|------|------------|
| gp3 | General purpose | 3,000-16,000 | 125-1,000 MB/s |
| io2 | High performance | Up to 256,000 | 4,000 MB/s |
| st1 | Throughput (HDD) | 500 | 500 MB/s |
| sc1 | Cold (HDD) | 250 | 250 MB/s |

### Block vs Object

| Aspect | Block Storage | Object Storage |
|--------|---------------|----------------|
| Access | Mount as disk | HTTP API |
| Performance | Low latency, high IOPS | Higher latency |
| Modification | In-place updates | Replace entire object |
| Max size | TB range | Multi-TB objects |
| Use case | Databases, VMs | Files, media, backups |
| Sharing | One instance (usually) | Many readers |

### When to Use Block Storage

- Database storage (PostgreSQL, MySQL data files)
- Application requiring filesystem
- Boot volumes for VMs
- High-IOPS workloads
- Transactional systems

---

## Network File Storage

### Overview

Shared filesystem accessible by multiple clients over network protocols.

```
┌──────────────┐
│   Server 1   │──┐
└──────────────┘  │     ┌─────────────────┐
                  ├────►│   NFS/SMB       │
┌──────────────┐  │     │   File Share    │
│   Server 2   │──┤     │   /shared/data  │
└──────────────┘  │     └─────────────────┘
                  │
┌──────────────┐  │
│   Server 3   │──┘
└──────────────┘
```

### Protocols

| Protocol | Platform | Use Case |
|----------|----------|----------|
| NFS | Linux/Unix | Traditional file sharing |
| SMB/CIFS | Windows | Windows file shares |
| AFP | macOS | Legacy Apple file sharing |

### Cloud File Storage

| Provider | Service | Protocol |
|----------|---------|----------|
| AWS | EFS | NFS v4 |
| AWS | FSx for Windows | SMB |
| AWS | FSx for Lustre | Lustre (HPC) |
| Google Cloud | Filestore | NFS |
| Azure | Azure Files | SMB, NFS |

### NFS vs SMB

| Aspect | NFS | SMB |
|--------|-----|-----|
| Platform | Linux-native | Windows-native |
| Authentication | Host-based, Kerberos | User-based, AD |
| Performance | Generally faster | More overhead |
| Features | Simple | Rich (permissions, locks) |
| Cross-platform | Good | Requires Samba on Linux |

### When to Use File Storage

- Shared configuration files
- Legacy applications needing filesystem
- Home directories
- Content management systems
- Container shared volumes

---

## Distributed File Systems

### Overview

Cluster filesystems that span multiple nodes. High throughput, fault-tolerant.

```
┌─────────────────────────────────────────────────────┐
│                 Distributed File System              │
├─────────────────────────────────────────────────────┤
│  ┌─────────┐  ┌─────────┐  ┌─────────┐             │
│  │ Node 1  │  │ Node 2  │  │ Node 3  │   ...       │
│  │ 10TB    │  │ 10TB    │  │ 10TB    │             │
│  └─────────┘  └─────────┘  └─────────┘             │
│       Data replicated/striped across nodes          │
└─────────────────────────────────────────────────────┘
```

### Major Systems

| System | Best For | Notes |
|--------|----------|-------|
| HDFS | Hadoop/big data | Write-once, read-many |
| Ceph | General distributed | Object + block + file |
| GlusterFS | Scale-out NAS | Simple, no metadata server |
| Lustre | HPC | Extreme throughput |
| MinIO | S3-compatible | Kubernetes-native |
| SeaweedFS | Small files | Fast, simple |

### HDFS

Hadoop Distributed File System. Designed for large files, batch processing.

```
┌─────────────────────────────────────────┐
│            NameNode                     │
│     (metadata, namespace)               │
└─────────────────┬───────────────────────┘
                  │
    ┌─────────────┼─────────────┐
    ▼             ▼             ▼
┌────────┐   ┌────────┐   ┌────────┐
│DataNode│   │DataNode│   │DataNode│
│ Block1 │   │ Block1 │   │ Block2 │
│ Block2 │   │ Block3 │   │ Block3 │
└────────┘   └────────┘   └────────┘
(blocks replicated 3x by default)
```

**Characteristics:**
- 128MB default block size
- 3x replication (configurable)
- Write-once semantics
- Optimized for streaming reads

### Ceph

Unified storage: object (RADOS Gateway), block (RBD), file (CephFS).

| Component | Function |
|-----------|----------|
| RADOS | Core object store |
| RGW | S3/Swift-compatible gateway |
| RBD | Block devices for VMs |
| CephFS | POSIX filesystem |
| MON | Cluster monitoring |
| OSD | Object storage daemon |

### GlusterFS vs Ceph

| Aspect | GlusterFS | Ceph |
|--------|-----------|------|
| Architecture | No metadata server | Metadata servers (MON) |
| Complexity | Simpler | More complex |
| Storage types | File only | Object + Block + File |
| Scale | Good | Excellent |
| Use case | NAS replacement | Cloud infrastructure |

### When to Use Distributed FS

- Big data / Hadoop workloads
- Media streaming at scale
- Scientific computing
- Private cloud storage
- Large-scale backups

---

## Content Delivery & Edge Storage

### CDN Integration

Distribute static assets globally for low latency.

```
                    ┌─────────────┐
                    │   Origin    │
                    │  (S3, etc)  │
                    └──────┬──────┘
                           │
        ┌──────────────────┼──────────────────┐
        ▼                  ▼                  ▼
   ┌─────────┐        ┌─────────┐        ┌─────────┐
   │  Edge   │        │  Edge   │        │  Edge   │
   │  (US)   │        │  (EU)   │        │ (Asia)  │
   └─────────┘        └─────────┘        └─────────┘
        ▲                  ▲                  ▲
        │                  │                  │
      Users              Users              Users
```

### Major CDNs

| CDN | Notes |
|-----|-------|
| CloudFront | AWS-native, Lambda@Edge |
| Cloudflare | Global network, R2 integration |
| Fastly | Edge compute, real-time |
| Akamai | Enterprise, largest network |
| Bunny CDN | Cost-effective |
| Cloud CDN | Google Cloud native |
| Azure CDN | Azure native |

### Edge Storage

Store data at edge locations for ultra-low latency:

| Service | Approach |
|---------|----------|
| Cloudflare R2 | S3-compatible at edge |
| Cloudflare KV | Key-value at edge |
| Cloudflare Durable Objects | Stateful edge compute |
| AWS CloudFront + S3 | Origin + caching |

---

## Choosing Storage

### Decision Flow

```
┌─────────────────────────────────────────┐
│     What are you storing?               │
└────────────────┬────────────────────────┘
                 │
    ┌────────────┼────────────┬────────────┐
    ▼            ▼            ▼            ▼
 Database     Files/       Shared        Big Data
  files      Media        config         Analytics
    │            │            │            │
    ▼            ▼            ▼            ▼
  Block       Object        File       Distributed
 (EBS)        (S3)        (EFS)        (HDFS)
```

### Quick Reference

| Need | Consider |
|------|----------|
| Static website assets | S3 + CloudFront |
| User uploads | S3, R2, or GCS |
| Database storage | EBS, Persistent Disk |
| Shared app config | EFS, Azure Files |
| Backups | S3 Glacier, Backblaze B2 |
| Big data processing | HDFS, S3 |
| On-premise object | MinIO, Ceph |
| Cost-sensitive storage | R2, B2, Wasabi |
| Multi-cloud | S3-compatible (MinIO) |

### Cost Considerations

| Factor | Impact |
|--------|--------|
| Storage ($/GB/mo) | Primary cost driver |
| Egress ($/GB) | Can exceed storage cost |
| API requests ($/1000) | Significant at scale |
| Data retrieval (archive) | High for cold tiers |
| Replication | 2-3x storage cost |

**Cost Optimization:**
- Use lifecycle policies for tiering
- Consider egress-free providers (R2, Wasabi)
- Compress before storing
- Delete unused versions
- Use appropriate storage classes

---

## Self-Hosted Solutions

### MinIO

S3-compatible object storage for Kubernetes and on-premise.

| Aspect | Details |
|--------|---------|
| API | Full S3 compatibility |
| Deployment | Single binary, Docker, K8s |
| Features | Versioning, replication, encryption |
| Performance | Optimized for NVMe |
| Use case | Private cloud, air-gapped |

### Ceph

Complete storage platform (object + block + file).

| Aspect | Details |
|--------|---------|
| Scale | Exabyte-scale |
| Redundancy | Configurable replication/erasure |
| APIs | S3, Swift, block, POSIX |
| Complexity | High (requires expertise) |
| Use case | OpenStack, large private cloud |

### SeaweedFS

Fast, simple distributed storage for billions of files.

| Aspect | Details |
|--------|---------|
| Design | Optimized for small files |
| Master | Lightweight metadata |
| API | REST, S3, Filer (filesystem) |
| Use case | Image storage, file archives |

---

## Security Best Practices

| Practice | Implementation |
|----------|----------------|
| Encryption at rest | Enable by default (SSE-S3, SSE-KMS) |
| Encryption in transit | HTTPS only, TLS 1.2+ |
| Access control | IAM policies, bucket policies |
| Public access | Block by default |
| Versioning | Enable for critical data |
| Logging | Enable access logs |
| MFA delete | Require for destructive ops |
| Cross-region replication | For disaster recovery |

---

## Related

- [[Database Engines]]
- [[Deployment]]
- [[Security Concepts]]
- [[domains/Web Development|Web Development]]
