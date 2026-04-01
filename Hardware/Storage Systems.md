---
title: Storage Systems
aliases:
  - Enterprise Storage
  - Storage Hardware
  - Data Storage
tags:
  - hardware
  - storage
  - enterprise
  - infrastructure
type: reference
status: complete
created: "2025-12-16"
---

# Storage Systems

Hardware and architectures for persisting data, from local disks to distributed storage arrays.

## Overview

| Category | Examples | Use Case |
|----------|----------|----------|
| **Local Storage** | SSD, HDD, NVMe | Single server |
| **Network Storage** | NAS, SAN | Shared enterprise |
| **Object Storage** | S3, MinIO, Ceph | Cloud, unstructured data |
| **Distributed** | HDFS, Ceph, GlusterFS | Big data, scale-out |

## Storage Media

### Solid State Drives (SSD)

| Interface | Speed | Use Case |
|-----------|-------|----------|
| **SATA SSD** | ~550 MB/s | Budget, capacity |
| **NVMe (PCIe 3.0)** | ~3.5 GB/s | Consumer, workstation |
| **NVMe (PCIe 4.0)** | ~7 GB/s | High performance |
| **NVMe (PCIe 5.0)** | ~14 GB/s | Data center |

**NAND Types:**

| Type | Bits/Cell | Endurance | Cost |
|------|-----------|-----------|------|
| **SLC** | 1 | Highest | Expensive |
| **MLC** | 2 | High | Moderate |
| **TLC** | 3 | Moderate | Common |
| **QLC** | 4 | Lower | Cheapest |

### Hard Disk Drives (HDD)

| Type | RPM | Use Case |
|------|-----|----------|
| **Enterprise** | 7200-15000 | Servers, NAS |
| **Desktop** | 7200 | Workstations |
| **SMR (Shingled)** | 5400-7200 | Archive, backup |
| **CMR** | Varies | Random I/O |

**Capacity:** Up to 32 TB per drive.

**Still relevant for:**

- Cold storage / archive
- Cost per TB (3x cheaper than SSD)
- Large sequential workloads

### NVMe Form Factors

| Form Factor | Description |
|-------------|-------------|
| **M.2** | Small, consumer/workstation |
| **U.2** | 2.5" enterprise |
| **U.3** | Universal Bay (SAS/SATA/NVMe) |
| **EDSFF E1.S/E3.S** | Next-gen data center |
| **Add-in Card** | PCIe slot mount |

## Storage Architectures

### Direct-Attached Storage (DAS)

```
┌─────────────┐
│   Server    │
│  ┌───────┐  │
│  │ Disk  │  │
│  │ Disk  │  │
│  │ Disk  │  │
│  └───────┘  │
└─────────────┘
```

**Characteristics:**

- Lowest latency
- No network overhead
- Limited sharing
- Simple

### Network-Attached Storage (NAS)

```
┌────────┐     ┌─────────────────┐
│ Server │────▶│                 │
└────────┘     │   NAS Device    │
┌────────┐     │  ┌───────────┐  │
│ Server │────▶│  │  Storage  │  │
└────────┘     │  └───────────┘  │
┌────────┐     │                 │
│ Laptop │────▶│    (NFS/SMB)    │
└────────┘     └─────────────────┘
```

**Protocols:**

| Protocol | Platform | Use Case |
|----------|----------|----------|
| **NFS** | Unix/Linux | File sharing |
| **SMB/CIFS** | Windows | File sharing |
| **AFP** | macOS (legacy) | Apple file sharing |

**Vendors:** Synology, QNAP, NetApp, Dell EMC Isilon.

### Storage Area Network (SAN)

```
┌────────┐     ┌──────────────┐     ┌──────────────┐
│ Server │────▶│              │     │              │
└────────┘     │   Fibre      │────▶│   Storage    │
┌────────┐     │   Channel    │     │   Array      │
│ Server │────▶│   Switch     │────▶│              │
└────────┘     │              │     │  (Block I/O) │
└────────┘     └──────────────┘     └──────────────┘
```

**Characteristics:**

- Block-level storage
- High performance
- Expensive
- Complex

**Protocols:**

| Protocol | Speed | Description |
|----------|-------|-------------|
| **Fibre Channel** | 16/32/64 Gbps | Traditional SAN |
| **iSCSI** | 1-100 Gbps | Block over Ethernet |
| **NVMe-oF** | 100+ Gbps | NVMe over fabric |
| **FCoE** | 10-100 Gbps | FC over Ethernet |

**Vendors:** Dell EMC (PowerStore), NetApp, Pure Storage, HPE.

## Enterprise Storage Arrays

### All-Flash Arrays (AFA)

| Vendor | Product | Notes |
|--------|---------|-------|
| **Pure Storage** | FlashArray | Evergreen model |
| **NetApp** | AFF | ONTAP software |
| **Dell EMC** | PowerStore | Unified block/file |
| **HPE** | Alletra** | dHCI, cloud-native |

### Hybrid Arrays

Mix of SSD and HDD with automated tiering.

| Vendor | Product |
|--------|---------|
| **Dell EMC** | PowerVault, Unity |
| **NetApp** | FAS |
| **HPE** | Primera |

### Key Features

| Feature | Description |
|---------|-------------|
| **Deduplication** | Eliminate redundant data |
| **Compression** | Reduce storage footprint |
| **Thin Provisioning** | Allocate on demand |
| **Snapshots** | Point-in-time copies |
| **Replication** | Sync/async remote copies |
| **Tiering** | Move data between SSD/HDD |
| **Encryption** | At-rest and in-flight |

## Object Storage

### Concepts

| Term | Description |
|------|-------------|
| **Object** | Data + metadata + unique ID |
| **Bucket** | Container for objects |
| **Key** | Object identifier (path-like) |
| **Metadata** | Custom attributes |

### S3 API

**De facto standard for object storage.**

```python
import boto3

s3 = boto3.client('s3')
s3.upload_file('data.csv', 'my-bucket', 'path/to/data.csv')
s3.download_file('my-bucket', 'path/to/data.csv', 'local.csv')
```

### Implementations

| Product | Type | Notes |
|---------|------|-------|
| **AWS S3** | Cloud | Original, most popular |
| **Azure Blob** | Cloud | Microsoft |
| **GCS** | Cloud | Google |
| **MinIO** | Self-hosted | S3-compatible |
| **Ceph RGW** | Self-hosted | S3/Swift compatible |
| **Wasabi** | Cloud | S3-compatible, cheaper |

### Use Cases

- **Data lakes** — Unstructured data at scale
- **Backup/archive** — Long-term retention
- **Static assets** — Web content, media
- **ML datasets** — Training data storage

## Distributed Storage

### Software-Defined Storage

| System | Type | Use Case |
|--------|------|----------|
| **Ceph** | Block/Object/File | OpenStack, Kubernetes |
| **GlusterFS** | File | Scale-out NAS |
| **MinIO** | Object | S3-compatible |
| **HDFS** | File | Hadoop/Big Data |
| **Longhorn** | Block | Kubernetes |

### Ceph Architecture

```
┌──────────────────────────────────────────┐
│              Client Access                │
│    RBD (Block)  |  RGW (S3)  |  CephFS   │
├──────────────────────────────────────────┤
│                  RADOS                    │
│  (Reliable Autonomic Distributed Object  │
│               Storage)                    │
├──────────────────────────────────────────┤
│   ┌─────┐  ┌─────┐  ┌─────┐  ┌─────┐    │
│   │ OSD │  │ OSD │  │ OSD │  │ OSD │    │
│   └─────┘  └─────┘  └─────┘  └─────┘    │
│   (Object Storage Daemons)               │
└──────────────────────────────────────────┘
```

### Kubernetes Storage

| Solution | Type |
|----------|------|
| **Rook-Ceph** | Block, Object, File |
| **Longhorn** | Block (simple) |
| **OpenEBS** | Block |
| **Portworx** | Enterprise |
| **NetApp Trident** | NetApp integration |

**CSI (Container Storage Interface):**

- Standard for K8s storage plugins
- Dynamic provisioning
- Snapshot support

## Comparison

### Block vs File vs Object

| Aspect | Block | File | Object |
|--------|-------|------|--------|
| **Access** | Raw blocks | Hierarchy (paths) | Key-value |
| **Protocol** | FC, iSCSI, NVMe-oF | NFS, SMB | S3, Swift |
| **Use Case** | Databases, VMs | File sharing | Web, backup, data lakes |
| **Scalability** | Limited | Moderate | Massive |
| **Metadata** | Minimal | File attributes | Rich, custom |

### Performance Tiers

| Tier | Media | IOPS | Latency | Use Case |
|------|-------|------|---------|----------|
| **Ultra** | NVMe/Optane | 1M+ | <100 μs | Real-time |
| **Performance** | NVMe | 100K+ | <500 μs | Databases |
| **Capacity** | TLC SSD | 10K+ | ~1 ms | General |
| **Archive** | HDD/QLC | 100s | ~10 ms | Cold data |

### Cost Comparison

| Storage Type | $/TB/Month (approx) |
|--------------|---------------------|
| **NVMe SSD (enterprise)** | $50-100 |
| **SATA SSD (enterprise)** | $20-50 |
| **HDD (enterprise)** | $5-15 |
| **S3 Standard** | $23 |
| **S3 Glacier** | $4 |
| **Tape (LTO-9)** | $1-3 |

## Data Protection

### RAID Levels

| Level | Description | Overhead | Use Case |
|-------|-------------|----------|----------|
| **RAID 0** | Stripe, no redundancy | 0% | Performance only |
| **RAID 1** | Mirror | 50% | Simple redundancy |
| **RAID 5** | Stripe + parity | 1 disk | Read-heavy |
| **RAID 6** | Stripe + 2 parity | 2 disks | Write-heavy |
| **RAID 10** | Mirror + stripe | 50% | Performance + safety |

### Erasure Coding

**Alternative to RAID for distributed storage:**

- Lower overhead than mirroring
- Configurable redundancy (e.g., 8+3)
- Used in object storage, Ceph

### Backup Strategies

| Type | Frequency | Retention |
|------|-----------|-----------|
| **Full** | Weekly/Monthly | Long-term |
| **Incremental** | Daily | Medium-term |
| **Differential** | Daily | Medium-term |
| **Continuous (CDP)** | Real-time | Short-term |

### 3-2-1 Rule

- **3** copies of data
- **2** different storage types
- **1** offsite/cloud copy

## When to Choose

| Scenario | Recommendation |
|----------|----------------|
| **Database (OLTP)** | NVMe SSD, SAN, or local |
| **File Sharing** | NAS (Synology, NetApp) |
| **Web Assets** | Object storage (S3) |
| **Big Data/Analytics** | HDFS, object storage |
| **Kubernetes** | Rook-Ceph, Longhorn |
| **Backup** | Object storage, tape |
| **Archive** | S3 Glacier, tape |
| **Mainframe** | DASD, tape (IBM) |

## Related

- [[Database Engines]] — Databases on storage
- [[Mainframes]] — Enterprise storage (DASD)
- [[Kubernetes]] — Container storage
- [[File Storage]] — Cloud file storage options
