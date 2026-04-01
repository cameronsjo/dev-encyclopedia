---
title: Mainframes
aliases:
  - IBM Mainframe
  - z/Series
  - Big Iron
  - Enterprise Servers
tags:
  - hardware
  - enterprise
  - legacy
  - mainframe
type: reference
status: complete
created: "2025-12-16"
---

# Mainframes

High-reliability enterprise computing systems designed for massive transaction volumes, continuous availability, and backward compatibility spanning decades.

## Overview

| Aspect | Details |
|--------|---------|
| **Primary Vendor** | IBM (90%+ market share) |
| **Current Platform** | IBM z16 (2022), z17 (expected) |
| **Architecture** | z/Architecture (64-bit, big-endian) |
| **Operating Systems** | z/OS, z/VM, z/VSE, Linux on Z |
| **Key Workloads** | Banking, insurance, airlines, government |
| **Reliability** | 99.999%+ uptime (5+ nines) |
| **Daily Transactions** | Trillions globally |

## Architecture

### z/Architecture

**Successor to S/360 (1964)** — Continuous binary compatibility for 60+ years.

| Feature | Description |
|---------|-------------|
| **Registers** | 16 general-purpose (64-bit) |
| **Addressing** | 64-bit virtual, up to 16 EB addressable |
| **Instruction Set** | CISC, thousands of instructions |
| **Endianness** | Big-endian |
| **I/O** | Channel-based (parallel I/O processors) |

### Hardware Components

```
┌─────────────────────────────────────────────┐
│              Central Processor              │
│  ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐   │
│  │ CP  │ │ CP  │ │ CP  │ │ zIIP│ │ zIAP│   │
│  └─────┘ └─────┘ └─────┘ └─────┘ └─────┘   │
├─────────────────────────────────────────────┤
│              Memory (up to 40 TB)           │
├─────────────────────────────────────────────┤
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  │
│  │ Channel  │  │ Channel  │  │ Channel  │  │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  │
│       │             │             │         │
│  ┌────▼─────┐  ┌────▼─────┐  ┌────▼─────┐  │
│  │  DASD    │  │  Tape    │  │ Network  │  │
│  └──────────┘  └──────────┘  └──────────┘  │
└─────────────────────────────────────────────┘
```

### Processor Types

| Type | Purpose | Billing |
|------|---------|---------|
| **CP** | General Purpose (GP) | Full cost |
| **zIIP** | Java, XML, DB2 workloads | Reduced cost |
| **zAAP** | Java workloads (deprecated) | Reduced cost |
| **IFL** | Linux-only processor | Reduced cost |
| **ICF** | Coupling Facility | Clustering |
| **SAP** | System Assist Processor | I/O management |

### I/O Subsystem

**Channel Architecture** — Dedicated I/O processors.

| Component | Function |
|-----------|----------|
| **Channel** | I/O processor handling device communication |
| **Control Unit** | Device controller (DASD, tape) |
| **FICON** | Fiber channel connection (up to 16 Gbps) |
| **ESCON** | Earlier serial channel (obsolete) |

## Operating Systems

### z/OS

**Primary mainframe OS** — Evolved from OS/360 (1966).

| Component | Purpose |
|-----------|---------|
| **JES2/JES3** | Job Entry Subsystem (batch scheduling) |
| **RACF** | Security (authentication, authorization) |
| **SMS** | Storage Management Subsystem |
| **WLM** | Workload Manager (resource allocation) |
| **VTAM** | Network communications |

**Key Features:**

- Runs [[COBOL]], [[JCL]], Assembler, PL/I
- DB2 for z/OS (relational database)
- CICS (online transaction processing)
- IMS (hierarchical database)
- MQ Series (messaging)

### z/VM

**Virtualization hypervisor** — Runs thousands of virtual machines.

- Type 1 hypervisor (bare metal)
- Each VM is isolated
- Hosts Linux guests efficiently
- CMS (Conversational Monitor System) for interactive use

### Linux on Z

**Enterprise Linux on mainframe hardware.**

| Distribution | Vendor |
|--------------|--------|
| RHEL for IBM Z | Red Hat |
| SUSE Linux Enterprise | SUSE |
| Ubuntu for IBM Z | Canonical |

**Benefits:**

- Consolidation (thousands of Linux VMs)
- Mainframe reliability
- Existing Linux skills transfer
- Open-source ecosystem

## Data Storage

### DASD (Direct Access Storage Device)

**Disk storage with mainframe-specific features.**

| Technology | Type |
|------------|------|
| **DS8900F** | High-end enterprise storage |
| **3390** | Logical disk geometry (legacy) |
| **ECKD** | Extended Count Key Data format |

### Tape Storage

**Still critical for mainframe backup and archive.**

| Technology | Capacity |
|------------|----------|
| **TS7770** | Virtual tape (disk cache) |
| **3592** | Physical tape drives |
| **LTO** | Linear Tape-Open |

### VSAM (Virtual Storage Access Method)

**Native file access method:**

| Type | Description |
|------|-------------|
| **KSDS** | Key-Sequenced Data Set |
| **ESDS** | Entry-Sequenced Data Set |
| **RRDS** | Relative Record Data Set |
| **LDS** | Linear Data Set |

## Transaction Processing

### CICS (Customer Information Control System)

**Online transaction processing (OLTP).**

| Feature | Description |
|---------|-------------|
| **Transactions/sec** | Thousands per second |
| **ACID** | Full transaction support |
| **API** | EXEC CICS commands in COBOL |
| **Regions** | Multiple isolated environments |

### IMS (Information Management System)

**Hierarchical database and transaction manager.**

- Pre-dates relational databases
- Still used for high-volume banking
- Fastest transaction throughput

### DB2 for z/OS

**Enterprise relational database.**

- SQL support
- Tight z/OS integration
- Handles massive data volumes
- Data sharing across sysplexes

## High Availability

### Parallel Sysplex

**Cluster of mainframes sharing data.**

```
┌──────────────┐     ┌──────────────┐
│  z/OS LPAR   │     │  z/OS LPAR   │
│   System A   │     │   System B   │
└──────┬───────┘     └───────┬──────┘
       │                     │
       └──────────┬──────────┘
                  │
         ┌────────▼────────┐
         │ Coupling Facility│
         │   (Shared State) │
         └─────────────────┘
```

| Feature | Benefit |
|---------|---------|
| **Data Sharing** | Multiple systems access same DB2 |
| **Workload Balancing** | Automatic distribution |
| **Rolling Upgrades** | Zero-downtime updates |
| **Disaster Recovery** | Geographic distribution |

### GDPS (Geographically Dispersed Parallel Sysplex)

**Multi-site disaster recovery.**

- Synchronous replication (near-zero data loss)
- Automatic failover
- Continuous availability

## Modernization

### API Enablement

**Exposing mainframe functions via REST/GraphQL:**

| Tool | Purpose |
|------|---------|
| **z/OS Connect** | REST API gateway |
| **CICS TS** | Web services support |
| **IBM API Connect** | API management |

### DevOps on Mainframe

| Tool | Purpose |
|------|---------|
| **Wazi** | VS Code for mainframe |
| **DBB** | Dependency-based build |
| **Git for z/OS** | Source control |
| **Jenkins/Tekton** | CI/CD pipelines |

### Containerization

- **zCX** — Run Docker containers on z/OS
- **OpenShift on Z** — Kubernetes on Linux on Z
- **Hybrid workloads** — Mix containers with COBOL

## Comparison with Distributed Systems

| Aspect | Mainframe | x86 Cluster |
|--------|-----------|-------------|
| **Reliability** | ✅ 99.999%+ | ⚠️ Requires HA design |
| **Transaction Volume** | ✅ Billions/day | ⚠️ Scaling complexity |
| **Security** | ✅ Hardware crypto, RACF | ⚠️ Software-based |
| **Virtualization** | ✅ Native, efficient | ✅ Mature (VMware, KVM) |
| **Cost Model** | ❌ High MIPS-based | ✅ Commodity pricing |
| **Talent Pool** | ❌ Shrinking | ✅ Large |
| **Agility** | ⚠️ Slower change cycles | ✅ Fast iteration |
| **Legacy Support** | ✅ 60 years of code runs | ❌ Migration needed |

## Economics

### MIPS-Based Pricing

**Software licensing tied to CPU capacity.**

| Term | Meaning |
|------|---------|
| **MIPS** | Millions of Instructions Per Second |
| **MSU** | Million Service Units |
| **MLC** | Monthly License Charge |
| **OTC** | One-Time Charge |

**Cost Optimization:**

- Shift workloads to zIIP (cheaper)
- Use Linux on Z (IFL pricing)
- Right-size LPAR capacity
- Container consolidation

### Total Cost of Ownership

| Factor | Mainframe | Distributed |
|--------|-----------|-------------|
| **Hardware** | High | Lower |
| **Software Licensing** | Very High | Varies |
| **Operations Staff** | Lower (consolidated) | Higher (many servers) |
| **Floor Space/Power** | Lower | Higher |
| **Reliability Cost** | Built-in | Additional infrastructure |

## Industry Usage

| Industry | Workloads |
|----------|-----------|
| **Banking** | Core banking, ATM networks, payments |
| **Insurance** | Policy administration, claims |
| **Airlines** | Reservations, scheduling |
| **Retail** | Inventory, POS transactions |
| **Government** | Tax processing, social security |
| **Healthcare** | Claims processing |

### By the Numbers

| Metric | Value |
|--------|-------|
| **Fortune 100** | 71% use mainframes |
| **Global Banking** | 96 of top 100 banks |
| **Daily Transactions** | 30+ billion |
| **COBOL Lines** | 220+ billion in production |

## When to Use Mainframes

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Extreme Reliability** | Hardware redundancy, proven uptime |
| **Transaction Volume** | Designed for billions of daily transactions |
| **Data Integrity** | Hardware-level protection |
| **Security** | Cryptographic processors, RACF |
| **Backward Compatibility** | Run 1960s code unmodified |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Cost** | High licensing fees (MIPS-based) |
| **Skills Gap** | Aging workforce |
| **Vendor Lock-in** | IBM dominance |
| **Perception** | "Legacy" stigma affects hiring |
| **Agility** | Slower development cycles |

### Best For

- **High-volume OLTP** — Banking, payments, reservations
- **Mission-critical** — Systems where downtime costs millions/hour
- **Regulatory compliance** — Audit trails, data integrity
- **Existing investment** — Large COBOL codebases

### Migration Considerations

**Not all workloads should migrate away:**

- Transaction-heavy batch processing
- Systems with regulatory data requirements
- Stable, rarely-changed business logic

**Candidates for migration:**

- Web-facing applications
- Analytics and reporting
- New development initiatives

## Related

- [[COBOL]] — Primary mainframe programming language
- [[JCL]] — Job Control Language for batch processing
- [[Fortran]] — Scientific computing on mainframes
- [[CPU Architectures]] — Comparison with x86, ARM
- [[Languages MOC]] — Overview of all languages
