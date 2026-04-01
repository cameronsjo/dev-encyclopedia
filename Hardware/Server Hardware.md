---
title: Server Hardware
aliases:
  - Servers
  - Rack Servers
  - Data Center Hardware
tags:
  - hardware
  - servers
  - infrastructure
  - enterprise
type: reference
status: complete
created: "2025-12-16"
---

# Server Hardware

Enterprise computing systems designed for reliability, scalability, and remote management in data centers.

## Overview

| Category | Description | Examples |
|----------|-------------|----------|
| **Rack Servers** | Standard 19" rack mount | Dell PowerEdge, HPE ProLiant |
| **Blade Servers** | High-density, shared chassis | HPE BladeSystem, Dell M-series |
| **Tower Servers** | Standalone, SMB/SOHO | Dell PowerEdge Tower |
| **Mainframes** | Enterprise transaction systems | IBM z16 |
| **Hyperconverged** | Compute + Storage integrated | Nutanix, VMware vSAN |

## Form Factors

### Rack Units (U)

| Height | Use Case |
|--------|----------|
| **1U** | Dense compute, web servers |
| **2U** | General purpose, storage |
| **4U** | GPU servers, high storage |
| **8U+** | Specialized systems |

**Standard Rack:** 42U height, 19" width.

### Server Types

```
┌─────────────────────────────────────┐
│           Tower Server              │
│    (Standalone, quiet, SMB)         │
└─────────────────────────────────────┘

┌─────────────────────────────────────┐ ─┐
│         1U Rack Server              │  │
├─────────────────────────────────────┤  │
│         1U Rack Server              │  │ 42U
├─────────────────────────────────────┤  │ Rack
│         2U Rack Server              │  │
│                                     │  │
├─────────────────────────────────────┤  │
│         4U GPU Server               │  │
│                                     │  │
│                                     │  │
│                                     │  │
└─────────────────────────────────────┘ ─┘

┌─────────────────────────────────────┐
│      Blade Chassis (10U+)           │
│  ┌─────┐┌─────┐┌─────┐┌─────┐      │
│  │Blade││Blade││Blade││Blade│      │
│  └─────┘└─────┘└─────┘└─────┘      │
│  (Shared power, networking, mgmt)   │
└─────────────────────────────────────┘
```

## Components

### Processors

| Vendor | Product Line | Target |
|--------|--------------|--------|
| **Intel** | Xeon Scalable (Sapphire Rapids) | General, HPC |
| **AMD** | EPYC (Genoa, Bergamo) | High core count |
| **Arm** | Ampere Altra, AWS Graviton | Cloud, efficiency |
| **IBM** | POWER10 | Enterprise, AIX |

**Socket Configurations:**

| Config | Use Case |
|--------|----------|
| **1P** | Web, small workloads |
| **2P** | General enterprise |
| **4P/8P** | Large databases, in-memory |

### Memory

| Type | Speed | Use Case |
|------|-------|----------|
| **DDR4** | 3200 MT/s | Current generation |
| **DDR5** | 4800-6400 MT/s | New platforms |
| **RDIMM** | Standard | General |
| **LRDIMM** | High capacity | Large memory |
| **NVDIMM** | Persistent | Database caching |

**Capacity:** Up to 8 TB+ per server (8-socket systems).

### Storage Controllers

| Type | Description |
|------|-------------|
| **RAID Controller** | Hardware RAID, battery-backed |
| **HBA** | Pass-through, software RAID |
| **NVMe** | Direct PCIe, no controller |

### Networking

| Interface | Speed | Use Case |
|-----------|-------|----------|
| **1GbE** | 1 Gbps | Management, legacy |
| **10GbE** | 10 Gbps | Standard data center |
| **25GbE** | 25 Gbps | Modern standard |
| **100GbE** | 100 Gbps | Storage, HPC |
| **400GbE** | 400 Gbps | Spine, AI clusters |

**Technologies:**

| Technology | Purpose |
|------------|---------|
| **RDMA** | Low-latency networking |
| **RoCE** | RDMA over Converged Ethernet |
| **InfiniBand** | HPC interconnect |
| **SR-IOV** | Virtual NIC hardware |

### Power and Cooling

| Component | Description |
|-----------|-------------|
| **Redundant PSU** | N+1 or 2N power |
| **Hot-swap** | Replace without downtime |
| **240V/208V** | Data center power |
| **PDU** | Power Distribution Unit |

**Cooling:**

- Air cooling (most common)
- Rear-door heat exchangers
- Liquid cooling (GPUs, HPC)
- Immersion cooling (emerging)

## Management

### Out-of-Band Management

| Vendor | Technology |
|--------|------------|
| **Dell** | iDRAC |
| **HPE** | iLO |
| **Lenovo** | XClarity |
| **Supermicro** | BMC/IPMI |
| **Intel** | BMC (standard) |

**Features:**

- Remote power control
- Virtual console (KVM over IP)
- Virtual media (ISO mounting)
- Hardware monitoring
- Firmware updates

### IPMI (Intelligent Platform Management Interface)

**Standard for server management.**

| Feature | Description |
|---------|-------------|
| **SOL** | Serial Over LAN |
| **Sensor Data** | Temperature, voltage, fans |
| **Event Logging** | Hardware events |
| **Power Control** | On/off/reset |

### Redfish

**Modern REST API for hardware management.**

```bash
# Get system info via Redfish
curl -k https://bmc-ip/redfish/v1/Systems/1 \
  -u admin:password
```

**Advantages over IPMI:**

- RESTful (JSON)
- HTTPS security
- Scalable
- Schema-driven

## Vendors

### Enterprise Vendors

| Vendor | Products | Strengths |
|--------|----------|-----------|
| **Dell** | PowerEdge | Broad portfolio, iDRAC |
| **HPE** | ProLiant, Synergy | Enterprise support, iLO |
| **Lenovo** | ThinkSystem | Acquired IBM x86 |
| **Cisco** | UCS | Unified fabric |

### ODM/Whitebox

| Vendor | Notes |
|--------|-------|
| **Supermicro** | Customizable, fast |
| **Inspur** | Large Chinese vendor |
| **Quanta** | Cloud provider supplier |
| **Wiwynn** | Facebook/Microsoft supplier |

### Cloud Hardware

| Provider | Custom Hardware |
|----------|-----------------|
| **AWS** | Nitro, Graviton |
| **Google** | Custom TPU, servers |
| **Microsoft** | Cobalt, custom racks |
| **Meta** | Open Compute Project |

## Server Categories

### General Purpose

**Balanced compute, memory, I/O.**

| Vendor | Example |
|--------|---------|
| **Dell** | PowerEdge R760 |
| **HPE** | ProLiant DL380 Gen11 |
| **Lenovo** | ThinkSystem SR650 V3 |

### High Memory

**In-memory databases, SAP HANA.**

- 4-8 sockets
- 6-24 TB RAM
- Intel Xeon or POWER

### GPU Servers

**AI training, HPC, rendering.**

| Config | GPUs | Use Case |
|--------|------|----------|
| **4U, 4-8 GPUs** | A100/H100 | AI training |
| **DGX H100** | 8x H100 | LLM training |
| **OVX** | NVIDIA OVX | Digital twins |

### Storage Servers

**High-density disk/SSD platforms.**

- 2U with 24 NVMe bays
- 4U with 60-90 HDDs
- JBOD expansion shelves

## Reliability Features

### Hardware Redundancy

| Component | Redundancy |
|-----------|------------|
| **Power Supplies** | N+1, 2N |
| **Fans** | N+1 |
| **NICs** | Bonding/teaming |
| **Disks** | RAID, erasure coding |

### Error Correction

| Feature | Description |
|---------|-------------|
| **ECC Memory** | Single-bit correction, multi-bit detection |
| **SDDC** | Single Device Data Correction |
| **Memory Mirroring** | Full redundancy (50% capacity) |
| **Memory Sparing** | Hot spare DIMMs |

### Predictive Failure

- **SMART** for drives
- **DIMM error logging**
- **PCIe error monitoring**
- **ML-based predictions**

## Comparison

### Rack vs Blade vs HCI

| Aspect | Rack | Blade | HCI |
|--------|------|-------|-----|
| **Density** | Medium | High | Medium |
| **Flexibility** | ✅ High | ⚠️ Chassis-locked | ⚠️ Software-locked |
| **Management** | Per-server | Unified | Software-defined |
| **Networking** | Standard | Integrated | Converged |
| **Cost** | Lower entry | Higher (chassis) | Premium |
| **Use Case** | General | Very high density | Simplified ops |

### Bare Metal vs Cloud

| Aspect | Bare Metal | Cloud |
|--------|------------|-------|
| **Cost** | CapEx, lower TCO at scale | OpEx, pay-per-use |
| **Control** | ✅ Full | ⚠️ Limited |
| **Provisioning** | Hours/days | Minutes |
| **Performance** | ✅ Consistent | ⚠️ Variable (shared) |
| **Compliance** | ✅ Full control | ⚠️ Shared responsibility |

## Data Center Considerations

### Power

| Metric | Description |
|--------|-------------|
| **PUE** | Power Usage Effectiveness (1.0 = perfect) |
| **TDP** | Thermal Design Power per server |
| **kW/Rack** | Typical 10-30 kW |

### Networking

| Topology | Description |
|----------|-------------|
| **Spine-Leaf** | Standard data center |
| **Fat Tree** | HPC, AI clusters |
| **Dragonfly** | Large-scale HPC |

### Standards

| Standard | Purpose |
|----------|---------|
| **Open Compute Project (OCP)** | Open hardware designs |
| **Redfish** | Management API |
| **CXL** | Compute Express Link (memory pooling) |

## When to Choose

| Workload | Recommendation |
|----------|----------------|
| **Web/App servers** | 1U-2U rack servers |
| **Databases** | 2U with NVMe, high memory |
| **AI Training** | 4U GPU servers |
| **Virtualization** | 2U balanced servers |
| **HPC** | Blade or dense rack |
| **SMB/Branch** | Tower or 1U |
| **Edge** | Ruggedized, low power |

## Related

- [[CPU Architectures]] — Processor choices
- [[GPUs]] — Accelerator hardware
- [[Storage Systems]] — Enterprise storage
- [[Mainframes]] — IBM enterprise systems
- [[Kubernetes]] — Container orchestration on servers
