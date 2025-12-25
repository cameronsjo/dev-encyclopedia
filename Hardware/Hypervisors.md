---
title: Hypervisors
aliases:
  - Virtual Machine Monitor
  - VMM
  - Virtualization
tags:
  - hardware
  - virtualization
  - infrastructure
type: reference
status: complete
created: "2025-12-18"
---

# Hypervisors

Software layer that creates and manages virtual machines by abstracting physical hardware.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Run multiple OS instances on single hardware |
| **Types** | Type 1 (bare-metal), Type 2 (hosted) |
| **Use Cases** | Server consolidation, dev/test, cloud |
| **Key Benefit** | Hardware utilization, isolation |

## Types of Hypervisors

### Type 1 (Bare-Metal)

```
┌─────────┐ ┌─────────┐ ┌─────────┐
│  VM 1   │ │  VM 2   │ │  VM 3   │
│ (Linux) │ │(Windows)│ │ (Linux) │
└────┬────┘ └────┬────┘ └────┬────┘
     │           │           │
┌────┴───────────┴───────────┴────┐
│         HYPERVISOR              │
│    (VMware ESXi, Hyper-V)       │
└────────────────┬────────────────┘
                 │
┌────────────────┴────────────────┐
│         HARDWARE                │
│    (CPU, RAM, Storage, NIC)     │
└─────────────────────────────────┘
```

| Product | Vendor | Use Case |
|---------|--------|----------|
| **VMware ESXi** | VMware | Enterprise |
| **Microsoft Hyper-V** | Microsoft | Windows environments |
| **Xen** | Linux Foundation | Cloud (AWS original) |
| **KVM** | Open source | Linux, OpenStack |
| **Proxmox VE** | Proxmox | Open source enterprise |

### Type 2 (Hosted)

```
┌─────────┐ ┌─────────┐ ┌─────────┐
│  VM 1   │ │  VM 2   │ │  VM 3   │
└────┬────┘ └────┬────┘ └────┬────┘
     │           │           │
┌────┴───────────┴───────────┴────┐
│         HYPERVISOR              │
│  (VirtualBox, VMware Workstation)│
└────────────────┬────────────────┘
                 │
┌────────────────┴────────────────┐
│       HOST OPERATING SYSTEM     │
│     (Windows, macOS, Linux)     │
└────────────────┬────────────────┘
                 │
┌────────────────┴────────────────┐
│         HARDWARE                │
└─────────────────────────────────┘
```

| Product | Vendor | Use Case |
|---------|--------|----------|
| **VirtualBox** | Oracle | Development, testing |
| **VMware Workstation** | VMware | Professional desktop |
| **VMware Fusion** | VMware | macOS |
| **Parallels** | Parallels | macOS |
| **QEMU** | Open source | Emulation, development |

## Type 1 vs Type 2

| Aspect | Type 1 | Type 2 |
|--------|--------|--------|
| **Performance** | Near-native | Some overhead |
| **Use Case** | Production servers | Desktop/dev |
| **Hardware Access** | Direct | Through host OS |
| **Management** | Enterprise tools | Desktop apps |
| **Cost** | Often licensed | Often free |

## How Virtualization Works

### CPU Virtualization

```
┌─────────────────────────────────────────────────────┐
│                    Physical CPU                      │
│  ┌──────────────────────────────────────────────┐   │
│  │              Ring 0 (Kernel)                  │   │
│  │  ┌────────────────────────────────────────┐  │   │
│  │  │            Ring -1 (Hypervisor)         │  │   │
│  │  │         VMX root (host mode)            │  │   │
│  │  └────────────────────────────────────────┘  │   │
│  │                      │                       │   │
│  │              VM Entry/Exit                   │   │
│  │                      │                       │   │
│  │  ┌────────────────────────────────────────┐  │   │
│  │  │         VMX non-root (guest mode)       │  │   │
│  │  │           VM Ring 0 (Guest Kernel)      │  │   │
│  │  │           VM Ring 3 (Guest Apps)        │  │   │
│  │  └────────────────────────────────────────┘  │   │
│  └──────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────┘
```

### Hardware Assist

| Technology | Vendor | Function |
|------------|--------|----------|
| **VT-x** | Intel | CPU virtualization |
| **AMD-V** | AMD | CPU virtualization |
| **VT-d / AMD-Vi** | Intel/AMD | I/O virtualization (IOMMU) |
| **EPT / NPT** | Intel/AMD | Memory virtualization |
| **SR-IOV** | PCIe spec | Network virtualization |

### Memory Virtualization

```
┌─────────────────────────────────────────────────────┐
│                 Physical Memory                      │
│  ┌──────────────────────────────────────────────┐   │
│  │                                              │   │
│  │  VM1 Physical → Host Physical (EPT/NPT)      │   │
│  │                                              │   │
│  │  ┌────────┐     ┌────────┐     ┌────────┐   │   │
│  │  │ VM1    │────▶│ Shadow │────▶│Physical│   │   │
│  │  │Page Tbl│     │Page Tbl│     │ Memory │   │   │
│  │  └────────┘     └────────┘     └────────┘   │   │
│  │                                              │   │
│  └──────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────┘
```

## KVM (Kernel-based Virtual Machine)

### Architecture

```
┌─────────┐ ┌─────────┐
│  VM 1   │ │  VM 2   │  (QEMU processes)
└────┬────┘ └────┬────┘
     │           │
┌────┴───────────┴────┐
│    QEMU (Device     │
│    Emulation)       │
└──────────┬──────────┘
           │
┌──────────┴──────────┐
│   Linux Kernel      │
│  ┌────────────────┐ │
│  │   KVM Module   │ │
│  └────────────────┘ │
└──────────┬──────────┘
           │
┌──────────┴──────────┐
│      Hardware       │
│  (VT-x/AMD-V)       │
└─────────────────────┘
```

### KVM Commands

```bash
# Check if KVM is available
lscpu | grep Virtualization
cat /proc/cpuinfo | grep vmx  # Intel
cat /proc/cpuinfo | grep svm  # AMD

# Load KVM module
modprobe kvm
modprobe kvm_intel  # or kvm_amd

# Create VM with virt-install
virt-install \
  --name myvm \
  --ram 2048 \
  --vcpus 2 \
  --disk size=20 \
  --os-variant ubuntu22.04 \
  --cdrom ubuntu.iso

# List VMs
virsh list --all

# Start/stop VM
virsh start myvm
virsh shutdown myvm
virsh destroy myvm  # Force stop

# Connect to console
virsh console myvm
```

## VMware ESXi

### Architecture

```
┌─────────────────────────────────────────────────────┐
│                  vSphere Client                      │
└────────────────────────┬────────────────────────────┘
                         │
┌────────────────────────┴────────────────────────────┐
│                   vCenter Server                     │
│        (Management, vMotion, HA, DRS)               │
└────────────────────────┬────────────────────────────┘
                         │
    ┌────────────────────┼────────────────────────────┐
    │                    │                            │
┌───┴────┐          ┌────┴───┐          ┌────────────┐
│ ESXi   │          │ ESXi   │          │   ESXi     │
│ Host 1 │          │ Host 2 │          │   Host 3   │
└────────┘          └────────┘          └────────────┘
```

### Key Features

| Feature | Description |
|---------|-------------|
| **vMotion** | Live migrate VMs between hosts |
| **Storage vMotion** | Migrate VM storage without downtime |
| **HA** | Restart VMs on another host if one fails |
| **DRS** | Automatic load balancing across hosts |
| **Fault Tolerance** | Shadow VM for zero-downtime failover |
| **VSAN** | Software-defined storage |

## Hyper-V

### Architecture

```
┌─────────┐ ┌─────────┐ ┌─────────┐
│ Child   │ │ Child   │ │  Root   │
│Partition│ │Partition│ │Partition│
│ (VM 1)  │ │ (VM 2)  │ │(Windows)│
└────┬────┘ └────┬────┘ └────┬────┘
     │           │           │
┌────┴───────────┴───────────┴────┐
│         Hyper-V Hypervisor      │
└────────────────┬────────────────┘
                 │
┌────────────────┴────────────────┐
│            Hardware             │
└─────────────────────────────────┘
```

### PowerShell Management

```powershell
# Create VM
New-VM -Name "MyVM" -MemoryStartupBytes 2GB -NewVHDPath "C:\VMs\MyVM.vhdx" -NewVHDSizeBytes 40GB

# Start/Stop
Start-VM -Name "MyVM"
Stop-VM -Name "MyVM"

# List VMs
Get-VM

# Configure VM
Set-VM -Name "MyVM" -ProcessorCount 4

# Enable nested virtualization
Set-VMProcessor -VMName "MyVM" -ExposeVirtualizationExtensions $true

# Create checkpoint (snapshot)
Checkpoint-VM -Name "MyVM" -SnapshotName "BeforeUpdate"
```

## Resource Management

### CPU Allocation

| Method | Description |
|--------|-------------|
| **Reservation** | Guaranteed minimum CPU |
| **Limit** | Maximum CPU allowed |
| **Shares** | Relative priority |

### Memory Techniques

| Technique | Description |
|-----------|-------------|
| **Memory Overcommit** | Allocate more than physical |
| **Ballooning** | Reclaim unused guest memory |
| **Page Sharing** | Deduplicate identical pages |
| **Memory Compression** | Compress less-used pages |
| **Swap to Disk** | Last resort overflow |

### Storage

| Type | Description |
|------|-------------|
| **Thick Provisioned** | Full space allocated upfront |
| **Thin Provisioned** | Grows as needed |
| **Eager Zeroed** | Pre-zeroed (better performance) |
| **Lazy Zeroed** | Zeroed on first write |

## Containers vs VMs

```
Virtual Machines:              Containers:
┌─────────┐ ┌─────────┐       ┌─────────┐ ┌─────────┐
│  App A  │ │  App B  │       │  App A  │ │  App B  │
├─────────┤ ├─────────┤       ├─────────┤ ├─────────┤
│  Bins   │ │  Bins   │       │  Bins   │ │  Bins   │
├─────────┤ ├─────────┤       └────┬────┘ └────┬────┘
│Guest OS │ │Guest OS │            │           │
└────┬────┘ └────┬────┘       ┌────┴───────────┴────┐
     │           │            │   Container Runtime │
┌────┴───────────┴────┐       │     (Docker)        │
│     Hypervisor      │       └──────────┬──────────┘
└──────────┬──────────┘                  │
           │                  ┌──────────┴──────────┐
┌──────────┴──────────┐       │    Host OS Kernel   │
│      Host OS        │       └──────────┬──────────┘
└──────────┬──────────┘                  │
           │                  ┌──────────┴──────────┐
┌──────────┴──────────┐       │      Hardware       │
│      Hardware       │       └─────────────────────┘
└─────────────────────┘
```

| Aspect | VMs | Containers |
|--------|-----|------------|
| **Isolation** | Full (separate kernel) | Process-level |
| **Boot Time** | Minutes | Seconds |
| **Size** | GBs | MBs |
| **Density** | 10s per host | 100s per host |
| **Overhead** | Significant | Minimal |
| **Use Case** | Different OS, strong isolation | Microservices, CI/CD |

## Best Practices

| Practice | Rationale |
|----------|-----------|
| **Right-size VMs** | Don't over-provision |
| **Use templates** | Consistent deployments |
| **Regular snapshots** | Before changes |
| **Monitor utilization** | Identify contention |
| **Update hypervisor** | Security patches |
| **Separate workloads** | Performance isolation |
| **Plan for capacity** | Growth projections |

## Related

- [[Virtual Networking]] — VM networking
- [[CPU Architectures]] — x86, ARM virtualization
- [[Server Hardware]] — Physical infrastructure
- [[Hardware MOC]] — Hardware overview
