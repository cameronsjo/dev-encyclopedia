---
title: Virtual Networking
aliases:
  - Software-Defined Networking
  - SDN
  - Network Virtualization
tags:
  - hardware
  - networking
  - virtualization
type: reference
status: complete
created: "2025-12-18"
---

# Virtual Networking

Network infrastructure implemented in software, abstracting physical network hardware.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Flexible, programmable networking |
| **Abstraction** | Logical networks over physical |
| **Control** | Centralized, software-defined |
| **Use Cases** | Cloud, data center, multi-tenant |

## Virtual Network Components

### Virtual Switch (vSwitch)

```
┌───────────────────────────────────────────────────────────┐
│                     Physical Host                          │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐                    │
│  │  VM 1   │  │  VM 2   │  │  VM 3   │                    │
│  │ vNIC    │  │ vNIC    │  │ vNIC    │                    │
│  └────┬────┘  └────┬────┘  └────┬────┘                    │
│       │            │            │                          │
│  ┌────┴────────────┴────────────┴────┐                    │
│  │         Virtual Switch             │                    │
│  │   (OVS, VMware vSwitch, Hyper-V)   │                    │
│  └──────────────────┬─────────────────┘                    │
│                     │                                      │
│  ┌──────────────────┴─────────────────┐                    │
│  │        Physical NIC (pNIC)         │                    │
│  └──────────────────┬─────────────────┘                    │
└─────────────────────┼─────────────────────────────────────┘
                      │
              Physical Network
```

### Virtual NIC (vNIC)

| Type | Description |
|------|-------------|
| **Emulated** | Software emulates real NIC (e1000) |
| **Paravirtual** | Guest-aware drivers (virtio, vmxnet3) |
| **SR-IOV** | Direct hardware access (VF) |
| **DPDK** | User-space packet processing |

## Open vSwitch (OVS)

### Architecture

```
┌────────────────────────────────────────────────────────┐
│                   User Space                            │
│  ┌──────────────────────────────────────────────────┐  │
│  │                   ovs-vswitchd                    │  │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────────┐   │  │
│  │  │ OpenFlow │  │ OVSDB    │  │ Port/VLAN    │   │  │
│  │  │ Protocol │  │ Protocol │  │ Management   │   │  │
│  │  └──────────┘  └──────────┘  └──────────────┘   │  │
│  └──────────────────────────────────────────────────┘  │
│                          │                              │
│  ┌──────────────────────────────────────────────────┐  │
│  │              ovsdb-server (config DB)             │  │
│  └──────────────────────────────────────────────────┘  │
└────────────────────────────┬───────────────────────────┘
                             │
┌────────────────────────────┴───────────────────────────┐
│                   Kernel Space                          │
│  ┌──────────────────────────────────────────────────┐  │
│  │            OVS Kernel Module (datapath)           │  │
│  │        Fast path packet forwarding               │  │
│  └──────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

### OVS Commands

```bash
# Create bridge
ovs-vsctl add-br br0

# Add port
ovs-vsctl add-port br0 eth0
ovs-vsctl add-port br0 veth0

# Set VLAN
ovs-vsctl set port veth0 tag=100

# Create trunk port
ovs-vsctl set port eth0 trunks=100,200,300

# Show configuration
ovs-vsctl show

# Flow management
ovs-ofctl dump-flows br0
ovs-ofctl add-flow br0 "in_port=1,actions=output:2"

# Port statistics
ovs-ofctl dump-ports br0
```

## Network Modes

### Bridge Mode

```
┌─────────────────────────────────────────┐
│               Physical Host              │
│  ┌────────┐  ┌────────┐                 │
│  │  VM 1  │  │  VM 2  │                 │
│  │10.0.0.2│  │10.0.0.3│                 │
│  └───┬────┘  └───┬────┘                 │
│      │           │                       │
│  ┌───┴───────────┴───┐                  │
│  │    Bridge (br0)    │                  │
│  └─────────┬─────────┘                  │
│            │                             │
│  ┌─────────┴─────────┐                  │
│  │  Physical NIC     │                  │
│  │    10.0.0.1       │                  │
│  └─────────┬─────────┘                  │
└────────────┼────────────────────────────┘
             │
     Physical Network (10.0.0.0/24)

• VMs appear as separate hosts on physical network
• VMs get IPs from same range as host
```

### NAT Mode

```
┌─────────────────────────────────────────┐
│               Physical Host              │
│  ┌────────┐  ┌────────┐                 │
│  │  VM 1  │  │  VM 2  │                 │
│  │192.168.│  │192.168.│                 │
│  │122.10  │  │122.11  │                 │
│  └───┬────┘  └───┬────┘                 │
│      │           │                       │
│  ┌───┴───────────┴───┐                  │
│  │  Virtual Network   │                  │
│  │  (192.168.122.0)   │                  │
│  └─────────┬─────────┘                  │
│            │ NAT                         │
│  ┌─────────┴─────────┐                  │
│  │  Physical NIC     │                  │
│  │    10.0.0.1       │                  │
│  └─────────┬─────────┘                  │
└────────────┼────────────────────────────┘
             │
     Physical Network

• VMs share host's IP for outbound
• Port forwarding for inbound
• VMs isolated from physical network
```

### Host-Only Mode

```
┌─────────────────────────────────────────┐
│               Physical Host              │
│  ┌────────┐  ┌────────┐  ┌─────────┐   │
│  │  VM 1  │  │  VM 2  │  │  Host   │   │
│  │192.168.│  │192.168.│  │ 192.168.│   │
│  │56.10   │  │56.11   │  │ 56.1    │   │
│  └───┬────┘  └───┬────┘  └────┬────┘   │
│      │           │            │         │
│  ┌───┴───────────┴────────────┴───┐    │
│  │       Host-Only Network         │    │
│  │        (192.168.56.0/24)        │    │
│  └────────────────────────────────┘    │
│                                         │
│  ┌─────────────────────────────────┐   │
│  │  Physical NIC (no connection)   │   │
│  └─────────────────────────────────┘   │
└─────────────────────────────────────────┘

• VMs can talk to each other and host
• No external network access
• Good for isolated testing
```

## Overlay Networks

### VXLAN

```
┌─────────────────────────────────────────────────────────────┐
│                      VXLAN Overlay                           │
│    ┌─────────────────────────────────────────────────────┐  │
│    │              Virtual Network (VNI 1000)              │  │
│    │   ┌──────┐              ┌──────┐                    │  │
│    │   │ VM A │              │ VM B │                    │  │
│    │   │10.0.1│              │10.0.1│                    │  │
│    │   │ .10  │              │ .11  │                    │  │
│    │   └──┬───┘              └──┬───┘                    │  │
│    └──────┼──────────────────────┼───────────────────────┘  │
│           │                      │                           │
│    ┌──────┴────┐          ┌──────┴────┐                     │
│    │   VTEP    │          │   VTEP    │                     │
│    │ 192.168.  │          │ 192.168.  │                     │
│    │   1.10    │          │   1.20    │                     │
│    └──────┬────┘          └──────┬────┘                     │
│           │                      │                           │
└───────────┼──────────────────────┼───────────────────────────┘
            │                      │
     ┌──────┴──────────────────────┴──────┐
     │         Physical Network            │
     │       (Underlay 192.168.1.0/24)     │
     └─────────────────────────────────────┘

VXLAN encapsulates L2 frames in UDP (port 4789)
VNI (VXLAN Network Identifier) = 24 bits = 16M networks
```

### VXLAN Packet Structure

```
┌─────────────────────────────────────────────────────┐
│ Outer Ethernet │ Outer IP │ UDP │ VXLAN │ Inner    │
│    Header      │  Header  │ Hdr │ Header│ Ethernet │
│                │          │     │       │ Frame    │
└─────────────────────────────────────────────────────┘
                 │                │
                 │                └─ VNI (24 bits)
                 └─ VTEP IP addresses
```

### GRE (Generic Routing Encapsulation)

```bash
# Linux GRE tunnel
ip tunnel add gre1 mode gre remote 10.0.0.2 local 10.0.0.1
ip link set gre1 up
ip addr add 192.168.100.1/24 dev gre1
```

## Software-Defined Networking (SDN)

### SDN Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                         │
│    ┌─────────┐  ┌─────────┐  ┌─────────┐                   │
│    │ Network │  │ Security│  │ Monitor │                   │
│    │  Apps   │  │  Apps   │  │  Apps   │                   │
│    └────┬────┘  └────┬────┘  └────┬────┘                   │
└─────────┼────────────┼────────────┼─────────────────────────┘
          │            │            │
          └────────────┼────────────┘
                       │ Northbound API (REST)
┌──────────────────────┼──────────────────────────────────────┐
│                      │ Control Layer                         │
│    ┌─────────────────┴─────────────────┐                    │
│    │         SDN Controller             │                    │
│    │    (OpenDaylight, ONOS, Floodlight)│                    │
│    └─────────────────┬─────────────────┘                    │
│                      │ Southbound API (OpenFlow)             │
└──────────────────────┼──────────────────────────────────────┘
                       │
┌──────────────────────┼──────────────────────────────────────┐
│                      │ Data Layer                            │
│    ┌─────────┐  ┌────┴────┐  ┌─────────┐                   │
│    │ Switch  │  │ Switch  │  │ Switch  │                    │
│    │   A     │  │   B     │  │   C     │                    │
│    └─────────┘  └─────────┘  └─────────┘                    │
└─────────────────────────────────────────────────────────────┘
```

### OpenFlow

```
# OpenFlow rules (conceptual)
Match: src_ip=10.0.0.0/24, dst_port=80
Action: forward to port 2

Match: src_ip=192.168.1.0/24
Action: drop

Match: any
Action: send to controller
```

## Cloud Virtual Networking

### AWS VPC

```
┌─────────────────────────────────────────────────────────────┐
│                        VPC (10.0.0.0/16)                     │
│  ┌─────────────────────────┐  ┌────────────────────────┐   │
│  │  Public Subnet          │  │  Private Subnet        │   │
│  │  (10.0.1.0/24)          │  │  (10.0.2.0/24)         │   │
│  │  ┌──────┐  ┌──────┐     │  │  ┌──────┐  ┌──────┐   │   │
│  │  │ EC2  │  │ EC2  │     │  │  │ EC2  │  │ RDS  │   │   │
│  │  └──────┘  └──────┘     │  │  └──────┘  └──────┘   │   │
│  │        │                │  │        │               │   │
│  │  ┌─────┴─────┐          │  │  ┌─────┴─────┐        │   │
│  │  │ Internet  │          │  │  │ NAT       │        │   │
│  │  │ Gateway   │          │  │  │ Gateway   │        │   │
│  │  └─────┬─────┘          │  │  └─────┬─────┘        │   │
│  └────────┼────────────────┘  └────────┼──────────────┘   │
│           │                            │                   │
│  ┌────────┴────────────────────────────┴────────────┐     │
│  │              Route Tables                         │     │
│  └────────────────────────┬─────────────────────────┘     │
└───────────────────────────┼─────────────────────────────────┘
                            │
                       Internet
```

### Key Cloud Networking Concepts

| Concept | AWS | Azure | GCP |
|---------|-----|-------|-----|
| **Virtual Network** | VPC | VNet | VPC |
| **Subnets** | Subnet | Subnet | Subnet |
| **Firewall** | Security Groups, NACLs | NSG | Firewall Rules |
| **Load Balancer** | ALB/NLB/ELB | Load Balancer | Cloud Load Balancing |
| **NAT** | NAT Gateway | NAT Gateway | Cloud NAT |
| **VPN** | VPN Gateway | VPN Gateway | Cloud VPN |
| **Peering** | VPC Peering | VNet Peering | VPC Peering |

## Network Function Virtualization (NFV)

```
Traditional:                     NFV:
┌─────────┐                     ┌─────────────────────┐
│Firewall │                     │    NFV Platform     │
│Appliance│                     │  ┌───────────────┐  │
└─────────┘                     │  │ Virtual FW    │  │
┌─────────┐                     │  ├───────────────┤  │
│  Load   │      ────────▶      │  │ Virtual LB    │  │
│Balancer │                     │  ├───────────────┤  │
└─────────┘                     │  │ Virtual Router│  │
┌─────────┐                     │  └───────────────┘  │
│ Router  │                     │  (Standard servers) │
│Appliance│                     └─────────────────────┘
└─────────┘
```

| VNF Type | Function |
|----------|----------|
| vFirewall | Security filtering |
| vRouter | Routing |
| vLB | Load balancing |
| vWAN Optimizer | WAN acceleration |
| vIDS/IPS | Intrusion detection |

## Container Networking

### CNI (Container Network Interface)

| CNI Plugin | Description |
|------------|-------------|
| **Calico** | L3 networking, network policies |
| **Flannel** | Simple overlay (VXLAN) |
| **Weave** | Mesh networking |
| **Cilium** | eBPF-based, security |
| **AWS VPC CNI** | Native AWS VPC |

### Kubernetes Networking

```
┌───────────────────────────────────────────────────────────┐
│                     Kubernetes Cluster                     │
│  ┌─────────────────────────────────────────────────────┐  │
│  │                    Pod Network                       │  │
│  │  ┌────────┐  ┌────────┐  ┌────────┐  ┌────────┐    │  │
│  │  │ Pod A  │  │ Pod B  │  │ Pod C  │  │ Pod D  │    │  │
│  │  │10.244. │  │10.244. │  │10.244. │  │10.244. │    │  │
│  │  │ 1.10   │  │ 1.11   │  │ 2.10   │  │ 2.11   │    │  │
│  │  └────┬───┘  └────┬───┘  └────┬───┘  └────┬───┘    │  │
│  │       │           │           │           │         │  │
│  │  ┌────┴───────────┴───┐  ┌────┴───────────┴───┐    │  │
│  │  │     Node 1         │  │     Node 2         │    │  │
│  │  │ (10.244.1.0/24)    │  │ (10.244.2.0/24)    │    │  │
│  │  └────────────────────┘  └────────────────────┘    │  │
│  └─────────────────────────────────────────────────────┘  │
│                            │                               │
│  ┌─────────────────────────┴───────────────────────────┐  │
│  │              Service Network (ClusterIP)             │  │
│  │                    10.96.0.0/12                      │  │
│  └─────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────┘
```

## Performance Optimization

| Technique | Description |
|-----------|-------------|
| **SR-IOV** | Direct NIC access bypassing hypervisor |
| **DPDK** | User-space packet processing |
| **virtio** | Paravirtualized drivers |
| **Jumbo Frames** | Larger MTU (9000) for efficiency |
| **RSS** | Receive Side Scaling |
| **TSO/LRO** | TCP Segmentation Offload |

## Related

- [[Hypervisors]] — VM management
- [[Routers and Switches]] — Physical networking
- [[Network Topologies]] — Network design
- [[Hardware MOC]] — Hardware overview
