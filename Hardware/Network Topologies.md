---
title: Network Topologies
aliases:
  - Network Design
  - Network Architecture
  - LAN Topologies
tags:
  - hardware
  - networking
  - architecture
type: reference
status: complete
created: "2025-12-18"
---

# Network Topologies

Physical and logical arrangements of network devices and connections.

## Overview

| Aspect | Physical Topology | Logical Topology |
|--------|-------------------|------------------|
| **Definition** | Actual cable/device layout | How data flows |
| **Example** | Star wiring to switch | Ethernet broadcast |
| **Visibility** | What you see | What you configure |

## Physical Topologies

### Bus Topology

```
┌──────┐    ┌──────┐    ┌──────┐    ┌──────┐
│ PC 1 │    │ PC 2 │    │ PC 3 │    │ PC 4 │
└──┬───┘    └──┬───┘    └──┬───┘    └──┬───┘
   │           │           │           │
═══╧═══════════╧═══════════╧═══════════╧═══
          Shared Backbone Cable
```

| Aspect | Details |
|--------|---------|
| **Pros** | Simple, inexpensive |
| **Cons** | Single point of failure, collisions |
| **Legacy** | 10BASE2, 10BASE5 Ethernet |
| **Status** | Obsolete for LANs |

### Star Topology

```
              ┌──────┐
              │ PC 1 │
              └──┬───┘
                 │
   ┌──────┐   ┌──▼───┐   ┌──────┐
   │ PC 2 ├───┤Switch├───┤ PC 4 │
   └──────┘   └──┬───┘   └──────┘
                 │
              ┌──▼───┐
              │ PC 3 │
              └──────┘
```

| Aspect | Details |
|--------|---------|
| **Pros** | Easy management, fault isolation |
| **Cons** | Switch is single point of failure |
| **Use** | Most common LAN topology |
| **Standard** | Modern Ethernet |

### Ring Topology

```
      ┌──────┐
      │ PC 1 │
      └──┬───┘
         │
    ┌────▼────┐
    │         │
┌───┴──┐  ┌───┴──┐
│ PC 4 │  │ PC 2 │
└───┬──┘  └───┬──┘
    │         │
    └────┬────┘
         │
      ┌──▼───┐
      │ PC 3 │
      └──────┘
```

| Aspect | Details |
|--------|---------|
| **Pros** | Predictable performance, no collisions |
| **Cons** | Single break affects all |
| **Use** | Token Ring (legacy), FDDI, SONET |
| **Variant** | Dual ring for redundancy |

### Mesh Topology

```
Full Mesh:                   Partial Mesh:
┌──────┐────────┌──────┐    ┌──────┐────────┌──────┐
│ A    │────┐   │ B    │    │ A    │        │ B    │
└──┬───┘    │   └──┬───┘    └──┬───┘        └──┬───┘
   │   \    │   /  │           │              │
   │    \   │  /   │           │              │
   │     \  │ /    │           └──────┬───────┘
   │      \ │/     │                  │
┌──┴───┐───────┌───┴──┐    ┌──────┐───┴───┌──────┐
│ C    │────────│ D    │    │ C    │───────│ D    │
└──────┘        └──────┘    └──────┘       └──────┘
```

| Aspect | Full Mesh | Partial Mesh |
|--------|-----------|--------------|
| **Redundancy** | Maximum | Selective |
| **Cost** | Very high | Moderate |
| **Use** | Core networks | WAN, backbone |
| **Connections** | n(n-1)/2 | As needed |

### Hybrid Topology

```
                    Core (Mesh)
             ┌─────────────────────┐
             │                     │
        ┌────┴────┐          ┌─────┴───┐
        │Router A │──────────│Router B │
        └────┬────┘          └────┬────┘
             │                    │
    ┌────────┴────────┐          │
    │                 │          │
┌───┴───┐        ┌────┴──┐  ┌────┴──┐
│Switch1│        │Switch2│  │Switch3│
└───┬───┘        └───┬───┘  └───┬───┘
    │                │          │
  Star             Star       Star
 Network          Network    Network
```

## Enterprise Network Design

### Three-Tier Architecture

```
                        ┌─────────────────────┐
                        │       CORE          │
                        │  (High-speed mesh)  │
                        │    Core Switches    │
                        └──────────┬──────────┘
                                   │
              ┌────────────────────┼────────────────────┐
              │                    │                    │
     ┌────────▼────────┐  ┌───────▼────────┐  ┌───────▼────────┐
     │  DISTRIBUTION   │  │  DISTRIBUTION  │  │  DISTRIBUTION  │
     │   (Routing,     │  │   (Routing,    │  │   (Routing,    │
     │    VLANs)       │  │    VLANs)      │  │    VLANs)      │
     └────────┬────────┘  └───────┬────────┘  └───────┬────────┘
              │                   │                   │
     ┌────────┴────────┐          │          ┌───────┴────────┐
     │                 │          │          │                │
┌────▼────┐      ┌─────▼──┐ ┌─────▼──┐ ┌─────▼──┐       ┌─────▼──┐
│ ACCESS  │      │ ACCESS │ │ ACCESS │ │ ACCESS │       │ ACCESS │
│ Switch  │      │ Switch │ │ Switch │ │ Switch │       │ Switch │
└────┬────┘      └────┬───┘ └────┬───┘ └────┬───┘       └────┬───┘
     │                │          │          │                │
   Users            Users      Users      Users            Users
```

| Tier | Function | Equipment |
|------|----------|-----------|
| **Core** | High-speed backbone | L3 switches, routers |
| **Distribution** | Routing, filtering, VLANs | L3 switches |
| **Access** | End-user connectivity | L2 switches |

### Collapsed Core (Two-Tier)

```
                    ┌──────────────────────────┐
                    │   CORE + DISTRIBUTION    │
                    │    (Combined Layer)      │
                    │     L3 Switches          │
                    └────────────┬─────────────┘
                                 │
         ┌───────────────────────┼───────────────────────┐
         │                       │                       │
    ┌────▼────┐            ┌─────▼────┐            ┌─────▼────┐
    │ ACCESS  │            │ ACCESS   │            │ ACCESS   │
    │ Switch  │            │ Switch   │            │ Switch   │
    └────┬────┘            └────┬─────┘            └────┬─────┘
         │                      │                       │
       Users                  Users                   Users
```

| Use Case | Benefits |
|----------|----------|
| Small-medium business | Cost savings, simpler |
| Single building | Fewer devices |
| < 500 users | Adequate performance |

### Spine-Leaf Architecture

```
              SPINE LAYER (East-West Traffic)
    ┌─────────┐    ┌─────────┐    ┌─────────┐
    │ Spine 1 │    │ Spine 2 │    │ Spine 3 │
    └────┬────┘    └────┬────┘    └────┬────┘
         │              │              │
    ┌────┼──────────────┼──────────────┼────┐
    │    │              │              │    │
    │ ┌──┴──────────────┴──────────────┴──┐ │
    │ │         Full Mesh Connections      │ │
    │ └──┬──────────────┬──────────────┬──┘ │
    │    │              │              │    │
    └────┼──────────────┼──────────────┼────┘
         │              │              │
    ┌────▼────┐    ┌────▼────┐    ┌────▼────┐
    │ Leaf 1  │    │ Leaf 2  │    │ Leaf 3  │
    └────┬────┘    └────┬────┘    └────┬────┘
         │              │              │
      Servers        Servers        Servers

Every leaf connects to every spine = predictable latency
```

| Aspect | Details |
|--------|---------|
| **Use** | Data centers, cloud |
| **Benefits** | Predictable latency, scalable |
| **Routing** | BGP or OSPF between layers |
| **Protocols** | VXLAN, EVPN for overlay |

## Data Center Design

### Pod Design

```
┌───────────────────────────────────────────────────────┐
│                        POD 1                           │
│  ┌─────────┐    ┌─────────┐                           │
│  │ ToR Sw  │    │ ToR Sw  │   Top of Rack             │
│  └────┬────┘    └────┬────┘                           │
│       │              │                                 │
│  ┌────▼──────────────▼────┐                           │
│  │    Aggregation Sw      │                           │
│  └────────────┬───────────┘                           │
│               │                                        │
│  ┌────────────┴────────────┐                          │
│  │        Racks            │                          │
│  │  [Srv][Srv][Srv][Srv]   │                          │
│  └─────────────────────────┘                          │
└───────────────────────────────────────────────────────┘
```

### Clos Network

```
          Tier 3 (Spine)
    ┌───┐    ┌───┐    ┌───┐
    │ S │    │ S │    │ S │
    └─┬─┘    └─┬─┘    └─┬─┘
      │        │        │
      └────────┼────────┘
               │
    ┌──────────┼──────────┐
    │          │          │
  ┌─┴─┐      ┌─┴─┐      ┌─┴─┐
  │ A │      │ A │      │ A │  Tier 2 (Aggregation)
  └─┬─┘      └─┬─┘      └─┬─┘
    │          │          │
    └──────────┼──────────┘
               │
    ┌──────────┼──────────┐
    │          │          │
  ┌─┴─┐      ┌─┴─┐      ┌─┴─┐
  │ L │      │ L │      │ L │  Tier 1 (Leaf/ToR)
  └─┬─┘      └─┬─┘      └─┬─┘
    │          │          │
 Servers    Servers    Servers
```

## WAN Topologies

### Hub and Spoke

```
                    ┌──────────┐
              ┌─────┤   HQ     ├─────┐
              │     │  (Hub)   │     │
              │     └────┬─────┘     │
              │          │           │
         ┌────▼───┐ ┌────▼───┐ ┌─────▼──┐
         │Branch 1│ │Branch 2│ │Branch 3│
         │(Spoke) │ │(Spoke) │ │(Spoke) │
         └────────┘ └────────┘ └────────┘
```

| Aspect | Details |
|--------|---------|
| **Pros** | Centralized control, cost-effective |
| **Cons** | Hub is single point of failure |
| **Use** | Enterprise WAN, retail |

### Full/Partial Mesh WAN

```
           ┌────────────┐
      ┌────┤    HQ      ├────┐
      │    └──────┬─────┘    │
      │           │          │
      │           │          │
┌─────▼────┐      │    ┌─────▼────┐
│ Region A │◄─────┼────► Region B │
└─────┬────┘      │    └─────┬────┘
      │           │          │
      │      ┌────▼───┐      │
      └──────► Region C◄──────┘
             └────────┘
```

## Redundancy Patterns

### Dual-Homed

```
           ┌─────────────────┐
           │    Internet     │
           └────┬───────┬────┘
                │       │
           ┌────▼─┐ ┌───▼────┐
           │ ISP1 │ │ ISP2   │
           └────┬─┘ └───┬────┘
                │       │
           ┌────▼───────▼────┐
           │    Router       │
           │  (Multi-homing) │
           └─────────────────┘
```

### High Availability Pair

```
        ┌─────────────┐
        │   Uplink    │
        └──────┬──────┘
               │
    ┌──────────┴──────────┐
    │                     │
┌───▼───┐   VRRP/    ┌────▼──┐
│ FW 1  │   HSRP     │ FW 2  │
│Active │◄──────────►│Standby│
└───┬───┘            └───┬───┘
    │                    │
    └─────────┬──────────┘
              │
        ┌─────▼─────┐
        │  Network  │
        └───────────┘
```

## Network Segmentation

### VLAN Segmentation

```
┌─────────────────────────────────────────────┐
│              Physical Switch                 │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐     │
│  │ VLAN 10 │  │ VLAN 20 │  │ VLAN 30 │     │
│  │  Users  │  │ Servers │  │  Guest  │     │
│  └─────────┘  └─────────┘  └─────────┘     │
└─────────────────────────────────────────────┘

• Logical separation on shared hardware
• Inter-VLAN routing required for communication
• Broadcast domain isolation
```

### Micro-segmentation

```
┌───────────────────────────────────────────────────┐
│                    Data Center                     │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐        │
│  │ Web Tier │  │ App Tier │  │ DB Tier  │        │
│  │    ↓     │→→│    ↓     │→→│    ↓     │        │
│  │ Firewall │  │ Firewall │  │ Firewall │        │
│  └──────────┘  └──────────┘  └──────────┘        │
│  Fine-grained east-west security controls         │
└───────────────────────────────────────────────────┘
```

## Design Considerations

| Factor | Considerations |
|--------|----------------|
| **Scale** | Number of devices, growth |
| **Redundancy** | Fault tolerance requirements |
| **Performance** | Bandwidth, latency needs |
| **Security** | Segmentation, filtering |
| **Management** | Complexity, automation |
| **Cost** | CapEx, OpEx |
| **Standards** | Compliance requirements |

## Common Protocols

| Protocol | Purpose |
|----------|---------|
| **STP/RSTP** | Loop prevention |
| **LACP** | Link aggregation |
| **VRRP/HSRP** | Gateway redundancy |
| **BGP** | WAN/Internet routing |
| **OSPF** | Internal routing |
| **VXLAN** | Overlay networking |
| **EVPN** | Multi-tenant networking |

## Related

- [[Routers and Switches]] — Network devices
- [[Firewalls]] — Security placement
- [[Networking Fundamentals]] — Protocols
- [[Hardware MOC]] — Hardware overview
