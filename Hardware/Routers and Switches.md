---
title: Routers and Switches
aliases:
  - Network Switches
  - Network Routers
  - L2 and L3 Devices
tags:
  - hardware
  - networking
  - infrastructure
type: reference
status: complete
created: "2025-12-18"
---

# Routers and Switches

Fundamental network devices for connecting and routing traffic.

## Overview

| Device | OSI Layer | Function |
|--------|-----------|----------|
| **Hub** | Layer 1 (Physical) | Broadcast to all ports |
| **Switch** | Layer 2 (Data Link) | Forward by MAC address |
| **Router** | Layer 3 (Network) | Forward by IP address |
| **L3 Switch** | Layer 2-3 | Switch + routing |

## Switches

### How Switches Work

```
Device A ────┐
             │
Device B ────┼───[ SWITCH ]───┬─── Device D
             │                │
Device C ────┘                └─── Device E

1. Device A sends frame to Device D
2. Switch reads destination MAC address
3. Switch checks MAC address table
4. Switch forwards ONLY to port with Device D
```

### MAC Address Table

| Port | MAC Address | VLAN |
|------|-------------|------|
| 1 | AA:BB:CC:DD:EE:01 | 10 |
| 2 | AA:BB:CC:DD:EE:02 | 10 |
| 3 | AA:BB:CC:DD:EE:03 | 20 |

```bash
# View MAC table (Cisco)
show mac address-table

# Clear MAC table
clear mac address-table dynamic
```

### Switch Types

| Type | Description | Use Case |
|------|-------------|----------|
| **Unmanaged** | Plug-and-play, no config | Home, small office |
| **Smart/Web-managed** | Basic GUI management | Small business |
| **Managed** | Full CLI/GUI, advanced features | Enterprise |
| **PoE Switch** | Power over Ethernet | IP phones, cameras, APs |
| **Stackable** | Multiple units as one logical switch | Data centers |

### Key Features

| Feature | Description |
|---------|-------------|
| **VLANs** | Virtual LANs for segmentation |
| **STP** | Spanning Tree prevents loops |
| **Port Security** | Limit MACs per port |
| **Link Aggregation** | Combine ports for bandwidth |
| **QoS** | Traffic prioritization |
| **Port Mirroring** | Copy traffic for monitoring |

### VLAN Configuration

```bash
# Cisco IOS
# Create VLAN
vlan 10
  name Engineering

# Assign port to VLAN
interface GigabitEthernet0/1
  switchport mode access
  switchport access vlan 10

# Trunk port (carries multiple VLANs)
interface GigabitEthernet0/24
  switchport mode trunk
  switchport trunk allowed vlan 10,20,30
```

### Spanning Tree Protocol (STP)

```
       [Root Bridge]
          /     \
         /       \
    [Switch A]  [Switch B]
         \       /
          \     /
       [Switch C]
           X (Blocked port - prevents loop)
```

| Protocol | Convergence | Description |
|----------|-------------|-------------|
| **STP (802.1D)** | 30-50 sec | Original standard |
| **RSTP (802.1w)** | 1-3 sec | Rapid convergence |
| **MSTP (802.1s)** | 1-3 sec | Multiple spanning trees |

## Routers

### How Routers Work

```
Network A              Network B
10.0.1.0/24           10.0.2.0/24
    │                     │
    └────[ ROUTER ]───────┘
         10.0.1.1   10.0.2.1

1. Packet arrives from Network A destined for Network B
2. Router checks routing table
3. Router decrements TTL
4. Router forwards to appropriate interface
```

### Routing Table

```bash
# View routing table
show ip route

# Example output
C    10.0.1.0/24 is directly connected, GigabitEthernet0/0
C    10.0.2.0/24 is directly connected, GigabitEthernet0/1
S    192.168.1.0/24 [1/0] via 10.0.1.254
O    172.16.0.0/16 [110/20] via 10.0.2.254
```

| Code | Meaning |
|------|---------|
| C | Directly connected |
| S | Static route |
| O | OSPF |
| B | BGP |
| R | RIP |
| * | Default route |

### Static vs Dynamic Routing

| Aspect | Static | Dynamic |
|--------|--------|---------|
| **Configuration** | Manual | Automatic |
| **Scalability** | Poor | Good |
| **Bandwidth** | None | Protocol overhead |
| **Failover** | Manual | Automatic |
| **Use Case** | Small networks, stubs | Large, complex networks |

### Routing Protocols

| Protocol | Type | Metric | Use Case |
|----------|------|--------|----------|
| **RIP** | Distance Vector | Hop count | Legacy, small networks |
| **OSPF** | Link State | Cost | Enterprise internal |
| **EIGRP** | Hybrid | Composite | Cisco networks |
| **BGP** | Path Vector | AS Path | Internet, between ISPs |
| **IS-IS** | Link State | Cost | Large ISPs |

### OSPF Configuration

```bash
# Enable OSPF
router ospf 1
  router-id 1.1.1.1
  network 10.0.1.0 0.0.0.255 area 0
  network 10.0.2.0 0.0.0.255 area 0

# Verify
show ip ospf neighbor
show ip ospf interface
```

### BGP Configuration

```bash
# Configure BGP
router bgp 65001
  neighbor 203.0.113.1 remote-as 65002
  network 198.51.100.0 mask 255.255.255.0

# Verify
show ip bgp summary
show ip bgp neighbors
```

## Layer 3 Switches

**Combines switching and routing in hardware.**

```
             ┌──────────────────┐
             │   L3 Switch      │
             │ ┌──────────────┐ │
             │ │ Routing      │ │
             │ │ (Hardware)   │ │
             │ └──────────────┘ │
             │ ┌──────────────┐ │
             │ │ Switching    │ │
             │ │ (Hardware)   │ │
             │ └──────────────┘ │
             └──────────────────┘
```

### Inter-VLAN Routing

```bash
# Create SVIs (Switch Virtual Interfaces)
interface vlan 10
  ip address 10.0.10.1 255.255.255.0
  no shutdown

interface vlan 20
  ip address 10.0.20.1 255.255.255.0
  no shutdown

# Enable IP routing
ip routing
```

## Router vs L3 Switch

| Aspect | Router | L3 Switch |
|--------|--------|-----------|
| **Routing Speed** | Software/hardware | Wire-speed (hardware) |
| **Port Density** | Low (few ports) | High (many ports) |
| **WAN Features** | Full (NAT, VPN, etc.) | Limited |
| **Cost per Port** | High | Low |
| **Use Case** | WAN edge, internet | Campus core, distribution |

## Network Address Translation (NAT)

```
Private Network          Router           Internet
192.168.1.x ───────[ NAT Router ]─────── 203.0.113.1
                    (Translation)
```

### NAT Types

| Type | Description |
|------|-------------|
| **Static NAT** | 1:1 private to public mapping |
| **Dynamic NAT** | Pool of public IPs |
| **PAT (Overload)** | Many private to one public + ports |

```bash
# PAT Configuration (Cisco)
ip nat inside source list 1 interface GigabitEthernet0/1 overload

access-list 1 permit 192.168.1.0 0.0.0.255

interface GigabitEthernet0/0
  ip nat inside

interface GigabitEthernet0/1
  ip nat outside
```

## Access Control Lists (ACLs)

```bash
# Standard ACL (source only)
access-list 10 permit 192.168.1.0 0.0.0.255
access-list 10 deny any

# Extended ACL (source, dest, protocol, port)
access-list 100 permit tcp 192.168.1.0 0.0.0.255 any eq 80
access-list 100 permit tcp 192.168.1.0 0.0.0.255 any eq 443
access-list 100 deny ip any any

# Apply to interface
interface GigabitEthernet0/0
  ip access-group 100 in
```

## Common Vendors

| Vendor | Products | Strengths |
|--------|----------|-----------|
| **Cisco** | Catalyst, Nexus, ISR | Industry standard, enterprise |
| **Juniper** | EX, QFX, MX | High performance, service providers |
| **Arista** | 7000 series | Data center, low latency |
| **HPE/Aruba** | ProCurve, CX | Cost-effective enterprise |
| **Ubiquiti** | UniFi, EdgeSwitch | SMB, prosumer |

## Troubleshooting Commands

```bash
# Cisco IOS
show interfaces status           # Port status
show interfaces trunk            # Trunk ports
show vlan brief                  # VLAN summary
show mac address-table           # MAC table
show ip route                    # Routing table
show ip ospf neighbor            # OSPF neighbors
show spanning-tree               # STP status
show cdp neighbors               # Connected Cisco devices
ping 10.0.1.1                    # Basic connectivity
traceroute 10.0.1.1              # Path to destination
```

## Best Practices

| Practice | Rationale |
|----------|-----------|
| **Document everything** | Diagrams, IP assignments |
| **Use VLANs** | Segment traffic, security |
| **Enable STP** | Prevent loops |
| **Secure management** | SSH, no telnet |
| **Disable unused ports** | Security |
| **Regular backups** | Config backup |
| **Monitor** | SNMP, syslog |

## Related

- [[Networking Fundamentals]] — OSI model, protocols
- [[Firewalls]] — Network security devices
- [[Network Topologies]] — Network designs
- [[Hardware MOC]] — Hardware overview
