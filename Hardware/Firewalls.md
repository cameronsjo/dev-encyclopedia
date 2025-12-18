---
title: Firewalls
aliases:
  - Network Firewall
  - Firewall Security
tags:
  - hardware
  - networking
  - security
type: reference
status: complete
created: "2025-12-18"
---

# Firewalls

Network security devices that monitor and control traffic based on security rules.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Control network traffic flow |
| **Function** | Allow/deny based on rules |
| **Placement** | Network perimeter, segments |
| **Types** | Hardware, software, cloud |

## Firewall Types

### By Implementation

| Type | Description | Use Case |
|------|-------------|----------|
| **Hardware Firewall** | Dedicated appliance | Enterprise perimeter |
| **Software Firewall** | OS-based (iptables, Windows Firewall) | Host protection |
| **Virtual Firewall** | VM-based appliance | Cloud, virtualization |
| **Cloud Firewall** | Provider-managed (AWS SG, Azure NSG) | Cloud workloads |

### By Generation

| Generation | Technology | Capabilities |
|------------|------------|--------------|
| **Packet Filter** | L3-L4 inspection | IP, port filtering |
| **Stateful** | Connection tracking | Session awareness |
| **Application (WAF)** | L7 inspection | HTTP, SQL injection |
| **NGFW** | Deep inspection | App awareness, IPS, SSL |

## How Firewalls Work

### Packet Filtering

```
┌─────────────────────────────────────────────────────┐
│ Packet arrives                                       │
│ ┌─────────────────────────────────────────────────┐ │
│ │ Source IP: 192.168.1.100                        │ │
│ │ Dest IP: 10.0.0.50                              │ │
│ │ Protocol: TCP                                    │ │
│ │ Source Port: 52431                               │ │
│ │ Dest Port: 443                                   │ │
│ └─────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────┘
                          │
                          ▼
              ┌──────────────────────┐
              │ Check against rules   │
              │                       │
              │ Rule 1: ALLOW        │
              │   TCP to port 443    │
              │                       │
              │ Rule 2: DENY         │
              │   TCP to port 23     │
              │                       │
              │ Default: DENY ALL    │
              └──────────────────────┘
                          │
                          ▼
                   ┌────────────┐
                   │  ALLOWED   │
                   └────────────┘
```

### Stateful Inspection

```
┌───────────────────────────────────────────────────────┐
│                 State Table                            │
├───────────────────────────────────────────────────────┤
│ Src IP        │ Dst IP      │ Src Port │ Dst Port │ State      │
│ 192.168.1.100 │ 10.0.0.50   │ 52431    │ 443      │ ESTABLISHED│
│ 192.168.1.101 │ 8.8.8.8     │ 43211    │ 53       │ NEW        │
└───────────────────────────────────────────────────────┘

• Tracks connection state (NEW, ESTABLISHED, RELATED)
• Return traffic automatically allowed for established connections
• More secure than stateless packet filtering
```

## Rule Structure

### Basic Components

| Component | Description | Example |
|-----------|-------------|---------|
| **Action** | Allow or deny | ACCEPT, DROP, REJECT |
| **Source** | Origin IP/network | 192.168.1.0/24 |
| **Destination** | Target IP/network | 10.0.0.0/8 |
| **Protocol** | L4 protocol | TCP, UDP, ICMP |
| **Port** | Service port | 80, 443, 22 |
| **Direction** | Inbound/outbound | IN, OUT |

### Rule Processing

```
┌─────────────────────────────────────┐
│ Rules processed TOP to BOTTOM       │
├─────────────────────────────────────┤
│ Rule 1: Allow HTTPS (443)          │ ← First match wins
│ Rule 2: Allow SSH from admin       │
│ Rule 3: Allow DNS                  │
│ Rule 4: Deny all from blacklist    │
│ ...                                 │
│ Default: DENY ALL                   │ ← Implicit deny
└─────────────────────────────────────┘
```

## Linux iptables

### Basic Commands

```bash
# View rules
iptables -L -v -n
iptables -L INPUT -v -n --line-numbers

# Allow incoming SSH
iptables -A INPUT -p tcp --dport 22 -j ACCEPT

# Allow established connections
iptables -A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT

# Allow from specific IP
iptables -A INPUT -s 192.168.1.100 -j ACCEPT

# Block IP
iptables -A INPUT -s 10.0.0.50 -j DROP

# Allow HTTP/HTTPS
iptables -A INPUT -p tcp --dport 80 -j ACCEPT
iptables -A INPUT -p tcp --dport 443 -j ACCEPT

# Default deny
iptables -P INPUT DROP
iptables -P FORWARD DROP
iptables -P OUTPUT ACCEPT

# Save rules (Debian/Ubuntu)
iptables-save > /etc/iptables/rules.v4

# Delete rule by number
iptables -D INPUT 3
```

### iptables Chains

| Chain | Purpose |
|-------|---------|
| **INPUT** | Packets destined for local system |
| **OUTPUT** | Packets originating from local system |
| **FORWARD** | Packets routed through system |
| **PREROUTING** | Before routing decision (NAT) |
| **POSTROUTING** | After routing decision (NAT) |

## nftables (Modern Linux)

```bash
# Basic nftables rules
nft add table inet filter
nft add chain inet filter input { type filter hook input priority 0 \; policy drop \; }

# Allow SSH
nft add rule inet filter input tcp dport 22 accept

# Allow established
nft add rule inet filter input ct state established,related accept

# View rules
nft list ruleset
```

## Windows Firewall

### PowerShell Commands

```powershell
# View rules
Get-NetFirewallRule | Format-Table Name, Enabled, Direction, Action

# Create inbound rule
New-NetFirewallRule -DisplayName "Allow SSH" `
  -Direction Inbound -Protocol TCP -LocalPort 22 -Action Allow

# Block IP
New-NetFirewallRule -DisplayName "Block Bad IP" `
  -Direction Inbound -RemoteAddress 10.0.0.50 -Action Block

# Enable/disable rule
Enable-NetFirewallRule -DisplayName "Allow SSH"
Disable-NetFirewallRule -DisplayName "Allow SSH"

# Remove rule
Remove-NetFirewallRule -DisplayName "Allow SSH"
```

## Cloud Security Groups

### AWS Security Groups

```hcl
# Terraform example
resource "aws_security_group" "web" {
  name        = "web-sg"
  description = "Web server security group"
  vpc_id      = aws_vpc.main.id

  # Inbound rules
  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["10.0.0.0/8"]  # Internal only
  }

  # Outbound rules
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}
```

### Key Differences

| Feature | Security Group | NACL |
|---------|----------------|------|
| **Level** | Instance | Subnet |
| **State** | Stateful | Stateless |
| **Rules** | Allow only | Allow and deny |
| **Evaluation** | All rules | Numbered order |

## Next-Generation Firewall (NGFW)

### Features

| Feature | Description |
|---------|-------------|
| **Application Awareness** | Identify apps regardless of port |
| **User Identity** | Rules based on user, not just IP |
| **IPS/IDS** | Intrusion detection/prevention |
| **SSL Inspection** | Decrypt and inspect HTTPS |
| **URL Filtering** | Block categories of websites |
| **Malware Protection** | Sandboxing, threat intelligence |
| **Cloud Integration** | Sync with cloud workloads |

### Example: Palo Alto

```
# Application-based rule
rule "Allow-Web-Apps" {
  source-zone: trust
  destination-zone: untrust
  application: [ ssl, web-browsing, google-base ]
  user: [ domain\employees ]
  action: allow
  profile: {
    url-filtering: corporate-filter
    threat-prevention: strict
  }
}
```

## Firewall Architecture

### DMZ Design

```
                        Internet
                            │
                    ┌───────▼───────┐
                    │ External FW   │
                    └───────┬───────┘
                            │
              ┌─────────────┼─────────────┐
              │             │             │
         ┌────▼────┐  ┌─────▼─────┐  ┌────▼────┐
         │ Web     │  │ Mail      │  │ DNS     │
         │ Server  │  │ Server    │  │ Server  │
         └─────────┘  └───────────┘  └─────────┘
                      DMZ Network
                            │
                    ┌───────▼───────┐
                    │ Internal FW   │
                    └───────┬───────┘
                            │
              ┌─────────────┼─────────────┐
              │             │             │
         ┌────▼────┐  ┌─────▼─────┐  ┌────▼────┐
         │Database │  │App        │  │Internal │
         │Servers  │  │Servers    │  │Users    │
         └─────────┘  └───────────┘  └─────────┘
                    Internal Network
```

### Zero Trust Model

```
┌─────────────────────────────────────────────────────┐
│ Traditional: Trust internal, verify external        │
│ Zero Trust: Never trust, always verify              │
└─────────────────────────────────────────────────────┘

• Micro-segmentation
• Identity-based access
• Least privilege
• Continuous verification
• Assume breach
```

## Common Vendors

| Vendor | Products | Strengths |
|--------|----------|-----------|
| **Palo Alto** | PA Series | App-ID, threat prevention |
| **Fortinet** | FortiGate | Price/performance, UTM |
| **Cisco** | Firepower, ASA | Integration, enterprise |
| **Check Point** | Quantum | Management, security |
| **Sophos** | XG Firewall | Ease of use, SMB |
| **pfSense** | pfSense | Open source, flexible |

## Best Practices

| Practice | Description |
|----------|-------------|
| **Default deny** | Block all, allow specific |
| **Least privilege** | Minimum necessary access |
| **Document rules** | Comments, change tracking |
| **Regular audits** | Review and clean up rules |
| **Log everything** | Enable logging for analysis |
| **Segment networks** | Isolate sensitive systems |
| **Update signatures** | Keep IPS/threat DB current |
| **Test changes** | Validate before production |

## Troubleshooting

```bash
# Linux - check if traffic is being dropped
iptables -L -v -n | grep DROP

# Watch live traffic
tcpdump -i eth0 host 192.168.1.100

# Check connection tracking
conntrack -L

# Windows - check firewall logs
Get-WinEvent -LogName "Microsoft-Windows-Windows Firewall With Advanced Security/Firewall"

# Test connectivity
nc -zv hostname 443
telnet hostname 443
```

## Related

- [[Routers and Switches]] — Network infrastructure
- [[Network Topologies]] — Network design
- [[Networking Fundamentals]] — Protocols and layers
- [[Security MOC]] — Security topics
