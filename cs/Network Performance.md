---
title: Network Performance
aliases:
  - Network Latency
  - Bandwidth
  - Throughput
tags:
  - cs
  - networking
  - performance
  - fundamentals
type: concept
status: complete
difficulty: intermediate
created: 2025-12-03
---

# Network Performance

Understanding and optimizing how fast data moves across networks.

## Key Metrics

| Metric | Definition | Unit |
|--------|------------|------|
| **Bandwidth** | Maximum data rate | bits/second (bps) |
| **Throughput** | Actual data rate achieved | bits/second |
| **Latency** | Time for packet to travel | milliseconds (ms) |
| **RTT** | Round-trip time | milliseconds |
| **Jitter** | Latency variation | milliseconds |
| **Packet Loss** | Packets that don't arrive | percentage |

---

## Latency Components

```mermaid
graph LR
    A[Propagation] --> B[Transmission]
    B --> C[Processing]
    C --> D[Queuing]

    style A fill:#E8F4FD
    style B fill:#FFF3CD
    style C fill:#D4EDDA
    style D fill:#FFE4E1
```

| Component | Description | Optimization |
|-----------|-------------|--------------|
| **Propagation** | Speed of light delay | Move closer (CDN) |
| **Transmission** | Time to push bits onto wire | More bandwidth |
| **Processing** | Router/switch handling | Better hardware |
| **Queuing** | Waiting in buffers | Reduce congestion |

### Propagation Delay

Light travels ~200,000 km/s in fiber (2/3 speed of light in vacuum).

| Route | Distance | Min Latency |
|-------|----------|-------------|
| Same city | 50 km | 0.25 ms |
| Cross-country (US) | 4,000 km | 20 ms |
| Transatlantic | 6,000 km | 30 ms |
| Around the world | 40,000 km | 200 ms |

**Reality:** Add processing, queuing, routing. Actual RTT ~2-3x minimum.

---

## Bandwidth vs Latency

| Scenario | Bandwidth Helps | Latency Matters |
|----------|----------------|-----------------|
| Downloading large file | ✅ | Less critical |
| Video streaming | ✅ | Buffering helps |
| Video call | Moderate | ✅ Real-time |
| Gaming | Less critical | ✅ Responsiveness |
| API requests | Less critical | ✅ Time to first byte |
| Database queries | Less critical | ✅ Query round-trips |

**High bandwidth, high latency:** Good for bulk transfer, bad for interactive.

**Low bandwidth, low latency:** Responsive but slow downloads.

---

## Bandwidth-Delay Product

Maximum data "in flight" at any time:

```
BDP = Bandwidth × RTT
```

| Link | Bandwidth | RTT | BDP |
|------|-----------|-----|-----|
| LAN | 1 Gbps | 1 ms | 125 KB |
| Cross-country | 100 Mbps | 50 ms | 625 KB |
| Intercontinental | 100 Mbps | 150 ms | 1.9 MB |

**Why it matters:** TCP buffers must be at least BDP for full utilization.

---

## TCP Performance

### Congestion Control

TCP adjusts sending rate based on network conditions.

| Algorithm | Description | Use Case |
|-----------|-------------|----------|
| **Reno** | Classic AIMD | Legacy |
| **CUBIC** | Default Linux | General purpose |
| **BBR** | Google's model-based | High BDP paths |
| **BBRv2** | Improved fairness | Replacing BBR |

### TCP Slow Start

New connections start slow, ramp up exponentially until loss.

```mermaid
graph LR
    A[1 segment] --> B[2 segments]
    B --> C[4 segments]
    C --> D[8 segments]
    D --> E[Loss detected]
    E --> F[Cut in half]
```

**Impact:** Cold connections are slow. Keep connections alive.

### Head-of-Line Blocking

One lost packet blocks all subsequent data until retransmitted.

**Solutions:**
- HTTP/2: Multiplexed streams (still blocked at TCP level)
- HTTP/3/QUIC: Per-stream loss handling

---

## Measuring Performance

### Tools

| Tool | Measures | Platform |
|------|----------|----------|
| `ping` | RTT, packet loss | Universal |
| `traceroute`/`mtr` | Path latency per hop | Universal |
| `iperf3` | Bandwidth, jitter | Universal |
| `netstat`/`ss` | Connection stats | Linux |
| `tcpdump`/`Wireshark` | Packet analysis | Universal |
| `curl -w` | HTTP timing breakdown | Universal |

### HTTP Timing Breakdown

```
DNS Lookup    → TCP Connect → TLS Handshake → Request → TTFB → Download
|-------------|-------------|---------------|---------|------|---------|
   dns_time     connect_time   tls_time      send     wait   receive
```

**TTFB (Time To First Byte):** Server processing + network latency.

---

## Optimization Strategies

### Reduce Latency

| Strategy | How |
|----------|-----|
| **CDN** | Serve from edge locations |
| **Connection reuse** | HTTP keep-alive, connection pooling |
| **DNS prefetch** | Resolve domains before needed |
| **TCP Fast Open** | Send data with SYN |
| **0-RTT TLS** | Resume sessions without handshake |
| **Edge computing** | Process at edge, not origin |

### Reduce Round Trips

| Protocol | Round Trips |
|----------|-------------|
| HTTP/1.1 (new conn) | TCP (1) + TLS (2) + Request (1) = 4 |
| HTTP/2 (new conn) | Same, but multiplexed after |
| HTTP/3 (new conn) | QUIC (1) + Request (0) = 1-2 |
| HTTP/3 (resumed) | 0-RTT possible |

### Increase Throughput

| Strategy | How |
|----------|-----|
| **Compression** | gzip, Brotli, zstd |
| **Minification** | Smaller JS/CSS |
| **Image optimization** | WebP, AVIF, responsive images |
| **Caching** | Reduce redundant transfers |
| **Parallel connections** | HTTP/1.1 workaround (6 per domain) |
| **HTTP/2 multiplexing** | Many requests, one connection |

---

## Bufferbloat

Excessive buffering causes latency spikes under load.

```mermaid
graph LR
    A[Packets arrive] --> B[Queue fills]
    B --> C[Latency increases]
    C --> D[Eventually dropped]
    D --> E[TCP backs off]
```

**Symptoms:** High latency when uploading/downloading.

**Solutions:**
- Active Queue Management (AQM)
- CoDel, fq_codel, CAKE algorithms
- Smaller buffers

---

## Quality of Service (QoS)

Prioritize certain traffic types.

| Traffic Type | Priority | Characteristics |
|--------------|----------|-----------------|
| Voice/Video | Highest | Low latency, low jitter |
| Interactive | High | Low latency |
| Bulk transfer | Normal | Throughput matters |
| Background | Low | Best effort |

### DSCP (Differentiated Services)

6-bit field in IP header marking priority class.

| DSCP | Name | Use |
|------|------|-----|
| EF (46) | Expedited Forwarding | VoIP |
| AF41 (34) | Assured Forwarding | Video |
| AF21 (18) | Assured Forwarding | Important data |
| BE (0) | Best Effort | Default |

---

## Mobile Network Performance

| Generation | Typical Latency | Typical Bandwidth |
|------------|-----------------|-------------------|
| 3G | 100-500 ms | 1-5 Mbps |
| 4G/LTE | 30-50 ms | 10-50 Mbps |
| 5G | 1-10 ms | 100+ Mbps |

**Challenges:**
- Variable conditions
- Handoffs between towers
- Radio resource scheduling

---

## Performance Budgets

### Web Performance Targets

| Metric | Target | Measures |
|--------|--------|----------|
| TTFB | < 200 ms | Server + network |
| FCP | < 1.8 s | First Contentful Paint |
| LCP | < 2.5 s | Largest Contentful Paint |
| FID | < 100 ms | First Input Delay |
| CLS | < 0.1 | Cumulative Layout Shift |

### API Performance Targets

| Tier | Latency | Use Case |
|------|---------|----------|
| Real-time | < 10 ms | Gaming, trading |
| Interactive | < 100 ms | User-facing APIs |
| Standard | < 1 s | Background operations |
| Batch | > 1 s | Bulk processing |

---

## Common Issues

| Symptom | Likely Cause | Investigation |
|---------|--------------|---------------|
| High latency, low loss | Distance, routing | `traceroute`, check geography |
| High latency, high loss | Congestion | Check utilization, QoS |
| Variable latency | Jitter, bufferbloat | `mtr`, check buffers |
| Low throughput | Small buffers, loss | Check BDP, tune buffers |
| Periodic slowdowns | Competing traffic | Monitor over time |

---

## Related

- [[Networking Fundamentals]]
- [[Sockets & Low-Level Networking]]
- [[System Design]]
- [[Technical Measurements]]
