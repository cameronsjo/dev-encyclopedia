---
title: Unix History
aliases:
  - History of Unix
  - Unix Timeline
  - Unix Family Tree
tags:
  - cs
  - history
  - unix
  - operating-systems
type: concept
status: complete
created: "2025-12-16"
---

# Unix History

The evolution of Unix from Bell Labs research project to the foundation of modern computing.

## Timeline

```
1969  ┌─────────────────────────────────────────────────────┐
      │  Unix born at Bell Labs (Ken Thompson, Dennis Ritchie)
1971  │  First Edition Unix
1973  │  Unix rewritten in C
      │
1975  │  Version 6 (V6) - First widely distributed
      │         ↓
1977  │    ┌────┴────┐
      │    ↓         ↓
      │   BSD      System III
1979  │    │         │
      │   3BSD     System V
1983  │    │         │
      │   4.2BSD    SVR2
1986  │    │         │
      │   4.3BSD    SVR3 → Solaris, HP-UX, AIX
1989  │    │
      │   4.4BSD → FreeBSD, NetBSD, OpenBSD
1991  │
      │  Linux (Linus Torvalds) - Not Unix, Unix-like
1994  │
      │  Single Unix Specification (POSIX)
2000  │
      │  macOS (Darwin/BSD kernel)
      └─────────────────────────────────────────────────────┘
```

## Origins (1969-1975)

### Bell Labs

| Year | Event |
|------|-------|
| **1964** | Multics project begins (MIT, GE, Bell Labs) |
| **1969** | Bell Labs withdraws from Multics |
| **1969** | Ken Thompson writes Unix on PDP-7 |
| **1970** | Unix named (play on Multics) |
| **1971** | First Edition, PDP-11 |
| **1972** | Pipes introduced |
| **1973** | Rewritten in C (Dennis Ritchie) |

### Key Innovations

| Innovation | Impact |
|------------|--------|
| **Written in C** | Portable across hardware |
| **Hierarchical filesystem** | /usr/bin/program |
| **Pipes** | cmd1 \| cmd2 \| cmd3 |
| **Everything is a file** | Unified I/O model |
| **Shell** | Programmable command interpreter |
| **Small tools** | Do one thing well |

### The Unix Philosophy

1. Write programs that do one thing well
2. Write programs to work together
3. Write programs to handle text streams (universal interface)
4. Small is beautiful
5. Build a prototype as soon as possible
6. Choose portability over efficiency
7. Store data in flat text files
8. Use software leverage to your advantage
9. Avoid captive user interfaces
10. Make every program a filter

## The Great Schism (1977-1994)

### BSD Line (Berkeley)

| Version | Year | Key Features |
|---------|------|--------------|
| **1BSD** | 1977 | Ex editor, Pascal compiler |
| **2BSD** | 1978 | Vi editor, C shell |
| **3BSD** | 1979 | Virtual memory |
| **4.2BSD** | 1983 | TCP/IP networking, sockets |
| **4.3BSD** | 1986 | Performance improvements |
| **4.4BSD** | 1993 | Final Berkeley release |

**BSD Legacy:**

- TCP/IP implementation (the internet runs on it)
- Sockets API
- Vi editor
- C shell (csh)
- Virtual memory

### System V Line (AT&T)

| Version | Year | Key Features |
|---------|------|--------------|
| **System III** | 1981 | First commercial Unix |
| **System V** | 1983 | Init system, curses |
| **SVR2** | 1984 | Shell functions |
| **SVR3** | 1986 | STREAMS, shared libraries |
| **SVR4** | 1989 | Merged BSD features |

**System V Legacy:**

- Init scripts (/etc/init.d)
- System V IPC (shared memory, semaphores)
- STREAMS I/O framework
- terminfo/curses

### The Unix Wars

```
        AT&T (System V)         Berkeley (BSD)
              ↓                       ↓
    ┌─────────┼─────────┐    ┌───────┼───────┐
    ↓         ↓         ↓    ↓       ↓       ↓
  HP-UX    Solaris    AIX   SunOS  FreeBSD  macOS
   (HP)    (Sun/Oracle) (IBM)        ↓
                                  NetBSD
                                     ↓
                                  OpenBSD
```

## BSD Descendants

### FreeBSD (1993)

**Focus:** Performance, features, servers.

- PlayStation 4/5 OS based on FreeBSD
- Netflix CDN runs on FreeBSD
- WhatsApp infrastructure
- ZFS, jails, bhyve

### NetBSD (1993)

**Focus:** Portability.

- "Of course it runs NetBSD"
- Supports 50+ hardware platforms
- Clean, portable codebase

### OpenBSD (1995)

**Focus:** Security, correctness.

- Created by Theo de Raadt (forked from NetBSD)
- "Only two remote holes in the default install"
- OpenSSH, LibreSSL, PF firewall
- Proactive security auditing

### Darwin/macOS (2000)

**Foundation of Apple's OS.**

- NeXTSTEP (Mach microkernel + BSD)
- Acquired by Apple (1997)
- macOS, iOS, iPadOS, watchOS, tvOS

## System V Descendants

### Solaris (Sun Microsystems, 1992)

- SunOS → Solaris transition
- ZFS, DTrace, Zones, SMF
- Oracle Solaris (2010+)
- OpenSolaris → illumos (open source)

### HP-UX (Hewlett-Packard, 1984)

- PA-RISC, then Itanium
- Enterprise Unix for HP hardware

### AIX (IBM, 1986)

- IBM POWER systems
- Enterprise features, LPARs
- Still actively developed

## Linux: The Unix-Like (1991)

### Not Unix, But

**Linux is Unix-like, not Unix.**

- Written from scratch by Linus Torvalds
- POSIX-compatible (mostly)
- GNU userland (GNU's Not Unix)
- Free and open source (GPL)

### Linux Timeline

| Year | Event |
|------|-------|
| **1991** | Linux 0.01 announced |
| **1992** | Linux goes GPL |
| **1993** | Slackware, Debian |
| **1994** | Linux 1.0, Red Hat |
| **1996** | Linux 2.0 (SMP) |
| **2003** | Linux 2.6 |
| **2011** | Linux 3.0 |
| **2015** | Linux 4.0 |
| **2019** | Linux 5.0 |
| **2022** | Linux 6.0 |

### Linux Distributions

```
                    Debian (1993)
                        ↓
            ┌───────────┼───────────┐
            ↓           ↓           ↓
         Ubuntu     Linux Mint   Raspbian
          (2004)

                  Red Hat (1994)
                        ↓
            ┌───────────┼───────────┐
            ↓           ↓           ↓
          RHEL      Fedora      CentOS→Rocky

                  Slackware (1993)
                        ↓
                     Arch (2002)
                        ↓
                     Manjaro
```

## Standards and POSIX

### POSIX (1988+)

**Portable Operating System Interface.**

- IEEE standard for Unix compatibility
- Defines APIs, shell, utilities
- Allows code portability

### Single Unix Specification (SUS)

**Official "Unix" certification.**

- The Open Group owns "UNIX" trademark
- Only certified systems can use the name
- macOS, Solaris, AIX are certified Unix
- Linux is Unix-like, not certified

### Certified UNIX Systems

| System | Version |
|--------|---------|
| macOS | Current |
| AIX | 7.x |
| HP-UX | 11i |
| Solaris | 11 |
| z/OS Unix System Services | Current |

## Unix Family Tree

```
Multics (1964)
    │
    ↓
UNICS/Unix (1969) ─────────────────────────────────────┐
    │                                                   │
    ├─→ BSD (1977) ──────────────────────┐             │
    │       │                             │             │
    │       ├─→ SunOS                     │             │
    │       │                             │             │
    │       ├─→ FreeBSD (1993)            │             │
    │       │                             │             │
    │       ├─→ NetBSD (1993)             │             │
    │       │       │                     │             │
    │       │       └─→ OpenBSD (1995)    │             │
    │       │                             │             │
    │       └─→ NeXTSTEP → Darwin/macOS   │             │
    │                                     │             │
    └─→ System V (1983) ─────────────────┤             │
            │                             │             │
            ├─→ Solaris                   │             │
            │       │                     │             │
            │       └─→ illumos           │             │
            │                             │             │
            ├─→ HP-UX                     │             │
            │                             │             │
            └─→ AIX                       │             │
                                          │             │
    Linux (1991) ─────────────────────────┘             │
        │ (Unix-like, not derived)                      │
        │                                               │
        ├─→ Debian → Ubuntu, Mint                       │
        ├─→ Red Hat → RHEL, Fedora                      │
        ├─→ Slackware → Arch                            │
        └─→ Android (Linux kernel)                      │
                                                        │
    Minix (1987) ───────────────────────────────────────┘
        (Inspired Linux, now microkernel research)
```

## Key People

| Person | Contribution |
|--------|--------------|
| **Ken Thompson** | Original Unix, B language, Go |
| **Dennis Ritchie** | C language, Unix |
| **Brian Kernighan** | "K&R C", Unix documentation |
| **Bill Joy** | BSD, vi, csh, Sun co-founder |
| **Richard Stallman** | GNU project, FSF, GPL |
| **Linus Torvalds** | Linux kernel |
| **Andrew Tanenbaum** | MINIX, inspired Linux |
| **Theo de Raadt** | OpenBSD, OpenSSH |

## Legacy and Influence

### Concepts From Unix

| Concept | Modern Manifestation |
|---------|---------------------|
| Pipes | Unix shells, PowerShell |
| Everything is a file | /proc, /sys, Plan 9 |
| Plain text config | YAML, JSON, TOML |
| Small tools | Unix philosophy, microservices |
| Scripting | Shell, Python, automation |
| Hierarchical filesystem | All modern OS |

### Running Unix Today

| Want... | Use... |
|---------|--------|
| BSD experience | FreeBSD, OpenBSD |
| macOS | Certified Unix with GUI |
| Unix-like (most common) | Linux (Ubuntu, RHEL) |
| Enterprise Unix | AIX, Solaris |
| Learning | FreeBSD, OpenBSD |

## Related

- [[Linux Distributions]] — Linux flavors
- [[Shells]] — Unix command interpreters
- [[C]] — Language Unix was written in
- [[Computer Science MOC]] — All CS topics
