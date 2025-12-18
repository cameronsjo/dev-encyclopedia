---
title: CICS
aliases:
  - Customer Information Control System
  - CICS TS
  - CICS Transaction Server
tags:
  - tools
  - mainframe
  - legacy
  - enterprise
  - transactions
type: reference
status: complete
created: "2025-12-16"
---

# CICS

IBM's mainframe transaction processing system, handling billions of transactions daily across banking, retail, and enterprise systems.

## Overview

| Aspect | Details |
|--------|---------|
| **Full Name** | Customer Information Control System |
| **Current Version** | CICS Transaction Server (TS) 6.x |
| **First Released** | 1969 |
| **Platform** | z/OS |
| **Primary Use** | Online Transaction Processing (OLTP) |
| **Transaction Volume** | 1+ million transactions/second capable |
| **Market Reach** | Used by 90%+ of Fortune 500 |

## Core Concepts

### What CICS Does

```
┌─────────────────────────────────────────────┐
│                  Terminals                   │
│  ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐ ┌─────┐   │
│  │3270 │ │3270 │ │ Web │ │ MQ  │ │ API │   │
│  └──┬──┘ └──┬──┘ └──┬──┘ └──┬──┘ └──┬──┘   │
└─────┼───────┼───────┼───────┼───────┼───────┘
      │       │       │       │       │
      └───────┴───────┼───────┴───────┘
                      ▼
┌─────────────────────────────────────────────┐
│                   CICS                       │
│  ┌─────────────────────────────────────┐    │
│  │      Transaction Manager            │    │
│  │  • Task Management                  │    │
│  │  • Program Control                  │    │
│  │  • Terminal Control                 │    │
│  │  • File Control                     │    │
│  │  • Temporary Storage                │    │
│  └─────────────────────────────────────┘    │
│                     │                        │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐       │
│  │  VSAM   │ │   DB2   │ │   IMS   │       │
│  └─────────┘ └─────────┘ └─────────┘       │
└─────────────────────────────────────────────┘
```

### Key Services

| Service | Purpose |
|---------|---------|
| **Task Management** | Concurrent transaction execution |
| **Program Control** | LINK, XCTL, RETURN |
| **Terminal Control** | Screen I/O (SEND/RECEIVE MAP) |
| **File Control** | VSAM file access |
| **Temporary Storage** | Scratch pad data |
| **Transient Data** | Queued I/O |
| **Interval Control** | Time-based operations |

## Transaction Model

### ACID Properties

| Property | CICS Implementation |
|----------|---------------------|
| **Atomicity** | Syncpoint/Rollback |
| **Consistency** | Program logic |
| **Isolation** | Record-level locking |
| **Durability** | Logging, recovery |

### Transaction Lifecycle

```
Terminal Input (TRANID)
    ↓
Program Load
    ↓
Execute (EXEC CICS commands)
    ↓
SYNCPOINT (commit) or ROLLBACK
    ↓
Response to Terminal
```

## Programming Model

### COBOL with CICS

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTINQ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CUSTNO        PIC 9(8).
       01  WS-CUSTNAME      PIC X(50).
       01  WS-BALANCE       PIC S9(9)V99.
       01  WS-RESP          PIC S9(8) COMP.

       COPY DFHAID.
       COPY CUSTMAP.

       PROCEDURE DIVISION.
           EXEC CICS RECEIVE MAP('CUSTMAP')
                     MAPSET('CUSTSET')
                     INTO(CUSTMAPI)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE CUSTNOI TO WS-CUSTNO
               PERFORM READ-CUSTOMER
               PERFORM SEND-RESPONSE
           END-IF

           EXEC CICS RETURN END-EXEC.

       READ-CUSTOMER.
           EXEC CICS READ FILE('CUSTMAST')
                     INTO(CUSTOMER-RECORD)
                     RIDFLD(WS-CUSTNO)
                     RESP(WS-RESP)
           END-EXEC.

       SEND-RESPONSE.
           MOVE WS-CUSTNAME TO CUSTNAMEO
           MOVE WS-BALANCE TO BALANCEO
           EXEC CICS SEND MAP('CUSTMAP')
                     MAPSET('CUSTSET')
                     FROM(CUSTMAPO)
                     ERASE
           END-EXEC.
```

### Common EXEC CICS Commands

#### Terminal I/O

```cobol
* Receive screen input
EXEC CICS RECEIVE MAP('mapname')
          MAPSET('mapset')
          INTO(data-area)
END-EXEC

* Send screen output
EXEC CICS SEND MAP('mapname')
          MAPSET('mapset')
          FROM(data-area)
          ERASE
END-EXEC

* Send text
EXEC CICS SEND TEXT FROM(message)
          LENGTH(msg-len)
          ERASE
END-EXEC
```

#### File Operations

```cobol
* Read by key
EXEC CICS READ FILE('filename')
          INTO(record-area)
          RIDFLD(key-field)
          RESP(response)
END-EXEC

* Read for update
EXEC CICS READ FILE('filename')
          INTO(record-area)
          RIDFLD(key-field)
          UPDATE
END-EXEC

* Rewrite (after READ UPDATE)
EXEC CICS REWRITE FILE('filename')
          FROM(record-area)
END-EXEC

* Write new record
EXEC CICS WRITE FILE('filename')
          FROM(record-area)
          RIDFLD(key-field)
END-EXEC

* Delete
EXEC CICS DELETE FILE('filename')
          RIDFLD(key-field)
END-EXEC

* Browse
EXEC CICS STARTBR FILE('filename')
          RIDFLD(key-field)
          GTEQ
END-EXEC

EXEC CICS READNEXT FILE('filename')
          INTO(record-area)
          RIDFLD(key-field)
END-EXEC

EXEC CICS ENDBR FILE('filename') END-EXEC
```

#### Program Control

```cobol
* Call another program, return here
EXEC CICS LINK PROGRAM('progname')
          COMMAREA(data-area)
          LENGTH(data-len)
END-EXEC

* Transfer control, don't return
EXEC CICS XCTL PROGRAM('progname')
          COMMAREA(data-area)
END-EXEC

* Return to CICS
EXEC CICS RETURN END-EXEC

* Return with next transaction
EXEC CICS RETURN TRANSID('MENU')
          COMMAREA(data-area)
END-EXEC
```

#### Temporary Storage

```cobol
* Write to TS queue
EXEC CICS WRITEQ TS QUEUE('queuename')
          FROM(data-area)
          LENGTH(data-len)
          ITEM(item-number)
END-EXEC

* Read from TS queue
EXEC CICS READQ TS QUEUE('queuename')
          INTO(data-area)
          LENGTH(data-len)
          ITEM(item-number)
END-EXEC

* Delete queue
EXEC CICS DELETEQ TS QUEUE('queuename')
END-EXEC
```

#### Error Handling

```cobol
* Using RESP
EXEC CICS READ FILE('CUSTFILE')
          INTO(CUST-REC)
          RIDFLD(CUST-KEY)
          RESP(WS-RESP)
          RESP2(WS-RESP2)
END-EXEC

EVALUATE WS-RESP
    WHEN DFHRESP(NORMAL)
        CONTINUE
    WHEN DFHRESP(NOTFND)
        PERFORM RECORD-NOT-FOUND
    WHEN DFHRESP(LENGERR)
        PERFORM LENGTH-ERROR
    WHEN OTHER
        PERFORM GENERAL-ERROR
END-EVALUATE

* Using HANDLE CONDITION (legacy)
EXEC CICS HANDLE CONDITION
          NOTFND(NOT-FOUND-PARA)
          ERROR(ERROR-PARA)
END-EXEC
```

## BMS (Basic Mapping Support)

### Map Definition

```
CUSTSET  DFHMSD TYPE=&SYSPARM,LANG=COBOL,MODE=INOUT,      *
               CTRL=FREEKB,STORAGE=AUTO,TIOAPFX=YES
*
CUSTMAP  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*
         DFHMDF POS=(1,30),LENGTH=20,                      *
               ATTRB=(ASKIP,BRT),                          *
               INITIAL='CUSTOMER INQUIRY'
*
         DFHMDF POS=(5,1),LENGTH=11,                       *
               ATTRB=ASKIP,                                *
               INITIAL='CUST NUMBER'
*
CUSTNO   DFHMDF POS=(5,13),LENGTH=8,                       *
               ATTRB=(UNPROT,NUM,IC)
*
         DFHMDF POS=(7,1),LENGTH=11,                       *
               ATTRB=ASKIP,                                *
               INITIAL='CUST NAME'
*
CUSTNAME DFHMDF POS=(7,13),LENGTH=50,                      *
               ATTRB=(ASKIP,BRT)
*
         DFHMSD TYPE=FINAL
         END
```

### Attribute Bytes

| Attribute | Meaning |
|-----------|---------|
| **ASKIP** | Auto-skip (protected) |
| **UNPROT** | Unprotected (input) |
| **NUM** | Numeric only |
| **BRT** | Bright (highlighted) |
| **DRK** | Dark (hidden) |
| **IC** | Initial cursor position |

## Modern CICS

### Web Services

```cobol
* RESTful service
EXEC CICS WEB RECEIVE
          INTO(WS-REQUEST)
          LENGTH(WS-REQ-LEN)
END-EXEC

* Process JSON
EXEC CICS JSON PARSE CHANNEL('JSONCHNL')
          CONTAINER('JSONDATA')
          INTO(WS-DATA-AREA)
END-EXEC

* Send response
EXEC CICS WEB SEND
          FROM(WS-RESPONSE)
          LENGTH(WS-RESP-LEN)
          MEDIATYPE('application/json')
          STATUSCODE(200)
END-EXEC
```

### Liberty Integration

CICS can host Java/Liberty for modern apps:

- JAX-RS REST APIs
- MicroProfile
- Spring Boot

### Containers (Channels and Containers)

```cobol
* Put data in container
EXEC CICS PUT CONTAINER('CUSTDATA')
          CHANNEL('CUSTCHNL')
          FROM(CUSTOMER-RECORD)
END-EXEC

* Get data from container
EXEC CICS GET CONTAINER('CUSTDATA')
          CHANNEL('CUSTCHNL')
          INTO(CUSTOMER-RECORD)
END-EXEC

* Link with channel
EXEC CICS LINK PROGRAM('CUSTPROC')
          CHANNEL('CUSTCHNL')
END-EXEC
```

## Administration

### Key Resources

| Resource | Definition | Purpose |
|----------|------------|---------|
| **PROGRAM** | PPT entry | Program attributes |
| **TRANSACTION** | PCT entry | Transaction → Program mapping |
| **FILE** | FCT entry | File access definitions |
| **TDQUEUE** | DCT entry | Transient data queues |
| **TSMODEL** | TST entry | Temp storage models |

### CEDA Commands

```
CEDA DEFINE PROGRAM(CUSTINQ) GROUP(CUSTGRP)
CEDA DEFINE TRANSACTION(CUST) PROGRAM(CUSTINQ) GROUP(CUSTGRP)
CEDA INSTALL GROUP(CUSTGRP)
```

### CEMT Commands

```
CEMT INQUIRE PROGRAM(CUSTINQ)
CEMT SET PROGRAM(CUSTINQ) NEWCOPY
CEMT INQUIRE TRANSACTION(CUST)
CEMT SET FILE(CUSTMAST) OPEN
```

## High Availability

### Sysplex Support

- **Workload balancing** across regions
- **Data sharing** with DB2
- **Recovery** and restart

### CICSPlex SM

Central management of multiple CICS regions:

- Workload management
- Business application management
- Real-time analysis

## Comparison with Alternatives

| Aspect | CICS | IMS TM | Tuxedo |
|--------|------|--------|--------|
| **Platform** | z/OS | z/OS | Unix/Linux |
| **Scale** | Massive | Massive | Large |
| **Language** | COBOL, Java, + | COBOL | C, COBOL |
| **Integration** | DB2, VSAM, MQ | IMS DB | Oracle |

## When to Use CICS

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Scale** | Billions of transactions daily |
| **Reliability** | Decades of proven uptime |
| **ACID** | Full transaction support |
| **Integration** | Deep z/OS ecosystem |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Cost** | z/OS licensing |
| **Skills** | Specialized knowledge |
| **Complexity** | Many moving parts |

### Best For

- High-volume OLTP
- Banking and finance
- Insurance claims
- Airline reservations
- Retail POS backend

## Related

- [[COBOL]] — Primary CICS language
- [[JCL]] — Batch job control
- [[Mainframes]] — Platform overview
- [[Tools MOC]] — All tools
