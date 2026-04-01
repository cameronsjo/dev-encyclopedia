---
title: RPG
aliases:
  - RPG IV
  - RPGLE
  - ILE RPG
  - Report Program Generator
tags:
  - language
  - legacy
  - mainframe
  - ibm
  - enterprise
type: reference
status: complete
created: "2025-12-16"
---

# RPG

IBM's business programming language for AS/400 (IBM i), evolved from fixed-format report generation to modern free-format programming.

## Overview

| Aspect | Details |
|--------|---------|
| **Full Name** | Report Program Generator |
| **Platform** | IBM i (AS/400, iSeries) |
| **First Released** | 1959 (RPG I), 1994 (RPG IV) |
| **Current Version** | RPG IV (ILE RPG, free-format) |
| **Paradigm** | Procedural, now supports procedures |
| **Primary Use** | Business applications, ERP, database processing |

## Evolution

| Version | Year | Key Features |
|---------|------|--------------|
| **RPG I** | 1959 | Report generation |
| **RPG II** | 1969 | S/36, S/38 |
| **RPG III** | 1979 | Structured operations |
| **RPG/400** | 1988 | AS/400 version |
| **RPG IV** | 1994 | ILE, procedures, long names |
| **Free-format** | 2001 | C-style free-format (partial) |
| **Fully free** | 2013 | **FREE - no column restrictions |

## Fixed vs Free Format

### Fixed Format (Legacy)

**Columns matter** — Each column has specific meaning.

```rpg
     C     CUSTNO        CHAIN     CUSTMAST
     C                   IF        %FOUND(CUSTMAST)
     C                   EVAL      BALANCE = BALANCE + PAYMENT
     C                   UPDATE    CUSTREC
     C                   ENDIF
```

| Columns | Purpose |
|---------|---------|
| 1-5 | Sequence number |
| 6 | Form type (H, F, D, I, C, O, P) |
| 7-8 | Control level |
| 9-11 | Indicators |
| 12+ | Operation-specific |

### Free Format (Modern)

**No column restrictions** — Modern syntax.

```rpg
**free
ctl-opt dftactgrp(*no) actgrp(*caller);

dcl-f CUSTMAST disk usage(*update) keyed;

dcl-s custNo packed(8);
dcl-s payment packed(11:2);

chain (custNo) CUSTMAST;
if %found(CUSTMAST);
    BALANCE += payment;
    update CUSTREC;
endif;

*inlr = *on;
```

## Program Structure

### Control Options

```rpg
**free
ctl-opt dftactgrp(*no)    // Not default activation group
        actgrp(*caller)    // Use caller's activation group
        option(*srcstmt)   // Source statement numbers
        debug(*yes);       // Debug mode
```

### File Declarations

```rpg
// Physical file (table) - update access
dcl-f CUSTMAST disk usage(*update) keyed;

// Display file (screen)
dcl-f CUSTDSP workstn;

// Printer file
dcl-f CUSTRPT printer oflind(*in90);

// Program-described file
dcl-f FLATFILE disk usage(*input);
```

### Data Definitions

```rpg
// Standalone variables
dcl-s custNo packed(8);
dcl-s custName char(50);
dcl-s balance packed(11:2);
dcl-s isActive ind;

// Constants
dcl-c MAX_RECORDS 1000;
dcl-c COMPANY_NAME 'ACME Corp';

// Data structures
dcl-ds custRec qualified;
    custNo packed(8);
    custName char(50);
    balance packed(11:2);
end-ds;

// Based on file record format
dcl-ds custFile likerec(CUSTREC);

// Array
dcl-s codes char(3) dim(100);
dcl-ds custArray likeds(custRec) dim(500);
```

### Procedures

```rpg
// Main procedure
dcl-proc main;
    dcl-pi *n;
        inputCustNo packed(8) const;
    end-pi;

    dcl-s result packed(11:2);

    result = getBalance(inputCustNo);

    return;
end-proc;

// Subprocedure with return value
dcl-proc getBalance;
    dcl-pi *n packed(11:2);
        custNo packed(8) const;
    end-pi;

    chain (custNo) CUSTMAST;
    if %found(CUSTMAST);
        return BALANCE;
    endif;

    return 0;
end-proc;

// Exported procedure (for service programs)
dcl-proc calculateTax export;
    dcl-pi *n packed(11:2);
        amount packed(11:2) const;
        rate packed(5:4) const;
    end-pi;

    return amount * rate;
end-proc;
```

## Database Operations

### Native I/O

```rpg
// Read by key
chain (custNo) CUSTMAST;
if %found(CUSTMAST);
    // Process record
endif;

// Sequential read
read CUSTMAST;
dow not %eof(CUSTMAST);
    // Process record
    read CUSTMAST;
enddo;

// Partial key read
setll (stateCode) CUSTBYSTATE;
reade (stateCode) CUSTBYSTATE;
dow not %eof(CUSTBYSTATE);
    // Process records for state
    reade (stateCode) CUSTBYSTATE;
enddo;

// Update
chain (custNo) CUSTMAST;
if %found(CUSTMAST);
    BALANCE += payment;
    update CUSTREC;
endif;

// Write new record
custRec.CUSTNO = newCustNo;
custRec.CUSTNAME = newName;
custRec.BALANCE = 0;
write CUSTREC custRec;

// Delete
chain (custNo) CUSTMAST;
if %found(CUSTMAST);
    delete CUSTREC;
endif;
```

### Embedded SQL

```rpg
// Single row fetch
exec sql
    SELECT CUSTNAME, BALANCE
    INTO :custName, :balance
    FROM CUSTMAST
    WHERE CUSTNO = :custNo;

if sqlcode = 0;
    // Found
elseif sqlcode = 100;
    // Not found
else;
    // Error
endif;

// Cursor for multiple rows
exec sql DECLARE custCursor CURSOR FOR
    SELECT CUSTNO, CUSTNAME, BALANCE
    FROM CUSTMAST
    WHERE STATE = :stateCode
    ORDER BY CUSTNAME;

exec sql OPEN custCursor;

exec sql FETCH custCursor INTO :custNo, :custName, :balance;
dow sqlcode = 0;
    // Process row
    exec sql FETCH custCursor INTO :custNo, :custName, :balance;
enddo;

exec sql CLOSE custCursor;

// Insert
exec sql
    INSERT INTO CUSTMAST (CUSTNO, CUSTNAME, BALANCE)
    VALUES (:custNo, :custName, :balance);

// Update
exec sql
    UPDATE CUSTMAST
    SET BALANCE = BALANCE + :payment
    WHERE CUSTNO = :custNo;
```

## Built-In Functions

### String Functions

| Function | Description |
|----------|-------------|
| `%trim(string)` | Remove leading/trailing blanks |
| `%subst(string:start:len)` | Substring |
| `%scan(needle:haystack)` | Find position |
| `%replace(new:old:start:len)` | Replace substring |
| `%xlate(from:to:string)` | Translate characters |
| `%len(string)` | Length |
| `%upper(string)` | Uppercase |
| `%lower(string)` | Lowercase |

### Numeric Functions

| Function | Description |
|----------|-------------|
| `%dec(value:digits:decimals)` | Convert to decimal |
| `%int(value)` | Convert to integer |
| `%abs(number)` | Absolute value |
| `%rem(dividend:divisor)` | Remainder |
| `%div(dividend:divisor)` | Integer division |
| `%editc(number:editcode)` | Format number |

### Date Functions

| Function | Description |
|----------|-------------|
| `%date(value)` | Convert to date |
| `%time(value)` | Convert to time |
| `%timestamp(value)` | Convert to timestamp |
| `%diff(date1:date2:unit)` | Date difference |
| `%days(number)` | Duration in days |

### File Functions

| Function | Description |
|----------|-------------|
| `%found(file)` | Record found |
| `%eof(file)` | End of file |
| `%equal(file)` | Exact key match |
| `%open(file)` | File is open |

## ILE Concepts

### Modules and Programs

```
Source (.rpgle)
    ↓ CRTRPGMOD
Module (.module)
    ↓ CRTPGM
Program (.pgm)
```

### Service Programs

**Reusable procedure libraries:**

```
Module A + Module B
    ↓ CRTSRVPGM
Service Program (.srvpgm)
    ↓ Bind to
Multiple Programs
```

### Binding

```cl
CRTPGM PGM(MYPGM) MODULE(MYMOD)
       BNDSRVPGM(MYUTIL MATHLIB)
```

## Error Handling

### Monitor Blocks

```rpg
monitor;
    result = numerator / denominator;
on-error *all;
    result = 0;
    errorOccurred = *on;
endmon;

// Specific errors
monitor;
    chain (custNo) CUSTMAST;
on-error 1218;  // Record locked
    // Handle lock
on-error 1211;  // I/O error
    // Handle I/O error
endmon;
```

### Program Status Data Structure

```rpg
dcl-ds pgmStatus psds;
    pgmName *proc;
    pgmStatus *status;
    prevStatus char(5) pos(16);
    lineNum char(8) pos(21);
    routine *routine;
    parms *parms;
    exceptionId char(7) pos(40);
    user char(10) pos(254);
    jobNumber char(6) pos(264);
end-ds;
```

## Comparison with Modern Languages

| Aspect | RPG | Java | C# |
|--------|-----|------|-----|
| **Typing** | Static | Static | Static |
| **Database** | Integrated | JDBC | ADO.NET |
| **Performance** | ✅ Excellent | ✅ Good | ✅ Good |
| **Learning Curve** | ⚠️ Unusual syntax | ✅ Familiar | ✅ Familiar |
| **Modern Tooling** | ⚠️ Limited | ✅ Excellent | ✅ Excellent |
| **Community** | ⚠️ Small | ✅ Large | ✅ Large |

## When to Use RPG

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Database Performance** | Native I/O extremely fast |
| **IBM i Integration** | First-class platform language |
| **Business Logic** | Designed for business apps |
| **Stability** | Decades of production code |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Platform Lock-in** | IBM i only |
| **Skills Availability** | Shrinking talent pool |
| **Modern Practices** | TDD, CI/CD harder |
| **Perception** | "Legacy" stigma |

### Best For

- IBM i application development
- High-performance database operations
- Maintaining existing systems
- ERP and business applications

## Related

- [[AS400]] — The platform RPG runs on
- [[COBOL]] — Similar legacy business language
- [[JCL]] — Mainframe job control
- [[Languages MOC]] — All language references
