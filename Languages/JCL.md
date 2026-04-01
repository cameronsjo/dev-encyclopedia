---
title: JCL
aliases:
  - Job Control Language
  - z/OS JCL
  - MVS JCL
tags:
  - language
  - legacy
  - mainframe
  - scripting
  - batch
type: reference
status: complete
created: "2025-12-16"
---

# JCL

Job Control Language for IBM mainframes — defines jobs, allocates resources, and controls program execution in batch processing environments.

## Overview

| Aspect | Details |
|--------|---------|
| **Type** | Job control / scripting language |
| **Platform** | IBM z/OS, MVS, OS/390 |
| **First Appeared** | 1964 (OS/360) |
| **Primary Use** | Batch job submission, resource allocation |
| **Syntax Style** | Fixed-format, positional/keyword parameters |
| **File Extension** | None (members in PDS) |
| **Modern Equivalent** | Shell scripts, workflow managers |

## Core Concepts

### Statement Types

JCL consists of three primary statement types:

| Statement | Prefix | Purpose |
|-----------|--------|---------|
| **JOB** | `//name JOB` | Defines job, sets accounting |
| **EXEC** | `//name EXEC` | Executes program or procedure |
| **DD** | `//name DD` | Data Definition — allocates datasets |

**Comment:** `//*` starts a comment line.

### Basic Job Structure

```jcl
//MYJOB    JOB (ACCT),'PROGRAMMER NAME',CLASS=A,MSGCLASS=X
//*
//* This job compiles and runs a COBOL program
//*
//STEP1    EXEC PGM=IGYCRCTL
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DSN=USER.SOURCE(PROGRAM1),DISP=SHR
//SYSLIN   DD DSN=&&LOADSET,DISP=(MOD,PASS),
//            UNIT=SYSDA,SPACE=(TRK,(3,3))
//*
//STEP2    EXEC PGM=IEWL
//SYSLIN   DD DSN=&&LOADSET,DISP=(OLD,DELETE)
//SYSLMOD  DD DSN=USER.LOAD(PROGRAM1),DISP=SHR
//SYSPRINT DD SYSOUT=*
//*
//STEP3    EXEC PGM=PROGRAM1
//INFILE   DD DSN=PROD.INPUT.DATA,DISP=SHR
//OUTFILE  DD DSN=PROD.OUTPUT.DATA,DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)
//SYSOUT   DD SYSOUT=*
```

### JOB Statement

**Identifies and Configures the Job:**

```jcl
//PAYROLL  JOB (ACCT123,DEPT456),'JOHN SMITH',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,
//             TIME=(5,30),
//             REGION=0M
```

| Parameter | Purpose |
|-----------|---------|
| **CLASS** | Job class (priority/resource pool) |
| **MSGCLASS** | Output class for job messages |
| **MSGLEVEL** | Verbosity of JCL listing |
| **NOTIFY** | User to notify on completion |
| **TIME** | Maximum CPU time (minutes,seconds) |
| **REGION** | Memory allocation |

### EXEC Statement

**Executes Programs or Procedures:**

```jcl
//STEP1    EXEC PGM=IEFBR14
//STEP2    EXEC PGM=SORT,PARM='SIZE=MAX'
//STEP3    EXEC PROC=COBCL
```

| Parameter | Purpose |
|-----------|---------|
| **PGM** | Program name to execute |
| **PROC** | Procedure name to invoke |
| **PARM** | Parameters passed to program |
| **COND** | Conditional execution |
| **TIME** | Step time limit |
| **REGION** | Step memory limit |

### DD Statement

**Data Definition — The Heart of JCL:**

```jcl
//INPUT    DD DSN=PROD.CUSTOMER.MASTER,DISP=SHR
//OUTPUT   DD DSN=PROD.CUSTOMER.BACKUP,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(100,50),RLSE),
//            DCB=(RECFM=FB,LRECL=200,BLKSIZE=20000)
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
  Control card data goes here
/*
```

### DISP Parameter

**Dataset Disposition** — Controls access and handling:

```jcl
DISP=(status,normal-end,abnormal-end)
```

| Status | Meaning |
|--------|---------|
| **NEW** | Create new dataset |
| **OLD** | Exclusive access to existing |
| **SHR** | Shared read access |
| **MOD** | Append to existing |

| Disposition | Meaning |
|-------------|---------|
| **DELETE** | Remove after use |
| **KEEP** | Retain without cataloging |
| **CATLG** | Catalog the dataset |
| **PASS** | Pass to next step |

### SPACE Parameter

**Disk Space Allocation:**

```jcl
SPACE=(unit,(primary,secondary,directory),RLSE)
```

| Unit | Size |
|------|------|
| **TRK** | Tracks |
| **CYL** | Cylinders |
| **bytes** | Exact bytes |

### DCB Parameter

**Data Control Block** — Record format:

```jcl
DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)
```

| Attribute | Values |
|-----------|--------|
| **RECFM** | F (fixed), V (variable), FB (fixed blocked), VB |
| **LRECL** | Logical record length |
| **BLKSIZE** | Physical block size |

## Conditional Execution

### COND Parameter

**Skip Step Based on Return Codes:**

```jcl
//STEP2    EXEC PGM=PROG2,COND=(4,LT)
```

Meaning: Skip if ANY prior return code is **less than** 4.

| Operator | Meaning |
|----------|---------|
| **GT** | Greater than |
| **GE** | Greater than or equal |
| **EQ** | Equal |
| **NE** | Not equal |
| **LT** | Less than |
| **LE** | Less than or equal |

### IF/THEN/ELSE (Modern)

```jcl
//         IF (STEP1.RC = 0) THEN
//STEP2    EXEC PGM=PROG2
//         ELSE
//STEP2A   EXEC PGM=ERRORHANDLER
//         ENDIF
```

## Procedures (PROCs)

**Reusable JCL Templates:**

### Cataloged Procedure

```jcl
//COBCL    PROC
//COMPILE  EXEC PGM=IGYCRCTL
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DSN=&SOURCE,DISP=SHR
//SYSLIN   DD DSN=&&OBJ,DISP=(,PASS)
//LINK     EXEC PGM=IEWL
//SYSLIN   DD DSN=&&OBJ,DISP=(OLD,DELETE)
//SYSLMOD  DD DSN=&LOAD,DISP=SHR
//         PEND
```

### Invoking with Overrides

```jcl
//STEP1    EXEC PROC=COBCL,SOURCE='MY.SOURCE(PROG1)',
//             LOAD='MY.LOADLIB(PROG1)'
//COMPILE.SYSPRINT DD SYSOUT=A
```

## Common Programs

| Program | Purpose |
|---------|---------|
| **IEFBR14** | Do nothing (allocate/delete datasets) |
| **IEBGENER** | Copy sequential datasets |
| **IEBCOPY** | Copy/compress PDS members |
| **SORT** | Sort/merge data (DFSORT, Syncsort) |
| **IDCAMS** | VSAM utility (define, delete, repro) |
| **IKJEFT01** | TSO batch |

### IEFBR14 Example (Create Dataset)

```jcl
//CREATE   EXEC PGM=IEFBR14
//NEWFILE  DD DSN=USER.NEW.DATASET,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(10,5)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)
```

### SORT Example

```jcl
//SORT     EXEC PGM=SORT
//SORTIN   DD DSN=INPUT.DATA,DISP=SHR
//SORTOUT  DD DSN=OUTPUT.DATA,DISP=(NEW,CATLG)
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A,15,5,ZD,D)
  INCLUDE COND=(20,2,CH,EQ,C'US')
/*
//SYSOUT   DD SYSOUT=*
```

## Symbolic Parameters

**Variable Substitution:**

```jcl
//MYJOB    JOB ...
// SET DATE=20251216
// SET ENV=PROD
//STEP1    EXEC PGM=REPORT
//INPUT    DD DSN=&ENV..DATA.&DATE,DISP=SHR
```

**Note:** Double periods (`..`) separate symbolic from literal text.

## Common Pitfalls

| Issue | Cause | Solution |
|-------|-------|----------|
| **JCL Error** | Syntax error, invalid parameter | Check column positions, spelling |
| **S806** | Program not found | Verify STEPLIB/JOBLIB |
| **S0C7** | Data exception | Invalid numeric data |
| **S0C4** | Protection exception | Memory/addressing error |
| **S913** | Security violation | Check RACF permissions |
| **S322** | Time exceeded | Increase TIME parameter |
| **SB37** | Out of space | Increase SPACE, compress |

## Comparison with Modern Equivalents

| Aspect | JCL | Bash/Shell | Kubernetes Jobs |
|--------|-----|------------|-----------------|
| **Resource Allocation** | DD statements | Volumes/mounts | Resource specs |
| **Conditional Execution** | COND/IF | `if`/`&&`/`||` | Init containers |
| **Parameterization** | Symbolic & | Environment vars | ConfigMaps |
| **Job Dependencies** | Return codes | Exit codes | Job dependencies |
| **Scheduling** | JES2/JES3 | Cron | CronJobs |

## When to Use JCL

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Resource Control** | Fine-grained dataset allocation |
| **Job Management** | Native batch scheduling |
| **COBOL Integration** | Standard for mainframe programs |
| **Audit Trail** | Comprehensive job logging |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Cryptic Syntax** | Steep learning curve |
| **Fixed Format** | Column-sensitive (mostly) |
| **Mainframe-Only** | No portability |
| **Verbose** | Simple tasks require many lines |

### Best For

- Running [[COBOL]] batch programs
- Mainframe data processing
- Enterprise batch scheduling
- Integration with CICS/DB2

## Related

- [[COBOL]] — Primary language executed via JCL
- [[Fortran]] — Also runs on mainframes
- [[Languages MOC]] — Overview of all language references
