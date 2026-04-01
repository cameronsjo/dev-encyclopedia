---
title: PL/I
aliases:
  - PL/1
  - Programming Language One
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

# PL/I

IBM's "do everything" language from the 1960s, combining features from FORTRAN, COBOL, and ALGOL for scientific and business programming.

## Overview

| Aspect | Details |
|--------|---------|
| **Full Name** | Programming Language One |
| **First Released** | 1964 (IBM) |
| **Paradigm** | Procedural, structured |
| **Typing** | Static, with extensive conversions |
| **Platforms** | z/OS, z/VM, OS/2, Windows, Unix |
| **Primary Use** | Mainframe systems, scientific, business |
| **Design Goal** | One language for all purposes |

## History and Context

### Why PL/I Was Created

**1960s Problem:**

- FORTRAN — Scientific, poor I/O
- COBOL — Business, poor math
- ALGOL — Academic, limited adoption

**IBM's Solution:** Create one language with everything.

### Influence on Later Languages

| Feature | PL/I (1964) | Later Adoption |
|---------|-------------|----------------|
| Exception handling | `ON` conditions | Java, C++, Python |
| Pointers | `POINTER` | C (1972) |
| Structures | `STRUCTURE` | C structs |
| Dynamic allocation | `ALLOCATE` | malloc/new |
| String operations | Built-in | Most languages |
| Preprocessor | `%` statements | C preprocessor |

## Basic Syntax

### Program Structure

```pli
EXAMPLE: PROCEDURE OPTIONS(MAIN);
    /* Variable declarations */
    DECLARE CUSTOMER_NAME CHARACTER(50);
    DECLARE BALANCE DECIMAL FIXED(11,2);
    DECLARE COUNT BINARY FIXED(31);

    /* Executable statements */
    CUSTOMER_NAME = 'JOHN SMITH';
    BALANCE = 1234.56;
    COUNT = 0;

    PUT SKIP LIST('Customer:', CUSTOMER_NAME);
    PUT SKIP LIST('Balance:', BALANCE);

END EXAMPLE;
```

### Data Types

#### Arithmetic Types

```pli
/* Fixed-point decimal (like COBOL) */
DECLARE PRICE DECIMAL FIXED(7,2);      /* 99999.99 */
DECLARE QUANTITY DECIMAL FIXED(5);      /* 99999 */

/* Binary (integers) */
DECLARE INDEX BINARY FIXED(15);         /* 16-bit int */
DECLARE COUNTER BINARY FIXED(31);       /* 32-bit int */

/* Floating-point */
DECLARE RATE DECIMAL FLOAT(16);         /* Decimal float */
DECLARE RESULT BINARY FLOAT(53);        /* Binary float (double) */

/* Complex numbers */
DECLARE Z COMPLEX BINARY FLOAT;
Z = 3 + 4I;
```

#### String Types

```pli
/* Fixed-length strings */
DECLARE NAME CHARACTER(30);
DECLARE CODE CHARACTER(5);

/* Varying-length strings */
DECLARE MESSAGE CHARACTER(100) VARYING;

/* Bit strings */
DECLARE FLAGS BIT(8);
DECLARE MASK BIT(32);
```

#### Structured Types

```pli
DECLARE 1 CUSTOMER,
          2 CUST_ID DECIMAL FIXED(8),
          2 NAME,
            3 FIRST CHARACTER(20),
            3 LAST CHARACTER(30),
          2 ADDRESS,
            3 STREET CHARACTER(40),
            3 CITY CHARACTER(25),
            3 STATE CHARACTER(2),
            3 ZIP CHARACTER(10),
          2 BALANCE DECIMAL FIXED(11,2);

/* Access */
CUSTOMER.NAME.FIRST = 'JOHN';
CUSTOMER.BALANCE = 1000.00;
```

### Arrays

```pli
/* Single dimension */
DECLARE SCORES(100) DECIMAL FIXED(5,2);

/* Multi-dimensional */
DECLARE MATRIX(10,10) BINARY FLOAT;

/* With bounds */
DECLARE TABLE(-5:5, 1:12) DECIMAL FIXED(7,2);

/* Array operations */
DECLARE A(100) FIXED, B(100) FIXED, C(100) FIXED;
C = A + B;              /* Element-wise addition */
C = A * 2;              /* Scalar multiplication */
```

## Control Structures

### Conditionals

```pli
/* IF statement */
IF BALANCE > 0 THEN
    PUT LIST('Credit');
ELSE IF BALANCE < 0 THEN
    PUT LIST('Debit');
ELSE
    PUT LIST('Zero');

/* SELECT (like CASE) */
SELECT (GRADE);
    WHEN ('A') PUT LIST('Excellent');
    WHEN ('B') PUT LIST('Good');
    WHEN ('C') PUT LIST('Average');
    OTHERWISE PUT LIST('Below Average');
END;
```

### Loops

```pli
/* DO loop (like FOR) */
DO I = 1 TO 100;
    TOTAL = TOTAL + ARRAY(I);
END;

/* DO WHILE */
DO WHILE (NOT END_OF_FILE);
    READ FILE(INPUT) INTO(RECORD);
    PROCESS(RECORD);
END;

/* DO UNTIL */
DO UNTIL (RESPONSE = 'Q');
    GET LIST(RESPONSE);
END;

/* Iterative with increment */
DO I = 1 TO 100 BY 2;     /* Odd numbers */
    PUT LIST(I);
END;

/* Multiple iteration */
DO I = 1, 3, 5, 7 TO 20 BY 2;
    PUT LIST(I);
END;
```

## Procedures and Functions

### Internal Procedures

```pli
MAIN: PROCEDURE OPTIONS(MAIN);
    DECLARE RESULT DECIMAL FIXED(11,2);

    RESULT = CALCULATE_TAX(1000.00, 0.08);
    PUT LIST('Tax:', RESULT);

    CALCULATE_TAX: PROCEDURE(AMOUNT, RATE)
                   RETURNS(DECIMAL FIXED(11,2));
        DECLARE AMOUNT DECIMAL FIXED(11,2);
        DECLARE RATE DECIMAL FIXED(5,4);

        RETURN(AMOUNT * RATE);
    END CALCULATE_TAX;

END MAIN;
```

### Parameter Passing

```pli
/* By reference (default) */
PROCEDURE UPDATE_BALANCE(BALANCE);
    DECLARE BALANCE DECIMAL FIXED(11,2);
    BALANCE = BALANCE + 100;    /* Modifies caller's variable */
END;

/* By value (copy) */
PROCEDURE SAFE_CALC(VALUE) BYVALUE;
    DECLARE VALUE DECIMAL FIXED(11,2);
    VALUE = VALUE * 2;          /* Only local copy modified */
END;
```

### Recursive Procedures

```pli
FACTORIAL: PROCEDURE(N) RETURNS(FIXED BINARY(31)) RECURSIVE;
    DECLARE N FIXED BINARY(31);

    IF N <= 1 THEN
        RETURN(1);
    ELSE
        RETURN(N * FACTORIAL(N - 1));
END FACTORIAL;
```

## Exception Handling (ON Conditions)

### Built-in Conditions

```pli
/* Handle zero divide */
ON ZERODIVIDE BEGIN;
    PUT LIST('Division by zero!');
    RESULT = 0;
END;

/* Handle overflow */
ON OVERFLOW BEGIN;
    PUT LIST('Numeric overflow');
    /* SIGNAL or GOTO */
END;

/* Handle end of file */
ON ENDFILE(INFILE) EOF_FLAG = '1'B;

/* Handle conversion error */
ON CONVERSION BEGIN;
    PUT LIST('Invalid data conversion');
    /* Take corrective action */
END;

/* Multiple conditions */
ON ERROR BEGIN;
    PUT LIST('An error occurred');
    PUT LIST('In:', ONLOC());
    PUT LIST('Condition:', ONCODE());
END;
```

### Condition Codes

| Condition | Trigger |
|-----------|---------|
| **ZERODIVIDE** | Division by zero |
| **OVERFLOW** | Numeric overflow |
| **UNDERFLOW** | Numeric underflow |
| **CONVERSION** | Invalid type conversion |
| **STRINGRANGE** | String bounds exceeded |
| **SUBSCRIPTRANGE** | Array bounds exceeded |
| **ENDFILE** | End of file reached |
| **KEY** | Key not found |
| **ERROR** | Catch-all |

### SIGNAL and REVERT

```pli
/* Manually raise condition */
SIGNAL ENDFILE(MYFILE);

/* Disable handler */
REVERT ZERODIVIDE;

/* Re-enable system default */
ON ZERODIVIDE SYSTEM;
```

## File I/O

### Stream I/O

```pli
/* Print output */
PUT LIST('Hello, World!');
PUT SKIP LIST('Value:', X, 'Total:', Y);
PUT EDIT(NAME, BALANCE) (A(30), F(11,2));

/* Read input */
GET LIST(A, B, C);
GET EDIT(NAME, AMOUNT) (A(30), F(10,2));

/* File I/O */
DECLARE REPORT FILE PRINT;
OPEN FILE(REPORT) OUTPUT;
PUT FILE(REPORT) LIST('Report Title');
CLOSE FILE(REPORT);
```

### Record I/O

```pli
/* Declare file */
DECLARE CUSTFILE FILE RECORD SEQUENTIAL;
DECLARE 1 CUSTREC,
          2 CUSTNO FIXED DECIMAL(8),
          2 NAME CHARACTER(50),
          2 BALANCE FIXED DECIMAL(11,2);

/* Sequential read */
OPEN FILE(CUSTFILE) INPUT;
ON ENDFILE(CUSTFILE) MORE_RECORDS = '0'B;

READ FILE(CUSTFILE) INTO(CUSTREC);
DO WHILE(MORE_RECORDS);
    /* Process record */
    READ FILE(CUSTFILE) INTO(CUSTREC);
END;
CLOSE FILE(CUSTFILE);

/* Keyed access */
DECLARE INDEXFILE FILE RECORD KEYED;
READ FILE(INDEXFILE) INTO(RECORD) KEY(CUSTNO);
REWRITE FILE(INDEXFILE) FROM(RECORD) KEY(CUSTNO);
```

## Pointers and Storage

### Pointer Operations

```pli
DECLARE PTR POINTER;
DECLARE X FIXED DECIMAL(11,2) BASED(PTR);

/* Allocate storage */
ALLOCATE X SET(PTR);
X = 1234.56;

/* Free storage */
FREE X;

/* Null pointer */
IF PTR = NULL() THEN
    PUT LIST('Pointer is null');
```

### Linked Structures

```pli
DECLARE 1 NODE BASED(P),
          2 VALUE FIXED DECIMAL(11,2),
          2 NEXT POINTER;

DECLARE (HEAD, P, NEWNODE) POINTER;

/* Create node */
ALLOCATE NODE SET(NEWNODE);
NEWNODE->VALUE = 100;
NEWNODE->NEXT = HEAD;
HEAD = NEWNODE;

/* Traverse list */
P = HEAD;
DO WHILE(P ^= NULL());
    PUT LIST(P->VALUE);
    P = P->NEXT;
END;
```

## Preprocessor

```pli
%DECLARE DEBUG CHARACTER;
%DEBUG = 'YES';

%IF DEBUG = 'YES' %THEN
    %ACTIVATE TRACE;
%ELSE;
    %DEACTIVATE TRACE;
%END;

%INCLUDE COPYBOOK;

%MACRO SWAP(A, B);
    TEMP = A;
    A = B;
    B = TEMP;
%END;
```

## Modern PL/I

### Enterprise PL/I for z/OS

**Current IBM Compiler Features:**

- Unicode support
- XML processing
- SQL/DB2 integration
- 64-bit support
- Improved optimization

### Integration with Modern Systems

```pli
/* DB2 SQL */
EXEC SQL
    SELECT NAME, BALANCE INTO :NAME, :BALANCE
    FROM CUSTOMERS
    WHERE CUSTNO = :CUSTNO;

/* CICS */
EXEC CICS READ FILE('CUSTFILE')
     INTO(CUSTREC)
     RIDFLD(CUSTNO);

/* XML */
DECLARE DOC XMLCHAR(10000);
```

## Comparison with Other Languages

| Aspect | PL/I | COBOL | C |
|--------|------|-------|---|
| **Decimal Arithmetic** | ✅ Native | ✅ Native | ❌ Libraries |
| **Pointers** | ✅ Full | ⚠️ Limited | ✅ Full |
| **Structures** | ✅ Yes | ✅ Yes | ✅ Yes |
| **Exception Handling** | ✅ ON conditions | ❌ None | ❌ None (C) |
| **String Handling** | ✅ Built-in | ✅ Built-in | ⚠️ Library |
| **Arrays** | ✅ Flexible bounds | ⚠️ Fixed | ⚠️ Zero-based |

## When to Use PL/I

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Versatility** | Scientific and business in one language |
| **Exception Handling** | Pioneering error handling |
| **Data Types** | Rich numeric and string types |
| **Mainframe Integration** | First-class z/OS support |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Complexity** | Many features, large language |
| **Niche Usage** | Mainly mainframes |
| **Skills** | Very limited talent pool |

### Best For

- Legacy mainframe systems
- Mixed scientific/business applications
- Systems requiring structured exception handling

## Related

- [[COBOL]] — Business-focused mainframe language
- [[Fortran]] — Scientific computing predecessor
- [[Mainframes]] — Primary platform
- [[Languages MOC]] — All language references
