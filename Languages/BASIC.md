---
title: BASIC
aliases:
  - Beginner's All-purpose Symbolic Instruction Code
  - Visual Basic
  - QuickBASIC
tags:
  - language
  - legacy
  - educational
  - scripting
type: reference
status: complete
created: "2025-12-16"
---

# BASIC

Beginner-friendly language that introduced programming to millions, evolving from mainframe timesharing to the foundation of Visual Basic.

## Overview

| Aspect | Details |
|--------|---------|
| **Paradigm** | Procedural, event-driven (VB) |
| **Typing** | Dynamic (classic), Static (VB.NET) |
| **Memory Model** | Managed |
| **First Appeared** | 1964 (Kemeny & Kurtz, Dartmouth) |
| **Primary Use Cases** | Education, rapid prototyping, Windows apps |
| **Notable Variants** | GW-BASIC, QuickBASIC, Visual Basic, VBA |
| **Modern Successor** | VB.NET (still supported) |

## Historical Significance

| Era | Variant | Impact |
|-----|---------|--------|
| **1964-1980** | Dartmouth BASIC | Timesharing, education revolution |
| **1975-1985** | Microcomputer BASIC | Home computers (Apple II, C64, TRS-80) |
| **1985-1991** | QuickBASIC/Turbo BASIC | Structured programming, compilation |
| **1991-2002** | Visual Basic 1-6 | Windows GUI development revolution |
| **2002-present** | VB.NET | Modern .NET language |

## Classic BASIC (Line Numbers)

### Original Syntax

```basic
10 REM This is a comment
20 PRINT "What is your name?"
30 INPUT NAME$
40 PRINT "Hello, "; NAME$
50 LET X = 0
60 FOR I = 1 TO 10
70     X = X + I
80 NEXT I
90 PRINT "Sum 1-10 ="; X
100 END
```

### Key Characteristics

- **Line Numbers** — Required for every statement
- **GOTO** — Primary flow control
- **Single-Letter Variables** — Limited namespace
- **String Variables** — Suffix `$` (e.g., `NAME$`)
- **Arrays** — DIM statement

### Common Statements

| Statement | Purpose |
|-----------|---------|
| **PRINT** | Output to screen |
| **INPUT** | Read user input |
| **LET** | Variable assignment |
| **IF...THEN** | Conditional |
| **FOR...NEXT** | Loop |
| **GOTO** | Unconditional jump |
| **GOSUB...RETURN** | Subroutine call |
| **DIM** | Array declaration |
| **REM** | Comment |
| **DATA/READ** | Inline data |

## Structured BASIC (QuickBASIC)

### Modern Syntax

```basic
' QuickBASIC / QBasic example
DECLARE SUB ProcessData (filename AS STRING)
DIM numbers(100) AS INTEGER
DIM total AS LONG

INPUT "Enter filename: ", filename$
CALL ProcessData(filename$)

FOR i = 1 TO 100
    total = total + numbers(i)
NEXT i

PRINT "Total:"; total

SUB ProcessData (filename AS STRING)
    OPEN filename FOR INPUT AS #1
    ' ... process file
    CLOSE #1
END SUB
```

### Improvements Over Classic

- No line numbers required
- Named subroutines (SUB/FUNCTION)
- Structured loops (DO...LOOP, SELECT CASE)
- User-defined types (TYPE...END TYPE)
- Multiple modules
- Compiled executables

## Visual Basic (VB6)

### Event-Driven Programming

```vb
' Form with button
Private Sub btnCalculate_Click()
    Dim result As Double
    result = Val(txtInput.Text) * 2
    lblOutput.Caption = "Result: " & result
End Sub

Private Sub Form_Load()
    txtInput.Text = "0"
End Sub
```

### Key Features

| Feature | Description |
|---------|-------------|
| **RAD** | Rapid Application Development |
| **GUI Designer** | Drag-and-drop interface builder |
| **COM Integration** | ActiveX controls and automation |
| **Database** | ADO/DAO data access |
| **Events** | Click, Load, Change, etc. |

### VB6 Impact

- Democratized Windows development
- Millions of business applications
- VBA embedded in Office suite
- Estimated 3+ million developers at peak

## VB.NET (Modern)

### Current Syntax

```vb
Imports System

Module Program
    Sub Main(args As String())
        Dim numbers = New List(Of Integer) From {1, 2, 3, 4, 5}

        Dim sum = numbers.Where(Function(n) n Mod 2 = 0).Sum()
        Console.WriteLine($"Sum of evens: {sum}")

        Dim person = New Person With {
            .Name = "Alice",
            .Age = 30
        }
        Console.WriteLine(person)
    End Sub
End Module

Public Class Person
    Public Property Name As String
    Public Property Age As Integer

    Public Overrides Function ToString() As String
        Return $"{Name} ({Age})"
    End Function
End Class
```

### .NET Features

- Full OOP (classes, inheritance, interfaces)
- LINQ support
- Async/Await
- Generics
- .NET ecosystem access
- Cross-platform (.NET Core/5+)

## VBA (Visual Basic for Applications)

### Office Automation

```vb
Sub FormatReport()
    ' Excel VBA example
    Dim ws As Worksheet
    Set ws = ActiveSheet

    With ws.Range("A1:D1")
        .Font.Bold = True
        .Interior.Color = RGB(200, 200, 255)
    End With

    Dim lastRow As Long
    lastRow = ws.Cells(Rows.Count, 1).End(xlUp).Row

    ws.Range("D2:D" & lastRow).Formula = "=B2*C2"
End Sub
```

### VBA Hosts

- Microsoft Excel
- Microsoft Word
- Microsoft Access
- Microsoft Outlook
- AutoCAD
- Many CAD/engineering tools

## BASIC Family Tree

```
Dartmouth BASIC (1964)
    ├── Microsoft BASIC (1975)
    │   ├── GW-BASIC / BASICA
    │   ├── QuickBASIC (1985)
    │   │   └── QBasic (1991)
    │   └── Visual Basic (1991)
    │       ├── VBA (1993)
    │       └── VB.NET (2002)
    ├── BBC BASIC
    ├── Commodore BASIC
    ├── Applesoft BASIC
    └── FreeBASIC (open source)
```

## Comparison with Alternatives

| Aspect | BASIC/VB | Python | C# |
|--------|----------|--------|-----|
| **Learning Curve** | ✅ Very easy | ✅ Easy | ⚠️ Moderate |
| **Syntax Verbosity** | ⚠️ Verbose | ✅ Concise | ⚠️ Moderate |
| **Modern Relevance** | ⚠️ Declining | ✅ Growing | ✅ Strong |
| **RAD/GUI** | ✅ Historical strength | ⚠️ Frameworks needed | ✅ WinForms/WPF |
| **Office Integration** | ✅ VBA native | ⚠️ Libraries | ⚠️ COM interop |

## When to Use

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Learning** | Designed for beginners |
| **Office Automation** | VBA is the native scripting language |
| **Legacy Maintenance** | Millions of VB6/VBA apps in production |
| **RAD** | Quick prototyping |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Declining Community** | Fewer resources, packages |
| **VB6 End of Life** | No longer supported |
| **Performance** | Interpreted variants slower |
| **Modern Paradigms** | Limited functional programming |

### Best For

- **Office Automation** — VBA for Excel/Word macros
- **Legacy Systems** — Maintaining VB6 applications
- **Education** — Teaching programming concepts
- **Quick Prototypes** — Rapid Windows tools

**Avoid For:**

- New enterprise applications (use C#)
- Web development
- Mobile applications
- Performance-critical systems

## Related

- [[C Sharp|C#]] — Modern .NET alternative
- [[Python]] — Current beginner-friendly language
- [[COBOL]] — Another enterprise legacy language
- [[Languages MOC]] — Overview of all language references
