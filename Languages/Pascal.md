---
title: Pascal
aliases:
  - Object Pascal
  - Delphi
  - Free Pascal
tags:
  - language
  - legacy
  - educational
  - structured
type: reference
status: complete
created: "2025-12-16"
---

# Pascal

Influential teaching language emphasizing structured programming, spawning Delphi and influencing generations of language design.

## Overview

| Aspect | Details |
|--------|---------|
| **Paradigm** | Procedural, structured (OOP in Object Pascal) |
| **Typing** | Static, strong |
| **Memory Model** | Manual (stack + heap) |
| **First Appeared** | 1970 (Niklaus Wirth) |
| **Primary Use Cases** | Education, desktop applications (Delphi) |
| **Notable Implementations** | Turbo Pascal, Delphi, Free Pascal, Lazarus |
| **Design Goal** | Teaching good programming practices |

## Core Concepts

### Program Structure

```pascal
program HelloWorld;
uses
    SysUtils;
var
    name: string;
begin
    Write('Enter your name: ');
    ReadLn(name);
    WriteLn('Hello, ', name, '!');
    WriteLn('Today is ', DateToStr(Date));
end.
```

### Structure Overview

| Section | Purpose |
|---------|---------|
| **program** | Program name declaration |
| **uses** | Unit imports |
| **const** | Constants |
| **type** | Type definitions |
| **var** | Variable declarations |
| **begin...end** | Main code block |

### Data Types

**Built-in Types:**

| Type | Description |
|------|-------------|
| **Integer** | Whole numbers |
| **Real** | Floating-point |
| **Char** | Single character |
| **String** | Character sequence |
| **Boolean** | True/False |
| **Array** | Fixed-size collection |
| **Record** | Structured data |
| **Set** | Collection of values |
| **Pointer** | Memory address |

### Type Definitions

```pascal
type
    TMonth = 1..12;                    { Subrange }
    TDays = (Mon, Tue, Wed, Thu, Fri); { Enumeration }
    TName = string[50];                { Bounded string }
    TMatrix = array[1..10, 1..10] of Real;

    TPerson = record
        Name: string;
        Age: Integer;
        Active: Boolean;
    end;
```

### Procedures and Functions

```pascal
{ Procedure - no return value }
procedure Greet(name: string);
begin
    WriteLn('Hello, ', name);
end;

{ Function - returns a value }
function Factorial(n: Integer): Integer;
begin
    if n <= 1 then
        Result := 1
    else
        Result := n * Factorial(n - 1);
end;

{ Var parameter - pass by reference }
procedure Swap(var a, b: Integer);
var
    temp: Integer;
begin
    temp := a;
    a := b;
    b := temp;
end;
```

### Control Structures

```pascal
{ If statement }
if x > 0 then
    WriteLn('Positive')
else if x < 0 then
    WriteLn('Negative')
else
    WriteLn('Zero');

{ Case statement }
case grade of
    'A': WriteLn('Excellent');
    'B': WriteLn('Good');
    'C': WriteLn('Average');
else
    WriteLn('Below average');
end;

{ For loop }
for i := 1 to 10 do
    WriteLn(i);

{ While loop }
while not EOF(inputFile) do
begin
    ReadLn(inputFile, line);
    ProcessLine(line);
end;

{ Repeat-until }
repeat
    ReadLn(value);
until value > 0;
```

## Pascal Evolution

| Version | Year | Key Features |
|---------|------|--------------|
| **Original Pascal** | 1970 | Structured programming, strong typing |
| **UCSD Pascal** | 1978 | Units, p-code portability |
| **Turbo Pascal** | 1983 | Fast compilation, integrated IDE |
| **Object Pascal** | 1986 | Object-oriented extensions |
| **Delphi** | 1995 | RAD, VCL, Windows development |
| **Free Pascal** | 1997 | Open source, cross-platform |
| **Delphi 2009** | 2008 | Unicode, generics |
| **Delphi 11+** | 2021+ | Cross-platform (FMX), modern features |

## Object Pascal (Delphi)

### Class Definition

```pascal
type
    TAnimal = class
    private
        FName: string;
    protected
        procedure SetName(const Value: string);
    public
        constructor Create(const AName: string);
        destructor Destroy; override;
        property Name: string read FName write SetName;
        procedure Speak; virtual; abstract;
    end;

    TDog = class(TAnimal)
    public
        procedure Speak; override;
    end;

implementation

constructor TAnimal.Create(const AName: string);
begin
    inherited Create;
    FName := AName;
end;

procedure TDog.Speak;
begin
    WriteLn(FName, ' says: Woof!');
end;
```

### Interfaces

```pascal
type
    ILogger = interface
        ['{GUID-HERE}']
        procedure Log(const Message: string);
    end;

    TFileLogger = class(TInterfacedObject, ILogger)
    public
        procedure Log(const Message: string);
    end;
```

### Generics (Delphi 2009+)

```pascal
type
    TStack<T> = class
    private
        FItems: array of T;
    public
        procedure Push(const Item: T);
        function Pop: T;
    end;

var
    IntStack: TStack<Integer>;
    StrStack: TStack<string>;
```

## Delphi VCL/FMX

### Visual Component Library

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
    ShowMessage('Hello, ' + Edit1.Text);
    ListBox1.Items.Add(Edit1.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    Caption := 'My Application';
    Edit1.Text := '';
end;
```

### Key Components

| Component | Purpose |
|-----------|---------|
| **TForm** | Window/dialog |
| **TButton** | Clickable button |
| **TEdit** | Text input field |
| **TLabel** | Static text |
| **TListBox** | Scrollable list |
| **TDataSet** | Database access |
| **TTimer** | Scheduled events |

## Implementations

| Implementation | License | Platforms | Notes |
|----------------|---------|-----------|-------|
| **Delphi** | Commercial | Windows, macOS, iOS, Android, Linux | RAD Studio, VCL/FMX |
| **Free Pascal** | GPL | 20+ platforms | Compiler-only |
| **Lazarus** | GPL | Cross-platform | Free Pascal + IDE |
| **PascalABC.NET** | Free | Windows (.NET) | Educational |
| **Turbo Pascal** | Historical | DOS | Borland (discontinued) |

## Comparison with Alternatives

| Aspect | Pascal/Delphi | C# | Java |
|--------|---------------|-----|------|
| **Compilation** | Native | IL/JIT | Bytecode/JIT |
| **Memory** | Manual + ARC | GC | GC |
| **Performance** | ✅ Excellent native | ✅ Good | ✅ Good |
| **RAD/GUI** | ✅ VCL/FMX | ✅ WinForms/WPF | ⚠️ Swing/JavaFX |
| **Community Size** | ⚠️ Small | ✅ Large | ✅ Large |
| **Learning Resources** | ⚠️ Limited | ✅ Abundant | ✅ Abundant |
| **Job Market** | ⚠️ Niche | ✅ Strong | ✅ Strong |

## Design Influence

Pascal influenced many languages:

| Language | Influence |
|----------|-----------|
| **Modula-2** | Direct successor by Wirth |
| **Ada** | Syntax and structure |
| **Oberon** | Simplified Pascal by Wirth |
| **Java** | Some syntax elements |
| **TypeScript** | `type` keyword, enum syntax |

## When to Use

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Readability** | English-like, structured code |
| **Strong Typing** | Catches errors at compile time |
| **Fast Compilation** | Quick edit-compile-run cycle |
| **Native Performance** | No VM overhead |
| **RAD (Delphi)** | Rapid Windows development |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Small Community** | Fewer libraries, resources |
| **Niche Market** | Limited job opportunities |
| **Commercial Tools** | Delphi is expensive |
| **Platform Focus** | Historically Windows-centric |

### Best For

- **Legacy Delphi Apps** — Maintaining existing systems
- **Desktop Applications** — Windows business software
- **Teaching** — Structured programming concepts
- **Embedded (Free Pascal)** — Resource-constrained systems

**Avoid For:**

- Web development (prefer TypeScript, C#)
- Mobile-first development (prefer Kotlin, Swift)
- New enterprise projects (prefer C#, Java)
- Machine learning (prefer Python)

## Related

- [[C Sharp|C#]] — Modern alternative with similar RAD capabilities
- [[BASIC]] — Contemporary educational language
- [[Ada]] — Pascal-influenced safety-critical language
- [[Languages MOC]] — Overview of all language references
