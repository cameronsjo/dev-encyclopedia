---
title: Ada
aliases:
  - Ada Programming Language
  - Ada 2012
  - Ada 2022
  - SPARK Ada
tags:
  - language
  - legacy
  - safety-critical
  - defense
  - aerospace
type: reference
status: complete
created: "2025-12-16"
---

# Ada

Strongly-typed, safety-focused language designed for mission-critical and real-time systems where failure is not an option.

## Overview

| Aspect | Details |
|--------|---------|
| **Paradigm** | Procedural, OOP, concurrent, contract-based |
| **Typing** | Static, strong, nominal |
| **Memory Model** | Manual + controlled types + optional GC |
| **First Appeared** | 1980 (DoD mandate, Jean Ichbiah) |
| **Current Standard** | Ada 2022 |
| **Primary Use Cases** | Aerospace, defense, rail, medical, avionics |
| **Named After** | Ada Lovelace, first programmer |
| **Key Feature** | Built-in concurrency and contracts |

## Core Concepts

### Program Structure

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Hello is
   Name : String(1..50);
   Last : Natural;
begin
   Put("Enter your name: ");
   Get_Line(Name, Last);
   Put_Line("Hello, " & Name(1..Last) & "!");
end Hello;
```

### Strong Typing

**Distinct Types** — Same underlying representation, different types:

```ada
type Meters is new Float;
type Feet is new Float;

Distance_M : Meters := 100.0;
Distance_F : Feet := 328.0;

-- Distance_M := Distance_F;  -- Compile error! Type mismatch
Distance_M := Meters(Distance_F * 0.3048);  -- Explicit conversion
```

### Subtypes and Ranges

```ada
type Day_Number is range 1 .. 31;
type Month_Number is range 1 .. 12;
type Year_Number is range 1900 .. 2100;

subtype Working_Day is Day_Number range 1 .. 5;
subtype Percentage is Float range 0.0 .. 100.0;

-- Runtime constraint check
Today : Day_Number := 32;  -- Raises Constraint_Error
```

### Records

```ada
type Date is record
   Day   : Day_Number;
   Month : Month_Number;
   Year  : Year_Number;
end record;

type Person is record
   Name      : String(1..50);
   Birth     : Date;
   Is_Active : Boolean := True;
end record;

Employee : Person := (
   Name      => "Alice Smith" & (12..50 => ' '),
   Birth     => (Day => 15, Month => 6, Year => 1990),
   Is_Active => True
);
```

### Arrays

```ada
type Vector is array (1 .. 3) of Float;
type Matrix is array (1 .. 3, 1 .. 3) of Float;

-- Unconstrained array type
type Int_Array is array (Positive range <>) of Integer;

Numbers : Int_Array(1..100);
Identity : constant Matrix := (
   (1.0, 0.0, 0.0),
   (0.0, 1.0, 0.0),
   (0.0, 0.0, 1.0)
);
```

## Packages

### Package Specification (Interface)

```ada
-- math_utils.ads
package Math_Utils is
   type Complex is private;

   function Create(Re, Im : Float) return Complex;
   function "+"(Left, Right : Complex) return Complex;
   function Magnitude(C : Complex) return Float;

private
   type Complex is record
      Re, Im : Float;
   end record;
end Math_Utils;
```

### Package Body (Implementation)

```ada
-- math_utils.adb
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

package body Math_Utils is
   function Create(Re, Im : Float) return Complex is
   begin
      return (Re => Re, Im => Im);
   end Create;

   function "+"(Left, Right : Complex) return Complex is
   begin
      return (Re => Left.Re + Right.Re,
              Im => Left.Im + Right.Im);
   end "+";

   function Magnitude(C : Complex) return Float is
   begin
      return Sqrt(C.Re**2 + C.Im**2);
   end Magnitude;
end Math_Utils;
```

## Concurrency (Tasks)

### Built-In Concurrency

```ada
task type Worker is
   entry Start(Job_ID : Integer);
   entry Stop;
end Worker;

task body Worker is
   ID : Integer;
begin
   accept Start(Job_ID : Integer) do
      ID := Job_ID;
   end Start;

   loop
      select
         accept Stop;
         exit;
      or
         delay 1.0;
         Put_Line("Worker" & ID'Image & " working...");
      end select;
   end loop;
end Worker;
```

### Protected Objects

**Thread-Safe Data:**

```ada
protected type Shared_Counter is
   procedure Increment;
   procedure Decrement;
   function Value return Integer;
private
   Count : Integer := 0;
end Shared_Counter;

protected body Shared_Counter is
   procedure Increment is
   begin
      Count := Count + 1;
   end Increment;

   procedure Decrement is
   begin
      Count := Count - 1;
   end Decrement;

   function Value return Integer is
   begin
      return Count;
   end Value;
end Shared_Counter;
```

## Design by Contract

### Pre/Post Conditions (Ada 2012+)

```ada
function Divide(A, B : Float) return Float
   with Pre  => B /= 0.0,
        Post => Divide'Result * B = A;

procedure Push(S : in out Stack; Item : Element)
   with Pre  => not Is_Full(S),
        Post => not Is_Empty(S) and Top(S) = Item;
```

### Type Invariants

```ada
type Account is private
   with Type_Invariant => Balance(Account) >= 0.0;
```

### Subtype Predicates

```ada
subtype Even is Integer
   with Dynamic_Predicate => Even mod 2 = 0;

subtype Valid_Email is String
   with Dynamic_Predicate =>
      (for some I in Valid_Email'Range => Valid_Email(I) = '@');
```

## SPARK Ada

### Formal Verification Subset

```ada
package Stack
   with SPARK_Mode
is
   Max_Size : constant := 100;
   type Stack is private;

   function Is_Empty(S : Stack) return Boolean;
   function Is_Full(S : Stack) return Boolean;

   procedure Push(S : in out Stack; Item : Integer)
      with Pre  => not Is_Full(S),
           Post => not Is_Empty(S);

   procedure Pop(S : in out Stack; Item : out Integer)
      with Pre  => not Is_Empty(S),
           Post => not Is_Full(S);

private
   type Stack is record
      Data : array (1 .. Max_Size) of Integer;
      Top  : Natural := 0;
   end record;
end Stack;
```

**SPARK Guarantees:**

- No runtime errors (proven statically)
- No undefined behavior
- No data races
- Functional correctness proofs

## Ada Evolution

| Version | Year | Key Features |
|---------|------|--------------|
| **Ada 83** | 1983 | Original DoD standard |
| **Ada 95** | 1995 | OOP, protected types, child packages |
| **Ada 2005** | 2005 | Interfaces, containers, scheduling |
| **Ada 2012** | 2012 | Contracts, expressions, iterators |
| **Ada 2022** | 2022 | Parallel blocks, delta aggregates, 'Image |

## Real-World Usage

| Domain | Examples |
|--------|----------|
| **Aviation** | Boeing 777 fly-by-wire, Airbus |
| **Space** | Ariane rockets, ISS, Mars missions |
| **Rail** | Train control systems (TGV, metros) |
| **Defense** | Military systems worldwide |
| **Medical** | Life-critical devices |
| **Finance** | Trading systems |

## Tooling

| Tool | Purpose |
|------|---------|
| **GNAT** | Free Ada compiler (GCC-based) |
| **GNAT Studio** | IDE for Ada/SPARK |
| **GNATprove** | SPARK formal verification |
| **AdaCore** | Commercial support |
| **Alire** | Package manager |

## Comparison with Alternatives

| Aspect | Ada | Rust | C++ | Java |
|--------|-----|------|-----|------|
| **Memory Safety** | ✅ Strong typing | ✅ Borrow checker | ❌ Manual | ✅ GC |
| **Concurrency** | ✅ Native tasks | ✅ Ownership | ⚠️ Libraries | ✅ Threads |
| **Contracts** | ✅ Native | ⚠️ Limited | ⚠️ Assertions | ⚠️ Assertions |
| **Formal Verification** | ✅ SPARK | ⚠️ Tools exist | ⚠️ Limited | ⚠️ Limited |
| **Real-Time** | ✅ Ravenscar profile | ⚠️ Possible | ⚠️ Possible | ❌ GC pauses |
| **Learning Curve** | ⚠️ Steep | ⚠️ Steep | ❌ Very steep | ✅ Moderate |
| **Community Size** | ⚠️ Small | ✅ Growing | ✅ Large | ✅ Large |

## When to Use Ada

### Strengths

| Strength | Rationale |
|----------|-----------|
| **Safety** | Catches errors at compile time |
| **Reliability** | Proven in mission-critical systems |
| **Readability** | Verbose but clear |
| **Concurrency** | First-class language support |
| **Long-Term Maintenance** | 40+ year codebases |
| **Certification** | DO-178C, IEC 61508 compliance |

### Considerations

| Consideration | Impact |
|---------------|--------|
| **Verbosity** | More code than modern languages |
| **Small Community** | Fewer libraries and resources |
| **Learning Curve** | Complex type system |
| **Niche Market** | Limited outside safety-critical |

### Best For

- **Avionics** — Flight control systems
- **Space Systems** — Satellites, rockets
- **Rail** — Signaling, train control
- **Medical Devices** — Life-critical equipment
- **Defense** — Military applications
- **High-Integrity Systems** — Where failure costs lives

**Avoid For:**

- Web applications
- Mobile apps
- Rapid prototyping
- Startups (unless safety-critical)

## Related

- [[Rust]] — Modern systems language with safety focus
- [[Pascal]] — Syntactic ancestor
- [[C++]] — Alternative for systems programming
- [[Languages MOC]] — Overview of all language references
