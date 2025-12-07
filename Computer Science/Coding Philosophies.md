---
title: Coding Philosophies
aliases:
  - Clean Code
  - Coding Styles
  - Programming Philosophies
tags:
  - cs
  - fundamentals
  - best-practices
  - reference
type: reference
status: complete
created: "2025-12-07"
---

# Coding Philosophies

Different schools of thought on how to write good code, from enterprise patterns to kernel hacking.

## Overview

| Philosophy | Key Proponent | Core Idea | Context |
|------------|---------------|-----------|---------|
| **Clean Code** | Robert C. Martin | Readable code through small functions, good names | Enterprise/OOP |
| **Refactoring** | Martin Fowler | Improve structure without changing behavior | All codebases |
| **Linux Kernel Style** | Linus Torvalds | Simple, obvious, performant C | Systems programming |
| **Pragmatic** | Hunt & Thomas | Trade-offs matter, context is king | General |
| **Functional** | Various | Immutability, composition, pure functions | FP languages |

## Clean Code (Robert C. Martin)

Popularized through "Clean Code" (2008). Influential in enterprise Java/.NET circles.

### Core Principles

| Principle | Description |
|-----------|-------------|
| **Meaningful names** | Variables/functions should reveal intent |
| **Small functions** | Functions should do one thing, be <20 lines |
| **Single Responsibility** | Classes should have one reason to change |
| **DRY** | Don't Repeat Yourself |
| **Boy Scout Rule** | Leave code cleaner than you found it |
| **Comments are failures** | Code should be self-documenting |

### The SOLID Principles

| Principle | Meaning | Practical Impact |
|-----------|---------|------------------|
| **S**ingle Responsibility | One class, one reason to change | Smaller, focused classes |
| **O**pen-Closed | Open for extension, closed for modification | Use interfaces/inheritance |
| **L**iskov Substitution | Subtypes must be substitutable for base types | Don't violate contracts |
| **I**nterface Segregation | Many specific interfaces > one general interface | Avoid fat interfaces |
| **D**ependency Inversion | Depend on abstractions, not concretions | Inject dependencies |

### Criticisms

| Criticism | Argument |
|-----------|----------|
| **Over-abstraction** | Leads to "enterprise FizzBuzz" — simple problems buried in interfaces |
| **Dogmatic application** | Rules applied without considering context |
| **Java-centric** | Many principles don't translate well to other paradigms |
| **Function size obsession** | Arbitrary line limits can harm readability |
| **Performance blind** | Abstractions have costs; ignored in enterprise contexts |

**Example of over-engineering:**

```java
// "Clean" version with abstractions
interface NumberProcessor { int process(int n); }
class IncrementProcessor implements NumberProcessor {
    public int process(int n) { return n + 1; }
}
class ProcessorFactory {
    public NumberProcessor createIncrementProcessor() {
        return new IncrementProcessor();
    }
}

// What it replaced
n + 1
```

### When It's Useful

- Large teams needing consistency
- Long-lived enterprise codebases
- Onboarding junior developers
- Code that changes frequently

### When to Be Skeptical

- Performance-critical code
- Small scripts or utilities
- Functional programming contexts
- When it adds complexity without benefit

## Martin Fowler's Approach

Chief Scientist at ThoughtWorks. Author of "Refactoring" (1999), "Patterns of Enterprise Application Architecture" (2002).

### Refactoring

Improving code structure without changing behavior.

| Refactoring | Before | After |
|-------------|--------|-------|
| **Extract Method** | Long function | Smaller functions with clear names |
| **Rename** | `d` | `elapsedDays` |
| **Extract Class** | God class | Focused classes |
| **Replace Conditional with Polymorphism** | Switch statements | Method dispatch |
| **Introduce Parameter Object** | Many params | Single object |

**Key insight:** Refactoring is continuous, not a separate phase. Small, safe changes compound.

### Code Smells

Indicators that code might need refactoring:

| Smell | Description |
|-------|-------------|
| **Long Method** | Function doing too much |
| **Large Class** | Class with too many responsibilities |
| **Primitive Obsession** | Using primitives instead of small objects |
| **Feature Envy** | Method more interested in other class's data |
| **Data Clumps** | Same group of data appearing together |
| **Shotgun Surgery** | One change requires editing many classes |
| **Divergent Change** | One class changed for multiple reasons |

### Enterprise Patterns

| Pattern | Use Case |
|---------|----------|
| **Repository** | Abstract data access |
| **Unit of Work** | Track changes for transaction |
| **Domain Model** | Rich business logic objects |
| **Service Layer** | Coordinate domain operations |
| **Data Transfer Object** | Move data between layers |

### Fowler's Pragmatism

Unlike dogmatic approaches, Fowler emphasizes:

- **"It depends"** — Context matters
- **Trade-offs** — Every pattern has costs
- **Evolutionary design** — Don't over-architect upfront
- **Technical debt** — Sometimes it's okay, but track it

## Linux Kernel Style (Linus Torvalds)

Systems programming philosophy from Linux development.

### Core Principles

| Principle | Description |
|-----------|-------------|
| **Simplicity** | Simple, stupid code over clever abstractions |
| **Obviousness** | Code should be immediately understandable |
| **Performance** | Abstractions have costs; measure everything |
| **Practicality** | Working code beats elegant theory |
| **Direct communication** | Say what you mean, bluntly |

### Kernel Coding Style

```c
// Tabs for indentation (8 spaces wide)
// Forces you to keep nesting shallow

// Function names: lowercase with underscores
void good_function_name(void);
void BadFunctionName(void);  // No

// Braces: opening brace on same line (except functions)
if (condition) {
    do_something();
}

// But functions: opening brace on new line
int function(void)
{
    return 0;
}
```

### Philosophy in Practice

**On abstraction:**

> "Bad programmers worry about the code. Good programmers worry about data structures and their relationships."

**On complexity:**

> "Controlling complexity is the essence of computer programming."

**On debugging:**

> "Given enough eyeballs, all bugs are shallow." (Linus's Law)

### Criticisms of "Enterprise" Style

From kernel perspective:

| Enterprise Practice | Kernel Criticism |
|--------------------|------------------|
| Deep class hierarchies | Adds indirection, harms understanding |
| Design patterns everywhere | Patterns are not goals |
| Getters/setters for everything | Just make the field public if that's what you mean |
| "Future-proofing" | You aren't gonna need it (YAGNI) |

### When It's Useful

- Systems programming (kernels, drivers, embedded)
- Performance-critical code
- C and low-level languages
- Small, focused projects

## Pragmatic Programming

From "The Pragmatic Programmer" (Hunt & Thomas, 1999).

### Key Principles

| Principle | Description |
|-----------|-------------|
| **DRY** | Don't Repeat Yourself — knowledge in one place |
| **Orthogonality** | Components shouldn't affect each other |
| **Reversibility** | Design for change; avoid irreversible decisions |
| **Tracer Bullets** | Build end-to-end skeleton first |
| **Prototypes** | Throw-away code to learn |
| **Domain Languages** | Write code in problem domain terms |
| **Estimate** | Learn to estimate time and resources |

### Good Enough Software

> "Great software today is often preferable to perfect software tomorrow."

- Ship iteratively
- Get feedback early
- Know when to stop polishing

### The Broken Window Theory

One piece of bad code invites more. Fix broken windows (bad code) immediately or the whole neighborhood (codebase) degrades.

## Functional Programming Style

Different philosophy from OOP-centric clean code.

### Core Principles

| Principle | Description |
|-----------|-------------|
| **Immutability** | Data doesn't change; create new versions |
| **Pure functions** | Same input → same output, no side effects |
| **Composition** | Build complex from simple via function composition |
| **Data > Objects** | Data structures + functions, not objects with behavior |
| **Declarative** | Describe what, not how |

### FP vs Clean Code Tension

| Clean Code Says | FP Says |
|-----------------|---------|
| Use objects to encapsulate | Use data + pure functions |
| Dependency injection | Partial application, closures |
| Design patterns | Higher-order functions eliminate most patterns |
| Class hierarchies | Algebraic data types + pattern matching |
| Mutable state is okay if encapsulated | Immutability by default |

### Example: Strategy Pattern

```java
// OOP Strategy Pattern
interface PaymentStrategy { void pay(int amount); }
class CreditCard implements PaymentStrategy { ... }
class PayPal implements PaymentStrategy { ... }

class ShoppingCart {
    private PaymentStrategy strategy;
    void checkout() { strategy.pay(total); }
}
```

```haskell
-- FP: Just pass a function
checkout :: (Int -> IO ()) -> Int -> IO ()
checkout paymentFn total = paymentFn total

-- Usage
checkout creditCardPay 100
checkout paypalPay 100
```

The pattern disappears — it's just a function parameter.

## Comparing Philosophies

### By Context

| Context | Recommended Approach |
|---------|---------------------|
| **Large enterprise team** | Clean Code principles (with judgment) |
| **Systems/kernel code** | Linux kernel style |
| **Startup/small team** | Pragmatic, ship fast |
| **Functional language** | FP principles |
| **Performance critical** | Measure first, optimize with intent |
| **Throwaway script** | Whatever works |

### What Everyone Agrees On

| Principle | Universal? |
|-----------|-----------|
| **Meaningful names** | ✅ Yes |
| **Avoid duplication** | ✅ Yes (mostly) |
| **Test your code** | ✅ Yes |
| **Version control** | ✅ Yes |
| **Code review** | ✅ Yes |
| **Small functions** | ⚠️ Depends on context |
| **Comments** | ⚠️ Debated |
| **Abstractions** | ⚠️ Trade-offs |

### What's Actually Debated

| Topic | Range of Opinions |
|-------|-------------------|
| **Function length** | 4 lines (Uncle Bob) → "as long as needed" (Torvalds) |
| **Comments** | "Code smell" → "Essential documentation" |
| **OOP vs FP** | Objects everywhere → Functions everywhere |
| **DRY extremism** | "Never repeat" → "Some duplication is fine" |
| **Testing** | 100% coverage → Test what matters |

## Practical Guidelines

### What Actually Helps

1. **Consistent style** — Pick one and stick to it
2. **Clear names** — Time spent naming is well spent
3. **Appropriate abstraction** — Not too much, not too little
4. **Tests for important paths** — Don't need 100% coverage
5. **Regular refactoring** — Small, continuous improvements
6. **Code review** — Fresh eyes catch issues

### Red Flags

| Warning Sign | Problem |
|--------------|---------|
| Following rules without understanding why | Cargo culting |
| Every class has interface + impl | Over-abstraction |
| Can't explain code to junior dev | Too clever |
| Premature optimization | Wrong priorities |
| Premature abstraction | Also wrong priorities |
| "Best practices" without context | Dogma |

## Related

- [[CS Fundamentals Glossary]]
- [[Design Patterns]]
- [[Testing Strategies]]
- [[Code Review]]

## References

- Fowler, M. "Refactoring: Improving the Design of Existing Code"
- Hunt & Thomas. "The Pragmatic Programmer"
- Martin, R. "Clean Code" (read critically)
- [Linux Kernel Coding Style](https://www.kernel.org/doc/html/latest/process/coding-style.html)
- [Write Code That Is Easy to Delete](https://programmingisterrible.com/post/139222674273/write-code-that-is-easy-to-delete-not-easy-to)
