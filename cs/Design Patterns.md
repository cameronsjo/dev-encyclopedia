---
title: Design Patterns
aliases:
  - GoF Patterns
  - Software Patterns
tags:
  - cs
  - design-patterns
  - architecture
  - oop
type: concept
status: complete
difficulty: intermediate
created: 2025-11-28
---

# Design Patterns

Reusable solutions to common software design problems.

## Overview

**Origin:** "Gang of Four" book (1994) — Gamma, Helm, Johnson, Vlissides.

**Categories:**
- **Creational** — Object creation
- **Structural** — Object composition
- **Behavioral** — Object interaction

**Modern context:** Many patterns exist to work around language limitations. Some are less relevant with modern features (lambdas, generics, etc.).

---

## Creational Patterns

### Singleton

**Intent:** Ensure only one instance exists.

**When to use:**
- Database connection pools
- Configuration managers
- Logging services

**Problems:**
- Global state
- Testing difficulties
- Hidden dependencies

**Modern alternative:** Dependency injection. Let the DI container manage lifetime.

### Factory Method

**Intent:** Let subclasses decide which class to instantiate.

**When to use:**
- Don't know exact type at compile time
- Want to delegate creation logic
- Need to return interface implementations

### Abstract Factory

**Intent:** Create families of related objects.

**When to use:**
- Multiple platforms (UI toolkit per OS)
- Theming systems
- Database providers

### Builder

**Intent:** Construct complex objects step by step.

**When to use:**
- Many constructor parameters
- Optional parameters
- Immutable objects with complex construction

**Modern:** Fluent APIs, named parameters in some languages.

### Prototype

**Intent:** Clone existing objects.

**When to use:**
- Object creation is expensive
- Need copies with slight modifications

---

## Structural Patterns

### Adapter

**Intent:** Make incompatible interfaces work together.

**When to use:**
- Integrating third-party libraries
- Legacy system integration
- API version compatibility

**Example:** Wrapping an old payment API to match your interface.

### Decorator

**Intent:** Add behavior dynamically without subclassing.

**When to use:**
- Adding features to objects at runtime
- Avoiding subclass explosion
- Middleware pipelines

**Example:** Logging decorator, caching decorator, auth decorator.

### Facade

**Intent:** Simplified interface to complex subsystem.

**When to use:**
- Hiding complexity
- Providing high-level API
- Reducing coupling to subsystem

### Proxy

**Intent:** Placeholder for another object.

**Types:**
| Type | Purpose |
|------|---------|
| Virtual | Lazy loading |
| Protection | Access control |
| Remote | Network location transparency |
| Caching | Store results |

### Composite

**Intent:** Tree structures with uniform interface.

**When to use:**
- File systems (files and folders)
- UI components (containers and elements)
- Organization hierarchies

### Bridge

**Intent:** Separate abstraction from implementation.

**When to use:**
- Platform-specific implementations
- Avoiding cartesian product of subclasses

---

## Behavioral Patterns

### Strategy

**Intent:** Define family of interchangeable algorithms.

**When to use:**
- Multiple algorithms for same task
- Algorithm selection at runtime
- Avoiding conditionals

**Modern:** Often just pass a function/lambda instead of strategy object.

### Observer

**Intent:** Notify multiple objects of state changes.

**When to use:**
- Event systems
- UI data binding
- Pub/sub messaging

**Modern:** Reactive streams (RxJS, Reactor), event emitters.

### Command

**Intent:** Encapsulate request as object.

**When to use:**
- Undo/redo functionality
- Queuing operations
- Logging/auditing
- Macro recording

### State

**Intent:** Object behavior changes with internal state.

**When to use:**
- State machines
- Workflow engines
- Document status transitions

### Template Method

**Intent:** Define algorithm skeleton, let subclasses fill steps.

**When to use:**
- Framework hooks
- Invariant algorithm with variable steps

**Modern:** Often use composition/callbacks instead of inheritance.

### Iterator

**Intent:** Sequential access without exposing internals.

**Modern:** Built into most languages (`for...of`, `foreach`, `iter()`).

### Visitor

**Intent:** Add operations to object structure without modifying it.

**When to use:**
- Compilers (AST operations)
- Document processing
- Reporting on object graphs

**Trade-off:** Adding new element types is hard; adding new operations is easy.

### Chain of Responsibility

**Intent:** Pass request along chain until handled.

**When to use:**
- Middleware pipelines
- Event bubbling
- Approval workflows

### Mediator

**Intent:** Centralize complex communications.

**When to use:**
- Chat rooms
- Air traffic control
- UI component coordination

### Memento

**Intent:** Capture and restore object state.

**When to use:**
- Undo mechanisms
- Checkpointing
- Game saves

---

## Modern Patterns

Beyond GoF, patterns that emerged later:

### Dependency Injection

**Intent:** Provide dependencies from outside rather than creating them.

**Benefits:**
- Testability (mock dependencies)
- Loose coupling
- Configuration flexibility

### Repository

**Intent:** Abstract data access behind collection-like interface.

**When to use:**
- Database access
- Hiding persistence details
- Enabling mocking

### Unit of Work

**Intent:** Track changes and commit atomically.

**When to use:**
- ORMs (DbContext, Session)
- Transaction management

### CQRS

**Intent:** Separate read and write models.

**When to use:**
- Different read/write patterns
- Event sourcing systems
- Performance optimization

### Null Object

**Intent:** Provide default behavior instead of null checks.

**Example:** `NullLogger` that discards messages vs. null checks everywhere.

---

## Anti-Patterns

Patterns to avoid:

| Anti-Pattern | Problem |
|--------------|---------|
| God Object | One class does everything |
| Spaghetti Code | Tangled, unstructured logic |
| Golden Hammer | Using one pattern for everything |
| Premature Optimization | Optimizing without profiling |
| Copy-Paste | Duplicated code instead of abstraction |

---

## When to Use Patterns

**Do use patterns when:**
- Problem clearly matches pattern intent
- Team understands the pattern
- Adds clear value (testability, flexibility)

**Don't use patterns when:**
- Simple solution works
- Pattern adds unnecessary complexity
- Just to say you used a pattern

**Remember:** Patterns are tools, not goals. The simplest solution that works is often best.

---

## Language-Specific Notes

### Functional Languages

Many OOP patterns become trivial:
- Strategy → Pass a function
- Command → Functions are commands
- Factory → Functions return values
- Template Method → Higher-order functions

### Modern OOP Languages

Some patterns are built in:
- Iterator → For-each loops, generators
- Observer → Events, reactive libraries
- Singleton → DI containers

---

## Related

- [[System Design]]
- [[Architectural Patterns]]
- [[SOLID Principles]]
