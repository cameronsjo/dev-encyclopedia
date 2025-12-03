---
title: TypeScript
aliases:
  - TS
tags:
  - language
  - web
  - javascript
type: reference
status: draft
created: '2025-12-03'
---

# TypeScript

Typed superset of JavaScript that compiles to plain JavaScript.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Multi-paradigm (OOP, functional, procedural) |
| Typing | Static, structural |
| Compiled to | JavaScript |
| First release | 2012 |
| Designed by | Microsoft |

---

## Key Features

- Static type checking
- Type inference
- Interfaces and type aliases
- Generics
- Enums
- Decorators (experimental)
- Module system (ES modules)

---

## Type System

| Feature | Example |
|---------|---------|
| Primitives | `string`, `number`, `boolean`, `null`, `undefined` |
| Arrays | `number[]` or `Array<number>` |
| Tuples | `[string, number]` |
| Union | `string \| number` |
| Intersection | `TypeA & TypeB` |
| Literal | `"hello"` or `42` |

---

## When to Use TypeScript

**Strengths:**

- Catch errors at compile time
- Better IDE support (autocomplete, refactoring)
- Self-documenting code
- Scales well for large codebases

**Considerations:**

- Build step required
- Learning curve for type system
- Some JavaScript patterns hard to type

**Best for:**

- Large applications
- Team projects
- Long-term maintenance

---

## Related

- [[React]]
- [[Angular]]
- [[Svelte]]
- [[domains/Web Development|Web Development]]
