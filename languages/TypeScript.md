---
title: TypeScript
aliases:
  - TS
tags:
  - language
  - web
  - javascript
  - typescript
type: reference
status: complete
created: '2025-12-03'
---

# TypeScript

Typed superset of JavaScript. Compiles to JS. Industry standard for large codebases.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Multi-paradigm (OOP, functional) |
| Typing | Static, structural, inferred |
| Compiles to | JavaScript (any version) |
| Runtime | Node.js, browsers, Deno, Bun |
| First appeared | 2012 (Microsoft) |
| Current | 5.x |

---

## Type System

### Basic Types

```typescript
// Primitives
let name: string = "Alice";
let age: number = 30;
let active: boolean = true;

// Arrays
let items: string[] = ["a", "b"];
let numbers: Array<number> = [1, 2, 3];

// Tuples
let pair: [string, number] = ["age", 30];
```

### Union & Intersection

```typescript
// Union: one of several types
type ID = string | number;

// Intersection: combine types
type Employee = Person & { employeeId: number };
```

### Type Inference

TypeScript infers types when obvious.

```typescript
let x = 10;           // inferred as number
const y = "hello";    // inferred as "hello" (literal type)
```

### Interfaces vs Types

| Feature | Interface | Type Alias |
|---------|-----------|------------|
| Extend | `extends` | `&` intersection |
| Declaration merging | Yes | No |
| Computed properties | No | Yes |
| Primitives/unions | No | Yes |

**Convention:** Interfaces for objects, types for unions/utilities.

```typescript
interface User {
  name: string;
  age: number;
}

type Status = "active" | "inactive" | "pending";
```

---

## Advanced Types

### Generics

```typescript
function first<T>(arr: T[]): T | undefined {
  return arr[0];
}

// Generic constraints
function getLength<T extends { length: number }>(item: T): number {
  return item.length;
}
```

### Utility Types

| Type | Description |
|------|-------------|
| `Partial<T>` | All properties optional |
| `Required<T>` | All properties required |
| `Readonly<T>` | All properties readonly |
| `Pick<T, K>` | Select specific properties |
| `Omit<T, K>` | Remove specific properties |
| `Record<K, V>` | Object with key type K, value type V |
| `ReturnType<F>` | Extract function return type |

```typescript
interface User {
  id: number;
  name: string;
  email: string;
}

type UserPreview = Pick<User, "id" | "name">;
type UpdateUser = Partial<Omit<User, "id">>;
```

### Conditional Types

```typescript
type IsString<T> = T extends string ? true : false;

// Infer keyword
type ReturnType<T> = T extends (...args: any[]) => infer R ? R : never;
```

### Template Literal Types

```typescript
type EventName = `on${Capitalize<string>}`;
type Getter<T extends string> = `get${Capitalize<T>}`;
```

---

## Modern Features

### Satisfies Operator (4.9+)

Type check without widening.

```typescript
const config = {
  apiUrl: "https://api.example.com",
  timeout: 5000,
} satisfies Record<string, string | number>;
// config.apiUrl is still string, not string | number
```

### const Assertions

```typescript
const routes = ["home", "about", "contact"] as const;
// Type: readonly ["home", "about", "contact"]
```

### Discriminated Unions

```typescript
type Result<T> =
  | { success: true; data: T }
  | { success: false; error: string };

function handle(result: Result<User>) {
  if (result.success) {
    console.log(result.data); // TypeScript knows data exists
  } else {
    console.log(result.error); // TypeScript knows error exists
  }
}
```

---

## Configuration

### tsconfig.json Essentials

```json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "esModuleInterop": true,
    "skipLibCheck": true
  }
}
```

### Strict Mode Options

| Option | Effect |
|--------|--------|
| `strict` | Enable all strict checks |
| `strictNullChecks` | null/undefined are separate types |
| `noImplicitAny` | Error on implicit any |
| `strictFunctionTypes` | Stricter function type checking |

**Recommendation:** Always use `strict: true` for new projects.

---

## Ecosystem

### Runtimes

| Runtime | Notes |
|---------|-------|
| Node.js | Requires compilation or ts-node |
| Deno | Native TypeScript support |
| Bun | Native TypeScript support |
| Browsers | Compile to JS first |

### Build Tools

| Tool | Use Case |
|------|----------|
| tsc | Official compiler |
| esbuild | Fast bundling |
| swc | Fast compilation (Rust) |
| Vite | Dev server + bundling |
| tsup | Library bundling |

### Type Definitions

```bash
# DefinitelyTyped - community types
npm install --save-dev @types/node
npm install --save-dev @types/react
```

---

## TypeScript with Frameworks

### React

```typescript
// Props typing
interface ButtonProps {
  label: string;
  onClick: () => void;
  disabled?: boolean;
}

function Button({ label, onClick, disabled }: ButtonProps) {
  return <button onClick={onClick} disabled={disabled}>{label}</button>;
}

// Hooks
const [count, setCount] = useState<number>(0);
const inputRef = useRef<HTMLInputElement>(null);
```

### Node.js/Express

```typescript
import express, { Request, Response } from "express";

interface CreateUserBody {
  name: string;
  email: string;
}

app.post("/users", (req: Request<{}, {}, CreateUserBody>, res: Response) => {
  const { name, email } = req.body;
  // ...
});
```

---

## Best Practices

### Do

- Enable strict mode
- Use `unknown` over `any`
- Prefer interfaces for public APIs
- Use discriminated unions for state
- Let TypeScript infer when obvious

### Avoid

- Overusing `any`
- Type assertions (`as`) without validation
- Overly complex conditional types
- Ignoring compiler errors with `// @ts-ignore`

### Type Guards

```typescript
function isUser(obj: unknown): obj is User {
  return (
    typeof obj === "object" &&
    obj !== null &&
    "name" in obj &&
    "email" in obj
  );
}
```

---

## When to Use TypeScript

**Strengths:**

- Catch errors at compile time
- Excellent IDE support
- Self-documenting code
- Refactoring confidence
- Industry standard

**Considerations:**

- Build step required
- Learning curve
- Some dynamic JS patterns hard to type
- Type definition maintenance

**Best for:**

- Any JavaScript project > 1000 LOC
- Team projects
- Libraries and frameworks
- Long-term maintenance

---

## Related

- [[React]]
- [[Angular]]
- [[Vue]]
- [[Next.js]]
- [[domains/Web Development|Web Development]]
