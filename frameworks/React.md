---
title: React
aliases:
  - React.js
  - ReactJS
tags:
  - framework
  - web
  - frontend
  - javascript
  - typescript
  - meta
type: reference
status: complete
created: '2025-11-28'
---

# React

Component-based UI library for building web interfaces.

## Overview

| Aspect | Details |
|--------|---------|
| Type | UI library (not full framework) |
| Language | JavaScript/TypeScript |
| Rendering | Virtual DOM, RSC |
| Architecture | Component-based, unidirectional data flow |
| First release | 2013 |
| Backing | Meta (Facebook) |

---

## Core Concepts

### Components

Building blocks of React UIs.

```jsx
function Greeting({ name }) {
  return <h1>Hello, {name}!</h1>;
}
```

### JSX

HTML-like syntax in JavaScript.

```jsx
const element = <div className="container">{content}</div>;
```

### Hooks

State and lifecycle in functional components.

| Hook | Purpose |
|------|---------|
| useState | Component state |
| useEffect | Side effects |
| useContext | Context consumption |
| useReducer | Complex state logic |
| useRef | Mutable references |
| useMemo | Memoized values |
| useCallback | Memoized callbacks |

---

## State Management

### Built-in Options

| Approach | Use Case |
|----------|----------|
| useState | Local component state |
| useReducer | Complex local state |
| Context | Prop drilling avoidance |
| URL state | Shareable state |

### External Libraries

| Library | Philosophy |
|---------|------------|
| Redux Toolkit | Centralized store, actions |
| Zustand | Minimal, hooks-based |
| Jotai | Atomic state |
| Recoil | Atomic with graphs |
| TanStack Query | Server state |

### When to Use What

| Scenario | Recommendation |
|----------|----------------|
| Simple app | useState + Context |
| Complex client state | Zustand or Redux |
| Server data | TanStack Query |
| Forms | React Hook Form |

---

## React Ecosystem

### Meta-Frameworks

| Framework | Focus |
|-----------|-------|
| Next.js | Full-stack, SSR, RSC |
| Remix | Web standards, forms |
| Gatsby | Static sites, GraphQL |

**Recommendation:** Use a framework. Plain React for SPAs only.

### Routing

| Library | Notes |
|---------|-------|
| React Router | Standard SPA routing |
| TanStack Router | Type-safe, newer |
| Next.js | Built-in file routing |

### Styling

| Approach | Libraries |
|----------|-----------|
| CSS Modules | Built-in isolation |
| Tailwind CSS | Utility classes |
| Styled Components | CSS-in-JS |
| Emotion | CSS-in-JS |
| Vanilla Extract | Zero-runtime CSS |

### Forms

| Library | Approach |
|---------|----------|
| React Hook Form | Uncontrolled, performant |
| Formik | Controlled, mature |
| Zod | Validation (pair with RHF) |

---

## React Server Components

### RSC Model

Server-rendered components that don't ship JS to client.

```
┌─────────────────────────────────────────┐
│           Server Components             │
│    (Data fetching, no interactivity)    │
└────────────────┬────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────┐
│           Client Components             │
│      (Interactivity, event handlers)    │
└─────────────────────────────────────────┘
```

### Key Distinctions

| Aspect | Server Components | Client Components |
|--------|-------------------|-------------------|
| Directive | Default (no directive) | `'use client'` |
| Interactivity | ❌ | ✅ |
| Hooks | ❌ | ✅ |
| Browser APIs | ❌ | ✅ |
| Data fetching | Direct (async) | Via hooks/libraries |
| Bundle size | Zero | Included |

### Best Practices

- Start with Server Components
- Add `'use client'` only when needed
- Push client boundaries down the tree
- Keep interactive islands small

---

## Performance

### Common Optimizations

| Technique | Purpose |
|-----------|---------|
| React.memo | Skip re-renders |
| useMemo | Cache computed values |
| useCallback | Stable function references |
| lazy/Suspense | Code splitting |
| Virtualization | Large lists |

### When NOT to Optimize

- Don't memoize everything
- Profile first, optimize second
- React is already fast for most cases

### Profiling Tools

- React DevTools Profiler
- Chrome Performance tab
- why-did-you-render library

---

## Testing

### Libraries

| Library | Purpose |
|---------|---------|
| Vitest / Jest | Test runner |
| React Testing Library | Component testing |
| Playwright | E2E testing |
| MSW | API mocking |

### Philosophy

- Test behavior, not implementation
- Query by accessibility (role, label)
- Avoid testing internal state

---

## React 19 Features

| Feature | Description |
|---------|-------------|
| Actions | Form handling with useActionState |
| use() | Suspense-enabled data reading |
| Compiler | Automatic memoization (experimental) |
| Document metadata | Built-in title/meta |

---

## React vs Alternatives

| Aspect | React | Vue | Svelte | Angular |
|--------|-------|-----|--------|---------|
| Size | Medium | Small | Tiny (compiled) | Large |
| Learning | Moderate | Easy | Easy | Steep |
| Flexibility | High (library) | Medium | Medium | Low (opinionated) |
| Ecosystem | Massive | Large | Growing | Built-in |
| Jobs | Most | Many | Growing | Enterprise |

---

## Common Patterns

### Composition over Inheritance

```jsx
// ✅ Composition
<Card>
  <CardHeader />
  <CardBody>{children}</CardBody>
</Card>

// ❌ Inheritance
class SpecialCard extends Card { ... }
```

### Render Props / Children as Function

For sharing logic between components.

### Custom Hooks

Extract and reuse stateful logic.

```jsx
function useDebounce(value, delay) {
  const [debounced, setDebounced] = useState(value);

  useEffect(() => {
    const timer = setTimeout(() => setDebounced(value), delay);
    return () => clearTimeout(timer);
  }, [value, delay]);

  return debounced;
}
```

---

## When to Use React

**Strengths:**

- Massive ecosystem and community
- Excellent job market
- Flexible (library, not framework)
- Great TypeScript support
- Meta-frameworks (Next.js) are powerful

**Considerations:**

- "Just a library" — need to choose everything
- Many ways to do same thing
- Bundle size if not careful
- Frequent ecosystem changes

**Best for:**

- Teams with existing React experience
- Complex, interactive applications
- When Next.js/Remix fit requirements
- Long-term projects (stability)

---

## Related

- [[Vue]]
- [[Angular]]
- [[Svelte]]
- [[TypeScript]]
- [[domains/Web Development|Web Development]]
