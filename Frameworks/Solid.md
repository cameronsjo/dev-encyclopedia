---
title: Solid.js
aliases:
  - SolidJS
  - Solid
tags:
  - web
  - frontend
  - framework
  - javascript
  - typescript
type: reference
status: complete
created: 2025-11-30
---

# Solid.js

A declarative JavaScript library for building user interfaces with fine-grained reactivity and no virtual DOM.

## Overview

| Aspect | Details |
|--------|---------|
| **Type** | Frontend UI Library |
| **Language** | JavaScript/TypeScript |
| **Paradigm** | Reactive, Component-Based |
| **Reactivity** | Fine-grained signals (not VDOM) |
| **Templating** | JSX (compiled to real DOM) |
| **Bundle Size** | ~7KB minified+gzipped |
| **First Release** | 2018 |
| **Backing** | Ryan Carniato + Community |
| **License** | MIT |

## Core Concepts

### Fine-Grained Reactivity

Solid uses signals for reactive state management. Changes propagate directly to affected DOM nodes without reconciliation or diffing.

**Signals** — Reactive primitive holding a value:

```javascript
const [count, setCount] = createSignal(0);
console.log(count());  // Read: 0
setCount(5);           // Write: 5
```

**Effects** — Code that runs when dependencies change:

```javascript
createEffect(() => {
  console.log('Count is:', count());
});
```

**Memos** — Cached derived values:

```javascript
const doubled = createMemo(() => count() * 2);
```

### JSX Without Virtual DOM

Solid compiles JSX directly to efficient DOM operations. Components run once, reactive expressions update specific DOM nodes.

```jsx
function Counter() {
  const [count, setCount] = createSignal(0);

  return (
    <div>
      <p>Count: {count()}</p>
      <button onClick={() => setCount(count() + 1)}>Increment</button>
    </div>
  );
}
```

**Key difference from React:** JSX expressions are reactive subscriptions, not function calls on every render.

### Components as Functions

Components execute once during mount. Reactive primitives handle updates automatically.

```jsx
function Timer() {
  const [elapsed, setElapsed] = createSignal(0);

  const interval = setInterval(() => setElapsed(e => e + 1), 1000);
  onCleanup(() => clearInterval(interval));

  return <div>Elapsed: {elapsed()}s</div>;
}
```

### Control Flow Components

Solid provides optimized control flow components instead of `map()` or ternaries:

```jsx
<Show when={user()} fallback={<Login />}>
  <h1>Welcome {user().name}</h1>
</Show>

<For each={items()}>
  {(item, index) => <li>{item.name}</li>}
</For>

<Switch fallback={<NotFound />}>
  <Match when={state.route === 'home'}><Home /></Match>
  <Match when={state.route === 'about'}><About /></Match>
</Switch>
```

## State Management

### Stores

Nested reactive objects for complex state:

```javascript
import { createStore } from 'solid-js/store';

const [state, setState] = createStore({
  user: { name: 'Alice', age: 30 },
  todos: []
});

// Immutable updates
setState('user', 'age', 31);
setState('todos', todos => [...todos, newTodo]);
```

Store properties are accessed without calling as functions:

```jsx
<p>{state.user.name}</p>  // Not state.user.name()
```

### Context API

Share state across component trees:

```jsx
const CounterContext = createContext();

function App() {
  const [count, setCount] = createSignal(0);

  return (
    <CounterContext.Provider value={[count, setCount]}>
      <Child />
    </CounterContext.Provider>
  );
}

function Child() {
  const [count, setCount] = useContext(CounterContext);
  return <button onClick={() => setCount(c => c + 1)}>{count()}</button>;
}
```

## Routing

**Solid Router** — File-based and component-based routing:

```jsx
import { Router, Route, Routes } from '@solidjs/router';

function App() {
  return (
    <Router>
      <Routes>
        <Route path="/" component={Home} />
        <Route path="/about" component={About} />
        <Route path="/users/:id" component={User} />
      </Routes>
    </Router>
  );
}
```

Navigation:

```jsx
import { A, useNavigate, useParams } from '@solidjs/router';

<A href="/about">About</A>

const navigate = useNavigate();
navigate('/home');

const params = useParams();
console.log(params.id);
```

## Server-Side Rendering

**SolidStart** — Meta-framework for SSR, streaming, and islands:

```jsx
// routes/index.tsx
export default function Home() {
  const data = createServerData$(async () => {
    return await fetchData();
  });

  return <div>Data: {data()}</div>;
}
```

**Features:**

- File-based routing
- Server functions with `server$`
- Streaming SSR
- Islands architecture
- Deployment adapters (Node, Vercel, Netlify, Cloudflare)

## Lifecycle & Resources

**Lifecycle hooks:**

```javascript
onMount(() => console.log('Component mounted'));
onCleanup(() => console.log('Cleanup before disposal'));
```

**Resources** — Async data loading:

```jsx
const [data] = createResource(userId, fetchUser);

<Show when={data()} fallback={<Loading />}>
  <User data={data()} />
</Show>
```

## Comparison with React

| Feature | Solid.js | React |
|---------|----------|-------|
| **Reactivity** | Fine-grained signals | Virtual DOM diffing |
| **Re-renders** | None (surgical updates) | Component re-execution |
| **JSX Execution** | Once per component | Every render |
| **Reading State** | `count()` function call | `count` direct access |
| **Bundle Size** | ~7KB | ~40KB (+ React DOM) |
| **Performance** | Faster (no reconciliation) | Fast (optimized VDOM) |
| **Control Flow** | `<Show>`, `<For>` components | `&&`, `map()` expressions |
| **Learning Curve** | Moderate (new mental model) | Moderate (hooks, effects) |
| **Ecosystem** | Growing | Massive |
| **TypeScript** | Excellent | Excellent |

**Migration notes:**

- No `useState` — use `createSignal`
- No `useEffect` — use `createEffect`
- No `useMemo` — use `createMemo`
- No dependency arrays (automatic tracking)
- State reads require function calls: `count()` not `count`

## When to Use

### Strengths

✅ **Performance-critical apps** — No VDOM overhead, minimal runtime
✅ **Real-time interfaces** — Fine-grained reactivity handles rapid updates efficiently
✅ **Small bundle requirements** — Tiny core library
✅ **TypeScript projects** — First-class TS support
✅ **Server-side rendering** — SolidStart provides modern SSR/streaming
✅ **Developer experience** — Familiar JSX syntax, predictable reactivity

### Considerations

❌ **Ecosystem maturity** — Smaller than React/Vue (fewer third-party libraries)
❌ **Team familiarity** — Requires learning reactive primitives vs. component lifecycle
❌ **Legacy browser support** — Requires modern JavaScript features
❌ **Rapid prototyping** — React's ecosystem may be faster for quick MVPs

### Best For

- High-performance dashboards and data visualizations
- Real-time collaborative applications
- Progressive web apps with strict size budgets
- Projects prioritizing runtime performance over ecosystem breadth
- Teams comfortable with reactive programming paradigms

## Related

- [[Domains/Web Development]]
- [[React]]
- [[Vue]]
- [[Svelte]]
- [[Next.js]]
- [[TypeScript]]
