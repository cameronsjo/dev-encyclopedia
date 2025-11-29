---
title: Svelte
aliases:
  - SvelteJS
tags:
  - framework
  - web
  - frontend
  - javascript
  - typescript
  - compiler
type: reference
status: complete
created: 2025-11-28
---

# Svelte

Compiler-based framework with no virtual DOM.

## Overview

| Aspect | Details |
|--------|---------|
| Type | Compiler / Framework |
| Language | JavaScript/TypeScript |
| Rendering | Direct DOM manipulation |
| Architecture | Component-based, compiled reactivity |
| First release | 2016 |
| Backing | Vercel + community (Rich Harris) |

---

## Key Difference: Compilation

Unlike React/Vue, Svelte compiles components to vanilla JavaScript at build time.

```
┌─────────────────┐        ┌─────────────────┐
│   .svelte file  │ ──────▶│  Vanilla JS     │
│   (compile time)│        │  (runtime)      │
└─────────────────┘        └─────────────────┘
```

**Result:** No framework runtime shipped to browser.

### Benefits

| Benefit | Description |
|---------|-------------|
| Smaller bundles | No virtual DOM library |
| Faster | Direct DOM updates |
| Less JavaScript | Optimized output |
| Simpler mental model | No diffing to understand |

---

## Core Concepts

### Components

`.svelte` files with script, markup, and styles.

```svelte
<script lang="ts">
  let count = 0;

  function increment() {
    count += 1;
  }
</script>

<button on:click={increment}>
  Count: {count}
</button>

<style>
  button {
    padding: 1rem;
  }
</style>
```

### Reactivity

Svelte tracks assignments. Change a variable, UI updates.

```svelte
<script>
  let count = 0;
  $: doubled = count * 2;  // Reactive declaration
</script>
```

The `$:` syntax marks reactive statements.

### Runes (Svelte 5)

New reactivity primitives.

```svelte
<script>
  let count = $state(0);
  let doubled = $derived(count * 2);
</script>
```

| Rune | Purpose |
|------|---------|
| $state | Reactive state |
| $derived | Computed values |
| $effect | Side effects |
| $props | Component props |
| $bindable | Two-way bindable props |

---

## Svelte 5

Major evolution of reactivity system.

### Svelte 4 → Svelte 5

| Svelte 4 | Svelte 5 |
|----------|----------|
| `let x = 0` | `let x = $state(0)` |
| `$: doubled = x * 2` | `let doubled = $derived(x * 2)` |
| `$: { sideEffect() }` | `$effect(() => { sideEffect() })` |
| `export let prop` | `let { prop } = $props()` |

### Benefits of Runes

- Explicit reactivity (easier to trace)
- Works in `.js` files (not just `.svelte`)
- Better TypeScript support
- More predictable behavior

---

## SvelteKit

Meta-framework for Svelte (like Next.js for React).

### Features

| Feature | Description |
|---------|-------------|
| File-based routing | `src/routes/` structure |
| SSR/SSG/SPA | Multiple rendering modes |
| API routes | `+server.js` endpoints |
| Data loading | `+page.server.js` loaders |
| Form actions | Server-side form handling |

### Routing

```
src/routes/
├── +page.svelte          # /
├── about/
│   └── +page.svelte      # /about
├── blog/
│   ├── +page.svelte      # /blog
│   └── [slug]/
│       └── +page.svelte  # /blog/:slug
```

### Data Loading

```javascript
// +page.server.js
export async function load({ params }) {
  const post = await getPost(params.slug);
  return { post };
}
```

```svelte
<!-- +page.svelte -->
<script>
  let { data } = $props();
</script>

<h1>{data.post.title}</h1>
```

---

## State Management

### Built-in Stores (Svelte 4)

```javascript
import { writable, derived } from 'svelte/store';

const count = writable(0);
const doubled = derived(count, $c => $c * 2);
```

### With Runes (Svelte 5)

State can live anywhere, no special stores needed.

```javascript
// state.svelte.js
export const appState = $state({
  user: null,
  theme: 'light'
});
```

### When to Use What

| Scenario | Approach |
|----------|----------|
| Component state | `$state()` |
| Shared state | `$state()` in module |
| Complex async | Stores or TanStack Query |

---

## Styling

### Scoped by Default

Styles in `<style>` are scoped to component.

```svelte
<style>
  /* Only applies to this component */
  p { color: blue; }
</style>
```

### Global Styles

```svelte
<style>
  :global(body) {
    margin: 0;
  }
</style>
```

### Styling Options

| Approach | Notes |
|----------|-------|
| Scoped CSS | Built-in |
| Tailwind | Very popular |
| CSS-in-JS | Less common |
| PostCSS | Built-in support |

---

## Bindings

Two-way bindings built into the language.

```svelte
<input bind:value={name}>
<input type="checkbox" bind:checked={active}>
<select bind:value={selected}>
<div bind:clientWidth={width}>
```

### Component Bindings

```svelte
<ChildComponent bind:value={parentValue}>
```

---

## Events

### DOM Events

```svelte
<button on:click={handleClick}>Click</button>
<button on:click|preventDefault={handleClick}>Submit</button>
```

### Event Modifiers

| Modifier | Effect |
|----------|--------|
| preventDefault | Calls event.preventDefault() |
| stopPropagation | Prevents event bubbling |
| once | Removes handler after first call |
| self | Only fires if event.target is element |

### Component Events

```svelte
<!-- Child.svelte -->
<script>
  import { createEventDispatcher } from 'svelte';
  const dispatch = createEventDispatcher();
</script>

<button on:click={() => dispatch('submit', data)}>

<!-- Parent.svelte -->
<Child on:submit={handleSubmit}>
```

---

## Transitions

Built-in animation support.

```svelte
<script>
  import { fade, slide, fly } from 'svelte/transition';
</script>

{#if visible}
  <div transition:fade>Fades in/out</div>
  <div in:fly={{ y: 200 }} out:fade>Flies in, fades out</div>
{/if}
```

### Available Transitions

| Transition | Effect |
|------------|--------|
| fade | Opacity |
| blur | Blur + opacity |
| fly | Position + opacity |
| slide | Height collapse |
| scale | Size + opacity |
| draw | SVG path drawing |

---

## Performance

### Why Svelte is Fast

| Factor | Benefit |
|--------|---------|
| No virtual DOM | No diffing overhead |
| Compile-time optimizations | Minimal runtime code |
| Fine-grained updates | Only update what changed |
| Small bundles | Less to parse/execute |

### Bundle Size Comparison

| Framework | Baseline |
|-----------|----------|
| Svelte | ~2 KB |
| Vue | ~16 KB |
| React | ~40 KB |
| Angular | ~100+ KB |

---

## Testing

### Libraries

| Library | Purpose |
|---------|---------|
| Vitest | Test runner |
| @testing-library/svelte | Component testing |
| Playwright | E2E testing |

---

## Svelte vs Others

| Aspect | Svelte | React | Vue |
|--------|--------|-------|-----|
| Approach | Compiler | Runtime | Runtime |
| Size | Tiny | Medium | Small |
| Learning curve | Easy | Moderate | Easy |
| Reactivity | Automatic | Manual (hooks) | Automatic |
| Syntax | HTML-like | JSX | Template |
| Meta-framework | SvelteKit | Next.js | Nuxt |
| Ecosystem | Growing | Massive | Large |
| Jobs | Growing | Most | Many |

---

## Ecosystem

### UI Libraries

| Library | Style |
|---------|-------|
| Skeleton | Tailwind-based |
| shadcn-svelte | Tailwind + Radix ports |
| Flowbite Svelte | Tailwind components |
| Carbon Components | IBM Carbon |

### Other Libraries

| Library | Purpose |
|---------|---------|
| TanStack Query | Data fetching |
| Superforms | Form handling |
| Paraglide | i18n |

---

## When to Use Svelte

**Strengths:**
- Smallest bundle sizes
- Fastest runtime performance
- Easiest to learn
- Great developer experience
- Built-in transitions/animations
- Minimal boilerplate

**Considerations:**
- Smaller ecosystem
- Fewer jobs (growing)
- Less enterprise adoption
- Runes migration (Svelte 5)

**Best for:**
- Performance-critical apps
- Small teams/solo developers
- Learning modern frontend
- Static sites (SvelteKit)
- When bundle size matters

---

## Related

- [[React]]
- [[Vue]]
- [[Angular]]
- [[TypeScript]]
- [[domains/Web Development|Web Development]]
