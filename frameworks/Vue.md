---
title: Vue
aliases:
  - Vue.js
  - VueJS
tags:
  - framework
  - web
  - frontend
  - javascript
  - typescript
type: reference
status: complete
created: 2025-11-28
---

# Vue

Progressive framework for building web interfaces.

## Overview

| Aspect | Details |
|--------|---------|
| Type | Progressive framework |
| Language | JavaScript/TypeScript |
| Rendering | Virtual DOM, compiler optimizations |
| Architecture | Component-based, reactivity system |
| First release | 2014 |
| Backing | Community + sponsors (Evan You) |

---

## Core Concepts

### Single-File Components

`.vue` files combining template, script, and styles.

```vue
<script setup lang="ts">
import { ref } from 'vue'

const count = ref(0)
</script>

<template>
  <button @click="count++">{{ count }}</button>
</template>

<style scoped>
button { padding: 1rem; }
</style>
```

### Composition API vs Options API

| API | Style | Recommendation |
|-----|-------|----------------|
| Composition | Functions, `setup()` | ✅ New projects |
| Options | Object-based (`data`, `methods`) | Legacy/simple cases |

---

## Reactivity System

### Core Primitives

| Function | Purpose |
|----------|---------|
| ref() | Primitive reactive values |
| reactive() | Object reactive state |
| computed() | Derived values |
| watch() | Side effects on changes |
| watchEffect() | Auto-tracking effects |

### How It Works

Vue tracks dependencies automatically at runtime.

```javascript
const count = ref(0)
const doubled = computed(() => count.value * 2)
// doubled updates automatically when count changes
```

### ref vs reactive

| Use Case | Choice |
|----------|--------|
| Primitives | ref() |
| Objects | reactive() or ref() |
| Replacing entire value | ref() |
| Deep nesting | reactive() |

**Convention:** Many teams use `ref()` for everything for consistency.

---

## State Management

### Built-in Options

| Approach | Use Case |
|----------|----------|
| ref/reactive | Component state |
| provide/inject | Dependency injection |
| Composables | Shared stateful logic |

### Pinia

Official state management library.

| Feature | Description |
|---------|-------------|
| Stores | Modular state containers |
| DevTools | Full integration |
| TypeScript | First-class support |
| SSR | Built-in support |

```javascript
// stores/counter.js
export const useCounterStore = defineStore('counter', () => {
  const count = ref(0)
  const increment = () => count.value++
  return { count, increment }
})
```

**Note:** Vuex is deprecated in favor of Pinia.

---

## Vue Ecosystem

### Meta-Frameworks

| Framework | Focus |
|-----------|-------|
| Nuxt | Full-stack, SSR, file routing |
| VitePress | Documentation sites |
| Quasar | Multi-platform (web, mobile, desktop) |

### Routing

Vue Router — the official solution.

```javascript
const routes = [
  { path: '/', component: Home },
  { path: '/about', component: About },
  { path: '/user/:id', component: User }
]
```

### Styling

| Approach | Notes |
|----------|-------|
| Scoped CSS | Built-in `<style scoped>` |
| CSS Modules | Built-in support |
| Tailwind | Popular choice |
| UnoCSS | Atomic CSS, Vue ecosystem |

### UI Libraries

| Library | Style |
|---------|-------|
| Vuetify | Material Design |
| PrimeVue | Enterprise components |
| Naive UI | Modern, TypeScript |
| Radix Vue | Unstyled, accessible |
| shadcn-vue | Tailwind + Radix |

---

## Vue 3 Features

### Composition API

Functional approach to component logic.

### `<script setup>`

Compiler macro for cleaner Composition API.

### Teleport

Render content outside component tree (modals, tooltips).

### Suspense

Async component handling with fallbacks.

### Multiple v-models

```vue
<UserForm v-model:name="name" v-model:email="email" />
```

---

## Performance

### Built-in Optimizations

| Feature | Benefit |
|---------|---------|
| Compiler hints | Skip static content |
| Reactivity tracking | Fine-grained updates |
| Tree-shaking | Small bundles |
| Static hoisting | Reduced overhead |

### Manual Optimizations

| Technique | When |
|-----------|------|
| v-once | Truly static content |
| v-memo | Expensive list items |
| shallowRef | Large objects, manual triggers |
| Virtualization | Very long lists |

---

## Testing

### Libraries

| Library | Purpose |
|---------|---------|
| Vitest | Test runner (Vue team) |
| Vue Test Utils | Component testing |
| Playwright | E2E testing |

### Philosophy

- Test components in isolation
- Mock child components when needed
- Focus on user interactions

---

## Vue vs React

| Aspect | Vue | React |
|--------|-----|-------|
| Template | HTML-based | JSX |
| Reactivity | Automatic tracking | Manual dependencies |
| State management | Pinia (official) | Many options |
| Learning curve | Gentler | Steeper (ecosystem) |
| Docs | Excellent | Good |
| Job market | Smaller | Larger |
| Bundle size | Smaller | Larger |

### When Vue Wins

- Teams new to frontend
- Prefer templates over JSX
- Want official solutions (router, state)
- Value great documentation

### When React Wins

- Larger talent pool needed
- Heavy Meta/enterprise adoption
- Need React Native (mobile)
- Prefer JSX flexibility

---

## Common Patterns

### Composables

Reusable stateful logic.

```javascript
// composables/useMouse.js
export function useMouse() {
  const x = ref(0)
  const y = ref(0)

  onMounted(() => {
    window.addEventListener('mousemove', update)
  })

  function update(e) {
    x.value = e.pageX
    y.value = e.pageY
  }

  return { x, y }
}
```

### Provide/Inject

Dependency injection for component trees.

### Slots

Content distribution for component composition.

```vue
<Card>
  <template #header>Title</template>
  <template #default>Body content</template>
</Card>
```

---

## When to Use Vue

**Strengths:**
- Gentle learning curve
- Excellent documentation
- Official solutions (routing, state)
- Great TypeScript support
- Smaller bundle size
- Template syntax (familiar to HTML devs)

**Considerations:**
- Smaller job market than React
- Fewer third-party libraries
- Less common in large enterprises

**Best for:**
- Teams new to modern frontend
- Startups and mid-size companies
- Projects valuing simplicity
- When Nuxt fits requirements
- Laravel/PHP ecosystem integration

---

## Related

- [[React]]
- [[Angular]]
- [[Svelte]]
- [[TypeScript]]
- [[domains/Web Development|Web Development]]
