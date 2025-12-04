---
title: Nuxt
aliases:
  - NuxtJS
  - Nuxt.js
tags:
  - framework
  - web
  - frontend
  - fullstack
  - vue
  - typescript
type: reference
status: complete
created: '2025-11-28'
---

# Nuxt

The intuitive Vue framework.

## Overview

| Aspect | Details |
|--------|---------|
| Base | Vue 3 |
| Type | Full-stack meta-framework |
| Rendering | SSR, SSG, SPA, hybrid |
| Language | TypeScript/JavaScript |
| Backing | NuxtLabs |
| First release | 2016 |

---

## Why Nuxt?

Vue is a library. Nuxt is the framework.

| Vue Alone | Nuxt Adds |
|-----------|-----------|
| Client rendering | Server rendering |
| Vue Router setup | File-based routing |
| Manual config | Auto-imports |
| DIY structure | Conventions |

---

## Project Structure

```
my-nuxt-app/
├── .nuxt/              # Build output
├── app.vue             # Root component
├── nuxt.config.ts      # Configuration
├── pages/              # File-based routing
│   ├── index.vue       # /
│   ├── about.vue       # /about
│   └── blog/
│       ├── index.vue   # /blog
│       └── [slug].vue  # /blog/:slug
├── components/         # Auto-imported
├── composables/        # Auto-imported
├── layouts/            # Page layouts
├── middleware/         # Route middleware
├── plugins/            # Vue plugins
├── public/             # Static assets
└── server/             # API & server
    ├── api/
    └── middleware/
```

---

## Routing

### File-Based Routes

| File | Route |
|------|-------|
| pages/index.vue | / |
| pages/about.vue | /about |
| pages/blog/index.vue | /blog |
| pages/blog/[slug].vue | /blog/:slug |
| pages/[...slug].vue | Catch-all |

### Dynamic Routes

```vue
<!-- pages/blog/[slug].vue -->
<script setup>
const route = useRoute();
const { slug } = route.params;

const { data: post } = await useFetch(`/api/posts/${slug}`);
</script>

<template>
  <article>
    <h1>{{ post.title }}</h1>
  </article>
</template>
```

### Nested Routes

```
pages/
├── users/
│   ├── index.vue           # /users
│   └── [id]/
│       ├── index.vue       # /users/:id
│       └── settings.vue    # /users/:id/settings
```

---

## Rendering Modes

### Universal (SSR + Hydration)

Default. Server renders, client hydrates.

```ts
// nuxt.config.ts
export default defineNuxtConfig({
  ssr: true, // default
});
```

### Static (SSG)

Pre-render at build time.

```ts
export default defineNuxtConfig({
  ssr: true,
  nitro: {
    prerender: {
      routes: ['/blog/post-1', '/blog/post-2'],
    },
  },
});
```

### SPA Mode

Client-only.

```ts
export default defineNuxtConfig({
  ssr: false,
});
```

### Hybrid Rendering

Per-route control.

```ts
export default defineNuxtConfig({
  routeRules: {
    '/': { prerender: true },           // Static
    '/blog/**': { swr: 3600 },          // ISR (1 hour)
    '/admin/**': { ssr: false },        // SPA
    '/api/**': { cors: true },          // API rules
  },
});
```

---

## Data Fetching

### useFetch (Recommended)

```vue
<script setup>
const { data, pending, error, refresh } = await useFetch('/api/users');
</script>

<template>
  <div v-if="pending">Loading...</div>
  <div v-else-if="error">Error: {{ error.message }}</div>
  <ul v-else>
    <li v-for="user in data" :key="user.id">{{ user.name }}</li>
  </ul>
</template>
```

### useAsyncData

More control over caching key.

```vue
<script setup>
const { data } = await useAsyncData('users', () => {
  return $fetch('/api/users');
});
</script>
```

### $fetch

Direct fetch (no SSR deduplication).

```vue
<script setup>
const user = await $fetch('/api/users/1');
</script>
```

### Caching Options

```vue
<script setup>
const { data } = await useFetch('/api/data', {
  key: 'my-data',
  server: true,           // Fetch on server
  lazy: false,            // Block navigation
  default: () => [],      // Default value
  transform: (data) => data.items,
  pick: ['id', 'name'],   // Only these fields
});
</script>
```

---

## Server API

### API Routes

```ts
// server/api/users.ts
export default defineEventHandler(async (event) => {
  return await db.query('SELECT * FROM users');
});

// server/api/users/[id].ts
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id');
  return await db.findById(id);
});

// server/api/users.post.ts (POST method)
export default defineEventHandler(async (event) => {
  const body = await readBody(event);
  return await db.insert(body);
});
```

### Server Middleware

```ts
// server/middleware/auth.ts
export default defineEventHandler((event) => {
  const token = getHeader(event, 'authorization');
  if (!token) {
    throw createError({ statusCode: 401, message: 'Unauthorized' });
  }
});
```

---

## Auto-Imports

### Components

```
components/
├── Button.vue          # <Button />
├── Card/
│   ├── Header.vue      # <CardHeader />
│   └── Body.vue        # <CardBody />
└── UI/
    └── Modal.vue       # <UIModal />
```

### Composables

```ts
// composables/useCounter.ts
export function useCounter() {
  const count = ref(0);
  const increment = () => count.value++;
  return { count, increment };
}

// Used anywhere without import
const { count, increment } = useCounter();
```

### Built-in Composables

| Composable | Purpose |
|------------|---------|
| useFetch | Data fetching |
| useAsyncData | Async data |
| useRoute | Current route |
| useRouter | Navigation |
| useState | SSR-safe state |
| useCookie | Cookie access |
| useHead | Head management |

---

## State Management

### useState

SSR-safe reactive state.

```ts
// composables/useUser.ts
export const useUser = () => useState<User | null>('user', () => null);

// In components
const user = useUser();
user.value = { name: 'John' };
```

### Pinia (Recommended for Complex State)

```ts
// stores/counter.ts
export const useCounterStore = defineStore('counter', () => {
  const count = ref(0);
  const double = computed(() => count.value * 2);
  function increment() { count.value++; }
  return { count, double, increment };
});
```

---

## Layouts

```vue
<!-- layouts/default.vue -->
<template>
  <div>
    <Header />
    <slot />
    <Footer />
  </div>
</template>

<!-- layouts/admin.vue -->
<template>
  <div class="admin">
    <Sidebar />
    <main><slot /></main>
  </div>
</template>
```

```vue
<!-- pages/dashboard.vue -->
<script setup>
definePageMeta({
  layout: 'admin',
});
</script>
```

---

## Middleware

### Route Middleware

```ts
// middleware/auth.ts
export default defineNuxtRouteMiddleware((to, from) => {
  const user = useUser();
  if (!user.value && to.path !== '/login') {
    return navigateTo('/login');
  }
});
```

```vue
<!-- pages/dashboard.vue -->
<script setup>
definePageMeta({
  middleware: 'auth',
});
</script>
```

### Global Middleware

```ts
// middleware/analytics.global.ts
export default defineNuxtRouteMiddleware((to) => {
  trackPageView(to.fullPath);
});
```

---

## Modules

### Official Modules

| Module | Purpose |
|--------|---------|
| @nuxt/image | Image optimization |
| @nuxt/content | Markdown/CMS |
| @nuxt/ui | Component library |
| @nuxtjs/i18n | Internationalization |
| @pinia/nuxt | State management |
| @vueuse/nuxt | Utility composables |

### Installing Modules

```ts
// nuxt.config.ts
export default defineNuxtConfig({
  modules: [
    '@nuxt/image',
    '@nuxt/content',
    '@pinia/nuxt',
  ],
});
```

---

## Configuration

```ts
// nuxt.config.ts
export default defineNuxtConfig({
  devtools: { enabled: true },

  modules: ['@nuxt/image', '@pinia/nuxt'],

  css: ['~/assets/main.css'],

  runtimeConfig: {
    // Server-only
    apiSecret: process.env.API_SECRET,
    // Public (exposed to client)
    public: {
      apiBase: process.env.API_BASE,
    },
  },

  routeRules: {
    '/api/**': { cors: true },
    '/blog/**': { swr: 3600 },
  },

  nitro: {
    preset: 'vercel', // or 'cloudflare', 'node-server', etc.
  },
});
```

---

## Deployment

### Presets

| Preset | Platform |
|--------|----------|
| vercel | Vercel |
| netlify | Netlify |
| cloudflare | Cloudflare Pages |
| node-server | Node.js server |
| static | Static hosting |

### Build Commands

```bash
npm run build    # Production build
npm run generate # Static generation
npm run preview  # Preview build
```

---

## Nuxt vs Alternatives

| Aspect | Nuxt | Next.js | SvelteKit |
|--------|------|---------|-----------|
| Base | Vue | React | Svelte |
| Auto-imports | Yes | No | Partial |
| File routing | Yes | Yes | Yes |
| API routes | Yes | Yes | Yes |
| DX | Excellent | Good | Excellent |
| Bundle size | Medium | Larger | Smaller |

---

## When to Use Nuxt

**Strengths:**

- Best Vue DX
- Auto-imports everything
- Excellent TypeScript
- Flexible rendering
- Great modules ecosystem

**Considerations:**

- Vue-only
- Smaller ecosystem than Next.js
- Enterprise adoption smaller

**Best for:**

- Vue teams
- Full-stack Vue apps
- Content sites (with Nuxt Content)
- SEO-important apps

---

## Related

- [[Vue]]
- [[Next.js]]
- [[SvelteKit]]
- [[Astro]]
- [[domains/Web Development|Web Development]]
