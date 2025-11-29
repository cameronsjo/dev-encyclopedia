---
title: Astro
aliases:
  - Astro.build
tags:
  - framework
  - web
  - frontend
  - static
  - content
type: reference
status: complete
created: 2025-11-28
---

# Astro

The web framework for content-driven websites.

## Overview

| Aspect | Details |
|--------|---------|
| Type | Static-first meta-framework |
| Philosophy | Ship zero JavaScript by default |
| UI Libraries | Any (React, Vue, Svelte, Solid) |
| Language | TypeScript/JavaScript |
| Backing | Astro (company) |
| First release | 2022 |

---

## Why Astro?

### Zero JavaScript by Default

```
Traditional SPA:       Astro:
┌─────────────────┐    ┌─────────────────┐
│   HTML (shell)  │    │   Full HTML     │
│   + 200KB JS    │    │   + 0KB JS      │
│   ↓             │    │   (static page) │
│   Renders UI    │    │                 │
└─────────────────┘    └─────────────────┘
```

### Islands Architecture

Only hydrate interactive components.

```
┌─────────────────────────────────────────────────────┐
│                  Static HTML                         │
│  ┌─────────┐                      ┌─────────┐       │
│  │ Island  │  Static content...   │ Island  │       │
│  │ (React) │                      │ (Svelte)│       │
│  └─────────┘                      └─────────┘       │
│       ↑ Hydrated                       ↑ Hydrated   │
└─────────────────────────────────────────────────────┘
```

---

## Project Structure

```
my-astro-site/
├── src/
│   ├── components/
│   │   ├── Header.astro
│   │   └── Counter.tsx      # React island
│   ├── layouts/
│   │   └── Layout.astro
│   ├── pages/
│   │   ├── index.astro      # /
│   │   ├── about.astro      # /about
│   │   └── blog/
│   │       ├── index.astro  # /blog
│   │       └── [slug].astro # /blog/:slug
│   ├── content/
│   │   └── blog/            # Content collections
│   │       ├── post-1.md
│   │       └── post-2.md
│   └── styles/
├── public/                   # Static assets
└── astro.config.mjs
```

---

## Astro Components

### .astro Files

HTML-first components.

```astro
---
// Component Script (runs at build time)
const greeting = 'Hello';
const items = ['Apple', 'Banana', 'Cherry'];

interface Props {
  title: string;
}

const { title } = Astro.props;
---

<!-- Component Template -->
<div class="card">
  <h2>{title}</h2>
  <p>{greeting}, World!</p>

  <ul>
    {items.map(item => <li>{item}</li>)}
  </ul>
</div>

<style>
  /* Scoped by default */
  .card {
    padding: 1rem;
    border: 1px solid #ccc;
  }
</style>
```

### Slots

```astro
---
// Card.astro
---
<div class="card">
  <header>
    <slot name="header" />
  </header>
  <main>
    <slot />  <!-- Default slot -->
  </main>
</div>

<!-- Usage -->
<Card>
  <h2 slot="header">Title</h2>
  <p>Body content here</p>
</Card>
```

---

## Routing

### File-Based

| File | Route |
|------|-------|
| pages/index.astro | / |
| pages/about.astro | /about |
| pages/blog/index.astro | /blog |
| pages/blog/[slug].astro | /blog/:slug |
| pages/[...path].astro | Catch-all |

### Dynamic Routes

```astro
---
// pages/blog/[slug].astro
import { getCollection } from 'astro:content';

export async function getStaticPaths() {
  const posts = await getCollection('blog');
  return posts.map(post => ({
    params: { slug: post.slug },
    props: { post },
  }));
}

const { post } = Astro.props;
const { Content } = await post.render();
---

<h1>{post.data.title}</h1>
<Content />
```

---

## Content Collections

Type-safe content management.

```ts
// src/content/config.ts
import { defineCollection, z } from 'astro:content';

const blog = defineCollection({
  type: 'content',
  schema: z.object({
    title: z.string(),
    date: z.date(),
    draft: z.boolean().default(false),
    tags: z.array(z.string()).optional(),
  }),
});

export const collections = { blog };
```

```md
---
# src/content/blog/my-post.md
title: My First Post
date: 2024-01-15
tags: [astro, web]
---

# Hello World

This is my post content.
```

```astro
---
// Query content
import { getCollection } from 'astro:content';

const posts = await getCollection('blog', ({ data }) => {
  return !data.draft;  // Filter drafts
});
---
```

---

## UI Framework Integrations

Use React, Vue, Svelte, Solid together.

```bash
npx astro add react
npx astro add vue
npx astro add svelte
```

```astro
---
import ReactCounter from './Counter.tsx';
import VueCard from './Card.vue';
import SvelteToggle from './Toggle.svelte';
---

<ReactCounter />
<VueCard />
<SvelteToggle />
```

---

## Client Directives (Islands)

Control when/if components hydrate.

### Directives

| Directive | When Hydrates |
|-----------|---------------|
| client:load | Immediately on page load |
| client:idle | When browser is idle |
| client:visible | When scrolled into view |
| client:media | When media query matches |
| client:only | Never SSR, client only |

### Examples

```astro
---
import Counter from './Counter.tsx';
import HeavyChart from './Chart.tsx';
import MobileMenu from './Menu.tsx';
---

<!-- Interactive immediately -->
<Counter client:load />

<!-- Hydrate when visible (good for below-fold) -->
<HeavyChart client:visible />

<!-- Only on mobile -->
<MobileMenu client:media="(max-width: 768px)" />

<!-- Never SSR, React-only -->
<ReactOnlyComponent client:only="react" />

<!-- No directive = no JS, static HTML only -->
<Counter />
```

---

## Data Fetching

### At Build Time

```astro
---
// Runs at build time
const response = await fetch('https://api.example.com/data');
const data = await response.json();
---

<ul>
  {data.map(item => <li>{item.name}</li>)}
</ul>
```

### On Demand (SSR Mode)

```astro
---
// With output: 'server' in config
const response = await fetch(`https://api.example.com/user/${Astro.params.id}`);
const user = await response.json();
---

<h1>{user.name}</h1>
```

---

## Rendering Modes

### Static (Default)

```js
// astro.config.mjs
export default {
  output: 'static',  // Default
};
```

Pre-rendered at build time.

### Server (SSR)

```js
export default {
  output: 'server',
  adapter: vercel(),  // or netlify(), cloudflare(), etc.
};
```

### Hybrid

Per-page control.

```js
export default {
  output: 'hybrid',
};
```

```astro
---
// Static by default
export const prerender = true;

// Or force server render
export const prerender = false;
---
```

---

## View Transitions

Built-in page transitions.

```astro
---
// Layout.astro
import { ViewTransitions } from 'astro:transitions';
---

<html>
  <head>
    <ViewTransitions />
  </head>
  <body>
    <slot />
  </body>
</html>
```

```astro
<!-- Persist elements across pages -->
<header transition:persist>
  <nav>...</nav>
</header>

<!-- Named transitions -->
<h1 transition:name="title">{title}</h1>
```

---

## Configuration

```js
// astro.config.mjs
import { defineConfig } from 'astro/config';
import react from '@astrojs/react';
import tailwind from '@astrojs/tailwind';
import vercel from '@astrojs/vercel/serverless';

export default defineConfig({
  site: 'https://example.com',
  output: 'hybrid',
  adapter: vercel(),
  integrations: [
    react(),
    tailwind(),
  ],
  markdown: {
    shikiConfig: {
      theme: 'github-dark',
    },
  },
});
```

---

## Deployment

### Static

Any static host: Netlify, Vercel, GitHub Pages, S3.

```bash
npm run build
# Outputs to dist/
```

### Server

| Adapter | Platform |
|---------|----------|
| @astrojs/vercel | Vercel |
| @astrojs/netlify | Netlify |
| @astrojs/cloudflare | Cloudflare |
| @astrojs/node | Node server |
| @astrojs/deno | Deno |

---

## Astro vs Alternatives

| Aspect | Astro | Next.js | Gatsby |
|--------|-------|---------|--------|
| Default output | Static | Server | Static |
| JS by default | No | Yes | Yes |
| UI libraries | Any | React | React |
| Content focus | Yes | General | Yes |
| Islands | Native | No | No |
| Learning curve | Low | High | Medium |

---

## When to Use Astro

**Strengths:**
- Zero JS by default
- Use any UI library
- Great for content sites
- Fast builds
- View transitions
- Islands architecture

**Considerations:**
- Less suited for app-like SPAs
- Newer, smaller ecosystem
- Islands require thinking differently

**Best for:**
- Marketing sites
- Documentation
- Blogs
- Portfolios
- Landing pages
- Content-heavy sites

---

## Related

- [[React]]
- [[Vue]]
- [[Svelte]]
- [[Next.js]]
- [[SvelteKit]]
- [[domains/Web Development|Web Development]]
