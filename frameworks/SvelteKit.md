---
title: SvelteKit
aliases:
  - Svelte Kit
tags:
  - framework
  - web
  - frontend
  - fullstack
  - svelte
  - typescript
type: reference
status: complete
created: 2025-11-28
---

# SvelteKit

The fastest way to build Svelte apps.

## Overview

| Aspect | Details |
|--------|---------|
| Base | Svelte |
| Type | Full-stack meta-framework |
| Rendering | SSR, SSG, SPA |
| Language | TypeScript/JavaScript |
| Backing | Vercel |
| First release | 2022 (1.0) |

---

## Why SvelteKit?

| Svelte Alone | SvelteKit Adds |
|--------------|----------------|
| Component compiler | Full framework |
| No routing | File-based routing |
| Client only | Server rendering |
| Manual setup | Opinionated structure |

---

## Project Structure

```
my-sveltekit-app/
├── src/
│   ├── app.html          # HTML template
│   ├── app.css           # Global styles
│   ├── routes/           # File-based routing
│   │   ├── +page.svelte  # /
│   │   ├── +layout.svelte
│   │   ├── about/
│   │   │   └── +page.svelte
│   │   └── blog/
│   │       ├── +page.svelte
│   │       ├── +page.server.ts
│   │       └── [slug]/
│   │           └── +page.svelte
│   ├── lib/              # Shared code ($lib alias)
│   └── hooks.server.ts   # Server hooks
├── static/               # Static assets
├── svelte.config.js
├── vite.config.ts
└── package.json
```

---

## Routing

### File Conventions

| File | Purpose |
|------|---------|
| +page.svelte | Page component |
| +page.ts | Load data (universal) |
| +page.server.ts | Load data (server-only) |
| +layout.svelte | Shared layout |
| +layout.ts | Layout data |
| +error.svelte | Error page |
| +server.ts | API endpoint |

### Route Examples

| Path | Route |
|------|-------|
| src/routes/+page.svelte | / |
| src/routes/about/+page.svelte | /about |
| src/routes/blog/[slug]/+page.svelte | /blog/:slug |
| src/routes/[...path]/+page.svelte | Catch-all |
| src/routes/(group)/+page.svelte | Route group |

### Dynamic Routes

```svelte
<!-- src/routes/blog/[slug]/+page.svelte -->
<script>
  export let data;
</script>

<h1>{data.post.title}</h1>
<article>{@html data.post.content}</article>
```

```ts
// src/routes/blog/[slug]/+page.server.ts
export async function load({ params }) {
  const post = await getPost(params.slug);
  return { post };
}
```

---

## Data Loading

### Universal Load (+page.ts)

Runs on server AND client.

```ts
// src/routes/+page.ts
export async function load({ fetch }) {
  const res = await fetch('/api/posts');
  const posts = await res.json();
  return { posts };
}
```

### Server Load (+page.server.ts)

Server-only. Access databases, secrets.

```ts
// src/routes/+page.server.ts
import { db } from '$lib/database';

export async function load() {
  const posts = await db.query('SELECT * FROM posts');
  return { posts };
}
```

### Using Data

```svelte
<script>
  export let data;  // Populated by load function
</script>

{#each data.posts as post}
  <article>{post.title}</article>
{/each}
```

### Streaming

```ts
export async function load() {
  return {
    fast: await getFastData(),
    slow: getSlowData(), // Promise, streams in
  };
}
```

```svelte
{#await data.slow}
  <p>Loading...</p>
{:then value}
  <p>{value}</p>
{/await}
```

---

## Form Actions

Handle forms without client JS.

```ts
// src/routes/login/+page.server.ts
import { fail, redirect } from '@sveltejs/kit';

export const actions = {
  default: async ({ request, cookies }) => {
    const data = await request.formData();
    const email = data.get('email');
    const password = data.get('password');

    const user = await authenticate(email, password);

    if (!user) {
      return fail(401, { email, message: 'Invalid credentials' });
    }

    cookies.set('session', user.token, { path: '/' });
    throw redirect(303, '/dashboard');
  }
};
```

```svelte
<!-- src/routes/login/+page.svelte -->
<script>
  export let form;  // Action result
</script>

<form method="POST">
  <input name="email" value={form?.email ?? ''}>
  <input name="password" type="password">
  <button>Log in</button>

  {#if form?.message}
    <p class="error">{form.message}</p>
  {/if}
</form>
```

### Enhanced Forms

Progressive enhancement with JS.

```svelte
<script>
  import { enhance } from '$app/forms';
</script>

<form method="POST" use:enhance>
  <!-- Form submits without page reload when JS available -->
</form>
```

---

## API Routes

```ts
// src/routes/api/users/+server.ts
import { json } from '@sveltejs/kit';

export async function GET() {
  const users = await db.getUsers();
  return json(users);
}

export async function POST({ request }) {
  const body = await request.json();
  const user = await db.createUser(body);
  return json(user, { status: 201 });
}
```

```ts
// src/routes/api/users/[id]/+server.ts
export async function GET({ params }) {
  const user = await db.getUser(params.id);
  if (!user) {
    throw error(404, 'Not found');
  }
  return json(user);
}
```

---

## Layouts

```svelte
<!-- src/routes/+layout.svelte -->
<script>
  import Header from '$lib/Header.svelte';
  import Footer from '$lib/Footer.svelte';
</script>

<Header />
<main>
  <slot />  <!-- Page content here -->
</main>
<Footer />
```

### Nested Layouts

```
routes/
├── +layout.svelte        # All pages
├── dashboard/
│   ├── +layout.svelte    # Dashboard pages only
│   └── +page.svelte
```

### Layout Data

```ts
// src/routes/+layout.server.ts
export async function load({ locals }) {
  return {
    user: locals.user
  };
}
```

---

## Hooks

### Server Hooks

```ts
// src/hooks.server.ts
import type { Handle } from '@sveltejs/kit';

export const handle: Handle = async ({ event, resolve }) => {
  // Before request
  const session = event.cookies.get('session');
  event.locals.user = await getUser(session);

  const response = await resolve(event);

  // After request
  response.headers.set('x-custom', 'value');

  return response;
};
```

### Handle Fetch

```ts
// src/hooks.server.ts
export const handleFetch: HandleFetch = async ({ request, fetch }) => {
  // Modify outgoing fetches
  if (request.url.startsWith('https://api.internal/')) {
    request = new Request(
      request.url.replace('https://api.internal/', 'http://localhost:3000/'),
      request
    );
  }
  return fetch(request);
};
```

---

## Rendering Modes

### Per-Route Options

```ts
// src/routes/+page.ts

// SSR (default)
export const ssr = true;

// Client-side only
export const ssr = false;

// Prerender at build
export const prerender = true;

// Disable client-side router
export const csr = false;
```

### Config

```ts
// svelte.config.js
export default {
  kit: {
    prerender: {
      entries: ['/', '/about', '/blog/*'],
    },
    adapter: adapter(),
  },
};
```

---

## Adapters

Deploy anywhere.

| Adapter | Platform |
|---------|----------|
| @sveltejs/adapter-auto | Auto-detect |
| @sveltejs/adapter-vercel | Vercel |
| @sveltejs/adapter-netlify | Netlify |
| @sveltejs/adapter-cloudflare | Cloudflare |
| @sveltejs/adapter-node | Node server |
| @sveltejs/adapter-static | Static files |

```js
// svelte.config.js
import adapter from '@sveltejs/adapter-vercel';

export default {
  kit: {
    adapter: adapter()
  }
};
```

---

## $lib and Aliases

```ts
// Import from src/lib/
import { db } from '$lib/database';
import Button from '$lib/components/Button.svelte';

// $app modules
import { page, navigating } from '$app/stores';
import { goto, invalidate } from '$app/navigation';
import { browser, dev } from '$app/environment';
```

---

## Error Handling

### Error Pages

```svelte
<!-- src/routes/+error.svelte -->
<script>
  import { page } from '$app/stores';
</script>

<h1>{$page.status}</h1>
<p>{$page.error?.message}</p>
```

### Throwing Errors

```ts
import { error } from '@sveltejs/kit';

export function load({ params }) {
  const post = await getPost(params.slug);
  if (!post) {
    throw error(404, 'Post not found');
  }
  return { post };
}
```

---

## SvelteKit vs Alternatives

| Aspect | SvelteKit | Next.js | Nuxt |
|--------|-----------|---------|------|
| Base | Svelte | React | Vue |
| Bundle size | Smallest | Largest | Medium |
| Performance | Fastest | Good | Good |
| Complexity | Simple | Complex | Medium |
| Ecosystem | Growing | Massive | Large |
| Form handling | Built-in actions | DIY/libraries | DIY |

---

## When to Use SvelteKit

**Strengths:**
- Smallest bundles
- Best performance
- Simple mental model
- Built-in form actions
- Great DX

**Considerations:**
- Smaller ecosystem
- Fewer jobs (growing)
- Less enterprise adoption

**Best for:**
- Performance-critical sites
- Small teams
- Content sites
- Apps where bundle size matters
- Developers wanting simplicity

---

## Related

- [[Svelte]]
- [[Next.js]]
- [[Nuxt]]
- [[Astro]]
- [[domains/Web Development|Web Development]]
