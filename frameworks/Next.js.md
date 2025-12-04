---
title: Next.js
aliases:
  - NextJS
  - Next
tags:
  - framework
  - web
  - frontend
  - fullstack
  - react
  - typescript
  - vercel
type: reference
status: complete
created: '2025-11-28'
---

# Next.js

The React framework for production.

## Overview

| Aspect | Details |
|--------|---------|
| Base | React |
| Type | Full-stack meta-framework |
| Rendering | SSR, SSG, ISR, RSC |
| Language | TypeScript/JavaScript |
| Backing | Vercel |
| First release | 2016 |

---

## Why Next.js?

React is a library. Next.js is the framework.

| React Alone | Next.js Adds |
|-------------|--------------|
| Client rendering | Server rendering |
| No routing | File-based routing |
| Manual bundling | Optimized builds |
| No conventions | Opinionated structure |
| DIY everything | Batteries included |

---

## App Router vs Pages Router

### App Router (Recommended)

Next.js 13+ default. React Server Components.

```
app/
├── layout.tsx          # Root layout
├── page.tsx            # / route
├── about/
│   └── page.tsx        # /about
├── blog/
│   ├── page.tsx        # /blog
│   └── [slug]/
│       └── page.tsx    # /blog/:slug
└── api/
    └── users/
        └── route.ts    # API route
```

### Pages Router (Legacy)

Still supported, simpler mental model.

```
pages/
├── index.tsx           # /
├── about.tsx           # /about
├── blog/
│   ├── index.tsx       # /blog
│   └── [slug].tsx      # /blog/:slug
└── api/
    └── users.ts        # API route
```

### Which to Use?

| Use App Router | Use Pages Router |
|----------------|------------------|
| New projects | Existing projects |
| RSC benefits | Simpler migration |
| Latest features | More stable |

---

## Rendering Strategies

### Server Components (Default)

```tsx
// app/users/page.tsx
// This is a Server Component by default
async function UsersPage() {
  const users = await db.query('SELECT * FROM users');

  return (
    <ul>
      {users.map(user => <li key={user.id}>{user.name}</li>)}
    </ul>
  );
}
```

### Client Components

```tsx
'use client';

import { useState } from 'react';

function Counter() {
  const [count, setCount] = useState(0);
  return <button onClick={() => setCount(c => c + 1)}>{count}</button>;
}
```

### When to Use Which

| Server Components | Client Components |
|-------------------|-------------------|
| Data fetching | Interactivity (onClick, onChange) |
| Backend resources | Browser APIs |
| Sensitive data | State (useState, useReducer) |
| Large dependencies | Effects (useEffect) |

### Static Generation (SSG)

```tsx
// Generated at build time
export default async function Page() {
  const data = await fetch('https://api.example.com/data');
  return <div>{data}</div>;
}

// With dynamic params
export async function generateStaticParams() {
  const posts = await getPosts();
  return posts.map(post => ({ slug: post.slug }));
}
```

### Incremental Static Regeneration (ISR)

```tsx
// Revalidate every 60 seconds
export const revalidate = 60;

// Or on-demand
import { revalidatePath } from 'next/cache';
revalidatePath('/blog');
```

---

## Routing

### File Conventions

| File | Purpose |
|------|---------|
| page.tsx | Route UI |
| layout.tsx | Shared layout |
| loading.tsx | Loading UI |
| error.tsx | Error boundary |
| not-found.tsx | 404 page |
| route.ts | API endpoint |

### Dynamic Routes

```
app/
├── blog/
│   └── [slug]/           # /blog/:slug
│       └── page.tsx
├── shop/
│   └── [...categories]/  # /shop/a/b/c (catch-all)
│       └── page.tsx
└── [[...slug]]/          # Optional catch-all
    └── page.tsx
```

### Route Groups

```
app/
├── (marketing)/          # Group, no URL impact
│   ├── about/
│   └── contact/
└── (app)/
    ├── dashboard/
    └── settings/
```

### Parallel Routes

```
app/
├── @modal/
│   └── login/
│       └── page.tsx
└── layout.tsx            # Renders both
```

---

## Data Fetching

### Server Components

```tsx
async function Page() {
  // Direct database/API access
  const data = await prisma.user.findMany();

  // Or fetch with caching
  const res = await fetch('https://api.example.com', {
    cache: 'force-cache',     // Default, cached
    // cache: 'no-store',     // Never cache
    // next: { revalidate: 60 } // ISR
  });

  return <div>{/* render data */}</div>;
}
```

### Server Actions

```tsx
// app/actions.ts
'use server';

export async function createUser(formData: FormData) {
  const name = formData.get('name');
  await db.insert({ name });
  revalidatePath('/users');
}

// app/form.tsx
import { createUser } from './actions';

function Form() {
  return (
    <form action={createUser}>
      <input name="name" />
      <button type="submit">Create</button>
    </form>
  );
}
```

---

## API Routes

### Route Handlers (App Router)

```tsx
// app/api/users/route.ts
import { NextResponse } from 'next/server';

export async function GET() {
  const users = await db.query('SELECT * FROM users');
  return NextResponse.json(users);
}

export async function POST(request: Request) {
  const body = await request.json();
  const user = await db.insert(body);
  return NextResponse.json(user, { status: 201 });
}
```

### Dynamic API Routes

```tsx
// app/api/users/[id]/route.ts
export async function GET(
  request: Request,
  { params }: { params: { id: string } }
) {
  const user = await db.findById(params.id);
  return NextResponse.json(user);
}
```

---

## Middleware

```tsx
// middleware.ts (root)
import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';

export function middleware(request: NextRequest) {
  // Auth check
  const token = request.cookies.get('token');
  if (!token && request.nextUrl.pathname.startsWith('/dashboard')) {
    return NextResponse.redirect(new URL('/login', request.url));
  }

  // Add headers
  const response = NextResponse.next();
  response.headers.set('x-custom-header', 'value');
  return response;
}

export const config = {
  matcher: ['/dashboard/:path*', '/api/:path*'],
};
```

---

## Styling

### CSS Modules (Default)

```tsx
import styles from './Button.module.css';

function Button() {
  return <button className={styles.primary}>Click</button>;
}
```

### Tailwind CSS

```bash
npm install tailwindcss postcss autoprefixer
npx tailwindcss init -p
```

```tsx
function Button() {
  return <button className="bg-blue-500 text-white px-4 py-2">Click</button>;
}
```

### CSS-in-JS

styled-components, Emotion — requires client components.

---

## Performance

### Built-in Optimizations

| Feature | Benefit |
|---------|---------|
| Image Optimization | Automatic resizing, lazy loading |
| Font Optimization | No layout shift |
| Script Optimization | Controlled loading |
| Link Prefetching | Instant navigation |

### Image Component

```tsx
import Image from 'next/image';

function Avatar() {
  return (
    <Image
      src="/avatar.jpg"
      alt="Avatar"
      width={100}
      height={100}
      priority  // LCP image
    />
  );
}
```

### Lazy Loading

```tsx
import dynamic from 'next/dynamic';

const HeavyComponent = dynamic(() => import('./Heavy'), {
  loading: () => <Skeleton />,
  ssr: false,  // Client-only
});
```

---

## Configuration

### next.config.js

```js
/** @type {import('next').NextConfig} */
const nextConfig = {
  images: {
    domains: ['cdn.example.com'],
  },
  experimental: {
    serverActions: true,
  },
  redirects: async () => [
    { source: '/old', destination: '/new', permanent: true },
  ],
  headers: async () => [
    {
      source: '/api/:path*',
      headers: [{ key: 'Access-Control-Allow-Origin', value: '*' }],
    },
  ],
};

module.exports = nextConfig;
```

---

## Deployment

### Vercel (Optimal)

```bash
npm i -g vercel
vercel
```

Zero-config deployment with edge functions.

### Self-Hosted

```bash
npm run build
npm start  # Requires Node.js server
```

### Static Export

```js
// next.config.js
module.exports = {
  output: 'export',
};
```

No server features (SSR, API routes, ISR).

---

## Next.js 14+ Features

| Feature | Description |
|---------|-------------|
| Server Actions | Form handling without API routes |
| Partial Prerendering | Static + dynamic in one route |
| Turbopack | Faster dev server |
| Improved caching | More granular control |

---

## Next.js vs Alternatives

| Aspect | Next.js | Remix | Nuxt | SvelteKit |
|--------|---------|-------|------|-----------|
| Base | React | React | Vue | Svelte |
| Rendering | All modes | SSR-focused | All modes | All modes |
| Backing | Vercel | Shopify | NuxtLabs | Vercel |
| Complexity | Higher | Medium | Medium | Lower |
| Bundle size | Larger | Medium | Medium | Smallest |

---

## When to Use Next.js

**Strengths:**

- Most popular React framework
- Excellent Vercel integration
- All rendering strategies
- Large ecosystem
- Great TypeScript support

**Considerations:**

- Complex mental model (RSC)
- Vercel-optimized (self-host harder)
- Fast-moving (breaking changes)
- Bundle size

**Best for:**

- Production React apps
- SEO-important sites
- E-commerce
- Marketing sites
- SaaS applications

---

## Related

- [[React]]
- [[Remix]]
- [[Nuxt]]
- [[SvelteKit]]
- [[Astro]]
- [[domains/Web Development|Web Development]]
