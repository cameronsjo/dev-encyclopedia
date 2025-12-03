---
title: Remix
aliases:
  - Remix Run
tags:
  - framework
  - web
  - frontend
  - fullstack
  - react
  - typescript
type: reference
status: complete
created: '2025-11-28'
---

# Remix

Full-stack web framework focused on web standards.

## Overview

| Aspect | Details |
|--------|---------|
| Base | React |
| Type | Full-stack meta-framework |
| Philosophy | Web standards, progressive enhancement |
| Language | TypeScript/JavaScript |
| Backing | Shopify |
| First release | 2021 |

---

## Why Remix?

### Philosophy Differences

| Next.js | Remix |
|---------|-------|
| Many rendering modes | SSR-focused |
| React-centric | Web standards |
| API routes | Loaders/actions |
| Client-heavy | Server-first |

### Core Principles

- **Web fundamentals:** Forms, HTTP, URLs
- **Progressive enhancement:** Works without JS
- **Nested routing:** Parallel data loading
- **No loading spinners:** Data before render

---

## Project Structure

```
my-remix-app/
├── app/
│   ├── entry.client.tsx
│   ├── entry.server.tsx
│   ├── root.tsx
│   └── routes/
│       ├── _index.tsx          # /
│       ├── about.tsx           # /about
│       ├── blog._index.tsx     # /blog
│       ├── blog.$slug.tsx      # /blog/:slug
│       └── dashboard/
│           ├── _layout.tsx     # Layout
│           ├── _index.tsx      # /dashboard
│           └── settings.tsx    # /dashboard/settings
├── public/
└── remix.config.js
```

---

## Routing

### File Naming Conventions

| File | Route |
|------|-------|
| _index.tsx | Index route |
| about.tsx | /about |
| blog.$slug.tsx | /blog/:slug |
| $.tsx | Splat/catch-all |
| _layout.tsx | Layout (no URL segment) |
| blog_.new.tsx | /blog/new (escape nesting) |

### Nested Routes

```
routes/
├── dashboard.tsx           # /dashboard layout
├── dashboard._index.tsx    # /dashboard (index)
├── dashboard.profile.tsx   # /dashboard/profile
└── dashboard.settings.tsx  # /dashboard/settings
```

```tsx
// routes/dashboard.tsx
import { Outlet } from '@remix-run/react';

export default function DashboardLayout() {
  return (
    <div className="dashboard">
      <Sidebar />
      <main>
        <Outlet />  {/* Child routes render here */}
      </main>
    </div>
  );
}
```

---

## Data Loading

### Loaders

Server-side data fetching.

```tsx
// routes/posts.$slug.tsx
import { json, LoaderFunctionArgs } from '@remix-run/node';
import { useLoaderData } from '@remix-run/react';

export async function loader({ params }: LoaderFunctionArgs) {
  const post = await db.post.findUnique({
    where: { slug: params.slug }
  });

  if (!post) {
    throw new Response('Not Found', { status: 404 });
  }

  return json({ post });
}

export default function Post() {
  const { post } = useLoaderData<typeof loader>();

  return (
    <article>
      <h1>{post.title}</h1>
      <div dangerouslySetInnerHTML={{ __html: post.content }} />
    </article>
  );
}
```

### Parallel Loading

Nested routes load data in parallel.

```
/dashboard/settings

Dashboard loader  ─┬─▶ Dashboard component
                   │
Settings loader   ─┘─▶ Settings component (in Outlet)

Both load simultaneously, not sequentially!
```

---

## Actions

Handle form submissions server-side.

```tsx
// routes/login.tsx
import { ActionFunctionArgs, redirect } from '@remix-run/node';
import { Form, useActionData } from '@remix-run/react';

export async function action({ request }: ActionFunctionArgs) {
  const formData = await request.formData();
  const email = formData.get('email');
  const password = formData.get('password');

  const user = await authenticate(email, password);

  if (!user) {
    return json({ error: 'Invalid credentials' }, { status: 401 });
  }

  return redirect('/dashboard', {
    headers: {
      'Set-Cookie': await createSession(user.id)
    }
  });
}

export default function Login() {
  const actionData = useActionData<typeof action>();

  return (
    <Form method="post">
      <input name="email" type="email" required />
      <input name="password" type="password" required />
      <button type="submit">Log In</button>

      {actionData?.error && (
        <p className="error">{actionData.error}</p>
      )}
    </Form>
  );
}
```

### Multiple Actions

```tsx
export async function action({ request }: ActionFunctionArgs) {
  const formData = await request.formData();
  const intent = formData.get('intent');

  switch (intent) {
    case 'create':
      return createItem(formData);
    case 'delete':
      return deleteItem(formData);
    default:
      throw new Response('Invalid intent', { status: 400 });
  }
}

// In component
<Form method="post">
  <input name="title" />
  <button name="intent" value="create">Create</button>
</Form>

<Form method="post">
  <input type="hidden" name="id" value={item.id} />
  <button name="intent" value="delete">Delete</button>
</Form>
```

---

## Forms

### The `<Form>` Component

Progressive enhancement - works without JS.

```tsx
import { Form } from '@remix-run/react';

// Standard form - full page navigation
<Form method="post" action="/subscribe">
  <input name="email" />
  <button>Subscribe</button>
</Form>

// With JS, prevents default and fetches
// Falls back to normal form without JS
```

### useFetcher

For forms without navigation.

```tsx
import { useFetcher } from '@remix-run/react';

function LikeButton({ postId }: { postId: string }) {
  const fetcher = useFetcher();
  const isLiking = fetcher.state !== 'idle';

  return (
    <fetcher.Form method="post" action={`/posts/${postId}/like`}>
      <button disabled={isLiking}>
        {isLiking ? 'Liking...' : 'Like'}
      </button>
    </fetcher.Form>
  );
}
```

### Optimistic UI

```tsx
function TodoItem({ todo }) {
  const fetcher = useFetcher();

  // Optimistic state
  const isDeleting = fetcher.state !== 'idle' &&
    fetcher.formData?.get('intent') === 'delete';

  if (isDeleting) return null; // Optimistically hide

  return (
    <li>
      <span>{todo.title}</span>
      <fetcher.Form method="post">
        <input type="hidden" name="id" value={todo.id} />
        <button name="intent" value="delete">Delete</button>
      </fetcher.Form>
    </li>
  );
}
```

---

## Error Handling

### Error Boundaries

```tsx
// routes/posts.$slug.tsx
import { isRouteErrorResponse, useRouteError } from '@remix-run/react';

export function ErrorBoundary() {
  const error = useRouteError();

  if (isRouteErrorResponse(error)) {
    return (
      <div>
        <h1>{error.status}</h1>
        <p>{error.statusText}</p>
      </div>
    );
  }

  return (
    <div>
      <h1>Error</h1>
      <p>{error instanceof Error ? error.message : 'Unknown error'}</p>
    </div>
  );
}
```

### Scoped to Route

Each route can have its own ErrorBoundary - errors don't break the whole app.

---

## Headers & Meta

### Meta Tags

```tsx
import { MetaFunction } from '@remix-run/node';

export const meta: MetaFunction<typeof loader> = ({ data }) => {
  return [
    { title: data?.post.title ?? 'Blog' },
    { name: 'description', content: data?.post.excerpt },
  ];
};
```

### HTTP Headers

```tsx
import { HeadersFunction } from '@remix-run/node';

export const headers: HeadersFunction = () => ({
  'Cache-Control': 'max-age=300, s-maxage=3600',
});
```

---

## Resource Routes

Return non-HTML responses.

```tsx
// routes/api.users.tsx
import { json, LoaderFunctionArgs } from '@remix-run/node';

export async function loader({ request }: LoaderFunctionArgs) {
  const users = await db.user.findMany();
  return json(users);
}

// No default export = no UI
```

```tsx
// routes/posts.$slug.pdf.tsx
export async function loader({ params }: LoaderFunctionArgs) {
  const post = await getPost(params.slug);
  const pdf = await generatePDF(post);

  return new Response(pdf, {
    headers: {
      'Content-Type': 'application/pdf',
      'Content-Disposition': `attachment; filename="${post.title}.pdf"`,
    },
  });
}
```

---

## Deployment

### Adapters

| Adapter | Platform |
|---------|----------|
| @remix-run/vercel | Vercel |
| @remix-run/netlify | Netlify |
| @remix-run/cloudflare | Cloudflare Workers |
| @remix-run/express | Express server |
| @remix-run/architect | AWS Lambda |

### Stacks

Official templates:

| Stack | Features |
|-------|----------|
| Blues | Fly.io, SQLite |
| Indie | Fly.io, SQLite, auth |
| Grunge | AWS, DynamoDB |

```bash
npx create-remix@latest --template remix-run/indie-stack
```

---

## Remix vs Next.js

| Aspect | Remix | Next.js |
|--------|-------|---------|
| Philosophy | Web standards | React-centric |
| Data loading | Loaders | RSC, fetch |
| Mutations | Actions, Form | Server Actions |
| Rendering | SSR | SSR, SSG, ISR |
| Forms | Built-in | Libraries |
| Caching | Standard HTTP | Custom |
| Learning curve | Lower | Higher |
| Ecosystem | Smaller | Larger |

---

## When to Use Remix

**Strengths:**

- Web fundamentals focus
- Progressive enhancement
- Parallel data loading
- Simple mental model
- Great form handling
- Works without JavaScript

**Considerations:**

- Smaller ecosystem
- Fewer tutorials
- SSR-focused (no SSG)

**Best for:**

- Form-heavy applications
- Progressive enhancement needs
- Teams valuing web standards
- CRUD applications

---

## Related

- [[React]]
- [[Next.js]]
- [[SvelteKit]]
- [[domains/Web Development|Web Development]]
