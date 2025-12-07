# HTMX

A lightweight JavaScript library that extends HTML with AJAX, WebSockets, and Server-Sent Events using declarative attributes, enabling modern web interactions without writing JavaScript.

## Overview

| Aspect | Details |
|--------|---------|
| **Type** | Frontend library (hypermedia-driven) |
| **Language** | JavaScript (used via HTML attributes) |
| **Size** | ~14KB gzipped |
| **Paradigm** | Hypermedia as the Engine of Application State (HATEOAS) |
| **First Release** | 2020 |
| **Backend** | Framework-agnostic (works with any server) |
| **Philosophy** | HTML over the wire, progressive enhancement |
| **License** | BSD 2-Clause |

## Core Concepts

### Hypermedia-Driven Architecture

HTMX extends HTML's capabilities to make any element trigger HTTP requests and update the DOM with the response. The server sends HTML fragments rather than JSON, keeping rendering logic server-side.

### Key Attributes

**Request Attributes:**

- `hx-get`, `hx-post`, `hx-put`, `hx-patch`, `hx-delete` — Issue HTTP requests
- `hx-trigger` — Specify what triggers the request (click, change, load, revealed, etc.)
- `hx-include` — Include additional form data in the request
- `hx-params` — Filter which parameters to include

**Response Handling:**

- `hx-target` — Specify which element receives the response
- `hx-swap` — Control how content is swapped (innerHTML, outerHTML, beforebegin, afterend, etc.)
- `hx-select` — Extract a portion of the response HTML
- `hx-swap-oob` — Update multiple targets with a single response (out-of-band swaps)

**Advanced Features:**

- `hx-push-url` — Update browser URL and history
- `hx-boost` — Progressively enhance standard links and forms
- `hx-confirm` — Show confirmation dialog before request
- `hx-headers` — Add custom headers to requests
- `hx-vals` — Add values to request parameters

### Event Model

HTMX triggers custom events throughout the request lifecycle:

- `htmx:beforeRequest` — Before AJAX request sent
- `htmx:afterSwap` — After new content swapped into DOM
- `htmx:responseError` — On HTTP error response
- Allows JavaScript integration when needed

### Advanced Capabilities

**WebSockets:** `hx-ws` attribute enables bidirectional communication
**Server-Sent Events:** `hx-sse` for real-time server push
**History Support:** Browser back/forward with snapshot caching
**Extensions:** Plugin system (client-side validation, debugging, Alpine.js integration, etc.)

## Comparison with Modern Frameworks

| Aspect | HTMX | React | Vue | Alpine.js |
|--------|------|-------|-----|-----------|
| **Paradigm** | Hypermedia-driven | Component-based | Component-based | Behavior-driven |
| **Bundle Size** | ~14KB | ~40KB (React + ReactDOM) | ~34KB | ~15KB |
| **Rendering** | Server-side | Client-side (or SSR) | Client-side (or SSR) | Client-side |
| **State Management** | Server maintains state | Client state required | Client state required | Minimal client state |
| **Learning Curve** | Minimal (HTML attributes) | Moderate (JSX, hooks) | Moderate (SFC, directives) | Low (HTML attributes) |
| **JavaScript Required** | Almost none | Extensive | Moderate | Minimal |
| **Build Step** | None | Required | Optional | None |
| **SEO** | ✅ Natural | Requires SSR | Requires SSR | ✅ Natural |
| **Progressive Enhancement** | ✅ Core philosophy | Challenging | Challenging | ✅ Supported |
| **Backend Coupling** | HTML templates | API-driven | API-driven | API or HTML |
| **Real-time** | WebSockets, SSE | WebSockets + libraries | WebSockets + libraries | WebSockets + libraries |
| **Interactivity** | Server round-trips | Instant client-side | Instant client-side | Instant client-side |

## Integration Patterns

### Backend Framework Support

**Django:**

```python
# Template returns HTML fragment
def search_results(request):
    query = request.GET.get('q')
    results = Articles.objects.filter(title__icontains=query)
    return render(request, 'partials/results.html', {'results': results})
```

**Spring Boot:**

```java
// Thymeleaf fragment
@GetMapping("/users")
public String getUsers(Model model) {
    model.addAttribute("users", userService.findAll());
    return "fragments/user-list :: users";
}
```

**Rails:** Turbo (Rails 7+) uses similar patterns, HTMX provides more fine-grained control

**Go/Templ:** Natural fit with Go's html/template package

### State Management

Server maintains application state; client sends requests to update. No client-side state synchronization needed. Session/database handles persistence.

### Client-Side Enhancement

Can combine with Alpine.js for local interactivity (dropdowns, modals) while using HTMX for server communication.

## When to Use

### Strengths

✅ **Simple applications** — CRUD apps, dashboards, admin panels
✅ **Server-rendered apps** — Leverage existing backend templates
✅ **Small teams** — Fewer specializations needed (full-stack developers)
✅ **Progressive enhancement** — Works without JavaScript, enhances with it
✅ **Rapid prototyping** — No build step, minimal boilerplate
✅ **SEO-first** — Server rendering by default
✅ **Low bandwidth** — Send HTML diffs, not full JSON + client rendering
✅ **Backend-heavy teams** — Frontend complexity stays minimal

### Considerations

❌ **Highly interactive UIs** — Games, drawing apps, real-time collaboration
❌ **Offline-first apps** — Client state management required
❌ **Complex client state** — Shopping carts, multi-step forms with branching
❌ **Mobile apps** — Not designed for native mobile
❌ **Large SPAs** — React/Vue better for complex routing and state
❌ **GraphQL-centric** — Works best with REST/RPC endpoints returning HTML

### Best For

- Content-driven websites with dynamic elements
- Traditional web apps moving from full page reloads to AJAX
- Prototypes and MVPs with limited frontend resources
- Teams wanting simplicity over sophisticated client architecture
- Applications where server-side rendering is already strong

## Related

- [[Domains/Web Development]]
- [[Languages/JavaScript]]
- [[Alpine.js]] — Complementary library for client-side interactivity
- [[React]] — Component-based alternative
- [[Vue]] — Progressive framework alternative
- [[Turbo]] — Rails' similar approach to server-driven UIs
