---
title: Angular
aliases:
  - Angular.js
  - AngularJS
tags:
  - framework
  - web
  - frontend
  - typescript
  - google
type: reference
status: complete
created: '2025-11-28'
---

# Angular

Opinionated, full-featured framework for enterprise web applications.

## Overview

| Aspect | Details |
|--------|---------|
| Type | Full framework |
| Language | TypeScript (required) |
| Rendering | Incremental DOM, Signals |
| Architecture | Component-based, DI, modules |
| First release | 2016 (Angular 2+) |
| Backing | Google |

**Note:** Angular (v2+) is a complete rewrite of AngularJS (v1.x). They are different frameworks.

---

## Core Concepts

### Components

```typescript
@Component({
  selector: 'app-greeting',
  template: `<h1>Hello, {{ name }}!</h1>`,
  standalone: true
})
export class GreetingComponent {
  name = 'World';
}
```

### Standalone Components

Modern Angular approach (v14+). No NgModules required.

```typescript
@Component({
  standalone: true,
  imports: [CommonModule, RouterModule],
  // ...
})
```

### Templates

HTML with Angular-specific syntax.

| Syntax | Purpose |
|--------|---------|
| `{{ value }}` | Interpolation |
| `[property]` | Property binding |
| `(event)` | Event binding |
| `[(ngModel)]` | Two-way binding |
| `*ngIf`, `*ngFor` | Structural directives |
| `@if`, `@for` | Control flow (v17+) |

---

## Angular Architecture

### Dependency Injection

Core to Angular's design.

```typescript
@Injectable({ providedIn: 'root' })
export class UserService {
  getUsers() { /* ... */ }
}

@Component({ /* ... */ })
export class UserListComponent {
  constructor(private userService: UserService) {}
}
```

### Services

Business logic and data access.

### Modules (Legacy)

NgModules organize related code. Being replaced by standalone components.

### Pipes

Transform displayed values.

```html
{{ date | date:'short' }}
{{ price | currency:'USD' }}
{{ name | uppercase }}
```

---

## Signals (v16+)

Modern reactive primitive.

```typescript
@Component({ /* ... */ })
export class CounterComponent {
  count = signal(0);
  doubled = computed(() => this.count() * 2);

  increment() {
    this.count.update(n => n + 1);
  }
}
```

### Signals vs RxJS

| Use Case | Recommendation |
|----------|----------------|
| UI state | Signals |
| Async operations | RxJS |
| Event streams | RxJS |
| Simple state | Signals |
| Complex transformations | RxJS |

**Direction:** Angular moving toward Signals for simplicity.

---

## State Management

### Built-in Options

| Approach | Use Case |
|----------|----------|
| Services | Shared state |
| Signals | Reactive state |
| RxJS BehaviorSubject | Observable state |

### Libraries

| Library | Philosophy |
|---------|------------|
| NgRx | Redux pattern, RxJS |
| NGXS | Simpler Redux |
| Akita | Minimal boilerplate |
| Elf | Modern, minimal |

### When to Use What

| Scenario | Recommendation |
|----------|----------------|
| Simple app | Services + Signals |
| Medium complexity | Services + RxJS |
| Large enterprise | NgRx or NGXS |

---

## Angular CLI

Powerful code generation and tooling.

```bash
ng new my-app          # Create project
ng generate component  # Generate component
ng serve               # Dev server
ng build               # Production build
ng test                # Unit tests
ng e2e                 # E2E tests
```

### Schematics

Code generation templates. CLI and libraries provide these.

---

## Routing

Built-in router.

```typescript
const routes: Routes = [
  { path: '', component: HomeComponent },
  { path: 'users', component: UsersComponent },
  { path: 'users/:id', component: UserDetailComponent },
  { path: '**', component: NotFoundComponent }
];
```

### Features

| Feature | Description |
|---------|-------------|
| Lazy loading | Load modules on demand |
| Guards | Route protection |
| Resolvers | Pre-fetch data |
| Child routes | Nested routing |

---

## Forms

### Two Approaches

| Type | Use Case |
|------|----------|
| Template-driven | Simple forms |
| Reactive | Complex forms, validation |

### Reactive Forms

```typescript
form = new FormGroup({
  name: new FormControl('', Validators.required),
  email: new FormControl('', [Validators.required, Validators.email])
});
```

**Recommendation:** Use Reactive Forms for anything non-trivial.

---

## HTTP Client

Built-in with interceptors.

```typescript
@Injectable({ providedIn: 'root' })
export class ApiService {
  constructor(private http: HttpClient) {}

  getUsers() {
    return this.http.get<User[]>('/api/users');
  }
}
```

### Interceptors

Middleware for HTTP requests/responses.

```typescript
export const authInterceptor: HttpInterceptorFn = (req, next) => {
  const token = inject(AuthService).getToken();
  const authReq = req.clone({
    headers: req.headers.set('Authorization', `Bearer ${token}`)
  });
  return next(authReq);
};
```

---

## Testing

### Built-in Support

| Tool | Purpose |
|------|---------|
| Karma | Test runner (default) |
| Jasmine | Test framework (default) |
| TestBed | Component testing utility |

### Modern Alternative

| Tool | Purpose |
|------|---------|
| Jest | Test runner |
| Spectator | Testing utilities |
| Playwright | E2E testing |

---

## Modern Angular (v17+)

### Key Changes

| Feature | Description |
|---------|-------------|
| Standalone by default | No NgModules required |
| Control flow | `@if`, `@for`, `@switch` |
| Deferrable views | `@defer` for lazy loading |
| Signals | Simpler reactivity |
| New build system | Esbuild + Vite |

### Control Flow Syntax

```html
@if (user) {
  <p>Welcome, {{ user.name }}</p>
} @else {
  <p>Please log in</p>
}

@for (item of items; track item.id) {
  <li>{{ item.name }}</li>
}
```

---

## Performance

### Built-in Optimizations

| Feature | Benefit |
|---------|---------|
| AOT compilation | Faster startup |
| Tree-shaking | Smaller bundles |
| Lazy loading | Code splitting |
| OnPush change detection | Skip unchanged components |

### Manual Optimizations

| Technique | When |
|-----------|------|
| OnPush strategy | Large component trees |
| trackBy | Lists with updates |
| Lazy loading routes | Large apps |
| Pure pipes | Expensive transformations |

---

## Angular vs React vs Vue

| Aspect | Angular | React | Vue |
|--------|---------|-------|-----|
| Type | Framework | Library | Framework |
| Language | TypeScript | JS/TS | JS/TS |
| Size | Large | Medium | Small |
| Learning curve | Steep | Moderate | Gentle |
| Opinions | Many | Few | Some |
| Built-in | Most features | Minimal | Router, devtools |
| Enterprise | Strong | Strong | Growing |

---

## When Angular Wins

- Large teams needing consistency
- Enterprise with TypeScript mandate
- Complex, long-lived applications
- Teams familiar with Angular patterns
- Need built-in solutions (forms, HTTP, routing)

### When to Look Elsewhere

- Small projects (overkill)
- Teams preferring flexibility
- Rapid prototyping
- Hiring challenges

---

## Common Patterns

### Smart/Dumb Components

```
SmartComponent (container)
├── Handles data/state
├── Injects services
└── DumbComponent (presentational)
    ├── @Input() data
    └── @Output() events
```

### Feature Modules (Legacy)

Organize by feature, lazy load.

### Barrel Exports

`index.ts` files for clean imports.

---

## When to Use Angular

**Strengths:**

- Complete framework (batteries included)
- TypeScript-first
- Consistent patterns across projects
- Strong enterprise adoption
- Great tooling (CLI, DevTools)
- Google backing and long-term support

**Considerations:**

- Steep learning curve
- Verbose for small projects
- Large bundle (improving)
- RxJS learning curve (optional now)

**Best for:**

- Enterprise applications
- Large teams
- Long-term projects
- TypeScript-first organizations
- When consistency > flexibility

---

## Related

- [[React]]
- [[Vue]]
- [[Svelte]]
- [[TypeScript]]
- [[Domains/Web Development|Web Development]]
