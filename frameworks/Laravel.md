---
title: Laravel
aliases:
  - Laravel PHP
tags:
  - framework
  - backend
  - web
  - php
  - full-stack
type: reference
status: complete
created: '2025-11-28'
---

# Laravel

The PHP framework for web artisans.

## Overview

| Aspect | Details |
|--------|---------|
| Language | PHP |
| Type | Full-stack framework |
| Architecture | MVC, expressive syntax |
| First release | 2011 |
| Creator | Taylor Otwell |
| Backing | Laravel LLC |

---

## Philosophy

### Expressive, Elegant Syntax

Laravel prioritizes developer experience with clean, readable code.

```php
// Expressive query building
$users = User::where('active', true)
    ->orderBy('created_at', 'desc')
    ->take(10)
    ->get();

// Fluent validation
$validated = $request->validate([
    'email' => 'required|email|unique:users',
    'password' => 'required|min:8|confirmed',
]);
```

---

## Core Concepts

### MVC Architecture

| Component | Location | Purpose |
|-----------|----------|---------|
| Models | app/Models/ | Data & business logic |
| Views | resources/views/ | Blade templates |
| Controllers | app/Http/Controllers/ | Request handling |

### Eloquent ORM

Active Record implementation.

```php
class User extends Model
{
    protected $fillable = ['name', 'email'];

    public function posts()
    {
        return $this->hasMany(Post::class);
    }
}

// Usage
$user = User::create(['name' => 'John', 'email' => 'john@example.com']);
$user->posts()->create(['title' => 'First Post']);
User::where('active', true)->with('posts')->get();
```

### Blade Templating

```blade
{{-- resources/views/users/show.blade.php --}}
@extends('layouts.app')

@section('content')
    <h1>{{ $user->name }}</h1>

    @if($user->posts->count())
        @foreach($user->posts as $post)
            <article>{{ $post->title }}</article>
        @endforeach
    @else
        <p>No posts yet.</p>
    @endif
@endsection
```

---

## Routing

```php
// routes/web.php
Route::get('/', [HomeController::class, 'index']);

Route::resource('users', UserController::class);

Route::middleware('auth')->group(function () {
    Route::get('/dashboard', [DashboardController::class, 'index']);
});

// routes/api.php
Route::prefix('v1')->group(function () {
    Route::apiResource('articles', ArticleController::class);
});
```

### Resource Controllers

`Route::resource('users', UserController::class)` generates:

| Method | URI | Action |
|--------|-----|--------|
| GET | /users | index |
| GET | /users/{id} | show |
| GET | /users/create | create |
| POST | /users | store |
| GET | /users/{id}/edit | edit |
| PUT/PATCH | /users/{id} | update |
| DELETE | /users/{id} | destroy |

---

## Controllers

```php
class UserController extends Controller
{
    public function index()
    {
        $users = User::paginate(15);
        return view('users.index', compact('users'));
    }

    public function store(StoreUserRequest $request)
    {
        $user = User::create($request->validated());
        return redirect()->route('users.show', $user)
            ->with('success', 'User created!');
    }

    public function show(User $user)
    {
        return view('users.show', compact('user'));
    }
}
```

### Route Model Binding

Laravel automatically resolves model instances.

```php
// Route
Route::get('/users/{user}', [UserController::class, 'show']);

// Controller - $user is automatically resolved
public function show(User $user) { ... }
```

---

## Validation

### Form Requests

```php
class StoreUserRequest extends FormRequest
{
    public function authorize(): bool
    {
        return true;
    }

    public function rules(): array
    {
        return [
            'name' => 'required|string|max:255',
            'email' => 'required|email|unique:users',
            'password' => 'required|min:8|confirmed',
        ];
    }
}
```

### Validation Rules

| Rule | Purpose |
|------|---------|
| required | Must be present |
| email | Valid email format |
| unique:table | Unique in database |
| confirmed | Matches {field}_confirmation |
| min:n, max:n | Length constraints |
| in:a,b,c | Must be in list |

---

## Database

### Migrations

```php
class CreateUsersTable extends Migration
{
    public function up(): void
    {
        Schema::create('users', function (Blueprint $table) {
            $table->id();
            $table->string('name');
            $table->string('email')->unique();
            $table->timestamp('email_verified_at')->nullable();
            $table->string('password');
            $table->timestamps();
        });
    }

    public function down(): void
    {
        Schema::dropIfExists('users');
    }
}
```

```bash
php artisan migrate
php artisan migrate:rollback
php artisan db:seed
```

### Relationships

```php
class User extends Model
{
    public function posts() { return $this->hasMany(Post::class); }
    public function profile() { return $this->hasOne(Profile::class); }
    public function roles() { return $this->belongsToMany(Role::class); }
}

class Post extends Model
{
    public function user() { return $this->belongsTo(User::class); }
    public function tags() { return $this->belongsToMany(Tag::class); }
}
```

---

## Authentication

### Laravel Breeze

Simple authentication scaffolding.

```bash
composer require laravel/breeze --dev
php artisan breeze:install
```

### Laravel Sanctum

API token authentication.

```php
// Issue token
$token = $user->createToken('api-token')->plainTextToken;

// Protect routes
Route::middleware('auth:sanctum')->group(function () {
    Route::get('/user', fn() => auth()->user());
});
```

### Starter Kits

| Kit | Stack |
|-----|-------|
| Breeze | Blade, Livewire, or React/Vue |
| Jetstream | Livewire or Inertia (more features) |
| Fortify | Headless auth backend |

---

## Frontend

### Livewire

Full-stack framework for dynamic UIs without JavaScript.

```php
class Counter extends Component
{
    public int $count = 0;

    public function increment()
    {
        $this->count++;
    }

    public function render()
    {
        return view('livewire.counter');
    }
}
```

```blade
<div>
    <h1>{{ $count }}</h1>
    <button wire:click="increment">+</button>
</div>
```

### Inertia.js

Use React/Vue with Laravel backend.

```php
// Controller
return Inertia::render('Users/Show', [
    'user' => $user,
]);
```

```jsx
// React component
export default function Show({ user }) {
    return <h1>{user.name}</h1>;
}
```

---

## Queues & Jobs

```php
class ProcessPodcast implements ShouldQueue
{
    use Dispatchable, InteractsWithQueue, Queueable, SerializesModels;

    public function __construct(public Podcast $podcast) {}

    public function handle(): void
    {
        // Process the podcast...
    }
}

// Dispatch
ProcessPodcast::dispatch($podcast);
ProcessPodcast::dispatch($podcast)->delay(now()->addMinutes(10));
```

### Queue Backends

| Backend | Notes |
|---------|-------|
| Database | Simple, built-in |
| Redis | Fast, recommended |
| SQS | AWS integration |
| Beanstalkd | Dedicated queue |

---

## Artisan CLI

```bash
php artisan make:model User -mfc  # Model + migration + factory + controller
php artisan make:controller UserController --resource
php artisan make:request StoreUserRequest
php artisan tinker  # REPL
php artisan route:list
php artisan optimize:clear
```

---

## Testing

```php
class UserTest extends TestCase
{
    use RefreshDatabase;

    public function test_users_can_be_created(): void
    {
        $response = $this->post('/users', [
            'name' => 'John',
            'email' => 'john@example.com',
            'password' => 'password',
            'password_confirmation' => 'password',
        ]);

        $response->assertRedirect('/users/1');
        $this->assertDatabaseHas('users', ['email' => 'john@example.com']);
    }
}
```

### Testing Utilities

| Feature | Purpose |
|---------|---------|
| RefreshDatabase | Clean DB per test |
| Factories | Generate test data |
| HTTP tests | Test routes |
| Browser tests | Dusk for E2E |

---

## Key Packages

### First-Party

| Package | Purpose |
|---------|---------|
| Sanctum | API authentication |
| Horizon | Redis queue dashboard |
| Telescope | Debug assistant |
| Cashier | Subscription billing |
| Scout | Full-text search |
| Socialite | OAuth authentication |

### Third-Party

| Package | Purpose |
|---------|---------|
| Spatie packages | Permissions, media, etc. |
| Laravel Debugbar | Development debugging |
| Laravel Excel | Spreadsheet import/export |

---

## Deployment

### Laravel Forge

Official deployment platform.

### Other Options

| Platform | Notes |
|----------|-------|
| Vapor | Serverless on AWS |
| Forge | Server management |
| Ploi | Alternative to Forge |
| Docker | Self-hosted |

### Optimization

```bash
php artisan config:cache
php artisan route:cache
php artisan view:cache
php artisan optimize
```

---

## Laravel vs Alternatives

| Aspect | Laravel | Rails | Django |
|--------|---------|-------|--------|
| Language | PHP | Ruby | Python |
| Community | Largest (PHP) | Mature | Large |
| Learning curve | Moderate | Moderate | Moderate |
| Ecosystem | Huge | Large | Large |
| Performance | Good | Good | Good |
| Real-time | Livewire, Echo | Hotwire | Channels |

---

## When to Use Laravel

**Strengths:**

- Elegant, expressive syntax
- Huge PHP ecosystem
- Excellent documentation
- Livewire for reactive UIs
- Strong first-party packages
- Active development

**Considerations:**

- PHP reputation (though modern PHP is good)
- Hosting requirements (PHP hosting common)
- Some "magic" can be confusing

**Best for:**

- Web applications with CRUD
- SaaS products
- APIs (with Sanctum)
- Teams with PHP experience
- Rapid development needs

---

## Related

- [[PHP]]
- [[Rails]]
- [[Django]]
- [[Spring Boot]]
- [[domains/Web Development|Web Development]]
