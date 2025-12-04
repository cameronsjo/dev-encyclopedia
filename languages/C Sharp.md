---
title: C Sharp
aliases:
  - C#
  - CSharp
tags:
  - language
  - dotnet
  - desktop
  - web
  - game
  - csharp
type: reference
status: complete
created: '2025-12-03'
---

# C Sharp

Modern, type-safe language. Powers .NET, Unity, and enterprise applications.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Multi-paradigm (OOP, functional) |
| Typing | Static, strong |
| Runtime | .NET (CLR) |
| Memory | Garbage collected |
| First appeared | 2000 (Microsoft) |
| Current | C# 12 / .NET 8 |

---

## Modern C# Features

### Records (C# 9+)

Immutable reference types with value semantics.

```csharp
public record User(string Name, string Email);

// With additional members
public record Product(string Name, decimal Price)
{
    public string Display => $"{Name}: ${Price}";
}

// Non-destructive mutation
var updated = user with { Email = "new@email.com" };
```

### Pattern Matching

```csharp
// Type patterns
if (obj is string s)
{
    Console.WriteLine(s.Length);
}

// Switch expressions
string GetCategory(int score) => score switch
{
    >= 90 => "A",
    >= 80 => "B",
    >= 70 => "C",
    _ => "F"
};

// Property patterns
if (user is { Age: >= 18, Active: true })
{
    // Adult active user
}

// List patterns (C# 11)
int[] nums = { 1, 2, 3 };
if (nums is [1, 2, ..])
{
    // Starts with 1, 2
}
```

### Nullable Reference Types

```csharp
#nullable enable

string name = "Alice";     // Non-null
string? nickname = null;   // Nullable

// Null-forgiving operator
string definitelyNotNull = nickname!;

// Null-conditional
int? length = nickname?.Length;

// Null-coalescing
string display = nickname ?? "No nickname";
```

### Primary Constructors (C# 12)

```csharp
public class User(string name, string email)
{
    public string Name { get; } = name;
    public string Email { get; } = email;
}
```

---

## Async/Await

```csharp
public async Task<User> GetUserAsync(int id)
{
    var response = await httpClient.GetAsync($"/users/{id}");
    response.EnsureSuccessStatusCode();
    return await response.Content.ReadFromJsonAsync<User>();
}

// Parallel async
var tasks = ids.Select(id => GetUserAsync(id));
var users = await Task.WhenAll(tasks);

// Cancellation
public async Task ProcessAsync(CancellationToken ct)
{
    await Task.Delay(1000, ct);
    ct.ThrowIfCancellationRequested();
}
```

---

## LINQ

Language Integrated Query — query any collection.

```csharp
var adults = users
    .Where(u => u.Age >= 18)
    .OrderBy(u => u.Name)
    .Select(u => new { u.Name, u.Email });

// Query syntax
var query = from u in users
            where u.Age >= 18
            orderby u.Name
            select new { u.Name, u.Email };

// Aggregations
var avgAge = users.Average(u => u.Age);
var grouped = users.GroupBy(u => u.Department);
```

---

## Dependency Injection

Built into .NET Core.

```csharp
// Registration
builder.Services.AddScoped<IUserService, UserService>();
builder.Services.AddSingleton<ICache, RedisCache>();
builder.Services.AddTransient<IEmailSender, SmtpEmailSender>();

// Injection
public class UserController : ControllerBase
{
    private readonly IUserService _userService;

    public UserController(IUserService userService)
    {
        _userService = userService;
    }
}
```

---

## ASP.NET Core

### Minimal APIs

```csharp
var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();

app.MapGet("/", () => "Hello World");

app.MapGet("/users/{id}", async (int id, IUserService service) =>
{
    var user = await service.GetByIdAsync(id);
    return user is null ? Results.NotFound() : Results.Ok(user);
});

app.MapPost("/users", async (User user, IUserService service) =>
{
    var created = await service.CreateAsync(user);
    return Results.Created($"/users/{created.Id}", created);
});

app.Run();
```

### Controllers

```csharp
[ApiController]
[Route("api/[controller]")]
public class UsersController : ControllerBase
{
    private readonly IUserService _service;

    public UsersController(IUserService service) => _service = service;

    [HttpGet("{id}")]
    public async Task<ActionResult<User>> Get(int id)
    {
        var user = await _service.GetByIdAsync(id);
        return user is null ? NotFound() : Ok(user);
    }

    [HttpPost]
    public async Task<ActionResult<User>> Create(CreateUserDto dto)
    {
        var user = await _service.CreateAsync(dto);
        return CreatedAtAction(nameof(Get), new { id = user.Id }, user);
    }
}
```

---

## Entity Framework Core

ORM for .NET.

```csharp
// DbContext
public class AppDbContext : DbContext
{
    public DbSet<User> Users => Set<User>();
    public DbSet<Order> Orders => Set<Order>();

    protected override void OnModelCreating(ModelBuilder builder)
    {
        builder.Entity<User>()
            .HasMany(u => u.Orders)
            .WithOne(o => o.User);
    }
}

// Queries
var users = await context.Users
    .Where(u => u.Active)
    .Include(u => u.Orders)
    .ToListAsync();

// Transactions
await using var transaction = await context.Database.BeginTransactionAsync();
try
{
    // operations
    await context.SaveChangesAsync();
    await transaction.CommitAsync();
}
catch
{
    await transaction.RollbackAsync();
    throw;
}
```

---

## Ecosystem

### Frameworks

| Framework | Use Case |
|-----------|----------|
| ASP.NET Core | Web APIs, MVC |
| Blazor | Web UI (C# in browser) |
| WPF | Windows desktop |
| MAUI | Cross-platform mobile/desktop |
| Unity | Game development |

### Testing

| Tool | Purpose |
|------|---------|
| xUnit | Test framework |
| NUnit | Test framework |
| Moq | Mocking |
| FluentAssertions | Assertions |
| Bogus | Fake data |

### Tooling

```bash
dotnet new webapi          # Create project
dotnet build               # Build
dotnet run                 # Run
dotnet test                # Test
dotnet publish             # Publish
dotnet ef migrations add   # EF migrations
```

---

## When to Use C#

**Strengths:**

- Modern, feature-rich language
- Excellent tooling (VS, Rider)
- Strong enterprise adoption
- Cross-platform (.NET Core+)
- Unity game development
- Mature ecosystem

**Considerations:**

- .NET ecosystem commitment
- Runtime overhead vs Go/Rust
- Historically Windows-focused

**Best for:**

- Enterprise applications
- Web APIs (ASP.NET Core)
- Game development (Unity)
- Desktop apps (WPF, MAUI)
- Cloud services (Azure)

---

## Related

- [[dotNET MAUI]]
- [[Unity]]
- [[Avalonia]]
- [[Web Frameworks]]
- [[Testing Frameworks]]
