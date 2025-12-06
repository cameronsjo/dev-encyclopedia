---
title: PHP
aliases:
  - PHP Language
tags:
  - language
  - php
  - web
  - backend
type: reference
status: complete
created: '2025-11-28'
---

# PHP

Web's most deployed server-side language. Powers WordPress, Laravel.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Multi-paradigm (OOP, procedural) |
| Typing | Dynamic (gradually typed 7+) |
| Runtime | Interpreted (Zend Engine) |
| Memory | Garbage collected |
| First appeared | 1995 |
| Current | PHP 8.3+ |

---

## Modern PHP (7-8)

**PHP has evolved significantly.** Modern PHP is not your 2005 PHP.

### Key Improvements

| Version | Features |
|---------|----------|
| 7.0 | Scalar types, return types, 2x performance |
| 7.4 | Typed properties, arrow functions |
| 8.0 | JIT, union types, attributes, named arguments |
| 8.1 | Enums, readonly properties, fibers |
| 8.2 | Readonly classes, true/false/null types |
| 8.3 | Typed class constants, json_validate |

### Type System

```php
// Scalar types
function add(int $a, int $b): int {
    return $a + $b;
}

// Union types
function process(int|string $value): void {}

// Nullable
function find(int $id): ?User {}

// Typed properties
class User {
    public readonly string $name;
    public int $age;
}
```

### Enums (8.1)

```php
enum Status: string {
    case Draft = 'draft';
    case Published = 'published';
}
```

### Attributes (8.0)

```php
#[Route('/users', methods: ['GET'])]
public function index(): Response {}

#[Deprecated]
function oldFunction(): void {}
```

### Named Arguments

```php
function createUser(string $name, int $age, bool $active = true) {}

createUser(name: 'Alice', age: 30);
```

---

## Ecosystem

### Frameworks

| Framework | Style |
|-----------|-------|
| Laravel | Full-featured, elegant |
| Symfony | Components, enterprise |
| Slim | Micro-framework |
| CakePHP | Convention over config |
| Laminas | Enterprise (Zend successor) |

### Laravel

**The dominant framework.** Batteries included.

- Eloquent ORM
- Blade templating
- Artisan CLI
- Queue system
- Testing tools

### Symfony

**Component-based.** Many components used by Laravel.

- Dependency injection
- Event system
- Console component
- HTTP foundation

### Composer

**Standard package manager.**

```bash
composer require vendor/package
```

**Packagist** — Central repository.

---

## CMS / Platforms

PHP powers massive platforms:

| Platform | Market Share |
|----------|--------------|
| WordPress | ~40% of web |
| Drupal | Enterprise CMS |
| Magento | E-commerce |
| MediaWiki | Wikipedia |

---

## Performance

### PHP-FPM

Process manager for production.

### OPcache

Bytecode caching. **Always enable in production.**

### JIT (PHP 8.0)

Just-in-time compilation. Benefits CPU-intensive code.

### Benchmarks

Modern PHP (8+) is competitive with other interpreted languages.

---

## Testing

| Tool | Purpose |
|------|---------|
| PHPUnit | Unit testing |
| Pest | Modern testing syntax |
| Mockery | Mocking |
| PHPStan | Static analysis |
| Psalm | Static analysis |

### Static Analysis

**PHPStan/Psalm** catch bugs before runtime.

```php
// phpstan.neon
parameters:
    level: max
```

---

## Deployment

### Traditional

- Apache + mod_php
- Nginx + PHP-FPM

### Modern

- Docker containers
- Laravel Vapor (serverless)
- Platform.sh
- Laravel Forge

### Shared Hosting

PHP's original deployment model. Still works.

---

## PHP's Reputation

### The Criticism

- Inconsistent stdlib naming
- Historical security issues
- Easy to write bad code
- Mixed metaphors (OOP + procedural)

### The Reality

- Modern PHP (8+) is solid
- Laravel/Symfony are excellent
- WordPress succeeds despite everything
- Huge deployment base

---

## When to Use PHP

**Strengths:**

- Massive ecosystem
- Easy hosting
- WordPress/CMS
- Laravel productivity
- Cheap developers

**Considerations:**

- Legacy reputation
- Type system bolted on
- Not for non-web

**Best for:**

- Web applications
- Content sites
- WordPress ecosystem
- Rapid development

---

## Related

- [[Web Development]]
- [[Web Frameworks]]
- [[Laravel]]
- [[Python]]
