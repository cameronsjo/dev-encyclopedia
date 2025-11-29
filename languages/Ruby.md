---
title: Ruby
aliases:
  - Ruby Language
tags:
  - language
  - ruby
  - rails
  - web
type: reference
status: complete
created: 2025-11-28
---

# Ruby

Optimized for developer happiness. Powers Rails.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | OOP, functional |
| Typing | Dynamic, strong |
| Runtime | Interpreted (CRuby/YARV) |
| Memory | Garbage collected |
| First appeared | 1995 (Matz) |
| Current | Ruby 3.3+ |

---

## Philosophy

**"Optimized for developer happiness."**

- Principle of least surprise
- Everything is an object
- Expressive, readable syntax
- Multiple ways to do things

---

## Key Features

### Blocks and Procs

```ruby
# Block
[1, 2, 3].each { |n| puts n }

# Multi-line block
[1, 2, 3].each do |n|
  puts n
end

# Proc (stored block)
double = ->(x) { x * 2 }
double.call(5)  # 10
```

### Everything is an Object

```ruby
5.times { puts "Hello" }
"hello".upcase
[1, 2, 3].first
```

### Metaprogramming

Ruby's superpower (and foot-gun).

```ruby
class User
  attr_accessor :name, :age  # Generates getters/setters
end

# Define methods dynamically
define_method :greet do
  "Hello!"
end
```

### Symbols

Immutable, interned strings.

```ruby
:name  # Symbol
{ name: "Alice", age: 30 }  # Symbol keys
```

### Pattern Matching (3.0+)

```ruby
case data
in { name:, age: } if age > 18
  puts "Adult: #{name}"
in [first, *rest]
  puts "Array starting with #{first}"
end
```

---

## Ruby 3.x

### Ractor (Parallelism)

True parallelism, no GVL restrictions.

```ruby
r = Ractor.new { heavy_computation }
result = r.take
```

### Fiber Scheduler (Async)

Non-blocking I/O.

```ruby
Fiber.set_scheduler(scheduler)
Fiber.schedule { fetch_data }
```

### Static Typing (RBS/Sorbet)

Optional type annotations.

```ruby
# signature file (.rbs)
class User
  attr_reader name: String
  def greet: () -> String
end
```

### YJIT

JIT compiler in Ruby 3.1+. Significant speedups.

---

## Rails

**Ruby on Rails** — The framework that made Ruby famous.

### Philosophy

- Convention over configuration
- Don't Repeat Yourself (DRY)
- MVC architecture
- "Omakase" (chef's choice)

### Components

| Component | Purpose |
|-----------|---------|
| Active Record | ORM |
| Action Controller | HTTP handling |
| Action View | Templates |
| Action Cable | WebSockets |
| Active Job | Background jobs |

### Rails Way

```ruby
# Model
class User < ApplicationRecord
  has_many :posts
  validates :email, presence: true
end

# Controller
class UsersController < ApplicationController
  def index
    @users = User.all
  end
end
```

---

## Ecosystem

### Gems

**RubyGems** + **Bundler** — Package management.

| Gem | Purpose |
|-----|---------|
| rails | Web framework |
| rspec | Testing |
| sidekiq | Background jobs |
| devise | Authentication |
| pundit | Authorization |

### Testing

| Tool | Style |
|------|-------|
| RSpec | BDD, popular |
| Minitest | Simple, stdlib |
| Capybara | Browser testing |
| FactoryBot | Test data |

### Web Servers

| Server | Notes |
|--------|-------|
| Puma | Default, threaded |
| Unicorn | Multi-process |
| Passenger | Enterprise |

---

## Beyond Rails

| Framework | Use Case |
|-----------|----------|
| Sinatra | Micro-framework |
| Hanami | Alternative to Rails |
| Roda | Routing-focused |

| Tool | Purpose |
|------|---------|
| Jekyll | Static sites |
| Homebrew | Package manager (macOS) |
| Vagrant | VMs |

---

## Performance

### Historical

Ruby was slow. Valid criticism through 2.x.

### Modern (3.x)

- YJIT (3.1+) — 15-20% faster
- Ractors — True parallelism
- Better GC

Still not Go/Rust fast, but fast enough.

---

## When to Use Ruby

**Strengths:**
- Developer productivity
- Rails ecosystem
- Rapid prototyping
- Startups
- Happiness

**Considerations:**
- Performance (better, not best)
- Smaller talent pool than Python/JS
- Startup scene shrinking

**Best for:**
- Web applications (Rails)
- MVPs and startups
- DevOps tooling
- Scripting

---

## Ruby vs Python

| Aspect | Ruby | Python |
|--------|------|--------|
| Philosophy | Developer happiness | One obvious way |
| Syntax | More magic, flexible | Explicit, readable |
| Web | Rails dominates | Django, FastAPI |
| Data science | Minimal | Dominant |
| Ecosystem | Web-focused | Broad |
| Community | Smaller, passionate | Massive |

---

## Related

- [[Web Development]]
- [[Web Frameworks]]
- [[Rails]]
- [[Python]]
