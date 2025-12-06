---
title: Ruby on Rails
aliases:
  - Rails
  - RoR
tags:
  - framework
  - backend
  - web
  - ruby
  - full-stack
type: reference
status: complete
created: '2025-11-28'
---

# Ruby on Rails

Full-stack web framework emphasizing convention over configuration.

## Overview

| Aspect | Details |
|--------|---------|
| Language | Ruby |
| Type | Full-stack framework |
| Architecture | MVC, convention over configuration |
| First release | 2004 |
| Creator | David Heinemeier Hansson (DHH) |
| Backing | Rails Core Team |

---

## Philosophy

### Convention Over Configuration

Rails makes assumptions about what you need. Follow conventions, write less code.

| Convention | Meaning |
|------------|---------|
| Model `User` | Table `users` |
| Controller `UsersController` | Route `/users` |
| View in `app/views/users/` | Renders for UsersController |

### The Rails Doctrine

1. Optimize for programmer happiness
2. Convention over configuration
3. The menu is omakase (opinionated)
4. No paradigm zealots
5. Progress over stability

---

## Core Concepts

### MVC Architecture

```
┌─────────────────────────────────────────┐
│                 Browser                  │
└────────────────┬────────────────────────┘
                 │ Request
                 ▼
┌─────────────────────────────────────────┐
│              Controller                  │
│        (Business logic glue)            │
└───────┬───────────────────┬─────────────┘
        ▼                   ▼
┌──────────────┐    ┌──────────────┐
│    Model     │    │     View     │
│  (Database)  │    │   (HTML)     │
└──────────────┘    └──────────────┘
```

### Active Record

ORM pattern for database access.

```ruby
class User < ApplicationRecord
  has_many :posts
  validates :email, presence: true, uniqueness: true
end

# Usage
user = User.create(name: "John", email: "john@example.com")
user.posts.create(title: "First Post")
User.where(active: true).order(:created_at)
```

### Action Controller

Handle HTTP requests.

```ruby
class UsersController < ApplicationController
  def index
    @users = User.all
  end

  def show
    @user = User.find(params[:id])
  end

  def create
    @user = User.new(user_params)
    if @user.save
      redirect_to @user
    else
      render :new, status: :unprocessable_entity
    end
  end

  private

  def user_params
    params.require(:user).permit(:name, :email)
  end
end
```

### Action View

Templates and helpers.

```erb
<!-- app/views/users/show.html.erb -->
<h1><%= @user.name %></h1>
<p><%= @user.email %></p>

<%= link_to "Edit", edit_user_path(@user) %>
<%= button_to "Delete", @user, method: :delete %>
```

---

## Routing

```ruby
# config/routes.rb
Rails.application.routes.draw do
  root "home#index"

  resources :users do
    resources :posts, only: [:index, :create]
  end

  namespace :api do
    namespace :v1 do
      resources :articles
    end
  end
end
```

### RESTful Resources

`resources :users` generates:

| HTTP | Path | Controller#Action |
|------|------|-------------------|
| GET | /users | users#index |
| GET | /users/:id | users#show |
| GET | /users/new | users#new |
| POST | /users | users#create |
| GET | /users/:id/edit | users#edit |
| PATCH | /users/:id | users#update |
| DELETE | /users/:id | users#destroy |

---

## Database & Migrations

### Migrations

Version control for database schema.

```ruby
class CreateUsers < ActiveRecord::Migration[7.1]
  def change
    create_table :users do |t|
      t.string :name, null: false
      t.string :email, null: false, index: { unique: true }
      t.timestamps
    end
  end
end
```

```bash
rails db:migrate
rails db:rollback
rails db:seed
```

### Associations

| Association | Example |
|-------------|---------|
| has_many | User has_many :posts |
| belongs_to | Post belongs_to :user |
| has_one | User has_one :profile |
| has_many :through | User has_many :comments, through: :posts |
| has_and_belongs_to_many | Posts and Tags |

---

## Modern Rails

### Hotwire

Modern frontend without heavy JavaScript.

| Component | Purpose |
|-----------|---------|
| Turbo Drive | SPA-like navigation |
| Turbo Frames | Partial page updates |
| Turbo Streams | Real-time updates |
| Stimulus | Minimal JavaScript |

```erb
<%= turbo_frame_tag "user_#{@user.id}" do %>
  <div class="user-card">
    <%= @user.name %>
    <%= link_to "Edit", edit_user_path(@user) %>
  </div>
<% end %>
```

### Stimulus

```javascript
// app/javascript/controllers/hello_controller.js
import { Controller } from "@hotwired/stimulus"

export default class extends Controller {
  static targets = ["name"]

  greet() {
    alert(`Hello, ${this.nameTarget.value}!`)
  }
}
```

```erb
<div data-controller="hello">
  <input data-hello-target="name" type="text">
  <button data-action="click->hello#greet">Greet</button>
</div>
```

---

## Rails 7+ Features

| Feature | Description |
|---------|-------------|
| Hotwire (default) | Turbo + Stimulus |
| Import maps | No bundler needed |
| Encryption | Built-in attribute encryption |
| Active Storage | File uploads to cloud |
| Action Mailbox | Inbound email processing |
| Action Text | Rich text editing |

---

## Testing

### Built-in Support

```ruby
# test/models/user_test.rb
class UserTest < ActiveSupport::TestCase
  test "requires email" do
    user = User.new(name: "John")
    assert_not user.valid?
    assert_includes user.errors[:email], "can't be blank"
  end
end
```

### Popular Alternatives

| Library | Purpose |
|---------|---------|
| RSpec | BDD testing |
| Capybara | Integration testing |
| FactoryBot | Test fixtures |
| VCR | HTTP mocking |

---

## Authentication

### Devise

Most popular authentication solution.

```ruby
class User < ApplicationRecord
  devise :database_authenticatable, :registerable,
         :recoverable, :rememberable, :validatable
end
```

### Alternatives

| Library | Philosophy |
|---------|------------|
| Devise | Full-featured, batteries included |
| Rodauth | More secure, more control |
| Authlogic | Minimal, flexible |
| has_secure_password | Built-in, DIY |

---

## Background Jobs

### Active Job

Framework-agnostic job interface.

```ruby
class UserMailerJob < ApplicationJob
  queue_as :default

  def perform(user)
    UserMailer.welcome(user).deliver_now
  end
end

# Enqueue
UserMailerJob.perform_later(user)
```

### Backends

| Backend | Notes |
|---------|-------|
| Sidekiq | Redis-backed, popular |
| GoodJob | PostgreSQL-backed |
| Solid Queue | Database-backed (Rails 8) |
| Delayed Job | ActiveRecord-backed |

---

## API Mode

```bash
rails new api_app --api
```

Slimmer stack for JSON APIs.

```ruby
class Api::V1::UsersController < ApplicationController
  def index
    render json: User.all
  end

  def show
    render json: User.find(params[:id])
  end
end
```

---

## Performance

### Caching

```ruby
# Fragment caching
<% cache @user do %>
  <div class="user-card">
    <%= render @user.posts %>
  </div>
<% end %>

# Low-level caching
Rails.cache.fetch("user_#{id}", expires_in: 1.hour) do
  User.find(id)
end
```

### Database

| Technique | Purpose |
|-----------|---------|
| Eager loading | Avoid N+1 queries |
| Indexes | Query performance |
| Counter cache | Avoid COUNT queries |
| Connection pooling | Concurrent requests |

```ruby
# N+1 problem
User.all.each { |u| u.posts.count }  # Bad

# Eager loading
User.includes(:posts).each { |u| u.posts.count }  # Good
```

---

## Deployment

### Platforms

| Platform | Notes |
|----------|-------|
| Heroku | Classic Rails host |
| Render | Modern alternative |
| Fly.io | Edge deployment |
| Railway | Simple PaaS |
| AWS/GCP | Full control |

### Production Servers

| Server | Type |
|--------|------|
| Puma | Application server (default) |
| Unicorn | Fork-based |
| Falcon | Async |

---

## Rails vs Alternatives

| Aspect | Rails | Django | Laravel |
|--------|-------|--------|---------|
| Language | Ruby | Python | PHP |
| Philosophy | Convention | Explicit | Elegant |
| ORM | ActiveRecord | Django ORM | Eloquent |
| Admin | ActiveAdmin | Built-in | Nova (paid) |
| Community | Mature | Large | Very large |
| Performance | Good | Good | Good |

---

## When to Use Rails

**Strengths:**

- Rapid development
- Convention over configuration
- Mature ecosystem (gems)
- Full-stack with Hotwire
- Excellent documentation
- Strong community

**Considerations:**

- Ruby talent pool (smaller than Python/JS)
- Performance (good, not best)
- Learning curve (conventions)
- Monolithic (microservices harder)

**Best for:**

- Startups (speed to market)
- Content-heavy applications
- MVPs and prototypes
- CRUD-heavy applications
- Small to medium teams

---

## Notable Apps Built with Rails

- GitHub (was Rails, now hybrid)
- Shopify
- Basecamp
- Airbnb
- Twitch
- Zendesk

---

## Related

- [[Ruby]]
- [[Django]]
- [[Laravel]]
- [[Spring Boot]]
- [[Domains/Web Development|Web Development]]
