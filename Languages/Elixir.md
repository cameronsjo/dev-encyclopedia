---
title: Elixir
aliases:
  - Elixir Language
  - Elixir Programming
tags:
  - language
  - functional
  - concurrent
  - erlang
  - beam
type: reference
status: complete
created: "2025-11-30"
---

# Elixir

A functional, concurrent programming language built on the Erlang VM (BEAM), designed for building scalable and maintainable applications with excellent fault tolerance.

## Overview

| Aspect | Details |
|--------|---------|
| **Paradigm** | Functional, concurrent, distributed |
| **First Released** | 2012 (José Valim) |
| **Runtime** | BEAM (Erlang Virtual Machine) |
| **Typing** | Dynamic, strong |
| **Compilation** | Compiled to BEAM bytecode |
| **Concurrency** | Actor model with lightweight processes |
| **Primary Use Cases** | Web services, real-time systems, distributed applications, IoT |
| **Package Manager** | Hex (hex.pm) |
| **Build Tool** | Mix |
| **Notable Users** | Discord, Pinterest, PepsiCo, Moz, Bleacher Report |

## Core Concepts

### BEAM Virtual Machine

Runs on the Erlang VM, inheriting decades of battle-tested reliability and concurrency features:

- **Preemptive Scheduling**: Ensures fair process execution
- **Hot Code Swapping**: Update code without stopping the system
- **Distributed by Default**: Built-in support for clustering
- **Garbage Collection**: Per-process GC prevents stop-the-world pauses
- **Fault Isolation**: Process failures don't crash the entire system

### Processes & Concurrency

Lightweight processes (not OS threads) as the fundamental unit of concurrency:

```elixir
# Spawn a process
pid = spawn(fn -> IO.puts("Hello from process") end)

# Send messages
send(pid, {:message, "data"})

# Receive messages
receive do
  {:message, data} -> IO.puts(data)
end
```

**Key Characteristics:**

- Millions of processes on a single machine
- ~2KB memory per process
- Message passing via immutable data
- Share-nothing architecture

### OTP (Open Telecom Platform)

Framework for building concurrent, fault-tolerant applications:

**GenServer** - Generic server behavior for managing state:

```elixir
defmodule Counter do
  use GenServer

  def start_link(initial) do
    GenServer.start_link(__MODULE__, initial, name: __MODULE__)
  end

  def increment do
    GenServer.cast(__MODULE__, :increment)
  end

  def get do
    GenServer.call(__MODULE__, :get)
  end

  # Callbacks
  def init(initial), do: {:ok, initial}
  def handle_cast(:increment, state), do: {:noreply, state + 1}
  def handle_call(:get, _from, state), do: {:reply, state, state}
end
```

**Supervisor** - Fault tolerance through process supervision:

```elixir
defmodule MyApp.Supervisor do
  use Supervisor

  def start_link(opts) do
    Supervisor.start_link(__MODULE__, :ok, opts)
  end

  def init(:ok) do
    children = [
      {Counter, 0},
      {MyWorker, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end
```

**Supervision Strategies:**

- `:one_for_one` - Restart only failed process
- `:one_for_all` - Restart all children if one fails
- `:rest_for_one` - Restart failed process and those started after it

### Pattern Matching

Core feature used throughout the language:

```elixir
# Variable binding
{:ok, result} = {:ok, 42}

# Function clauses
def factorial(0), do: 1
def factorial(n), do: n * factorial(n - 1)

# Case expressions
case result do
  {:ok, value} -> value
  {:error, _} -> nil
end

# With expressions for pipeline error handling
with {:ok, user} <- fetch_user(id),
     {:ok, posts} <- fetch_posts(user) do
  {:ok, {user, posts}}
end
```

### Pipe Operator

Chains function calls for readable data transformations:

```elixir
# Without pipes
String.upcase(String.trim("  hello  "))

# With pipes
"  hello  "
|> String.trim()
|> String.upcase()

# Complex pipeline
users
|> Enum.filter(&(&1.active))
|> Enum.map(&(&1.email))
|> Enum.sort()
|> Enum.take(10)
```

### Immutability

All data structures are immutable:

```elixir
list = [1, 2, 3]
new_list = [0 | list]  # Prepend (efficient)
# list is still [1, 2, 3]

map = %{name: "Alice", age: 30}
updated = %{map | age: 31}  # Update syntax
# map is unchanged
```

## Key Features

### Phoenix Framework

Modern web framework with exceptional performance:

```elixir
defmodule MyAppWeb.Router do
  use Phoenix.Router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", MyAppWeb do
    pipe_through :api

    resources "/users", UserController
    get "/health", HealthController, :index
  end
end
```

**LiveView** - Real-time server-rendered UI without JavaScript:

```elixir
defmodule MyAppWeb.CounterLive do
  use Phoenix.LiveView

  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  def handle_event("increment", _params, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  def render(assigns) do
    ~H"""
    <div>
      <p>Count: <%= @count %></p>
      <button phx-click="increment">+</button>
    </div>
    """
  end
end
```

### Ecto

Database wrapper and query generator:

```elixir
# Schema definition
defmodule User do
  use Ecto.Schema

  schema "users" do
    field :name, :string
    field :email, :string
    field :age, :integer
    has_many :posts, Post
    timestamps()
  end
end

# Queries
import Ecto.Query

users = from u in User,
        where: u.age > 18,
        select: u.email

# Changesets for data validation
def changeset(user, params) do
  user
  |> cast(params, [:name, :email, :age])
  |> validate_required([:name, :email])
  |> validate_format(:email, ~r/@/)
  |> unique_constraint(:email)
end
```

### Mix

Build tool and task runner:

```bash
# Create new project
mix new my_app
mix phx.new my_web_app

# Dependencies (mix.exs)
defp deps do
  [
    {:phoenix, "~> 1.7"},
    {:ecto_sql, "~> 3.10"},
    {:jason, "~> 1.4"}
  ]
end

# Common commands
mix deps.get          # Fetch dependencies
mix compile           # Compile project
mix test              # Run tests
mix format            # Format code
mix ecto.migrate      # Run database migrations
```

### Macros & Metaprogramming

Compile-time code generation using abstract syntax trees:

```elixir
defmacro unless(condition, do: block) do
  quote do
    if !unquote(condition), do: unquote(block)
  end
end

# Use macros for DSLs
use Phoenix.Router  # Injects router functions
use GenServer       # Adds behavior callbacks
```

### Protocols

Polymorphism through protocol dispatch:

```elixir
defprotocol Serializable do
  def serialize(data)
end

defimpl Serializable, for: Map do
  def serialize(map), do: Jason.encode!(map)
end

defimpl Serializable, for: List do
  def serialize(list), do: Enum.join(list, ",")
end
```

## Concurrency Model

### Actor Model

Each process is an independent actor:

- **Isolated State**: No shared memory between processes
- **Message Passing**: Asynchronous communication via mailboxes
- **Location Transparency**: Send messages to local or remote processes identically

### Fault Tolerance

"Let It Crash" philosophy:

```elixir
# Supervisor restarts failed processes
defmodule MyApp.Application do
  use Application

  def start(_type, _args) do
    children = [
      {MyApp.Repo, []},
      {Phoenix.PubSub, name: MyApp.PubSub},
      MyAppWeb.Endpoint,
      {Task.Supervisor, name: MyApp.TaskSupervisor}
    ]

    opts = [strategy: :one_for_one, name: MyApp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

### Distribution

Built-in clustering support:

```elixir
# Start named node
iex --name node1@127.0.0.1 --cookie secret

# Connect nodes
Node.connect(:"node2@127.0.0.1")

# Send to remote process
send({:process_name, :node2@127.0.0.1}, :message)

# Distributed tasks
Task.Supervisor.async({MyApp.TaskSupervisor, :node2@127.0.0.1}, fn ->
  # Work happens on remote node
end)
```

## Comparison with Alternatives

| Feature | Elixir | Erlang | Go | Rust |
|---------|--------|--------|----|----- |
| **VM/Runtime** | BEAM | BEAM | Native/GC | Native/No GC |
| **Syntax** | Ruby-inspired, modern | Prolog-like, terse | C-like, simple | C-like, complex |
| **Concurrency** | Lightweight processes | Lightweight processes | Goroutines | OS threads + async |
| **Fault Tolerance** | ✅ OTP supervision trees | ✅ OTP supervision trees | Manual error handling | Manual error handling |
| **Hot Code Reload** | ✅ Built-in | ✅ Built-in | ❌ Requires restart | ❌ Requires restart |
| **Performance** | High throughput | High throughput | Very high throughput | Highest throughput |
| **Memory Usage** | Moderate | Moderate | Low | Very low |
| **Tooling** | Excellent (Mix, Hex, LSP) | Good (rebar3, OTP tools) | Excellent (go tools) | Excellent (cargo) |
| **Learning Curve** | Moderate | Steep | Gentle | Steep |
| **Ecosystem** | Growing (web-focused) | Mature (telecom-focused) | Very large | Large |
| **Type System** | Dynamic | Dynamic | Static | Static + advanced |
| **Pattern Matching** | ✅ Extensive | ✅ Extensive | ❌ Limited | ✅ Powerful |
| **Metaprogramming** | ✅ Macros | ✅ Parse transforms | ❌ Code generation | ✅ Macros |
| **Distribution** | ✅ First-class | ✅ First-class | Manual | Manual |
| **Best For** | Web apps, real-time, chat | Telecom, embedded, routing | Microservices, CLI tools | Systems, performance-critical |

### Elixir vs Erlang

**Elixir Advantages:**

- Modern, readable syntax
- Better tooling (Mix, ExUnit, formatter)
- Richer standard library
- Active web ecosystem (Phoenix)
- Easier to learn

**Erlang Advantages:**

- More mature libraries for telecom
- Slightly better raw performance
- Larger pool of battle-tested code
- More systems-level libraries

**Interoperability:** Elixir can call Erlang modules directly and vice versa.

### Elixir vs Go

**Elixir Advantages:**

- Superior fault tolerance (supervision trees)
- Hot code reloading
- Built-in distribution
- Functional programming benefits
- Better for long-lived connections (WebSockets)

**Go Advantages:**

- Faster raw performance
- Lower memory footprint
- Simpler deployment (single binary)
- Larger ecosystem
- Better for CPU-intensive tasks

### Elixir vs Rust

**Elixir Advantages:**

- Easier concurrency (actor model)
- Fault tolerance built-in
- Faster development cycles
- Better for I/O-bound systems
- Hot code reloading

**Rust Advantages:**

- 10-100x faster for CPU-bound tasks
- Compile-time safety guarantees
- No garbage collection
- Lower memory usage
- Better for systems programming

## When to Use

### Strengths

| Use Case | Why Elixir Excels |
|----------|-------------------|
| **Real-time Web** | WebSockets, LiveView, Phoenix Channels for chat, collaborative editing, live dashboards |
| **High Concurrency** | Millions of concurrent connections with low overhead |
| **Fault Tolerance** | Self-healing systems via supervision trees |
| **Distributed Systems** | Built-in clustering, node communication, distributed task processing |
| **Data Pipelines** | GenStage, Flow for stream processing and ETL |
| **IoT Platforms** | Nerves framework for embedded systems on BEAM |
| **Microservices** | Independent services with excellent uptime requirements |
| **Event-Driven** | EventStore, Commanded for event sourcing and CQRS |

### Considerations

| Factor | Consideration |
|--------|---------------|
| **CPU-Bound Tasks** | Not ideal for heavy computation (use NIFs or Rust/C ports) |
| **Memory Usage** | Higher baseline than Go/Rust for simple tasks |
| **Ecosystem** | Smaller than Node.js, Python, Java |
| **Team Expertise** | Functional programming paradigm may require learning curve |
| **Type Safety** | Dynamic typing means runtime errors possible |
| **Deployment** | Requires BEAM VM (larger than single binary) |
| **Cold Start** | Slower than Go for serverless environments |

### Best For

- **Web Applications**: Phoenix provides Rails-like productivity with better performance
- **Real-time Features**: Chat, notifications, live updates, multiplayer games
- **Highly Available Systems**: 99.9%+ uptime requirements
- **Concurrent Request Handling**: API servers with many simultaneous connections
- **Streaming/Pipeline Processing**: Data transformation and aggregation
- **Soft Real-time**: Systems needing predictable latency (not hard real-time)

### Not Ideal For

- **CPU-Intensive Computing**: Scientific computing, video encoding, ML training
- **Mobile Apps**: Elixir is backend-focused
- **Desktop Applications**: Not designed for GUI applications
- **Hard Real-time Systems**: BEAM GC makes guarantees difficult
- **Serverless Functions**: Cold start time and memory usage are higher than Go/Rust

## Tooling & Ecosystem

### Development Tools

| Tool | Purpose |
|------|---------|
| **Mix** | Build tool, dependency manager, task runner |
| **IEx** | Interactive shell with debugging and introspection |
| **ExUnit** | Built-in testing framework with excellent assertion messages |
| **Dialyzer** | Static analysis for type checking |
| **Credo** | Static code analysis for code quality |
| **ExDoc** | Documentation generator |
| **Language Server** | ElixirLS for IDE integration (VS Code, Vim, Emacs) |

### Key Libraries

| Library | Purpose |
|---------|---------|
| **Phoenix** | Web framework (controllers, views, LiveView) |
| **Ecto** | Database wrapper, query builder, migrations |
| **Absinthe** | GraphQL implementation |
| **Broadway** | Concurrent data processing pipelines |
| **Oban** | Background job processing |
| **Tesla** | HTTP client with middleware support |
| **Jason** | JSON parser/encoder |
| **Finch** | HTTP client built on Mint |
| **Quantum** | Cron-like job scheduler |
| **Commanded** | Event sourcing and CQRS |

### Deployment

| Approach | Description |
|----------|-------------|
| **Releases** | Mix releases create self-contained packages |
| **Docker** | Official Elixir Docker images available |
| **Fly.io** | Elixir-friendly hosting with clustering support |
| **Gigalixir** | Heroku-like PaaS specialized for Elixir |
| **Kubernetes** | Common for larger deployments with libcluster |
| **systemd** | Traditional Linux service management |

## Related

- [[Go]] - Alternative for high-concurrency services
- [[Rust]] - Systems programming with similar pattern matching
- [[Web Frameworks]] - Phoenix comparison with other frameworks
- [[Erlang]] - The underlying VM and runtime
- [[Functional Programming]] - Paradigm concepts
- [[Concurrency MOC]] - Concurrency models across languages
