---
title: Rust
aliases:
  - rs
tags:
  - language
  - systems
  - embedded
  - rust
type: reference
status: complete
created: '2025-12-03'
---

# Rust

Memory-safe systems language. No GC, no runtime. C++ performance with safety guarantees.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Multi-paradigm (functional, imperative) |
| Typing | Static, strong, inferred |
| Memory | Ownership system (no GC) |
| Compiles to | Native code (LLVM) |
| First appeared | 2010 (1.0 in 2015) |
| Current | 1.75+ |

---

## Ownership System

Rust's core innovation. Memory safety without garbage collection.

### Rules

1. Each value has exactly one owner
2. When owner goes out of scope, value is dropped
3. Values can be borrowed (referenced)

```rust
fn main() {
    let s1 = String::from("hello");
    let s2 = s1;  // s1 is MOVED to s2, s1 no longer valid

    let s3 = s2.clone();  // Explicit copy

    // s2 and s3 are both valid here
}
```

### Borrowing

```rust
fn calculate_length(s: &String) -> usize {  // Borrow, don't take ownership
    s.len()
}

fn main() {
    let s = String::from("hello");
    let len = calculate_length(&s);
    // s is still valid here
}
```

### Mutable Borrowing

```rust
fn main() {
    let mut s = String::from("hello");

    change(&mut s);  // Mutable borrow

    // Rule: Only ONE mutable reference at a time
    // OR any number of immutable references
}

fn change(s: &mut String) {
    s.push_str(", world");
}
```

### Lifetimes

Compiler tracks how long references are valid.

```rust
// Explicit lifetime annotation
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}
```

---

## Error Handling

No exceptions. Explicit error handling via `Result` and `Option`.

### Result

```rust
use std::fs::File;
use std::io::Read;

fn read_file(path: &str) -> Result<String, std::io::Error> {
    let mut file = File::open(path)?;  // ? propagates error
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

// Pattern matching
match read_file("data.txt") {
    Ok(contents) => println!("{}", contents),
    Err(e) => eprintln!("Error: {}", e),
}
```

### Option

```rust
fn find_user(id: u32) -> Option<User> {
    // Returns Some(user) or None
}

// Handling
if let Some(user) = find_user(42) {
    println!("Found: {}", user.name);
}

// Or with combinators
let name = find_user(42)
    .map(|u| u.name)
    .unwrap_or_default();
```

---

## Structs & Enums

### Structs

```rust
struct User {
    name: String,
    email: String,
    active: bool,
}

impl User {
    // Associated function (constructor)
    fn new(name: String, email: String) -> Self {
        Self { name, email, active: true }
    }

    // Method
    fn deactivate(&mut self) {
        self.active = false;
    }
}
```

### Enums

Rust enums are algebraic data types (sum types).

```rust
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    Color(u8, u8, u8),
}

fn process(msg: Message) {
    match msg {
        Message::Quit => println!("Quit"),
        Message::Move { x, y } => println!("Move to {}, {}", x, y),
        Message::Write(text) => println!("Write: {}", text),
        Message::Color(r, g, b) => println!("RGB: {}, {}, {}", r, g, b),
    }
}
```

---

## Traits

Like interfaces, but more powerful.

```rust
trait Summary {
    fn summarize(&self) -> String;

    // Default implementation
    fn preview(&self) -> String {
        format!("Read more: {}", self.summarize())
    }
}

impl Summary for Article {
    fn summarize(&self) -> String {
        format!("{} by {}", self.title, self.author)
    }
}

// Trait bounds
fn notify<T: Summary>(item: &T) {
    println!("Breaking: {}", item.summarize());
}

// Multiple bounds
fn complex<T: Summary + Display>(item: &T) { ... }
```

---

## Concurrency

"Fearless concurrency" — compiler prevents data races.

### Threads

```rust
use std::thread;

let handle = thread::spawn(|| {
    println!("Hello from thread!");
});

handle.join().unwrap();
```

### Channels

```rust
use std::sync::mpsc;

let (tx, rx) = mpsc::channel();

thread::spawn(move || {
    tx.send("Hello").unwrap();
});

let received = rx.recv().unwrap();
```

### Shared State

```rust
use std::sync::{Arc, Mutex};

let counter = Arc::new(Mutex::new(0));

let handles: Vec<_> = (0..10).map(|_| {
    let counter = Arc::clone(&counter);
    thread::spawn(move || {
        let mut num = counter.lock().unwrap();
        *num += 1;
    })
}).collect();

for handle in handles {
    handle.join().unwrap();
}
```

### Async/Await

```rust
async fn fetch_data(url: &str) -> Result<String, reqwest::Error> {
    let response = reqwest::get(url).await?;
    response.text().await
}

#[tokio::main]
async fn main() {
    let data = fetch_data("https://example.com").await.unwrap();
}
```

---

## Ecosystem

### Cargo

Package manager and build tool.

```bash
cargo new myproject        # Create project
cargo build               # Build
cargo run                 # Build and run
cargo test                # Run tests
cargo doc --open          # Generate docs
cargo clippy              # Linter
cargo fmt                 # Formatter
```

### Cargo.toml

```toml
[package]
name = "myproject"
version = "0.1.0"
edition = "2021"

[dependencies]
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1", features = ["full"] }

[dev-dependencies]
criterion = "0.5"
```

### Key Crates

| Crate | Purpose |
|-------|---------|
| serde | Serialization |
| tokio | Async runtime |
| reqwest | HTTP client |
| clap | CLI parsing |
| anyhow | Error handling |
| thiserror | Custom errors |
| tracing | Logging/tracing |
| sqlx | Database (async) |

---

## Use Cases

| Domain | Examples |
|--------|----------|
| CLI tools | ripgrep, bat, fd, exa |
| Web | Actix, Axum, Rocket |
| Embedded | esp32, ARM Cortex |
| WASM | Frontend, serverless |
| Systems | Linux kernel modules |
| Games | Bevy engine |

---

## When to Use Rust

**Strengths:**

- C/C++ performance
- Memory safety guaranteed
- No data races
- Excellent tooling
- Growing ecosystem

**Considerations:**

- Steep learning curve
- Slower compilation
- Smaller talent pool
- Fighting the borrow checker

**Best for:**

- Performance-critical code
- Systems programming
- CLI tools
- WebAssembly
- When safety matters

---

## Related

- [[C++]]
- [[Bevy]]
- [[Tauri]]
- [[Build Systems]]
- [[domains/Embedded & Systems|Embedded & Systems]]
