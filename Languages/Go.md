---
title: Go
aliases:
  - Golang
tags:
  - language
  - backend
  - systems
  - go
type: reference
status: complete
created: '2025-12-03'
---

# Go

Simple, efficient, built for concurrency. The language of cloud infrastructure.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Procedural, concurrent |
| Typing | Static, strong, inferred |
| Compiled | Yes (fast compilation) |
| Memory | Garbage collected |
| First appeared | 2009 (Google) |
| Current | 1.22+ |

---

## Language Basics

### Variables & Types

```go
// Type inference
name := "Alice"
age := 30

// Explicit typing
var count int = 0
var active bool

// Constants
const MaxSize = 100

// Multiple assignment
x, y := 10, 20
```

### Slices & Maps

```go
// Slices (dynamic arrays)
nums := []int{1, 2, 3}
nums = append(nums, 4)

// Maps
users := map[string]int{
    "alice": 30,
    "bob":   25,
}
users["charlie"] = 35

// Check existence
age, exists := users["alice"]
```

### Structs

```go
type User struct {
    Name  string
    Email string
    Age   int
}

// Methods
func (u User) IsAdult() bool {
    return u.Age >= 18
}

// Pointer receiver (can modify)
func (u *User) SetAge(age int) {
    u.Age = age
}
```

### Interfaces

Implicit implementation — no `implements` keyword.

```go
type Writer interface {
    Write([]byte) (int, error)
}

// Any type with Write method implements Writer
type FileWriter struct { ... }

func (f *FileWriter) Write(data []byte) (int, error) {
    // implementation
}

// Usage
func Save(w Writer, data []byte) error {
    _, err := w.Write(data)
    return err
}
```

---

## Error Handling

No exceptions. Errors are values.

```go
func ReadFile(path string) ([]byte, error) {
    data, err := os.ReadFile(path)
    if err != nil {
        return nil, fmt.Errorf("reading %s: %w", path, err)
    }
    return data, nil
}

// Caller handles error
data, err := ReadFile("config.json")
if err != nil {
    log.Fatal(err)
}
```

### Error Wrapping (1.13+)

```go
// Wrap errors with context
if err != nil {
    return fmt.Errorf("failed to connect: %w", err)
}

// Unwrap to check specific errors
if errors.Is(err, sql.ErrNoRows) {
    // handle not found
}

// Type assertion on errors
var pathErr *os.PathError
if errors.As(err, &pathErr) {
    fmt.Println("Path:", pathErr.Path)
}
```

---

## Concurrency

Go's killer feature. Lightweight goroutines + channels.

### Goroutines

```go
func main() {
    go processData()  // Starts goroutine
    go func() {
        fmt.Println("Anonymous goroutine")
    }()

    time.Sleep(time.Second)  // Wait (not production code)
}
```

### Channels

```go
// Unbuffered channel
ch := make(chan int)

// Buffered channel
ch := make(chan int, 100)

// Send and receive
go func() {
    ch <- 42  // Send
}()
value := <-ch  // Receive

// Close channel
close(ch)
```

### Select

Multiplex channel operations.

```go
select {
case msg := <-msgCh:
    fmt.Println("Message:", msg)
case err := <-errCh:
    log.Fatal(err)
case <-time.After(5 * time.Second):
    fmt.Println("Timeout")
case <-ctx.Done():
    return
}
```

### Patterns

```go
// Worker pool
func worker(jobs <-chan Job, results chan<- Result) {
    for job := range jobs {
        results <- process(job)
    }
}

// Fan-out
for i := 0; i < numWorkers; i++ {
    go worker(jobs, results)
}

// WaitGroup
var wg sync.WaitGroup
for _, item := range items {
    wg.Add(1)
    go func(item Item) {
        defer wg.Done()
        process(item)
    }(item)
}
wg.Wait()
```

### Context

For cancellation and deadlines.

```go
ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
defer cancel()

select {
case result := <-doWork(ctx):
    return result
case <-ctx.Done():
    return ctx.Err()
}
```

---

## Generics (1.18+)

```go
func Map[T, U any](slice []T, f func(T) U) []U {
    result := make([]U, len(slice))
    for i, v := range slice {
        result[i] = f(v)
    }
    return result
}

// Constraints
type Number interface {
    int | int64 | float64
}

func Sum[T Number](nums []T) T {
    var sum T
    for _, n := range nums {
        sum += n
    }
    return sum
}
```

---

## Project Structure

```
myproject/
├── go.mod              # Module definition
├── go.sum              # Dependency checksums
├── main.go             # Entry point
├── internal/           # Private packages
│   └── db/
│       └── db.go
├── pkg/                # Public packages
│   └── api/
│       └── api.go
└── cmd/                # Multiple commands
    └── server/
        └── main.go
```

### go.mod

```go
module github.com/user/myproject

go 1.22

require (
    github.com/gin-gonic/gin v1.9.1
    github.com/lib/pq v1.10.9
)
```

---

## Ecosystem

### Web Frameworks

| Framework | Style |
|-----------|-------|
| net/http | Standard library |
| Gin | Fast, middleware |
| Echo | Minimalist |
| Fiber | Express-like |
| Chi | Lightweight router |

### Standard Library Highlights

| Package | Purpose |
|---------|---------|
| `net/http` | HTTP server/client |
| `encoding/json` | JSON encoding |
| `database/sql` | Database interface |
| `context` | Cancellation/deadlines |
| `sync` | Concurrency primitives |
| `testing` | Test framework |

### Tooling

```bash
go build           # Build
go run main.go     # Run
go test ./...      # Test all
go mod tidy        # Clean dependencies
go fmt ./...       # Format
go vet ./...       # Static analysis
golangci-lint run  # Linter
```

---

## When to Use Go

**Strengths:**

- Simple, readable
- Fast compilation
- Excellent concurrency
- Single binary deployment
- Strong standard library
- Great for DevOps tooling

**Considerations:**

- Verbose error handling
- No exceptions
- Limited expressiveness
- Garbage collection pauses

**Best for:**

- Microservices
- CLI tools
- Cloud infrastructure
- Network services
- DevOps tooling
- APIs

---

## Related

- [[API Design Patterns]]
- [[Deployment]]
- [[Build Systems]]
- [[Testing Frameworks]]
