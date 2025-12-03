---
title: Python
aliases:
  - py
  - Python3
tags:
  - language
  - scripting
  - ml
  - web
  - python
type: reference
status: complete
created: '2025-12-03'
---

# Python

Readable, versatile language. Dominant in ML/AI, data science, and scripting.

## Overview

| Aspect | Details |
|--------|---------|
| Paradigm | Multi-paradigm (OOP, functional, procedural) |
| Typing | Dynamic, strong, optional hints |
| Interpreted | Yes (CPython, PyPy) |
| Memory | Garbage collected (reference counting + GC) |
| First appeared | 1991 |
| Current | 3.12+ |

---

## Modern Python (3.10+)

### Type Hints

Optional static typing for tooling.

```python
def greet(name: str) -> str:
    return f"Hello, {name}"

# Generic types
from typing import Optional, List, Dict

def find_user(id: int) -> Optional[User]:
    ...

# Python 3.10+ - Union with |
def process(value: str | int) -> None:
    ...
```

### Pattern Matching (3.10+)

```python
match command:
    case ["quit"]:
        exit()
    case ["load", filename]:
        load_file(filename)
    case ["save", filename, *options]:
        save_file(filename, options)
    case _:
        print("Unknown command")

# Structural matching
match point:
    case Point(x=0, y=0):
        print("Origin")
    case Point(x=0, y=y):
        print(f"On y-axis at {y}")
```

### Data Classes (3.7+)

```python
from dataclasses import dataclass, field

@dataclass
class User:
    name: str
    email: str
    age: int = 0
    tags: list[str] = field(default_factory=list)

# Immutable
@dataclass(frozen=True)
class Point:
    x: float
    y: float
```

### Async/Await

```python
import asyncio

async def fetch_data(url: str) -> dict:
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            return await response.json()

async def main():
    # Concurrent execution
    results = await asyncio.gather(
        fetch_data("https://api.example.com/a"),
        fetch_data("https://api.example.com/b"),
    )
```

---

## Ecosystem

### Package Management

| Tool | Purpose |
|------|---------|
| pip | Package installer |
| venv | Virtual environments (built-in) |
| Poetry | Dependency management + packaging |
| uv | Fast pip/venv replacement (Rust) |
| conda | Data science environments |

### Web Frameworks

| Framework | Use Case |
|-----------|----------|
| Django | Full-stack, batteries-included |
| FastAPI | Modern APIs, async, type hints |
| Flask | Lightweight, flexible |
| Starlette | ASGI framework (FastAPI base) |

### FastAPI Example

```python
from fastapi import FastAPI
from pydantic import BaseModel

app = FastAPI()

class User(BaseModel):
    name: str
    email: str

@app.post("/users")
async def create_user(user: User) -> User:
    return user
```

### Data Science / ML

| Library | Purpose |
|---------|---------|
| NumPy | Numerical computing |
| pandas | Data manipulation |
| scikit-learn | Traditional ML |
| PyTorch | Deep learning |
| TensorFlow | Deep learning |
| Matplotlib | Plotting |
| Jupyter | Notebooks |

### Testing

| Tool | Purpose |
|------|---------|
| pytest | Test framework |
| unittest | Built-in testing |
| hypothesis | Property-based testing |
| coverage | Code coverage |

---

## Python Idioms

### List Comprehensions

```python
# Basic
squares = [x**2 for x in range(10)]

# With condition
evens = [x for x in range(20) if x % 2 == 0]

# Dict comprehension
word_lengths = {word: len(word) for word in words}

# Generator expression (lazy)
sum_squares = sum(x**2 for x in range(1000000))
```

### Context Managers

```python
# File handling
with open("file.txt") as f:
    content = f.read()

# Custom context manager
from contextlib import contextmanager

@contextmanager
def timer():
    start = time.time()
    yield
    print(f"Elapsed: {time.time() - start:.2f}s")
```

### Unpacking

```python
# Tuple unpacking
a, b = 1, 2
a, b = b, a  # Swap

# Extended unpacking
first, *rest = [1, 2, 3, 4]
first, *middle, last = [1, 2, 3, 4, 5]

# Dict unpacking
defaults = {"timeout": 30, "retries": 3}
config = {**defaults, "timeout": 60}
```

---

## Concurrency

### Options

| Model | Use Case |
|-------|----------|
| `threading` | I/O-bound (limited by GIL) |
| `multiprocessing` | CPU-bound |
| `asyncio` | I/O-bound, many connections |
| `concurrent.futures` | High-level API |

### GIL (Global Interpreter Lock)

CPython limitation: only one thread executes Python code at a time.

**Workarounds:**

- Use `multiprocessing` for CPU-bound
- Use `asyncio` for I/O-bound
- Use C extensions (NumPy releases GIL)
- Use sub-interpreters (3.12+)

---

## Project Structure

```
myproject/
├── pyproject.toml      # Project config (modern)
├── src/
│   └── myproject/
│       ├── __init__.py
│       └── main.py
├── tests/
│   └── test_main.py
└── README.md
```

### pyproject.toml

```toml
[project]
name = "myproject"
version = "0.1.0"
dependencies = [
    "fastapi>=0.100",
    "pydantic>=2.0",
]

[project.optional-dependencies]
dev = ["pytest", "ruff", "mypy"]
```

---

## Tooling

### Linting & Formatting

| Tool | Purpose |
|------|---------|
| Ruff | Fast linter + formatter (Rust) |
| Black | Opinionated formatter |
| Flake8 | Linter |
| isort | Import sorting |

### Type Checking

| Tool | Notes |
|------|-------|
| mypy | Standard type checker |
| Pyright | Microsoft, fast |
| Pyre | Facebook |

---

## When to Use Python

**Strengths:**

- Readable, expressive syntax
- Vast ecosystem
- Rapid prototyping
- ML/AI dominance
- Great for scripting

**Considerations:**

- Slower than compiled languages
- GIL limits parallelism
- Runtime errors (dynamic typing)
- Packaging complexity

**Best for:**

- Data science and ML
- Scripting and automation
- Web APIs (FastAPI, Django)
- Prototyping
- Glue code

---

## Related

- [[Django]]
- [[ML Fundamentals]]
- [[Data Structures]]
- [[Testing Frameworks]]
