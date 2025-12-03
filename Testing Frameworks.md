---
title: Testing Frameworks
aliases:
  - Unit Testing
  - Test Runners
tags:
  - comparison
  - languages
  - tools
  - testing
  - csharp
  - go
  - python
  - typescript
  - rust
type: comparison
status: complete
created: '2025-11-28'
---

# Testing Frameworks

Cross-language comparison of testing frameworks, patterns, and best practices.

## Overview

| Language | Framework | Runner | Assertions | Mocking |
|----------|-----------|--------|------------|---------|
| C# | xUnit / NUnit | Built-in | Fluent (FluentAssertions) | Moq, NSubstitute |
| Go | testing | `go test` | testify | gomock, testify/mock |
| Python | pytest | pytest | Built-in + pytest | unittest.mock, pytest-mock |
| TypeScript | Jest / Vitest | Built-in | Built-in / Chai | Built-in |
| Rust | Built-in | `cargo test` | Built-in | mockall |

## Feature Matrix

| Feature | xUnit | go test | pytest | Jest/Vitest | cargo test |
|---------|-------|---------|--------|-------------|------------|
| Parallel execution | ✅ | ✅ | ✅ | ✅ | ✅ |
| Test discovery | ✅ Attributes | ✅ `Test*` prefix | ✅ `test_*` prefix | ✅ `*.test.*` | ✅ `#[test]` |
| Fixtures/Setup | `IClassFixture` | `TestMain` | `@pytest.fixture` | `beforeEach` | Custom |
| Parameterized | `[Theory]` | Table-driven | `@pytest.mark.parametrize` | `test.each` | Macros |
| Snapshots | Verify | cupaloy | syrupy | ✅ Built-in | insta |
| Coverage | coverlet | `go test -cover` | pytest-cov | Built-in | cargo-tarpaulin |
| Watch mode | dotnet watch | ❌ | pytest-watch | ✅ | cargo-watch |
| Async tests | ✅ | ✅ | ✅ | ✅ | ✅ tokio::test |

## Test Structure

### C# — xUnit

```csharp
public class CalculatorTests
{
    private readonly Calculator _calculator = new();

    [Fact]
    public void Add_TwoNumbers_ReturnsSum()
    {
        var result = _calculator.Add(2, 3);

        Assert.Equal(5, result);
    }

    [Theory]
    [InlineData(1, 1, 2)]
    [InlineData(2, 3, 5)]
    [InlineData(-1, 1, 0)]
    public void Add_Parameterized_ReturnsExpected(int a, int b, int expected)
    {
        Assert.Equal(expected, _calculator.Add(a, b));
    }
}
```

### Go — Table-Driven Tests

```go
func TestAdd(t *testing.T) {
    tests := []struct {
        name     string
        a, b     int
        expected int
    }{
        {"positive", 2, 3, 5},
        {"negative", -1, 1, 0},
        {"zeros", 0, 0, 0},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            result := Add(tt.a, tt.b)
            if result != tt.expected {
                t.Errorf("Add(%d, %d) = %d; want %d",
                    tt.a, tt.b, result, tt.expected)
            }
        })
    }
}
```

### Python — pytest

```python
import pytest
from calculator import Calculator

@pytest.fixture
def calculator():
    return Calculator()

def test_add_two_numbers(calculator):
    assert calculator.add(2, 3) == 5

@pytest.mark.parametrize("a,b,expected", [
    (1, 1, 2),
    (2, 3, 5),
    (-1, 1, 0),
])
def test_add_parameterized(calculator, a, b, expected):
    assert calculator.add(a, b) == expected
```

### TypeScript — Vitest

```typescript
import { describe, it, expect, beforeEach } from 'vitest';
import { Calculator } from './calculator';

describe('Calculator', () => {
    let calculator: Calculator;

    beforeEach(() => {
        calculator = new Calculator();
    });

    it('adds two numbers', () => {
        expect(calculator.add(2, 3)).toBe(5);
    });

    it.each([
        [1, 1, 2],
        [2, 3, 5],
        [-1, 1, 0],
    ])('add(%i, %i) = %i', (a, b, expected) => {
        expect(calculator.add(a, b)).toBe(expected);
    });
});
```

### Rust — Built-in

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_two_numbers() {
        let calc = Calculator::new();
        assert_eq!(calc.add(2, 3), 5);
    }

    #[test]
    fn add_negative() {
        let calc = Calculator::new();
        assert_eq!(calc.add(-1, 1), 0);
    }

    // Parameterized with rstest crate
    // #[rstest]
    // #[case(1, 1, 2)]
    // #[case(2, 3, 5)]
    // fn add_parameterized(#[case] a: i32, #[case] b: i32, #[case] expected: i32) {
    //     assert_eq!(add(a, b), expected);
    // }
}
```

---

## Mocking & Test Doubles

### Mocking Comparison

| Language | Library | Style |
|----------|---------|-------|
| C# | Moq | Lambda-based setup |
| C# | NSubstitute | Natural syntax |
| Go | gomock | Code generation |
| Go | testify/mock | Struct embedding |
| Python | unittest.mock | Patch decorators |
| TS | Jest | `jest.fn()`, `jest.mock()` |
| Rust | mockall | Derive macros |

### C# — Moq

```csharp
[Fact]
public async Task GetUser_CallsRepository()
{
    var mockRepo = new Mock<IUserRepository>();
    mockRepo
        .Setup(r => r.GetByIdAsync(42))
        .ReturnsAsync(new User { Id = 42, Name = "Test" });

    var service = new UserService(mockRepo.Object);
    var user = await service.GetUserAsync(42);

    Assert.Equal("Test", user.Name);
    mockRepo.Verify(r => r.GetByIdAsync(42), Times.Once);
}
```

### Go — testify/mock

```go
type MockRepository struct {
    mock.Mock
}

func (m *MockRepository) GetByID(id int) (*User, error) {
    args := m.Called(id)
    return args.Get(0).(*User), args.Error(1)
}

func TestGetUser(t *testing.T) {
    mockRepo := new(MockRepository)
    mockRepo.On("GetByID", 42).Return(&User{ID: 42, Name: "Test"}, nil)

    service := NewUserService(mockRepo)
    user, err := service.GetUser(42)

    assert.NoError(t, err)
    assert.Equal(t, "Test", user.Name)
    mockRepo.AssertExpectations(t)
}
```

### Python — pytest-mock

```python
def test_get_user(mocker):
    mock_repo = mocker.Mock()
    mock_repo.get_by_id.return_value = User(id=42, name="Test")

    service = UserService(mock_repo)
    user = service.get_user(42)

    assert user.name == "Test"
    mock_repo.get_by_id.assert_called_once_with(42)
```

### TypeScript — Vitest

```typescript
import { vi, describe, it, expect } from 'vitest';

describe('UserService', () => {
    it('calls repository', async () => {
        const mockRepo = {
            getById: vi.fn().mockResolvedValue({ id: 42, name: 'Test' }),
        };

        const service = new UserService(mockRepo);
        const user = await service.getUser(42);

        expect(user.name).toBe('Test');
        expect(mockRepo.getById).toHaveBeenCalledWith(42);
    });
});
```

---

## Async Testing

### C\#

```csharp
[Fact]
public async Task FetchData_ReturnsExpected()
{
    var result = await _service.FetchDataAsync();
    Assert.NotNull(result);
}
```

### Go

```go
func TestFetchData(t *testing.T) {
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()

    result, err := service.FetchData(ctx)

    require.NoError(t, err)
    assert.NotNil(t, result)
}
```

### Python

```python
import pytest

@pytest.mark.asyncio
async def test_fetch_data():
    result = await service.fetch_data()
    assert result is not None
```

### TypeScript

```typescript
it('fetches data', async () => {
    const result = await service.fetchData();
    expect(result).toBeDefined();
});
```

### Rust

```rust
#[tokio::test]
async fn fetch_data_returns_expected() {
    let result = service.fetch_data().await;
    assert!(result.is_ok());
}
```

---

## Test Organization

### Directory Structure

| Language | Convention |
|----------|------------|
| C# | `*.Tests` project, mirrors source structure |
| Go | `*_test.go` alongside source |
| Python | `tests/` directory, `test_*.py` files |
| TypeScript | `__tests__/` or `*.test.ts` alongside |
| Rust | `#[cfg(test)]` modules inline, or `tests/` for integration |

### C# Project Structure

```
src/
  MyApp/
    Services/
      UserService.cs
tests/
  MyApp.Tests/
    Services/
      UserServiceTests.cs
```

### Go Package Structure

```
pkg/
  user/
    service.go
    service_test.go
```

### Python Structure

```
src/
  myapp/
    services/
      user_service.py
tests/
  services/
    test_user_service.py
```

### Rust Structure

```rust
// Inline unit tests
// src/lib.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(add(2, 2), 4);
    }
}

// Integration tests in tests/ directory
// tests/integration_test.rs
use mylib::add;

#[test]
fn integration_add() {
    assert_eq!(add(2, 2), 4);
}
```

---

## Running Tests

| Language | Run All | Run Specific | Watch | Coverage |
|----------|---------|--------------|-------|----------|
| C# | `dotnet test` | `dotnet test --filter "Name~Add"` | `dotnet watch test` | `dotnet test --collect:"XPlat Code Coverage"` |
| Go | `go test ./...` | `go test -run TestAdd` | N/A | `go test -cover ./...` |
| Python | `pytest` | `pytest -k "test_add"` | `ptw` | `pytest --cov` |
| TS | `npm test` | `npm test -- --grep "add"` | `npm test -- --watch` | `npm test -- --coverage` |
| Rust | `cargo test` | `cargo test add` | `cargo watch -x test` | `cargo tarpaulin` |

---

## Best Practices

### Test Naming

| Style | Example |
|-------|---------|
| BDD | `should_return_sum_when_adding_two_numbers` |
| Given-When-Then | `GivenTwoNumbers_WhenAdded_ThenReturnsSum` |
| Method_Scenario_Expected | `Add_TwoPositiveNumbers_ReturnsSum` |

### AAA Pattern

```
Arrange — Set up test data and dependencies
Act — Execute the code under test
Assert — Verify the results
```

### What to Test

| Level | Focus | Tools |
|-------|-------|-------|
| Unit | Single function/method | Mocks for dependencies |
| Integration | Multiple components | Real dependencies, test DB |
| E2E | Full user flows | Browser automation, API calls |

### Test Quality Checklist

- [ ] Tests are independent (no shared state)
- [ ] Tests are deterministic (no flakiness)
- [ ] Tests are fast (< 100ms for unit tests)
- [ ] Tests have clear names describing behavior
- [ ] Tests cover edge cases and error paths
- [ ] Mocks verify interactions, not implementation

---

## Decision Guide

| Priority | Recommendation |
|----------|----------------|
| Most mature | pytest (Python), xUnit (C#) |
| Fastest execution | Go built-in, Rust built-in |
| Best DX | Vitest (TS), pytest (Python) |
| Best parallelization | Go, Rust |
| Richest assertions | FluentAssertions (C#), pytest |

---

## Related

- [[Terminal UI & Language Features]]
- [[Logging Libraries]]
- [[Build Systems]]
- [[Debugging Tools]]
