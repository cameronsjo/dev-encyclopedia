---
title: Property-Based Testing
aliases:
  - PBT
  - QuickCheck
  - Generative Testing
tags:
  - cs
  - testing
  - practices
type: reference
status: complete
created: "2025-12-18"
---

# Property-Based Testing

Testing technique that verifies properties hold for randomly generated inputs.

## Overview

| Aspect | Details |
|--------|---------|
| **Concept** | Define properties, generate test cases |
| **Origin** | QuickCheck (Haskell, 1999) |
| **Approach** | Many random inputs vs few handpicked examples |
| **Benefit** | Finds edge cases humans miss |

## Example-Based vs Property-Based

### Example-Based (Traditional)

```python
def test_reverse():
    assert reverse([1, 2, 3]) == [3, 2, 1]
    assert reverse([]) == []
    assert reverse([1]) == [1]
    # Limited to cases we think of
```

### Property-Based

```python
from hypothesis import given
from hypothesis import strategies as st

@given(st.lists(st.integers()))
def test_reverse_twice_is_identity(xs):
    assert reverse(reverse(xs)) == xs

@given(st.lists(st.integers()))
def test_reverse_preserves_length(xs):
    assert len(reverse(xs)) == len(xs)

# Tests run with hundreds of random inputs
```

## Key Concepts

### Properties

**Invariants that should hold for all valid inputs.**

| Property Type | Description | Example |
|---------------|-------------|---------|
| **Round-trip** | Operation is reversible | encode(decode(x)) == x |
| **Idempotent** | Applying twice = once | sort(sort(x)) == sort(x) |
| **Commutative** | Order doesn't matter | a + b == b + a |
| **Invariant** | Something preserved | len(sort(x)) == len(x) |
| **Comparison** | Compare with known-good | fastSort(x) == sort(x) |

### Generators (Strategies)

```python
from hypothesis import strategies as st

# Primitive types
st.integers()
st.floats()
st.text()
st.booleans()

# Bounded
st.integers(min_value=0, max_value=100)
st.text(min_size=1, max_size=10)

# Collections
st.lists(st.integers())
st.sets(st.text())
st.dictionaries(st.text(), st.integers())

# Composite
st.tuples(st.integers(), st.text())
st.one_of(st.integers(), st.text())

# Custom
@st.composite
def user_strategy(draw):
    name = draw(st.text(min_size=1))
    age = draw(st.integers(min_value=0, max_value=150))
    return User(name=name, age=age)
```

### Shrinking

**When a failing case is found, automatically minimize it.**

```
Found failing input: [847, -293, 0, 12, 99, -5, 8]
Shrinking...
Minimal failing input: [0, -1]
```

## Common Properties

### Mathematical Properties

```python
# Associativity
@given(st.integers(), st.integers(), st.integers())
def test_addition_associative(a, b, c):
    assert (a + b) + c == a + (b + c)

# Commutativity
@given(st.integers(), st.integers())
def test_addition_commutative(a, b):
    assert a + b == b + a

# Identity
@given(st.integers())
def test_addition_identity(a):
    assert a + 0 == a
```

### Data Structure Properties

```python
# Sorting
@given(st.lists(st.integers()))
def test_sort_idempotent(xs):
    assert sorted(sorted(xs)) == sorted(xs)

@given(st.lists(st.integers()))
def test_sort_preserves_elements(xs):
    assert sorted(xs) == sorted(sorted(xs))
    assert set(sorted(xs)) == set(xs)

@given(st.lists(st.integers()))
def test_sort_is_ordered(xs):
    result = sorted(xs)
    for i in range(len(result) - 1):
        assert result[i] <= result[i + 1]
```

### Encoding/Serialization Properties

```python
@given(st.dictionaries(st.text(), st.integers()))
def test_json_roundtrip(data):
    assert json.loads(json.dumps(data)) == data

@given(st.binary())
def test_base64_roundtrip(data):
    assert base64.b64decode(base64.b64encode(data)) == data
```

### API/Database Properties

```python
@given(user_strategy())
def test_user_crud(user):
    # Create
    user_id = db.create_user(user)

    # Read
    retrieved = db.get_user(user_id)
    assert retrieved.name == user.name
    assert retrieved.age == user.age

    # Update
    user.age = user.age + 1
    db.update_user(user_id, user)
    assert db.get_user(user_id).age == user.age

    # Delete
    db.delete_user(user_id)
    assert db.get_user(user_id) is None
```

## Property-Based Testing Libraries

| Language | Library |
|----------|---------|
| **Python** | Hypothesis |
| **JavaScript** | fast-check |
| **Java** | jqwik, junit-quickcheck |
| **Haskell** | QuickCheck |
| **Scala** | ScalaCheck |
| **Rust** | proptest, quickcheck |
| **Go** | gopter |
| **C#** | FsCheck |
| **Elixir** | StreamData |

## Hypothesis (Python)

### Basic Usage

```python
from hypothesis import given, settings, assume
from hypothesis import strategies as st

@given(st.integers(), st.integers())
def test_addition_commutative(x, y):
    assert x + y == y + x

# With settings
@settings(max_examples=1000)
@given(st.lists(st.integers()))
def test_with_more_examples(xs):
    assert reverse(reverse(xs)) == xs

# With assumptions (filter invalid inputs)
@given(st.integers(), st.integers())
def test_division(x, y):
    assume(y != 0)  # Skip when y is 0
    assert (x * y) / y == x
```

### Custom Strategies

```python
from hypothesis import strategies as st
from dataclasses import dataclass

@dataclass
class Order:
    id: str
    items: list
    total: float

# Build strategy from fields
order_strategy = st.builds(
    Order,
    id=st.uuids().map(str),
    items=st.lists(st.text(min_size=1), min_size=1),
    total=st.floats(min_value=0, max_value=10000)
)

@given(order_strategy)
def test_order_processing(order):
    result = process_order(order)
    assert result.status in ['success', 'pending']
```

## fast-check (JavaScript)

```javascript
import fc from 'fast-check';

// Basic test
test('string reverse', () => {
  fc.assert(
    fc.property(fc.string(), (s) => {
      return reverse(reverse(s)) === s;
    })
  );
});

// With complex data
test('user serialization', () => {
  const userArb = fc.record({
    name: fc.string({ minLength: 1 }),
    email: fc.emailAddress(),
    age: fc.integer({ min: 0, max: 150 })
  });

  fc.assert(
    fc.property(userArb, (user) => {
      const json = JSON.stringify(user);
      const parsed = JSON.parse(json);
      return parsed.name === user.name &&
             parsed.email === user.email &&
             parsed.age === user.age;
    })
  );
});

// With preconditions
test('division', () => {
  fc.assert(
    fc.property(
      fc.integer(),
      fc.integer().filter(n => n !== 0),
      (x, y) => {
        return Math.abs((x * y) / y - x) < 0.0001;
      }
    )
  );
});
```

## Common Patterns

### Oracle Testing

**Compare your implementation against a reference.**

```python
@given(st.lists(st.integers()))
def test_quicksort_matches_builtin(xs):
    assert quicksort(xs) == sorted(xs)
```

### Metamorphic Testing

**If input changes in known way, output should change predictably.**

```python
@given(st.lists(st.integers()))
def test_sort_adding_element(xs):
    x = 0
    result1 = sorted(xs)
    result2 = sorted(xs + [x])
    # Adding element should increase length by 1
    assert len(result2) == len(result1) + 1
```

### Stateful Testing

**Test sequences of operations.**

```python
from hypothesis.stateful import RuleBasedStateMachine, rule, invariant

class SetMachine(RuleBasedStateMachine):
    def __init__(self):
        super().__init__()
        self.model = set()  # Reference implementation
        self.impl = MySet()  # Our implementation

    @rule(x=st.integers())
    def add(self, x):
        self.model.add(x)
        self.impl.add(x)

    @rule(x=st.integers())
    def remove(self, x):
        self.model.discard(x)
        self.impl.remove(x)

    @invariant()
    def sets_match(self):
        assert set(self.impl) == self.model

TestSet = SetMachine.TestCase
```

## Benefits and Challenges

### Benefits

| Benefit | Description |
|---------|-------------|
| **Edge case discovery** | Finds inputs you'd never think of |
| **Higher coverage** | Tests thousands of cases |
| **Specification clarity** | Forces you to think about properties |
| **Regression testing** | Shrunk failing cases become examples |
| **Less test maintenance** | Properties stable even as impl changes |

### Challenges

| Challenge | Mitigation |
|-----------|------------|
| **Finding good properties** | Start with simple invariants |
| **Slow tests** | Limit examples, optimize generators |
| **Flaky tests** | Use seeds, avoid time-dependent tests |
| **Complex custom types** | Build composable strategies |
| **Understanding failures** | Rely on shrinking |

## When to Use

| Good For | Not Ideal For |
|----------|---------------|
| ✅ Pure functions | ❌ UI testing |
| ✅ Parsers/serializers | ❌ Integration tests |
| ✅ Data structures | ❌ Stateful systems (harder) |
| ✅ Mathematical code | ❌ Tests requiring specific setup |
| ✅ Finding edge cases | ❌ When properties unclear |

## Related

- [[TDD and BDD]] — Test-driven development
- [[Testing Frameworks]] — Testing tools
- [[Computer Science MOC]] — CS topics
