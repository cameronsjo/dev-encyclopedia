---
title: Testing Strategies
aliases:
  - Testing Types
  - Test Pyramid
tags:
  - concept
  - testing
  - quality
type: reference
status: complete
created: 2025-12-04
---

# Testing Strategies

Types of software testing, when to use each, and how they fit together.

## Test Pyramid

```
                    ┌───────────┐
                   ╱             ╲         Fewer, slower, expensive
                  ╱     E2E       ╲        Full system tests
                 ╱                 ╲
                ├───────────────────┤
               ╱                     ╲
              ╱     Integration       ╲     Service boundaries
             ╱                         ╲
            ├───────────────────────────┤
           ╱                             ╲
          ╱           Unit                ╲   Many, fast, cheap
         ╱                                 ╲
        └───────────────────────────────────┘
```

| Level | Speed | Cost | Scope | Quantity |
|-------|-------|------|-------|----------|
| Unit | Fast (ms) | Cheap | Single function/class | Many |
| Integration | Medium (s) | Medium | Multiple components | Some |
| E2E | Slow (min) | Expensive | Entire system | Few |

---

## Unit Tests

Test individual functions, methods, or classes in isolation.

### Characteristics

- **Fast** — Milliseconds per test
- **Isolated** — No external dependencies
- **Focused** — One behavior per test
- **Deterministic** — Same result every time

### Example

```javascript
// function
function calculateTotal(items, taxRate) {
  const subtotal = items.reduce((sum, item) => sum + item.price, 0);
  return subtotal * (1 + taxRate);
}

// unit test
describe('calculateTotal', () => {
  it('calculates total with tax', () => {
    const items = [{ price: 10 }, { price: 20 }];
    expect(calculateTotal(items, 0.1)).toBe(33);
  });

  it('returns 0 for empty cart', () => {
    expect(calculateTotal([], 0.1)).toBe(0);
  });

  it('handles zero tax rate', () => {
    const items = [{ price: 100 }];
    expect(calculateTotal(items, 0)).toBe(100);
  });
});
```

```python
# Python with pytest
def calculate_total(items, tax_rate):
    subtotal = sum(item['price'] for item in items)
    return subtotal * (1 + tax_rate)

def test_calculate_total_with_tax():
    items = [{'price': 10}, {'price': 20}]
    assert calculate_total(items, 0.1) == 33

def test_calculate_total_empty_cart():
    assert calculate_total([], 0.1) == 0
```

### When to Use

- Pure functions
- Business logic
- Data transformations
- Utility functions
- Algorithm verification

### When Not to Use

- Database queries (use integration)
- API calls (use integration)
- UI interactions (use E2E)

---

## Integration Tests

Test how multiple components work together.

### Characteristics

- **Broader scope** — Multiple units together
- **Real dependencies** — Database, APIs, queues
- **Slower** — Seconds per test
- **Test boundaries** — Service interfaces

### Types

| Type | Tests |
|------|-------|
| Database integration | Queries, transactions |
| API integration | HTTP endpoints |
| Service integration | Microservice communication |
| Message queue | Pub/sub, async processing |

### Example

```javascript
// API integration test
describe('POST /users', () => {
  beforeEach(async () => {
    await db.clear('users');
  });

  it('creates a user and stores in database', async () => {
    const response = await request(app)
      .post('/users')
      .send({ name: 'Alice', email: 'alice@example.com' });

    expect(response.status).toBe(201);
    expect(response.body.id).toBeDefined();

    // Verify in database
    const user = await db.users.findById(response.body.id);
    expect(user.name).toBe('Alice');
  });

  it('returns 400 for invalid email', async () => {
    const response = await request(app)
      .post('/users')
      .send({ name: 'Alice', email: 'invalid' });

    expect(response.status).toBe(400);
  });
});
```

```python
# Python with pytest and test database
import pytest
from app import create_app, db

@pytest.fixture
def client():
    app = create_app({'TESTING': True, 'DATABASE_URL': 'sqlite:///:memory:'})
    with app.test_client() as client:
        with app.app_context():
            db.create_all()
        yield client

def test_create_user(client):
    response = client.post('/users', json={
        'name': 'Alice',
        'email': 'alice@example.com'
    })

    assert response.status_code == 201
    assert response.json['id'] is not None

    # Verify in database
    user = User.query.get(response.json['id'])
    assert user.name == 'Alice'
```

### Test Containers

Use real services in Docker containers.

```java
// Java with Testcontainers
@Testcontainers
class UserRepositoryTest {
    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:15");

    @Test
    void shouldSaveUser() {
        UserRepository repo = new UserRepository(postgres.getJdbcUrl());
        User user = repo.save(new User("Alice"));

        assertThat(user.getId()).isNotNull();
        assertThat(repo.findById(user.getId())).isPresent();
    }
}
```

---

## End-to-End (E2E) Tests

Test the entire application from user's perspective.

### Characteristics

- **Full stack** — Frontend → Backend → Database
- **User flows** — Complete scenarios
- **Slowest** — Minutes per test
- **Brittle** — Many failure points

### Tools

| Tool | Language | Best For |
|------|----------|----------|
| Playwright | JS/TS, Python | Modern, cross-browser |
| Cypress | JS/TS | Developer experience |
| Selenium | Many | Legacy, wide support |
| Puppeteer | JS/TS | Chrome/Chromium |

### Example (Playwright)

```typescript
import { test, expect } from '@playwright/test';

test.describe('User Registration', () => {
  test('user can sign up and log in', async ({ page }) => {
    // Sign up
    await page.goto('/signup');
    await page.fill('[name="email"]', 'alice@example.com');
    await page.fill('[name="password"]', 'securepassword');
    await page.click('button[type="submit"]');

    // Verify redirect to dashboard
    await expect(page).toHaveURL('/dashboard');
    await expect(page.locator('h1')).toContainText('Welcome, Alice');

    // Log out
    await page.click('button:text("Logout")');
    await expect(page).toHaveURL('/login');

    // Log back in
    await page.fill('[name="email"]', 'alice@example.com');
    await page.fill('[name="password"]', 'securepassword');
    await page.click('button[type="submit"]');
    await expect(page).toHaveURL('/dashboard');
  });
});
```

### Example (Cypress)

```javascript
describe('Shopping Cart', () => {
  beforeEach(() => {
    cy.login('testuser@example.com', 'password');
  });

  it('adds item to cart and checks out', () => {
    cy.visit('/products');
    cy.contains('Widget').click();
    cy.get('[data-testid="add-to-cart"]').click();

    cy.get('[data-testid="cart-count"]').should('contain', '1');

    cy.visit('/cart');
    cy.contains('Widget').should('be.visible');

    cy.get('[data-testid="checkout"]').click();
    cy.get('[name="card-number"]').type('4242424242424242');
    cy.get('[name="expiry"]').type('12/25');
    cy.get('[name="cvc"]').type('123');
    cy.get('button[type="submit"]').click();

    cy.url().should('include', '/order-confirmation');
    cy.contains('Thank you for your order').should('be.visible');
  });
});
```

---

## Behavior-Driven Development (BDD)

Write tests in natural language using Given-When-Then.

### Gherkin Syntax

```gherkin
# features/login.feature
Feature: User Login
  As a user
  I want to log in to my account
  So that I can access my dashboard

  Scenario: Successful login with valid credentials
    Given I am on the login page
    And I have a registered account with email "alice@example.com"
    When I enter my email "alice@example.com"
    And I enter my password "correctpassword"
    And I click the login button
    Then I should be redirected to the dashboard
    And I should see "Welcome, Alice"

  Scenario: Failed login with wrong password
    Given I am on the login page
    When I enter my email "alice@example.com"
    And I enter my password "wrongpassword"
    And I click the login button
    Then I should see an error message "Invalid credentials"
    And I should remain on the login page
```

### Step Definitions (Cucumber.js)

```javascript
const { Given, When, Then } = require('@cucumber/cucumber');
const { expect } = require('@playwright/test');

Given('I am on the login page', async function() {
  await this.page.goto('/login');
});

When('I enter my email {string}', async function(email) {
  await this.page.fill('[name="email"]', email);
});

When('I enter my password {string}', async function(password) {
  await this.page.fill('[name="password"]', password);
});

When('I click the login button', async function() {
  await this.page.click('button[type="submit"]');
});

Then('I should be redirected to the dashboard', async function() {
  await expect(this.page).toHaveURL('/dashboard');
});

Then('I should see {string}', async function(text) {
  await expect(this.page.locator('body')).toContainText(text);
});
```

### BDD Frameworks

| Language | Framework |
|----------|-----------|
| JavaScript | Cucumber.js |
| Python | Behave, pytest-bdd |
| Java | Cucumber-JVM |
| Ruby | Cucumber |
| .NET | SpecFlow |

---

## Contract Tests

Verify API contracts between services.

### Consumer-Driven Contracts

```
┌──────────────┐         ┌──────────────┐
│   Consumer   │────────▶│   Provider   │
│   (Client)   │         │   (Server)   │
└──────────────┘         └──────────────┘
       │                        │
       ▼                        ▼
  Consumer Test            Provider Test
  "I expect this"          "I provide this"
       │                        │
       └────────┬───────────────┘
                │
           ┌────▼────┐
           │ Contract │
           │  Broker  │
           └──────────┘
```

### Example (Pact)

```javascript
// Consumer test
const { Pact } = require('@pact-foundation/pact');

describe('User API', () => {
  const provider = new Pact({
    consumer: 'Frontend',
    provider: 'UserService',
  });

  beforeAll(() => provider.setup());
  afterAll(() => provider.finalize());

  it('gets user by id', async () => {
    await provider.addInteraction({
      state: 'user 123 exists',
      uponReceiving: 'a request for user 123',
      withRequest: {
        method: 'GET',
        path: '/users/123',
      },
      willRespondWith: {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
        body: {
          id: 123,
          name: 'Alice',
          email: 'alice@example.com',
        },
      },
    });

    const response = await fetch(`${provider.mockService.baseUrl}/users/123`);
    const user = await response.json();

    expect(user.name).toBe('Alice');
  });
});
```

---

## Snapshot Tests

Compare output against saved "snapshot."

### UI Snapshots

```javascript
// React component snapshot
import { render } from '@testing-library/react';
import UserCard from './UserCard';

test('UserCard matches snapshot', () => {
  const { container } = render(
    <UserCard name="Alice" email="alice@example.com" />
  );
  expect(container).toMatchSnapshot();
});
```

### API Response Snapshots

```javascript
test('GET /users returns expected structure', async () => {
  const response = await request(app).get('/users');
  expect(response.body).toMatchSnapshot();
});
```

### When to Use

- UI components
- API responses
- Generated code/config
- Serialized data

### When Not to Use

- Frequently changing data
- Timestamps, random IDs
- Large outputs

---

## Property-Based Tests

Test with randomly generated inputs.

### Example (fast-check)

```javascript
import fc from 'fast-check';

// Test properties that should always hold
describe('sort function', () => {
  it('should return same length', () => {
    fc.assert(
      fc.property(fc.array(fc.integer()), (arr) => {
        return sort(arr).length === arr.length;
      })
    );
  });

  it('should return sorted array', () => {
    fc.assert(
      fc.property(fc.array(fc.integer()), (arr) => {
        const sorted = sort(arr);
        for (let i = 1; i < sorted.length; i++) {
          if (sorted[i] < sorted[i - 1]) return false;
        }
        return true;
      })
    );
  });

  it('should contain same elements', () => {
    fc.assert(
      fc.property(fc.array(fc.integer()), (arr) => {
        const sorted = sort(arr);
        return arr.every(x => sorted.includes(x)) &&
               sorted.every(x => arr.includes(x));
      })
    );
  });
});
```

### Frameworks

| Language | Framework |
|----------|-----------|
| JavaScript | fast-check |
| Python | Hypothesis |
| Scala | ScalaCheck |
| Haskell | QuickCheck |
| Rust | proptest |

---

## Mutation Tests

Verify test quality by introducing bugs.

```
Original Code              Mutant (bug introduced)
─────────────              ─────────────────────
if (x > 0)                 if (x >= 0)     // Changed > to >=
return x + y               return x - y     // Changed + to -
```

If tests still pass → **weak tests** (mutant survived)
If tests fail → **good tests** (mutant killed)

### Tools

| Language | Tool |
|----------|------|
| JavaScript | Stryker |
| Python | mutmut |
| Java | PITest |
| Ruby | mutant |

---

## Performance Tests

### Load Testing

```javascript
// k6 load test
import http from 'k6/http';
import { check, sleep } from 'k6';

export const options = {
  vus: 100,           // Virtual users
  duration: '5m',     // Test duration
  thresholds: {
    http_req_duration: ['p(95)<500'],  // 95% under 500ms
  },
};

export default function() {
  const res = http.get('http://api.example.com/users');

  check(res, {
    'status is 200': (r) => r.status === 200,
    'response time < 500ms': (r) => r.timings.duration < 500,
  });

  sleep(1);
}
```

### Tools

| Tool | Type |
|------|------|
| k6 | Load testing |
| Artillery | Load testing |
| Locust | Load testing (Python) |
| Apache Benchmark | Simple load |
| wrk | HTTP benchmarking |

---

## Test Organization

### Naming Conventions

```
Pattern: should_ExpectedBehavior_When_Condition

Examples:
- should_ReturnNull_When_UserNotFound
- should_ThrowException_When_InvalidInput
- should_SendEmail_When_OrderConfirmed
```

### Arrange-Act-Assert (AAA)

```javascript
test('calculates order total correctly', () => {
  // Arrange
  const items = [
    { name: 'Widget', price: 25.00, quantity: 2 },
    { name: 'Gadget', price: 15.00, quantity: 1 },
  ];
  const order = new Order(items);

  // Act
  const total = order.calculateTotal();

  // Assert
  expect(total).toBe(65.00);
});
```

### Test Fixtures

```python
# pytest fixtures
@pytest.fixture
def database():
    db = create_test_database()
    db.seed_test_data()
    yield db
    db.cleanup()

@pytest.fixture
def authenticated_user(database):
    user = database.create_user(email='test@example.com')
    return user

def test_user_can_create_post(authenticated_user, database):
    post = database.create_post(author=authenticated_user, title='Test')
    assert post.id is not None
```

---

## Test Coverage

### Types of Coverage

| Type | Measures |
|------|----------|
| Line | Lines executed |
| Branch | Decision branches taken |
| Function | Functions called |
| Statement | Statements executed |

### Coverage Goals

| Level | Recommendation |
|-------|----------------|
| <50% | Insufficient |
| 50-70% | Minimum viable |
| 70-80% | Good |
| 80-90% | Very good |
| >90% | Excellent (but diminishing returns) |

### Don't Aim for 100%

```javascript
// This code doesn't need testing
if (process.env.NODE_ENV === 'development') {
  console.log('Debug mode');
}
```

---

## Decision Guide

| Goal | Test Type |
|------|-----------|
| Function correctness | Unit |
| Component integration | Integration |
| User workflows | E2E |
| Stakeholder communication | BDD |
| API compatibility | Contract |
| Regression prevention | Snapshot |
| Edge cases | Property-based |
| Test quality | Mutation |
| Performance | Load |

### Test Ratio (Suggested)

```
Typical ratio:
├── Unit tests:        70%
├── Integration tests: 20%
└── E2E tests:         10%
```

---

## Related

- [[Testing Frameworks]]
- [[Debugging Tools]]
- [[Deployment]]
