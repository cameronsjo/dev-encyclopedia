---
title: TDD and BDD
aliases:
  - Test-Driven Development
  - Behavior-Driven Development
  - Test First Development
tags:
  - cs
  - testing
  - practices
  - methodology
type: reference
status: complete
created: "2025-12-18"
---

# TDD and BDD

Development methodologies that prioritize testing as a design and specification tool.

## Overview

| Aspect | TDD | BDD |
|--------|-----|-----|
| **Full Name** | Test-Driven Development | Behavior-Driven Development |
| **Focus** | Code correctness | System behavior |
| **Tests Written By** | Developers | Developers + stakeholders |
| **Language** | Technical assertions | Natural language specs |
| **Granularity** | Unit level | Feature/behavior level |

## Test-Driven Development (TDD)

### The TDD Cycle (Red-Green-Refactor)

```
        ┌─────────────────────┐
        │                     │
        │        RED          │
        │  Write failing test │
        │                     │
        └──────────┬──────────┘
                   │
                   ▼
        ┌──────────────────────┐
        │                      │
        │        GREEN         │
        │  Write minimal code  │
        │  to pass the test    │
        │                      │
        └──────────┬───────────┘
                   │
                   ▼
        ┌──────────────────────┐
        │                      │
        │      REFACTOR        │
        │  Improve code while  │
        │  keeping tests green │
        │                      │
        └──────────┬───────────┘
                   │
                   └───────────────┐
                                   │
                                   ▼
                            (Repeat cycle)
```

### TDD Example

```python
# Step 1: RED - Write failing test
def test_add_returns_sum():
    result = add(2, 3)
    assert result == 5

# Running this fails: NameError: name 'add' is not defined

# Step 2: GREEN - Write minimal code to pass
def add(a, b):
    return 5  # Minimal code to pass this specific test

# Test passes, but implementation is wrong...

# Step 3: Refactor with more tests
def test_add_returns_sum():
    assert add(2, 3) == 5
    assert add(0, 0) == 0
    assert add(-1, 1) == 0

# Now we fix the implementation
def add(a, b):
    return a + b
```

### TDD Best Practices

| Practice | Description |
|----------|-------------|
| **One test at a time** | Focus on single behavior |
| **Minimal code** | Only write code to pass test |
| **Run tests frequently** | After every small change |
| **Refactor with confidence** | Tests catch regressions |
| **Test behavior, not implementation** | Focus on what, not how |

### TDD Anti-Patterns

| Anti-Pattern | Problem |
|--------------|---------|
| **Writing tests after code** | Loses design benefits |
| **Testing implementation** | Tests break on refactor |
| **Too many tests at once** | Loses focus, harder to debug |
| **Skipping refactor step** | Accumulates technical debt |
| **Testing private methods** | Couples to implementation |

## Behavior-Driven Development (BDD)

### BDD Structure (Given-When-Then)

```gherkin
Feature: User Authentication
  As a registered user
  I want to log into my account
  So that I can access my personal dashboard

  Scenario: Successful login
    Given I am on the login page
    And I have a registered account with email "user@example.com"
    When I enter email "user@example.com"
    And I enter password "correctpassword"
    And I click the login button
    Then I should be redirected to the dashboard
    And I should see "Welcome back" message

  Scenario: Failed login with wrong password
    Given I am on the login page
    When I enter email "user@example.com"
    And I enter password "wrongpassword"
    And I click the login button
    Then I should see "Invalid credentials" error
    And I should remain on the login page
```

### BDD Flow

```
┌─────────────────────────────────────────────────────────────┐
│  1. Discovery Workshop                                       │
│     Stakeholders + Devs + QA discuss feature                │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  2. Write Scenarios                                          │
│     Gherkin/natural language specifications                 │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  3. Automate Scenarios                                       │
│     Step definitions that run the application               │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│  4. Implement Feature                                        │
│     Make scenarios pass (TDD at implementation level)       │
└─────────────────────────────────────────────────────────────┘
```

### Step Definitions

```python
# Python with Behave
from behave import given, when, then

@given('I am on the login page')
def step_on_login_page(context):
    context.browser.get('/login')

@when('I enter email "{email}"')
def step_enter_email(context, email):
    context.browser.find_element_by_id('email').send_keys(email)

@when('I click the login button')
def step_click_login(context):
    context.browser.find_element_by_id('login-btn').click()

@then('I should be redirected to the dashboard')
def step_on_dashboard(context):
    assert context.browser.current_url.endswith('/dashboard')
```

```javascript
// JavaScript with Cucumber
const { Given, When, Then } = require('@cucumber/cucumber');

Given('I am on the login page', async function() {
  await this.page.goto('/login');
});

When('I enter email {string}', async function(email) {
  await this.page.fill('#email', email);
});

Then('I should be redirected to the dashboard', async function() {
  expect(this.page.url()).toContain('/dashboard');
});
```

### BDD Frameworks

| Language | Framework |
|----------|-----------|
| **JavaScript** | Cucumber.js, Jest with Gherkin |
| **Python** | Behave, pytest-bdd |
| **Ruby** | Cucumber, RSpec |
| **Java** | Cucumber-JVM, JBehave |
| **C#** | SpecFlow |
| **Go** | Godog |

## TDD vs BDD Comparison

| Aspect | TDD | BDD |
|--------|-----|-----|
| **Primary Users** | Developers | Whole team |
| **Test Granularity** | Unit tests | Feature/integration |
| **Language** | Code assertions | Natural language |
| **Focus** | Implementation correctness | Business behavior |
| **Documentation** | Code is documentation | Living documentation |
| **Collaboration** | Developer-centric | Cross-functional |

## Combining TDD and BDD

```
┌─────────────────────────────────────────────────────────────┐
│                     BDD Layer                                │
│   Feature specs, acceptance tests, business behavior        │
│   (Cucumber/Gherkin scenarios)                              │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           │ Drives
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                     TDD Layer                                │
│   Unit tests, implementation details, code correctness      │
│   (JUnit, pytest, Jest unit tests)                          │
└─────────────────────────────────────────────────────────────┘
```

### Example Workflow

```
1. BDD: Write scenario "User can view order history"

2. Run scenario (fails - no implementation)

3. TDD: Write unit test for OrderRepository.getByUserId()
   - Red: Test fails
   - Green: Implement method
   - Refactor: Clean up

4. TDD: Write unit test for OrderService.getUserOrders()
   - Red/Green/Refactor cycle

5. TDD: Write unit test for OrderController endpoint
   - Red/Green/Refactor cycle

6. Run BDD scenario again (now passes)
```

## Testing Pyramid with TDD/BDD

```
                    ╱╲
                   ╱  ╲
                  ╱ E2E╲         BDD Scenarios
                 ╱──────╲        (Few, slow)
                ╱        ╲
               ╱Integration╲     BDD + TDD
              ╱────────────╲     (Some, medium)
             ╱              ╲
            ╱   Unit Tests   ╲   TDD
           ╱──────────────────╲  (Many, fast)
          ╱                    ╲
```

## Benefits

### TDD Benefits

| Benefit | Description |
|---------|-------------|
| **Design feedback** | Tests reveal poor design early |
| **Regression safety** | Changes don't break existing code |
| **Documentation** | Tests document expected behavior |
| **Confidence** | Refactor without fear |
| **Focus** | Work on one thing at a time |

### BDD Benefits

| Benefit | Description |
|---------|-------------|
| **Shared understanding** | Team aligned on behavior |
| **Living documentation** | Specs always up to date |
| **Business alignment** | Tests reflect requirements |
| **Reduced rework** | Catch misunderstandings early |
| **Traceability** | Link tests to requirements |

## Common Patterns

### Arrange-Act-Assert (AAA)

```python
def test_deposit_increases_balance():
    # Arrange
    account = BankAccount(initial_balance=100)

    # Act
    account.deposit(50)

    # Assert
    assert account.balance == 150
```

### Test Doubles

| Type | Purpose | Example |
|------|---------|---------|
| **Stub** | Provide canned answers | Return fixed value |
| **Mock** | Verify interactions | Check method called |
| **Fake** | Working implementation | In-memory database |
| **Spy** | Record calls | Log method invocations |

```python
# Mock example
def test_sends_welcome_email():
    # Arrange
    email_service = Mock()
    user_service = UserService(email_service)

    # Act
    user_service.register("user@example.com")

    # Assert
    email_service.send.assert_called_once_with(
        to="user@example.com",
        template="welcome"
    )
```

## Related

- [[Testing Frameworks]] — Testing tools
- [[Property-Based Testing]] — Generative testing
- [[Code Review]] — Quality practices
- [[Computer Science MOC]] — CS topics
