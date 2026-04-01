---
title: Code Review
aliases:
  - Code Reviews
  - Pull Request Review
  - PR Review
tags:
  - cs
  - practices
  - collaboration
  - quality
type: reference
status: complete
created: "2025-12-18"
---

# Code Review

Systematic examination of source code by peers to improve quality and share knowledge.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Find bugs, improve code quality, share knowledge |
| **Timing** | Before merging to main branch |
| **Participants** | Author + 1-2 reviewers (typically) |
| **Tools** | GitHub, GitLab, Bitbucket, Gerrit, Phabricator |

## Benefits

| Benefit | Description |
|---------|-------------|
| **Bug Detection** | Catch issues before production |
| **Knowledge Sharing** | Spread understanding across team |
| **Code Quality** | Maintain standards and consistency |
| **Learning** | Junior devs learn from reviews |
| **Documentation** | PR descriptions explain changes |
| **Reduced Risk** | Multiple eyes reduce single points of failure |

## Review Process

### Typical Workflow

```
┌─────────────────────────────────────────────────────────────┐
│ 1. Developer creates branch and makes changes               │
│                          │                                   │
│                          ▼                                   │
│ 2. Opens Pull Request with description                       │
│                          │                                   │
│                          ▼                                   │
│ 3. Automated checks run (CI, linting, tests)                │
│                          │                                   │
│                          ▼                                   │
│ 4. Reviewer(s) assigned or requested                        │
│                          │                                   │
│                          ▼                                   │
│ 5. Reviewer examines code, leaves comments                  │
│                          │                                   │
│                          ▼                                   │
│ 6. Author addresses feedback, pushes updates                │
│                          │                                   │
│                          ▼                                   │
│ 7. Reviewer approves (or requests more changes)             │
│                          │                                   │
│                          ▼                                   │
│ 8. PR merged to main branch                                 │
└─────────────────────────────────────────────────────────────┘
```

## What to Look For

### Code Quality

| Area | Check For |
|------|-----------|
| **Correctness** | Does it do what it's supposed to? |
| **Logic** | Are there edge cases missed? |
| **Performance** | Any obvious inefficiencies? |
| **Security** | Input validation, injection risks? |
| **Error Handling** | Are errors handled gracefully? |
| **Tests** | Are there adequate tests? |

### Design

| Area | Check For |
|------|-----------|
| **Architecture** | Does it fit the system design? |
| **Complexity** | Can it be simplified? |
| **DRY** | Is there unnecessary duplication? |
| **SOLID** | Are principles followed? |
| **Dependencies** | Are new deps justified? |

### Maintainability

| Area | Check For |
|------|-----------|
| **Readability** | Is it easy to understand? |
| **Naming** | Are names clear and consistent? |
| **Comments** | Are complex parts explained? |
| **Documentation** | Are public APIs documented? |
| **Consistency** | Does it match codebase style? |

## Writing Good PR Descriptions

### Template

```markdown
## Summary
Brief description of what this PR does.

## Motivation
Why is this change needed? Link to issue/ticket.

## Changes
- Added X feature
- Fixed Y bug
- Refactored Z module

## Testing
How was this tested?
- [ ] Unit tests added
- [ ] Manual testing done
- [ ] Integration tests pass

## Screenshots (if UI changes)
Before / After images

## Checklist
- [ ] Code follows style guidelines
- [ ] Tests pass locally
- [ ] Documentation updated
- [ ] No sensitive data exposed
```

### Good Example

```markdown
## Summary
Add rate limiting to the authentication API endpoints.

## Motivation
We've seen increased brute force attempts on login.
Relates to SEC-1234.

## Changes
- Added redis-based rate limiter middleware
- Limited login attempts to 5 per minute per IP
- Added 429 response with Retry-After header
- Added rate limit metrics to Prometheus

## Testing
- Unit tests for rate limiter logic
- Integration test for blocked requests
- Load tested with 100 req/s

## Rollback
Disable with `RATE_LIMIT_ENABLED=false` env var
```

## Giving Feedback

### Be Constructive

| Instead Of | Say |
|------------|-----|
| "This is wrong" | "Consider X approach because..." |
| "Why did you do this?" | "Help me understand the reasoning for..." |
| "This is confusing" | "Could we add a comment explaining...?" |
| "Bad naming" | "What about `calculateTotal` for clarity?" |

### Comment Types

```
# Blocking - must change
"This will cause a null pointer exception when X is empty."

# Suggestion - consider changing
"nit: Consider extracting this to a helper function."

# Question - seeking understanding
"Question: Why is this timeout set to 30s? Is that documented?"

# Praise - acknowledge good work
"Nice! This is a clean solution to a tricky problem."

# FYI - informational
"FYI: We have a similar utility in `src/utils/dates.ts`"
```

### Use Conventional Comments

```
# Prefix with category
nitpick: Trailing whitespace on line 42.
suggestion: We could use Optional.ofNullable() here.
issue: This doesn't handle the case where user is null.
question: Is this timeout value from requirements?
thought: Wonder if we should add caching here in the future.
praise: Great test coverage!
```

## Receiving Feedback

### Best Practices

| Do | Don't |
|----|-------|
| Assume good intent | Take criticism personally |
| Ask clarifying questions | Get defensive |
| Thank reviewers | Ignore comments |
| Explain your reasoning | Dismiss without consideration |
| Accept you might be wrong | Dig in on every point |

### Responding to Comments

```
# Agree and fix
"Good catch! Fixed in abc123."

# Discuss
"I considered that approach but chose X because of Y.
What do you think?"

# Defer
"That's a good point but out of scope.
Created issue #456 to track."

# Push back (respectfully)
"I think the current approach is better because...
but happy to discuss more."
```

## Review Size

### Keep PRs Small

| Size | Lines Changed | Review Time |
|------|---------------|-------------|
| **XS** | < 50 | 5-10 min |
| **S** | 50-200 | 15-30 min |
| **M** | 200-500 | 30-60 min |
| **L** | 500-1000 | 1-2 hours |
| **XL** | > 1000 | Consider splitting |

### Why Small PRs

- Faster reviews
- Better feedback
- Easier to understand
- Lower risk
- Quicker deployment
- Easier to revert

## Automation

### Pre-Review Checks

```yaml
# GitHub Actions example
name: PR Checks
on: pull_request

jobs:
  checks:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Lint
        run: npm run lint

      - name: Type Check
        run: npm run typecheck

      - name: Tests
        run: npm test

      - name: Security Scan
        run: npm audit
```

### Code Owners

```
# CODEOWNERS file
# Auto-assign reviewers

*.js @frontend-team
*.go @backend-team
/docs/ @tech-writers
/security/ @security-team
*.tf @devops-team
```

## Review Efficiency

### For Reviewers

| Practice | Benefit |
|----------|---------|
| **Review promptly** | Unblock teammates |
| **Set aside focused time** | Better quality review |
| **Review in passes** | High-level first, details second |
| **Use checklists** | Don't miss common issues |
| **Trust but verify** | Run the code locally |

### For Authors

| Practice | Benefit |
|----------|---------|
| **Self-review first** | Catch obvious issues |
| **Small, focused PRs** | Easier to review |
| **Good descriptions** | Context for reviewers |
| **Respond quickly** | Keep momentum |
| **Pre-flight checks** | Don't waste reviewer time |

## Common Anti-Patterns

| Anti-Pattern | Problem |
|--------------|---------|
| **Rubber-stamping** | Approving without reading |
| **Nitpick overload** | Focusing only on style |
| **Blocking on opinions** | Preferences vs requirements |
| **Review by committee** | Too many reviewers |
| **Review avoidance** | PRs sitting for days |
| **Gatekeeping** | Using reviews as power |

## Metrics

| Metric | Target | Why |
|--------|--------|-----|
| **Review turnaround** | < 24 hours | Avoid blocking |
| **PR size** | < 400 LOC | Better reviews |
| **Review cycles** | < 3 | Avoid thrashing |
| **Coverage** | 100% of PRs | No exceptions |

## Tools & Features

| Feature | GitHub | GitLab | Bitbucket |
|---------|--------|--------|-----------|
| Inline comments | ✅ | ✅ | ✅ |
| Suggestions | ✅ | ✅ | ✅ |
| Required reviewers | ✅ | ✅ | ✅ |
| Code owners | ✅ | ✅ | ✅ |
| Auto-merge | ✅ | ✅ | ✅ |
| Draft PRs | ✅ | ✅ | ✅ |

## Related

- [[Git Internals]] — Version control
- [[Testing Frameworks]] — Automated testing
- [[Computer Science MOC]] — CS topics
