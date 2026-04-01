---
title: Technical Writing
aliases:
  - Technical Documentation
  - Developer Documentation
  - Tech Writing
tags:
  - cs
  - practices
  - documentation
  - communication
type: reference
status: complete
created: "2025-12-18"
---

# Technical Writing

Creating clear, accurate documentation for technical audiences.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Enable users to accomplish tasks |
| **Audience** | Developers, users, operators |
| **Formats** | Docs, READMEs, API refs, tutorials |
| **Key Principle** | Clarity over cleverness |

## Documentation Types

| Type | Purpose | Audience |
|------|---------|----------|
| **README** | Project overview, quick start | New users |
| **Tutorial** | Learning-oriented, step-by-step | Beginners |
| **How-to Guide** | Task-oriented, solve problems | Practitioners |
| **Reference** | Information-oriented, complete | Experienced users |
| **Explanation** | Understanding-oriented, concepts | All levels |
| **API Reference** | Endpoint/function details | Developers |
| **Architecture** | System design decisions | Engineers |

## The Four Quadrants (Diátaxis)

```
                    PRACTICAL
                        │
    ┌───────────────────┼───────────────────┐
    │                   │                   │
    │    TUTORIALS      │    HOW-TO GUIDES  │
    │    (Learning)     │    (Goals)        │
    │                   │                   │
    │ "Follow along     │ "How do I...?"    │
STUDY │  to learn"       │                   │ WORK
    │                   │                   │
    ├───────────────────┼───────────────────┤
    │                   │                   │
    │   EXPLANATION     │    REFERENCE      │
    │   (Understanding) │    (Information)  │
    │                   │                   │
    │ "Why does this    │ "What are the     │
    │  work this way?"  │  options?"        │
    └───────────────────┼───────────────────┘
                        │
                   THEORETICAL
```

## Writing Principles

### Clarity

| Do | Don't |
|----|-------|
| Use simple words | Use jargon unnecessarily |
| Short sentences | Run-on sentences |
| Active voice | Passive voice |
| One idea per sentence | Pack multiple concepts |
| Define acronyms first | Assume knowledge |

### Examples

```markdown
# Bad
The API endpoint, when invoked with the appropriate parameters
that have been correctly formatted according to the specification,
will return a response object containing the requested data.

# Good
Call the `/users` endpoint with a user ID.
The API returns the user's profile data as JSON.
```

### Voice and Tone

| Use | Avoid |
|-----|-------|
| Direct address ("you") | Third person ("the user") |
| Present tense | Future tense |
| Imperative mood | Suggestions |
| Confident | Hedging |

```markdown
# Bad
The user might want to click the button, which will probably
start the process.

# Good
Click **Start** to begin the process.
```

## README Structure

```markdown
# Project Name

One-line description of what this project does.

## Features

- Feature 1
- Feature 2

## Quick Start

\`\`\`bash
npm install myproject
myproject init
\`\`\`

## Installation

### Prerequisites
- Node.js 18+
- npm or yarn

### Steps
1. Clone the repository
2. Install dependencies
3. Configure environment

## Usage

Basic usage example with code.

## Configuration

| Option | Default | Description |
|--------|---------|-------------|
| `port` | 3000 | Server port |

## API

Brief API overview or link to full docs.

## Contributing

How to contribute to the project.

## License

MIT License
```

## API Documentation

### Endpoint Documentation

```markdown
## Create User

Creates a new user account.

### Request

\`\`\`
POST /api/v1/users
\`\`\`

**Headers**
| Header | Required | Description |
|--------|----------|-------------|
| Authorization | Yes | Bearer token |
| Content-Type | Yes | application/json |

**Body**
\`\`\`json
{
  "email": "user@example.com",
  "name": "Alice Smith",
  "role": "member"
}
\`\`\`

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| email | string | Yes | Valid email address |
| name | string | Yes | Display name |
| role | string | No | User role (default: member) |

### Response

**Success (201)**
\`\`\`json
{
  "id": "user_123",
  "email": "user@example.com",
  "name": "Alice Smith",
  "createdAt": "2024-01-15T10:30:00Z"
}
\`\`\`

**Error (400)**
\`\`\`json
{
  "error": "validation_error",
  "message": "Email already exists"
}
\`\`\`

### Example

\`\`\`bash
curl -X POST https://api.example.com/api/v1/users \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"email": "user@example.com", "name": "Alice"}'
\`\`\`
```

## Code Examples

### Good Code Examples

```markdown
# Good: Complete, runnable example
\`\`\`python
from mylib import Client

# Initialize the client
client = Client(api_key="your-api-key")

# Fetch user data
user = client.get_user(user_id=123)
print(f"Hello, {user.name}")
\`\`\`

# Bad: Incomplete, assumes context
\`\`\`python
user = client.get_user(123)
\`\`\`
```

### Progressive Complexity

```markdown
## Basic Usage

\`\`\`python
# Simple example - most common use case
result = search("query")
\`\`\`

## Advanced Usage

\`\`\`python
# With all options
result = search(
    query="query",
    filters={"date": "2024-01"},
    sort="relevance",
    limit=10,
    include_metadata=True
)
\`\`\`
```

## Formatting Best Practices

### Use Tables for Options

```markdown
| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `timeout` | number | 30 | Request timeout in seconds |
| `retries` | number | 3 | Number of retry attempts |
| `debug` | boolean | false | Enable debug logging |
```

### Use Admonitions

```markdown
> **Note**: This feature requires version 2.0+

> **Warning**: This action cannot be undone.

> **Tip**: Use tab completion for faster input.

> **Important**: Back up your data before proceeding.
```

### Use Lists for Steps

```markdown
## Installation

1. Download the installer
2. Run the setup wizard
3. Accept the license agreement
4. Choose installation directory
5. Click **Install**
```

## Error Messages

### Document Error Codes

```markdown
## Error Codes

| Code | Message | Solution |
|------|---------|----------|
| E001 | Invalid API key | Check your key in Settings |
| E002 | Rate limit exceeded | Wait 60s and retry |
| E003 | Resource not found | Verify the ID exists |
```

### Troubleshooting Sections

```markdown
## Troubleshooting

### Connection refused

**Symptom**: `Error: ECONNREFUSED`

**Cause**: The server isn't running or is unreachable.

**Solution**:
1. Verify the server is running: `systemctl status myapp`
2. Check the port is correct in config
3. Ensure firewall allows the port
```

## Versioning Documentation

### Version Notices

```markdown
> **Version**: This feature is available in v2.0+

> **Deprecated**: This method is deprecated.
> Use `newMethod()` instead.

> **Breaking Change** (v3.0): The `oldParam` parameter
> has been renamed to `newParam`.
```

### Changelog Format

```markdown
## Changelog

### [2.1.0] - 2024-01-15

#### Added
- New export feature (#123)
- Support for custom themes

#### Changed
- Improved search performance

#### Fixed
- Bug where dates displayed incorrectly (#456)

#### Deprecated
- `oldMethod()` - use `newMethod()` instead
```

## Tools

| Tool | Purpose |
|------|---------|
| **Docusaurus** | Documentation sites (React) |
| **MkDocs** | Documentation sites (Python) |
| **Sphinx** | Python documentation |
| **GitBook** | Hosted documentation |
| **ReadTheDocs** | Documentation hosting |
| **Swagger/OpenAPI** | API documentation |
| **JSDoc/TSDoc** | JS/TS code documentation |
| **Vale** | Prose linting |

## Documentation as Code

```yaml
# .github/workflows/docs.yml
name: Build Docs
on:
  push:
    paths:
      - 'docs/**'

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Build docs
        run: npm run docs:build
      - name: Deploy
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./docs/build
```

## Measuring Documentation

| Metric | How to Measure |
|--------|----------------|
| **Completeness** | Coverage of features |
| **Freshness** | Last updated date |
| **Findability** | Search analytics |
| **Usefulness** | User feedback, ratings |
| **Accuracy** | Bug reports, complaints |

## Common Mistakes

| Mistake | Better Approach |
|---------|-----------------|
| Assuming knowledge | Define terms, link to prerequisites |
| Wall of text | Use headings, lists, whitespace |
| Outdated examples | Test examples in CI |
| No examples | Every concept needs an example |
| Only happy path | Document errors and edge cases |
| Too much detail upfront | Progressive disclosure |

## Related

- [[Code Review]] — Review documentation too
- [[Computer Science MOC]] — CS topics
