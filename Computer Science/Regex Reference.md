---
title: Regex Reference
aliases:
  - Regular Expressions
  - Regex
  - RegExp
tags:
  - cs
  - reference
  - patterns
  - text-processing
type: reference
status: complete
created: "2025-12-16"
---

# Regex Reference

Comprehensive reference for regular expression syntax and patterns.

## Basic Syntax

### Literals

| Pattern | Matches |
|---------|---------|
| `abc` | Literal "abc" |
| `123` | Literal "123" |

### Metacharacters

| Char | Meaning | Example |
|------|---------|---------|
| `.` | Any character (except newline) | `a.c` → "abc", "a1c" |
| `^` | Start of string/line | `^Hello` |
| `$` | End of string/line | `world$` |
| `\|` | Alternation (OR) | `cat\|dog` |
| `\` | Escape metacharacter | `\.` matches "." |

## Character Classes

| Pattern | Matches |
|---------|---------|
| `[abc]` | Any of a, b, c |
| `[^abc]` | Not a, b, or c |
| `[a-z]` | Any lowercase letter |
| `[A-Z]` | Any uppercase letter |
| `[0-9]` | Any digit |
| `[a-zA-Z0-9]` | Any alphanumeric |

### Shorthand Classes

| Pattern | Equivalent | Meaning |
|---------|------------|---------|
| `\d` | `[0-9]` | Digit |
| `\D` | `[^0-9]` | Non-digit |
| `\w` | `[a-zA-Z0-9_]` | Word character |
| `\W` | `[^a-zA-Z0-9_]` | Non-word character |
| `\s` | `[ \t\n\r\f]` | Whitespace |
| `\S` | `[^ \t\n\r\f]` | Non-whitespace |

### POSIX Classes

| Pattern | Meaning |
|---------|---------|
| `[:alpha:]` | Letters |
| `[:digit:]` | Digits |
| `[:alnum:]` | Alphanumeric |
| `[:space:]` | Whitespace |
| `[:upper:]` | Uppercase |
| `[:lower:]` | Lowercase |

## Quantifiers

| Pattern | Meaning | Example |
|---------|---------|---------|
| `*` | 0 or more | `ab*c` → "ac", "abc", "abbc" |
| `+` | 1 or more | `ab+c` → "abc", "abbc" (not "ac") |
| `?` | 0 or 1 | `colou?r` → "color", "colour" |
| `{n}` | Exactly n | `\d{4}` → "2024" |
| `{n,}` | n or more | `\d{2,}` → "12", "123", "1234" |
| `{n,m}` | Between n and m | `\d{2,4}` → "12", "123", "1234" |

### Greedy vs Lazy

| Greedy | Lazy | Behavior |
|--------|------|----------|
| `*` | `*?` | Match as few as possible |
| `+` | `+?` | Match as few as possible |
| `?` | `??` | Match as few as possible |
| `{n,m}` | `{n,m}?` | Match as few as possible |

```
String: <div>content</div>
Greedy: <.*>   → "<div>content</div>"
Lazy:   <.*?>  → "<div>"
```

## Groups and Capturing

### Capturing Groups

```regex
(\d{4})-(\d{2})-(\d{2})
```

| Group | Content |
|-------|---------|
| `$0` or `\0` | Entire match |
| `$1` or `\1` | First group (year) |
| `$2` or `\2` | Second group (month) |
| `$3` or `\3` | Third group (day) |

### Non-Capturing Groups

```regex
(?:abc)+
```

Groups without capturing (for quantifiers).

### Named Groups

```regex
(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})
```

Access by name: `$+{year}`, `\k<year>`

### Backreferences

```regex
# Match repeated words
\b(\w+)\s+\1\b
# Matches: "the the", "is is"
```

## Anchors

| Pattern | Meaning |
|---------|---------|
| `^` | Start of string (or line with `m` flag) |
| `$` | End of string (or line with `m` flag) |
| `\b` | Word boundary |
| `\B` | Non-word boundary |
| `\A` | Start of string (absolute) |
| `\Z` | End of string (absolute) |

### Word Boundary Examples

```regex
\bcat\b     # Matches "cat" not "category"
\bcat       # Matches "cat" and "category"
cat\b       # Matches "cat" and "tomcat"
```

## Lookahead and Lookbehind

### Lookahead

| Pattern | Meaning |
|---------|---------|
| `(?=...)` | Positive lookahead |
| `(?!...)` | Negative lookahead |

```regex
# Match "foo" followed by "bar"
foo(?=bar)        # Matches "foo" in "foobar"

# Match "foo" not followed by "bar"
foo(?!bar)        # Matches "foo" in "foobaz"
```

### Lookbehind

| Pattern | Meaning |
|---------|---------|
| `(?<=...)` | Positive lookbehind |
| `(?<!...)` | Negative lookbehind |

```regex
# Match "bar" preceded by "foo"
(?<=foo)bar       # Matches "bar" in "foobar"

# Match "bar" not preceded by "foo"
(?<!foo)bar       # Matches "bar" in "bazbar"
```

### Password Validation Example

```regex
^(?=.*[A-Z])(?=.*[a-z])(?=.*\d)(?=.*[@$!%*?&])[A-Za-z\d@$!%*?&]{8,}$

# Breakdown:
# (?=.*[A-Z])    - At least one uppercase
# (?=.*[a-z])    - At least one lowercase
# (?=.*\d)       - At least one digit
# (?=.*[@$!%*?&]) - At least one special char
# {8,}           - At least 8 characters
```

## Flags/Modifiers

| Flag | Name | Effect |
|------|------|--------|
| `i` | Case-insensitive | `a` matches "A" |
| `g` | Global | Find all matches |
| `m` | Multiline | `^` and `$` match line boundaries |
| `s` | Dotall | `.` matches newlines |
| `x` | Extended | Ignore whitespace, allow comments |
| `u` | Unicode | Full Unicode support |

## Common Patterns

### Email

```regex
^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$
```

### URL

```regex
https?://[^\s/$.?#].[^\s]*
```

### Phone Number (US)

```regex
^(\+1)?[-.\s]?\(?\d{3}\)?[-.\s]?\d{3}[-.\s]?\d{4}$
```

### IP Address (IPv4)

```regex
^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$
```

### Date (YYYY-MM-DD)

```regex
^\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$
```

### Credit Card

```regex
^(?:4[0-9]{12}(?:[0-9]{3})?          # Visa
  |5[1-5][0-9]{14}                    # MasterCard
  |3[47][0-9]{13}                     # Amex
  |6(?:011|5[0-9]{2})[0-9]{12})$      # Discover
```

### HTML Tag

```regex
<([a-z]+)([^>]*)>(.*?)</\1>
```

### Slug

```regex
^[a-z0-9]+(?:-[a-z0-9]+)*$
```

## Language-Specific

### JavaScript

```javascript
const regex = /\d{3}-\d{4}/g;
const regex2 = new RegExp('\\d{3}-\\d{4}', 'g');

'123-4567'.match(regex);           // ['123-4567']
regex.test('123-4567');            // true
'123-4567'.replace(regex, 'XXX');  // 'XXX'
```

### Python

```python
import re

pattern = r'\d{3}-\d{4}'
re.search(pattern, '123-4567')     # Match object
re.findall(pattern, 'a 123-4567 b')  # ['123-4567']
re.sub(pattern, 'XXX', '123-4567')   # 'XXX'

# Compiled regex
compiled = re.compile(pattern)
compiled.search('123-4567')
```

### Go

```go
import "regexp"

re := regexp.MustCompile(`\d{3}-\d{4}`)
re.MatchString("123-4567")           // true
re.FindString("a 123-4567 b")        // "123-4567"
re.ReplaceAllString("123-4567", "X") // "X"
```

### Rust

```rust
use regex::Regex;

let re = Regex::new(r"\d{3}-\d{4}").unwrap();
re.is_match("123-4567");              // true
re.find("a 123-4567 b");              // Some(Match)
re.replace("123-4567", "XXX");        // "XXX"
```

## Performance Tips

| Tip | Rationale |
|-----|-----------|
| **Anchor when possible** | `^abc` faster than `abc` |
| **Be specific** | `\d{3}` faster than `\d+` for 3 digits |
| **Avoid catastrophic backtracking** | `(a+)+b` on "aaaaaaaaac" |
| **Use non-capturing groups** | `(?:abc)` when you don't need capture |
| **Compile once, use many** | Don't recreate regex in loops |
| **Use character classes** | `[aeiou]` faster than `a\|e\|i\|o\|u` |

### Catastrophic Backtracking

```regex
# Bad - exponential backtracking on failure
(a+)+$

# Better - possessive or atomic groups
(?>a+)+$   # Atomic group
a++$       # Possessive quantifier
```

## Related

- [[Testing Frameworks]] — Regex testing
- [[Computer Science MOC]] — All CS topics
