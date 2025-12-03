# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

An Obsidian vault serving as a comprehensive developer reference. **Not a codebase** - this is a knowledge base of markdown files with Obsidian features (dataview queries, wikilinks, frontmatter).

## Structure

```
/                           # Root - MOCs and tool comparisons
├── Home.md                 # Main index
├── *MOC.md                 # Maps of Content (Languages, Tools, CS, Math, ML, Domains)
├── cs/                     # CS concepts (Big O, algorithms, patterns)
├── math/                   # Math reference (calculus, linear algebra, probability)
├── ml/                     # ML/AI (fundamentals, neural networks, LLMs)
├── languages/              # Language pages (Java, Kotlin, Swift, C++, etc.)
├── frameworks/             # Framework/engine pages (React, Django, Unity, etc.)
├── domains/                # Domain MOCs (Web, Mobile, Desktop, Game, Embedded)
├── tools/                  # Tool pages (Database Engines, Prometheus, Grafana)
├── docs/                   # Project docs (ROADMAP.md)
└── templates/              # Obsidian templates for new pages
```

## Frontmatter Requirements

Every page MUST have:

```yaml
---
title: Page Title
aliases:
  - Alternative Name
tags:
  - category      # framework, tool, language, concept, comparison
  - domain        # web, mobile, desktop, game, etc.
  - languages     # csharp, python, rust, typescript, etc.
type: reference | comparison | concept | moc
status: draft | in-progress | complete
created: YYYY-MM-DD
---
```

## Writing Style

**DO:**

- Start with one-line description after H1
- Use overview tables (Aspect | Details)
- Include comparison tables between alternatives
- Add "When to Use" and decision guidance
- Use `[[wikilinks]]` for internal cross-references
- Use Mermaid for architecture diagrams
- Keep code examples minimal and illustrative

**DON'T:**

- Write walls of code or full tutorials
- Use emojis in prose (✅ ❌ in tables only)
- Duplicate information across pages
- Explain basic syntax

## Page Patterns

**Framework/Tool pages follow:**

1. Overview table (Type, Language, Platforms, Backing, etc.)
2. Core concepts with brief explanations
3. Key features/APIs
4. Comparison with alternatives table
5. "When to Use" section (Strengths, Considerations, Best for)
6. Related links section

**Comparison pages follow:**

1. Overview table across options
2. Feature matrix
3. Per-option details
4. Decision guide table
5. Related links

## File Naming

**Title Case with Spaces** — Obsidian-native convention.

- `Big O Notation.md` not `big-o-notation.md`
- `React Native.md` not `ReactNative.md`
- MOCs: `{Name} MOC.md` (e.g., `Languages MOC.md`)

## Key Files

- `docs/ROADMAP.md` - Planned and completed content
- `templates/comparison-template.md` - Template for new comparisons
- `templates/concept-template.md` - Template for concept pages

## Content Scope

**Languages:** C#, Go, Python, TypeScript, Rust, Java, Kotlin, Swift, C++, Dart, PHP, Ruby

**Domains:** Web, Mobile, Desktop, Game, Embedded & Systems

**Frameworks:** Frontend (React, Vue, Angular, Svelte), Meta (Next.js, Nuxt, SvelteKit, Remix, Astro), Backend (Spring Boot, Rails, Laravel, Django), Mobile (Flutter, React Native, MAUI), Desktop (Electron, Tauri, Avalonia), Game (Unity, Godot, Bevy)

**Tools:** Testing, Logging, HTTP clients, ORMs, Build systems, Deployment, Databases, Observability (Prometheus, Grafana, OpenTelemetry)

## Validation

Before committing changes, run validation:

```bash
npm run validate          # Lint + frontmatter + wikilinks
npm run lint:fix          # Auto-fix markdown issues
```

### Validation Scripts

| Command | What it checks |
|---------|----------------|
| `npm run lint` | Markdown formatting (markdownlint) |
| `npm run lint:fix` | Auto-fix markdown issues |
| `npm run validate:frontmatter` | Required frontmatter fields |
| `npm run validate:links` | Wikilinks resolve to files |
| `npm run spell` | Spelling (cspell) |

### Pre-commit Requirements

All commits must pass:

1. **Markdown lint** — Formatting consistency
2. **Frontmatter validation** — Required fields present and valid

Wikilinks and spelling are checked but don't block commits (planned content is allowed in MOC files).
