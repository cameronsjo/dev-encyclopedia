---
title: Git Internals
aliases:
  - How Git Works
  - Git Objects
  - Git Architecture
tags:
  - cs
  - git
  - version-control
  - internals
type: concept
status: complete
created: "2025-12-16"
---

# Git Internals

Understanding how Git stores and manages version history under the hood.

## Overview

Git is fundamentally a content-addressable filesystem with a VCS built on top.

```
.git/
├── HEAD              # Current branch pointer
├── config            # Repository config
├── index             # Staging area
├── objects/          # All content (blobs, trees, commits)
│   ├── ab/
│   │   └── cdef...   # Object files
│   └── pack/         # Packed objects
├── refs/             # Branch and tag pointers
│   ├── heads/
│   │   └── main
│   └── tags/
└── logs/             # Reflog history
```

## Git Objects

### Four Object Types

| Type | Purpose | Content |
|------|---------|---------|
| **Blob** | File content | Raw file data |
| **Tree** | Directory | List of blobs and trees |
| **Commit** | Snapshot | Tree + metadata + parent(s) |
| **Tag** | Named reference | Points to commit |

### Object Storage

**All objects stored by SHA-1 hash of content.**

```bash
# Examine object type
git cat-file -t abc123

# View object content
git cat-file -p abc123

# Compute hash without storing
echo "hello" | git hash-object --stdin
```

### Blob Object

**Stores file content only (no filename).**

```bash
# Create a blob
echo "Hello, World!" | git hash-object -w --stdin
# Returns: af5626b4a114abcb82d63db7c8082c3c4756e51b

# View blob content
git cat-file -p af5626b
# Output: Hello, World!
```

### Tree Object

**Represents a directory.**

```bash
git cat-file -p HEAD^{tree}

# Output:
100644 blob abc123...  README.md
100644 blob def456...  package.json
040000 tree 789abc...  src
```

| Mode | Type |
|------|------|
| `100644` | Regular file |
| `100755` | Executable file |
| `120000` | Symbolic link |
| `040000` | Directory (tree) |

### Commit Object

```bash
git cat-file -p HEAD

# Output:
tree 4b825dc...
parent a1b2c3d...
author Alice <alice@example.com> 1704063600 -0500
committer Alice <alice@example.com> 1704063600 -0500

Add new feature
```

| Field | Description |
|-------|-------------|
| `tree` | Root tree SHA |
| `parent` | Parent commit(s) SHA |
| `author` | Who wrote the code |
| `committer` | Who made the commit |
| `message` | Commit message |

## Object Relationships

```
Commit (abc123)
    │
    └─► Tree (root)
           │
           ├─► Blob (README.md)
           ├─► Blob (package.json)
           └─► Tree (src/)
                  │
                  ├─► Blob (index.js)
                  └─► Blob (utils.js)
```

## References (Refs)

### Branch = Pointer to Commit

```bash
cat .git/refs/heads/main
# Output: abc123def456...  (commit SHA)

# Branches are just files containing SHAs!
```

### HEAD = Current Branch

```bash
cat .git/HEAD
# Output: ref: refs/heads/main

# Detached HEAD (points directly to commit):
# abc123def456...
```

### Reference Types

| Ref | Location | Purpose |
|-----|----------|---------|
| **Branch** | `refs/heads/` | Movable commit pointer |
| **Tag** | `refs/tags/` | Fixed commit pointer |
| **Remote** | `refs/remotes/` | Remote tracking branches |
| **HEAD** | `.git/HEAD` | Current position |

## The Index (Staging Area)

```bash
# View index contents
git ls-files --stage

# Output:
100644 abc123... 0    README.md
100644 def456... 0    src/index.js

# Index is a binary file
file .git/index
# Output: Git index, version 2, 15 entries
```

### Index States

```
Working Directory    Index (Stage)    Repository
      │                   │                │
      │  git add file     │                │
      ├──────────────────►│                │
      │                   │  git commit    │
      │                   ├───────────────►│
      │                   │                │
```

## Content Addressing

### SHA-1 Hashing

**Everything in Git is content-addressable:**

```python
# How Git computes blob hash
import hashlib

content = b"Hello, World!"
header = f"blob {len(content)}\0".encode()
store = header + content

sha1 = hashlib.sha1(store).hexdigest()
# af5626b4a114abcb82d63db7c8082c3c4756e51b
```

### Why SHA-1?

| Benefit | Description |
|---------|-------------|
| **Integrity** | Any corruption changes hash |
| **Deduplication** | Same content = same hash |
| **Distributed** | No central authority needed |
| **Immutable** | Can't change without new hash |

## Pack Files

### Object Compression

```bash
# Trigger packing
git gc

# View pack contents
git verify-pack -v .git/objects/pack/pack-*.idx
```

### Delta Compression

```
Object A (1000 bytes) ─────────────────────► Stored fully
                                              │
Object B (1005 bytes) ─► Delta from A (50 bytes)
                                              │
Object C (1010 bytes) ─► Delta from B (55 bytes)
```

Git stores deltas between similar objects to save space.

## Common Operations Explained

### git add

```bash
git add file.txt

# Internally:
# 1. Hash file content → blob SHA
# 2. Store blob in .git/objects/
# 3. Update index with (mode, SHA, path)
```

### git commit

```bash
git commit -m "message"

# Internally:
# 1. Create tree from index
# 2. Create commit pointing to tree
# 3. Update HEAD's branch to new commit
```

### git branch

```bash
git branch feature

# Internally:
# Create file .git/refs/heads/feature
# Content: current HEAD commit SHA
```

### git merge (fast-forward)

```
Before:
main:    A ─ B ─ C
                  │
feature:          D ─ E

After (FF):
main:    A ─ B ─ C ─ D ─ E
                          │
feature:                  (same)

# Just moves main pointer to E
```

### git merge (three-way)

```
Before:
main:    A ─ B ─ C ─ F
                  │
feature:          D ─ E

After:
main:    A ─ B ─ C ─ F ─ G (merge commit)
                  │     /
feature:          D ─ E

# G has two parents: F and E
```

### git rebase

```
Before:
main:    A ─ B ─ C
              │
feature:      D ─ E

After:
main:    A ─ B ─ C
                  │
feature:          D' ─ E'

# D' and E' are NEW commits (different SHA)
# Same changes, different parent
```

## Reflog

**History of HEAD movements.**

```bash
git reflog

# Output:
abc123 HEAD@{0}: commit: Add feature
def456 HEAD@{1}: checkout: moving from main to feature
789abc HEAD@{2}: commit: Fix bug

# Recover "lost" commits
git checkout HEAD@{2}
```

## Porcelain vs Plumbing

| Porcelain (User) | Plumbing (Internal) |
|------------------|---------------------|
| `git add` | `git hash-object`, `git update-index` |
| `git commit` | `git write-tree`, `git commit-tree` |
| `git branch` | `git update-ref` |
| `git log` | `git rev-list`, `git cat-file` |

### Low-level Example

```bash
# Create commit manually (plumbing)
echo "Hello" | git hash-object -w --stdin       # Create blob
git update-index --add --cacheinfo 100644 <sha> hello.txt
git write-tree                                   # Create tree
git commit-tree <tree-sha> -m "Manual commit"   # Create commit
git update-ref refs/heads/main <commit-sha>     # Update branch
```

## Garbage Collection

```bash
# Run GC
git gc

# What it does:
# 1. Packs loose objects
# 2. Removes unreachable objects
# 3. Compresses pack files
# 4. Prunes reflog entries
```

### Unreachable Objects

```
Reachable: HEAD or any ref can reach it
Unreachable: No ref points to it (orphaned)

# Keep unreachable objects for 2 weeks (default)
git config gc.pruneExpire "2 weeks ago"
```

## Related

- [[Build Systems]] — Development tooling
- [[Computer Science MOC]] — All CS topics
