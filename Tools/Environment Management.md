---
title: Environment Management
aliases:
  - Environment Variables
  - direnv
  - dotenv
tags:
  - tool
  - comparison
  - development
type: comparison
status: complete
created: "2025-12-04"
---

# Environment Management

Tools for managing environment variables, project settings, and development environments.

## Overview

| Tool | Type | Shell Integration | Language Agnostic |
|------|------|-------------------|-------------------|
| direnv | Directory-based env | ✅ | ✅ |
| dotenv | File loader | Library per language | Libraries |
| mise | Polyglot env + versions | ✅ | ✅ |
| asdf-direnv | asdf + direnv integration | ✅ | ✅ |
| envchain | Secure env storage | ✅ | ✅ |
| 1Password CLI | Secrets manager | ✅ | ✅ |

---

## direnv

Automatically load/unload environment variables per directory.

### How It Works

```
~/projects/app1/          ~/projects/app2/
├── .envrc                 ├── .envrc
│   DATABASE_URL=...       │   DATABASE_URL=...
│   API_KEY=...            │   API_KEY=...
│                          │
cd app1 → loads app1 env   cd app2 → loads app2 env
cd ..   → unloads env      cd ..   → unloads env
```

### Installation

```bash
# macOS
brew install direnv

# Ubuntu
sudo apt install direnv

# Shell setup
# Bash (~/.bashrc)
eval "$(direnv hook bash)"

# Zsh (~/.zshrc)
eval "$(direnv hook zsh)"

# Fish (~/.config/fish/config.fish)
direnv hook fish | source
```

### Basic Usage

```bash
# Create .envrc
echo 'export DATABASE_URL="postgres://localhost/dev"' > .envrc

# Allow the file (security feature)
direnv allow

# Now entering directory loads variables
cd myproject
echo $DATABASE_URL  # postgres://localhost/dev

# Leaving unloads them
cd ..
echo $DATABASE_URL  # (empty)
```

### .envrc Examples

```bash
# .envrc

# Simple exports
export DATABASE_URL="postgres://localhost/dev"
export API_KEY="dev-key-123"
export DEBUG=true

# Load from .env file
dotenv

# Load from .env if exists
dotenv_if_exists

# Load from specific file
dotenv .env.local

# Use PATH_add for local binaries
PATH_add bin
PATH_add node_modules/.bin

# Use layout for language-specific setup
layout python3       # Creates/activates venv
layout node          # Sets up node_modules/.bin
layout ruby          # Sets up bundle paths

# Source another file
source_env .envrc.local

# Use direnv's stdlib
use nix              # Load nix-shell
use flake            # Load nix flake
```

### Security

```bash
# .envrc files must be explicitly allowed
direnv allow .

# Deny a file
direnv deny .

# Edit and re-allow
direnv edit .

# Block patterns in ~/.config/direnv/direnv.toml
[global]
load_dotenv = false
```

### With mise/asdf

```bash
# .envrc - integrate with mise
use mise

# Or with asdf
use asdf
```

---

## dotenv (.env files)

Standard for storing environment variables in files.

### Format

```bash
# .env
DATABASE_URL=postgres://localhost/dev
API_KEY=secret123
DEBUG=true

# Quoted values (preserves spaces)
MESSAGE="Hello World"
MULTILINE="Line 1\nLine 2"

# Comments
# This is a comment

# Variable expansion (some implementations)
BASE_URL=https://api.example.com
USERS_URL=${BASE_URL}/users
```

### File Hierarchy

```
project/
├── .env                 # Shared defaults (commit this)
├── .env.local           # Local overrides (gitignore)
├── .env.development     # Development settings
├── .env.production      # Production settings
├── .env.test            # Test settings
└── .env.example         # Template (commit this)
```

### Language Libraries

#### JavaScript/Node.js

```javascript
// npm install dotenv
require('dotenv').config();

// Or ES modules
import 'dotenv/config';

// Access
console.log(process.env.DATABASE_URL);

// With options
require('dotenv').config({
  path: '.env.local',
  override: true
});
```

#### Python

```python
# pip install python-dotenv

from dotenv import load_dotenv
import os

load_dotenv()  # Loads .env
# or
load_dotenv('.env.local')

database_url = os.getenv('DATABASE_URL')

# Auto-load in Flask
# FLASK_DEBUG=1 in .env works automatically
```

#### Ruby

```ruby
# gem install dotenv

require 'dotenv/load'  # Auto-loads .env

# Or manually
require 'dotenv'
Dotenv.load('.env.local')

ENV['DATABASE_URL']
```

#### Go

```go
// go get github.com/joho/godotenv

import "github.com/joho/godotenv"

func init() {
    godotenv.Load()  // Loads .env
    // or
    godotenv.Load(".env.local")
}

os.Getenv("DATABASE_URL")
```

### Security Best Practices

```bash
# .gitignore
.env.local
.env.*.local
.env.development.local
.env.production.local

# Never commit actual secrets
# Use .env.example as template
```

---

## mise for Environment

mise handles both versions and environment.

### Environment Features

```toml
# mise.toml or .mise.toml

[env]
DATABASE_URL = "postgres://localhost/dev"
API_KEY = "dev-key"

# From file
_.file = ".env"

# Path manipulation
_.path = ["./bin", "./node_modules/.bin"]

# Conditional
[env.development]
DEBUG = "true"

[env.production]
DEBUG = "false"
```

### With Tools

```toml
# .mise.toml
[tools]
node = "20"
python = "3.12"

[env]
NODE_ENV = "development"
PYTHONPATH = "./src"
```

### Tasks

```toml
# .mise.toml
[tasks.dev]
run = "npm run dev"
env = { DEBUG = "true" }

[tasks.test]
run = "pytest"
env = { TESTING = "true" }
```

```bash
mise run dev
mise run test
```

---

## Secret Management

### envchain (macOS Keychain)

Store secrets in system keychain.

```bash
# Install
brew install envchain

# Store secret
envchain --set myproject AWS_ACCESS_KEY_ID
# (prompts for value, stores in Keychain)

envchain --set myproject AWS_SECRET_ACCESS_KEY

# Run command with secrets
envchain myproject aws s3 ls

# List namespaces
envchain --list
```

### 1Password CLI

```bash
# Install
brew install 1password-cli

# Sign in
op signin

# Reference in .env
DATABASE_URL="op://Vault/Database/password"

# Run with secrets injected
op run -- npm start

# In scripts
export API_KEY=$(op read "op://Vault/API/key")
```

### AWS Secrets Manager / Parameter Store

```bash
# Fetch at runtime
export DB_PASSWORD=$(aws ssm get-parameter \
  --name /myapp/db-password \
  --with-decryption \
  --query Parameter.Value \
  --output text)
```

### HashiCorp Vault

```bash
# Login
vault login

# Read secret
vault kv get -field=password secret/myapp/db

# In app
export DB_PASSWORD=$(vault kv get -field=password secret/myapp/db)
```

---

## Framework Integration

### Next.js

```bash
# Automatically loads:
.env                # All environments
.env.local          # Local (gitignored)
.env.development    # Development only
.env.production     # Production only
.env.development.local  # Local dev overrides
```

```javascript
// Must prefix with NEXT_PUBLIC_ for browser
NEXT_PUBLIC_API_URL=https://api.example.com
SECRET_KEY=server-only  // Only available server-side
```

### Vite

```bash
# Automatically loads based on mode
.env
.env.local
.env.[mode]
.env.[mode].local
```

```bash
# Must prefix with VITE_ for client
VITE_API_URL=https://api.example.com
SECRET=server-only  // Not exposed
```

### Rails

```ruby
# Uses credentials or dotenv-rails gem
# config/credentials.yml.enc (encrypted)

# With dotenv-rails gem:
# Loads in order:
# .env
# .env.development / .env.test / .env.production
# .env.local
# .env.development.local / etc.
```

### Django

```python
# settings.py with django-environ
import environ

env = environ.Env()
environ.Env.read_env()  # Loads .env

DATABASES = {
    'default': env.db('DATABASE_URL')
}
SECRET_KEY = env('SECRET_KEY')
DEBUG = env.bool('DEBUG', default=False)
```

---

## Comparison

### direnv vs dotenv vs mise

| Aspect | direnv | dotenv | mise |
|--------|--------|--------|------|
| Type | Shell hook | Library | Shell hook |
| Automatic | ✅ On cd | ❌ Manual load | ✅ On cd |
| Language | Any | Per language | Any |
| Version management | ❌ | ❌ | ✅ |
| Shell functions | ✅ | ❌ | ✅ |
| Task running | ❌ | ❌ | ✅ |

### When to Use What

| Scenario | Tool |
|----------|------|
| Quick env per project | direnv |
| App configuration | dotenv library |
| Polyglot + versions | mise |
| Secrets | envchain, 1Password, Vault |
| Team standardization | mise or direnv + dotenv |

---

## Best Practices

### Project Setup

```bash
# Recommended structure
project/
├── .envrc              # direnv (loads .env, sets PATH)
├── .env                # Shared defaults (safe to commit)
├── .env.local          # Local secrets (gitignored)
├── .env.example        # Template for new devs
├── .mise.toml          # Tool versions + env
└── .gitignore          # Ignore .env.local, .env.*.local
```

### .envrc Template

```bash
# .envrc
# Load .env file
dotenv_if_exists

# Load local overrides
dotenv_if_exists .env.local

# Add local binaries to PATH
PATH_add bin
PATH_add node_modules/.bin

# Use mise for tool versions
use mise

# Project-specific vars
export PROJECT_NAME="myapp"
```

### .gitignore

```bash
# .gitignore

# Local environment files
.env.local
.env.*.local

# But keep templates
!.env.example

# direnv
.direnv/
```

---

## Related

- [[Version Managers]]
- [[Shells]]
- [[Remote Development]]
