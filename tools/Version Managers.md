---
title: Version Managers
aliases:
  - Runtime Version Managers
  - Language Version Managers
tags:
  - tool
  - comparison
  - development
type: comparison
status: complete
created: "2025-12-04"
---

# Version Managers

Tools for managing multiple versions of programming languages and runtimes.

## Overview

| Tool | Languages | Shell Integration | Config File |
|------|-----------|-------------------|-------------|
| mise (rtx) | All (polyglot) | All shells | `.mise.toml`, `.tool-versions` |
| asdf | All (polyglot) | Bash, Zsh, Fish | `.tool-versions` |
| nvm | Node.js | Bash, Zsh | `.nvmrc` |
| fnm | Node.js | All shells | `.nvmrc`, `.node-version` |
| pyenv | Python | Bash, Zsh | `.python-version` |
| rbenv | Ruby | Bash, Zsh | `.ruby-version` |
| rustup | Rust | N/A (manages itself) | `rust-toolchain.toml` |
| sdkman | JVM (Java, Kotlin, etc.) | Bash, Zsh | — |

---

## Polyglot Managers

### mise (formerly rtx)

Modern, fast, polyglot. Written in Rust.

```bash
# Install
curl https://mise.run | sh
# or
brew install mise

# Shell setup (add to shell rc)
eval "$(mise activate bash)"   # or zsh, fish

# Install runtimes
mise install node@20           # Latest 20.x
mise install node@lts          # Latest LTS
mise install python@3.12
mise install go@latest

# Use globally
mise use -g node@20
mise use -g python@3.12

# Use in project (creates .mise.toml)
mise use node@20
mise use python@3.12

# List versions
mise list node                 # Installed versions
mise list-remote node          # Available versions

# Show current
mise current                   # All active versions
mise where node                # Path to current node

# Run commands
mise exec node@18 -- node -v   # Run with specific version
mise run build                 # Run task from mise.toml
```

```toml
# .mise.toml
[tools]
node = "20"
python = "3.12"
go = "1.22"

[env]
NODE_ENV = "development"

[tasks.build]
run = "npm run build"

[tasks.test]
run = "npm test"
```

### asdf

Plugin-based, community standard.

```bash
# Install
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.14.0

# Shell setup (add to shell rc)
. "$HOME/.asdf/asdf.sh"
. "$HOME/.asdf/completions/asdf.bash"

# Add plugins
asdf plugin add nodejs
asdf plugin add python
asdf plugin add golang

# Install versions
asdf install nodejs 20.10.0
asdf install nodejs latest
asdf install python 3.12.1

# Set versions
asdf global nodejs 20.10.0     # Global default
asdf local nodejs 20.10.0      # Project (creates .tool-versions)
asdf shell nodejs 18.0.0       # Current shell only

# List versions
asdf list nodejs               # Installed
asdf list all nodejs           # Available

# Update plugins
asdf plugin update --all
```

```
# .tool-versions
nodejs 20.10.0
python 3.12.1
golang 1.22.0
```

### mise vs asdf

| Aspect | mise | asdf |
|--------|------|------|
| Speed | Faster (Rust) | Slower (Shell) |
| Compatibility | asdf plugins + own | Plugins only |
| Config | `.mise.toml` or `.tool-versions` | `.tool-versions` |
| Tasks | Built-in | No |
| Env vars | Built-in | Via direnv |
| Shell startup | Faster | Slower |

---

## Node.js

### nvm (Node Version Manager)

The original. Most widely used.

```bash
# Install
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash

# Shell setup (automatic in install)
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# Install versions
nvm install 20                 # Latest 20.x
nvm install --lts              # Latest LTS
nvm install node               # Latest

# Use
nvm use 20                     # Switch version
nvm alias default 20           # Set default

# List
nvm list                       # Installed
nvm list-remote                # Available

# Run with version
nvm exec 18 node -v            # Run with specific version
nvm run 18 app.js              # Run script with version

# Auto-switch (add to shell rc)
autoload -U add-zsh-hook
load-nvmrc() {
  if [[ -f .nvmrc ]]; then
    nvm use
  fi
}
add-zsh-hook chpwd load-nvmrc
```

```
# .nvmrc
20
```

### fnm (Fast Node Manager)

Rust-based, faster alternative to nvm.

```bash
# Install
curl -fsSL https://fnm.vercel.app/install | bash
# or
brew install fnm

# Shell setup
eval "$(fnm env --use-on-cd)"  # Auto-switch on cd

# Install
fnm install 20
fnm install --lts

# Use
fnm use 20
fnm default 20                 # Set default

# List
fnm list                       # Installed
fnm list-remote                # Available
```

### n

Simple, no shell modifications needed.

```bash
# Install
npm install -g n

# Install versions
n 20                           # Install and switch
n lts                          # Latest LTS
n latest                       # Latest

# Interactive picker
n                              # Shows picker

# List
n ls                           # Installed
n ls-remote                    # Available
```

### Node Version Managers Compared

| Aspect | nvm | fnm | n |
|--------|-----|-----|---|
| Speed | Slow | Fast | Fast |
| Shell integration | Heavy | Light | None |
| Auto-switch | Via hook | Built-in | No |
| Windows | nvm-windows | Yes | No |

---

## Python

### pyenv

Standard Python version manager.

```bash
# Install
curl https://pyenv.run | bash
# or
brew install pyenv

# Shell setup
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# Install versions
pyenv install 3.12.1
pyenv install 3.11.7

# Set version
pyenv global 3.12.1            # Global default
pyenv local 3.11.7             # Project (.python-version)
pyenv shell 3.10.0             # Current shell

# List
pyenv versions                 # Installed
pyenv install --list           # Available

# With virtualenv plugin
pyenv virtualenv 3.12.1 myenv  # Create virtualenv
pyenv activate myenv           # Activate
pyenv deactivate               # Deactivate
```

### uv

Fast Python package and version manager. Written in Rust.

```bash
# Install
curl -LsSf https://astral.sh/uv/install.sh | sh
# or
brew install uv

# Install Python
uv python install 3.12         # Install version
uv python install              # Install latest

# Use in projects
uv init                        # Initialize project
uv add requests                # Add dependency
uv sync                        # Sync environment
uv run python script.py        # Run with env

# Direct usage
uv pip install requests        # pip replacement
uv venv                        # Create venv
```

---

## Ruby

### rbenv

Simple Ruby version manager.

```bash
# Install
brew install rbenv ruby-build

# Shell setup
eval "$(rbenv init -)"

# Install versions
rbenv install 3.3.0
rbenv install --list           # Available versions

# Set version
rbenv global 3.3.0             # Global
rbenv local 3.2.0              # Project (.ruby-version)

# Rehash (after installing gems with binaries)
rbenv rehash
```

### rvm (Ruby Version Manager)

Feature-rich, but heavier.

```bash
# Install
\curl -sSL https://get.rvm.io | bash -s stable

# Install versions
rvm install 3.3.0
rvm install ruby --latest

# Use
rvm use 3.3.0
rvm use 3.3.0 --default        # Set default

# Gemsets (isolated gem environments)
rvm gemset create myproject
rvm use 3.3.0@myproject
```

---

## Java/JVM

### SDKMAN

JVM ecosystem manager. Java, Kotlin, Gradle, Maven, etc.

```bash
# Install
curl -s "https://get.sdkman.io" | bash

# Shell setup (automatic)
source "$HOME/.sdkman/bin/sdkman-init.sh"

# Install Java
sdk install java               # Latest
sdk install java 21.0.1-tem    # Specific (Temurin)
sdk install java 21.0.1-graal  # GraalVM

# List available
sdk list java                  # All Java distributions
sdk list kotlin                # Kotlin versions

# Use
sdk use java 21.0.1-tem        # Current shell
sdk default java 21.0.1-tem    # Default

# Other tools
sdk install kotlin
sdk install gradle
sdk install maven
sdk install scala

# Env file
sdk env init                   # Create .sdkmanrc
sdk env                        # Load from .sdkmanrc
```

```properties
# .sdkmanrc
java=21.0.1-tem
kotlin=1.9.22
gradle=8.5
```

---

## Rust

### rustup

Official Rust toolchain manager.

```bash
# Install
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Toolchains
rustup install stable
rustup install nightly
rustup install 1.75.0          # Specific version

# Use
rustup default stable          # Set default
rustup override set nightly    # Project override

# Components
rustup component add clippy
rustup component add rustfmt
rustup component add rust-analyzer

# Targets (cross-compilation)
rustup target add wasm32-wasip1
rustup target add aarch64-apple-darwin

# Update
rustup update                  # Update all toolchains
```

```toml
# rust-toolchain.toml
[toolchain]
channel = "1.75.0"
components = ["rustfmt", "clippy"]
targets = ["wasm32-wasip1"]
```

---

## Go

### goenv

pyenv-style Go version manager.

```bash
# Install
git clone https://github.com/go-nv/goenv.git ~/.goenv

# Shell setup
export GOENV_ROOT="$HOME/.goenv"
export PATH="$GOENV_ROOT/bin:$PATH"
eval "$(goenv init -)"

# Install
goenv install 1.22.0

# Use
goenv global 1.22.0
goenv local 1.21.0
```

### g

Simple Go version manager.

```bash
# Install
curl -sSL https://git.io/g-install | sh -s

# Use
g install 1.22.0               # Install and switch
g                              # Interactive picker
```

---

## Decision Guide

| Need | Tool |
|------|------|
| Multiple languages | mise or asdf |
| Just Node.js | fnm (fast) or nvm (standard) |
| Just Python | pyenv or uv |
| Just Ruby | rbenv |
| JVM ecosystem | SDKMAN |
| Rust | rustup (only option) |
| Speed priority | mise, fnm, uv |
| Wide compatibility | asdf |
| Simple, minimal | Language-specific tools |

### Polyglot Setup

```bash
# Option 1: mise (recommended)
brew install mise
echo 'eval "$(mise activate zsh)"' >> ~/.zshrc

# Option 2: asdf
git clone https://github.com/asdf-vm/asdf.git ~/.asdf
echo '. $HOME/.asdf/asdf.sh' >> ~/.zshrc
asdf plugin add nodejs python golang ruby
```

---

## Related

- [[JavaScript Runtimes]]
- [[Package Managers]]
- [[Shells]]
