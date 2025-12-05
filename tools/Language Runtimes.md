---
title: Language Runtimes
aliases:
  - Interpreters
  - VMs
  - Runtime Comparison
tags:
  - tool
  - comparison
  - runtime
type: comparison
status: complete
created: "2025-12-04"
---

# Language Runtimes

Alternative runtimes and implementations for Python, Ruby, Java, .NET, and PHP.

## Overview

| Language | Reference | Alternatives |
|----------|-----------|--------------|
| Python | CPython | PyPy, GraalPy, Jython, Cython |
| Ruby | MRI/CRuby | JRuby, TruffleRuby, mruby |
| Java | OpenJDK | GraalVM, Azul, Amazon Corretto, Eclipse Temurin |
| .NET | .NET (Core) | Mono, NativeAOT, .NET Framework |
| PHP | Zend Engine | Swoole, FrankenPHP, RoadRunner |

---

## Python Runtimes

### CPython

The reference implementation. Written in C.

| Aspect | Details |
|--------|---------|
| Type | Interpreter + bytecode VM |
| GIL | Yes (limits parallelism) |
| Performance | Baseline |
| Ecosystem | 100% compatible |
| Use case | Default, general purpose |

```bash
# Standard Python
python3 script.py
```

### PyPy

JIT-compiled Python. Much faster for long-running code.

| Aspect | Details |
|--------|---------|
| Type | JIT compiler |
| GIL | Yes |
| Performance | 2-10x faster than CPython |
| Compatibility | ~99% (C extensions vary) |
| Use case | CPU-bound, long-running |

```bash
# Install
# macOS: brew install pypy3
# Ubuntu: apt install pypy3

pypy3 script.py

# Compatibility notes
# - Most pure Python works
# - C extensions need PyPy versions
# - numpy, scipy have PyPy builds
```

### GraalPy

GraalVM's Python implementation.

| Aspect | Details |
|--------|---------|
| Type | JIT (Truffle framework) |
| GIL | No (in native mode) |
| Performance | Competitive with PyPy |
| Interop | Java, JS, Ruby, R |
| Use case | Polyglot, Java integration |

```bash
# Install GraalVM with Python
gu install python

graalpython script.py

# Java interop
from java.util import ArrayList
list = ArrayList()
list.add("Hello")
```

### Python Runtime Comparison

| Runtime | Startup | Peak Perf | Memory | C Extensions |
|---------|---------|-----------|--------|--------------|
| CPython | Fast | Baseline | Low | ✅ Native |
| PyPy | Medium | 2-10x | Higher | ⚠️ Limited |
| GraalPy | Slow | High | Higher | ⚠️ Limited |

---

## Ruby Runtimes

### MRI / CRuby

Matz's Ruby Interpreter. The reference implementation.

| Aspect | Details |
|--------|---------|
| Type | Interpreter + YARV bytecode |
| GIL | Yes (GVL) |
| JIT | YJIT (Ruby 3.1+), MJIT |
| Use case | Default, general purpose |

```bash
# Standard Ruby with YJIT (Ruby 3.1+)
ruby --yjit script.rb

# Check YJIT status
ruby --yjit -e "p RubyVM::YJIT.enabled?"
```

### JRuby

Ruby on the JVM.

| Aspect | Details |
|--------|---------|
| Type | JVM bytecode compiler |
| GIL | No (real threads) |
| Performance | Good for threaded apps |
| Interop | Full Java interop |
| Use case | Java integration, threading |

```bash
# Install
# macOS: brew install jruby
# sdkman: sdk install jruby

jruby script.rb

# Java interop
require 'java'
java_import 'java.util.ArrayList'

list = ArrayList.new
list.add("Hello")
```

### TruffleRuby

GraalVM's Ruby implementation.

| Aspect | Details |
|--------|---------|
| Type | JIT (Truffle framework) |
| GIL | No |
| Performance | Fastest (warmed up) |
| Interop | Java, JS, Python, R |
| Use case | Performance critical |

```bash
# Install via GraalVM
gu install ruby

truffleruby script.rb

# Or via ruby-build/rbenv
rbenv install truffleruby-23.1.0
```

### Ruby Runtime Comparison

| Runtime | Startup | Peak Perf | Threading | C Extensions |
|---------|---------|-----------|-----------|--------------|
| MRI | Fast | Baseline | GVL | ✅ Native |
| MRI+YJIT | Fast | 1.5-2x | GVL | ✅ Native |
| JRuby | Slow | Good | ✅ Real | ⚠️ Limited |
| TruffleRuby | Slow | Fastest | ✅ Real | ⚠️ Limited |

---

## Java/JVM Runtimes

### OpenJDK

The open-source reference implementation.

| Aspect | Details |
|--------|---------|
| Type | JIT (C2, Graal) |
| License | GPL v2 + Classpath |
| Vendors | Many (builds differ slightly) |
| Use case | Default, general purpose |

### Major OpenJDK Distributions

| Distribution | Vendor | LTS Support | Notes |
|--------------|--------|-------------|-------|
| Eclipse Temurin | Adoptium | Free | Community standard |
| Amazon Corretto | Amazon | Free | AWS optimized |
| Azul Zulu | Azul | Free/Paid | Wide platform support |
| Microsoft Build | Microsoft | Free | Azure optimized |
| Oracle OpenJDK | Oracle | 6 months | Reference builds |
| Oracle JDK | Oracle | Paid LTS | Commercial license |
| Red Hat Build | Red Hat | RHEL subscription | Enterprise |
| SAP Machine | SAP | Free | SAP workloads |

```bash
# Install via SDKMAN
sdk list java              # See all distributions
sdk install java 21.0.1-tem    # Temurin
sdk install java 21.0.1-amzn   # Corretto
sdk install java 21.0.1-zulu   # Azul Zulu
```

### GraalVM

Polyglot VM with native compilation.

| Aspect | Details |
|--------|---------|
| Type | JIT + AOT (Native Image) |
| Languages | Java, JS, Python, Ruby, R, WASM |
| Native Image | AOT compile to binary |
| Use case | Polyglot, fast startup, cloud |

```bash
# Install
sdk install java 21.0.1-graal

# Native image compilation
native-image -jar myapp.jar

# Run polyglot
js --jvm script.js
```

### Native Image (GraalVM)

Ahead-of-time compilation to native binaries.

| Aspect | Without Native Image | With Native Image |
|--------|---------------------|-------------------|
| Startup | 1-5 seconds | 10-50 ms |
| Memory | Higher (JVM) | Lower |
| Peak perf | Higher (JIT) | Lower |
| Reflection | Full | Requires config |

```bash
# Compile to native
native-image --no-fallback -jar app.jar -o app

# Result: standalone binary
./app
```

### JVM Decision Guide

| Need | Distribution |
|------|--------------|
| Community standard | Eclipse Temurin |
| AWS deployment | Amazon Corretto |
| Long-term support | Azul Zulu or Temurin |
| Native compilation | GraalVM |
| Enterprise support | Oracle JDK or Red Hat |
| Microservices | GraalVM Native Image |

---

## .NET Runtimes

### .NET (Core → Modern .NET)

The modern, cross-platform runtime.

| Aspect | Details |
|--------|---------|
| Type | JIT (RyuJIT) + AOT |
| Platforms | Windows, Linux, macOS |
| Current | .NET 8 (LTS), .NET 9 |
| Use case | Everything (default) |

```bash
# Install
# macOS: brew install dotnet
# Windows: winget install Microsoft.DotNet.SDK.8

dotnet new console
dotnet run
```

### .NET Framework

Legacy Windows-only runtime.

| Aspect | Details |
|--------|---------|
| Type | JIT |
| Platforms | Windows only |
| Current | 4.8.1 (final) |
| Status | Maintenance mode |
| Use case | Legacy Windows apps |

### Mono

Open-source .NET Framework implementation.

| Aspect | Details |
|--------|---------|
| Type | JIT + AOT |
| Platforms | All (incl. mobile, WASM) |
| Use case | Xamarin, Unity, legacy |

### NativeAOT

Ahead-of-time compilation for .NET.

| Aspect | Details |
|--------|---------|
| Type | AOT (no JIT) |
| Platforms | Windows, Linux, macOS |
| Benefits | Fast startup, smaller, no runtime |
| Trade-offs | Larger binary, no reflection |

```bash
# Publish with NativeAOT
dotnet publish -c Release -r linux-x64 \
  -p:PublishAot=true

# Result: self-contained native binary
```

### .NET Comparison

| Runtime | Startup | Size | Reflection | Platforms |
|---------|---------|------|------------|-----------|
| .NET (JIT) | Medium | Medium | ✅ Full | Cross-platform |
| NativeAOT | Fast | Larger | ⚠️ Limited | Cross-platform |
| Mono | Medium | Small | ✅ Full | All |
| Framework | Medium | N/A | ✅ Full | Windows |

### .NET Decision Guide

| Need | Runtime |
|------|---------|
| General purpose | .NET 8+ |
| Fast startup (cloud) | NativeAOT |
| Mobile apps | Mono via MAUI |
| Unity games | Mono |
| Legacy Windows | .NET Framework |
| WebAssembly | Blazor (Mono) |

---

## PHP Runtimes

### Zend Engine (Standard PHP)

The reference implementation.

| Aspect | Details |
|--------|---------|
| Type | Interpreter + OPcache |
| Model | Process-per-request (PHP-FPM) |
| Performance | Baseline |
| Use case | Default, traditional |

```bash
# PHP-FPM (FastCGI Process Manager)
php-fpm

# OPcache enabled by default in production
php -d opcache.enable_cli=1 script.php

# JIT (PHP 8+)
php -d opcache.jit=1255 script.php
```

### Swoole

Coroutine-based async runtime.

| Aspect | Details |
|--------|---------|
| Type | Extension (C) |
| Model | Event loop, coroutines |
| Performance | 5-10x traditional |
| Use case | High-concurrency, real-time |

```php
<?php
use Swoole\Http\Server;
use Swoole\Http\Request;
use Swoole\Http\Response;

$server = new Server("0.0.0.0", 9501);

$server->on("request", function (Request $request, Response $response) {
    $response->header("Content-Type", "text/plain");
    $response->end("Hello World");
});

$server->start();
```

```bash
# Install
pecl install swoole

# Run
php server.php
# Long-running process, handles many concurrent requests
```

### FrankenPHP

Modern PHP app server built on Caddy.

| Aspect | Details |
|--------|---------|
| Type | Go + C (embedded PHP) |
| Model | Worker mode (persistent) |
| Features | HTTP/3, Early Hints, real-time |
| Use case | Modern deployment |

```bash
# Run Symfony app
frankenphp php-server --root public/

# Worker mode (persistent processes)
frankenphp php-server --worker public/index.php
```

### RoadRunner

High-performance PHP application server.

| Aspect | Details |
|--------|---------|
| Type | Go + PHP workers |
| Model | Long-running workers |
| Features | gRPC, queues, WebSocket |
| Use case | Microservices, high-load |

```yaml
# .rr.yaml
server:
  command: "php worker.php"

http:
  address: 0.0.0.0:8080
  pool:
    num_workers: 4
```

### PHP Runtime Comparison

| Runtime | Model | Startup | Concurrency | Memory |
|---------|-------|---------|-------------|--------|
| PHP-FPM | Process/request | Per request | Low | Per process |
| Swoole | Coroutines | Once | Very high | Shared |
| FrankenPHP | Workers | Once | High | Workers |
| RoadRunner | Workers | Once | High | Workers |

### PHP Decision Guide

| Need | Runtime |
|------|---------|
| Traditional hosting | PHP-FPM |
| High concurrency | Swoole |
| Modern deployment | FrankenPHP |
| Microservices | RoadRunner |
| Real-time features | Swoole |
| Easy Caddy integration | FrankenPHP |

---

## Cross-Language Comparison

### Performance Profile

| Language | Reference | Fastest Alternative |
|----------|-----------|---------------------|
| Python | CPython | PyPy (2-10x) |
| Ruby | MRI | TruffleRuby (warm) |
| Java | OpenJDK | GraalVM Native Image (startup) |
| .NET | .NET JIT | NativeAOT (startup) |
| PHP | PHP-FPM | Swoole (concurrency) |

### When to Use Alternatives

| Scenario | Consider |
|----------|----------|
| Long-running compute | PyPy, JRuby, TruffleRuby |
| Fast startup needed | GraalVM Native, NativeAOT |
| High concurrency | JRuby, Swoole |
| Polyglot project | GraalVM |
| Microservices | GraalVM Native, NativeAOT, FrankenPHP |

---

## Related

- [[JavaScript Runtimes]]
- [[WebAssembly Runtimes]]
- [[Systems Language Performance]]
- [[Version Managers]]
