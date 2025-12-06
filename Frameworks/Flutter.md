---
title: Flutter
aliases:
  - Flutter SDK
tags:
  - framework
  - mobile
  - desktop
  - web
  - cross-platform
  - dart
type: reference
status: complete
created: '2025-11-28'
---

# Flutter

Google's cross-platform UI toolkit. Single codebase for mobile, web, and desktop.

## Overview

| Aspect | Details |
|--------|---------|
| Language | Dart |
| Rendering | Custom (Skia → Impeller) |
| Platforms | iOS, Android, Web, Windows, macOS, Linux |
| Architecture | Widget-based, reactive |
| First release | 2017 |
| Backing | Google |

---

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| iOS | ✅ Stable | Primary target |
| Android | ✅ Stable | Primary target |
| Web | ✅ Stable | Good, some limitations |
| Windows | ✅ Stable | Native Win32 |
| macOS | ✅ Stable | Native Cocoa |
| Linux | ✅ Stable | GTK-based |

**True cross-platform:** Same Dart code runs everywhere with platform-specific UI adaptations.

---

## Architecture

### Everything is a Widget

```
MyApp (Widget)
└── MaterialApp (Widget)
    └── Scaffold (Widget)
        ├── AppBar (Widget)
        └── Column (Widget)
            ├── Text (Widget)
            └── Button (Widget)
```

### Widget Types

| Type | Description | Example |
|------|-------------|---------|
| StatelessWidget | Immutable, no internal state | `Text`, `Icon` |
| StatefulWidget | Mutable state, rebuilds | Forms, animations |
| InheritedWidget | Data down the tree | Theme, MediaQuery |

### Rendering Pipeline

```
Widget → Element → RenderObject → Skia/Impeller → Screen
```

- **Widget:** Immutable configuration
- **Element:** Mutable instance in tree
- **RenderObject:** Layout and painting
- **Impeller:** New rendering engine (iOS default)

---

## State Management

### Built-in

| Approach | Scope | Use Case |
|----------|-------|----------|
| `setState` | Single widget | Simple local state |
| `InheritedWidget` | Subtree | Theme, localization |
| `ValueNotifier` | Single value | Simple reactivity |

### Popular Packages

| Package | Style | Complexity |
|---------|-------|------------|
| Provider | InheritedWidget wrapper | Low |
| Riverpod | Provider evolution, compile-safe | Medium |
| Bloc/Cubit | Event-driven, streams | Medium-High |
| GetX | All-in-one, less boilerplate | Low |
| MobX | Observable/reactions | Medium |

### Recommendations

| Scenario | Choice |
|----------|--------|
| Learning Flutter | `setState`, then Provider |
| Production app | Riverpod or Bloc |
| Rapid prototyping | GetX |
| Large team | Bloc (structure) |

---

## Navigation

### Navigator 1.0

Imperative, stack-based.

```dart
Navigator.push(context, MaterialPageRoute(builder: (_) => DetailPage()));
Navigator.pop(context);
```

### Navigator 2.0 / Router

Declarative, URL-based. Complex but powerful.

### go_router

Recommended package. Simpler declarative routing.

```dart
GoRouter(
  routes: [
    GoRoute(path: '/', builder: (_, __) => HomePage()),
    GoRoute(path: '/user/:id', builder: (_, state) => UserPage(state.params['id']!)),
  ],
)
```

---

## Key Packages

### UI

| Package | Purpose |
|---------|---------|
| flutter_hooks | React-like hooks |
| animations | Material motion |
| flutter_animate | Declarative animations |

### State

| Package | Purpose |
|---------|---------|
| provider | Simple state management |
| riverpod | Modern state management |
| flutter_bloc | Bloc pattern |

### Networking

| Package | Purpose |
|---------|---------|
| http | Simple HTTP |
| dio | Advanced HTTP, interceptors |
| retrofit | Type-safe API client |

### Storage

| Package | Purpose |
|---------|---------|
| shared_preferences | Key-value storage |
| hive | Fast NoSQL |
| drift | Type-safe SQLite |
| isar | Fast embedded DB |

### Code Generation

| Package | Purpose |
|---------|---------|
| freezed | Immutable classes, unions |
| json_serializable | JSON parsing |
| build_runner | Code generation |

---

## Development Experience

### Hot Reload

Sub-second UI updates without losing state.

**What preserves state:** Widget changes, new widgets.
**What doesn't:** State class changes, main() changes.

### DevTools

Built-in debugging tools:

- Widget inspector
- Performance overlay
- Memory profiler
- Network inspector

### Testing

| Type | Tool |
|------|------|
| Unit | `test` |
| Widget | `flutter_test` |
| Integration | `integration_test` |
| Golden | `golden_toolkit` |

---

## Performance

### Strengths

- 60/120 fps rendering
- AOT compilation for production
- Direct rendering (no bridge)

### Considerations

- Initial app size (~5-15 MB)
- Web performance (improving)
- Platform-specific optimizations needed

### Optimization Tips

| Issue | Solution |
|-------|----------|
| Unnecessary rebuilds | `const` widgets, selective rebuilds |
| Large lists | `ListView.builder` |
| Heavy images | Caching, proper sizing |
| Jank | Profile with DevTools |

---

## Platform Integration

### Method Channels

Call native code from Dart.

```dart
final result = await platform.invokeMethod('getBatteryLevel');
```

### FFI

Direct C interop for performance-critical code.

### Platform-Specific UI

```dart
if (Platform.isIOS) {
  return CupertinoButton(...);
} else {
  return ElevatedButton(...);
}
```

---

## Flutter vs React Native

| Aspect | Flutter | React Native |
|--------|---------|--------------|
| Language | Dart | JavaScript/TypeScript |
| Rendering | Custom (Skia) | Native components |
| Performance | Excellent | Good (improving) |
| Look & feel | Custom, consistent | Native, per-platform |
| Hot reload | Sub-second | Fast |
| Desktop | Official | Community |
| Web | Official | Limited |
| Learning curve | Dart + Flutter | JS/React + RN |
| Community | Growing fast | Larger, established |

---

## When to Use Flutter

**Strengths:**

- True single codebase
- Excellent performance
- Beautiful custom UIs
- Hot reload productivity
- Growing ecosystem

**Considerations:**

- Dart (new language for most)
- App size larger than native
- Platform-specific features need channels
- Web not ideal for SEO-heavy sites

**Best for:**

- Cross-platform apps (mobile primary)
- Custom branded UIs
- Startups/small teams
- MVPs with expansion plans

---

## Related

- [[Languages/Dart|Dart]]
- [[React Native]]
- [[dotNET MAUI]]
- [[Domains/Mobile Development|Mobile Development]]
- [[Domains/Desktop Development|Desktop Development]]
