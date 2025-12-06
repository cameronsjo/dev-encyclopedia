---
title: React Native
aliases:
  - RN
  - React Native CLI
tags:
  - framework
  - mobile
  - cross-platform
  - typescript
  - javascript
  - react
type: reference
status: complete
created: '2025-11-28'
---

# React Native

Build native mobile apps using React and JavaScript/TypeScript.

## Overview

| Aspect | Details |
|--------|---------|
| Language | JavaScript/TypeScript |
| Rendering | Native components |
| Platforms | iOS, Android, (Windows, macOS via community) |
| Architecture | New Architecture (Fabric + TurboModules) |
| First release | 2015 |
| Backing | Meta |

---

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| iOS | ✅ Stable | Primary target |
| Android | ✅ Stable | Primary target |
| Windows | ⚠️ Community | Microsoft maintains |
| macOS | ⚠️ Community | Microsoft maintains |
| Web | ⚠️ Community | react-native-web |

---

## Architecture

### Old Architecture

```
JavaScript ←→ Bridge (JSON) ←→ Native
```

**Problem:** Serialization overhead, async only.

### New Architecture (0.68+)

```
JavaScript ←→ JSI ←→ Native (Fabric + TurboModules)
```

| Component | Purpose |
|-----------|---------|
| JSI | Direct JS ↔ Native calls |
| Fabric | New rendering system |
| TurboModules | Lazy-loaded native modules |
| Codegen | Type-safe native interfaces |

**Benefits:** Synchronous calls, better performance, type safety.

---

## Development Approaches

### React Native CLI

Full control, native code access.

```bash
npx react-native init MyApp
```

### Expo

Managed workflow, faster development.

```bash
npx create-expo-app MyApp
```

| Aspect | CLI | Expo |
|--------|-----|------|
| Setup | Complex | Simple |
| Native code | Full access | Via config plugins |
| Build | Local Xcode/Android Studio | EAS Build (cloud) |
| OTA updates | Manual | Built-in |
| Best for | Complex native needs | Most apps |

**Modern Expo:** Can eject to bare workflow, config plugins for native.

---

## Core Concepts

### Components

```tsx
import { View, Text, StyleSheet } from 'react-native';

function App() {
  return (
    <View style={styles.container}>
      <Text>Hello, React Native!</Text>
    </View>
  );
}

const styles = StyleSheet.create({
  container: { flex: 1, justifyContent: 'center' },
});
```

### Core Components

| Component | Maps To |
|-----------|---------|
| `<View>` | UIView / ViewGroup |
| `<Text>` | UILabel / TextView |
| `<Image>` | UIImageView / ImageView |
| `<ScrollView>` | UIScrollView / ScrollView |
| `<TextInput>` | UITextField / EditText |
| `<FlatList>` | Optimized list |

### Styling

StyleSheet API (similar to CSS, but not CSS).

```tsx
const styles = StyleSheet.create({
  container: {
    flex: 1,
    flexDirection: 'row',
    backgroundColor: '#fff',
    padding: 16,
  },
});
```

**Flexbox by default.** No CSS grid, limited web CSS features.

---

## State Management

Same as React web:

| Library | Style |
|---------|-------|
| useState/useReducer | Built-in, local |
| Context | Built-in, global |
| Redux Toolkit | Flux pattern |
| Zustand | Simple, hooks-based |
| Jotai | Atomic state |
| TanStack Query | Server state |

---

## Navigation

### React Navigation

The standard. Stack, tabs, drawer.

```tsx
const Stack = createNativeStackNavigator();

function App() {
  return (
    <NavigationContainer>
      <Stack.Navigator>
        <Stack.Screen name="Home" component={HomeScreen} />
        <Stack.Screen name="Details" component={DetailsScreen} />
      </Stack.Navigator>
    </NavigationContainer>
  );
}
```

### Expo Router

File-based routing (like Next.js).

```
app/
├── index.tsx        → /
├── about.tsx        → /about
└── user/
    └── [id].tsx     → /user/:id
```

---

## Key Libraries

### UI

| Library | Purpose |
|---------|---------|
| React Native Paper | Material Design |
| NativeBase | Cross-platform components |
| Tamagui | Universal UI (web + native) |
| Gluestack | Accessible components |

### Navigation

| Library | Purpose |
|---------|---------|
| React Navigation | Standard navigation |
| Expo Router | File-based routing |

### Networking

| Library | Purpose |
|---------|---------|
| fetch | Built-in |
| axios | HTTP client |
| TanStack Query | Data fetching + caching |

### Storage

| Library | Purpose |
|---------|---------|
| AsyncStorage | Key-value |
| MMKV | Fast key-value |
| WatermelonDB | SQLite wrapper |
| Realm | Object database |

### Animations

| Library | Purpose |
|---------|---------|
| Reanimated | Performant animations |
| Moti | Declarative animations |
| Lottie | After Effects animations |

---

## Native Modules

### When Needed

- Platform APIs not exposed
- Performance-critical code
- Existing native libraries

### Creating (New Architecture)

1. Define spec in TypeScript
2. Run codegen
3. Implement native side
4. Import in JS

### Using Existing

Many packages wrap native functionality:

- `react-native-camera`
- `react-native-maps`
- `react-native-gesture-handler`

---

## Performance

### Best Practices

| Issue | Solution |
|-------|----------|
| Slow lists | Use `FlatList`, not `ScrollView` |
| JS thread blocked | Move to native, use `InteractionManager` |
| Re-renders | `memo`, `useMemo`, `useCallback` |
| Large images | Proper sizing, caching |
| Animations | Use Reanimated (runs on UI thread) |

### New Architecture Benefits

- Synchronous native calls
- Concurrent rendering
- Better memory management

---

## Testing

| Type | Tool |
|------|------|
| Unit | Jest |
| Component | React Native Testing Library |
| E2E | Detox, Maestro |

---

## React Native vs Flutter

| Aspect | React Native | Flutter |
|--------|--------------|---------|
| Language | JS/TypeScript | Dart |
| UI | Native components | Custom rendering |
| Look & feel | Native per platform | Consistent custom |
| Learning curve | React developers: easy | New language |
| Performance | Good (improving) | Excellent |
| Hot reload | Yes | Yes (faster) |
| Web support | Community | Official |
| Desktop | Community | Official |

---

## When to Use React Native

**Strengths:**

- React/JS team can build mobile
- Native look and feel
- Large ecosystem
- Meta backing
- Expo simplifies development

**Considerations:**

- Native modules sometimes needed
- Two platforms to debug
- Performance requires attention
- Smaller desktop story than Flutter

**Best for:**

- Teams with React experience
- Apps needing native feel
- Rapid mobile development
- Existing React web codebases

---

## Related

- [[Flutter]]
- [[dotNET MAUI]]
- [[React]]
- [[Domains/Mobile Development|Mobile Development]]
- [[TypeScript]]
