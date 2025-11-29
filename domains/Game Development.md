---
title: Game Development
aliases:
  - Game Dev
  - Game Engines
tags:
  - domain
  - game
  - graphics
type: moc
status: complete
created: 2025-11-28
---

# Game Development

Building interactive games and real-time applications.

## Engine Overview

| Engine | Language | Platforms | License |
|--------|----------|-----------|---------|
| Unity | C# | All major | Proprietary |
| Unreal | C++, Blueprints | All major | Royalty-based |
| Godot | GDScript, C#, C++ | All major | MIT |
| Bevy | Rust | Desktop, Web | MIT |
| GameMaker | GML | Desktop, Mobile, Web | Proprietary |
| Defold | Lua | Desktop, Mobile, Web | Free |

---

## Major Engines

### [[Unity]]

**The generalist.** Largest market share, especially mobile/indie.

**Strengths:**
- Huge asset store
- Massive community
- C# (accessible)
- Mobile optimization
- VR/AR support

**Considerations:**
- Runtime fee controversy (2023, revised)
- Performance vs Unreal for AAA
- Render pipeline complexity

**Best for:** Mobile, indie, VR, 2D, rapid prototyping.

### Unreal Engine

**The powerhouse.** AAA graphics, complex systems. (See dedicated page coming soon)

**Strengths:**
- Industry-leading graphics
- Blueprints (visual scripting)
- Full source access
- Nanite, Lumen, MetaHumans

**Considerations:**
- Steep learning curve
- C++ complexity
- Heavy tooling
- 5% royalty after $1M

**Best for:** AAA, realistic graphics, large teams.

### [[Godot]]

**The open-source contender.** Growing fast.

**Strengths:**
- Completely free (MIT)
- Lightweight
- Scene-based architecture
- GDScript (Python-like)
- C# support

**Considerations:**
- Smaller ecosystem
- 3D still maturing
- Fewer learning resources

**Best for:** Indie, 2D, learning, open-source projects.

### [[Bevy]]

**Rust ECS game engine.** Data-oriented, modern.

**Strengths:**
- Rust safety
- Entity Component System
- Hot reloading
- Modern architecture

**Considerations:**
- Pre-1.0, changing rapidly
- Smaller community
- Rust learning curve

**Best for:** Rust developers, data-oriented design enthusiasts.

---

## Architecture Patterns

### Entity Component System (ECS)

**Data-oriented design.** Entities are IDs, components are data, systems are logic.

| Concept | Role |
|---------|------|
| Entity | Unique ID |
| Component | Data only |
| System | Logic that operates on components |

**Benefits:**
- Cache-friendly
- Easy composition
- Parallelization

**Engines:** Bevy (native), Unity DOTS, Unreal Mass

### Traditional OOP

**Inheritance-based.** GameObject/Actor with attached scripts.

**Engines:** Unity (default), Godot, GameMaker

---

## Game Development Concepts

### Game Loop

```
while (running):
    processInput()
    update(deltaTime)
    render()
```

**Fixed vs variable timestep:** Fixed for physics, variable for rendering.

### Common Systems

| System | Purpose |
|--------|---------|
| Input | Controller, keyboard, touch |
| Physics | Collision, rigidbody |
| Rendering | Graphics, shaders |
| Audio | Sound, music |
| Animation | Skeletal, procedural |
| AI | Pathfinding, behavior |
| Networking | Multiplayer |
| UI | Menus, HUD |

### Physics Engines

| Engine | Used By |
|--------|---------|
| PhysX | Unity, Unreal |
| Box2D | 2D games |
| Jolt | Open source 3D |
| Rapier | Rust ecosystem |

---

## Graphics & Rendering

### Render Pipelines

| Pipeline | Description |
|----------|-------------|
| Forward | Simple, good for mobile |
| Deferred | Many lights, more memory |
| Forward+ | Hybrid approach |

### Graphics APIs

| API | Platforms |
|-----|-----------|
| Vulkan | Cross-platform, low-level |
| DirectX 12 | Windows, Xbox |
| Metal | Apple platforms |
| WebGPU | Browsers |
| OpenGL | Legacy cross-platform |

### Shaders

| Language | API |
|----------|-----|
| HLSL | DirectX |
| GLSL | OpenGL, Vulkan |
| MSL | Metal |
| WGSL | WebGPU |
| ShaderLab | Unity |

---

## 2D vs 3D

### 2D Game Development

| Engine | Notes |
|--------|-------|
| Godot | Excellent 2D, first-class |
| Unity | Good 2D, evolved from 3D |
| GameMaker | 2D-focused |
| Defold | 2D-focused, lightweight |

### 3D Game Development

| Engine | Notes |
|--------|-------|
| Unreal | Best graphics |
| Unity | Versatile |
| Godot | Improving |
| Bevy | Growing |

---

## Multiplayer

### Architectures

| Architecture | Description |
|--------------|-------------|
| Client-server | Authoritative server |
| Peer-to-peer | Direct connections |
| Client-authoritative | Simple, cheat-prone |

### Networking Solutions

| Solution | Platform |
|----------|----------|
| Mirror | Unity (open source) |
| Photon | Unity, Unreal |
| Steam Networking | Desktop |
| Netcode for GameObjects | Unity official |
| Unreal Replication | Unreal built-in |

### Challenges

- Latency compensation
- State synchronization
- Cheating prevention
- Matchmaking

---

## Asset Pipeline

### 3D Assets

| Format | Use |
|--------|-----|
| FBX | Industry standard exchange |
| glTF | Modern, open standard |
| USDZ | Apple AR |
| Blend | Blender native |

### 2D Assets

| Format | Use |
|--------|-----|
| PNG | Sprites, transparency |
| Aseprite | Pixel art |
| SVG | Vector graphics |

### Audio

| Format | Use |
|--------|-----|
| WAV | Uncompressed |
| OGG | Compressed, open |
| MP3 | Compressed, licensed |

---

## Platform Considerations

| Platform | Considerations |
|----------|----------------|
| PC | Most flexible, keyboard/mouse |
| Console | Certification, controller |
| Mobile | Touch, battery, performance |
| Web | Browser limits, WASM |
| VR | Performance critical, motion |

---

## Decision Guide

| Scenario | Recommendation |
|----------|----------------|
| First game | Godot or Unity |
| Mobile game | Unity |
| AAA graphics | Unreal |
| 2D indie | Godot |
| Open source preference | Godot or Bevy |
| Rust developer | Bevy |
| Large team, resources | Unreal |
| VR/AR | Unity or Unreal |

---

## Related

- [[Desktop Development]]
- [[Graphics Programming]]
- [[Runtimes]]
