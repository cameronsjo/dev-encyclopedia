---
title: Godot
aliases:
  - Godot Engine
tags:
  - engine
  - game
  - cross-platform
  - gdscript
  - csharp
  - open-source
type: reference
status: complete
created: '2025-11-28'
---

# Godot

Open-source game engine with its own scripting language.

## Overview

| Aspect | Details |
|--------|---------|
| Languages | GDScript, C#, C++, GDExtension |
| Type | Game engine |
| License | MIT (completely free) |
| Platforms | Windows, macOS, Linux, mobile, web, consoles |
| First release | 2014 |
| Backing | Godot Foundation (non-profit) |

---

## Why Godot?

### Key Advantages

| Advantage | Description |
|-----------|-------------|
| Truly free | MIT license, no royalties ever |
| Lightweight | ~50MB download, fast startup |
| All-in-one | Editor and engine together |
| Great for 2D | First-class 2D support |
| GDScript | Python-like, easy to learn |

### Platform Support

| Platform | Status |
|----------|--------|
| Windows | ✅ |
| macOS | ✅ |
| Linux | ✅ |
| Android | ✅ |
| iOS | ✅ |
| Web (HTML5) | ✅ |
| Consoles | Via third-party |

---

## Core Concepts

### Scene Tree

Everything is a tree of nodes.

```
Root (Node)
└── Main (Node2D)
    ├── Player (CharacterBody2D)
    │   ├── Sprite2D
    │   ├── CollisionShape2D
    │   └── Camera2D
    ├── TileMap
    └── UI (CanvasLayer)
        └── HUD (Control)
```

### Nodes

Building blocks of Godot.

| Node Type | Purpose |
|-----------|---------|
| Node2D | Base for 2D |
| Node3D | Base for 3D |
| Control | UI elements |
| CharacterBody2D/3D | Player/NPC movement |
| RigidBody2D/3D | Physics objects |
| Area2D/3D | Detection zones |

### Scenes

Reusable node compositions (like Unity prefabs).

```
player.tscn ← Scene file
├── CharacterBody2D
├── Sprite2D
└── CollisionShape2D
```

Scenes can instance other scenes.

---

## GDScript

Python-like language designed for Godot.

```gdscript
extends CharacterBody2D

@export var speed: float = 200.0
@export var jump_force: float = 400.0

var gravity: float = ProjectSettings.get_setting("physics/2d/default_gravity")

func _physics_process(delta: float) -> void:
    # Gravity
    if not is_on_floor():
        velocity.y += gravity * delta

    # Jump
    if Input.is_action_just_pressed("jump") and is_on_floor():
        velocity.y = -jump_force

    # Movement
    var direction := Input.get_axis("move_left", "move_right")
    velocity.x = direction * speed

    move_and_slide()
```

### Key Features

| Feature | Syntax |
|---------|--------|
| Type hints | `var x: int = 5` |
| Export | `@export var speed: float` |
| Onready | `@onready var sprite = $Sprite2D` |
| Signals | `signal health_changed(value)` |
| Static typing | Optional but recommended |

### GDScript vs Python

| Aspect | GDScript |
|--------|----------|
| Syntax | Very similar |
| Typing | Optional static types |
| Built-ins | Game-specific (Vector2, etc.) |
| Performance | Faster than Python in Godot |

---

## Signals

Event system for decoupled communication.

```gdscript
# Define signal
signal health_changed(new_health: int)
signal died

# Emit signal
func take_damage(amount: int) -> void:
    health -= amount
    health_changed.emit(health)
    if health <= 0:
        died.emit()

# Connect signal (code)
func _ready() -> void:
    player.health_changed.connect(_on_health_changed)

func _on_health_changed(new_health: int) -> void:
    health_bar.value = new_health
```

### Built-in Signals

Every node has signals: `ready`, `tree_entered`, `tree_exited`, etc.

---

## Physics

### 2D Physics Bodies

| Body | Use Case |
|------|----------|
| CharacterBody2D | Player, NPCs (manual control) |
| RigidBody2D | Physics-driven objects |
| StaticBody2D | Walls, platforms |
| Area2D | Triggers, detection |

### 3D Physics Bodies

Same concepts: `CharacterBody3D`, `RigidBody3D`, etc.

### Collision Layers

Organize what collides with what.

| Layer | Mask |
|-------|------|
| What I am | What I detect |

---

## Animation

### AnimationPlayer

Timeline-based animation for any property.

| Feature | Description |
|---------|-------------|
| Keyframes | Animate any property |
| Tracks | Position, rotation, method calls |
| Blend | Mix animations |
| Call methods | Trigger functions |

### AnimationTree

State machine for complex animation logic.

```
Idle ←→ Walk ←→ Run
  ↓
Jump → Fall → Land
```

### Tweens

Programmatic animation.

```gdscript
var tween = create_tween()
tween.tween_property($Sprite2D, "modulate", Color.RED, 0.2)
tween.tween_property($Sprite2D, "modulate", Color.WHITE, 0.2)
```

---

## UI System

Control nodes for UI.

| Node | Purpose |
|------|---------|
| Control | Base UI node |
| Label | Text display |
| Button | Clickable |
| TextEdit | Text input |
| Container | Layout (HBox, VBox, Grid) |
| TextureRect | Image display |

### Anchors & Margins

Position UI relative to screen edges.

### Themes

Global styling for UI elements.

---

## Resources

Data containers (like Unity ScriptableObjects).

```gdscript
# weapon_data.gd
class_name WeaponData
extends Resource

@export var name: String
@export var damage: int
@export var fire_rate: float
```

Save as `.tres` file, reference anywhere.

---

## Godot 4 Features

| Feature | Description |
|---------|-------------|
| Vulkan renderer | Modern graphics |
| GDScript 2.0 | Improved syntax, types |
| Scene tree improvements | Better composition |
| Multiplayer rewrite | High-level networking |
| GDExtension | C++ without recompiling |

### Godot 3 vs 4

| Aspect | Godot 3 | Godot 4 |
|--------|---------|---------|
| Renderer | OpenGL | Vulkan/OpenGL |
| GDScript | Older syntax | Improved |
| 3D quality | Good | Better |
| Stability | Very stable | Maturing |

---

## C# Support

```csharp
using Godot;

public partial class Player : CharacterBody2D
{
    [Export]
    public float Speed { get; set; } = 200.0f;

    public override void _PhysicsProcess(double delta)
    {
        var direction = Input.GetVector("left", "right", "up", "down");
        Velocity = direction * Speed;
        MoveAndSlide();
    }
}
```

### C# Considerations

| Aspect | Notes |
|--------|-------|
| Performance | Better for complex logic |
| Ecosystem | Access to NuGet |
| Export | Larger builds |
| Web export | Not supported (yet) |

---

## Project Organization

```
project/
├── project.godot
├── scenes/
│   ├── player/
│   │   └── player.tscn
│   └── levels/
│       └── level_1.tscn
├── scripts/
│   ├── player.gd
│   └── enemy.gd
├── resources/
│   └── weapons/
└── assets/
    ├── sprites/
    └── audio/
```

---

## Godot vs Alternatives

| Aspect | Godot | Unity | Unreal |
|--------|-------|-------|--------|
| License | Free (MIT) | Revenue-based | Revenue-based |
| Size | ~50MB | ~10GB | ~50GB |
| 2D | Excellent | Good | Adequate |
| 3D | Good | Good | Excellent |
| Language | GDScript/C# | C# | C++/Blueprints |
| Learning | Easiest | Moderate | Hardest |
| Community | Growing fast | Largest | Large |

---

## Exporting

### Templates

Download export templates per platform.

### Export Process

1. Configure export preset
2. Set icons, splash screens
3. Export to target platform

### Platform Notes

| Platform | Notes |
|----------|-------|
| Windows/Linux/Mac | Easy |
| Android | Needs SDK setup |
| iOS | Needs Mac + Xcode |
| Web | Easy (except C#) |
| Console | Third-party publishers |

---

## When to Use Godot

**Strengths:**

- Completely free, forever
- Lightweight and fast
- Excellent 2D support
- GDScript is easy
- Great for learning
- Active community
- Improving rapidly

**Considerations:**

- 3D not as mature as Unity/Unreal
- Smaller asset ecosystem
- Console export via third parties
- Fewer tutorials (growing)

**Best for:**

- 2D games
- Indie developers
- Learning game development
- Open-source projects
- Jam games
- Budget-conscious teams

---

## Notable Godot Games

- Sonic Colors: Ultimate (partial)
- Dome Keeper
- Cassette Beasts
- Brotato
- Halls of Torment

---

## Related

- [[Unity]]
- [[Bevy]]
- [[Domains/Game Development|Game Development]]
