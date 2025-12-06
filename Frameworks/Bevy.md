---
title: Bevy
aliases:
  - Bevy Engine
tags:
  - engine
  - game
  - cross-platform
  - rust
  - ecs
  - open-source
type: reference
status: complete
created: '2025-11-28'
---

# Bevy

Data-driven game engine built in Rust with ECS architecture.

## Overview

| Aspect | Details |
|--------|---------|
| Language | Rust |
| Type | Game engine |
| Architecture | Entity Component System (ECS) |
| License | MIT/Apache 2.0 |
| Platforms | Windows, macOS, Linux, Web, mobile |
| First release | 2020 |
| Status | Pre-1.0, actively developed |

---

## Why Bevy?

### Key Differentiators

| Feature | Description |
|---------|-------------|
| Pure Rust | Memory safety, no GC pauses |
| ECS-first | Composition over inheritance |
| Data-oriented | Cache-friendly, parallel by default |
| Hot reloading | Fast iteration (experimental) |
| Modular | Use only what you need |

### Platform Support

| Platform | Status |
|----------|--------|
| Windows | ✅ Stable |
| macOS | ✅ Stable |
| Linux | ✅ Stable |
| Web (WASM) | ✅ Stable |
| iOS | ⚠️ Experimental |
| Android | ⚠️ Experimental |

---

## ECS Architecture

### Core Concepts

| Concept | Description |
|---------|-------------|
| Entity | Unique ID (just a number) |
| Component | Data attached to entity |
| System | Logic that operates on components |
| Resource | Global singleton data |
| Query | Filter for systems |

### Traditional vs ECS

```
Traditional OOP:
Player class {
  position, velocity, health, sprite, input...
}

ECS:
Entity 1:
  + Position { x, y }
  + Velocity { dx, dy }
  + Health { current, max }
  + Sprite { texture }
  + PlayerInput
```

### Why ECS?

| Benefit | Description |
|---------|-------------|
| Composition | Mix and match components |
| Performance | Data locality, parallelism |
| Flexibility | Add/remove components at runtime |
| Decoupling | Systems don't know about each other |

---

## Basic Example

```rust
use bevy::prelude::*;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, setup)
        .add_systems(Update, move_player)
        .run();
}

#[derive(Component)]
struct Player;

#[derive(Component)]
struct Speed(f32);

fn setup(mut commands: Commands) {
    commands.spawn(Camera2dBundle::default());

    commands.spawn((
        SpriteBundle {
            sprite: Sprite {
                color: Color::rgb(0.25, 0.25, 0.75),
                custom_size: Some(Vec2::new(50.0, 50.0)),
                ..default()
            },
            ..default()
        },
        Player,
        Speed(200.0),
    ));
}

fn move_player(
    keyboard: Res<ButtonInput<KeyCode>>,
    mut query: Query<(&mut Transform, &Speed), With<Player>>,
    time: Res<Time>,
) {
    let (mut transform, speed) = query.single_mut();
    let mut direction = Vec3::ZERO;

    if keyboard.pressed(KeyCode::KeyW) { direction.y += 1.0; }
    if keyboard.pressed(KeyCode::KeyS) { direction.y -= 1.0; }
    if keyboard.pressed(KeyCode::KeyA) { direction.x -= 1.0; }
    if keyboard.pressed(KeyCode::KeyD) { direction.x += 1.0; }

    transform.translation += direction.normalize_or_zero() * speed.0 * time.delta_seconds();
}
```

---

## Components

Data containers with no behavior.

```rust
#[derive(Component)]
struct Position { x: f32, y: f32 }

#[derive(Component)]
struct Velocity { x: f32, y: f32 }

#[derive(Component)]
struct Health {
    current: i32,
    max: i32,
}

#[derive(Component)]
struct Enemy;

#[derive(Component)]
struct Player;
```

### Component Bundles

Group related components.

```rust
#[derive(Bundle)]
struct PlayerBundle {
    player: Player,
    health: Health,
    speed: Speed,
    sprite: SpriteBundle,
}

// Spawn with bundle
commands.spawn(PlayerBundle {
    player: Player,
    health: Health { current: 100, max: 100 },
    speed: Speed(200.0),
    sprite: SpriteBundle::default(),
});
```

---

## Systems

Functions that run on matching entities.

```rust
// Runs on all entities with Position and Velocity
fn movement(mut query: Query<(&mut Position, &Velocity)>) {
    for (mut pos, vel) in &mut query {
        pos.x += vel.x;
        pos.y += vel.y;
    }
}

// Filter with With/Without
fn player_movement(
    query: Query<&mut Transform, With<Player>>,
) { ... }

fn enemy_ai(
    query: Query<&mut Transform, (With<Enemy>, Without<Player>)>,
) { ... }
```

### System Ordering

```rust
app
    .add_systems(Update, (
        input_system,
        movement_system.after(input_system),
        collision_system.after(movement_system),
    ))
```

### System Sets

Group and order systems.

```rust
#[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
enum GameSet {
    Input,
    Movement,
    Collision,
}

app
    .configure_sets(Update, (
        GameSet::Input,
        GameSet::Movement.after(GameSet::Input),
        GameSet::Collision.after(GameSet::Movement),
    ))
    .add_systems(Update, input_system.in_set(GameSet::Input))
    .add_systems(Update, movement_system.in_set(GameSet::Movement))
```

---

## Resources

Global singletons.

```rust
#[derive(Resource)]
struct GameState {
    score: u32,
    level: u32,
}

// Insert resource
app.insert_resource(GameState { score: 0, level: 1 });

// Access in system
fn update_score(mut state: ResMut<GameState>) {
    state.score += 10;
}

fn display_score(state: Res<GameState>) {
    println!("Score: {}", state.score);
}
```

---

## Events

Decouple systems with events.

```rust
#[derive(Event)]
struct DamageEvent {
    entity: Entity,
    amount: i32,
}

// Send events
fn attack_system(mut events: EventWriter<DamageEvent>) {
    events.send(DamageEvent { entity, amount: 10 });
}

// Receive events
fn damage_system(
    mut events: EventReader<DamageEvent>,
    mut query: Query<&mut Health>,
) {
    for event in events.read() {
        if let Ok(mut health) = query.get_mut(event.entity) {
            health.current -= event.amount;
        }
    }
}

// Register event
app.add_event::<DamageEvent>();
```

---

## States

Game state management.

```rust
#[derive(States, Debug, Clone, PartialEq, Eq, Hash, Default)]
enum GameState {
    #[default]
    Menu,
    Playing,
    Paused,
    GameOver,
}

app
    .init_state::<GameState>()
    .add_systems(OnEnter(GameState::Playing), setup_game)
    .add_systems(OnExit(GameState::Playing), cleanup_game)
    .add_systems(Update, game_logic.run_if(in_state(GameState::Playing)))
```

---

## Rendering

### 2D

```rust
// Sprite
commands.spawn(SpriteBundle {
    texture: asset_server.load("sprite.png"),
    transform: Transform::from_xyz(0.0, 0.0, 0.0),
    ..default()
});

// Sprite sheet
commands.spawn(SpriteSheetBundle {
    texture,
    atlas: TextureAtlas { layout, index: 0 },
    ..default()
});
```

### 3D

```rust
// Mesh
commands.spawn(PbrBundle {
    mesh: meshes.add(Cuboid::new(1.0, 1.0, 1.0)),
    material: materials.add(Color::rgb(0.8, 0.2, 0.2)),
    ..default()
});

// Light
commands.spawn(PointLightBundle {
    point_light: PointLight {
        intensity: 1500.0,
        ..default()
    },
    transform: Transform::from_xyz(4.0, 8.0, 4.0),
    ..default()
});
```

---

## Assets

```rust
#[derive(Resource)]
struct GameAssets {
    player_texture: Handle<Image>,
    font: Handle<Font>,
}

fn load_assets(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
) {
    commands.insert_resource(GameAssets {
        player_texture: asset_server.load("player.png"),
        font: asset_server.load("fonts/main.ttf"),
    });
}
```

---

## Plugins

Modular code organization.

```rust
pub struct EnemyPlugin;

impl Plugin for EnemyPlugin {
    fn build(&self, app: &mut App) {
        app
            .add_systems(Startup, spawn_enemies)
            .add_systems(Update, (enemy_movement, enemy_attack));
    }
}

// Use plugin
app.add_plugins(EnemyPlugin);
```

### Default Plugins

```rust
app.add_plugins(DefaultPlugins)  // Window, input, rendering, etc.
app.add_plugins(MinimalPlugins)  // Headless, testing
```

---

## Bevy vs Alternatives

| Aspect | Bevy | Unity | Godot |
|--------|------|-------|-------|
| Language | Rust | C# | GDScript/C# |
| Architecture | ECS | Component | Node tree |
| Performance | Excellent | Good | Good |
| Memory safety | Compile-time | Runtime | Runtime |
| Maturity | Young | Mature | Mature |
| Editor | Minimal | Full | Full |
| Learning curve | Steep (Rust) | Moderate | Easy |

---

## Ecosystem

### Key Crates

| Crate | Purpose |
|-------|---------|
| bevy_rapier | Physics (Rapier integration) |
| bevy_egui | Immediate mode UI |
| bevy_kira_audio | Audio |
| leafwing-input-manager | Input handling |
| bevy_asset_loader | Asset loading |

### Community Resources

| Resource | Description |
|----------|-------------|
| Bevy Cheatbook | Comprehensive guide |
| Bevy Examples | Official examples |
| Bevy Discord | Active community |

---

## When to Use Bevy

**Strengths:**

- Rust safety and performance
- Modern ECS architecture
- Parallel by default
- Growing rapidly
- Great for learning ECS
- No runtime fees

**Considerations:**

- Rust learning curve
- No visual editor (yet)
- Pre-1.0 (breaking changes)
- Smaller ecosystem
- Mobile still experimental

**Best for:**

- Rust enthusiasts
- Performance-critical games
- Learning ECS concepts
- Open-source projects
- Experimental/jam games

---

## Related

- [[Rust]]
- [[Unity]]
- [[Godot]]
- [[Domains/Game Development|Game Development]]
