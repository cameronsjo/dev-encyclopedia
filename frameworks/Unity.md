---
title: Unity
aliases:
  - Unity Engine
  - Unity3D
tags:
  - engine
  - game
  - cross-platform
  - csharp
type: reference
status: complete
created: 2025-11-28
---

# Unity

Cross-platform game engine for 2D, 3D, VR, and AR.

## Overview

| Aspect | Details |
|--------|---------|
| Language | C# |
| Type | Game engine |
| Rendering | Built-in, URP, HDRP |
| Platforms | 20+ (PC, mobile, console, VR, web) |
| First release | 2005 |
| Backing | Unity Technologies |

---

## Platform Support

| Platform | Status |
|----------|--------|
| Windows | ✅ |
| macOS | ✅ |
| Linux | ✅ |
| iOS | ✅ |
| Android | ✅ |
| WebGL | ✅ |
| PlayStation | ✅ (license) |
| Xbox | ✅ (license) |
| Nintendo Switch | ✅ (license) |
| VR (Quest, PSVR) | ✅ |

---

## Core Concepts

### GameObjects & Components

Everything is a GameObject with Components attached.

```
GameObject "Player"
├── Transform (position, rotation, scale)
├── MeshRenderer (visual)
├── Rigidbody (physics)
├── Collider (collision)
└── PlayerController (custom script)
```

### Component Pattern

```csharp
public class PlayerController : MonoBehaviour
{
    public float speed = 5f;

    void Update()
    {
        float h = Input.GetAxis("Horizontal");
        float v = Input.GetAxis("Vertical");
        transform.Translate(new Vector3(h, 0, v) * speed * Time.deltaTime);
    }
}
```

### Lifecycle Methods

| Method | When Called |
|--------|-------------|
| Awake() | Instance created |
| Start() | Before first Update |
| Update() | Every frame |
| FixedUpdate() | Fixed time step (physics) |
| LateUpdate() | After all Updates |
| OnDestroy() | Object destroyed |

---

## Scene & Hierarchy

### Scene Structure

```
Scene
├── Main Camera
├── Directional Light
├── Player
│   └── Child Objects
├── Environment
│   ├── Ground
│   └── Props
└── UI Canvas
```

### Prefabs

Reusable GameObject templates.

| Concept | Description |
|---------|-------------|
| Prefab | Template asset |
| Instance | Copy in scene |
| Prefab Variant | Modified prefab |
| Nested Prefab | Prefab within prefab |

---

## Scripting

### C# in Unity

```csharp
using UnityEngine;

public class Enemy : MonoBehaviour
{
    [SerializeField] private float health = 100f;
    [SerializeField] private GameObject deathEffect;

    public void TakeDamage(float damage)
    {
        health -= damage;
        if (health <= 0)
        {
            Die();
        }
    }

    private void Die()
    {
        Instantiate(deathEffect, transform.position, Quaternion.identity);
        Destroy(gameObject);
    }
}
```

### Inspector Attributes

| Attribute | Effect |
|-----------|--------|
| [SerializeField] | Show private in Inspector |
| [HideInInspector] | Hide public from Inspector |
| [Range(0, 100)] | Slider in Inspector |
| [Header("Section")] | Section header |
| [Tooltip("...")] | Hover tooltip |

### Finding Objects

```csharp
// By name (slow)
GameObject.Find("Player");

// By tag
GameObject.FindWithTag("Enemy");
GameObject[] enemies = GameObject.FindGameObjectsWithTag("Enemy");

// By component
FindObjectOfType<PlayerController>();

// Child
transform.Find("ChildName");
GetComponentInChildren<Renderer>();
```

---

## Physics

### 3D Physics

```csharp
public class PhysicsExample : MonoBehaviour
{
    private Rigidbody rb;

    void Start()
    {
        rb = GetComponent<Rigidbody>();
    }

    void FixedUpdate()
    {
        rb.AddForce(Vector3.forward * 10f);
    }

    void OnCollisionEnter(Collision collision)
    {
        if (collision.gameObject.CompareTag("Ground"))
        {
            // Hit ground
        }
    }

    void OnTriggerEnter(Collider other)
    {
        // Entered trigger zone
    }
}
```

### 2D Physics

Same concepts with `Rigidbody2D`, `Collider2D`, `OnCollisionEnter2D`.

---

## Rendering Pipelines

| Pipeline | Use Case |
|----------|----------|
| Built-in | Legacy, simple projects |
| URP (Universal) | Mobile, cross-platform, performance |
| HDRP (High Definition) | High-end PC/console, photorealism |

### Choosing Pipeline

| Scenario | Recommendation |
|----------|----------------|
| Mobile game | URP |
| VR | URP |
| AAA quality | HDRP |
| Stylized graphics | URP or Built-in |
| Learning | Built-in or URP |

---

## UI Systems

### Canvas UI (uGUI)

```
Canvas
├── Panel
│   ├── Text (TMP)
│   ├── Button
│   └── Image
└── Event System
```

### UI Toolkit

Newer, web-like approach with USS (like CSS) and UXML.

| System | Best For |
|--------|----------|
| uGUI | In-game UI, established |
| UI Toolkit | Editor UI, complex layouts |

---

## Animation

### Animator Controller

State machine for animations.

```
Idle ←→ Walk ←→ Run
  ↓
Jump
```

### Animation System

| Component | Purpose |
|-----------|---------|
| Animator | Controls animation state machine |
| Animation Clip | Single animation |
| Animator Controller | State machine |
| Avatar | Humanoid rig mapping |

```csharp
animator.SetFloat("Speed", velocity.magnitude);
animator.SetTrigger("Jump");
animator.SetBool("IsGrounded", isGrounded);
```

---

## Asset Store

Unity's marketplace for assets.

| Category | Examples |
|----------|----------|
| 3D Models | Characters, environments |
| 2D Assets | Sprites, tilesets |
| Audio | Music, sound effects |
| Tools | Editor extensions |
| Complete Projects | Game templates |

---

## Input System

### New Input System

```csharp
using UnityEngine.InputSystem;

public class PlayerInput : MonoBehaviour
{
    public void OnMove(InputAction.CallbackContext context)
    {
        Vector2 moveInput = context.ReadValue<Vector2>();
    }

    public void OnJump(InputAction.CallbackContext context)
    {
        if (context.performed)
        {
            Jump();
        }
    }
}
```

### Input Actions Asset

Define inputs in asset, map to devices.

| Benefit | Description |
|---------|-------------|
| Device agnostic | Works with any controller |
| Rebindable | Players can customize |
| Multiplayer ready | Per-player input |

---

## Scripting Backends

| Backend | Description |
|---------|-------------|
| Mono | JIT compilation, faster builds |
| IL2CPP | AOT to C++, better performance |

### When to Use

| Scenario | Backend |
|----------|---------|
| Development | Mono |
| Release builds | IL2CPP |
| iOS (required) | IL2CPP |
| WebGL (required) | IL2CPP |

---

## Common Patterns

### Singleton

```csharp
public class GameManager : MonoBehaviour
{
    public static GameManager Instance { get; private set; }

    void Awake()
    {
        if (Instance == null)
        {
            Instance = this;
            DontDestroyOnLoad(gameObject);
        }
        else
        {
            Destroy(gameObject);
        }
    }
}
```

### Object Pooling

Reuse objects instead of instantiate/destroy.

### Scriptable Objects

Data containers for configuration.

```csharp
[CreateAssetMenu(fileName = "WeaponData", menuName = "Game/Weapon")]
public class WeaponData : ScriptableObject
{
    public string weaponName;
    public int damage;
    public float fireRate;
}
```

---

## Monetization & Services

### Unity Services

| Service | Purpose |
|---------|---------|
| Unity Ads | In-game advertising |
| Unity Analytics | Player behavior |
| Unity Cloud Build | CI/CD |
| Unity Multiplayer | Networking |

---

## Unity vs Alternatives

| Aspect | Unity | Unreal | Godot |
|--------|-------|--------|-------|
| Language | C# | C++/Blueprints | GDScript/C# |
| 2D Support | Good | Adequate | Excellent |
| 3D Quality | Good | Best | Good |
| Learning curve | Moderate | Steeper | Easiest |
| License | Revenue-based | Revenue-based | Free (MIT) |
| Mobile | Excellent | Good | Good |
| Documentation | Good | Good | Good |

---

## When to Use Unity

**Strengths:**
- Cross-platform (20+ targets)
- C# language (familiar to many)
- Huge asset store
- Great for 2D and 3D
- Strong mobile support
- Large community

**Considerations:**
- Licensing costs at scale
- Can feel bloated
- Not best for AAA 3D (vs Unreal)
- Runtime fees controversy (2023)

**Best for:**
- Mobile games
- Indie games
- VR/AR development
- Cross-platform titles
- 2D games
- Prototyping

---

## Notable Unity Games

- Hollow Knight
- Cuphead
- Among Us
- Pokémon GO
- Genshin Impact
- Hearthstone
- Fall Guys

---

## Related

- [[Godot]]
- [[Bevy]]
- [[C Sharp|C#]]
- [[domains/Game Development|Game Development]]
