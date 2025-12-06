---
title: .NET MAUI
aliases:
  - MAUI
  - Multi-platform App UI
tags:
  - framework
  - mobile
  - desktop
  - cross-platform
  - csharp
  - dotnet
type: reference
status: complete
created: '2025-11-28'
---

# .NET MAUI

.NET Multi-platform App UI. Microsoft's cross-platform framework for mobile and desktop.

## Overview

| Aspect | Details |
|--------|---------|
| Language | C#, XAML |
| Rendering | Native controls |
| Platforms | iOS, Android, Windows, macOS |
| Architecture | MVVM, handlers |
| First release | 2022 |
| Predecessor | Xamarin.Forms |
| Backing | Microsoft |

---

## Platform Support

| Platform | Status | Rendering |
|----------|--------|-----------|
| iOS | ✅ Stable | Native UIKit |
| Android | ✅ Stable | Native Android Views |
| Windows | ✅ Stable | WinUI 3 |
| macOS | ✅ Stable | Mac Catalyst |
| Linux | ❌ | Community efforts |

**Native controls:** Each platform uses its own UI controls, styled by MAUI.

---

## Architecture

### Handler Architecture

```
MAUI Control → Handler → Platform View
     ↓              ↓           ↓
   Button    →  ButtonHandler  →  UIButton (iOS)
                               →  MaterialButton (Android)
                               →  Button (WinUI)
```

**Improvement over Xamarin.Forms renderers:** Cleaner, more performant.

### Project Structure

```
MyApp/
├── App.xaml              # Application resources
├── MainPage.xaml         # UI definition
├── MainPage.xaml.cs      # Code-behind
├── MauiProgram.cs        # App configuration
├── Platforms/
│   ├── Android/
│   ├── iOS/
│   ├── MacCatalyst/
│   └── Windows/
└── Resources/
```

**Single project:** One project targets all platforms.

---

## UI Development

### XAML

```xml
<ContentPage xmlns="http://schemas.microsoft.com/dotnet/2021/maui">
    <VerticalStackLayout Padding="20" Spacing="10">
        <Label Text="Hello, MAUI!"
               FontSize="24"
               HorizontalOptions="Center" />
        <Button Text="Click Me"
                Clicked="OnButtonClicked" />
        <Entry Placeholder="Enter text"
               Text="{Binding UserInput}" />
    </VerticalStackLayout>
</ContentPage>
```

### C# Markup (Alternative)

```csharp
new VerticalStackLayout
{
    Children =
    {
        new Label { Text = "Hello, MAUI!" },
        new Button { Text = "Click Me" }
            .OnClicked(OnButtonClicked)
    }
};
```

### Layouts

| Layout | Purpose |
|--------|---------|
| StackLayout | Linear arrangement |
| Grid | Rows and columns |
| FlexLayout | CSS Flexbox-like |
| AbsoluteLayout | Precise positioning |
| ScrollView | Scrollable content |

---

## Data Binding & MVVM

### Binding

```xml
<Label Text="{Binding UserName}" />
<Entry Text="{Binding SearchQuery, Mode=TwoWay}" />
<Button Command="{Binding SubmitCommand}" />
```

### ViewModel

```csharp
public partial class MainViewModel : ObservableObject
{
    [ObservableProperty]
    private string userName;

    [RelayCommand]
    private async Task Submit()
    {
        // Handle submission
    }
}
```

### MVVM Toolkit

`CommunityToolkit.Mvvm` — Source generators reduce boilerplate.

| Attribute | Generates |
|-----------|-----------|
| `[ObservableProperty]` | Property with change notification |
| `[RelayCommand]` | ICommand implementation |
| `[NotifyPropertyChangedFor]` | Dependent property updates |

---

## Navigation

### Shell

Recommended navigation pattern.

```xml
<Shell>
    <TabBar>
        <Tab Title="Home" Icon="home.png">
            <ShellContent ContentTemplate="{DataTemplate local:HomePage}" />
        </Tab>
        <Tab Title="Settings">
            <ShellContent ContentTemplate="{DataTemplate local:SettingsPage}" />
        </Tab>
    </TabBar>
</Shell>
```

### Navigation Methods

```csharp
// URI-based
await Shell.Current.GoToAsync("//settings");
await Shell.Current.GoToAsync($"details?id={item.Id}");

// With parameters
await Shell.Current.GoToAsync("details", new Dictionary<string, object>
{
    { "Item", selectedItem }
});
```

---

## Platform-Specific Code

### Conditional Compilation

```csharp
#if ANDROID
    // Android-specific code
#elif IOS
    // iOS-specific code
#endif
```

### Partial Classes

```csharp
// Shared
public partial class DeviceService
{
    public partial string GetDeviceId();
}

// Platforms/Android/DeviceService.cs
public partial class DeviceService
{
    public partial string GetDeviceId() =>
        Android.Provider.Settings.Secure.GetString(...);
}
```

### Platform Folders

Place platform-specific files in `Platforms/{Platform}/`.

---

## Key Features

### Handlers

Custom control rendering per platform.

### Hot Reload

XAML and C# hot reload during debugging.

### Graphics API

Cross-platform drawing with `Microsoft.Maui.Graphics`.

### Essentials

Built-in device APIs:

| API | Purpose |
|-----|---------|
| Geolocation | GPS, location |
| Accelerometer | Motion sensing |
| Connectivity | Network status |
| Preferences | Key-value storage |
| SecureStorage | Encrypted storage |
| Clipboard | Copy/paste |
| Share | System share sheet |

---

## Key Libraries

### UI

| Library | Purpose |
|---------|---------|
| CommunityToolkit.Maui | Extra controls, behaviors |
| Syncfusion | Commercial controls |
| Telerik | Commercial controls |

### MVVM

| Library | Purpose |
|---------|---------|
| CommunityToolkit.Mvvm | MVVM source generators |
| Prism.Maui | Full MVVM framework |

### Networking

| Library | Purpose |
|---------|---------|
| HttpClient | Built-in |
| Refit | Type-safe REST |

### Storage

| Library | Purpose |
|---------|---------|
| Preferences | Built-in key-value |
| SQLite-net | Local database |
| LiteDB | Document database |

---

## Migration from Xamarin.Forms

### Key Changes

| Xamarin.Forms | .NET MAUI |
|---------------|-----------|
| Renderers | Handlers |
| Multiple projects | Single project |
| `App.xaml.cs` setup | `MauiProgram.cs` |
| PCL/shared projects | Single project |
| `Device.RuntimePlatform` | Conditional compilation |

### Migration Path

1. Update to latest Xamarin.Forms
2. Use .NET Upgrade Assistant
3. Update namespaces
4. Convert custom renderers to handlers
5. Test on each platform

---

## Performance

### Ahead-of-Time (AOT)

Native compilation for iOS, Android (optional).

### Trimming

Remove unused code for smaller binaries.

### Startup

```csharp
builder.UseMauiApp<App>()
       .ConfigureFonts(...)
       // Add only needed services
```

---

## .NET MAUI vs Flutter vs React Native

| Aspect | MAUI | Flutter | React Native |
|--------|------|---------|--------------|
| Language | C# | Dart | JS/TypeScript |
| UI | Native controls | Custom rendering | Native controls |
| Look & feel | Native | Custom | Native |
| Desktop | ✅ Official | ✅ Official | ⚠️ Community |
| Ecosystem | .NET, NuGet | pub.dev | npm |
| Learning curve | .NET devs: easy | Dart required | React devs: easy |

---

## When to Use .NET MAUI

**Strengths:**

- .NET ecosystem (NuGet, C#, tooling)
- Native controls per platform
- Single project structure
- Microsoft support
- Existing Xamarin investment

**Considerations:**

- macOS via Catalyst (limitations)
- No Linux (official)
- Smaller community than Flutter/RN
- Some rough edges (newer)

**Best for:**

- .NET/C# teams
- Enterprise with .NET investment
- Apps needing native platform feel
- Windows + mobile targets

---

## Related

- [[Flutter]]
- [[React Native]]
- [[Avalonia]]
- [[C Sharp|C#]]
- [[Domains/Mobile Development|Mobile Development]]
- [[Domains/Desktop Development|Desktop Development]]
