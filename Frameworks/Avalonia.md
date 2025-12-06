---
title: Avalonia
aliases:
  - Avalonia UI
  - AvaloniaUI
tags:
  - framework
  - desktop
  - cross-platform
  - csharp
  - dotnet
  - xaml
type: reference
status: complete
created: '2025-11-28'
---

# Avalonia

Cross-platform .NET UI framework. "WPF everywhere."

## Overview

| Aspect | Details |
|--------|---------|
| Language | C#, XAML |
| Rendering | Skia (custom) |
| Platforms | Windows, macOS, Linux, Web, iOS, Android |
| Architecture | MVVM, XAML-based |
| Backing | AvaloniaUI (company) |

---

## Platform Support

| Platform | Status | Notes |
|----------|--------|-------|
| Windows | ✅ Stable | Primary target |
| macOS | ✅ Stable | Native feel |
| Linux | ✅ Stable | X11, Wayland |
| Web (WASM) | ✅ Stable | Browser-based |
| iOS | ⚠️ Preview | Mobile expanding |
| Android | ⚠️ Preview | Mobile expanding |

**True Linux support:** Unlike MAUI, Avalonia has first-class Linux.

---

## Why Avalonia?

### WPF Developers

- Familiar XAML
- Similar APIs
- MVVM patterns work
- Styles and templates

### Cross-Platform .NET

| Framework | Windows | macOS | Linux | Web |
|-----------|---------|-------|-------|-----|
| WPF | ✅ | ❌ | ❌ | ❌ |
| WinUI | ✅ | ❌ | ❌ | ❌ |
| MAUI | ✅ | ✅* | ❌ | ❌ |
| Avalonia | ✅ | ✅ | ✅ | ✅ |

*MAUI macOS via Catalyst

---

## Architecture

### Rendering

Custom rendering via Skia—not native controls.

**Benefit:** Pixel-perfect consistency across platforms.

**Trade-off:** May not feel 100% native.

### Visual Tree

```
Window
└── Grid
    ├── TextBlock
    ├── TextBox
    └── Button
        └── TextBlock
```

### Styling

CSS-like, more powerful than WPF.

```xml
<Style Selector="Button:pointerover">
    <Setter Property="Background" Value="LightBlue"/>
</Style>
```

---

## Getting Started

### Create Project

```bash
dotnet new install Avalonia.Templates
dotnet new avalonia.app -n MyApp
```

### Project Structure

```
MyApp/
├── App.axaml           # Application resources
├── MainWindow.axaml    # Main window XAML
├── MainWindow.axaml.cs # Code-behind
├── Program.cs          # Entry point
└── ViewModels/
    └── MainWindowViewModel.cs
```

---

## XAML Basics

### Window

```xml
<Window xmlns="https://github.com/avaloniaui"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="My App" Width="800" Height="600">
    <StackPanel Margin="20">
        <TextBlock Text="Hello, Avalonia!" FontSize="24"/>
        <Button Content="Click Me" Command="{Binding ClickCommand}"/>
        <TextBox Text="{Binding UserInput}"/>
    </StackPanel>
</Window>
```

### Layouts

| Layout | Purpose |
|--------|---------|
| StackPanel | Linear stacking |
| Grid | Rows/columns |
| DockPanel | Dock to edges |
| WrapPanel | Wrapping flow |
| Canvas | Absolute positioning |

### Controls

| Control | Purpose |
|---------|---------|
| TextBlock | Display text |
| TextBox | Text input |
| Button | Clickable button |
| ComboBox | Dropdown |
| ListBox | List selection |
| DataGrid | Tabular data |
| TreeView | Hierarchical data |
| TabControl | Tabbed interface |

---

## Data Binding

### Basic Binding

```xml
<TextBlock Text="{Binding UserName}"/>
<TextBox Text="{Binding SearchQuery, Mode=TwoWay}"/>
```

### Command Binding

```xml
<Button Content="Save" Command="{Binding SaveCommand}"/>
```

### Collection Binding

```xml
<ListBox ItemsSource="{Binding Items}">
    <ListBox.ItemTemplate>
        <DataTemplate>
            <TextBlock Text="{Binding Name}"/>
        </DataTemplate>
    </ListBox.ItemTemplate>
</ListBox>
```

---

## MVVM

### With CommunityToolkit.Mvvm

```csharp
public partial class MainViewModel : ObservableObject
{
    [ObservableProperty]
    private string userName = "";

    [ObservableProperty]
    private ObservableCollection<Item> items = new();

    [RelayCommand]
    private async Task Save()
    {
        // Save logic
    }
}
```

### ReactiveUI

Popular alternative for Avalonia.

```csharp
public class MainViewModel : ReactiveObject
{
    private string _userName;
    public string UserName
    {
        get => _userName;
        set => this.RaiseAndSetIfChanged(ref _userName, value);
    }

    public ReactiveCommand<Unit, Unit> SaveCommand { get; }
}
```

---

## Styling

### CSS-like Selectors

```xml
<Style Selector="Button">
    <Setter Property="Background" Value="#007ACC"/>
    <Setter Property="Foreground" Value="White"/>
</Style>

<Style Selector="Button:pointerover">
    <Setter Property="Background" Value="#005A9E"/>
</Style>

<Style Selector="Button.primary">
    <Setter Property="Background" Value="Green"/>
</Style>
```

### Themes

| Theme | Look |
|-------|------|
| Fluent | Windows 11 style |
| Simple | Minimal |
| Citrus | Colorful |

```xml
<Application.Styles>
    <FluentTheme />
</Application.Styles>
```

---

## Platform Integration

### Conditional Code

```csharp
if (OperatingSystem.IsWindows())
{
    // Windows-specific
}
else if (OperatingSystem.IsMacOS())
{
    // macOS-specific
}
```

### Native Interop

Access platform APIs via P/Invoke or platform-specific projects.

### File Dialogs

```csharp
var dialog = new OpenFileDialog
{
    AllowMultiple = true,
    Filters = new List<FileDialogFilter>
    {
        new() { Name = "Images", Extensions = { "png", "jpg" } }
    }
};

var result = await dialog.ShowAsync(window);
```

---

## Key Libraries

### UI

| Library | Purpose |
|---------|---------|
| Avalonia.Controls.DataGrid | Data grid |
| Avalonia.Controls.TreeDataGrid | Tree grid |
| AvaloniaEdit | Code editor |
| Dock.Avalonia | Docking panels |

### MVVM

| Library | Purpose |
|---------|---------|
| CommunityToolkit.Mvvm | Source generators |
| ReactiveUI | Reactive MVVM |

### Dialogs

| Library | Purpose |
|---------|---------|
| MessageBox.Avalonia | Message dialogs |
| Avalonia.Dialogs | File dialogs |

---

## Web (WASM)

Run Avalonia in browser.

```bash
dotnet new avalonia.xplat -n MyApp
# Includes Browser project
```

**Considerations:**

- Larger download (Skia + .NET runtime)
- Not for content sites (SEO)
- Good for app-like experiences

---

## Avalonia vs WPF

| Aspect | Avalonia | WPF |
|--------|----------|-----|
| Platforms | All | Windows only |
| Rendering | Skia | DirectX |
| XAML | Similar, some differences | Standard |
| Styles | CSS-like selectors | Triggers, templates |
| Maturity | Younger | Very mature |
| Controls | Growing | Extensive |

### Migration from WPF

1. XAML mostly works
2. Namespace changes
3. Style syntax differs
4. Some controls missing/different
5. Templates work similarly

---

## Avalonia vs MAUI

| Aspect | Avalonia | MAUI |
|--------|----------|------|
| Rendering | Custom (Skia) | Native controls |
| Linux | ✅ | ❌ |
| Web | ✅ | ❌ |
| Look & feel | Consistent | Native per platform |
| WPF familiarity | High | Medium |
| Microsoft support | Community | Official |

---

## When to Use Avalonia

**Strengths:**

- True cross-platform (including Linux)
- WPF developers feel at home
- Consistent look everywhere
- Web target available
- Active development

**Considerations:**

- Not native look per platform
- Smaller ecosystem than WPF
- Mobile still maturing

**Best for:**

- Linux desktop apps
- WPF skills → cross-platform
- Apps needing consistent UI
- JetBrains-style applications

---

## Related

- [[dotNET MAUI|.NET MAUI]]
- [[Electron]]
- [[Tauri]]
- [[C Sharp|C#]]
- [[Domains/Desktop Development|Desktop Development]]
