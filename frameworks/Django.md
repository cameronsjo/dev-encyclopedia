---
title: Django
aliases:
  - Django Framework
tags:
  - framework
  - backend
  - web
  - python
  - full-stack
type: reference
status: complete
created: '2025-11-28'
---

# Django

The web framework for perfectionists with deadlines.

## Overview

| Aspect | Details |
|--------|---------|
| Language | Python |
| Type | Full-stack framework |
| Architecture | MTV (Model-Template-View) |
| First release | 2005 |
| Backing | Django Software Foundation |
| Philosophy | Batteries included |

---

## Philosophy

### Batteries Included

Django includes everything out of the box:

- ORM
- Admin interface
- Authentication
- URL routing
- Template engine
- Form handling
- Security features

### DRY Principle

Don't Repeat Yourself. Define things once.

### Explicit Over Implicit

Clear code over magic. Python philosophy applied.

---

## Core Concepts

### MTV Architecture

| Django | Traditional MVC |
|--------|-----------------|
| Model | Model |
| Template | View |
| View | Controller |

```
URL → View → Model → Template → Response
```

### Project Structure

```
myproject/
├── manage.py
├── myproject/
│   ├── settings.py
│   ├── urls.py
│   ├── wsgi.py
│   └── asgi.py
└── myapp/
    ├── models.py
    ├── views.py
    ├── urls.py
    ├── admin.py
    ├── forms.py
    └── templates/
```

---

## Models & ORM

### Defining Models

```python
from django.db import models

class User(models.Model):
    name = models.CharField(max_length=100)
    email = models.EmailField(unique=True)
    created_at = models.DateTimeField(auto_now_add=True)
    is_active = models.BooleanField(default=True)

    def __str__(self):
        return self.name

class Post(models.Model):
    author = models.ForeignKey(User, on_delete=models.CASCADE, related_name='posts')
    title = models.CharField(max_length=200)
    content = models.TextField()
    published = models.BooleanField(default=False)
```

### QuerySet API

```python
# Create
user = User.objects.create(name='John', email='john@example.com')

# Read
User.objects.all()
User.objects.filter(is_active=True)
User.objects.get(id=1)
User.objects.exclude(is_active=False).order_by('-created_at')

# Update
User.objects.filter(id=1).update(name='Jane')

# Delete
User.objects.filter(id=1).delete()

# Chaining
User.objects.filter(is_active=True).exclude(name='Admin').order_by('name')[:10]
```

### Relationships

| Field | Relationship |
|-------|--------------|
| ForeignKey | Many-to-one |
| OneToOneField | One-to-one |
| ManyToManyField | Many-to-many |

---

## Views

### Function-Based Views

```python
from django.shortcuts import render, get_object_or_404

def user_list(request):
    users = User.objects.all()
    return render(request, 'users/list.html', {'users': users})

def user_detail(request, pk):
    user = get_object_or_404(User, pk=pk)
    return render(request, 'users/detail.html', {'user': user})
```

### Class-Based Views

```python
from django.views.generic import ListView, DetailView, CreateView

class UserListView(ListView):
    model = User
    template_name = 'users/list.html'
    context_object_name = 'users'
    paginate_by = 20

class UserDetailView(DetailView):
    model = User
    template_name = 'users/detail.html'

class UserCreateView(CreateView):
    model = User
    fields = ['name', 'email']
    success_url = '/users/'
```

### When to Use Which

| Use Case | Recommendation |
|----------|----------------|
| Simple CRUD | Class-based views |
| Complex logic | Function-based views |
| API endpoints | Django REST Framework |

---

## URL Routing

```python
# myproject/urls.py
from django.contrib import admin
from django.urls import path, include

urlpatterns = [
    path('admin/', admin.site.urls),
    path('users/', include('users.urls')),
    path('api/', include('api.urls')),
]

# users/urls.py
from django.urls import path
from . import views

urlpatterns = [
    path('', views.UserListView.as_view(), name='user-list'),
    path('<int:pk>/', views.UserDetailView.as_view(), name='user-detail'),
    path('create/', views.UserCreateView.as_view(), name='user-create'),
]
```

---

## Templates

### Django Template Language

```html
{% extends 'base.html' %}

{% block content %}
<h1>{{ user.name }}</h1>

{% if user.posts.exists %}
    <ul>
    {% for post in user.posts.all %}
        <li>{{ post.title|truncatewords:10 }}</li>
    {% endfor %}
    </ul>
{% else %}
    <p>No posts yet.</p>
{% endif %}

<a href="{% url 'user-edit' user.pk %}">Edit</a>
{% endblock %}
```

### Template Tags and Filters

| Tag/Filter | Purpose |
|------------|---------|
| {% for %} | Loop |
| {% if %} | Conditional |
| {% url %} | URL reverse |
| {% include %} | Include template |
| {{ var\|filter }} | Apply filter |
| {{ date\|date:"Y-m-d" }} | Format date |
| {{ text\|truncatewords:20 }} | Truncate text |

---

## Admin Interface

Built-in admin for data management.

```python
# admin.py
from django.contrib import admin
from .models import User, Post

@admin.register(User)
class UserAdmin(admin.ModelAdmin):
    list_display = ['name', 'email', 'is_active', 'created_at']
    list_filter = ['is_active', 'created_at']
    search_fields = ['name', 'email']

@admin.register(Post)
class PostAdmin(admin.ModelAdmin):
    list_display = ['title', 'author', 'published']
    list_filter = ['published']
```

**Key feature:** Automatic admin UI from models.

---

## Forms

### Model Forms

```python
from django import forms
from .models import User

class UserForm(forms.ModelForm):
    class Meta:
        model = User
        fields = ['name', 'email']
        widgets = {
            'email': forms.EmailInput(attrs={'class': 'form-control'}),
        }

    def clean_email(self):
        email = self.cleaned_data['email']
        if 'spam' in email:
            raise forms.ValidationError("Invalid email domain")
        return email
```

### In Views

```python
def user_create(request):
    if request.method == 'POST':
        form = UserForm(request.POST)
        if form.is_valid():
            user = form.save()
            return redirect('user-detail', pk=user.pk)
    else:
        form = UserForm()
    return render(request, 'users/form.html', {'form': form})
```

---

## Authentication

### Built-in

```python
from django.contrib.auth.decorators import login_required
from django.contrib.auth.mixins import LoginRequiredMixin

@login_required
def dashboard(request):
    return render(request, 'dashboard.html')

class DashboardView(LoginRequiredMixin, TemplateView):
    template_name = 'dashboard.html'
```

### User Model

```python
from django.contrib.auth.models import User

# Or custom user model
from django.contrib.auth.models import AbstractUser

class CustomUser(AbstractUser):
    bio = models.TextField(blank=True)
```

---

## Django REST Framework

API development toolkit.

```python
from rest_framework import serializers, viewsets

class UserSerializer(serializers.ModelSerializer):
    class Meta:
        model = User
        fields = ['id', 'name', 'email']

class UserViewSet(viewsets.ModelViewSet):
    queryset = User.objects.all()
    serializer_class = UserSerializer
```

### Features

| Feature | Description |
|---------|-------------|
| Serializers | Data validation & transformation |
| ViewSets | CRUD endpoints |
| Routers | Automatic URL routing |
| Authentication | Token, JWT, OAuth |
| Pagination | Built-in |
| Browsable API | Interactive docs |

---

## Migrations

```bash
python manage.py makemigrations
python manage.py migrate
python manage.py showmigrations
python manage.py migrate app_name 0001  # Rollback
```

### Migration File

```python
from django.db import migrations, models

class Migration(migrations.Migration):
    dependencies = [
        ('myapp', '0001_initial'),
    ]

    operations = [
        migrations.AddField(
            model_name='user',
            name='bio',
            field=models.TextField(blank=True),
        ),
    ]
```

---

## Testing

```python
from django.test import TestCase, Client

class UserTestCase(TestCase):
    def setUp(self):
        self.user = User.objects.create(name='John', email='john@example.com')

    def test_user_creation(self):
        self.assertEqual(self.user.name, 'John')

    def test_user_list_view(self):
        client = Client()
        response = client.get('/users/')
        self.assertEqual(response.status_code, 200)
        self.assertContains(response, 'John')
```

### Testing Tools

| Tool | Purpose |
|------|---------|
| TestCase | Database testing |
| SimpleTestCase | No database |
| Client | HTTP testing |
| pytest-django | pytest integration |

---

## Async Support

Django 4.1+ has async views.

```python
async def async_view(request):
    data = await sync_to_async(expensive_operation)()
    return JsonResponse(data)
```

### ASGI

```python
# asgi.py
from django.core.asgi import get_asgi_application
application = get_asgi_application()
```

---

## Performance

### QuerySet Optimization

```python
# Select related (ForeignKey)
Post.objects.select_related('author').all()

# Prefetch related (reverse FK, M2M)
User.objects.prefetch_related('posts').all()

# Only/defer
User.objects.only('name', 'email')
User.objects.defer('bio')
```

### Caching

```python
from django.core.cache import cache

# Low-level
cache.set('my_key', 'value', 300)
value = cache.get('my_key')

# View caching
from django.views.decorators.cache import cache_page

@cache_page(60 * 15)
def my_view(request):
    ...
```

---

## Deployment

### Production Checklist

```bash
python manage.py check --deploy
```

| Setting | Production Value |
|---------|------------------|
| DEBUG | False |
| SECRET_KEY | From environment |
| ALLOWED_HOSTS | Your domains |
| SECURE_SSL_REDIRECT | True |
| SESSION_COOKIE_SECURE | True |

### Platforms

| Platform | Notes |
|----------|-------|
| Heroku | Easy deployment |
| Railway | Modern PaaS |
| DigitalOcean | App Platform |
| AWS/GCP | Full control |

---

## Django vs Alternatives

| Aspect | Django | Flask | FastAPI |
|--------|--------|-------|---------|
| Type | Full-stack | Micro | API-focused |
| Batteries | Included | Choose your own | Minimal |
| ORM | Django ORM | SQLAlchemy | SQLAlchemy |
| Admin | Built-in | Add-on | None |
| Async | Added (4.x) | Via extensions | Native |
| Learning curve | Steeper | Easier | Moderate |

---

## Key Packages

| Package | Purpose |
|---------|---------|
| Django REST Framework | APIs |
| Celery | Background tasks |
| django-allauth | Authentication |
| django-debug-toolbar | Debugging |
| django-extensions | Management commands |
| whitenoise | Static files |

---

## When to Use Django

**Strengths:**

- Batteries included
- Excellent admin interface
- Strong security defaults
- Great documentation
- Large community
- Python ecosystem

**Considerations:**

- Monolithic (microservices harder)
- ORM limitations for complex queries
- Sync-first (async added later)

**Best for:**

- Content-heavy sites
- Admin-heavy applications
- Rapid prototyping
- Teams with Python experience
- When you need an admin interface

---

## Related

- [[Python]]
- [[Rails]]
- [[Laravel]]
- [[Spring Boot]]
- [[domains/Web Development|Web Development]]
