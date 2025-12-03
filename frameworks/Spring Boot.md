---
title: Spring Boot
aliases:
  - Spring
  - Spring Framework
tags:
  - framework
  - backend
  - web
  - java
  - kotlin
  - enterprise
type: reference
status: complete
created: '2025-11-28'
---

# Spring Boot

Opinionated, production-ready Spring-based applications.

## Overview

| Aspect | Details |
|--------|---------|
| Languages | Java, Kotlin |
| Type | Backend framework |
| Architecture | Convention over configuration |
| First release | 2014 |
| Backing | VMware (Broadcom) |
| Base | Spring Framework |

---

## Spring Boot vs Spring Framework

| Spring Framework | Spring Boot |
|------------------|-------------|
| Powerful but complex | Opinionated defaults |
| Manual configuration | Auto-configuration |
| XML/JavaConfig | Starter dependencies |
| War deployment | Embedded servers |

**Spring Boot = Spring Framework + Sensible Defaults + Embedded Server**

---

## Core Concepts

### Auto-Configuration

Spring Boot configures beans based on classpath.

```java
@SpringBootApplication
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
```

`@SpringBootApplication` combines:

- `@Configuration` — Config class
- `@EnableAutoConfiguration` — Auto-configure
- `@ComponentScan` — Find beans

### Starter Dependencies

Pre-configured dependency bundles.

| Starter | Purpose |
|---------|---------|
| spring-boot-starter-web | Web applications |
| spring-boot-starter-data-jpa | JPA + Hibernate |
| spring-boot-starter-security | Authentication/authorization |
| spring-boot-starter-test | Testing utilities |
| spring-boot-starter-actuator | Production monitoring |

---

## Web Development

### REST Controllers

```java
@RestController
@RequestMapping("/api/users")
public class UserController {

    @GetMapping
    public List<User> getAll() {
        return userService.findAll();
    }

    @GetMapping("/{id}")
    public User getById(@PathVariable Long id) {
        return userService.findById(id);
    }

    @PostMapping
    public User create(@RequestBody @Valid UserDto dto) {
        return userService.create(dto);
    }
}
```

### Request Mapping

| Annotation | HTTP Method |
|------------|-------------|
| @GetMapping | GET |
| @PostMapping | POST |
| @PutMapping | PUT |
| @DeleteMapping | DELETE |
| @PatchMapping | PATCH |

### Validation

```java
public class UserDto {
    @NotBlank
    private String name;

    @Email
    private String email;

    @Min(18)
    private int age;
}
```

---

## Data Access

### Spring Data JPA

```java
@Entity
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    private String name;
    private String email;
}

public interface UserRepository extends JpaRepository<User, Long> {
    List<User> findByName(String name);
    Optional<User> findByEmail(String email);

    @Query("SELECT u FROM User u WHERE u.createdAt > :date")
    List<User> findRecentUsers(@Param("date") LocalDateTime date);
}
```

### Query Methods

Spring Data derives queries from method names.

| Method | Generated Query |
|--------|-----------------|
| findByName | WHERE name = ? |
| findByNameAndEmail | WHERE name = ? AND email = ? |
| findByAgeBetween | WHERE age BETWEEN ? AND ? |
| findByNameContaining | WHERE name LIKE %?% |
| countByStatus | SELECT COUNT(*) WHERE status = ? |

---

## Dependency Injection

### Core Pattern

```java
@Service
public class UserService {
    private final UserRepository userRepository;

    // Constructor injection (preferred)
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }
}
```

### Bean Scopes

| Scope | Lifecycle |
|-------|-----------|
| singleton | One per container (default) |
| prototype | New instance per request |
| request | One per HTTP request |
| session | One per HTTP session |

---

## Configuration

### application.properties / application.yml

```yaml
server:
  port: 8080

spring:
  datasource:
    url: jdbc:postgresql://localhost:5432/mydb
    username: ${DB_USER}
    password: ${DB_PASS}
  jpa:
    hibernate:
      ddl-auto: validate
    show-sql: false

logging:
  level:
    root: INFO
    com.myapp: DEBUG
```

### Profiles

Environment-specific configuration.

```yaml
# application-dev.yml
spring:
  jpa:
    show-sql: true

# application-prod.yml
spring:
  jpa:
    show-sql: false
```

Activate: `--spring.profiles.active=dev`

---

## Security

### Spring Security

```java
@Configuration
@EnableWebSecurity
public class SecurityConfig {

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) {
        return http
            .authorizeHttpRequests(auth -> auth
                .requestMatchers("/api/public/**").permitAll()
                .requestMatchers("/api/admin/**").hasRole("ADMIN")
                .anyRequest().authenticated()
            )
            .oauth2ResourceServer(OAuth2ResourceServerConfigurer::jwt)
            .build();
    }
}
```

### Authentication Methods

| Method | Use Case |
|--------|----------|
| Basic Auth | Simple, internal APIs |
| JWT | Stateless APIs |
| OAuth2 | Third-party auth |
| SAML | Enterprise SSO |

---

## Testing

### Test Slices

| Annotation | Tests |
|------------|-------|
| @SpringBootTest | Full context |
| @WebMvcTest | Controllers only |
| @DataJpaTest | Repository layer |
| @MockBean | Mock dependencies |

```java
@WebMvcTest(UserController.class)
class UserControllerTest {

    @Autowired
    MockMvc mockMvc;

    @MockBean
    UserService userService;

    @Test
    void shouldReturnUser() throws Exception {
        when(userService.findById(1L)).thenReturn(new User("John"));

        mockMvc.perform(get("/api/users/1"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.name").value("John"));
    }
}
```

---

## Production Features

### Actuator

Monitoring and management endpoints.

| Endpoint | Purpose |
|----------|---------|
| /actuator/health | Health check |
| /actuator/info | App information |
| /actuator/metrics | Application metrics |
| /actuator/env | Environment properties |

### Observability

| Integration | Purpose |
|-------------|---------|
| Micrometer | Metrics facade |
| OpenTelemetry | Distributed tracing |
| Logback/Log4j2 | Structured logging |

---

## Modern Spring (3.x)

### Native Compilation (GraalVM)

AOT compilation for faster startup.

```bash
./mvnw -Pnative native:compile
```

| Comparison | JVM | Native |
|------------|-----|--------|
| Startup | 2-5 seconds | 50-100ms |
| Memory | Higher | Lower |
| Throughput | Optimized over time | Consistent |

### Virtual Threads (Java 21)

Project Loom support for high concurrency.

```yaml
spring:
  threads:
    virtual:
      enabled: true
```

### Records & Immutability

```java
public record UserDto(String name, String email) {}
```

---

## Spring Boot vs Alternatives

| Aspect | Spring Boot | Quarkus | Micronaut |
|--------|-------------|---------|-----------|
| Maturity | Most mature | Growing | Growing |
| Native support | Good (3.x) | First-class | First-class |
| Startup time | Slower (JVM) | Faster | Faster |
| Memory | Higher | Lower | Lower |
| Ecosystem | Massive | Growing | Growing |
| Learning resources | Abundant | Moderate | Moderate |

---

## Key Libraries

### Ecosystem

| Library | Purpose |
|---------|---------|
| Spring Data | Data access |
| Spring Security | Authentication |
| Spring Cloud | Microservices |
| Spring Batch | Batch processing |
| Spring Integration | Enterprise integration |

### Third-Party

| Library | Purpose |
|---------|---------|
| Lombok | Boilerplate reduction |
| MapStruct | Object mapping |
| Flyway/Liquibase | Database migrations |
| OpenAPI | API documentation |

---

## When to Use Spring Boot

**Strengths:**

- Massive ecosystem
- Excellent documentation
- Enterprise proven
- Strong tooling (IDE support)
- GraalVM native support
- Kotlin support

**Considerations:**

- Steeper learning curve
- Memory footprint (JVM)
- Magic via annotations
- Slower startup (vs native-first)

**Best for:**

- Enterprise applications
- Teams with Java experience
- Complex business logic
- Microservices (Spring Cloud)
- Long-running services

---

## Related

- [[Java]]
- [[Kotlin]]
- [[Rails]]
- [[Laravel]]
- [[Django]]
- [[domains/Web Development|Web Development]]
