---
title: MCP Registry
aliases:
  - Model Context Protocol Registry
  - MCP Server Discovery
  - MCP Catalog
tags:
  - ai
  - mcp
  - infrastructure
  - tooling
type: reference
status: complete
created: "2025-11-30"
---

# MCP Registry

Centralized discovery and cataloging systems for Model Context Protocol (MCP) servers, enabling automated discovery, capability advertisement, and version management.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Discovery, cataloging, and metadata management for MCP servers |
| **Protocol** | Model Context Protocol (MCP) specification |
| **Key Function** | Server registration, capability advertisement, version resolution |
| **Primary Use** | AI agent tooling, LLM application development, developer tooling |
| **Ecosystem** | Smithery, mcp.run, awesome-mcp-servers, custom registries |

## Core Concepts

### Server Registration

Registries maintain metadata about available MCP servers including:

- **Server Identity**: Name, version, maintainer, repository
- **Capabilities**: Tools, resources, prompts offered by the server
- **Requirements**: Runtime dependencies, authentication needs, configuration
- **Compatibility**: MCP protocol version, supported platforms

### Discovery Mechanisms

**Public Registries**: Community-maintained catalogs (Smithery, mcp.run)

- Searchable by capability, domain, language
- Version history and changelog tracking
- Usage statistics and community ratings

**Private Registries**: Organization-internal catalogs

- Custom approval workflows
- Enterprise authentication integration
- Compliance and security scanning

**Dynamic Discovery**: Runtime server enumeration

- Server announces capabilities on connection
- Client queries available tools/resources
- No pre-registration required

### Metadata Schema

```yaml
name: server-name
version: 1.2.3
description: Brief server description
author: Maintainer name
repository: https://github.com/org/repo
mcp_version: "2024-11-05"
capabilities:
  tools:
    - name: tool_name
      description: What the tool does
      input_schema: {...}
  resources:
    - uri_template: resource://{path}
      name: Resource type
      mime_type: application/json
authentication:
  type: api_key | oauth2 | none
  required: true
platform:
  - linux
  - darwin
  - win32
runtime:
  type: node | python | docker
  version: ">=18.0.0"
```

## Registry Patterns

### Centralized Registry

**Architecture**: Single authoritative source

| Aspect | Details |
|--------|---------|
| **Discovery** | Search/browse central catalog |
| **Updates** | Push to registry on release |
| **Versioning** | Semantic versioning with tags |
| **Benefits** | Single source of truth, consistent metadata |
| **Challenges** | Single point of failure, approval bottlenecks |

### Federated Registry

**Architecture**: Multiple registries with aggregation

| Aspect | Details |
|--------|---------|
| **Discovery** | Query multiple registries, merge results |
| **Updates** | Each registry manages own subset |
| **Versioning** | Per-registry version management |
| **Benefits** | Resilience, organizational autonomy |
| **Challenges** | Naming conflicts, inconsistent metadata |

### Package Manager Integration

**Architecture**: Leverage existing package ecosystems

| Aspect | Details |
|--------|---------|
| **Discovery** | npm, PyPI, crates.io, etc. |
| **Updates** | Standard package publishing flow |
| **Versioning** | Package manager semantics |
| **Benefits** | Familiar tooling, existing infrastructure |
| **Challenges** | MCP-specific metadata may be limited |

## Existing Registries

### Comparison Matrix

| Registry | Type | Scope | Features | Maintenance |
|----------|------|-------|----------|-------------|
| **Smithery** | Public catalog | Community servers | Web UI, search, version tracking | Active |
| **mcp.run** | Public catalog | Curated collection | Direct server URLs, minimal metadata | Active |
| **awesome-mcp-servers** | GitHub list | Community curation | README-based, links to repos | Community-driven |
| **Custom/Private** | Organization-specific | Internal tools | Custom workflows, compliance | Self-managed |

### Smithery

**Focus**: Comprehensive community registry

- Web-based discovery and search
- Server submission and approval workflow
- Version history and release notes
- Integration examples and documentation
- Usage analytics and trending servers

**Best for**: Public server discovery, community-driven catalog

### mcp.run

**Focus**: Lightweight server catalog

- Direct server connection URLs
- Minimal registration overhead
- Quick discovery for prototyping
- No approval process

**Best for**: Rapid prototyping, simple server lookup

### awesome-mcp-servers

**Focus**: Curated GitHub repository

- README-based categorization
- Community contributions via PR
- Links to source repositories
- No hosted infrastructure required

**Best for**: Developer-centric discovery, GitHub-native workflow

## Tool Schema Advertisement

Registries expose tool schemas for static analysis and validation:

**Schema Structure**:

- Tool name and description
- Input parameter types and constraints
- Output format specification
- Required vs optional parameters
- Examples and usage patterns

**Use Cases**:

- IDE autocomplete and validation
- Client-side validation before invocation
- Documentation generation
- Test case generation

## Resource Discovery

Registries catalog available resource types:

**Resource Metadata**:

- URI template patterns
- MIME types and formats
- Access control requirements
- Freshness/caching hints
- Rate limiting information

**Discovery Flow**:

1. Client queries registry for resource-providing servers
2. Registry returns matching servers with resource templates
3. Client connects to server and requests specific resources
4. Server validates access and returns resource content

## Authentication & Authorization

### Registry Access Control

| Pattern | Description | Use Case |
|---------|-------------|----------|
| **Public read** | Anyone can browse/search | Community registries |
| **Authenticated read** | Login required to view | Organization-internal catalogs |
| **Role-based write** | Permissions for server registration | Approval workflows |
| **API key access** | Programmatic registry queries | CI/CD, automation |

### Server Authentication Requirements

Registries document server auth needs:

- **None**: Open servers, no credentials
- **API Key**: Static token required
- **OAuth2**: Dynamic token exchange
- **Custom**: Server-specific auth flow

## Versioning & Compatibility

### Semantic Versioning

Registries enforce version semantics:

- **Major**: Breaking changes to tool/resource interfaces
- **Minor**: Backward-compatible feature additions
- **Patch**: Bug fixes, no interface changes

### MCP Protocol Compatibility

Servers declare MCP version compatibility:

```yaml
mcp_version: "2024-11-05"
compatible_versions:
  - "2024-11-05"
  - "2024-10-15"  # backward compatible
```

Clients filter servers by protocol support.

### Deprecation Management

Registries track server lifecycle:

- **Active**: Current, maintained
- **Deprecated**: Superseded, still functional
- **Archived**: No longer maintained
- **Sunset**: Scheduled for removal

## Registry Implementation Patterns

### Static Catalog (JSON/YAML)

**Approach**: Version-controlled file with server metadata

✅ Simple, no infrastructure
✅ Git-based versioning
❌ No dynamic updates
❌ Limited search capabilities

### API-Based Registry

**Approach**: RESTful API for server CRUD operations

✅ Dynamic updates
✅ Rich search/filtering
✅ Analytics and metrics
❌ Infrastructure overhead
❌ Requires hosting

### Blockchain/Distributed Ledger

**Approach**: Decentralized registry with cryptographic verification

✅ No central authority
✅ Tamper-resistant
❌ Complexity
❌ Performance overhead

## Best Practices

### For Registry Operators

- **Schema validation**: Enforce consistent metadata structure
- **Security scanning**: Check servers for known vulnerabilities
- **Provenance tracking**: Link to source repositories, verify maintainers
- **Deprecation notices**: Communicate server lifecycle changes
- **Performance metrics**: Track server reliability and response times

### For Server Publishers

- **Complete metadata**: Provide thorough descriptions, examples
- **Semantic versioning**: Follow version semantics strictly
- **Changelog maintenance**: Document changes between versions
- **Deprecation warnings**: Give users advance notice of breaking changes
- **Security disclosures**: Communicate vulnerabilities promptly

### For Registry Consumers

- **Pin versions**: Avoid automatic updates without testing
- **Monitor deprecations**: Subscribe to server lifecycle notifications
- **Cache metadata**: Reduce registry queries in production
- **Fallback registries**: Configure multiple registry sources
- **Verify authenticity**: Check server provenance before use

## When to Use

### Strengths

- **Centralized discovery**: Find servers by capability, domain, language
- **Version management**: Track releases, handle compatibility
- **Metadata standardization**: Consistent server descriptions, schemas
- **Community building**: Share and discover community contributions
- **Security**: Centralized vulnerability scanning and disclosure

### Considerations

- **Centralization risk**: Registry downtime affects discovery
- **Approval overhead**: Registration workflows may slow releases
- **Metadata staleness**: Registry may lag actual server state
- **Trust model**: Must trust registry operators and publishers

### Best For

- **Large ecosystems**: Many servers, diverse capabilities
- **Enterprise adoption**: Compliance, approval workflows
- **Public discovery**: Community-driven server sharing
- **Version coordination**: Managing compatibility across clients/servers

## Related

- [[MCP Servers]]
- [[Agent Frameworks]]
- [[LLM Tool Use]]
- [[API Design]]
