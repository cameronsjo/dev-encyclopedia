---
title: MCP Servers
aliases:
  - Model Context Protocol
  - MCP
tags:
  - ai
  - infrastructure
  - protocol
  - agents
type: reference
status: complete
created: '2025-11-28'
---

# MCP Servers

Model Context Protocol - standardized way for AI models to access external tools and data.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Connect AI models to tools, data, and services |
| Created by | Anthropic |
| Transport | stdio, HTTP/SSE |
| Format | JSON-RPC 2.0 |
| SDKs | TypeScript, Python |

---

## What is MCP?

```
┌─────────────────┐     MCP Protocol     ┌─────────────────┐
│                 │◄───────────────────►│                 │
│   AI Client     │     JSON-RPC 2.0    │   MCP Server    │
│  (Claude, etc)  │                     │   (Your tools)  │
│                 │                     │                 │
└─────────────────┘                     └─────────────────┘
        │                                       │
        │                                       │
        ▼                                       ▼
   Sends requests                      Exposes capabilities:
   for tools/data                      - Tools (functions)
                                       - Resources (data)
                                       - Prompts (templates)
```

### Why MCP?

| Before MCP | With MCP |
|------------|----------|
| Custom integrations per AI | Standardized protocol |
| Vendor lock-in | Works with any MCP client |
| Reinvent auth, transport | Built-in patterns |
| Per-tool documentation | Discoverable schemas |

---

## Core Concepts

### Capabilities

| Capability | Description | Example |
|------------|-------------|---------|
| **Tools** | Functions the AI can call | `search_files`, `run_query` |
| **Resources** | Data the AI can read | Files, database records |
| **Prompts** | Reusable prompt templates | Code review template |

### Tools

Functions with typed parameters.

```typescript
{
  name: "search_notes",
  description: "Search notes by query",
  inputSchema: {
    type: "object",
    properties: {
      query: { type: "string", description: "Search query" },
      limit: { type: "number", default: 10 }
    },
    required: ["query"]
  }
}
```

### Resources

Data exposed to the AI.

```typescript
{
  uri: "file:///path/to/document.md",
  name: "Project README",
  mimeType: "text/markdown"
}
```

### Prompts

Reusable templates.

```typescript
{
  name: "code_review",
  description: "Review code for issues",
  arguments: [
    { name: "code", description: "Code to review", required: true }
  ]
}
```

---

## Building MCP Servers

### TypeScript SDK

```typescript
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";

const server = new McpServer({
  name: "my-server",
  version: "1.0.0"
});

// Add a tool
server.tool(
  "get_weather",
  "Get weather for a city",
  {
    city: { type: "string", description: "City name" }
  },
  async ({ city }) => {
    const weather = await fetchWeather(city);
    return { content: [{ type: "text", text: JSON.stringify(weather) }] };
  }
);

// Add a resource
server.resource(
  "config",
  "file:///config.json",
  "Application configuration",
  "application/json",
  async () => ({ text: JSON.stringify(config) })
);

// Start server
const transport = new StdioServerTransport();
await server.connect(transport);
```

### Python SDK

```python
from mcp.server import Server
from mcp.server.stdio import stdio_server

server = Server("my-server")

@server.tool()
async def get_weather(city: str) -> str:
    """Get weather for a city."""
    weather = await fetch_weather(city)
    return json.dumps(weather)

@server.resource("file:///config.json")
async def get_config() -> str:
    """Application configuration."""
    return json.dumps(config)

async def main():
    async with stdio_server() as (read, write):
        await server.run(read, write)
```

---

## Transport Methods

### stdio (Default)

Server runs as subprocess, communicates via stdin/stdout.

```json
{
  "mcpServers": {
    "my-server": {
      "command": "node",
      "args": ["server.js"]
    }
  }
}
```

### HTTP/SSE

Server runs as HTTP service.

```json
{
  "mcpServers": {
    "my-server": {
      "url": "http://localhost:3000/mcp"
    }
  }
}
```

| Transport | Use Case |
|-----------|----------|
| stdio | Local tools, simple setup |
| HTTP/SSE | Remote servers, shared access |

---

## Popular MCP Servers

### Official (Anthropic)

| Server | Purpose |
|--------|---------|
| filesystem | Read/write local files |
| git | Git operations |
| github | GitHub API access |
| postgres | PostgreSQL queries |
| sqlite | SQLite database |
| puppeteer | Browser automation |
| brave-search | Web search |

### Community

| Server | Purpose |
|--------|---------|
| obsidian-mcp | Obsidian vault access |
| notion-mcp | Notion integration |
| slack-mcp | Slack messages |
| linear-mcp | Linear issues |
| mcp-server-fetch | HTTP requests |

---

## Client Configuration

### Claude Desktop

```json
// ~/Library/Application Support/Claude/claude_desktop_config.json
{
  "mcpServers": {
    "filesystem": {
      "command": "npx",
      "args": ["-y", "@anthropic/mcp-server-filesystem", "/path/to/dir"]
    },
    "github": {
      "command": "npx",
      "args": ["-y", "@anthropic/mcp-server-github"],
      "env": {
        "GITHUB_TOKEN": "ghp_..."
      }
    }
  }
}
```

### Claude Code

```json
// .mcp.json in project root
{
  "mcpServers": {
    "project-db": {
      "command": "python",
      "args": ["mcp_server.py"],
      "cwd": "./tools"
    }
  }
}
```

---

## Security Considerations

### Best Practices

| Practice | Why |
|----------|-----|
| Principle of least privilege | Only expose necessary capabilities |
| Input validation | Sanitize all tool inputs |
| Rate limiting | Prevent abuse |
| Audit logging | Track tool usage |
| Scoped permissions | Limit file/network access |

### Dangerous Patterns

```typescript
// ❌ BAD - arbitrary code execution
server.tool("eval", async ({ code }) => eval(code));

// ❌ BAD - unrestricted file access
server.tool("read", async ({ path }) => fs.readFile(path));

// ✅ GOOD - scoped to allowed directory
server.tool("read", async ({ filename }) => {
  const safePath = path.join(ALLOWED_DIR, path.basename(filename));
  return fs.readFile(safePath);
});
```

---

## MCP vs Alternatives

| Aspect | MCP | OpenAI Functions | LangChain Tools |
|--------|-----|------------------|-----------------|
| Standardized | ✅ Protocol spec | ❌ OpenAI only | ❌ Framework only |
| Discovery | ✅ Runtime | ❌ Compile time | ❌ Compile time |
| Resources | ✅ First-class | ❌ No | ❌ Retrievers |
| Transport | stdio, HTTP | HTTP only | In-process |
| Multi-client | ✅ | ❌ | ❌ |

---

## When to Use MCP

**Strengths:**

- Standardized protocol (not vendor-locked)
- Runtime capability discovery
- Clean separation of concerns
- Growing ecosystem
- Works with Claude Desktop, Claude Code

**Considerations:**

- Anthropic-originated (adoption growing)
- Newer than alternatives
- Requires MCP-compatible client

**Best for:**

- Building AI tools for Claude
- Multi-tool integrations
- Sharing tools across projects
- Local-first AI workflows

---

## Related

- [[RAG]]
- [[Agent Frameworks]]
- [[LLMs & Transformers]]
