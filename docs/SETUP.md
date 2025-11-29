# Setup Guide

## Quick Start

1. Clone the repo
2. Open folder as vault in Obsidian
3. Enable community plugins (Settings → Community plugins → Turn on)
4. Install the configured plugins (they're listed, just need to install)

## Configured Plugins

The vault is pre-configured for these community plugins:

| Plugin | Purpose | Auto-configured |
|--------|---------|-----------------|
| **Local REST API** | Secure API access to vault | ✅ Port 27123, auth enabled |
| **MCP Tools** | Claude/AI integration via MCP | Install needed |
| **Smart Connections** | Local AI chat, semantic search | Install needed |
| **Dataview** | Query vault like a database | ✅ JS enabled |
| **Templater** | Advanced templates | ✅ Points to templates/ |

## Installing Plugins

1. Settings → Community plugins → Browse
2. Search for each plugin name
3. Install and Enable

Or use the Obsidian URI:
```
obsidian://show-plugin?id=obsidian-local-rest-api
obsidian://show-plugin?id=dataview
obsidian://show-plugin?id=templater-obsidian
obsidian://show-plugin?id=smart-connections
```

## Local REST API

Once installed, the API runs at `https://localhost:27123`.

### Get API Key

Settings → Local REST API → Copy API Key

### Test Connection

```bash
curl -k https://localhost:27123/ \
  -H "Authorization: Bearer YOUR_API_KEY"
```

### Endpoints

| Endpoint | Description |
|----------|-------------|
| GET /vault/ | List files |
| GET /vault/{path} | Read file |
| PUT /vault/{path} | Write file |
| POST /search/simple/ | Search vault |
| GET /active/ | Get active file |

Full docs: https://coddingtonbear.github.io/obsidian-local-rest-api/

## MCP Integration

### For Claude Desktop

Add to `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "obsidian": {
      "command": "npx",
      "args": ["-y", "obsidian-mcp"],
      "env": {
        "OBSIDIAN_API_KEY": "your-api-key",
        "OBSIDIAN_HOST": "https://localhost:27123"
      }
    }
  }
}
```

### For Claude Code

The MCP Tools plugin exposes your vault via WebSocket. Configure in Claude Code's MCP settings.

## Smart Connections

After installing:

1. Settings → Smart Connections
2. Choose embedding model (local or API)
3. Build initial embeddings (may take a few minutes)
4. Use command palette: "Smart Connections: Open Chat"

## Dataview Examples

Already used throughout the vault. Examples:

```dataview
TABLE status, type
FROM ""
WHERE type = "reference"
SORT file.name ASC
```

```dataview
LIST
FROM #framework AND #web
```
