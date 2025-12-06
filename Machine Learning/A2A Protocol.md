---
title: A2A Protocol
aliases:
  - Agent-to-Agent Protocol
  - Agent2Agent
  - Google A2A
tags:
  - ai
  - agents
  - protocol
  - infrastructure
type: reference
status: complete
created: "2025-11-30"
---

# A2A Protocol

Google's open protocol for agent-to-agent communication and interoperability.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Enable AI agents to communicate and collaborate |
| Created by | Google (2025) |
| Transport | HTTP, JSON-RPC |
| Key feature | Cross-framework agent interoperability |
| Relationship | Complementary to MCP (tools), A2A (agents) |

---

## Why A2A?

### The Agent Interoperability Problem

```
Without A2A:
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│ LangChain   │  ?  │  AutoGen    │  ?  │  CrewAI     │
│   Agent     │◄───►│   Agent     │◄───►│   Agent     │
└─────────────┘     └─────────────┘     └─────────────┘
     Custom integrations for each pair

With A2A:
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│ LangChain   │     │  AutoGen    │     │  CrewAI     │
│   Agent     │     │   Agent     │     │   Agent     │
└──────┬──────┘     └──────┬──────┘     └──────┬──────┘
       │                   │                   │
       └───────────────────┼───────────────────┘
                           │
                    ┌──────▼──────┐
                    │ A2A Protocol │
                    └─────────────┘
```

### A2A vs MCP

| Aspect | MCP | A2A |
|--------|-----|-----|
| Purpose | Connect agents to **tools** | Connect **agents** to agents |
| Communication | Agent ↔ Tool/Resource | Agent ↔ Agent |
| Pattern | Client-server | Peer-to-peer |
| Discovery | Capability-based | Agent Card |
| Created by | Anthropic | Google |

**Together:** MCP gives agents tools, A2A lets agents collaborate.

---

## Core Concepts

### Agent Card

JSON document describing an agent's capabilities and endpoints.

```json
{
  "name": "research-assistant",
  "description": "An agent that searches and synthesizes information",
  "version": "1.0.0",
  "url": "https://agents.example.com/research",
  "capabilities": {
    "streaming": true,
    "pushNotifications": false,
    "stateTransitionHistory": true
  },
  "skills": [
    {
      "id": "web-search",
      "name": "Web Search",
      "description": "Search the web for current information",
      "inputSchema": {
        "type": "object",
        "properties": {
          "query": { "type": "string" },
          "maxResults": { "type": "integer", "default": 10 }
        },
        "required": ["query"]
      }
    },
    {
      "id": "summarize",
      "name": "Summarize Content",
      "description": "Summarize documents or web pages",
      "inputSchema": {
        "type": "object",
        "properties": {
          "content": { "type": "string" },
          "maxLength": { "type": "integer" }
        },
        "required": ["content"]
      }
    }
  ],
  "authentication": {
    "schemes": ["bearer", "oauth2"]
  }
}
```

### Tasks

Unit of work between agents.

```json
{
  "id": "task-123",
  "skill": "web-search",
  "input": {
    "query": "latest AI agent protocols 2025",
    "maxResults": 5
  },
  "state": "working",
  "history": [
    {
      "state": "submitted",
      "timestamp": "2025-01-15T10:00:00Z"
    },
    {
      "state": "working",
      "timestamp": "2025-01-15T10:00:01Z",
      "message": "Searching web..."
    }
  ]
}
```

### Task States

```
                    ┌─────────────┐
                    │  submitted  │
                    └──────┬──────┘
                           │
                    ┌──────▼──────┐
              ┌─────│   working   │─────┐
              │     └──────┬──────┘     │
              │            │            │
       ┌──────▼──────┐    │     ┌──────▼──────┐
       │ input-needed│    │     │   failed    │
       └──────┬──────┘    │     └─────────────┘
              │           │
              └───────────┤
                          │
                   ┌──────▼──────┐
                   │  completed  │
                   └─────────────┘
```

| State | Description |
|-------|-------------|
| `submitted` | Task received, not started |
| `working` | Agent is processing |
| `input-needed` | Waiting for more information |
| `completed` | Task finished successfully |
| `failed` | Task failed |
| `canceled` | Task was canceled |

### Artifacts

Results produced by tasks.

```json
{
  "taskId": "task-123",
  "artifacts": [
    {
      "id": "artifact-1",
      "type": "text/markdown",
      "name": "search-results.md",
      "content": "## Search Results\n\n1. **A2A Protocol**..."
    },
    {
      "id": "artifact-2",
      "type": "application/json",
      "name": "raw-results.json",
      "content": "{\"results\": [...]}"
    }
  ]
}
```

---

## Protocol Operations

### Discovery

Find agent capabilities.

```http
GET /.well-known/agent.json HTTP/1.1
Host: agents.example.com

HTTP/1.1 200 OK
Content-Type: application/json

{
  "name": "research-assistant",
  "skills": [...],
  ...
}
```

### Send Task

Request work from an agent.

```http
POST /tasks HTTP/1.1
Host: agents.example.com
Content-Type: application/json
Authorization: Bearer <token>

{
  "skill": "web-search",
  "input": {
    "query": "A2A protocol specification"
  }
}
```

```http
HTTP/1.1 201 Created
Content-Type: application/json

{
  "id": "task-456",
  "state": "submitted"
}
```

### Get Task Status

Poll for task completion.

```http
GET /tasks/task-456 HTTP/1.1
Host: agents.example.com

HTTP/1.1 200 OK
Content-Type: application/json

{
  "id": "task-456",
  "state": "completed",
  "artifacts": [...]
}
```

### Streaming (SSE)

Real-time updates for long-running tasks.

```http
GET /tasks/task-456/events HTTP/1.1
Host: agents.example.com
Accept: text/event-stream

HTTP/1.1 200 OK
Content-Type: text/event-stream

event: state
data: {"state": "working", "message": "Searching..."}

event: progress
data: {"percent": 50, "message": "Found 3 results"}

event: state
data: {"state": "completed"}

event: artifact
data: {"id": "artifact-1", "type": "text/markdown", ...}
```

### Provide Input

Respond to `input-needed` state.

```http
POST /tasks/task-456/input HTTP/1.1
Content-Type: application/json

{
  "clarification": "Focus on academic papers only"
}
```

---

## Architecture Patterns

### Direct Agent-to-Agent

```
┌─────────────┐         ┌─────────────┐
│  Agent A    │◄───────►│  Agent B    │
│ (Orchestrator)        │ (Specialist)│
└─────────────┘         └─────────────┘
```

### Hub-and-Spoke

```
                    ┌─────────────┐
                    │   Agent A   │
                    │ (Coordinator)│
                    └──────┬──────┘
                           │
           ┌───────────────┼───────────────┐
           ▼               ▼               ▼
    ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
    │  Agent B    │ │  Agent C    │ │  Agent D    │
    │ (Research)  │ │  (Code)     │ │ (Review)    │
    └─────────────┘ └─────────────┘ └─────────────┘
```

### Agent Mesh

```
    ┌─────────────┐         ┌─────────────┐
    │  Agent A    │◄───────►│  Agent B    │
    └──────┬──────┘         └──────┬──────┘
           │                       │
           │    ┌─────────────┐    │
           └───►│  Agent C    │◄───┘
                └──────┬──────┘
                       │
                ┌──────▼──────┐
                │  Agent D    │
                └─────────────┘
```

### With Agent Gateway

```
┌─────────────────────────────────────────────────────────┐
│                    Agent Gateway                         │
│  • Discovery    • Auth    • Rate Limiting    • Logging  │
└─────────────────────────────┬───────────────────────────┘
                              │
          ┌───────────────────┼───────────────────┐
          ▼                   ▼                   ▼
   ┌─────────────┐     ┌─────────────┐     ┌─────────────┐
   │  Agent A    │     │  Agent B    │     │  Agent C    │
   └─────────────┘     └─────────────┘     └─────────────┘
```

---

## Implementation Example

### Python Agent Server

```python
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from typing import Optional
import uuid

app = FastAPI()

# Agent Card
AGENT_CARD = {
    "name": "summarizer-agent",
    "description": "Summarizes text content",
    "version": "1.0.0",
    "skills": [
        {
            "id": "summarize",
            "name": "Summarize",
            "description": "Summarize text content",
            "inputSchema": {
                "type": "object",
                "properties": {
                    "text": {"type": "string"},
                    "max_length": {"type": "integer", "default": 200}
                },
                "required": ["text"]
            }
        }
    ]
}

tasks = {}

class TaskRequest(BaseModel):
    skill: str
    input: dict

class TaskResponse(BaseModel):
    id: str
    state: str
    artifacts: Optional[list] = None

@app.get("/.well-known/agent.json")
async def get_agent_card():
    return AGENT_CARD

@app.post("/tasks", response_model=TaskResponse)
async def create_task(request: TaskRequest):
    if request.skill not in ["summarize"]:
        raise HTTPException(400, "Unknown skill")

    task_id = str(uuid.uuid4())
    tasks[task_id] = {
        "id": task_id,
        "state": "submitted",
        "input": request.input,
        "artifacts": []
    }

    # Process async (simplified - inline for demo)
    result = await process_summarize(request.input)
    tasks[task_id]["state"] = "completed"
    tasks[task_id]["artifacts"] = [result]

    return tasks[task_id]

@app.get("/tasks/{task_id}", response_model=TaskResponse)
async def get_task(task_id: str):
    if task_id not in tasks:
        raise HTTPException(404, "Task not found")
    return tasks[task_id]

async def process_summarize(input: dict) -> dict:
    text = input["text"]
    max_length = input.get("max_length", 200)

    # Call LLM for summarization
    summary = await llm.summarize(text, max_length)

    return {
        "id": str(uuid.uuid4()),
        "type": "text/plain",
        "name": "summary.txt",
        "content": summary
    }
```

### Python Agent Client

```python
import httpx
from typing import Optional

class A2AClient:
    def __init__(self, base_url: str, auth_token: Optional[str] = None):
        self.base_url = base_url.rstrip('/')
        self.headers = {}
        if auth_token:
            self.headers["Authorization"] = f"Bearer {auth_token}"

    async def discover(self) -> dict:
        """Get agent card."""
        async with httpx.AsyncClient() as client:
            resp = await client.get(
                f"{self.base_url}/.well-known/agent.json",
                headers=self.headers
            )
            resp.raise_for_status()
            return resp.json()

    async def send_task(self, skill: str, input: dict) -> dict:
        """Send a task to the agent."""
        async with httpx.AsyncClient() as client:
            resp = await client.post(
                f"{self.base_url}/tasks",
                json={"skill": skill, "input": input},
                headers=self.headers
            )
            resp.raise_for_status()
            return resp.json()

    async def get_task(self, task_id: str) -> dict:
        """Get task status and results."""
        async with httpx.AsyncClient() as client:
            resp = await client.get(
                f"{self.base_url}/tasks/{task_id}",
                headers=self.headers
            )
            resp.raise_for_status()
            return resp.json()

    async def wait_for_completion(self, task_id: str, poll_interval: float = 1.0) -> dict:
        """Poll until task completes."""
        import asyncio
        while True:
            task = await self.get_task(task_id)
            if task["state"] in ["completed", "failed", "canceled"]:
                return task
            await asyncio.sleep(poll_interval)

# Usage
async def main():
    client = A2AClient("https://agents.example.com")

    # Discover capabilities
    card = await client.discover()
    print(f"Agent: {card['name']}")
    print(f"Skills: {[s['id'] for s in card['skills']]}")

    # Send task
    task = await client.send_task("summarize", {
        "text": "Long document content...",
        "max_length": 100
    })

    # Wait for result
    result = await client.wait_for_completion(task["id"])
    print(f"Summary: {result['artifacts'][0]['content']}")
```

---

## Multi-Agent Orchestration

### Orchestrator Pattern

```python
class OrchestratorAgent:
    def __init__(self):
        self.agents = {
            "research": A2AClient("https://research-agent.example.com"),
            "code": A2AClient("https://code-agent.example.com"),
            "review": A2AClient("https://review-agent.example.com"),
        }

    async def handle_request(self, user_request: str):
        # 1. Research phase
        research_task = await self.agents["research"].send_task(
            "web-search",
            {"query": user_request}
        )
        research_result = await self.agents["research"].wait_for_completion(
            research_task["id"]
        )

        # 2. Code generation phase
        code_task = await self.agents["code"].send_task(
            "generate",
            {"spec": user_request, "context": research_result["artifacts"]}
        )
        code_result = await self.agents["code"].wait_for_completion(
            code_task["id"]
        )

        # 3. Review phase
        review_task = await self.agents["review"].send_task(
            "review-code",
            {"code": code_result["artifacts"]}
        )
        review_result = await self.agents["review"].wait_for_completion(
            review_task["id"]
        )

        return {
            "research": research_result,
            "code": code_result,
            "review": review_result
        }
```

### Parallel Execution

```python
async def parallel_research(self, topics: list[str]):
    """Run multiple research tasks in parallel."""
    tasks = []
    for topic in topics:
        task = await self.agents["research"].send_task(
            "web-search",
            {"query": topic}
        )
        tasks.append((topic, task["id"]))

    # Wait for all
    results = {}
    for topic, task_id in tasks:
        result = await self.agents["research"].wait_for_completion(task_id)
        results[topic] = result

    return results
```

---

## Security Considerations

### Authentication

| Method | Use Case |
|--------|----------|
| Bearer tokens | Service-to-service |
| OAuth 2.0 | User-delegated access |
| mTLS | High-security environments |
| API keys | Simple integrations |

### Authorization

```json
{
  "permissions": {
    "skills": ["summarize", "search"],
    "rateLimit": {
      "requestsPerMinute": 100
    },
    "maxTaskDuration": 300
  }
}
```

### Best Practices

| Practice | Description |
|----------|-------------|
| Validate input | Schema validation on all inputs |
| Rate limiting | Prevent abuse |
| Audit logging | Track all agent interactions |
| Timeout handling | Set max task duration |
| Error handling | Graceful failure modes |

---

## A2A + MCP Together

```
┌─────────────────────────────────────────────────────────────┐
│                    User Request                             │
└─────────────────────────────┬───────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                  Orchestrator Agent                         │
└──────────┬─────────────────────────────────────┬────────────┘
           │                                     │
           │ A2A                                 │ A2A
           ▼                                     ▼
┌─────────────────────┐               ┌─────────────────────┐
│   Research Agent    │               │    Code Agent       │
└──────────┬──────────┘               └──────────┬──────────┘
           │                                     │
           │ MCP                                 │ MCP
           ▼                                     ▼
┌─────────────────────┐               ┌─────────────────────┐
│   Web Search MCP    │               │   GitHub MCP        │
│   Browser MCP       │               │   Filesystem MCP    │
└─────────────────────┘               └─────────────────────┘
```

---

## Ecosystem

### Framework Support

| Framework | A2A Support |
|-----------|-------------|
| LangChain | In progress |
| AutoGen | Planned |
| CrewAI | Community |
| Vertex AI | Native |
| Claude Agent SDK | Planned |

### Related Protocols

| Protocol | Purpose |
|----------|---------|
| MCP | Agent ↔ Tools |
| A2A | Agent ↔ Agent |
| OpenAI Assistants | OpenAI ecosystem |
| Semantic Kernel | Microsoft ecosystem |

---

## When to Use A2A

**Strengths:**

- Cross-framework agent collaboration
- Standardized discovery and communication
- Async, long-running task support
- Clear state management

**Considerations:**

- Newer protocol (2025)
- Ecosystem still developing
- Overhead for simple single-agent use

**Best for:**

- Multi-agent systems
- Heterogeneous agent environments
- Enterprise agent platforms
- Agent marketplaces

---

## Related

- [[MCP Servers]]
- [[MCP Gateway]]
- [[Agent Gateway]]
- [[Agent Frameworks]]
- [[DNS & Service Discovery|Service Discovery]]
