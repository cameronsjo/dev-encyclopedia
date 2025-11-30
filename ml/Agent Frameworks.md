---
title: Agent Frameworks
aliases:
  - AI Agents
  - LLM Agents
  - Agentic AI
tags:
  - ai
  - ml
  - llm
  - agents
  - framework
type: reference
status: complete
created: 2025-11-28
---

# Agent Frameworks

Frameworks for building autonomous AI agents that can reason, plan, and act.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Autonomous task completion |
| Core loop | Observe → Think → Act → Repeat |
| Key capability | Tool use, planning, memory |
| Challenge | Reliability, cost, safety |

---

## What is an AI Agent?

```
┌─────────────────────────────────────────────────────────────┐
│                         AGENT                                │
│                                                             │
│   ┌─────────┐    ┌─────────┐    ┌─────────┐               │
│   │ Observe │───►│  Think  │───►│   Act   │               │
│   │  input  │    │  (LLM)  │    │ (tools) │               │
│   └─────────┘    └─────────┘    └────┬────┘               │
│        ▲                             │                     │
│        │         ┌─────────┐         │                     │
│        └─────────│ Memory  │◄────────┘                     │
│                  └─────────┘                               │
└─────────────────────────────────────────────────────────────┘
```

### Agent vs Chatbot

| Chatbot | Agent |
|---------|-------|
| Single turn | Multi-step |
| Responds | Acts |
| Stateless | Has memory |
| No tools | Uses tools |
| Predictable | Autonomous |

---

## Agent Architectures

### ReAct (Reason + Act)

Interleaved reasoning and action.

```
Thought: I need to find the weather in Tokyo
Action: search("Tokyo weather")
Observation: Tokyo is 15°C and sunny
Thought: I have the answer
Action: respond("Tokyo is 15°C and sunny")
```

### Plan-and-Execute

Separate planning from execution.

```
Plan:
1. Search for Tokyo weather
2. Search for Paris weather
3. Compare and summarize

Execute: [runs each step]
```

### Reflexion

Self-reflection and improvement.

```
Attempt 1: [fails]
Reflection: "I didn't check the date format"
Attempt 2: [succeeds with fix]
```

### Multi-Agent

Multiple specialized agents collaborating.

```
┌──────────────┐     ┌──────────────┐
│   Planner    │────►│   Executor   │
└──────────────┘     └──────┬───────┘
                            │
              ┌─────────────┼─────────────┐
              ▼             ▼             ▼
       ┌──────────┐  ┌──────────┐  ┌──────────┐
       │ Searcher │  │  Coder   │  │ Reviewer │
       └──────────┘  └──────────┘  └──────────┘
```

---

## Framework Comparison

| Framework | Language | Style | Best For |
|-----------|----------|-------|----------|
| LangChain | Python/JS | Comprehensive | General purpose |
| LangGraph | Python/JS | Graph-based | Complex workflows |
| AutoGen | Python | Multi-agent | Conversations |
| CrewAI | Python | Role-based | Team simulation |
| Claude Agent SDK | TypeScript | Native | Claude integration |
| OpenAI Assistants | API | Managed | Quick start |
| Semantic Kernel | C#/Python | Enterprise | .NET shops |
| Haystack | Python | Pipelines | Search/RAG |
| DSPy | Python | Programmatic | Optimization |

---

## LangChain

Most popular, comprehensive toolkit.

```python
from langchain.agents import create_react_agent
from langchain_openai import ChatOpenAI
from langchain.tools import Tool

# Define tools
tools = [
    Tool(
        name="search",
        func=search_function,
        description="Search the web"
    ),
    Tool(
        name="calculator",
        func=calculator,
        description="Do math"
    )
]

# Create agent
llm = ChatOpenAI(model="gpt-4")
agent = create_react_agent(llm, tools, prompt)

# Run
result = agent.invoke({"input": "What's 25% of Tesla's stock price?"})
```

### Pros/Cons

| Pros | Cons |
|------|------|
| Huge ecosystem | Abstractions can be leaky |
| Great docs | Frequent breaking changes |
| Many integrations | Can be over-engineered |

---

## LangGraph

Stateful, graph-based workflows.

```python
from langgraph.graph import StateGraph, END

# Define state
class AgentState(TypedDict):
    messages: list
    next_action: str

# Define nodes
def call_model(state):
    response = model.invoke(state["messages"])
    return {"messages": [response], "next_action": "tool"}

def call_tool(state):
    result = execute_tool(state["messages"][-1])
    return {"messages": [result], "next_action": "model"}

# Build graph
workflow = StateGraph(AgentState)
workflow.add_node("model", call_model)
workflow.add_node("tool", call_tool)
workflow.add_edge("model", "tool")
workflow.add_conditional_edges("tool", should_continue)
workflow.set_entry_point("model")

app = workflow.compile()
```

### When to Use LangGraph

- Complex control flow
- Cycles and loops
- Human-in-the-loop
- Persistent state

---

## Claude Agent SDK

Native TypeScript SDK for Claude agents.

```typescript
import { Agent, Tool } from "@anthropic/agent-sdk";

const searchTool = new Tool({
  name: "web_search",
  description: "Search the web",
  parameters: {
    query: { type: "string", description: "Search query" }
  },
  execute: async ({ query }) => {
    return await search(query);
  }
});

const agent = new Agent({
  model: "claude-sonnet-4-20250514",
  tools: [searchTool],
  systemPrompt: "You are a helpful research assistant."
});

const result = await agent.run("Find recent news about AI agents");
```

### Claude Agent SDK Features

| Feature | Description |
|---------|-------------|
| Native Claude | Optimized for Claude models |
| Tool use | Built-in tool execution |
| Streaming | Real-time responses |
| MCP integration | Connect to MCP servers |

---

## AutoGen

Microsoft's multi-agent conversation framework.

```python
from autogen import AssistantAgent, UserProxyAgent

assistant = AssistantAgent(
    name="assistant",
    llm_config={"model": "gpt-4"}
)

user_proxy = UserProxyAgent(
    name="user",
    human_input_mode="NEVER",
    code_execution_config={"work_dir": "coding"}
)

user_proxy.initiate_chat(
    assistant,
    message="Write a Python script to analyze CSV data"
)
```

### AutoGen Patterns

| Pattern | Description |
|---------|-------------|
| Two-agent chat | User proxy + Assistant |
| Group chat | Multiple agents discuss |
| Hierarchical | Manager coordinates workers |

---

## CrewAI

Role-based agent teams.

```python
from crewai import Agent, Task, Crew

researcher = Agent(
    role="Researcher",
    goal="Find accurate information",
    backstory="Expert at finding data",
    tools=[search_tool]
)

writer = Agent(
    role="Writer",
    goal="Write clear reports",
    backstory="Technical writer",
    tools=[write_tool]
)

research_task = Task(
    description="Research AI agent frameworks",
    agent=researcher
)

write_task = Task(
    description="Write a summary report",
    agent=writer
)

crew = Crew(
    agents=[researcher, writer],
    tasks=[research_task, write_task]
)

result = crew.kickoff()
```

---

## Tool Design

### Good Tool Design

```python
# ✅ Clear name and description
Tool(
    name="search_documentation",
    description="Search official documentation for a library. "
                "Input: library name and query. "
                "Returns: relevant documentation excerpts.",
    func=search_docs
)

# ❌ Vague
Tool(
    name="search",
    description="Searches stuff",
    func=search
)
```

### Tool Categories

| Category | Examples |
|----------|----------|
| Information | Web search, RAG, APIs |
| Action | Send email, create file |
| Computation | Calculator, code execution |
| Communication | Slack, Discord |

---

## Memory Types

### Short-term (Context Window)

Recent conversation history.

### Long-term (Vector Store)

```python
# Store
memory.save({"input": query, "output": response})

# Retrieve
relevant = memory.search(current_query, k=5)
```

### Episodic

Specific past interactions.

### Semantic

General knowledge extracted from interactions.

---

## Reliability Patterns

### Retry with Backoff

```python
@retry(stop=stop_after_attempt(3), wait=wait_exponential())
def agent_step(state):
    return llm.invoke(state)
```

### Output Validation

```python
from pydantic import BaseModel

class SearchResult(BaseModel):
    query: str
    results: list[str]
    confidence: float

# Force structured output
result = llm.with_structured_output(SearchResult).invoke(prompt)
```

### Human-in-the-Loop

```python
def execute_action(action):
    if action.risk_level == "high":
        approved = ask_human(f"Approve: {action}?")
        if not approved:
            return "Action cancelled by user"
    return action.execute()
```

### Guardrails

```python
# Input guardrails
if contains_pii(user_input):
    return "Cannot process PII"

# Output guardrails
if is_harmful(response):
    return "I cannot help with that"
```

---

## Cost Management

| Strategy | Implementation |
|----------|----------------|
| Smaller models first | Try GPT-3.5 before GPT-4 |
| Caching | Cache tool results |
| Batch operations | Group similar requests |
| Token limits | Cap context window |
| Early stopping | Stop when confident |

---

## When to Use What

| Scenario | Recommendation |
|----------|----------------|
| Simple automation | Single agent + tools |
| Complex workflows | LangGraph |
| Multi-perspective | AutoGen, CrewAI |
| Claude-native | Claude Agent SDK |
| .NET enterprise | Semantic Kernel |
| Quick prototype | OpenAI Assistants |
| Research/optimization | DSPy |

---

## Agent vs Workflow

| Agents | Workflows |
|--------|-----------|
| Autonomous decisions | Predetermined steps |
| Flexible | Predictable |
| Higher cost | Lower cost |
| Can fail unexpectedly | Fails predictably |
| Good for open-ended | Good for structured |

**Hybrid approach:** Use workflows for known paths, agents for decisions.

---

## Related

- [[MCP Servers]]
- [[RAG]]
- [[LLMs & Transformers]]
- [[Prompt Engineering]]
