---
title: Prompt Engineering
aliases:
  - Prompting
  - Prompt Design
  - Prompt Patterns
tags:
  - ai
  - ml
  - llm
  - prompting
type: reference
status: complete
created: 2025-11-30
---

# Prompt Engineering

The art and science of crafting inputs that elicit desired outputs from LLMs.

## Overview

| Aspect | Details |
|--------|---------|
| Purpose | Get better, more reliable LLM outputs |
| Core idea | Structure and context dramatically affect results |
| Key techniques | Few-shot, chain-of-thought, role prompting |
| Trade-off | Prompt length vs cost vs quality |

---

## Why Prompt Engineering Matters

```
Same model, different prompts:

❌ "Summarize this"
   → Generic, misses key points

✅ "Summarize this document in 3 bullet points,
    focusing on actionable insights for a PM"
   → Targeted, useful output
```

Small prompt changes → dramatically different results.

---

## Fundamental Techniques

### Zero-Shot

Direct instruction, no examples.

```
Classify the sentiment of this review as positive, negative, or neutral:

"The battery life is amazing but the screen is too dim."

Sentiment:
```

### Few-Shot

Provide examples to demonstrate the pattern.

```
Classify the sentiment:

Review: "Absolutely love it!"
Sentiment: positive

Review: "Worst purchase ever."
Sentiment: negative

Review: "The battery life is amazing but the screen is too dim."
Sentiment:
```

### Few-Shot Guidelines

| Aspect | Recommendation |
|--------|----------------|
| Number | 3-5 examples typically optimal |
| Diversity | Cover edge cases and variations |
| Order | Can matter; experiment |
| Quality | Examples set the ceiling |

---

## Chain-of-Thought (CoT)

Encourage step-by-step reasoning.

### Basic CoT

```
Q: A store has 45 apples. They sell 12 in the morning and receive
   a shipment of 30 in the afternoon. How many apples do they have?

A: Let me think step by step.
   - Started with: 45 apples
   - Sold in morning: 45 - 12 = 33 apples
   - Received shipment: 33 + 30 = 63 apples
   - Final count: 63 apples
```

### Zero-Shot CoT

Just add "Let's think step by step."

```
Q: [complex problem]

Let's think step by step.
```

### When CoT Helps

| Task Type | CoT Benefit |
|-----------|-------------|
| Math problems | High |
| Logic puzzles | High |
| Multi-step reasoning | High |
| Simple factual Q&A | Low/None |
| Creative writing | Low |

---

## Role Prompting

Assign a persona or expertise.

```
You are a senior security engineer reviewing code for vulnerabilities.
Analyze this function and identify any security issues:

[code]
```

### Effective Roles

| Role | Effect |
|------|--------|
| Expert in X | More technical, detailed |
| Teacher | Clearer explanations |
| Critic | More thorough analysis |
| Editor | Better writing feedback |
| Devil's advocate | Challenges assumptions |

### Role + Constraints

```
You are a technical writer who specializes in API documentation.

Rules:
- Use present tense
- Keep sentences under 20 words
- Include code examples for every endpoint
- Write for developers with 2+ years experience

Document this API endpoint:
[endpoint details]
```

---

## Structured Output

### Format Specification

```
Extract the following information from this job posting:

- Job title:
- Company:
- Location:
- Salary range:
- Required skills (list):

Job posting:
[text]
```

### JSON Output

```
Extract entities from this text and return as JSON:

{
  "people": [],
  "organizations": [],
  "locations": [],
  "dates": []
}

Text: [input]
```

### Markdown Tables

```
Compare these frameworks and format as a markdown table with columns:
Name | Language | Learning Curve | Best For

Frameworks to compare: React, Vue, Angular, Svelte
```

---

## Advanced Techniques

### Self-Consistency

Generate multiple reasoning paths, take majority vote.

```
Solve this problem 5 times with different reasoning approaches,
then give the most common answer.
```

### Tree of Thoughts (ToT)

Explore multiple reasoning branches.

```
Consider 3 different approaches to solve this:

Approach 1: [reasoning]
Evaluation: [pros/cons]

Approach 2: [reasoning]
Evaluation: [pros/cons]

Approach 3: [reasoning]
Evaluation: [pros/cons]

Best approach and final answer:
```

### Decomposition

Break complex tasks into subtasks.

```
To answer this question, I need to:
1. First, identify [X]
2. Then, calculate [Y]
3. Finally, combine to get [Z]

Let me work through each step:
```

### ReAct (Reason + Act)

Interleave reasoning with actions.

```
Question: What is the population of the capital of France?

Thought: I need to find the capital of France first.
Action: Search "capital of France"
Observation: Paris is the capital of France.

Thought: Now I need the population of Paris.
Action: Search "population of Paris"
Observation: Paris has approximately 2.1 million people.

Thought: I have the answer.
Answer: The population of Paris, the capital of France, is approximately 2.1 million.
```

---

## System Prompts

### Anatomy of a System Prompt

```
┌────────────────────────────────────────────────────────────┐
│                     SYSTEM PROMPT                          │
├────────────────────────────────────────────────────────────┤
│ 1. ROLE DEFINITION                                         │
│    Who is the assistant? What expertise?                   │
├────────────────────────────────────────────────────────────┤
│ 2. TASK DESCRIPTION                                        │
│    What should it do? Core responsibilities?               │
├────────────────────────────────────────────────────────────┤
│ 3. RULES & CONSTRAINTS                                     │
│    What must it always/never do?                           │
├────────────────────────────────────────────────────────────┤
│ 4. OUTPUT FORMAT                                           │
│    How should responses be structured?                     │
├────────────────────────────────────────────────────────────┤
│ 5. CONTEXT & KNOWLEDGE                                     │
│    What does it know? Current date? Domain info?           │
├────────────────────────────────────────────────────────────┤
│ 6. EXAMPLES (optional)                                     │
│    Sample interactions                                     │
└────────────────────────────────────────────────────────────┘
```

### Example System Prompt

```
You are a code review assistant for a Python backend team.

RESPONSIBILITIES:
- Review code for bugs, security issues, and style
- Suggest improvements with explanations
- Be constructive and educational

RULES:
- Always cite PEP 8 when discussing style
- Flag any SQL that might be vulnerable to injection
- If you're unsure about something, say so
- Keep feedback actionable and specific

FORMAT:
For each issue found:
1. Location (file:line)
2. Severity (critical/warning/suggestion)
3. Issue description
4. Suggested fix with code

CONTEXT:
- Team uses SQLAlchemy and FastAPI
- Python 3.11
- Code should be production-ready
```

---

## Prompt Patterns

### The Persona Pattern

```
Act as [ROLE] with [CHARACTERISTICS].
Your goal is [OBJECTIVE].
```

### The Template Pattern

```
I will give you [INPUT TYPE].
For each one, provide:
- [OUTPUT 1]
- [OUTPUT 2]
- [OUTPUT 3]

Here is the input:
[INPUT]
```

### The Guardrail Pattern

```
You must:
- [REQUIREMENT 1]
- [REQUIREMENT 2]

You must never:
- [PROHIBITION 1]
- [PROHIBITION 2]

If asked to [EDGE CASE], respond with [FALLBACK].
```

### The Verification Pattern

```
After generating your response:
1. Check that [CRITERION 1]
2. Verify that [CRITERION 2]
3. If any check fails, revise before responding
```

### The Refinement Pattern

```
Generate an initial response, then:
1. Critique your response for [ASPECT]
2. Identify weaknesses
3. Generate an improved version
```

---

## Prompt Optimization

### Iteration Process

```
1. Start with simple prompt
2. Test on diverse inputs
3. Identify failure modes
4. Add specificity/examples
5. Repeat until reliable
```

### Common Failure Fixes

| Failure | Fix |
|---------|-----|
| Too verbose | "Be concise" / "Max 50 words" |
| Wrong format | Explicit format instructions + example |
| Hallucinations | "Only use provided information" |
| Inconsistent | Add examples, be more specific |
| Misses edge cases | Add edge case examples |
| Too generic | Add domain context, be specific |

### A/B Testing Prompts

```python
prompts = {
    "v1": "Summarize this article.",
    "v2": "Summarize this article in 3 bullet points.",
    "v3": "As a news editor, summarize the key facts in 3 bullets."
}

# Test each on same inputs, measure:
# - Accuracy
# - Consistency
# - User preference
# - Cost (token usage)
```

---

## Token Efficiency

### Prompt Length Trade-offs

| Longer Prompts | Shorter Prompts |
|----------------|-----------------|
| More context | Lower cost |
| Better results | Faster |
| Higher cost | May miss nuance |
| Slower | Less control |

### Optimization Techniques

```
# Instead of:
"Please analyze this text and provide a summary of the main points,
making sure to include any important details that might be relevant
to someone who wants to understand the key takeaways..."

# Use:
"Summarize key points in 3 bullets:"
```

### When to Use More Tokens

- Complex tasks requiring examples
- High-stakes outputs
- Nuanced requirements
- When consistency matters

---

## Domain-Specific Tips

### Code Generation

```
Language: Python 3.11
Style: PEP 8, type hints required
Libraries available: pandas, numpy, requests

Write a function that [specification].
Include docstring and error handling.
```

### Data Extraction

```
Extract from this text. Return null for missing fields.
Be strict—only extract explicitly stated information.

{
  "name": null,
  "date": null,
  "amount": null
}

Text: [input]
```

### Writing/Editing

```
Rewrite this paragraph to be:
- More concise (reduce by 30%)
- Active voice
- 8th grade reading level

Preserve the core message.

Original: [text]
```

### Analysis/Reasoning

```
Analyze this [situation/data/problem].

Structure your response as:
1. Summary of key facts
2. Analysis of implications
3. Recommendations (if applicable)
4. Confidence level and caveats
```

---

## Anti-Patterns

| Anti-Pattern | Problem | Better |
|--------------|---------|--------|
| "Be helpful" | Too vague | Specific instructions |
| Very long prompts | Cost, confusion | Concise + examples |
| No examples | Inconsistent output | Few-shot |
| Conflicting instructions | Confused model | Clear hierarchy |
| "Don't hallucinate" | Doesn't work well | "Only use provided info" |
| Assuming knowledge | May not have context | Provide context |

---

## Evaluation

### Manual Evaluation

| Criterion | Questions |
|-----------|-----------|
| Accuracy | Is the output factually correct? |
| Relevance | Does it address the prompt? |
| Completeness | Are all requirements met? |
| Format | Does it follow the specified format? |
| Consistency | Same input → similar output? |

### Automated Evaluation

```python
# LLM-as-judge
judge_prompt = """
Rate this response on a scale of 1-5 for:
- Accuracy
- Helpfulness
- Clarity

Response to evaluate:
{response}

Original prompt:
{prompt}
"""
```

---

## Tools & Frameworks

| Tool | Purpose |
|------|---------|
| LangChain | Prompt templates, chains |
| Promptfoo | Prompt testing/evaluation |
| LMQL | Programmatic prompting |
| Guidance | Constrained generation |
| DSPy | Programmatic prompt optimization |

---

## Related

- [[LLMs & Transformers]]
- [[LLM Internals]]
- [[RAG]]
- [[Grounding]]
- [[Agent Frameworks]]
