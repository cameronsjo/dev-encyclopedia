# Developer's Encyclopedia Roadmap

## Planned Content

| Item | Status | Impact | Effort | Notes |
|------|--------|--------|--------|-------|
| Unreal Engine | `planned` | Medium | Medium | Complete game engines coverage |

## Ideas (Not Committed)

### AI/LLM Infrastructure

| Item | Status | Impact | Effort | Notes |
|------|--------|--------|--------|-------|
| Vector Databases | `idea` | High | Medium | Pinecone, Weaviate, Qdrant, pgvector, Chroma |
| Embeddings | `idea` | High | Medium | Models, semantic search, use cases |
| Model Serving | `idea` | High | Medium | vLLM, TGI, Triton, Ollama, optimization |
| Fine-tuning | `idea` | Medium | Medium | LoRA, QLoRA, PEFT, when vs RAG |
| LLM Evaluation | `idea` | Medium | Medium | RAGAS, benchmarks, evals frameworks |
| AI Security | `idea` | High | Medium | Prompt injection, jailbreaks, red teaming |
| AI Observability | `idea` | Medium | Small | LangSmith, Helicone, agent tracing |
| Semantic Caching | `idea` | Medium | Small | LLM-specific caching patterns |
| Multimodal AI | `idea` | Medium | Medium | Vision-language models, audio, video |

### Agent Protocols & Registries

| Item | Status | Impact | Effort | Notes |
|------|--------|--------|--------|-------|
| MCP Registry | `idea` | Medium | Small | Discovery, cataloging MCP servers |
| Agent Registry | `idea` | Medium | Small | Agent discovery, capabilities |
| Service Discovery | `idea` | High | Medium | Consul, etcd, DNS-SD, Eureka |
| Service Registry | `idea` | Medium | Medium | Registration, health, metadata |

### Web3 & Blockchain

| Item | Status | Impact | Effort | Notes |
|------|--------|--------|--------|-------|
| Blockchain Fundamentals | `idea` | Medium | Medium | Consensus, blocks, merkle trees |
| Cryptocurrency | `idea` | Medium | Medium | Bitcoin, Ethereum, tokenomics |
| Smart Contracts | `idea` | Medium | Medium | Solidity, EVM, auditing |
| Web3 Development | `idea` | Medium | Medium | Wallets, dApps, web3.js/ethers |
| Cryptography | `idea` | High | Large | Hashing, encryption, signatures, ZK proofs |

### Infrastructure & DevOps

| Item | Status | Impact | Effort | Notes |
|------|--------|--------|--------|-------|
| Service Mesh | `idea` | Medium | Medium | Istio, Linkerd, sidecar patterns |
| Secrets Management | `idea` | High | Medium | Vault, AWS Secrets Manager, rotation |
| CDN | `idea` | Medium | Small | Cloudflare, Fastly, edge caching |
| Container Security | `idea` | Medium | Medium | Image scanning, runtime security |
| Supply Chain Security | `idea` | High | Medium | SBOM, Sigstore, dependency scanning, attestation |
| Identity (IAM) | `idea` | High | Medium | OAuth 2.0, OIDC, SSO, SAML deep dive |
| DevOps/Infrastructure | `idea` | Medium | Large | CI/CD, IaC, Kubernetes |
| Cloud platforms | `idea` | Medium | Large | AWS, GCP, Azure comparisons |

### Other

| Item | Status | Impact | Effort | Notes |
|------|--------|--------|--------|-------|
| More CS algorithms | `idea` | High | Medium | Dynamic Programming, Backtracking, etc. |
| Additional languages | `idea` | Medium | Medium | C, Zig, Elixir, Scala, Haskell |
| Frontend extras | `idea` | Low | Small | Solid, HTMX |
| CLI frameworks | `idea` | Low | Small | Click, Clap, Commander.js |
| Embedded frameworks | `idea` | Low | Medium | Arduino, ESP-IDF, RTOS |

## Completed

| Item | Status | Impact | Effort | Notes |
|------|--------|--------|--------|-------|
| AI Infrastructure | `done` | High | Medium | MCP Gateway, Agent Gateway, Grounding, LLM Internals, Prompt Engineering |
| Agent Protocols | `done` | High | Medium | A2A Protocol |
| Web Infrastructure | `done` | High | Medium | API Gateways, Load Balancing, Web Security |
| Cross-platform frameworks | `done` | High | Large | Flutter, RN, MAUI, Electron, Tauri, Avalonia |
| Web frontend frameworks | `done` | High | Medium | React, Vue, Angular, Svelte |
| Backend frameworks | `done` | Medium | Medium | Spring Boot, Rails, Laravel, Django |
| Game engines | `done` | Medium | Medium | Unity, Godot, Bevy |
| Database Engines | `done` | High | Large | SQL, Document, KV, Graph, Vector, TimeSeries |
| Observability Stack | `done` | High | Medium | Prometheus, Grafana, Observability overview |
| Meta-frameworks | `done` | High | Medium | Next.js, Nuxt, SvelteKit, Remix, Astro |
| CS fundamentals | `done` | High | Medium | Big O, Data Structures, Patterns, Algorithms |
| Math foundations | `done` | Medium | Medium | Calculus, Linear Algebra, Probability |
| ML concepts | `done` | Medium | Medium | Fundamentals, Neural Networks, LLMs |
| AI/Agent infrastructure | `done` | High | Medium | MCP Servers, RAG, Agent Frameworks |
| API Design Patterns | `done` | High | Medium | REST, GraphQL, gRPC, tRPC, Webhooks |
| Security Concepts | `done` | High | Large | OWASP, Auth patterns, Crypto basics |
| Tool comparisons | `done` | High | Large | Testing, Logging, HTTP, ORMs, Build, Deploy |
| Observability basics | `done` | Medium | Small | OpenTelemetry, Distributed Tracing |
| File Storage | `done` | High | Medium | Object, block, file, distributed storage |

## Technical Debt

| Item | Status | Impact | Effort | Notes |
|------|--------|--------|--------|-------|
| Add Obsidian config files | `idea` | Low | Small | .obsidian/ settings |
| Consistent frontmatter audit | `idea` | Low | Small | Ensure all pages have proper tags |
| Cross-link audit | `idea` | Low | Small | Ensure all [[links]] resolve |

## Status Legend

- `idea` — Proposed, not yet committed
- `planned` — Committed, not started
- `in-progress` — Currently being worked on
- `done` — Completed
- `blocked` — Waiting on something
