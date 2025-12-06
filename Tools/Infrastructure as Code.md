---
title: Infrastructure as Code
aliases:
  - IaC
  - IaC Tools
tags:
  - devops
  - infrastructure
  - cloud
  - tool
type: reference
status: complete
created: 2025-11-30
---

# Infrastructure as Code

Manage and provision infrastructure through machine-readable definition files rather than manual configuration or interactive tools.

## Overview

| Aspect | Details |
|--------|---------|
| **Purpose** | Automate infrastructure provisioning, ensure consistency, enable version control |
| **Key Paradigms** | Declarative (desired state), Imperative (step-by-step), Hybrid |
| **Core Capabilities** | Provisioning, state management, drift detection, dependency resolution |
| **Common Targets** | Cloud resources, Kubernetes, networking, storage, compute, databases |
| **Integration** | CI/CD pipelines, GitOps workflows, policy-as-code, cost analysis |

## Core Concepts

### Declarative vs Imperative

**Declarative** - Specify desired end state, tool determines how to achieve it:
- Focus on "what" not "how"
- Idempotent by design
- Examples: Terraform HCL, CloudFormation YAML, Pulumi (declarative mode)

**Imperative** - Explicitly define steps to execute:
- Fine-grained control over provisioning flow
- Requires manual idempotency handling
- Examples: Pulumi (imperative mode), AWS CDK, custom scripts

### State Management

**State File** - Records actual infrastructure state to detect changes:
- Tracks resource mappings (logical name → cloud resource ID)
- Enables drift detection (actual vs desired state)
- Must be stored remotely for team collaboration (S3, Terraform Cloud, cloud storage)
- Locking mechanisms prevent concurrent modifications

**Stateless** - No persistent state file:
- Relies on cloud provider APIs for current state
- Simpler but less powerful drift detection
- Example: CloudFormation (uses stack metadata)

### Drift Detection

Identifies resources modified outside IaC workflow:
- Manual console changes
- External automation scripts
- Emergency hotfixes
- Tools periodically compare state file with actual infrastructure

### Modules and Reusability

**Modules/Stacks** - Reusable infrastructure components:
- Encapsulate common patterns (VPC setup, EKS cluster, RDS instance)
- Parameterized for different environments
- Published to registries (Terraform Registry, Pulumi Registry)
- Versioned for stability

## Major Tools

| Tool | Language | Paradigm | State | Provider Ecosystem | Best For |
|------|----------|----------|-------|-------------------|----------|
| **Terraform** | HCL | Declarative | External file | 3000+ providers | Multi-cloud, established ecosystems |
| **OpenTofu** | HCL | Declarative | External file | Terraform-compatible | Open-source Terraform alternative |
| **Pulumi** | TypeScript/Python/Go/C#/Java | Hybrid | Service/self-managed | 150+ providers | Developers preferring general-purpose languages |
| **AWS CDK** | TypeScript/Python/Java/C#/Go | Imperative | CloudFormation | AWS only | AWS-native, type-safe infrastructure |
| **CloudFormation** | YAML/JSON | Declarative | AWS-managed | AWS only | AWS-native, no additional tooling |
| **Crossplane** | YAML (CRDs) | Declarative | Kubernetes API | 50+ providers | Kubernetes-native control plane |
| **Ansible** | YAML | Imperative | Stateless | Modules for major clouds | Configuration management + provisioning |

## Detailed Comparison

### Terraform

**Strengths:**
- Industry standard with largest provider ecosystem
- Strong community and module registry
- Cloud-agnostic (AWS, Azure, GCP, Kubernetes, SaaS APIs)
- Mature state management and locking
- Plan/apply workflow for safe changes

**Considerations:**
- HCL learning curve for developers unfamiliar with DSLs
- State file management complexity
- Enterprise features require Terraform Cloud subscription
- HashiCorp license change in 2023 (BSL, not fully open-source)

**When to Use:**
- Multi-cloud infrastructure
- Need extensive provider support
- Team comfortable with DSLs
- Established Terraform ecosystem in organization

### OpenTofu

**Strengths:**
- Fully open-source (Linux Foundation project)
- Drop-in Terraform replacement (compatible with 1.5.x)
- Community-driven development
- Same HCL syntax and provider ecosystem
- No vendor lock-in concerns

**Considerations:**
- Newer project (forked 2023)
- Smaller community than Terraform
- Feature parity still catching up
- Provider compatibility may lag slightly

**When to Use:**
- Require truly open-source tooling
- Concerned about HashiCorp licensing
- Migrating from Terraform <1.6
- Contributing to governance-driven projects

### Pulumi

**Strengths:**
- Use familiar programming languages (TypeScript, Python, Go, C#, Java)
- Full IDE support (autocomplete, refactoring, debugging)
- Imperative control flow (loops, conditionals, functions)
- Built-in testing frameworks
- Pulumi Service for state and secrets management

**Considerations:**
- Smaller community than Terraform
- State management requires Pulumi Service or self-hosted backend
- Mixing infrastructure and application logic can blur boundaries
- Less module reuse across language ecosystems

**When to Use:**
- Developer-first teams
- Complex provisioning logic
- Need programmatic abstractions
- TypeScript/Python expertise

### AWS CDK

**Strengths:**
- Native AWS integration and type safety
- Constructs library for high-level abstractions
- Synthesizes to CloudFormation for deployment
- Strong typing prevents configuration errors
- L1/L2/L3 constructs (low to high level)

**Considerations:**
- AWS-only (no multi-cloud)
- CloudFormation limitations (resource limits, rollback behavior)
- Steeper learning curve for infrastructure teams
- Tightly coupled to AWS service releases

**When to Use:**
- AWS-only infrastructure
- Development teams building on AWS
- Need type-safe infrastructure definitions
- Leveraging CDK construct patterns

### CloudFormation

**Strengths:**
- Fully AWS-managed (no external state)
- Deep AWS integration and immediate new service support
- StackSets for multi-account/multi-region deployment
- Change sets for safe previews
- No additional tooling required

**Considerations:**
- YAML/JSON verbosity
- AWS-only
- Limited abstraction capabilities
- Stack update limitations (some resources require replacement)
- No native testing frameworks

**When to Use:**
- AWS-exclusive infrastructure
- Prefer AWS-native tooling
- Simple to moderate infrastructure complexity
- Minimal external dependencies

### Crossplane

**Strengths:**
- Kubernetes-native (CRDs, controllers, GitOps)
- Control plane for multi-cloud resources
- Composition for reusable patterns
- Unified API across providers
- Policy enforcement via admission controllers

**Considerations:**
- Requires Kubernetes cluster
- Kubernetes expertise needed
- Smaller provider ecosystem
- Immature compared to Terraform
- State stored in etcd (Kubernetes API server)

**When to Use:**
- Kubernetes-centric platform teams
- GitOps workflows (ArgoCD, Flux)
- Self-service infrastructure via CRDs
- Unified control plane for cloud resources

## Testing IaC

| Testing Level | Purpose | Tools | Example |
|---------------|---------|-------|---------|
| **Static Analysis** | Validate syntax, check best practices | `terraform validate`, `tflint`, `checkov`, `tfsec` | Detect security misconfigurations |
| **Unit Testing** | Test individual modules in isolation | `terraform-compliance`, Pulumi unit tests, CDK assertions | Verify module outputs for given inputs |
| **Integration Testing** | Test modules together in realistic environment | `terratest`, Pulumi integration tests, CDK integration tests | Provision test infrastructure, run assertions |
| **Policy Testing** | Enforce compliance and cost controls | Sentinel (Terraform), OPA, Cloud Custodian, Infracost | Block non-compliant deployments |
| **Drift Detection** | Identify manual changes | `terraform plan`, Pulumi refresh, CloudFormation drift detection | Continuous compliance monitoring |

## Advanced Patterns

### Workspaces and Environments

Manage multiple environments (dev, staging, prod) from single codebase:
- Terraform workspaces (shared code, separate state)
- Environment-specific variable files
- Directory-per-environment structure
- Separate repositories per environment (strict isolation)

### GitOps for IaC

Version control as source of truth:
1. Infrastructure changes via pull requests
2. Automated planning on PR creation
3. Peer review of proposed changes
4. Apply on merge to main branch
5. Audit trail via git history

### Policy as Code

Enforce governance through automated policy checks:
- Sentinel (Terraform Cloud/Enterprise)
- Open Policy Agent (OPA) - Rego language
- Cloud Custodian - cloud compliance automation
- AWS Config Rules, Azure Policy

### Cost Estimation

Predict infrastructure costs before provisioning:
- Infracost - estimates from Terraform/CloudFormation
- Cloud provider calculators
- FinOps integration in CI/CD
- Budget alerts and anomaly detection

## Best Practices

| Practice | Rationale |
|----------|-----------|
| **Remote state storage** | Enable collaboration, prevent conflicts, ensure durability |
| **State locking** | Prevent concurrent modifications causing corruption |
| **Modular design** | Reusability, testability, maintainability |
| **Version control** | Audit trail, rollback capability, collaboration |
| **Immutable infrastructure** | Replace rather than modify (pets vs cattle) |
| **Least privilege IAM** | Security-by-default for provisioning credentials |
| **Secrets management** | Never commit credentials (use vaults, environment variables) |
| **Automated testing** | Catch errors before production deployment |
| **Plan before apply** | Review changes, prevent surprises |
| **Tag everything** | Cost allocation, resource organization, automation |

## When to Use

### Terraform

✅ Multi-cloud or hybrid cloud infrastructure
✅ Large provider ecosystem required
✅ Team familiar with HCL or willing to learn
✅ Mature module registry needed

❌ Strongly prefer general-purpose languages
❌ Open-source licensing is critical (consider OpenTofu)

### OpenTofu

✅ Need fully open-source IaC tool
✅ Migrating from Terraform 1.5.x
✅ Concerned about vendor licensing changes
✅ Contributing to community governance

❌ Require latest Terraform features
❌ Need HashiCorp commercial support

### Pulumi

✅ Developer-first teams (TypeScript, Python, Go)
✅ Complex provisioning logic
✅ Strong typing and IDE support critical
✅ Programmatic abstractions over declarative

❌ Infrastructure team prefers DSLs
❌ Need largest module ecosystem
❌ Avoiding vendor-specific state backends

### AWS CDK

✅ AWS-only infrastructure
✅ Development teams building on AWS
✅ Type safety prevents misconfigurations
✅ Leverage high-level construct patterns

❌ Multi-cloud requirements
❌ Infrastructure team not comfortable with programming languages
❌ Need to avoid CloudFormation limitations

### CloudFormation

✅ AWS-native tooling preference
✅ Simple to moderate infrastructure
✅ Minimal external dependencies
✅ AWS StackSets for multi-account deployment

❌ Complex abstractions needed
❌ Multi-cloud infrastructure
❌ Prefer general-purpose languages

### Crossplane

✅ Kubernetes-native platform engineering
✅ GitOps workflows (ArgoCD, Flux)
✅ Self-service infrastructure via CRDs
✅ Unified control plane for multi-cloud

❌ No Kubernetes cluster available
❌ Team lacks Kubernetes expertise
❌ Need mature provider ecosystem

## Decision Guide

| Requirement | Recommended Tool |
|-------------|------------------|
| **Multi-cloud infrastructure** | Terraform, OpenTofu, Pulumi |
| **AWS-only, type-safe** | AWS CDK |
| **AWS-only, minimal tooling** | CloudFormation |
| **Kubernetes-native platform** | Crossplane |
| **Developer-first, general-purpose languages** | Pulumi, AWS CDK |
| **Established ecosystem, community modules** | Terraform |
| **Open-source licensing critical** | OpenTofu, Pulumi (Apache 2.0) |
| **Complex provisioning logic** | Pulumi, AWS CDK |
| **GitOps and Kubernetes** | Crossplane |
| **Largest provider support** | Terraform, OpenTofu |

## Related

- [[Kubernetes]]
- [[Docker]]
- [[CI CD Tools]]
- [[Cloud Platforms Comparison]]
- [[DevOps MOC]]
- [[Prometheus]]
- [[Grafana]]
