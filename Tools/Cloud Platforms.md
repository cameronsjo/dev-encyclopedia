---
title: Cloud Platforms
aliases:
  - Cloud Providers
  - Public Cloud
  - AWS vs GCP vs Azure
tags:
  - cloud
  - infrastructure
  - devops
  - platform
  - comparison
type: reference
status: complete
created: "2025-11-30"
---

# Cloud Platforms

Managed infrastructure platforms providing compute, storage, networking, and services on-demand with pay-as-you-go pricing.

## Overview

| Aspect | Details |
|--------|---------|
| **Major Providers** | AWS (Amazon), GCP (Google), Azure (Microsoft) |
| **Deployment Models** | Public, Private, Hybrid, Multi-Cloud |
| **Pricing Model** | Pay-per-use, Reserved Instances, Spot/Preemptible, Savings Plans |
| **Global Reach** | 25+ regions per provider, 60+ availability zones |
| **Compliance** | SOC 2, ISO 27001, HIPAA, PCI-DSS, GDPR, FedRAMP |
| **Free Tiers** | All three offer 12-month trials + always-free services |

## Service Equivalency Matrix

| Category | AWS | GCP | Azure |
|----------|-----|-----|-------|
| **Compute - VMs** | EC2 | Compute Engine | Virtual Machines |
| **Compute - Serverless** | Lambda | Cloud Functions, Cloud Run | Functions, Container Apps |
| **Containers - Managed K8s** | EKS | GKE | AKS |
| **Containers - Serverless** | Fargate | Cloud Run | Container Instances |
| **Object Storage** | S3 | Cloud Storage | Blob Storage |
| **Block Storage** | EBS | Persistent Disk | Managed Disks |
| **File Storage** | EFS | Filestore | Files |
| **SQL Database** | RDS (PostgreSQL, MySQL, etc.) | Cloud SQL | Azure Database |
| **NoSQL - Document** | DocumentDB | Firestore | Cosmos DB |
| **NoSQL - Key-Value** | DynamoDB | Bigtable, Firestore | Table Storage, Cosmos DB |
| **Data Warehouse** | Redshift | BigQuery | Synapse Analytics |
| **CDN** | CloudFront | Cloud CDN | Front Door, CDN |
| **Load Balancer** | ELB/ALB/NLB | Cloud Load Balancing | Load Balancer, App Gateway |
| **DNS** | Route 53 | Cloud DNS | DNS, Traffic Manager |
| **VPC/Networking** | VPC | VPC | Virtual Network |
| **Identity & Access** | IAM | IAM & Admin | Azure AD, RBAC |
| **Secrets Management** | Secrets Manager | Secret Manager | Key Vault |
| **Message Queue** | SQS | Pub/Sub | Service Bus, Queue Storage |
| **Stream Processing** | Kinesis | Dataflow | Stream Analytics |
| **ML Platform** | SageMaker | Vertex AI | Machine Learning |
| **ML APIs** | Rekognition, Comprehend, etc. | Vision AI, Natural Language, etc. | Cognitive Services |
| **Monitoring** | CloudWatch | Cloud Monitoring (Stackdriver) | Monitor, Application Insights |
| **Logging** | CloudWatch Logs | Cloud Logging | Log Analytics |
| **Tracing** | X-Ray | Cloud Trace | Application Insights |
| **Infrastructure as Code** | CloudFormation | Deployment Manager | ARM Templates, Bicep |
| **CI/CD** | CodePipeline, CodeBuild | Cloud Build, Cloud Deploy | DevOps, Pipelines |
| **API Gateway** | API Gateway | Apigee, API Gateway | API Management |
| **Edge Computing** | Lambda@Edge, CloudFront Functions | Cloud CDN, Media CDN | Front Door, CDN |

## Core Service Categories

### Compute

**Virtual Machines:**

- **AWS EC2**: Widest instance variety, Nitro hypervisor, Graviton ARM processors
- **GCP Compute Engine**: Live migration, custom machine types, per-second billing
- **Azure VMs**: Deep Windows integration, hybrid benefits, B-series burstable

**Serverless Functions:**

- **AWS Lambda**: Most mature, 15-minute timeout, extensive event sources
- **GCP Cloud Functions**: 9-minute timeout (2nd gen), auto-scaling, integrated with Firebase
- **Azure Functions**: Durable Functions for workflows, premium plan for VNet integration

**Container Orchestration:**

- **AWS EKS**: Managed Kubernetes, Fargate serverless pods, extensive marketplace
- **GCP GKE**: Autopilot mode (fully managed), fastest releases, multi-cluster service mesh
- **Azure AKS**: Free control plane, Azure Arc integration, Azure Policy for governance

### Storage

**Object Storage:**

- **AWS S3**: Industry standard, versioning, lifecycle policies, 11 9's durability
- **GCP Cloud Storage**: Unified buckets (no regions), multi-region automatic, Turbo Replication
- **Azure Blob**: Hot/Cool/Archive tiers, immutable storage, hierarchical namespace

**Databases:**

- **AWS RDS**: 7 engines, read replicas, automated backups, Performance Insights
- **GCP Cloud SQL**: MySQL/PostgreSQL/SQL Server, HA with zero data loss, query insights
- **Azure Database**: Hyperscale for PostgreSQL, flexible server deployment, zone redundancy

**NoSQL:**

- **AWS DynamoDB**: Single-digit millisecond latency, on-demand pricing, global tables
- **GCP Firestore**: Real-time sync, offline support, mobile SDKs, strong consistency
- **Azure Cosmos DB**: Multi-model (document, graph, key-value), 5 consistency levels, SLA-backed

### Networking

**Load Balancing:**

- **AWS**: ALB (HTTP/S), NLB (TCP/UDP), Gateway LB (3rd party appliances)
- **GCP**: Global HTTP(S) LB with anycast IPs, Internal LB for private workloads
- **Azure**: Application Gateway (WAF), Standard LB (zone redundant), Cross-region LB

**Private Connectivity:**

- **AWS**: VPC Peering, Transit Gateway, Direct Connect (dedicated fiber)
- **GCP**: VPC Peering, Cloud Interconnect, Private Service Connect
- **Azure**: VNet Peering, Virtual WAN, ExpressRoute

### AI/ML Services

**Platform:**

- **AWS SageMaker**: Studio notebooks, AutoML, MLOps pipelines, edge deployment
- **GCP Vertex AI**: Unified ML platform, AutoML, Workbench, Feature Store
- **Azure ML**: Designer (no-code), Responsible AI dashboard, MLflow integration

**Pre-trained APIs:**

- **AWS**: Rekognition (vision), Comprehend (NLP), Polly (text-to-speech), Lex (chatbots)
- **GCP**: Vision AI, Natural Language AI, Speech-to-Text, Dialogflow CX
- **Azure**: Computer Vision, Text Analytics, Speech, Bot Service

## Pricing Models

### Compute Pricing

| Model | AWS | GCP | Azure | Best For |
|-------|-----|-----|-------|----------|
| **On-Demand** | Per-second (Linux) | Per-second | Per-minute | Variable workloads |
| **Reserved** | 1-3 year, 75% savings | Committed use, 57% savings | 1-3 year, 72% savings | Steady state |
| **Spot/Preemptible** | Up to 90% off, interruptible | Up to 91% off, 24hr max | Up to 90% off, eviction notice | Fault-tolerant batch |
| **Savings Plans** | Flexible across services | Flexible, spend-based | Flexible, hybrid benefit | Mixed workloads |

### Data Transfer Costs

| Direction | Cost Pattern |
|-----------|--------------|
| **Inbound** | Free on all platforms |
| **Between AZs** | $0.01-0.02/GB (all platforms) |
| **Outbound to Internet** | $0.05-0.12/GB tiered by volume |
| **Cross-Region** | $0.02/GB (same continent), higher for cross-continent |

### Free Tier Highlights

**AWS:**

- EC2: 750 hours/month t2.micro/t3.micro (12 months)
- S3: 5GB standard storage (12 months)
- Lambda: 1M requests/month, 400K GB-seconds (always free)
- DynamoDB: 25GB storage, 25 read/write units (always free)

**GCP:**

- Compute Engine: f1-micro instance (always free in us-west1, us-central1, us-east1)
- Cloud Storage: 5GB standard storage (always free)
- Cloud Functions: 2M invocations/month (always free)
- Firestore: 1GB storage, 50K reads/day (always free)

**Azure:**

- VMs: 750 hours/month B1S (12 months)
- Blob Storage: 5GB LRS (12 months)
- Functions: 1M executions/month (always free)
- Cosmos DB: 25GB storage, 1000 RU/s (always free)

## CLI Tools & Infrastructure as Code

### Command-Line Interfaces

| Platform | CLI Tool | Installation | Authentication |
|----------|----------|--------------|----------------|
| **AWS** | aws-cli v2 | `curl`, Homebrew, pip | IAM credentials, SSO, EC2 instance profile |
| **GCP** | gcloud | Interactive installer, Homebrew | Service accounts, user auth, OAuth |
| **Azure** | az (Azure CLI) | MSI installer, Homebrew, apt | Service principals, managed identity, device code |

### Terraform Provider Maturity

| Provider | Version | Resources | Data Sources | Maturity |
|----------|---------|-----------|--------------|----------|
| **hashicorp/aws** | 5.0+ | 1,200+ | 600+ | ✅ Most comprehensive |
| **hashicorp/google** | 5.0+ | 800+ | 400+ | ✅ Excellent coverage |
| **hashicorp/azurerm** | 3.0+ | 1,100+ | 500+ | ✅ Strong, improving |

### Native IaC Tools

**AWS CloudFormation:**

- YAML/JSON templates, drift detection, StackSets for multi-account
- Change sets for preview, nested stacks for modularity

**GCP Deployment Manager:**

- YAML/Jinja2/Python templates, less mature than competitors
- Many teams use Terraform instead

**Azure Resource Manager (ARM) + Bicep:**

- ARM: JSON templates, verbose
- Bicep: Domain-specific language, transpiles to ARM, type safety, modules

## Regional Availability

### Region Count (as of 2025)

| Provider | Regions | Availability Zones | Edge Locations |
|----------|---------|-------------------|----------------|
| **AWS** | 32 | 100+ | 400+ CloudFront PoPs |
| **GCP** | 40 | 120+ | 200+ edge locations |
| **Azure** | 60+ | 160+ | 200+ CDN PoPs |

### Multi-Region Strategies

**Active-Active:**

- AWS: Route 53 health checks, global DynamoDB tables, Aurora Global Database
- GCP: Global HTTP(S) Load Balancer, Cloud Spanner (globally distributed)
- Azure: Traffic Manager, Cosmos DB multi-region writes, Front Door

**Disaster Recovery:**

- Cross-region replication for object storage (all platforms)
- Database read replicas in secondary regions
- Infrastructure as Code for rapid region failover

## Certifications

### AWS

| Level | Certification | Focus |
|-------|---------------|-------|
| **Foundational** | Cloud Practitioner | Overview, billing, basic services |
| **Associate** | Solutions Architect | Design distributed systems |
| **Associate** | Developer | Build and deploy applications |
| **Associate** | SysOps Administrator | Operations and automation |
| **Professional** | Solutions Architect | Complex, multi-tier architectures |
| **Professional** | DevOps Engineer | CI/CD, infrastructure as code |
| **Specialty** | Security, Networking, ML, etc. | Domain expertise |

### GCP

| Level | Certification | Focus |
|-------|---------------|-------|
| **Foundational** | Cloud Digital Leader | Business-focused cloud concepts |
| **Associate** | Cloud Engineer | Deploy applications, monitor operations |
| **Professional** | Cloud Architect | Design infrastructure, optimize costs |
| **Professional** | Data Engineer | Build data processing systems |
| **Professional** | DevOps Engineer | CI/CD, SRE practices |
| **Professional** | Cloud Security Engineer | Security controls, compliance |

### Azure

| Level | Certification | Focus |
|-------|---------------|-------|
| **Fundamentals** | AZ-900 | Cloud concepts, core services |
| **Associate** | AZ-104 (Administrator) | Manage resources, identity, storage |
| **Associate** | AZ-204 (Developer) | Build cloud solutions, APIs |
| **Expert** | AZ-305 (Architect) | Design infrastructure and applications |
| **Expert** | AZ-400 (DevOps Engineer) | CI/CD, monitoring, security |
| **Specialty** | Security (AZ-500), AI (AI-102), etc. | Domain expertise |

## Decision Guide

### Choose AWS If

| Scenario | Reason |
|----------|--------|
| **Widest service catalog needed** | 200+ services, most mature ecosystem |
| **Enterprise adoption priority** | Largest market share, extensive partner network |
| **Complex hybrid requirements** | Outposts, Wavelength, Local Zones |
| **Deep ML/AI customization** | SageMaker flexibility, custom silicon (Trainium) |
| **Strong third-party integrations** | Largest marketplace, most tooling support |

### Choose GCP If

| Scenario | Reason |
|----------|--------|
| **Data analytics workloads** | BigQuery performance, Dataflow, Looker |
| **Kubernetes-native applications** | GKE Autopilot, Anthos for hybrid |
| **Cost optimization priority** | Per-second billing, sustained use discounts |
| **Google services integration** | Workspace, Firebase, Maps, TensorFlow |
| **Global reach with simplicity** | Unified VPC, global load balancing |

### Choose Azure If

| Scenario | Reason |
|----------|--------|
| **Microsoft ecosystem** | Active Directory, Office 365, Windows Server |
| **Enterprise .NET workloads** | Native C#/F# support, Visual Studio integration |
| **Hybrid cloud requirements** | Azure Arc, Azure Stack, seamless on-prem integration |
| **Government/compliance** | Azure Government, strong compliance portfolio |
| **Strong PaaS needs** | App Service, Logic Apps, extensive PaaS catalog |

## Hybrid and Multi-Cloud

### Hybrid Solutions

**AWS Outposts:**

- AWS hardware in your datacenter
- Consistent APIs, local data residency
- ECS, EKS, RDS, S3 locally

**GCP Anthos:**

- Kubernetes-based hybrid platform
- Run on-prem, AWS, Azure, or GCP
- Centralized policy and config management

**Azure Arc:**

- Manage servers, Kubernetes, data services anywhere
- Azure Policy and RBAC across environments
- SQL Managed Instance on-prem

### Multi-Cloud Management

| Tool | Purpose | Supports |
|------|---------|----------|
| **Terraform** | Infrastructure as Code | All platforms, 3,000+ providers |
| **Pulumi** | IaC with programming languages | AWS, GCP, Azure, Kubernetes |
| **Crossplane** | Kubernetes-native multi-cloud control | AWS, GCP, Azure via K8s CRDs |
| **Spacelift** | Terraform/OpenTofu management | Policy as code, drift detection |

## Best Practices

### Cost Optimization

**Right-Sizing:**

- Start small, scale up based on metrics
- Use monitoring to identify underutilized resources
- Reserved instances for 24/7 workloads, spot for batch jobs

**Resource Cleanup:**

- Tag everything for cost allocation
- Automated shutdown of dev/test environments
- Lifecycle policies for old data (S3 Glacier, Azure Archive)

**Cost Tools:**

- AWS Cost Explorer, Budgets, Savings Plans recommendations
- GCP Cost Management, Committed Use Discount analyzer
- Azure Cost Management + Billing, Advisor recommendations
- Third-party: Infracost, CloudHealth, Kubecost

### Security

**Identity & Access:**

- Principle of least privilege (IAM policies)
- Multi-factor authentication for all users
- Service accounts/managed identities for workloads
- Rotate credentials regularly, use secrets managers

**Network Security:**

- Private subnets for databases and backends
- Security groups (AWS), firewall rules (GCP), NSGs (Azure)
- VPN or private connectivity for sensitive data
- Enable flow logs for traffic analysis

**Data Protection:**

- Encryption at rest (default for most services)
- Encryption in transit (TLS 1.2+, HTTPS)
- Regular backups, test restoration procedures
- Immutable backups for ransomware protection

### Resilience

**Availability:**

- Multi-AZ deployments for production workloads
- Health checks and auto-scaling groups
- Load balancers across availability zones
- Database read replicas for read-heavy workloads

**Disaster Recovery:**

- Define RTO (Recovery Time Objective) and RPO (Recovery Point Objective)
- Automated backups with cross-region replication
- Infrastructure as Code for rapid environment rebuilds
- Regular DR drills and runbooks

## Common Pitfalls

| Mistake | Impact | Mitigation |
|---------|--------|------------|
| **No cost monitoring** | Bill shock, wasted spend | Set budgets, alerts, tagging strategy |
| **Public resources** | Security breaches | Default to private, whitelist access |
| **Single AZ deployments** | Outage vulnerability | Multi-AZ by default for production |
| **Vendor lock-in** | Migration difficulty | Abstract with Terraform, use portable services |
| **No backup testing** | Failed recovery | Automated restore tests, DR drills |
| **Overly permissive IAM** | Privilege escalation | Least privilege, regular access reviews |
| **Ignoring data transfer costs** | Unexpected bills | Keep data in same region/AZ when possible |

## Related

- [[Kubernetes]] - Container orchestration across cloud platforms
- [[Infrastructure as Code]] - Terraform, Pulumi for cloud provisioning
- [[Docker]] - Container runtime for cloud deployments
- [[Observability Tools]] - Prometheus, Grafana for cloud monitoring
- [[CI-CD Tools]] - GitHub Actions, GitLab CI for cloud deployments
