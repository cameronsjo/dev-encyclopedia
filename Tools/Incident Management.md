---
title: Incident Management
aliases:
  - Incident Response
  - On-Call
  - Postmortems
  - Blameless Culture
tags:
  - tools
  - devops
  - sre
  - operations
  - reliability
type: reference
status: complete
created: "2025-12-16"
---

# Incident Management

Structured approach to detecting, responding to, resolving, and learning from production incidents.

## Overview

| Phase | Focus |
|-------|-------|
| **Detection** | Monitoring, alerting, customer reports |
| **Response** | Triage, communication, assembly |
| **Mitigation** | Stop the bleeding |
| **Resolution** | Full fix and recovery |
| **Learning** | Postmortem, action items |

## Incident Lifecycle

```
Detection → Triage → Response → Mitigation → Resolution → Postmortem
    │         │         │           │            │            │
    ▼         ▼         ▼           ▼            ▼            ▼
  Alert    Severity  Assemble    Stop the    Full fix     Learn &
 fires     assign     team       bleeding    deployed     improve
```

## Severity Levels

| Level | Name | Description | Response |
|-------|------|-------------|----------|
| **SEV1** | Critical | Complete outage, data loss | Immediate, all hands |
| **SEV2** | Major | Significant degradation | Immediate, on-call team |
| **SEV3** | Minor | Limited impact | Business hours |
| **SEV4** | Low | Minimal impact | Ticket queue |

### Severity Matrix

| Impact | Widespread | Limited | Minimal |
|--------|------------|---------|---------|
| **Complete loss** | SEV1 | SEV2 | SEV3 |
| **Degraded** | SEV2 | SEV3 | SEV4 |
| **Inconvenience** | SEV3 | SEV4 | SEV4 |

## Incident Roles

### Core Roles

| Role | Responsibilities |
|------|-----------------|
| **Incident Commander (IC)** | Overall coordination, decisions |
| **Communications Lead** | Status updates, stakeholder comms |
| **Operations Lead** | Technical investigation, mitigation |
| **Scribe** | Document timeline, actions, decisions |

### Incident Commander Duties

```
┌─────────────────────────────────────────────────────┐
│              Incident Commander                      │
├─────────────────────────────────────────────────────┤
│                                                      │
│  • Declare incident severity                        │
│  • Assign roles                                      │
│  • Coordinate responders                            │
│  • Make decisions (or delegate)                     │
│  • Keep focus on mitigation                         │
│  • Call for escalation when needed                  │
│  • Declare incident resolved                        │
│                                                      │
│  ❌ NOT responsible for:                            │
│  • Doing the technical work                         │
│  • Knowing all the answers                          │
│                                                      │
└─────────────────────────────────────────────────────┘
```

## Incident Response Process

### 1. Detection & Declaration

```markdown
## Detection Checklist
- [ ] Alert fired (or customer report received)
- [ ] Verify alert is valid (not false positive)
- [ ] Assess initial scope and impact
- [ ] Declare incident and severity
- [ ] Create incident channel/bridge
- [ ] Page appropriate responders
```

### 2. Assembly & Triage

```markdown
## Initial Response (First 5 Minutes)
- [ ] IC identified and announced
- [ ] Roles assigned (Comms, Ops, Scribe)
- [ ] Initial status posted
- [ ] Known information shared
- [ ] Initial theories identified
```

### 3. Investigation & Mitigation

```markdown
## Investigation
- [ ] Check recent deployments
- [ ] Review recent config changes
- [ ] Examine metrics and logs
- [ ] Check dependencies
- [ ] Identify blast radius

## Mitigation Options
- [ ] Rollback deployment
- [ ] Toggle feature flag
- [ ] Scale up resources
- [ ] Failover to backup
- [ ] Enable degraded mode
```

### 4. Communication

**Internal Status Updates:**

```markdown
## Status Update - 14:35 UTC

**Severity:** SEV1
**Status:** Investigating
**Duration:** 15 minutes

**Impact:**
- Payment processing failing for ~30% of users
- Error rate: 28% (normally <1%)

**Current Actions:**
- Investigating correlation with deploy at 14:15
- Preparing rollback as mitigation option

**Next Update:** 14:45 UTC or on significant change
```

**External Communication:**

```markdown
## Statuspage Update

**Investigating Payment Issues**

We are currently investigating issues with payment
processing. Some users may experience failures when
completing purchases.

Our team is actively working on resolution. We will
provide updates as we have more information.

Posted: 14:35 UTC
```

### 5. Resolution

```markdown
## Resolution Checklist
- [ ] Service restored to normal operation
- [ ] Metrics confirm recovery
- [ ] Customer-facing comms updated
- [ ] Incident timeline documented
- [ ] IC declares incident resolved
- [ ] Schedule postmortem
```

## On-Call Best Practices

### On-Call Rotation

| Practice | Rationale |
|----------|-----------|
| **1-week rotations** | Long enough to learn, short enough to not burn out |
| **Primary + Secondary** | Backup for coverage gaps |
| **Follow-the-sun** | 24/7 without night shifts |
| **Minimum team size: 6** | Sustainable rotation |

### On-Call Responsibilities

```markdown
## On-Call Engineer Duties

**During Shift:**
- Respond to pages within SLA (typically 5-15 min)
- Acknowledge or escalate all alerts
- Update incident channels
- Perform handoff at rotation end

**Page Response:**
1. Acknowledge the page
2. Assess severity
3. Begin investigation or escalate
4. Communicate status
```

### Reducing On-Call Burden

| Action | Impact |
|--------|--------|
| **Fix noisy alerts** | Fewer false positives |
| **Automate remediation** | Reduce human intervention |
| **Improve runbooks** | Faster resolution |
| **Invest in reliability** | Fewer incidents overall |

## Postmortems

### Blameless Culture

```markdown
## Blameless Postmortem Principles

1. **Assume good intent** - People tried to do the right thing
2. **Focus on systems** - What allowed this to happen?
3. **Learn, don't blame** - Goal is improvement, not punishment
4. **Share widely** - Transparency builds trust
5. **Follow through** - Action items must be completed
```

### Postmortem Template

```markdown
# Incident Postmortem: [Title]

**Date:** 2024-01-15
**Duration:** 45 minutes
**Severity:** SEV2
**Author:** @jane
**Reviewers:** @ops-team

## Summary
Brief description of what happened and impact.

## Impact
- 30% of payment requests failed
- ~500 affected transactions
- Estimated revenue impact: $X

## Timeline (UTC)
| Time | Event |
|------|-------|
| 14:15 | Deploy of payment-service v2.3.4 |
| 14:20 | First alert: Error rate > 10% |
| 14:22 | On-call acknowledges, begins investigation |
| 14:30 | Correlation with deploy identified |
| 14:35 | Rollback initiated |
| 14:40 | Rollback complete, errors decreasing |
| 14:50 | Error rate normal, incident resolved |

## Root Cause
The deploy included a database query that performed a
full table scan instead of using an index, causing
connection pool exhaustion under load.

## Contributing Factors
- Query was tested but not at production scale
- Load testing does not cover payment flow
- No query performance monitoring in CI

## What Went Well
- Fast detection (5 minutes)
- Quick correlation with deploy
- Rollback was smooth

## What Could Be Improved
- Detection could have been faster with better metrics
- Load testing should cover critical paths
- Query review process needed

## Action Items
| Action | Owner | Due | Status |
|--------|-------|-----|--------|
| Add query performance tests | @jane | 2024-01-22 | TODO |
| Implement load testing for payments | @bob | 2024-01-29 | TODO |
| Add connection pool metrics | @alice | 2024-01-20 | TODO |
| Review deploy process | @ops | 2024-01-25 | TODO |

## Lessons Learned
- Database queries need production-scale testing
- Connection pool exhaustion is a common failure mode
- Quick rollback capability is essential
```

### Postmortem Meeting

| Phase | Duration | Focus |
|-------|----------|-------|
| **Review timeline** | 10 min | What happened when |
| **Root cause analysis** | 15 min | Why did this happen |
| **Contributing factors** | 10 min | What enabled this |
| **Action items** | 15 min | What will we do |
| **Wrap up** | 5 min | Next steps, owners |

## Tools

### Incident Management Platforms

| Tool | Features |
|------|----------|
| **PagerDuty** | Alerting, on-call, incident response |
| **Opsgenie** | Alerting, on-call management |
| **Incident.io** | Incident response, postmortems |
| **Rootly** | Incident management, Slack-native |
| **FireHydrant** | Incident response automation |
| **Jira Service Management** | ITSM, incident tracking |

### Status Pages

| Tool | Type |
|------|------|
| **Statuspage (Atlassian)** | Hosted |
| **Cachet** | Self-hosted |
| **Instatus** | Hosted |
| **Status.io** | Hosted |

### Communication

| Tool | Use |
|------|-----|
| **Slack/Teams** | Incident channels |
| **Zoom/Meet** | War room bridges |
| **Statuspage** | External communication |

## Metrics

### Incident Metrics

| Metric | Definition |
|--------|------------|
| **MTTD** | Mean Time to Detect |
| **MTTA** | Mean Time to Acknowledge |
| **MTTR** | Mean Time to Resolve |
| **MTBF** | Mean Time Between Failures |
| **Incident Count** | By severity, by service |

### Tracking Example

```
┌─────────────────────────────────────────────────────┐
│           Incident Metrics - Q4 2024                │
├─────────────────────────────────────────────────────┤
│                                                      │
│  Total Incidents: 23                                │
│  SEV1: 2  SEV2: 8  SEV3: 13                        │
│                                                      │
│  MTTD: 4.2 min  (target: <5 min)  ✅               │
│  MTTA: 6.8 min  (target: <15 min) ✅               │
│  MTTR: 38 min   (target: <60 min) ✅               │
│                                                      │
│  Top Causes:                                         │
│  1. Deploy issues (35%)                             │
│  2. Dependency failures (26%)                       │
│  3. Resource exhaustion (22%)                       │
│                                                      │
└─────────────────────────────────────────────────────┘
```

## Anti-Patterns

| Anti-Pattern | Problem | Solution |
|--------------|---------|----------|
| **Hero culture** | Burnout, single point of failure | Spread knowledge, rotate |
| **Alert fatigue** | Ignored alerts | Fix noisy alerts |
| **Blame** | Hidden problems, fear | Blameless culture |
| **No postmortems** | Same incidents repeat | Always learn |
| **Postmortem theater** | Action items ignored | Track completion |

## Related

- [[SLOs and SLIs]] — Reliability targets
- [[Runbooks]] — Operational procedures
- [[Chaos Engineering]] — Proactive resilience
- [[Observability Stack]] — Detection capabilities
- [[Tools MOC]] — All tools
