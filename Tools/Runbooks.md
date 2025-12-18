---
title: Runbooks
aliases:
  - Operations Runbooks
  - Playbooks
  - SOPs
  - Standard Operating Procedures
tags:
  - tools
  - devops
  - sre
  - operations
  - documentation
type: reference
status: complete
created: "2025-12-16"
---

# Runbooks

Documented procedures for operating systems, responding to incidents, and performing routine tasks.

## Overview

| Type | Purpose | When Used |
|------|---------|-----------|
| **Alert Runbooks** | Respond to specific alerts | Incident response |
| **Operations Runbooks** | Routine maintenance | Scheduled tasks |
| **Troubleshooting Guides** | Diagnose issues | Investigation |
| **Deployment Runbooks** | Release procedures | Deployments |

## Runbook Structure

### Standard Template

```markdown
# [Runbook Title]

**Service:** payment-service
**Owner:** @payments-team
**Last Updated:** 2024-01-15
**Review Frequency:** Quarterly

## Overview
Brief description of what this runbook covers.

## Prerequisites
- Access requirements
- Tools needed
- Knowledge required

## Procedure
Step-by-step instructions.

## Verification
How to confirm success.

## Rollback
How to undo if needed.

## Escalation
When and who to escalate to.

## Related
Links to related runbooks and documentation.
```

## Alert Runbook Example

```markdown
# High Error Rate - Payment Service

**Alert:** payment-service-high-error-rate
**Severity:** SEV2
**Service:** payment-service
**Owner:** @payments-team

## Alert Details

**Query:**
```promql
sum(rate(http_requests_total{service="payment",status=~"5.."}[5m]))
/
sum(rate(http_requests_total{service="payment"}[5m])) > 0.01
```

**Threshold:** Error rate > 1%
**Duration:** 5 minutes

## Impact

Payment processing may be failing for users. Revenue impact
possible if not resolved quickly.

## Quick Diagnosis

### 1. Check Error Distribution

```bash
# View error breakdown by endpoint
kubectl logs -l app=payment-service --tail=100 | \
  grep "ERROR" | \
  jq '.endpoint' | sort | uniq -c | sort -rn
```

### 2. Check Dependencies

- [ ] Database: [DB Dashboard](https://grafana/db)
- [ ] Payment Gateway: [Gateway Status](https://status.stripe.com)
- [ ] Redis: [Cache Dashboard](https://grafana/redis)

### 3. Check Recent Changes

```bash
# Recent deployments
kubectl rollout history deployment/payment-service

# Recent config changes
git log --oneline -10 -- config/payment/
```

## Common Causes & Fixes

### Database Connection Issues

**Symptoms:** Timeout errors, connection refused
**Check:**

```bash
kubectl exec -it payment-service-xxx -- \
  pg_isready -h $DB_HOST
```

**Fix:** Scale up connection pool or restart pods

### Payment Gateway Timeout

**Symptoms:** Gateway timeout errors
**Check:** [Stripe Status](https://status.stripe.com)
**Fix:** Enable circuit breaker fallback

### Memory Pressure

**Symptoms:** OOMKilled pods, slow responses
**Check:**

```bash
kubectl top pods -l app=payment-service
```

**Fix:** Scale horizontally or increase memory limits

## Mitigation Steps

### Option 1: Rollback (if recent deploy)

```bash
kubectl rollout undo deployment/payment-service
kubectl rollout status deployment/payment-service
```

### Option 2: Scale Up

```bash
kubectl scale deployment/payment-service --replicas=10
```

### Option 3: Enable Degraded Mode

```bash
# Toggle feature flag
curl -X POST https://feature-flags/api/flags/payment-degraded-mode \
  -d '{"enabled": true}'
```

## Verification

- [ ] Error rate returned to < 1%
- [ ] Latency p99 < 200ms
- [ ] No new errors in logs
- [ ] Payments processing successfully

## Escalation Paths

| Condition | Escalate To |
|-----------|-------------|
| Cannot diagnose in 15 min | Senior on-call |
| Database issues confirmed | DBA on-call |
| Payment gateway issue | Stripe support |
| Revenue impact > $10k | Engineering management |

## Related Runbooks

- [[Database Failover Runbook]]
- [[Payment Gateway Fallback]]
- [[Scaling Procedures]]

```

## Operations Runbook Example

```markdown
# Database Backup Verification

**Frequency:** Weekly
**Owner:** @platform-team
**Duration:** 30 minutes

## Overview
Verify database backups are completing successfully and
can be restored.

## Prerequisites
- [ ] Access to backup storage (S3)
- [ ] Access to staging environment
- [ ] Database admin credentials

## Procedure

### 1. Check Backup Completion
```bash
# List recent backups
aws s3 ls s3://backups/postgres/ --recursive | \
  sort | tail -7
```

Expected: Daily backups for last 7 days

### 2. Verify Backup Integrity

```bash
# Download latest backup
aws s3 cp s3://backups/postgres/latest.sql.gz ./

# Check file integrity
gunzip -t latest.sql.gz

# Check file size (should be > 1GB for production)
ls -lh latest.sql.gz
```

### 3. Test Restore (Staging)

```bash
# Restore to staging
gunzip -c latest.sql.gz | \
  psql -h staging-db.internal -U admin -d restore_test

# Verify row counts
psql -h staging-db.internal -d restore_test -c \
  "SELECT COUNT(*) FROM users;"
```

### 4. Document Results

Record in [Backup Verification Log](https://wiki/backup-log):

- Date verified
- Backup size
- Restore time
- Any issues found

## Success Criteria

- [ ] Backup files exist for all scheduled days
- [ ] Files are not corrupted (gunzip passes)
- [ ] Restore completes without errors
- [ ] Data spot-check passes

## Failure Actions

| Issue | Action |
|-------|--------|
| Missing backup | Check backup job logs, alert DBA |
| Corrupted file | Restore from previous day, investigate |
| Restore fails | Page DBA on-call |

## Escalation Procedures

- Missing >1 day of backups: Page DBA
- Restore failure: SEV3 incident

```

## Troubleshooting Guide Example

```markdown
# Troubleshooting: Slow API Response Times

**Service:** api-gateway
**Owner:** @platform-team

## Symptoms
- API latency > 500ms (normally < 100ms)
- User complaints about slow page loads
- Timeout errors increasing

## Diagnosis Flowchart

```

Start
  │
  ▼
Is it all endpoints? ──No──► Check specific endpoint
  │                           (see "Single Endpoint" below)
  Yes
  │
  ▼
Check dependencies ──Slow──► Diagnose dependency
  │                          (Database? Cache? External?)
  Healthy
  │
  ▼
Check resource usage ──High──► Scale or optimize
  │                             (CPU? Memory? Connections?)
  Normal
  │
  ▼
Check recent changes ──Found──► Consider rollback
  │
  None
  │
  ▼
Deep dive with profiling

```

## Diagnostic Commands

### Overall Health
```bash
# Check pod health
kubectl get pods -l app=api-gateway

# Check resource usage
kubectl top pods -l app=api-gateway

# Check recent events
kubectl get events --sort-by='.lastTimestamp' | \
  grep api-gateway
```

### Network Diagnostics

```bash
# Check DNS resolution time
time nslookup database.internal

# Check connection to dependencies
kubectl exec api-gateway-xxx -- \
  curl -w "@curl-format.txt" -o /dev/null -s \
  http://user-service/health
```

### Database Diagnostics

```bash
# Check active connections
psql -c "SELECT count(*) FROM pg_stat_activity;"

# Check slow queries
psql -c "SELECT query, calls, mean_time
         FROM pg_stat_statements
         ORDER BY mean_time DESC LIMIT 10;"
```

### Application Diagnostics

```bash
# Get thread dump (Java)
kubectl exec api-gateway-xxx -- \
  jstack $(pgrep java)

# Check GC logs
kubectl logs api-gateway-xxx | grep "GC"

# Enable debug logging temporarily
kubectl set env deployment/api-gateway LOG_LEVEL=DEBUG
```

## Common Causes

### 1. Database Slow Queries

**Indicators:** High DB latency in traces
**Fix:**

- Add missing indexes
- Optimize queries
- Scale read replicas

### 2. Connection Pool Exhaustion

**Indicators:** Connection timeout errors
**Fix:**

- Increase pool size
- Check for connection leaks
- Add connection timeout

### 3. Memory Pressure / GC

**Indicators:** High GC pause times, OOM events
**Fix:**

- Increase heap size
- Tune GC parameters
- Fix memory leaks

### 4. Noisy Neighbor

**Indicators:** CPU throttling, variable latency
**Fix:**

- Request dedicated nodes
- Implement resource limits
- Scale horizontally

## Resolution Verification

- [ ] Latency returned to baseline
- [ ] Error rate normal
- [ ] No ongoing resource pressure
- [ ] Root cause identified and tracked

```

## Best Practices

### Writing Good Runbooks

| Practice | Rationale |
|----------|-----------|
| **Use simple language** | Stressful situations impair reading |
| **Be specific** | Exact commands, not descriptions |
| **Include copy-paste** | Reduce typing errors |
| **Add verification steps** | Confirm each action worked |
| **Show expected output** | So operators know what's normal |
| **Keep current** | Outdated runbooks cause harm |

### Runbook Maintenance

```markdown
## Runbook Review Checklist (Quarterly)

- [ ] Run through procedure in staging
- [ ] Verify all commands still work
- [ ] Update screenshots/output examples
- [ ] Check all links are valid
- [ ] Review escalation contacts
- [ ] Update based on recent incidents
- [ ] Get peer review
```

### Runbook Automation

| Level | Description |
|-------|-------------|
| **Manual** | Human follows document |
| **Assisted** | Scripts for complex steps |
| **Semi-automated** | Human triggers, automation runs |
| **Fully automated** | No human intervention |

```bash
# Example: Assisted runbook script
#!/bin/bash
# runbook-high-error-rate.sh

echo "=== Payment Service High Error Rate Runbook ==="

echo -e "\n[1/4] Checking error distribution..."
kubectl logs -l app=payment-service --tail=100 2>/dev/null | \
  grep "ERROR" | head -20

echo -e "\n[2/4] Checking recent deployments..."
kubectl rollout history deployment/payment-service | tail -5

echo -e "\n[3/4] Checking resource usage..."
kubectl top pods -l app=payment-service

echo -e "\n[4/4] Checking dependencies..."
for dep in database redis payment-gateway; do
  echo -n "$dep: "
  kubectl exec deploy/payment-service -- \
    curl -s -o /dev/null -w "%{http_code}" http://$dep/health
  echo
done

echo -e "\n=== Suggested Actions ==="
echo "1. If recent deploy: kubectl rollout undo deployment/payment-service"
echo "2. If high resource: kubectl scale deployment/payment-service --replicas=10"
echo "3. If dependency down: Check dependency runbook"
```

## Tools

| Tool | Purpose |
|------|---------|
| **Confluence/Notion** | Documentation hosting |
| **Rundeck** | Runbook automation |
| **StackStorm** | Event-driven automation |
| **PagerDuty** | Runbook links in alerts |
| **Jupyter Notebooks** | Interactive runbooks |

## Anti-Patterns

| Anti-Pattern | Problem |
|--------------|---------|
| **Runbook rot** | Outdated procedures cause mistakes |
| **Too complex** | Won't be followed under stress |
| **Not tested** | May not work when needed |
| **Only one person knows** | Knowledge silos |
| **No verification steps** | Can't confirm success |

## Related

- [[Incident Management]] — Using runbooks in incidents
- [[SLOs and SLIs]] — What runbooks protect
- [[Chaos Engineering]] — Testing runbooks
- [[Technical Writing]] — Writing better docs
- [[Tools MOC]] — All tools
