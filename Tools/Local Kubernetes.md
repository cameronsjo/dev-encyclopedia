---
title: Local Kubernetes
aliases:
  - Local K8s
  - KIND
  - K3s
  - minikube
tags:
  - tool
  - kubernetes
  - containers
  - local-dev
type: reference
status: complete
created: "2025-12-18"
---

# Local Kubernetes

Running Kubernetes clusters on your development machine.

## Overview

| Tool | Best For | Resource Usage | Multi-Node |
|------|----------|----------------|------------|
| **KIND** | CI/CD, testing | Medium | Yes |
| **K3s** | Edge, lightweight | Low | Yes |
| **minikube** | Learning, simple | Medium-High | Limited |
| **Docker Desktop** | Convenience | High | No |
| **Rancher Desktop** | Docker alternative | Medium | No |

## KIND (Kubernetes IN Docker)

**Runs K8s nodes as Docker containers.**

### Installation

```bash
# macOS
brew install kind

# Linux
curl -Lo ./kind https://kind.sigs.k8s.io/dl/v0.20.0/kind-linux-amd64
chmod +x ./kind
sudo mv ./kind /usr/local/bin/kind

# Windows
choco install kind
```

### Basic Usage

```bash
# Create cluster
kind create cluster

# Create named cluster
kind create cluster --name my-cluster

# List clusters
kind get clusters

# Delete cluster
kind delete cluster --name my-cluster

# Get kubeconfig
kind get kubeconfig --name my-cluster
```

### Multi-Node Cluster

```yaml
# kind-config.yaml
kind: Cluster
apiVersion: kind.x-k8s.io/v1alpha4
nodes:
  - role: control-plane
  - role: worker
  - role: worker
```

```bash
kind create cluster --config kind-config.yaml
```

### With Ingress

```yaml
# kind-ingress.yaml
kind: Cluster
apiVersion: kind.x-k8s.io/v1alpha4
nodes:
  - role: control-plane
    kubeadmConfigPatches:
      - |
        kind: InitConfiguration
        nodeRegistration:
          kubeletExtraArgs:
            node-labels: "ingress-ready=true"
    extraPortMappings:
      - containerPort: 80
        hostPort: 80
        protocol: TCP
      - containerPort: 443
        hostPort: 443
        protocol: TCP
```

```bash
kind create cluster --config kind-ingress.yaml

# Install nginx ingress
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/main/deploy/static/provider/kind/deploy.yaml
```

### Load Local Images

```bash
# Build image
docker build -t myapp:latest .

# Load into KIND (no registry needed!)
kind load docker-image myapp:latest --name my-cluster

# Use in deployment
# image: myapp:latest
# imagePullPolicy: Never
```

## K3s

**Lightweight Kubernetes for edge/IoT, also great for local dev.**

### Installation

```bash
# Linux (single node)
curl -sfL https://get.k3s.io | sh -

# Check status
sudo systemctl status k3s

# Get kubeconfig
sudo cat /etc/rancher/k3s/k3s.yaml

# Copy to local kubeconfig
mkdir -p ~/.kube
sudo cp /etc/rancher/k3s/k3s.yaml ~/.kube/config
sudo chown $USER ~/.kube/config
```

### K3d (K3s in Docker)

```bash
# Install k3d (runs K3s in Docker)
curl -s https://raw.githubusercontent.com/k3d-io/k3d/main/install.sh | bash
# or
brew install k3d

# Create cluster
k3d cluster create my-cluster

# With port mapping
k3d cluster create my-cluster -p "8080:80@loadbalancer"

# Multi-node
k3d cluster create my-cluster --agents 2

# List clusters
k3d cluster list

# Delete
k3d cluster delete my-cluster
```

### K3d with Registry

```bash
# Create cluster with local registry
k3d cluster create my-cluster --registry-create my-registry:5000

# Tag and push image
docker tag myapp:latest localhost:5000/myapp:latest
docker push localhost:5000/myapp:latest

# Use in deployment
# image: my-registry:5000/myapp:latest
```

### K3d Config File

```yaml
# k3d-config.yaml
apiVersion: k3d.io/v1alpha5
kind: Simple
metadata:
  name: my-cluster
servers: 1
agents: 2
ports:
  - port: 8080:80
    nodeFilters:
      - loadbalancer
registries:
  create:
    name: my-registry
    host: "0.0.0.0"
    hostPort: "5000"
options:
  k3s:
    extraArgs:
      - arg: --disable=traefik
        nodeFilters:
          - server:*
```

```bash
k3d cluster create --config k3d-config.yaml
```

## minikube

**Original local K8s, good for learning.**

### Installation

```bash
# macOS
brew install minikube

# Linux
curl -LO https://storage.googleapis.com/minikube/releases/latest/minikube-linux-amd64
sudo install minikube-linux-amd64 /usr/local/bin/minikube

# Windows
choco install minikube
```

### Basic Usage

```bash
# Start (uses Docker by default)
minikube start

# With specific driver
minikube start --driver=docker
minikube start --driver=hyperkit  # macOS
minikube start --driver=hyperv    # Windows

# With resources
minikube start --cpus=4 --memory=8192

# Status
minikube status

# Stop
minikube stop

# Delete
minikube delete
```

### Useful Features

```bash
# Dashboard
minikube dashboard

# Access service via tunnel
minikube service my-service

# SSH into node
minikube ssh

# Mount local directory
minikube mount /local/path:/container/path

# Enable addons
minikube addons list
minikube addons enable ingress
minikube addons enable metrics-server
```

### Local Images

```bash
# Point Docker CLI to minikube's Docker
eval $(minikube docker-env)

# Now builds go directly to minikube
docker build -t myapp:latest .

# Use in deployment (no push needed)
# image: myapp:latest
# imagePullPolicy: Never
```

## Comparison

### When to Use What

| Scenario | Best Choice |
|----------|-------------|
| **CI/CD pipelines** | KIND |
| **Quick local testing** | KIND or k3d |
| **Learning Kubernetes** | minikube |
| **Multi-node testing** | KIND or k3d |
| **Resource-constrained** | k3d (K3s) |
| **Edge/IoT simulation** | K3s |
| **Just need kubectl** | Docker Desktop |

### Resource Comparison

| Tool | RAM (1 node) | Startup Time |
|------|--------------|--------------|
| **KIND** | ~1-2 GB | ~30-60s |
| **k3d** | ~500MB-1GB | ~20-30s |
| **minikube** | ~2-4 GB | ~60-120s |
| **Docker Desktop** | ~2-4 GB | Already running |

## Common Patterns

### Development Workflow

```bash
# 1. Create cluster
kind create cluster --name dev

# 2. Set context
kubectl config use-context kind-dev

# 3. Deploy app
kubectl apply -f k8s/

# 4. Port forward for testing
kubectl port-forward svc/my-service 8080:80

# 5. Make changes, rebuild
docker build -t myapp:latest .
kind load docker-image myapp:latest --name dev
kubectl rollout restart deployment/my-app

# 6. Cleanup when done
kind delete cluster --name dev
```

### Tilt (Hot Reload)

```python
# Tiltfile
docker_build('myapp', '.')
k8s_yaml('k8s/deployment.yaml')
k8s_resource('my-app', port_forwards=8080)
```

```bash
# Install Tilt
curl -fsSL https://raw.githubusercontent.com/tilt-dev/tilt/master/scripts/install.sh | bash

# Run (watches files, auto-rebuilds)
tilt up
```

### Skaffold (CI/CD focused)

```yaml
# skaffold.yaml
apiVersion: skaffold/v4beta6
kind: Config
build:
  artifacts:
    - image: myapp
deploy:
  kubectl:
    manifests:
      - k8s/*.yaml
```

```bash
# Dev mode (watch + rebuild)
skaffold dev

# One-time deploy
skaffold run
```

## Local Registry

### Docker Registry

```bash
# Run local registry
docker run -d -p 5000:5000 --name registry registry:2

# Connect KIND to registry
# (add to kind config)
containerdConfigPatches:
  - |-
    [plugins."io.containerd.grpc.v1.cri".registry.mirrors."localhost:5000"]
      endpoint = ["http://registry:5000"]

# Push images
docker tag myapp localhost:5000/myapp
docker push localhost:5000/myapp
```

## Tips

| Tip | Details |
|-----|---------|
| **Use contexts** | `kubectl config get-contexts` to avoid accidents |
| **Resource limits** | Set in your manifests for realistic testing |
| **Clean up** | Delete clusters when not in use |
| **Persistence** | Use PVCs to test storage |
| **Network policies** | KIND supports Calico for realistic networking |
| **Multiple clusters** | Easy to spin up for testing upgrades |

## Related

- [[Kubernetes]] — Kubernetes overview
- [[Container Runtimes]] — Docker, Podman
- [[Local Observability]] — Grafana, Prometheus locally
- [[Tools MOC]] — All tools
