---
title: Kubernetes
aliases:
  - K8s
  - Container Orchestration
tags:
  - tool
  - comparison
  - containers
  - kubernetes
  - devops
type: reference
status: complete
created: "2025-12-04"
---

# Kubernetes

Container orchestration platform and its lightweight distributions.

## Overview

| Distribution | Type | Use Case | Resources |
|--------------|------|----------|-----------|
| Kubernetes (K8s) | Full | Production | High |
| K3s | Lightweight | Edge, IoT, dev | Low |
| kind | Docker-based | CI/CD, testing | Medium |
| minikube | Local dev | Learning, dev | Medium |
| MicroK8s | Snap-based | Dev, edge | Low-Medium |
| Docker Desktop | Built-in | Local dev | Medium |
| Rancher Desktop | GUI + K3s | Local dev | Medium |

---

## Kubernetes Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Control Plane                             │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │ API Server  │  │  Scheduler  │  │ Controller  │              │
│  │             │  │             │  │  Manager    │              │
│  └──────┬──────┘  └─────────────┘  └─────────────┘              │
│         │                                                        │
│  ┌──────┴──────┐                                                │
│  │    etcd     │                                                │
│  │  (state)    │                                                │
│  └─────────────┘                                                │
└─────────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┼───────────────┐
              ▼               ▼               ▼
┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐
│   Worker Node   │ │   Worker Node   │ │   Worker Node   │
│  ┌───────────┐  │ │  ┌───────────┐  │ │  ┌───────────┐  │
│  │  kubelet  │  │ │  │  kubelet  │  │ │  │  kubelet  │  │
│  ├───────────┤  │ │  ├───────────┤  │ │  ├───────────┤  │
│  │kube-proxy │  │ │  │kube-proxy │  │ │  │kube-proxy │  │
│  ├───────────┤  │ │  ├───────────┤  │ │  ├───────────┤  │
│  │ Container │  │ │  │ Container │  │ │  │ Container │  │
│  │  Runtime  │  │ │  │  Runtime  │  │ │  │  Runtime  │  │
│  └───────────┘  │ │  └───────────┘  │ │  └───────────┘  │
│  ┌───┐ ┌───┐    │ │  ┌───┐ ┌───┐    │ │  ┌───┐ ┌───┐    │
│  │Pod│ │Pod│    │ │  │Pod│ │Pod│    │ │  │Pod│ │Pod│    │
│  └───┘ └───┘    │ │  └───┘ └───┘    │ │  └───┘ └───┘    │
└─────────────────┘ └─────────────────┘ └─────────────────┘
```

---

## K3s

Lightweight Kubernetes by Rancher. Single binary, production-ready.

### Key Features

- **Single binary** — ~70MB, includes everything
- **Low resources** — 512MB RAM minimum
- **Batteries included** — Traefik, ServiceLB, local-path storage
- **SQLite/etcd** — SQLite default, etcd optional
- **ARM support** — Raspberry Pi, edge devices

### Installation

```bash
# Server (control plane)
curl -sfL https://get.k3s.io | sh -

# Check status
sudo k3s kubectl get nodes
sudo systemctl status k3s

# Get kubeconfig
sudo cat /etc/rancher/k3s/k3s.yaml

# Agent (worker node)
curl -sfL https://get.k3s.io | K3S_URL=https://server:6443 \
  K3S_TOKEN=$(sudo cat /var/lib/rancher/k3s/server/node-token) sh -
```

### Configuration

```bash
# Install with options
curl -sfL https://get.k3s.io | sh -s - \
  --disable traefik \
  --disable servicelb \
  --write-kubeconfig-mode 644

# Or via config file
# /etc/rancher/k3s/config.yaml
write-kubeconfig-mode: "0644"
disable:
  - traefik
  - servicelb
cluster-init: true
```

### K3s vs K8s

| Aspect | K3s | K8s |
|--------|-----|-----|
| Binary size | ~70MB | ~300MB+ |
| RAM (min) | 512MB | 2GB+ |
| Default storage | SQLite | etcd |
| Default ingress | Traefik | None |
| Default LB | ServiceLB | None |
| HA setup | Built-in | Complex |
| Edge/IoT | ✅ Excellent | ❌ Heavy |

---

## kind (Kubernetes in Docker)

Run Kubernetes clusters using Docker containers as nodes.

### Key Features

- **Docker-based** — Nodes are containers
- **Multi-node** — Simulate real clusters
- **CI/CD friendly** — Fast creation/deletion
- **Config-driven** — YAML cluster definitions

### Installation

```bash
# macOS
brew install kind

# Linux
curl -Lo ./kind https://kind.sigs.k8s.io/dl/latest/kind-linux-amd64
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
  - role: worker
  - role: worker
  - role: worker
```

```bash
kind create cluster --config kind-config.yaml
```

### Load Images

```bash
# Load local image into kind
docker build -t myapp:latest .
kind load docker-image myapp:latest --name my-cluster

# Now use in pods
# image: myapp:latest
# imagePullPolicy: Never
```

---

## minikube

Local Kubernetes for learning and development.

### Key Features

- **Multiple drivers** — Docker, VirtualBox, Hyper-V, etc.
- **Addons** — Dashboard, ingress, metrics-server
- **Profiles** — Multiple clusters
- **LoadBalancer** — Tunnel support

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
# Start cluster
minikube start

# Start with specific driver
minikube start --driver=docker

# Start with resources
minikube start --cpus=4 --memory=8192

# Status
minikube status

# Stop/delete
minikube stop
minikube delete
```

### Addons

```bash
# List addons
minikube addons list

# Enable addons
minikube addons enable ingress
minikube addons enable dashboard
minikube addons enable metrics-server

# Access dashboard
minikube dashboard
```

### Access Services

```bash
# Get service URL
minikube service myservice --url

# Tunnel for LoadBalancer
minikube tunnel

# SSH into node
minikube ssh
```

---

## MicroK8s

Canonical's lightweight Kubernetes. Snap-based.

### Installation

```bash
# Install
sudo snap install microk8s --classic

# Add to group
sudo usermod -a -G microk8s $USER

# Check status
microk8s status

# Enable addons
microk8s enable dns dashboard ingress

# Use kubectl
microk8s kubectl get nodes
# Or alias
alias kubectl='microk8s kubectl'
```

### Features

```bash
# Enable common addons
microk8s enable dns
microk8s enable dashboard
microk8s enable ingress
microk8s enable storage
microk8s enable registry
microk8s enable prometheus

# HA cluster
microk8s add-node  # On first node
# Run output command on other nodes
```

---

## Docker Desktop Kubernetes

Built into Docker Desktop for Mac/Windows.

### Enable

1. Docker Desktop → Settings → Kubernetes
2. Enable Kubernetes
3. Apply & Restart

```bash
# Switch context
kubectl config use-context docker-desktop

# Verify
kubectl get nodes
```

---

## Comparison Matrix

| Feature | K3s | kind | minikube | MicroK8s |
|---------|:---:|:----:|:--------:|:--------:|
| Production ready | ✅ | ❌ | ❌ | ✅ |
| Multi-node | ✅ | ✅ | ❌ | ✅ |
| CI/CD friendly | ✅ | ✅ | ⚠️ | ⚠️ |
| ARM support | ✅ | ✅ | ✅ | ✅ |
| Ingress included | ✅ | ❌ | Addon | Addon |
| Dashboard | ❌ | ❌ | Addon | Addon |
| Resource usage | Low | Medium | Medium | Low |
| Setup speed | Fast | Fast | Medium | Fast |
| Cluster upgrade | ✅ | Recreate | ✅ | ✅ |

---

## Essential kubectl Commands

### Cluster Info

```bash
# Cluster info
kubectl cluster-info
kubectl get nodes
kubectl get namespaces

# Context management
kubectl config get-contexts
kubectl config use-context my-cluster
kubectl config current-context
```

### Workloads

```bash
# Pods
kubectl get pods
kubectl get pods -A                    # All namespaces
kubectl describe pod my-pod
kubectl logs my-pod
kubectl logs -f my-pod                 # Follow
kubectl exec -it my-pod -- /bin/sh

# Deployments
kubectl get deployments
kubectl create deployment nginx --image=nginx
kubectl scale deployment nginx --replicas=3
kubectl rollout status deployment nginx
kubectl rollout undo deployment nginx

# Services
kubectl get services
kubectl expose deployment nginx --port=80 --type=LoadBalancer
kubectl port-forward svc/nginx 8080:80
```

### Apply/Delete

```bash
# Apply manifests
kubectl apply -f deployment.yaml
kubectl apply -f ./manifests/
kubectl apply -k ./kustomize/

# Delete
kubectl delete -f deployment.yaml
kubectl delete pod my-pod
kubectl delete deployment nginx
```

### Debugging

```bash
# Events
kubectl get events --sort-by='.lastTimestamp'

# Resource usage
kubectl top nodes
kubectl top pods

# Describe for details
kubectl describe node my-node
kubectl describe pod my-pod

# Debug container
kubectl debug my-pod -it --image=busybox
```

---

## Kubernetes Manifests

### Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: myapp
  labels:
    app: myapp
spec:
  replicas: 3
  selector:
    matchLabels:
      app: myapp
  template:
    metadata:
      labels:
        app: myapp
    spec:
      containers:
        - name: myapp
          image: myapp:1.0.0
          ports:
            - containerPort: 8080
          resources:
            requests:
              memory: "128Mi"
              cpu: "100m"
            limits:
              memory: "256Mi"
              cpu: "500m"
          livenessProbe:
            httpGet:
              path: /health
              port: 8080
            initialDelaySeconds: 10
            periodSeconds: 10
          readinessProbe:
            httpGet:
              path: /ready
              port: 8080
            initialDelaySeconds: 5
            periodSeconds: 5
```

### Service

```yaml
apiVersion: v1
kind: Service
metadata:
  name: myapp
spec:
  selector:
    app: myapp
  ports:
    - port: 80
      targetPort: 8080
  type: ClusterIP  # or LoadBalancer, NodePort
```

### Ingress

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: myapp
  annotations:
    nginx.ingress.kubernetes.io/rewrite-target: /
spec:
  ingressClassName: nginx
  rules:
    - host: myapp.example.com
      http:
        paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: myapp
                port:
                  number: 80
  tls:
    - hosts:
        - myapp.example.com
      secretName: myapp-tls
```

### ConfigMap & Secret

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: myapp-config
data:
  DATABASE_HOST: "postgres"
  LOG_LEVEL: "info"
---
apiVersion: v1
kind: Secret
metadata:
  name: myapp-secrets
type: Opaque
stringData:
  DATABASE_PASSWORD: "supersecret"
```

---

## Helm

Package manager for Kubernetes.

```bash
# Install Helm
brew install helm

# Add repo
helm repo add bitnami https://charts.bitnami.com/bitnami
helm repo update

# Search charts
helm search repo nginx

# Install chart
helm install my-nginx bitnami/nginx
helm install my-nginx bitnami/nginx -f values.yaml
helm install my-nginx bitnami/nginx --set service.type=LoadBalancer

# List releases
helm list

# Upgrade
helm upgrade my-nginx bitnami/nginx --set replicaCount=3

# Uninstall
helm uninstall my-nginx

# Create chart
helm create mychart
```

---

## Decision Guide

| Use Case | Recommendation |
|----------|----------------|
| Learning Kubernetes | minikube or Docker Desktop |
| Local development | kind or minikube |
| CI/CD testing | kind |
| Edge/IoT production | K3s |
| Raspberry Pi cluster | K3s |
| Single-node production | K3s or MicroK8s |
| Multi-node production | K3s or full K8s |
| Quick testing | kind |
| GUI preference | Rancher Desktop |

---

## Related

- [[Container Runtimes]]
- [[Container Tools]]
- [[Deployment]]
