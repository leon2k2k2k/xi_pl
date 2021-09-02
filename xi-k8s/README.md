# xi-k8s

This crate is the k8s "library" in Aplite. It is a library in the frontend, that is, there are data structures and 
functions to call. However, it is really communicating to a k8s-operator that runs in the k8s cluster.

## Frontend
At the frontend, we can use the functions and data structures of this library, through the remote syntax:

```
remote "<remote_address>" {
    k8s: Type -> Type,
    create_deployment: k8s UnitType,
    ...
}
```

## Backend
On the backend, the library is really a (currently primitive) kubernetes operator, running in the cluster as a pod. When we remote import function in the frontend, what really happens is that there is a local server communicating with the operator running on the pod through Aplite's standard HTTP protocals. This gives the flexibility for Aplite to interact with different software. Currently the operator is written in Python, using the k8s-python client library.

## Setup
Here's are the prerequisites:
- A kubernetes cluster with RBAC enabled 
- kubectl configured to the cluster.
In the following, we will use a cluster running on minikube.
we have to do two things: 1. Deploy the operator code in a pod and enable auth. 2. Expose the pod.

### 1. Build Docker Image and Deploy to Pod.
We first package the code as a Docker image. First `cd` into the director.
```
cd ./py-operator
```
For minikube cluster, we have to uses minikube's Docker:
```
eval $(minikube-docker-env)$
docker build -t py-oprator . 
```
Now we deploy the pods as well as the RBAC role, rolebinding, and serviceaccount:
``` 
cd ../k8s-yamls
kubectl apply -f py-operator.yaml
kubectl apply -f py-operator-role.yaml
```
The roles  are necessary because by default Kubernetes doesn't allow its pod to unlimitedly communicate with the k8s API server. 

### 2. Expose the pod
In minikube running on Docker, then only way I can find to expose the pod is using kubectl port-forward:
```
kubectl port-forward <py-opearator-pod-name> 8080:8080
```
This forwards `localhost:8080` to port `8080` on the pod, where the server is listening at.

## Using it
Now we can remotely "import" functions on the k8s-library by writting remote statments:
``` 
remote "localhost:8080" {
    create_deployment: k8s UnitType
}
let main = {
    do create_deployment!
    val ()!
}
```

