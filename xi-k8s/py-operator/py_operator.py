# I just want to create a function that talks to the API and create a nginx deployment.

from kubernetes import client, config

DEPLOYMENT_NAME = "nginx-deployment"


def create_deployment_object():
    # Configureate Pod template container
    container = client.V1Container(
        name="nginx",
        image="nginx:1.15.4",
        ports=[client.V1ContainerPort(container_port=80)],
        resources=client.V1ResourceRequirements(
            requests={"cpu": "100m", "memory": "200Mi"},
            limits={"cpu": "500m", "memory": "500Mi"},
        ),
    )

    # Create and configurate a spec section
    template = client.V1PodTemplateSpec(
        metadata=client.V1ObjectMeta(labels={"app": "nginx"}),
        spec=client.V1PodSpec(containers=[container]),
    )

    # Create the specification of deployment
    spec = client.V1DeploymentSpec(
        replicas=3, template=template, selector={"matchLabels": {"app": "nginx"}}
    )

    # Instantiate the deployment object
    deployment = client.V1Deployment(
        api_version="apps/v1",
        kind="Deployment",
        metadata=client.V1ObjectMeta(name=DEPLOYMENT_NAME),
        spec=spec,
    )

    return deployment


def create_deployment(api, deployment):
    # Create deployement
    resp = api.create_namespaced_deployment(body=deployment, namespace="default")

    print("\n[INFO] deployment `nginx-deployment` created.\n")
    print("%s\t%s\t\t\t%s\t%s" % ("NAMESPACE", "NAME", "REVISION", "IMAGE"))
    print(
        "%s\t\t%s\t%s\t\t%s\n"
        % (
            resp.metadata.namespace,
            resp.metadata.name,
            resp.metadata.generation,
            resp.spec.template.spec.containers[0].image,
        )
    )


def main():

    config.load_kube_config()
    # do this when running the code inside the cluster
    # config.load_incluster_config()
    apps_v1 = client.AppsV1Api()
    deployment = create_deployment_object()
    create_deployment(apps_v1, deployment)


if __name__ == "__main__":
    main()
