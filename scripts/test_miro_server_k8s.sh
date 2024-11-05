#!/bin/bash

set -e
set -o pipefail

function cleanup {
    kind export logs --name $CI_PIPELINE_ID "../kind-logs/$K8_VERSION"
    kind delete cluster --name $CI_PIPELINE_ID
}

wait_for_url() {
    local url=$1
    local timeout=$2
    local start_time=$(date +%s)

    while true; do
        # Try to connect to the service
        if wget -q --spider "$url" >/dev/null 2>&1; then
            echo "Service at $url is up!"
            return 0
        fi

        # Check if the timeout has been reached
        local current_time=$(date +%s)
        local elapsed_time=$((current_time - start_time))
        if [ $elapsed_time -ge $timeout ]; then
            echo "Timeout of $timeout seconds reached. Service at $url is still not available."
            return 1
        fi

        # Wait for 1 second before retrying
        sleep 1
    done
}

if [[ -z "${K8S_VERSIONS_TO_TEST}" ]]; then
    echo "K8S_VERSIONS_TO_TEST environment variable not set. Please set it to list of Kubernetes versions to test (comma-separated)"
    exit 1
fi

pushd server > /dev/null

    python3 miro_server.py release -f --k8s
    unzip miro_server.zip

    for K8_VERSION in ${K8S_VERSIONS_TO_TEST//,/ }
      do
          kind create cluster --image kindest/node:v$K8_VERSION --name $CI_PIPELINE_ID --wait 180s --config .ci/kind-config.yaml
          sed -i -e "s/0.0.0.0/docker/g" $HOME/.kube/config
          kubectl create secret docker-registry gitlab --from-file=.dockerconfigjson=$HOME/.docker/config.json
          pushd gams-miro-server > /dev/null
            helm dep up
          popd > /dev/null
          pushd miro_server > /dev/null
            helm install engine gams-miro/ \
                --set global.imagePullSecrets[0]=gitlab \
                --set global.imageRegistry=$CI_REGISTRY_IMAGE \
                --set image.tag=unstable \
                --set service.nodePort=30080 \
                --set persistence.local.path=${PWD}/mnt \
                --set proxy.config.engine.apiUrl=${ENGINE_URL} \
                --set proxy.config.engine.namespace=${ENGINE_NS} \
          popd > /dev/null
          trap 'cleanup' ERR
          wait_for_url "http://docker:30080" 180

          kubectl get pods

          pytest tests/

          cleanup
      done

popd > /dev/null
