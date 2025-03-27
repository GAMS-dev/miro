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

wait_for_pods_ready() {
    timeout="$1s"
    kubectl wait pod --all --for=condition=Ready --timeout=$timeout
}

if [[ -z "${K8S_VERSIONS_TO_TEST}" ]]; then
    echo "K8S_VERSIONS_TO_TEST environment variable not set. Please set it to list of Kubernetes versions to test (comma-separated)"
    exit 1
fi

IMAGE_TAG=$([[ "$CI_COMMIT_BRANCH" == "master" ]] && echo "latest" || { [[ "$CI_COMMIT_BRANCH" == "develop" || "$CI_COMMIT_BRANCH" == "rc" || "$CI_MERGE_REQUEST_SOURCE_BRANCH_NAME" == "rc" ]] && echo "unstable" || echo "feature"; })

pushd server > /dev/null
    cat > network-policy-default-deny-all.yaml <<EOF
---
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: default-deny-all
spec:
  podSelector: {}
  policyTypes:
  - Ingress
  - Egress
EOF
    cat <<EOF > audit-policy.yaml
apiVersion: audit.k8s.io/v1
kind: Policy
rules:
- level: Metadata
EOF

    python3 miro_server.py release -f --k8s
    unzip miro_server.zip

    export RUN_K8S_TESTS=true

    for K8_VERSION in ${K8S_VERSIONS_TO_TEST//,/ }
      do
          rm -fr mnt
          mkdir -m 0770 mnt
          kind create cluster --image kindest/node:v$K8_VERSION --name $CI_PIPELINE_ID --wait 180s --config ../ci/kind-config.yaml
          sed -i -e "s/0.0.0.0/docker/g" $HOME/.kube/config
          kubectl create secret docker-registry gitlab --from-file=.dockerconfigjson=$HOME/.docker/config.json
          # apply default-deny-all network policy
          if [ "${K8S_TEST_DISABLE_NP}" != "true" ]; then
            ENABLE_NP=true
            kubectl apply -f network-policy-default-deny-all.yaml
          else
            ENABLE_NP=false
            echo "Disabling network policies because K8S_TEST_DISABLE_NP is set to 'true'."
          fi
          # warn of violations of restricted pod security standard
          kubectl label --overwrite ns default pod-security.kubernetes.io/warn=restricted \
            pod-security.kubernetes.io/warn-version=latest
          API_SERVER_IP=$(kubectl get svc kubernetes -n default -o jsonpath='{.spec.clusterIP}')
          pushd miro_server > /dev/null
            pushd gams-miro-server > /dev/null
                helm dep up
                mkdir custom
              cp "${PWD}/../../tests/data/gams_logo.png" custom
            popd > /dev/null
            helm install test gams-miro-server/ \
                --set 'global.imagePullSecrets[0]=gitlab' \
                --set global.imageRegistry=$CI_REGISTRY_IMAGE \
                --set global.networkPolicy.enabled="$ENABLE_NP" \
                --set global.networkPolicy.apiServerIp="$API_SERVER_IP" \
                --set image.tag=$IMAGE_TAG \
                --set proxy.service.type=NodePort \
                --set proxy.service.nodePort=30080 \
                --set auth.service.type=NodePort \
                --set auth.service.nodePort=30081 \
                --set db.password=mySuperStrongPassword \
                --set persistence.local.path=/home/mnt \
                --set proxy.config.engine.apiUrl=${ENGINE_URL} \
                --set proxy.config.engine.namespace=${ENGINE_NS_2} \
                --set proxy.config.forceSignedApps.enabled=true \
                --set proxy.config.logo.enabled=true \
                --set proxy.config.logo.path="custom/gams_logo.png" \
                --set proxy.config.security.secureCookies=false \
                --set-file 'proxy.config.forceSignedApps.acceptedPublicKeysPEM[0]'="${PWD}/../tests/data/signing_key_pub.pem"
          popd > /dev/null
          trap 'cleanup' ERR
          wait_for_pods_ready 180
          wait_for_url "http://docker:30080" 60

          kubectl get pods

          pytest tests/

          PSP_VIOLATIONS=$(docker exec $CI_PIPELINE_ID-control-plane cat /var/log/kubernetes/kube-apiserver-audit.log | jq '
  select(.annotations["pod-security.kubernetes.io/audit-violations"] != null and
    (.annotations["pod-security.kubernetes.io/audit-violations"] | contains("uses restricted volume type \"hostPath\"") | not)
  ) |
  {
    timestamp: .stageTimestamp,
    user: .user.username,
    namespace: .objectRef.namespace,
    pod: .objectRef.name,
    violations: .annotations["pod-security.kubernetes.io/audit-violations"]
  }
')
          if [[ "$(echo $PSP_VIOLATIONS | grep -c '^{' || true)" -gt 0 ]]; then
            echo "Found pod security policy violations: $PSP_VIOLATIONS"
            exit 1
          fi

          kubectl patch deployment test-gams-miro-server-auth --type='strategic' -p \
                  "{\"spec\": {\"template\": {\"spec\": {\"containers\": [{\"name\": \"auth\", \"image\": \"$CI_REGISTRY_IMAGE/miro-auth-test:unstable\", \"livenessProbe\": null, \"env\": [{\"name\": \"ENGINE_USER\", \"value\": \"$ENGINE_USER\"}, {\"name\": \"ENGINE_PASSWORD\", \"value\": \"$ENGINE_PASSWORD\"}]}]}}}}"
          wait_for_pods_ready 60
          sleep 2
          POD_NAME=$(kubectl get pods -l app=test-gams-miro-server-auth --field-selector=status.phase=Running -o jsonpath='{.items[0].metadata.name}')
          kubectl exec $POD_NAME -- env COVERAGE_FILE=/tmp/.coverage pytest tests/ -o cache_dir=/tmp/.pytest_cache \
            --junitxml=/tmp/$PYTEST_JUNIT --cov=/app --cov-report=xml:/tmp/$PYTEST_COV

          kubectl cp $POD_NAME:/tmp/$PYTEST_COV $PYTEST_COV
          kubectl cp $POD_NAME:/tmp/$PYTEST_JUNIT $PYTEST_JUNIT

          python3 miro_server.py fix_coverage_paths -b 'server/auth/' $PYTEST_COV

          mv $PYTEST_JUNIT $PYTEST_COV ..

          cleanup
      done

popd > /dev/null
