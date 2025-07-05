#!/bin/bash

set -e
set -o pipefail

function cleanup {
    pushd miro_server > /dev/null
        docker compose logs 2>&1 > ../../miro_server_all_logs.txt
        docker compose down -v
    popd > /dev/null
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

pushd server > /dev/null

    if [[ "$CI_COMMIT_BRANCH" == "master" ]]; then
        python3 miro_server.py download ${CI_REGISTRY_IMAGE} --tag-as-released --image-tag=latest
    elif [[ "$CI_COMMIT_BRANCH" == "develop" ]] || [[ "$CI_COMMIT_BRANCH" == "rc" ]] || [[ "$CI_MERGE_REQUEST_SOURCE_BRANCH_NAME" == "rc" ]]; then
        python3 miro_server.py download ${CI_REGISTRY_IMAGE} --tag-as-released --image-tag=unstable
    else
        python3 miro_server.py download ${CI_REGISTRY_IMAGE} --tag-as-released --image-tag=feature
    fi

    python3 miro_server.py release -f
    unzip miro_server.zip

    pushd miro_server > /dev/null

        echo "GMS_MIRO_DATABASE_PWD=clAuY25Gy3W0L07KMFyN3lDepBjMMKuo0onEtMeU" > .env # pragma: allowlist secret
        echo "GMS_MIRO_ENGINE_HOST=${ENGINE_URL}" >> .env
        echo "GMS_MIRO_ENGINE_NS=${ENGINE_NS}" >> .env
        mkdir logs
        chown -R 1000:1000 logs
        mkdir data
        cp -a data_raw/. data/
        chown -R 1000:1000 data
        mkdir models
        chown -R 1000:1000 models

        ./miro-server start

    popd > /dev/null

    trap 'cleanup' ERR

    wait_for_url "http://docker:8080" 60

    pytest tests/

    cleanup

popd > /dev/null
