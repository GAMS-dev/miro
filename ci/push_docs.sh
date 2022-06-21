#!/bin/bash

set -e

MIRO_VERSION_FULL=$(grep -e '"version": .*' package.json |cut -f4 -d"\"")
pushd src > /dev/null
    MIRO_RELEASE_DATE=$(grep -m 1 -e 'MIRORDate' app.R | cut -f2 -d'"' | xargs -0 date +%Y-%m-%d -d )
popd > /dev/null
./scripts/fix_docs.sh

# update MIRO Server API OpenAPI schema
pushd server > /dev/null
    cp -R release_data/data_raw/ data
    mkdir models
    python3 miro_server.py dump_schema ../doc/miro_server_api.json
    python3 miro_server.py update_readmes "${MIRO_VERSION_FULL}"
    rm -rf data models
popd > /dev/null

git add ./doc/release.html ./doc/index.html ./doc/miro_server_api.json ./server/image-docs/*

git commit -m "[CI skip] Update documentation" || true

git remote rm origin && git remote add origin "https://${GITLAB_USERNAME}:${GITLAB_TOKEN}@${CI_SERVER_HOST}/${CI_PROJECT_PATH}.git"

git tag -fa "v${MIRO_VERSION_FULL}" -m "version ${MIRO_VERSION_FULL}"

git push origin "HEAD:$CI_COMMIT_REF_NAME"
git push origin "v${MIRO_VERSION_FULL}" || {
    git push origin ":refs/tags/v${MIRO_VERSION_FULL}"
    git push origin "v${MIRO_VERSION_FULL}"
}

rsync -rlptvz doc/* ubuntu@new.gams.com:/var/www/html/new.gams.com/public_html/miro
