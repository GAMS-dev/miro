#!/bin/bash

set -e

GAMSCOMDIR=update_website

[ -d "$GAMSCOMDIR" ] || {
  git clone "https://miro_ci:${GITLAB_WEBSITE_TOKEN}@git.gams.com/smann/new.gams.com.git" --branch master --single-branch "$GAMSCOMDIR"
}

MIRO_VERSION_FULL=$(grep -e '"version": .*' package.json |cut -f4 -d"\"")
pushd src > /dev/null
    MIRO_RELEASE_DATE=$(grep -m 1 -e 'MIRORDate' app.R | cut -f2 -d'"' | xargs -0 date +%Y-%m-%d -d )
popd > /dev/null

pushd "$GAMSCOMDIR" > /dev/null
  git pull
  sed -i -e 's/^\( *miro_release *= *\).*/\1"'"${MIRO_VERSION_FULL}"'"/' ./config.toml
  sed -i -e 's/^\( *miro_release_date *= *\).*/\1"'"${MIRO_RELEASE_DATE}"'"/' ./config.toml
  git add config.toml
  git commit -m "Update MIRO version" && {
      git push origin HEAD:master || true
  }
popd > /dev/null
