#!/bin/bash
# Runs build.sh unless a pipeline with the same inputs (key written by build_linux_job) already built the R bundle.
# Bundles are kept in the runner cache; releases (master) always build from scratch.
set -euo pipefail

FILE="miro-r-linux-$(uname -m).tar.gz"
CACHE_DIR=.r-bundle-cache
CACHED="$CACHE_DIR/$(cat build/linux/r-bundle.key)-$FILE"
mkdir -p "$OUT" "$CACHE_DIR"

if [ "${CI_COMMIT_BRANCH:-}" = master ]; then
  bash ci/linux-bundled-r/build.sh
  exit 0
fi
if [ -f "$CACHED" ]; then
  echo "Reusing cached R bundle $CACHED"
  cp "$CACHED" "$OUT/$FILE"
  exit 0
fi
bash ci/linux-bundled-r/build.sh
cp "$OUT/$FILE" "$CACHED"
# keep the 3 newest bundles
ls -t "$CACHE_DIR"/* | tail -n +4 | xargs -r rm -f
