#!/bin/sh
# Check that the bundle from build.sh runs on a clean distribution without R or compilers.
# Usage (from the repository root): ci/linux-bundled-r/smoke-test.sh <bundle.tar.gz>
set -eu

if command -v R >/dev/null || command -v cc >/dev/null; then
  echo "Image must not provide R or a C compiler" >&2
  exit 1
fi
DEST=$(mktemp -d)/relocated
mkdir -p "$DEST"
tar -xzf "$1" -C "$DEST"
"$DEST/r/bin/Rscript" --vanilla ci/linux-bundled-r/smoke-test.R
