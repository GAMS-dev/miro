#!/usr/bin/env bash
set -euo pipefail

: "${HELM_REPO_S3_BUCKET:?HELM_REPO_S3_BUCKET is required (name only, no s3://)}"
: "${HELM_REPO_BASE_URL:?HELM_REPO_BASE_URL is required (e.g. https://charts.example.com)}"

WORKDIR="$(mktemp -d)"
trap 'rm -rf "$WORKDIR"' EXIT

echo ">>> Fetching existing index.yaml (if any) from s3://${HELM_REPO_S3_BUCKET}/index.yaml"
aws s3 cp "s3://${HELM_REPO_S3_BUCKET}/index.yaml" "${WORKDIR}/old-index.yaml" --only-show-errors || true

echo ">>> Rebuilding index.yaml with base URL: ${HELM_REPO_BASE_URL}"
# Find all *.tgz packages uploaded (at repo root or under /charts)
PKG_DIR="${CI_PROJECT_DIR:-.}"
PKGS=()
while IFS= read -r -d '' f; do PKGS+=("$f"); done < <(find "${PKG_DIR}" -maxdepth 2 -type f -name '*.tgz' -print0)

if [ "${#PKGS[@]}" -eq 0 ]; then
  echo "No packages found (*.tgz). Did 'helm package' run?"
  exit 1
fi

# Build into a fresh index.yaml
helm repo index "${PKG_DIR}" --url "${HELM_REPO_BASE_URL}"

# Merge if an old index exists
if [ -f "${WORKDIR}/old-index.yaml" ]; then
  echo ">>> Merging with existing index.yaml"
  helm repo index "${PKG_DIR}" --url "${HELM_REPO_BASE_URL}" --merge "${WORKDIR}/old-index.yaml"
fi

echo ">>> Uploading updated index.yaml"
aws s3 cp "${PKG_DIR}/index.yaml" "s3://${HELM_REPO_S3_BUCKET}/index.yaml" \
  --cache-control "no-store" \
  --content-type "application/x-yaml" \
  --only-show-errors

echo ">>> Done."
