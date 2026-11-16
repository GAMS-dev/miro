#!/bin/bash
# Uploads the installers and update metadata for the auto-updater to updates/<channel>/<platform>-<arch>/,
# replacing the previous build. On the dev channel, platforms that were not built in this pipeline are kept
# and builds of newer pipelines are not replaced.
# Usage: ci/s3_upload_updates.sh latest|dev

set -eo pipefail
shopt -s nullglob

CHANNEL=$1
# blockmaps of earlier versions allow differential downloads from these versions
KEEP_BLOCKMAPS=5

upload() {
    local target="${S3_URL%/}/updates/${CHANNEL}/$1" metadata=$2 published dir
    shift 2
    if [ ! -f "$metadata" ]; then
        echo "No update for $target"
        return
    fi
    if [ "$CHANNEL" = dev ]; then
        # pipeline of the published build, from its file names (e.g. GAMS-MIRO-2.14.0-dev.4600.develop.AppImage)
        published=$({ aws s3 ls "$target/" || true; } | sed -n 's/.*-dev\.\([0-9]*\)\..*/\1/p' | sort -n | tail -n 1)
        if [ -n "$published" ] && [ "$published" -gt "$CI_PIPELINE_IID" ]; then
            echo "Keeping the build of the newer pipeline $published in $target"
            return
        fi
    fi
    dir=$(mktemp -d)
    cp "$@" "$dir"
    # new files, then the metadata pointing to them, then remove the previous installers
    aws s3 sync "$dir" "$target" --exclude '*.yml' --acl public-read
    aws s3 cp "$metadata" "$target/" --acl public-read --cache-control no-cache
    aws s3 sync "$dir" "$target" --delete --exclude '*.yml' --exclude '*.blockmap' --acl public-read
    aws s3 ls "$target/" | { grep '\.blockmap$' || true; } | sort | head -n -"$KEEP_BLOCKMAPS" | awk '{print $4}' |
        while read -r file; do aws s3 rm "$target/$file"; done
}

if [ "$CHANNEL" = latest ]; then
    # a release must update all platforms
    for metadata in "dist/latest.yml" "dist/latest-mac.yml" "dist/arm/latest-mac.yml" "dist/latest-linux.yml"; do
        [ -f "$metadata" ] || { echo "Missing $metadata" >&2; exit 1; }
    done
fi

upload win32-x64 "dist/${CHANNEL}.yml" dist/*.exe dist/*.exe.blockmap
upload darwin-x64 "dist/${CHANNEL}-mac.yml" dist/*.zip dist/*.zip.blockmap
upload darwin-arm64 "dist/arm/${CHANNEL}-mac.yml" dist/arm/*.zip dist/arm/*.zip.blockmap
upload linux-x64 "dist/${CHANNEL}-linux.yml" dist/*.AppImage
