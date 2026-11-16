#!/bin/bash
# Uploads the installers and update metadata for the auto-updater to updates/<channel>/<platform>-<arch>/,
# replacing the previous build. Platforms that were not built in this pipeline are kept.
# Usage: ci/s3_upload_updates.sh latest|dev

set -e
shopt -s nullglob

CHANNEL=$1

upload() {
    local target="${S3_URL%/}/updates/${CHANNEL}/$1" metadata=$2
    shift 2
    if [ ! -f "$metadata" ]; then
        echo "No update for $target"
        return
    fi
    local dir
    dir=$(mktemp -d)
    cp "$@" "$dir"
    # installers first, so the metadata never points to missing files;
    # old blockmaps are kept for differential downloads
    aws s3 sync "$dir" "$target" --delete --exclude '*.yml' --exclude '*.blockmap' --acl public-read
    aws s3 sync "$dir" "$target" --exclude '*' --include '*.blockmap' --acl public-read
    aws s3 cp "$metadata" "$target/" --acl public-read --cache-control no-cache
}

upload win32-x64 "dist/${CHANNEL}.yml" dist/*.exe dist/*.exe.blockmap
upload darwin-x64 "dist/${CHANNEL}-mac.yml" dist/*.zip dist/*.zip.blockmap
upload darwin-arm64 "dist/arm/${CHANNEL}-mac.yml" dist/arm/*.zip dist/arm/*.zip.blockmap
upload linux-x64 "dist/${CHANNEL}-linux.yml" dist/*.AppImage
