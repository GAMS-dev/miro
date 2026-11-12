#!/bin/bash

set -e
set -o pipefail

if [ "$CI_MERGE_REQUEST_TARGET_BRANCH_NAME" = "master" ]; then
    VERSION=$(node -p "require('./package.json').version")
    SHORT_VERSION=$(echo "$VERSION" | cut -d. -f1-2)
    FILE="src/whats-new/${SHORT_VERSION}.json"
    echo "Checking for $FILE ..."
    if [ ! -f "$FILE" ]; then
        echo "❌ Missing changelog file: $FILE"
        exit 1
    else
        echo "✅ Found $FILE"
    fi
    echo "🔍 Checking for future changelog files ..."
    for f in src/whats-new/*.json; do
        [ -e "$f" ] || continue
        BASE=$(basename "$f" .json)
        if [ "$(awk -v a="$BASE" -v b="$SHORT_VERSION" 'BEGIN {if (a > b) print 1}')" = "1" ]; then
        echo "❌ Found future changelog file: $f (current version is $SHORT_VERSION)"
        exit 1
        fi
    done
    echo "✅ No future changelog files found."
fi
