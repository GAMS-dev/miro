#!/bin/bash

set -e

MIRO_VERSION_FULL=$(grep -e '"version": .*' package.json |cut -f4 -d"\"")

## get MIRO version information for artifact and tag name
MIRO_VERSION_MAJOR=$(echo $MIRO_VERSION_FULL | cut -f1 -d".")
MIRO_VERSION_MINOR=$(echo $MIRO_VERSION_FULL | cut -f2 -d".")
MIRO_VERSION_PATCH=$(echo $MIRO_VERSION_FULL | cut -f3 -d".")
FOLDER_NAME="${MIRO_VERSION_MAJOR}.${MIRO_VERSION_MINOR}"

pushd dist > /dev/null
    ## move artifacts to correct location
    mkdir -p $FOLDER_NAME/linux
    mv *.AppImage $FOLDER_NAME/linux/GAMS-MIRO-${MIRO_VERSION_MAJOR}.${MIRO_VERSION_MINOR}.${MIRO_VERSION_PATCH}.AppImage
    mkdir -p $FOLDER_NAME/macosx
    mv *.dmg $FOLDER_NAME/macosx/GAMS-MIRO-${MIRO_VERSION_MAJOR}.${MIRO_VERSION_MINOR}.${MIRO_VERSION_PATCH}.dmg
    mkdir -p $FOLDER_NAME/windows
    mv *.exe $FOLDER_NAME/windows/GAMS-MIRO-Setup-${MIRO_VERSION_MAJOR}.${MIRO_VERSION_MINOR}.${MIRO_VERSION_PATCH}.exe
    ## S3 upload to gams.com
    s3cmd sync --acl-public ./ ${S3_URL} --access_key=${S3_ACCESS_KEY} --secret_key=${S3_SECRET_KEY}
    ## S3 content listing
    s3cmd ls -r ${S3_URL} --access_key=${S3_ACCESS_KEY} --secret_key=${S3_SECRET_KEY}
popd > /dev/null
