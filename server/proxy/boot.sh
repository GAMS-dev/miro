#!/bin/sh

if [ -n "$GMS_MIRO_TARGET_IMG_DIR" ]; then
    mkdir -p "$GMS_MIRO_TARGET_IMG_DIR"
    for file in "/home/miroproxy/templates/2col/assets/img_default"/*; do
        filename=$(basename "$file")
        if [ ! -e "$GMS_MIRO_TARGET_IMG_DIR/$filename" ]; then
            echo "Copying $filename to $GMS_MIRO_TARGET_IMG_DIR"
            cp "$file" "$GMS_MIRO_TARGET_IMG_DIR/"
        fi
    done
fi

[[ -e data/application.yml ]] && cp data/application.yml .
[[ -e data/logo.png ]] && cp data/logo.png templates/2col/assets/img/logo.png
[ -d mnt ] && ln -s mnt/data data
exec java -jar shinyproxy.jar
