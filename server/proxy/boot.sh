#!/bin/sh
cp data/application.yml .
[[ -e data/logo.png ]] && cp data/logo.png templates/2col/assets/img/logo.png
exec java -jar shinyproxy.jar
