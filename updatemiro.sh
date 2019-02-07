#!/bin/bash

cd $HOME/miro

UPSTREAM=${1:-'@{u}'}
LOCAL=$(git rev-parse @)
BASE=$(git merge-base @ "$UPSTREAM")
if [ "$BASE" != "$LOCAL" ]; then
    git pull
    grep -m 1 -e "^MIROVersion" app.R|cut -f3 -d" "|xargs|sed 's/\./,/g' > ./doc/latest.ver
    sed -e '/<code class="language-json">/r./conf/config_schema.json' ./doc/schema_template.html >./doc/schema.html
else
    echo Up to date: $BASE == $LOCAL
fi
