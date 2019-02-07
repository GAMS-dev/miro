#!/bin/sh

cd $HOME/miro
git remote update

UPSTREAM=${1:-'@{u}'}
LOCAL=$(git rev-parse @)
REMOTE=$(git rev-parse "$UPSTREAM")
BASE=$(git merge-base @ "$UPSTREAM")

if [ $LOCAL = $REMOTE ]; then
    echo "Up-to-date: $BASE == $LOCAL"
elif [ $LOCAL = $BASE ]; then
    git pull
    grep -m 1 -e "^MIROVersion" app.R|cut -f3 -d" "|xargs|sed 's/\./,/g' > ./doc/latest.ver
    sed -e '/<code class="language-json">/r./conf/config_schema.json' ./doc/schema_template.html >./doc/schema.html
fi
