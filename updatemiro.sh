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
    git fetch origin master
    git reset --hard FETCH_HEAD
    git clean -df
    MIRO_VERSION=$(grep -m 1 -e "^MIROVersion" app.R|cut -f3 -d" "|xargs) 
    echo $MIRO_VERSION | sed 's/\./,/g'> ./doc/latest.ver
    sed -e '/<code class="language-json">/r./conf/config_schema.json' ./doc/schema_template.html >./doc/schema.html
    sed -i "s/Download GAMS MIRO (v\. .*) for/Download GAMS MIRO (v\. $MIRO_VERSION) for/g" ./doc/index.html
fi
