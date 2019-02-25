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
    cd model
    zip miroDemoApps.zip pickstock/pickstock.gms pickstock/dowjones2016.csv pickstock/conf/pickstock.json pickstock/static/pickstock.png kport/kport.gms kport/conf/kport.json kport/static/kport.jpg pickstock_live/pickstock_live.gms pickstock_live/conf/pickstock_live.json pickstock_live/static/pickstock-live.png transport/transport.gms transport/conf/transport.json transport/customRenderer/* transport_live/transport_live.gms transport_live/conf/transport_live.json transport_live/customRenderer/*
    mv miroDemoApps.zip ../doc
    cd ..
    MIRO_VERSION=$(grep -m 1 -e "^MIROVersion" app.R|cut -f3 -d" "|xargs) 
    echo $MIRO_VERSION | sed 's/\./,/g'> ./doc/latest.ver
    sed -e '/<code class="language-json">/r./conf/config_schema.json' ./doc/schema_template.html >./doc/schema.html
    sed -i "s/Download GAMS MIRO (v\. .*) for/Download GAMS MIRO (v\. $MIRO_VERSION) for/g" ./doc/index.html
fi
