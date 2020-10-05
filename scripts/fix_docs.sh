#!/bin/bash

./scripts/create_miro_lib.sh > /dev/null
MIRO_VERSION_FULL=$(grep -m 1 -e "^MIROVersion" app.R|cut -f3 -d" "|xargs)
MIRO_VERSION_MAJOR=$(echo $MIRO_VERSION_FULL | cut -f1 -d".")
MIRO_VERSION_MINOR=$(echo $MIRO_VERSION_FULL | cut -f2 -d".")
MIRO_VERSION_SHORT="${MIRO_VERSION_MAJOR}.${MIRO_VERSION_MINOR}"
MIRO_RELEASE_DATE=$(grep -m 1 -e '<h2 class="section-title" style="display:inline-block">GAMS MIRO' doc/release.html | cut -f2 -d"("|cut -f1 -d ")"|xargs)
echo $MIRO_VERSION_FULL | sed 's/\./,/g'> ./doc/latest.ver
sed -e '/<code class="language-json">/r./conf/config_schema.json' ./doc/schema_template.html >./doc/schema.html
sed -e '/<pre id="miro-license">/r./LICENSE' ./doc/license_template.html >./doc/license.html
sed -i -e "s/\.\/download\.html\">Get GAMS MIRO .*<\/a>/\.\/download\.html\">Get GAMS MIRO $MIRO_VERSION_FULL<\/a>/g" ./doc/index.html
sed -e 's/__VERSION__/'${MIRO_VERSION_SHORT}'/g' ./doc/download_template.html | sed -e 's/__VERSION_FULL__/'${MIRO_VERSION_FULL}'/g'>./doc/download.html
