#!/bin/bash

case `uname` in
    Linux ) DATEBIN=date ;;
    Darwin ) DATEBIN=gdate ;;
    *) echo "Currently only Linux/macOS supported." && exit 1
esac

export $(grep -v '^#' .hashes.txt | xargs)

./scripts/create_miro_lib.sh > /dev/null
MIRO_VERSION_FULL=$(grep -m 1 -e "^MIROVersion" src/app.R|cut -f3 -d" "|xargs)
MIRO_VERSION_MAJOR=$(echo $MIRO_VERSION_FULL | cut -f1 -d".")
MIRO_VERSION_MINOR=$(echo $MIRO_VERSION_FULL | cut -f2 -d".")
MIRO_VERSION_SHORT="${MIRO_VERSION_MAJOR}.${MIRO_VERSION_MINOR}"
MIRO_RELEASE_DATE=$(grep -m 1 -e 'MIRORDate' src/app.R | cut -f2 -d'"' | xargs -0 ${DATEBIN} +%Y-%m-%d -d )

pushd server > /dev/null
    python3 miro_server.py release -f
popd > /dev/null
mv server/miro_server.zip doc/GAMS-MIRO-Server-${MIRO_VERSION_FULL}.zip

echo $MIRO_VERSION_FULL | sed 's/\./,/g'> ./doc/latest.ver
sed -e '/<code class="language-json config-schema-container">/r./src/conf/config_schema.json' ./doc/schema_template.html >./doc/schema.html
sed -i -e '/<div id="ci-vuln-report" class="section-block">/r./ci-vuln-report.html' ./doc/security_ci_report.html
sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g' ./src/LICENSE > ./src/LICENSE-escaped && {
    sed -e '/<pre id="miro-license">/r./src/LICENSE-escaped' ./doc/license_template.html >./doc/license.html && rm ./src/LICENSE-escaped
}
sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g; s/"/\&quot;/g; s/'"'"'/\&#39;/g' ./server/LICENSE > ./server/LICENSE-escaped && {
    sed -e '/<pre id="miro-license">/r./server/LICENSE-escaped' ./doc/license_template_server.html >./doc/license-server.html && rm ./server/LICENSE-escaped
}
sed -i -e "s/>Get GAMS MIRO .*<\/a/>Get GAMS MIRO $MIRO_VERSION_FULL<\/a/g" ./doc/index.html
sed -e 's/__VERSION__/'${MIRO_VERSION_SHORT}'/g' \
    -e 's/__VERSION_FULL__/'${MIRO_VERSION_FULL}'/g' \
    -e 's/__SHA_HASH_WIN__/'${MIRO_SHA_HASH_WIN}'/g' \
    -e 's/__SHA_HASH_MAC_ARM__/'${MIRO_SHA_HASH_MAC_ARM}'/g' \
    -e 's/__SHA_HASH_MAC_X86__/'${MIRO_SHA_HASH_MAC_X86}'/g' \
    -e 's/__SHA_HASH_LINUX__/'${MIRO_SHA_HASH_LINUX}'/g' ./doc/download_template.html >./doc/download.html
sed -i -e "s/(xxxx-xx-xx)/($MIRO_RELEASE_DATE)/" ./doc/release.html
