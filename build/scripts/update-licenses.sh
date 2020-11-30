#!/bin/bash

LICENSE_FILE=LICENSE
npm list -g npm-license-crawler > /dev/null || {
    npm install -g npm-license-crawler
}
MORE_DIR_EXCL=""
if [ -d "dist" ]; then
    # on jenkins r and dist don't exist yet when this script is run
    MORE_DIR_EXCL="--exclude dist --exclude r"
fi
npm-license-crawler ${MORE_DIR_EXCL}--exclude build --exclude src --exclude admin --exclude r-src --dependencies --production --csv licenses.csv > /dev/null
cat >$LICENSE_FILE <<EOL
GAMS MIRO as a whole is distributed under GPL-3 (GNU GENERAL PUBLIC LICENSE version 3). A copy of this license is included below.

The entire source code is available at: https://github.com/GAMS-dev/miro

GAMS MIRO includes other open source software components. The following
is a list of the components used for the MIRO Desktop Electron app to launch
GAMS MIRO applications.
Third-party software used in the R/Shiny part of GAMS MIRO can be found
in a sepeare LICENSE filed located in src/LICENSE or online at: https://gams.com/miro/license.html


EOL
cat licenses.csv>>$LICENSE_FILE
cat >>$LICENSE_FILE <<EOL


EOL
cat scripts/GPL-3.txt>>$LICENSE_FILE

cat >allowed-licenses <<EOL
(BSD-2-Clause OR MIT OR Apache-2.0)
(CC-BY-4.0 AND OFL-1.1 AND MIT)
(MIT OR CC0-1.0)
(MIT OR WTFPL)
Apache-2.0
BSD-2-Clause
BSD-3-Clause
CC-BY-3.0
CC0-1.0
GPL-3.0-only
ISC
MIT
WTFPL
WTFPL OR ISC
EOL

awk -F "\"*,\"*" '{print $2}' licenses.csv | tail -n +2 | sort |uniq | while read x; do grep "$x" allowed-licenses || { >&2 echo "$x";exit 1; }; done > /dev/null || {
    \rm -f licenses.csv allowed-licenses
    exit 1
}
\rm -f licenses.csv allowed-licenses
exit 0
