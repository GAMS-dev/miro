#!/bin/bash
set -e

# Download and extract R.framework
# Requires xar and cpio
rm -rf r
mkdir -p r
if [[ $(uname -p) == 'arm' ]]; then
    curl -o r/r.pkg https://cloud.r-project.org/bin/macosx/big-sur-arm64/base/R-${R_BASE_VERSION}-arm64.pkg
else
    curl -o r/r.pkg https://cloud.r-project.org/bin/macosx/base/R-${R_BASE_VERSION}.pkg
fi

pushd r > /dev/null
    xar -xf r.pkg
    rm -r R-app.pkg Resources tcltk.pkg texinfo.pkg Distribution r.pkg
    cat R-fw.pkg/Payload | gunzip -dc | cpio -i
    rm -r R-fw.pkg

    mv R.framework/Versions/Current/Resources/* .

    # clean up directory a little
    rm -rf bin/R.bak SVN-REVISION doc tests lib/*.dSYM bin/Rscript Rscript Info.plist R.framework library/foreign/files/._sids.dbf

popd > /dev/null
