#!/bin/bash
#
# create an sfx installation file from a freshly installed GAMS version
#
GITDIR=/home/gams/WC/miro
GHOMEDIR=/home/gams
INSTALLER=/home/gams/linux_x64_64_sfx.exe

# No changes beyond this point
MIRONAME=gams_miro0.4_linux_x64_64_sfx
[ -d $GITDIR ] || {
    echo "Directory $GITDIR does not exist"
    return 1
}

# install of GAMS
GAMSDIR=$GHOMEDIR/$MIRONAME
[ -d $GAMSDIR ] || {
    cd $GHOMEDIR && $INSTALLER && mv gams26.1_linux_x64_64_sfx $MIRONAME
}

mkdir -p $GAMSDIR/GMSR/library

# Extra Python modules
[ -d $GAMSDIR/GMSPython/lib/python3.6/site-packages/geopy ] || {
    cd $GAMSDIR/GMSPythoni/bin
    ./pip3 install matplotlib pandas numpy xlsxwriter pandas_datareader geocoder geopy
}

# miro libinclude
\cp -f $GITDIR/model/miro.gms $GAMSDIR/inclib

cd $GAMSDIR
[ -d miro ] || {
    mkdir miro
}
rm -rf miro\*
cd miro
\cp -fr $GITDIR/JS .
\cp -fr $GITDIR/R .
\cp -fr $GITDIR/UI .
\cp -fr $GITDIR/conf .
\cp -fr $GITDIR/modules .
\cp -fr $GITDIR/resources .
\cp -fr $GITDIR/tools .
\cp -fr $GITDIR/www .
\cp     $GITDIR/LICENSE .
\cp     $GITDIR/app.R .
\cp     $GITDIR/global.R .

# APILIB Example models
cd $GITDIR/model/kport
$GAMSDIR/gmszip -r kport_conf.zip conf/kport.json static customRenderer
\cp -f kport_conf.zip kport.gms $GAMSDIR/datalib_ml
\rm -f kport_conf.zip

cd $GITDIR/model/pickstock
$GAMSDIR/gmszip -r pickstock_conf.zip conf/pickstock.json static customRenderer
\cp -f pickstock_conf.zip pickstock.gms dowjones2016.csv $GAMSDIR/datalib_ml
\rm -f pickstock_conf.zip

cd $GITDIR/model/pickstock_live
$GAMSDIR/gmszip -r pickstock_live_conf.zip conf/pickstock_live.json static customRenderer
\cp -f pickstock_live_conf.zip pickstock_live.gms $GAMSDIR/datalib_ml
\rm -f pickstock_live_conf.zip

cd $GITDIR/model/transport
$GAMSDIR/gmszip -r transport_conf.zip conf/transport.json static customRenderer
\cp -f transport_conf.zip transport.gms $GAMSDIR/datalib_ml
\rm -f transport_conf.zip

cd $GITDIR/model/transport_live
$GAMSDIR/gmszip -r transport_live_conf.zip conf/transport_live.json static customRenderer
\cp -f transport_live_conf.zip transport_live.gms $GAMSDIR/datalib_ml
\rm -f transport_live_conf.zip

cd $GITDIR
\cp -f mkconfig.inc datalib.glb $GAMSDIR/datalib_ml

cd $GAMSDIR/datalib_ml
echo \$include mkconfig.inc >> kport.gms
echo \$include mkconfig.inc >> pickstock.gms
echo \$include mkconfig.inc >> pickstock_live.gms
echo \$include mkconfig.inc >> transport.gms
echo \$include mkconfig.inc >> transport_live.gms

# Newer cmex
cd $GAMSDIR
scp distrib@anton.gams.com:michael/porting/products/src/gamscmex/optgams.def .
scp distrib@anton.gams.com:michael/porting/products/src/gamscmex/gamserrs.txt .
scp distrib@anton.gams.com:michael/porting/btree/gamscmex/leg/gamscmex.out .

cd $GHOMEDIR
#$GAMSDIR/gmszip -r x.zip $MIRONAME
#cat ~/porting/products/platform/libleg/unzipsfx x.zip > miro0.4_linux_x64_64.zip
