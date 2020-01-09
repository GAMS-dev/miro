#!/bin/sh

cd model
cd transport
zip transport.conf.unzip conf_transport/transport.json -x "*.DS_Store"
cd ..
cd pickstock
zip -r pickstock.conf.unzip conf_pickstock/pickstock.json scripts_pickstock/hcube_analysis.ipynb static_pickstock -x "*.DS_Store"
cd ..
rm -rf miro_lib || true
mkdir miro_lib
cp mirolib.glb miro_lib
mv transport/transport.conf.unzip miro_lib
cp transport/transport.gms miro_lib
mv pickstock/pickstock.conf.unzip miro_lib
cp pickstock/pickstock.gms pickstock/dowjones2016.csv miro_lib
zip -r miroDemoApps.zip miro_lib -x "*.DS_Store"
cd ../doc
mv ../model/miroDemoApps.zip .
rm -rf ../model/miro_lib
cd ..
