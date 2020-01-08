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
cp transport/transport.conf.unzip transport/transport.gms miro_lib
cp pickstock/pickstock.gms pickstock/dowjones2016.csv pickstock/pickstock.conf.unzip miro_lib
cd ../doc
zip -r miroDemoApps.zip ../model/miro_lib -x "*.DS_Store"
rm -rf ../model/miro_lib
cd ..
