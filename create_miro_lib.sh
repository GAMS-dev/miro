#!/bin/sh

cd model
cd transport
zip transport.conf.unzip transport_files.txt conf_transport/transport.json -x "*.DS_Store"
cd ..
cd pickstock
zip -r pickstock.conf.unzip pickstock_files.txt dowjones2016.csv conf_pickstock/pickstock.json scripts_pickstock/hcube_analysis.ipynb static_pickstock -x "*.DS_Store"
cd ..
cd sudoku
zip -r sudoku.conf.unzip sudoku_files.txt conf_sudoku/sudoku.json renderer_sudoku/sudoku.R static_sudoku -x "*.DS_Store"
cd ..
cd inscribedsquare
zip -r inscribedsquare.conf.unzip inscribedsquare_files.txt conf_inscribedsquare/inscribedsquare.json renderer_inscribedsquare/inscribedsquare_custom.R data_inscribedsquare -x "*.DS_Store"
cd ..
cd tsp
zip -r tsp.conf.unzip tsp_files.txt conf_tsp/tsp.json data_tsp static_tsp -x "*.DS_Store"
cd ..
rm -rf miro_lib || true
mkdir miro_lib
cp mirolib.glb miro_lib
mv transport/transport.conf.unzip miro_lib
cp transport/transport.gms miro_lib
mv pickstock/pickstock.conf.unzip miro_lib
cp pickstock/pickstock.gms miro_lib
mv sudoku/sudoku.conf.unzip miro_lib
cp sudoku/sudoku.gms miro_lib
mv inscribedsquare/inscribedsquare.conf.unzip miro_lib
cp inscribedsquare/inscribedsquare.gms miro_lib
mv tsp/tsp.conf.unzip miro_lib
cp tsp/tsp.gms miro_lib
zip -r miroDemoApps.zip miro_lib -x "*.DS_Store"
cd ../doc
mv ../model/miroDemoApps.zip .
rm -rf ../model/miro_lib
cd ..
