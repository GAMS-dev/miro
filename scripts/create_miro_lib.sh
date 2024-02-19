#!/bin/bash

ZIP_TOOL=zip
if ! command -v zip &> /dev/null
then
    if [[ ! -z "${GAMS_SYS_DIR}" ]]; then
       GAMS_SYS_DIR_TMP=${GAMS_SYS_DIR}/
    fi
    ZIP_TOOL=${GAMS_SYS_DIR_TMP}gmszip
fi

echo "ZIP tool: $ZIP_TOOL"

pushd src/model > /dev/null
    cd transport
    $ZIP_TOOL -r transport.conf.unzip transport_files.txt conf_transport README.md -x "*.DS_Store"
    cd ..
    cd pickstock
    $ZIP_TOOL -r pickstock.conf.unzip pickstock_files.txt dowjones2016.csv conf_pickstock scripts_pickstock/hcube_analysis.ipynb static_pickstock README.md -x "*.DS_Store"
    cd ..
    cd sudoku
    $ZIP_TOOL -r sudoku.conf.unzip sudoku_files.txt conf_sudoku renderer_sudoku static_sudoku -x "*.DS_Store"
    cd ..
    cd inscribedsquare
    $ZIP_TOOL -r inscribedsquare.conf.unzip inscribedsquare_files.txt conf_inscribedsquare renderer_inscribedsquare data_inscribedsquare README.md -x "*.DS_Store"
    cd ..
    cd tsp
    $ZIP_TOOL -r tsp.conf.unzip tsp_files.txt conf_tsp data_tsp static_tsp renderer_tsp README.md -x "*.DS_Store"
    cd ..
    cd farming
    $ZIP_TOOL -r farming.conf.unzip farming_files.txt conf_farming data_farming static_farming renderer_farming README.md -x "*.DS_Store"
    cd ..
    cd cpack
    $ZIP_TOOL -r cpack.conf.unzip cpack_files.txt conf_cpack data_cpack static_cpack README.md -x "*.DS_Store"
    cd ..
    cd cutstock
    $ZIP_TOOL -r cutstock.conf.unzip cutstock_files.txt conf_cutstock data_cutstock renderer_cutstock static_cutstock README.md -x "*.DS_Store"
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
    mv farming/farming.conf.unzip miro_lib
    cp farming/farming.gms miro_lib
    mv cpack/cpack.conf.unzip miro_lib
    cp cpack/cpack.gms miro_lib
    mv cutstock/cutstock.conf.unzip miro_lib
    cp cutstock/cutstock.gms miro_lib
    $ZIP_TOOL -r GAMS-MIRO-Demo-Apps.zip miro_lib -x "*.DS_Store"
popd > /dev/null

pushd src/model/gamspy > /dev/null
    cd transport
    $ZIP_TOOL -r transport.conf.unzip transport_files.txt conf_transport README.md -x "*.DS_Store"
    cd ..
    cd pickstock
    $ZIP_TOOL -r pickstock.conf.unzip pickstock_files.txt dowjones2016.csv conf_pickstock scripts_pickstock/hcube_analysis.ipynb static_pickstock README.md -x "*.DS_Store"
    cd ..
    cd cpack
    $ZIP_TOOL -r cpack.conf.unzip cpack_files.txt conf_cpack data_cpack static_cpack README.md -x "*.DS_Store"
    cd ..
    cd sudoku
    $ZIP_TOOL -r sudoku.conf.unzip sudoku_files.txt conf_sudoku data_sudoku static_sudoku -x "*.DS_Store"
    cd ..
    rm -rf miro_lib_gamspy || true
    mkdir miro_lib_gamspy
    mv transport/transport.conf.unzip miro_lib_gamspy
    cp transport/transport.py miro_lib_gamspy
    mv pickstock/pickstock.conf.unzip miro_lib_gamspy
    cp pickstock/pickstock.py miro_lib_gamspy
    mv cpack/cpack.conf.unzip miro_lib_gamspy
    cp cpack/cpack.py miro_lib_gamspy
    mv sudoku/sudoku.conf.unzip miro_lib_gamspy
    cp sudoku/sudoku.py miro_lib_gamspy
    $ZIP_TOOL -r GAMS-MIRO-Demo-Apps-GAMSPy.zip miro_lib_gamspy -x "*.DS_Store"
popd > /dev/null

pushd doc > /dev/null
    mv ../src/model/GAMS-MIRO-Demo-Apps.zip ../src/model/gamspy/GAMS-MIRO-Demo-Apps-GAMSPy.zip .
    rm -rf ../src/model/miro_lib
    rm -rf ../src/model/gamspy/miro_lib_gamspy
popd > /dev/null
