#!/bin/bash

set -e
set -o pipefail

pushd src/model/gamspy > /dev/null

    for dir in */
    do
        dir=${dir%*/}
        if [[ "$dir" == 'pickstock' ]]; then
            continue
        fi
        cd "$dir"
            mv "conf_${dir}/${dir}_io.json" "conf_${dir}/${dir}_io_test.json"
            MIRO=1 python "$dir.py"
            diff -w "conf_${dir}/${dir}_io.json" "conf_${dir}/${dir}_io_test.json"
        cd ..
    done

popd > /dev/null
