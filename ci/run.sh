#!/usr/bin/env bash

export PATH="/home/miroci/ci/node_modules/.bin:$PATH"

function mchecklic () {
    pushd src > /dev/null
      Rscript scripts/checkLicense.R || {
        echo "Error checking license"
        popd > /dev/null
        return 1
      }
    popd > /dev/null
    return 0
}

function mtestall () {
    pushd src > /dev/null
      Rscript tests/testthat.R --stop || {
        echo "Error running tests"
        popd > /dev/null
        return 1
      }
    popd > /dev/null
    return 0
}
