#!/usr/bin/env bash

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
      Rscript tests/testthat.R || {
        echo "Error running tests"
        popd > /dev/null
        return 1
      }
    popd > /dev/null
    return 0
}
