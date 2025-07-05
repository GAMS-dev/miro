#!/usr/bin/env bash

function mchecklic () {
    Rscript scripts/checkLicense.R || {
      echo "Error checking license"
      return 1
    }
    return 0
}

function mtestall () {
    pushd src > /dev/null
      /usr/bin/time -v -o ../profile.txt Rscript tests/testthat.R --stop || {
        echo "Error running tests"
        popd > /dev/null
        return 1
      }
    popd > /dev/null
    return 0
}
