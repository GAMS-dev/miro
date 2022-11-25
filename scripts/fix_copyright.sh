#!/bin/bash

set -e
set -o pipefail

current_year=`date +"%Y"`
for f in $(find src -name '*.R' -or -name '*.r')
do
    for copyright_year in $(grep -Po 'Copyright \(c\) \K([0-9]+)(?= GAMS)' "$f")
    do
        if [ ! -z "$copyright_year" ]
        then
            echo "$f"
            if [[ "${current_year}" != "${copyright_year}" ]]
            then
                echo "Bad Copyright year: ${copyright_year} in $f"
                exit 1
            fi
        fi
    done
done
