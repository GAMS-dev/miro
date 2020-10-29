#!/usr/bin/env/bash

function fixodocs () {
    MIRO_VERSION_FULL=$(grep -m 1 -e "^MIROVersion" app.R|cut -f3 -d" "|xargs)
    ./scripts/fix_docs.sh || {
       echo Problems running ./scripts/fix_docs.sh
    }
    git add 
}
