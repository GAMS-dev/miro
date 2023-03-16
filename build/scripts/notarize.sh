#!/bin/bash

set -e

xcrun notarytool submit "$1" \
    --apple-id "$APPLEID" \
    --password "$APPLEIDPASS" \
    --team-id "$APPLETEAMID" \
    --wait

xcrun stapler staple "$1"
