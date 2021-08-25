# Notarization script in large part taken from: https://github.com/rstudio/rstudio/blob/master/docker/jenkins/notarize-release.sh
# Copyright (C) 2009-19 by RStudio, Inc.
# License: GNU AGPL v.3.0

XCRUN_RESULT="$(mktemp)"
xcrun altool --notarize-app \
    --primary-bundle-id "com.gams.miro" \
    --username $APPLEID \
    --password "@env:APPLEIDPASS" \
    --file "$1" \
    --output-format xml > $XCRUN_RESULT

# Check result - PlistBuddy does not work when the same DMG has been uploaded twice.
#                Then altool returns some text plus the XML plist stuff. This is why
#                the second version is used.
if [ $? -eq 0 ]; then
    # Extract the request UUID from the result
    REQUEST_UUID=$(/usr/libexec/PlistBuddy -c "Print :notarization-upload:RequestUUID" $XCRUN_RESULT)
    echo "Notarization request with UUID $REQUEST_UUID created."
else
    echo "Notarization request submission failed. Server response:"
    cat $XCRUN_RESULT
    exit 1
fi
#REQUEST_UUID=$(cat $XCRUN_RESULT | grep RequestUUID | cut -f2 -d"=" | tr -d '[:space:]')
#if [ -z "$REQUEST_UUID" ];
#then
#    REQUEST_UUID=$(cat $XCRUN_RESULT | grep -e "\*\*\* Error: ERROR ITMS-90732: .*" |   \
#                   sed -nE "s/^[[:digit:]]+.*ID is ([0-9a-z-]+)\".*/\1/p")
#fi

# Wait for notarization to complete
echo "Waiting for notarization to complete. This will take several minutes."
sleep 120
while true; do
    sleep 60
    echo "Checking notarization status..."
    xcrun altool --notarization-info $REQUEST_UUID \
                 --username $APPLEID \
                 --password "@env:APPLEIDPASS" \
                 --output-format xml > $XCRUN_RESULT
    NOTARIZATION_STATUS=$(/usr/libexec/PlistBuddy -c "Print :notarization-info:Status" $XCRUN_RESULT)
    if [ $? -eq 0 ]; then
        if [ "$NOTARIZATION_STATUS" != "in progress" ]; then
            echo "Notarization ended; result: $NOTARIZATION_STATUS"
            break
        fi
        echo "Notarization still in progress. Waiting 60s to check again."
    else
        echo "Could not determine notarization status; giving up. Server response:"
        cat $XCRUN_RESULT
        exit 1
    fi
done

# Staple the notarization ticket to the app bundle
if [ "$NOTARIZATION_STATUS" == "success" ]; then
    LOGFILE_URL=$(/usr/libexec/PlistBuddy -c "Print :notarization-info:LogFileURL" $XCRUN_RESULT)
    LOGFILE_PATH="$(mktemp)"
    curl ${LOGFILE_URL} > "${LOGFILE_PATH}" || {
        echo "Problems receiving notarization log file"
        exit 1
    }
    if grep -q -F '"issues": [' "${LOGFILE_PATH}"; then
        echo "Notarization finished with issues! Can not distribute GAMS like this. Check issues at: ${LOGFILE_URL}"
        exit 1
    fi
    echo "Notarization successful; stapling ticket to app bundle"
    xcrun stapler staple "$1"
else
    echo "Notarization failed."
    exit 1
fi
