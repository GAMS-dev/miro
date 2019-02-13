$stitle Create static config files for MIRO by unzipping the appropriate zip file
$setNames "%gams.input%" fp fn fe
$if exist %fn%_conf.zip $hiddencall gmsunzip -qq -o -a %fn%_conf.zip
