$title "Data conversion utility for ACOPF models"
$if not set in $abort "No input file given!"
$setnames "%in%" inpath inname inextension

$setnames "%gams.i%" filepath filename fileextension
$ifthen %system.filesys% == UNIX
$set exe ''
$else
$set exe '.exe'
$endif

$if not set type $set type matpower
$ifthen not set mode
$if %type%==psse28   $set mode cpp
$if %type%==psse32   $set mode cpp
$if %type%==matpower $set mode awk
$endif
$if not set out $set out "%inpath%%inname%.gdx"
$if not set monitorall $set monitorall 1

$ifthen %mode%==awk
$call awk -f %filepath%read_%type%.awk "%in%" > "%out%_temp.gms"
$offlisting
$include "%out%_temp.gms"
$onlisting
execute_unload "%out%raw", baseMVA, ints, bus, gen, circuit, 
                           branchrows, line, busdata, gendata,
                           gencostdata, branchdata, pwcostcoef, switchedshuntdata
execute 'rm "%out%_temp.gms"'
$elseif %mode%==cpp
$if %type%==psse28 $call '%filepath%read_psse28%exe% "%in%" "%out%raw"'
$if %type%==psse32 $call '%filepath%read_psse32%exe% "%in%" "%out%raw"'
$else abort("Invalid file read mode specified.")
$endif
if(errorlevel ne 0, abort "Creating %out%raw temp file failed!");

$if %quitearly%==1 $exit
execute 'gams "%filepath%raw2gdx.gms" lo=3 gdxcompress=1 --raw="%out%raw" gdx="%out%" --monitorall=%monitorall% profile=1 stepsum=1'
if(errorlevel ne 0, abort "raw2gdx failed!");
execute 'gams "%filepath%save_domain_info.gms" gdx="%out%" --in="%out%raw"'
if(errorlevel ne 0, abort "Save domain info failed!");
$if %quitearly%==2 $exit
execute 'rm "%out%raw.gdx"'

$ifthen (%Sbus%==1 and %verbose% == 1)
execute 'gams "%filepath%calc_S_matrix.gms" lo=3 gdxcompress=1 --case="%out%"'
$elseif %Sbus%==1
execute 'gams "%filepath%calc_S_matrix.gms" lo=3 gdxcompress=1 --case="%out%" lo=0'
$endif

