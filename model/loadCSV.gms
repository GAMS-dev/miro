$set sym %1
$ifi %sym%==scalars $goto doScalars
$ifthen.l1 dimension 1 %sym%
$  if     parType %sym% $call csv2gdx %sym%.csv id=%sym% useheader=y index=1 values=2 %system.reDirLog%
$  if not parType %sym% $call csv2gdx %sym%.csv id=%sym% useheader=y index=1          %system.reDirLog%
$else.l1
$  if dimension 2 %sym% $set dim 1
$  if dimension 3 %sym% $set dim 2
$  if dimension 4 %sym% $set dim 3
$  if dimension 5 %sym% $set dim 4
$  if dimension 6 %sym% $set dim 5
$  if dimension 7 %sym% $set dim 6
$  if dimension 8 %sym% $set dim 7
$  if dimension 9 %sym% $set dim 8
$  if dimension 10 %sym% $set dim 9
$  if dimension 11 %sym% $set dim 10
$  if dimension 12 %sym% $set dim 11
$  if dimension 13 %sym% $set dim 12
$  if dimension 14 %sym% $set dim 13
$  if dimension 15 %sym% $set dim 14
$  if dimension 16 %sym% $set dim 15
$  if dimension 17 %sym% $set dim 16
$  if dimension 18 %sym% $set dim 17
$  if dimension 19 %sym% $set dim 18
$  if dimension 20 %sym% $set dim 19
$  if not set dim $abort dimension not properly set for %sym%
$  eval dimp1 %dim%+1
$  set valdim 1
$  ifthen.a %sysEnv.GMSWEBUI_EXPAND_HEADER% == 1
$    eval dim %dim%+1
$    set valdim 0
$  endif.a
$  eval dimp1 %dim%+1
$  call csv2gdx %sym%.csv id=%sym% useheader=y valueDim=%valdim% index=1..%dim% values=%dimp1%..lastCol %system.reDirLog%
$endif.l1
$ifthen.l1 errorlevel 1
$  abort problems reading csv %sym%
$else.l1
$  gdxin %sym%
$  set n 1
$  label start_loadcsv2
$  shift
$  if "x"=="x%1" $goto term_loadcsv2
$    loadM %1<%sym%.dim%n%
$    eval n %n%+1
$    goto start_loadcsv2
$  label term_loadcsv2
$  loadDC %sym%
$  gdxin
$  hiddencall rm -f %sym%.gdx
$endif.l1
$exit
$label doScalars
$setNames "%gams.input%" fp fn fe
$onembeddedCode Python:
import csv
import os
if os.path.isfile('scalars.csv'):
   with open('scalars.csv', 'r') as csvfile:
     scalars = csv.reader(csvfile)
     next(scalars) # skip header row
     for row in scalars:
       os.environ['%fn%'.upper() + '_' + row[0]] = row[2]
$offembeddedCode
