$set sym %1
$ifi %sym%==scalars $goto doScalars
$ifthen.l1 dimension 1 %sym%
$  if     parType %sym% $call csv2gdx %sym%.csv id=%sym% useheader=y index=1 values=2 %system.reDirLog%
$  if not parType %sym% $call csv2gdx %sym%.csv id=%sym% useheader=y index=1          %system.reDirLog%
$else.l1
$  if dimension 2 %sym% $set dim 1
$  if dimension 3 %sym% $set dim 2
$  if dimension 4 %sym% $set dim 3
$  if not set dim $abort dimension not properly set for %sym%
$  eval dimp1 %dim%+1
$  call csv2gdx %sym%.csv id=%sym% useheader=y valueDim=1 index=1..%dim% values=%dimp1%..lastCol %system.reDirLog%
$endif.l1
$ifthen.l1 errorlevel 1
$  abort problems reading csv %sym%
$else.l1
$  gdxin %sym%
$  set n 1
$  label start
$  shift
$  if "x"=="x%1" $goto term
$    loadM %1<%sym%.dim%n%
$    eval n %n%+1
$    goto start
$  label term
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
       os.environ['%fn%'.upper() + '_' + row[0].upper()] = row[2]
$offembeddedCode
