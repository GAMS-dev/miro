* Define input case
$if not set case $setGlobal case %MODELPATH%cases%system.dirsep%case118.gdx
$if set casename $setGlobal case %MODELPATH%cases%system.dirsep%%casename%
* set times
$ifthen.out %times_MIN% == %times_MAX%
$   setGlobal times %times_MIN%
$elseif.out set times_MIN
$  ifthen.in set times_MAX
$     setGlobal times %times_MIN%*%times_MAX%
$  endif.in
$endif.out