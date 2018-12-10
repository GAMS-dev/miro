* Define input case
$if not set case $setGlobal case %MODELPATH%cases%system.dirsep%case118.gdx
$if set casename $setGlobal case %MODELPATH%cases%system.dirsep%%casename%
* set times
$ifthen.out %times_lo% == %times_up%
$   setGlobal times %times_lo%
$elseif.out set times_lo
$  ifthen.in set times_up
$     setGlobal times %times_lo%*%times_up%
$  endif.in
$endif.out