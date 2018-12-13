$if not set modelname $set modelname dcopf
$ifthen.modelname exist "%gams.idir1%..%system.dirsep%%modelname%%system.dirsep%%modelname%.gms"
$include "%gams.idir1%..%system.dirsep%%modelname%%system.dirsep%%modelname%.gms"
$else.modelname
$abort "Model %modelname%.gms not found."
$endif.modelname
