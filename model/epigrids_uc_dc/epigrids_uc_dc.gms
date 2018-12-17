$ifthen.modelname exist "%gams.idir1%..%system.dirsep%uc_dc%system.dirsep%uc_dc.gms"
$include "%gams.idir1%..%system.dirsep%uc_dc%system.dirsep%uc_dc.gms"
$else.modelname
$abort "Model uc_dc.gms not found."
$endif.modelname
