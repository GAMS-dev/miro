*configuration of WebUI input
$onecho > webuiconf.json
{ "GMSPAR_casename": {
     "alias": "Input case",
     "dropdown": {
         "label": "Select input case [case]",
	 "choices": "casename$filename"	 
     },
     "noBatch": true
  }, 
  "GMSPAR_times": {
     "alias": "Time range",
     "slider": {
         "label": "Select the time range to solve. Note: times > 1 only usable for RTS-96 cases with 24 hour demand data! [times]",
         "min": 1,
         "max": "max(casename$noPeriods)",
         "default": [1,"max(casename$noPeriods)"],
         "step": 1
     }
  },
  "GMSPAR_allon": {
     "alias": "allon",
     "dropdown": {
         "label": "Turn on all gens and/or lines during solve [allon]",
         "aliases": ["lines", "none"],
         "choices": ["lines", 0],
         "selected": "%allon%"
     }
  },
  "GMSPAR_obj": {
     "alias": "Obj",
     "dropdown": {
         "label": "Objective function [obj]",
         "aliases": ["quadratic","piecewise linear"],
         "choices": ["quad", "pwl"],
         "selected": "%obj%"
     }
  },
  "GMSPAR_linelimits": {
     "alias": "linelimits",
     "dropdown": {
         "label": "Type of line limit data to use [linelimits]",
         "choices": ["given", "uwcalc", "inf"],
         "selected": "%linelimits%"
     }
  },
  "GMSPAR_genPmin": {
     "alias": "genPmin",
     "dropdown": {
         "label": "Data for Generator lower limit [genPmin]",
         "choices": ["0", "given", "uwcalc"],
         "selected": "%genPmin%"
     }
  },
  "GMSPAR_ramprates": {
     "alias": "ramprates",
     "dropdown": {
         "label": "Type of ramprate data to use [ramprates]",
         "choices": ["given", "uwcalc"],
         "selected": "%ramprates%"
     }
  },
  "GMSPAR_demandbids": {
     "alias": "demandbids",
     "checkbox": {
         "label": "Turn on elastic demand bidding? [demandbids]",
         "value": %demandbids%,
         "class": "checkbox-material"
     }
  },
  "GMSPAR_slim": {
     "alias": "slim",
     "checkbox": {
         "label": "Use apparent power limits on line? [slim]",
         "value": 0,
         "class": "checkbox-material"
     }
  },
  "GMSPAR_relax": {
     "alias": "relax",
     "checkbox": {
         "label": "Relax integer models? [relax]",
         "value": %relax%,
         "class": "checkbox-material"
     }
  },
  "GMSPAR_wind": {
     "alias": "wind",
     "checkbox": {
         "label": "Turn off wind turbines? [wind]",
         "value": 0,
         "class": "checkbox-material"
     }
  },
  "GMSOPT_rminlp": {
     "alias": "rminlp-Solver",
     "dropdown": {
         "label": "Solver to use for rminlp",
         "aliases": ["ANTIGONE","CONOPT 3","CONOPT 4","COUENNE","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","MINOS","MOSEK","MSNLP","OQNLP","SCIP","SNOPT"],
         "choices": ["ANTIGONE","CONOPT3","CONOPT4","COUENNE","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","MINOS","MOSEK","MSNLP","OQNLP","SCIP","SNOPT"],
         "selected": "CONOPT3"
     }
  },
  "GMSOPT_minlp": {
     "alias": "minlp-Solver",
     "dropdown": {
         "label": "Solver to use for minlp",
         "aliases": ["ALPHAECP","ANTIGONE","BONMIN","COUENNE","DICOPT","KNITRO","LINDO","LINDOGLOBAL","OQNLP","SBB","SCIP"],
         "choices": ["ALPHAECP","ANTIGONE","BONMIN","COUENNE","DICOPT","KNITRO","LINDO","LINDOGLOBAL","OQNLP","SBB","SCIP"],
         "selected": "DICOPT"
     }
  },
  "GMSPAR_savesol": {
     "alias": "savesol",
     "checkbox": {
         "label": "Save solution? [savesol]",
         "value": 0,
         "class": "checkbox-material"
     }
  },
  "GMSPAR_verbose": {
     "alias": "verbose",
     "checkbox": {
         "label": "Print input in listing output? [verbose]",
         "value": 1,
         "class": "checkbox-material"
     }
  }
}
$offecho

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