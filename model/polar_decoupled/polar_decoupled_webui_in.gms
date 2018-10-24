$onecho > webuiconf.json
{ "GMSPAR_casename": {
     "alias": "Input case",
     "dropdown": {
         "label": "Select input case [case]",
	 "choices": "casename$filename"
     },
     "noBatch": true
  }, 
  "GMSPAR_timeperiod": {
     "alias": "Timeperiod",
     "slider": {
         "label": "Select the time period to solve [t]",
         "min": 1,
         "max": "max(casename$noPeriods)",
         "default": 1,
         "step": 1
     }
  },
  "GMSPAR_allon": {
     "alias": "allon",
     "dropdown": {
         "label": "Turn on all gens and/or lines during solve [allon]",
         "aliases": ["generators", "lines", "generators & lines", "none"],
         "choices": ["gens", "lines", "both", 0],
         "selected": "%allon%"
     }
  },
  "GMSPAR_obj": {
     "alias": "Obj",
     "dropdown": {
         "label": "Objective function [obj]",
         "aliases": ["quadratic","piecewise linear","linear","none"],
         "choices": ["quad", "pwl", "linear", 0],
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
  "GMSPAR_qlim": {
     "alias": "qlim",
     "checkbox": {
         "label": "Enforce reactive power limits as D-curve circle constraints? [qlim]",
         "value": 0,
         "class": "checkbox-material"
     }
  }, 
  "GMSPAR_iter": {
     "alias": "iter",
     "slider": {
         "label": "Number of iterations [iter]",
         "min": 1,
         "max": 1000,
         "default": 1,
         "step": 1
     }
  },
  "GMSOPT_nlp": {
     "alias": "nlp-Solver",
     "dropdown": {
         "label": "Solver to use for nlp",
         "aliases": ["ANTIGONE","CONOPT 3","CONOPT 4","COUENNE","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","MINOS","MOSEK","MSNLP","OQNLP","SCIP","SNOPT"],
         "choices": ["ANTIGONE","CONOPT3","CONOPT4","COUENNE","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","MINOS","MOSEK","MSNLP","OQNLP","SCIP","SNOPT"],
         "selected": "CONOPT3"
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