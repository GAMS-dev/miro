*configuration of WebUI input
$onecho > webuiconf.json
{ "GMSPAR_casename": {
     "alias": "Input case",
     "dropdown": {
         "label": "Select input case [case]",
	 "choices": "casename$filename"
     }
  }, 
  "GMSPAR_timeperiod": {
     "alias": "Timeperiod",
     "slider": {
         "label": "Select the time period to solve [t]",
         "min": 1,
         "max": "max(casename$noPeriods)",
         "default": "max(casename$noPeriods)",
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
  "GMSPAR_lineloss": {
     "alias": "lineloss",
     "checkbox": {
         "label": "Approximate lineloss? [lineloss]",
         "value": 0,
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
  "GMSOPT_lp": {
     "alias": "LP-Solver",
     "dropdown": {
         "label": "Solver to use for LP",
         "aliases": ["BARON","BDMLP","CBC","CONOPT 3","CONOPT 4","CPLEX","DECIS","GUROBI","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","MINOS","MOSEK","SNOPT","SOPLEX","XA","XPRESS"],
         "choices": ["BARON","BDMLP","CBC","CONOPT3","CONOPT4","CPLEX","DECIS","GUROBI","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","MINOS","MOSEK","SNOPT","SOPLEX","XA","XPRESS"],
         "selected": "CPLEX"
     }
  },
  "GMSOPT_qcp": {
     "alias": "QCP-Solver",
     "dropdown": {
         "label": "Solver to use for QCP",
         "aliases": ["ANTIGONE","BARON","CONOPT 3","CONOPT 4","COUENNE","CPLEX","GLOMIQO","GUROBI","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","LOCALSOLVER","MINOS","MOSEK","MSNLP","OQNLP","SCIP","SNOPT","XPRESS"],
         "choices": ["ANTIGONE","BARON","CONOPT3","CONOPT4","COUENNE","CPLEX","GLOMIQO","GUROBI","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","LOCALSOLVER70","MINOS","MOSEK","MSNLP","OQNLP","SCIP","SNOPT","XPRESS"],
         "selected": "CPLEX"
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