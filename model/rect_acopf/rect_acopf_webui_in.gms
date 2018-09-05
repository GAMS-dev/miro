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
         "default": 1,
         "step": 1
     }
  },
  "GMSPAR_allon": {
     "alias": "allon",
     "dropdown": {
         "label": "Option to turn on all gens or lines during solve [allon]",
         "choices": ["gens", "lines", "both", 0],
         "selected": "%allon%"
     }
  },
  "GMSPAR_obj": {
     "alias": "Obj",
     "dropdown": {
         "label": "Objective function, piecewise linear or quadratic [obj]",
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
  "GMSPAR_slim": {
     "alias": "slim",
     "checkbox": {
         "label": "Use apparent power limits on line? [slim]",
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
  "GMSPAR_ic": {
     "alias": "ic",
     "dropdown": {
         "label": "Choose a method for generating initial conditions, i.e. NLP starting point [ic]",
         "aliases": ["_","1: Random","2: Flat","3: Random/AC","4: DC/AC","5: DC-/AC","6: Decoupled","7: DCLoss","8: Matpower","9: inputFile"],
         "choices": ["_",1,2,3,4,5,6,7,8,9]
     }
  },
  "GMSOPT_nlp": {
     "alias": "nlp-Solver",
     "dropdown": {
         "label": "Solver to use for nlp",
         "aliases": ["ANTIGONE","BARON","CONOPT 3","CONOPT 4","COUENNE","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","LOCALSOLVER","MINOS","MOSEK","MSNLP","OQNLP","SCIP","SNOPT"],
         "choices": ["ANTIGONE","BARON","CONOPT3","CONOPT4","COUENNE","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","LOCALSOLVER70","MINOS","MOSEK","MSNLP","OQNLP","SCIP","SNOPT"],
         "selected": "CONOPT3"
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