*configuration of WebUI input
$onecho > webuiconf.json
{ "GMSPAR_casename": {
     "alias": "Input case",
     "dropdown": {
         "label": "Select input case [case]",
	 "choices": "casename$filename"
	 
     }
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
  "GMSPAR_relax": {
     "alias": "relax",
     "checkbox": {
         "label": "Relax integer models? [relax]",
         "value": %relax%,
         "class": "checkbox-material"
     }
  },
  "GMSPAR_lineloss": {
     "alias": "lineloss",
     "checkbox": {
         "label": "Approximate lineloss? [lineloss]",
         "value": %lineloss%,
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
  "GMSOPT_mip": {
     "alias": "MIP-Solver",
     "dropdown": {
         "label": "Solver to use for MIP",
         "aliases": ["BARON","BDMLP","CBC","CPLEX","GUROBI","LINDO","LINDOGLOBAL","LOCALSOLVER","MOSEK","ODHCPLEX","SCIP","XA","XPRESS"],
         "choices": ["BARON","BDMLP","CBC","CPLEX","GUROBI","LINDO","LINDOGLOBAL","LOCALSOLVER70","MOSEK","ODHCPLEX","SCIP","XA","XPRESS"],
         "selected": "CPLEX"
     }
  },
  "GMSOPT_miqcp": {
     "alias": "miqcp-Solver",
     "dropdown": {
         "label": "Solver to use for miqcp",
         "aliases": ["ALPHAECP","ANTIGONE","BARON","BONMIN","COUENNE","CPLEX","DICOPT","GLOMIQO","GUROBI","KNITRO","LINDO","LINDOGLOBAL","LOCALSOLVER","MOSEK","ODHCPLEX","OQNLP","SBB","SCIP","XPRESS"],
         "choices": ["ALPHAECP","ANTIGONE","BARON","BONMIN","COUENNE","CPLEX","DICOPT","GLOMIQO","GUROBI","KNITRO","LINDO","LINDOGLOBAL","LOCALSOLVER70","MOSEK","ODHCPLEX","OQNLP","SBB","SCIP","XPRESS"],
         "selected": "CPLEX"
     }
  },
  "GMSOPT_rmip": {
     "alias": "rmip-Solver",
     "dropdown": {
         "label": "Solver to use for rmip",
         "aliases": ["BARON","BDMLP","CBC","CONOPT 3","CONOPT 4","CPLEX","DECIS","GUROBI","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","MINOS","MOSEK","SNOPT","SOPLEX","XA","XPRESS"],
         "choices": ["BARON","BDMLP","CBC","CONOPT3","CONOPT4","CPLEX","DECIS","GUROBI","IPOPT","KNITRO","LGO","LINDO","LINDOGLOBAL","MINOS","MOSEK","SNOPT","SOPLEX","XA","XPRESS"],
         "selected": "CPLEX"
     }
  },
  "GMSOPT_rmiqcp": {
     "alias": "rmiqcp-Solver",
     "dropdown": {
         "label": "Solver to use for rmiqcp",
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
* set times
$ifthen.out %times_MIN% == %times_MAX%
$   setGlobal times %times_MIN%
$elseif.out set times_MIN
$  ifthen.in set times_MAX
$     setGlobal times %times_MIN%*%times_MAX%
$  endif.in
$endif.out