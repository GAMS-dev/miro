*configuration of WebUI input
$ifthen.UIin set gmswebui
$onecho > webuiconf.json
{ "GMSPAR_case": {
     "alias": "Input case",
     "dropdown": {
         "label": "Select input case",
	 "choices": "case$filename"
     }
  }, 
  "GMSPAR_timeperiod": {
     "alias": "Timeperiod",
     "slider": {
         "label": "Select the time period to solve",
         "min": 1,
         "max": "max(case$noPeriods)",
         "default": 1,
         "step": 1
     }
  },
  "GMSPAR_allon": {
     "alias": "allon",
     "dropdown": {
         "label": "Turn on all gens and/or lines during solve",
         "aliases": ["generators", "lines", "generators & lines", "none"],
         "choices": ["gens", "lines", "both", 0],
         "selected": "%allon%"
     }
  },
  "GMSPAR_obj": {
     "alias": "Obj",
     "dropdown": {
         "label": "Objective function",
         "aliases": ["quadratic","piecewise linear","linear","none"],
         "choices": ["quad", "pwl", "linear", 0],
         "selected": "%obj%"
     }
  },
  "GMSPAR_linelimits": {
     "alias": "linelimits",
     "dropdown": {
         "label": "Type of line limit data to use",
         "choices": ["given", "uwcalc", "inf"],
         "selected": "%linelimits%"
     }
  },
  "GMSPAR_genPmin": {
     "alias": "genPmin",
     "dropdown": {
         "label": "Data for Generator lower limit",
         "choices": ["0", "given", "uwcalc"],
         "selected": "%genPmin%"
     }
  },
  "GMSPAR_lineloss": {
     "alias": "lineloss",
     "checkbox": {
         "label": "Approximate lineloss?",
         "value": 0,
         "class": "checkbox-material"
     }
  },
  "GMSPAR_savesol": {
     "alias": "savesol",
     "checkbox": {
         "label": "Save solution?",
         "value": 0,
         "class": "checkbox-material"
     }
  },
  "GMSPAR_wind": {
     "alias": "wind",
     "checkbox": {
         "label": "Turn off wind turbines?",
         "value": 0,
         "class": "checkbox-material"
     }
  }
}
$offecho
$endif.UIin

$ifthen.webui NOT set gmswebui
$ifthen.inner NOT set GMSWEBUI
* Define input case
$if not set case $abort "Model aborted. Please provide input case"
$endif.inner
$else.webui
* Define input case
$if set case $setGlobal case ..%system.dirsep%..%system.dirsep%model%system.dirsep%cases%system.dirsep%%case%
$log ### %case%
$if not set case $setGlobal case ..%system.dirsep%..%system.dirsep%model%system.dirsep%cases%system.dirsep%case118.gdx
$log ### %case%
$endif.webui