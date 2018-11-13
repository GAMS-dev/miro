$title "DC Optimal Power Flow with unit commitment"
*_______________________________________________________________________________
* Filename: uc_dc.gms
* Description: DC Optimal Power Flow model with unit commitment
*
* Usage: gams uc_dc --case=/path/case.gdx
*
* Options:
* --verbose: Supresses printout(0). Default=1
* --obj: Objective function, piecewise linear or quadratic. Default="pwl"
* --times: Select timeframe(s) to solve. Default provided by input file
* --linelimits: Type of line limit data to use. Default="given"
* --ramprates: Type of ramprate data to use. Default="given"
* --genPmin: Data for Generator lower limit. Default="given"
* --allon: Option to turn on all lines during solve. Default=none
* --lineloss: Option to approximate lineloss. Default = 0
* --relax: Turn on relaxed integer option(1).Default=0.
* --savesol: Turn on save solution option(1). Default=0
* --wind: Whether to turn off wind turbines. Can only be used with
*         PrimeMover,pm_WT. Default=0.
*_______________________________________________________________________________

*===== SECTION: OPTIONS & ENVIRONMENT VARIABLES
* Printout options
$ifthen %verbose% == 0
* Turn off print options
$offlisting
option limrow=0, limcol=0
$endif

* Default: Piecewise linear objective
$if not set obj $set obj "pwl"
* Default: elastic demand bidding turned off
$if not set demandbids $set demandbids 0
* Default: Use provided line limits (as opposed to uwcalc)
$if not set linelimits $set linelimits "given"
* Default: Use provided ramprates (as opposed to uwcalc)
$if not set ramprates $set ramprates "given"
* Default: Use provided generator lower limit
$if not set genPmin $set genPmin "given"
* Default: Relaxed MIP option turned off
$if not set relax $set relax 0
* Default: Save solution option turned off
$if not set savesol $set savesol 0
* Default: Line loss not approximated
$if not set lineloss $set lineloss 0
* Default: Available generator scenario: all generators
$if not set genrun $set genrun allgen

* Define filepath, name and extension.
$setnames "%gams.i%" filepath filename fileextension
$setglobal MODELPATH '%filepath%..%system.dirsep%'

$if set gmswebui $include uc_dc_webui_in.gms

* Define type of model
$set modeltype "DC"
* Define input case
$if not set case $abort "Model aborted. Please provide input case"
$setnames "%case%" casepath casename caseextension

*===== SECTION: EXTRACT DATA
$batinclude "%MODELPATH%extract_data_uc.gms" modeltype case times demandbids linelimits ramprates genPmin allon

*===== SECTION: DATA MANIPULATION
* If no data is provided for a generator's minimum up and down time, set to 1
minuptime(gen) = max(1,minuptime(gen));
mindowntime(gen) = max(1,mindowntime(gen));

* DCOPF assumes reactive power = 0
B(i,j,c)$line(i,j,c) = -1/x(i,j,c);

* Define active lines in (i,j) space
set isLine(i,j) "Active (i,j) line";
option isLine <  branchstatus;

* Approximate line loss
$ifthene.loss %lineloss%=0 
$elseife.loss %lineloss%>=1 Pd(i,t)=Pd(i,t)*%lineloss%;
$else.loss  $abort "Invalid lineloss"
$endif.loss

*===== SECTION: VARIABLE DEFINITION
free variables
    V_P(gen,t) "Real power generation of generator at time t"
    V_Theta(bus,t) "Voltage phase angle at bus at time t"
    V_LineP(i,j,c,t)   "Real power flowing from bus i towards bus j on line c at time t"
;

binary variables
    V_genstatus(gen,t)    "Generator commitment status for time t"
;

positive variables
    V_startup(gen,t) "(0,1) startup status of generator at time t"
    V_shutdown(gen,t) "(0,1) shutdown status of generator at time t"
    V_interfaceP(i,j,c,t) "Real power flow on interface (i,j,c) at time t"
    V_pw_cost(gen,t) "Piecewise linear cost of generation at time t"
    V_Pd_elastic(demandbid,t) "Elastic incremental demand"
    V_demandbid_rev(demandbid,t) "Revenue from elastic incremental demand"
;

free variable V_objcost "Total cost of objective function";


*===== SECTION: EQUATION DEFINITION
equations
    c_LinePowerDef(i,j,c,t) "Definition of real power on line (i,j,c) at time t"
    c_BalanceP(bus,t) "Balance of real power for bus at time t"
    c_InterfaceP(i,j,c,t) "Definition of real power on interfaces involving (i,j,c) at time t"
    c_InterfaceLimit(interface,t) "Limit of real power on interface at time t"

    c_AngleDifferenceIJ(i,j,t) "Bound on angle difference of (i,j)"
    c_AngleDifferenceJI(i,j,t) "Bound on angle difference of (j,i)"

    c_GenStatusMin(gen,t) "Generator minimum operating capacity"
    c_GenStatusMax(gen,t) "Generator maximum operating capacity"
    c_StartupShutdown(gen,t) "Relationship of binary (start,shut,status) variables"
    c_MinUptime(gen,t) "Minimum generator run time"
    c_MinDowntime(gen,t) "Minimum generator down time"
    c_RampUp(gen,t) "Generator ramp up constraints"
    c_RampDown(gen,t) "Generator ramp down constraints"

* If elastic bidding is turned on
$if %demandbids% ==1 c_demandbid_revenue(demandbid,t,demandbid_s) "Revenue from elastic demand"

    c_pw_cost(gen,t,costptset) "Generator piecewise cost functions"
    c_obj "Objective function"
;

*===== SECTION: EQUATIONS PART 1
* Definition of real power on line (i,j,c) at time t
c_LinePowerDef(i,j,c,t)$branchstatus(i,j,c,t)..
V_LineP(i,j,c,t) =e= (B(i,j,c)/ratio(i,j,c)) * (V_Theta(j,t) - V_Theta(i,t) + angle(i,j,c));

* Balance of real power for bus at time t
c_BalanceP(i,t)$(type(i) ne 4)..
sum(gen$(atBus(gen,i)), V_P(gen,t))
    - sum((j,c)$(branchstatus(i,j,c,t)), V_LineP(i,j,c,t))
    + sum((j,c)$(branchstatus(j,i,c,t)), V_LineP(j,i,c,t))
    - Pd(i,t)
    - sum(demandbid$demandbidmap(demandbid,i), V_Pd_elastic(demandbid,t))
    - Gs(i)
        =e=  0
;

* Definition of real power on interfaces involving (i,j,c) at time t
* Since we only care about interfaces in the specified direction, we don't need abs(LinePower)
c_InterfaceP(i,j,c,t)$((branchstatus(i,j,c,t) or branchstatus(j,i,c,t))
    and (sum(interface$interfacemap(interface,i,j), 1) ge 1))..
V_interfaceP(i,j,c,t) =e=  V_LineP(i,j,c,t);

* Limit of real power on interface at time t
c_InterfaceLimit(interface,t)..
sum((i,j,c)$interfacemap(interface,i,j), V_interfaceP(i,j,c,t)) =l= interfaceLimit(interface,t);

* Bound on angle difference between (i,j)
c_AngleDifferenceIJ(i,j,t)$isLine(i,j)..
V_Theta(j,t)-V_Theta(i,t) =l= pi/3;

* Bound on angle difference between (j,i)
c_AngleDifferenceJI(i,j,t)$isLine(i,j)..
V_Theta(i,t)-V_Theta(j,t) =l= pi/3;

* Generator minimum operating capacity
c_GenStatusMin(gen,t)..
V_genstatus(gen,t) * Pmin(gen) =l= V_P(gen,t);

* Generator maximum operating capacity
c_GenStatusMax(gen,t)..
V_P(gen,t) =l= V_genstatus(gen,t) * Pmax(gen);

* Relationship of binary (start,shut,status) variables
c_StartupShutdown(gen,t)..
V_startup(gen,t) - V_shutdown(gen,t) =e= V_genstatus(gen,t) - V_genstatus(gen,t-1);


* Minimum generator run time
c_MinUptime(gen,t)$(minuptime(gen) gt 0)..
V_genstatus(gen,t) =g=
    sum(t1$((ord(t1) ge (ord(t) - minuptime(gen) + 1)) and (ord(t1) le ord(t))), V_startup(gen,t1))
;

* Minimum generator down time
c_MinDowntime(gen,t)$(mindowntime(gen) gt 0)..
V_genstatus(gen,t) =l=
    1 - sum(t1$((ord(t1) ge (ord(t) - mindowntime(gen) + 1)) and (ord(t1) le ord(t))), V_shutdown(gen,t1))
;

* Generator ramp up constraints
c_RampUp(gen,t)$(ord(t) ge 2)..
V_P(gen,t) =l=  V_P(gen,t-1) + rampup(gen)*V_genstatus(gen,t) + (Pmax(gen)-rampup(gen))*V_startup(gen,t);

* Generator ramp down constraints
c_RampDown(gen,t)$(ord(t) ge 2)..
V_P(gen,t-1) =l=
    V_P(gen,t) + rampdown(gen)*V_genstatus(gen,t) + (Pmax(gen)-rampdown(gen))*V_shutdown(gen,t)
;

* Objective functions and pwl costs are listed in a separate file
$batinclude "%MODELPATH%cost_objective_uc.gms" obj demandbids


*===== SECTION: VARIABLE BOUNDS
V_startup.up(gen,t)=1;
V_shutdown.up(gen,t)=1;

* Generator power generation limits
V_P.lo(gen,t) = 0;
V_P.up(gen,t) = Pmax(gen);
$ifthen %wind%==1
* Needed to avoid compilation error. Puts strings into UEL
set winddesc /'PrimeMover', 'pm_WT'/;
* Wind turbines are not reliable sources of power, treated differently
parameter windTurbine(gen);
windTurbine(gen)$(geninfo(gen, 'PrimeMover', 'pm_WT') eq 1) = 1;
V_P.fx(gen,t)$(windTurbine(gen)) = 0;
$endif

* Line real power flow limits
V_LineP.lo(i,j,c,t)$(branchstatus(i,j,c,t)) = -rateA(i,j,c);
V_LineP.up(i,j,c,t)$(branchstatus(i,j,c,t)) = rateA(i,j,c);

* Set bounds and fix reference bus angle
V_Theta.lo(bus,t) = -Pi;
V_Theta.up(bus,t) = Pi;
V_Theta.fx(bus,t)$(type(bus) eq 3) = 0;

*--- Elastic demand
* If user chooses option --demandbids=0, no elastic demand is considered
* Otherwise, set bounds on elastic demand
$ifthen.nobids %demandbids% == 0
  V_Pd_elastic.fx(demandbid,t) = 0;
  V_demandbid_rev.fx(demandbid,t) = 0;
$else.nobids
  V_Pd_elastic.lo(demandbid,t) = smin(demandbid_s, demandpts_x(demandbid,t,demandbid_s))/baseMVA;
  V_Pd_elastic.up(demandbid,t) = smax(demandbid_s, demandpts_x(demandbid,t,demandbid_s))/baseMVA;
  V_demandbid_rev.lo(demandbid,t) = smin(demandbid_s, demandpts_y(demandbid,t,demandbid_s));
  V_demandbid_rev.up(demandbid,t) = smax(demandbid_s, demandpts_y(demandbid,t,demandbid_s));
$endif.nobids


*===== SECTION: VARIABLE STARTING LEVELS
* Starting values may be provided in the data file
* Startup and shutdown variables are binary {0,1} via equations
V_genstatus.l(gen,t) = status(gen,t);
V_startup.l(gen,t)  = max(0, status(gen,t)-status(gen,t-1));
V_shutdown.l(gen,t) = max(0, status(gen,t-1)-status(gen,t));

* Starting values for real power and theta may be provided
V_P.l(gen,t) = Pg(gen,t);
V_Theta.l(bus,t) = Va(bus,t);

*===== SECTION: MODEL DEFINITION
* Feasibility model
model feas /c_BalanceP, c_LinePowerDef, c_InterfaceP, c_InterfaceLimit,
            c_AngleDifferenceIJ, c_AngleDifferenceJI,
            c_GenStatusMin, c_GenStatusMax, c_StartupShutdown,
            c_MinUptime, c_MinDowntime,c_RampUp, c_RampDown /;

* UC DCOPF model
model m_ucdc /feas, c_pw_cost, c_obj
$if %demandbids% ==1    ,c_demandbid_revenue
      /;

*===== SECTION: MODEL OPTIONS AND SOLVE
* Type of solve
$iftheni.sol %obj% == "quad"

$ifthen.rmiqp %relax% ==1 solve m_ucdc min V_objcost using rmiqcp;
$else.rmiqp  solve m_ucdc min V_objcost using miqcp;
$endif.rmiqp

$else.sol

$ifthen.rmip %relax% ==1 solve m_ucdc min V_objcost using rmip;
$else.rmip  solve m_ucdc min V_objcost using mip;
$endif.rmip

$endif.sol


*==== SECTION: Solution Analysis
* See if model is solved
parameter
    infeas "Number of infeasibilities from model solve";

infeas = m_ucdc.numInfes;
display infeas;

* Declaration needs to be made outside loop
set
  lines_at_limit(i,j,c,t) "lines at their bound"
;
parameters
    total_cost "Final objective value"
    LMP(bus,t) "Locational Marginal Price of bus at time t"
    LineSP(i,j,c,t) "Marginals of active power flowing on line (i,j,c) at time t"
;

*-- Rest of file is only executed if the model solved properly
if(infeas eq 0,

* Final Objective function value
total_cost = V_objCost.l;
* Generator real power solution
Pg(gen,t) = V_P.l(gen,t);
* Generator on/off solution
status(gen,t) = V_genstatus.l(gen,t);

* DCOPF assumes voltage magnitude is 1
Vm(bus,t) = 1;
* Voltage angle solution
Va(bus,t) = V_Theta.l(bus,t);
* Locational marginal price of bus at time t
LMP(bus,t) = c_BalanceP.m(bus,t);
* Marginal for active power on a line
LineSP(i,j,c,t)$branchstatus(i,j,c,t) = V_LineP.m(i,j,c,t);

* Find which lines are at their limits
lines_at_limit(i,j,c,t)$branchstatus(i,j,c,t) = yes$(abs(LineSP(i,j,c,t)) gt 1e-8);
display lines_at_limit;

*==== SECTION: Solution Save
$SetGlobal out %casename%_DC_UC_solution.gdx
execute_unload 'temp_solution.gdx', t, Pg, Vm, Va, total_cost, LMP, LineSP, status;
execute 'gams %MODELPATH%save_solution_uc.gms gdxcompress=1 --ac=0 --uc=1 --timeperiod=%timeperiod% --case=%case% --savesol=%savesol% --solution=temp_solution.gdx --out=%out%'
if(errorlevel ne 0, abort "Saving solution failed!");
execute 'rm temp_solution.gdx'

* END IF-loop if(infeas eq 0)
);

$onExternalOutput
Parameter ePrice(i,t) 'Electricity prices';
$offExternalOutput
ePrice(i,t) = c_BalanceP.m(i,t);

$if set gmswebui $include %MODELPATH%webui_out.gms
$if set gmswebui $batinclude %MODELPATH%webui.gms