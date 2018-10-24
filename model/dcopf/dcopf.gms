$title "DC Optimal Power Flow model"
*_______________________________________________________________________________
* Filename: dcopf.gms
* Description: DC Optimal Power Flow model
*
* Usage: gams dcopf --case=/path/case.gdx
*
* Options:
* --timeperiod: Select time period to solve. Default=1
* --obj: Objective function, piecewise linear or quadratic. Default="quad"
* --linelimits: Type of line limit data to use. Default="given"
* --genPmin: Data for Generator lower limit. Default="given"
* --allon: Option to turn on all gens or lines during solve. Default=none
* --lineloss: Option to approximate lineloss. Default = 0
* --savesol: Turn on save solution option(1). Default=0
* --verbose: Supresses printout(0). Default=1
* --wind: Whether to turn off wind turbines. Can only be used with
*         PrimeMover,pm_WT. Default=0.
*_______________________________________________________________________________

*===== SECTION: OPTIONS & ENVIRONMENT VARIABLES
* Printout options
$ifthen %verbose% == 0
* Turn off print options
$offlisting
* Turn off solution listing
option solprint=off
option limrow=0, limcol=0
$endif

* Default: timeperiod = 1
$if not set timeperiod $setGlobal timeperiod "1"
* Default: allon=0
$if not set allon $setGlobal allon 0
* Default: Quadratic objective function
$if not set obj $setGlobal obj "quad"
* Default: elastic demand bidding turned off
$setGlobal demandbids 0
* Default: Use provided line limits (as opposed to uwcalc)
$if not set linelimits $setGlobal linelimits "given"
* Default: Use provided generator lower limit
$if not set genPmin $setGlobal genPmin "given"
* Default: Line loss not approximated
$if not set lineloss $setGlobal lineloss 0
* Default: Save solution option turned off
$if not set savesol $setGlobal savesol 0

* Define filepath, name and extension.
$setnames "%gams.i%" filepath filename fileextension
$setglobal MODELPATH '%filepath%..%system.dirsep%'
$LOG %MODELPATH%
$if set gmswebui $include dcopf_webui_in.gms

* Define type of model
$set modeltype "DC"
* Define input case
$if not set case $abort "Model aborted. Please provide input case"
$setnames "%case%" casepath casename caseextension

*===== SECTION: EXTRACT DATA
$batinclude %MODELPATH%extract_data.gms modeltype case timeperiod demandbids linelimits genPmin allon

*===== SECTION: DATA MANIPULATION
* DCOPF assumes reactive power = 0
B(i,j,c)$line(i,j,c) = -1/x(i,j,c);

sets isLine(i,j);
isLine(i,j)$(sum(c$branchstatus(i,j,c),1) > 0) = yes;

* Approximate line loss
$ifthene.loss %lineloss%=0 
$elseife.loss %lineloss%>=1 Pd(i)=Pd(i)*%lineloss%;
$else.loss  $abort "Invalid lineloss"
$endif.loss

*parameter maxangle;
*maxangle=smax((i,j,c)$line(i,j,c), abs(angle(i,j,c)));
*angle(i,j,c)$(line(i,j,c) AND (abs(angle(i,j,c)) - maxangle < 1e-2)) = 0;

*===== SECTION: VARIABLE DEFINITION
free variables
    V_P(gen)  "Real power generation of generator"
    V_Theta(bus)  "Voltage phase angle at bus"
    V_LineP(i,j,c)  "Real power flowing from bus i towards bus j on line c"
;

positive variables
    V_interfaceP(i,j,c) "Real power flow on interface (i,j,c)"
    V_pw_cost(gen) "Piecewise linear cost of generation at time t"
    V_Pd_elastic(demandbid) "Elastic incremental demand"
    V_demandbid_rev(demandbid) "Revenue from elastic incremental demand"
;

free variable V_objcost "Total cost of objective function";


*===== SECTION: EQUATION DEFINITION
equations
    c_LinePowerDef(i,j,c) "Definition of real power on line (i,j,c)"
    c_BalanceP(bus) "Balance of real power for bus"
    c_InterfaceP(i,j,c) "Definition of real power on interfaces involving (i,j,c)"
    c_InterfaceLimit(interface) "Limit of real power on interface"
    c_AngleDifferenceIJ(i,j) "Bound on angle difference of (i,j)"
    c_AngleDifferenceJI(i,j) "Bound on angle difference of (j,i)"

    c_pw_cost(gen,costptset) "Generator piecewise cost functions"
    c_obj  "Objective function"
;

*===== SECTION: EQUATIONS PART 1
* Definition of real power on line (i,j,c)
c_LinePowerDef(i,j,c)$branchstatus(i,j,c)..
V_LineP(i,j,c) =e= (B(i,j,c)/ratio(i,j,c)) * (V_Theta(j) - V_Theta(i) + angle(i,j,c))
;

* Balance of real power for bus
c_BalanceP(i)$(type(i) ne 4)..
sum(gen$(atBus(gen,i) and status(gen)), V_P(gen))
    - sum((j,c)$(branchstatus(i,j,c)), V_LineP(i,j,c))
    + sum((j,c)$(branchstatus(j,i,c)), V_LineP(j,i,c))
    - Pd(i) - Gs(i)  =e=  0
;

* Definition of real power on interfaces involving (i,j,c) at time t
* Since we only care about interfaces in the specified direction, we don't need abs(LinePower)
c_InterfaceP(i,j,c)$((branchstatus(i,j,c) or branchstatus(j,i,c))
    and (sum(interface$interfacemap(interface,i,j), 1) ge 1))..
V_interfaceP(i,j,c) =e= V_LineP(i,j,c);

* Limit of real power on interface at time t
c_InterfaceLimit(interface)..
sum((i,j,c)$interfacemap(interface,i,j), V_interfaceP(i,j,c)) =l=  interfaceLimit(interface);

* Bound on angle difference between (i,j)
c_AngleDifferenceIJ(i,j)$isLine(i,j)..
V_Theta(j)-V_Theta(i) =l= pi/3;

* Bound on angle difference between (i,j)
c_AngleDifferenceJI(i,j)$isLine(i,j)..
V_Theta(i)-V_Theta(j) =l= pi/3;

* Objective functions and pwl costs are listed in a separate file
$batinclude %MODELPATH%cost_objective.gms obj demandbids

*===== SECTION: VARIABLE BOUNDS
* Generator power generation limits
V_P.lo(gen)$status(gen) = Pmin(gen);
V_P.up(gen)$status(gen) = Pmax(gen);
V_P.fx(gen)$(not status(gen)) = 0;
$ifthen %wind%==1
* Needed to avoid compilation error. Puts strings into UEL
set winddesc /'PrimeMover', 'pm_WT'/;
* Wind turbines are not reliable sources of power, treated differently
parameter windTurbine(gen);
windTurbine(gen)$(geninfo(gen, 'PrimeMover', 'pm_WT') eq 1) = 1;
V_P.fx(gen)$(windTurbine(gen)) = 0;
$endif

* Line real power flow limits
V_LineP.up(i,j,c)$(branchstatus(i,j,c)) =  rateA(i,j,c);
V_LineP.lo(i,j,c)$(branchstatus(i,j,c)) = -rateA(i,j,c);

* Set bounds and fix reference bus angle
*V_Theta.fx(bus)$(type(bus) eq 3) = Va(bus);
V_Theta.fx(bus)$(type(bus) eq 3) = 0;

*--- Elastic demand
* Elastic demand not considered
V_Pd_elastic.fx(demandbid) = 0;
V_demandbid_rev.fx(demandbid) = 0;

*===== SECTION: VARIABLE STARTING LEVELS
* Gen solution
V_P.l(gen) = Pg(gen);

* DCOPF assumes voltage magnitude is 1
Vm(bus) = 1;

* Voltage angle solution
V_Theta.l(bus) = Va(bus);

*===== SECTION: MODEL DEFINITION
* Feasibility model
model feas /c_LinePowerDef, c_BalanceP, c_InterfaceP, c_InterfaceLimit,
       c_AngleDifferenceIJ, c_AngleDifferenceJI/;

*DCOPF model
model m_dcopf /feas, c_pw_cost, c_obj /;

*===== SECTION: MODEL OPTIONS AND SOLVE
* Type of solve
$iftheni.sol %obj% == "quad"
* QCP model
  solve m_dcopf min V_objcost using qcp;
$else.sol
* LP model
  solve m_dcopf min V_objcost using lp;
$endif.sol


*==== SECTION: Solution Analysis
* See if model is solved
parameter
    infeas "Number of infeasibilities from model solve";

infeas = m_dcopf.numInfes;
display infeas;

* Declaration needs to be made outside loop
set
    lines_at_limit(i,j,c) "lines at their bound"
;
parameters
    total_cost "Final objective value"
    LMP(bus) "Locational Marginal Price of bus at time t"
    LineSP(i,j,c) "Marginals of active power flowing on line (i,j,c)"
;

if(infeas eq 0,

* Final Objective function value
total_cost = V_objcost.l;
* Generator real power solution
Pg(gen) = V_P.l(gen);

* DCOPF assumes voltage magnitude is 1
Vm(bus) = 1;
* Voltage angle solution
Va(bus) = V_Theta.l(bus)/pi*180;
* Locational marginal price of bus at time t
LMP(bus) = c_BalanceP.m(bus);
* Marginal for active power on a line
LineSP(i,j,c)$branchstatus(i,j,c) = V_LineP.m(i,j,c);

* Find which lines are at their limits
lines_at_limit(i,j,c)$branchstatus(i,j,c) = yes$(abs(LineSP(i,j,c)) gt 1e-8);
display lines_at_limit;

*==== SECTION: Solution Save
$Set out %casename%_DC_base_solution.gdx
execute_unload 'temp_solution.gdx',  Pg, Vm, Va, total_cost, LMP, LineSP;
execute 'gams %MODELPATH%save_solution.gms gdxcompress=1 --ac=0 --case=%case% --solution=temp_solution.gdx --out=%out% --timeperiod=%timeperiod% --savesol=%savesol%';
if(errorlevel ne 0, abort "Saving solution failed!");
execute 'rm temp_solution.gdx'
);

$if set gmswebui $include %MODELPATH%webui_out.gms
$if set gmswebui $batinclude %MODELPATH%webui.gms