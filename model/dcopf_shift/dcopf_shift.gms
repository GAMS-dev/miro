$title "Shift Factor DC Optimal Power Flow model"
*_______________________________________________________________________________
* Filename: dcopf_shift.gms
* Description: DC Optimal Power Flow model using Shift matrices
*
* Usage: gams dcopf_shift --case=/casepath/case.gdx
* REQUIRES: Shift matrix file, located at casepath/case_Shift_Matrix.gdx
*           See datarchive/calc_S_matrix.gms to generate Shift matrix
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

* Define filepath, name and extension.
*$setnames "%gams.i%" filepath filename fileextension
$set MODELPATH '%gams.idir1%..%system.dirsep%'
$if set webui $include %MODELPATH%webui_in.gms
* Define type of model
$set modeltype "DC"
* Define input case
$if not set case $abort "Model aborted. Please provide input case"
$setnames "%case%" casepath casename caseextension

* Default: timeperiod = 1
$if not set timeperiod $set timeperiod "1"
* Default: allon=0
$if not set allon $set allon 0
* Default: Quadratic objective function
$if not set obj $set obj "quad"
* Default: elastic demand bidding does not apply here
$set demandbids 0
* Default: Use provided line limits (as opposed to uwcalc)
$if not set linelimits $set linelimits "given"
* Default: Use provided generator lower limit
$if not set genPmin $set genPmin "given"
* Default: Lineloss not approximated
$if not set lineloss $set lineloss 0
* Default: Save solution option turned off
$if not set savesol $set savesol 0

*===== SECTION: EXTRACT DATA
$batinclude %MODELPATH%extract_data.gms modeltype case timeperiod demandbids linelimits genPmin allon

$ifthen not exist %casepath%%casename%_Shift_Matrix%caseextension%
$call 'gams %MODELPATH%..%system.dirsep%tools%system.dirsep%DataUtilities%system.dirsep%calc_S_matrix.gms --case=%case%';
if(errorlevel ne 0, abort "Calculating Shift matrix failed!");
$endif


parameter S "Shift matrix information";
$GDXIN '%casepath%%casename%_Shift_Matrix%caseextension%'
$LOAD S
$GDXIN

*===== SECTION: DATA MANIPULATION
* DCOPF assumes reactive power = 0
B(i,j,c)$line(i,j,c) = -1/x(i,j,c);

sets
    isLine(i,j) "Reduced dimension of branchstatus(i,j,c)"
    hasTap(i,j,c) "Line has a tap transformer"
;
isLine(i,j)$(sum(c$branchstatus(i,j,c),1) > 0) = yes;
hasTap(i,j,c)$(branchstatus(i,j,c) and angle(i,j,c)) = yes;

* Approximate line loss
$ifthene.loss %lineloss%=0 
$elseife.loss %lineloss%>=1 Pd(i)=Pd(i)*%lineloss%;
$else.loss  $abort "Invalid lineloss"
$endif.loss

* Additional set alias relationships
alias(bus,k);
alias(c,c1);

*===== SECTION: VARIABLE DEFINITION
free variables
    V_P(gen)             "Real power generation of generator",
    V_LineP(i,j,c)   "Real power flowing from bus i towards bus j on line c"
;

positive variables
    V_interfaceP(i,j,c) "Real power flow on interface (i,j,c)"
    V_pw_cost(gen)  "Piecewise linear cost of generation at time t"
    V_Pd_elastic(demandbid) "Elastic incremental demand"
    V_demandbid_rev(demandbid) "Revenue from elastic incremental demand"
;

free variable V_objcost  "Total cost of objective function";

*===== SECTION: EQUATION DEFINITION
equations
    c_LinePower(i,j,c)  "Definition of real power on line (i,j,c)"
    c_balanceP "Balance of real power for bus"
    c_InterfaceP(i,j,c) "Definition of real power on interfaces involving (i,j,c)"
    c_InterfaceLimit(interface) "Limit of real power on interface"
    c_pw_cost(gen,costptset) "Generator piecewise cost functions"
    c_obj  "Objective function"
;

*===== SECTION: EQUATIONS PART 1
c_LinePower(i,j,c)$(branchstatus(i,j,c))..
V_LineP(i,j,c)  =e=
        sum(bus, S(i,j,c,bus)* (sum(gen$(atBus(gen,bus) and status(gen)), V_P(gen)) - Pd(bus) - Gs(bus)
	- sum((k,c1)$hasTap(bus,k,c1),B(bus,k,c1)*angle(bus,k,c1)/ratio(bus,k,c1))
	+ sum((k,c1)$hasTap(k,bus,c1),B(k,bus,c1)*angle(k,bus,c1)/ratio(k,bus,c1))
    	))
    + B(i,j,c)*angle(i,j,c)/ratio(i,j,c)
;

*Balance of real power for bus
c_balanceP..
    sum(gen, V_P(gen))- sum(bus, Pd(bus) + Gs(bus)) =e= 0
;

* Definition of real power on interfaces involving (i,j,c) at time t
* Since we only care about interfaces in the specified direction, we don't need abs(LinePower)
c_InterfaceP(i,j,c)$((branchstatus(i,j,c) or branchstatus(j,i,c))
    and (sum(interface$interfacemap(interface,i,j), 1) ge 1))..
V_interfaceP(i,j,c) =e= V_LineP(i,j,c);

* Limit of real power on interface at time t
c_InterfaceLimit(interface)..
sum((i,j,c)$interfacemap(interface,i,j), V_interfaceP(i,j,c)) =l=  interfaceLimit(interface);

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

*--- Elastic demand
* Elastic demand not considerd in this model
V_Pd_elastic.fx(demandbid) = 0;
V_demandbid_rev.fx(demandbid) = 0;

*===== SECTION: VARIABLE STARTING LEVELS
* Gen solution
V_P.l(gen) = Pg(gen);

*===== SECTION: MODEL DEFINITION
model feas /c_LinePower, c_balanceP, c_InterfaceP, c_InterfaceLimit/;
model m_dcopf /feas, c_pw_cost, c_obj/;

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
    lambda
    LMP(bus) 
    LMP_Energy(bus)
    LMP_Loss(bus)
    LMP_Congestion(bus)
    LossSensitivity(i,j)
    P_Loss
    total_cost
    LineSP(i,j,c)
;

alias(bus,l);

if(infeas eq 0,

* Final Objective function value
total_cost = V_objcost.l;
* Generator real power solution
Pg(gen) = V_P.l(gen);

* DCOPF assumes voltage magnitude is 1, and shift factor form doesn't consider voltage angle
Vm(bus) = 1;
Va(bus) = 0;

* Marginal for active power on a line
LineSP(i,j,c)$branchstatus(i,j,c) = V_LineP.m(i,j,c);

* Find which lines are at their limits
lines_at_limit(i,j,c)$branchstatus(i,j,c) = yes$(abs(LineSP(i,j,c)) gt 1e-8);

* Information specific to this formulation
lambda = c_balanceP.m;

*--- Placeholder: This is incorrect.
LMP(bus) = 0;
$ontext
LossSensitivity(i,j)$(sum(gen$(atBus(gen,j) and status(gen)),1) > 0 ) = sum((k,l,c)$(branchstatus(k,l,c)),
                                  r(k,l,c) * S(k,l,c,i) * S(k,l,c,j));

LMP_Energy(bus) = lambda;
LMP_Loss(bus) = -lambda * sum(i, LossSensitivity(bus,i) * sum(gen$(atBus(gen,i) and status(gen)), V_P.l(gen)));
LMP_Congestion(bus) = sum((i,j,c)$(branchstatus(i,j,c)),
                                 S(i,j,c,bus) * c_LinePower.m(i,j,c));
LMP(bus) = LMP_Energy(bus) + LMP_Loss(bus) + LMP_Congestion(bus);

display LMP_Energy, LMP_Congestion, LMP_Loss, LMP;
$offtext

display lines_at_limit;

$Set out %casename%_DC_shift_solution.gdx
execute_unload 'temp_solution.gdx', Pg, Vm, Va, total_cost, LMP_Energy, LMP_Loss, LMP_Congestion, LMP, LineSP;
execute 'gams %MODELPATH%save_solution.gms gdxcompress=1 --ac=0 --decompose_lmp=1 --case=%case% --solution=temp_solution.gdx --out=%casename%_DC_shift_solution.gdx --timeperiod=%timeperiod%';
if(errorlevel ne 0, abort "Saving solution failed!");
execute 'rm temp_solution.gdx'
);

$if set webui $include %MODELPATH%webui_out.gms
$if set webui $libinclude webui.gms