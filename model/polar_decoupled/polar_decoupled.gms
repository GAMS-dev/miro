$title "AC optimal power flow model, polar power-voltage formulation"
*_______________________________________________________________________________
* Filename: polar_decoupled.gms
* Description: Decoupled AC optimal power flow model, polar power-voltage formulation
* 
* Usage: gams polar_decoupled.gms --case=/path/case.gdx
* 
* Options:
* --timeperiod: Select time period to solve. Default=1
* --obj: Objective function, piecewise linear or quadratic. Default="quad"
* --linelimits: Type of line limit data to use. Default="given"
* --genPmin: Data for Generator lower limit. Default="given"
* --allon: Option to turn on all gens or lines during solve. Default=none
* --qlim: Option to use D-curve constraints. Default=0 (not used)
* --savesol: Turn on save solution option(1). Default=0
* --verbose: Supresses printout(0). Default=1
*_______________________________________________________________________________

* System dependence
$if %system.filesys% == UNIX $set sep '/'
$if not %system.filesys% == UNIX $set sep '\'


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
$set modeltype "AC"
* Define input case
$if not set case $abort "Model aborted. Please provide input case"
$setnames "%case%" casepath casename caseextension

* Default: timeperiod = 1
$if not set timeperiod $set timeperiod "1"
* Default: Quadratic objective function
$if not set obj $set obj "quad"
* Default: Use provided line limits (as opposed to uwcalc)
$if not set linelimits $set linelimits "given"
* Default: Use provided generator lower limit
$if not set genPmin $set genPmin "given"
* Default: allon=0
$if not set allon $set allon 0
* Default: Option slim does not apply. Apparent limits always used
$set slim 0
* Default: Ignore D-curve constraints
$if not set qlim $set qlim 0
* Default: Save solution option turned off
$if not set savesol $set savesol 0
* Default: elastic demand bidding does not apply here
$set demandbids 0
$set condensed 'no'

*===== SECTION: EXTRACT DATA
$batinclude %MODELPATH%extract_data.gms modeltype case timeperiod demandbids linelimits genPmin allon

*===== SECTION: DATA MANIPULATION
*--- Define load, gen buses and active lines
sets
    load(bus) "Load buses"
    isGen(bus) "Generator buses"
    activeGen(bus) "Active generator buses"
    isLine(i,j) "Active (i,j) line"
;

load(bus)$(sum(gen, atBus(gen,bus)) eq 0)  = 1;
isGen(bus)$(not(load(bus))) = 1;
activeGen(bus)$(isGen(bus) and (sum(gen$atBus(gen,bus), status(gen))>0) ) = 1;
option isLine < branchstatus;

* Calculate Ybus matrix
$batinclude %MODELPATH%calc_Ybus.gms
* Set Starting Point for P-subproblem (Power Flow Problem)
$batinclude %MODELPATH%calc_PowerFlow.gms


*===== SECTION: VARIABLE DEFINITION
free variables
    V_P(gen)             "Real power generation of generator",
    V_Q(gen)             "Real power generation of generator",
    V_Theta(bus)         "Bus voltage angle",
    V_LineP(i,j,c)       "Real power flowing from bus i towards bus j on line c",
    V_LineQ(i,j,c)       "Reactive power flowing from bus i towards bus j on line c",
    V_objcost            "Objective function value"
    Loss
;
positive variables
    V_V(bus)             "Bus voltage magnitude",
    V_shunt(bus,bus_s)   "Bus shunt susceptance"
    V_pw_cost(gen)       "Generator piecewise cost"
    V_demandbid_rev(demandbid) "Revenue from elastic incremental demand"
;

*===== SECTION: EQUATION DEFINITION
equations
    c_S_Limit(i,j,c)     "Limit apparent power on a line",
    c_LinePij(i,j,c)     "Real power flowing from bus i into bus j along line c",
    c_LinePji(i,j,c)     "Real power flowing from bus j into bus i along line c",
    c_LineQij(i,j,c)     "Reactive power flowing from bus i into bus j along line c",
    c_LineQji(i,j,c)     "Reactive power flowing from bus j into bus i along line c",
    c_BalanceP_decouple(bus)
    c_BalanceQ_decouple(bus)
    c_pw_cost(gen,costptset)
    c_obj
    Loss_objective
;

c_S_Limit(i,j,c)$(branchstatus(i,j,c) or branchstatus(j,i,c))..
    sqr(V_LineP(i,j,c)) + sqr(V_LineQ(i,j,c)) =l= sqr(rateA(i,j,c))
;

c_LinePij(i,j,c)$(branchstatus(i,j,c))..
         V_LineP(i,j,c) =e=
            (g(i,j,c) * sqr(V_V(i)) / sqr(ratio(i,j,c)))
            - (V_V(i) * V_V(j) / ratio(i,j,c)) *
                (  g(i,j,c) * cos(V_Theta(i) - V_Theta(j) - angle(i,j,c))
                 + b(i,j,c) * sin(V_Theta(i) - V_Theta(j) - angle(i,j,c)))
;

c_LinePji(i,j,c)$(branchstatus(i,j,c))..
         V_LineP(j,i,c) =e=
           g(i,j,c) * sqr(V_V(j))
           - (V_V(i) * V_V(j) / ratio(i,j,c)) *
               (  g(i,j,c) * cos(V_Theta(j) - V_Theta(i) + angle(i,j,c))
                + b(i,j,c) * sin(V_Theta(j) - V_Theta(i) + angle(i,j,c)))
;

c_LineQij(i,j,c)$(branchstatus(i,j,c))..
         V_LineQ(i,j,c) =e=
            - (sqr(V_V(i)) * (b(i,j,c) + bc(i,j,c)/2) / sqr(ratio(i,j,c)))
            - (V_V(i) * V_V(j) / ratio(i,j,c)) *
                (  g(i,j,c) * sin(V_Theta(i) - V_Theta(j) - angle(i,j,c))
                 - b(i,j,c) * cos(V_Theta(i) - V_Theta(j) - angle(i,j,c)))
;

c_LineQji(i,j,c)$(branchstatus(i,j,c))..
         V_LineQ(j,i,c) =e=
            - (sqr(V_V(j)) * (b(i,j,c) + bc(i,j,c)/2))
            - (V_V(i) * V_V(j) / ratio(i,j,c)) *
                (  g(i,j,c) * sin(V_Theta(j) - V_Theta(i) + angle(i,j,c))
                 - b(i,j,c) * cos(V_Theta(j) - V_Theta(i) + angle(i,j,c)))
;

c_BalanceP_decouple(i)$(type(i) ne 4)..
          sum(gen$(atBus(gen,i) and status(gen)), V_P(gen))
          - Pd(i)
            =e=
          sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)), V_LineP(i,j,c))
          + sqr(V_V(i)) * Gs(i)
;

c_BalanceQ_decouple(i)$(type(i) ne 4)..
          sum(gen$(atBus(gen,i) and status(gen)), V_Q(gen))
          - Qd(i)
            =e=
          sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)), V_LineQ(i,j,c))
          - sqr(V_V(i)) * Bs(i)
          - sqr(V_V(i)) * sum(bus_s$(not sameas(bus_s,'given')), Bswitched(i,bus_s) * V_shunt(i,bus_s))
;

Loss_objective..
         Loss =e= sum(i, sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)), V_LineP(i,j,c))
                   + sqr(V_V(i)) * Gs(i))
;

* Objective functions and pwl costs are listed in a separate file
$batinclude %MODELPATH%cost_objective.gms obj

*===== SECTION: MODEL DEFINITION
model decoupled_P / c_S_Limit, c_LinePij, c_LinePji, c_LineQij, c_LineQji,
		    c_BalanceP_decouple, c_pw_cost, c_obj/;

model decoupled_Q / c_S_Limit, c_LinePij, c_LinePji, c_LineQij, c_LineQji,
		    c_BalanceQ_decouple, Loss_objective /;
*===== SECTION: VARIABLE BOUNDS
* Generator active power generation limits
V_P.lo(gen)$status(gen) = Pmin(gen);
V_P.up(gen)$status(gen) = Pmax(gen);
*V_P.fx(gen)$(not status(gen)) = 0;

* Fix swing bus angle
V_Theta.lo(bus) = -pi;
V_Theta.up(bus) =  pi;
V_Theta.fx(bus)$(type(bus) eq 3) = 0;

* Line real power flow limits
V_LineP.lo(i,j,c)$branchstatus(i,j,c) = -rateA(i,j,c);
V_LineP.up(i,j,c)$branchstatus(i,j,c) =  rateA(i,j,c);
V_LineQ.lo(i,j,c)$branchstatus(i,j,c) = -rateA(i,j,c);
V_LineQ.up(i,j,c)$branchstatus(i,j,c) =  rateA(i,j,c);

* First Solve P-theta subproblem
* Fix voltage magnitudes as constant value and ingnore reactive power flow balance equations

* Set initial conditions
V_P.l(gen)$status(gen) = 1;
V_Theta.l(bus) = 1;

V_shunt.up(bus,bus_s) = numBswitched(bus,bus_s);
$if %switchedshunts% == 0 V_shunt.fx(bus,bus_s) = V_shunt.up(bus,bus_s);
$if %qlim% == 1 $batinclude '%MODELPATH%reactive_limits.gms' case

*===== SECTION: VARIABLE INITIAL STARTING POINTS
* Set initial conditions
V_Q.l(gen)$status(gen) = 1;
V_V.l(bus) = 1;
V_shunt.l(bus,bus_s)  = 1;

*===== SECTION: MODEL OPTIONS AND SOLVE
* Model options
option decimals = 8;

*---------- Part 1
* Solve Q-V subproblem
* Fix voltage angles as constant value and ingnore active power flow balance equations
* and minimize loss

$if not set iter $set iter 1
set iteration / 1*%iter% /
loop(iteration,
* From flat starting point // V.fx(bus) = 1;
* From power flow starting point // V.fx(bus) = V_pf.l(bus) ;
* If we solve decouled OPF iteratively, after first iteration we use
* voltage magnitude starting point from Q-V subproblem;
 V_V.fx(bus) = (1$(ord(iteration)=1) + V_V.l(bus)$(ord(iteration)>1));
 solve decoupled_P min V_objcost using nlp;

 if(decoupled_P.ModelStat > 2, abort "Optimal solution not found.";);

* Reset bounds
V_Q.lo(gen)$status(gen) = Qmin(gen);
V_Q.up(gen)$status(gen) = Qmax(gen);
* Q.fx(gen)$(not status(gen)) = 0 ;

V_V.lo(bus) = MinVm(bus);
V_V.up(bus) = MaxVm(bus);

V_LineP.lo(i,j,c)$branchstatus(i,j,c) = -rateA(i,j,c);
V_LineP.up(i,j,c)$branchstatus(i,j,c) =  rateA(i,j,c);
V_LineQ.lo(i,j,c)$branchstatus(i,j,c) = -rateA(i,j,c);
V_LineQ.up(i,j,c)$branchstatus(i,j,c) =  rateA(i,j,c);

* Fix Voltage angles as constant from P_subproblem
V_theta.fx(bus) = V_Theta.l(bus);

solve decoupled_Q min Loss using nlp;
if(decoupled_Q.ModelStat > 2, abort "Optimal solution not found.";);

);

*==== SECTION: Solution Analysis
* See if model is solved
parameter
    infeas "Number of infeasibilities from model solve";

infeas = decoupled_Q.numInfes;
display infeas;

* Declaration needs to be made outside loop
set lines_at_limit(i,j,c);
parameters
    Power(i) "to calculate injection power from generator after Q-Vm subproble"
    total_cost "Cost of objective function"
    LMP(bus) "Locational marginal price"
    LineSP(i,j,c) "Marginal price of active power on line (i,j,c)"
    shunt_solved(bus,bus_s)
    shuntB(i)
;

if(infeas eq 0,
    
Power(i)= Pd(i) + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)), V_LineP.l(i,j,c))
          + sqr(V_V.l(i)) * Gs(i);

* Final Objective function value
total_cost = V_objcost.l;
* Generator real power solution
Pg(gen) = sum(i$(atBus(gen,i) and status(gen)), Power(i));
* Bus shunt solution
shunt_solved(bus,bus_s) = V_shunt.L(bus,bus_s);
* Bus shunt solution
    shuntB(i) = sum(bus_s, V_shunt.l(i,bus_s)*Bswitched(i,bus_s));
* Generator reactive power solution
Qg(gen) = V_Q.l(gen);
* Voltage magnitude solution
Vm(bus)  = V_V.l(bus);
* Voltage angle solution 
Va(bus)  = V_Theta.l(bus);
* Locational marginal price of bus at time t
LMP(bus) = c_BalanceP_decouple.m(bus);
* Marginal for active power on a line
LineSP(i,j,c)$branchstatus(i,j,c) = c_S_Limit.m(i,j,c);
LineSP(j,i,c)$branchstatus(i,j,c) = c_S_Limit.m(j,i,c);


* Find which lines are at their limits
lines_at_limit(i,j,c)$branchstatus(i,j,c) = yes$(abs(LineSP(i,j,c)) gt 1e-8);
display lines_at_limit;

*==== SECTION: Solution Save
$SetGlobal out %casename%_AC_base_solution.gdx
execute_unload 'temp_solution.gdx', Pg, Qg, Vm, Va, shuntB, total_cost, LMP, LineSP;
execute 'gams %MODELPATH%save_solution.gms gdxcompress=1 --ac=1 --case=%case% --solution=temp_solution.gdx --out=%out% --timeperiod=%timeperiod%';
if(errorlevel ne 0, abort "Saving solution failed!");
execute 'rm temp_solution.gdx'
);

$if set webui $include %MODELPATH%webui_out.gms
$if set webui $libinclude webui.gms
