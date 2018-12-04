$title "Polar Ybus Power-Voltage P-Q decoupling"
*_______________________________________________________________________________
* Filename: ybus_polar_decoupled.gms
* Description: Decoupled polar AC optimal power flow model, ybus formulation
* 
* Usage: gams ybus_polar_decoupled.gms --case=/path/case.gdx
* 
* Options:
* --timeperiod: Select time period to solve. Default=1
* --obj: Objective function, piecewise linear or quadratic. Default="quad"
* --linelimits: Type of line limit data to use. Default="given"
* --ramprates: Type of ramprate data to use. Default="given"
* --genPmin: Data for Generator lower limit. Default="given"
* --allon: Option to turn on all gens or lines during solve. Default=none
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

* Default: timeperiod = 1
$if not set timeperiod $setGlobal timeperiod "1"
* Default: verbose=1
$if not set verbose $setGlobal verbose 1
* Default: allon=0
$if not set allon $setGlobal allon 0
* Default: Quadratic objective function
$if not set obj $setGlobal obj "quad"
* Default: Ignore D-curve constraints
$if not set qlim $setGlobal qlim 0
* Default: elastic demand bidding does not apply here
$set demandbids 0
* Default: Use provided line limits (as opposed to uwcalc)
$if not set linelimits $setGlobal linelimits "given"
* Default: Use provided generator lower limit
$if not set genPmin $setGlobal genPmin "given"
* Default: Save solution option turned off
$if not set savesol $setGlobal savesol 0

$set condensed yes

option decimals=8;

* Number of iterations
$if not set iter $set iter 1

* Define filepath, name and extension.
$setnames "%gams.i%" filepath filename fileextension
$setglobal MODELPATH '%filepath%..%system.dirsep%'

$if set webui $include ybus_polar_decoupled_webui_in.gms

* Define type of model
$set modeltype "AC"
* Define input case
$if not set case $abort "Model aborted. Please provide input case"
$setnames "%case%" casepath casename caseextension

*===== SECTION: EXTRACT DATA
$batinclude "%MODELPATH%extract_data.gms" case Plim limits allon timeperiod

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
$batinclude '%MODELPATH%calc_Ybus.gms'

* Set Starting Point for P-subproblem (Power Flow Problem)
$batinclude '%MODELPATH%calc_PowerFlow.gms'

*===== SECTION: VARIABLE DEFINITION
free variables
    V_P(gen)            "Real power generation of generator",
    V_Q(gen)            "Real power generation of generator",
    V_Theta(bus)        "Bus voltage angle",
    V_objcost "Objective cost"
    Loss
;

positive variables
    V_V(bus)            "Bus voltage magnitude",
    V_pw_cost(gen),
    V_shunt(bus,bus_s)
    V_Pd_elastic(demandbid)    "Elastic incremental demand"
    V_demandbid_rev(demandbid) "Revenue from elastic incremental demand"
;

*===== SECTION: EQUATION DEFINITION
equations
    c_S_Limitij(i,j,c)         "Limit apparent power on a line",
    c_S_Limitji(i,j,c)         "Limit apparent power on a line",
    c_BalanceP_decouple(bus)   "Power Flow Balance for Active",
    c_pw_cost(gen,costptset) "Generator piecewise cost"
    c_obj "Objective function" 

    c_BalanceQ_decouple(bus)   "Power Flow Balance for Reactive",
    Loss_objective
;

*===== SECTION: EQUATIONS PART 1
c_S_Limitij(i,j,c)$(branchstatus(i,j,c) and monitored_lines(i,j,c))..
    sqr(
        (g(i,j,c) * sqr(V_V(i)) / sqr(ratio(i,j,c)))
        - (V_V(i) * V_V(j) / ratio(i,j,c)) *
            (  g(i,j,c) * cos(V_Theta(i) - V_Theta(j) - angle(i,j,c))
             + b(i,j,c) * sin(V_Theta(i) - V_Theta(j) - angle(i,j,c)))
       )
    + sqr(
          - (sqr(V_V(i)) * (b(i,j,c) + bc(i,j,c)/2) / sqr(ratio(i,j,c)))
          - (V_V(i) * V_V(j) / ratio(i,j,c)) *
              (  g(i,j,c) * sin(V_Theta(i) - V_Theta(j) - angle(i,j,c))
               - b(i,j,c) * cos(V_Theta(i) - V_Theta(j) - angle(i,j,c)))
         )
    =l=
        sqr(rateA(i,j,c))
;

c_S_Limitji(i,j,c)$(branchstatus(j,i,c) and monitored_lines(j,i,c))..
    sqr(
        (g(i,j,c) * sqr(V_V(i)))
        - (V_V(i) * V_V(j) / ratio(i,j,c)) *
            (  g(i,j,c) * cos(V_Theta(i) - V_Theta(j) + angle(i,j,c))
             + b(i,j,c) * sin(V_Theta(i) - V_Theta(j) + angle(i,j,c)))
       )
    + sqr(
          - (sqr(V_V(i)) * (b(i,j,c) + bc(i,j,c)/2))
          - (V_V(i) * V_V(j) / ratio(i,j,c)) *
              (  g(i,j,c) * sin(V_Theta(i) - V_Theta(j) + angle(i,j,c))
               - b(i,j,c) * cos(V_Theta(i) - V_Theta(j) + angle(i,j,c)))
         )
    =l=
        sqr(rateA(i,j,c))
;

c_BalanceP_decouple(i)$(type(i) ne 4)..
          sum(gen$(atBus(gen,i) and status(gen)), V_P(gen)) - Pd(i)
            =e=
          V_V(i) * sum(j, V_V(j)*(yb(i,j,'real')*cos(V_Theta(i) - V_Theta(j))
                             +yb(i,j,'imag')*sin(V_Theta(i) - V_Theta(j))))
                 + sqr(V_V(i)) * Gs(i)
;

c_BalanceQ_decouple(i)$(type(i) ne 4)..
          sum(gen$(atBus(gen,i) and status(gen)), V_Q(gen)) - Qd(i)
            =e=
          V_V(i) * sum(j, V_V(j)*(yb(i,j,'real')*sin(V_Theta(i) - V_Theta(j))
                             -yb(i,j,'imag')*cos(V_Theta(i) - V_Theta(j))))
               - sqr(V_V(i)) * Bs(i)
               - sqr(V_V(i)) * sum(bus_s, Bswitched(i,bus_s) * V_shunt(i,bus_s))
;

Loss_objective..
         Loss =e= sum(i, V_V(i) * sum(j, V_V(j)*(yb(i,j,'real')*cos(V_Theta(i) - V_Theta(j))
                   + yb(i,j,'imag')*sin(V_Theta(i) - V_Theta(j)))) + sqr(V_V(i)) * Gs(i)) ;

* Objective functions and pwl costs are listed in a separate file
$batinclude "%MODELPATH%cost_objective.gms" obj demandbids

* D-curve limits
$if %Qlim% == 1 $batinclude '%MODELPATH%reactive_limits.gms' case

*===== SECTION: MODEL DEFINITION
model decoupled_P / c_S_Limitij, c_S_Limitji, c_BalanceP_decouple, c_pw_cost, c_obj  /;
model decoupled_Q / c_S_Limitij, c_S_Limitji, c_BalanceQ_decouple, Loss_objective /;

*===== SECTION: VARIABLE BOUNDS
shunt.up(bus,bus_s) = numBswitched(bus,bus_s);

* Set initial conditions
V_P.l(gen)$status(gen) = 1;
V_Theta.l(bus) = 1;
V_Q.l(gen)$status(gen) = 1;
V_V.l(bus) = 1 ;

* Bus shunt susceptance
$if %switchedshunts% == 0 V_shunt.fx(bus,bus_s) = V_shunt.up(bus,bus_s);

* Elastic demand not considered
V_Pd_elastic.fx(demandbid) = 0;
V_demandbid_rev.fx(demandbid) = 0;

*------ Part 1
* First Solve P-theta subproblem
* Fix voltage magnitudes as constant value and ingnore reactive power flow balance equations
* Then Solve Q-V subproblem
* Fix voltage angles as constant value and ingnore active power flow balance equations and minimize loss

set iteration / 1*%iter% /
loop(iteration,
* From flat starting point // V.fx(bus) = 1;
* From power flow starting point // V.fx(bus) = V_pf.l(bus) ;
* If we solve decouled OPF iteratively, after first iteration we use voltage magnitude starting point
* from Q-V subproblem;
V_V.fx(bus) = (1$(ord(iteration)=1) + V_V.l(bus)$(ord(iteration)>1));

* Set bounds
V_P.lo(gen)$status(gen) = Pmin(gen);
V_P.up(gen)$status(gen) = Pmax(gen);
*V_P.fx(gen)$(not status(gen)) = 0;

V_Theta.lo(bus) = -pi;
V_Theta.up(bus) =  pi;
V_Theta.fx(bus)$(type(bus) eq 3) = 0;

V_Theta.fx(bus)$(type(bus) eq 3) = 0;
solve decoupled_P min V_objcost using nlp;

if(decoupled_P.ModelStat > 2, abort "Optimal solution not found.";);

* Set bounds
V_Q.lo(gen)$status(gen) = Qmin(gen);
V_Q.up(gen)$status(gen) = Qmax(gen);
* V_Q.fx(gen)$(not status(gen)) = 0 ;

V_V.lo(bus) = MinVm(bus);
V_V.up(bus) = MaxVm(bus);

* Fix Voltage angles as constant from P_subproblem
V_Theta.fx(bus) = V_Theta.l(bus);

          solve decoupled_Q min Loss using nlp;
          if(decoupled_Q.ModelStat > 2,
          abort "Optimal solution not found.";);
);

parameters
    Power(i) "to calculate injection power from generator after Q-Vm subproblem"
;

Power(i)= Pd(i) + ( V_V.l(i) * sum(j, V_V.l(j)*(yb(i,j,'real')*cos(V_Theta.l(i) - V_Theta.l(j))
                                               + yb(i,j,'imag')*sin(V_Theta.l(i) - V_Theta.l(j))))
                                               + sqr(V_V.l(i)) * Gs(i) );

parameters
    total_cost,
    LMP(bus),
    LineSP(i,j,c)
;

total_cost = V_objcost.l;
Pg(gen) = sum(i$(atBus(gen,i) and status(gen)), Power(i));
Qg(gen) = V_Q.l(gen);
Vm(bus)  = V_V.l(bus);
Va(bus)  = V_Theta.l(bus);
LMP(bus) = c_BalanceP_decouple.m(bus);
LineSP(i,j,c)$branchstatus(i,j,c) = c_S_Limitij.m(i,j,c);
LineSP(j,i,c)$branchstatus(j,i,c) = c_S_Limitji.m(i,j,c);

*added to get results
parameters
    shuntB(i) "Bus shunt solution"
;
shuntB(i) = sum(bus_s, V_shunt.l(i,bus_s)*Bswitched(i,bus_s));   

$SetGlobal out %casename%_ybus_solution.gdx
execute_unload 'temp_solution.gdx' Pg, Qg, Vm, Va, total_cost, LMP, LineSP, shuntB;
execute 'gams %MODELPATH%save_solution.gms gdxcompress=1 --out=%out% --case=%case% --solution=temp_solution.gdx --timeperiod=%timeperiod% --savesol=%savesol%'
if(errorlevel ne 0, abort "Saving solution failed!");
execute 'rm temp_solution.gdx'


* Find which lines are at their limits
set lines_at_limit(i,j,c);

lines_at_limit(i,j,c)$(branchstatus(i,j,c) or branchstatus(j,i,c))
    = yes$(sqr(rateA(i,j,c)) - c_S_Limitij.l(i,j,c)  le 1e-4 );

display lines_at_limit;

$if set webui $include %MODELPATH%webui_out.gms
$if set webui $libinclude webui.gms
