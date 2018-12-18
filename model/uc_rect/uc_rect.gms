$title "AC Optimal Power Flow with unit commitment"
*_______________________________________________________________________________
* Filename: uc_rect.gms
* Description: Rect power-voltage AC Optimal Power Flow model with unit commitment
* 
* Usage: gams uc_rect --case=/path/case.gdx
* 
* Options:
* --verbose: Supresses printout(0). Default=1
* --obj: Objective function, piecewise linear or quadratic. Default="pwl"
* --times: Select timeframe(s) to solve. Default provided by input file
* --demandbids: Elastic demand bidding turned on(1) or off(0). Default=0
* --linelimits: Type of line limit data to use. Default="given"
* --ramprates: Type of ramprate data to use. Default="given"
* --genPmin: Data for Generator lower limit. Default="given"
* --allon: Option to turn on all lines during solve. Default=none
* --slim: Option to turn on apparent power limits. Default=0
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
* Turn off solution printing
option solprint=off
option limrow=0, limcol=0
$endif

* Define filepath, name and extension.
*$setnames "%gams.i%" filepath filename fileextension
$set MODELPATH '%gams.idir1%..%system.dirsep%'
$if set webui $include "%MODELPATH%webui_in.gms"
* Define type of model
$set modeltype "AC"
* Define input case
$if not set case $abort "Model aborted. Please provide input case"
$setnames "%case%" casepath casename caseextension

* Default: Piecewise linear objective
$if not set obj $set obj "pwl"
* Default: elastic demand bidding turned off
$if not set demandbids $set demandbids 0
* Default: Use provided ramprates (as opposed to uwcalc)
$if not set ramprates $set ramprates "given"
* Default: Use provided line limits (as opposed to uwcalc)
$if not set linelimits $set linelimits "given"
* Default: Use provided generator lower limit
$if not set genPmin $set genPmin "given"
* Default: allon=0
$if not set allon $set allon 0
* Default: Use active power limits on line, instead of apparent limits
$if not set slim $set slim 0
* Default: Ignore D-curve constraints
$if not set qlim $set qlim 0
* Default: Relaxed MIP option turned off
$if not set relax $set relax 0
* Default: Save solution option turned off
$if not set savesol $set savesol 0

*===== SECTION: EXTRACT DATA
$batinclude "%MODELPATH%extract_data_uc.gms" modeltype case times demandbids linelimits ramprates genPmin allon

*===== SECTION: DATA MANIPULATION
* If no data is provided for a generator's minimum up and down time, set to 1
minuptime(gen) = max(1,minuptime(gen));
mindowntime(gen) = max(1,mindowntime(gen));

rampup(gen) = rampup(gen)*baseMVA;
rampdown(gen) = rampdown(gen)*baseMVA;

*--- Define load, gen buses and active lines
sets
    load(bus) "Load buses"
    isGen(bus) "Generator buses"
    isLine(i,j) "Active (i,j) line"
;

load(bus)$(sum(gen, atBus(gen,bus)) eq 0)  = 1;
isGen(bus)$(not(load(bus))) = 1;
option isLine < branchstatus;

*===== SECTION: VARIABLE DEFINITION
free variables
    V_P(gen,t)            "Real power generation of generator at time t"
    V_Q(gen,t)            "Reactive power generation of generator at time t"

    V_real(i,t)          "Real part of bus voltage"
    V_imag(i,t)          "Imaginary part of bus voltage"
    
    V_LineP(i,j,c,t)   "Real power flowing from bus i towards bus j on line c at time t"
    V_LineQ(i,j,c,t)   "Real power flowing from bus i towards bus j on line c at time t"
    V_interfaceP(i,j,c,t) "Real power flow on interface (i,j,c) at time t"
;

binary variables
    V_genstatus(gen,t)    "Generator commitment status for time t"
;

positive variables
    V_startup(gen,t) "(0,1) startup status of generator at time t"
    V_shutdown(gen,t) "(0,1) shutdown status of generator at time t"
    
    V_shunt(bus,bus_s,t)    "Bus shunt susceptance"
    V_V(bus,t)         "Voltage real part at bus at time t"
    
    V_pw_cost(gen,t) "Generator piecewise cost"
    V_Pd_elastic(demandbid,t) "Elastic incremental demand"
    V_demandbid_rev(demandbid,t) "Revenue from elastic incremental demand"
;

free variable V_objcost "Total cost of objective function";


*===== SECTION: EQUATION DEFINITION
equations
    c_SLimit(i,j,c,t)   "Apparent power limit on line ijc"
    c_V_limit_lo(i,t)     "Limit voltage magnitude on a line"
    c_V_limit_up(i,t)     "Limit voltage magnitude on a line"
    
    c_LinePij(i,j,c,t)    "Real power flowing from bus i into bus j along line c"
    c_LinePji(i,j,c,t)    "Real power flowing from bus j into bus i along line c"
    c_LineQij(i,j,c,t)    "Reactive power flowing from bus i into bus j along line c"
    c_LineQji(i,j,c,t)    "Reactive power flowing from bus j into bus i along line c"
    
    c_BalanceP(bus,t) "Balance of real power for bus at time t"
    c_BalanceQ(bus,t) "Balance of reactive power for bus at time t"

    c_GenStatusMin(gen,t) "Generator minimum operating capacity"
    c_GenStatusMax(gen,t) "Generator maximum operating capacity"
    c_GenStatusQMin(gen,t) "Generator minimum operating capacity"
    c_GenStatusQMax(gen,t) "Generator maximum operating capacity"
    c_StartupShutdown(gen,t) "Relationship of binary (start,shut,status) variables"
    c_MinUptime(gen,t) "Minimum generator run time"
    c_MinDowntime(gen,t) "Minimum generator down time"
    c_RampUp(gen,t) "Generator ramp up constraints"
    c_RampDown(gen,t) "Generator ramp down constraints"

    c_InterfaceP(i,j,c,t) "Definition of real power on interfaces involving (i,j,c) at time"
    c_InterfaceLimit(interface,t) "Limit of real power on interface at time t"

* If elastic bidding is turned on    
$if %demandbids% ==1 c_demandbid_revenue(demandbid,t,demandbid_s) "Revenue from elastic demand"

    c_pw_cost(gen,t,costptset)
    c_obj
;

*===== SECTION: EQUATIONS PART 1
* Apparent power limit on line ijc
c_SLimit(i,j,c,t)$(branchstatus(i,j,c,t) or branchstatus(j,i,c,t))..
sqr(V_LineP(i,j,c,t)) + sqr(V_LineQ(i,j,c,t)) =l= sqr(rateA(i,j,c));

* Limit voltage magnitude on a line
c_V_limit_lo(i,t)..
    sqr(V_real(i,t)) + sqr(V_imag(i,t)) =g= sqr(minVm(i))
;

* Limit voltage magnitude on a line
c_V_limit_up(i,t)..
    sqr(V_real(i,t)) + sqr(V_imag(i,t)) =l= sqr(maxVm(i))
;

*Real power flowing from bus i into bus j along line c
c_LinePij(i,j,c,t)$(branchstatus(i,j,c,t))..
         V_LineP(i,j,c,t) =e=
            (g(i,j,c) / sqr(ratio(i,j,c)))
                * (sqr(V_real(i,t)) + sqr(V_imag(i,t)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(i,t)*V_real(j,t) + V_imag(i,t)*V_imag(j,t))
                   +(b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(j,t)*V_imag(i,t) - V_real(i,t)*V_imag(j,t)))
;

*Real power flowing from bus j into bus i along line c
c_LinePji(i,j,c,t)$(branchstatus(i,j,c,t))..
         V_LineP(j,i,c,t) =e=
            g(i,j,c) * (sqr(V_real(j,t)) + sqr(V_imag(j,t)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) + b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(j,t)*V_real(i,t) + V_imag(j,t)*V_imag(i,t))
                   +(b(i,j,c)*cos(angle(i,j,c)) - g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(i,t)*V_imag(j,t) - V_real(j,t)*V_imag(i,t)))
;

*Reactive power flowing from bus i into bus j along line c
c_LineQij(i,j,c,t)$(branchstatus(i,j,c,t))..
         V_LineQ(i,j,c,t) =e=
            - ((b(i,j,c) + bc(i,j,c)/2) / sqr(ratio(i,j,c)))
                * (sqr(V_real(i,t)) + sqr(V_imag(i,t)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(j,t)*V_imag(i,t) - V_real(i,t)*V_imag(j,t))
                   -(b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(i,t)*V_real(j,t) + V_imag(i,t)*V_imag(j,t)))
;

*Reactive power flowing from bus j into bus i along line c
c_LineQji(i,j,c,t)$(branchstatus(i,j,c,t))..
         V_LineQ(j,i,c,t) =e=
            - (b(i,j,c) + bc(i,j,c)/2)
                * (sqr(V_real(j,t)) + sqr(V_imag(j,t)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) + b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(i,t)*V_imag(j,t) - V_real(j,t)*V_imag(i,t))
                   -(b(i,j,c)*cos(angle(i,j,c)) - g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(j,t)*V_real(i,t) + V_imag(j,t)*V_imag(i,t)))
;


* Active power node balance eqn
c_BalanceP(i,t)$(type(i) ne 4)..
          sum(gen$(atBus(gen,i) and status(gen,t)), V_P(gen,t))
          - Pd(i,t)
	  - sum(demandbid$demandbidmap(demandbid,i), V_Pd_elastic(demandbid,t))
            =e=
	  sum((j,c)$(branchstatus(i,j,c,t)), V_LineP(i,j,c,t))
	  + sum((j,c)$(branchstatus(j,i,c,t)), V_LineP(i,j,c,t))
          + sqr(V_V(i,t)) * Gs(i)
;

* Reactive power node balance eqn
c_BalanceQ(i,t)$(type(i) ne 4)..
          sum(gen$(atBus(gen,i) and status(gen,t)), V_Q(gen,t))
          - Qd(i,t)
            =e=
	   sum((j,c)$(branchstatus(i,j,c,t)), V_LineQ(i,j,c,t))
	  + sum((j,c)$(branchstatus(j,i,c,t)), V_LineQ(i,j,c,t))
          - sqr(V_V(i,t)) * Bs(i)
          - sqr(V_V(i,t)) * sum(bus_s$(not sameas(bus_s,'given')), bswitched(i,bus_s) * V_shunt(i,bus_s,t))
;


* Definition of real power on interfaces involving (i,j,c) at time t
* Since we only care about interfaces in the specified direction, we don't need abs(LinePower)
c_InterfaceP(i,j,c,t)$((branchstatus(i,j,c,t) or branchstatus(j,i,c,t))
    and (sum(interface$interfacemap(interface,i,j), 1) ge 1))..
V_interfaceP(i,j,c,t) =e= V_LineP(i,j,c,t);

* Limit of real power on interface at time t
c_InterfaceLimit(interface,t)..
sum((i,j,c)$(interfacemap(interface,i,j) and (branchstatus(i,j,c,t) or branchstatus(j,i,c,t))),
    V_interfaceP(i,j,c,t)) =l=  interfaceLimit(interface,t);

* Generator minimum operating capacity
c_GenStatusMin(gen,t)..
    V_genstatus(gen,t) * Pmin(gen) =l= V_P(gen,t)
;

* Generator maximum operating capacity
c_GenStatusMax(gen,t)..
    V_P(gen,t) =l= V_genstatus(gen,t) * Pmax(gen)
;

* Generator minimum operating capacity
c_GenStatusQMin(gen,t)..
    V_genstatus(gen,t) * Qmin(gen) =l= V_Q(gen,t)
;

* Generator maximum operating capacity
c_GenStatusQMax(gen,t)..
    V_Q(gen,t) =l= V_genstatus(gen,t) * Qmax(gen)
;

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
$batinclude "%MODELPATH%cost_objective_uc.gms" obj

*===== SECTION: MODEL DEFINITION
model feas /c_V_limit_lo, c_V_limit_up,
	    c_LinePij, c_LinePji, c_LineQij, c_LineQji,
            c_BalanceP, c_BalanceQ, c_GenStatusMin, c_GenStatusMax, c_GenStatusQMin, c_GenStatusQMax,
	    c_StartupShutdown, c_MinUptime, c_MinDowntime, c_RampUp, c_RampDown,
	    c_InterfaceP, c_InterfaceLimit
$if %slim% == 1 , c_SLimit
$if %demandbids% == 1 , c_demandbid_revenue
      /;
model uc_ac /feas, c_pw_cost, c_obj/;

*===== SECTION: VARIABLE BOUNDS
* Generator active power generation limits
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
* Generator reactive power generation limits
V_Q.lo(gen,t) = min(Qmin(gen),0);
V_Q.up(gen,t) = Qmax(gen);

* Bus voltage magnitude limits
V_real.lo(bus,t) = -MaxVm(bus);V_real.up(bus,t) = MaxVm(bus);
V_imag.lo(bus,t) = -MaxVm(bus);V_imag.up(bus,t) = MaxVm(bus);
V_imag.fx(bus,t)$(type(bus) eq 3) = 0;

$ifthene %slim%<>1
* Line real power flow limits
V_LineP.lo(i,j,c,t)$branchstatus(i,j,c,t) = -rateA(i,j,c);
V_LineP.up(i,j,c,t)$branchstatus(i,j,c,t) =  rateA(i,j,c);
V_LineP.lo(j,i,c,t)$(branchstatus(i,j,c,t)) = -rateA(i,j,c);
V_LineP.up(j,i,c,t)$(branchstatus(i,j,c,t)) =  rateA(i,j,c);
$endif

* Bus shunt susceptance
V_shunt.up(bus,bus_s,t) = numBswitched(bus,bus_s);
$if %switchedshunts% == 0 V_shunt.fx(bus,bus_s,t) = V_shunt.up(bus,bus_s,t);

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


*===== SECTION: VARIABLE INITIAL STARTING POINTS
V_shunt.l(bus,bus_s,t)  = 1;
* Starting values may be provided in the data file
* Startup and shutdown variables are binary {0,1} via equations
V_genstatus.fx(gen,t) = status(gen,t);
V_genstatus.l(gen,t) = status(gen,t);
V_startup.l(gen,t)  = max(0, status(gen,t)-status(gen,t-1));
V_shutdown.l(gen,t) = max(0, status(gen,t-1)-status(gen,t));

V_P.l(gen,t) = Pg(gen,t);
V_Q.l(gen,t) = Qg(gen,t);
V_real.l(bus,t) = Vm(bus,t);
V_imag.l(bus,t) = Vm(bus,t);

* Derived variables
V_LineP.l(i,j,c,t)$branchstatus(i,j,c,t) =  (g(i,j,c) / sqr(ratio(i,j,c)))
                * (sqr(V_real.l(i,t)) + sqr(V_imag.l(i,t)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real.l(i,t)*V_real.l(j,t) + V_imag.l(i,t)*V_imag.l(j,t))
                   +(b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real.l(j,t)*V_imag.l(i,t) - V_real.l(i,t)*V_imag.l(j,t)));

V_LineP.l(j,i,c,t)$branchstatus(i,j,c,t) = g(i,j,c) * (sqr(V_real.l(j,t)) + sqr(V_imag.l(j,t)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) + b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real.l(j,t)*V_real.l(i,t) + V_imag.l(j,t)*V_imag.l(i,t))
                   +(b(i,j,c)*cos(angle(i,j,c)) - g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real.l(i,t)*V_imag.l(j,t) - V_real.l(j,t)*V_imag.l(i,t)));

V_LineQ.l(i,j,c,t)$branchstatus(i,j,c,t) = - ((b(i,j,c) + bc(i,j,c)/2) / sqr(ratio(i,j,c)))
                * (sqr(V_real.l(i,t)) + sqr(V_imag.l(i,t)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real.l(j,t)*V_imag.l(i,t) - V_real.l(i,t)*V_imag.l(j,t))
                   -(b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real.l(i,t)*V_real.l(j,t) + V_imag.l(i,t)*V_imag.l(j,t)));

V_LineQ.l(j,i,c,t)$branchstatus(i,j,c,t) = - (b(i,j,c) + bc(i,j,c)/2)
                * (sqr(V_real.l(j,t)) + sqr(V_imag.l(j,t)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) + b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real.l(i,t)*V_imag.l(j,t) - V_real.l(j,t)*V_imag.l(i,t))
                   -(b(i,j,c)*cos(angle(i,j,c)) - g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real.l(j,t)*V_real.l(i,t) + V_imag.l(j,t)*V_imag.l(i,t)));

V_interfaceP.l(i,j,c,t)$((branchstatus(i,j,c,t) or branchstatus(j,i,c,t))
    and (sum(interface$interfacemap(interface,i,j), 1) ge 1)) = V_LineP.l(i,j,c,t);

* Derived objective function
V_pw_cost.l(gen,t) = max(0,smax((costptset)$((ord(costptset) < numcostpts(gen)) and (costmodel(gen) eq 1)),
    ((costpts_y(gen,costptset+1) - costpts_y(gen,costptset))/
     (costpts_x(gen,costptset+1) - costpts_x(gen,costptset)))
      * (V_P.l(gen,t)*baseMVA - costpts_x(gen,costptset))
    + costpts_y(gen,costptset)*V_genstatus.l(gen,t)))
;


V_objcost.l = 
$iftheni.obj0L %obj% == 0
    0
$else.obj0L
    + sum((gen,t),V_startup.l(gen,t)*startupcost(gen) + V_shutdown.l(gen,t)*shutdowncost(gen))
$iftheni.solL %obj% == "pwl"
* Piecewise linear objective function
    + sum((gen,t)$(costmodel(gen) eq 1), V_pw_cost.l(gen,t))
$elseifi.solL %obj% == "quad"
* Quadratic objective function
    + sum((gen,t)$(costmodel(gen) eq 2),costcoef(gen,'0')*V_genstatus.l(gen,t)
	+ costcoef(gen,'1')*V_P.l(gen,t)*baseMVA
	+ costcoef(gen,'2')*sqr(V_P.l(gen,t)*baseMVA))
$elseifi.solL %obj% == "linear"
* Linear objective function
    + sum((gen,t)$(costmodel(gen) eq 2),costcoef(gen,'0')*V_genstatus.l(gen,t)
	+ costcoef(gen,'1')*V_P.l(gen,t)*baseMVA)
$endif.solL
$endif.obj0L
;

display rampup, rampdown, minuptime, mindowntime;

*===== SECTION: MODEL OPTIONS AND SOLVE
*---- Basic options
$ifthen.rminlp %relax% == 1
    solve uc_ac min V_objcost using rminlp;
$else.rminlp
    solve uc_ac min V_objcost using minlp;
$endif.rminlp



*==== SECTION: Solution Analysis
* See if model is solved
parameter
    infeas "Number of infeasibilities from model solve";

infeas = uc_ac.numInfes;
display infeas;

* Declaration needs to be made outside loop
set
    lines_at_limit(i,j,c,t) "lines at their bound"
;
parameters
    total_cost "Cost of objective function"
    LMP(bus,t) "Locational marginal price"
    LineSP(i,j,c,t) "Marginal price of active power on line (i,j,c)"
    shuntB(i,t)
;

if(infeas eq 0,
* Final Objective function value
    total_cost = V_objcost.l;
* Status information
    status(gen,t) = V_genstatus.l(gen,t);
* Generator real power solution
    Pg(gen,t) = V_P.l(gen,t);
* Generator reactive power solution
    Qg(gen,t) = V_Q.l(gen,t);
* Voltage magnitude solution
    Vm(bus,t)  = sqrt(sqr(V_real.l(bus,t)) + sqr(V_imag.l(bus,t)));
* Voltage angle solution
    Va(bus,t)$(V_real.l(bus,t) > 0) = arctan(V_imag.l(bus,t)/V_real.l(bus,t)) * 180/pi;
    Va(bus,t)$(V_real.l(bus,t) le 0) = arctan(V_imag.l(bus,t)/V_real.l(bus,t)) * 180/pi + 180;
* Bus shunt solution
    shuntB(i,t) = sum(bus_s, V_shunt.l(i,bus_s,t)*Bswitched(i,bus_s));
* Locational marginal price of bus at time t
    LMP(bus,t) = c_BalanceP.m(bus,t);
* Marginal for active power on a line
    LineSP(i,j,c,t)$branchstatus(i,j,c,t) = V_LineP.m(i,j,c,t)$(abs(V_LineP.m(i,j,c,t)) > Eps);
    LineSP(j,i,c,t)$branchstatus(i,j,c,t) = V_LineP.m(j,i,c,t)$(abs(V_LineP.m(j,i,c,t)) > Eps);

* Find which lines are at their limits
lines_at_limit(i,j,c,t)$branchstatus(i,j,c,t) = yes$(abs(LineSP(i,j,c,t)) gt 1e-8);
display lines_at_limit;

*==== SECTION: Solution Save
$SetGlobal out %casename%_AC_UC_solution.gdx
*added to execute_unload: t
execute_unload 'temp_solution.gdx', t, Pg, Qg, Vm, Va, shuntB, total_cost, LMP, LineSP, status;
execute 'gams "save_solution_uc.gms" idir="%MODELPATH%" gdxcompress=1 --ac=1 --uc=1 --timeperiod=%timeperiod% --case="%case%" --solution=temp_solution.gdx --out=%out% lo=3';
if(errorlevel ne 0, abort "Saving solution failed!");
execute 'rm temp_solution.gdx'
    
* END IF-loop if(infeas eq 0)
);

$if set webui $include "%MODELPATH%webui_out.gms"
$if set webui $libinclude webui.gms