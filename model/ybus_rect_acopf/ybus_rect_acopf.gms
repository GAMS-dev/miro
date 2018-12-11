$title "Rect Ybus Power-Voltage AC-OPF"
*_______________________________________________________________________________
* Filename: ybus_rect_acopf.gms
* Description: AC optimal power flow model, Ybus rect power-voltage formulation
*
* Usage: gams ybus_rect_acopf.gms --case=/path/case.gdx
*
* Options:
* --timeperiod: Select time period to solve. Default=1
* --obj: Objective function, piecewise linear or quadratic. Default="quad"
* --linelimits: Type of line limit data to use. Default="given"
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
* Turn of solution listing
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
* Default: allon=0
$if not set allon $set allon 0
* Default: Quadratic objective function
$if not set obj $set obj "quad"
* Default: Ignore D-curve constraints
$if not set qlim $set qlim 0
* Default: elastic demand bidding does not apply here
$set demandbids 0
* Default: Use provided line limits (as opposed to uwcalc)
$if not set linelimits $set linelimits "given"
* Default: Use provided generator lower limit
$if not set genPmin $set genPmin "given"
* Default: Save solution option turned off
$if not set savesol $set savesol 0

$set condensed 'no'

*===== SECTION: EXTRACT DATA
$batinclude "%MODELPATH%extract_data.gms" modeltype case timeperiod demandbids linelimits genPmin allon

* Calculate Ybus matrix
$batinclude '%MODELPATH%calc_Ybus.gms'

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

*===== SECTION: VARIABLE DEFINITION
free variables
    V_P(gen)            "Real power generation of generator"
    V_Q(gen)            "Reactive power generation of generator"

    V_real(i)          "Real part of bus voltage"
    V_imag(i)          "Imaginary part of bus voltage"

    V_LineP(i,j,c)       "Real power flowing from bus i towards bus j on line c"
    V_LineQ(i,j,c)       "Reactive power flowing from bus i towards bus j on line c"
    V_interfaceP(i,j,c) "Real power flowing on interface (i,j,c)"
;

positive variables
    V_shunt(bus,bus_s),

    V_pw_cost(gen)   "Generator piecewise cost"
    V_Pd_elastic(demandbid) "Elastic incremental demand"
    V_demandbid_rev(demandbid) "Revenue from elastic incremental demand"
;

free variable V_objcost  "Total cost of objective function";;

*===== SECTION: EQUATION DEFINITION
equations
    c_V_limit_lo(i)     "Limit voltage magnitude on a line"
    c_V_limit_up(i)     "Limit voltage magnitude on a line"

    c_LinePij(i,j,c)    "Real power flowing from bus i into bus j along line c"
    c_LinePji(i,j,c)    "Real power flowing from bus j into bus i along line c"
    c_LineQij(i,j,c)    "Reactive power flowing from bus i into bus j along line c"
    c_LineQji(i,j,c)    "Reactive power flowing from bus j into bus i along line c"
    c_BalanceP(bus) "Balance of real power for bus"
    c_BalanceQ(bus) "Balance of reactive power for bus"

    c_InterfaceP(i,j,c) "Definition of real power on interfaces involving (i,j,c) at time"
    c_InterfaceLimit(interface) "Limit of real power on interface at time t"

    c_pw_cost(gen,costptset) "Piecewise cost functions"
    c_obj "Objective function"
;

* Limit voltage magnitude on a line
c_V_limit_lo(i)..
    sqr(V_real(i)) + sqr(V_imag(i)) =g= sqr(minVm(i))
;

* Limit voltage magnitude on a line
c_V_limit_up(i)..
    sqr(V_real(i)) + sqr(V_imag(i)) =l= sqr(maxVm(i))
;

* Real power flowing from bus i into bus j along line c
c_LinePij(i,j,c)$(branchstatus(i,j,c))..
         V_LineP(i,j,c) =e=
            (g(i,j,c) / sqr(ratio(i,j,c)))
                * (sqr(V_real(i)) + sqr(V_imag(i)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(i)*V_real(j) + V_imag(i)*V_imag(j))
                   +(b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(j)*V_imag(i) - V_real(i)*V_imag(j)))
;

* Real power flowing from bus j into bus i along line c
c_LinePji(i,j,c)$(branchstatus(i,j,c))..
         V_LineP(j,i,c) =e=
            g(i,j,c)
                * (sqr(V_real(j)) + sqr(V_imag(j)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) + b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(j)*V_real(i) + V_imag(j)*V_imag(i))
                   +(b(i,j,c)*cos(angle(i,j,c)) - g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(i)*V_imag(j) - V_real(j)*V_imag(i)))
;

* Reactive power flowing from bus i into bus j along line c"
c_LineQij(i,j,c)$(branchstatus(i,j,c))..
         V_LineQ(i,j,c) =e=
            - ((b(i,j,c) + bc(i,j,c)/2) / sqr(ratio(i,j,c)))
                * (sqr(V_real(i)) + sqr(V_imag(i)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(j)*V_imag(i) - V_real(i)*V_imag(j))
                   -(b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(i)*V_real(j) + V_imag(i)*V_imag(j)))
;

* Reactive power flowing from bus j into bus i along line c"
c_LineQji(i,j,c)$(branchstatus(i,j,c))..
         V_LineQ(j,i,c) =e=
            - (b(i,j,c) + bc(i,j,c)/2)
                * (sqr(V_real(j)) + sqr(V_imag(j)))
            - (1 / ratio(i,j,c))
                * ( (g(i,j,c)*cos(angle(i,j,c)) + b(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(i)*V_imag(j) - V_real(j)*V_imag(i))
                   -(b(i,j,c)*cos(angle(i,j,c)) - g(i,j,c)*sin(angle(i,j,c)))
                        * (V_real(j)*V_real(i) + V_imag(j)*V_imag(i)))
;

* Active power node balance eqn
c_BalanceP(i)$(type(i) ne 4)..
          sum(gen$(atBus(gen,i) and status(gen)), V_P(gen))
        - Pd(i)
            =e=
          V_real(i)*sum(j, yb(i,j,'real')*V_real(j) - yb(i,j,'imag')*V_imag(j))
        + V_imag(i)*sum(j, yb(i,j,'real')*V_imag(j) + yb(i,j,'imag')*V_real(j))
        + (sqr(V_real(i)) + sqr(V_imag(i))) * Gs(i)
;

* Reactive power node balance eqn
c_BalanceQ(i)$(type(i) ne 4)..
          sum(gen$(atBus(gen,i) and status(gen)), V_Q(gen))
        - Qd(i)
            =e=
        - V_real(i)*sum(j, yb(i,j,'real')*V_imag(j) + yb(i,j,'imag')*V_real(j))
        + V_imag(i)*sum(j, yb(i,j,'real')*V_real(j) - yb(i,j,'imag')*V_imag(j))
        - (sqr(V_real(i)) + sqr(V_imag(i))) * Bs(i)
        - (sqr(V_real(i)) + sqr(V_imag(i))) * sum(bus_s$(not sameas(bus_s,'given')), Bswitched(i,bus_s) * V_shunt(i,bus_s))
;


* Definition of real power on interfaces involving (i,j,c) at time t
* Since we only care about interfaces in the specified direction, we don't need abs(LinePower)
c_InterfaceP(i,j,c)$((branchstatus(i,j,c) or branchstatus(j,i,c))
    and (sum(interface$interfacemap(interface,i,j), 1) ge 1))..
V_interfaceP(i,j,c) =e= V_LineP(i,j,c);

* Limit of real power on interface at time t
c_InterfaceLimit(interface)..
sum((i,j,c)$(interfacemap(interface,i,j) and (branchstatus(i,j,c) or branchstatus(j,i,c))),
    V_interfaceP(i,j,c)) =l=  interfaceLimit(interface);

* Objective functions and pwl costs are listed in a separate file
$batinclude "%MODELPATH%cost_objective.gms" obj demandbids

* D-curve limits
$if %Qlim% == 1 $batinclude '%MODELPATH%reactive_limits.gms' case


*===== SECTION: MODEL DEFINITION
model feas / c_V_limit_lo, c_V_limit_up,
      c_LinePij, c_LinePji, c_LineQij, c_LineQji,
      c_BalanceP, c_BalanceQ, c_InterfaceP, c_InterfaceLimit,
    /;

model acopf /feas, c_pw_cost, c_obj /;

*===== SECTION: VARIABLE BOUNDS
* Generator active power generation limits
V_P.lo(gen)$status(gen) = Pmin(gen);
V_P.up(gen)$status(gen) = Pmax(gen);
V_P.fx(gen)$(not status(gen)) = 0;

* Generator reactive power generation limits
* Does not impose Qmax, Qmin limits when the D-curve contraint is applied
$ifthen %qlim% == 0
V_Q.lo(gen)$status(gen) = Qmin(gen);
V_Q.up(gen)$status(gen) = Qmax(gen);
$endif
V_Q.fx(gen)$(not status(gen)) = 0;

* Bus voltage magnitude limits
V_real.lo(bus) = -MaxVm(bus); V_real.up(bus) = MaxVm(bus);
V_imag.lo(bus) = -MaxVm(bus); V_imag.up(bus) = MaxVm(bus);
V_imag.fx(bus)$(type(bus) eq 3) = 0;

* Line real power flow limits
V_LineP.lo(i,j,c)$branchstatus(i,j,c) = -rateA(i,j,c);
V_LineP.up(i,j,c)$branchstatus(i,j,c) =  rateA(i,j,c);

* Bus shunt susceptance
V_shunt.up(bus,bus_s) = numBswitched(bus,bus_s);
$if %switchedshunts% == 0 V_shunt.fx(bus,bus_s) = shunt.up(bus,bus_s);

* Elastic demand not considered
V_Pd_elastic.fx(demandbid) = 0;
V_demandbid_rev.fx(demandbid) = 0;

*===== SECTION: VARIABLE INITIAL STARTING POINTS
V_shunt.l(bus,bus_s)  = 1;

* Set initial conditions
$ifthen %ic% == 1 $batinclude '%MODELPATH%ic_rect%sep%random_all.gms' condensed verbose
$elseif %ic% == 2 $batinclude '%MODELPATH%ic_rect%sep%flat.gms' condensed verbose
$elseif %ic% == 3 $batinclude '%MODELPATH%ic_rect%sep%random_v.gms' condensed verbose
$elseif %ic% == 4 $batinclude '%MODELPATH%ic_rect%sep%dcopf_pv.gms' limits condensed verbose allon obj Plim timeperiod
$elseif %ic% == 5 $batinclude '%MODELPATH%ic_rect%sep%dcopf_v.gms' limits condensed verbose allon obj Plim timeperiod
$elseif %ic% == 6 $batinclude '%MODELPATH%ic_rect%sep%decoupled.gms' condensed verbose
$elseif %ic% == 7 $batinclude '%MODELPATH%ic_rect%sep%dcopf_pv_loss ' condensed verbose
$elseif %ic% == 8 $batinclude '%MODELPATH%ic_rect%sep%matpower.gms' condensed verbose
$elseif %ic% == 9 $batinclude '%MODELPATH%ic_rect%sep%given.gms' condensed verbose
$else $batinclude '%MODELPATH%ic_rect%sep%default.gms' condensed verbose
$endif

*===== SECTION: MODEL OPTIONS AND SOLVE
*---- Basic options
solve acopf min V_objcost using nlp;


*==== SECTION: Solution Analysis
* See if model is solved
parameter
    infeas "Number of infeasibilities from model solve";

infeas = acopf.numInfes;
display infeas;


* Declaration needs to be made outside loop
set
    lines_at_limit(i,j,c) "lines at their bound"
;
parameters
    total_cost "Cost of objective function"
    LMP(bus) "Locational marginal price"
    LineSP(i,j,c) "Marginal price of active power on line (i,j,c)"
    shuntB(i)
;


if(infeas eq 0,
* Final Objective function value
    total_cost = V_objcost.l;
* Generator real power solution
    Pg(gen) = V_P.l(gen);
* Generator reactive power solution
    Qg(gen) = V_Q.l(gen);
* Voltage magnitude solution
    Vm(bus) = sqrt(sqr(V_real.l(bus)) + sqr(V_imag.l(bus)));
* Voltage angle solution
    loop(bus,
    if(V_real.l(bus) > 0,
        Va(bus) = arctan(V_imag.l(bus)/V_real.l(bus)) * 180/pi;
    else
        Va(bus) = arctan(V_imag.l(bus)/V_real.l(bus)) * 180/pi + 180;
    );
    );
* Bus shunt solution
shuntB(i) = sum(bus_s, V_shunt.l(i,bus_s)*Bswitched(i,bus_s));

* Locational marginal price of bus at time t
LMP(bus) = c_BalanceP.m(bus);

* Marginal for active power on a line
LineSP(i,j,c)$branchstatus(i,j,c) = V_LineP.m(i,j,c);
LineSP(j,i,c)$branchstatus(i,j,c) = V_LineP.m(j,i,c);

* Find which lines are at their limits
lines_at_limit(i,j,c)$(branchstatus(i,j,c) or branchstatus(j,i,c)) = yes$
     (sqr(rateA(i,j,c)) - sqr(V_LineP.L(i,j,c)) - sqr(V_LineQ.L(i,j,c)) le 0.0001);
display lines_at_limit;

$SetGlobal out %casename%_ybus_solution.gdx
execute_unload 'temp_solution.gdx', Pg, Qg, Vm, Va, shuntB, total_cost, LMP, LineSP;
execute 'gams %MODELPATH%save_solution.gms gdxcompress=1 --ac=1 --case=%case% --solution=temp_solution.gdx --timeperiod=%timeperiod%  --out=%out%';
if(errorlevel ne 0, abort "Saving solution failed!");
execute 'rm temp_solution.gdx'
);

$if set webui $include %MODELPATH%webui_out.gms
$if set webui $libinclude webui.gms
