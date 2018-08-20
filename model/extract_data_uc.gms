*==== SECTION: Data (pre) declaration
sets
* New sets  
  conj /real,imag/
  costcoefset "Placeholder set for quadratic function coefficients (0,1,2)" /0*2/ 
  costptset "Placeholder set for pwl function pieces" /1*40/

* Based on %times% option. See LOADR t below for default option if %times% not set
$if set times t /%times%/,
$if not set times t,

* Dataset sets
  bus, gen, circuit,
  interface, interfacemap (interface,bus,bus),
  fuel_t, fuel_s, prime_mover,
  bus_t, bus_s,
  gen_t, gen_s,
  branch_t, branch_s,
    
  interface_t,
  line(bus,bus,circuit),
  transformer(bus,bus,circuit),
  monitored_lines(bus,bus,circuit)
;

*-- Aliases
alias(bus,i,j);
alias(circuit,c);
alias(t,t1);
alias (gen,gen1);

parameters
    version, baseMVA, total_cost,
* Domain info not stated because of how we iterate through data
    businfo(bus,*,*),
    geninfo(gen,*,*),
    fuelinfo(*,*),
    branchinfo(*,*,*,*,*),
    interfaceinfo(*,*,*)
;

*-- Specially handled data (option specific)
$onempty
sets
  demandbid_t(*) / /
  demandbid_s(*) / /
  demandbid(*) / /
  demandbidmap (*,*) / /
;

parameters
  demandbidinfo(*,*,*,*) / /
;    
$offempty


*==== SECTION: Data read-in from input file
$GDXIN %case%
$LOAD version, baseMVA, total_cost

* If %times% not provided, use default in dataset
$if not set times $LOADR t

$LOAD bus, gen, circuit, line, transformer, monitored_lines
$LOAD bus_t, bus_s, gen_t, gen_s, branch_t, branch_s
$LOAD fuel_t, fuel_s, prime_mover
$LOAD businfo, geninfo, branchinfo, fuelinfo
$LOAD interface, interfacemap, interfaceinfo, interface_t

* Option to use elastic demand bidding turned on
$if %demandbids% == 1 $LOADR demandbid_t, demandbid_s, demandbid, demandbidmap, demandbidinfo
$GDXIN

*==== SECTION: Validity of options
* linelimits, case insensitive
$iftheni %linelimits% == "inf"
$elseifi %linelimits% == "uwcalc"
$elseifi %linelimits% == "given"
$else $abort "Fix invalid option: --linelimit=%linelimits%"
$endif

* ramprates, case insensitive
$iftheni %ramprates% == "given"
$elseifi %ramprates% == "uwcalc"
$else $abort "Fix invalid option: --ramprates=%ramprates%"
$endif

* genPmin, case insensitive
$iftheni %genPmin% == "0"
$elseifi %genPmin% == "uwcalc"
$elseifi %genPmin% == "given"
$else $abort "Fix invalid option: --genPmin=%genPmin%"
$endif

* allon, case insensitive
$iftheni %allon% == "lines"
$elseif %allon% == 0
$else $if set allon $abort "Fix invalid option: --allon=%allon%"
$endif

*==== SECTION: Data Declaration (extracted/manipulated from datafile)
*-- All OPF models
parameters
  type(bus)          "bus type (probably irrelevant, but gives reference bus[es])"
  pf(bus)            "bus demand power factor"
  Pd(bus,t)          "bus real power demand"

  Pg(gen,t)          "gen real power output"
  Pmax(gen)          "gen maximum real power output"
  Pmin(gen)          "gen minimum real power output"
  Va(bus,t)          "bus voltage angle"

  Vm(bus,t)          "bus voltage magnitude"
  MaxVm(bus) "maximum bus voltage magnitude"
  MinVm(bus) "minimum bus voltage magnitude"
  Gs(bus)            "bus shunt conductance"

  rampup(gen)        "increasing ramp rate of generator"
  rampdown(gen)      "decreasing ramp rate of generator"
  atBus(gen,bus)     "Location of generator"
  status(gen,t)      "generator status"
  
  costcoef           "gen cost coefficients"
  costpts_x          "gen cost breakpoints (piecewise linear)"
  costpts_y          "gen cost breakpoints (piecewise linear)"

  costmodel(gen)     "gen cost model type"
  numcostpts(gen)    "gen cost number of piecewise points"
  numcostcoef(gen)   "gen cost number of coefficients"
  noloadcost(gen)    "generator no load operating cost for piecewise cost functions"
  startupcost(gen)   "generator startup cost"
  shutdowncost(gen)  "generator shutdown cost"
  minuptime(gen)     "generator minimum time to remain on"
  mindowntime(gen)   "generator minimum time to remain off"
  
  r(i,j,c)           "line resistance",
  x(i,j,c)           "line reactance",
  B(i,j,c)           "line susceptance",
  ratio(i,j,c)       "transformer tap ratio",
  angle(i,j,c)       "transformer tap angle",
  rateA(i,j,c)       "line power limits (MW)",
  currentrate(i,j,c)  "line current limits",
  branchstatus(i,j,c,t) "line status",
  interfaceLimit(interface,t) "Limit on power across each interface"
;

* Bus type
type(bus)  = businfo(bus,'type','given');
* Power factor
pf(bus)    = businfo(bus,'pf','given');
* Bus demand (real power)
Pd(bus,t)  = businfo(bus,'Pd',t)/baseMVA;

* Bus shunt conductance
Gs(bus)    = businfo(bus,'Gs','given')/baseMVA;

atBus(gen,bus)$geninfo(gen,'atBus',bus) = 1;
Pg(gen,t) = geninfo(gen,'Pg',t)/baseMVA;

* Maximum power generation
Pmax(gen) = geninfo(gen,'Pmax','given')/baseMVA;

* Minimum power generation options
$ifthen %genPmin% == 0
  Pmin(gen) = 0;
$else
  Pmin(gen)= geninfo(gen,'Pmin','%genPmin%')/baseMVA;
$endif

* Voltage angle
Va(bus,t)  = businfo(bus,'Va',t)*pi/180;
* Voltage  magnitude information
Vm(bus,t)  = businfo(bus,'Vm',t);
maxVm(bus) = businfo(bus,'maxVm','given');
minVm(bus) = businfo(bus,'minVm','given');

* Initial generator commitment
status(gen,t) = geninfo(gen,'status',t);

* Initial branch status (active/not connected)
branchstatus(i,j,c,t)$line(i,j,c) = branchinfo(i,j,c,'branchstatus',t);

* Min up and down times
minuptime(gen)   = geninfo(gen,'MinUptime','given');
mindowntime(gen) = geninfo(gen,'MinUptime','given');

* Ramp rates, uwcalc or given
rampup(gen)   = geninfo(gen,'RampUp','%ramprates%')/baseMVA;
rampdown(gen) = geninfo(gen,'RampDown','%ramprates%')/baseMVA;

* Define original cost model in dataset
costmodel(gen) = geninfo(gen,'costmodel','given');

* No load, startup, shutdown cost
noloadcost(gen) = geninfo(gen,'noloadcost','given');
startupcost(gen) = geninfo(gen,'startupcost','given');
shutdowncost(gen) = geninfo(gen,'shutdowncost','given');

* Quadratic objective function
numcostcoef(gen) = geninfo(gen,'numcostcoef','given');
costcoef(gen,costcoefset)$geninfo(gen,'costcoef',costcoefset) = geninfo(gen,'costcoef',costcoefset);

* Piecewise linear information
numcostpts(gen) = geninfo(gen,'numcostpts','given');
costpts_x(gen,costptset)$geninfo(gen,'costpts_x',costptset)   = geninfo(gen,'costpts_x',costptset);
costpts_y(gen,costptset)$geninfo(gen,'costpts_y',costptset)   = geninfo(gen,'costpts_y',costptset);

* Line resistance (r) and reactance (x)
r(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'r','given');
x(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'x','given');

* Line limit (active power)
rateA(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'rateA','%linelimits%')/baseMVA;
rateA(j,i,c)$line(i,j,c) = rateA(i,j,c);
* If linelimits=inf, no monitored lines
$if %linelimits% == 'inf' monitored_lines(i,j,c) = no;

* Limit on power across each interface
interfaceLimit(interface,t) = interfaceinfo(interface,t,'rateA')/baseMVA;

* Line current
currentrate(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'currentrateA','%linelimits%');
currentrate(j,i,c)$line(i,j,c) = branchinfo(i,j,c,'currentrateA','%linelimits%');

* Take down all lines to buses marked as "isolated"
branchstatus(i,j,c,t)$(type(i) eq 4 or type(j) eq 4) = 0;

* Line susceptance
B(i,j,c)$line(i,j,c) = -x(i,j,c)/(sqr(r(i,j,c))+sqr(x(i,j,c)));
B(j,i,c)$b(i,j,c) = b(i,j,c);

* transformer tap ratios and angles
ratio(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'ratio','given');
ratio(j,i,c)$ratio(i,j,c) =  ratio(i,j,c);
angle(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'angle','given') * pi/180;
angle(j,i,c)$angle(i,j,c) = -angle(i,j,c);



$ifthen.ac %modeltype% == "AC"
*---- AC model data types
parameters
  Qd(bus,t) "bus reactive power demand"
  
  Qg(gen,t)   "gen reactive power output"
  Qmax(gen)   "gen maximum reactive power output"
  Qmin(gen)   "gen minimum reactive power output"

  Bs(bus) "bus shunt susceptance"
  yb(i,j,conj,t) "Bus admittance matrix, Ybus"

  g(i,j,c) "line conductance",
  bc(i,j,c) "line charging susceptance"
  Bswitched(bus,bus_s) "susceptance of switched shunts",
  numBswitched(bus,bus_s) "number of each type of switched shunt elements at each bus"
;

* Reactive power information
Qd(bus,t)  = businfo(bus,'Qd',t)/baseMVA;
Qmax(gen) = geninfo(gen,'Qmax','given')/baseMVA;
Qmin(gen) = geninfo(gen,'Qmin','given')/baseMVA;
Qg(gen,t) = geninfo(gen,'Qg',t)/baseMVA;

* Bus shunt conductance and susceptance 
Bs(bus)    = businfo(bus,'Bs','given')/baseMVA;

* line conductance
g(i,j,c)$line(i,j,c) =  r(i,j,c)/(sqr(r(i,j,c))+sqr(x(i,j,c)));
g(j,i,c)$g(i,j,c) = g(i,j,c);

* line charging conductance
bc(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'bc','given');
bc(j,i,c)$bc(i,j,c) = bc(i,j,c);

* number and susceptance of switched shunt element data
numBswitched(bus,bus_s) = businfo(bus,'switchedelements',bus_s);
Bswitched(bus,bus_s)    = businfo(bus,'switchedBs',bus_s)/baseMVA;

$endif.ac


* ==== SECTION: Additional Model Options
*-- Elastic demand bidding turned on/off
$ifthen %demandbids% ==1
parameters
  demandpts_x        "price responsive demand cost breakpoints (piecewise linear)"
  demandpts_y        "price responsive demand cost breakpoints (piecewise linear)"
  numdemandpts       "number of price responsive demand points"
;

  numdemandpts(demandbid,t) = demandbidinfo(demandbid,t,'numbids','given');
  demandpts_x(demandbid,t,demandbid_s) = demandbidinfo(demandbid,t,'Quantity',demandbid_s);
  demandpts_y(demandbid,t,demandbid_s) = demandbidinfo(demandbid,t,'Price',demandbid_s);
$endif


*-- %allon% options
$ifthen %allon% == "lines"
* All lines turned on, each period
  branchstatus(i,j,c,t)$line(i,j,c) = 1;
$endif

