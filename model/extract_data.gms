*==== SECTION: Data (pre) declaration
sets
* New sets
    conj /real,imag/
    costcoefset "Placeholder set for quadratic function coefficients (0,1,2)" /0*2/ 
    costptset "Placeholder set for pwl function pieces"  /1*40/

* Timeperiod
    t /%timeperiod%/,
    
* Dataset sets
    bus, gen, circuit,
    interface, interfacemap (interface,bus,bus),
    demandbid, demandbidmap,    
    fuel_t, fuel_s, prime_mover,
    bus_t, bus_s,
    gen_t, gen_s,
    branch_t, branch_s,
    
    interface_t,
    line(bus,bus,circuit),
    transformer(bus,bus,circuit),
    monitored_lines(bus,bus,circuit)
;

alias(bus,i,j);
alias(circuit,c);
alias(gen,gen1);

parameters
    version, baseMVA, total_cost,
* Domain info not stated because of how we iterate through data
     businfo(*,*,*),
     geninfo(*,*,*),
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

$LOAD bus, gen, circuit, line, transformer, monitored_lines,
$LOAD bus_t, bus_s, gen_t, gen_s, branch_t, branch_s
$LOAD fuel_t, fuel_s, prime_mover
$LOAD businfo, geninfo, branchinfo, fuelinfo
$LOAD interface, interface_t, interfacemap, interfaceinfo
$GDXIN

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

* genPmin, case insensitive
$iftheni %genPmin% == "0"
$elseifi %genPmin% == "uwcalc"
$elseifi %genPmin% == "given"
$else $abort "Fix invalid option: --genPmin=%genPmin%"
$endif

* allon, case insensitive
$iftheni %allon% == "gens"
$elseifi %allon% == "lines"
$elseifi %allon% == "both"
$elseif %allon% == 0
$else $if set allon $abort "Fix invalid option: --allon=%allon%"
$endif

*==== SECTION: Data Declaration (extracted/manipulated from datafile)
*-- All OPF models
parameters
  type(bus)         "bus type (probably irrelevant, but gives reference bus[es])"
  pf(bus)           "bus demand power factor"
  Pd(bus)           "bus real power demand"

  Pg(gen)           "gen real power output"
  Pmax(gen)         "gen maximum real power output"
  Pmin(gen)         "gen minimum real power output"
  Va(bus)           "bus voltage angle"
  
  Vm(bus)           "bus voltage magnitude"
  MaxVm(bus)        "maximum bus voltage magnitude"
  MinVm(bus)        "minimum bus voltage magnitude"
  Gs(bus)           "bus shunt conductance"

  atBus(gen,bus)    "Location of generator"
  status(gen)       "generator status"

  costcoef          "gen cost coefficients"
  costpts_x         "gen cost breakpoints (piecewise linear)"
  costpts_y         "gen cost breakpoints (piecewise linear)"

  costmodel(gen)    "gen cost model type"
  numcostpts(gen)   "gen cost number of piecewise points"
  numcostcoef(gen)  "gen cost number of coefficients"
  noloadcost(gen)   "generator no load operating cost for piecewise cost functions"
  
  r(i,j,c)          "line resistance"
  x(i,j,c)          "line reactance"
  B(i,j,c)          "line susceptance"
  ratio(i,j,c)      "transformer tap ratio",
  angle(i,j,c)      "transformer tap angle",
  rateA(i,j,c)      "line power limits (MW)",
  currentrate(i,j,c)  "line current limits",
  branchstatus(i,j,c) "line status",
  interfaceLimit(interface) "Limit on power across each interface"
;

* Bus type
type(bus) = businfo(bus,'type','given');
* Power factor
pf(bus) = businfo(bus,'pf','given');
* Bus demand(real power)
Pd(bus) = businfo(bus,'Pd','%timeperiod%')/baseMVA;

* Bus shunt conductance
Gs(bus) = businfo(bus,'Gs','given')/baseMVA;

atBus(gen,bus)$geninfo(gen,'atBus',bus) = 1;
Pg(gen) = geninfo(gen,'Pg','%timeperiod%')/baseMVA;

*Maximum power generation
Pmax(gen) = geninfo(gen,'Pmax','given')/baseMVA;

* Minimum power generation options
$ifthen %genPmin% == 0
  Pmin(gen) = 0;
$else
  Pmin(gen)= geninfo(gen,'Pmin','%genPmin%')/baseMVA;
$endif

* Voltage angle
Va(bus) = businfo(bus,'Va','%timeperiod%')*pi/180;
* Voltage magnitude information
Vm(bus) = businfo(bus,'Vm','%timeperiod%');
maxVm(bus) = businfo(bus,'maxVm','given');
minVm(bus) = businfo(bus,'minVm','given');

* Initial generator commitment
status(gen) = geninfo(gen,'status','%timeperiod%');

* Initial branch status (active/not connected)
branchstatus(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'branchstatus','%timeperiod%');

* Define original cost model in dataset
costmodel(gen) = geninfo(gen,'costmodel','given');

* No load cost
noloadcost(gen) = geninfo(gen,'noloadcost','given');

*Quadratic objective function
numcostpts(gen) = geninfo(gen,'numcostpts','given');
costcoef(gen,costcoefset)$geninfo(gen,'costcoef',costcoefset) = geninfo(gen,'costcoef',costcoefset);

* Piecewise linear information
numcostcoef(gen) = geninfo(gen,'numcostcoef','given');
costpts_x(gen,costptset)$geninfo(gen,'costpts_x',costptset)   = geninfo(gen,'costpts_x',costptset);
costpts_y(gen,costptset)$geninfo(gen,'costpts_y',costptset)   = geninfo(gen,'costpts_y',costptset);

* Line resistance (r) and reactance (x)
r(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'r','given');
x(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'x','given');

* Line limit (active power)
rateA(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'rateA','%linelimits%')/baseMVA;
rateA(j,i,c)$(line(i,j,c)) = branchinfo(i,j,c,'rateA','%linelimits%')/baseMVA;

* If linelimits=inf, no monitored lines
$if %linelimits% == 'inf' monitored_lines(i,j,c) = no;

* Limit on power across each interface
interfaceLimit(interface) = interfaceinfo(interface,'%timeperiod%','rateA')/baseMVA;

* Line current
currentrate(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'currentrateA','%linelimits%');
currentrate(j,i,c)$(line(i,j,c)) = branchinfo(i,j,c,'currentrateA','%linelimits%');

* Take down all lines to buses marked as "isolated"
branchstatus(i,j,c)$(type(i) eq 4 or type(j) eq 4) = 0;

* Line susceptance
B(i,j,c)$line(i,j,c) = -x(i,j,c)/(sqr(r(i,j,c))+sqr(x(i,j,c)));
B(j,i,c)$(b(i,j,c)) = b(i,j,c);

* transformer tap ratios and angles
ratio(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'ratio','given');
ratio(j,i,c)$(ratio(i,j,c)) =  ratio(i,j,c);
angle(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'angle','given') * pi/180;
angle(j,i,c)$(angle(i,j,c)) = -angle(i,j,c);



$ifthen.ac %modeltype% == "AC"
*---- AC model data types
parameters
  Qd(bus)           "bus reactive power demand"
  
  Qg(gen)           "gen reactive power output"
  Qmax(gen)         "gen maximum reactive power output"
  Qmin(gen)         "gen minimum reactive power output"

  Bs(bus)           "bus shunt susceptance"
  yb(i,j,conj)      "Bus admittance matrix, Ybus"
  
  g(i,j,c)          "line conductance"
  bc(i,j,c)         "line charging susceptance"
  Bswitched(bus,bus_s)     "susceptance of switched shunts",
  numBswitched(bus,bus_s)  "number of each type of switched shunt elements at each bus"
;

* Reactive power information
Qd(bus) = businfo(bus,'Qd','%timeperiod%')/baseMVA;
Qmax(gen) = geninfo(gen,'Qmax','given')/baseMVA;
Qmin(gen) = geninfo(gen,'Qmin','given')/baseMVA;
Qg(gen) = geninfo(gen,'Qg','%timeperiod%')/baseMVA;

* Bus shunt conductance and susceptance 
Bs(bus) = businfo(bus,'Bs','given')/baseMVA;

* line conductance
g(i,j,c)$line(i,j,c) =  r(i,j,c)/(sqr(r(i,j,c))+sqr(x(i,j,c)));
g(j,i,c)$(g(i,j,c)) = g(i,j,c);

* line charging conductance
bc(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'bc','given');
bc(j,i,c)$(bc(i,j,c)) = bc(i,j,c);

* number and susceptance of switched shunt element data
numBswitched(bus,bus_s) = businfo(bus,'switchedelements',bus_s);
Bswitched(bus,bus_s)    = businfo(bus,'switchedBs',bus_s)/baseMVA;

$endif.ac


* ==== SECTION: Additional Model Options
*-- Elastic demand bidding turned on/off
$ifthen %demandbids% ==1
parameters
  demandpts_x       "price responsive demand cost breakpoints (piecewise linear)"
  demandpts_y       "price responsive demand cost breakpoints (piecewise linear)"
  numdemandpts      "number of price responsive demand points"
;

  numdemandpts(demandbid) = demandbidinfo(demandbid,'%timeperiod%','numbids','given');
  demandpts_x(demandbid,demandbid_s) = demandbidinfo(demandbid,'%timeperiod%','Quantity',demandbid_s);
  demandpts_y(demandbid,demandbid_s) = demandbidinfo(demandbid,'%timeperiod%','Price',demandbid_s);
$endif

*-- %allon% options
$ifthen %allon% == "gens"
  status(gen) = 1;
$elseif %allon% == "lines"
  branchstatus(i,j,c)$line(i,j,c) = 1; 
$elseif %allon% == "both"
  status(gen) = 1;
  branchstatus(i,j,c)$line(i,j,c) = 1;
$endif

