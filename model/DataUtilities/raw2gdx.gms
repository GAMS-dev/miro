$title "Power System GDX data file maker"

$if not set raw $abort "No input file selected..."
$if not set filepath $setnames "%gams.i%" filepath filename fileextension
$if not set rawpath $setnames "%raw%" rawpath rawname rawextension
$if not set out $set out "%rawpath%%rawname%.gdx"
$if not set outpath $setnames "%out%" outpath outname outextension
$if not set timeperiod $set timeperiod 1

* Auxiliary sets used for computations
sets
   ints        "Set of integers for looping and ordering",
   limtype     "Types of thermal limits" /'given', 'uwcalc', 'min', 'max', 'inf'/,
   costcoefset "Assuming at most 42-25=17 cost points or coeffs" /0*41/,
   costptset   "Assuming at most 42-25=17 cost points or coeffs" /1*42/,
   cols        "Columns of input matrices from read_* scripts"   /1*28/,
   doubles;
alias(costcoefset,ccs);
alias(costptset,cps);
doubles(costptset,cps)$(2*costptset.ord eq cps.ord) = yes;

* Sets relating to model elements
sets
   t               "Set of time periods (up to 24 for UC)" /%timeperiod%/,
   bus             "Set of buses",
   gen            "Set of generators",
   swing(bus)      "Set of swing buses",
   branchrows      "Set of rows in the branchdata matrix",
* Placeholder circuit set, very large
   circuit          "Indices for multiple lines between buses" /1*20/,
* Original circuit set, what is read in
   inputc(circuit)          "Input circuit set"
   line            "Set of lines in the transmission network",
   transformer(bus,bus,circuit)     "Set of lines with transformers",
*   dcline          "Set of dc lines in the transmission network",
   demandbid       "Set of price responsive demand bids",
   interface       "Set of interfaces to monitor in the transmission network",
   monitored_lines "Set of lines to compute shift factor rows for and enforce line limit constraints",
   demandbidmap(demandbid,bus)        "Mapping of demand bid identifier to bus (demandbid,bus)",
   interfacemap(interface,bus,bus)    "Mapping of interface identifier to buses (interface,from,to)";
alias(bus,from,to);
alias(circuit,c);


* Identifier to track GDX creation date
parameter version;
version = Jnow;

* Raw model data
parameters baseMVA, total_cost /0/,
           busdata, gendata, gencostdata, branchdata, switchedshuntdata;
* dcbranchdata

$GDXIN %raw%
$LOAD baseMVA
$LOAD ints
$LOAD bus
$LOAD gen
$LOAD busdata
$LOAD gendata
$LOAD gencostdata
$LOAD branchdata
$LOAD branchrows
*$LOAD dcbranchrows
*$LOAD dcbranchdata
$LOAD switchedshuntdata
$LOAD inputc=circuit
$GDXIN


sets
    fuel_t "Set of fuel types" /NA/,
    fuel_s "Set of fuel info selectors" /cost, id, num_gens/,
    prime_mover "Set of prime mover types for generators" /NA/,
    bus_t "Set of bus info data types"
          /'type', 'Pd', 'Qd', 'Gs', 'Bs', 'Area', 'Vm', 'Va',
           'baseKV', 'zone', 'maxVm', 'minVm', 'pf',
           'numswitchedshunts', 'switchedelements', 'switchedBs', 'switchedBsSolved',
           'LMP', 'LMP_Energy', 'LMP_Loss', 'LMP_Congestion',
           'numdemandpts', 'demandpts_price', 'demandpts_pd', 'demandpts_qd'/,
    gen_t "Set of generator info data types"
          /'atBus', 'Pg', 'Qg', 'Qmax', 'Qmin', 'Vg', 'mBase', 'status',
           'Pmax', 'Pmin', 'Pc1', 'Pc2', 'Qc1min', 'Qc1max', 'Qc2min', 'Qc2max',
           'RampUp', 'RampUp10', 'RampUp30', 'RampUpReactive',
           'RampDown', 'RampDown10', 'RampDown30', 'RampDownReactive',
           'APF', 'MinUptime', 'MinDowntime', 'nameplate_pf', 'min_pf',
           'R_max', 'Qfield', 'Rfield', 'Qend', 'Rend',
           'costmodel', 'startupcost', 'shutdowncost', 'numcostpts', 'numcostcoef',
           'costpts_x', 'costpts_y', 'costcoef', 'noloadcost',
           'Fuel', 'PrimeMover', 'Nameplate',
           'Eford', 'MTTF', 'MTTR', 'CO2Rate', 'NOxRate', 'SO2Rate',
           'NominalHeatRate', 'HeatRateMW', 'HeatRateMMBTU', 'NumHeatRatePoints'/,
    branch_t "Set of branch info data types"
             /'r', 'x', 'bc', 'rateA', 'rateB', 'rateC', 'ratio', 'angle',
              'line_v', 'branchstatus', 'minAngleDiff', 'maxAngleDiff',
              'currentrateA', 'currentrateB', 'currentrateC', 'LineSP'/,
    demandbid_t "Set of demandbid info data types" /Quantity, Price, NumBids/,
    interface_t "Set of interface limit types" /RateA, RateB, RateC/,
    bus_s "Set of bus info selectors" /'given', 'UWCalc', 1*24/,
    gen_s "Set of generator info selectors" /'given', 'UWCalc', 0*40/,
    branch_s "Set of branch info selectors" /'given', 'SIL_avg', 'UWCalc', 'min', 'max', 'inf'/
    demandbid_s "Set of demandbid info selectors"  /1*10, given/
;
bus_s(t) = yes;
gen_s(t) = yes;
gen_s(bus) = yes;
gen_s(prime_mover) = yes;
branch_s(t) = yes;


* Formatted and completed model data
parameters businfo, geninfo, branchinfo, fuelinfo(fuel_t,fuel_s),
           demandbidinfo(demandbid,t,demandbid_t,demandbid_s),
           interfaceinfo(interface,t,interface_t);
* dclineinfo;


parameters type, Pd, Qd, Gs, Bs, Area, Vm, Va, baseKV, zone, maxVm, minVm, pf;
loop((ints,bus)$((ints.val le card(bus)) and (busdata(ints,'1') eq bus.val)),
    type(bus)   = busdata(ints,'2');
    Pd(bus,'%timeperiod%') = busdata(ints,'3');
    Qd(bus,'%timeperiod%') = busdata(ints,'4');
    Gs(bus)     = busdata(ints,'5');
    Bs(bus)     = busdata(ints,'6');
    Area(bus)   = busdata(ints,'7');
    Vm(bus,t)   = busdata(ints,'8');
    Va(bus,t)   = busdata(ints,'9');
    baseKV(bus) = busdata(ints,'10');
    zone(bus)   = busdata(ints,'11');
    maxVm(bus)  = busdata(ints,'12');
    minVm(bus)  = busdata(ints,'13');
    pf(bus)$Pd(bus,'given') = sign(Qd(bus,'%timeperiod%')) * Pd(bus,'%timeperiod%') / sqrt(sqr(Pd(bus,'%timeperiod%')) + sqr(Qd(bus,'%timeperiod%')));
);

businfo(bus,'type','given')   = type(bus);
businfo(bus,'Pd',t)           = Pd(bus,t);
businfo(bus,'Qd',t)           = Qd(bus,t);
businfo(bus,'Gs','given')     = Gs(bus);
businfo(bus,'Bs','given')     = Bs(bus);
businfo(bus,'Area','given')   = Area(bus);
businfo(bus,'Vm',t)           = Vm(bus,t);
businfo(bus,'Va',t)           = Va(bus,t);
businfo(bus,'baseKV','given') = baseKV(bus);
businfo(bus,'zone','given')   = zone(bus);
businfo(bus,'maxVm','given')  = maxVm(bus);
businfo(bus,'minVm','given')  = minVm(bus);
businfo(bus,'pf','given')     = pf(bus);

businfo(bus,'numswitchedshunts','given') = switchedshuntdata(bus,'numswitchedshunts','given');
businfo(bus,'switchedelements',cols)     = switchedshuntdata(bus,'switchedelements',cols);
businfo(bus,'switchedBs',cols)           = switchedshuntdata(bus,'switchedBs',cols);
businfo(bus,'switchedBsSolved',t)        = switchedshuntdata(bus,'switchedBs','given');

swing(bus)$(type(bus) eq 3) = yes;


parameters atBus, Pg, Qg, Qmax, Qmin, Vg, mBase, status, Pmax, Pmin, Pc1, Pc2,
           Qc1min, Qc1max, Qc2min, Qc2max, RampUp, RampUp10, RampUp30, RampUpReactive,
           RampDown, RampDown10, RampDown30, RampDownReactive, APF, MinUptime, MinDowntime,
           nameplateHigh, UWRampUp, UWRampUp10, UWRampUp30, UWRampUpReactive,
           UWRampDown, UWRampDown10, UWRampDown30, UWRampDownReactive
           ;

atBus(gen,bus)$(gendata(gen,'1') eq bus.val) = 1;
Pg(gen)     = gendata(gen,'2');
Qg(gen)     = gendata(gen,'3');
Qmax(gen)   = gendata(gen,'4');
Qmin(gen)   = gendata(gen,'5');
Vg(gen)     = gendata(gen,'6');
mBase(gen)  = gendata(gen,'7');
status(gen) = gendata(gen,'8');
Pmax(gen)   = gendata(gen,'9');
Pmin(gen)   = gendata(gen,'10');
Pc1(gen)    = gendata(gen,'11');
Pc2(gen)    = gendata(gen,'12');
Qc1min(gen) = gendata(gen,'13');
Qc1max(gen) = gendata(gen,'14');
Qc2min(gen) = gendata(gen,'15');
Qc2max(gen) = gendata(gen,'16');
RampUp(gen)              = gendata(gen,'17');
RampUp10(gen)            = gendata(gen,'17')/6;
RampUp30(gen)            = gendata(gen,'17')/2;
RampUpReactive(gen)      = gendata(gen,'20');
RampUpReactive(gen)$(RampUpReactive(gen) eq 0) = inf;
RampDown(gen)            = gendata(gen,'22');
RampDown10(gen)          = gendata(gen,'22')/6;
RampDown30(gen)          = gendata(gen,'22')/2;
RampDownReactive(gen)    = gendata(gen,'20');
RampDownReactive(gen)$(RampDownReactive(gen) eq 0) = inf;
APF(gen)             = gendata(gen,'21');

* For generators with missing ramp rates, use Dan's approximation.
$batinclude '%filepath%calc_ramp_rates.gms'

* We don't have any data for this yet, so they're all 0
MinUptime(gen) = gendata(gen,'24');
MinDowntime(gen) = gendata(gen,'23');

* Generator status, Pg, and Qg are per-time-period quantities
geninfo(gen,'status','%timeperiod%') = status(gen);
geninfo(gen,'Pg','%timeperiod%') = Pg(gen);
geninfo(gen,'Qg','%timeperiod%') = Qg(gen);

geninfo(gen,'atBus',bus)      = atBus(gen,bus);
geninfo(gen,'atBus','given')  = gendata(gen,'1');
geninfo(gen,'Qmax','given')   = Qmax(gen);
geninfo(gen,'Qmin','given')   = Qmin(gen);
geninfo(gen,'Vg','given')     = Vg(gen);
geninfo(gen,'mBase','given')  = mBase(gen);
geninfo(gen,'Pmax','given')   = Pmax(gen);
geninfo(gen,'Pmin','given')   = Pmin(gen);
geninfo(gen,'Pc1','given')    = Pc1(gen);
geninfo(gen,'Pc2','given')    = Pc2(gen);
geninfo(gen,'Qc1min','given') = Qc1min(gen);
geninfo(gen,'Qc1max','given') = Qc1max(gen);
geninfo(gen,'Qc2min','given') = Qc2min(gen);
geninfo(gen,'Qc2max','given') = Qc2max(gen);
geninfo(gen,'RampUp','given') = RampUp(gen);
geninfo(gen,'RampUp10','given') = RampUp10(gen);
geninfo(gen,'RampUp30','given') = RampUp30(gen);
geninfo(gen,'RampUpReactive','given') = RampUpReactive(gen);
geninfo(gen,'RampDown','given')   = RampDown(gen) + RampUp(gen)$(RampDown(gen) eq 0);
geninfo(gen,'RampDown10','given') = RampDown10(gen) + RampUp10(gen)$(RampDown10(gen) eq 0);
geninfo(gen,'RampDown30','given') = RampDown30(gen) + RampUp30(gen)$(RampDown30(gen) eq 0);
geninfo(gen,'RampDownReactive','given') = RampDownReactive(gen) + RampUpReactive(gen)$(RampDownReactive(gen) eq 0);
geninfo(gen,'RampUp','uwcalc') = UWRampUp(gen);
geninfo(gen,'RampUp10','uwcalc') = UWRampUp10(gen);
geninfo(gen,'RampUp30','uwcalc') = UWRampUp30(gen);
geninfo(gen,'RampUpReactive','uwcalc') = UWRampUpReactive(gen);
geninfo(gen,'RampDown','uwcalc')   = UWRampDown(gen);
geninfo(gen,'RampDown10','uwcalc') = UWRampDown10(gen);
geninfo(gen,'RampDown30','uwcalc') = UWRampDown30(gen);
geninfo(gen,'RampDownReactive','uwcalc') = UWRampDownReactive(gen);
geninfo(gen,'APF','given') = APF(gen);

geninfo(gen,'MinUptime','given') = MinUptime(gen);
geninfo(gen,'MinDowntime','given') = MinDowntime(gen);

$batinclude '%filepath%calc_cost_curves.gms'
$batinclude '%filepath%calc_active_limits.gms'
$batinclude '%filepath%calc_reactive_limits.gms'

parameters r, x, bc, rateA, rateB, rateC, ratio, angle, line_v, branchstatus, minAngleDiff, maxAngleDiff;
loop(branchrows,
    loop(from$(branchdata(branchrows,'1') eq from.val),
        loop(to$(branchdata(branchrows,'2') eq to.val),
            loop(c$(branchdata(branchrows,'14') eq c.val),
                line(from,to,c) = yes;
                r(from,to,c) = branchdata(branchrows,'3');
                x(from,to,c) = branchdata(branchrows,'4');
                bc(from,to,c) = branchdata(branchrows,'5');
                rateA(from,to,c) = branchdata(branchrows,'6');
                rateB(from,to,c) = branchdata(branchrows,'7');
                rateC(from,to,c) = branchdata(branchrows,'8');
                ratio(from,to,c) = branchdata(branchrows,'9');
                ratio(from,to,c)$(ratio(from,to,c) eq 0) = 1;
                angle(from,to,c) = branchdata(branchrows,'10');
                line_v(from,to,c)$(baseKV(from) eq baseKV(to)) = baseKV(from);
                branchstatus(from,to,c,t) = branchdata(branchrows,'11');
                minAngleDiff(from,to,c) = branchdata(branchrows,'12');
                maxAngleDiff(from,to,c) = branchdata(branchrows,'13');

                transformer(from,to,c)$branchdata(branchrows,'15') = yes;
             );
        );
    );
);

parameter maxbaseKV;
maxbaseKV = smax(bus, baseKV(bus));
$ifthen %monitorall% == 1 monitored_lines(from,to,c)$line(from,to,c) = yes;
$else monitored_lines(from,to,c)$(line(from,to,c) and (max(baseKV(from), baseKV(to)) eq maxbaseKV)) = yes;
$endif


branchinfo(from,to,c,'r','given')$line(from,to,c) = r(from,to,c);
branchinfo(from,to,c,'x','given')$line(from,to,c) = x(from,to,c);
branchinfo(from,to,c,'bc','given')$line(from,to,c) = bc(from,to,c);
branchinfo(from,to,c,'rateA','given')$line(from,to,c) = rateA(from,to,c);
branchinfo(from,to,c,'rateB','given')$line(from,to,c) = rateB(from,to,c);
branchinfo(from,to,c,'rateC','given')$line(from,to,c) = rateC(from,to,c);
branchinfo(from,to,c,'ratio','given')$line(from,to,c) = ratio(from,to,c);
branchinfo(from,to,c,'angle','given')$line(from,to,c) = angle(from,to,c);
branchinfo(from,to,c,'line_v','given')$line(from,to,c) = line_v(from,to,c);
branchinfo(from,to,c,'branchstatus',t)$line(from,to,c) = branchstatus(from,to,c,t);
branchinfo(from,to,c,'minAngleDiff','given')$line(from,to,c) = minAngleDiff(from,to,c);
branchinfo(from,to,c,'maxAngleDiff','given')$line(from,to,c) = maxAngleDiff(from,to,c);

*------- Checking for loops, that is both lines ijc and jic exist
alias(bus,i,j);
sets
    isDouble(i,j,c) "Both lines ijc and jic exist"
* New circuit set, after data modification
    newC(circuit)  "Indices for multiple lines between buses"
;

parameters
    largestc "largest c"
    tempc "temp circuit input"
;


isDouble(i,j,c)$line(i,j,c)=0;
loop((i,j,c)$line(i,j,c),
  isDouble(i,j,c)$(line(i,j,c)) = 1$(line(j,i,c) and not(isDouble(j,i,c)) );
);
display isDouble;


largestc = card(inputc);
*-- Tack jic line on to the ij line format
loop((i,j,c)$isDouble(i,j,c),
    tempc = 0;
    loop(circuit$((tempc eq 0) and not(line(i,j,circuit) or line(j,i,circuit))),
	tempc=ord(circuit);
    );
    
    newC(circuit) = yes$(ord(circuit) eq (tempc));
    display newC;

    if(card(newC) eq 0,
	abort "Error: Line symmetry exists and circuit set size is too small.";
    );

*- Create a new circuit in ij
    line(j,i,newC) = 1;

*- Update all necessary information
    monitored_lines(i,j,newC) = yes;

    branchinfo(j,i,newC,'r','given') = r(j,i,c);
    branchinfo(j,i,newC,'x','given') = x(j,i,c);
    branchinfo(j,i,c,'r','given') = 0; r(j,i,c)=0;
    branchinfo(j,i,c,'x','given') = 0; x(j,i,c)=0;
    
    branchinfo(j,i,newC,'bc','given') = bc(j,i,c);
    branchinfo(j,i,c,'bc','given') = 0; bc(j,i,c)=0;

    branchinfo(j,i,newC,'rateA','given') = rateA(j,i,c);
    branchinfo(j,i,c,'rateA','given') = 0; rateA(j,i,c)=0;
    branchinfo(j,i,newC,'rateB','given') = rateB(j,i,c);
    branchinfo(j,i,c,'rateB','given') = 0; rateB(j,i,c)=0;
    branchinfo(j,i,newC,'rateC','given') = rateC(j,i,c);
    branchinfo(j,i,c,'rateC','given') = 0; rateC(j,i,c)=0;

    branchinfo(j,i,newC,'ratio','given') = ratio(j,i,c);
    branchinfo(j,i,c,'ratio','given') = 0; ratio(j,i,c)=0;
    branchinfo(j,i,newC,'angle','given') = angle(j,i,c);
    branchinfo(j,i,c,'angle','given') = 0; angle(j,i,c)=0;
    
    branchinfo(j,i,newC,'line_v','given') = line_v(j,i,c);
    branchinfo(j,i,c,'line_v','given') = 0; line_v(j,i,c)=0;
    
    branchinfo(j,i,newC,'branchstatus',t) = branchstatus(j,i,c,t);
    branchinfo(j,i,c,'branchstatus',t) = 0; branchstatus(j,i,c,t)=0;
    
    branchinfo(j,i,newC,'minAngleDiff','given') = minAngleDiff(j,i,c);
    branchinfo(j,i,c,'minAngleDiff','given') = 0; minAngleDiff(j,i,c)=0;
    branchinfo(j,i,newC,'maxAngleDiff','given') = maxAngleDiff(j,i,c);
    branchinfo(j,i,c,'maxAngleDiff','given') = 0; maxAngleDiff(j,i,c)=0;
*- Create a new circuit in ij
    line(j,i,c) = 0;

*- Find new largest circuit, should be last line
    largestc = max(tempc, largestc);
);

isDouble(i,j,c)$line(i,j,c)=0;
loop((i,j,c)$line(i,j,c),
  isDouble(i,j,c)$(line(i,j,c)) = 1$(line(j,i,c) and not(isDouble(j,i,c)) );
);
display isDouble;
newC(c) = yes$(ord(c) le largestc);
display largestc, newC;

$batinclude '%filepath%calc_line_limits.gms'
branchinfo(from,to,c,'currentrateA',limtype)$branchinfo(from,to,c,'rateA',limtype) =
        branchinfo(from,to,c,'rateA',limtype) / baseMVA;
branchinfo(from,to,c,'currentrateB',limtype)$branchinfo(from,to,c,'rateB',limtype) =
        branchinfo(from,to,c,'rateB',limtype) / baseMVA;
branchinfo(from,to,c,'currentrateC',limtype)$branchinfo(from,to,c,'rateC',limtype) =
        branchinfo(from,to,c,'rateC',limtype) / baseMVA;

*------- Unload information
execute_unload "%out%", version, total_cost, baseMVA, ints, t, bus, gen, newC=circuit,
                        line, transformer, monitored_lines, demandbid, demandbidmap, interface, interfacemap
                        bus_t, bus_s, gen_t, gen_s, branch_t, branch_s, fuel_t, fuel_s, prime_mover
                        demandbid_t, demandbid_s, interface_t
                        businfo, geninfo, branchinfo, demandbidinfo, interfaceinfo, fuelinfo
;
