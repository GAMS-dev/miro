$title "Power System GDX data file maker"

$if not set filepath $setnames "%gams.i%" filepath filename fileextension
$if not set casepath $setnames "%case%" casepath casename caseextension
$if not set solution $set solution "temp_solution.gdx"
$if not set out $set out %filepath%%casename%_solution.gdx
$if not set ac $set ac 1

$if not set timeperiod abort "Time period not set!"


sets
    ints,
    t,
    bus,
    gen,
    circuit,
    demandbid,
    demandbidmap,
    interface,
    interfacemap,
    fuel_t, fuel_s, prime_mover,
    bus_t, bus_s,
    gen_t, gen_s,
    branch_t, branch_s,
    demandbid_t, demandbid_s
    interface_t,
    line(bus,bus,circuit),
    transformer(bus,bus,circuit),
    monitored_lines(bus,bus,circuit)
;

parameters version, baseMVA, total_cost;
parameters businfo(bus,bus_t,bus_s), geninfo(gen,gen_t,gen_s), fuelinfo(fuel_t,fuel_s),
           branchinfo(bus,bus,circuit,branch_t,branch_s),
           demandbidinfo(demandbid,t,demandbid_t,demandbid_s),
           interfaceinfo(interface,t,interface_t);

$GDXIN %case%
$LOAD version, baseMVA, total_cost
$LOAD ints, t, bus, gen, circuit, line, transformer, monitored_lines,
$LOAD bus_t, bus_s, gen_t, gen_s, branch_t, branch_s,
$LOAD demandbid_t, demandbid_s, interface_t
$LOAD fuel_t, fuel_s, prime_mover
$LOAD businfo, geninfo, branchinfo
$LOAD demandbid, demandbidmap, demandbidinfo
$LOAD interface, interfacemap, interfaceinfo
$GDXIN

alias(bus,i,j);
alias(circuit,c);

parameters Vm, Va, Pg, Qg, total_cost,
           LMP_Energy(bus), LMP_Loss(bus), LMP_Congestion(bus), LMP(bus),
           LineSP(i,j,c), status(gen), shuntB(bus);
$GDXIN %solution%
$LOADR Vm, Va, Pg

$ifthen %ac% == 1
$LOADR Qg, shuntB
$endif

$ifthen %decompose_lmp% == 1
$LOADR LMP_Energy, LMP_Loss, LMP_Congestion
$endif

$LOADR total_cost, LMP, LineSP
$GDXIN
version = Jnow;

businfo(bus,'Vm','%timeperiod%') = Vm(bus);
businfo(bus,'Va','%timeperiod%') = Va(bus);
businfo(bus,'LMP','%timeperiod%') = LMP(bus) / baseMVA;
geninfo(gen,'Pg','%timeperiod%') = Pg(gen) * baseMVA;

$ifthen %ac% == 1
geninfo(gen,'Qg','%timeperiod%') = Qg(gen) * baseMVA;
businfo(bus,'switchedBsSolved','%timeperiod%') = shuntB(bus) * baseMVA;
$endif

$ontext
parameter qd, bus_pf;
$GDXIN %solution%
$load Qd, bus_pf
$GDXIN
businfo(bus,'Qd','%timeperiod%') = Qd(bus);
businfo(bus,'pf','given') = bus_pf(bus);
$offtext

$ifthen %decompose_lmp% == 1
businfo(bus,'LMP_Energy','%timeperiod%') = LMP_Energy(bus) / baseMVA;
businfo(bus,'LMP_Loss','%timeperiod%') = LMP_Loss(bus) / baseMVA;
businfo(bus,'LMP_Congestion','%timeperiod%') = LMP_Congestion(bus) / baseMVA;
$endif

branchinfo(i,j,c,'LineSP','%timeperiod%') = LineSP(i,j,c) * baseMVA;


execute_unload "%out%temp", version, total_cost, baseMVA, ints, t, bus, gen, circuit,
                        line, transformer, monitored_lines, demandbid, demandbidmap, interface, interfacemap
                        bus_t, bus_s, gen_t, gen_s, branch_t, branch_s, fuel_t, fuel_s, prime_mover
                        demandbid_t, demandbid_s, interface_t
                        businfo, geninfo, branchinfo, demandbidinfo, interfaceinfo, fuelinfo
;

execute 'gams "%filepath%save_domain_info.gms" --in="%out%temp" gdx="%out%"'
if(errorlevel ne 0, abort "Saving domain info failed!");
execute 'rm "%out%temp"';

