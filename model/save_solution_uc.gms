$title "Power System GDX data file maker"

$if not set filepath $setnames "%gams.i%" filepath filename fileextension
$if not set casepath $setnames "%case%" casepath casename caseextension
$if not set solution $set solution "temp_solution.gdx"
$if not set out $set out %casepath%%casename%_UC_solution.gdx


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
parameters businfo(*,*,*), geninfo(*,*,*), fuelinfo(*,*),
           branchinfo(*,*,*,*,*), demandbidinfo(*,*,*,*), interfaceinfo(*,*,*);

$GDXIN %case%
$LOAD version, baseMVA, total_cost
$LOAD ints, t, bus, gen, circuit, line, transformer, monitored_lines,
$LOAD bus_t, bus_s, gen_t, gen_s, branch_t, branch_s,
$LOAD demandbid_t, demandbid_s, interface_t
$LOAD fuel_t, fuel_s, prime_mover
$LOAD businfo, geninfo, branchinfo, fuelinfo
$LOAD demandbid, demandbidmap, demandbidinfo
$LOAD interface, interfacemap, interfaceinfo
$GDXIN

alias(bus,i,j);
alias(circuit,c);

set t_solved;
parameters Vm, Va, Pg, Qg, total_cost,
           LMP_Energy(bus,t_solved), LMP_Loss(bus,t_solved), LMP_Congestion(bus,t_solved), LMP(bus,t_solved),
           LineSP(i,j,c,t_solved), status(gen,t_solved), shuntB(bus,*);
$GDXIN %solution%
$LOAD t_solved=t
$LOADR Vm, Va, Pg

$ifthen %ac% == 1
$LOADR Qg, shuntB
$endif

$ifthen %uc% == 1
$LOADR status
$endif

$ifthen %decompose_lmp% == 1
$LOADR LMP_Energy, LMP_Loss, LMP_Congestion
$endif

$LOADR total_cost, LMP, LineSP
$GDXIN
version = Jnow;

businfo(bus,'Vm',t_solved) = Vm(bus,t_solved);
businfo(bus,'Va',t_solved) = Va(bus,t_solved);
businfo(bus,'LMP',t_solved) = LMP(bus,t_solved) / baseMVA;

geninfo(gen,'Pg',t_solved) = Pg(gen,t_solved) * baseMVA;
$ifthen %ac% == 1
geninfo(gen,'Qg',t_solved) = Qg(gen,t_solved) * baseMVA;
businfo(bus,'switchedBsSolved',t_solved) = shuntB(bus,t_solved) * baseMVA;
$endif

$ifthen %uc% == 1
geninfo(gen,'status',t_solved) = status(gen,t_solved);
$endif

$ifthen %decompose_lmp% == 1
businfo(bus,'LMP_Energy',t_solved) = LMP_Energy(bus,t_solved) / baseMVA;
businfo(bus,'LMP_Loss',t_solved) = LMP_Loss(bus,t_solved) / baseMVA;
businfo(bus,'LMP_Congestion',t_solved) = LMP_Congestion(bus,t_solved) / baseMVA;
$endif

branchinfo(i,j,c,'LineSP',t_solved) = LineSP(i,j,c,t_solved) * baseMVA;


execute_unload "%out%temp", version, total_cost, baseMVA, ints, t, bus, gen, circuit,
                        line, transformer, monitored_lines, demandbid, demandbidmap, interface, interfacemap
                        bus_t, bus_s, gen_t, gen_s, branch_t, branch_s, fuel_t, fuel_s, prime_mover
                        demandbid_t, demandbid_s, interface_t
                        businfo, geninfo, branchinfo, demandbidinfo, interfaceinfo, fuelinfo
;

execute 'gams "%filepath%save_domain_info.gms" --in="%out%temp" gdx="%out%"'
if(errorlevel ne 0, abort "Saving domain info failed!");
execute 'rm "%out%temp"';

