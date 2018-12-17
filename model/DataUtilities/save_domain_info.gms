$title "Rewrite GDX files including all domain info (must be called with gdx=*)"

$if not set in $abort "No input file given."
$setnames "%gams.i%" filepath filename fileextension

sets
    ints,
    t,
    bus,
    gen,
    circuit,
    fuel_t, fuel_s, prime_mover,
    demandbid, demandbidmap(demandbid,bus),
    interface, interfacemap(interface,bus,bus),
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
parameters businfo(bus,bus_t,bus_s), geninfo(gen,gen_t,gen_s),
           fuelinfo(fuel_t,fuel_s),
           branchinfo(bus,bus,circuit,branch_t,branch_s),
           demandbidinfo(demandbid,t,demandbid_t,demandbid_s),
           interfaceinfo(interface,t,interface_t);

$GDXIN %in%
$LOAD version, baseMVA, total_cost
$LOAD ints, t, bus, gen, circuit, line, transformer, monitored_lines,
$LOAD bus_t, bus_s, gen_t, gen_s, branch_t, branch_s,
$LOAD demandbid_t, demandbid_s, interface_t
$LOAD fuel_t, fuel_s, prime_mover
$LOAD businfo, geninfo, branchinfo, fuelinfo
$LOAD demandbid, demandbidmap, demandbidinfo
$LOAD interface, interfacemap, interfaceinfo
$GDXIN

