$if not set case $abort Need to set --case
$GDXIN %case%
$onempty

Set ints(*) Set of integers for looping and ordering ;
$LOADDC ints

Set t(t) Set of time periods (up to 24 for UC) ;
$LOADDC t

Set bus(bus) Set of buses ;
$LOADDC bus

Set gen(gen) Set of generators ;
$LOADDC gen

Set circuit(circuit) Indices for multiple lines between buses ;
$LOADDC circuit

Set fuel_t(fuel_t) Set of fuel types ;
$LOADDC fuel_t

Set fuel_s(fuel_s) Set of fuel info selectors ;
$LOADDC fuel_s

Set prime_mover(*) Set of prime mover types for generators ;
$LOADDC prime_mover

Set demandbid(demandbid) Set of buses ;
$LOADDC demandbid

Set demandbidmap(demandbid,bus) 'Mapping of demand bid identifier to bus (demandbid,bus)' ;
$LOADDC demandbidmap

Set interface(interface) Set of interfaces to monitor in the transmission network / /;

Set interfacemap(interface,bus,bus) 'Mapping of interface identifier to buses (interface,from,to)' / /;

Set bus_t(bus_t) Set of bus info data types ;
$LOADDC bus_t

Set bus_s(bus_s) Set of bus info selectors ;
$LOADDC bus_s

Set gen_t(gen_t) Set of generator info data types ;
$LOADDC gen_t

Set gen_s(gen_s) Set of generator info selectors ;
$LOADDC gen_s

Set branch_t(branch_t) Set of branch info data types ;
$LOADDC branch_t

Set branch_s(branch_s) Set of branch info selectors ;
$LOADDC branch_s

Set demandbid_t(demandbid_t) Set of demandbid info data types ;
$LOADDC demandbid_t

Set demandbid_s(demandbid_s) Set of demandbid info selectors ;
$LOADDC demandbid_s

Set interface_t(interface_t) Set of interface limit types ;
$LOADDC interface_t

Set line(bus,bus,circuit) Set of lines in the transmission network ;
$LOADDC line

Set transformer(bus,bus,circuit) Set of lines with transformers ;
$LOADDC transformer

Set monitored_lines(bus,bus,circuit) Set of lines to compute shift factor rows for and enforce line limit constraints ;
$LOADDC monitored_lines

Scalar version ;
$LOADDC version

Scalar baseMVA ;
$LOADDC baseMVA

Scalar total_cost ;
$LOADDC total_cost

Parameter businfo(bus,bus_t,bus_s) ;
$LOADDC businfo

Parameter geninfo(gen,gen_t,gen_s) ;
$LOADDC geninfo

Parameter fuelinfo(fuel_t,fuel_s) / /;

Parameter branchinfo(bus,bus,circuit,branch_t,branch_s) ;
$LOADDC branchinfo

Parameter demandbidinfo(demandbid,t,demandbid_t,demandbid_s) ;
$LOADDC demandbidinfo

Parameter interfaceinfo(interface,t,interface_t) / /;

$offempty

$if not set NUMRUN   $set NUMRUN   10
$if not set PCTAVAIL $set PCTAVAIL  0.1
set genRun / grun1*grun%NUMRUN% /;
set genRunAvail(genRun,gen) 'Generators available in run';
genRunAvail(genRun,gen) = uniform(0,1) < %PCTAVAIL%;
