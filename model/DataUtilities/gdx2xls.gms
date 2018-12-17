$if not set in $exit
$setnames "%in%" inpath inname inextension
$setnames "%gams.i%" filepath filename fileextension
$if not set out $set out "%inpath%%inname%.xlsx"

* We won't be writing any 3-winding transformers because it's
* too hard to go through and find the imaginary star bus if one exists.
sets     ints, t, bus, gen, circuit, line, prime_mover, demandbid, demandbidmap, interface, interfacemap, transformer, monitored_lines,
         fuel_t, fuel_s, bus_t, bus_s, gen_t, gen_s, branch_t, branch_s, demandbid_t, demandbid_s, interface_t ;

alias(bus,i,j);
alias(circuit,c);

parameters       businfo(bus,bus_t,bus_s),
                 geninfo(gen,gen_t,gen_s),
                 branchinfo(i,j,c,branch_t,branch_s),
                 fuelinfo(fuel_t, fuel_s),
                 demandbidinfo(demandbid,t,demandbid_t,demandbid_s),
                 demandbidinfo_1(demandbid,t,demandbid_t,demandbid_s),
                 demandbidinfo_2(demandbid,t,demandbid_t,demandbid_s),
                 interfaceinfo(interface,t,interface_t);
parameters  version, baseMVA, total_cost;

set entry /baseMVA, branchinfo, branch_s, branch_t, bus, businfo, bus_s, bus_t, circuit, demandbid, demandbidinfo,
           demandbidmap, demandbid_s, demandbid_t, fuelinfo, fuel_s, fuel_t, gen, geninfo, gen_s, gen_t, interface,
           interfaceinfo, interfacemap, interface_t, ints, line, monitored_lines, prime_mover, t, total_cost, transformer, version/;

$GDXIN %in%
$LOAD ints, t, bus, gen, circuit, line, prime_mover, demandbid, demandbidmap, interface, interfacemap, transformer, monitored_lines
$LOAD fuel_t, fuel_s, bus_t, bus_s, gen_t, gen_s, branch_t, branch_s, demandbid_t, demandbid_s, interface_t
$LOAD version, baseMVA, total_cost, businfo, geninfo, branchinfo, fuelinfo, demandbidinfo, interfaceinfo
$GDXIN

parameters count_baseMVA, count_branchinfo, count_branch_s, count_branch_t, count_bus, count_businfo, count_bus_s, count_bus_t, count_circuit, count_demandbid,
           count_demandbidinfo, count_demandbidinfo_1, count_demandbidinfo_2, count_demandbidmap, count_demandbid_s, count_demandbid_t, count_fuelinfo, count_fuel_s, count_fuel_t, count_gen, count_geninfo,
           count_gen_s, count_gen_t, count_interface, count_interfaceinfo, count_interfacemap, count_interface_t, count_ints, count_line, count_monitored_lines,
           count_prime_mover, count_t, count_total_cost, count_transformer, count_version ;

count_baseMVA = card(baseMVA);
count_branchinfo = card(branchinfo);
count_branch_s = card(branch_s);
count_branch_t = card(branch_t);
count_bus = card(bus);
count_businfo = card(businfo);
count_bus_s = card(bus_s);
count_bus_t = card(bus_t);
count_circuit = card(circuit);
count_demandbid = card(demandbid);
count_demandbidinfo = card(demandbidinfo);
count_demandbidmap = card(demandbidmap);
count_demandbid_s = card(demandbid_s);
count_demandbid_t = card(demandbid_t);
count_fuelinfo = card(fuelinfo);
count_fuel_s = card(fuel_s);
count_fuel_t = card(fuel_t);
count_gen = card(gen);
count_geninfo = card(geninfo);
count_gen_s = card(gen_s);
count_gen_t = card(gen_t);
count_interface = card(interface);
count_interfaceinfo = card(interfaceinfo);
count_interfacemap = card(interfacemap);
count_interface_t = card(interface_t);
count_ints = card(ints);
count_line = card(line);
count_monitored_lines = card(monitored_lines);
count_prime_mover = card(prime_mover);
count_t = card(t);
count_total_cost = card(total_cost);
count_transformer = card(transformer);
count_version = card(version);

if(count_demandbidinfo > 1000000 and count_demandbidinfo < 2000000, demandbidinfo_1(demandbid,t,demandbid_t,demandbid_s) = demandbidinfo(demandbid,t,demandbid_t,demandbid_s)$(ord(demandbid) > card(demandbid)*0.5) ;
                                                                    demandbidinfo(demandbid,t,demandbid_t,demandbid_s) = demandbidinfo(demandbid,t,demandbid_t,demandbid_s)$(ord(demandbid) <= card(demandbid)*0.5)  ;
                                                                    count_demandbidinfo = card(demandbidinfo) ;
                                                                    count_demandbidinfo_1 = card(demandbidinfo_1) ;
);

if(count_demandbidinfo > 2000000, demandbidinfo_2(demandbid,t,demandbid_t,demandbid_s) = demandbidinfo(demandbid,t,demandbid_t,demandbid_s)$(ord(demandbid) > card(demandbid)*0.66) ;
                                  demandbidinfo_1(demandbid,t,demandbid_t,demandbid_s) = demandbidinfo(demandbid,t,demandbid_t,demandbid_s)$(ord(demandbid) > card(demandbid)*0.33 and ord(demandbid) <= card(demandbid)*0.66) ;
                                  demandbidinfo(demandbid,t,demandbid_t,demandbid_s) = demandbidinfo(demandbid,t,demandbid_t,demandbid_s)$(ord(demandbid) <= card(demandbid)*0.33) ;
                                  count_demandbidinfo = card(demandbidinfo) ;
                                  count_demandbidinfo_1 = card(demandbidinfo_1) ;
                                  count_demandbidinfo_2 = card(demandbidinfo_2) ;
);

execute_unload '%filepath%excel_temp.gdx' baseMVA, branchinfo, branch_s, branch_t, bus, businfo, bus_s, bus_t, circuit, demandbid, demandbidinfo, demandbidinfo_1, demandbidinfo_2,
           demandbidmap, demandbid_s, demandbid_t, fuelinfo, fuel_s, fuel_t, gen, geninfo, gen_s, gen_t, interface,
           interfaceinfo, interfacemap, interface_t, ints, line, monitored_lines, prime_mover, t, total_cost, transformer, version,
           count_baseMVA, count_branchinfo, count_branch_s, count_branch_t, count_bus, count_businfo, count_bus_s, count_bus_t, count_circuit, count_demandbid,
           count_demandbidinfo, count_demandbidinfo_1, count_demandbidinfo_2, count_demandbidmap, count_demandbid_s, count_demandbid_t, count_fuelinfo, count_fuel_s, count_fuel_t, count_gen, count_geninfo,
           count_gen_s, count_gen_t, count_interface, count_interfaceinfo, count_interfacemap, count_interface_t, count_ints, count_line, count_monitored_lines,
           count_prime_mover, count_t, count_total_cost, count_transformer, count_version ;
$if exist "%out%" $call 'rm "%out%"'
$onecho > recommands.txt
input = "%filepath%excel_temp.gdx"
output = "%out%"
Trace = 3

*---------------------------------------------------------------------------------------------------------------------
* Make the table of contents
*---------------------------------------------------------------------------------------------------------------------

text="NAME" rng=Table_of_Contents!a1 text="TYPE" rng=Table_of_Contents!b1 text="Dim" rng=Table_of_Contents!c1 text="Count" rng=Table_of_Contents!d1 text="Explanatory Text" rng=Table_of_Contents!e1
text="baseMVA" rng=Table_of_Contents!a2 link=Scalar!a1 text="parameter" rng=Table_of_Contents!b2 text="0" rng=Table_of_Contents!c2 Par=count_baseMVA rng=Table_of_Contents!d2
text="branchinfo" rng=Table_of_Contents!a3 link=Branchinfo!a1 text="parameter" rng=Table_of_Contents!b3 text="5" rng=Table_of_Contents!c3 Par=count_branchinfo rng=Table_of_Contents!d3
text="branch_s" rng=Table_of_Contents!a4 link=Branch_s!a1 text="set" rng=Table_of_Contents!b4 text="1" rng=Table_of_Contents!c4 Par=count_branch_s rng=Table_of_Contents!d4 text="Set of branch info selectors" rng=Table_of_Contents!e4
text="branch_t" rng=Table_of_Contents!a5 link=Branch_t!a1 text="set" rng=Table_of_Contents!b5 text="1" rng=Table_of_Contents!c5 Par=count_branch_t rng=Table_of_Contents!d5 text="Set of branch info data types" rng=Table_of_Contents!e5
text="bus" rng=Table_of_Contents!a6 link=Bus!a1 text="set" rng=Table_of_Contents!b6 text="1" rng=Table_of_Contents!c6 Par=count_bus rng=Table_of_Contents!d6 text="Set of buses" rng=Table_of_Contents!e6
text="businfo" rng=Table_of_Contents!a7 link=Businfo!a1 text="parameter" rng=Table_of_Contents!b7 text="3" rng=Table_of_Contents!c7 Par=count_businfo rng=Table_of_Contents!d7
text="bus_s" rng=Table_of_Contents!a8 link=Bus_s!a1 text="set" rng=Table_of_Contents!b8 text="1" rng=Table_of_Contents!c8 Par=count_bus_s rng=Table_of_Contents!d8 text="Set of bus info selectors" rng=Table_of_Contents!e8
text="bus_t" rng=Table_of_Contents!a9 link=Bus_t!a1 text="set" rng=Table_of_Contents!b9 text="1" rng=Table_of_Contents!c9 Par=count_bus_t rng=Table_of_Contents!d9 text="Set of bus info data types" rng=Table_of_Contents!e9
text="circuit" rng=Table_of_Contents!a10 link=Circuit!a1 text="set" rng=Table_of_Contents!b10 text="1" rng=Table_of_Contents!c10 Par=count_circuit rng=Table_of_Contents!d10 text="Indices for multiple lines between buses" rng=Table_of_Contents!e10
text="demandbid" rng=Table_of_Contents!a11 link=Demandbid!a1 text="set" rng=Table_of_Contents!b11 text="1" rng=Table_of_Contents!c11 Par=count_demandbid rng=Table_of_Contents!d11 text="Set of buses" rng=Table_of_Contents!e11
text="demandbidinfo" rng=Table_of_Contents!a12 link=Demandbidinfo!a1 text="parameter" rng=Table_of_Contents!b12 text="4" rng=Table_of_Contents!c12 Par=count_demandbidinfo rng=Table_of_Contents!d12
text="demandbidinfo_1" rng=Table_of_Contents!a13 link=Demandbidinfo_1!a1 text="parameter" rng=Table_of_Contents!b13 text="4" rng=Table_of_Contents!c13 Par=count_demandbidinfo_1 rng=Table_of_Contents!d13
text="demandbidinfo_2" rng=Table_of_Contents!a14 link=Demandbidinfo_2!a1 text="parameter" rng=Table_of_Contents!b14 text="4" rng=Table_of_Contents!c14 Par=count_demandbidinfo_2 rng=Table_of_Contents!d14
text="demandbidmap" rng=Table_of_Contents!a15 link=Demandbidmap!a1 text="set" rng=Table_of_Contents!b15 text="2" rng=Table_of_Contents!c15 Par=count_demandbidmap rng=Table_of_Contents!d15
text="Mapping of demand bid identifier to bus (demandbid,bus)" rng=Table_of_Contents!e15

text="demandbid_s" rng=Table_of_Contents!a16 link=Demandbid_s!a1 text="set" rng=Table_of_Contents!b16 text="1" rng=Table_of_Contents!c16 Par=count_demandbid_s rng=Table_of_Contents!d16 text="Set of demandbid info selectors" rng=Table_of_Contents!e16
text="demandbid_t" rng=Table_of_Contents!a17 link=Demandbid_t!a1 text="set" rng=Table_of_Contents!b17 text="1" rng=Table_of_Contents!c17 Par=count_demandbid_t rng=Table_of_Contents!d17 text="Set of demandbid info data types" rng=Table_of_Contents!e17
text="fuelinfo" rng=Table_of_Contents!a18 link=Fuleinfo!a1 text="set" rng=Table_of_Contents!b18 text="2" rng=Table_of_Contents!c18 Par=count_fuelinfo rng=Table_of_Contents!d18
text="fuel_s" rng=Table_of_Contents!a19 link=Fuel_s!a1 text="set" rng=Table_of_Contents!b19 text="1" rng=Table_of_Contents!c19 Par=count_fuel_s rng=Table_of_Contents!d19 text="Set of fuel info selectors" rng=Table_of_Contents!e19
text="fuel_t" rng=Table_of_Contents!a20 link=Fuel_t!a1 text="set" rng=Table_of_Contents!b20 text="1" rng=Table_of_Contents!c20 Par=count_fuel_t rng=Table_of_Contents!d20 text="Set of fuel types" rng=Table_of_Contents!e20
text="gen" rng=Table_of_Contents!a21 link=Gen!a1 text="set" rng=Table_of_Contents!b21 text="1" rng=Table_of_Contents!c21 Par=count_gen rng=Table_of_Contents!d21 text="Set of generators" rng=Table_of_Contents!e21
text="geninfo" rng=Table_of_Contents!a22 link=Geninfo!a1 text="parameter" rng=Table_of_Contents!b22 text="3" rng=Table_of_Contents!c22 Par=count_geninfo rng=Table_of_Contents!d22
text="gen_s" rng=Table_of_Contents!a23 link=Gen_s!a1 text="set" rng=Table_of_Contents!b23 text="1" rng=Table_of_Contents!c23 Par=count_gen_s rng=Table_of_Contents!d23 text="Set of generator info selectors" rng=Table_of_Contents!e23
text="gen_t" rng=Table_of_Contents!a24 link=Gen_t!a1 text="set" rng=Table_of_Contents!b24 text="1" rng=Table_of_Contents!c24 Par=count_gen_t rng=Table_of_Contents!d24 text="Set of generator info data types" rng=Table_of_Contents!e24
text="interface" rng=Table_of_Contents!a25 link=Interface!a1 text="set" rng=Table_of_Contents!b25 text="1" rng=Table_of_Contents!c25 Par=count_interface rng=Table_of_Contents!d25 text="Sets of interfaces to monitor in the network" rng=Table_of_Contents!e25
text="interfaceinfo" rng=Table_of_Contents!a26 link=Interfaceinfo!a1 text="parameter" rng=Table_of_Contents!b26 text="3" rng=Table_of_Contents!c26 Par=count_interfaceinfo rng=Table_of_Contents!d26
text="interfacemap" rng=Table_of_Contents!a27 link=Interfacemap!a1 text="set" rng=Table_of_Contents!b27 text="3" rng=Table_of_Contents!c27 Par=count_interfacemap rng=Table_of_Contents!d27
text="Mapping of interface identifier to buses (interface,from,to)" rng=Table_of_Contents!e27

text="interface_t" rng=Table_of_Contents!a28 link=Interface_t!a1 text="set" rng=Table_of_Contents!b28 text="1" rng=Table_of_Contents!c28 Par=count_interface_t rng=Table_of_Contents!d28 text="Set of interface limit types" rng=Table_of_Contents!e28
text="ints" rng=Table_of_Contents!a29 link=Ints!a1 text="set" rng=Table_of_Contents!b29 text="1" rng=Table_of_Contents!c29 Par=count_ints rng=Table_of_Contents!d29 text="Set of integers for looping and ordering" rng=Table_of_Contents!e29
text="line" rng=Table_of_Contents!a30 link=Line!a1 text="set" rng=Table_of_Contents!b30 text="3" rng=Table_of_Contents!c30 Par=count_line rng=Table_of_Contents!d30 text="Set of lines in the transmission network" rng=Table_of_Contents!e30
text="monitored_lines" rng=Table_of_Contents!a31 link=Monitored_Lines!a1 text="set" rng=Table_of_Contents!b31 text="3" rng=Table_of_Contents!c31 Par=count_monitored_lines rng=Table_of_Contents!d31
text="Set of lines to compute shift factor rows for and enforce line limit constraints" rng=Table_of_Contents!e31

text="prime_mover" rng=Table_of_Contents!a32 link=Prime_mover!a1 text="set" rng=Table_of_Contents!b32 text="1" rng=Table_of_Contents!c32 Par=count_prime_mover rng=Table_of_Contents!d32
text="Set of prime mover types for generators" rng=Table_of_Contents!e32

text="t" rng=Table_of_Contents!a33 link=T!a1 text="set" rng=Table_of_Contents!b33 text="1" rng=Table_of_Contents!c33 Par=count_t rng=Table_of_Contents!d33 text="Set of time periods (up to 24 for UC)" rng=Table_of_Contents!e33
text="total_cost" rng=Table_of_Contents!a34 link=Scalar!a1 text="parameter" rng=Table_of_Contents!b34 text="0" rng=Table_of_Contents!c34 Par=count_total_cost rng=Table_of_Contents!d34
text="transformer" rng=Table_of_Contents!a35 link=Transformer!a1 text="set" rng=Table_of_Contents!b35 text="3" rng=Table_of_Contents!c35 Par=count_transformer rng=Table_of_Contents!d35 text="Set of lines with transformers" rng=Table_of_Contents!e35
text="version" rng=Table_of_Contents!a36 link=Scalar!a1 text="parameter" rng=Table_of_Contents!b36 text="0" rng=Table_of_Contents!c36 Par=count_version rng=Table_of_Contents!d36


*---------------------------------------------------------------------------------------------------------------------
* Make each sheet for wiriting data
*---------------------------------------------------------------------------------------------------------------------

text="TOC" rng=Scalar!a1 link=Table_of_Contents!a1
text="Parameter" rng=Scalar!a2 text="Value" rng=Scalar!b2 text ="Explanatory Text" rng=Scalar!c2
text="baseMVA" rng=Scalar!a3 text="total_cost" rng=Scalar!a4 text="version" rng=Scalar!a5
Par=baseMVA rng=Scalar!b3 Par=total_cost rng=Scalar!b4 Par=version rng=Scalar!b5

text="TOC" rng=Ints!a1 link=Table_of_Contents!a1
text="int" rng=Ints!a2 text="(set)" rng=Ints!b2 text ="Set of integers for looping and ordering" rng=Ints!c2
set=ints Rng=Ints!a1 dim=1 rdim=1

text="TOC" rng=T!a1 link=Table_of_Contents!a1
text="t" rng=T!a2 text="(set)" rng=T!b2 text ="Set of time periods (up to 24 for UC)" rng=T!c2
set=t Rng=T!a4 dim=1 rdim=1

text="TOC" rng=Circuit!a1 link=Table_of_Contents!a1
text="Circuit" rng=Circuit!a2 text="(set)" rng=Circuit!b2 text ="Indices for multiple lines between buses" rng=Circuit!c2
set=circuit values=YN Rng=Circuit!a4 dim=1 rdim=1

text="TOC" rng=Bus!a1 link=Table_of_Contents!a1
text="Bus" rng=Bus!a2 text="(set)" rng=Bus!b2 text ="Set of buses" rng=Bus!c2
set=bus values=YN Rng=Bus!a4 dim=1 rdim=1

text="TOC" rng=Bus_t!a1 link=Table_of_Contents!a1
text="Bus_t" rng=Bus_t!a2 text="(set)" rng=Bus_t!b2 text ="Set of bus info data types" rng=Bus_t!c2
set=bus_t values=YN Rng=Bus_t!a4 dim=1 rdim=1

text="TOC" rng=Bus_s!a1 link=Table_of_Contents!a1
text="Bus_s" rng=Bus_s!a2 text="(set)" rng=Bus_s!b2 text ="Set of bus info selectors" rng=Bus_s!c2
set=bus_s values=YN Rng=Bus_s!a4 dim=1 rdim=1

text="TOC" rng=Businfo!a1 link=Table_of_Contents!a1
text="businfo" rng=Businfo!a2 text="(parameter)" rng=Businfo!b2
text="bus" rng=Businfo!a3 text="bus_t" rng=Businfo!b3 text="bus_s" rng=Businfo!c3 text="Value" rng=Businfo!d3
Par=businfo rng=Businfo!a4 dim=3 rdim=3

text="TOC" rng=Gen!a1 link=Table_of_Contents!a1
text="Gen" rng=Gen!a2 text="(set)" rng=Gen!b2 text ="Set of generators" rng=Bus!c2
set=gen values=YN Rng=Gen!a4 dim=1 rdim=1

text="TOC" rng=Gen_t!a1 link=Table_of_Contents!a1
text="gen_t" rng=Gen_t!a2 text="(set)" rng=Gen_t!b2 text ="Set of generator info data types" rng=Gen_t!c2
set=gen_t values=YN Rng=Gen_t!a4 dim=1 rdim=1

text="TOC" rng=Gen_s!a1 link=Table_of_Contents!a1
text="gen_s" rng=Gen_s!a2 text="(set)" rng=Gen_s!b2 text ="Set of generator info selectors" rng=Gen_s!c2
set=gen_s values=YN Rng=Gen_s!a4 dim=1 rdim=1

text="TOC" rng=Geninfo!a1 link=Table_of_Contents!a1
text="geninfo" rng=Geninfo!a2 text="(parameter)" rng=Geninfo!b2
text="gen" rng=Geninfo!a3 text="gen_t" rng=Geninfo!b3 text="gen_s" rng=Geninfo!c3 text="Value" rng=Geninfo!d3
Par=geninfo rng=Geninfo!a4 dim=3 rdim=3

text="TOC" rng=Line!a1 link=Table_of_Contents!a1
text="Line" rng=Line!a2 text="(set)" rng=Line!b2 text="Set of lines in the transmission network" rng=Line!c2
text="bus" rng=Line!a3 text="bus" rng=Line!b3 text="circuit" rng=Line!c3
set=line values=YN rng=Line!a4 dim=3 rdim=3

text="TOC" rng=Monitored_Lines!a1 link=Table_of_Contents!a1
text="Monitored_Lines" rng=Monitored_Lines!a2 text="(set)" rng=Monitored_Lines!b2 text="Set of lines to compute shift factor rows for and enforce line limit constraints" rng=Monitored_Lines!c2
text="bus" rng=Monitored_Lines!a3 text="bus" rng=Monitored_Lines!b3 text="circuit" rng=Monitored_Lines!c3
set=line values=YN rng=Monitored_Lines!a4 dim=3 rdim=3

text="TOC" rng=Branch_t!a1 link=Table_of_Contents!a1
text="branch_t" rng=Branch_t!a2 text="(set)" rng=Branch_t!b2 text ="Set of branch info data types" rng=Branch_t!c2
set=branch_t values=YN Rng=Branch_t!a4 dim=1 rdim=1

text="TOC" rng=Branch_s!a1 link=Table_of_Contents!a1
text="branch_s" rng=Branch_s!a2 text="(set)" rng=Branch_s!b2 text ="Set of branch info selectors" rng=Branch_s!c2
set=branch_s values=YN Rng=Branch_s!a4 dim=1 rdim=1

text="TOC" rng=Branchinfo!a1 link=Table_of_Contents!a1
text="Branchinfo" rng=Branchinfo!a2 text="(parameter)" rng=Branchinfo!b2
text="bus" rng=Branchinfo!a3 text="bus" rng=Branchinfo!b3 text="circuit" rng=Branchinfo!c3 text="branch_t" rng=Branchinfo!d3 text="branch_s" rng=Branchinfo!e3 text="Value" rng=Branchinfo!f3
Par=branchinfo rng=Branchinfo!a4 dim=5 rdim=5

text="TOC" rng=Gen_s!a1 link=Table_of_Contents!a1
text="gen_s" rng=Gen_s!a2 text="(set)" rng=Gen_s!b2 text ="Set of generator info selectors" rng=Gen_s!c2
set=gen_s values=YN Rng=Gen_s!a4 dim=1 rdim=1

text="TOC" rng=Demandbid!a1 link=Table_of_Contents!a1
text="Demandbid" rng=Demandbid!a2 text="(set)" rng=Demandbid!b2
set=demandbid rng=Demandbid!a4 dim=1 rdim=1

text="TOC" rng=Demandbidmap!a1 link=Table_of_Contents!a1
text="Demandbidmap" rng=Demandbidmap!a2 text="(set)" rng=Demandbidmap!b2 text="Mapping of demand bid identifier to bus (demandbid,bus)" rng=Demandbidmap!c2
text="demandbid" rng=Demandbidmap!a3 text="bus" rng=Demandbidmap!b3
set=demandbidmap rng=Demandbidmap!a4 dim=2 rdim=2

text="TOC" rng=Demandbid_t!a1 link=Table_of_Contents!a1
text="Demandbid_t" rng=Demandbid_t!a2 text="(set)" rng=Demandbid_t!b2 text="Set of demandbid info data types" rng=Demandbid_t!c2
set=demandbid_t rng=Demandbid_t!a4 dim=1 rdim=1

text="TOC" rng=Demandbid_s!a1 link=Table_of_Contents!a1
text="Demandbid_s" rng=Demandbid_s!a2 text="(set)" rng=Demandbid_s!b2 text="Set of demandbid info selectors" rng=Demandbid_s!c2
set=demandbid_s rng=Demandbid_s!a4 dim=1 rdim=1

text="TOC" rng=Demandbidinfo!a1 link=Table_of_Contents!a1
text="Demandbidinfo" rng=Demandbidinfo!a2 text="(parameter)" rng=Demandbidinfo!b2
text="demandbid" rng=Demandbidinfo!a3 text="t" rng=Demandbidinfo!b3 text="demandbid_t" rng=Demandbidinfo!c3 text="demandbid_s" rng=Demandbidinfo!d3 text="Value" rng=Demandbidinfo!e3
Par=demandbidinfo rng=Demandbidinfo!a4 dim=4 rdim=4

text="TOC" rng=Demandbidinfo_1!a1 link=Table_of_Contents!a1
text="Demandbidinfo_1" rng=Demandbidinfo_1!a2 text="(parameter)" rng=Demandbidinfo_1!b2
text="demandbid" rng=Demandbidinfo_1!a3 text="t" rng=Demandbidinfo_1!b3 text="demandbid_t" rng=Demandbidinfo_1!c3 text="demandbid_s" rng=Demandbidinfo_1!d3 text="Value" rng=Demandbidinfo_1!e3
Par=demandbidinfo_1 rng=Demandbidinfo_1!a4 dim=4 rdim=4

text="TOC" rng=Demandbidinfo_2!a1 link=Table_of_Contents!a1
text="Demandbidinfo_2" rng=Demandbidinfo_2!a2 text="(parameter)" rng=Demandbidinfo_2!b2
text="demandbid" rng=Demandbidinfo_2!a3 text="t" rng=Demandbidinfo_2!b3 text="demandbid_t" rng=Demandbidinfo_2!c3 text="demandbid_s" rng=Demandbidinfo_2!d3 text="Value" rng=Demandbidinfo_2!e3
Par=demandbidinfo_2 rng=Demandbidinfo_2!a4 dim=4 rdim=4

text="TOC" rng=Fuel_t!a1 link=Table_of_Contents!a1
text="Fuel_t" rng=Fuel_t!a2 text="(set)" rng=Fuel_t!b2 text="Set of fuel types" rng=Fuel_t!c2
set=fuel_t rng=Fuel_t!a4 dim=1 rdim=1

text="TOC" rng=Fuel_s!a1 link=Table_of_Contents!a1
text="Fuel_s" rng=Fuel_s!a2 text="(set)" rng=Fuel_s!b2 text="Set of fuel info selectors" rng=Fuel_s!c2
set=fuel_s rng=Fuel_s!a4 dim=1 rdim=1

text="TOC" rng=Fuelinfo!a1 link=Table_of_Contents!a1
text="Fuelinfo" rng=Fuelinfo!a2 text="(parameter)" rng=Fuelinfo!b2
text="fuel_t" rng=Fuelinfo!a3 text="fuel_s" rng=Fuelinfo!b3 text="Value" rng=Fuelinfo!c3
Par=fuelinfo rng=Fuelinfo!a4 dim=2 rdim=2

text="TOC" rng=Interface!a1 link=Table_of_Contents!a1
text="Interface" rng=Interface!a2 text="(set)" rng=Interface!b2 text="Set of interfaces to monitor in the transmission network" rng=Interface!c2
set=interface rng=Interface!a4 dim=1 rdim=1

text="TOC" rng=Interfacemap!a1 link=Table_of_Contents!a1
text="Interfacemap" rng=Interfacemap!a2 text="(set)" rng=Interfacemap!b2 text="Mapping of interface identifier to buses (interface,from,to)" rng=Interfacemap!c2
text="interface" rng=Interfacemap!a3 text="bus" rng=Interfacemap!b3 text="bus" rng=Interfacemap!c3
set=interfacemap rng=Interfacemap!a4 dim=3 rdim=1

text="TOC" rng=Interface_t!a1 link=Table_of_Contents!a1
text="Interface_t" rng=Interface_t!a2 text="(set)" rng=Interface_t!b2 text="Set of interface limit types" rng=Interface_t!c2
set=interface_t rng=Interface_t!a4 dim=1 rdim=1

text="TOC" rng=Interfaceinfo!a1 link=Table_of_Contents!a1
text="Interfaceinfo" rng=Interfaceinfo!a2 text="(parameter)" rng=Interfaceinfo!b2
text="interface" rng=Interfaceinfo!a3 text="t" rng=Interfaceinfo!b3 text="interface_t" rng=Interfaceinfo!c3 text="Value" rng=Interfaceinfo!d3
Par=interfaceinfo rng=Interfaceinfo!a4 dim=3 rdim=3

text="TOC" rng=Prime_mover!a1 link=Table_of_Contents!a1
text="Prime_mover" rng=Prime_mover!a2 text="(set)" rng=Prime_mover!b2 text="Set of prime mover types for generators" rng=Prime_mover!c2
set=prime_mover rng=Prime_mover!a4 dim=1 rdim=1

text="TOC" rng=Transformer!a1 link=Table_of_Contents!a1
text="Transformer" rng=Transformer!a2 text="(set)" rng=Transformer!b2 text="Set of lines with transformers" rng=Transformer!c2
text="bus" rng=Transformer!a3 text="bus" rng=Transformer!b3 text="circuit" rng=Transformer!c3
set=transformer rng=Transformer!a4 dim=3 rdim=3


$offecho
execute 'gdxxrw @recommands.txt';
execute "rm %filepath%excel_temp.gdx"


