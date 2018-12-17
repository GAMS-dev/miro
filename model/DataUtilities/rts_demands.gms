$if not set in $abort "No input file given!"
$setnames "%in%" inpath inname inextension

sets
    ints,
    t "Set of time periods (up to 24 for UC)" /1*24/,
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
alias(bus,i,j);
alias(circuit,c);

parameters version, baseMVA, total_cost;
parameters businfo, geninfo,
           fuelinfo,
           branchinfo,
           demandbidinfo,
           interfaceinfo;

$GDXIN %in%
$LOAD version, baseMVA, total_cost
$LOAD ints, bus, gen, circuit, line, transformer, monitored_lines,
$LOAD bus_t, bus_s, gen_t, gen_s, branch_t, branch_s,
$LOAD demandbid_t, demandbid_s, interface_t
$LOAD fuel_t, fuel_s, prime_mover
$LOAD businfo, geninfo, branchinfo, fuelinfo
$LOAD demandbid, demandbidmap, demandbidinfo
$LOAD interface, interfacemap, interfaceinfo
$GDXIN
bus_s(t) = yes;
gen_s(t) = yes;
branch_s(t) = yes;
demandbid_s(t) = yes;

*======= SECTION: Setup data
parameters
    origPd(bus)
    origQd(bus)
;
origPd(bus) = businfo(bus,'Pd','1');
origQd(bus) = businfo(bus,'Qd','1');
    
sets
    season "Seasons" /winter, summer, spring / 
    daytype "Weekday or weekend" /wday, wend /
;

table hourlyload(t, season, daytype) "Hourly load percentages (out of 100)"
	winter.wday	winter.wend	summer.wday	summer.wend	spring.wday  spring.wend
1	67	        78	        64        	74	        63	      75
2	63	        72	        60        	70	        62	      73
3	60	        68	        58        	66	        60	      69
4	59	        66	        56	        65	        58	      66
5	59	        64	        56	        64	        59	      65
6	60	        65	        58	        62	        65	      65
7	74	        66	        64	        62	        72	      68
8	86	        70	        76	        66	        85	      74
9	95	        80	        87	        81	        95	      83
10	96	        88	        95	        86	        99	      89
11	96	        90	        99	        91	        100	      92
12	95	        91	        100	        93	        99	      94
13	95	        90	        99	        93	        93 	      91
14	95	        88	        100	        92	        92	      90
15	93	        87	        100	        91	        90	      90
16	94	        87	        97	        91	        88	      86
17	99	        91	        96	        92	        90	      85
18	100	        100	        96	        94	        92	      88
19	100	        99	        93	        95	        96	      92
20	96	        97	        92	        95	        98	      100
21	91	        94	        92	        100	        96 	      97
22	83	        92	        93	        93	        90	      95
23	73	        87	        87	        88	        80	      90
24	63	        81	        72	        80	        70	      85;

*======= SECTION: Data calculation and file output
*--- Define output file
file
    fx1
    fx2 "Output file handle"
;

put fx1;
put_utility 'ren' /'rtslist.out'
   
*loop((season,daytype)$(ord(season) eq 1),
loop((season,daytype),
* Calculate new information
    businfo(bus,'Pd',t) = 1.1*origPd(bus) * (hourlyload(t,season,daytype)/100);
    businfo(bus,'Qd',t) = 1.1*origQd(bus) * (hourlyload(t,season,daytype)/100);
    businfo(bus,'Vm',t) = 1;
    businfo(bus,'Va',t) = 0;

    geninfo(gen,'status',t) = geninfo(gen,'status','1');
    branchinfo(i,j,c,'branchstatus',t) = branchinfo(i,j,c,'branchstatus','1');

    geninfo(gen,'MTTF','given') = 2940$(geninfo(gen,'Pmax','given') eq 12)
                             +  450$(geninfo(gen,'Pmax','given') eq 20)
                             + 1980$(geninfo(gen,'Pmax','given') eq 50)
                             + 1960$(geninfo(gen,'Pmax','given') eq 76)
                             + 1200$(geninfo(gen,'Pmax','given') eq 100)
                             +  960$(geninfo(gen,'Pmax','given') eq 155)
                             +  950$(geninfo(gen,'Pmax','given') eq 197)
                             + 1150$(geninfo(gen,'Pmax','given') eq 350)
                             + 1100$(geninfo(gen,'Pmax','given') eq 400);

    geninfo(gen,'MTTR','given') =  60$(geninfo(gen,'Pmax','given') eq 12)
                             +  50$(geninfo(gen,'Pmax','given') eq 20)
                             +  20$(geninfo(gen,'Pmax','given') eq 50)
                             +  40$(geninfo(gen,'Pmax','given') eq 76)
                             +  50$(geninfo(gen,'Pmax','given') eq 100)
                             +  40$(geninfo(gen,'Pmax','given') eq 155)
                             +  50$(geninfo(gen,'Pmax','given') eq 197)
                             + 100$(geninfo(gen,'Pmax','given') eq 350)
                             + 150$(geninfo(gen,'Pmax','given') eq 400);
			     
* Output information to file
    put fx2;
    put_utility 'gdxout' /'%inpath%%inname%_' season.tl:0 '_' daytype.tl:0
    execute_unload version, total_cost, baseMVA, ints, t, bus, gen, circuit,
        line, transformer, monitored_lines, demandbid, demandbidmap, interface, interfacemap
        bus_t, bus_s, gen_t, gen_s, branch_t, branch_s, fuel_t, fuel_s, prime_mover
        demandbid_t, demandbid_s, interface_t
        businfo, geninfo, branchinfo, demandbidinfo, interfaceinfo, fuelinfo;

    put fx1;
    put_utility 'inc' /'rtslist.out'
*    put '%inpath%%inname%_' season.tl:0 '_' daytype.tl:0 ;
    put '%inname%_' season.tl:0 '_' daytype.tl:0 ;
*    execute 'gams save_domain_info.gms gdx=set.tn:0.gdx --in="rts96temp.gdx"';
*$if errorlevel 1 abort 'Call failed!'
*    execute 'rm rts96temp.gdx';
);
