$title "Power System GDX data file maker"

$if not set in $abort "No input file given."
$if not set lmp $abort "No LMP info given."
$if not set filepath $setnames "%gams.i%" filepath filename fileextension
$if not set out $set out "%in%"
$if not set ac $set ac 1

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
parameters businfo(bus,*,*), geninfo(gen,*,*),
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
$LOAD demandbid=bus, demandbidmap, demandbidinfo
$LOAD interface, interfacemap, interfaceinfo
$GDXIN

*---- Declaration section
set
    demandpts(demandbid_s) /1*10/
    validbid(demandbid,t)
    hasbid(demandbid)
;
parameters
    Pd(bus,t) "Pd values"
    LMP(demandbid,t) "LMP values"
    plen(t) "length of segment"
    prev_demandbid_price(t) "Prev price breakpoint ($/MW/h)"
    prev_demandbid_qty(t) "Prev q breakpoint (MW)"
    cur_demandbid_price(t) "Demanbid price/unit for current piece ($/MW/h)"

    lmpinfo(bus,*,*)
;

*--- Setup info
execute_load "%lmp%" lmpinfo=businfo;

sets
    whicht(t) "If lmp only exists for 1 specific time period"
;

whicht(t) = 1$(sum(bus, abs(lmpinfo(bus,'LMP',t))) > 0);
Pd(bus,t)  = businfo(bus,'Pd',t)$whicht(t);
display whicht;

* Find valid bids
validbid(demandbid,t)$whicht(t) = 1$(sum(bus$((ord(bus) eq ord(demandbid)) and (Pd(bus,t) > 0)), 1) >0);
option hasbid < validbid;

* If Pd=0, no demandbid information.
demandbidmap(demandbid,bus)$(ord(demandbid) eq ord(bus)) = yes;
demandbidmap(demandbid,bus)$(not(hasbid(demandbid))) = no;

* Extract LMP info
LMP(demandbid,t)$validbid(demandbid,t) = sum(bus$(demandbidmap(demandbid,bus)), lmpinfo(bus,'LMP',t));


loop(demandbid$hasbid(demandbid),
* Loop if it is a valid bid
    demandbidinfo(demandbid,t,"NumBids","given")$validbid(demandbid,t) = 10;
    
* Maximum demandbid quantity is 7% of Pd(bus)
    plen(t)$validbid(demandbid,t) = sum(bus$demandbidmap(demandbid,bus), Pd(bus,t))*0.07/9;
    prev_demandbid_price(t)$validbid(demandbid,t) = 0;
    
    loop(demandpts,
* Update price in step function. Max impact of demandbid is ~3.9% of Pd*LMP
	cur_demandbid_price(t)$validbid(demandbid,t) = LMP(demandbid,t) * (11-ord(demandpts))/9;

*	Calculate next (q,p) breakpoint. Store information in gdx format.
	demandbidinfo(demandbid,t,"Quantity",demandpts)$validbid(demandbid,t) = plen(t)*(ord(demandpts)-1);
	if(ord(demandpts) > 1,
	    demandbidinfo(demandbid,t,"Price",demandpts)$validbid(demandbid,t) =
		prev_demandbid_price(t) + plen(t)*cur_demandbid_price(t);
	else
	    demandbidinfo(demandbid,t,"Price",demandpts)$validbid(demandbid,t) = 0;
	);

*	Save information for next loop
	prev_demandbid_price(t)$validbid(demandbid,t) = demandbidinfo(demandbid,t,"Price",demandpts);	
    );
);


parameters
    perc(demandbid,t)
    pdval(demandbid,t)
    perc2(demandbid,t)
;

pdval(demandbid,t)$validbid(demandbid,t) = sum(bus$demandbidmap(demandbid,bus), Pd(bus,t) );
perc(demandbid,t)$validbid(demandbid,t) =
    smax(demandpts, demandbidinfo(demandbid,t,"Price",demandpts)/(LMP(demandbid,t)*pdval(demandbid,t) ));
perc2(demandbid,t)$validbid(demandbid,t) = smax(demandpts, demandbidinfo(demandbid,t,"Quantity",demandpts))/pdval(demandbid,t);
display perc, perc2;

execute_unload "%out%temp", version, total_cost, baseMVA, ints, t, bus, gen, circuit,
                        line, transformer, monitored_lines, demandbid, demandbidmap, interface, interfacemap
                        bus_t, bus_s, gen_t, gen_s, branch_t, branch_s, fuel_t, fuel_s, prime_mover
                        demandbid_t, demandbid_s, interface_t
                        businfo, geninfo, branchinfo, demandbidinfo, interfaceinfo, fuelinfo
;

execute 'gams "%filepath%save_domain_info.gms" gdx="%out%" --in="%out%temp"'
if(errorlevel ne 0, abort "Save domain info failed!");
execute 'rm "%out%temp.gdx"'
if(errorlevel ne 0, abort "Call failed!");
