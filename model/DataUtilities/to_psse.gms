$if not set in $exit
$setnames "%in%" filepath filename fileextension

$if not set out $set out "%filepath%%filename%.raw"
$if not set limits $set limits "given"
$if not set timeperiod $set timeperiod 1

sets    shunt_set /1*40/, bus, gen, circuit, line;
parameter  baseMVA;
alias(bus,i,j);
alias(circuit,c);

sets bus_t, bus_s, gen_t, gen_s, branch_t, branch_s;
parameters businfo, geninfo, branchinfo;

$GDXIN %in%
$LOAD baseMVA, bus, gen, circuit, line
$LOAD bus_t, bus_s, gen_t, gen_s, branch_t, branch_s
$LOAD businfo, geninfo, branchinfo
$GDXIN

$setnames "%in%" inpath inname inextension
parameter y,m,d,n;
n = Jnow;
y = gyear(n);
m = gmonth(n);
d = gday(n);

file o /"%out%"/;
o.pw = 10000;
o.pc = 8;
put o;
put " 0, ", baseMVA::6/;
put "%inname%.raw / File written by to_psse.gms"/;
put m::0, "/", d::0, "/", y::0/;
loop(bus,
  put " ", bus.val::0, ", '", bus.val::0, "', ", businfo(bus,'baseKV','given')::6, ", ",
      businfo(bus,'type','given')::0, ", ", businfo(bus,'Gs','given')::6, ", ", businfo(bus,'Bs','given')::6, ", ",
      businfo(bus,'area','given')::0, ", ", businfo(bus,'zone','given')::0, ", ", businfo(bus,'Vm','%timeperiod%')::6, ", ", businfo(bus,'Va','%timeperiod%')::6
  /;
);
put "0 / END OF BUS DATA, BEGIN LOAD DATA"/;
loop(bus,
  put " ", bus.val::0, ", '1 ', 1, ", businfo(bus,'area','given')::0, ", ", businfo(bus,'zone','given')::0, ", ", 
      businfo(bus,'Pd','%timeperiod%')::6, ", ", businfo(bus,'Qd','%timeperiod%')::6, ", 0, 0, 0, 0, 1, 1"
  /;  
);
put "0 / END OF LOAD DATA, BEGIN FIXED SHUNT DATA"/;
put "0 / END OF FIXED SHUNT DATA DATA, BEGIN GENERATOR DATA"/;
* Ramp rates don"t have a place in PSSE raw?
loop(gen,
  put " ", geninfo(gen,'atBus','given')::0, ", ", gen.ord::0, ", ", geninfo(gen,'Pg','%timeperiod%')::6, ", ", geninfo(gen,'Qg','%timeperiod%')::6, ", ", 
      geninfo(gen,'Qmax','given')::6, ", ", geninfo(gen,'Qmin','given')::6, ", ", geninfo(gen,'Vg','given')::6, ", 0, ", 
      geninfo(gen,'mBase','given')::6, ", 0, 1, 0, 0, 1, ", geninfo(gen,'status','%timeperiod%')::0, ", 100, ",
      geninfo(gen,'Pmax','given')::6, ", ", geninfo(gen,'Pmin','given')::6, ", 1, 1"
  /;
);
put "0 / END OF GENERATOR DATA, BEGIN BRANCH DATA"/;
loop((i,j,c)$(line(i,j,c) and (branchinfo(i,j,c,'ratio','given') eq 1) and (branchinfo(i,j,c,'angle','given') eq 0)),
  put " ", i.val::0, ", ", j.val::0, ", ", c.val::0, ", ",
      branchinfo(i,j,c,'r','given')::6, ", ", branchinfo(i,j,c,'x','given')::6, ", ", branchinfo(i,j,c,'bc','given')::6, ", ", 
      branchinfo(i,j,c,'rateA',"%limits%")::6, ", ", branchinfo(i,j,c,'rateB',"%limits%")::6, ", ", branchinfo(i,j,c,'rateC',"%limits%")::6, ", ", 
      "0, 0, 0, 0, ", branchinfo(i,j,c,'branchstatus','%timeperiod%')::0, ", 0, 1, 1"
  /; 
);
put "0 / END OF BRANCH DATA, BEGIN TRANSFORMER DATA"/;
* Angle should still be in degrees in the GDX files?
loop((i,j,c)$(line(i,j,c) and ((branchinfo(i,j,c,'ratio','given') ne 1) or (branchinfo(i,j,c,'angle','given') ne 0))),
  put " ", i.val::0, ", ", j.val::0, ", 0, ", c.val::0, ", 1, 1, 1, 0, 0, 2, '            ', ",
      branchinfo(i,j,c,'branchstatus','%timeperiod%')::0, ", 1, 1"
      /
      "  ", branchinfo(i,j,c,'r','given')::6, ", ", branchinfo(i,j,c,'x','given')::6, ", ", baseMVA::6
      /
      "  ", branchinfo(i,j,c,'ratio','given')::6, ", 0, ", branchinfo(i,j,c,'angle','given')::6, ", ",
      branchinfo(i,j,c,'rateA',"%limits%")::6, ", ", branchinfo(i,j,c,'rateB','%limits%')::6, ", ", branchinfo(i,j,c,'rateC','%limits%')::6, ", 0, 0, ",
      businfo(i,'maxVM','given'), ", ", businfo(i,'minVM','given'), ", ", businfo(i,'maxVM','given'), ", ", businfo(i,'minVM','given'), ", 33, 0, 0, 0"
      /
      "   1.000000, 0" 
  /;
);
put "0 / END OF TRANSFORMER DATA, BEGIN AREA DATA"/;
* Area data not implemented -- can we just leave this out?
put "0 / END OF AREA DATA, BEGIN TWO-TERMINAL DC DATA"/;
put "0 / END OF TWO-TERMINAL DC DATA, BEGIN VSC DC LINE DATA"/;
put "0 / END OF VSC DC LINE DATA, BEGIN SWITCHED SHUNT DATA"/;
loop(bus$(businfo(bus,'numswitchedshunts','given') > 0),
    put " ", bus.val::0, ", 0, ", businfo(bus,'maxVm','given')::5, ", ",
        businfo(bus,'minVm','given')::5, ", 0, ", businfo(bus,'switchedBs','given')::5;
    loop(shunt_set$(ord(shunt_set) le businfo(bus,'numswitchedshunts','given')),
        put ", ", businfo(bus,'switchedelements',shunt_set)::0, ", ", businfo(bus,'switchedBs',shunt_set)::5;
    );
    put /;
);
put "0 / END OF SWITCHED SHUNT DATA, BEGIN IMPEDANCE CORRECTION DATA"/;
put "0 / END OF IMPEDANCE CORRECTION DATA, BEGIN MULTI-TERMINAL DC DATA"/;
put "0 / END OF MULTI-TERMINAL DC DATA, BEGIN MULTI-SECTION LINE DATA"/;
put "0 / END OF MULTI-SECTION LINE DATA, BEGIN ZONE DATA"/;
put "0 / END OF ZONE DATA, BEGIN INTER-AREA TRANSFER DATA"/;
* Zone data not implemented -- can we just leave this out?
put "0 / END OF INTER-AREA TRANSFER DATA, BEGIN OWNER DATA"/;
put "0 / END OF OWNER DATA, BEGIN FACTS CONTROL DEVICE DATA"/;
put "0 / END OF FACTS CONTROL DEVICE DATA"/;
* Generator cost data not included in PSSE format?
putclose o;

