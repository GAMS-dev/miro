$if not set in $exit
$setnames "%in%" inpath inname inextension
$setnames "%gams.i%" filepath filename fileextension

$if not set out $set out "%inpath%%inname%.m"
$if not set limits $set limits "given"
$if not set timeperiod $set timeperiod 1
$if not set shiftmatrix $set shiftmatrix "%inpath%%inname%_Shift_Matrix.gdx"
$if %all% == 1 $set PRDemand 1
$if %all% == 1 $set Dcurve 1
$if %all% == 1 $set Sbus 1

set costcoefset /0*20/,
    demandcoefset /1*10/,
    rev(costcoefset,costcoefset);
alias(costcoefset,ccs);

set ints;
sets bus, gen, circuit, line, monitored_lines;
parameter baseMVA;
alias(bus,i,j);
alias(circuit,c);

sets bus_t, bus_s, gen_t, gen_s, branch_t, branch_s;
parameters businfo, geninfo, branchinfo, type;
parameter S(i,j,c,bus);
parameter max_pts;

* We only need the "up" ramp rate since Matpower doesn't separate up/down
$GDXIN %in%
$LOAD baseMVA, ints, bus, gen, circuit, line, monitored_lines
$LOAD bus_t, bus_s, gen_t, gen_s, branch_t, branch_s
$LOAD businfo, geninfo, branchinfo
$GDXIN

$ifthen %Sbus% == 1
$GDXIN %shiftmatrix%
$LOAD S
$GDXIN
$endif

type(bus) = businfo(bus,'type','given');
file o /'%out%'/;
o.pw = 6000;
o.pc = 8;
put o;
put 'function mpc = %inname%'/;
put '%% File written by to_matpower.gms'/;
put "mpc.version = '2';"/;
put 'mpc.baseMVA = ', baseMVA::6, ';'/;
put 'mpc.bus = ['/;
parameter temp;
loop(bus,
  temp = max(type(bus), 1);
  put ' ', bus.val::0,  temp::0,  businfo(bus,'Pd','%timeperiod%')::6,  businfo(bus,'Qd','%timeperiod%')::6,
      businfo(bus,'Gs','given')::6,  businfo(bus,'Bs','given')::6,  businfo(bus,'area','given')::0,  businfo(bus,'Vm','%timeperiod%')::6,
      businfo(bus,'Va','%timeperiod%')::6,  businfo(bus,'baseKV','given')::6,  businfo(bus,'zone','given')::0,
      businfo(bus,'maxVM','given')::6,  businfo(bus,'minVM','given')::6, ';'
  /;
);
put '];'//;
put 'mpc.gen = ['/;
loop(gen,
  put ' ', geninfo(gen,'atBus','given')::0,  geninfo(gen,'Pg','%timeperiod%')::6, geninfo(gen,'Qg','%timeperiod%')::6,
      geninfo(gen,'Qmax','given')::6, geninfo(gen,'Qmin','given')::6, geninfo(gen,'Vg','given')::6, 
      geninfo(gen,'mBase','given')::6, geninfo(gen,'status','%timeperiod%')::0, geninfo(gen,'Pmax','given')::6,
      geninfo(gen,'Pmin','given')::6, geninfo(gen,'Pc1','given')::6, geninfo(gen,'Pc2','given')::6,
      geninfo(gen,'Qc1min','given')::6, geninfo(gen,'Qc1max','given')::6, geninfo(gen,'Qc2min','given')::6,
      geninfo(gen,'Qc2max','given')::6, geninfo(gen,'RampUp','given')::6, geninfo(gen,'RampUp10','given')::6,
      geninfo(gen,'RampUp30','given')::6;
      if(geninfo(gen,'RampUpReactive','given') eq inf,
	  put 'inf';
      else
          put geninfo(gen,'RampUpReactive','given')::6;
      );
      put geninfo(gen,'APF','given')::6, ';'
  /;
);
put '];'//;
put 'mpc.branch = ['/;
loop((i,j,c)$(line(i,j,c) and (type(i) ne 4) and (type(j) ne 4)),
  put ' ', i.val::0, j.val::0, branchinfo(i,j,c,'r','given')::6, branchinfo(i,j,c,'x','given')::6,
      branchinfo(i,j,c,'bc','given')::6, branchinfo(i,j,c,'rateA','%limits%')::6, branchinfo(i,j,c,'rateB','%limits%')::6,
      branchinfo(i,j,c,'rateC','%limits%')::6, branchinfo(i,j,c,'ratio','given')::6, branchinfo(i,j,c,'angle','given')::6,
      branchinfo(i,j,c,'branchstatus','%timeperiod%')::0, branchinfo(i,j,c,'minAngleDiff','given')::6,
      branchinfo(i,j,c,'maxAngleDiff','given')::6, c.val::0, ';'
  /;
);
put '];'//;
put 'mpc.gencost = ['/;
max_pts = max(smax(gen$(geninfo(gen,'costmodel','given') eq 1), geninfo(gen,'numcostpts','given')),
              smax(gen$(geninfo(gen,'costmodel','given') eq 2), geninfo(gen,'numcostcoef','given')));
loop(gen,
  put ' ', geninfo(gen,'costmodel','given')::0, geninfo(gen,'startupcost','given')::6,
      geninfo(gen,'shutdowncost','given')::6;
  if((geninfo(gen,'costmodel','given') eq 1),
    put geninfo(gen,'numcostpts','given')::0,;
    loop(costcoefset$(ord(costcoefset) le max_pts),
        put geninfo(gen,'costpts_x',costcoefset+1)::6, geninfo(gen,'costpts_y',costcoefset+1)::6;
    );
  elseif(geninfo(gen,'costmodel','given') eq 2),
    rev(ccs,costcoefset) = no;
    rev(ccs,ccs+[geninfo(gen,'numcostcoef','given')-2*ord(ccs)+1]) = yes;
    put geninfo(gen,'numcostcoef','given')::0,;
    loop(rev(costcoefset,ccs)$(ord(costcoefset) le geninfo(gen,'numcostcoef','given')),
        put geninfo(gen,'costcoef',ccs)::6;
    );
    loop(costcoefset$(ord(costcoefset) le (max_pts - geninfo(gen,'numcostcoef','given'))),
        put 0;
    );
  );
  put ';'/;
);
put '];'//;

put '% Generator reactive power D-curve parameters'/;
put '% S_max nameplate_pf min_pf Qfield Rfield Qend Rend'/;
put 'mpc.gendcurve = ['/;
loop(gen,
  put ' ', geninfo(gen,'R_max','given')::6, geninfo(gen,'nameplate_pf','given')::6, geninfo(gen,'min_pf','given')::6;
  if ( (geninfo(gen,'Qfield','given') eq 0) or (geninfo(gen,'Qfield','given') eq NA),
      put 'nan nan';
  else
      put geninfo(gen,'Qfield','given')::6, geninfo(gen,'Rfield','given')::6;);
  if ((geninfo(gen,'Qend','given') eq 0) or (geninfo(gen,'Qend','given') eq NA),
      put 'nan nan';
  else
      put geninfo(gen,'Qend','given')::6, geninfo(gen,'Rend','given')::6;);
  put ';'/;
);
put '];'//;

$ontext This needs to be changed with new demand bid handling
$ifthen %PRDemand% == 1
put '% List of price-responsive demand curve points:'/;
put '% numdemandpts price0 Pd0 Qd0 price1 Pd1 Qd1 ...'/;
put 'mpc.prdemand = ['/;
loop(bus$businfo(bus,'Pd','given'),
  put ' ', businfo(bus,'numdemandpts','given');
  loop(demandcoefset$(ord(demandcoefset) le businfo(bus,'numdemandpts','given')),
      put businfo(bus,'demandpts_price',demandcoefset)::6, businfo(bus,'demandpts_Pd',demandcoefset)::6, businfo(bus,'demandpts_Qd',demandcoefset)::6;
  );
  put ';'/;
);
put '];'//;
$endif
$offtext

$ifthen %Sbus% == 1
put 'mpc.sbus = ['/;
loop((i,j,c)$monitored_lines(i,j,c),
  put ' ', i.val::0, j.val::0;
  loop(bus,
    put S(i,j,c,bus)::6;
  );
  put ';'/;
);
put '];'//;
$endif

putclose o;

* Deal with GAMS' inability to write lines longer than 255 characters and GAMS' version of sed's lack of in-place option...
* This doesn't work -- we'll just have to do postprocessing OUTSIDE of gams, which can only be done easily in *NIX.
*execute "cp '%out%' '%out%_temp' ; until [ 0 -eq `diff '%out%' '%out%_temp' | wc -l` ] ; do mv '%out%_temp' '%out%' ; sed ':a;N;$!ba;s/\([0123456789.]\)\n/\1 /g' '%out%' > '%out%_temp' ; echo wc -l diff '%out%' '%out%_temp'; done";
*execute 'rm -f "%out%_temp"';
*execute "%filepath%/fix_matpower_files.sh '%out%'"
*if(errorlevel ne 0, abort "Call failed!");

