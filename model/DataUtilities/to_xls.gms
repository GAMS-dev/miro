$if not set in $exit
$setnames "%in%" filepath filename fileextension

$if not set out $set out "%filepath%%filename%.xls"
$if not set limits $set limits "given"

sets costcoefset /1*40/, bus, gen, circuit, line;
parameter  baseMVA;
alias(bus,i,j);
alias(circuit,c);

* We won't be writing any 3-winding transformers because it's
* too hard to go through and find the imaginary star bus if one exists.
set translines /1*4/;
alias(translines,tl);
parameters busmatrix(bus,*), loadmatrix(bus,*), genmatrix(gen,*),
           branchmatrix(i,j,c,*), transmatrix(i,j,c,translines,*);

sets bus_t, bus_s, gen_t, gen_s, branch_t, branch_s;
parameters businfo(bus,bus_t,bus_s), geninfo(gen,gen_t,gen_s), branchinfo(i,j,c,branch_t,branch_s);


$GDXIN %in%
$LOAD baseMVA, bus, gen, circuit, line
$LOAD bus_t, bus_s, gen_t, gen_s, branch_t, branch_s
$LOAD businfo, geninfo, branchinfo
$GDXIN

$setnames "%in%" inpath inname inextension
parameter version;
version = Jnow;

busmatrix(bus,'NAME') = bus.val;
busmatrix(bus,'BASKV') = businfo(bus,'baseKV','given') + eps;
busmatrix(bus,'IDE') = businfo(bus,'type','given') + eps;
busmatrix(bus,'GL') = businfo(bus,'Gs','given') + eps;
busmatrix(bus,'BL') = businfo(bus,'Bs','given') + eps;
busmatrix(bus,'AREA') = businfo(bus,'area','given') + eps;
busmatrix(bus,'ZONE') = businfo(bus,'zone','given') + eps;
busmatrix(bus,'VM') = businfo(bus,'Vm','given') + eps;
busmatrix(bus,'VA') = businfo(bus,'Va','given') + eps;
busmatrix(bus,'OWNER') = 1;

loadmatrix(bus,'I') = bus.val;
loadmatrix(bus,'ID') = 1;
loadmatrix(bus,'STATUS') = 1;
loadmatrix(bus,'AREA') = businfo(bus,'area','given') + eps;
loadmatrix(bus,'ZONE') = businfo(bus,'zone','given') + eps;
loadmatrix(bus,'PL') = businfo(bus,'Pd','given') + eps;
loadmatrix(bus,'QL') = businfo(bus,'Qd','given') + eps;
loadmatrix(bus,'IP') = eps;
loadmatrix(bus,'IQ') = eps;
loadmatrix(bus,'YP') = eps;
loadmatrix(bus,'YQ') = eps;
loadmatrix(bus,'OWNER') = 1;
loadmatrix(bus,'SHARE') = 1;

genmatrix(gen,'I') = geninfo(gen,'atbus','given') + eps;
genmatrix(gen,'ID') = gen.ord;
genmatrix(gen,'Pg') = geninfo(gen,'Pg','given') + eps;
genmatrix(gen,'Qq') = geninfo(gen,'Qg','given') + eps;
genmatrix(gen,'QT') = geninfo(gen,'Qmax','given') + eps;
genmatrix(gen,'QB') = geninfo(gen,'Qmin','given') + eps;
genmatrix(gen,'VS') = geninfo(gen,'Vg','given') + eps;
genmatrix(gen,'IREG') = eps;
genmatrix(gen,'MBASE') = geninfo(gen,'mBase','given') + eps;
genmatrix(gen,'ZR') = eps;
genmatrix(gen,'ZX') = 1;
genmatrix(gen,'RT') = eps;
genmatrix(gen,'XT') = eps;
genmatrix(gen,'GTAP') = 1;
genmatrix(gen,'STAT') = geninfo(gen,'status','given') + eps;
genmatrix(gen,'RMPCT') = 100;
genmatrix(gen,'PT') = geninfo(gen,'Pmax','given') + eps;
genmatrix(gen,'PB') = geninfo(gen,'Pmin','given') + eps;
genmatrix(gen,'Oi') = 1;
genmatrix(gen,'Fi') = 1;

loop((i,j,c)$(line(i,j,c) and (branchinfo(i,j,c,'ratio','given') eq 1) and (branchinfo(i,j,c,'angle','given') eq 0)),
    branchmatrix(i,j,c,'I') = i.val;
    branchmatrix(i,j,c,'J') = j.val;
    branchmatrix(i,j,c,'CKT') = c.val;
    branchmatrix(i,j,c,'R') = branchinfo(i,j,c,'r','given') + eps;
    branchmatrix(i,j,c,'X') = branchinfo(i,j,c,'x','given') + eps;
    branchmatrix(i,j,c,'B') = branchinfo(i,j,c,'bc','given') + eps;
    branchmatrix(i,j,c,'RATEA') = branchinfo(i,j,c,'rateA',"%limits%") + eps;
    branchmatrix(i,j,c,'RATEB') = branchinfo(i,j,c,'rateB',"%limits%") + eps;
    branchmatrix(i,j,c,'RATEC') = branchinfo(i,j,c,'rateC',"%limits%") + eps;
    branchmatrix(i,j,c,'GI') = eps;
    branchmatrix(i,j,c,'BI') = eps;
    branchmatrix(i,j,c,'GJ') = eps;
    branchmatrix(i,j,c,'BJ') = eps;
    branchmatrix(i,j,c,'ST') = branchinfo(i,j,c,'branchstatus','given') + eps;
    branchmatrix(i,j,c,'LEN') = eps;
    branchmatrix(i,j,c,'Oi') = 1;
    branchmatrix(i,j,c,'Fi') = 1;
)

loop((i,j,c)$(line(i,j,c) and ((branchinfo(i,j,c,'ratio','given') ne 1) or (branchinfo(i,j,c,'angle','given') ne 0))),
    transmatrix(i,j,c,'1','I') = i.val;
    transmatrix(i,j,c,'1','J') = j.val;
    transmatrix(i,j,c,'1','K') = eps;
    transmatrix(i,j,c,'1','CKT') = c.val;
    transmatrix(i,j,c,'1','CW') = 1;
    transmatrix(i,j,c,'1','CZ') = 1;
    transmatrix(i,j,c,'1','CM') = 1;
    transmatrix(i,j,c,'1','MAG1') = eps;
    transmatrix(i,j,c,'1','MAG2') = eps;
    transmatrix(i,j,c,'1','NMETR') = 2;
    transmatrix(i,j,c,'1','NAME') = eps;
    transmatrix(i,j,c,'1','STAT') = branchinfo(i,j,c,'branchstatus','given') + eps;
    transmatrix(i,j,c,'1','Oi') = 1;
    transmatrix(i,j,c,'1','Fi') = 1;
    transmatrix(i,j,c,'2','R1-2') = branchinfo(i,j,c,'r','given') + eps;
    transmatrix(i,j,c,'2','X1-2') = branchinfo(i,j,c,'x','given') + eps;
    transmatrix(i,j,c,'2','SBASE1-2') = baseMVA;
    transmatrix(i,j,c,'3','WINDV1') = branchinfo(i,j,c,'ratio','given') + eps;
    transmatrix(i,j,c,'3','NOMV1') = eps;
    transmatrix(i,j,c,'3','ANG1') = branchinfo(i,j,c,'angle','given') + eps;
    transmatrix(i,j,c,'3','RATEA') = branchinfo(i,j,c,'rateA',"%limits%") + eps;
    transmatrix(i,j,c,'3','RATEB') = branchinfo(i,j,c,'rateB',"%limits%") + eps;
    transmatrix(i,j,c,'3','RATEC') = branchinfo(i,j,c,'rateC',"%limits%") + eps;
    transmatrix(i,j,c,'3','COD1') = eps;
    transmatrix(i,j,c,'3','CONT1') = eps;
    transmatrix(i,j,c,'3','RMA1') = businfo(i,'maxVM','given') + eps;
    transmatrix(i,j,c,'3','RMI1') = businfo(i,'minVM','given') + eps;
    transmatrix(i,j,c,'3','VMA1') = businfo(i,'maxVM','given') + eps;
    transmatrix(i,j,c,'3','VMI1') = businfo(i,'minVM','given') + eps;
    transmatrix(i,j,c,'3','NTP1') = 33;
    transmatrix(i,j,c,'3','TAB1') = eps;
    transmatrix(i,j,c,'3','CR1') = eps;
    transmatrix(i,j,c,'3','CX1') = eps;
    transmatrix(i,j,c,'4','WINDV2') = 1;
    transmatrix(i,j,c,'4','NOMV2') = eps;
)

execute_unload '%filepath%excel_temp.gdx' version, baseMVA, busmatrix, loadmatrix,
                                genmatrix, branchmatrix, transmatrix;
execute "gdxxrw %filepath%excel_temp.gdx epsout=0 o=%out% par=version rng=Version!A1";
execute "gdxxrw %filepath%excel_temp.gdx epsout=0 o=%out% par=baseMVA rng=baseMVA!A1";
execute "gdxxrw %filepath%excel_temp.gdx epsout=0 o=%out% par=busmatrix rng=BusData!A1";
execute "gdxxrw %filepath%excel_temp.gdx epsout=0 o=%out% par=loadmatrix rng=LoadData!A1";
execute "gdxxrw %filepath%excel_temp.gdx epsout=0 o=%out% par=genmatrix rng=GeneratorData!A1";
execute "gdxxrw %filepath%excel_temp.gdx epsout=0 o=%out% par=branchmatrix rng=BranchData!A1";
execute "gdxxrw %filepath%excel_temp.gdx epsout=0 o=%out% par=transmatrix rng=TransformerData!A1";
execute "rm %filepath%excel_temp.gdx"

