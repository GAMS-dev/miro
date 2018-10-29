*the declaration of Ouput for the WebUI is done within the $onExternalOutput / $offExternalOutput keywords.

$onExternalOutput
parameter      
    total_cost_Report                                    'Final objective value     '
    version_Report                                       'version                   '
    baseMVA_Report                                       'baseMVA                   '     
    businfo_Report(bus,bus_t,bus_s)                      'businfo                   '
    geninfo_Report(gen,gen_t,gen_s)                      'geninfo                   '
    branchinfo_Report(bus,bus,circuit,branch_t,branch_s) 'branchinfo                '
    fuelinfo_Report(fuel_t,fuel_s)                       'fuelinfo                  '
*    demandbidinfo_Report(demandbid,t,demandbid_t,demandbid_s) 'demandbidinfo'
*    interfaceinfo_Report(interface,t,interface_t) 'interfaceinfo'
;

singleton set
    case_input(*)       'Selected Testcase'
    obj_input(*)        'Objective function'
    LP_solver(*)        'LP-solver'
    QCP_solver(*)       'QCP-solver'
    NLP_solver(*)       'NLP-solver'
    CNS_solver(*)       'CNS-solver'
    timeperiod_input(*) 'Selected time period to solve'
    allon_input(*)      'Turned on gens and/or lines during solve'
    linelimits_input(*) 'Type of line limit data to use'
    genPmin_input(*)    'Data for Generator lower limit'
    lineloss_input(*)   'Whether to approximate lineloss'
    qlim_input(*)       'Whether to enforce reactive power limits as D-curve circle constraints'
    slim_input(*)       'Whether to use apparent power limits on line'
    ic_input(*)         'Choosen method for generating initial conditions, i.e. NLP starting point'
    iter_input(*)       'Number of iterations'
    times_input(*)      'Time range'
    ramprates_input(*)  'Type of ramprate data to use'
    relax_input(*)      'Whether to relax integer models'
    demandbids_input(*) 'Whether to turn on elastic demand bidding'    
    wind_input(*)       'Whether to turn off wind turbines'
    savesol_input(*)    'Whether to save the solution as GDX'
    verbose_input(*)    'Whether to print input in listing output'
;
$offExternalOutput

$ifthen.case set case
    case_input('%casename%') = yes;
$endif.case

$ifthen.linear %obj% == 'linear'
    LP_solver('%system.lp%') = yes;
$endif.linear

$ifthen.pwl %obj% == 'pwl'
    LP_solver('%system.lp%') = yes;
$endif.pwl

$ifthen.qcp %obj% == 'quad'
    QCP_solver('%system.qcp%') = yes;
$endif.qcp

$ifthen.nlp %modeltype% == 'AC'
    NLP_solver('%system.nlp%') = yes;
$endif.nlp

$ifthen.cns set cns
    CNS_solver('%system.cns%') = yes;
$endif.cns


$ifthen.obj set obj
    obj_input('%obj%') = yes;
$endif.obj
$ifthen.timeperiod set timeperiod
    timeperiod_input('%timeperiod%') = yes;
$endif.timeperiod
$ifthen.allon set allon
    allon_input('%allon%') = yes; 
$endif.allon
$ifthen.linelimits set linelimits
    linelimits_input('%linelimits%') = yes;
$endif.linelimits
$ifthen.genPmin set genPmin
    genPmin_input('%genPmin%') = yes;    
$endif.genPmin
$ifthen.lineloss set lineloss
    lineloss_input('%lineloss%') = yes;   
$endif.lineloss
$ifthen.qlim set qlim
    qlim_input('%qlim%') = yes;       
$endif.qlim
$ifthen.slim set slim
    slim_input('%slim%') = yes;     
$endif.slim
$ifthen.ic set ic
    ic_input('%ic%') = yes;        
$endif.ic
$ifthen.iter set iter
    iter_input('%iter%') = yes;
$endif.iter

$ifthen.times set times
    times_input('%times%') = yes;
$endif.times
$ifthen.ramprates set ramprates
    ramprates_input('%ramprates%') = yes;
$endif.ramprates
$ifthen.relax set relax
    relax_input('%relax%') = yes;
$endif.relax
$ifthen.demandbids set demandbids
    demandbids_input('%demandbids%') = yes;
$endif.demandbids


$ifthen.wind set wind
    wind_input('%wind%') = yes;       
$endif.wind
$ifthen.savesol set savesol
    savesol_input('%savesol%') = yes;    
$endif.savesol
$ifthen.verbose set verbose
    verbose_input('%verbose%') = yes;
$endif.verbose


parameters version_tmp, baseMVA_tmp, total_cost_tmp;
parameters businfo_tmp(bus,bus_t,bus_s), geninfo_tmp(gen,gen_t,gen_s), fuelinfo_tmp(fuel_t,fuel_s),
           branchinfo_tmp(bus,bus,circuit,branch_t,branch_s);
*           demandbidinfo_tmp(demandbid,t,demandbid_t,demandbid_s), interfaceinfo_tmp(interface,t,interface_t)
           
*load results
if(infeas eq 0,
execute_load '%out%', version, baseMVA, total_cost;
execute_load '%out%', businfo_tmp = businfo, geninfo_tmp = geninfo, fuelinfo_tmp = fuelinfo;
execute_load '%out%', branchinfo_tmp = branchinfo;
*, demandbidinfo_tmp = demandbidinfo, interfaceinfo_tmp = interfaceinfo;
);
total_cost_Report           = total_cost;
version_Report              = version;
baseMVA_Report              = baseMVA;


businfo_Report(bus,bus_t,bus_s)                             = businfo_tmp(bus,bus_t,bus_s);
geninfo_Report(gen,gen_t,gen_s)                             = geninfo_tmp(gen,gen_t,gen_s);
branchinfo_Report(i,j,c,branch_t,branch_s)                  = branchinfo_tmp(i,j,c,branch_t,branch_s);
fuelinfo_Report(fuel_t,fuel_s)                              = fuelinfo_tmp(fuel_t,fuel_s);
*demandbidinfo_Report(demandbid,t,demandbid_t,demandbid_s)    = demandbidinfo_tmp(demandbid,t,demandbid_t,demandbid_s);
*interfaceinfo_Report(interface,t,interface_t)                = interfaceinfo_tmp(interface,t,interface_t);




* delete result-GDX if savesol = 0
$ifthen.sol %savesol% == 0
execute 'rm "%out%"';
$endif.sol