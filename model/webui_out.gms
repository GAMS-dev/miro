*the declaration of Ouput for the WebUI is done within the $onExternalOutput / $offExternalOutput keywords.

$onExternalOutput
scalar
    total_cost_Report                                    'Final objective value     '
    version_Report                                       'version                   '
    baseMVA_Report                                       'baseMVA                   ' 
parameter      
    businfo_Report(bus,bus_t,bus_s)                      'businfo                   '
    geninfo_Report(gen,gen_t,gen_s)                      'geninfo                   '
    branchinfo_Report(bus,bus,circuit,branch_t,branch_s) 'branchinfo                '
    fuelinfo_Report(fuel_t,fuel_s)                       'fuelinfo                  '
    demandbidinfo_Report(demandbid,t,demandbid_t,demandbid_s) 'demandbidinfo'
    interfaceinfo_Report(interface,t,interface_t) 'interfaceinfo'
;
$offExternalOutput
          
*load results
if(infeas eq 0,
execute_load '%out%', version_Report = version, baseMVA_Report = baseMVA,
total_cost_Report = total_cost, businfo_Report = businfo,
geninfo_Report = geninfo, fuelinfo_Report = fuelinfo,
branchinfo_Report = branchinfo, demandbidinfo_Report = demandbidinfo,
interfaceinfo_Report = interfaceinfo;
);

* delete result-GDX if savesol = 0
$ifthen.sol %savesol% == 0
execute 'rm "%out%"';
$endif.sol