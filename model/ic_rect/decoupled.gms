$ontext
Use given initial conditions for Polar ACOPF.
$offtext

$if not set timeperiod $abort "Time period not chosen, should default in model..."

$if not set filepath $setnames "%gams.i%" filepath filename fileextension

$call 'gams "polar_decoupled.gms" idir1="%MODELPATH%polar_decoupled" idir2="%MODELPATH%" --genPmin=%genPmin% --allon=%allon% --linelimits=%linelimits% --case="%case%" --obj=%obj% gdx=decoupledtemp.gdx lo=3 --timeperiod=%timeperiod% reslim=3600 nlp=ipopth --verbose=%verbose%'

if(errorlevel ne 0, abort "Failed to find a starting point using Decoupled AC!");

variables ic_Pg, ic_Qg, ic_Vm, ic_Va, ic_shunt;

$GDXIN decoupledtemp.gdx
$LOAD  ic_Pg=V_P, ic_Qg=V_Q, ic_Vm=V_V, ic_Va=V_Theta, ic_shunt=V_shunt
$GDXIN

execute 'rm "decoupledtemp.gdx"' ;   

Vm(bus) = ic_Vm.l(bus);
Va(bus) = ic_Va.l(bus);
V_P.l(gen) = ic_Pg.l(gen);
V_Q.l(gen) = ic_Qg.l(gen);
V_real.l(bus) = Vm(bus)*cos(Va(bus));
V_imag.l(bus) = Vm(bus)*sin(Va(bus));
V_shunt.l(bus,bus_s) = ic_shunt.l(bus,bus_s);

$if %condensed% == 'no' $include '%MODELPATH%ic_rect%sep%calc_derived_vars.gms'
* Set cost level from decoupled solution point
V_pw_cost.l(gen)$(status(gen) and (costmodel(gen) eq 1)) = max(0,
    smax(costptset$(ord(costptset) < numcostpts(gen)),
            ((costpts_y(gen,costptset+1) - costpts_y(gen,costptset))/
             (costpts_x(gen,costptset+1) - costpts_x(gen,costptset)))
              * (V_P.l(gen)*baseMVA - costpts_x(gen,costptset))
            + costpts_y(gen,costptset) - noloadcost(gen)))
;

V_objcost.l =
$ifthen.linear %obj% == "linear"
             sum(gen$(status(gen) and (costmodel(gen) eq 2)),
                             costcoef(gen,'0')
                           + costcoef(gen,'1')*P.l(gen)*baseMVA
                )
$else.linear
             sum(gen$(status(gen) and (costmodel(gen) eq 2)),
                             costcoef(gen,'0')
                           + costcoef(gen,'1')*V_P.l(gen)*baseMVA
                           + costcoef(gen,'2')*sqr(V_P.l(gen)*baseMVA)
                )
$endif.linear
           + sum(gen$(status(gen) and (costmodel(gen) eq 1)), V_pw_cost.l(gen)
                                                              + noloadcost(gen))
;

