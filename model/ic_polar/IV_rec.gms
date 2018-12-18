$ontext
Use given initial conditions for Polar ACOPF.
$offtext

$if not set timeperiod $abort "Time period not chosen, should default in model..."

$if not set filepath $setnames "%gams.i%" filepath filename fileextension
$call 'gams "iv_acopf.gms" idir1="%MODELPATH%iv_acopf" idir2="%MODELPATH%" --genPmin=%genPmin% --allon=%allon% --linelimits=%linelimits% --case="%case%" --obj=%obj% gdx=iv_acopftemp.gdx lo=3 --timeperiod=%timeperiod% reslim=3600 nlp=ipopth --verbose=%verbose%'
if(errorlevel ne 0, abort "Failed to find a starting point using IV_rec-ACOPF!");

parameters Pg_in(gen), Qg_in(gen), Vm_in(bus), Va_in(bus), shunt_in(bus,bus_s);

variables V_real, V_imag, ic_Pg, ic_Qg, ic_shunt;

$GDXIN iv_acopftemp.gdx
$LOAD ic_Pg=V_P, ic_Qg=V_Q, V_real, V_imag, ic_shunt=V_shunt
$GDXIN

*execute 'rm "iv_acopftemp.gdx"' ;

V_P.l(gen) = ic_Pg.l(gen);
V_Q.l(gen) = ic_Qg.l(gen);
V_V.l(bus) = sqrt(sqr(V_real.l(bus)) + sqr(V_imag.l(bus)));
loop(bus,
    if(V_real.l(bus) > 0,
        V_Theta.l(bus) = arctan(V_imag.l(bus)/V_real.l(bus));
    else
        V_Theta.l(bus) = arctan(V_imag.l(bus)/V_real.l(bus)) + pi;
    );
);
V_shunt.l(bus,bus_s) = ic_shunt.l(bus,bus_s);


$if %condensed% == 'no' $include '%MODELPATH%ic_polar%sep%calc_derived_vars.gms'
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
                           + costcoef(gen,'1')*V_P.l(gen)*baseMVA
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

