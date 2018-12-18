$ontext
Use given initial conditions for Polar ACOPF.
$offtext

$if not set filepath $setnames "%gams.i%" filepath filename fileextension
$if not set casepath $setnames "%case%" casepath casename caseextension
$call 'gams "ic_xls2gdx.gms" idir1="%MODELPATH%ic_rect" idir2="%MODELPATH%" --in="%MODELPATH%ic_rect%sep%sol_%casename%.xlsx"'

parameters ic_bus, ic_gen;

$GDXIN "%MODELPATH%ic_rect%sep%sol_%casename%.gdx"
$LOAD ic_bus, ic_gen
$GDXIN

V_P.l(gen) = ic_gen(gen,'ic_Pg')/baseMVA;
V_Q.l(gen) = ic_gen(gen,'ic_Qg')/baseMVA;
Vm(bus) = ic_bus(bus,'ic_Vm');
Va(bus) = ic_bus(bus,'ic_Va')*(pi/180);

V_real.l(bus) = Vm(bus)*cos(Va(bus));
V_imag.l(bus) = Vm(bus)*sin(Va(bus));

execute 'rm "%MODELPATH%ic_rect%sep%sol_%casename%.gdx"' ;

$if %condensed% == 'no' $include '%MODELPATH%ic_iv%sep%calc_derived_vars.gms'
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

