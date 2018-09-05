$ontext
Default initial conditions for polar ACOPF:
Just pick midpoint of variable ranges.
$offtext

$if not set filepath $setnames "%gams.i%" filepath filename fileextension

V_P.l(gen)$status(gen) = (Pmin(gen)+Pmax(gen))/2;
V_Q.l(gen)$status(gen) = (Qmin(gen)+Qmax(gen))/2;
V_V.l(bus)  = (minVm(bus)+maxVm(bus))/2;
* Theta can stay 0, since angles are allowed to range in (-pi, pi)
V_Theta.l(bus) = 0;

$if %condensed% == 'no' $include '%MODELPATH%ic_polar%sep%calc_derived_vars.gms'

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

