$ontext
Randomized initial conditions for polar ACOPF
$offtext

$if not set filepath $setnames "%gams.i%" filepath filename fileextension

V_P.l(gen)$status(gen) = uniform(Pmin(gen),Pmax(gen));
V_Q.l(gen)$status(gen) = uniform(Qmin(gen),Qmax(gen));

* There should be a better way of choosing bounds for theta
V_V.l(bus) = uniform(minVm(bus),maxVm(bus));

* There should be a better way of choosing bounds for theta
V_Theta.l(bus) = 0;

*$if %condensed% == 'no' V_LineP.l(i,j,c)$(branchstatus(i,j,c) or branchstatus(j,i,c)) = uniform(-rateA(i,j,c),rateA(i,j,c));
*$if %condensed% == 'no' V_LineQ.l(i,j,c)$(branchstatus(i,j,c) or branchstatus(j,i,c)) = uniform(-rateA(i,j,c),rateA(i,j,c));

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

