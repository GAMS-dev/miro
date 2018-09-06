$ontext
Randomized initial conditions for polar ACOPF
$offtext

$if not set filepath $setnames "%gams.i%" filepath filename fileextension

V_P.l(gen)$status(gen) = uniform(Pmin(gen),Pmax(gen));
V_Q.l(gen)$status(gen) = uniform(Qmin(gen),Qmax(gen));

* There should be a better way of choosing bounds for theta
V_V.l(bus) = uniform(minVm(bus),maxVm(bus));
V_Theta.l(bus) = uniform(-pi,pi);

$if %condensed% == 'no' $include '%MODELPATH%ic_polar%sep%calc_derived_vars.gms'
* Set P(gen) and Q(gen) from AC power flow equations
scalar temp, count;
loop(i,
    count = sum(gen$(atBus(gen,i) and status(gen)), 1);
    temp = Pd(i)
$ifthen.ICcondensed %condensed% == 'no'
           + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)), V_LineP.l(i,j,c))
$elseif.ICcondensed %condensed% == 'yes'
           + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)),
                (g(i,j,c) * sqr(V_V.l(i)) / sqr(ratio(i,j,c)$branchstatus(i,j,c) + 1$branchstatus(j,i,c)))
                - (V_V.l(i) * V_V.l(j) / ratio(i,j,c)) *
                    (  g(i,j,c) * cos(V_Theta.l(i) - V_Theta.l(j) - angle(i,j,c))
                     + b(i,j,c) * sin(V_Theta.l(i) - V_Theta.l(j) - angle(i,j,c)))
             )
$endif.ICcondensed
           + Gs(i) * sqr(V_V.l(i));
    V_P.l(gen)$(atBus(gen,i) and status(gen)) = temp/count;

    temp = Qd(i)
$ifthen.ICcondensed2 %condensed% == 'no'
           + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)), V_LineQ.l(i,j,c))
$elseif.ICcondensed2 %condensed% == 'yes'
           + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)),
                - (sqr(V_V.l(i)) * (b(i,j,c) + bc(i,j,c)/2) / sqr(ratio(i,j,c)$branchstatus(i,j,c) + 1$branchstatus(j,i,c)))
                - (V_V.l(i) * V_V.l(j) / ratio(i,j,c)) * 
                    (  g(i,j,c) * sin(V_Theta.l(i) - V_Theta.l(j) - angle(i,j,c))
                     - b(i,j,c) * cos(V_Theta.l(i) - V_Theta.l(j) - angle(i,j,c)))
             )
$endif.ICcondensed2
           - Bs(i) * sqr(V_V.l(i));
    V_Q.l(gen)$(atBus(gen,i) and status(gen)) = temp/count;
);

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

