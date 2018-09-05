$ontext
Randomized initial conditions for rectangular ACOPF
$offtext

$if not set filepath $setnames "%gams.i%" filepath filename fileextension

V_P.l(gen)$status(gen) = uniform(Pmin(gen),Pmax(gen));
V_Q.l(gen)$status(gen) = uniform(Qmin(gen),Qmax(gen));

* There should be a better way of choosing bounds for these
Vm(bus) = uniform(minVm(bus),maxVm(bus));
Va(bus) = uniform(-pi,pi);
V_real.l(bus) = Vm(bus)*cos(Va(bus));
V_imag.l(bus) = Vm(bus)*sin(Va(bus));

$if %condensed% == 'no' $include '%MODELPATH%ic_rect%sep%calc_derived_vars.gms'
* Set P(gen) and Q(gen) from AC power flow equations
scalar temp, count;
loop(i,
    count = sum(gen$(atBus(gen,i) and status(gen)), 1);
    temp = Pd(i)
$ifthen.ICcondensed %condensed% == 'no'
           + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)), V_LineP.l(i,j,c))
$elseif.ICcondensed %condensed% == 'yes'
           + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)),
                (g(i,j,c) / sqr(ratio(i,j,c)$branchstatus(i,j,c) + 1$branchstatus(j,i,c)))
                    * (sqr(V_real.l(i)) + sqr(V_imag.l(i)))
                - (1 / ratio(i,j,c))
                    * ( (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c)))
                            * (V_real.l(i)*V_real.l(j) + V_imag.l(i)*V_imag.l(j))
                       +(b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c)))
                            * (V_real.l(j)*V_imag.l(i) - V_real.l(i)*V_imag.l(j)))
             )
$endif.ICcondensed
           + Gs(i) * (sqr(V_real.l(i)) + sqr(V_imag.l(i)));
    V_P.l(gen)$(atBus(gen,i) and status(gen)) = temp/count;

    temp = Qd(i)
$ifthen.ICcondensed2 %condensed% == 'no'
           + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)), V_LineQ.l(i,j,c))
$elseif.ICcondensed2 %condensed% == 'yes'
           + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)),
                - ((b(i,j,c) + bc(i,j,c)/2) / sqr(ratio(i,j,c)$branchstatus(i,j,c) + 1$branchstatus(j,i,c)))
                    * (sqr(V_real.l(i)) + sqr(V_imag.l(i)))
                - (1 / ratio(i,j,c))
                    * ( (g(i,j,c)*cos(angle(i,j,c)) - b(i,j,c)*sin(angle(i,j,c)))
                            * (V_real.l(j)*V_imag.l(i) - V_real.l(i)*V_imag.l(j))
                       -(b(i,j,c)*cos(angle(i,j,c)) + g(i,j,c)*sin(angle(i,j,c)))
                            * (V_real.l(i)*V_real.l(j) + V_imag.l(i)*V_imag.l(j)))
           )
$endif.ICcondensed2
           - Bs(i) * (sqr(V_real.l(i)) + sqr(V_imag.l(i)));
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

