$ontext
Use given initial conditions for Polar ACOPF.
$offtext

$if not set timeperiod $abort "Time period not chosen, should default in model..."

$if not set filepath $setnames "%gams.i%" filepath filename fileextension
$if not set casepath $setnames "%case%" casepath casename caseextension
$call 'gams "dcopf.gms" lp=%system.lp% qcp=%system.qcp% idir1="%MODELPATH%dcopf" idir2="%MODELPATH%" --Plim=%Plim% --allon=%allon% --limits=%limits% --case="%case%" --obj=%obj% --lineloss=1.055 gdx=dctemp.gdx lo=3 --timeperiod=%timeperiod% --verbose=%verbose%'
if(errorlevel ne 0, abort "Failed to find a starting point using DCOPF!");

variables ic_Pg, ic_Va;

$GDXIN dctemp.gdx
$LOADR ic_Pg = V_P, ic_Va = V_Theta
$GDXIN

execute 'rm "dctemp.gdx"' ;

V_P.l(gen) = ic_Pg.l(gen);
V_Theta.l(bus) = ic_Va.l(bus);

* Assume starting voltage magnitude of 1 p.u.
V_V.l(bus) = 1;

$if %condensed% == 'no' $include '%MODELPATH%ic_polar%sep%calc_derived_vars.gms'
* Set P(gen) and Q(gen) from AC power flow equations
scalar temp, count;
loop(i,
    count = sum(gen$(atBus(gen,i) and status(gen)), 1);
    temp = Qd(i)
$ifthen.ICcondensed %condensed% == 'no'
           + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)), V_LineQ.l(i,j,c))
$elseif.ICcondensed %condensed% == 'yes'
           + sum((j,c)$(branchstatus(j,i,c) or branchstatus(i,j,c)),
                - (sqr(V_V.l(i)) * (b(i,j,c) + bc(i,j,c)/2) / sqr(ratio(i,j,c)$branchstatus(i,j,c) + 1$branchstatus(j,i,c)))
                - (V_V.l(i) * V_V.l(j) / ratio(i,j,c)) *
                    (  g(i,j,c) * sin(V_Theta.l(i) - V_Theta.l(j) - angle(i,j,c))
                     - b(i,j,c) * cos(V_Theta.l(i) - V_Theta.l(j) - angle(i,j,c)))
             )
$endif.ICcondensed
           + Gs(i) * sqr(V_V.l(i));
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

