$title Cost objective calculations for UC models
$iftheni %obj% == "pwl" costmodel(gen)= 1;
$elseifi %obj% == "quad" costmodel(gen)= 2;
$elseifi %obj% == "linear" costmodel(gen)= 2;
$elseifi %obj% == "0" costmodel(gen)= 0;
$else $abort "Fix invalid option --obj=%obj%";
$endif

*-- Convexity Check
* Not part of system of equations
* LP/QCP/NLP can't handle nonconvex piecewise linear cost functions
set thisgen(gen);

parameters cur_slope, next_slope;
loop(gen$((smax(t,status(gen,t)) ge 1)  and (numcostpts(gen) > 2)),
    next_slope = (costpts_y(gen,'2') - costpts_y(gen,'1'))
                 / (costpts_x(gen,'2') - costpts_x(gen,'1'));
    loop(costptset$(ord(costptset) < numcostpts(gen) - 1),
        cur_slope = next_slope;
        if((ord(costptset) < numcostpts(gen) - 2) and (costpts_x(gen,costptset+2) eq costpts_x(gen,costptset+1)),
            abort "Zero-length piecewise segment detected";
        );
        next_slope = (costpts_y(gen,costptset+2) - costpts_y(gen,costptset+1))
                     / (costpts_x(gen,costptset+2) - costpts_x(gen,costptset+1))
        if(cur_slope-next_slope > 1e-8 ,
	    thisgen(gen1)=no; thisgen(gen)=yes;
            display thisgen, cur_slope, next_slope;
            abort "Nonconvex piecewise linear costs not supported";
        );    
    );
);


*===== SECTION: EQUATIONS PART 2
* Defining piecewise linear generator cost curves
* P is in per-unit, costpts_x is in MW, and costpts_y is in $/hr
c_pw_cost(gen,t,costptset)$((ord(costptset) < numcostpts(gen)) and (costmodel(gen) eq 1))..
V_pw_cost(gen,t) =g=
    ((costpts_y(gen,costptset+1) - costpts_y(gen,costptset))/
     (costpts_x(gen,costptset+1) - costpts_x(gen,costptset)))
      * (V_P(gen,t)*baseMVA - costpts_x(gen,costptset))
    + costpts_y(gen,costptset)*V_genstatus(gen,t)
;

$ifthen.nobids %demandbids%==1
* Revenue from elastic demand are less than a concave function
c_demandbid_revenue(demandbid,t,demandbid_s)$((ord(demandbid_s) lt numdemandpts(demandbid,t)))..
V_demandbid_rev(demandbid,t) =l=
        ((demandpts_y(demandbid,t,demandbid_s+1) - demandpts_y(demandbid,t,demandbid_s))/
      (demandpts_x(demandbid,t,demandbid_s+1) - demandpts_x(demandbid,t,demandbid_s)))
       * (V_Pd_elastic(demandbid,t)*baseMVA - demandpts_x(demandbid,t,demandbid_s))
   + demandpts_y(demandbid,t,demandbid_s)
;
$endif.nobids

* Objective function
c_obj..
    V_objcost =e= 
$iftheni.obj0 %obj% == 0
    0
$else.obj0
    - sum((demandbid,t), V_demandbid_rev(demandbid,t))
    + sum((gen,t),V_startup(gen,t)*startupcost(gen) + V_shutdown(gen,t)*shutdowncost(gen))
$iftheni.sol %obj% == "pwl"
* Piecewise linear objective function
    + sum((gen,t)$(costmodel(gen) eq 1), V_pw_cost(gen,t))
$elseifi.sol %obj% == "quad"
* Quadratic objective function
    + sum((gen,t)$(costmodel(gen) eq 2),costcoef(gen,'0')*V_genstatus(gen,t)
	+ costcoef(gen,'1')*V_P(gen,t)*baseMVA
	+ costcoef(gen,'2')*sqr(V_P(gen,t)*baseMVA))
$elseifi.sol %obj% == "linear"
* Linear objective function
    + sum((gen,t)$(costmodel(gen) eq 2),costcoef(gen,'0')*V_genstatus(gen,t)
	+ costcoef(gen,'1')*V_P(gen,t)*baseMVA)
$endif.sol
$endif.obj0
;
