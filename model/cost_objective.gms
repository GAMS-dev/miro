$title Cost objective calculations for OPF models

* Set costmodel variable
$iftheni %obj% == "pwl" costmodel(gen)= 1;
$elseifi %obj% == "quad" costmodel(gen)= 2;
$elseifi %obj% == "linear" costmodel(gen)= 2;
$elseifi %obj% == "0" costmodel(gen)= 0;
$else $abort "Fix invalid option: --obj=%obj%"  
$endif


*-- Convexity Check
* Not part of system of equations
* LP/QCP/NLP can't handle nonconvex piecewise linear cost functions
set thisgen(gen);

parameters cur_slope, next_slope;
loop(gen$(status(gen) and (costmodel(gen) eq 1) and (numcostpts(gen) > 2)),
    next_slope = (costpts_y(gen,'2') - costpts_y(gen,'1'))
                 / (costpts_x(gen,'2') - costpts_x(gen,'1'));
    loop(costptset$(ord(costptset) < numcostpts(gen) - 1),
        cur_slope = next_slope;
        if((ord(costptset) < numcostpts(gen) - 2) and (costpts_x(gen,costptset+2) eq costpts_x(gen,costptset+1)),
            abort "Zero-length piecewise segment detected";
        );
        next_slope = (costpts_y(gen,costptset+2) - costpts_y(gen,costptset+1))
                     / (costpts_x(gen,costptset+2) - costpts_x(gen,costptset+1))
    
        if(cur_slope - next_slope > 1e-8,
	    thisgen(gen1)=no; thisgen(gen)=yes;
            display thisgen;
            abort "Nonconvex piecewise linear costs not supported";
        );
    );
);


*===== SECTION: EQUATIONS PART 2
* Defining piecewise linear generator cost curves
* P is in per-unit, costpts_x is in MW, and costpts_y is in $/hr
c_pw_cost(gen,costptset)$(status(gen) and (ord(costptset) < numcostpts(gen)) and (costmodel(gen) eq 1))..
V_pw_cost(gen) =g=
    ((costpts_y(gen,costptset+1) - costpts_y(gen,costptset))/
     (costpts_x(gen,costptset+1) - costpts_x(gen,costptset)))
      * (V_P(gen)*baseMVA - costpts_x(gen,costptset))
    + costpts_y(gen,costptset)
;

$ifthen.nobids %demandbids%==1
* Revenue from elastic demand are less than a concave function
c_demandbid_revenue(demandbid,demandbid_s)$((ord(demandbid_s) lt numdemandpts(demandbid)))..
V_demandbid_rev(demandbid) =l=
    ((demandpts_y(demandbid,demandbid_s+1) - demandpts_y(demandbid,demandbid_s))/
      (demandpts_x(demandbid,demandbid_s+1) - demandpts_x(demandbid,demandbid_s)))
       * (V_Pd_elastic(demandbid)*baseMVA - demandpts_x(demandbid,demandbid_s))
   + demandpts_y(demandbid,demandbid_s)
;
$endif.nobids

c_obj..
    V_objcost =e=
*0 objective function
	0
$iftheni %obj% == "pwl"
* Piecewise linear objective function
    + sum((gen)$(costmodel(gen) eq 1), V_pw_cost(gen))
$elseifi %obj% == "quad"
* Quadratic objective function
    + sum((gen)$(status(gen) and costmodel(gen) eq 2),costcoef(gen,'0')
	+ costcoef(gen,'1')*V_P(gen)*baseMVA
	+ costcoef(gen,'2')*sqr(V_P(gen)*baseMVA))
$elseifi %obj% == "linear"
* Linear objective function
    + sum((gen)$(status(gen) and costmodel(gen) eq 2),costcoef(gen,'0')
	+ costcoef(gen,'1')*V_P(gen)*baseMVA)
$endif
;
