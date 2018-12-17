$title Generator cost parameters

set rev(ccs,ccs);

parameters costmodel(gen), numcostpts(gen), numcostcoef(gen),
           costpts_x(gen,cps), costpts_y(gen,cps), noloadcost(gen), costcoef(gen,ccs);
costmodel(gen) = gencostdata(gen,'1');
numcostpts(gen) = gencostdata(gen,'4');
numcostcoef(gen) = gencostdata(gen,'4');

parameters
    p_range,
    x_point,
    y_point,
    num_interp_points /10/,
    interp_points(costptset)
       /1 0,
        2 0.095012509837637,
        3 0.281603550779259,
        4 0.458016777657227,
        5 0.617876244402644,
        6 0.755404408355003,
        7 0.865631202387832,
        8 0.944575023073233,
        9 0.989400934991650,
        10 1/
;

* Model for fitting quadratic to cost points
parameters temp_x(costptset), temp_y(costptset), temp_numcostpts;
positive variable x_2;
variables x_1, x_0, obj;
equations least_sqrs, keep_max_cost;

* Scaling?
least_sqrs..
    obj =g= sum(costptset$(ord(costptset) le temp_numcostpts),
                sqr((x_2*sqr(temp_x(costptset)) + x_1*temp_x(costptset) + x_0 - temp_y(costptset)))/1e4)
;
keep_max_cost..
      x_2*sqr(sum(costptset$(ord(costptset) eq temp_numcostpts),temp_x(costptset)))
    + x_1*sum(costptset$(ord(costptset) eq temp_numcostpts),temp_x(costptset))
    + x_0
        =e=
      sum(costptset$(ord(costptset) eq temp_numcostpts),temp_y(costptset))
;
model lsqr /least_sqrs, keep_max_cost/;
lsqr.solvelink = 5;


loop(gen,

rev(ccs,costcoefset) = no;
rev(ccs,ccs+[numcostcoef(gen)-2*ord(ccs)+1]) = yes;

*---- PWL cost function given
if(costmodel(gen) eq 1,
* Extract points from row x1,y1,x2,y2,...
loop((costptset,cps)$((ord(costptset) le numcostpts(gen)) and (doubles(costptset,cps))),
  costpts_x(gen,costptset) = gencostdata(gen,cps+3);
  costpts_y(gen,costptset) = gencostdata(gen,cps+4);
* END loop((costptset,cps)$((ord(costptset) le numcostpts(gen)) and (doubles(costptset,cps))
);

* (x1,y1) as given may be the minimum operating cost (x1=Pmin), so extrapolate to get no-load cost.
* Note that if Pmin=0, this extrapolation is trivial and still correct.
noloadcost(gen) = costpts_y(gen,'1') - ((costpts_y(gen,'2') - costpts_y(gen,'1'))
                     /(costpts_x(gen,'2') - costpts_x(gen,'1')))*costpts_x(gen,'1');
* Now fit a quadratic to the cost points
temp_x(costptset) = 0;
temp_y(costptset) = 0;
temp_x(costptset) = costpts_x(gen,costptset);
temp_y(costptset) = costpts_y(gen,costptset);
temp_numcostpts   = numcostpts(gen);
x_2.l = 0;
x_1.l = 0;
x_0.fx = noloadcost(gen);

solve lsqr using nlp min obj;
if(lsqr.ModelStat > 2, abort "Unable to find optimal solution in calc_cost_curves.gms"; );

numcostcoef(gen) = 3;
costcoef(gen,costcoefset) = 0;
costcoef(gen,'0') = x_0.l;
costcoef(gen,'1') = x_1.l;
costcoef(gen,'2') = x_2.l;

elseif (costmodel(gen) eq 2),
  loop(rev(costcoefset,ccs)$(ord(costcoefset) le numcostcoef(gen)),
      costcoef(gen,ccs) = gencostdata(gen,costcoefset+5)
* END loop(rev(costcoefset,ccs .... ))
   );

noloadcost(gen) = costcoef(gen,'0');

* Generate the piecewise linear approximation for this generator cost curve
* but don't split it up if the step is too small, or GAMS will give divide-by-zero errors.
p_range = Pmax(gen) - Pmin(gen);
costpts_x(gen,'1') = 0;
costpts_y(gen,'1') = noloadcost(gen);

if(p_range le 0.1,
*--- IF range is too small
  if(Pmax(gen) > 0,
* If Pmax is !=0		
    numcostpts(gen) = 2;
    x_point = Pmax(gen);
    y_point = costcoef(gen,'0') + costcoef(gen,'1')*x_point + costcoef(gen,'2')*sqr(x_point);
    costpts_x(gen,'2') = x_point;
    costpts_y(gen,'2') = y_point;
  else
* If Pmax =0
    numcostpts(gen) = 0;
  );  
    
elseif((costcoef(gen,'0') eq 0) AND (costcoef(gen,'1') eq 0) AND (costcoef(gen,'3') eq 0)),
*--- All coefficients are equal to 0
  numcostpts(gen) = 0;

elseif (costcoef(gen,'2') eq 0),
*--- Linear objective function given
  if(Pmin(gen) > 0,
* If Pmin is !=0		
    numcostpts(gen) = 3;
    x_point = Pmin(gen);
    costpts_x(gen,'2') = x_point;
    costpts_y(gen,'2') = costcoef(gen,'0') + costcoef(gen,'1')*x_point;
    x_point = Pmax(gen);
    costpts_x(gen,'3') = x_point;
    costpts_y(gen,'3') = costcoef(gen,'0') + costcoef(gen,'1')*x_point;	
  else
* If Pmin =0
    numcostpts(gen) = 2;

    x_point = Pmax(gen);
    costpts_x(gen,'2') = x_point;
    costpts_y(gen,'2') = costcoef(gen,'0') + costcoef(gen,'1')*x_point;	
  );
            
else
*--- Regular quad function
numcostpts(gen) = num_interp_points;
loop(costptset$((ord(costptset) gt 1) and (ord(costptset) le num_interp_points)),
  x_point = interp_points(costptset) * Pmax(gen);
  y_point = costcoef(gen,'0') + costcoef(gen,'1')*x_point + costcoef(gen,'2')*sqr(x_point);
  costpts_x(gen,costptset) = x_point;
  costpts_y(gen,costptset) = y_point;
  );
* END if(p_range le 0.1)
);

* END if(costmodel(gen) eq 1
);
* END loop(gen)
);

* Save all the cost data into the geninfo parameter
geninfo(gen,'costmodel','given')     = gencostdata(gen,'1');
geninfo(gen,'startupcost','given')   = gencostdata(gen,'2');
geninfo(gen,'shutdowncost','given')  = gencostdata(gen,'3');
geninfo(gen,'numcostpts','given')    = numcostpts(gen);
geninfo(gen,'numcostcoef','given')   = numcostcoef(gen);
geninfo(gen,'costpts_x',costptset)   = costpts_x(gen,costptset);
geninfo(gen,'costpts_y',costptset)   = costpts_y(gen,costptset);
geninfo(gen,'costcoef',costcoefset)  = costcoef(gen,costcoefset);
geninfo(gen,'noloadcost','given')    = noloadcost(gen);
