$title Power generation D-curve parameters

set limtypes /given, uwcalc/;

* Assuming 0.8 lagging for intersection
geninfo(gen,'nameplate_pf',limtypes) = 0.8;

* Set R_max for the sake of calculation, but we will change this later for the actual constraint.
geninfo(gen,'R_max',limtypes) = max(geninfo(gen,'Pmax',limtypes), geninfo(gen,'Qmax',limtypes));

* Force Qmin and Qmax to be such that D-curves are all physical
geninfo(gen,'Qmax','uwcalc') = sqrt(1-sqr(geninfo(gen,'nameplate_pf','uwcalc')))*1.1* geninfo(gen,'Pmax','uwcalc');
geninfo(gen,'Qmin','uwcalc') = sign(geninfo(gen,'Qmin','given')) * 0.4 * geninfo(gen,'R_max','uwcalc');

* Define minimum power factor from Pmin and Qmin (zero if Pmin is zero, regardless of Qmin)
geninfo(gen,'min_pf',limtypes)$(geninfo(gen,'Pmin',limtypes) ne 0) = abs(geninfo(gen,'Pmin',limtypes))/sqrt(sqr(geninfo(gen,'Pmin',limtypes)) + sqr(geninfo(gen,'Qmin',limtypes)));

* Calcluate centers and radii of reactive power limits
geninfo(gen,'Qfield',limtypes)$((geninfo(gen,'Qmax',limtypes) > sqrt(1-sqr(geninfo(gen,'nameplate_pf',limtypes)))*geninfo(gen,'R_max',limtypes))
          and (geninfo(gen,'Qmax',limtypes) le geninfo(gen,'R_max',limtypes))) =
    (sqr(geninfo(gen,'Qmax',limtypes)) - sqr(geninfo(gen,'R_max',limtypes)))/
    (2*(geninfo(gen,'Qmax',limtypes) - sqrt(1-sqr(geninfo(gen,'nameplate_pf',limtypes)))*geninfo(gen,'R_max',limtypes)))
;
geninfo(gen,'Qfield',limtypes)$(geninfo(gen,'Qfield',limtypes) ge 0) = NA;
geninfo(gen,'Rfield',limtypes) = geninfo(gen,'Qmax',limtypes) - geninfo(gen,'Qfield',limtypes);

* Assuming armature and end region limits intersect at 0.95 leading pf
geninfo(gen,'Qend',limtypes)$((abs(geninfo(gen,'Qmin',limtypes)) le geninfo(gen,'R_max',limtypes))
        and (abs(geninfo(gen,'Qmin',limtypes)) > 0.31*geninfo(gen,'R_max',limtypes))) =
    (sqr(geninfo(gen,'Qmin',limtypes)) - sqr(geninfo(gen,'R_max',limtypes))) /
    (2*(geninfo(gen,'Qmin',limtypes) + 0.31*geninfo(gen,'R_max',limtypes)))
;

geninfo(gen,'Qend',limtypes)$(geninfo(gen,'Qend',limtypes) le 0) = NA;
geninfo(gen,'Rend',limtypes) = geninfo(gen,'Qend',limtypes) - geninfo(gen,'Qmin',limtypes);

* At Pmax, we should be able to generate and consume reactive power corresponding to a 0.95 power factor
*geninfo(gen,'R_max',limtypes) = max(geninfo(gen,'Pmax',limtypes)*(1 + sqrt(1/sqr(0.95) - 1)),
*                                     geninfo(gen,'Qmax',limtypes));

* To find gen buses that are not suitable for the D-curve constraint (same with the meaning to find gen buses having not appropriate rectangular )

geninfo(gen,'Pmin',limtypes)$(geninfo(gen,'Pmin',limtypes) eq 0) = NA;
geninfo(gen,'Qmax',limtypes)$(geninfo(gen,'Qmax',limtypes) eq 0) = NA;
geninfo(gen,'Qfield',limtypes)$( (geninfo(gen,'Pmax',limtypes) = 0) or ((geninfo(gen,'Pmax',limtypes)/geninfo(gen,'Pmin',limtypes)) le 1.1)
                            or ((geninfo(gen,'Qmax',limtypes)-geninfo(gen,'Qmin',limtypes))/geninfo(gen,'Qmax',limtypes) le 0.1) ) = EPS  ;

geninfo(gen,'Qend',limtypes)$( ((geninfo(gen,'Pmax',limtypes)/geninfo(gen,'Pmin',limtypes)) le 1.1)
                            or ((geninfo(gen,'Qmax',limtypes)-geninfo(gen,'Qmin',limtypes))/geninfo(gen,'Qmax',limtypes) le 0.1) ) = EPS  ;
geninfo(gen,'Pmin',limtypes)$(geninfo(gen,'Pmin',limtypes) eq NA) = 0;
geninfo(gen,'Qmax',limtypes)$(geninfo(gen,'Qmax',limtypes) eq NA) = 0;
