$ontext
Reactive power circle constraints (see Dan's pdf for derivation).
Add to a model with $batinclude
$offtext

parameters R_max, nameplate_pf, Qfield, Rfield, Qend, Rend;
R_max(gen) = geninfo(gen,'R_max','given')/baseMVA;
nameplate_pf(gen) = geninfo(gen,'nameplate_pf','given')/baseMVA;
Qfield(gen) = geninfo(gen,'Qfield','given')/baseMVA;
Rfield(gen) = geninfo(gen,'Rfield','given')/baseMVA;
Qend(gen) = geninfo(gen,'Qend','given')/baseMVA;
Rend(gen) = geninfo(gen,'Rend','given')/baseMVA;

equations
    c_Armature(gen)     "Armature current limit for reactive power",
    c_Field(gen)        "Field current limit for reactive power",
    c_Heating(gen)      "End region heating limit for reactive power"
;

c_Armature(gen)$(status(gen) and (Qfield(gen) ne EPS))..
    sqr(V_P(gen)) + sqr(V_Q(gen)) =l= sqr(R_max(gen))
;

c_Field(gen)$(status(gen) and (Qfield(gen) ne NA) and (Qfield(gen) ne EPS))..
    sqr(V_P(gen)) + sqr(V_Q(gen) - Qfield(gen)) =l= sqr(Rfield(gen))
;

c_Heating(gen)$(status(gen) and (Qend(gen) ne NA) and (Qend(gen) ne EPS))..
    sqr(V_P(gen)) + sqr(V_Q(gen) - Qend(gen)) =l= sqr(Rend(gen))
;

* To represent the lower portion of the capability curve as a horizonrtal line when Qend is not physical
* To use rectangular constraints when size of box is not big enough to be inside of the D-curve
V_Q.lo(gen)$(status(gen) and (Qend(gen) eq NA) or (Qfield(gen) eq EPS)) = Qmin(gen);
* To represent the upper portion of the capability curve as a horizonrtal line when Qfield is not physical
* To use rectangular constraints when size of box is not big enough to be inside of the D-curve
V_Q.up(gen)$(status(gen) and (Qfield(gen) eq NA) or (Qfield(gen) eq EPS)) = Qmax(gen);
