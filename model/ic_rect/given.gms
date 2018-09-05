$ontext
Use given initial conditions for Polar ACOPF.
$offtext

$if not set filepath $setnames "%gams.i%" filepath filename fileextension

V_P.l(gen)$status(gen) = Pg(gen);
V_Q.l(gen)$status(gen) = Qg(gen);
V_real.l(bus) = Vm(bus)*cos(Va(bus));
V_imag.l(bus) = Vm(bus)*sin(Va(bus));

Parameter Bs_solved(bus);
Bs_solved(bus) = businfo(bus,'switchedBsSolved','%timeperiod%')/baseMVA;
loop((bus,bus_s)$((not sameas(bus_s,'given') and (Bswitched(bus,bus_s) ne 0)) and (abs(Bs_solved(bus)) > 1e-6)),
    V_shunt.l(bus,bus_s) = min(Bs_solved(bus)/Bswitched(bus,bus_s), V_shunt.up(bus,bus_s));
    Bs_solved(bus) = Bs_solved(bus) - V_shunt.l(bus,bus_s)*Bswitched(bus,bus_s);
);


$if %condensed% == 'no' $include '%MODELPATH%ic_rect%sep%calc_derived_vars.gms'

V_pw_cost.l(gen)$(status(gen) and (costmodel(gen) eq 1)) = max(0,
    smax(costptset$(ord(costptset) < numcostpts(gen)),
            ((costpts_y(gen,costptset+1) - costpts_y(gen,costptset))/
             (costpts_x(gen,costptset+1) - costpts_x(gen,costptset)))
              * (V_P.l(gen)*baseMVA - costpts_x(gen,costptset))
            + costpts_y(gen,costptset) - noloadcost(gen)))
;

V_objcost.l = total_cost;

