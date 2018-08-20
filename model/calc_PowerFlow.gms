$title "Calculate Power Flow"
$offlisting
option limrow=0, limcol=0 ;

free variables
    Theta_pf(bus)         "Bus voltage angle" ;
positive variables
    shunt(bus,bus_s),
    V_pf(bus)             "Bus voltage magnitude" ;

shunt.up(bus,bus_s) = numBswitched(bus,bus_s);
$if %switchedshunts% == 0 shunt.fx(bus,bus_s) = shunt.up(bus,bus_s);

equations
    c_BalanceP(bus),
    c_BalanceQ(bus);

c_BalanceP(i)$(type(i) < 3 )..
          sum(gen$(atBus(gen,i) and status(gen)), Pg(gen))
          - Pd(i)
            =e=
          V_pf(i) * sum(j, V_pf(j)*(yb(i,j,'real')*cos(Theta_pf(i) - Theta_pf(j))
                                  +yb(i,j,'imag')*sin(Theta_pf(i) - Theta_pf(j)))
                    )
          + sqr(V_pf(i)) * Gs(i)
;

c_BalanceQ(i)$(type(i) = 1)..
          sum(gen$(atBus(gen,i) and status(gen)), Qg(gen))
          - Qd(i)
            =e=
          V_pf(i) * sum(j, V_pf(j)*(yb(i,j,'real')*sin(Theta_pf(i) - Theta_pf(j))
                             -yb(i,j,'imag')*cos(Theta_pf(i) - Theta_pf(j)))
                    )
          - sqr(V_pf(i)) * Bs(i)
          - sqr(V_pf(i)) * sum(bus_s$(not sameas(bus_s,'given')), Bswitched(i,bus_s) * shunt(i,bus_s))
;

model powerflow / c_BalanceP, c_BalanceQ /;

* Set bounds
V_pf.fx(bus)$(type(bus) eq 3) = Vm(bus)$(type(bus) eq 3);
V_pf.fx(bus)$(type(bus) eq 2) = Vm(bus)$(type(bus) eq 2);
Theta_pf.fx(bus)$(type(bus) eq 3) = 0;

Theta_pf.up(bus)$(type(bus) ne 3) = pi;
Theta_pf.lo(bus)$(type(bus) ne 3) = -pi;

V_pf.l(bus) = 1;
* Theta can stay 0, since angles are allowed to range in (-pi, pi)
Theta_pf.l(bus)= 0;
solve powerflow using cns;

