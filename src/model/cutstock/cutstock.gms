$title Cutting Stock - A Column Generation Approach with BCH (BCHSTOCK,SEQ=349)

$onText
The task is to cut out some paper products of different sizes from a
large raw paper roll, in order to meet a customer's order. The objective
is to minimize the required number of paper rolls.

The CG method is implemented using BCH. The running LP solver calls
out to a BCH pricing call and that supplies new columns.


P. C. Gilmore and R. E. Gomory, A linear programming approach to the
cutting stock problem, Part I, Operations Research 9 (1961), 849-859.

P. C. Gilmore and R. E. Gomory, A linear programming approach to the
cutting stock problem, Part II, Operations Research 11 (1963), 863-888.

Keywords: mixed integer linear programming, cutting stock, column generation,
          branch and cut and heuristic faciliy, paper industry
$offText

$eolCom //

Set
   i_prime 'widths numeric'
   i 'widths'
   cols     'column'      / 1*1000 /
   p 'patterns' / p1*p1000 /;

Parameter
   w(i)     'width'
   d(i<)     'demand';

$onExternalInput
Parameter
   r        'Raw width' / 199 /
   demand_miro(i_prime<)  / 45 97, 36 610, 31 395, 14 211/;

$offExternalInput

$onEmbeddedCode Python:
widths = []
demand = []

for j, w in enumerate(gams.get("demand_miro")):
    widths.append(("w" + str(j + 1), float(w[0])))
    demand.append(("w" + str(j + 1), w[1]))

gams.set("w", widths)
gams.set("d", demand)
$offEmbeddedCode d, w

Parameter
   aip(i,p) 'number of width i in pattern growing in p';

* Master model
Variable
   xp(p) 'patterns used'
   z     'objective variable';

Integer Variable xp;
xp.up(p) = sum(i, d(i));

Equation
   numpat    'number of patterns used'
   demand(i) 'meet demand';

numpat..    sum(p, xp(p))          =e= z;

demand(i).. sum(p, aip(i,p)*xp(p)) =g= d(i);

Model master / numpat, demand /;

* Initialization - the initial patterns have a single width
aip(i,p)$(ord(i) = ord(p)) = floor(r/w(i));

$echo userpricingcall pricing.gms > cplex.opt

execute_unload 'data', i, w, d, r;
z.lo = 0; // We need to prevent reformulation for now

option rmip = cplex, optCr = 0; master.optFile = 1;

solve master using rmip minimizing z;

* Read back the additional columns
Set
   cc(cols) 'new columns'
   info     'column info' / level, lower, upper /;

Parameter
   demand_c(cols,i) 'patterns generated'
   col_info(cols, info);

execute_load 'bchsol.gdx', col_info, demand_c;

option cc < col_info;

* Fill the aip data with the new patters
loop((cc(cols),p)$(ord(cols) = ord(p) - card(i)), aip(i,p) = demand_c(cols,i));

master.optFile = 0;

solve master using mip minimizing z;

abort$(master.modelStat <> 1) 'wrong solution';

$onEchoV > pricing.gms
Set i;

Parameter w(i), d(i), r;

$gdxIn data
$load i w d r

Equation demand(i);

$gdxIn bchout
$load demand

* Pricing problem - Knapsack model
Variable
   z
   y(i) 'new pattern';

Integer Variable y;
y.up(i) = ceil(r/w(i));

Equation defobj, knapsack;

defobj..   z =e= 1 - sum(i, demand.m(i)*y(i));

knapsack.. sum(i, w(i)*y(i)) =l= r;

Model pricing / defobj, knapsack /;

option optCr = 0;

solve pricing using mip minimizing z;

Set cc / 1 /;
Parameter
   numcols                      'number of columns generated' / 0 /
*  level, lower, upper, type: 0 cont, 1 bin, 2 int, 3 semicont, 4 semiint
   col_info(cc,*)               'column information'
   numpat_c(cc), demand_c(cc,i) 'matrix entries';

* pattern that might improve the master model found?
if(z.l < -0.001,
   numcols              = numcols + 1;
   numpat_c(cc)         = 1;
   demand_c(cc,i)       = round(y.l(i));
   col_info(cc,'lower') = 0;
   col_info(cc,'upper') = smax(i$demand_c(cc,i), d(i)/demand_c(cc,i));
   col_info(cc,'type')  = 2;
);
$offEcho

Set ptuHdr /width,times,used/;

$onExternalOutput
Parameter
    patterns_used(i,p,ptuHdr) 'Patterns used in solution';

Table patterns_used;

Scalar width_out 'Paper roll width'
       number 'Number of paper rolls used';
$offExternalOutput

number = z.l;
width_out = r;
patterns_used(i,p,'times')$(xp.l(p) > 0) = aip(i,p);
patterns_used(i,p,'width')$(xp.l(p) > 0 and patterns_used(i,p,'times')) = w(i);
patterns_used(i,p,'used')$(xp.l(p) > 0 and patterns_used(i,p,'times')) = xp.l(p);
