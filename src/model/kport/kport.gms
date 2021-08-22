$title Product Portfolio Optimization

$onText
This problem computes minimal cost solutions satisfying the
demand of pre-given product portfolios. It determines the number
and size of reactors and gives a schedule of how may batches of
each product run on each reactor. There are two scenarios (s1 20
products. s2 40 products), add --scenario s2 as a GAMS
parameter to specify the second scenario.

The global optimal reactor volumes are:
data set s1
vr.fx('r1') = 132.5; vr.fx('r2') = 250;

data set s2
vr.fx('r1') = 20; vr.fx('r2') = 100; vr.fx('r3') = 250;

Two formulations are presented, a compact MINLP formulations
and a linearized MIP formulation using special ordered sets.

Problem sizes for data set s1

                        MINLP        MIP
variables                 102        548
equations                  28        418
Non-zeros                 190       1574
discrete variables         40        186


Kallrath, J. Exact Computation of Global Minima of a Nonconvex
Portfolio Optimization Problem. In Frontiers in Global Optimization.
Eds Floudas C A and Pardalos P M. Kluwer Academic Publishers,
Dortrecht, 2003.

Keywords: mixed integer nonlinear programming, mixed integer linear programming,
          portfolio optimization, special ordered sets, global optimization,
          concave objective functions, chemical engineering

$offText

$eolCom //

Set
   rhdr 'Reactor data headers' / VMIN,VMAX/
   phdr 'Product data headers' / Demand, 'Production Time' /
   s    'Scenario'             / s1,s2  /
   rR   'Reactor'              / R1*R3  /
   pP   'Product'              / L1*L37 /;

$if not set scenario $set scenario s1

$onExternalInput
Singleton set actScen(s) 'Active scenario' / %scenario% /;

Table RData(rR,s,rhdr) 'Reactor data'
        s1.VMIN  s1.VMAX  s2.VMIN  s2.VMAX
   R1    102.14      250     20         50
   R2    176.07      250     52.5      250
   R3                      151.25      250;

Table PData(pP,s,phdr) 'Product data'
         s1.Demand  s1.'Production Time'  s2.Demand  s2.'Production Time'
   L1         2600                    6        2600                    6
   L2         2300                    6        2300                    6
   L3         1700                    6         450                    6
   L4          530                    6        1200                    6
   L5          530                    6         560                    6
   L6          280                    6         530                    6
   L7          250                    6         530                    6
   L8          230                    6         140                    6
   L9          160                    6         110                    6
   L10          90                    6         110                    6
   L11          70                    6          10                    6
   L12         390                    6         110                    6
   L13         250                    6          90                    6
   L14         160                    6          90                    6
   L15         100                    6          90                    6
   L16          70                    6          70                    6
   L17          50                    6          50                    6
   L18          50                    6          30                    6
   L19          50                    6          10                    6
   L20                                           10                    6
   L21                                           10                    6
   L22                                          190                    6
   L23                                          180                    6
   L24                                           70                    6
   L25                                           70                    6
   L26                                           40                    6
   L27                                           40                    6
   L28                                           40                    6
   L29                                           30                    6
   L30                                           20                    6
   L31                                           20                    6
   L32                                           20                    6
   L33                                           10                    6
   L34                                           10                    6
   L35                                           10                    6
   L36                                           10                    6
   L37                                           10                    6;

Scalar
   WHRS 'Length of considered period [hours]'                  / 168  /
   CSTI 'Depreciation cost per m^3 reactor and period [k€]'    / 0.97 /
   CSTF 'Fixed cost per week and reactor [k€]'                 / 2.45 /
   ESF  'Economies of scale factor'                            / 0.5  /;
$offExternalInput

Set
   r(rR)  'reactors considered in scenario'
   p(pP)  'products considered in scenario';

Parameter
   VMIN(rR)     'volume flow of products in m^3 per week'
   VMAX(rR)     'volume flow of products in m^3 per week'
   DEMAND(pP)   'volume flow of products in m^3 per week'
   PRODTIME(pP) 'production time in hours per batch';

VMIN(rR)     = RDATA(rR,actScen,'VMIN');
VMAX(rR)     = RDATA(rR,actScen,'VMAX');
DEMAND(pP)   = PDATA(pP,actScen,'Demand');
PRODTIME(pP) = PDATA(pP,actScen,'Production Time');

* determine scenario sets
r(rR) = VMAX(rR)   > 0;
p(pP) = DEMAND(pP) > 0;

* definition of compact MINLP model
Variable
$onExternalOutput
   cTotal    'total  costs'
$offExternalOutput
   cInvest   'invest cost'
   cFixed    'fix    costs'
   f(rR,pP)  'utilization rate'
   vR(rR)    'reactor volume in m^3'
   pS(pP)    'surplus production'
   bvr(rR)   'indicating whether reactor r is active'
   nB(rR,pP) 'number of batches of product p in reactor r';

Positive Variable f, vR, pS;
Integer  Variable nB;
Binary   Variable bvr;

Equation
   DEFcT    'total costs'
   DEFcF    'fix costs'
   DEFcI    'invest cost'
   TR(rR)   'production time of reactor r'
   SPP(pP)  'compute surplus production p'
   RVUB(rR) 'maximal volume of reactor r'
   RVLB(rR) 'minimal volume reactor r';

* define the total cost
DEFcT..   cTotal  =e= cFixed + cInvest;

DEFcF..   cFixed  =e= sum(r, CSTF*bvr(r));

DEFcI..   cInvest =e= sum(r, CSTI**ESF*vR(r)**ESF);

* limit the total production time of reactor r
TR(r)..   sum(p, PRODTIME(p)*nB(r,p)) =l= WHRS*bvr(r);

* compute the surplus production
SPP(p)..  pS(p) =e= sum(r, nB(r,p)*f(r,p)*vR(r))/DEMAND(p) - 1;

* lower and upper bounds on reactor volume
RVLB(r).. vR(r) =g= VMIN(r)*bvr(r);

RVUB(r).. vR(r) =l= VMAX(r)*bvr(r);

Model portfolioMINLP / DEFcT, DEFcF, DEFcI, TR, SPP, RVLB, RVUB /;

f.lo(r,p) = 0.4; f.up(r,p) = 1;  // bounds on the utilization rates
pS.lo(p)  =   0; pS.up(p)  = 1;  // bounds on the surplus production

* bounds on the number of batches
nB.lo(r,p) = 0;
nB.up(r,p) = min(WHRS/PRODTIME(p),floor(2*DEMAND(p)/(VMIN(r)*f.lo(r,p))));
nB.up(r,p)$(2*DEMAND(p) < f.lo(r,p)*VMIN(r)) = 0;

vR.l(rR) = 99;
vR.lo(r) = VMIN(r);

*solve portfolioMINLP using minlp minimizing cTotal;

* additional variables and equations to define the MIP formulation
* first we need to linearize the product terms:

Set
   i            'dyadic represenation set'       / 0*10 /
   j            'discretization points for SOS2' / 0*10 /
   rpi(rR,pP,i) 'i required for representing np';

Parameter
   vRj(rR,j)    'x part of SOS2 construct'
   ESFvRj(rR,j) 'y part of SOS2 construct';

Positive Variable
   pT(rR,pP)    'number of batches x reactor volume in m^3'
   pT2(rR,pP,i) 'same for in dyadic representation'
   ESFvR(rR)    'economies of scale for vR';

Binary Variable
   nBx(rR,pP,i) 'dyadic represenation of nB';

SOS2 Variable
   lambda(rR,j) 'approximation of economies of scale function';

Equation
$onExternalOutput
   SPPx(pP)       'compute surplus production p'
$offExternalOutput
   CNPl0(rR,pP)   'linearized version of CNP'
   CNPl1(rR,pP)   'linearized version of CNP'
   CNPl2(rR,pP,i) 'linearized version of CNP'
   CNPl3(rR,pP,i) 'linearized version of CNP'
   CNPl4(rR,pP,i) 'linearized version of CNP'
   DEFSOSx(rR)    'SOS2 x construct'
   DEFSOSy(rR)    'SOS2 y construct'
   DEFSOSone(rR)  'SOS2 sum construct'
   DEFcIlp        'linearized version of DEFcI';


* new surplus production equation
SPPx(p)..   pS(p)   =e= sum(r, pT(r,p))/DEMAND(p) - 1;

* Linearized version of CNP
CNPl0(r,p)..        pT(r,p)  =e= sum(rpi(r,p,i),2**(ord(i) - 1)*pT2(rpi));

CNPl1(r,p)..        nB(r,p)  =e= sum(rpi(r,p,i),2**(ord(i) - 1)*nBx(rpi));

CNPl2(rpi(r,p,i)).. pT2(rpi) =l= VMAX(r)*nBx(rpi);

CNPl3(rpi(r,p,i)).. pT2(rpi) =l= vR(r);

CNPl4(rpi(r,p,i)).. pT2(rpi) =g= f.lo(r,p)*(vR(r) - VMAX(r)*(1 - nbx(rpi)));

* SOS2 approximation of economies of scale function
DEFSOSx(r)..   vR(r)    =e= sum(j, vRj(r,j)*lambda(r,j));

DEFSOSy(r)..   ESFvR(r) =e= sum(j, ESFvRj(r,j)*lambda(r,j));

DEFSOSone(r).. sum(j, lambda(r,j)) =e= 1;

DEFcIlp..      cInvest =e= sum(r, CSTI**ESF*ESFvR(r));

rpi(r,p,i)  = ord(i) <= ceil(log(max(1,nB.up(r,p)))/log(2)) + 1$nB.up(r,p);
vRj(r,j)    = (VMAX(r) - VMIN(r))*(ord(j) - 1)/(card(j) - 1) + VMIN(r);
ESFvRj(r,j) = vRj(r,j)**ESF;

Model PortfolioMIP / TR,      SPPx,  RVLB,    RVUB,    DEFcF
                     DEFcIlp, DEFcT, CNPl0,   CNPl1,   CNPl2
                     CNPl3,   CNPl4, DEFSOSx, DEFSOSy, DEFSOSone /;

portfolioMIP.optCr = 1.05;

solve portfolioMIP using mip minimizing cTotal;
abort$(portfolioMIP.modelstat<>%modelstat.optimal% and
       portfolioMIP.modelstat<>%modelstat.integerSolution%) 'No feasible solution';

Set
   CHdr   'Cost Header'           / 'Fixed Cost', 'Investment Cost' /
   SHdr   'Surplus Header'        / Demand, Surplus /
   PTHdr  'ProductionTime Header' / 'Production Time', 'max. Production Time' /
   RPHdr  'RP Header'             / 'Utilization rate', 'Number of batches' /;

$onExternalOutput
Parameter
   Cost(CHdr)                     'Cost report'
   Surplus(pP,SHdr)               'Demand/Surplus report'
   ProductionTime(rR,PTHdr)       'Production time report'
   RPreport(rR,pP,RPHdr)          'Reactor/Product report';
Table Surplus, ProductionTime, RPreport;
$offExternalOutput

Cost('Fixed Cost')      = cFixed.l;
Cost('Investment Cost') = cInvest.l;

Surplus(p,'Demand')     = demand(p);
Surplus(p,'Surplus')    = demand(p)*pS.l(p);

ProductionTime(r,'Production Time')      = sum(p, PRODTIME(p)*nB.l(r,p));
ProductionTime(r,'max. Production Time') = WHRS*bvr.l(r);

RPreport(r,p,'Utilization rate')$(nb.l(r,p)*vR.l(r)) = pT.l(r,p)/(nb.l(r,p)*vR.l(r));
RPreport(r,p,'Number of batches') = nb.l(r,p);

display Cost, Surplus, ProductionTime, RPreport;
