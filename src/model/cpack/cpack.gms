$title Packing identical size circles in the unit circle (CPACK,SEQ=387)

$onText
Given the unit circle (of radius 1), find a set of identical
size circles with an optimized (maximal) radius r so that all
such circles are contained by the unit circle, in a non-overlapping
arrangement.

A test example from  the LGO library


Pinter, J D, Nonlinear optimization with GAMS/LGO.
Journal of Global Optimization 38 (2007), 79-101.

Keywords: quadratic constraint programming, circle packing problem, mathematics
$offText

$onExternalInput
scalar k number of circles / 5 /;
$offExternalInput

$eval K k
set i inner circles / innerCircle_1*innerCircle_%K% /;

Alias (i,j);

* Here we define the set ij(i,j) of ordered pairs i,j i < j.
Set ij(i,j); ij(i,j)$(ord(i) < ord(j)) = yes;

Variable
   r    'radius of identical sized circles'
   x(i) 'x coordinate of circle i'
   y(i) 'y coordinate of circle i';

Equation
   circumscribe(i) 'enforce circle is enclose in unit circle'
   nooverlap(i,j)  'enforce that circles do not overlap';

circumscribe(i)..    sqr(1 - r) =g= sqr(x(i)) + sqr(y(i));

nooverlap(ij(i,j)).. sqr(x(i) - x(j)) + sqr(y(i) - y(j)) =g= 4*sqr(r);

x.lo(i) = -1; x.up(i) = 1;
y.lo(i) = -1; y.up(i) = 1;

* starting values such that some feasible solution is produced
r.l    = 1/k;
x.l(i) = -1 + (2*ord(i)-1)*r.l;
y.l(i) = 0;

* bounds on r
r.lo = r.l; r.up = sqrt(1/k);

Model m / all /;

$iftheni.grb %gams.qcp%==gurobi
  m.optfile = 1;
$ echo nonconvex 2 > gurobi.opt
$endif.grb

solve m using qcp maximizing r;

set circleDataHeader / x,y,radius /;
alias(*,circle)
$onExternalOutput
table circleData(circle,circleDataHeader) circle data;
$offExternalOutput

circleData('outerCircle','x')      = eps;
circleData('outerCircle','y')      = eps;
circleData('outerCircle','radius') = 1;
circleData(i,'x')      = x.l(i) + eps;
circleData(i,'y')      = y.l(i) + eps;
circleData(i,'radius') = r.l + eps;

File mlog /cpack_miroLog.dat/;

put mlog '--------------------------------'/
         '   Circle Packing MIRO Report   '/
         '--------------------------------'/ /
         'Outer Circle Area          : ' pi:0:4 /
         'Area covered by start point: ' (card(i)*pi*sqr(1/k)):0:4 /
         'Area covered by solution   : ' (card(i)*pi*sqr(r.l)):0:4 /
;
