$title A Transportation Problem with multiple version LP/MIP/MINLP
$onText
This problem finds a least cost shipping schedule that meets
requirements at markets and supplies at factories.


Dantzig, G B, Chapter 3.3. In Linear Programming and Extensions.
Princeton University Press, Princeton, New Jersey, 1963.

$offText

Set
   i      'canning plants'
   j      'markets'
   t      'available model types' / lp, mip, minlp /
   locHdr 'location data header'  / 1, 2 /;

$onExternalInput
Singleton Set type(t) 'selected model type' / lp /;

Parameter
   a(i<) 'capacity of plant i in cases'
        / Seattle     350
          San-Diego   600 /

   b(j<) 'demand at market j in cases'
        / New-york   325
          Chicago   300
          Topeka   275 /;

Table d(i,j) 'distance in thousands of miles'
              New-York  Chicago  Topeka
Seattle         2.5       1.7     1.8
San-Diego       2.5       1.8     1.4;

Scalar f 'freight in dollars per case per thousand miles' / 90 /
       minS 'minimum shipment (MIP- and MINLP-only)' / 100 /
       beta 'beta (MINLP-only)' / 0.95 /;

Table ilocData(i,locHdr) 'Plant location information'
               1           2
Seattle     47.608013  -122.335167
San-Diego   32.715736  -117.161087;

Table jlocData(j,locHdr) 'Market location information'
           1           2
New-York   40.730610  -73.935242
Chicago    41.881832  -87.623177
Topeka     39.056198  -95.695312;
$offExternalInput

Parameter
c(i,j) 'transport cost in thousands of dollars per case';
c(i,j) = f*d(i,j)/1000;


* input validataion
$ifthen set largeLog
embeddedCode Python:
with open('miro.log', 'w') as f:
   f.write('ABC\n'*int(1e5))
endEmbeddedCode
$else
file log / miro.log /;
put log '------------------------------------'/;
put log '        Validating data'/;
put log '------------------------------------'/;
if(sum(i, a(i)) < sum(j, b(j)),
  put log 'a:: Capacity insufficient to meet demand'/;
else
  put log 'OK'/;
);
putclose log;
$endif

Variable
   x(i,j) 'shipment quantities in cases'
   z      'total transportation costs in thousands of dollars';

Positive Variable x;

Equation
   cost        'define objective function'
   supply(i)   'observe supply limit at plant i'
   demand(j)   'satisfy demand at market j';

cost ..        z  =e=  sum((i,j), c(i,j)*x(i,j));

supply(i).. sum(j, x(i,j)) =l= a(i);

demand(j).. sum(i, x(i,j)) =g= b(j);

Model transportLP / all /;

scalar bigM big M;
bigM = min(smax(i,a(i)), smax(j,b(j)));

binary variable ship(i,j) '1 if we ship from i to j, otherwise 0';

equation minship(i,j) minimum shipment
         maxship(i,j) maximum shipment;

minship(i,j) .. x(i,j) =g= minS * ship(i,j);
maxship(i,j) .. x(i,j) =l= bigM * ship(i,j);

Model  transportMIP / transportLP, minship, maxship / ;
option optcr = 0;

Equation
   costnlp 'define non-linear objective function';
   costnlp .. z  =e=  sum((i,j), c(i,j)*x(i,j)**beta) ;

Model transportMINLP / transportMIP - cost + costnlp /;

$eval.Set type type.TL
$if not set type $set type lp

*some starting point
x.l(i,j) = 1;

$if set sleep $call sleep 25

solve transport%type% using %type% minimizing z;
abort$(transport%type%.modelstat > 2 and transport%type%.modelstat <> 8) "No feasible solution found"


Set scheduleHdr 'schedule header' / '1', '2', '3', '4', '5', '6', '7' /;

$onExternalOutput
Parameter
   schedule(i,j,scheduleHdr) 'shipment quantities in cases'
   total_cost                'total transportation costs in thousands of dollars';
Table schedule;
$offExternalOutput

total_cost = z.l;
schedule(i,j, '1')       = iLocData(i,'2');
schedule(i,j, '2')       = iLocData(i,'1');
schedule(i,j, '3')       = jLocData(j,'2');
schedule(i,j, '4')       = jLocData(j,'1');
schedule(i,j, '5')        = a(i);
schedule(i,j, '6')     = b(j);
schedule(i,j, '7') = x.l(i,j);
