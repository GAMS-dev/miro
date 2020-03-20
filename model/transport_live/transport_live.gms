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
   locHdr 'location data header' /lat, lng/;

$onExternalInput   
Parameter
   a(i<) 'capacity of plant i in cases'
        / London  550
          Warsaw  425 /

   b(j<) 'demand at market j in cases'
        / Madrid   325
          Munich   300
          Istanbul 275 /;

Scalars
   f    'freight in dollars per case per thousand miles' /  90  /
   minS 'minimum shipment (MIP- and MINLP-only)'         / 100  /
   beta 'beta (MINLP-only)'                              / 0.95 /;
$offExternalInput

Parameter
   ilocData(i,locHdr) 'Plant location information',
   jlocData(j,locHdr) 'Market location information'
   d(i,j)             'distance in thousands of miles'
   c(i,j)             'transport cost in thousands of dollars per case';


EmbeddedCode Python:
from geocoder import osm
import itertools
import geopy.distance

loc = {}
def mkLocData(s):
   LocData = []
   for i in gams.get(s):
      g = osm(i)
      LocData.append((i, 'lat', g.latlng[0]))
      LocData.append((i, 'lng', g.latlng[1]))
      loc[i] = g.latlng
   gams.set(s+"LocData", LocData)

mkLocData('i')
mkLocData('j')
    
d = []
for i,j in itertools.product(gams.get("i"),gams.get("j")):
    d.append((i,j,geopy.distance.distance(loc[i],loc[j]).miles/1000))
gams.set("d", d)
endEmbeddedCode iLocData, jLocData, d

c(i,j) = f*d(i,j)/1000;

Variable
   x(i,j) 'shipment quantities in cases'
   z      'total transportation costs in thousands of dollars';

Positive Variable x;

Equation
  cost        define objective function
  supply(i)   observe supply limit at plant i
  demand(j)   satisfy demand at market j ;

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

$if not set type $set type lp

*some starting point
x.l(i,j) = 1;

solve transport%type% using %type% minimizing z;
abort$(transport%type%.modelstat > 2 and transport%type%.modelstat <> 8) "No feasible solution found"

Set scheduleHdr 'schedule header' / 'lngP', 'latP', 'lngM', 'latM', 'cap', 'demand', 'quantities' /;

$onExternalOutput
Parameter
   schedule(i,j,scheduleHdr) 'shipment quantities in cases'
   total_cost                'total transportation costs in thousands of dollars';
Table schedule;
$offExternalOutput

$ifthen set type
total_cost = z.l;

schedule(i,j, 'lngP') = iLocData(i,'lng');
schedule(i,j, 'latP') = iLocData(i,'lat');
schedule(i,j, 'lngM') = jLocData(j,'lng');
schedule(i,j, 'latM') = jLocData(j,'lat');
schedule(i,j, 'cap') = a(i);
schedule(i,j, 'demand') = b(j);
schedule(i,j, 'quantities') = x.l(i,j);
$endif
 
