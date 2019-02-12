$title A Transportation Problem (TRNSPORT,SEQ=1)
$onText
This problem finds a least cost shipping schedule that meets
requirements at markets and supplies at factories.


Dantzig, G B, Chapter 3.3. In Linear Programming and Extensions.
Princeton University Press, Princeton, New Jersey, 1963.

This formulation is described in detail in:
Rosenthal, R E, Chapter 2: A GAMS Tutorial. In GAMS: A User's Guide.
The Scientific Press, Redwood City, California, 1988.

The line numbers will not match those in the book because of these
comments.

Keywords: linear programming, transportation problem, scheduling
$offText

Set
   locHdr 'location data header' /lat, lng/
   i      'canning plants' 
   j      'markets';

$onExternalInput   
Parameter
   a(i<) 'capacity of plant i in cases'
        / London  550
          Warsaw  425 /

   b(j<) 'demand at market j in cases'
        / Madrid   325
          Munich   300
          Istanbul 275 /;

Scalar f    'freight in dollars per case per thousand miles' /  90  /
       minS 'minimum shipment (MIP- and MINLP-only)'         / 100  /
       beta 'beta (MINLP-only)'                              / 0.95 /;
$offExternalInput

Parameter
ilocData(i,locHdr) 'Plant location information',
jlocData(j,locHdr) 'Market location information'
d(i,j)             'distance in thousands of miles'
c(i,j)             'transport cost in thousands of dollars per case';


EmbeddedCode Python:
try:
   from geocoder import osm
except:
   import pip
   if(hasattr(pip, 'main')):
      pip.main(['install', 'geocoder'])
   else:
      pip._internal.main(['install', 'geocoder'])
   from geocoder import osm
from math import sin, cos, sqrt, atan2, radians
import itertools

def calcDist(coords):
    lat1 = radians(coords[0][0])
    lon1 = radians(coords[0][1])
    lat2 = radians(coords[1][0])
    lon2 = radians(coords[1][1])
    R = 6373.0
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    return R * c * 0.62137119 / 1000

i = list(gams.get("i"))
iCoords = []
iLocData = []
for plant in i:
    g = osm(plant)
    coords = tuple(g.latlng)
    iLocData.append((plant, 'lat', coords[0]))
    iLocData.append((plant, 'lng', coords[1]))
    iCoords.append(coords)
gams.set("iLocData", iLocData)

j = list(gams.get("j"))
jCoords = []
jLocData = []
for market in j:
    g = osm(market)
    coords = tuple(g.latlng)
    jLocData.append((market, 'lat', coords[0]))
    jLocData.append((market, 'lng', coords[1]))
    jCoords.append(coords)
    
gams.set("jLocData", jLocData)
    
coords = list(itertools.product(iCoords,jCoords))
d = []
for idx in range(len(coords)):
    d.append((i[idx//len(j)], j[idx%len(j)], calcDist(coords[idx])))

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
   schedule(i,j,scheduleHdr) 'shipment quantities in cases [MIRO:table]'
   total_cost                'total transportation costs in thousands of dollars';
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
