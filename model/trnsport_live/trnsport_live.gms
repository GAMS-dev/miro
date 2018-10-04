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

set
   locHdr 'location data header' /lat, lng/;

*configuration of WebUI input
$ifthen set gmswebui
$onecho > webuiconf.json
{ "GMSPAR_type": {"alias": "Model Type", "dropdown": {"label": "Select model type", "aliases": ["LP", "MIP", "MINLP"], "choices": ["lp", "mip", "minlp"]}}}
$offecho
$endif

$onExternalInput
Set
   i 'canning plants' / seattle,  san-diego /
   j 'markets'        / new-york, chicago, topeka /;
Parameter
   a(i) 'capacity of plant i in cases'
        / seattle   350
          san-diego   600 /

   b(j) 'demand at market j in cases'
        / new-york   325
          chicago   300
          topeka   275 /;

Scalar f 'freight in dollars per case per thousand miles ### { "slider":{"min":1, "max":500, "default":90,  "step":1 }}' / 90 /
       minS 'minimum shipment (MIP- and MINLP-only) ### { "slider":{"min":0, "max":500, "default":100,  "step":1 }}' / 100 /
       beta 'beta (MINLP-only) ### { "slider":{"min":0, "max":1, "default":0.95,  "step":0.01 }}' / 0.95 /;
$offExternalInput

Parameter
ilocData(i,locHdr) 'Plant location information',
jlocData(j,locHdr) 'Market location information'
d(i,j) 'distance in thousands of miles'
c(i,j) 'transport cost in thousands of dollars per case';


EmbeddedCode Python:
import geocoder
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
    g = geocoder.osm(plant)
    coords = tuple(g.latlng)
    iLocData.append((plant, 'lat', coords[0]))
    iLocData.append((plant, 'lng', coords[1]))
    iCoords.append(coords)
gams.set("iLocData", iLocData)

j = list(gams.get("j"))
jCoords = []
jLocData = []
for market in j:
    g = geocoder.osm(market)
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

$ifthen.lp not %type% == "mip"
solve transportLP using lp minimizing z;
abort$(transportLP.modelstat <> 1) "No feasible solution found"
$endif.lp



* MIP
$ifthen.noLP not %type% == "lp"
scalar bigM big M;
bigM = min(smax(i,a(i)), smax(j,b(j)));

binary variable ship(i,j) '1 if we ship from i to j, otherwise 0';

equation minship(i,j) minimum shipment
         maxship(i,j) maximum shipment;

minship(i,j).. x(i,j) =g= minS * ship(i,j);
maxship(i,j).. x(i,j) =l= bigM * ship(i,j);

Model transportMIP / transportLP, minship, maxship / ;
option optcr = 0;
$endif.noLP

$ifthen.mip %type% == "mip"
Solve transportMIP using MIP minimizing z ;
abort$(transportMIP.modelstat <> 1) "No feasible solution found"
$endif.mip


* MINLP
$ifthen.minlp %type% == "minlp"
Equation  costnlp define non-linear objective function;
costnlp.. z  =e=  sum((i,j), c(i,j)*x(i,j)**beta) ;

Model transportMINLP / transportMIP - cost + costnlp /;
Solve transportMINLP using MINLP minimizing z ;
abort$(transportMINLP.modelstat > 2 and transportMINLP.modelstat <> 8) "No feasible solution found"
$endif.minlp



Set
scheduleHdr 'schedule header' / 'lngP', 'latP', 'lngM',
'latM', 'cap', 'demand', 'quantities' /;
$onExternalOutput
Parameter
schedule(i,j,scheduleHdr) 'shipment quantities in cases';

Scalar
total_cost 'total transportation costs in thousands of dollars';
$offExternalOutput

$ifthen.type set type
total_cost = z.l;

schedule(i,j, 'lngP') = iLocData(i,'lng');
schedule(i,j, 'latP') = iLocData(i,'lat');
schedule(i,j, 'lngM') = jLocData(j,'lng');
schedule(i,j, 'latM') = jLocData(j,'lat');
schedule(i,j, 'cap') = a(i);
schedule(i,j, 'demand') = b(j);
schedule(i,j, 'quantities') = x.l(i,j);
$endif.type

$if not exist webui.gms
$if set GMSWEBUI $abort Asked to do webui but can't find webui.gms. Set idir=path/to/webui
$if set gmswebui $batinclude webui