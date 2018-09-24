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
   i 'canning plants' / seattle,  san-diego /
   j 'markets'        / new-york, chicago, topeka /
   aHdr 'a header' / capacity /
   bHdr 'b header' / demand /
   dHdr 'd header' / distance /
;
Table ilocData(i,*) 'Plant location information'
           lat           lng     
seattle   47.608013  -122.335167
san-diego 32.715736  -117.161087;
Table jlocData(j,*) 'Market location information'
           lat           lng     
new-york   40.730610  -73.935242
chicago    41.881832  -87.623177
topeka     39.056198  -95.695312;

$onExternalInput
Parameter
   aExt(i,aHdr) 'capacity of plant i in cases'
        / seattle.capacity   350
          san-diego.capacity   600 /

   bExt(j,bHdr) 'demand at market j in cases'
        / new-york.demand   325
          chicago.demand   300
          topeka.demand   275 /;

Table dExt(i,j,dHdr) 'distance in thousands of miles'
              new-york.distance  chicago.distance  topeka.distance
   seattle           2.5               1.7              1.8
   san-diego         2.5               1.8              1.4;

Scalar f 'freight in dollars per case per thousand miles ### { "slider":{"min":1, "max":500, "default":90,  "step":1 }}' / 90 /;
$offExternalInput

Parameter
a(i) 'capacity of plant i in cases'
b(j) 'demand at market j in cases'
d(i,j) 'distance in thousands of miles'
c(i,j) 'transport cost in thousands of dollars per case';


a(i) = aExt(i,'capacity');
b(j) = bExt(j,'demand');
d(i,j) = dExt(i,j,'distance');
c(i,j) = f*d(i,j)/1000;

Variable
   x(i,j) 'shipment quantities in cases'
   z      'total transportation costs in thousands of dollars';

Positive Variable x;

Equation
   cost      'define objective function'
   supply(i) 'observe supply limit at plant i'
   demand(j) 'satisfy demand at market j';

cost..      z =e= sum((i,j), c(i,j)*x(i,j));

supply(i).. sum(j, x(i,j)) =l= a(i);

demand(j).. sum(i, x(i,j)) =g= b(j);

Model transport / all /;

solve transport using lp minimizing z;

Set
scheduleHdr 'schedule header' / 'lngP', 'latP', 'lngM',
'latM', 'cap', 'demand', 'quantities' /;
$onExternalOutput
Parameter
schedule(i,j,scheduleHdr) 'shipment quantities in cases';

Scalar
total_cost 'total transportation costs in thousands of dollars';
$offExternalOutput

total_cost = z.l;

schedule(i,j, 'lngP') = iLocData(i,'lng');
schedule(i,j, 'latP') = iLocData(i,'lat');
schedule(i,j, 'lngM') = jLocData(j,'lng');
schedule(i,j, 'latM') = jLocData(j,'lat');
schedule(i,j, 'cap') = a(i);
schedule(i,j, 'demand') = b(j);
schedule(i,j, 'quantities') = x.l(i,j);

$batinclude webui
