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
   locHdr 'location data header'  / lat, lng /;

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
               lat           lng
Seattle     47.608013  -122.335167
San-Diego   32.715736  -117.161087;

Table jlocData(j,locHdr) 'Market location information'
           lat           lng
New-York   40.730610  -73.935242
Chicago    41.881832  -87.623177
Topeka     39.056198  -95.695312;
Set active_plants(i) 'active plants' / Seattle, San-Diego /;
$offExternalInput

$ifE card(active_plants)==0 $abort "Have to select at least one plant"
Parameter
c(i,j) 'transport cost in thousands of dollars per case';
c(i,j) = f*d(i,j)/1000;


* input validataion
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

Variable
   x(i,j) 'shipment quantities in cases'
   z      'total transportation costs in thousands of dollars';

Positive Variable x;

Equation
   cost        'define objective function'
   supply(i)   'observe supply limit at plant i'
   demand(j)   'satisfy demand at market j';

cost ..        z  =e=  sum((active_plants,j), c(active_plants,j)*x(active_plants,j));

supply(active_plants).. sum(j, x(active_plants,j)) =l= a(active_plants);

demand(j).. sum(active_plants, x(active_plants,j)) =g= b(j);

Model transportLP / all /;

scalar bigM big M;
bigM = min(smax(active_plants(i),a(i)), smax(j,b(j)));

binary variable ship(i,j) '1 if we ship from i to j, otherwise 0';

equation minship(i,j) minimum shipment
         maxship(i,j) maximum shipment;

minship(i,j) .. x(i,j) =g= minS * ship(i,j);
maxship(i,j) .. x(i,j) =l= bigM * ship(i,j);


solve transportLP using lp minimizing z;
abort$(transportLP.modelstat > 2 and transportLP.modelstat <> 8) "No feasible solution found"


Set scheduleHdr 'schedule header' / 'lngP', 'latP', 'lngM', 'latM', 'cap', 'demand', 'quantities' /;

$onExternalOutput
Parameter
   schedule(i,j,scheduleHdr) 'shipment quantities in cases'
   total_cost                'total transportation costs in thousands of dollars';
Table schedule;
$offExternalOutput

total_cost = z.l;
schedule(active_plants(i),j, 'lngP')       = iLocData(i,'lng');
schedule(active_plants(i),j, 'latP')       = iLocData(i,'lat');
schedule(active_plants(i),j, 'lngM')       = jLocData(j,'lng');
schedule(active_plants(i),j, 'latM')       = jLocData(j,'lat');
schedule(active_plants(i),j, 'cap')        = a(i);
schedule(active_plants(i),j, 'demand')     = b(j);
schedule(active_plants(i),j, 'quantities') = x.l(i,j);
