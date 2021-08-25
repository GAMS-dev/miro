$onText
This is based on tsp5 from the GAMS model library.

The shortest round trip is calculated for a set of cities. The distance
between these cities is calculated as crow flight distance based on the
latitude and longitude of the cities specified in parameter iiLocData.


Keywords: mixed integer linear programming, traveling salesman problem,
          iterative subtour elimination
$offText

File log / tspMiro.log /; put log;
put '------------------------------------------------------'/;
put '                   Options selected'/;
put '------------------------------------------------------'/;


Set locHdr   Location data header' / lat, lng /
    ii       Cities used in the model
    i(ii)    Subset of cities;

Alias (ii,jj),(i,j,k),(u,*);

$onExternalInput
Table     iiLocData(ii<,locHdr) City location information
                        lat       lng
'Montgomery, AL'      32.3670  -86.3006
'Juneau, AK'          58.3019 -134.4197
'Phoenix, AZ'         33.4484 -112.0741
'Little Rock, AR'     34.7465  -92.2896
'Denver, CO'          39.7348 -104.9653
'Hartford, CT'        41.7646  -72.6909
'Dover, DE'           39.1582  -75.5244
'Tallahassee, FL'     30.4381  -84.2809
'Atlanta, GA'         33.7491  -84.3902
'Honolulu, HI'        21.3045 -157.8557
'Boise, ID'           43.6166 -116.2009
'Springfield, IL'     39.7990  -89.6440
'Indianapolis, IN'    39.7683  -86.1584
'Des Moines, IA'      41.5911  -93.6037
'Sacramento, CA'      38.5816 -121.4944
'Topeka, KS'          39.0490  -95.6776
'Frankfort, KY'       38.2009  -84.8733
'Baton Rouge, LA'     30.4460  -91.1874
'Augusta, ME'         44.3106  -69.7797
'Annapolis, MD'       38.9786  -76.4928
'Boston, MA'          42.3603  -71.0583
'Lansing, MI'         42.7338  -84.5554
'Saint Paul, MN'      44.9504  -93.1015
'Jackson, MS'         32.2990  -90.1848
'Jefferson City, MO'  38.5774  -92.1724
'Helena, MT'          46.5927 -112.0363
'Lincoln, NE'         40.8089  -96.7078
'Carson City, NV'     39.1649 -119.7666
'Concord, NH'         43.2072  -71.5375
'Trenton, NJ'         40.2171  -74.7429
'Santa Fe, NM'        35.6870 -105.9378
'Albany, NY'          42.6512  -73.7550
'Raleigh, NC'         35.7804  -78.6391
'Bismarck, ND'        46.8083 -100.7837
'Columbus, OH'        39.9623  -83.0007
'Oklahoma City, OK'   35.4730  -97.5171
'Salem, OR'           44.9392 -123.0331
'Harrisburg, PA'      40.2663  -76.8861
'Providence, RI'      41.8240  -71.4128
'Columbia, SC'        34.0007  -81.0343
'Pierre, SD'          44.3683 -100.3512
'Nashville, TN'       36.1622  -86.7744
'Austin, TX'          30.2711  -97.7437
'Salt Lake City, UT'  40.7596 -111.8868
'Montpelier, VT'      44.2604  -72.5757
'Richmond, VA'        37.5385  -77.4343
'Olympia, WA'         47.0451 -122.8950
'Charleston, WV'      38.3506  -81.6333
'Madison, WI'         43.0748  -89.3838
'Cheyenne, WY'        41.1400 -104.8202
;
$offExternalInput

Parameter c(ii,jj)              Cost coefficients (distance in 1000 of miles);

*Calculating "crow - flight - distances" in miles
Parameter llr(ii,locHdr) Latiutude and longitude of cities (radians)
          helpCD(ii,ii)  Help parameter for crow-distance calcualtion;
Scalar    equRad         Earth radius at equator in miles /3963.190/;

llr(ii,locHdr) = iiLocData(ii,locHdr)*pi/180;

helpCD(ii,jj)$(not sameas(ii,jj)) =   sin(llr(ii,'lat'))*sin(llr(jj,'lat'))
                                    + cos(llr(ii,'lat'))*cos(llr(jj,'lat'))
                                      * cos(llr(ii,'lng')-llr(jj,'lng'));

c(ii,jj)$(not sameas(ii,jj)) = [pi/2 - arctan(helpcd(ii,jj)
                                /sqrt(1-helpCD(ii,jj)*helpCD(ii,jj)))]*equRad/1000;

put '- The following cities were selected:'/;
loop(ii,
   put '  - ' ii.tl:0 /;
);

*
* for computational work with simple minded
* algorithm we can restrict size of problem
* and define the model over a subset of all cities.
*
*

variables x(ii,jj)  decision variables - leg of trip
          z         objective variable;
binary variable x;

equations objective   total cost
          rowsum(ii)  leave each city only once
          colsum(jj)  arrive at each city only once;
*
* the assignment problem is a relaxation of the TSP
*
objective.. z =e= sum((i,j), c(i,j)*x(i,j));

rowsum(i).. sum(j, x(i,j)) =e= 1;
colsum(j).. sum(i, x(i,j)) =e= 1;

* exclude diagonal
*
x.fx(ii,ii) = 0;

Set i(ii);

$onExternalInput
Scalar
   maxCities Limit numbers of cities in tour         /  100 /
   maxCuts   Limit number of cuts                    / 1000 /;
;
$offExternalInput

put '- Maximum number of cities in tour          : ' maxCities:0:0 /;
put$(card(ii)>maxCities) '  Attention: Maximum number of cities in tour is smaller than selected cities (' card(ii):0:0 ')!'  /;
put$(card(ii)>maxCities)  '             Increase the value of "maxCities", if all selected cities should be visited.'  /;
put '- Maximum number of subtour elimiantion cuts: ' maxCuts:0:0 /;
put '------------------------------------------------------'/;

if(card(ii)<2,
   put   'Need to have at least two cities to calculate a tour'/;
   abort 'Need to have at least two cities to calculate a tour';
);
if(maxCities<2,
   put   'MaxCities needs to be at least two to calculate a tour'/;
   abort 'MaxCities needs to be at least two to calculate a tour';
);
if(maxCuts<1,
   put   'MaxCuts needs to be at least one to run the problem'/;
   abort 'MaxCuts needs to be at least one to run the problem';
);

i(ii)$(ord(ii) <= maxCities) = yes;

* Dynamic subtour elimination
Set
   ste         'possible subtour elimination cuts' / c1*c100000 /
   a(ste)      'active cuts'
   tour(ii,jj) 'possible subtour'
   n(jj)       'nodes visited by subtour';

Parameter
   proceed       'indicator to continue to eliminate subtours' / 1 /
   cc(ste,ii,jj) 'cut coefficients'
   rhs(ste)      'right hand side of cut';

Equation defste(ste) 'subtour elimination cut';

defste(a).. sum((i,j), cc(a,i,j)*x(i,j)) =l= rhs(a);

Model DSE / all /;

a(ste)    = no;
cc(a,i,j) =  0;
rhs(a)    =  0;

option optCr = 0, resLim = 30;
option limRow = 0, limCol = 0, solPrint = silent, solveLink = 5;

loop(ste$(proceed and ord(ste)<maxCuts),
   if(proceed = 1,
      solve DSE min z using mip;
      put$(DSE.modelStat <> %modelStat.optimal%)   'Problems with MIP solver - No optimal solution found'/;
      abort$(DSE.modelStat <> %modelStat.optimal%) 'problems with MIP solver';
      x.l(i,j) = round(x.l(i,j));
      proceed = 2;
   );

*  Check for subtours
   tour(i,j) = no;
   n(j)      = no;
   loop((i,j)$(card(n) = 0 and x.l(i,j)), n(i) = yes);

*  Found all subtours, resolve with new cuts
   if(card(n) = 0,
      proceed = 1;
   else
*  Construct a single subtour and remove it by setting x.l=0 for its edges
      while(sum((n,j), x.l(n,j)),
         loop((i,j)$(n(i) and x.l(i,j)),
            tour(i,j) = yes;
            x.l(i,j)  =   0;
            n(j)      = yes;
         );
      );
      if(card(n) < card(j),
         a(ste)   = 1;
         rhs(ste) = card(n) - 1;
         cc(ste,i,j)$(n(i) and n(j)) = 1;
      else
         proceed = 0;
      );
   );
);

Set tourHdr 'Tour header' / 'lngA', 'latA', 'lngB', 'latB', 'Flow' /;

$onExternalOutput
Scalar
   total_cost 'Total Distance (miles)';
Table  tourDetails(ii,jj,tourHdr);
$offExternalOutput

total_cost = z.l * 1000;

if(proceed = 0,
   put     'Optimal tour found - Tour length: ' total_cost:0:2 ' miles'/;
   put     '------------------------------------------------------'/;
   display 'Optimal tour found', tour;
else
   put   'Out of subtour cuts, increase value of "maxCuts" to find better solution.'/;
   put   '------------------------------------------------------'/;
   abort 'Out of subtour cuts, increase maxCuts';
);

loop(tour(ii,jj),
   tourDetails(ii,jj, 'lngA') = iilocData(ii,'lng');
   tourDetails(ii,jj, 'latA') = iilocData(ii,'lat');
   tourDetails(ii,jj, 'lngB') = iilocData(jj,'lng');
   tourDetails(ii,jj, 'latB') = iilocData(jj,'lat');
   tourDetails(ii,jj, 'Flow') = 1;
);
