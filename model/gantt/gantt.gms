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
   id       'gannt_id'      / 1, 2, 3, 4, 5, 6 /
   start    'gannt_start'   / 2016-01-04, 2016-01-05, 2016-01-06, 2016-01-07, 2016-01-08, 2016-01-09 /
   end      'gannt_end'     / 2016-01-05, 2016-01-06, 2016-01-07, 2016-01-08, 2016-01-09, 2016-01-10 /
   content  'gannt_content' / test1, test2, test3, test4, test5, test6 /
   group    'gantt_group'   / a, b /
   ;

Parameter
   a(i) 'capacity of plant i in cases'
        / seattle    350
          san-diego  600 /

   b(j) 'demand at market j in cases'
        / new-york   325
          chicago    300
          topeka     275 /
   gantt(id, start, end, content, group) 'UIOutput: asdasd' ;
*gantt(id, start, end, content, group)$(ord(id) = ord(start) and (ord(id) = ord(end)) and (ord(id) = ord(content))) = 1;
gantt('1', '2016-01-04', '2016-01-05', 'test1', 'a') = 1;
gantt('2', '2016-01-05', '2016-01-06', 'test2', 'a') = 1;
gantt('3', '2016-01-06', '2016-01-07', 'test3', 'a') = 1;
gantt('4', '2016-01-07', '2016-01-08', 'test4', 'b') = 1;
gantt('5', '2016-01-08', '2016-01-09', 'test5', 'b') = 1;
gantt('6', '2016-01-09', '2016-01-10', 'test6', 'b') = 1;

Table d(i,j) 'distance in thousands of miles'
              new-york  chicago  topeka
   seattle         2.5      1.7     1.8
   san-diego       2.5      1.8     1.4;

Scalar f 'freight in dollars per case per thousand miles' / 90 /;

Parameter c(i,j) 'transport cost in thousands of dollars per case';
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

display x.l, x.m;
