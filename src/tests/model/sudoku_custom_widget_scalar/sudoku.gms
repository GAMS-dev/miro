set col 'col' /col1*col9/
    row 'row' /row1*row9/
    val 'val' /1*9/
    quad /quad1*quad9/
    rcquad_map(quad,col,row) /
    quad1.(col1*col3).(row1*row3)
    quad2.(col4*col6).(row1*row3)
    quad3.(col7*col9).(row1*row3)
    quad4.(col1*col3).(row4*row6)
    quad5.(col4*col6).(row4*row6)
    quad6.(col7*col9).(row4*row6)
    quad7.(col1*col3).(row7*row9)
    quad8.(col4*col6).(row7*row9)
    quad9.(col7*col9).(row7*row9)
/;
$onExternalInput
Set
   i 'canning plants' / seattle,  san-diego /
   j 'markets'        / new-york, chicago, topeka /;
Singleton Set test(i) /san-diego 'San-Diego'/;
Set ii(i) 'canning plant subset' /seattle/;
Parameter
   a(i) 'capacity of plant i in cases'
        / seattle    350
          san-diego  600 /

   b(j) 'demand at market j in cases'
        / new-york   325
          chicago    300
          topeka     275 /;

Table d(i,j) 'distance in thousands of miles'
              new-york  chicago  topeka
   seattle         2.5      1.7     1.8
   san-diego       2.5      1.8     1.4;
scalar force_unique_sol /1/
       test123 /2.3/
       test124 /3.3/;
table initial_state(row,col)
     col1 col2 col3 col4 col5 col6 col7 col8 col9
row1                     8    6
row2      7         9         2
row3 6    9                        2         8
row4 8                   9         7         2
row5 4                                       3
row6 2         9         1                   4
row7 5         3                        7    6
row8                5         8         2
row9                3    7
;
table initial_state2(row,col)
     col1 col2 col3 col4 col5 col6 col7 col8 col9
row1                     1    6
row2      7         9         2
row3 6    9                        2         8
row4 8                   9         7         2
row5 4                                       3
row6 2         9         1                   4
row7 5         3                        1    6
row8                5         8         2
row9                3    1
;
$offExternalInput

parameter initial_state_clean(row,col);

initial_state_clean(row,col) = initial_state(row,col);
initial_state_clean(row,col)$mapVal(initial_state_clean(row,col)) = 0;

set error01(row,col);
error01(row,col) = initial_state_clean(row,col) < 0 or initial_state_clean(row,col) > 9 or mod(initial_state_clean(row,col),1) <> 0;

file log / miro.log /;
put log '------------------------------------'/;
put log '        Data validation'/;
put log '------------------------------------'/;
if(test123 < 0,
    put log 'initial_state:: test123 must be positive!'/;
    abort "Data errors detected."
);
if(card(error01),
  put log 'initial_state_clean:: Digits must be integers between 0 and 9!'/;
  loop(error01(row,col),
      put log / ' Cell "' row.tl:4 ':' col.tl:4 '" has invalid value of ' initial_state_clean(row,col):0;
    );
  abort "Data errors detected."
);
put log 'Data ok'/;
putclose log;

File fcpx / cplex.opt /;

binary variable x(col,row,val);
variable z;

equations eq_z, eq_col(col,val),eq_row(row,val),eq_quad(quad,val),eq_val(col,row);

eq_col(col,val)..
   sum(row,x(col,row,val)) =e= 1;

eq_row(row,val)..
   sum(col,x(col,row,val)) =e= 1;

eq_quad(quad,val)..
   sum(rcquad_map(quad,col,row),x(col,row,val)) =e= 1;

eq_val(col,row)..
   sum(val,x(col,row,val)) =e= 1;

eq_z..
   1 =e= z;

model sudoku /all/;

x.fx(col,row,val)$(initial_state_clean(row,col) = ord(val)) = 1;
sudoku.optFile   = 1;
putClose fcpx 'solnpool solnpool.gdx' / 'solnpoolintensity 4' / 'solnpoolpop 2';

solve sudoku min z using mip;
if(sudoku.modelstat <> 1 and sudoku.modelstat <> 8,
display sudoku.modelstat;
  putclose log 'No solution exists for your input data.'/;
  abort 'Infeasible.';
);

Set
   soln           'possible solutions in the solution pool' / file1*file1000 /
   solnpool(soln) 'actual solutions';

execute_load 'solnpool.gdx', solnpool=index;

if(force_unique_sol and card(solnpool) > 1,
  putclose log 'The solution to the input data you provided is not unique!'/;
  abort "Solution is not unique!");

$onExternalOutput
table results(row,col);
$offExternalOutput
results(row,col) = sum(val$x.l(col,row,val), ord(val)) * (1-2$(initial_state_clean(row,col)>0.5));
