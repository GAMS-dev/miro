$title "Calculate Shift Factor matrix for DC-OPF"
*_______________________________________________________________________________
* Filename: calc_S_matrix.gms
* Description: Calc Shift matrix for dcopf_shift.gms. Also called PTDF matrix
*
* Usage: gams calc_S_matrix.gms --case=/casepath/case.gdx
* Output: /casepath/case_Shift_Matrix.gdx
*_______________________________________________________________________________

*===== SECTION: OPTIONS & ENVIRONMENT VARIABLES
$setenv GdxCompress 1
$if not set limits $set limits "given"
$if not set timeperiod $set timeperiod "1"
$setnames "%case%" casepath casename caseextension

option solprint=off, limcol=0, limrow=0;

*===== DATA DECLARATION
sets
    monitored_lines(*,*,*) 
    bus(*)
    line(*,*,*)
    circuit(*)

    t /'given', 1*24 /
    bus_t /'type'/
    branch_t /'branchstatus', 'r', 'x', 'ratio', 'angle'/
;

alias(bus,i,j);
alias(circuit,c);

parameters
    baseMVA 
    type(bus)         "bus type (irrelevant in dcopf, but gives reference bus)"
    r(i,j,c)          "line resistance"
    x(i,j,c)          "line reactance"
    S(i,j,c,bus)      "Shift Factor matrix, lines by buses"
    ratio(i,j,c)      "transformer tap ratio"
    angle(i,j,c)      "transformer tap angle"
    branchstatus(i,j,c) "Lines, matches row elements of Shift matrix"

    businfo(bus,bus_t,t),
    branchinfo(i,j,c,branch_t,t)
;

*===== EXTRACT DATA
$GDXIN "%case%"
$LOAD monitored_lines, bus, line, circuit
$LOAD baseMVA
$LOAD businfo, branchinfo
$GDXIN

type(bus) = businfo(bus,'type','given');

branchstatus(i,j,c)$line(i,j,c) = branchinfo(i,j,c,'branchstatus','%timeperiod%');
r(i,j,c)$branchstatus(i,j,c) = branchinfo(i,j,c,'r','given');
x(i,j,c)$branchstatus(i,j,c) = branchinfo(i,j,c,'x','given');
ratio(i,j,c)$branchstatus(i,j,c) = branchinfo(i,j,c,'ratio','given');
angle(i,j,c)$branchstatus(i,j,c) = branchinfo(i,j,c,'angle','given') * pi/180;

*===== DATA MANIPULATION
* Data symmetry
x(j,i,c)$branchstatus(i,j,c) = x(i,j,c);
ratio(j,i,c)$branchstatus(i,j,c) = ratio(i,j,c);
angle(j,i,c)$angle(i,j,c) = -angle(i,j,c);

sets isLine(i,j);
option isLine < branchstatus;

*===== DATA COMPUTATION
parameters
    b_temp(i,j,c) "Computation of b entry"
    B(bus,bus) "B matrix, n by n"
    Z(bus,bus) "Inverse matrix, Z=inv(B) without ref buses"
;
set
    nonref(bus) "non-reference buses"
;

*-- Compute b entry
* Regular computation
b_temp(i,j,c)$(branchstatus(i,j,c) or branchstatus(j,i,c)) =  1/(ratio(i,j,c)*x(i,j,c));

* There exists edge ijc and jic in the dataset. This format is not recommended, but may happen
b_temp(i,j,c)$(branchstatus(j,i,c) and (branchstatus(i,j,c))  ) =  1/(ratio(i,j,c)*x(i,j,c))+  1/(ratio(j,i,c)*x(j,i,c));
b_temp(j,i,c)$(branchstatus(j,i,c) and (branchstatus(i,j,c))  ) =  1/(ratio(i,j,c)*x(i,j,c))+  1/(ratio(j,i,c)*x(j,i,c));

* The swing bus is the reference bus (There should only be one swing bus)
nonref(bus)$(type(bus) ne 3) = yes;

* Fill B matrix
B(i,j)$(nonref(i) and nonref(j) and not sameas(i,j)) = sum(c$(branchstatus(i,j,c) or branchstatus(j,i,c)), b_temp(i,j,c));
B(i,i)$nonref(i) = sum((j,c)$(branchstatus(i,j,c) or branchstatus(j,i,c)), -b_temp(i,j,c))

*===== VARIABLES AND CONSTRAINTS
parameters
    e(bus) "RHS value which is part of ident matrix, either 0 or 1" / #bus 0 /;
;
free variables
    zj(bus) "Inverse matrix elements, solved row by row"
    obj "Dummy objective"
;

* not consider slack bus
equations
    c_invert(i) "Matrix inversion by solving for each column, Bx=e_i"
;

* Matrix inversion by a solve for each column (Bx=e_i)
c_invert(i)$nonref(i)..
    sum(j$(nonref(j) and (isLine(i,j) or isLine(j,i) or sameas(i,j))  ), B(i,j) * zj(j)) =e= e(i)
;

*===== CREATE MODEL AND SOLVE
model invert /c_invert/;
option solprint=off, limcol=0, limrow=0;

* Loop over buses to solve inverse problem. Looping is useful if data is too large
parameter identE(bus,bus);
identE(nonref,nonref) = 1;

Parameter
   o / SkipBaseCase 1, SolveEmpty 1 /;
Set dict / nonref.scenario.''
           o.opt.''
           e.param.   identE
           zj.level.  Z /;

solve invert using cns scenario dict;

*===== Compute Shift matrix based on Z and b
S(i,j,c,bus)$branchstatus(i,j,c) = -b_temp(i,j,c) * (Z(i,bus) - Z(j,bus));
S(i,j,c,bus)$(branchstatus(i,j,c) and (type(bus) eq 3)) = 0;

* There exists edge ijc and jic in the dataset. This format is not recommended, but may happen
S(i,j,c,bus)$(branchstatus(i,j,c) and branchstatus (j,i,c)) = -1/(ratio(i,j,c)*x(i,j,c)) * (Z(i,bus) - Z(j,bus));

*===== CREATE GDX FILE
execute_unload '%casepath%%casename%_Shift_Matrix%caseextension%', S, branchstatus;
