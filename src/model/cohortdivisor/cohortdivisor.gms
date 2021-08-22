* Partition students into cohorts so that inter-cohort connections are minimized
* via group concept. More details can be found in the README.md

Sets
  s           'students'
  g           'groups'
  c           'cohorts'            / A, B /
  mode        'splitting mode'     / split, 'no split', 'split even', 'prevent split' /
  pmode(mode) 'mode with priority' / 'split even', 'prevent split' /
  smode(mode) 'mode with max size' / 'split' /
  ghdr        'group data header'  / maxGroupSize, priority /;
Alias (*,gc);

$onExternalInput
Table groupData(g<,mode,ghdr)
                                        maxGroupSize  priority

6Et.                   'split even'                          1
6a.                    'split'                     5
6b.                    'split'                     5
6c.                    'split'                     5
6eR.                   'split even'                          1
6kR.                   'split even'                          1
muellers.              'prevent split'                     100
paul_and_friends.      'prevent split'                       5
schoolbus_springfield. 'prevent split'                      99
;

Set
  gsmap(g,s<) /
   6Et.                  (Gabi,Lasse,Sandra,Tina,Udo)
   6a.                   (Anna,Bernd,Charlotte,Dieter,Erwin,Frauke,Gabi)
   6b.                   (Hans,Ina,Jana,Karl,Lasse,Manuel)
   6c.                   (Nina,Otto,Paul,Quentin,Rita,Sandra,Tina,Udo)
   6eR.                  (Anna,Bernd,Charlotte,Hans,Nina,Otto,Paul,Quentin,Rita)
   6kR.                  (Dieter,Erwin,Frauke,Ina,Jana,Karl,Manuel)
   muellers.             (Karl,Lasse)
   paul_and_friends.     (Paul,Otto,Udo)
   schoolbus_springfield.(Frauke,Gabi,Jana,Quentin)
  /;

Set
  existingCohortGroupMap(s,g,c);
$ifThen "x%gams.idcGDXInput%" == "x"
  Set existingCohortGroupMap(s,g,c) / /;
$else
$ gdxIn "%gams.idcGDXInput%"
* Filtered load to exclude the cohorts A and B as groups
$ load existingCohortGroupMap
$endIf

Scalar
   priorityAB          'priority to keep the input assignment' / 0 /;
$offExternalInput

Set gmodemap(g,mode); option gmodemap<groupData;
Set existingCohortMap(s,c); option existingCohortMap<existingCohortGroupMap;

Parameter priority(g), maxGroupSize(g);

priority(g) $= sum(gmodemap(g, mode), groupData(gmodemap, 'priority'));
maxGroupSize(g) $= sum(gmodemap(g, mode), groupData(gmodemap,'maxGroupSize'));

* Reclassify 'prevent split' groups with priority 100 to 'no split' groups
gmodemap(g,'no split')$(gmodemap(g,'prevent split') and priority(g) = 100) = yes;
gmodemap(g,'prevent split')$(gmodemap(g,'prevent split') and priority(g) = 100) = no;

priority(g) = max(1,min(99,priority(g)));

Set error01(g) 'group with more than one mode';
error01(g) = sum(gmodemap(g,mode),1)<>1;
abort$card(error01) 'Groups with more than one mode', error01;

Set gp(g) 'groups with priority'
    gs(g) 'groups with maxGroupSize'
;
gp(g) = sum(gmodemap(g,pmode),1);
gs(g)= sum(gmodemap(g,smode),1);

$macro studentsInGroup(g) sum(gsmap(g,s),1)

Set error02(g) 'group to large for maxGroupSize';
error02(gs) = maxGroupSize(gs)*2 < studentsInGroup(gs);
abort$card(error02) 'Groups too large for maxGroupSize', error02;

Binary variable
  sAssign(s)      'Cohort assignment of student 0: Cohort A, 1: Cohort B'
  allInA(g)       'Indicator that all students of group are in cohort A, (or B if 0)'
  popBiggerInA(g) 'Indicator that more students assigned to cohort A, (or B if 0)'
Positive variable
  gSurplus(g)     'Deviation of perfect split in half of group that should be split evenly'
  gRemain(g)      'Number of students in the smaller group where splitting should be prevented'
Variable
  obj             'Objective variable'
;

Equation
  eqMaxGroupSizeA(g) 'Maximum size of group in cohort A'
  eqMaxGroupSizeB(g) 'Maximum size of group in cohort B'
  eqEvenSplitA(g)    'Definition of deviation of perfect split of group in cohort A'
  eqEvenSplitB(g)    'Definition of deviation of perfect split of group in cohort B'
  eqLargerGroupA(g)  'Definition of larger group indicator in cohort A'
  eqLargerGroupB(g)  'Definition of larger group indicator in cohort B'
  eqRemainingA(g)    'Definition of remaining smaller group size in cohort A'
  eqRemainingB(g)    'Definition of remaining smaller group size in cohort B'
  eqNoSplit(g)       'Guarantee that "no split" groups are not split'
  eqObj              'Objective function'
;
$macro studentsInA(g) sum(gsmap(g,s),sAssign(s))
$macro studentsInB(g) sum(gsmap(g,s),1-sAssign(s))

eqMaxGroupSizeA(gs).. studentsInA(gs) =l= maxGroupSize(gs);
eqMaxGroupSizeB(gs).. studentsInB(gs) =l= maxGroupSize(gs);

eqEvenSplitA(g)$gmodemap(g,'split even').. gSurplus(g) =g= floor(studentsInGroup(g)/2) - studentsInA(g);
eqEvenSplitB(g)$gmodemap(g,'split even').. gSurplus(g) =g= floor(studentsInGroup(g)/2) - studentsInB(g);

eqLargerGroupA(g)$gmodemap(g,'prevent split').. studentsInA(g) =g= (studentsInGroup(g)/2)*popBiggerInA(g);
eqLargerGroupB(g)$gmodemap(g,'prevent split').. studentsInB(g) =g= (studentsInGroup(g)/2)*(1-popBiggerInA(g));

eqRemainingA(g)$gmodemap(g,'prevent split').. gRemain(g) =g= studentsInA(g) - studentsInGroup(g)*popBiggerInA(g);
eqRemainingB(g)$gmodemap(g,'prevent split').. gRemain(g) =g= studentsInB(g) - studentsInGroup(g)*(1-popBiggerInA(g));

eqNoSplit(g)$gmodemap(g,'no split').. studentsInA(g) =e= allInA(g)*studentsInGroup(g);

eqObj.. obj =e=   sum(gmodemap(g,'split even'), gSurplus(g)*priority(g))
                + sum(gmodemap(g,'prevent split'), gRemain(g)*priority(g))
                + (sum(existingCohortMap(s,'A'), 1-sAssign(s)) + sum(existingCohortMap(s,'B'), sAssign(s)))*priorityAB;

option mip=cbc;
model cohortAB /all/;
solve cohortAB min obj using mip;

set gRepHdr / 'size A', 'size B', maxSize, 'surplus penalty', 'remain penalty' /;
set cRepHdr / 'size', moved, 'move penalty' /;

Set Table studentAssignment(s,c);
$onExternalOutput
Set studentGroupCohort(s,gc,c) 'Student assignment';
Parameter
   groupReport(g,*)  'report on groups'
   cohortReport(c,*) 'report on cohort'
;
$offExternalOutput

* If we don't get a solution change 'no split' to 'prevent split' with a 99 priority
if (cohortAB.modelstat <> %modelStat.Optimal% and cohortAB.modelstat <> %modelStat.Integer Solution%,
  gmodemap(g,'prevent split')$gmodemap(g,'no split') = yes;
  priority(g)$gmodemap(g,'no split') = 99;
  groupReport(g,'reclassified')$gmodemap(g,'no split') = 1;
  gmodemap(g,'no split') = no;
  solve cohortAB min obj using mip;
);
abort$(cohortAB.modelstat <> %modelStat.Optimal% and cohortAB.modelstat <> %modelStat.Integer Solution%) 'No solution';

$onDotL
studentAssignment(s,'A')  = sAssign(s) > 0.5;
studentAssignment(s,'B')  = sAssign(s) < 0.5;
studentGroupCohort(s,g,c) = sum(gsmap(g,s), studentAssignment(s,c));
studentGroupCohort(s,c,c) = studentAssignment(s,c);

groupReport(g,'size A')                                     = studentsInA(g);
groupReport(g,'size B')                                     = studentsInB(g);
groupReport(gs,'maxSize')                                   = maxGroupSize(gs);
groupReport(g,'totalSize')                                  = studentsInA(g) + studentsInB(g);
groupReport(g,'surplus penalty')$gmodemap(g,'split even')   = gSurplus(g)*priority(g);
groupReport(g,'remain penalty')$gmodemap(g,'prevent split') = gRemain(g)*priority(g);

cohortReport(c,'size')         = sum(studentAssignment(s,c),1);
cohortReport('A','moved')      = sum(s$existingCohortMap(s,'A'), 1-sAssign(s));
cohortReport('B','moved')      = sum(s$existingCohortMap(s,'B'),   sAssign(s));
cohortReport(c,'move penalty') = cohortReport(c,'moved')*priorityAB;
