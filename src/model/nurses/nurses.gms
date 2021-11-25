$title A Nurse Scheduling Problem (NURSES,SEQ=428)

$onText
This nurse scheduling problem allocates nurses to shifts. The objective contains cost and fairness parts.
Moreover, the model demonstrates the use of a user specified decomposition for the ODHCplex solver as well
as the limited variable domain feature.

The model has been adapted from an example nurses.py provided by
IBM (https://ibmdecisionoptimization.github.io/docplex-doc/mp/nurses.html)


Keywords: scheduling, decomposition, limited variable domain
$offText

set nh         'NurseData header' / Seniority, Qualification, 'Pay rate' /
    sh         'ShiftData header' / 'Start time', 'End time', 'Minimum requirement', 'Maximum requirement' /
    nurse      'Nurses'
    shift      'Shifts'
    department 'Departments'
    skill      'Nurse skills'
    day        'Days of the week' / monday, tuesday, wednesday, thursday, friday, saturday, sunday /
;


$onExternalInput
Table nurseData(nurse<,nh) 'Nurse Data'
$onDelim
Name, Seniority, Qualification, 'Pay rate'
Anne, 11, 1, 25
Bethanie, 4, 5, 28
Betsy, 2, 2, 17
Cathy, 2, 2, 17
Cecilia, 9, 5, 38
Chris, 11, 4, 38
Cindy, 5, 2, 21
David, 1, 2, 15
Debbie, 7, 2, 24
Dee, 3, 3, 21
Edith, 8, 2, 25
Elaina, 8, 4, 31
Elaine, 8, 3, 28
Eleanor, 8, 3, 28
Elena, 8, 3, 28
Eliana, 8, 2, 27
Elianna, 8, 4, 31
Elisa, 8, 4, 32
Elise, 8, 4, 30
Elizabeth, 8, 5, 34
Ella, 8, 4, 31
Elle, 8, 4, 30
Elliana, 8, 5, 32
Ellie, 8, 4, 30
Eloise, 8, 2, 25
Elsie, 8, 5, 33
Ethel, 8, 3, 26
Ember, 8, 5, 30
Emelia, 8, 5, 30
Gloria, 8, 2, 25
Isabelle, 3, 1, 16
Jane, 3, 4, 23
Janelle, 4, 3, 22
Janice, 2, 2, 17
Jemma, 2, 4, 22
Joan, 5, 3, 24
Joanna, 5, 2, 18
Joyce, 8, 3, 29
Jude, 4, 3, 22
Julie, 6, 2, 22
Juliet, 7, 4, 31
Kate, 5, 3, 24
Mary, 9, 5, 36
Nancy, 8, 4, 32
Nathalie, 9, 5, 38
Nicole, 0, 2, 14
Patricia, 1, 1, 13
Pippa, 1, 1, 25
Patrick, 6, 1, 19
Roberta, 3, 5, 26
Suzanne, 5, 1, 18
Vickie, 7, 1, 20
Wendie, 5, 2, 21
Zoe, 8, 3, 29
$offDelim
;

Table shiftData(shift<,department<,day,sh) 'Shift Data'
$onDelim
Shift, Department, Day, 'Start time', 'End time', 'Minimum requirement', 'Maximum requirement'
s1, Emergency, monday, 2, 8, 3, 5
s2, Emergency, monday, 8, 12, 4, 7
s3, Emergency, monday, 12, 18, 2, 5
s4, Emergency, monday, 18, 2, 3, 7
s5, Consultation, monday, 8, 12, 10, 13
s6, Consultation, monday, 12, 18, 8, 12
s7, Cardiac_Care, monday, 8, 12, 10, 13
s8, Cardiac_Care, monday, 12, 18, 8, 12
s9, Geriatrics, monday, 8, 12, 8, 10
s10, Geriatrics, monday, 12, 18, 8, 15
s11, Emergency, tuesday, 8, 12, 4, 7
s12, Emergency, tuesday, 12, 18, 2, 5
s13, Emergency, tuesday, 18, 2, 3, 7
s14, Consultation, tuesday, 8, 12, 10, 13
s15, Consultation, tuesday, 12, 18, 8, 12
s16, Cardiac_Care, tuesday, 8, 12, 4, 7
s17, Cardiac_Care, tuesday, 12, 18, 2, 5
s18, Cardiac_Care, tuesday, 18, 2, 3, 7
s19, Geriatrics, tuesday, 8, 12, 8, 10
s20, Geriatrics, tuesday, 12, 18, 8, 15
s21, Emergency, wednesday, 2, 8, 3, 5
s22, Emergency, wednesday, 8, 12, 4, 7
s23, Emergency, wednesday, 12, 18, 2, 5
s24, Emergency, wednesday, 18, 2, 3, 7
s25, Consultation, wednesday, 8, 12, 10, 13
s26, Consultation, wednesday, 12, 18, 8, 12
s27, Geriatrics, wednesday, 8, 12, 8, 10
s28, Geriatrics, wednesday, 12, 18, 8, 15
s29, Emergency, thursday, 2, 8, 3, 5
s30, Emergency, thursday, 8, 12, 4, 7
s31, Emergency, thursday, 12, 18, 2, 5
s32, Emergency, thursday, 18, 2, 3, 7
s33, Consultation, thursday, 8, 12, 10, 13
s34, Consultation, thursday, 12, 18, 8, 12
s35, Geriatrics, thursday, 8, 12, 8, 10
s36, Geriatrics, thursday, 12, 18, 8, 15
s37, Emergency, friday, 2, 8, 3, 5
s38, Emergency, friday, 8, 12, 4, 7
s39, Emergency, friday, 12, 18, 2, 5
s40, Emergency, friday, 18, 2, 3, 7
s41, Consultation, friday, 8, 12, 10, 13
s42, Consultation, friday, 12, 18, 8, 12
s43, Geriatrics, friday, 8, 12, 8, 10
s44, Geriatrics, friday, 12, 18, 8, 15
s45, Emergency, saturday, 2, 12, 5, 7
s46, Emergency, saturday, 12, 20, 7, 9
s47, Emergency, saturday, 20, 2, 12, 12
s48, Geriatrics, saturday, 8, 12, 8, 10
s49, Geriatrics, saturday, 12, 18, 8, 15
s50, Emergency, sunday, 2, 12, 5, 7
s51, Emergency, sunday, 12, 20, 7, 9
s52, Emergency, sunday, 20, 2, 12, 12
s53, Geriatrics, sunday, 8, 12, 8, 10
s54, Geriatrics, sunday, 12, 18, 8, 15
$offDelim
;

Set nurseSkills(nurse,skill<) 'Nurse has particular skill' /
  Anne     .(Anaesthesiology, Oncology, Pediatrics)
  Betsy    .(Cardiac_Care)
  Cathy    .(Anaesthesiology)
  Cecilia  .(Anaesthesiology, Oncology, Pediatrics)
  Chris    .(Cardiac_Care, Oncology, Geriatrics)
  Gloria   .(Pediatrics)
  Jemma    .(Cardiac_Care)
  Joyce    .(Anaesthesiology, Pediatrics)
  Julie    .(Geriatrics)
  Juliet   .(Pediatrics)
  Kate     .(Pediatrics)
  Nancy    .(Cardiac_Care)
  Nathalie .(Anaesthesiology, Geriatrics)
  Patrick  .(Oncology)
  Suzanne  .(Pediatrics)
  Wendie   .(Geriatrics)
  Zoe      .(Cardiac_Care)
/;

Parameter SkillRequirements(department, skill) / Emergency.Cardiac_Care 1 /;

Set vacation(nurse,day) /
  Anne     .(friday, sunday)
  Cathy    .(thursday,tuesday)
  Joan     .(thursday,saturday)
  Juliet   .(monday,thursday)
  Nathalie .(sunday,thursday)
  Isabelle .(monday,thursday)
  Patricia .(saturday,wednesday)
  Nicole   .(friday,wednesday)
  Jude     .(tuesday,friday)
  Debbie   .(saturday,wednesday)
  Joyce    .(sunday,thursday)
  Chris    .(thursday,tuesday)
  Cecilia  .(friday,wednesday)
  Patrick  .(saturday,sunday)
  Cindy    .(sunday)
  Dee      .(tuesday,friday)
  Jemma    .(friday,wednesday)
  Bethanie .(wednesday,tuesday)
  Betsy    .(monday,thursday)
  David    .(monday)
  Gloria   .(monday)
  Jane     .(saturday,sunday)
  Janelle  .(wednesday,friday)
  Julie    .(sunday)
  Kate     .(tuesday,monday)
  Nancy    .(sunday)
  Roberta  .(friday,saturday)
  Janice   .(tuesday,friday)
  Suzanne  .(monday)
  Vickie   .(wednesday,friday)
  Wendie   .(thursday,saturday)
  Zoe      .(saturday,sunday)
/;

Set nurseAssoc(nurse,nurse) / Isabelle.Dee, Anne.Patrick /;

Set nurseIncompat(nurse,nurse) 'cannot work together' /
  Patricia.Patrick
  Janice.Wendie
  Suzanne.Betsy
  Janelle.Jane
  Gloria.David
  Dee.Jemma
  Bethanie.Dee
  Roberta.Zoe
  Nicole.Patricia
  Vickie.Dee
  Joan.Anne
/;

Scalar
  maxWorkTime      /  25 /
  fairnessWeight   / 100 /
  assignmentWeight /  10 /;

$offExternalInput

Set s(shift,department,day) 'Shift Department Day';
option s<shiftData; alias (s,t), (d,department), (nurse,n);
Set sPairs(shift,department,day,shift,department,day);
sPairs(s,t) = yes; sPairs(s,s) = no;

* Some error checks, more should be done for a prodcution version
Set error01(nurse,nurse) 'both associate and incompatible';
error01(n,nurse) = nurseAssoc(n,nurse) and nurseIncompat(n,nurse);
abort$card(error01) error01;

$macro duration(s) mod(shiftData(s,'End time')-shiftData(s,'Start time')+24,24)

* Continuous time parameters for start and end time to make overlapping shifts constraint (defOneShift) easy
Parameter startTime(shift,department,day), endTime(shift,department,day);
startTime(s(shift,d,day)) = shiftData(s,'Start time') + (ord(day)-1)*24;
endTime(s) = startTime(s) + duration(s);


Variable
$onExternalOutput
  nurseAssignments(nurse,shift,department,day)  'assign nurse to shift'
  nurseWorkTime(nurse)                          'working time in hours by nurse'
  nurseMoreThanAvgHours(nurse)                  'overtime'
  nurseLessThanAvgHours(nurse)                  'undertime'
$offExternalOutput
  costByDepartments(department)                 'cost by department'
  nurseAvgHours                                 'average working hours'
  fairness                                      'aggregation of all over- and undertime'
  totalAssignments                              'total number of shift assignments'
  obj                                           'objective variable'
;
Binary variable nurseAssignments;
Positive variable nurseMoreThanAvgHours, nurseLessThanAvgHours;

Equations
  defObj                                                       'composite objective to be minimized'
  defCostDep(department)                                       'cost by department'
  defShiftReqMin(shift,department,day)                         'a shift require between min and max Nurses '
  defShiftReqMax(shift,department,day)                         'a shift require between min and max Nurses '
  defNurseTime(nurse)                                          'time worked by a Nurse'
  defOneShift(nurse,shift,department,day,shift,department,day) 'two shifts at the same time are incompatible'
  defNurseIncompat(nurse,nurse,shift,department,day)           'Nurse-Nurse incompatibility'
  defNurseAssoc(nurse,nurse,shift,department,day)              'Nurse association'
  defSkillReq(department,skill,shift,department,day)           'Skill requirements'
  defAvgHours                                                  'compute average hours'
  defOverUnderTime(nurse)                                      'define under- and overtime of the average working hours per nurse'
  defFairness                                                  'aggration of all over- and undertime'
  defTotalAssign                                               'total number of assignments'
;

* composite objective to be minimized
defObj.. obj =e= sum(d, costByDepartments[d]) + fairnessWeight*fairness + assignmentWeight*totalAssignments;

* cost by department
defCostDep{d}.. costByDepartments[d] =e= sum{(n,s(shift,d,day)), nurseAssignments[n,s]*duration(s)*nurseData[n,'Pay rate']};

* a shift require between min and max Nurses
defShiftReqMin(s)..
  sum(n, nurseAssignments[n,s]) =g= shiftData(s,'Minimum requirement');

defShiftReqMax(s)..
  sum(n, nurseAssignments[n,s]) =l= shiftData(s,'Maximum requirement');

* time worked by a Nurse
defNurseTime(n)..
  nurseWorkTime[n] =e= sum(s, nurseAssignments[n,s]*duration(s));

* global max worked time
nurseWorkTime.up[n] = MaxWorkTime;

* two shifts at the same time are incompatible
defOneShift(n,sPairs(s,t))$(startTime(t) >= startTime(s) and startTime(t) < endTime(s))..
  nurseAssignments[n,s] + nurseAssignments[n,t] =l= 1;

* Nurse-Nurse incompatibility
defNurseIncompat(nurseIncompat(n,nurse),s)..
  nurseAssignments[n,s] + nurseAssignments[nurse,s] =l= 1;

* Nurse association
defNurseAssoc(nurseAssoc(n,nurse),s)..
  nurseAssignments[n,s] =e= nurseAssignments[nurse,s];

* Skill requirements
defSkillReq(d,skill,s(shift,d,day))$skillRequirements(d, skill)..
  sum(nurseSkills(n,skill), nurseAssignments[n,s]) =g= skillRequirements(d, skill);

* compute average hours
defAvgHours..
  card(nurse)*nurseAvgHours =e= sum(n, nurseWorkTime(n));

* fairness: want each nurse's allocated hours to be similar (there is an objetive penalty if not)
defOverUnderTime(n)..
  nurseWorkTime[n] =e= nurseAvgHours + nurseMoreThanAvgHours[n] - nurseLessThanAvgHours[n];

defFairness..
  fairness =e= sum(n, NurseMoreThanAvgHours[n] + NurseLessThanAvgHours[n]);

* total assignments
defTotalAssign..
  totalAssignments =e= sum((n,s), nurseAssignments[n,s]);

$if not set onduty $set ONDUTY 0
$ifThen %ONDUTY%==0
* Nurse vacations
nurseAssignments.fx[n,s(shift,d,day)]$vacation(n,day) = 0;

model nurseScheduling / all /;
$else
$onText
Rather than fixing assignment variables to 0 for vacation days one could exclude the assignment variables
from the model via a dynamic set that only containts the available shifts (onDuty) for a nurse. Instead
of using this set everywhere in the constraint definition one can also convenienyly use the limited variable
domain feature (https://www.gams.com/latest/docs/UG_ModelSolve.html#UG_ModelSolve_LimitedDomain).
$offText
set onDuty(nurse,shift,department,day);
onDuty(n,s) = yes; onDuty(n,s(shift,d,day))$vacation(n,day) = no;

model nurseScheduling / all, nurseAssignments(onDuty) /;
$endIf


$ifThenI "%gams.mip%"=="odhcplex"
* Try some custom decomposition scheme for ODHCplex
file fopt /odhcplex.opt/;
put fopt 'decomposition 2';
loop(n,     put / 'nurseAssignments.key("' n.tl:0 '",*,*,*) ' ord(n):0:0);
*loop(shift, put / 'nurseAssignments.key(*,"' shift.tl:0 '",*,*) ' ord(shift):0:0);
*loop(d,     put / 'nurseAssignments.key(*,*,"' d.tl:0 '",*) ' ord(d):0:0);
*loop(day,   put / 'nurseAssignments.key(*,*,*,"' day.tl:0 '") ' ord(day):0:0);
putclose fopt;

nurseScheduling.optFile = 1;
$endIf

* This makes the model harder
*shiftData(s,'Minimum requirement') = max(round(shiftData(s,'Minimum requirement')*.25),1);

nurseScheduling.resLim = 100;
solve nurseScheduling min obj us mip;

abort.noError$(nurseScheduling.modelStat<>%modelStat.optimal% and
               nurseScheduling.modelStat<>%modelStat.integerSolution%) 'no solution';

$onExternalOutput
Scalar
  nurseAvgHours_miro     'average working hours'
  fairness_miro          'aggregation of all over- and undertime'
  obj_miro               'objective variable'
  salaryCost             'Total salary cost'
  numberAssignments      'Total number of assignments'
  overAverageWork        'Total over-average worktime'
  underAverageWork       'Total under-average worktime'
;
Parameter
  costByDepartments_miro(department)
;
$offExternalOutput
$onDotL
costByDepartments_miro(department) = costByDepartments(department);

nurseAvgHours_miro    = nurseAvgHours;
fairness_miro         = fairness;
obj_miro              = obj;
salaryCost            = sum(d, CostByDepartments[d]);
numberAssignments     = TotalAssignments;
overAverageWork       = sum(n, NurseMoreThanAvgHours[n]);
underAverageWork      = sum(n, NurseLessThanAvgHours[n]);

file frep / report.lst /;
put frep 'Allocation By Department:';
loop(d,  put / '  ' d.tl:15 ':' (sum((n,s(shift,d,day)), NurseAssignments[n,s])):4:0);
put / 'Cost By Department:';
loop(d,  put / '  ' d.tl:15 ':' CostByDepartments.l(d):7:0);
put / 'Nurses Assignments:';
loop(n,
  put / '  ' n.tl:10 ': total hours:' nurseWorkTime.l(n):3:0;
  loop(s(shift,d,day)$(nurseAssignments[n,s]>0.5),
    put / '    ' day.tl:10 ':' d.tl:15 shiftData(s,'Start time'):2:0 '-' shiftData(s,'End time'):2:0;
  );
);

* Create a parameter containing shiftData and and nurseAssignment
