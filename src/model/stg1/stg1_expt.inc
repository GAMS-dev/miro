* ======= Experiment file ================================================
$ontext
 This file sets up and runs experiments on a model. It has a standard structure
 which is described directly below, but makes use (in sections 1 and 7) of two
 include files where users can define and implement their experiments.

 Structure (indentation indicates code structure)
   1.  Declare and Assign Simulation Sets
       1a. Load sets from GDX data file
   2.  Declare and Assign experiment parameters
       2a. Declare experiment parameters
       2b. Assign experiment parameters
   3.  Declare Result parameters
   4.  Experiment Closure loop begins
       4a. Declaring Parameters for Closure
   5.  Implementing Closure Changes
   6.  sim loop begins
   7.  Implementing sim shocks
   8.  Solve statement
   9.  Storing results
       9a. Writing out results parameters
       9b. building a micro SAM
       9c. Testing for row and column total differences
   10. Terminating sim and expclos loops
   11. Analyses of Results

The experiment code consists of a number of files; the relationships between
these files is illustrated below.

 stg1.gms                                master programme file
     |
    .
     .
      .
       .
     |
     | 16. POLICY EXPERIMENTS
     |
     |<----- stg1_expt_**.inc          Experiment files
                 |
                 |<----- stg1_resparm.inc     Declare results params
                 |
                 |<----- stg1_varinit2.inc    Reinitialise variables
                 |<----- stg1_reset.inc       Reset closure variables
                 |
                 |<----- stg1_cl***1.inc        Alternative closures
                 |<----- stg1_cl***..inc                 .
                 |<----- stg1_cl***..inc                 .
                 |<----- stg1_cl***n.inc        Alternative closures
                 |
                 |
                 |<----- stg1_resassign.inc   Assigning results params
                 |
                 |
      |
      |<----- stg1_anal.inc         Result Analyses sub routine
      |          |
                 |
                 |<----- stg1_reslevel.inc    Levels results saved
                 |
                 |<----- stg1_resmacro.inc    Macroeconomic results
                 |
                 |<----- stg1_reswelf.inc     Welfare results
                 |
                 |<----- stg1_resstruct.inc   Structural results
                 |
                 |<----- stg1_resindex.inc    Price index results
                 |
                 |<----- stg1_resperc.inc     Proportionate results

$offtext

* ------- 1. Declare and Assign Simulation Set ---------------------------

* By including a "base case" in the SIM set a comparator is produced

SETS
* Sets for simulations
 sim          simulations
 sim1(sim)    simulations actually done

* Sets for closures
 closelem     Sets for experiment closures
 clos         Set of sets of closure rules for each experiment

 elst         elasticities

* Sets for results
 adjres       adjuster results
 scalres      scalar results

  ;

*---- 1a. Load sets from GDX data file

$GDXIN data_in.gdx

* Sets to control simulations
$LOAD sim sim1

* Sets to control closure rule selection
$LOAD clos closelem

* Sets for results
$LOAD adjres scalres

$GDXIN

DISPLAY sim, sim1, clos, closelem, adjres, scalres ;

* ------- 2. Declare and Assign experiment parameters -------------------

*---- 2a. Declare experiment parameters

Parameter
 tmsim(c,sim)    tariff rate shock
  ;

* --- 2b. Assign experiment parameters

 tmsim(c,"base") = 0.00 * tm0(c) ;
 tmsim(c,"sim01") = -0.5 * tm0(c) ;

Display tmsim ;


* ------- 3. Declare Result parameters ----------------------------------

* The SETGLOBAL dollar control option set below must not be changed
* IF the percent change calcutations are to to be correct
* The matching $DROPGLOBAL appears in the file stg1_anal.inc

$SETGLOBAL  res  res

$INCLUDE stg1_resparm.inc

* ------- 4. Expclos loop  -----------------------------------------

* ------- 4a. Declaring Parameters for Closure ----------------------------

* Load closures from xls
$ontext
$GDXIN data_in.gdx
$LOAD closelem expclos closure
$GDXIN
$offtext

*LOOP(clos,

* ------- 5. Implementing Closure Changes --------------------------------

*$INCLUDE Closures.inc

* ------- 6. sim loop begins ---------------------------------------------

LOOP(sim1,

* ------- 7. Shocks implemented ------------------------------------------

* ... SHOCKS HERE ...

 dabtm(c)  = tmsim(c,sim1) ;


* ------- 8. Solve statement ---------------------------------------------

 OPTION DECIMALS = 6 ;
 option MCP           = PATH ;

 Models1.HOLDFIXED   = 1 ;
 Models1.TOLINFREP   = 0.0000001 ;

 Solve Models1 Using MCP ;

* ------- 9. Storing results ---------------------------------------------

$INCLUDE stg1_resassign.inc

* ------- 10. Terminating sim and expclos loops ---------------------------

* The next line terminates the closure loop
* ) ;

* The next line terminates the experiment loop
 ) ;

* ============================================ END OF EXPERIMENT CODE FILE
