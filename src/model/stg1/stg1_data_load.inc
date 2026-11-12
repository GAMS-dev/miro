*-------------------- smod data load file --------------------------------
$ontext
 This file:
    a) controls the sets and data that are read into the model
    b) conditions a range of sets etc., that determine certain
       characterisitics of the model.

 To implement the actions controlled by this file the user needs to generate:
    1) An Excel workbook that contains the following worksheets
         'Layout'      - contains information used by GDXXRW
         'Sets'        - all model sets except those defined from data
         'Maps'        - provides links between different but related accounts
         'comelast'    - elasticities defined by reference to commodities
         'actelast'    - elasticities defined by reference to activities
         'hoelast'     - income elasticities of demand for households
         'frischelast' - frisch elasticities for LES
         'controls'    - parameters that control model structure and flow

         'exp_sets'    - sets used to control experiments
         'res_sets'    - sets used in analyses of results
         'closures'    - parameters that control various closure option

       Optional worksheets - if not in GDX format - include
         'sam'        - the SAM for the model
         'factuse'    - quantities of factors used by activities (optional)
         'popn'       - population (optional)

         An example is provided as smod1_data.xls

    2) An optional GDX file that contains data not provided in Excel format
         SAM          - the SAM for the model
         Factuse      - quantities of factors used by activities (optional)
         popn         - population (optional)

 If the SAM has more than 250 accounts it is often easier to read in the
 SAM directly from GDX.

         An example is provided as SAMeg.gdx
           (This is the same SAM as in smod1_data.xls)

 This file is organised as follows:

  Structure:
         1. DATA  ENTRY SECTION
         2. DATA ADJUSTMENTS SECTION
         3. DATA SCALING AND INITIAL CHECKS SECTION
         4. FINAL CHECK ON MODEL DATA
         5. OTHER SET AND PARAMETER ASSIGNMENTS

NOTE: When loading empty sets and parameters (including parameters with only
zeroes), it is important to initialise those variables explicitly.

*#### 1. DATA  ENTRY SECTION ########
    *---- 1a. Excel workbook sets and data converted to gdx here
          In the statement
          $CALL "GDXXRW i=[filename].xls o=data_in.gdx INDEX=LAYOUT!A4"
          the user replaces [filename] with the Excel filename
    *---- 1b. All data, elasticities and sets from excel are assigned here
          There should be no need to make changes here unless the model
          is being changed.
          * 1bi. Loading a SAM from Excel via GDX
          * 1bii. Loading a SAM and data from GDX (optional)
    *---- 1c. Defining SETS by exclusions from previously defined SETS
          There should be no need to make changes here unless the model
          is being changed.
    *---- 1d. Defining MAPPING SETS by using subset information
          There should be no need to make changes here unless the model
          is being changed.
    *---- 1d. Initial SAM check

*#### 2. DATA ADJUSTMENTS SECTION ########

    *---- 2a. Standard SAM adjustments
          Adjusting the SAM to avoid common data problems that do not affect
          model behaviours
    *---- 2b. Data-specific SAM adjustments
          Requires the inclusion of data specific code, and example is
          ($INCLUDE) smod5_adjust.inc

*#### 3. DATA SCALING AND INITIAL CHECKS SECTION ########

    *---- 3a. Automatic Scaling of SAM
          This instruction works by successively increasing the samscal
          parameter as multiples of 10 WHILE a given proportion
          - mod_cont("scalprop") - of the SAM entries are below a given
          target - mod_cont("scaltarg").
            mod_cont("scalprop") and mod_cont("scaltarg") are set in the
            Excel worksheet 'controls'.
              The suggested default values for mod_cont("scalprop") and
              mod_cont("scaltarg") are 0.95 and 100.

    *---- 3b. Entering and Scaling of FACTUSE
    *---- 3c. Population data
    *---- 3d. Recalculating Account TOTALS, including grand total

*#### 4. FINAL CHECK ON MODEL DATA ########

      Programme ABORTS if
      "Totals Check failed - Check SAM after adjustments" ;

*#### 5. OTHER SET ASSIGNMENTS ########

    *---- 5a. Assign set members for unskilled labour by exclusion
    *---- 5b. Defining sets to control production nesting structure
          This includes a control over the minimium share of inputs costs
          due to intermediate inputs for a top level CES function
    *---- 5c. Defining sets to control aggregation commodities
    *---- 5d. Defining elasticities for export demand
    * Elasticities for commodity output

*#### 6. OTHER ELASTICITY ASSIGNMENTS ########

    This section is used to override specific elasticities set in Excel
    *---- 6a. Defining elasticities for export demand
    *---- 6b. Elasticities for commodity output

$offtext

* ------- 1. Converting XLS File to GDX File -----------------------------
$ontext
  1) Convert the data to gdx format.
        The resultant GDX file should contain
        a) all sets for the model;
        b) all transactions data, i.e., SAM,
        c) all factor quantity data, i.e., FACTUSE,FACTHHOLD
        d) all elasticity data
        e) parameters to control model structure and flow
        e) parameters to control experiment and closure options
        f) sets used in the experiment files
        h) sets used to analyse and report results.

 The output filename - data_in.gdx - should NOT be changed since it is used
 in several places.

$offtext

*#### 1. DATA  ENTRY SECTION ########

*---- 1a. Excel workbook sets and data converted to gdx here

$ifThen.MIRORUN "x%gams.IDCGDXInput%"=="x"
$ CALL "GDXXRW i=stg1_data.xls o=data_in.gdx index=Layout!A4"
$endif.MIRORUN

*---- 1b. All data, elasticities and sets from excel are assigned here

$GDXIN data_in.gdx

$LOADdc sac sacnt
$LOADdc c cagr cnat cfd cind cuti ccon cser cagg
$LOADdc m
$LOADdc a aagr anat afd aind auti acon aser aagg anch anchN aleon
$LOADdc f l ls lm k n
$LOADdc h
$LOADdc g gt tff
$LOADdc e
$LOADdc i
$LOADdc w
$LOADdc ss
$LOADdc ppn

$LOADdc mcons fcons

$LOADdc map_f_tff map_tff_f
$GDXIN

* 1bi. Loading a SAM from Excel via GDX

$GDXIN data_in.gdx

* Transactions and factor data
$LOADdc sam factuse
$LOAD popn

* Elasticities data
$LOADdc elastc elastx elasty elastmu

* Control data
$LOADdc  flow_cont mod_cont

$GDXIN

* Initialise empty parameters

* (currently none)

* 1bii. Loading a SAM and data from GDX
* If the SAM is a gdx file it can be read in here - NOT a preferred option

$ontext
$GDXIN SAMeg.gdx

$LOAD sam factuse popn

$GDXIN
$offtext

*---- 1c. Defining SETS by exclusions from previously defined SETS

* SAM Accounts excluding totals
 sacn(sac)      = YES ;
 sacn("total")  = NO ;

* ASAM Accounts excluding totals

 ssn(ss)        = YES ;
 ssn("totals")  = NO ;

*---- 1d. Defining MAPPING SETS by using subset information

* Mapping from commodities to aggregate commodities

 map_cagg_c("Agriculture",c)$(cagr(c))  = YES ;
 map_cagg_c("Minerals",c)$(cnat(c))     = YES ;
 map_cagg_c("Food",c)$(cfd(c))          = YES ;
 map_cagg_c("Industry",c)$(cind(c))     = YES ;
 map_cagg_c("Utilities",c)$(cuti(c))    = YES ;
 map_cagg_c("Construction",c)$(ccon(c)) = YES ;
 map_cagg_c("Services",c)$(cser(c))     = YES ;

* Mapping from activies to aggregate activities
 map_aagg_a("Agriculture",a)$(aagr(a))  = YES ;
 map_aagg_a("Minerals",a)$(anat(a))     = YES ;
 map_aagg_a("Food",a)$(afd(a))          = YES ;
 map_aagg_a("Industry",a)$(aind(a))     = YES ;
 map_aagg_a("Utilities",a)$(auti(a))    = YES ;
 map_aagg_a("Construction",a)$(acon(a)) = YES ;
 map_aagg_a("Services",a)$(aser(a))     = YES ;

Display map_cagg_c, map_aagg_a ;


*---- 1d. Initial SAM check

* Perform an initial SAM totals check

PARAMETER
 TOTALSCHK         Check that row and column totals are equal
 ACCTOTCHK(sac)    Check on account totals
 ;

 TOTALSCHK    = SUM(sacn,(ABS(SUM(sacnp , SAM(sacn, sacnp))
                    - SUM(sacnp , SAM(sacnp, sacn))))) ;

 ACCTOTCHK(sacn) = (SUM(sacnp , SAM(sacn, sacnp))
                    - SUM(sacnp , SAM(sacnp, sacn))) ;

Display TOTALSCHK, ACCTOTCHK ;

ABORT $(TOTALSCHK GT 0.000005) "Totals Check failed for BASE SAM" ;

*#### 2. DATA ADJUSTMENTS SECTION ########

*---- 2a. Standard SAM adjustments
* Adjusting the SAM to avoid common problems that do not affect model

* Eliminating transactions between accounts and themselves

 SAM(sac,sac)        = 0 ;

* Net transactions between domestic institutions and ROW

 SAM(h,w)            = SAM(h,w) - SAM(w,h) ;
 SAM(w,h)            = 0 ;
 SAM(e,w)            = SAM(e,w) - SAM(w,e) ;
 SAM(w,e)            = 0 ;
 SAM(g,w)            = SAM(g,w) - SAM(w,g) ;
 SAM(w,g)            = 0 ;
 SAM("kap",w)        = SAM("kap",w) - SAM(w,"kap") ;
 SAM(w,"kap")        = 0 ;
 SAM("dstoc",w)      = SAM("dstoc",w) - SAM(w,"dstoc") ;
 SAM(w,"dstoc")      = 0 ;

* Net transfers between domestic non-govt institutions and govt

 SAM(h,"govt")       = SAM(h,"govt") - SAM("govt",h) ;
 SAM("govt",h)       = 0 ;
 SAM(e,"govt")       = SAM(e,"govt") - SAM("govt",e) ;
 SAM("govt",e)       = 0 ;

*---- 2b. Data-specific SAM adjustments

$INCLUDE stg1_adjust.inc

*#### 3. DATA SCALING AND INITIAL CHECKS SECTION ########

*---- 3a. Automatic Scaling of SAM

$ontext
AUTOMATIC CALCULATING OF A SCALING FACTOR - samscal

This instruction works by successively increasing the samscal parameter as
multiples of 10 WHILE a given proportion - mod_cont("scalprop") - of the
SAM entries are below a given target - mod_cont("scaltarg").

The conditional statement is computed as follows
Denominator - the total number of non zero cells in the SAM(sacn,sacnp)
Numerator - the total number of cells in SAM(sacn,sacnp) that are
            i)  non zero cells; AND
            ii) whose absolute values when divided by the current value of
                samscal are less that the target.

The ABORT statement protects against illegal values of scaltarg
$offtext

* Assigning values to initialise scaling parameter

 samscal            = mod_cont("samscal") ;

 ABORT $(mod_cont("scaltarg") LE 0.0)
 "samautoscaltarg is less than or equal to ZERO - set to positive value" ;

WHILE(
 ((SUM((sacn,sacnp)$(SAM(sacn,sacnp)
            AND (ABS(SAM(sacn,sacnp)/samscal)
              < mod_cont("scaltarg"))),1))
        /SUM((sacn,sacnp)$SAM(sacn,sacnp), 1)) < mod_cont("scalprop") ,

*     samscal is increased in multiples of 10 until the condition is satisfied
      samscal = samscal * 10;
  ) ;

Display samscal ;

* Scale the SAM data for algorithm performance
 SAM(sac,sacp)  = SAM(sac,sacp)/samscal ;

*---- 3b. Entering and Scaling of FACTUSE

* Remove factuse where there is no corresponding SAM payment
 factuse(f,a)$(not sam(f,a)) = 0 ;

* Declare factuse from SAM if neccessary

 factuse(f,a)$((NOT factuse(f,a)) AND sam(f,a)) =   sam(f,a) ;

* By default set factor use scaling factor equal to SAM scaling factor

 factscal       = samscal ;

 FACTUSE(f,a)   = FACTUSE(f,a)/factscal ;

*---- 3c. Population data
* Population data - select measure of population here

* pophaeq  - adult equivalents measure of population
* pophh  - simple total measure of population

If(mod_cont("setpop") =1,

 pop(h)       = popn(h,"pophh") ;

  ) ;

If(mod_cont("setpop") =2,

 pop(h)       = popn(h,"pophaeq") ;

  ) ;

* Scale population data in line with factor use data

 pop(h)         = pop(h)/factscal ;

*---- 3d. Recalculating Account TOTALS, including grand total

 SAM("TOTAL",sac)    = SUM(sacn, SAM(sacn,sac));
 SAM(sac,"TOTAL")    = SUM(sacn, SAM(sac,sacn));

*#### 4. FINAL CHECK ON MODEL DATA ########

* Recompute SAM account totals

 SAM(sacn,"TOTAL")      = 0.0 ;
 SAM("TOTAL",sacn)      = 0.0 ;

 SAM(sacn,"TOTAL")      = SUM(sacnp, SAM(sacn,sacnp)) ;
 SAM("TOTAL",sacnp)     = SUM(sacn, SAM(sacn,sacnp)) ;

*Used in MIRO
PARAMETER
 SAM_NT(sacnt,sacntp)    SAM without totals
 SAM_EXPSH(sacnt,sacntp) SAM expenditure shares
 SAM_INCSH(sacnt,sacntp) SAM income shares
 SAM_EXPSHCHK            SAM expenditure shares check sum to 1
 SAM_INCSHCHK            SAM income shares check sum to 1 ;


 SAM_NT(sacnt,sacntp) = SAM(sacnt,sacntp) ;

 SAM_EXPSH(sacnt,sacntp)$SAM("TOTAL",sacntp)
                        = SAM(sacnt,sacntp)/SAM("TOTAL",sacntp) ;
 SAM_INCSH(sacnt,sacntp)$SAM(sacnt,"TOTAL")
                        = SAM(sacnt,sacntp)/SAM(sacnt,"TOTAL") ;

* Perform a SAM totals check after adjustments

 TOTALSCHK    = SUM(sacn,(ABS(SUM(sacnp , SAM(sacn, sacnp))
                    - SUM(sacnp , SAM(sacnp, sacn))))) ;

 ACCTOTCHK(sacn) = (SUM(sacnp , SAM(sacn, sacnp))
                    - SUM(sacnp , SAM(sacnp, sacn))) ;

Display TOTALSCHK, ACCTOTCHK, SAM, FACTUSE, pop ;

ABORT $(TOTALSCHK GT 0.000005)
                  "Totals Check failed - Check SAM after adjustments" ;

*#### 5. OTHER SET ASSIGNMENTS ########

*---- 5a. Assign set members for unskilled labour by exclusion

 lu(l)          = (NOT ls(l)) AND (NOT lm(l));

*---- 5b. Defining sets to control production nesting structure
$ontext
  The set aqxn(a) - activities with Leontief at the top level is set as
     the complement to aqx(a,r)

Selecting aqx
 a) All activities WITH intermediate inputs where intermediates account
    for more than the value share set in Excel worksheet 'controls'
    are assigned to aqx.
 b) Selected members of a in aqx are cancelled - use the set aleon.
 c) Other members of a in aqx are cancelled manually.

Setting aqxn
    Defined as the complement to aqx.

$offtext

* Top level CES chosen by reference to intermediate input shares

 aqx(a)$((SUM(c,SAM(c,a))/SAM("total",a)) GT mod_cont("minaqxsh") )
                  = YES ;

* Top level CES canceled for selected activities

 aqx(aleon)       = NO ;

* Top level CES canceled for selected activities manually

* aqx("amines")   = NO ;

* Top level Leontief set for all activities without top level CES

 aqxn(a)         = not aqx(a) ;

*---- 5c. Defining sets to control aggregation commodities

* Linear aggregation of homogenous commodities

* cxac(c)         = YES ;
* cxac(cagr)      = NO ;

* cxacn(c)        = not cxac(c) ;

*#### 6. OTHER ELASTICITY ASSIGNMENTS ########

$ontext
NB: If the values for elasticities of substitution and/or transformation
    are set here they OVERRIDE the values set in the Excel database.
$offtext

*---- 6a. Defining elasticities for export demand

*Syntax example
* elastc("cmines","exdem")    = 0.5 ;



*---- 6b. Elasticities for commodity output

*Syntax example
* elastc("cmines","sigmaxc")    = 0.5 ;


*-------------------- end of smod data load file -------------------------
