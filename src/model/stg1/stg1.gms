$TITLE STAGE Static CGE Model V1
$OFFUPPER ONEMPTY

$ontext
################################# NOTE #################################
STAGE_1 is an earlier version of the STAGE family of CGE models.
It is used here as a compact and accessible example model for learning
and demonstration purposes within GAMS MIRO.

More recent versions of STAGE are available and may provide additional
features and enhancements. For current documentation and information on
the STAGE model family, please see: https://www.cgemod.org.uk/stage.html
$offtext
*$OFFLISTING OFFSYMXREF OFFSYMLIST

$ontext
On Dollar Control Directives see Chapter 10 GAMS manual
$offtext

*################ GENERAL INFORMATION ####################################
$ontext

This programme was developed by Scott McDonald

Contact details:
     cgemod,
     14 Moncrieffe Road,
     Sheffield, UK
       email: jrs.mcdonald@gmail.com, scott@cgemod.org.uk

Copyright (c) 2006 Scott McDonald.
First written: January 2006
Last revised:  December 2006

If you are using or have a copy of this programme without the author's
agreement you are in breach of the copyright. This programme and all
related files should be deleted from your PC, etc., immediately and
no copy should be kept.

Do NOT remove this notice even if you are this programme with agreement.

If you use this programme you should acknowledge the source and not claim
authorship. Altered versions should be clearly distinguished from the original.

$offtext

*##################### MODEL DEVELOPMENT NOTES ###########################
$ontext


$offtext

*################ WORK TO DO ON THIS VERSION #############################
$ontext

$offtext
*##################### 1. MODEL DESCRIPTION ##############################
$ontext

    1. Model structure derives from
       Dervis et al (1984) 'General Equilibrium Models....'
    2. SAM approach derives from
       Pyatt (1988) 'SAM Approach to Modelling'
    3. Programme organisation - data followed by model
       see Brooke et al (1992) 'GAMS: A Users Guide' Chapter 3
    4. Original code structure derives from
       USDA Model by Robinson et al (1990) AND Devarajan et al (1994)
       Getting the Model Right. Chapter 3 'Cameroon Model'.
    5. The code is closely related to the GLOBE model by McDonald,
       Thierfelder and Robinson (2005) 'A SAM Based Global CGE Model
       using GTAP Data', Sheffield Economics Research Paper 2005:001.
    5. The development of this model has been influenced by many other
       modellers and collaborators; a failure to acknowledge their input
       etc., is not intended to deny the value of their inputs.

Acknowledgemets: contributions by the following are gratefully acknowledged
                    Karen Thierfelder,
                    Cecilia Punt,
                    Lindsay Chant,
                    Melt van Schoor,
                    Kalie Pauw,
           Special thanks are due to Sherman Robinson for providing
           code and advice when this model started, in 1992, and since.

* ------ SCHEMATIC OUTLINE OF PROGRAMME

The model code consists of a number of files; the relationships between
these files is illustrated below.

 stg1.gms                                master programme file
     |
     |<----- stg1_descrip.inc            model data & behaviour description
     |<----- stg1_convent.inc            conventions used in model code
     |
     | 2. Model Origins
     |
     | 3. PARAMETER AND VARIABLE LISTINGS
     |
     |<----- stg1_list.inc              alphabetic listing
     |
     | 4. SET AND DATA PARAMETER DECLARATION
     |
     |<----- stg1_data_load.inc         data include file
     |<----- samgadjC_F_R.inc           SAM adjustment file
     |
     | 5. SET AND DATA ASSIGNMENT
     |
     | 6. DATA DIAGNOSTICS
     |
     |<----- stg1_diagnost.inc          data diagnostics file
     |
     | 7. PARAMETER CALIBRATION
     |
     |<----- stg1_parmcalib.inc         Parameter declaration
                                                 and calibration file
     |
     | 8. VARIABLE DECLARATION
     |
     | 9. VARIABLE INITIALISATION
     |
     |<----- stg1_varinit.inc           Variable initialisation file
     |
     | 10. SOCIAL ACCOUNTING MATRIX
     |
     |<----- stg1_samchk1.inc           MacroSAM check on calibration
     |
     |10b. Structural Tables
     |
     |<----- stg1_struct.inc            Descriptive structural tables
     |
     | 11. EQUATION DECLARATION
     |
     | 12. EQUATIONS
     |
     | 13. MODEL CLOSURE
     |
     |<----- stg1_bclose.inc           Base model closure
     |
     | 14. MODEL & SOLVE STATEMENTS
     |
     | 15. SOCIAL ACCOUNTING MATRIX
     |
     |<----- stg1_samchk2.inc          Macro SAM check on model
     |
     | 16. POLICY EXPERIMENTS
     |
     |<----- stg1_expt.inc             Experiment files
                 |
                 |<----- stg1_1_resparm.inc     Declare results params
                 |
                 |<----- stg1_1_varinit2.inc    Reinitialise variables
                 |<----- stg1_1_reset.inc       Reset closure variables
                 |
                 |<----- stg1_cl***1.inc        Alternative closures
                 |<----- stg1_cl***..inc                 .
                 |<----- stg1_cl***..inc                 .
                 |<----- stg1_cl***n.inc        Alternative closures
                 |
                 |
                 |<----- stg1_1_resassign.inc   Assigning results params
                 |
                 |
      |
      |<----- stg1_anal.inc            Result Analyses sub routine
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

file log / miro.log /;

$if not dexist results $call mkdir  results

* ------ DESCRIPTION OF BASIC MODEL

$INCLUDE stg1_descrip.inc

* Coding and notation conventions

$INCLUDE stg1_convent.inc

*############### 3. PARAMETER AND VARIABLE LISTINGS ######################

$INCLUDE stg1_list.inc

*############### 4. SET AND DATA PARAMETER DECLARATION ###################
$ontext
 The data are entered in 6 components
  i)     SAM accounts set SAC
  ii)    various subsets of SAC
  iii)   various mapping sets for the model and results
  iv)    various sets assigned from the data
  v)     sets to control model structure and flow
  vi)    sets for reporting results
$offtext

SETS

 sac        SAM accounts
 sacnt(sac) SAM accounts without total

 c(sac)     Commodities
 cagr(c)    Agricultural Commodities
 cnat(c)    Natural Resource Commodities
 cfd(c)     Food Commodities
 cind(c)    Industrial Commodities
 cuti(c)    Utility Commodities
 ccon(c)    Construction Commodities
 cser(c)    Service Commodities
 cagg       Aggregate commodity groups

 m(sac)     Margins

 a(sac)     Activities
 aagr(a)    Agricultural Activities
 anat(a)    Natural Resource Activities
 afd(a)     Food Activities
 aind(a)    Industrial Activities
 auti(a)    Utility Activities
 acon(a)    Construction Activities
 aser(a)    Service Activities
 aagg       Aggregate activity groups

 anch(a)    Anchor activity for fixing 1 WFDIST in various factor closures
 anchN(a)   Anchor activity for fixing 1 WFDIST in land factor closures

 aleon(a)   Activities with Leontief prodn function at Level 1

 f(sac)     Factors
 l(f)       Labour Factors
 ls(l)      Skilled Labour Factors
 lm(l)      Skilled or Unskilled Labour Factors
 lu(l)      Unskilled Labour Factors
 k(f)       Capital Factors
 n(f)       Land factors

 h(sac)     Households
 g(sac)     Government
 gt(g)      Government tax accounts
 tff(g)     factor tax account used in GDX program

 e(sac)     Enterprises
 i(sac)     Investment

 w(sac)     Rest of the world

 ppn        population

* Mapping sets

 map_f_tff(f,tff) Factor taxes to factors
 map_tff_f(tff,f) Factor taxes to factors reverse

$OnExternalOutput
 map_aagg_a(aagg,a) Mapping from activies to aggregate activities
 map_cagg_c(cagg,c) Mapping from commodities to aggregate commodities
$OffExternalOutput

* Declaring SETS that will be assigned from the data
 ce(c)      export commodities
 cen(c)     Non-export commodities

 ced(c)     export commodities with export demand functions
 cedn(c)    export commodities without export demand functions

 cm(c)      imported commodities
 cmn(c)     non-imported commodities

 cx(c)      commodities produced domestically
 cxn(c)     commodities NOT produced domestically AND imported

 cxac(c)    commodities that are differentiated by activity
 cxacn(c)   commodities that are NOT differentiated by activity

 cd(c)      commodities produced and demanded domestically
 cdn(c)     commodities NOT produced and demanded domestically

 aqx(a)     Activities with CES aggregation function at Level 1 of nest
 aqxn(a)    Activities with Leontief aggregation function at Level 1 of nest

* Declaring other sets

 sacn(sac)  SAM accounts excluding TOTAL

 ss         ASAM categories
 ssn(ss)    ASAM excluding totals

* Set used to control model flow

 fcons      set for parameters controlling program flow
 mcons      set for parameters controlling model content

    ;

* Setting up ALIASES

 ALIAS(sac,sacp), (sacn,sacnp),(sacnt,sacntp),
      (c,cp), (cagr,cagrp), (cnat,cnatp), (cfd,cfdp), (cind,cindp),
      (cuti,cutip), (ccon,cconp), (cser,cserp),
      (m,mp),
      (a,ap), (aagr,aagrp), (anat,anatp), (afd,afdp), (aind,aindp),
      (auti,autip), (acon,aconp), (aser,aserp),
      (f,fp), (l,lp), (ls,lsp), (lm,lmp), (lu,lup), (k,kp), (n,np),
      (h,hp), (e,ep), (g,gp), (gt,gtp), (tff,tffp), (i,ip), (w,wp),
      (ce,cep), (ced,cedp), (cm,cmp), (cx,cxp), (cxac,cxacp), (cd,cdp),
      (ss,ssp), (ssn,ssnp) ;


*#### DECLARING PARAMETERS FOR ALL MODEL DATA

* ------- Declare parameters for data loading ----------------------------
$ontext
 The data are entered in 8 components
  i)     SAM matrix
  ii)    FACTUSE (quantities) matrix
  iii)   Factor ownership by households
  iv)    Population data - population measure needs selecting
  v)     4 tables of elasticities

 In addtion there are
  vi)   2 (scalar) scaling parameters
  vii)  2 tables of parameters to control model flow and model structure
  viii) a table of parameters defining closures to be used for experiments
$offtext

PARAMETER
* Data parameters
 SAM(SAC,SACP)     the SAM for this model
 FACTUSE(f,a)      Factor use by activity

 facthhold(h,f)    Factor use supplied by household
 popn(h,ppn)       Population data using various measures
 pop(h)            Population data used in model

 ELASTC(c,*)       Trade Elasticities indexed on commodities
 ELASTX(a,*)       Production Elasticities indexed on activities
 ELASTY(c,h)       Income Demand Elasticities for households
 ELASTMU(h,*)      Elasticity of the MU of income

* Data scaling parameters

 samscal           algorithm performance scaling parameter for SAM
 factscal          algorithm performance scaling parameter for FACTUSE

* Parameters used to control model flow

 flow_cont(fcons)   values for parameters controlling program flow
 mod_cont(mcons)    values for parameters controlling model content

     ;

*############### 5. DATA ENTRY ###########################################

* ------- Data load include file -----------------------------------------
$ontext
All data and sets are read in using an include file

The output filename - data_in.gdx - should NOT be changed since it is used
in several places.

Adjustments to the data are also made in this file.
$offtext

$INCLUDE stg1_data_load.inc

*################ 6. ADDITIONAL SET ASSIGNMENT ###########################

* Define sets on the basis of read in data

 ce(c)$(SAM(c,"ROW"))          = YES ;
 cen(c)                        = NOT ce(c) ;

 ELASTC(c,"exdem")$(NOT ELASTC(c,"exdem")) =   0.0 ;

 ced(c)$(ELASTC(c,"exdem"))    = YES ;
 cedn(c)                       = NOT ced(c) ;

 cm(c)$(SAM("ROW",c))          = YES ;
 cmn(c)                        = not cm(c);

 cx(c)$(SUM(a,SAM(a,c)))       = YES ;
 cxn(c)                        = not cx(c) ;

 ELASTC(c,"sigmaxc")$(NOT ELASTC(c,"sigmaxc")) =   0.0 ;

 cxac(c)$ELASTC(c,"sigmaxc")   = YES ;
 cxacn(c)                      = not cxac(c) ;

 cd(c)$( SUM(a,SAM(a,c)) GT (SAM(c,"ROW") - SAM("EXPTAX",c)))
                               = YES ;
 cdn(c)                        = not cd(c) ;


DISPLAY ce, cen, ced, cedn, cm, cmn, cx, cxn, cxac, cxacn, cd, cdn ;


* ------- Displaying loaded data and sets -------------------------------

 DISPLAY SAM, FACTUSE, pop,
         ELASTC, ELASTX, ELASTY, ELASTMU,
         mod_cont, flow_cont,
         samscal, factscal
   ;

 DISPLAY sac,
         c, cagr, cnat, cfd, cind, cuti, ccon, cser, cagg,
         m,
         a, aagr, anat, afd, aind, auti, acon, aser, aagg, anch, anchN,
         f, l, ls, lm, lu, k, n,
         h, g, gt, tff, e, i, w,
* other sets
         sacn, sacnt, ss, ssn,
* sets for flow control
         mcons, fcons,
* mapping sets
         map_f_tff, map_tff_f,  map_aagg_a, map_cagg_c
  ;

*######################## END OF DATA ENTRY ##############################

*################ 7. DATA DIAGNOSTICS ####################################

$INCLUDE stg1_diagnost.inc

*################ 8. MODEL CALIBRATION ###################################
* Calibration proceeds by the blocks of equations
* each block contains all the related parameter declarations and assignments

$INCLUDE stg1_parmcalib.inc

*################ 9. VARIABLE DECLARATION ################################
* NOTE the default specification of variables as FREE

 Free Variables

* ------- TRADE BLOCK ----------------------------------------------------

* #### Exchange Rate Block

 ER           Exchange rate                         (domestic per world unit)

* #### Exports Block

 PWE(c)       World price of exports in dollars
 PE(c)        Domestic price of exports by activity a
 PD(c)        Consumer price for domestic supply of commodity c

 QE(c)        Domestic output exported by commodity c
 QD(c)        Domestic demand for commodity c

* #### Imports Block

 PWM(c)       World price of imports in dollars
 PM(c)        Domestic price of competitive imports of commodity c

 QM(c)        Imports of commodity c
 QQ(c)        Supply of composite commodity c

* ------- COMMODITY PRICE BLOCK ------------------------------------------

 PQS(c)       Supply price of composite commodity c
 PQD(c)       Purchaser price of composite commodity c

* ------- NUMERAIRE PRICE BLOCK ------------------------------------------

 CPI          Consumer price index
 PPI          Producer (domestic) price index

* ------- PRODUCTION BLOCK -----------------------------------------------

 PX(a)        Composite price of output by activity a

* CES aggregation functions for Level 1 of production nest

 PVA(a)       Value added price for activity a
 PINT(a)      Price of aggregate intermediate input

 ADX(a)       Shift parameter for CES production functions for QX
 ADXADJ       Scaling Factor for Shift parameter on CES functions for QX
 DADX         Partial scaling factor for Shift parameter on CES functions for QX

 ADFD(f,a)    Shift parameter for factor and activity specific efficiency

 QX(a)        Domestic production by activity a
 QVA(a)       Quantity of aggregate value added for level 1 production

* Leontief aggregation functions for Level 1 of production nest

 QINT(a)      Aggregate quantity of intermediates used by activity a

* CES aggregation functions for Level 2 of production nest

 ADVA(a)      Shift parameter for CES production functions for QVA
 ADVAADJ      Scaling Factor for Shift parameter on CES functions for QVA
 DADVA        Partial scaling factor for Shift parameter on CES functions for QVA

 WF(f)        Price of factor f
 WFDIST(f,a)  Sectoral proportion for factor prices
 FD(f,a)      Demand for factor f by activity a
 FS(f)        Supply of factor f

* Intermediate Input Demand

 QINTD(c)     Demand for intermediate inputs by commodity

* Commodity Output

 PXC(c)       Producer price of composite domestic output

 QXC(c)       Domestic production by commodity c

* Activity Output

 PXAC(a,c)    Activity commodity prices

 QXAC(a,c)    Domestic commodity output by each activity

* ------- FACTOR BLOCK ---------------------------------------------------

 YF(f)        Income to factor f
 YFDISP(f)    Factor income for distribution after depreciation

* ------- HOUSEHOLD BLOCK ------------------------------------------------
* ## Household Income

 YH(h)        Income to household h

* Household Expenditure

 HOHO(h,hp)   Inter household transfer
 HEXP(h)      Household consumption expenditure
 QCD(c,h)     Household consumption by commodity c

* ------- ENTERPRISE BLOCK -----------------------------------------------
* ## Enterprise Income

 YE(e)        Enterprise incomes

* ## Enterprise Expenditure

 QEDADJ       Enterprise demand volume Scaling Factor
 HEADJ        Scaling factor for enterprise transfers to households

 HOENT(h,e)   Household Income from enterprise e
 GOVENT(e)    Government income from enterprise e
 QED(c,e)     Enterprise consumption by commodity c
 VED(e)       Value of enterprise e consumption expenditure

* ------- GOVERNMENT BLOCK -----------------------------------------------

* #### Government Income Block

 TE(c)        Export taxes on exported comm'y c
 TM(c)        Tariff rates on imported comm'y c
 TS(c)        Sales tax rate
 TEX(c)       Excise tax rate
 TX(a)        Indirect tax rate
 TF(f,a)      Tax rate on factor use
 TYF(f)       Direct tax rate on factor income
 TYH(h)       Direct tax rate on households
 TYE(e)       Direct tax rate on enterprises

* ## Government Tax Revenues

 MTAX         Tariff revenue
 ETAX         Export tax revenue
 DTAX         Direct Income tax revenue
 FYTAX        Factor Income tax revenue
 ITAX         Indirect tax revenue
 STAX         Sales tax revenue
 FTAX         Factor use tax revenue
 EXTAX        Excise tax revenue

* ## Tax Rate Adjustment Factors

 TEADJ        Export subsidy Scaling Factor
 TMADJ        Tarrif rate Scaling Factor
 TSADJ        Sales tax rate scaling factor
 TEXADJ       Excise tax rate scaling factor
 TXADJ        Indirect Tax Scaling Factor
 TFADJ        Factor Use Tax Scaling Factor
 TYFADJ       Factor Tax Scaling Factor
 TYHADJ       Household Income Tax Scaling Factor
 TYADJ        Household and Enterprise Income Tax Scaling Factor
 TYEADJ       Enterprise income tax Scaling Factor

 DTE          Partial Export tax rate scaling factor
 DTM          Partial Tariff rate scaling factor
 DTS          Partial Sales tax rate scaling factor
 DTEX         Partial Excise tax rate scaling factor
 DTX          Partial Indirect tax rate scaling factor
 DTF          Uniform adjustment to factor use tax by activity
 DTYF         Partial direct tax on factor rate scaling factor
 DTYH         Partial direct tax on household rate scaling factor
 DTY          Partial direct tax on h'hold and enterprise rate scaling factor
 DTYE         Partial direct tax on enterprise rate scaling factor

* ## Government Income

 YG           Government income

* #### Government Expenditure Block

 QGDADJ       Government consumption demand scaling factor
 HGADJ        Scaling factor for government transfers to households
 EGADJ        Transfers to enterprises by government Scaling Factor

 QGD(c)       Government consumption demand by commodity c
 VGD          Value of Government consumption expenditure
 EG           Expenditure by government

* ------- KAPITAL BLOCK --------------------------------------------------

* ## Savings Block

 SHH(h)       Household savings rates
 SEN(e)       Enterprise savings rates

 SADJ         Savings rate scaling factor for BOTH households and enterprises
 SHADJ        Savings rate scaling factor for households
 SEADJ        Savings rate scaling factor for enterprises

 DSHH         Partial household savings rate scaling factor
 DS           Partial household and enterprise savings rate scaling factor
 DSEN         Partial enterprise savings rate scaling factor

 TOTSAV       Total savings

* ## Investment Block

 IADJ         Investment scaling factor

 QINVD(c)     Investment demand by commodity c
 INVEST       Total investment expenditure

* ------- FOREIGN INSTITUTIONS BLOCK -------------------------------------

 YFWOR(f)     Foreign factor income

* ------- MARKET CLEARING BLOCK ------------------------------------------

* ##### Account Closure

 KAPGOV       Government Savings
 CAPWOR       Current account balance

* #### Absorption Closure

 VFDOMD       Value of final domestic demand
 INVESTSH     Value share of investment in total final domestic demand
 VGDSH        Value share of Govt consumption in total final domestic demand
 VEDSH(e)     Value share of Ent consumption in total final domestic demand

* ##### GDP

 GDPVA        GDP from Value Added

* ##### Slack

 WALRAS       Slack variable for Walras's Law

        ;

* If the model is correctly specified WALRAS equals zero - a useful check
* See Devarajan et al (1994) Getting the Model Right


*################ 10. VARIABLE INITIALISATION ############################
$ontext
   This section initialises the variables to their base values.
$offtext

$INCLUDE stg1_varinit.inc


*################ 11. SOCIAL ACCOUNTING MATRICES #########################
$ontext
This section checks that the model is calibrated with a balanced SAM,
by using a Macro and a Micro SAM.

The MICRO SAM element can be switched off by setting "micsam1" = 0 in the
parameter 'flow_cont' in the worksheet 'controls'

 1) Macro SAM
  The programme aborts IF
           - ROW AND COLUMN SUMS FOR ASAM1 NOT EQUAL
           - ALL ASAM1 CELL ENTRIES DO NOT EQUAL ASAM0 ENTRIES

 2) Micro SAM
  The programme aborts IF
           - ALL SAM1 CELL ENTRIES DO NOT EQUAL SAM0 ENTRIES
$offtext

$INCLUDE stg1_samchk1.inc

*################ 13. STRUCTURAL DESCRIPTION #############################
$ontext
   This section produces various summary macroeconoic statistics and a
series of summary descriptive statistics.
$offtext

$INCLUDE stg1_struct.inc

*################ END OF CALIBRATION ######################################

EQUATIONS

*################ 14. EQUATION DECLARATIONS ###############################

* ------- TRADE BLOCK ----------------------------------------------------

* #### Exports Block

 PEDEF(c)        Domestic price of exports by commodity c

 CET(c)          CET function for domestic production
 ESUPPLY(c)      Export supply function (FOC)
 EDEMAND(c)      Export demand function
 CETALT(c)       CET fn for dom prodn with no exports OR only exported

* #### Imports Block

 PMDEF(c)        Domestic price of competitive imports of commodity c

 ARMINGTON(c)    Composite commodity aggregation function
 COSTMIN(c)      Cost minimisation for composite commodity (FOC)
 ARMALT(c)       Comp commody aggn fn with no imports OR no dom prodn

* ------- COMMODITY PRICE BLOCK ------------------------------------------

 PQSDEF(c)       Supply price of composite commodity c
 PQDDEF(c)       Purchaser price of composite commodity c
 PXCDEF(c)       Producer price for composite domestic output

* ------- NUMERAIRE PRICE BLOCK ------------------------------------------

 CPIDEF          Consumer price index
 PPIDEF          Producer (domestic) price index

* ------- PRODUCTION BLOCK -----------------------------------------------

 PXDEF(a)        Composite price of output by activity a

* CES aggregation functions for Level 1 of production nest

 PVADEF(a)       Value added price for activity a level 1 of nest
 PINTDEF(a)      Aggregate intermed input price for activity a level 1 of nest

 ADXEQ(a)        Shift parameter for CES production functions for QX
 QXPRODFN(a)     Production function for QX in activity a level 1 of nest
 QXFOC(a)        FOC for production function for QX level 1 of nest

* Leontief aggregation functions for Level 1 of production nest

 QINTDEF(a)      Leontief intermediate aggregation for Level 1 of nest
 QVADEF(a)       Leontief value added aggregation for Level 1 of nest

* CES aggregation functions for Level 2 of production nest

 ADVAEQ(a)       Shift parameter for CES production functions for QVA
 QVAPRODFN(a)    Production function for QVA in activity a level 2 of nest
 QVAFOC(f,a)     FOC Production function for QVA level 2 of nest

* Intermediate Input Demand

 QINTDEQ(c)      Intermediate input demand by commodity

* Commodity Output

 COMOUT(c)       Domestic differentiated commodity production
 COMOUTFOC(a,c)  FOC for Domestic differentiated commodity production

 COMOUT2(c)      Domestic homogenous commodity production
 COMOUTFOC2(a,c) FOC for Domestic homogenous commodity production

* Activity Output

 ACTIVOUT(a,c)    Domestic activity output

* ------- FACTOR BLOCK ---------------------------------------------------

 YFEQ(f)         Factor incomes
 YFDISPEQ(f)     Factor income for distribution after depreciation

* ------- HOUSEHOLD BLOCK ------------------------------------------------
* ## Household Income

 YHEQ(h)         Household incomes

* Household Expenditure

 HOHOEQ(h,hp)    Inter household transfer
 HEXPEQ(h)       Household consumption expenditure
 QCDEQ(c,h)      Household commodity consumption

* ------- ENTERPRISE BLOCK -----------------------------------------------
* ## Enterprise Income

 YEEQ(e)         Enterprise incomes

* ## Enterprise Expenditure

 QEDEQ(c,e)      Enterprise commodity consumption
 HOENTEQ(h,e)    Household Income from enterprise e
 VEDEQ(e)        Value of enterprise consumption expenditure
 GOVENTEQ(e)     Government income from enterprise e

* ------- GOVERNMENT BLOCK -----------------------------------------------

* #### Government Income Block

* ## Government Tax Rates

 TEDEF(c)        Export tax rates on exports of comm'y c
 TMDEF(c)        Tariff rates on imports of comm'y c
 TSDEF(c)        Sales tax rates
 TEXDEF(c)       Excise tax rates
 TXDEF(a)        Indirect tax rates
 TFDEF(f,a)      Factor use tax rates paid by activities
 TYFDEF(f)       Factor income tax rates
 TYHDEF(h)       Household income tax rates
 TYEDEF(e)       Enterprise income tax rates

* ## Government Tax Revenues

 MTAXEQ          Import tariff taxes
 ETAXEQ          Export taxes
 STAXEQ          Sales taxes
 EXTAXEQ         Excise taxes
 ITAXEQ          Indirect taxes on activities
 FTAXEQ          Factor use tax revenue
 FYTAXEQ         Factor income taxes
 DTAXEQ          Direct taxes on households and enterprises

* ## Government Income

 YGEQ            Government income

* #### Government Expenditure Block

 QGDEQ(c)        Government commodity consumption
 EGEQ            Government expenditure
 VGDEQ           Value of Government consumption expenditure

* ------- KAPITAL BLOCK --------------------------------------------------

* ## Savings Block

 SHHDEF(h)       Household savings rates
 SENDEF(e)       Enterprise savings rates
 TOTSAVEQ        Total Savings

* ## Investment Block

 QINVDEQ(c)      Investment demand in quantities
 INVESTEQ        Investment expenditure

* ------- FOREIGN INSTITUTIONS BLOCK -------------------------------------

 YFWOREQ(f)      Foreign factor income

* ------- MARKET CLEARING BLOCK ------------------------------------------

* ##### Account Closure

 FMEQUIL(f)      Factor market equilibrium
 QEQUIL(c)       Commodity market equilibrium
 GOVEQUIL        Government equilibrium
 CAEQUIL         Current account balance (foreign trade equilibrium)

* #### Absorption Closure

 VFDOMDEQ        Value of final domestic demand
 INVESTSHEQ      Value share of investment in total final domestic demand
 VGDSHEQ         Value share of Govt consumption in total final domestic demand
 VEDSHEQ(e)      Value share of Ent consumption in total final domestic demand

* ##### GDP

 GDPVAEQ         GDP from Value Added

* ##### Slack

 WALRASEQ        Savings and Investment equilibrium

  ;

*################ 15. EQUATIONS ASSIGNMENTS ##############################

* ------- TRADE BLOCK ----------------------------------------------------

* #### Exports Block

* For some c there are no exports hence only implement for ce(c)

 PEDEF(c)$ce(c)..  PE(c) =E= PWE(c) * ER * (1 - TE(c)) ;

* For some c there are no exports hence only implement for ce(c)

 CET(c)$(cd(c) AND ce(c))..
                  QXC(c) =E= at(c)*(gamma(c)*QE(c)**rhot(c) +
                                 (1-gamma(c))*QD(c)**rhot(c))**(1/rhot(c)) ;

 ESUPPLY(c)$(cd(c) AND ce(c))..
                   QE(c) =E= QD(c)*((PE(c)/PD(c))*((1-gamma(c))
                                 /gamma(c)))**(1/(rhot(c)-1)) ;

 EDEMAND(c)$ced(c)..
                   QE(c) =E= econ(c)*((PWE(c)/pwse(c))**(-eta(c))) ;

* For c with no exports OR for c with no domestic production
* domestic supply is by CETALT

 CETALT(c)$((cd(c) AND cen(c)) OR (cdn(c) AND ce(c)))..
                  QXC(c) =E= QD(c) + QE(c) ;

* #### Imports Block

* For some c there are no imports hence only implement for cm(c)

 PMDEF(c)$cm(c)..  PM(c) =E= (PWM(c) *(1 + TM(c))) * ER ;

* For some c there are no imports or domestic production
* hence only implement for cd(c) AND cm(c)

 ARMINGTON(c)$(cx(c) AND cm(c))..
                  QQ(c)  =E= ac(c)*(delta(c)*QM(c)**(-rhoc(c)) +
                                 (1-delta(c))*QD(c)**(-rhoc(c)))**(-1/rhoc(c)) ;

 COSTMIN(c)$(cx(c) AND cm(c))..
                   QM(c) =E= QD(c)*((PD(c)/PM(c))*(delta(c)/
                                 (1-delta(c))))**(1/(1+rhoc(c))) ;

* For c with no imports OR for c with no domestic production
* supply is from ARMALT

 ARMALT(c)$((cx(c) AND cmn(c)) OR (cxn(c) AND cm(c)))..
                    QQ(c) =E= QD(c) + QM(c) ;

* ------- COMMODITY PRICE BLOCK ------------------------------------------

 PQDDEF(c)$(cd(c) OR cm(c))..
                  PQD(c) =E= PQS(c) * (1 + TS(c) + TEX(c)) ;

 PQSDEF(c)$(cd(c) OR cm(c))..
            PQS(c)*QQ(c) =E= (PD(c)*QD(c))+(PM(c)*QM(c)) ;

 PXCDEF(c)$cx(c)..
           PXC(c)*QXC(c) =E= (PD(c)*QD(c)) + (PE(c)*QE(c))$ce(c) ;

* ------- NUMERAIRE PRICE BLOCK ------------------------------------------

 CPIDEF..            CPI =E= SUM(c,comtotsh(c)*PQD(c)) ;

 PPIDEF..            PPI =E= SUM(c,vddtotsh(c)*PD(c)) ;

* ------- PRODUCTION BLOCK -----------------------------------------------

 PXDEF(a)..        PX(a) =E= SUM(c,ioqxacqx(a,c)*PXAC(a,c)) ;

* CES aggregation functions for Level 1 of production nest

 PVADEF(a)..  PX(a)*(1 - TX(a))*QX(a)
                         =E= (PVA(a)*QVA(a)) + (PINT(a)*QINT(a)) ;

 PINTDEF(a)..    PINT(a) =E= SUM(c,ioqtdqd(c,a) * PQD(c)) ;

 ADXEQ(a)..       ADX(a) =E= ((adxb(a) + dabadx(a)) * ADXADJ)
                                      + (DADX * adx01(a)) ;

 QXPRODFN(a)$aqx(a)..
                   QX(a) =E= ADX(a)*(deltax(a)*QVA(a)**(-rhocx(a))
                              + (1-deltax(a))*QINT(a)**(-rhocx(a)))
                              **(-1/rhocx(a)) ;

 QXFOC(a)$aqx(a)..
                  QVA(a) =E= QINT(a)*((PINT(a)/PVA(a))*(deltax(a)/
                                 (1-deltax(a))))**(1/(1+rhocx(a))) ;

* Leontief aggregation functions for Level 1 of production nest

 QINTDEF(a)$aqxn(a)..
                 QINT(a) =E= ioqintqx(a) * QX(a) ;

 QVADEF(a)$aqxn(a)..
                  QVA(a) =E= ioqvaqx(a) * QX(a) ;

* CES aggregation functions for Level 2 of production nest

 ADVAEQ(a)..     ADVA(a) =E= ((advab(a) + dabadva(a)) * ADVAADJ)
                                      + (DADVA * adva01(a)) ;

 QVAPRODFN(a)..
                  QVA(a) =E= ADVA(a)*(SUM(f$deltava(f,a),
                              deltava(f,a)
                               *(ADFD(f,a)*FD(f,a))**(-rhocva(a))))
                                 **(-1/rhocva(a)) ;

 QVAFOC(f,a)$deltava(f,a)..
   WF(f)*WFDIST(f,a)*(1 + TF(f,a))
                         =E= PVA(a)*QVA(a)
                             *(SUM(fp$deltava(fp,a),deltava(fp,a)
                             *(ADFD(f,a)*FD(fp,a))**(-rhocva(a))))**(-1)
                             *deltava(f,a)*ADFD(f,a)**(-rhocva(a))
                              *FD(f,a)**(-rhocva(a)-1) ;


* Intermediate Input Demand

 QINTDEQ(c)..   QINTD(c) =E= SUM(a,ioqtdqd(c,a)*QINT(a)) ;

* Commodity Output

* CES aggregation of differentiated commodities

 COMOUT(c)$(cx(c) and cxac(c))..
                  QXC(c) =E= adxc(c)*(SUM(a$deltaxc(a,c),deltaxc(a,c)
                             *QXAC(a,c)**(-rhocxc(c))))**(-1/rhocxc(c)) ;

 COMOUTFOC(a,c)$(deltaxc(a,c) and cxac(c))..
               PXAC(a,c) =E= PXC(c)*QXC(c)
                             *(SUM(ap$deltaxc(ap,c),deltaxc(ap,c)
                             *QXAC(ap,c)**(-rhocxc(c))))**(-1)
                             *deltaxc(a,c)*QXAC(a,c)**(-rhocxc(c)-1) ;

* Aggregation of homogenous commodities

 COMOUT2(c)$(cx(c) and cxacn(c))..
                  QXC(c) =E= SUM(a,QXAC(a,c)) ;

 COMOUTFOC2(a,c)$(deltaxc(a,c) and cxacn(c))..
               PXAC(a,c) =E= PXC(c) ;

* Activity Output

 ACTIVOUT(a,c)$ioqxacqx(a,c)..
               QXAC(a,c) =E= ioqxacqx(a,c) * QX(a) ;

* ######## FACTOR BLOCK

 YFEQ(f)..         YF(f) =E= SUM(a,WF(f)*WFDIST(f,a)*FD(f,a))
                             + (factwor(f)*ER) ;

 YFDISPEQ(f).. YFDISP(f) =E= (YF(f) * (1- deprec(f)))*(1 - TYF(f)) ;

* ######## HOUSEHOLD BLOCK
* ## Household Income

 YHEQ(h)..         YH(h) =E= SUM(f,hovash(h,f)*YFDISP(f))
                             + SUM(hp,HOHO(h,hp))
                             + SUM(e,HOENT(h,e))
                             + (HGADJ * hogovconst(h)*CPI)
                             + (howor(h)*ER) ;

* Household Expenditure

 HOHOEQ(h,hp)..
              HOHO(h,hp) =E= hohosh(h,hp)
                             *((YH(h) * (1 - TYH(h))) * (1 - SHH(h))) ;

 HEXPEQ(h)..     HEXP(h) =E= ((YH(h) * (1 - TYH(h))) * (1 - SHH(h)))
                             - SUM(hp,HOHO(hp,h)) ;

 QCDEQ(c,h)..
         PQD(c)*QCD(c,h) =E= (PQD(c)*qcdconst(c,h))
                             + beta(c,h)
                              *(HEXP(h)-SUM(cp,PQD(cp)*qcdconst(cp,h))) ;

* ------- ENTERPRISE BLOCK -----------------------------------------------
* ## Enterprise Income

 YEEQ(e)..         YE(e) =E= SUM(f,entvash(e,f)*YFDISP(f))
                             + (EGADJ * entgovconst(e) * CPI)
                             + (entwor(e)*ER) ;

* ## Enterprise Expenditure

 QEDEQ(c,e)..
                QED(c,e) =E= QEDADJ*qedconst(c,e) ;

 HOENTEQ(h,e)..
              HOENT(h,e) =E= hoentsh(h,e)
                             * (((YE(e) * (1 - TYE(e))) * (1 - SEN(e)))
                             - SUM(c,QED(c,e)*PQD(c))) ;

 GOVENTEQ(e).. GOVENT(e) =E= goventsh(e)
                             * (((YE(e) * (1 - TYE(e))) * (1 - SEN(e)))
                              - SUM(c,QED(c,e)*PQD(c))) ;

 VEDEQ(e)..       VED(e) =E= SUM(c,QED(c,e)*PQD(c)) ;

* ------- GOVERNMENT BLOCK -----------------------------------------------

* #### Government Income Block

* ## Government Tax Rates

 TMDEF(c)..        TM(c) =E= ((tmb(c) + dabtm(c)) * TMADJ)
                                      + (DTM * tm01(c)) ;

 TEDEF(c)..        TE(c) =E= ((teb(c) + dabte(c)) * TEADJ)
                                      + (DTE * te01(c)) ;

 TSDEF(c)$(cd(c) OR cm(c))..
                   TS(c) =E= ((tsb(c) + dabts(c)) * TSADJ)
                                      + (DTS * ts01(c)) ;

 TEXDEF(c)$(cd(c) OR cm(c))..
                  TEX(c) =E= ((texb(c) + dabtex(c)) * TEXADJ)
                                      + (DTEX * tex01(c)) ;

 TXDEF(a)..        TX(a) =E= ((txb(a) + dabtx(a)) * TXADJ)
                                      + (DTX * tx01(a)) ;

 TFDEF(f,a)..    TF(f,a) =E= ((tfb(f,a) + dabtf(f,a))* TFADJ)
                                      + (DTF*tf01(f,a)) ;

 TYFDEF(f)..      TYF(f) =E= ((tyfb(f) + dabtyf(f)) * TYFADJ)
                                     + (DTYF * tyf01(f)) ;

 TYHDEF(h)..      TYH(h) =E= ((tyhb(h) + dabtyh(h)) * TYHADJ * TYADJ)
                                     + (DTYH * DTY * tyh01(h)) ;

 TYEDEF(e)..      TYE(e) =E= ((tyeb(e) + dabtye(e)) * TYEADJ * TYADJ)
                                      + (DTYE * DTY * tye01(e)) ;

* ## Government Tax Revenues

 MTAXEQ..            MTAX =E= SUM(c,TM(c)*PWM(c)*ER*QM(c)) ;

 ETAXEQ..            ETAX =E= SUM(c,TE(c)*PWE(c)*ER*QE(c)) ;

 STAXEQ..            STAX =E= SUM(c,TS(c)*PQS(c)*QQ(c)) ;

 EXTAXEQ..          EXTAX =E= SUM(c,TEX(c)*PQS(c)*QQ(c)) ;

 ITAXEQ..            ITAX =E= SUM(a,TX(a)*PX(a)*QX(a)) ;

 FTAXEQ..            FTAX =E= SUM((f,a),TF(f,a)*WF(f)*WFDIST(f,a)*FD(f,a)) ;

 FYTAXEQ..          FYTAX =E= SUM(f,TYF(f)*(YF(f) * (1- deprec(f)))) ;

 DTAXEQ..            DTAX =E= SUM(h,TYH(h)*YH(h))
                              + SUM(e,TYE(e)*YE(e)) ;

* ## Government Income

 YGEQ..               YG =E= MTAX + ETAX
                             + STAX + EXTAX + ITAX + FTAX
                             + FYTAX + DTAX
                             + SUM(f,govvash(f)*YFDISP(f))
                             + SUM(e,GOVENT(e)) + (govwor*ER) ;

* #### Government Expenditure Block

 QGDEQ(c)..       QGD(c) =E= QGDADJ*comgovconst(c) ;

 EGEQ..               EG =E= SUM(c,QGD(c)*PQD(c))
                             + SUM(h,hogovconst(h)*CPI*HGADJ)
                             + SUM(e,EGADJ*entgovconst(e)*CPI) ;

 VGDEQ..             VGD =E= SUM(c,QGD(c)*PQD(c)) ;

* ------- KAPITAL BLOCK --------------------------------------------------

* ## Savings Block

 SHHDEF(h)..     SHH(h) =E= ((shhb(h) + dabshh(h)) * SHADJ * SADJ)
                              + (DSHH * DS * shh01(h)) ;

 SENDEF(e)..     SEN(e) =E= ((senb(e) + dabsen(e)) * SEADJ * SADJ)
                              + (DSEN * DS * sen01(e)) ;

 TOTSAVEQ..       TOTSAV =E= SUM(f,(deprec(f)*YF(f)))
                             + SUM(h,(YH(h) * (1 - TYH(h))) * SHH(h))
                             + SUM(e,(YE(e) * (1 - TYE(e))) * SEN(e))
                             + KAPGOV
                             + (CAPWOR*ER) ;

* ## Investment Block

 QINVDEQ(c)..   QINVD(c) =E= (IADJ*invconst(c)) ;

 INVESTEQ..       INVEST =E= SUM(c,PQD(c)*(QINVD(c) + dstocconst(c))) ;

* ------- FOREIGN INSTITUTIONS BLOCK -------------------------------------

 YFWOREQ(f)..   YFWOR(f) =E= worvash(f)*YFDISP(f) ;

* ------- MARKET CLEARING BLOCK ------------------------------------------

* ##### Account Closure

 FMEQUIL(f)..      FS(f) =E= SUM(a,FD(f,a)) ;

 QEQUIL(c)..      QQ(c)  =E= QINTD(c) + SUM(h,QCD(c,h)) + SUM(e,QED(c,e))
                               + QGD(c) + QINVD(c) + dstocconst(c) ;

 GOVEQUIL..       KAPGOV =E= YG - EG ;

 CAEQUIL..        CAPWOR =E= SUM(cm,PWM(cm)*QM(cm))
                             + (SUM(f,YFWOR(f))/ER)
                             - SUM(ce,PWE(ce)*QE(ce))
                             - SUM(h,howor(h))
                             - SUM(e,entwor(e))
                             - govwor
                             - SUM(f,factwor(f)) ;

* #### Absorption Closure

 VFDOMDEQ..       VFDOMD =E= SUM(c, PQD(c) *
                                (SUM(h,QCD(c,h)) + SUM(e,QED(c,e))
                                 + QGD(c) + QINVD(c) + dstocconst(c))) ;

 INVESTSHEQ..
       INVESTSH * VFDOMD =E= INVEST ;

 VGDSHEQ..
          VGDSH * VFDOMD =E= VGD ;

 VEDSHEQ(e)..
       VEDSH(e) * VFDOMD =E= VED(e) ;

* ##### GDP

 GDPVAEQ..         GDPVA =E= SUM((f,a),WF(f)*WFDIST(f,a)*FD(f,a))
                               + MTAX + ETAX + STAX + EXTAX + ITAX + FTAX ;

* ##### Slack

 WALRASEQ..       TOTSAV =E= INVEST + WALRAS ;


*################ 18. INPUTS FOR GAMS MIRO ##################################
$onexternalinput
Scalar
 closurechoice Government financing choice /1/;
$offexternalinput

IF(closurechoice = 1,

$include stg1_bclose_capgovclos.inc

* end of IF statement
) ;


IF(closurechoice = 2,

$include stg1_bclose_inctaxclos.inc

* end of IF statement
) ;

*################ 17. MODEL & SOLVE STATEMENTS ###########################
put log /'Generating model... ';

$ontext

limrow - number of rows for each equation block in the *.lst file (default 3)
limcol - number of columns for each equation block in the *.lst file (default 3)
iterlim - limit on iterations (default 1000)
nlp - non-linear programming
MINOS5 - MINOS5 solver
CONOPT - CONOPT Solver
CONOPT2 - CONOPT2 Solver
mcp - mixed complementarity problem
PATH - PATH Solver

$offtext

 OPTION DECIMALS = 6 ;

 Options limrow=500,limcol=0;

* option iterlim = 25000 ;

 option MCP           = PATH ;

 Model Models1 /

$ontext
When matching equations and variables do NOT match variables that may be
fixed in any model closure.
$offtext
* ------- TRADE BLOCK ----------------------------------------------------
* #### Exports Block
 PEDEF.PE
 CET
 ESUPPLY
 EDEMAND
 CETALT
* #### Imports Block
 PMDEF.PM
 ARMINGTON
 COSTMIN
 ARMALT
* ------- COMMODITY PRICE BLOCK ------------------------------------------
 PQSDEF.PQS
 PQDDEF.PQD
 PXCDEF.PXC
* ------- NUMERAIRE PRICE BLOCK ------------------------------------------
 CPIDEF
 PPIDEF
* ------- PRODUCTION BLOCK -----------------------------------------------
 PXDEF.PX
 PVADEF.PVA
 PINTDEF.PINT
 ADXEQ.ADX
 QXPRODFN.QX
 QXFOC
 QINTDEF.QINT
 QVADEF.QVA
 ADVAEQ.ADVA
 QVAPRODFN
 QVAFOC
 QINTDEQ.QINTD
 COMOUT
 COMOUTFOC
 COMOUT2
 COMOUTFOC2
 ACTIVOUT.QXAC
* ------- FACTOR BLOCK ---------------------------------------------------
 YFEQ.YF
 YFDISPEQ.YFDISP
* ------- HOUSEHOLD BLOCK ------------------------------------------------
 YHEQ.YH
 HOHOEQ.HOHO
 HEXPEQ.HEXP
 QCDEQ.QCD
* ------- ENTERPRISE BLOCK -----------------------------------------------
 YEEQ.YE
 QEDEQ.QED
 HOENTEQ.HOENT
 VEDEQ
 GOVENTEQ.GOVENT
* ------- GOVERNMENT BLOCK -----------------------------------------------
 TEDEF.TE
 TMDEF.TM
 TSDEF.TS
 TEXDEF.TEX
 TXDEF.TX
 TFDEF.TF
 TYFDEF.TYF
 TYHDEF.TYH
 TYEDEF.TYE
 MTAXEQ.MTAX
 ETAXEQ.ETAX
 STAXEQ.STAX
 EXTAXEQ.EXTAX
 ITAXEQ.ITAX
 FTAXEQ.FTAX
 FYTAXEQ.FYTAX
 DTAXEQ.DTAX
 YGEQ.YG
 QGDEQ.QGD
 EGEQ.EG
 VGDEQ.VGD
* ------- KAPITAL BLOCK --------------------------------------------------
 SHHDEF.SHH
 SENDEF.SEN
 TOTSAVEQ.TOTSAV
 QINVDEQ.QINVD
 INVESTEQ.INVEST
* ------- FOREIGN INSTITUTIONS BLOCK -------------------------------------
 YFWOREQ.YFWOR
* ------- MARKET CLEARING BLOCK ------------------------------------------
 FMEQUIL
 QEQUIL.QQ
 GOVEQUIL
 CAEQUIL
 VFDOMDEQ
 INVESTSHEQ
 VGDSHEQ
 VEDSHEQ
 GDPVAEQ.GDPVA
 WALRASEQ.WALRAS

             / ;

 PM.FX(c)$(NOT cm(c))         = 0 ;
 PE.FX(c)$(NOT ce(c))         = 0 ;
 PD.FX(c)$(NOT cd(c))         = 0 ;
 PXC.FX(c)$(NOT cx(c))        = 0 ;
 PXAC.FX(a,c)$(NOT SAM(a,c) ) = 0 ;

 QM.FX(c)$(NOT cm(c))         = 0 ;
 QE.FX(c)$(NOT ce(c))         = 0 ;
 QD.FX(c)$(NOT cd(c))         = 0 ;
 QXC.FX(c)$(NOT cx(c))        = 0 ;
 QXAC.FX(a,c)$(NOT SAM(a,c))  = 0 ;

 FD.FX(f,a)$(NOT SAM(f,a))    = 0 ;

 HOHO.FX(h,hp)$(NOT SAM(h,hp)) = 0 ;

 Models1.HOLDFIXED   = 1 ;
 Models1.TOLINFREP   = 0.0000001 ;
put log 'Done'/;
put log /'Running model (1/2)... ';
 Solve Models1 Using MCP ;
put log 'Done'/;

put log /'Checking SAM... ';
*################ 19. SOCIAL ACCOUNTING MATRICES #########################
$ontext
This section checks that the model is solved with a balanced SAM,
by using a Macro and a Micro SAM.

The MICRO SAM element can be switched off by setting "micsam2" = 0 in the
parameter 'flow_cont' in the worksheet 'controls'

The check on the MICRO SAM is only implemented if "numerchk" = 1

 1) Macro SAM
  The programme aborts IF
           - ROW AND COLUMN SUMS FOR ASAM2 NOT EQUAL
           - ALL ASAM1 CELL ENTRIES DO NOT EQUAL ASAM0 ENTRIES

 2) Micro SAM
  The programme aborts IF
           - ALL SAM2 CELL ENTRIES DO NOT EQUAL SAM1 ENTRIES
$offtext

$INCLUDE stg1_samchk2.inc
put log 'Done'/;

*################ 20. POLICY EXPERIMENTS #################################

* ------- Experiment file include ----------------------------------------
$ontext
The file Experiment.GMS is a standard file, but it references other include
files for loading experiment parameters and for introducing experiment shocks

Experiment file not used in MIRO version as experiments implemented via MIRO.
$offtext

*$INCLUDE stg1_expt.inc

$ontext
The code in this section includes additional parameters to link the model
to GAMS MIRO and the closure for the model.

The standard or default closure is a minimal closure.
$offtext

$INCLUDE stg1_miro_input.inc

*################ 21. RESULTS ANALYSES ###################################
put log /'Analyzing results... ';

$ontext
The various files for the analysis of results can be called here or by using
programme analyse.gms to carry out the same actions using results stored
in GDX

EACH COMPONENT IS INDEPENDENT, i.e., each component only makes use of the
results saved into the standard GDX results file that is produced by the
simulations module. This includes the levels values for the model variables,
all the initial values of variables, all the model parameter values, all the
model and simulation sets

$offtext

$INCLUDE stg1_anal.inc
put log 'Done'/;
*################ 22. OUTPUT FOR GAMS MIRO ###############################
put log /'Processing data... ';

$ontext
The code in this section includes additional parameters used
for showing results in the GAMS MIRO application.

$offtext

$INCLUDE stg1_miro_output.inc
put log 'Done'/;


* ================================================= END OF MODEL CODE FILE
