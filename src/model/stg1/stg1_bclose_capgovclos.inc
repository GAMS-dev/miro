*-------------------- smod basic closure ---------------------------------
$ontext
  This is a minimal neo classical closure

                     CONDITION

     FEX        - Exchange rate flexible
                - KAPWOR fixed

     Investment - absorption share fixed

     Government - absorption share fixed
                - Tax rates                            - ALL Fixed
                - CAPGOV fixed

     Factors    - Land sector - mobile & full
                - Capital - mobile & full
                - Unskilled labour - mobile & full
                - Skilled labour - mobile & full

     Numeraire  - CPI

It is assumed that the closures used for experiments will be set in the
experiment files.
$offtext

*## FOREIGN EXCHANGE MARKET CLOSURE
$ontext
 In this MODEL
 EITHER the exchange rate is fixed and the current account balance is the
 equilibrating variable,
 OR the current account balance is fixed and the excahnge rate is the
 equilibrating variable.

 Import prices are always fixed BUT export prices can be determined via an
 export demand function; control via the set cedn.
$offtext

 ER.FX            = ER0 ;

* alternatively the external balance is fixed

* CAPWOR.FX       = CAPWOR0 ;

* Fixing of world prices
 PWM.FX(c)        = PWM0(c) ;
 PWE.FX(cedn)     = PWE0(cedn) ;

*## INVESTMENT-SAVINGS CLOSURE
$ontext
 IF Aggregate investment is determined by aggregate savings,
 i.e., the model is savings driven, then fix ALL of SADJ, SHADJ, SEADJ,
 DSHH, DS and DSEN.

 IF the model is investment driven only one of SADJ, SHADJ, SEADJ, DSHH,
 DS or DSEN should be variable.

 MULTIPLICATIVE adjustment
 SADJ variable - h'hold and enterprise savings rates vary equiproportionately
 SHADJ variable - h'hold savings rates vary equiproportionately
 SEADJ variable - enterprise savings rates vary equiproportionately

 ADDITIVE adjustment
 DS variable - selected h'hold and enterprise savings rates vary
 DSHH variable - selected h'hold savings rates vary
 DSEN variable - selected enterprise savings rates vary

 For investment driven closures the 'level' of investment can be defined
 in terms of the volume, value or share of final demand expenditure.
$offtext

* Savings Rate adjustments

* SADJ.FX          = SADJ0 ;
 SHADJ.FX         = SHADJ0 ;
 SEADJ.FX         = SEADJ0 ;

 DSHH.FX          = DSHH0 ;
 DS.FX            = DS0 ;
 DSEN.FX          = DSEN0 ;

* Investment 'level' adjustments

* IF the model is investment driven fix the investment scaling factor

* IADJ.FX         = IADJ0 ;

* OR the value or share of domestic final demand of investment is fixed

* INVEST.FX       = INVEST0 ;
 INVESTSH.FX     = INVESTSH0 ;

* Closure rule for Government savings is part of the Govt Closure Rules

*## ENTERPRISE CLOSURE RULES
$ontext
 ONE of the volume, value or share of final demand should be fixed
$offtext

 QEDADJ.FX        = QEDADJ0 ;
* VED.FX          = VED0 ;
* VEDSH.FX        = VEDSH0 ;

* Enterprise transfers to households

 HEADJ.FX        = HEADJ0 ;

*## GOVT CLOSURE RULES
$ontext
 IF ALL tax rates are fixed AND Government consumption expenditures
 and transfers are fixed
 the equilibriating variable is Government Savings

 IF a tax rate is variable then an alternative equilibriating variable
 is required.

 Note that the tax rates can be varied multiplicatively or additively
$offtext

* Tax rate scaling factors

 TEADJ.FX         = TEADJ0 ;
 TMADJ.FX         = TMADJ0 ;
 TSADJ.FX         = TSADJ0 ;
 TEXADJ.FX        = TEXADJ0 ;
 TYFADJ.FX        = TYFADJ0 ;
 TXADJ.FX         = TXADJ0 ;
 TFADJ.FX         = TFADJ0 ;
 TYHADJ.FX        = TYHADJ0 ;
 TYADJ.FX         = TYADJ0 ;
 TYEADJ.FX        = TYEADJ0 ;

 DTE.FX           = DTE0 ;
 DTM.FX           = DTM0 ;
 DTS.FX           = DTS0 ;
 DTEX.FX          = DTEX0 ;
 DTX.FX           = DTX0 ;
 DTF.FX           = DTF0 ;
 DTYF.FX          = DTYF0 ;
 DTYH.FX          = DTYH0 ;
 DTY.FX           = DTY0 ;
 DTYE.FX          = DTYE0 ;

* Government expenditure scaling factor
* EITHER the volume, value or share of final demand should be fixed

* QGDADJ.FX       = QGDADJ0 ;
* VGD.FX          = VGD0 ;
 VGDSH.FX        = VGDSH0 ;

* Government transfers scaling factors are fixed

 HGADJ.FX         = HGADJ0 ;
 EGADJ.FX         = EGADJ0 ;

* alternatively the internal balance / Govt savings can be fixed

* KAPGOV.FX       = KAPGOV0 ;

*## FACTOR MARKET CLOSURE

*# Basic Factor Market Closure
$ontext
 ALL factors are fully employed and mobile
$offtext
*$ontext

 FS.FX(f)                = FS0(f) ;
 WFDIST.FX(f,a)          = WFDIST0(f,a) ;
 WF.LO(f)                = -inf ;
 WF.UP(f)                = +inf ;

*$offtext

*# Alternative Factor Market Closures

$ontext
This code allows for controlling individual factors.

It relies on the following subsets of f AND l being defined
 k(f) - capital
 lnd(f) - land
 l(f) - labour
         lu(l) - unskilled labour
         lm(l) - skilled or unskilled Labour Factors
         ls(l) - skilled labour

When changing factor market closure rules be careful to count how many
conditions you relax, i.e., unfix, and how many you fix.

TO MAKE A FACTOR ACTIVITY SPECIFIC AND FIXED
i) Unfix FS.FX("factor") and WFDIST.FX("factor",a)
ii) Fix FD.FX("factor",a) AND ONE WFDIST.FX("??",a)

TO ALLOW FOR AN UNEMPLOYED FACTOR
i) Unfix FS.FX("factor")
ii) Fix WF.FX("factor") AND FS.LO("factor") and FS.UP("factor")

TO CONTROL FACTOR USE BY ACTIVITY
Adapt the procedure for making a factor activity specific and fixed
by specifying the activities.

$offtext

$ontext

* This section

  FS.FX(k)                = FS0(k) ;
  WFDIST.FX(k,a)          = WFDIST0(k,a) ;
  WF.LO(k)                = -inf ;
  WF.UP(k)                = +inf ;
*  WFDIST.FX(k,anch)       = WFDIST0(k,anch) ;
*  FD.FX(k,a)              = FD0(k,a) ;
*  WF.FX(k)                = WF0(k) ;
*  FS.LO(k)                = -inf ;
*  FS.UP(k)                = +inf ;

  FS.FX(n)                = FS0(n) ;
  WFDIST.FX(n,a)          = WFDIST0(n,a) ;
  WF.LO(n)                = -inf ;
  WF.UP(n)                = +inf ;
*  WFDIST.FX(n,anchN)     = WFDIST0(n,anchN) ;
*  FD.FX(n,a)              = FD0(n,a) ;
*  WF.FX(n)                = WF0(n) ;
*  FS.LO(n)                = -inf ;
*  FS.UP(n)                = +inf ;

  FS.FX(ls)               = FS0(ls) ;
  WFDIST.FX(ls,a)         = WFDIST0(ls,a) ;
  WF.LO(ls)               = -inf ;
  WF.UP(ls)               = +inf ;
*  WFDIST.FX(ls,anch)      = WFDIST0(ls,anch) ;
*  FD.FX(ls,a)             = FD0(ls,a) ;
*  WF.FX(ls)               = WF0(ls) ;
*  FS.LO(ls)               = -inf ;
*  FS.UP(ls)               = +inf ;

  FS.FX(lm)               = FS0(lm) ;
  WFDIST.FX(lm,a)         = WFDIST0(lm,a) ;
  WF.LO(lm)               = -inf ;
  WF.UP(lm)               = +inf ;
*  WFDIST.FX(lm,anch)      = WFDIST0(lm,anch) ;
*  FD.FX(lm,a)             = FD0(lm,a) ;
*  WF.FX(lm)               = WF0(lm) ;
*  FS.LO(lm)               = -inf ;
*  FS.UP(lm)               = +inf ;

  FS.FX(lu)               = FS0(lu) ;
  WFDIST.FX(lu,a)         = WFDIST0(lu,a) ;
  WF.LO(lu)               = -inf ;
  WF.UP(lu)               = +inf ;
*  WFDIST.FX(lu,anch)      = WFDIST0(lu,anch) ;
*  FD.FX(lu,a)             = FD0(lu,a) ;
*  WF.FX(lu)               = WF0(lu) ;
*  FS.LO(lu)               = -inf ;
*  FS.UP(lu)               = +inf ;

$offtext

* # Technology Closures for Factor Market
$ontext

$offtext
* Technology for CES production functions for Level 1 of production nest

 ADXADJ.FX        = ADXADJ0 ;
 DADX.FX          = DADX0 ;

* Technology for CES aggregation functions for Level 2 of production nest

 ADVAADJ.FX       = ADVAADJ0 ;
 DADVA.FX         = DADVA0 ;

* Technology for factor activity and region specific factor efficiency

 ADFD.FX(f,a)     = ADFD0(f,a) ;

*## MISCELLANEOUS FIXED VARIABLES
$ontext

$offtext
* To use CPI as the numeraire fix CPI

 CPI.FX            = CPI0 * mod_cont("numerchk") ;

* To fix the real exchange rate fix ER and PPI

* PPI.FX           = PPI0 * numerchk ;
