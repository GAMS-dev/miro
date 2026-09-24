*########### CHECKS ON CALIBRATION ###############################

* ------- TRADE BLOCK ----------------------------------------------------

PARAMETER
 QQ0chk(c)     check on QQ0
 QQ0chkcount   Count on QQ0chk
 QXC0chk(c)    check on QXC0
 QXC0chkcount  Count on QXC0chk

 QE0neg(c)     Negative Values for QE0
 QD0neg(c)     Negative Values for QD0
 QM0neg(c)     Negative Values for QM0

 QE0negcount   Negatives count for QD0
 QD0negcount   Negatives count for QD0
 QM0negcount   Negatives count for QD0

  ;

* ----- Supply and Demand Quantities

 QQ0chk(c)      = QM0(c) + QD0(c) - QQ0(c) ;
 QQ0chkcount    = SUM(c$QQ0chk(c),1) ;

 QXC0chk(c)     = QE0(c) + QD0(c) - QXC0(c) ;
 QXC0chkcount   = SUM(c$QXC0chk(c),1) ;

DISPLAY QQ0chk, QQ0chkcount, QXC0chk, QXC0chkcount ;

ABORT $(QQ0chkcount NE 0.0)
 "NOT All QQ0chk equal to zero" ;

*ABORT $(QXC0chkcount NE 0.0)
* "NOT All QXC0chk equal to zero" ;

* ----- 'ILLEGAL' Negativies

 QE0neg(c)$(QE0(c) LT 0.0)  = QE0(c) ;
 QE0negcount                = SUM(c$QE0neg(c),1) ;

 QD0neg(c)$(QD0(c) LT 0.0)  = QD0(c) ;
 QD0negcount                = SUM(c$QD0neg(c),1) ;

 QM0neg(c)$(QM0(c) LT 0.0)  = QM0(c) ;
 QM0negcount                = SUM(c$QM0neg(c),1) ;

Display  QE0neg, QD0neg, QM0neg ;

ABORT $(QE0negcount NE 0.0)
 "NOT All QE0 greater than zero" ;

ABORT $(QD0negcount NE 0.0)
 "NOT All QD0 greater than zero" ;

ABORT $(QM0negcount NE 0.0)
 "NOT All QM0 greater than zero" ;


* ------- NORMALISED PRICE BLOCK -----------------------------------------



* ------- COMMODITY PRICE BLOCK ------------------------------------------


* ------- NUMERAIRE PRICE BLOCK ------------------------------------------

PARAMETERS

 comtotshCHK   check on CPI weights
 vddtotshCHK   check on PPI weights

  ;

* #### Assignments

 comtotshCHK  = SUM(c,comtotsh(c)) ;
 vddtotshCHK = SUM(c,vddtotsh(c)) ;

DISPLAY comtotshCHK, vddtotshCHK ;

* ------- PRODUCTION BLOCK -----------------------------------------------

PARAMETER PXchk(a,c)       Check for PXDEF
          QXACchk(a,c)     Check for COMOUT

    ;

* PXchk(a,c)     = actcomcomsh(a,c)*PXC0(c) ;

* QXACchk(a,c)   = actcomactsh(a,c)*QX0(a) ;
 QXACchk(a,c)   = ioqxacqx(a,c)*QX0(a) ;

*DISPLAY PXchk ;
DISPLAY QXACchk ;

PARAMETERS

 ioqxCHK(a)    check on io coefficeints
 QXchk(a)      check on QX aggregation

  ;

 ioqxCHK(a)  = ioqintqx(a) + ioqvaqx(a) ;
 QXchk(a)    = QINT0(a) + QVA0(a) - QX0(a) ;

DISPLAY ioqxCHK, QXchk ;

PARAMETER

 deltaxCHK       check on deltax
 deltavaCHK(a)   check on deltava
 deltaxcCHK(c)   check on deltaxc

   ;

 deltaxCHK      = SUM(a,deltax(a)) ;
 deltavaCHK(a)  = SUM(f,deltava(f,a)) ;
 deltaxcCHK(c)  = SUM(a,deltaxc(a,c)) ;

DISPLAY deltaxCHK, deltavaCHK, deltaxcCHK ;

* ------- FACTOR BLOCK ---------------------------------------------------

PARAMETER
 vashCHK(f)     check on value added shares ;

 vashCHK(f)  = SUM(h,hovash(h,f)) + SUM(e,entvash(e,f))
                  + govvash(f) + worvash(f) ;

 DISPLAY vashCHK ;

* ------- HOUSEHOLD BLOCK ------------------------------------------------

PARAMETER
 FRISCHchk(h)       Check for non negative FRISCH parameters
 FRISCHcount        Count of non negative FRISCH parameters

 hexpschk(h)        Check on subsistence expenditure
 hexpschkcount      Count on h'holdsheck with NO subsistence expenditure

 hohototalCHK(h)    interhousehold transfer totals income
 hohototal2CHK(h)   interhousehold transfer totals expenditure
 ;

 FRISCHchk(h)$(frisch(h) GT 0.0)
                            = frisch(h) ;
 FRISCHcount                = SUM(h$FRISCHchk(h),1) ;

 QM0neg(c)$(QM0(c) LT 0.0)  = QM0(c) ;
 QM0negcount                = SUM(c$QM0neg(c),1) ;

Display  FRISCHchk, FRISCHcount ;

ABORT $(FRISCHcount NE 0.0)
 "NOT all frisch less than zero" ;

 hexpschk(h)$(hexps(h) GT HEXP0(h))
                            = hexps(h) ;

 hexpschkcount              = SUM(h$hexpschk(h),1) ;

Display hexpschk, hexpschkcount ;

ABORT $(hexpschkcount NE 0.0)
 "NOT all subsistence expenditures greater than HEXP0" ;

 hohototalCHK(h)     = SUM(hp,hohoconst(h,hp)) ;
 hohototal2CHK(h)    = SUM(hp,hohoconst(hp,h)) ;

DISPLAY hohototalCHK, hohototal2CHK ;

* ------- ENTERPRISE BLOCK -----------------------------------------------

* ------- GOVERNMENT BLOCK -----------------------------------------------

* ------- KAPITAL BLOCK --------------------------------------------------

* ------- FOREIGN INSTITUTIONS BLOCK -------------------------------------

* ------- MARKET CLEARING BLOCK ------------------------------------------
