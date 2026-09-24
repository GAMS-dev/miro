*-------------------- stg1 model calibration -----------------------------
$ontext
   Calibration proceeds by reference to the blocks of equations
each block contains all the related parameter declarations and assignments
$offtext

* ------- NORMALISED PRICE BLOCK -----------------------------------------

PARAMETERS

 PD0(c)        Consumer price for domestic supply of commodity c
 PE0(c)        Domestic price of exports by commodity c
 PM0(c)        Domestic price of competitive imports c

 PX0(a)        Composite price of output by activity a
 PXC0(c)       Producer price of composite domestic output
 PXAC0(a,c)    Activity commodity prices

 PQS0(c)       Supply price of composite commodity c

 ER0           Exchange rate                         (domestic per world unit)

   ;

* #### Assignments

 PD0(c)      = 1 ;

 PM0(c)      = 1 ;
 PE0(c)      = 1 ;

* NOTE PX, PXC, PXAC and PQS are weighted averages of normalised prices hence

 PX0(a)      = 1 ;
 PXC0(c)     = 1 ;
 PXAC0(a,c)  = 1 ;

 PQS0(c)     = 1 ;

 ER0         = 1 ;


* ------- TRADE BLOCK ----------------------------------------------------

* #### Exports Block

PARAMETERS

* ## Initial values for variables

 PWE0(c)       World price of exports in dollars

 QE0(c)        Domestic output exported by commodity c
 QD0(c)        Domestic demand for commodity c

* ## Parameters for CET functions

 at(c)          Shift parameter for Armington CET function
 gamma(c)       Share parameter for Armington CET function
 rhot(c)        Elasticity parameter for Output Armington CET function

* ## Parameters for Export Demand functions

 econ(c)        constant for export demand equations
 pwse(c)        world price of export substitutes
 eta(c)         export demand elasticity

  ;

* #### Assignments

* ## Initial values for variables

 QE0(c)      = (SAM(c,"ROW") - SAM("EXPTAX",c))/PE0(c) ;

* Using {QXC0(c) = (SUM(a,SAM(a,c))/PXC0(c))} in statement for
* QD0(c) = QXC0(c) - QE0(c) ;

 QD0(c)      = (SUM(a,SAM(a,c))/PXC0(c)) - QE0(c) ;

 PWE0(c)$QE0(c)
              = (SAM(c,"ROW")/ER0)/QE0(c) ;

* ## Parameters for CET functions

 rhot(c)$(cd(c) AND ce(c))    = (1/ELASTC(c,"omega")) + 1 ;

 gamma(c)$(cd(c) AND ce(c))
              = 1/(1+PD0(c)/PE0(c)*(QE0(c)/QD0(c))**(rhot(c)-1));

* Using {QXC0(c) = (SUM(a,SAM(a,c))/PXC0(c))} in statement for at(c)

 at(c)$(cd(c) AND ce(c))
              = (SUM(a,SAM(a,c))/PXC0(c))
                 /(gamma(c)*QE0(c)**rhot(c) + (1-gamma(c))*
                       QD0(c)**rhot(c))**(1/rhot(c));

* ## Parameters for Export Demand functions

 econ(c)         = QE0(c) ;
 eta(c)          = ELASTC(c,"exdem") ;

 pwse(c)$QE0(c)  = PWE0(c) ;

* #### Imports Block

PARAMETERS

* ## Initial values for variables

 PWM0(c)       World price of imports in dollars

 QM0(c)        Imports of commodity c
 QQ0(c)        Supply of composite commodity c

* ## Parameters for Armington/CES functions

 ac(c)          Shift parameter for Armington CES function
 delta(c)       Share parameter for Armington CES function
 rhoc(c)        Elasticity parameter for Armington CES function

  ;

* #### Assignments

 QM0(c)      = (SAM("ROW",c) + SAM("IMPTAX",c))/PM0(c) ;

 QQ0(c)      = QD0(c) + QM0(c) ;

 PWM0(c)$QM0(c)
              = (SAM("ROW",c)/ER0)/QM0(c) ;

* ## Parameters for Armington/CES functions

 rhoc(c)$(cx(c) AND cm(c))    = (1/ELASTC(c,"sigma")) - 1 ;

 delta(c)$(cd(c) AND cm(c))
               = (PM0(c)*QM0(c)**(rhoc(c)+1))
                  /(PM0(c)*QM0(c)**(rhoc(c)+1) + PD0(c)*QD0(c)**(rhoc(c)+1)) ;

 ac(c)$(cd(c) AND cm(c))
               = QQ0(c)/(delta(c)*QM0(c)**(-rhoc(c))
                         +(1-delta(c))*QD0(c)**(-rhoc(c)))**(-1/rhoc(c)) ;

* ------- COMMODITY PRICE BLOCK ------------------------------------------

PARAMETERS

* ## Initial values for variables

 PQD0(c)       Purchaser price of composite commodity c

  ;

* #### Assignments

* ## Initial values for variables

 PQD0(c)$QQ0(c)
             = (SAM(c,"TOTAL") - SAM(c,"ROW"))/QQ0(c) ;

* ------- NUMERAIRE PRICE BLOCK ------------------------------------------

PARAMETERS

* ## Initial values for variables

 CPI0          Consumer price index
 PPI0          Producer (domestic) price index

* ## Price index weights

 comtotsh(c)    Share of commodity c in total commodity demand
 vddtotsh(c)    Share of value of domestic output for the domestic market
  ;

* #### Assignments

* ## Initial values for variables

 CPI0         = SUM(c,SUM(h,SAM(c,h))/(SUM((cp,hp),SAM(cp,hp)))*PQD0(c)) ;
 PPI0         = SUM(c,(SUM(a,SAM(a,c))/SUM((ap,cp),SAM(ap,cp)))*PD0(c)) ;

* ## Price index weights

* note that the CPI weights are based on household expenditure shares ONLY
* alternate 1 - CPI weights based on household final demand expenditure shares

 comtotsh(c)$(SUM((cp,hp),SAM(cp,hp)))
              = SUM(h,SAM(c,h))/(SUM((cp,hp),SAM(cp,hp))) ;

* alternate 2 - CPI weights based on total final demand expenditure shares
$ontext
 comtotsh(c)   = (SUM(h,SAM(c,h)) + SAM(c,"govt") + SAM(c,"kap"))
                 /(SUM((cp,hp),SAM(cp,hp))
                  + SUM(cp,SAM(cp,"govt"))
                  + SUM(cp,SAM(cp,"kap")))  ;
$offtext

* PPI weights are based on the value of domestic output for the domestic market

 vddtotsh(c)$(SUM((ap,cp),SAM(ap,cp)))
               = SUM(a,SAM(a,c))/SUM((ap,cp),SAM(ap,cp)) ;

* ------- PRODUCTION BLOCK -----------------------------------------------

PARAMETERS

* ## Initial values for variables

 QX0(a)        Domestic production by activity a
 QXC0(c)       Domestic production by commodity c

 PVA0(a)       Value added price for activity a
 PINT0(a)      Price of aggregate intermediate input

 QINT0(a)      Aggregate quantity of intermediates used by activity a
 QVA0(a)       Quantity of aggregate value added for level 1 production

 QXAC0(a,c)    Domestic commodity output by each activity

 QINTD0(c)     Demand for intermediate inputs by commodity

 WF0(f)        Price of factor f
 WFDIST0(f,a)  Sectoral proportion for factor prices

 FD0(f,a)      Demand for factor f by activity a
 FS0(f)        Supply of factor f

* Technology Parameters and Adjustment factors

 ADX0(a)       Shift parameter for CES production functions for QX
 ADXADJ0       Scaling Factor for Shift parameter on CES functions for QX
 DADX0         Partial scaling factor for Shift parameter on CES functions for QX

 ADVA0(a)      Shift parameter for CES production functions for QVA
 ADVAADJ0      Scaling Factor for Shift parameter on CES functions for QVA
 DADVA0        Partial scaling factor for Shift parameter on CES functions for QVA

 ADFD0(f,a)    Shift parameter for factor and activity specific efficiency

* CES aggregation functions for Level 1 of production nest

 adxb(a)        Shift parameter for CES production functions for QX
 dabadx(a)      Change in base shift parameter on functions for QX
 adx01(a)       0-1 par for flexing of shift parameter on functions for QX

 predeltax(a)   dummy used to estimated deltax
 deltax(a)      Share parameter for CES production functions for QX
 rhocx(a)       Elasticity parameter for CES production function for QX

* Leontief aggregation functions for Level 1 of production nest

 use(c,a)       use matrix transactions
 comactco(c,a)  use matrix coefficients

 ioqintqx(a)    Agg intermed quantity per unit QX for Level 1 Leontief agg
 ioqvaqx(a)     Agg value added quant per unit QX for Level 1 Leontief agg

* CES aggregation functions for Level 2 of production nest

 advab(a)       Shift parameter for CES production functions for QVA
 dabadva(a)     Change in base shift parameter on functions for QVA
 adva01(a)      0-1 par for flexing of shift parameter on functions for QVA

 deltava(f,a)   Share parameters for CES production functions for QVA
 rhocva(a)      Elasticity parameter for CES production function for QVA

* Intermediate Input Demand

* comactactco(c,a) intermediate input output coefficients
 ioqtdqd(c,a)   intermediate input output coefficients

* Commodity Output

 adxc(c)        Shift parameter for commodity output CES aggregation
 deltaxc(a,c)   Share parameters for commodity output CES aggregation
 rhocxc(c)      Elasticity parameter for commodity output CES aggregation

* Activity Output

 ioqxacqx(a,c)    Share of commodity c in output by activity a

  ;

* #### Assignments

* ## Initial values for variables

 QX0(a)      = SAM("TOTAL",a)/PX0(a) ;
 QXC0(c)     = SUM(a,SAM(a,c))/PXC0(c) ;

 QINT0(a)    = SUM(c,SAM(c,a)/((SAM(c,"TOTAL") - SAM(c,"ROW"))/QQ0(c))) ;

 QVA0(a)     = SUM(f,SAM(f,a)) ;

 PINT0(a)$(QINT0(a))
              = SUM(c,(SAM(c,a)/PQD0(c)/QINT0(a)) * PQD0(c)) ;

 PVA0(a)$QVA0(a)
              = (SUM(f,SAM(f,a)) + SUM(tff,SAM(tff,a)) )/QVA0(a) ;

 QXAC0(a,c)  = SAM(a,c)/PXAC0(a,c) ;

 QINTD0(c)    = SUM(a,SAM(c,a)/((SAM(c,"TOTAL") - SAM(c,"ROW"))/QQ0(c))) ;

 FD0(f,a)     = FACTUSE(f,a) ;
 FS0(f)       = SUM(a,FACTUSE(f,a)) ;

 WF0(f)$(FS0(f))
              = SUM(a,SAM(f,a))/FS0(f) ;

 WFDIST0(f,a)$FD0(f,a) = (SAM(f,a)/FD0(f,a))/WF0(f) ;
 WFDIST0(f,a)$(FD0(f,a) EQ 0) = 0.0 ;

* Technology Parameters and Adjustment factors

 ADXADJ0       = 1 ;
 DADX0         = 0 ;

 ADVAADJ0      = 1 ;
 DADVA0        = 0 ;

 ADFD0(f,a)    = 1 ;

* CES aggregation functions for Level 1 of production nest

 rhocx(a)    = (1/ELASTX(a,"sigmax")) - 1 ;

 predeltax(a)$(QINT0(a))
               = (PVA0(a)/PINT0(a))*(QVA0(a)/QINT0(a))**(1+rhocx(a)) ;
 deltax(a)
               = predeltax(a)/(1.0+predeltax(a)) ;

 ADX0(a)$deltax(a)
               = QX0(a)/(deltax(a)*QVA0(a)**(-rhocx(a))
                      +(1-deltax(a))*QINT0(a)**(-rhocx(a)))**(-1/rhocx(a)) ;

 adxb(a)       = ADX0(a) ;
 dabadx(a)     = 0.0 ;
 adx01(a)      = 0.0 ;

* Leontief aggregation functions for Level 1 of production nest

 use(c,a)$SAM("TOTAL",a)
               = SAM(c,a)/SAM("TOTAL",a) ;
 comactco(c,a)$(QX0(a)$PQD0(c))
               = (SAM(c,a)/PQD0(c))/QX0(a) ;

 ioqintqx(a)$QX0(a)   = QINT0(a)/QX0(a) ;

 ioqvaqx(a)$QX0(a)    = QVA0(a)/QX0(a) ;

* CES aggregation functions for Level 2 of production nest

 rhocva(a)   = (1/ELASTX(a,"sigmava")) - 1 ;

Parameter
 tf02(f,a)  tf for calibration of production functions
  ;

 tf02(f,a)$SAM(f,a)     = ((SUM(tff$map_f_tff(f,tff),SAM(tff,a)))
                                 /SAM(f,a)) ;

* share parameters WITH factor use taxes - tf0(f,a)

 deltava(f,a)$SAM(f,a)
          = (WFDIST0(f,a)*WF0(f)*(1+tf02(f,a))*(FD0(f,a))**(1+rhocva(a)))
            /SUM(fp,WFDIST0(fp,a)*WF0(fp)*(1+tf02(fp,a))*(FD0(fp,a))
             **(1+rhocva(a))) ;

$ontext
 deltava(f,a)$SAM(f,a)
               = (WFDIST0(f,a)*WF0(f)*(FD0(f,a))**(1+rhocva(a)))
                 /SUM(fp,WFDIST0(fp,a)*WF0(fp)*(FD0(fp,a))**(1+rhocva(a))) ;
$offtext

 ADVA0(a)       = QVA0(a)/(SUM(f$(FD0(f,a)),deltava(f,a)*FD0(f,a)
                 **(-rhocva(a))))**(-1/rhocva(a));

 advab(a)      = ADVA0(a) ;
 dabadva(a)    = 0.0 ;
 adva01(a)     = 0.0 ;

* Intermediate Input Demand

 ioqtdqd(c,a)$(QINT0(a)$PQD0(c))
               = SAM(c,a)/PQD0(c)/QINT0(a) ;

* Commodity Output

 rhocxc(c)   = (1/ELASTC(c,"sigmaxc")) - 1 ;

 deltaxc(a,c)$SAM(a,c)
               = (PXAC0(a,c)*QXAC0(a,c)**(1+rhocxc(c)))
                 /SUM(ap,PXAC0(ap,c)*QXAC0(ap,c)**(1+rhocxc(c))) ;

 adxc(c)$QXC0(c)
               = QXC0(c)/(SUM(a,deltaxc(a,c)*QXAC0(a,c)
                    **(-rhocxc(c)))**(-1/rhocxc(c)));

* Activity Output

 ioqxacqx(a,c)$SUM(cp,SAM(a,cp))
               = SAM(a,c)/SUM(cp,SAM(a,cp)) ;

* ------- FACTOR BLOCK ---------------------------------------------------

PARAMETERS

* ## Initial values for variables

 YF0(f)        Income to factor f
 YFDISP0(f)    Factor income for distribution after depreciation

* ## Savings Expenditures

 deprec(f)      depreciation rate by factor f

* ## Transfer Expenditures

 hovash(h,f)    Share of income from factor f to household h
 entvash(e,f)   Share of income from factor f to enterprise e
 govvash(f)     Share of income from factor f to government
 worvash(f)     Share of income from factor f to RoW

  ;

* #### Assignments

* ## Initial values for variables

 YF0(f)      = SAM("TOTAL",f) ;
 YFDISP0(f)  = (SAM("TOTAL",f) - SAM("KAP",f) - SAM("FACTTAX",f)) ;

* ## Savings Expenditures

 deprec(f)$(SAM("TOTAL",f))
               = SAM("KAP",f)/SAM("TOTAL",f) ;

* ## Transfer Expenditures

 hovash(h,f)$(SAM("TOTAL",f) - SAM("KAP",f) - SAM("FACTTAX",f))
                 = SAM(h,f)/
                    (SAM("TOTAL",f) - SAM("KAP",f) - SAM("FACTTAX",f)) ;

 entvash(e,f)$(SAM("TOTAL",f) - SAM("KAP",f) - SAM("FACTTAX",f))
                 = SAM(e,f)/
                    (SAM("TOTAL",f) - SAM("KAP",f) - SAM("FACTTAX",f)) ;

 govvash(f)$(SAM("TOTAL",f) - SAM("KAP",f) - SAM("FACTTAX",f))
                 = SAM("GOVT",f)/
                    (SAM("TOTAL",f) - SAM("KAP",f) - SAM("FACTTAX",f)) ;

 worvash(f)$(SAM("TOTAL",f) - SAM("KAP",f) - SAM("FACTTAX",f))
                 = SAM("ROW",f)/
                    (SAM("TOTAL",f) - SAM("KAP",f) - SAM("FACTTAX",f)) ;

* ------- HOUSEHOLD BLOCK ------------------------------------------------

PARAMETERS

* ## Initial values for variables

 YH0(h)         Income to household h
 YH_cap0(h)     Income per capita adult equiv to household h
 HOHO0(h,hp)    Inter household transfer
 HEXP0(h)       Household consumption expenditure
 HEXP_cap0(h)   Household consumption expenditure per capita adult equiv

 QCD0(c,h)      Household consumption by commodity c

* ## Transfer Expenditures

 hohosh(h,hp)   Share of h'hold h after tax and saving income transferred to hp

 hohoconst(h,hp) interhousehold transfers

* ## Consumption Expenditures
* # Cobb Douglas Utility function

 comhoav(c,h)   Household consumption shares
 alphah(c,h)    Expenditure share by commodity c for household h

* # Stone-Geary Utility function

 sumelast(h)    Weighted sum of income elasticities
 yhelast(c,h)   (Normalised) household income elasticities
 beta(c,h)      Marginal budget shares
 hexps(h)       Subsistence consumption expenditure
 frisch(h)      Elasticity of the marginal utility of income
 qcdconst(c,h)  Volume of subsistence consumption

  ;

* #### Assignments

* ## Initial values for variables

 YH0(h)          = SAM("TOTAL",h) ;
 YH_cap0(h)$pop(h)
                 = YH0(h)/pop(h) ;

 HOHO0(h,hp)     = SAM(h,hp) ;

 HEXP0(h)        = SUM(c,SAM(c,h)) ;
 HEXP_cap0(h)$pop(h)
                 = HEXP0(h)/pop(h) ;

 QCD0(c,h)       = SAM(c,h)/PQD0(c) ;

* ## Transfer Expenditures

 hohosh(h,hp)$(SAM("TOTAL",h) - SAM("DIRTAX",h) - SAM("KAP",h))
                 = SAM(h,hp)/
                    (SAM("TOTAL",h) - SAM("DIRTAX",h) - SAM("KAP",h)) ;

 hohoconst(h,hp) = SAM(h,hp) ;

* ## Consumption Expenditures
* # Cobb Douglas Utility function

 comhoav(c,h)$SUM(cp,SAM(cp,h))
                 = SAM(c,h)/SUM(cp,SAM(cp,h)) ;

 alphah(c,h)$SUM(cp,SAM(cp,h))
                 = SAM(c,h)/(SUM(cp,SAM(cp,h))) ;

* # Stone-Geary Utility function
* Weighted sum of the expenditures elasticities must equal one.
* Weights are average budget shares. Normalised elasticities will satisfy
* Engel aggregation

 sumelast(h)   = SUM(c, alphah(c,h)*ELASTY(c,h)) ;

 yhelast(c,h)$sumelast(h)
               = ELASTY(c,h)/sumelast(h) ;

 yhelast(c,h)$(alphah(c,h) eq 0)  = 0 ;

 beta(c,h)     = yhelast(c,h)*alphah(c,h) ;

 frisch(h)     = ELASTMU(h,"frisc") ;

 qcdconst(c,h)$PQD0(c)
               = (HEXP0(h)/PQD0(c))*(alphah(c,h)+(beta(c,h)/frisch(h))) ;

 hexps(h)      = SUM(c,qcdconst(c,h) * PQD0(c)) ;

* ------- ENTERPRISE BLOCK -----------------------------------------------

PARAMETERS

* ## Initial values for scaling variables

 QEDADJ0       Enterprise demand volume Scaling Factor
 HEADJ0        Scaling factor for enterprise transfers to households

* ## Initial values for variables

 YE0(e)        Enterprise income

 HOENT0(h,e)   Household Income from enterprise e
 GOVENTSH0     Share of entp' income after tax save and consump to govt
 GOVENT0(e)    Government income from enterprise e

 QED0(c,e)     Enterprise consumption by commodity c
 VED0(e)       Value of enterprise consumption expenditure

* ## Transfer Expenditures

 hoentconst(h,e)  transfers to hhold h from enterprise e (nominal)
 hoentsh(h,e)   Share of entp' income after tax save and consump to h'hold
 goventsh(e)    Share of entp' income after tax save and consump to govt

* ## Consumption Expenditures

* comentconst(c,e) Enterprise demand volume
 qedconst(c,e) Enterprise demand volume

  ;

* #### Assignments

* ## Initial values for scaling variables

 QEDADJ0 = 1 ;
 HEADJ0    = 1 ;

* ## Initial values for variables

 YE0(e)      = SAM("TOTAL",e) ;

 HOENT0(h,e) = SAM(h,e) ;
 GOVENT0(e)  = SAM("GOVT",e) ;
 GOVENTSH0(e)$(SAM("TOTAL",e) - SAM("DIRTAX",e) - SAM("KAP",e) - SUM(c,SAM(c,e)))
             = SAM("GOVT",e)/
                (SAM("TOTAL",e) - SAM("DIRTAX",e)
                       - SAM("KAP",e) - SUM(c,SAM(c,e))) ;

 QED0(c,e) = SAM(c,e)/PQD0(c) ;
 VED0(e)   = SUM(c,SAM(c,e)) ;

* ## Transfer Expenditures

 hoentconst(h,e) = SAM(h,e)/CPI0 ;

 hoentsh(h,e)$(SAM("TOTAL",e) - SAM("DIRTAX",e) - SAM("KAP",e) - SUM(c,SAM(c,e)))
                 = SAM(h,e)/
                    (SAM("TOTAL",e) - SAM("DIRTAX",e)
                       - SAM("KAP",e) - SUM(c,SAM(c,e))) ;

 goventsh(e)$(SAM("TOTAL",e) - SAM("DIRTAX",e) - SAM("KAP",e) - SUM(c,SAM(c,e)))
                 = SAM("GOVT",e)/
                      (SAM("TOTAL",e) - SAM("DIRTAX",e)
                        - SAM("KAP",e) - SUM(c,SAM(c,e))) ;

* ## Consumption Expenditures

 qedconst(c,e)   = SAM(c,e)/PQD0(c) ;

* ------- GOVERNMENT BLOCK -----------------------------------------------

* #### Government Taxes Block

PARAMETERS
* ## Initial values for overall scaling variables
 TEADJ0        Export subsidy Scaling Factor
 TMADJ0        Tarrif rate Scaling Factor
 TSADJ0        Sales tax rate scaling factor
 TEXADJ0       Excise tax rate scaling factor
 TXADJ0        Indirect Tax Scaling Factor
 TFADJ0        Factor Use Tax Scaling Factor
 TYFADJ0       Factor Tax Scaling Factor
 TYHADJ0       Household Income Tax Scaling Factor
 TYADJ0        Household and Enterprise Income Tax Scaling Factor
 TYEADJ0       Enterprise income tax Scaling Factor

* ## Initial values for partial scaling variables
 DTE0         Partial Export tax rate scaling factor
 DTM0         Partial Tariff rate scaling factor
 DTS0         Partial Sales tax rate scaling factor
 DTEX0        Partial Excise tax rate scaling factor
 DTX0         Partial Indirect tax rate scaling factor
 DTF0         Uniform adjustment to factor use tax by activity
 DTYF0        Partial direct tax on factor rate scaling factor
 DTYH0        Partial direct tax on household rate scaling factor
 DTY0         Partial direct tax on h'hold and enterprise rate scaling factor
 DTYE0        Partial direct tax on enterprise rate scaling factor

* ## Initial tax revenues
 MTAX0         Tariff revenue
 ETAX0         Export tax revenue
 DTAX0         Direct Income tax revenue
 FYTAX0        Factor Income tax revenue
 ITAX0         Indirect tax revenue
 FTAX0         Factor use tax revenue
 STAX0         Sales tax revenue
 EXTAX0        Excise tax revenue
 FUETAX0       Fuel tax revenue

* ## Initial tax rates
 TE0(c)        Export subsidy rate
 TM0(c)        Tariff rate on commodity c
 TS0(c)        Sales tax rates
 TEX0(c)       Excise tax rates
 TYF0(f)       Factor Income tax rate
 TX0(a)        Indirect tax rate on activity a
 TF0(f,a)      Factor use tax rate
 TYH0(h)       Direct tax rate on household h
 TYE0(e)       Direct tax rate on enterprises

* ## base tax rates
 teb(c)        Export subsidy rate
 tmb(c)        Tariff rate on commodity c
 tsb(c)        Sales tax rates
 texb(c)       Excise tax rates
 tyfb(f)       Factor Income tax rate
 txb(a)        Indirect tax rate on activity a
 tfb(f,a)      Factor use tax rate
 tyhb(h)       Direct tax rate on household h
 tyeb(e)       Direct tax rate on enterprises

 tmr(c)        Real tariff rate

* ## absolute changes in base tax rates

 dabte(c)      Change in base export taxes on comm'y imported from region w
 dabtm(c)      Change in base tariff rates on comm'y imported from region w
 dabts(c)      Change in base sales tax rate
 dabtex(c)     Change in base excise tax rate
 dabtx(a)      Change in base indirect tax rate
 dabtf(f,a)    Change in base factor us tax rate on activities
 dabtyf(f)     Change in base direct tax rate on factors
 dabtyh(h)     Change in base direct tax rate on households
 dabtye(e)     Change in base direct tax rate on enterprises

* ## partial changes in tax rates

 te01(c)       0-1 par for potential flexing of export taxes on comm'ies
 tm01(c)       0-1 par for potential flexing of Tariff rates on comm'ies
 ts01(c)       0-1 par for potential flexing of sales tax rates
 tex01(c)      0-1 par for potential flexing of excise tax rates
 tx01(a)       0-1 par for potential flexing of indirect tax rates
 tf01(f,a)     0-1 par for potential flexing of factor use tax rates
 tyf01(f)      0-1 par for potential flexing of direct tax rates on factors
 tyh01(h)      0-1 par for potential flexing of direct tax rates on h'holds
 tye01(e)      0-1 par for potential flexing of direct tax rates on e'rises

  ;

* #### Assignments

* ## Initial values for scaling variables

 TEADJ0    = 1 ;
 TMADJ0    = 1 ;
 TYFADJ0   = 1 ;
 TSADJ0    = 1 ;
 TEXADJ0   = 1 ;
 TXADJ0    = 1 ;
 TFADJ0    = 1.0 ;
 TYHADJ0   = 1 ;
 TYADJ0    = 1 ;
 TYEADJ0   = 1 ;

* ## Initial values for partial scaling variables
 DTE0      = 0.0 ;
 DTM0      = 0.0 ;
 DTS0      = 0.0 ;
 DTEX0     = 0.0 ;
 DTX0      = 0.0 ;
 DTF0      = 0.0 ;
 DTYF0     = 0.0 ;
 DTYH0     = 0.0 ;
 DTY0      = 0.0 ;
 DTYE0     = 0.0 ;

* ## Initial tax revenues

 MTAX0       = SUM(c,SAM("IMPTAX",c)) ;
 STAX0       = SUM(c,SAM("SALTAX",c)) ;
 EXTAX0      = SUM(c,SAM("ECTAX",c)) ;
 ETAX0       = SUM(c,SAM("EXPTAX",c)) ;
 ITAX0       = SUM(a,SAM("INDTAX",a)) ;
 FTAX0       = SUM((f,a),SUM(tff$map_f_tff(f,tff),SAM(tff,a))) ;
 FYTAX0      = SUM(f,SAM("FACTTAX",f)) ;
 DTAX0       = SUM(h,SAM("DIRTAX",h)) + SUM(e,SAM("DIRTAX",e)) ;

* ## initial tax rates

 TM0(c)$cm(c) = SAM("IMPTAX",c)/SAM("ROW",c) ;
 TE0(c)$ce(c) = SAM("EXPTAX",c)/SAM(c,"ROW") ;

 TS0(c)$(SUM(a,SAM(c,a)) + SUM(h,SAM(c,h)) + SUM(e,SAM(c,e))+ SUM(g,SAM(c,g))
         + SAM(c,"KAP") + SAM(c,"DSTOC")
         - SAM("SALTAX",c) - SAM("ECTAX",c) )
              = SAM("SALTAX",c)/(SUM(a,SAM(c,a))
                                + SUM(h,SAM(c,h))
                                + SUM(e,SAM(c,e))
                                + SUM(g,SAM(c,g))
                                + SAM(c,"KAP")
                                + SAM(c,"DSTOC")
                                   - SAM("SALTAX",c)
                                   - SAM("ECTAX",c) ) ;

 TEX0(c)$(SUM(a,SAM(c,a)) + SUM(h,SAM(c,h)) + SUM(e,SAM(c,e))+ SUM(g,SAM(c,g))
         + SAM(c,"KAP") + SAM(c,"DSTOC")
         - SAM("SALTAX",c) - SAM("ECTAX",c) )
              = SAM("ECTAX",c)/(SUM(a,SAM(c,a))
                                + SUM(h,SAM(c,h))
                                + SUM(e,SAM(c,e))
                                + SUM(g,SAM(c,g))
                                + SAM(c,"KAP")
                                + SAM(c,"DSTOC")
                                   - SAM("SALTAX",c)
                                   - SAM("ECTAX",c) ) ;

 TYF0(f)$(SAM("TOTAL",f) - SAM("KAP",f))
              = SAM("FACTTAX",f)/(SAM("TOTAL",f) - SAM("KAP",f)) ;

 TX0(a)$SAM("TOTAL",a)
              = SAM("INDTAX",a)/SAM("TOTAL",a) ;

 TF0(f,a)$SAM(f,a)
              = SUM(tff$map_f_tff(f,tff),SAM(tff,a))/SAM(f,a) ;

 TYH0(h)$SAM("TOTAL",h)
              = SAM("DIRTAX",h)/SAM("TOTAL",h) ;

 TYE0(e)$SAM("TOTAL",e)
              = SAM("DIRTAX",e)/SAM("TOTAL",e) ;

* ## base tax rates
 teb(c)       = TE0(c) ;
 tmb(c)       = TM0(c) ;
 tsb(c)       = TS0(c) ;
 texb(c)      = TEX0(c) ;
 tyfb(f)      = TYF0(f) ;
 txb(a)       = TX0(a) ;
 tfb(f,a)     = TF0(f,a) ;
 tyhb(h)      = TYH0(h) ;
 tyeb(e)      = TYE0(e) ;

 tmr(c)$cm(c) = tm0(c)*PWM0(c)*ER0 ;

* ## absolute changes in base tax rates

 dabte(c)     = 0.0 ;
 dabtm(c)     = 0.0 ;
 dabts(c)     = 0.0 ;
 dabtex(c)    = 0.0 ;
 dabtx(a)     = 0.0 ;
 dabtf(f,a)   = 0.0 ;
 dabtyf(f)    = 0.0 ;
 dabtyh(h)    = 0.0 ;
 dabtye(e)    = 0.0 ;

* ## partial changes in tax rates

 te01(c)      = 0.0 ;
 tm01(c)      = 0.0 ;
 ts01(c)      = 0.0 ;
 tex01(c)     = 0.0 ;
 tx01(a)      = 0.0 ;
 tf01(f,a)    = 0.0 ;
 tyf01(f)     = 0.0 ;
 tyh01(h)     = 0.0 ;
 tye01(e)     = 0.0 ;


* #### Government Income and Expenditure Block

PARAMETERS
* ## Initial values for scaling variables

 QGDADJ0       Government consumption demand scaling factor
 EGADJ0        Transfers to enterprises by government Scaling Factor
 HGADJ0        Scaling factor for government transfers to households

* ## Initial values for variables

 YG0           Government income

 QGD0(c)       Government consumption demand
 EG0           Expenditure by government
 VGD0          Value of Government consumption expenditure

* ## Transfer Expenditures

 hogovconst(h)  Transfers to hhold h from government (nominal but scalable)
 entgovconst(e) Government transfers to enterprise e

* ## Consumption Expenditures

 comgovconst(c) Government demand volume

  ;

* #### Assignments

* ## Initial values for scaling variables

 QGDADJ0   = 1 ;
 EGADJ0    = 1 ;
 HGADJ0    = 1 ;

* ## Initial values for variables

 YG0         = SAM("TOTAL","GOVT") ;

 QGD0(c)     = SAM(c,"GOVT")/PQD0(c) ;
 EG0         = SAM("TOTAL","GOVT") - SAM("KAP","GOVT") ;
 VGD0        = SUM(c,SAM(c,"GOVT")) ;

* ## Transfer Expenditures

 hogovconst(h)   = SAM(h,"GOVT")/CPI0 ;

 entgovconst(e)  = SAM(e,"GOVT")/CPI0 ;

* ## Consumption Expenditures

 comgovconst(c)  = SAM(c,"GOVT")/PQD0(c) ;

* ------- KAPITAL BLOCK --------------------------------------------------

PARAMETERS
* ## Initial values for scaling variables

 SADJ0         Savings rate scaling factor for BOTH households and enterprises
 SHADJ0        Savings rate scaling factor for households
 SEADJ0        Savings rate scaling factor for enterprises
 IADJ0         Investment scaling factor

 DSHH0         Partial household savings rate scaling factor
 DS0           Partial household and enterprise savings rate scaling factor
 DSEN0         Partial enterprise savings rate scaling factor

* ## Initial values for variables

 INVEST0       Total investment expenditure
 QINVD0(c)     Investment consumption by commodity c
 TOTSAV0       Total savings

* ## base and initial savings rates
 shh0(h)       Initial Household saving rates
 shhb(h)       Base Household saving rates

 sen0(e)       Initial Enterprise saving rates
 senb(e)       Base Enterprise saving rates

* ## absolute changes in base savings rates

 dabshh(h)     Change in base Household saving rates
 dabsen(e)     Change in base Enterprise saving rates

* ## partial changes in savings rates
 shh01(h)      0-1 par for potential flexing of Household saving rates
 sen01(e)    0-1 par for potential flexing of Enterprise saving rates

* ## Consumption Expenditures

 invconst(c)    Investment demand volume

 dstocconst(c)  Stock change demand volume

  ;

* #### Assignments

* ## Initial values for scaling variables

 SADJ0     = 1 ;
 SHADJ0    = 1 ;
 SEADJ0    = 1 ;
 IADJ0     = 1 ;

 DSHH0     = 0.0 ;
 DS0       = 0.0 ;
 DSEN0     = 0.0 ;

* ## Initial values for variables

 QINVD0(c)    = SAM(c,"KAP")/PQD0(c) ;
 INVEST0      = SAM("TOTAL","KAP") ;
 TOTSAV0      = SAM("KAP","TOTAL") ;

* ## initial savings rates

 shh0(h)$(SAM("TOTAL",h) - SAM("DIRTAX",h))
              = SAM("KAP",h)/(SAM("TOTAL",h) - SAM("DIRTAX",h)) ;

 sen0(e)$(YE0(e) - SAM("DIRTAX",e))
              = SAM("KAP",e)/(YE0(e) - SAM("DIRTAX",e)) ;

* ## base savings rates

 shhb(h)      = shh0(h) ;
 senb(e)      = sen0(e) ;

* ## absolute changes in base savings rates

 dabshh(h)    = 0.0 ;
 dabsen(e)    = 0.0 ;

* ## partial changes in savings rates

 shh01(h)     = 0.0 ;
 sen01(e)     = 0.0 ;

* ## Consumption Expenditures

 invconst(c)   = SAM(c,"KAP")/PQD0(c) ;

 dstocconst(c) = SAM(c,"DSTOC")/PQD0(c) ;

* ------- FOREIGN INSTITUTIONS BLOCK -------------------------------------

PARAMETERS

* ## Initial values for variables

 YFWOR0(f)     Foreign factor income

* ## Savings Expenditures

* ## Transfer Expenditures

 factwor(f)     Factor payments from RoW (constant in foreign currency)
 howor(h)       Transfers to household from world (constant in foreign currency)
 entwor(e)      Transfers to enterprise e from world (constant in foreign currency)
 govwor         Transfers to government from world (constant in foreign currency)

  ;

* #### Assignments

* ## Initial values for variables

 YFWOR0(f)    = SAM("ROW",f) ;

* ## Transfer Expenditures

 factwor(f)   = SAM(f,"ROW")/ER0 ;

 howor(h)        = SAM(h,"ROW") ;

 entwor(e)       = SAM(e,"ROW") ;

 govwor          = SAM("GOVT","ROW")/ER0 ;

* ------- MARKET CLEARING BLOCK ------------------------------------------

PARAMETERS
* ## Initial values for variables

* # Account Closure

 KAPGOV0       Government Savings
 CAPWOR0       Current account balance

* # Absorption Closure

 VFDOMD0       Value of final domestic demand
 INVESTSH0     Value share of investment in total final domestic demand
 VGDSH0        Value share of Govt consumption in total final domestic demand
 VEDSH0(e)     Value share of Ent consumption in total final domestic demand

* ##### GDP

 GDPVA0        GDP from Value Added

* # Slack

 WALRAS0       Slack variable for Walras's Law

  ;

* #### Assignments

* ## Initial values for variables
* # Account Closure

 KAPGOV0     = SAM("KAP","GOVT") ;
 CAPWOR0     = SAM("KAP","ROW") ;

* # Absorption Closure

 VFDOMD0     = SUM((c,h),SAM(c,h))
                  + SUM((c,e),SAM(c,e))
                  + SUM(c,SAM(c,"GOVT"))
                  + SUM(c,SAM(c,"KAP"))
                  + SUM(c,SAM(c,"dstoc")) ;

 INVESTSH0$VFDOMD0
             = (SUM(c,SAM(c,"KAP")) + SUM(c,SAM(c,"dstoc")))/VFDOMD0 ;

 VGDSH0$VFDOMD0
             = SUM(c,SAM(c,"GOVT"))/VFDOMD0 ;
 VEDSH0(e)$VFDOMD0
             = SUM(c,SAM(c,e))/VFDOMD0 ;

* ##### GDP

* GDPVA0      = SUM((f,a),SAM(f,a))
 GDPVA0      = SUM((f,a),WF0(f)*WFDIST0(f,a)*FD0(f,a))

               + MTAX0 + ETAX0 + STAX0 + EXTAX0 + ITAX0 + FTAX0 ;

* # Slack

 WALRAS0   = 0 ;


*#### Various Checks on Calibration

$INCLUDE stg1_calibchk.inc


*-------------------- end of stg1 model calibration -----------------------------
