*------------ smod parameter and variable listing ------------------------
$ontext
  This file provides lists of the parameters, parameters for initial values
of variables and variables
         * ------- 1a. MODEL PARAMETERS
         * ------- 1b. PARAMETERS FOR INITIAL VALUES OF VARIABLES
         * ------- 1c. MODEL VARIABLES
$offtext

* ------- 1a. MODEL PARAMETERS
$ontext

 ac(c)          Shift parameter for Armington CES function
 advab(a)       Shift parameter for CES production functions for QVA
 adva01(a)      0-1 par for flexing of shift parameter on functions for QVA
 adxb(a)        Shift parameter for CES production functions for QX
 adx01(a)       0-1 par for flexing of shift parameter on functions for QX
 adxc(c)        Shift parameter for commodity output CES aggregation
 alphah(c,h)    Expenditure share by commodity c for household h
 at(c)          Shift parameter for Armington CET function
 beta(c,h)      Marginal budget shares
 comactco(c,a)  use matrix coefficients
 comgovconst(c) Government demand volume
 comhoav(c,h)   Household consumption shares
 comtotsh(c)    Share of commodity c in total commodity demand
 dabadva(a)     Change in base shift parameter on functions for QVA
 dabadx(a)      Change in base shift parameter on functions for QX
 dabsen(e)     Change in base Enterprise saving rates
 dabshh(h)     Change in base Household saving rates
 dabte(c)      Change in base export taxes on comm'y imported from region w
 dabtex(c)     Change in base excise tax rate
 dabtf(f,a)    Change in base factor us tax rate on activities
 dabtm(c)      Change in base tariff rates on comm'y imported from region w
 dabts(c)      Change in base sales tax rate
 dabtx(a)      Change in base indirect tax rate
 dabtye(e)     Change in base direct tax rate on enterprises
 dabtyf(f)     Change in base direct tax rate on factors
 dabtyh(h)     Change in base direct tax rate on households
 delta(c)       Share parameter for Armington CES function
 deltava(f,a)   Share parameters for CES production functions for QVA
 deltax(a)      Share parameter for CES production functions for QX
 deltaxc(a,c)   Share parameters for commodity output CES aggregation
 deprec(f)      depreciation rate by factor f
 dstocconst(c)  Stock change demand volume
 econ(c)        constant for export demand equations
 entgovconst(e) Government transfers to enterprise e
 entvash(e,f)   Share of income from factor f to enterprise e
 entwor(e)      Transfers to enterprise e from world (constant in foreign currency)
 eta(c)         export demand elasticity
 factwor(f)     Factor payments from RoW (constant in foreign currency)
 frisch(h)      Elasticity of the marginal utility of income
 gamma(c)       Share parameter for Armington CET function
 goventsh(e)    Share of entp' income after tax save and consump to govt
 govvash(f)     Share of income from factor f to government
 govwor         Transfers to government from world (constant in foreign currency)
 hexps(h)       Subsistence consumption expenditure
 hoentconst(h,e)  transfers to hhold h from enterprise e (nominal)
 hoentsh(h,e)   Share of entp' income after tax save and consump to h'hold
 hogovconst(h)  Transfers to hhold h from government (nominal but scalable)
 HOHO0(h,hp)    Inter household transfer
 hohoconst(h,hp) interhousehold transfers
 hohosh(h,hp)   Share of h'hold h after tax and saving income transferred to hp
 hovash(h,f)    Share of income from factor f to household h
 howor(h)       Transfers to household from world (constant in foreign currency)
 invconst(c)    Investment demand volume
 ioqintqx(a)    Agg intermed quantity per unit QX for Level 1 Leontief agg
 ioqtdqd(c,a)   intermediate input output coefficients
 ioqvaqx(a)     Agg value added quant per unit QX for Level 1 Leontief agg
 ioqxacqx(a,c)    Share of commodity c in output by activity a
 predeltax(a)   dummy used to estimated deltax
 pwse(c)        world price of export substitutes
 qcdconst(c,h)  Volume of subsistence consumption
 qedconst(c,e) Enterprise demand volume
 rhoc(c)        Elasticity parameter for Armington CES function
 rhocva(a)      Elasticity parameter for CES production function for QVA
 rhocx(a)       Elasticity parameter for CES production function for QX
 rhocxc(c)      Elasticity parameter for commodity output CES aggregation
 rhot(c)        Elasticity parameter for Output Armington CET function
 sen0(e)       Initial Enterprise saving rates
 sen01(e)    0-1 par for potential flexing of Enterprise saving rates
 senb(e)       Base Enterprise saving rates
 shh0(h)       Initial Household saving rates
 shh01(h)      0-1 par for potential flexing of Household saving rates
 shhb(h)       Base Household saving rates
 sumelast(h)    Weighted sum of income elasticities
 te01(c)       0-1 par for potential flexing of export taxes on comm'ies
 teb(c)        Export subsidy rate
 tex01(c)      0-1 par for potential flexing of excise tax rates
 texb(c)       Excise tax rates
 tf01(f,a)     0-1 par for potential flexing of factor use tax rates
 tfb(f,a)      Factor use tax rate
 tm01(c)       0-1 par for potential flexing of Tariff rates on comm'ies
 tmb(c)        Tariff rate on commodity c
 tmr(c)        Real tariff rate
 ts01(c)       0-1 par for potential flexing of sales tax rates
 tsb(c)        Sales tax rates
 tx01(a)       0-1 par for potential flexing of indirect tax rates
 txb(a)        Indirect tax rate on activity a
 tye01(e)      0-1 par for potential flexing of direct tax rates on e'rises
 tyeb(e)       Direct tax rate on enterprises
 tyf01(f)      0-1 par for potential flexing of direct tax rates on factors
 tyfb(f)       Factor Income tax rate
 tyh01(h)      0-1 par for potential flexing of direct tax rates on h'holds
 tyhb(h)       Direct tax rate on household h
 use(c,a)       use matrix transactions
 vddtotsh(c)    Share of value of domestic output for the domestic market
 worvash(f)     Share of income from factor f to RoW
 yhelast(c,h)   (Normalised) household income elasticities

$oftext
* ------- 1b. PARAMETERS FOR INITIAL VALUES OF VARIABLES
$ontext

 ADFD0(f,a)    Shift parameter for factor and activity specific efficiency
 ADVAADJ0      Scaling Factor for Shift parameter on CES functions for QVA
 ADVA0(a)       Shift parameter for CES production functions for QVA
 ADXADJ0       Scaling Factor for Shift parameter on CES functions for QX
 ADX0(a)        Shift parameter for CES production functions for QX
 CAPWOR0       Current account balance
 CPI0          Consumer price index
 DADVA0        Partial scaling factor for Shift parameter on CES functions for QVA
 DADX0         Partial scaling factor for Shift parameter on CES functions for QX
 DS0           Partial household and enterprise savings rate scaling factor
 DSEN0         Partial enterprise savings rate scaling factor
 DSHH0         Partial household savings rate scaling factor
 DTAX0         Direct Income tax revenue
 DTE0         Partial Export tax rate scaling factor
 DTEX0        Partial Excise tax rate scaling factor
 DTF0         Uniform adjustment to factor use tax by activity
 DTM0         Partial Tariff rate scaling factor
 DTS0         Partial Sales tax rate scaling factor
 DTX0         Partial Indirect tax rate scaling factor
 DTY0         Partial direct tax on h'hold and enterprise rate scaling factor
 DTYE0        Partial direct tax on enterprise rate scaling factor
 DTYF0        Partial direct tax on factor rate scaling factor
 DTYH0        Partial direct tax on household rate scaling factor
 EG0           Expenditure by government
 EGADJ0        Transfers to enterprises by government Scaling Factor
 ER0           Exchange rate                         (domestic per world unit)
 ETAX0         Export tax revenue
 EXTAX0        Excise tax revenue
 FD0(f,a)      Demand for factor f by activity a
 FS0(f)        Supply of factor f
 FTAX0         Factor use tax revenue
 FUETAX0       Fuel tax revenue
 FYTAX0        Factor Income tax revenue
 GDPVA0        GDP from Value Added
 GOVENT0(e)    Government income from enterprise e
 GOVENTSH0     Share of entp' income after tax save and consump to govt
 HEADJ0        Scaling factor for enterprise transfers to households
 HEXP_cap0(h)   Household consumption expenditure per capita adult equiv
 HEXP0(h)       Household consumption expenditure
 HGADJ0        Scaling factor for government transfers to households
 HOENT0(h,e)   Household Income from enterprise e
 HOHO0(h,hp)    Inter household transfer
 IADJ0         Investment scaling factor
 INVEST0       Total investment expenditure
 INVESTSH0     Value share of investment in total final domestic demand
 ITAX0         Indirect tax revenue
 KAPGOV0       Government Savings
 MTAX0         Tariff revenue
 PD0(c)        Consumer price for domestic supply of commodity c
 PE0(c)        Domestic price of exports by commodity c
 PINT0(a)      Price of aggregate intermediate input
 PM0(c)        Domestic price of competitive imports c
 PPI0          Producer (domestic) price index
 PQD0(c)       Purchaser price of composite commodity c
 PQS0(c)       Supply price of composite commodity c
 PVA0(a)       Value added price for activity a
 PWE0(c)       World price of exports in dollars
 PWM0(c)       World price of imports in dollars
 PX0(a)        Composite price of output by activity a
 PXAC0(a,c)    Activity commodity prices
 PXC0(c)       Producer price of composite domestic output
 QCD0(c,h)      Household consumption by commodity c
 QD0(c)        Domestic demand for commodity c
 QE0(c)        Domestic output exported by commodity c
 QED0(c,e)     Enterprise consumption by commodity c
 QEDADJ0       Enterprise demand volume Scaling Factor
 QGD0(c)       Government consumption demand
 QGDADJ0       Government consumption demand scaling factor
 QINT0(a)      Aggregate quantity of intermediates used by activity a
 QINTD0(c)     Demand for intermediate inputs by commodity
 QINVD0(c)     Investment consumption by commodity c
 QM0(c)        Imports of commodity c
 QQ0(c)        Supply of composite commodity c
 QVA0(a)       Quantity of aggregate value added for level 1 production
 QX0(a)        Domestic production by activity a
 QXAC0(a,c)    Domestic commodity output by each activity
 QXC0(c)       Domestic production by commodity c
 SADJ0         Savings rate scaling factor for BOTH households and enterprises
 SEADJ0        Savings rate scaling factor for enterprises
 SHADJ0        Savings rate scaling factor for households
 STAX0         Sales tax revenue
 TE0(c)        Export subsidy rate
 TEADJ0        Export subsidy Scaling Factor
 TEX0(c)       Excise tax rates
 TEXADJ0       Excise tax rate scaling factor
 TF0(f,a)      Factor use tax rate
 TFADJ0        Factor Use Tax Scaling Factor
 TM0(c)        Tariff rate on commodity c
 TMADJ0        Tarrif rate Scaling Factor
 TOTSAV0       Total savings
 TS0(c)        Sales tax rates
 TSADJ0        Sales tax rate scaling factor
 TX0(a)        Indirect tax rate on activity a
 TXADJ0        Indirect Tax Scaling Factor
 TYADJ0        Household and Enterprise Income Tax Scaling Factor
 TYE0(e)       Direct tax rate on enterprises
 TYEADJ0       Enterprise income tax Scaling Factor
 TYF0(f)       Factor Income tax rate
 TYFADJ0       Factor Tax Scaling Factor
 TYH0(h)       Direct tax rate on household h
 TYHADJ0       Household Income Tax Scaling Factor
 VED0(e)       Value of enterprise consumption expenditure
 VEDSH0(e)     Value share of Ent consumption in total final domestic demand
 VFDOMD0       Value of final domestic demand
 VGD0          Value of Government consumption expenditure
 VGDSH0        Value share of Govt consumption in total final domestic demand
 WALRAS0       Slack variable for Walras's Law
 WF0(f)        Price of factor f
 WFDIST0(f,a)  Sectoral proportion for factor prices
 YE0(e)        Enterprise income
 YF0(f)        Income to factor f
 YFDISP0(f)    Factor income for distribution after depreciation
 YFWOR0(f)     Foreign factor income
 YG0           Government income
 YH_cap0(h)     Income per capita adult equiv to household h
 YH0(h)         Income to household h

$offtext
* ------- 1c. MODEL VARIABLES
$ontext

 ADFD(f,a)    Shift parameter for factor and activity specific efficiency
 ADVA(a)      Shift parameter for CES production functions for QVA
 ADVAADJ      Scaling Factor for Shift parameter on CES functions for QVA
 ADX(a)       Shift parameter for CES production functions for QX
 ADXADJ       Scaling Factor for Shift parameter on CES functions for QX
 CAPWOR       Current account balance
 CPI          Consumer price index
 DADVA        Partial scaling factor for Shift parameter on CES functions for QVA
 DADX         Partial scaling factor for Shift parameter on CES functions for QX
 DS           Partial household and enterprise savings rate scaling factor
 DSEN         Partial enterprise savings rate scaling factor
 DSHH         Partial household savings rate scaling factor
 DTAX         Direct Income tax revenue
 DTE          Partial Export tax rate scaling factor
 DTEX         Partial Excise tax rate scaling factor
 DTF          Uniform adjustment to factor use tax by activity
 DTM          Partial Tariff rate scaling factor
 DTS          Partial Sales tax rate scaling factor
 DTX          Partial Indirect tax rate scaling factor
 DTY          Partial direct tax on h'hold and enterprise rate scaling factor
 DTYE         Partial direct tax on enterprise rate scaling factor
 DTYF         Partial direct tax on factor rate scaling factor
 DTYH         Partial direct tax on household rate scaling factor
 EG           Expenditure by government
 EGADJ        Transfers to enterprises by government Scaling Factor
 ER           Exchange rate                         (domestic per world unit)
 ETAX         Export tax revenue
 EXTAX        Excise tax revenue
 FD(f,a)      Demand for factor f by activity a
 FS(f)        Supply of factor f
 FTAX         Factor use tax revenue
 FYTAX        Factor Income tax revenue
 GDPVA        GDP from Value Added
 GOVENT(e)    Government income from enterprise e
 HEADJ        Scaling factor for enterprise transfers to households
 HEXP(h)      Household consumption expenditure
 HGADJ        Scaling factor for government transfers to households
 HOENT(h,e)   Household Income from enterprise e
 HOHO(h,hp)   Inter household transfer
 IADJ         Investment scaling factor
 INVEST       Total investment expenditure
 INVESTSH     Value share of investment in total final domestic demand
 ITAX         Indirect tax revenue
 KAPGOV       Government Savings
 MTAX         Tariff revenue
 PD(c)        Consumer price for domestic supply of commodity c
 PE(c)        Domestic price of exports by activity a
 PINT(a)      Price of aggregate intermediate input
 PM(c)        Domestic price of competitive imports of commodity c
 PPI          Producer (domestic) price index
 PQD(c)       Purchaser price of composite commodity c
 PQS(c)       Supply price of composite commodity c
 PVA(a)       Value added price for activity a
 PWE(c)       World price of exports in dollars
 PWM(c)       World price of imports in dollars
 PX(a)        Composite price of output by activity a
 PXAC(a,c)    Activity commodity prices
 PXC(c)       Producer price of composite domestic output
 QCD(c,h)     Household consumption by commodity c
 QD(c)        Domestic demand for commodity c
 QE(c)        Domestic output exported by commodity c
 QED(c,e)     Enterprise consumption by commodity c
 QEDADJ       Enterprise demand volume Scaling Factor
 QGD(c)       Government consumption demand by commodity c
 QGDADJ       Government consumption demand scaling factor
 QINT(a)      Aggregate quantity of intermediates used by activity a
 QINTD(c)     Demand for intermediate inputs by commodity
 QINVD(c)     Investment demand by commodity c
 QM(c)        Imports of commodity c
 QQ(c)        Supply of composite commodity c
 QVA(a)       Quantity of aggregate value added for level 1 production
 QX(a)        Domestic production by activity a
 QXAC(a,c)    Domestic commodity output by each activity
 QXC(c)       Domestic production by commodity c
 SADJ         Savings rate scaling factor for BOTH households and enterprises
 SEADJ        Savings rate scaling factor for enterprises
 SEN(e)       Enterprise savings rates
 SHADJ        Savings rate scaling factor for households
 SHH(h)       Household savings rates
 STAX         Sales tax revenue
 TE(c)        Export taxes on exported comm'y c
 TEADJ        Export subsidy Scaling Factor
 TEX(c)       Excise tax rate
 TEXADJ       Excise tax rate scaling factor
 TF(f,a)      Tax rate on factor use
 TFADJ        Factor Use Tax Scaling Factor
 TM(c)        Tariff rates on imported comm'y c
 TMADJ        Tarrif rate Scaling Factor
 TOTSAV       Total savings
 TS(c)        Sales tax rate
 TSADJ        Sales tax rate scaling factor
 TX(a)        Indirect tax rate
 TXADJ        Indirect Tax Scaling Factor
 TYADJ        Household and Enterprise Income Tax Scaling Factor
 TYE(e)       Direct tax rate on enterprises
 TYEADJ       Enterprise income tax Scaling Factor
 TYF(f)       Direct tax rate on factor income
 TYFADJ       Factor Tax Scaling Factor
 TYH(h)       Direct tax rate on households
 TYHADJ       Household Income Tax Scaling Factor
 VED(e)       Value of enterprise e consumption expenditure
 VEDSH(e)     Value share of Ent consumption in total final domestic demand
 VFDOMD       Value of final domestic demand
 VGD          Value of Government consumption expenditure
 VGDSH        Value share of Govt consumption in total final domestic demand
 WALRAS       Slack variable for Walras's Law
 WF(f)        Price of factor f
 WFDIST(f,a)  Sectoral proportion for factor prices
 YE(e)        Enterprise incomes
 YF(f)        Income to factor f
 YFDISP(f)    Factor income for distribution after depreciation
 YFWOR(f)     Foreign factor income
 YG           Government income
 YH(h)        Income to household h

$offtext

*------------ end of smod parameter and variable listing -----------------
