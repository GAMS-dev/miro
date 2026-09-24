*-------- stg1 declare result parameters ---------------------------------
$ontext
 This section declares the result parameter that are used to store model
 results between experiments and sims.
 It is used by the result analysis code found in include files such as
 reslevel.gms, macrocal.gms, welfcal.gms, etc.
$offtext

* The definitions depend on expclos and sim sets.

PARAMETERS

* These parameter are used to report the results from the simulations

* A MicroSAM for each Experiment

 %res%SAM(sac,sacp,%res_dimen%)   Result SAMs
 TOTALSCHECKP                 sum of abs value of differences as fraction of SAM total

* Table of scalar results

 %res%SCAL(scalres,%res_dimen%)   table of scalar results

* Table of adjuster results

 %res%ADJ(adjres,%res_dimen%)     table of adjuster results

* ------- TRADE BLOCK ----------------------------------------------------
* #### Exports Block

 %res%PWE(c,%res_dimen%)       World price of exports in dollars
 %res%PE(c,%res_dimen%)        Domestic price of exports by activity a
 %res%PD(c,%res_dimen%)        Consumer price for domestic supply of commodity c
 %res%QE(c,%res_dimen%)        Domestic output exported by commodity c
 %res%QD(c,%res_dimen%)        Domestic demand for commodity c

* #### Imports Block

 %res%PWM(c,%res_dimen%)       World price of imports in dollars
 %res%PM(c,%res_dimen%)        Domestic price of competitive imports of commodity c
 %res%QM(c,%res_dimen%)        Imports of commodity c
 %res%QQ(c,%res_dimen%)        Supply of composite commodity c

* ------- COMMODITY PRICE BLOCK ------------------------------------------

 %res%PQS(c,%res_dimen%)       Supply price of composite commodity c
 %res%PQD(c,%res_dimen%)       Purchaser price of composite commodity c

* ------- PRODUCTION BLOCK -----------------------------------------------

 %res%PX(a,%res_dimen%)        Composite price of output by activity a
 %res%PVA(a,%res_dimen%)       Value added price for activity a
 %res%PINT(a,%res_dimen%)      Price of aggregate intermediate input

 %res%ADX(a,%res_dimen%)       Shift parameter for CES production functions for QX
 %res%ADFD(f,a,%res_dimen%)    Shift parameter for factor and activity specific efficiency
 %res%QX(a,%res_dimen%)        Domestic production by activity a

 %res%QVA(a,%res_dimen%)       Quantity of aggregate value added for level 1 production
 %res%QINT(a,%res_dimen%)      Aggregate quantity of intermediates used by activity a
 %res%ADVA(a,%res_dimen%)      Shift parameter for CES production functions for QVA

 %res%WF(f,%res_dimen%)        Price of factor f
 %res%WFDIST(f,a,%res_dimen%)  Sectoral proportion for factor prices
 %res%FD(f,a,%res_dimen%)      Demand for factor f by activity a
 %res%FS(f,%res_dimen%)        Supply of factor f

 %res%QINTD(c,%res_dimen%)     Demand for intermediate inputs by commodity

 %res%PXC(c,%res_dimen%)       Producer price of composite domestic output
 %res%QXC(c,%res_dimen%)       Domestic production by commodity c

 %res%PXAC(a,c,%res_dimen%)    Activity commodity prices
 %res%QXAC(a,c,%res_dimen%)    Domestic commodity output by each activity

* ------- FACTOR BLOCK ---------------------------------------------------

 %res%YF(f,%res_dimen%)        Income to factor f
 %res%YFDISP(f,%res_dimen%)    Factor income for distribution after depreciation

* ------- HOUSEHOLD BLOCK ------------------------------------------------
* ## Household Income

 %res%YH(h,%res_dimen%)        Income to household h

* Household Expenditure

 %res%HOHO(h,hp,%res_dimen%)   Inter household transfer
 %res%HEXP(h,%res_dimen%)      Household consumption expenditure
 %res%QCD(c,h,%res_dimen%)     Household consumption by commodity c

* ------- ENTERPRISE BLOCK -----------------------------------------------
* ## Enterprise Income

 %res%YE(e,%res_dimen%)        Enterprise incomes

* ## Enterprise Expenditure

 %res%HOENT(h,e,%res_dimen%)   Household Income from enterprise e
 %res%GOVENT(e,%res_dimen%)    Government income from enterprise e
 %res%QED(c,e,%res_dimen%)     Enterprise consumption by commodity c
 %res%VED(e,%res_dimen%)       Value of enterprise e consumption expenditure

* ------- GOVERNMENT BLOCK -----------------------------------------------

* #### Government Income Block

 %res%TE(c,%res_dimen%)        Export taxes on exported comm'y c
 %res%TM(c,%res_dimen%)        Tariff rates on imported comm'y c
 %res%TS(c,%res_dimen%)        Sales tax rate
 %res%TEX(c,%res_dimen%)       Excise tax rate
 %res%TX(a,%res_dimen%)        Indirect tax rate
 %res%TF(f,a,%res_dimen%)      Tax rate on factor use
 %res%TYF(f,%res_dimen%)       Direct tax rate on factor income
 %res%TYH(h,%res_dimen%)       Direct tax rate on households
 %res%TYE(e,%res_dimen%)       Direct tax rate on enterprises

* #### Government Expenditure Block

 %res%QGD(c,%res_dimen%)       Government consumption demand by commodity c

* ------- KAPITAL BLOCK --------------------------------------------------

* ## Savings Block

 %res%SHH(h,%res_dimen%)       Household savings rates
 %res%SEN(e,%res_dimen%)       Enterprise savings rates

* ## Investment Block

 %res%QINVD(c,%res_dimen%)     Investment demand by commodity c

* ------- FOREIGN INSTITUTIONS BLOCK -------------------------------------

 %res%YFWOR(f,%res_dimen%)     Foreign factor income

* ------- MARKET CLEARING BLOCK ------------------------------------------
* #### Absorption Closure

 %res%VEDSH(e,%res_dimen%)     Value share of Ent consumption in total final domestic demand

 ;

*-------- end of stg1 declare result parameters --------------------------
