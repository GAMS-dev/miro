*--------------------- smod variable initialisation ----------------------

*## INITIAL VALUES OF VARIABLES FROM PARAMETER SPECIFICATION

* ------- NORMALISED PRICE BLOCK -----------------------------------------

 PD.L(c)      = PD0(c) ;

 PM.L(c)      = PM0(c) ;
 PE.L(c)      = PE0(c) ;

 PQS.L(c)     = PQS0(c) ;

 PXC.L(c)     = PXC0(c) ;
 PXAC.L(a,c)  = PXAC0(a,c) ;
 PX.L(a)      = PX0(a) ;

 ER.L         = ER0 ;

* ------- TRADE BLOCK ---------------------------------------------------

* #### Exports Block

 PWE.L(c)     = PWE0(c) ;

 QD.L(c)      = (SUM(a,SAM(a,c)/PD.L(c))) - QE0(c) ;

 QE.L(ce)     = QD.L(ce)*((PE.L(ce)/PD.L(ce))*((1-gamma(ce))
                                 /gamma(ce)))**(1/(rhot(ce)-1)) ;

* #### Imports Block

 PWM.L(c)     = PWM0(c) ;

 QQ.L(c)      = QQ0(c) ;

 QM.L(c)$(cd(c) AND cm(c))
              = QD.L(c)*((PD.L(c)/PM.L(c))
                        *(delta(c)/(1-delta(c))))**(1/(1+rhoc(c))) ;

 QM.L(c)$(cdn(c) AND cm(c))
              = QQ.L(c) ;

* ------- COMMODITY PRICE BLOCK ------------------------------------------

 PQD.L(c)     = PQS0(c) * (1 + ts0(c) + tex0(c)) ;

* ------- NUMERAIRE PRICE BLOCK ------------------------------------------

 CPI.L        = CPI0 ;
 PPI.L        = PPI0 ;

* ------- PRODUCTION BLOCK -----------------------------------------------


* CES aggregation functions for Level 1 of production nest

 PVA.L(a)     = PVA0(a) ;

 PINT.L(a)    = SUM(c,ioqtdqd(c,a) * PQD.L(c)) ;

 QX.L(a)      = SAM("TOTAL",a)/PX.L(a) ;

* Leontief aggregation functions for Level 1 of production nest

 ADXADJ.L     = ADXADJ0 ;

 DADX.L       = DADX0 ;

 ADX.L(a)     = ((adxb(a) + dabadx(a)) * ADXADJ.L)
                                      + (DADX.L * adx01(a)) ;

 QINT.L(a)    = SUM(c,SAM(c,a)/PQD.L(c)) ;

 QVA.L(a)     = QVA0(a) ;

* CES aggregation functions for Level 2 of production nest

 ADVAADJ.L    = ADVAADJ0 ;

 DADVA.L      = DADVA0 ;

 ADVA.L(a)    = ((advab(a) + dabadva(a)) * ADVAADJ.L)
                                      + (DADVA.L * adva01(a)) ;

 ADFD.L(f,a)  = ADFD0(f,a) ;

 FD.L(f,a)    = FD0(f,a) ;

 FS.L(f)      = SUM(a,FD.L(f,a)) ;

 WF.L(f)      = SUM(a,SAM(f,a))/FS.L(f) ;
 WFDIST.L(f,a)$FD.L(f,a)
              = (SAM(f,a)/FD.L(f,a))/WF.L(f) ;
 WFDIST.L(f,a)$(FD.L(f,a) EQ 0)
              = 0.0 ;

* Intermediate Input Demand

 QINTD.L(c)   = SUM(a,comactco(c,a)*QX.L(a)) ;

* Commodity Output

 QXC.L(c)     = adxc(c)*(SUM(a$deltaxc(a,c),deltaxc(a,c)
                             *QXAC0(a,c)**(-rhocxc(c))))**(-1/rhocxc(c)) ;

* Activity Output

 QXAC.L(a,c)  = ioqxacqx(a,c) * QX.L(a) ;

* ------- FACTOR BLOCK ---------------------------------------------------

 YF.L(f)      = SUM(a,WF.L(f)*WFDIST.L(f,a)*FD.L(f,a))
                             + (factwor(f)*ER.L) ;

 YFDISP.L(f)  = (YF.L(f) * (1- deprec(f)))*(1- tYf0(f)) ;

* ------- HOUSEHOLD BLOCK ------------------------------------------------
* ## Household Income

 YH.L(h)      = SUM(f,hovash(h,f)*YFDISP.L(f))
                             + SUM(hp,HOHO0(h,hp))
                             + SUM(e,HEADJ0*hoentconst(h,e) * CPI.L)
                             + (HGADJ0*hogovconst(h) * CPI.L)
                             + (howor(h)*ER.L) ;

* ## Household Expenditure

 HOHO.L(h,hp) = hohosh(h,hp)
                *((YH.L(h) * (1 - tyh0(h))) * (1 - shh0(h))) ;

 HEXP.L(h)   = ((YH.L(h) * (1 - tyh0(h))) * (1 - shh0(h)))
                    - SUM(hp,HOHO.L(hp,h)) ;

* QCD.L(c)     = (SUM(h,PQD.L(c)*qcdconst(c,h)) + SUM(h,beta(c,h)
*                    *(HEXP.L(h)-SUM(cp,PQD.L(cp)*qcdconst(cp,h)))))/PQD.L(c) ;

 QCD.L(c,h)   = (comhoav(c,h)*HEXP.L(h))/PQD.L(c) ;


* ######## ENTERPRISE BLOCK
* ## Enterprise Income

 YE.L(e)      = SUM(f,entvash(e,f)*YFDISP.L(f))
                             + (EGADJ0*entgovconst(e)*CPI.L)
                             + (entwor(e)*ER.L) ;

* ## Enterprise Expenditure

 QEDADJ.L   = QEDADJ0 ;

 QED.L(c,e) = QEDADJ.L*qedconst(c,e) ;

 HOENT.L(h,e) = hoentsh(h,e)
                 * (((YE.L(e) * (1 - tye0(e))) * (1 - sen0(e)))
                    - SUM(c,QED.L(c,e)*PQD.L(c))) ;

 GOVENT.L(e)  = goventsh(e)
                 * (((YE.L(e) * (1 - tye0(e))) * (1 - sen0(e)))
                    - SUM(c,QED.L(c,e)*PQD.L(c))) ;

 VED.L(e)      = SUM(c,QED.L(c,e)*PQD.L(c)) ;

* ------- GOVERNMENT BLOCK -----------------------------------------------

* #### Government Income Block

* ## Government Taxes

 TEADJ.L      = TEADJ0 ;
 TMADJ.L      = TMADJ0 ;
 TSADJ.L      = TSADJ0 ;
 TEXADJ.L     = TEXADJ0 ;
 TXADJ.L      = TXADJ0 ;
 TFADJ.L      = TFADJ0 ;
 TYFADJ.L     = TYFADJ0 ;
 TYHADJ.L     = TYHADJ0 ;
 TYADJ.L      = TYADJ0 ;
 TYEADJ.L     = TYEADJ0 ;

 DTE.L        = DTE0 ;
 DTM.L        = DTM0 ;
 DTS.L        = DTS0 ;
 DTEX.L       = DTEX0 ;
 DTX.L        = DTX0 ;
 DTF.L        = DTF0 ;
 DTYF.L       = DTYF0 ;
 DTYH.L       = DTYH0 ;
 DTY.L        = DTY0 ;
 DTYE.L       = DTYE0 ;

 TM.L(c)      = ((tmb(c) + dabtm(c))* TMADJ.L)
                       + (DTM.L*tm01(c)*tm0(c)) ;

 TE.L(c)      = ((teb(c) + dabte(c))* TEADJ.L)
                       + (DTE.L*te01(c)*te0(c)) ;

 TS.L(c)      = ((tsb(c) + dabts(c))* TSADJ.L)
                         + (DTS.L*ts01(c)*ts0(c)) ;

 TEX.L(c)     = ((texb(c) + dabtex(c))* TEXADJ.L)
                         + (DTEX.L*tex01(c)*tex0(c)) ;

 TX.L(a)      = ((txb(a) + dabtx(a))* TXADJ.L)
                         + (DTX.L*tx01(a)*tx0(a)) ;

 TF.L(f,a)    = ((tfb(f,a) + dabtf(f,a))* TFADJ.L)
                         + (DTF.L*tf01(f,a)) ;

 TYF.L(f)     = ((tyfb(f) + dabtyf(f))* TYFADJ.L)
                      + (DTYF.L*tyf01(f)*tyf0(f)) ;

 TYH.L(h)     = ((tyhb(h) + dabtyh(h))* TYHADJ.L * TYADJ.L)
                      + (DTYH.L*DTY.L*tyh01(h)*tyh0(h)) ;

 TYE.L(e)     = ((tyeb(e) + dabtye(e))* TYEADJ.L * TYADJ.L)
                        + (DTYE.L*DTY.L*tye01(e)*tye0(e)) ;

 MTAX.L       = SUM(c,TM.L(c)*PWM.L(c)*ER.L*QM.L(c)) ;

 ETAX.L       = SUM(c,TE.L(c)*PWE.L(c)*ER.L*QE.L(c)) ;

 STAX.L       = SUM(c,TS.L(c)*PQS.L(c)*QQ.L(c)) ;

 EXTAX.L      = SUM(c,TEX.L(c)*PQS.L(c)*QQ.L(c)) ;

 ITAX.L       = SUM(a,TX.L(a)*PX.L(a)*QX.L(a)) ;

 FTAX.L       = SUM((f,a), TF.L(f,a)*WF.L(f)*WFDIST.L(f,a)*FD.L(f,a)) ;

 FYTAX.L      = SUM(f,TYF.L(f)*(YF.L(f) * (1- deprec(f)))) ;

 DTAX.L       = SUM(h,TYH.L(h)*YH.L(h))
                 + SUM(e,TYE.L(e)*YE.L(e)) ;

* ## Government Income

 YG.L         = MTAX.L + ETAX.L
                + STAX.L + EXTAX.L + ITAX.L + FTAX.L
                + FYTAX.L + DTAX.L
                 + SUM(f,govvash(f)*YFDISP.L(f))
                 + SUM(e,GOVENT0(e)) + (govwor*ER.L) ;

* #### Government Expenditure Block

 QGDADJ.L     = QGDADJ0 ;
 HGADJ.L      = HGADJ0 ;
 HEADJ.L      = HEADJ0 ;
 EGADJ.L      = EGADJ0 ;

 QGD.L(c)     = QGDADJ.L*comgovconst(c) ;

 VGD.L       = SUM(c,QGD.L(c)*PQD.L(c)) ;

 EG.L         = SUM(c,QGD.L(c)*PQD.L(c))
                 + SUM(h,hogovconst(h)*CPI.L*HGADJ.L)
                 + SUM(e,EGADJ.L*entgovconst(e)*CPI.L) ;

* ------- KAPITAL BLOCK --------------------------------------------------

* ## Savings Block

 SADJ.L       = SADJ0 ;
 SHADJ.L      = SHADJ0 ;
 SEADJ.L      = SEADJ0 ;

 DSHH.L       = DSHH0 ;
 DS.L         = DS0 ;
 DSEN.L       = DSEN0 ;

 SHH.L(h)     = ((shh0(h) + dabshh(h)) * SHADJ.L * SADJ.L)
                   + (DSHH.L * DS.L * shh01(h)) ;

 SEN.L(e)     = ((sen0(e) + dabsen(e)) * SEADJ.L * SADJ.L)
                   + (DSEN.L * DS.L * sen01(e)) ;

 TOTSAV.L     = SUM(f,(deprec(f)*YF.L(f)))
                 + SUM(h,(YH.L(h) * (1 - TYH.L(h))) * SHH.L(h))
                 + SUM(e,(YE.L(e) * (1 - TYE.L(e))) * SEN.L(e))
                 + KAPGOV0
                 + (CAPWOR0*ER.L) ;

* ## Investment Block

 IADJ.L       = IADJ0 ;

 INVEST.L     = TOTSAV.L ;

 QINVD.L(c)   = (IADJ.L*invconst(c)) ;

* ------- FOREIGN INSTITUTIONS BLOCK -------------------------------------

 YFWOR.L(f)   = worvash(f)*YFDISP.L(f) ;

* ------- MARKET CLEARING BLOCK ------------------------------------------

* ##### Account Closure

 KAPGOV.L     = KAPGOV0 ;

 CAPWOR.L     = SUM(cm,PWM.L(cm)*QM.L(cm))
                + (SUM(f,YFWOR.L(f))/ER.L)
                - SUM(ce,PWE.L(ce)*QE.L(ce))
                - SUM(h,howor(h))
                - SUM(e,entwor(e))
                - govwor
                - SUM(f,factwor(f)) ;

* #### Absorption Closure

 VFDOMD.L     = SUM(c, PQD.L(c) * (SUM(h,QCD.L(c,h)) + SUM(e,QED.L(c,e))
                                   + QGD.L(c) + QINVD.L(c) + dstocconst(c))) ;

 INVESTSH.L   = INVEST.L/VFDOMD.L ;

 VGDSH.L      = VGD.L/VFDOMD.L ;

 VEDSH.L(e)   = VED.L(e)/VFDOMD.L ;

* ##### GDP

 GDPVA.L      = SUM((f,a),WF.L(f)*WFDIST.L(f,a)*FD.L(f,a)) + ITAX.L ;

* ##### Slack

 WALRAS.L     = WALRAS0 ;

 OPTION DECIMALS = 6 ;

*-----------------end of smod variable initialisation --------------------
