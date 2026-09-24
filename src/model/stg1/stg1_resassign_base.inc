*-------- stg1 assign result parameters ----------------------------------


* ---- 1a. Assigning values to the results parameters

* Assigning values to RESSCAL parameter

 %res%SCAL("sER","base")          = ER0 ;
 %res%SCAL("sCPI","base")         = CPI0 ;
 %res%SCAL("sPPI","base")         = PPI0 ;

 %res%SCAL("sMTAX","base")        = MTAX0 ;
 %res%SCAL("sETAX","base")        = ETAX0 ;
 %res%SCAL("sSTAX","base")        = STAX0 ;
 %res%SCAL("sEXTAX","base")       = EXTAX0 ;
 %res%SCAL("sITAX","base")        = ITAX0 ;
 %res%SCAL("sFTAX","base")        = FTAX0 ;
 %res%SCAL("sDTAX","base")        = DTAX0 ;
 %res%SCAL("sFYTAX","base")       = FYTAX0 ;

 %res%SCAL("sINVEST","base")      = INVEST0 ;
 %res%SCAL("sTOTSAV","base")      = TOTSAV0 ;
 %res%SCAL("sKAPGOV","base")      = KAPGOV0 ;
 %res%SCAL("sCAPWOR","base")      = CAPWOR0 ;

 %res%SCAL("sYG","base")          = YG0 ;
 %res%SCAL("sEG","base")          = EG0 ;

 %res%SCAL("sVFDOMD","base")      = VFDOMD0 ;
 %res%SCAL("sVGD","base")         = VGD0 ;
 %res%SCAL("sINVESTSH","base")    = INVESTSH0 ;
 %res%SCAL("sVGDSH","base")       = VGDSH0 ;

 %res%SCAL("sGDPVA","base")       = GDPVA0 ;

 %res%SCAL("sWALRAS","base")      = WALRAS0 ;

* Assigning values to resADJ parameter

 %res%ADJ("aTEADJ","base")      = TEADJ0 ;
 %res%ADJ("aTMADJ","base")      = TMADJ0 ;
 %res%ADJ("aTSADJ","base")      = TSADJ0 ;
 %res%ADJ("aTEXADJ","base")     = TEXADJ0 ;
 %res%ADJ("aTXADJ","base")      = TXADJ0 ;
 %res%ADJ("aTFADJ","base")      = TFADJ0 ;
 %res%ADJ("aTYFADJ","base")     = TYFADJ0 ;
 %res%ADJ("aTYHADJ","base")     = TYHADJ0 ;
 %res%ADJ("aTYADJ","base")      = TYADJ0 ;
 %res%ADJ("aTYEADJ","base")     = TYEADJ0 ;

 %res%ADJ("aDTE","base")        = DTE0 ;
 %res%ADJ("aDTM","base")        = DTM0 ;
 %res%ADJ("aDTS","base")        = DTS0 ;
 %res%ADJ("aDTEX","base")       = DTEX0 ;
 %res%ADJ("aDTX","base")        = DTX0 ;
 %res%ADJ("aDTF","base")        = DTF0 ;
 %res%ADJ("aDTYF","base")       = DTYF0 ;
 %res%ADJ("aDTYH","base")       = DTYH0 ;
 %res%ADJ("aDTY","base")        = DTY0 ;
 %res%ADJ("aDTYE","base")       = DTYE0 ;

 %res%ADJ("aSADJ","base")       = SADJ0 ;
 %res%ADJ("aSHADJ","base")      = SHADJ0 ;
 %res%ADJ("aSEADJ","base")      = SEADJ0 ;
 %res%ADJ("aDSHH","base")       = DSHH0 ;
 %res%ADJ("aDS","base")         = DS0 ;
 %res%ADJ("aDSEN","base")       = DSEN0 ;
 %res%ADJ("aIADJ","base")       = IADJ0 ;

 %res%ADJ("aQGDADJ","base")     = QGDADJ0 ;
 %res%ADJ("aQEDADJ","base")     = QEDADJ0 ;

 %res%ADJ("aHGADJ","base")      = HGADJ0 ;
 %res%ADJ("aEGADJ","base")      = EGADJ0 ;
 %res%ADJ("aHEADJ","base")      = HEADJ0 ;

 %res%ADJ("aADXADJ","base")     = ADXADJ0 ;
 %res%ADJ("aDADX","base")       = DADX0 ;
 %res%ADJ("aADVAADJ","base")    = ADVAADJ0 ;
 %res%ADJ("aDADVA","base")      = DADVA0 ;

* ------- TRADE BLOCK ----------------------------------------------------
* #### Exports Block

 %res%PWE(c,"base")             = PWE0(c) ;
 %res%PE(c,"base")              = PE0(c) ;
 %res%PD(c,"base")              = PD0(c) ;
 %res%QE(c,"base")              = QE0(c) ;
 %res%QD(c,"base")              = QD0(c) ;

* #### Imports Block

 %res%PWM(c,"base")             = PWM0(c) ;
 %res%PM(c,"base")              = PM0(c) ;
 %res%QM(c,"base")              = QM0(c) ;
 %res%QQ(c,"base")              = QQ0(c) ;

* ------- COMMODITY PRICE BLOCK ------------------------------------------

 %res%PQS(c,"base")             = PQS0(c) ;
 %res%PQD(c,"base")             = PQD0(c) ;

* ------- PRODUCTION BLOCK -----------------------------------------------

 %res%PX(a,"base")              = PX0(a) ;
 %res%PVA(a,"base")             = PVA0(a) ;
 %res%PINT(a,"base")            = PINT0(a) ;

 %res%ADX(a,"base")             = ADX0(a) ;
 %res%ADFD(f,a,"base")          = ADFD0(f,a) ;
 %res%QX(a,"base")              = QX0(a) ;

 %res%QVA(a,"base")             = QVA0(a) ;
 %res%QINT(a,"base")            = QINT0(a) ;
 %res%ADVA(a,"base")            = ADVA0(a) ;

 %res%WF(f,"base")              = WF0(f) ;
 %res%WFDIST(f,a,"base")        = WFDIST0(f,a) ;
 %res%FD(f,a,"base")            = FD0(f,a) ;
 %res%FS(f,"base")              = FS0(f) ;

 %res%QINTD(c,"base")           = QINTD0(c) ;

 %res%PXC(c,"base")             = PXC0(c) ;
 %res%QXC(c,"base")             = QXC0(c) ;

 %res%PXAC(a,c,"base")          = PXAC0(a,c) ;
 %res%QXAC(a,c,"base")          = QXAC0(a,c) ;

* ------- FACTOR BLOCK ---------------------------------------------------

 %res%YF(f,"base")              = YF0(f) ;
 %res%YFDISP(f,"base")          = YFDISP0(f) ;

* ------- HOUSEHOLD BLOCK ------------------------------------------------
* ## Household Income

 %res%YH(h,"base")              = YH0(h) ;

* Household Expenditure

 %res%HOHO(h,hp,"base")         = HOHO0(h,hp) ;
 %res%HEXP(h,"base")            = HEXP0(h) ;
 %res%QCD(c,h,"base")           = QCD0(c,h) ;

* ------- ENTERPRISE BLOCK -----------------------------------------------
* ## Enterprise Income

 %res%YE(e,"base")              = YE0(e) ;

* ## Enterprise Expenditure

 %res%HOENT(h,e,"base")         = HOENT0(h,e) ;
 %res%GOVENT(e,"base")          = GOVENT0(e) ;
 %res%QED(c,e,"base")           = QED0(c,e) ;
 %res%VED(e,"base")             = VED0(e) ;

* ------- GOVERNMENT BLOCK -----------------------------------------------

* #### Government Income Block

 %res%TE(c,"base")              = TE0(c) ;
 %res%TM(c,"base")              = TM0(c) ;
 %res%TS(c,"base")              = TS0(c) ;
 %res%TEX(c,"base")             = TEX0(c) ;
 %res%TX(a,"base")              = TX0(a) ;
 %res%TF(f,a,"base")            = TF0(f,a) ;
 %res%TYF(f,"base")             = TYF0(f) ;
 %res%TYH(h,"base")             = TYH0(h) ;
 %res%TYE(e,"base")             = TYE0(e) ;

* #### Government Expenditure Block

 %res%QGD(c,"base")             = QGD0(c) ;

* ------- KAPITAL BLOCK --------------------------------------------------

* ## Savings Block

 %res%SHH(h,"base")             = SHH0(h) ;
 %res%SEN(e,"base")             = SEN0(e) ;

* ## Investment Block

 %res%QINVD(c,"base")           = QINVD0(c) ;

* ------- FOREIGN INSTITUTIONS BLOCK -------------------------------------

 %res%YFWOR(f,"base")           = YFWOR0(f) ;

* ------- MARKET CLEARING BLOCK ------------------------------------------
* #### Absorption Closure

 %res%VEDSH(e,"base")           = VEDSH0(e) ;

* The following line terminates the if(sim2(sim1) block

* ) ;

*-------- end of stg1 assign result parameters ---------------------------
