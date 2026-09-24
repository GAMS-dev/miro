*-------- stg1 assign result parameters ----------------------------------


* ---- 1a. Assigning values to the results parameters

* Assigning values to RESSCAL parameter

 %res%SCAL("sER",%res_dimen%)          = ER.L ;
 %res%SCAL("sCPI",%res_dimen%)         = CPI.L ;
 %res%SCAL("sPPI",%res_dimen%)         = PPI.L ;

 %res%SCAL("sMTAX",%res_dimen%)        = MTAX.L ;
 %res%SCAL("sETAX",%res_dimen%)        = ETAX.L ;
 %res%SCAL("sSTAX",%res_dimen%)        = STAX.L ;
 %res%SCAL("sEXTAX",%res_dimen%)       = EXTAX.L ;
 %res%SCAL("sITAX",%res_dimen%)        = ITAX.L ;
 %res%SCAL("sFTAX",%res_dimen%)        = FTAX.L ;
 %res%SCAL("sDTAX",%res_dimen%)        = DTAX.L ;
 %res%SCAL("sFYTAX",%res_dimen%)       = FYTAX.L ;

 %res%SCAL("sINVEST",%res_dimen%)      = INVEST.L ;
 %res%SCAL("sTOTSAV",%res_dimen%)      = TOTSAV.L ;
 %res%SCAL("sKAPGOV",%res_dimen%)      = KAPGOV.L ;
 %res%SCAL("sCAPWOR",%res_dimen%)      = CAPWOR.L ;

 %res%SCAL("sYG",%res_dimen%)          = YG.L ;
 %res%SCAL("sEG",%res_dimen%)          = EG.L ;

 %res%SCAL("sVFDOMD",%res_dimen%)      = VFDOMD.L ;
 %res%SCAL("sVGD",%res_dimen%)         = VGD.L ;
 %res%SCAL("sINVESTSH",%res_dimen%)    = INVESTSH.L ;
 %res%SCAL("sVGDSH",%res_dimen%)       = VGDSH.L ;

 %res%SCAL("sGDPVA",%res_dimen%)       = GDPVA.L ;

 %res%SCAL("sWALRAS",%res_dimen%)      = WALRAS.L ;

* Assigning values to resADJ parameter

 %res%ADJ("aTEADJ",%res_dimen%)      = TEADJ.L ;
 %res%ADJ("aTMADJ",%res_dimen%)      = TMADJ.L ;
 %res%ADJ("aTSADJ",%res_dimen%)      = TSADJ.L ;
 %res%ADJ("aTEXADJ",%res_dimen%)     = TEXADJ.L ;
 %res%ADJ("aTXADJ",%res_dimen%)      = TXADJ.L ;
 %res%ADJ("aTFADJ",%res_dimen%)      = TFADJ.L ;
 %res%ADJ("aTYFADJ",%res_dimen%)     = TYFADJ.L ;
 %res%ADJ("aTYHADJ",%res_dimen%)     = TYHADJ.L ;
 %res%ADJ("aTYADJ",%res_dimen%)      = TYADJ.L ;
 %res%ADJ("aTYEADJ",%res_dimen%)     = TYEADJ.L ;

 %res%ADJ("aDTE",%res_dimen%)        = DTE.L ;
 %res%ADJ("aDTM",%res_dimen%)        = DTM.L ;
 %res%ADJ("aDTS",%res_dimen%)        = DTS.L ;
 %res%ADJ("aDTEX",%res_dimen%)       = DTEX.L ;
 %res%ADJ("aDTX",%res_dimen%)        = DTX.L ;
 %res%ADJ("aDTF",%res_dimen%)        = DTF.L ;
 %res%ADJ("aDTYF",%res_dimen%)       = DTYF.L ;
 %res%ADJ("aDTYH",%res_dimen%)       = DTYH.L ;
 %res%ADJ("aDTY",%res_dimen%)        = DTY.L ;
 %res%ADJ("aDTYE",%res_dimen%)       = DTYE.L ;

 %res%ADJ("aSADJ",%res_dimen%)       = SADJ.L ;
 %res%ADJ("aSHADJ",%res_dimen%)      = SHADJ.L ;
 %res%ADJ("aSEADJ",%res_dimen%)      = SEADJ.L ;
 %res%ADJ("aDSHH",%res_dimen%)       = DSHH.L ;
 %res%ADJ("aDS",%res_dimen%)         = DS.L ;
 %res%ADJ("aDSEN",%res_dimen%)       = DSEN.L ;
 %res%ADJ("aIADJ",%res_dimen%)       = IADJ.L ;

 %res%ADJ("aQGDADJ",%res_dimen%)     = QGDADJ.L ;
 %res%ADJ("aQEDADJ",%res_dimen%)     = QEDADJ.L ;

 %res%ADJ("aHGADJ",%res_dimen%)      = HGADJ.L ;
 %res%ADJ("aEGADJ",%res_dimen%)      = EGADJ.L ;
 %res%ADJ("aHEADJ",%res_dimen%)      = HEADJ.L ;

 %res%ADJ("aADXADJ",%res_dimen%)     = ADXADJ.L ;
 %res%ADJ("aDADX",%res_dimen%)       = DADX.L ;
 %res%ADJ("aADVAADJ",%res_dimen%)    = ADVAADJ.L ;
 %res%ADJ("aDADVA",%res_dimen%)      = DADVA.L ;

* ------- TRADE BLOCK ----------------------------------------------------
* #### Exports Block

 %res%PWE(c,%res_dimen%)             = PWE.L(c) ;
 %res%PE(c,%res_dimen%)              = PE.L(c) ;
 %res%PD(c,%res_dimen%)              = PD.L(c) ;
 %res%QE(c,%res_dimen%)              = QE.L(c) ;
 %res%QD(c,%res_dimen%)              = QD.L(c) ;

* #### Imports Block

 %res%PWM(c,%res_dimen%)             = PWM.L(c) ;
 %res%PM(c,%res_dimen%)              = PM.L(c) ;
 %res%QM(c,%res_dimen%)              = QM.L(c) ;
 %res%QQ(c,%res_dimen%)              = QQ.L(c) ;

* ------- COMMODITY PRICE BLOCK ------------------------------------------

 %res%PQS(c,%res_dimen%)             = PQS.L(c) ;
 %res%PQD(c,%res_dimen%)             = PQD.L(c) ;

* ------- PRODUCTION BLOCK -----------------------------------------------

 %res%PX(a,%res_dimen%)              = PX.L(a) ;
 %res%PVA(a,%res_dimen%)             = PVA.L(a) ;
 %res%PINT(a,%res_dimen%)            = PINT.L(a) ;

 %res%ADX(a,%res_dimen%)             = ADX.L(a) ;
 %res%ADFD(f,a,%res_dimen%)          = ADFD.L(f,a) ;
 %res%QX(a,%res_dimen%)              = QX.L(a) ;

 %res%QVA(a,%res_dimen%)             = QVA.L(a) ;
 %res%QINT(a,%res_dimen%)            = QINT.L(a) ;
 %res%ADVA(a,%res_dimen%)            = ADVA.L(a) ;

 %res%WF(f,%res_dimen%)              = WF.L(f) ;
 %res%WFDIST(f,a,%res_dimen%)        = WFDIST.L(f,a) ;
 %res%FD(f,a,%res_dimen%)            = FD.L(f,a) ;
 %res%FS(f,%res_dimen%)              = FS.L(f) ;

 %res%QINTD(c,%res_dimen%)           = QINTD.L(c) ;

 %res%PXC(c,%res_dimen%)             = PXC.L(c) ;
 %res%QXC(c,%res_dimen%)             = QXC.L(c) ;

 %res%PXAC(a,c,%res_dimen%)          = PXAC.L(a,c) ;
 %res%QXAC(a,c,%res_dimen%)          = QXAC.L(a,c) ;

* ------- FACTOR BLOCK ---------------------------------------------------

 %res%YF(f,%res_dimen%)              = YF.L(f) ;
 %res%YFDISP(f,%res_dimen%)          = YFDISP.L(f) ;

* ------- HOUSEHOLD BLOCK ------------------------------------------------
* ## Household Income

 %res%YH(h,%res_dimen%)              = YH.L(h) ;

* Household Expenditure

 %res%HOHO(h,hp,%res_dimen%)         = HOHO.L(h,hp) ;
 %res%HEXP(h,%res_dimen%)            = HEXP.L(h) ;
 %res%QCD(c,h,%res_dimen%)           = QCD.L(c,h) ;

* ------- ENTERPRISE BLOCK -----------------------------------------------
* ## Enterprise Income

 %res%YE(e,%res_dimen%)              = YE.L(e) ;

* ## Enterprise Expenditure

 %res%HOENT(h,e,%res_dimen%)         = HOENT.L(h,e) ;
 %res%GOVENT(e,%res_dimen%)          = GOVENT.L(e) ;
 %res%QED(c,e,%res_dimen%)           = QED.L(c,e) ;
 %res%VED(e,%res_dimen%)             = VED.L(e) ;

* ------- GOVERNMENT BLOCK -----------------------------------------------

* #### Government Income Block

 %res%TE(c,%res_dimen%)              = TE.L(c) ;
 %res%TM(c,%res_dimen%)              = TM.L(c) ;
 %res%TS(c,%res_dimen%)              = TS.L(c) ;
 %res%TEX(c,%res_dimen%)             = TEX.L(c) ;
 %res%TX(a,%res_dimen%)              = TX.L(a) ;
 %res%TF(f,a,%res_dimen%)            = TF.L(f,a) ;
 %res%TYF(f,%res_dimen%)             = TYF.L(f) ;
 %res%TYH(h,%res_dimen%)             = TYH.L(h) ;
 %res%TYE(e,%res_dimen%)             = TYE.L(e) ;

* #### Government Expenditure Block

 %res%QGD(c,%res_dimen%)             = QGD.L(c) ;

* ------- KAPITAL BLOCK --------------------------------------------------

* ## Savings Block

 %res%SHH(h,%res_dimen%)             = SHH.L(h) ;
 %res%SEN(e,%res_dimen%)             = SEN.L(e) ;

* ## Investment Block

 %res%QINVD(c,%res_dimen%)           = QINVD.L(c) ;

* ------- FOREIGN INSTITUTIONS BLOCK -------------------------------------

 %res%YFWOR(f,%res_dimen%)           = YFWOR.L(f) ;

* ------- MARKET CLEARING BLOCK ------------------------------------------
* #### Absorption Closure

 %res%VEDSH(e,%res_dimen%)           = VEDSH.L(e) ;

* The following line terminates the if(sim2(sim1) block

* ) ;

*-------- end of stg1 assign result parameters ---------------------------
