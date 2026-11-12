*############### 5b. CALCULATE PERCENTAGE CHANGE RESULTS #####################

*#### CALCULATING THE PERCENTAGE CHANGES

* Assigning values to resSCAL parameter

 %res%SCAL("sMTAX",%res_dimen%)$MTAX0
                          = (resSCAL("sMTAX",%res_dimen%)/MTAX0-1)*100 ;
 %res%SCAL("sETAX",%res_dimen%)$ETAX0
                          = (resSCAL("sETAX",%res_dimen%)/ETAX0-1)*100 ;
 %res%SCAL("sSTAX",%res_dimen%)$STAX0
                          = (resSCAL("sSTAX",%res_dimen%)/STAX0-1)*100 ;
 %res%SCAL("sEXTAX",%res_dimen%)$EXTAX0
                          = (resSCAL("sEXTAX",%res_dimen%)/EXTAX0-1)*100 ;
 %res%SCAL("sITAX",%res_dimen%)$ITAX0
                          = (resSCAL("sITAX",%res_dimen%)/ITAX0-1)*100 ;
 %res%SCAL("sDTAX",%res_dimen%)$DTAX0
                       = (resSCAL("sDTAX",%res_dimen%)/DTAX0-1)*100 ;
 %res%SCAL("sFYTAX",%res_dimen%)$FYTAX0
                     = (resSCAL("sFYTAX",%res_dimen%)/FYTAX0-1)*100 ;
 %res%SCAL("sINVEST",%res_dimen%)$INVEST0
                      = (resSCAL("sINVEST",%res_dimen%)/INVEST0-1)*100 ;
 %res%SCAL("sTOTSAV",%res_dimen%)$TOTSAV0
                      = (resSCAL("sTOTSAV",%res_dimen%)/TOTSAV0-1)*100 ;
 %res%SCAL("sKAPGOV",%res_dimen%)$KAPGOV0
                      = (resSCAL("sKAPGOV",%res_dimen%)/KAPGOV0-1)*100 ;
 %res%SCAL("sCAPWOR",%res_dimen%)$CAPWOR0
                      = (resSCAL("sCAPWOR",%res_dimen%)/CAPWOR0-1)*100 ;

 %res%SCAL("sYG",%res_dimen%)$YG0
                      = (resSCAL("sYG",%res_dimen%)/YG0-1)*100 ;
 %res%SCAL("sEG",%res_dimen%)$EG0
                       = (resSCAL("sEG",%res_dimen%)/EG0-1)*100 ;

 %res%SCAL("sVFDOMD",%res_dimen%)$VFDOMD0
                      = (resSCAL("sVFDOMD",%res_dimen%)/VFDOMD0-1)*100  ;
 %res%SCAL("sVGD",%res_dimen%)$VGD0
                       = (resSCAL("sVGD",%res_dimen%)/VGD0-1)*100 ;

 %res%SCAL("sINVESTSH",%res_dimen%)$INVESTSH0
                       = (resSCAL("sINVESTSH",%res_dimen%)
                             /INVESTSH0-1)*100  ;
 %res%SCAL("sVGDSH",%res_dimen%)$VGDSH0
                       = (resSCAL("sVGDSH",%res_dimen%)/VGDSH0-1)*100  ;

 %res%SCAL("sCPI",%res_dimen%)$CPI0 = (resSCAL("sCPI",%res_dimen%)/CPI0-1)*100 ;
 %res%SCAL("sPPI",%res_dimen%)$PPI0 = (resSCAL("sPPI",%res_dimen%)/PPI0-1)*100  ;
 %res%SCAL("sER",%res_dimen%)$ER0   = (resSCAL("sER",%res_dimen%)/ER0-1)*100  ;

 %res%SCAL("sWALRAS",%res_dimen%)$WALRAS0
                          = (resSCAL("sWALRAS",%res_dimen%)/WALRAS0-1)*100 ;

* Assigning values to resADJ parameter

 %res%ADJ("aTEADJ",%res_dimen%)$TEADJ0
                       = (resADJ("aTMADJ",%res_dimen%)/TEADJ0-1)*100 ;
 %res%ADJ("aTMADJ",%res_dimen%)$TMADJ0
                       = (resADJ("aTMADJ",%res_dimen%)/TMADJ0-1)*100 ;
 %res%ADJ("aTSADJ",%res_dimen%)$TSADJ0
                       = (resADJ("aTSADJ",%res_dimen%)/TSADJ0-1)*100 ;
 %res%ADJ("aTEXADJ",%res_dimen%)$TEXADJ0
                       = (resADJ("aTEXADJ",%res_dimen%)/TEXADJ0-1)*100 ;
 %res%ADJ("aTXADJ",%res_dimen%)$TXADJ0
                       = (resADJ("aTXADJ",%res_dimen%)/TXADJ0-1)*100 ;
 %res%ADJ("aTYFADJ",%res_dimen%)$TYFADJ0
                       = (resADJ("aTYFADJ",%res_dimen%)/TYFADJ0-1)*100 ;
 %res%ADJ("aTYHADJ",%res_dimen%)$TYHADJ0
                       = (resADJ("aTYHADJ",%res_dimen%)/TYHADJ0-1)*100 ;
 %res%ADJ("aTYADJ",%res_dimen%)$TYADJ0
                       = (resADJ("aTYHADJ",%res_dimen%)/TYADJ0-1)*100 ;
 %res%ADJ("aTYEADJ",%res_dimen%)$TYEADJ0
                       = (resADJ("aTYEADJ",%res_dimen%)/TYEADJ0-1)*100 ;

 %res%ADJ("aDTE",%res_dimen%)$DTE0
                      = (resADJ("aDTE",%res_dimen%)/DTE0-1)*100 ;
 %res%ADJ("aDTM",%res_dimen%)$DTM0
                      = (resADJ("aDTM",%res_dimen%)/DTM0-1)*100 ;
 %res%ADJ("aDTS",%res_dimen%)$DTS0
                      = (resADJ("aDTS",%res_dimen%)/DTS0-1)*100 ;
 %res%ADJ("aDTEX",%res_dimen%)$DTEX0
                      = (resADJ("aDTEX",%res_dimen%)/DTEX0-1)*100 ;
 %res%ADJ("aDTX",%res_dimen%)$DTX0
                      = (resADJ("aDTX",%res_dimen%)/DTX0-1)*100 ;
 %res%ADJ("aDTYF",%res_dimen%)$DTYF0
                      = (resADJ("aDTYF",%res_dimen%)/DTYF0-1)*100 ;
 %res%ADJ("aDTYH",%res_dimen%)$DTYH0
                      = (resADJ("aDTYH",%res_dimen%)/DTYH0-1)*100 ;
 %res%ADJ("aDTY",%res_dimen%)$DTY0
                      = (resADJ("aDTY",%res_dimen%)/DTY0-1)*100 ;
 %res%ADJ("aDTYE",%res_dimen%)$DTYE0
                      = (resADJ("aDTYE",%res_dimen%)/DTYE0-1)*100 ;

 %res%ADJ("aSADJ",%res_dimen%)$SADJ0
                       = (resADJ("aSADJ",%res_dimen%)/SADJ0-1)*100 ;
 %res%ADJ("aSHADJ",%res_dimen%)$SHADJ0
                       = (resADJ("aSHADJ",%res_dimen%)/SHADJ0-1)*100 ;
 %res%ADJ("aSEADJ",%res_dimen%)$SEADJ0
                       = (resADJ("aSEADJ",%res_dimen%)/SEADJ0-1)*100 ;
 %res%ADJ("aDSHH",%res_dimen%)$DSHH0
                       = (resADJ("aDSHH",%res_dimen%)/DSHH0-1)*100 ;
 %res%ADJ("aDS",%res_dimen%)$DS0
                       = (resADJ("aDS",%res_dimen%)/DS0-1)*100 ;
 %res%ADJ("aDSEN",%res_dimen%)$DSEN0
                       = (resADJ("aDSEN",%res_dimen%)/DSEN0-1)*100 ;
 %res%ADJ("aIADJ",%res_dimen%)$IADJ0
                       = (resADJ("aIADJ",%res_dimen%)/IADJ0-1)*100 ;

 %res%ADJ("aQGDADJ",%res_dimen%)$QGDADJ0
                       = (resADJ("aQGDADJ",%res_dimen%)/QGDADJ0-1)*100 ;
 %res%ADJ("aQEDADJ",%res_dimen%)$QEDADJ0
                       = (resADJ("aQEDADJ",%res_dimen%)/QEDADJ0-1)*100 ;

 %res%ADJ("aHGADJ",%res_dimen%)$HGADJ0
                       = (resADJ("aHGADJ",%res_dimen%)/HGADJ0-1)*100 ;
 %res%ADJ("aEGADJ",%res_dimen%)$EGADJ0
                       = (resADJ("aEGADJ",%res_dimen%)/EGADJ0)*100 ;
 %res%ADJ("aHEADJ",%res_dimen%)$HEADJ0
                          = (resADJ("aHEADJ",%res_dimen%)/HEADJ0-1)*100 ;
* Price results

 %res%PM(c,%res_dimen%)$PM0(c)    = (resPM(c,%res_dimen%)/PM0(c)-1)*100 ;
 %res%PE(c,%res_dimen%)$PE0(c)    = (resPE(c,%res_dimen%)/PE0(c)-1)*100 ;
 %res%PQS(c,%res_dimen%)$PQS0(c)  = (resPQS(c,%res_dimen%)/PQS0(c)-1)*100 ;
 %res%PQD(c,%res_dimen%)$PQD0(c)  = (resPQD(c,%res_dimen%)/PQD0(c)-1)*100 ;
 %res%PXC(c,%res_dimen%)$PXC0(c)  = (resPXC(c,%res_dimen%)/PXC0(c)-1)*100 ;
 %res%PXAC(a,c,%res_dimen%)$PXAC0(a,c)
                             = (resPXAC(a,c,%res_dimen%)/PXAC0(a,c)-1)*100 ;
 %res%PD(c,%res_dimen%)$PD0(c)    = (resPD(c,%res_dimen%)/PD0(c)-1)*100 ;
 %res%PX(a,%res_dimen%)$PX0(a)    = (resPX(a,%res_dimen%)/PX0(a)-1)*100 ;
 %res%PINT(a,%res_dimen%)$PINT0(a) = (resPINT(a,%res_dimen%)/PINT0(a)-1)*100 ;
 %res%PVA(a,%res_dimen%)$PVA0(a)  = (resPVA(a,%res_dimen%)/PVA0(a)-1)*100 ;
 %res%PWE(c,%res_dimen%)$PWE0(c)  = (resPWE(c,%res_dimen%)/PWE0(c)-1)*100 ;
 %res%PWM(c,%res_dimen%)$PWM0(c)  = (resPWM(c,%res_dimen%)/PWM0(c)-1)*100 ;

* Production results

 %res%QX(a,%res_dimen%)$QX0(a)    = (resQX(a,%res_dimen%)/QX0(a)-1)*100 ;
 %res%QXC(c,%res_dimen%)$QXC0(c)  = (resQXC(c,%res_dimen%)/QXC0(c)-1)*100 ;
 %res%QXAC(a,c,%res_dimen%)$QXAC0(a,c)
                            = (resQXAC(a,c,%res_dimen%)/QXAC0(a,c)-1)*100 ;
 %res%QE(c,%res_dimen%)$QE0(c)    = (resQE(c,%res_dimen%)/QE0(c)-1)*100 ;
 %res%QD(c,%res_dimen%)$QD0(c)    = (resQD(c,%res_dimen%)/QD0(c)-1)*100 ;
 %res%QM(c,%res_dimen%)$QM0(c)    = (resQM(c,%res_dimen%)/QM0(c)-1)*100 ;
 %res%QQ(c,%res_dimen%)$QQ0(c)    = (resQQ(c,%res_dimen%)/QQ0(c)-1)*100 ;
 %res%QINT(a,%res_dimen%)$QINT0(a) = (resQINT(a,%res_dimen%)/QINT0(a)-1)*100 ;
 %res%QVA(a,%res_dimen%)$QVA0(a)  = (resQVA(a,%res_dimen%)/QVA0(a)-1)*100 ;
 %res%QINTD(c,%res_dimen%)$QINTD0(c)
                                = (resQINTD(c,%res_dimen%)/QINTD0(c)-1)*100 ;

* Factor results

 %res%WF(f,%res_dimen%)$WF0(f)    = (resWF(f,%res_dimen%)/WF0(f)-1)*100 ;
 %res%WFDIST(f,a,%res_dimen%)$WFDIST0(f,a)
                             = (resWFDIST(f,a,%res_dimen%)
                                      /WFDIST0(f,a)-1)*100 ;
 %res%FD(f,a,%res_dimen%)$FD0(f,a) = (resFD(f,a,%res_dimen%)/FD0(f,a)-1)*100 ;
 %res%FS(f,%res_dimen%)$FS0(f)    = (resFS(f,%res_dimen%)/FS0(f)-1)*100 ;
 %res%YF(f,%res_dimen%)$YF0(f)    = (resYF(f,%res_dimen%)/YF0(f)-1)*100 ;
 %res%YFDISP(f,%res_dimen%)$YFDISP0(f)
                             = (resYFDISP(f,%res_dimen%)/YFDISP0(f)-1)*100 ;
 %res%YFWOR(f,%res_dimen%)$YFWOR0(f)
                                = (resYFWOR(f,%res_dimen%)/YFWOR0(f)-1)*100 ;

* Household results

 %res%HOENT(h,e,%res_dimen%)$HOENT0(h,e)
                             = (resHOENT(h,e,%res_dimen%)
                                 /HOENT0(h,e)-1)*100 ;
 %res%YH(h,%res_dimen%)$YH0(h)    = (resYH(h,%res_dimen%)/YH0(h)-1)*100 ;

 %res%HOHO(h,hp,%res_dimen%)$HOHO0(h,hp)
                             = (resHOHO(h,hp,%res_dimen%)
                                   /HOHO0(h,hp)-1)*100 ;
 %res%HEXP(h,%res_dimen%)$HEXP0(h) = (resHEXP(h,%res_dimen%)/HEXP0(h)-1)*100 ;

 %res%QCD(c,h,%res_dimen%)$QCD0(c,h)
                                 = (resQCD(c,h,%res_dimen%)/QCD0(c,h)-1)*100 ;

* Government results

 %res%TE(c,%res_dimen%)$TE0(c)     = (resTE(c,%res_dimen%)/TE0(c)-1)*100 ;
 %res%TM(c,%res_dimen%)$TM0(c)     = (resTM(c,%res_dimen%)/TM0(c)-1)*100 ;
 %res%TS(c,%res_dimen%)$TS0(c)     = (resTS(c,%res_dimen%)/TS0(c)-1)*100 ;
 %res%TEX(c,%res_dimen%)$TEX0(c)   = (resTEX(c,%res_dimen%)/TEX0(c)-1)*100 ;
 %res%TX(a,%res_dimen%)$TX0(a)     = (resTX(a,%res_dimen%)/TX0(a)-1)*100 ;
 %res%TYF(f,%res_dimen%)$TYF0(f)   = (resTYF(f,%res_dimen%)/TYF0(f)-1)*100 ;
 %res%TYH(h,%res_dimen%)$TYH0(h)   = (resTYH(h,%res_dimen%)/TYH0(h)-1)*100 ;
 %res%TYE(e,%res_dimen%)$TYE0(e)   = (resTYE(e,%res_dimen%)/TYE0(e)-1)*100 ;

 %res%GOVENT(e,%res_dimen%)$GOVENT0(e)
                                 = (resGOVENT(e,%res_dimen%)
                                     /GOVENT0(e)-1)*100 ;

 %res%QGD(c,%res_dimen%)$QGD0(c)   = (resQGD(c,%res_dimen%)/QGD0(c)-1)*100 ;

*Enterprise results(c)

 %res%YE(e,%res_dimen%)$YE0(e)     = (resYE(e,%res_dimen%)/YE0(e)-1)*100 ;

 %res%VED(e,%res_dimen%)$VED0(e)   = (resVED(e,%res_dimen%)/VED0(e)-1)*100 ;
 %res%VEDSH(e,%res_dimen%)$VEDSH0(e)
                            = (resVEDSH(e,%res_dimen%)/VEDSH0(e)-1)*100 ;

 %res%QED(c,e,%res_dimen%)$QED0(c,e)
                               = (resQED(c,e,%res_dimen%)/QED0(c,e)-1)*100 ;

* Investment results

 %res%SHH(h,%res_dimen%)$SHH0(h)   = (resSHH(h,%res_dimen%)/SHH0(h)-1)*100 ;

 %res%SEN(e,%res_dimen%)$SEN0(e)   = (resSEN(e,%res_dimen%)/SEN0(e)-1)*100 ;

 %res%QINVD(c,%res_dimen%)$QINVD0(c)
                                = (resQINVD(c,%res_dimen%)/QINVD0(c)-1)*100 ;



*################## EXPORT OF RESULT FILES TO GDX ###################

* This writes the results data to GDX
$ifThen.MIRORUN "x%gams.IDCGDXInput%"=="x"
Execute_Unload  'results/stg1_percent.gdx',

* Table of scalar results
 %res%SCAL,
* Table of adjuster results
 %res%ADJ,
* Price results
 %res%PM,
 %res%PE,
 %res%PQS,
 %res%PQD,
 %res%PXC,
 %res%PXAC,
 %res%PD,
 %res%PX,
 %res%PINT,
 %res%PVA,
 %res%PWE,
 %res%PWM,
* Production results
 %res%QX,
 %res%QXC,
 %res%QXAC,
 %res%QE,
 %res%QD,
 %res%QM,
 %res%QQ,
 %res%QINT,
 %res%QVA,
 %res%QINTD,
* Factor results
 %res%WF,
 %res%WFDIST,
 %res%FD,
 %res%FS,
 %res%YF,
 %res%YFDISP,
 %res%YFWOR,
* Household results
 %res%HOENT,
 %res%YH,
 %res%HOHO,
 %res%HEXP,
 %res%QCD,
* Government results
* Tax rates
 %res%TE,
 %res%TM,
 %res%TS,
 %res%TEX,
 %res%TX,
 %res%TYF,
 %res%TYH,
 %res%TYE,
 %res%GOVENT,
 %res%QGD,
* Enterprise results
 %res%YE,
 %res%VED,
 %res%VEDSH,
 %res%QED,
* Investment results
 %res%ShH,
 %res%SEN,
 %res%QINVD

     ;
$endIf.MIRORUN
*########################### THE END ########################
