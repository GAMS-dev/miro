* -------------- smod sam1 checks ----------------------------------------
$ontext
  This file checks that the model is calibrated correctly by checking that
the claibrated values for variables and parameters produce balanced Macro
and Micro SAM.

  The programme aborts IF checks are failed and reports are displayed.
$offtext

* ------- MACRO SAM ------------------------------------------------------
$ontext
  This section checks that the model is calibrated with a balanced SAM,
by using a Macro SAM.

  The programme aborts IF
           - ROW AND COLUMN SUMS FOR ASAM1 NOT EQUAL
           - ALL ASAM1 CELL ENTRIES DO NOT EQUAL ASAM0 ENTRIES

  Reports are produced for
      i)   the ratio of ASAM1 to ASAM0 (all should equal 1)
      ii)  the differences between the Macro SAMs
      iii) a count of the number of differences.
$offtext

 OPTION DECIMALS = 6 ;

 ASAM1("COMMDTY","ACTIVITY") = SUM(c,QINTD.L(c)*PQD.L(c)) ;
 ASAM1("COMMDTY","HHOLDS")   = SUM((c,h),QCD.L(c,h)*PQD.L(c)) ;
 ASAM1("COMMDTY","ENTP")     = SUM((c,e),QED.L(c,e)*PQD.L(c)) ;
 ASAM1("COMMDTY","GOVTN")    = SUM(c,QGD.L(c)*PQD.L(c)) ;
 ASAM1("COMMDTY","KAPITAL")  = SUM(c,QINVD.L(c)*PQD.L(c))
                               + SUM(c,PQD.L(c)*dstocconst(c)) ;
 ASAM1("COMMDTY","WORLD")    = SUM(c,PWE.L(c)*QE.L(c)*ER.L) ;

 ASAM1("ACTIVITY","COMMDTY") = SUM(a,PX.L(a)*QX.L(a)) ;

 ASAM1("VALUAD","ACTIVITY")  = SUM((f,a),WF.L(f)*WFDIST.L(f,a)*FD.L(f,a)) ;
 ASAM1("VALUAD","WORLD")     = SUM(f,factwor(f)*ER.L) ;

 ASAM1("HHOLDS","VALUAD")    = SUM((h,f),hovash(h,f)*YFDISP.L(f)) ;
 ASAM1("HHOLDS","HHOLDS")    = SUM((h,hp),hohosh(h,hp)
                                *((YH.L(h)*(1-TYH.L(h)))*(1-SHH.L(h)))) ;
 ASAM1("HHOLDS","ENTP")      = SUM((h,e),HOENT.L(h,e)) ;
 ASAM1("HHOLDS","GOVTN")     = SUM(h,hogovconst(h)*CPI.L*HGADJ.L) ;
 ASAM1("HHOLDS","WORLD")     = SUM(h,howor(h)*ER.L) ;

 ASAM1("ENTP","VALUAD")      = SUM((e,f),entvash(e,f)*YFDISP.L(f)) ;
 ASAM1("ENTP","GOVTN")       = SUM(e,EGADJ.L*entgovconst(e)*CPI.L) ;
 ASAM1("ENTP","WORLD")       = SUM(e,entwor(e)*ER.L) ;

 ASAM1("GOVTN","COMMDTY")    = SUM(c,TM.L(c)*PWM.L(c)*QM.L(c)*ER.L)
                              + SUM(c,TE.L(c)*PWE.L(c)*ER.L*QE.L(c))
                              + SUM(c,TS.L(c)*PQS.L(c)*QQ.L(c))
                              + SUM(c,TEX.L(c)*PQS.L(c)*QQ.L(c)) ;
 ASAM1("GOVTN","ACTIVITY")   = SUM(a,TX.L(a)*PX.L(a)*QX.L(a))
                              + SUM((f,a),TF.L(f,a)*WF.L(f)*WFDIST.L(f,a)
                                *FD.L(f,a)) ;
 ASAM1("GOVTN","VALUAD")     = SUM(f,govvash(f)*YFDISP.L(f))
                              + SUM(f,TYF.L(f)*(YF.L(f)*(1- deprec(f)))) ;
 ASAM1("GOVTN","HHOLDS")     = SUM(h,TYH.L(h)*YH.L(h)) ;
 ASAM1("GOVTN","ENTP")       = SUM(e,GOVENT.L(e))
                              + SUM(e,TYE.L(e)*YE.L(e)) ;
 ASAM1("GOVTN","WORLD")      = (govwor*ER.L) ;

 ASAM1("KAPITAL","VALUAD")   = SUM(f,deprec(f)*YF.L(f)) ;
 ASAM1("KAPITAL","HHOLDS")   = SUM(h,(YH.L(h)*(1-TYH.L(h)))*SHH.L(h)) ;
 ASAM1("KAPITAL","ENTP")     = SUM(e,(YE.L(e)*(1-TYE.L(e)))*SEN.L(e)) ;
 ASAM1("KAPITAL","GOVTN")    = KAPGOV.L ;
 ASAM1("KAPITAL","WORLD")    = (CAPWOR.L*ER.L) ;

 ASAM1("WORLD","COMMDTY")    = SUM(c,PWM.L(c)*QM.L(c)*ER.L) ;
 ASAM1("WORLD","VALUAD")     = SUM(f,YFWOR.L(f)) ;

 ASAM1("TOTALS",ssnp)        = SUM(ssn,ASAM1(ssn,ssnp)) ;
 ASAM1(ssn,"TOTALS")         = SUM(ssnp,ASAM1(ssn,ssnp)) ;


PARAMETER
 CHECK1             Check on row and column sums
 ASAM1CHK(ss,ss)    Check on differences between ASAM0 and ASAM1
   ;


*-------- Checking that a balanced and equal ASAM is produced
PARAMETER
 ASAM1CHK(ss,ss)    Ratio of ASAM1 to ASAM0 should be 1
 CHECK1             Differences in Row and Column totals of ASAM1

 DIFFASAM1(ss,ss)   Differences between ASAM and ASAM1

 CNTASAM1           Count of non zero entries in DIFFASAM1

 ;

 CHECK1 = SUM(ssn,(ABS(ASAM1(ssn,"TOTALS")-ASAM1("TOTALS",ssn)))) ;

 ASAM1CHK(ss,ssp)$ASAM0(ss,ssp)
                   = (ASAM1(ss,ssp))/ASAM0(ss,ssp) ;

 DIFFASAM1(ss,ssp)  = ASAM0(ss,ssp) - ASAM1(ss,ssp) ;

 DIFFASAM1(ss,ssp)$(ABS(DIFFASAM1(ss,ssp)
                  lt (10*mod_cont("toldiffsam")))) = 0.0 ;

 DIFFASAM1("totals","totals")   = 0.0 ;

 CNTASAM1  = SUM((ssn,ssnp)$DIFFASAM1(ssn,ssnp), 1) ;

 OPTION decimals = 6 ;

 DISPLAY ASAM1, ASAM1CHK, CHECK1, DIFFASAM1, CNTASAM1 ;

 OPTION decimals = 3 ;

 ABORT$(CNTASAM1 GT 0.0)
         "ALL ASAM1 CELL ENTRIES DO NOT EQUAL ASAM0 ENTRIES" ;

 ABORT $(CHECK1 GT 0.000005)
         "SAM ROW AND COLUMN SUMS NOT EQUAL" ;


* ------- Micro SAM ------------------------------------------------------
$ontext
  This section checks that the model is calibrated with a balanced SAM,
by using a Micro SAM.

  The programme aborts IF
           - ALL SAM1 CELL ENTRIES DO NOT EQUAL SAM0 ENTRIES

  Reports are produced for
      i)   the ratio of ASAM1 to ASAM0 (all should equal 1)
      ii)  the differences between the Macro SAMs
      iii) a count of the number of differences.
$offtext

PARAMETER
 SAM1(sac,sacp)      Micro SAM for checking model calibration

 SAM1CHK(sac,sac)    Ratio of SAM1 to SAM should be 1
 DIFFSAM1(sac,sac)   Differences between SAM and SAM1
 CNTSAM1             Count of non zero entries in DIFFSAM1
 ;

If(flow_cont("micsam1") = 1,

 SAM1(c,a)              = ioqtdqd(c,a)*QINT.L(a)*PQD.L(c) ;

 SAM1(c,h)              = QCD.L(c,h)*PQD.L(c) ;

 SAM1(c,e)              = QED.L(c,e)*PQD.L(c) ;

 SAM1(c,"GOVT")         = QGD.L(c)*PQD.L(c) ;

 SAM1(c,"KAP")          = QINVD.L(c)*PQD.L(c) ;
 SAM1(c,"DSTOC")        = PQD.L(c)*dstocconst(c) ;
 SAM1("DSTOC","KAP")    = SUM(c,PQD.L(c)*dstocconst(c)) ;

 SAM1(c,"ROW")          = PWE.L(c)*QE.L(c)*ER.L ;

 SAM1(a,c)              = PXAC.L(a,c)*QXAC.L(a,c) ;

 SAM1(f,a)              = WF.L(f)*WFDIST.L(f,a)*FD.L(f,a) ;

 SAM1(f,"ROW")          = factwor(f)*ER.L ;

 SAM1(h,f)              = hovash(h,f)*YFDISP.L(f) ;

 SAM1(h,hp)             = hohosh(h,hp)
                                * ((YH.L(h)*(1-TYH.L(h)))*(1-SHH.L(h))) ;

 SAM1(h,e)              = HOENT.L(h,e) ;

 SAM1(h,"GOVT")         = hogovconst(h)*CPI.L*HGADJ.L ;

 SAM1(h,"ROW")          = howor(h)*ER.L ;

 SAM1(e,f)              = entvash(e,f)*YFDISP.L(f) ;

 SAM1(e,"GOVT")         = EGADJ.L*entgovconst(e)*CPI.L ;

 SAM1(e,"ROW")          = entwor(e)*ER.L ;

 SAM1("GOVT","IMPTAX")  = SUM(c,TM.L(c)*PWM.L(c)*QM.L(c)*ER.L) ;
 SAM1("IMPTAX",c)       = TM.L(c)*PWM.L(c)*QM.L(c)*ER.L ;

 SAM1("GOVT","EXPTAX")  = SUM(c,TE.L(c)*PWE.L(c)*ER.L*QE.L(c)) ;
 SAM1("EXPTAX",c)       = TE.L(c)*PWE.L(c)*ER.L*QE.L(c) ;

 SAM1("GOVT","SALTAX")  = SUM(c,TS.L(c)*PQS.L(c)*QQ.L(c)) ;
 SAM1("SALTAX",c)       = TS.L(c)*PQS.L(c)*QQ.L(c) ;

 SAM1("GOVT","ECTAX")   = SUM(c,TEX.L(c)*PQS.L(c)*QQ.L(c)) ;
 SAM1("ECTAX",c)        = TEX.L(c)*PQS.L(c)*QQ.L(c) ;

 SAM1("GOVT","INDTAX")  = SUM(a,TX.L(a)*PX.L(a)*QX.L(a)) ;
 SAM1("INDTAX",a)       = TX.L(a)*PX.L(a)*QX.L(a) ;

 SAM1("GOVT",tff)       = SUM(a,SUM(f$map_tff_f(tff,f),
                             TF.L(f,a)*WF.L(f)*WFDIST.L(f,a)*FD.L(f,a))) ;
 SAM1(tff,a)            = SUM(f$map_tff_f(tff,f),
                             TF.L(f,a)*WF.L(f)*WFDIST.L(f,a)*FD.L(f,a)) ;

 SAM1("FACTTAX",f)      = TYF.L(f)*(YF.L(f)*(1- deprec(f))) ;
 SAM1("GOVT","FACTTAX") = SUM(f,TYF.L(f)*(YF.L(f)*(1- deprec(f)))) ;

 SAM1("DIRTAX",h)       = TYH.L(h)*YH.L(h) ;

 SAM1("DIRTAX",e)       = TYE.L(e)*YE.L(e) ;

 SAM1("GOVT","DIRTAX")  = SUM(h,TYH.L(h)*YH.L(h)) +
                                SUM(e,(TYE.L(e)*YE.L(e))) ;

 SAM1("GOVT",f)         = govvash(f)*YFDISP.L(f) ;

 SAM1("GOVT","ROW")     = (govwor*ER.L) ;

 SAM1("KAP",f)          = deprec(f)*YF.L(f) ;

 SAM1("KAP",h)          = (YH.L(h)*(1-TYH.L(h)))*SHH.L(h) ;

 SAM1("KAP",e)          = (YE.L(e) * (1-TYE.L(e)))*SEN.L(e) ;
 SAM1("KAP","GOVT")     = KAPGOV.L ;
 SAM1("KAP","ROW")      = (CAPWOR.L*ER.L) ;
 SAM1("ROW",c)          = PWM.L(c)*QM.L(c)*ER.L ;
 SAM1("ROW",f)          = YFWOR.L(f) ;

 SAM1("TOTAL",SAC)      = SUM(SACNP, SAM1(SACNP,SAC)) ;
 SAM1(SAC,"TOTAL")      = SUM(SACNP, SAM1(SAC,SACNP)) ;

 SAM1(SAC,SACP)         = SAM1(SAC,SACP) * samscal ;


* Testing for differences between base (adjusted) SAM and MICROSAM1

 SAM1CHK(sac,sacp)$SAM(sac,sacp)
                    = (SAM1(sac,sacp)/SAMSCAL)/SAM(sac,sacp) ;

 DIFFSAM1(sac,sacp) = SAM(sac,sacp)-(SAM1(sac,sacp)/SAMSCAL) ;

 DIFFSAM1(sac,sacp)$(ABS(DIFFSAM1(sac,sacp)) lt mod_cont("toldiffsam"))
                    = 0 ;

 DIFFSAM1("total","total")   = 0.0 ;

 CNTSAM1  = SUM((sac,sacp)$DIFFSAM1(sac,sacp), 1) ;

 OPTION decimals = 6 ;

DISPLAY SAM1CHK, DIFFSAM1, CNTSAM1 ;

 OPTION decimals = 3 ;

 ABORT$(CNTSAM1 GT 0.0)
         "ALL SAM1 CELL ENTRIES DO NOT EQUAL SAM ENTRIES" ;

* No totals check required since SAM has already been checked

* End of IF
 ) ;

* -------------- end of smod5 sam1 checks --------------------------------
