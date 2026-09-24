* -------------- smod sam2 checks ----------------------------------------
$ontext
  This file checks that the model solves correctly by checking that
the solution values for variables and parameters produce balanced Macro
and Micro SAM.

  The programme aborts IF checks are failed and reports are displayed.
$offtext
* ------- MACRO SAM ------------------------------------------------------
$ontext
  This section checks that the model is solved with a balanced SAM,
by using a Macro SAM.

  The programme aborts IF
           - ROW AND COLUMN SUMS FOR ASAM2 NOT EQUAL
           - ALL ASAM2 CELL ENTRIES DO NOT EQUAL ASAM1 ENTRIES

  Reports are produced for
      i)   the ratio of ASAM1 to ASAM0 (all should equal 1)
      ii)  the differences between the Macro SAMs
      iii) a count of the number of differences.
$offtext

 OPTION DECIMALS = 6 ;

 ASAM2("COMMDTY","ACTIVITY") = SUM(c,QINTD.L(c)*PQD.L(c)) ;
 ASAM2("COMMDTY","HHOLDS")   = SUM((c,h),QCD.L(c,h)*PQD.L(c)) ;
 ASAM2("COMMDTY","ENTP")     = SUM((c,e),QED.L(c,e)*PQD.L(c)) ;
 ASAM2("COMMDTY","GOVTN")    = SUM(c,QGD.L(c)*PQD.L(c)) ;
 ASAM2("COMMDTY","KAPITAL")  = SUM(c,QINVD.L(c)*PQD.L(c))
                               + SUM(c,PQD.L(c)*dstocconst(c)) ;
 ASAM2("COMMDTY","WORLD")    = SUM(c,PWE.L(c)*QE.L(c)*ER.L) ;

 ASAM2("ACTIVITY","COMMDTY") = SUM(a,PX.L(a)*QX.L(a)) ;

 ASAM2("VALUAD","ACTIVITY")  = SUM((f,a),WF.L(f)*WFDIST.L(f,a)*FD.L(f,a)) ;
 ASAM2("VALUAD","WORLD")     = SUM(f,factwor(f)*ER.L) ;

 ASAM2("HHOLDS","VALUAD")    = SUM((h,f),hovash(h,f)*YFDISP.L(f)) ;
 ASAM2("HHOLDS","HHOLDS")    = SUM((h,hp),hohosh(h,hp)
                                *((YH.L(h)*(1-TYH.L(h)))*(1-SHH.L(h)))) ;
 ASAM2("HHOLDS","ENTP")      = SUM((h,e),HOENT.L(h,e)) ;
 ASAM2("HHOLDS","GOVTN")     = SUM(h,hogovconst(h)*CPI.L*HGADJ.L) ;
 ASAM2("HHOLDS","WORLD")     = SUM(h,howor(h)*ER.L) ;

 ASAM2("ENTP","VALUAD")      = SUM((e,f),entvash(e,f)*YFDISP.L(f)) ;
 ASAM2("ENTP","GOVTN")       = SUM(e,EGADJ.L*entgovconst(e)*CPI.L) ;
 ASAM2("ENTP","WORLD")       = SUM(e,entwor(e)*ER.L) ;

 ASAM2("GOVTN","COMMDTY")    = SUM(c,TM.L(c)*PWM.L(c)*QM.L(c)*ER.L)
                              + SUM(c,TE.L(c)*PWE.L(c)*ER.L*QE.L(c))
                              + SUM(c,TS.L(c)*PQS.L(c)*QQ.L(c))
                              + SUM(c,TEX.L(c)*PQS.L(c)*QQ.L(c)) ;
 ASAM2("GOVTN","ACTIVITY")   = SUM(a,TX.L(a)*PX.L(a)*QX.L(a))
                              + SUM((f,a),TF.L(f,a)*WF.L(f)*WFDIST.L(f,a)
                                *FD.L(f,a)) ;
 ASAM2("GOVTN","VALUAD")     = SUM(f,govvash(f)*YFDISP.L(f))
                              + SUM(f,TYF.L(f)*(YF.L(f) * (1- deprec(f)))) ;
 ASAM2("GOVTN","HHOLDS")     = SUM(h,TYH.L(h)*YH.L(h)) ;

 ASAM2("GOVTN","ENTP")       = SUM(e,GOVENT.L(e))
                              + SUM(e,TYE.L(e)*YE.L(e)) ;

 ASAM2("GOVTN","WORLD")      = (govwor*ER.L) ;

 ASAM2("KAPITAL","VALUAD")   = SUM(f,deprec(f)*YF.L(f)) ;
 ASAM2("KAPITAL","HHOLDS")   = SUM(h,(YH.L(h)*(1-TYH.L(h)))*SHH.L(h)) ;
 ASAM2("KAPITAL","ENTP")     = SUM(e,(YE.L(e)*(1-TYE.L(e)))*SEN.L(e)) ;
 ASAM2("KAPITAL","GOVTN")    = KAPGOV.L ;
 ASAM2("KAPITAL","WORLD")    = (CAPWOR.L*ER.L) ;

 ASAM2("WORLD","COMMDTY")    = SUM(c,PWM.L(c)*QM.L(c)*ER.L) ;
 ASAM2("WORLD","VALUAD")     = SUM(f,YFWOR.L(f)) ;

 ASAM2("TOTALS",ssnp)        = SUM(ssn,ASAM2(ssn,ssnp)) ;
 ASAM2(ssn,"TOTALS")         = SUM(ssnp,ASAM2(ssn,ssnp)) ;


*-------- Checking that a balanced and equal ASAM is produced
PARAMETER
 ASAM2CHK(ss,ss)    Ratio of ASAM2 to ASAM1 - 1 except for numeraire check
 CHECK2             Differences in Row and Column totals of ASAM2

 DIFFASAM2(ss,ss)   Differences between ASAM and ASAM2

 CNTASAM2           Count of non zero entries in DIFFASAM2

 ;

 CHECK2 = SUM(ssn,(ABS(ASAM2(ssn,"TOTALS")-ASAM2("TOTALS",ssn)))) ;

 ABORT $(CHECK2 GT 0.000005)
 "SAM ROW AND COLUMN SUMS NOT EQUAL", CHECK2, ASAM2 ;

 ASAM2CHK(ss,ssp)$ASAM1(ss,ssp)
                   = (ASAM2(ss,ssp))/ASAM1(ss,ssp) ;

 DIFFASAM2(ss,ssp)  = ASAM1(ss,ssp) - ASAM2(ss,ssp) ;

 DIFFASAM2(ss,ssp)$(ABS(DIFFASAM2(ss,ssp)
                  lt (10*mod_cont("toldiffsam")))) = 0.0 ;

 DIFFASAM2("totals","totals")   = 0.0 ;

 CNTASAM2  = SUM((ssn,ssnp)$DIFFASAM2(ssn,ssnp), 1) ;

 OPTION decimals = 6 ;

 DISPLAY ASAM2, ASAM2CHK, CHECK2, DIFFASAM2, CNTASAM2 ;

 OPTION decimals = 3 ;

 ABORT$(CNTASAM2 GT 0.0)
         "ALL ASAM2 CELL ENTRIES DO NOT EQUAL ASAM1 ENTRIES" ;

* ------- Micro SAM ------------------------------------------------------
$ontext
  This section checks that the model solution produces a balanced SAM,
by using a Micro SAM.

  The programme aborts IF
           - ALL SAM2 CELL ENTRIES DO NOT EQUAL SAM1 ENTRIES

  Reports are produced for
      i)   the ratio of ASAM1 to ASAM0 (all should equal 1)
      ii)  the differences between the Macro SAMs
      iii) a count of the number of differences.
$offtext

PARAMETER
 SAM2(sac,sacp)     Micro SAM for checking initial model solve

 SAM2CHK(sac,sac)   Ratio of SAM2 to SAM1 should be 1
 DIFFSAM2(sac,sac)  Differences between SAM1 and SAM2
 CNTSAM2            Count of non zero entries in DIFFSAM2

  ;

If(flow_cont("micsam2") = 1,

 SAM2(c,a)              = ioqtdqd(c,a)*QINT.L(a)*PQD.L(c);

 SAM2(c,h)              = QCD.L(c,h)*PQD.L(c);

 SAM2(c,e)              = QED.L(c,e)*PQD.L(c);

 SAM2(c,"GOVT")         = QGD.L(c)*PQD.L(c);

 SAM2(c,"KAP")          = QINVD.L(c)*PQD.L(c);
 SAM2(c,"DSTOC")        = PQD.L(c)*dstocconst(c);
 SAM2("DSTOC","KAP")    = SUM(c,PQD.L(c)*dstocconst(c)) ;

 SAM2(c,"ROW")          = PWE.L(c)*QE.L(c)*ER.L;

 SAM2(a,c)              = PXAC.L(a,c)*QXAC.L(a,c);

 SAM2(f,a)              = WF.L(f)*WFDIST.L(f,a)*FD.L(f,a);

 SAM2(f,"ROW")          = factwor(f)*ER.L;

 SAM2(h,f)              = hovash(h,f)*YFDISP.L(f);

 SAM2(h,hp)             = hohosh(h,hp)
                               * ((YH.L(h)*(1-TYH.L(h)))*(1-SHH.L(h)));

 SAM2(h,e)              = HOENT.L(h,e);

 SAM2(h,"GOVT")         = hogovconst(h)*CPI.L*HGADJ.L;

 SAM2(h,"ROW")          = howor(h)*ER.L;

 SAM2(e,f)              = entvash(e,f)*YFDISP.L(f);

 SAM2(e,"GOVT")         = EGADJ.L*entgovconst(e)*CPI.L;

 SAM2(e,"ROW")          = entwor(e)*ER.L ;

 SAM2("GOVT","IMPTAX")  = SUM(c,TM.L(c)*PWM.L(c)*QM.L(c)*ER.L);
 SAM2("IMPTAX",c)       = TM.L(c)*PWM.L(c)*QM.L(c)*ER.L;

 SAM2("GOVT","EXPTAX")  = SUM(c,TE.L(c)*PWE.L(c)*ER.L*QE.L(c));
 SAM2("EXPTAX",c)       = TE.L(c)*PWE.L(c)*ER.L*QE.L(c);

 SAM2("GOVT","SALTAX")  = SUM(c,TS.L(c)*PQS.L(c)*QQ.L(c)) ;
 SAM2("SALTAX",c)       = TS.L(c)*PQS.L(c)*QQ.L(c);

 SAM2("GOVT","ECTAX")   = SUM(c,TEX.L(c)*PQS.L(c)*QQ.L(c)) ;
 SAM2("ECTAX",c)        = TEX.L(c)*PQS.L(c)*QQ.L(c) ;

 SAM2("GOVT","INDTAX")  = SUM(a,TX.L(a)*PX.L(a)*QX.L(a)) ;
 SAM2("INDTAX",a)       = TX.L(a)*PX.L(a)*QX.L(a);

 SAM2("GOVT",tff)       = SUM(a,SUM(f$map_tff_f(tff,f),
                             TF.L(f,a)*WF.L(f)*WFDIST.L(f,a)*FD.L(f,a))) ;
 SAM2(tff,a)            = SUM(f$map_tff_f(tff,f),
                             TF.L(f,a)*WF.L(f)*WFDIST.L(f,a)*FD.L(f,a)) ;

 SAM2("FACTTAX",f)      = TYF.L(f)*(YF.L(f)*(1- deprec(f)));
 SAM2("GOVT","FACTTAX") = SUM(f,TYF.L(f)*(YF.L(f)*(1- deprec(f)))) ;

 SAM2("DIRTAX",h)       = TYH.L(h)*YH.L(h);

 SAM2("DIRTAX",e)       = TYE.L(e)*YE.L(e);

 SAM2("GOVT","DIRTAX")  = SUM(h,TYH.L(h)*YH.L(h)) +
                                SUM(e,(TYE.L(e)*YE.L(e))) ;

 SAM2("GOVT",f)         = govvash(f)*YFDISP.L(f);

 SAM2("GOVT","ROW")     = (govwor*ER.L) ;

 SAM2("KAP",f)          = deprec(f)*YF.L(f);

 SAM2("KAP",h)          = (YH.L(h)*(1-TYH.L(h)))*SHH.L(h) ;

 SAM2("KAP",e)          = (YE.L(e)*(1-TYE.L(e)))*SEN.L(e) ;
 SAM2("KAP","GOVT")     = KAPGOV.L ;
 SAM2("KAP","ROW")      = (CAPWOR.L*ER.L) ;
 SAM2("ROW",c)          = PWM.L(c)*QM.L(c)*ER.L;
 SAM2("ROW",f)          = YFWOR.L(f);

 SAM2("TOTAL",SAC)      = SUM(SACNP, SAM2(SACNP,SAC));
 SAM2(SAC,"TOTAL")      = SUM(SACNP, SAM2(SAC,SACNP));

 SAM2(SAC,SACP)         = SAM2(SAC,SACP) * samscal ;

  ) ;

* Testing for differences between SAM2 and SAM1

IF((mod_cont("numerchk") and flow_cont("micsam2")) = 1,

option decimals = 6 ;

 SAM2CHK(sac,sacp)$SAM1(sac,sacp)
                    = SAM2(sac,sacp)/SAM1(sac,sacp) ;

 DIFFSAM2(sac,sacp) = SAM1(sac,sacp)-SAM2(sac,sacp) ;

 DIFFSAM2(sac,sacp)$(ABS(DIFFSAM2(sac,sacp)) lt mod_cont("toldiffsam"))
                    = 0 ;

 DIFFSAM2("total","total")   = 0.0 ;

 CNTSAM2  = SUM((sac,sacp)$DIFFSAM2(sac,sacp), 1) ;

 OPTION decimals = 6 ;

DISPLAY SAM2CHK, DIFFSAM2, CNTSAM2 ;

 OPTION decimals = 3 ;

ABORT $(SUM((sac,sacp),ABS(DIFFSAM2(sac,sacp))) gt 0.00000001)
          "SAM2 does not match SAM1" ;

  ) ;

* No totals check required since MICROSAM2 has already been checked

* -------------- end of smod5 sam2 checks --------------------------------
