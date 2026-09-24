*------------------ stg1 data diagnostics --------------------------------
$ontext
    *---- 1. Generating MacroSAM from final model SAM
          Programme ABORTS if MacroSAM does not balance
    *---- 2. Identifying illegal entries and aborting if present
          If illegal entries Programme ABORTS and ids illegal entry
    *---- 3. Checking for Illegal Negative SAM Entries
          If negative entries Programme ABORTS and ids illegal entry
    *---- 4. Checking for Unmatched Taxes
    *---- 3. Generating Summary Statistics
          Summary statisitics for model data
$offtext
*---- 1. Generating MacroSAM from final model SAM ------------------------

put log 'Validating data...'/;

PARAMETERS

* Macro SAM for checking calibration and initial solve

 ASAM0(ss,ss)   Aggregate SAM for data entry check
 ASAM1(ss,ss)   Macro Social Accounting Matrix for calibration
 ASAM2(ss,ss)   Macro Social Accounting Matrix for model equations      ;

* Macro SAM to check data entry

 ASAM0("COMMDTY","COMMDTY")   = SUM((c,cp),SAM(c,cp)) ;
 ASAM0("COMMDTY","ACTIVITY")  = SUM((c,a),SAM(c,a)) ;
 ASAM0("COMMDTY","VALUAD")    = SUM((c,f),SAM(c,f)) ;
 ASAM0("COMMDTY","HHOLDS")    = SUM((c,h),SAM(c,h)) ;
 ASAM0("COMMDTY","ENTP")      = SUM((c,e),SAM(c,e)) ;
 ASAM0("COMMDTY","GOVTN")     = SUM((c,g),SAM(c,g)) ;
 ASAM0("COMMDTY","KAPITAL")   = SUM((c,i),SAM(c,i)) ;
 ASAM0("COMMDTY","WORLD")     = SUM((c,w),SAM(c,w)) ;

 ASAM0("ACTIVITY","COMMDTY")  = SUM((a,c),SAM(a,c)) ;
 ASAM0("ACTIVITY","ACTIVITY") = SUM((a,ap),SAM(a,ap)) ;
 ASAM0("ACTIVITY","VALUAD")   = SUM((a,f),SAM(a,f)) ;
 ASAM0("ACTIVITY","HHOLDS")   = SUM((a,h),SAM(a,h)) ;
 ASAM0("ACTIVITY","ENTP")     = SUM((a,e),SAM(a,e)) ;
 ASAM0("ACTIVITY","GOVTN")    = SUM((a,g),SAM(a,g)) ;
 ASAM0("ACTIVITY","KAPITAL")  = SUM((a,i),SAM(a,i)) ;
 ASAM0("ACTIVITY","WORLD")    = SUM((a,w),SAM(a,w)) ;

 ASAM0("VALUAD","COMMDTY")    = SUM((f,c),SAM(f,c)) ;
 ASAM0("VALUAD","ACTIVITY")   = SUM((f,a),SAM(f,a)) ;
 ASAM0("VALUAD","VALUAD")     = SUM((f,fp),SAM(f,fp)) ;
 ASAM0("VALUAD","HHOLDS")     = SUM((f,h),SAM(f,h)) ;
 ASAM0("VALUAD","ENTP")       = SUM((f,e),SAM(f,e)) ;
 ASAM0("VALUAD","GOVTN")      = SUM((f,g),SAM(f,g)) ;
 ASAM0("VALUAD","KAPITAL")    = SUM((f,i),SAM(f,i)) ;
 ASAM0("VALUAD","WORLD")      = SUM((f,w),SAM(f,w)) ;

 ASAM0("HHOLDS","COMMDTY")    = SUM((h,c),SAM(h,c)) ;
 ASAM0("HHOLDS","ACTIVITY")   = SUM((h,a),SAM(h,a)) ;
 ASAM0("HHOLDS","VALUAD")     = SUM((h,f),SAM(h,f)) ;
 ASAM0("HHOLDS","HHOLDS")     = SUM((h,hp),SAM(h,hp)) ;
 ASAM0("HHOLDS","ENTP")       = SUM((h,e),SAM(h,e)) ;
 ASAM0("HHOLDS","GOVTN")      = SUM((h,g),SAM(h,g)) ;
 ASAM0("HHOLDS","KAPITAL")    = SUM((h,i),SAM(h,i)) ;
 ASAM0("HHOLDS","WORLD")      = SUM((h,w),SAM(h,w)) ;

 ASAM0("ENTP","COMMDTY")      = SUM((e,c),SAM(e,c)) ;
 ASAM0("ENTP","ACTIVITY")     = SUM((e,a),SAM(e,a)) ;
 ASAM0("ENTP","VALUAD")       = SUM((e,f),SAM(e,f)) ;
 ASAM0("ENTP","HHOLDS")       = SUM((e,hp),SAM(e,hp)) ;
 ASAM0("ENTP","ENTP")         = SUM((e,ep),SAM(e,ep)) ;
 ASAM0("ENTP","GOVTN")        = SUM((e,g),SAM(e,g)) ;
 ASAM0("ENTP","KAPITAL")      = SUM((e,i),SAM(e,i)) ;
 ASAM0("ENTP","WORLD")        = SUM((e,w),SAM(e,w)) ;

 ASAM0("GOVTN","COMMDTY")     = SUM((g,c),SAM(g,c)) ;
 ASAM0("GOVTN","ACTIVITY")    = SUM((g,a),SAM(g,a)) ;
 ASAM0("GOVTN","VALUAD")      = SUM((g,f),SAM(g,f)) ;
 ASAM0("GOVTN","HHOLDS")      = SUM((g,h),SAM(g,h)) ;
 ASAM0("GOVTN","ENTP")        = SUM((g,e),SAM(g,e)) ;
* ASAM0("GOVTN","GOVTN")       = SUM((g,gp),SAM(g,gp)) ;
 ASAM0("GOVTN","KAPITAL")     = SUM((g,i),SAM(g,i)) ;
 ASAM0("GOVTN","WORLD")       = SUM((g,w),SAM(g,w)) ;

 ASAM0("KAPITAL","COMMDTY")   = SUM((i,c),SAM(i,c)) ;
 ASAM0("KAPITAL","ACTIVITY")  = SUM((i,a),SAM(i,a)) ;
 ASAM0("KAPITAL","VALUAD")    = SUM((i,f),SAM(i,f)) ;
 ASAM0("KAPITAL","HHOLDS")    = SUM((i,h),SAM(i,h)) ;
 ASAM0("KAPITAL","ENTP")      = SUM((i,e),SAM(i,e)) ;
 ASAM0("KAPITAL","GOVTN")     = SUM((i,g),SAM(i,g)) ;
* ASAM0("KAPITAL","KAPITAL")   = SUM((i,ip),SAM(i,ip)) ;
 ASAM0("KAPITAL","WORLD")     = SUM((i,w),SAM(i,w)) ;

 ASAM0("WORLD","COMMDTY")     = SUM((w,c),SAM(w,c)) ;
 ASAM0("WORLD","ACTIVITY")    = SUM((w,a),SAM(w,a)) ;
 ASAM0("WORLD","VALUAD")      = SUM((w,f),SAM(w,f)) ;
 ASAM0("WORLD","HHOLDS")      = SUM((w,h),SAM(w,h)) ;
 ASAM0("WORLD","ENTP")        = SUM((w,e),SAM(w,e)) ;
 ASAM0("WORLD","GOVTN")       = SUM((w,g),SAM(w,g)) ;
 ASAM0("WORLD","KAPITAL")     = SUM((w,i),SAM(w,i)) ;
 ASAM0("WORLD","WORLD")       = SUM((w,wp),SAM(w,wp)) ;

 ASAM0("TOTALS",ssnp)         = SUM(ssn,ASAM0(ssn,ssnp)) ;
 ASAM0(ssn,"TOTALS")          = SUM(ssnp,ASAM0(ssn,ssnp)) ;

PARAMETER CHECK0    Differences in Row and Column totals of ASAM1  ;

CHECK0 = SUM(ssn,(ABS(ASAM0(ssn,"TOTALS")-ASAM0("TOTALS",ssn)))) ;

put log '  1. Checking generation of MacroSAM from final model SAM... ';
if(CHECK0 GT 0.000005,
  put log / 'ASAM0 ROW AND COLUMN SUMS NOT EQUAL!' /;
  abort "ASAM0 ROW AND COLUMN SUMS NOT EQUAL", CHECK0, ASAM0 ;
else
  put log 'Data OK'/;
);

OPTION DECIMALS=6 ;

DISPLAY CHECK0, ASAM0 ;

*---- 2. Identifying illegal entries and aborting if present -------------
put log '  2. Identifying illegal entries... ';
if(ASAM0("COMMDTY","COMMDTY") NE 0.0,
  put log / 'ASAM0(COMMDTY,COMMDTY) non zero' /;
  abort "ASAM0(COMMDTY,COMMDTY) non zero";
);

if(ASAM0("COMMDTY","VALUAD") NE 0.0,
  put log / 'ASAM0(COMMDTY,VALUAD) non zero' /;
  abort "ASAM0(COMMDTY,VALUAD) non zero";
);

if(ASAM0("ACTIVITY","ACTIVITY") NE 0.0,
  put log / 'ASAM0(ACTIVITY,ACTIVITY) non zero' /;
  abort "ASAM0(ACTIVITY,ACTIVITY) non zero";
);

if(ASAM0("ACTIVITY","VALUAD") NE 0.0,
  put log / 'ASAM0(ACTIVITY,VALUAD) non zero' /;
  abort "ASAM0(ACTIVITY,VALUAD) non zero";
);

if(ASAM0("ACTIVITY","HHOLDS") NE 0.0,
  put log / 'ASAM0(ACTIVITY,HHOLDS) non zero' /;
  abort "ASAM0(ACTIVITY,HHOLDS) non zero";
);

if(ASAM0("ACTIVITY","ENTP") NE 0.0,
  put log / 'ASAM0(ACTIVITY,ENTP) non zero' /;
  abort "ASAM0(ACTIVITY,ENTP) non zero";
);

if(ASAM0("ACTIVITY","GOVTN") NE 0.0,
  put log / 'ASAM0(ACTIVITY,GOVTN) non zero' /;
  abort "ASAM0(ACTIVITY,GOVTN) non zero";
);

if(ASAM0("ACTIVITY","KAPITAL") NE 0.0,
  put log / 'ASAM0(ACTIVITY,KAPITAL) non zero' /;
  abort "ASAM0(ACTIVITY,KAPITAL) non zero";
);

if(ASAM0("ACTIVITY","WORLD") NE 0.0,
  put log / 'ASAM0(ACTIVITY,WORLD) non zero' /;
  abort "ASAM0(ACTIVITY,WORLD) non zero";
);

if(ASAM0("VALUAD","COMMDTY") NE 0.0,
  put log / 'ASAM0(VALUAD,COMMDTY) non zero' /;
   abort "ASAM0(VALUAD,COMMDTY) non zero";
);

if(ASAM0("VALUAD","VALUAD") NE 0.0,
  put log / 'ASAM0(VALUAD,VALUAD) non zero' /;
  abort "ASAM0(VALUAD,VALUAD) non zero";
);

if (ASAM0("VALUAD","HHOLDS") NE 0.0,
  put log / 'ASAM0(VALUAD,HHOLDS) non zero' /;
  abort "ASAM0(VALUAD,HHOLDS) non zero";
);

if (ASAM0("VALUAD","ENTP") NE 0.0,
  put log / 'ASAM0(ENTP,HHOLDS) non zero' /;
  abort "ASAM0(ENTP,HHOLDS) non zero";
);

if (ASAM0("VALUAD","GOVTN") NE 0.0,
  put log / 'ASAM0(VALUAD,GOVTN) non zero' /;
  abort "ASAM0(VALUAD,GOVTN) non zero";
);

if (ASAM0("VALUAD","KAPITAL") NE 0.0,
  put log / 'ASAM0(VALUAD,KAPITAL) non zero' /;
  abort "ASAM0(VALUAD,KAPITAL) non zero";
);

if (ASAM0("HHOLDS","COMMDTY") NE 0.0,
  put log / 'ASAM0(HHOLDS,COMMDTY) non zero' /;
  abort "ASAM0(HHOLDS,COMMDTY) non zero";
);

if (ASAM0("HHOLDS","ACTIVITY") NE 0.0,
  put log / 'ASAM0(HHOLDS,ACTIVITY) non zero' /;
  abort "ASAM0(HHOLDS,ACTIVITY) non zero";
);

if (ASAM0("HHOLDS","KAPITAL") NE 0.0,
  put log / 'ASAM0(HHOLDS,KAPITAL) non zero' /;
  abort "ASAM0(HHOLDS,KAPITAL) non zero";
);

if (ASAM0("ENTP","COMMDTY") NE 0.0,
  put log / 'ASAM0(ENTP,COMMDTY) non zero' /;
  abort "ASAM0(ENTP,COMMDTY) non zero";
);

if (ASAM0("ENTP","ACTIVITY") NE 0.0,
  put log / 'ASAM0(ENTP,ACTIVITY) non zero' /;
  abort "ASAM0(ENTP,ACTIVITY) non zero";
);

if (ASAM0("ENTP","HHOLDS") NE 0.0,
  put log / 'ASAM0(ENTP,HHOLDS) non zero' /;
  abort "ASAM0(ENTP,HHOLDS) non zero";
);

if (ASAM0("ENTP","ENTP") NE 0.0,
  put log / 'ASAM0(ENTERP,ENTERP) non zero' /;
  abort "ASAM0(ENTERP,ENTERP) non zero";
);

if (ASAM0("ENTP","KAPITAL") NE 0.0,
  put log / 'ASAM0(ENTP,KAPITAL) non zero' /;
  abort "ASAM0(ENTP,KAPITAL) non zero";
);

if (ASAM0("VALUAD","HHOLDS") NE 0.0,
  put log / 'ASAM0(VALUAD,HHOLDS) non zero!' /;
  abort "ASAM0(VALUAD,HHOLDS) non zero";
);

if (ASAM0("VALUAD","ENTP") NE 0.0,
  put log / 'ASAM0(VALUAD,ENTP) non zero!' /;
  abort "ASAM0(VALUAD,ENTP) non zero";
);

if (ASAM0("VALUAD","GOVTN") NE 0.0,
  put log / 'ASAM0(VALUAD,GOVTN) non zero!' /;
  abort "ASAM0(VALUAD,GOVTN) non zero";
);

if (ASAM0("VALUAD","KAPITAL") NE 0.0,
  put log / 'ASAM0(VALUAD,KAPITAL) non zero!' /;
  abort "ASAM0(VALUAD,KAPITAL) non zero";
);

if (ASAM0("HHOLDS","COMMDTY") NE 0.0,
  put log / 'ASAM0(HHOLDS,COMMDTY) non zero!' /;
  abort "ASAM0(HHOLDS,COMMDTY) non zero";
);

if (ASAM0("HHOLDS","ACTIVITY") NE 0.0,
  put log / 'ASAM0(HHOLDS,ACTIVITY) non zero!' /;
  abort "ASAM0(HHOLDS,ACTIVITY) non zero";
);

if (ASAM0("HHOLDS","KAPITAL") NE 0.0,
  put log / 'ASAM0(HHOLDS,KAPITAL) non zero!' /;
  abort "ASAM0(HHOLDS,KAPITAL) non zero";
);

if (ASAM0("ENTP","COMMDTY") NE 0.0,
  put log / 'ASAM0(ENTP,COMMDTY) non zero!' /;
  abort "ASAM0(ENTP,COMMDTY) non zero";
);

if (ASAM0("ENTP","ACTIVITY") NE 0.0,
  put log / 'ASAM0(ENTP,ACTIVITY) non zero!' /;
  abort "ASAM0(ENTP,ACTIVITY) non zero";
);

if (ASAM0("ENTP","HHOLDS") NE 0.0,
  put log / 'ASAM0(ENTP,HHOLDS) non zero!' /;
  abort "ASAM0(ENTP,HHOLDS) non zero";
);

if (ASAM0("ENTP","ENTP") NE 0.0,
  put log / 'ASAM0(ENTERP,ENTERP) non zero!' /;
  abort "ASAM0(ENTERP,ENTERP) non zero";
);

if (ASAM0("ENTP","KAPITAL") NE 0.0,
  put log / 'ASAM0(ENTP,KAPITAL) non zero!' /;
  abort "ASAM0(ENTP,KAPITAL) non zero";
);

if (ASAM0("GOVTN","KAPITAL") NE 0.0,
  put log / 'ASAM0(GOVTN,KAPITAL) non zero!' /;
  abort "ASAM0(GOVTN,KAPITAL) non zero";
);

if (ASAM0("KAPITAL","COMMDTY") NE 0.0,
  put log / 'ASAM0(KAPITAL,COMMDTY) non zero!' /;
  abort "ASAM0(GOVTN,KAPITAL) non zero";
);

* ABORT $(ASAM0("COMMDTY","COMMDTY") NE 0.0)
* "ASAM0(COMMDTY,COMMDTY) non zero" ;
*
* ABORT $(ASAM0("COMMDTY","VALUAD") NE 0.0)
* "ASAM0(COMMDTY,VALUAD) non zero" ;
*
** ABORT $(ASAM0("COMMDTY","ENTP") NE 0.0)
** "ASAM0(COMMDTY,ENTP) non zero" ;
*
* ABORT $(ASAM0("ACTIVITY","ACTIVITY") NE 0.0)
* "ASAM0(ACTIVITY,ACTIVITY) non zero" ;
*
* ABORT $(ASAM0("ACTIVITY","VALUAD") NE 0.0)
* "ASAM0(ACTIVITY,VALUAD) non zero" ;
*
* ABORT $(ASAM0("ACTIVITY","HHOLDS") NE 0.0)
* "ASAM0(ACTIVITY,HHOLDS) non zero" ;
*
* ABORT $(ASAM0("ACTIVITY","ENTP") NE 0.0)
* "ASAM0(ACTIVITY,ENTP) non zero" ;
*
* ABORT $(ASAM0("ACTIVITY","GOVTN") NE 0.0)
* "ASAM0(ACTIVITY,GOVTN) non zero" ;
*
* ABORT $(ASAM0("ACTIVITY","KAPITAL") NE 0.0)
* "ASAM0(ACTIVITY,KAPITAL) non zero" ;
*
* ABORT $(ASAM0("ACTIVITY","WORLD") NE 0.0)
* "ASAM0(ACTIVITY,WORLD) non zero" ;
*
* ABORT $(ASAM0("VALUAD","COMMDTY") NE 0.0)
* "ASAM0(VALUAD,COMMDTY) non zero" ;
*
* ABORT $(ASAM0("VALUAD","VALUAD") NE 0.0)
* "ASAM0(VALUAD,VALUAD) non zero" ;
*
* ABORT $(ASAM0("VALUAD","HHOLDS") NE 0.0)
* "ASAM0(VALUAD,HHOLDS) non zero" ;
*
* ABORT $(ASAM0("VALUAD","ENTP") NE 0.0)
* "ASAM0(ENTP,HHOLDS) non zero" ;
*
* ABORT $(ASAM0("VALUAD","GOVTN") NE 0.0)
* "ASAM0(VALUAD,GOVTN) non zero" ;
*
* ABORT $(ASAM0("VALUAD","KAPITAL") NE 0.0)
* "ASAM0(VALUAD,KAPITAL) non zero" ;
*
* ABORT $(ASAM0("HHOLDS","COMMDTY") NE 0.0)
* "ASAM0(HHOLDS,COMMDTY) non zero" ;
*
* ABORT $(ASAM0("HHOLDS","ACTIVITY") NE 0.0)
* "ASAM0(HHOLDS,ACTIVITY) non zero" ;
*
* ABORT $(ASAM0("HHOLDS","KAPITAL") NE 0.0)
* "ASAM0(HHOLDS,KAPITAL) non zero" ;
*
* ABORT $(ASAM0("ENTP","COMMDTY") NE 0.0)
* "ASAM0(ENTP,COMMDTY) non zero" ;
*
* ABORT $(ASAM0("ENTP","ACTIVITY") NE 0.0)
* "ASAM0(ENTP,ACTIVITY) non zero" ;
*
* ABORT $(ASAM0("ENTP","HHOLDS") NE 0.0)
* "ASAM0(ENTP,HHOLDS) non zero" ;
*
* ABORT $(ASAM0("ENTP","ENTP") NE 0.0)
* "ASAM0(ENTERP,ENTERP) non zero" ;
*
* ABORT $(ASAM0("ENTP","KAPITAL") NE 0.0)
* "ASAM0(ENTP,KAPITAL) non zero" ;
*
** ABORT $(ASAM0("ENTP","WORLD") NE 0.0)
** "ASAM0(ENTP,WORLD) non zero" ;
*
* ABORT $(ASAM0("GOVTN","KAPITAL") NE 0.0)
* "ASAM0(GOVTN,KAPITAL) non zero" ;
*
* ABORT $(ASAM0("KAPITAL","COMMDTY") NE 0.0)
* "ASAM0(KAPITAL,COMMDTY) non zero" ;
*
* ABORT $(ASAM0("KAPITAL","ACTIVITY") NE 0.0)
* "ASAM0(KAPITAL,ACTIVITY) non zero" ;
*
** ABORT $(ASAM0("KAPITAL","VALUAD") NE 0.0)
** "ASAM0(KAPITAL,VALUAD) non zero" ;
*
* ABORT $(ASAM0("WORLD","ACTIVITY") NE 0.0)
* "ASAM0(WORLD,ACTIVITY) non zero" ;
*
* ABORT $(ASAM0("WORLD","HHOLDS") NE 0.0)
* "ASAM0(WORLD,HHOLDS) non zero" ;
*
* ABORT $(ASAM0("WORLD","ENTP") NE 0.0)
* "ASAM0(WORLD,ENTP) non zero" ;
*
* ABORT $(ASAM0("WORLD","GOVTN") NE 0.0)
* "ASAM0(WORLD,GOVTN) non zero" ;
*
* ABORT $(ASAM0("WORLD","KAPITAL") NE 0.0)
* "ASAM0(WORLD,KAPITAL) non zero" ;
*
* ABORT $(ASAM0("WORLD","WORLD") NE 0.0)
* "ASAM0(WORLD,WORLD) non zero" ;

put log 'Data OK'/;
*---- 3. Checking for Illegal Negative SAM Entries -----------------------

Parameter
 MDnegCNT       Count of negative margin demand
 INTDnegCNT     Count of negative intermediate input demand
 CDnegCNT       Count of negative household demand
 GDnegCNT       Count of negative government demand
 EnegCNT        Count of negative export demand

 MSnegCNT       Count of negative margin supply

 XCSnegCNT      Count of negative commodity supply

 FDnegCNT       Count of negative factor demands

 YHFnegCNT      Count of negative household income from factors

 YEFnegCNT      Count of negative enterprise income from factors

 MnegCNT        Count of negative import demand
   ;

   MDnegCNT = SUM((c,m)$(SAM(c,m) LT 0.0), 1) ;
 INTDnegCNT = SUM((c,a)$(SAM(c,a) LT 0.0), 1) ;
   CDnegCNT = SUM((c,h)$(SAM(c,h) LT 0.0), 1) ;
   GDnegCNT = SUM(c$(SAM(c,"govt") LT 0.0), 1) ;
    EnegCNT = SUM((c,w)$(SAM(c,w) LT 0.0), 1) ;

   MSnegCNT = SUM((m,c)$(SAM(m,c) LT 0.0), 1) ;

  XCSnegCNT = SUM((a,c)$(SAM(a,c) LT 0.0), 1) ;

   FDnegCNT = SUM((f,a)$(SAM(f,a) LT 0.0), 1) ;

  YHFnegCNT = SUM((h,f)$(SAM(h,f) LT 0.0), 1) ;

  YEFnegCNT = SUM((e,f)$(SAM(e,f) LT 0.0), 1) ;

    MnegCNT = SUM((w,c)$(SAM(w,c) LT 0.0), 1) ;

put log '  3. Checking for Illegal Negative SAM Entries... ';

if(MDnegCNT NE 0.0,
  put log 'negative margin demand!' /;
  abort "negative margin demand";
);

if(INTDnegCNT NE 0.0,
  put log 'negative intermediate input demand!' /;
  abort "negative intermediate input demand";
);

if(CDnegCNT NE 0.0,
  put log 'negative household demand!' /;
  abort "negative household demand";
);

if(GDnegCNT NE 0.0,
  put log 'negative government demand!' /;
  abort "negative government demand";
);

if(EnegCNT NE 0.0,
  put log 'negative export demand!' /;
  abort "negative export demand";
);

if(MSnegCNT NE 0.0,
  put log 'negative margin supply!' /;
  abort "negative margin supply";
);

if(XCSnegCNT NE 0.0,
  put log 'negative commodity supply!' /;
  abort "negative commodity supply";
);

if(FDnegCNT NE 0.0,
  put log 'negative factor demands!' /;
  abort "negative factor demands";
);

if(YHFnegCNT NE 0.0,
  put log 'negative household income from factors!' /;
  abort "negative household income from factors";
);

if(YEFnegCNT NE 0.0,
  put log 'negative enterprise income from factors!' /;
  abort "negative enterprise income from factors";
);

if(MnegCNT NE 0.0,
  put log 'negative import demand!' /;
  abort "negative import demand";
);
put log 'Data OK'/;
* ABORT $(MDnegCNT NE 0.0) "negative margin demand" ;
* ABORT $(INTDnegCNT NE 0.0) "negative intermediate input demand" ;
* ABORT $(CDnegCNT NE 0.0) "negative household demand" ;
* ABORT $(GDnegCNT NE 0.0) "negative government demand" ;
* ABORT $(EnegCNT NE 0.0) "negative export demand" ;
*
* ABORT $(MSnegCNT NE 0.0) "negative margin supply" ;
*
* ABORT $(XCSnegCNT NE 0.0) "negative commodity supply" ;
*
* ABORT $(FDnegCNT NE 0.0) "negative factor demands" ;
*
* ABORT $(YHFnegCNT NE 0.0) "negative household income from factors" ;
*
* ABORT $(YEFnegCNT NE 0.0) "negative enterprise income from factors" ;
*
* ABORT $(MnegCNT NE 0.0) "negative import demand" ;

*---- 4. Checking for Unmatched Taxes ------------------------------------

*---- 5. Generating Summary Statistics -----------------------------------

PARAMETER
 MAXTV        Maximum absolute value of transaction values in SAM
 MINTV        Maximum absolute value of transaction values in SAM
 TVRATIO      Ratio of max and min transaction values in SAM ;

 MAXTV      = SMAX((SAC,SACP),ABS (SAM(SAC,SACP))) ;
 MINTV      = SMIN((SAC,SACP)$SAM(SAC,SACP),ABS (SAM(SAC,SACP))) ;
 TVRATIO    = MAXTV/MINTV ;

 option decimals = 5 ;

 DISPLAY MAXTV, MINTV, TVRATIO ;

 option decimals=3 ;


put log 'Done.'/;
*------------------ end of stg1 data diagnostics -------------------------
