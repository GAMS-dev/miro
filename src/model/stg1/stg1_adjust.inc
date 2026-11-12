*--------------- stg1 DATA adjustments -----------------------------------

* --- 1a. Remove all trade and transport margins

* This version allows each margin to have its own use proportions

PARAMETER
 TRADEINC(a,c)    prorata increase in a-c block to remove trade margins    ;

LOOP(m,

   TRADEINC(a,c) = 0 ;

   TRADEINC(a,c)$SAM(a,c) = SAM(m,c) * (SAM(a,c) / SUM(ap, SAM(ap,c))) ;
   SAM(a,c)               = SAM(a,c) + TRADEINC(a,c) ;
   SAM(c,a)$SAM(c,m)      = SAM(c,a) + ( SUM(cp, TRADEINC(a,cp)) *
                                         (SAM(c,m)/SUM(cp, SAM(cp,m)))  );

   SAM(m,c) = 0 ;
   SAM(c,m) = 0 ;

 ) ;

* --- 1b. South Africa specific adjustments

* This section is customised to the South African SAM, adjusting it to fit
* the model requirements

* Adjusting for taxes on commodities

 SAM("saltax",c)      = SAM("saltax",c) + SAM("vattax",c) ;

 SAM("govt","saltax") = SUM(c,SAM("saltax",c)) ;

 SAM("vattax",c)      = 0.0 ;
 SAM("govt","vattax") = 0.0 ;

* Adjusting for taxes on activities

* Adjusting for taxes on households and enterprises


* ----------- end of stg1 DATA adjustments -------------------------------
