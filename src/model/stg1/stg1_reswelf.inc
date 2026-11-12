* ----------------- stg1 welfare results ---------------------------------

* Per capita household incomes and consumption expenditures

 %res%YH_cap(h,%res_dimen%)          = YH.L(h)/pop(h) ;

 %res%HEXP_cap(h,%res_dimen%)        = HEXP.L(h)/pop(h) ;

* resYH_cap(h,%res_dimen%)$YH_cap0(h)
*                                = (resYH_cap(h,%res_dimen%)/YH_cap0(h)-1)*100 ;

* resHEXP_cap(h,%res_dimen%)$HEXP_cap0(h)
*                                 = (resHEXP_cap(h,%res_dimen%)/HEXP_cap0(h)-1)*100 ;


*--------- SUMMARY WELFARE MEASURES -------------

 %res%EV(h,%res_dimen%) = PROD(c,(PQD0(c)/resPQD(c,%res_dimen%))**beta(c,h))
                      *(%res%HEXP(h,%res_dimen%)
                          -SUM(c,qcdconst(c,h)*%res%PQD(c,%res_dimen%)))
                      -(HEXP0(h)-SUM(c,qcdconst(c,h)
                      * PQD0(c))) ;


 %res%EV_cap(h,%res_dimen%)$(pop(h))
                   = %res%EV(h,%res_dimen%)/pop(h) ;

$ontext
IC and EV formulas from Blonigen, Bruce A., Joseph E. Flynn, and
Kenneth A. Reinert (1997) Sector-Focused General Equilibrium Modeling,
in eds. Joseph F. Francois and Kenneth A. Reinert, Applied Methods
for Trade Policy Analysis. Cambridge, MA: Cambridge University Press,
pp. 189-230.
$offtext

 %res%IC(h,%res_dimen%)  = ((%res%HEXP(h,%res_dimen%)
                         - SUM(c,%res%PQD(c,%res_dimen%)*qcdconst(c,h)))
                              /(PROD(c,%res%PQD(c,%res_dimen%)**beta(c,h))))
                                 * (PROD(c,PQD0(c)**beta(c,h)))
                                   + (SUM(c,PQD0(c)*qcdconst(c,h))) ;

 %res%IC_cap(h,%res_dimen%)$(pop(h))
                    =  %res%IC(h,%res_dimen%)/pop(h) ;

 %res%EV_alt(h,%res_dimen%)
                    = %res%IC(h,%res_dimen%) - HEXP0(h) ;

 %res%EV_alt_cap(h,%res_dimen%)$(pop(h))
                    = %res%EV_alt(h,%res_dimen%)/pop(h) ;

*--------- SLUTSKY WELFARE MEASURES -------------

 %res%EV_H_slut(h,%res_dimen%)
                     = SUM(c,PQD0(c) * %res%QCD(c,h,%res_dimen%))
                         - HEXP0(h) ;

 %res%EV_R_slut(%res_dimen%)
                     = SUM(c,PQD0(c) * (SUM(h,%res%QCD(c,h,%res_dimen%)
                               + %res%QGD(c,%res_dimen%)
                               + %res%QINVD(c,%res_dimen%))))
                         - (SUM(h,HEXP0(h))
                           + SUM(c,PQD0(c) * (QGD0(c) + QINVD0(c)))) ;


* ------- Exporting Welfare Results to GDX -------------------------------

* This writes the results data to GDX
$ifThen.MIRORUN "x%gams.IDCGDXInput%"=="x"
Execute_Unload  'results/stg1_reswelf.gdx',
                                     %res%YH_cap
                                     %res%HEXP_cap
                                     %res%EV
                                     %res%EV_cap
                                     %res%IC
                                     %res%IC_cap
                                     %res%EV_alt
                                     %res%EV_alt_cap
                                     %res%EV_H_slut
                                     %res%EV_R_slut

  ;
$endIf.MIRORUN
* ----------------- end of stg1 welfare results --------------------------
