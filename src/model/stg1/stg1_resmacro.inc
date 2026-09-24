* ----------------- stg1 Macroeconomic Tables ----------------------------
$ontext
The layout of this files is

   * ------- 1. Assign Macroeconomic Totals
             *---- 1a. Nominal Macroeconomic Totals
             *---- 1b. Real Macroeconomic Totals
   * ------- 2. Assign Macroeconomic Shares
             *---- 2a. Nominal Macroeconomic Shares in GDPEXP
             *---- 2b. Nominal Macroeconomic Shares in ABSORP
   * ------- 3. Assign Percentage Changes in Macroeconomic Totals
             *---- 3a. Percentage Change in Real Macroeconomic Totals
             *---- 3b. Percentage Change in Nominal Macroeconomic Totals
   * ------- 4. Exporting Macroeconomic Tables to GDX
$offtext
* ------- Exporting Macroeconomic Tables to GDX -----------------------------

* ------- 1. Assign Macroeconomic Totals ---------------------------------

*---- 1a. Nominal Macroeconomic Totals

* Value of household consumption
 nMACROTOT("stQCDTOT",%res_dimen%)
            = SUM((c,h),resPQD(c,%res_dimen%) * resQCD(c,h,%res_dimen%)) ;

* Value of enterprise consumption
 nMACROTOT("stQEDTOT",%res_dimen%)
            = SUM((c,e),resPQD(c,%res_dimen%) * resQED(c,e,%res_dimen%)) ;

* Value of government consumption
 nMACROTOT("stQGDTOT",%res_dimen%)
            = SUM(c,resPQD(c,%res_dimen%) * resQGD(c,%res_dimen%)) ;

* Value of investment consumption
 nMACROTOT("stQINVTOT",%res_dimen%)
            = SUM(c,resPQD(c,%res_dimen%) * resQINVD(c,%res_dimen%)) ;

* Value of Domestic final demand
 nMACROTOT("stQABSORP",%res_dimen%)
            = SUM((c,h),resPQD(c,%res_dimen%) * resQCD(c,h,%res_dimen%))
              + SUM((c,e),resPQD(c,%res_dimen%) * resQED(c,e,%res_dimen%))
              + SUM(c,resPQD(c,%res_dimen%) * resQGD(c,%res_dimen%))
              + SUM(c,resPQD(c,%res_dimen%) * resQINVD(c,%res_dimen%)) ;

* Value of import demand
 nMACROTOT("stIMPORT",%res_dimen%)
            = SUM(c,resPM(c,%res_dimen%) * resQM(c,%res_dimen%)) ;

* Value of export supply
 nMACROTOT("stEXPORT",%res_dimen%)
            = SUM(c,resPE(c,%res_dimen%) * resQE(c,%res_dimen%)) ;

* GDP from Income side - total value added plus indirect taxes on prodn
 nMACROTOT("stGDPVA",%res_dimen%)
            = SUM((f,a),resWF(f,%res_dimen%) * resWFDIST(f,a,%res_dimen%)
                      * resFD(f,a,%res_dimen%))
                      + resSCAL("sMTAX",%res_dimen%)
                      + resSCAL("sETAX",%res_dimen%)
                      + resSCAL("sSTAX",%res_dimen%)
                      + resSCAL("sEXTAX",%res_dimen%)
                      + resSCAL("sITAX",%res_dimen%)
                      + resSCAL("sFTAX",%res_dimen%) ;

* GDP from Expenditure side - C + I + G + E - M
 nMACROTOT("stGDPEXP",%res_dimen%)
            = SUM((c,h),resPQD(c,%res_dimen%) * resQCD(c,h,%res_dimen%))
                + SUM(c,resPQD(c,%res_dimen%) * resQGD(c,%res_dimen%))
                + SUM((c,e),resPQD(c,%res_dimen%) * resQED(c,e,%res_dimen%))
                + SUM(c,resPQD(c,%res_dimen%) * resQINVD(c,%res_dimen%))
                + SUM(c,resPE(c,%res_dimen%) * resQE(c,%res_dimen%))
                - SUM(c,resPM(c,%res_dimen%) * resQM(c,%res_dimen%)) ;

* total domestic production
 nMACROTOT("stSUPPLY",%res_dimen%)
            = SUM(c,resPXC(c,%res_dimen%)* resQXC(c,%res_dimen%)) ;

* total intermediate inputs
 nMACROTOT("stUSE",%res_dimen%)
            = SUM(c,resPQD(c,%res_dimen%)* resQINTD(c,%res_dimen%)) ;

* household income
 nMACROTOT("stYHTOT",%res_dimen%)
            = SUM(h,resYH(h,%res_dimen%)) ;

* government income
 nMACROTOT("stYGTOT",%res_dimen%)      = resSCAL("sYG",%res_dimen%) ;

* factor income
 nMACROTOT("stYFTOT",%res_dimen%)      = SUM(f,resYF(f,%res_dimen%)) ;

* distributed factor income
 nMACROTOT("stYFDISPTOT",%res_dimen%)  = SUM(f,resYFDISP(f,%res_dimen%)) ;

* Labour income
 nMACROTOT("stYFLABTOT",%res_dimen%)   = SUM(l,resYF(l,%res_dimen%)) ;

* total savings
 nMACROTOT("stTOTSAV",%res_dimen%)     = resSCAL("sTOTSAV",%res_dimen%) ;

* household savings
 nMACROTOT("stHOSAV",%res_dimen%)
            = SUM(h,resYH(h,%res_dimen%) * (1-resTYH(h,%res_dimen%))
                 * resSHH(h,%res_dimen%)) ;

* government savings
 nMACROTOT("stGOVSAV",%res_dimen%)     = resSCAL("sKAPGOV",%res_dimen%) ;

* foreign savings
 nMACROTOT("stFSAV",%res_dimen%)       = (resSCAL("sCAPWOR",%res_dimen%)
                                         *resSCAL("sER",%res_dimen%)) ;

* Depreciation
 nMACROTOT("stDEPREC",%res_dimen%)     = SUM(f,deprec(f)*resYF(f,%res_dimen%)) ;

* Import tariff revenue
 nMACROTOT("stMTAX",%res_dimen%)       = resSCAL("sMTAX",%res_dimen%) ;

* Export tax revenue
 nMACROTOT("stETAX",%res_dimen%)       = resSCAL("sETAX",%res_dimen%) ;

* Sales tax revenue
 nMACROTOT("stSTAX",%res_dimen%)       = resSCAL("sSTAX",%res_dimen%) ;

* Excise tax revenue
 nMACROTOT("stEXTAX",%res_dimen%)      = resSCAL("sEXTAX",%res_dimen%) ;

* Indirect tax revenue
 nMACROTOT("stITAX",%res_dimen%)       = resSCAL("sITAX",%res_dimen%) ;

* Factor use tax revenue
 nMACROTOT("stFTAX",%res_dimen%)       = resSCAL("sFTAX",%res_dimen%) ;

* Factor income tax revenue
 nMACROTOT("stFYTAX",%res_dimen%)      = resSCAL("sFYTAX",%res_dimen%) ;

* Direct income tax revenue
 nMACROTOT("stDTAX",%res_dimen%)       = resSCAL("sDTAX",%res_dimen%) ;

* Total tax revenue
 nMACROTOT("stTOTTAX",%res_dimen%)     = resSCAL("sMTAX",%res_dimen%)
                                      + resSCAL("sETAX",%res_dimen%)
                                      + resSCAL("sSTAX",%res_dimen%)
                                      + resSCAL("sEXTAX",%res_dimen%)
                                      + resSCAL("sITAX",%res_dimen%)
                                      + resSCAL("sFTAX",%res_dimen%)
                                      + resSCAL("sFYTAX",%res_dimen%)
                                      + resSCAL("sDTAX",%res_dimen%) ;

*---- 1b. Real Macroeconomic Totals

* Value of household consumption
 rMACROTOT("stQCDTOT",%res_dimen%)
            = SUM((c,h),PQD0(c) * resQCD(c,h,%res_dimen%)) ;

* Value of enterprise consumption
 nMACROTOT("stQEDTOT",%res_dimen%)
            = SUM((c,e),PQD0(c) * resQED(c,e,%res_dimen%)) ;

* Value of government consumption
 rMACROTOT("stQGDTOT",%res_dimen%)
            = SUM(c,PQD0(c) * resQGD(c,%res_dimen%)) ;

* Value of investment consumption
 rMACROTOT("stQINVTOT",%res_dimen%)
            = SUM(c,PQD0(c) * resQINVD(c,%res_dimen%)) ;

* Value of Domestic final demand
 rMACROTOT("stQABSORP",%res_dimen%)
            = SUM((c,h),PQD0(c) * resQCD(c,h,%res_dimen%))
              + SUM((c,e),PQD0(c) * resQED(c,e,%res_dimen%))
              + SUM(c,PQD0(c) * resQGD(c,%res_dimen%))
              + SUM(c,PQD0(c) * resQINVD(c,%res_dimen%)) ;

* Value of import demand
 rMACROTOT("stIMPORT",%res_dimen%)
            = SUM(c,PM0(c) * resQM(c,%res_dimen%)) ;

* Value of export supply
 rMACROTOT("stEXPORT",%res_dimen%)
            = SUM(c,PE0(c) * resQE(c,%res_dimen%)) ;

* GDP from Income side - total value added plus indirect taxes
 rMACROTOT("stGDPVA",%res_dimen%)
            = SUM((f,a),WF0(f) * WFDIST0(f,a)
                      * resFD(f,a,%res_dimen%))
                      + MTAX0 + ETAX0 + STAX0 + EXTAX0 + ITAX0 + FTAX0 ;

* GDP from Expenditure side - C + I + G + E - M
 rMACROTOT("stGDPEXP",%res_dimen%)
            = SUM((c,h),PQD0(c) * resQCD(c,h,%res_dimen%))
                + SUM((c,e),PQD0(c) * resQED(c,e,%res_dimen%))
                + SUM(c,PQD0(c) * resQGD(c,%res_dimen%))
                + SUM(c,PQD0(c) * resQINVD(c,%res_dimen%))
                + SUM(c,PE0(c) * resQE(c,%res_dimen%))
                - SUM(c,PM0(c) * resQM(c,%res_dimen%)) ;

* total domestic production
 rMACROTOT("stSUPPLY",%res_dimen%)
            = SUM(c,PXC0(c)* resQXC(c,%res_dimen%)) ;

* total intermediate inputs
 rMACROTOT("stUSE",%res_dimen%)
            = SUM(c,PQD0(c)* resQINTD(c,%res_dimen%)) ;

* ------- 2. Assign Macroeconomic Shares ---------------------------------

*---- 2a. Nominal Macroeconomic Shares in GDPEXP

* Share of household consumption in GDPEXP
 nMACROSH1("stQCDTOT",%res_dimen%)$nMACROTOT("stGDPEXP",%res_dimen%)
                         = SUM((c,h),resPQD(c,%res_dimen%) * resQCD(c,h,%res_dimen%))
                                /nMACROTOT("stGDPEXP",%res_dimen%) ;

* Share of enterprise consumption in GDPEXP
 nMACROSH1("stQEDTOT",%res_dimen%)$nMACROTOT("stGDPEXP",%res_dimen%)
            = SUM((c,e),resPQD(c,%res_dimen%) * resQED(c,e,%res_dimen%))
                                /nMACROTOT("stGDPEXP",%res_dimen%) ;

* Share of government consumption in GDPEXP
 nMACROSH1("stQGDTOT",%res_dimen%)$nMACROTOT("stGDPEXP",%res_dimen%)
            = SUM(c,resPQD(c,%res_dimen%) * resQGD(c,%res_dimen%))
                                /nMACROTOT("stGDPEXP",%res_dimen%) ;

* Share of investment consumption in GDPEXP
 nMACROSH1("stQINVTOT",%res_dimen%)$nMACROTOT("stGDPEXP",%res_dimen%)
            = SUM(c,resPQD(c,%res_dimen%) * resQINVD(c,%res_dimen%))
                                /nMACROTOT("stGDPEXP",%res_dimen%) ;

* Ratio of import demand to GDPEXP
 nMACROSH1("stIMPORT",%res_dimen%)$nMACROTOT("stGDPEXP",%res_dimen%)
            = SUM(c,resPM(c,%res_dimen%) * resQM(c,%res_dimen%))
                                /nMACROTOT("stGDPEXP",%res_dimen%) ;

* Ratio of export supply to GDPEXP
 nMACROSH1("stEXPORT",%res_dimen%)$nMACROTOT("stGDPEXP",%res_dimen%)
            = SUM(c,resPE(c,%res_dimen%) * resQE(c,%res_dimen%))
                                /nMACROTOT("stGDPEXP",%res_dimen%) ;

*---- 2b. Nominal Macroeconomic Shares in ABSORP

* Share of household consumption in ABSORP
 nMACROSH2("stQCDTOT",%res_dimen%)$nMACROTOT("stQABSORP",%res_dimen%)
            = SUM((c,h),resPQD(c,%res_dimen%) * resQCD(c,h,%res_dimen%))
                                /nMACROTOT("stQABSORP",%res_dimen%) ;

* Share of enterprise consumption in ABSORP
 nMACROSH2("stQEDTOT",%res_dimen%)$nMACROTOT("stQABSORP",%res_dimen%)
            = SUM((c,e),resPQD(c,%res_dimen%) * resQED(c,e,%res_dimen%))
                                /nMACROTOT("stQABSORP",%res_dimen%) ;

* Share of government consumption in ABSORP
 nMACROSH2("stQGDTOT",%res_dimen%)$nMACROTOT("stQABSORP",%res_dimen%)
            = SUM(c,resPQD(c,%res_dimen%) * resQGD(c,%res_dimen%))
                                /nMACROTOT("stQABSORP",%res_dimen%) ;

* Share of investment consumption in ABSORP
 nMACROSH2("stQINVTOT",%res_dimen%)$nMACROTOT("stQABSORP",%res_dimen%)
            = SUM(c,resPQD(c,%res_dimen%) * resQINVD(c,%res_dimen%))
                                /nMACROTOT("stQABSORP",%res_dimen%) ;

* Ration of import demand consumption to ABSORP
 nMACROSH2("stIMPORT",%res_dimen%)$nMACROTOT("stQABSORP",%res_dimen%)
            = SUM(c,resPM(c,%res_dimen%) * resQM(c,%res_dimen%))
                                /nMACROTOT("stQABSORP",%res_dimen%) ;

* Ratio of export supply to ABSORP
 nMACROSH2("stEXPORT",%res_dimen%)$nMACROTOT("stQABSORP",%res_dimen%)
            = SUM(c,resPE(c,%res_dimen%) * resQE(c,%res_dimen%))
                                /nMACROTOT("stQABSORP",%res_dimen%) ;

* ------- 3. Assign Percentage Changes in Macroeconomic Totals -----------

*---- 3a. Percentage Change in Real Macroeconomic Totals

* Real value of household consumption
 rperMACROTOT("stQCDTOT",%res_dimen%)$SUM((c,h),PQD0(c) * QCD0(c,h))
            = (SUM((c,h),PQD0(c) * resQCD(c,h,%res_dimen%))
                  /SUM((c,h),PQD0(c) * QCD0(c,h)) - 1) * 100 ;

* Real value of enterprise consumption
 rperMACROTOT("stQEDTOT",%res_dimen%)$SUM((c,e),PQD0(c) * QED0(c,e))
            = (SUM((c,e),PQD0(c) * resQED(c,e,%res_dimen%))
                  /SUM((c,e),PQD0(c) * QED0(c,e)) - 1) * 100 ;

* Real value of government consumption
 rperMACROTOT("stQGDTOT",%res_dimen%)$SUM(c,PQD0(c) * QGD0(c))
            = (SUM(c,PQD0(c) * resQGD(c,%res_dimen%))
                  /SUM(c,PQD0(c) * QGD0(c)) - 1) * 100 ;

* Real value of investment consumption
 rperMACROTOT("stQINVTOT",%res_dimen%)$SUM(c,PQD0(c) * QINVD0(c))
            = (SUM(c,PQD0(c) * resQINVD(c,%res_dimen%))
                  /SUM(c,PQD0(c) * QINVD0(c)) - 1) * 100 ;

* Real value of Domestic final demand
 rperMACROTOT("stQABSORP",%res_dimen%)$(SUM((c,h),PQD0(c) * QCD0(c,h))
                         + SUM((c,e),PQD0(c) * QED0(c,e))
                         + SUM(c,PQD0(c) * QGD0(c))
                         + SUM(c,PQD0(c) * QINVD0(c)))
                       = ((SUM((c,h),PQD0(c) * resQCD(c,h,%res_dimen%))
                         + SUM((c,e),PQD0(c) * resQED(c,e,%res_dimen%))
                         + SUM(c,PQD0(c) * resQGD(c,%res_dimen%))
                         + SUM(c,PQD0(c) * resQINVD(c,%res_dimen%)))
                         / (SUM((c,h),PQD0(c) * QCD0(c,h))
                         + SUM((c,e),PQD0(c) * QED0(c,e))
                         + SUM(c,PQD0(c) * QGD0(c))
                         + SUM(c,PQD0(c) * QINVD0(c))) - 1) * 100 ;

* Real value of import demand
 rperMACROTOT("stIMPORT",%res_dimen%)$SUM(c,PM0(c) * QM0(c))
                      = (SUM(c,PM0(c) * resQM(c,%res_dimen%))
                         /SUM(c,PM0(c) * QM0(c)) - 1) * 100 ;

* Real value of export supply
 rperMACROTOT("stEXPORT",%res_dimen%)$SUM(c,PE0(c) * QE0(c))
                      = (SUM(c,PE0(c) * resQE(c,%res_dimen%))
                         /SUM(c,PE0(c) * QE0(c)) - 1) * 100 ;

* Real GDP from Income side - total value added plus indirect taxes on prodn
 rperMACROTOT("stGDPVA",%res_dimen%)$(SUM((f,a),WF0(f) * WFDIST0(f,a) * FD0(f,a))
                           + MTAX0 + ETAX0 + STAX0 + EXTAX0 + ITAX0 + FTAX0)
                      = ((SUM((f,a),WF0(f) * WFDIST0(f,a) * resFD(f,a,%res_dimen%))
                        + resSCAL("sMTAX",%res_dimen%) + resSCAL("sETAX",%res_dimen%)
                        + resSCAL("sSTAX",%res_dimen%) + resSCAL("sEXTAX",%res_dimen%)
                        + resSCAL("sITAX",%res_dimen%) + resSCAL("sFTAX",%res_dimen%) )
                          /(SUM((f,a),WF0(f) * WFDIST0(f,a) * FD0(f,a))
                           + MTAX0 + ETAX0 + STAX0 + EXTAX0 + ITAX0 + FTAX0) - 1) * 100 ;

* Real GDP from Expenditure side - C + I + G + E - M
 rperMACROTOT("stGDPEXP",%res_dimen%)$(SUM((c,h),PQD0(c) * QCD0(c,h))
                           + SUM((c,e),PQD0(c) * QED0(c,e))
                           + SUM(c,PQD0(c) * QGD0(c))
                           + SUM(c,PQD0(c) * QINVD0(c))
                           + SUM(c,PE0(c) * QE0(c))
                           - SUM(c,PM0(c) * QM0(c)))
                      = ((SUM((c,h),PQD0(c) * resQCD(c,h,%res_dimen%))
                        + SUM((c,e),PQD0(c) * resQED(c,e,%res_dimen%))
                        + SUM(c,PQD0(c) * resQGD(c,%res_dimen%))
                        + SUM(c,PQD0(c) * resQINVD(c,%res_dimen%))
                        + SUM(c,PE0(c) * resQE(c,%res_dimen%))
                        - SUM(c,PM0(c) * resQM(c,%res_dimen%)))
                          /(SUM((c,h),PQD0(c) * QCD0(c,h))
                           + SUM((c,e),PQD0(c) * QED0(c,e))
                           + SUM(c,PQD0(c) * QGD0(c))
                           + SUM(c,PQD0(c) * QINVD0(c))
                           + SUM(c,PE0(c) * QE0(c))
                           - SUM(c,PM0(c) * QM0(c))) - 1) * 100 ;

* total domestic production
 rperMACROTOT("stSUPPLY",%res_dimen%)$(SUM(c,PXC0(c)* QXC0(c)))
            = ((SUM(c,PXC0(c)* resQXC(c,%res_dimen%)))
                /(SUM(c,PXC0(c)* QXC0(c))) - 1) * 100 ;

* total intermediate inputs
 rperMACROTOT("stUSE",%res_dimen%)$(SUM(c,PQD0(c)* QINTD0(c)))
            = ((SUM(c,PQD0(c)* resQINTD(c,%res_dimen%)))
                /(SUM(c,PQD0(c)* QINTD0(c))) - 1) * 100 ;

*---- 3b. Percentage Change in Nominal Macroeconomic Totals

* Value of household consumption
 nperMACROTOT("stQCDTOT",%res_dimen%)$SUM((c,h),PQD0(c) * QCD0(c,h))
            = (SUM((c,h),resPQD(c,%res_dimen%) * resQCD(c,h,%res_dimen%))
               /SUM((c,h),PQD0(c) * QCD0(c,h)) - 1) * 100 ;

* Value of enterprise consumption
 nperMACROTOT("stQEDTOT",%res_dimen%)$SUM((c,e),PQD0(c) * QED0(c,e))
            = (SUM((c,e),resPQD(c,%res_dimen%) * resQED(c,e,%res_dimen%))
               /SUM((c,e),PQD0(c) * QED0(c,e)) - 1) * 100 ;

* Value of government consumption
 nperMACROTOT("stQGDTOT",%res_dimen%)$SUM(c,PQD0(c) * QGD0(c))
            = (SUM(c,resPQD(c,%res_dimen%) * resQGD(c,%res_dimen%))
               /SUM(c,PQD0(c) * QGD0(c)) - 1) * 100 ;

* Value of investment consumption
 nperMACROTOT("stQINVTOT",%res_dimen%)$SUM(c,PQD0(c) * QINVD0(c))
            = (SUM(c,resPQD(c,%res_dimen%) * resQINVD(c,%res_dimen%))
               /SUM(c,PQD0(c) * QINVD0(c)) - 1) * 100 ;

* Value of Domestic final demand
 nperMACROTOT("stQABSORP",%res_dimen%)$(SUM((c,h),PQD0(c) * QCD0(c,h))
                                       + SUM((c,e),PQD0(c) * QED0(c,e))
                                       + SUM(c,PQD0(c) * QGD0(c))
                                       + SUM(c,PQD0(c) * QINVD0(c)))
            = ((SUM((c,h),resPQD(c,%res_dimen%) * resQCD(c,h,%res_dimen%))
              + SUM((c,e),resPQD(c,%res_dimen%) * resQED(c,e,%res_dimen%))
              + SUM(c,resPQD(c,%res_dimen%) * resQGD(c,%res_dimen%))
              + SUM(c,resPQD(c,%res_dimen%) * resQINVD(c,%res_dimen%)))
                / (SUM((c,h),PQD0(c) * QCD0(c,h))
                   + SUM((c,e),PQD0(c) * QED0(c,e))
                   + SUM(c,PQD0(c) * QGD0(c))
                   + SUM(c,PQD0(c) * QINVD0(c))) - 1) * 100 ;

* Value of import demand
 nperMACROTOT("stIMPORT",%res_dimen%)$SUM(c,PM0(c) * QM0(c))
            = (SUM(c,resPM(c,%res_dimen%) * resQM(c,%res_dimen%))
               /SUM(c,PM0(c) * QM0(c)) - 1) * 100 ;

* Value of export supply
 nperMACROTOT("stEXPORT",%res_dimen%)$SUM(c,PE0(c) * QE0(c))
            = (SUM(c,resPE(c,%res_dimen%) * resQE(c,%res_dimen%))
               /SUM(c,PE0(c) * QE0(c)) - 1) * 100 ;

* GDP from Income side - total value added plus indirect taxes on prodn
 nperMACROTOT("stGDPVA",%res_dimen%)$(SUM((f,a),WF0(f) * WFDIST0(f,a) * FD0(f,a))
                            + MTAX0 + ETAX0 + STAX0 + EXTAX0 + ITAX0 + FTAX0)
            = ((SUM((f,a),resWF(f,%res_dimen%) * resWFDIST(f,a,%res_dimen%)
                      * resFD(f,a,%res_dimen%))
                      + resSCAL("sMTAX",%res_dimen%) + resSCAL("sETAX",%res_dimen%)
                      + resSCAL("sSTAX",%res_dimen%) + resSCAL("sEXTAX",%res_dimen%)
                      + resSCAL("sITAX",%res_dimen%) + resSCAL("sFTAX",%res_dimen%))
                        /(SUM((f,a),WF0(f) * WFDIST0(f,a) * FD0(f,a))
                            + MTAX0 + ETAX0 + STAX0 + EXTAX0 + ITAX0 + FTAX0)
                                - 1) * 100 ;

* GDP from Expenditure side - C + I + G + E - M
 nperMACROTOT("stGDPEXP",%res_dimen%)$(SUM((c,h),PQD0(c) * QCD0(c,h))
                                       + SUM((c,e),PQD0(c) * QED0(c,e))
                                       + SUM(c,PQD0(c) * QGD0(c))
                                       + SUM(c,PQD0(c) * QINVD0(c))
                                       + SUM(c,PE0(c) * QE0(c))
                                       - SUM(c,PM0(c) * QM0(c)))
            = ((SUM((c,h),resPQD(c,%res_dimen%) * resQCD(c,h,%res_dimen%))
                + SUM((c,e),resPQD(c,%res_dimen%) * resQED(c,e,%res_dimen%))
                + SUM(c,resPQD(c,%res_dimen%) * resQGD(c,%res_dimen%))
                + SUM(c,resPQD(c,%res_dimen%) * resQINVD(c,%res_dimen%))
                + SUM(c,resPE(c,%res_dimen%) * resQE(c,%res_dimen%))
                - SUM(c,resPM(c,%res_dimen%) * resQM(c,%res_dimen%)))
                          /(SUM((c,h),PQD0(c) * QCD0(c,h))
                           + SUM((c,e),PQD0(c) * QED0(c,e))
                           + SUM(c,PQD0(c) * QGD0(c))
                           + SUM(c,PQD0(c) * QINVD0(c))
                           + SUM(c,PE0(c) * QE0(c))
                           - SUM(c,PM0(c) * QM0(c))) - 1) * 100 ;

* total domestic production
 nperMACROTOT("stSUPPLY",%res_dimen%)$(SUM(c,PXC0(c)* QXC0(c)))
            = (SUM(c,resPXC(c,%res_dimen%)* resQXC(c,%res_dimen%))
                /(SUM(c,PXC0(c)* QXC0(c))) - 1) * 100 ;

* total intermediate inputs
 nperMACROTOT("stUSE",%res_dimen%)$(SUM(c,PQD0(c)* QINTD0(c)))
            = (SUM(c,resPQD(c,%res_dimen%)* resQINTD(c,%res_dimen%))
                /(SUM(c,PQD0(c)* QINTD0(c))) - 1) * 100 ;

* household income
 nperMACROTOT("stYHTOT",%res_dimen%)$(SUM(h,YH0(h)))
            = (SUM(h,resYH(h,%res_dimen%))
               /(SUM(h,YH0(h))) - 1) * 100 ;

* government income
 nperMACROTOT("stYGTOT",%res_dimen%)$(YG0)
            = (resSCAL("sYG",%res_dimen%)
              /(YG0) - 1) * 100 ;

* factor income
 nperMACROTOT("stYFTOT",%res_dimen%)$(SUM(f,YF0(f)))
            = (SUM(f,resYF(f,%res_dimen%))
               /(SUM(f,YF0(f))) - 1) * 100 ;

* distributed factor income
 nperMACROTOT("stYFDISPTOT",%res_dimen%)$(SUM(f,YFDISP0(f)))
            = (SUM(f,resYFDISP(f,%res_dimen%))
               /(SUM(f,YFDISP0(f))) - 1) * 100 ;

* Labour income
 nperMACROTOT("stYFLABTOT",%res_dimen%)$(SUM(l,YF0(l)))
            = (SUM(l,resYF(l,%res_dimen%))
               /(SUM(l,YF0(l))) - 1) * 100 ;

* total savings
 nperMACROTOT("stTOTSAV",%res_dimen%)$(TOTSAV0)
            = (resSCAL("sTOTSAV",%res_dimen%)
               /(TOTSAV0) - 1) * 100 ;

* household savings
 nperMACROTOT("stHOSAV",%res_dimen%)$(SUM(h,YH0(h) * (1-TYH0(h))
                                    * SHH0(h)))
            = (SUM(h,resYH(h,%res_dimen%) * (1-resTYH(h,%res_dimen%))
                 * resSHH(h,%res_dimen%))
                    /(SUM(h,YH0(h) * (1-TYH0(h))
                      * SHH0(h))) - 1) * 100 ;

* government savings
 nperMACROTOT("stGOVSAV",%res_dimen%)$(KAPGOV0)
            = (resSCAL("sKAPGOV",%res_dimen%)
               /(KAPGOV0) - 1) * 100 ;

* foreign savings
 nperMACROTOT("stFSAV",%res_dimen%)$(CAPWOR0*ER0)
            = ((resSCAL("sCAPWOR",%res_dimen%)*resSCAL("sER",%res_dimen%))
                /(CAPWOR0*ER0) - 1) * 100 ;

* Depreciation
 nperMACROTOT("stDEPREC",%res_dimen%)$(SUM(f,deprec(f)*YF0(f)))
            = (SUM(f,deprec(f)*resYF(f,%res_dimen%))
               /(SUM(f,deprec(f)*YF0(f))) - 1) * 100 ;

* Import tariff revenue
 nperMACROTOT("stMTAX",%res_dimen%)$MTAX0
            = (resSCAL("sMTAX",%res_dimen%)/(MTAX0) - 1) * 100 ;

* Export tax revenue
 nperMACROTOT("stETAX",%res_dimen%)$ETAX0
            = (resSCAL("sETAX",%res_dimen%)/(ETAX0) - 1) * 100 ;

* Sales tax revenue
 nperMACROTOT("stSTAX",%res_dimen%)$STAX0
            = (resSCAL("sSTAX",%res_dimen%)/(STAX0) - 1) * 100 ;

* Excise tax revenue
 nperMACROTOT("stEXTAX",%res_dimen%)$EXTAX0
            = (resSCAL("sEXTAX",%res_dimen%)/(EXTAX0) - 1) * 100 ;

* Indirect tax revenue
 nperMACROTOT("stITAX",%res_dimen%)$ITAX0
            = (resSCAL("sITAX",%res_dimen%)/(ITAX0) - 1) * 100 ;

* Factor use tax revenue
 nperMACROTOT("stFTAX",%res_dimen%)$FTAX0
            = (resSCAL("sFTAX",%res_dimen%)/(FTAX0) - 1) * 100 ;

* Factor income tax revenue
 nperMACROTOT("stFYTAX",%res_dimen%)$FYTAX0
            = (resSCAL("sFYTAX",%res_dimen%)/(FYTAX0) - 1) * 100 ;

* Household income tax revenue
 nperMACROTOT("stDTAX",%res_dimen%)$DTAX0
            = (resSCAL("sDTAX",%res_dimen%)/(DTAX0) - 1) * 100 ;

$ontext
* Total tax revenue
 nperMACROTOT("stTOTTAX",%res_dimen%)$(MTAX1 + ETAX1 + STAX1 + EXTAX1
                                     + ITAX1 + FTAX1 + FYTAX1 + DTAX1)
            = (resSCAL("sMTAX",%res_dimen%) + resSCAL("sETAX",%res_dimen%)
               + resSCAL("sSTAX",%res_dimen%) + resSCAL("sEXTAX",%res_dimen%)
               + resSCAL("sITAX",%res_dimen%) + resSCAL("sFTAX",%res_dimen%)
               + resSCAL("sFYTAX",%res_dimen%) + resSCAL("sDTAX",%res_dimen%))
                 /(MTAX1 + ETAX1 + STAX1 + EXTAX1
                    + ITAX1 + FTAX1 + FYTAX1 + DTAX1) - 1) * 100 ;
$offtext

* ------- 4. Exporting Macroeconomic Tables to GDX -----------------------

* This writes the results data to GDX
$ifThen.MIRORUN "x%gams.IDCGDXInput%"=="x"
Execute_Unload  'results/stg1_resmacro.gdx',

                          nMACROTOT,
                          nMACROSH1,
                          nMACROSH2,
                          rMACROTOT,
                          rperMACROTOT,
                          nperMACROTOT

   ;
$endIf.MIRORUN
* ----------------- end of Macroeconomic Tables --------------------------
