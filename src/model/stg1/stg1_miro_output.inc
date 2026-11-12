*--------- stg1 Outputs for GAMS MIRO application -----------------------
$offIDCProtect

*################ GENERAL INFORMATION ###################################
$ontext

Contact: Lindsay Shutes, Elevate Economics
lindsayjshutes@gmail.com

File layout:
* Sets and parameters for MIRO
* Results parameters for MIRO
* - Macro
* - Structural change: production
* - Trade
* - Factor returns
* - Labour demand
* - Households
* - Government

$offText

*################ SETS AND PARAMETERS FOR MIRO ###########################
Set

*Sets for results labels
 sector           Sector labels
 flabel           Factor labels
 hlabel           Household labels
 map_flabel_f(flabel,f) Mapping from factors to factor labels
 map_hlabel_h(hlabel,h) Mapping from households to household labels

Scalar
*SDG parameters
 sharetour        Share of tourism in tourism related activities
 partrate         Labour force participation rate in the base
 unemprate        Unemployment rate in the base ;

* Load sets from GDX data file
$GDXIN  data_in.gdx

$LOADdc sector flabel map_flabel_f hlabel map_hlabel_h

$GDXIn

*################ RESULTS PARAMETERS FOR MIRO ###########################
* - Macro
Set mac  "Macro indicators " /GDP, Absorption, Production, Exports,
                                       Imports, 'Govt Savings', 'Income Tax Scaling', 'Exchange Rate'/;

Parameter
$onExternalOutput
 MACROlevel(mac,%res_dimen%)     Macro indicators (simulation value)
$offExternalOutput
 MACROperc(mac,%res_dimen%)      Macro indicators (pc);

 MACROlevel("GDP",%res_dimen%)          = rMACROTOT("stGDPEXP",%res_dimen%) ;
 MACROlevel("Absorption",%res_dimen%)   = rMACROTOT("stQABSORP",%res_dimen%) ;
 MACROlevel("Production",%res_dimen%)   = rMACROTOT("stSUPPLY",%res_dimen%) ;
 MACROlevel("Exports",%res_dimen%)      = rMACROTOT("stEXPORT",%res_dimen%) ;
 MACROlevel("Imports",%res_dimen%)      = rMACROTOT("stIMPORT",%res_dimen%) ;
 MACROlevel("Govt Savings",%res_dimen%)  = rMACROTOT("stGOVSAV",%res_dimen%) ;
 MACROlevel("Income Tax Scaling",%res_dimen%) = resADJ("aTYADJ",%res_dimen%) ;
 MACROlevel("Exchange Rate",%res_dimen%) = resSCAL("sER",%res_dimen%) ;

 MACROperc("GDP",%res_dimen%)              = rperMACROTOT("stGDPEXP",%res_dimen%) ;
 MACROperc("Absorption",%res_dimen%)       = rperMACROTOT("stQABSORP",%res_dimen%) ;
 MACROperc("Production",%res_dimen%)       = rperMACROTOT("stSUPPLY",%res_dimen%) ;
 MACROperc("Exports",%res_dimen%)          = rperMACROTOT("stEXPORT",%res_dimen%) ;
 MACROperc("Imports",%res_dimen%)          = rperMACROTOT("stIMPORT",%res_dimen%) ;
 MACROperc("Govt Savings",%res_dimen%)      = rperMACROTOT("stGOVSAV",%res_dimen%) ;
 MACROperc("Exchange Rate",%res_dimen%)     = res_p_scal("sER",%res_dimen%) ;
 MACROperc("Income Tax Scaling",%res_dimen%) = res_p_ADJ("aTYADJ",%res_dimen%) ;

*Round to remove very small changes
 MACROperc(mac,%res_dimen%)             = ROUND(MACROperc(mac,%res_dimen%),6) ;

* - GDP composition
Set gdpcomp  "GDP composition indicators" /GDP, "Household Consumption",
                                             "Enterprise Consumption", Investment,
                                             "Government Consumption",
                                             Exports, Imports/;

Set abscomp  "Absorption composition indicators" /Absorption, "Household Consumption",
                                             "Enterprise Consumption", Investment,
                                             "Government Consumption"/;

Set rnvalue / real, nominal /;

$onExternalOutput
Parameter
 perGDPComp(gdpcomp,rnvalue, %res_dimen%)    Composition of GDP (pc)
 perABSORPComp(abscomp,rnvalue,%res_dimen%) Composition of Absorption (pc);
$offExternalOutput

 perGDPComp("GDP","nominal",%res_dimen%)                   = nperMACROTOT("stGDPEXP",%res_dimen%) ;
 perGDPComp("Household Consumption","nominal",%res_dimen%)  = nperMACROTOT("stQCDTOT",%res_dimen%) ;
 perGDPComp("Enterprise Consumption","nominal",%res_dimen%) = nperMACROTOT("stQEDTOT",%res_dimen%) ;
 perGDPComp("Investment","nominal",%res_dimen%)            = nperMACROTOT("stQINVTOT",%res_dimen%) ;
 perGDPComp("Government Consumption","nominal",%res_dimen%) = nperMACROTOT("stQGDTOT",%res_dimen%) ;
 perGDPComp("Exports","nominal",%res_dimen%)               = nperMACROTOT("stEXPORT",%res_dimen%) ;
 pergdpcomp("Imports","nominal",%res_dimen%)               = nperMACROTOT("stIMPORT",%res_dimen%) ;

 perGDPComp("GDP","real",%res_dimen%)                   = rperMACROTOT("stGDPEXP",%res_dimen%) ;
 perGDPComp("Household Consumption","real",%res_dimen%)  = rperMACROTOT("stQCDTOT",%res_dimen%) ;
 perGDPComp("Enterprise Consumption","real",%res_dimen%) = rperMACROTOT("stQEDTOT",%res_dimen%) ;
 perGDPComp("Investment","real",%res_dimen%)            = rperMACROTOT("stQINVTOT",%res_dimen%) ;
 perGDPComp("Government Consumption","real",%res_dimen%) = rperMACROTOT("stQGDTOT",%res_dimen%) ;
 perGDPComp("Exports","real",%res_dimen%)               = rperMACROTOT("stEXPORT",%res_dimen%) ;
 perGDPComp("Imports","real",%res_dimen%)               = rperMACROTOT("stIMPORT",%res_dimen%) ;

 perABSORPComp("Absorption","nominal",%res_dimen%)            = nperMACROTOT("stQABSORP",%res_dimen%) ;
 perABSORPComp("Household Consumption","nominal",%res_dimen%)  = nperMACROTOT("stQCDTOT",%res_dimen%) ;
 perABSORPComp("Enterprise Consumption","nominal",%res_dimen%) = nperMACROTOT("stQEDTOT",%res_dimen%) ;
 perABSORPComp("Investment","nominal",%res_dimen%)            = nperMACROTOT("stQINVTOT",%res_dimen%) ;
 perABSORPComp("Government Consumption","nominal",%res_dimen%) = nperMACROTOT("stQGDTOT",%res_dimen%) ;

 perABSORPComp("Absorption","real",%res_dimen%)            = rperMACROTOT("stQABSORP",%res_dimen%) ;
 perABSORPComp("Household Consumption","real",%res_dimen%)  = rperMACROTOT("stQCDTOT",%res_dimen%) ;
 perABSORPComp("Enterprise Consumption","real",%res_dimen%) = rperMACROTOT("stQEDTOT",%res_dimen%) ;
 perABSORPComp("Investment","real",%res_dimen%)            = rperMACROTOT("stQINVTOT",%res_dimen%) ;
 perABSORPComp("Government Consumption","real",%res_dimen%) = rperMACROTOT("stQGDTOT",%res_dimen%) ;

*Round to remove very small changes
 perGDPComp(gdpcomp,rnvalue,%res_dimen%)    = ROUND(perGDPComp(gdpcomp,rnvalue,%res_dimen%),6);
 perABSORPComp(abscomp,rnvalue,%res_dimen%) = ROUND(perABSORPComp(abscomp,rnvalue,%res_dimen%),6);

* - Structural change: production
Set agrp "activity group" /Primary, Manufacturing, Services/

    aprimary(a) "primary activities" /#aagr, #anat/
    amanuf(a)   "manufacturing activities" /#afd, #aind/
    aserv(a)    "service activities" /#auti, #acon, #aser/

    sprimary(sector) "primary activities" /a_Agriculture, a_Minerals/
    smanu(sector)    "manufacturing activities" /a_Food, a_Textiles,
    a_Petrol_chemicals, a_Metal_mineral_products, a_Machinery, a_Vehicles,
    a_Other_manufacturing/
    sserv(sector)    "service activities" / a_Utilities, a_Construction,
    a_Trade_transport_communications, a_Services /

* Group labels
    map_sector_a(sector,a) mapping from activity to sector /#sector:#a/
    ;

$onExternalOutput
Parameter
 QXlev(sector,%res_dimen%)     Production by activity
 QXpc(agrp,sector,%res_dimen%) Production by activity (pc)
 QVAsh(agrp,%res_dimen%)       Sectoral share of value added;
$offExternalOutput

* -- Production
 QXlev(sector,%res_dimen%)                             = SUM(a$map_sector_a(sector,a),resQX(a,%res_dimen%));
 QXpc("Primary",sector,%res_dimen%)$sprimary(sector)   = SUM(a$map_sector_a(sector,a),res_p_QX(a,%res_dimen%));
 QXpc("Manufacturing",sector,%res_dimen%)$smanu(sector)= SUM(a$map_sector_a(sector,a),res_p_QX(a,%res_dimen%));
 QXpc("Services",sector,%res_dimen%)$sserv(sector)     = SUM(a$map_sector_a(sector,a),res_p_QX(a,%res_dimen%));

*Round to remove very small changes
 QXpc(agrp,sector,%res_dimen%) = ROUND(QXpc(agrp,sector,%res_dimen%),6) ;

* -- Value added (made into pie chart in MIRO)
 QVAsh("Primary","base")       = SUM(aprimary, PVA0(aprimary)*QVA0(aprimary))
                           /SUM(a,PVA0(a)*QVA0(a));
 QVAsh("Manufacturing","base") = SUM(amanuf, PVA0(amanuf)*QVA0(amanuf))
                           /SUM(a,PVA0(a)*QVA0(a));
 QVAsh("Services","base")      = SUM(aserv, PVA0(aserv)*QVA0(aserv))
                           /SUM(a,PVA0(a)*QVA0(a));

 QVAsh("Primary","scenario")        = SUM(aprimary, PVA.L(aprimary)*QVA.L(aprimary))
                           /SUM(a,PVA.L(a)*QVA.L(a));
 QVAsh("Manufacturing","scenario")  = SUM(amanuf, PVA.L(amanuf)*QVA.L(amanuf))
                           /SUM(a,PVA.L(a)*QVA.L(a));
 QVAsh("Services","scenario")       = SUM(aserv, PVA.L(aserv)*QVA.L(aserv))
                           /SUM(a,PVA.L(a)*QVA.L(a));

* - Trade
Set tgrp "trade " /Imports, Exports/ ;

$onExternalOutput
Parameter
 TRADEpc(tgrp,cagg,%res_dimen%) Percentage change in exports and imports ;
$offExternalOutput

 TRADEpc("Imports",cagg,%res_dimen%)$SUM(c$map_cagg_c(cagg,c),QM0(c))
                                  = (SUM(c$map_cagg_c(cagg,c),resQM(c,%res_dimen%))/
                                    SUM(c$map_cagg_c(cagg,c),QM0(c))-1)*100 ;

 TRADEpc("Exports",cagg,%res_dimen%)$SUM(c$map_cagg_c(cagg,c),QE0(c))
                                  = (SUM(c$map_cagg_c(cagg,c),resQE(c,%res_dimen%))/
                                    SUM(c$map_cagg_c(cagg,c),QE0(c))-1)*100 ;

*Round to remove very small changes
 TRADEpc(tgrp,cagg,%res_dimen%) = ROUND(TRADEpc(tgrp,cagg,%res_dimen%),6) ;

* - Factor returns
Set
    fgrp "factor group" /Land, Capital, Labour/
    nlabel(flabel) "Land labels" / Land /
    klabel(flabel) "Capital labels" / Capital /
    llabel(flabel) "Labour labels" / African_Unskilled, African_Skilled, Coloured_Asian_Unskilled,
                                     Coloured_Asian_Skilled, White_Unskilled, White_Skilled /
    nk(f) /#n,#k/
    ;

$onExternalOutput
Parameter
 WFfpc(fgrp,flabel,%res_dimen%)  Percentage change in factor returns
 WFlabpc(%res_dimen%)            Percentage change in average wage ;
$offExternalOutput

 WFfpc("Land",flabel,%res_dimen%)$nlabel(flabel)    = SUM(f$map_flabel_f(flabel,f),res_p_WF(f,%res_dimen%)) ;
 WFfpc("Capital",flabel,%res_dimen%)$klabel(flabel) = SUM(f$map_flabel_f(flabel,f),res_p_WF(f,%res_dimen%)) ;
 WFfpc("Labour",flabel,%res_dimen%)$llabel(flabel)  = SUM(f$map_flabel_f(flabel,f),res_p_WF(f,%res_dimen%)) ;

*Need to update with WFA style calculation
 WFlabpc(%res_dimen%)  =[SUM{l,resWF(l,%res_dimen%)
                         *[(resWF(l,%res_dimen%)*resFS(l,%res_dimen%))/
                         SUM(lp,resWF(lp,%res_dimen%)*resFS(lp,%res_dimen%))]}/
                         SUM{l,resWF(l,"base")
                         *[(resWF(l,"base")*resFS(l,"base"))/
                         SUM(lp,resWF(lp,"base")*resFS(lp,"base"))]}-1]*100 ;

*Round to remove very small changes
 WFfpc(fgrp,flabel,%res_dimen%)   = ROUND(WFfpc(fgrp,flabel,%res_dimen%),6) ;
 WFlabpc(%res_dimen%)             = ROUND(WFlabpc(%res_dimen%),6) ;

* - Labour demand

$onExternalOutput
Parameter
 FDlpc(aagg,%res_dimen%) Percentage change in labour demand by activity group ;
$offExternalOutput

 FDlpc(aagg,%res_dimen%)$SUM(a$ map_aagg_a(aagg,a),SUM(l,FD0(l,a)))
                      = (SUM(a$map_aagg_a(aagg,a),SUM(l,resFD(l,a,%res_dimen%)))/
                         SUM(a$map_aagg_a(aagg,a),SUM(l,FD0(l,a)))-1)*100 ;

*Round to remove very small changes
 FDlpc(aagg,%res_dimen%)   = ROUND(FDlpc(aagg,%res_dimen%),6) ;

* - Households
Set hgrp "household group" /LowMedEducation, HighEducation/
    hlowmeded(hlabel) "Low and medium educated households" / African_Female_Low ,  African_Female_Mid ,
                                                             African_Male_Low ,    African_Male_Mid ,
                                                             Coloured_Asian_Low ,  White_Low /
    hhighed(hlabel) "Highly educated households" / African_Female_High ,  African_Male_High ,
                                                   Coloured_Asian_High ,  White_High /
    LowMedEd(h) "Low and medium educated households" / h_African_female_low ,  h_African_female_med ,
     h_African_male_low ,  h_African_male_med ,  h_Coloured_Asian_low ,  h_White_low  /
    HighEd(h) "Highly educated  households" / h_African_female_high , h_African_male_high ,
     h_Coloured_Asian_high ,  h_White_high / ;

$onExternalOutput
Parameter
 YHlev(hgrp,hlabel,%res_dimen%)       Household income
 YHpc(hgrp,hlabel,%res_dimen%)   Household income by group (pc)
 YHavpc(%res_dimen%)             Average household income (pc)
 QCDpc(hgrp,cagg,%res_dimen%)    Household consumption by group(pc)
 QCDtotpc(h,%res_dimen%)         Total household consumption by group pc)
 QCDavpc(%res_dimen%)            Average household consumption (pc) ;
$offExternalOutput

 YHlev("LowMedEducation",hlabel,%res_dimen%)$hlowmeded(hlabel)          = SUM(h$map_hlabel_h(hlabel,h),resYH(h,%res_dimen%)) ;
 YHlev("HighEducation",hlabel,%res_dimen%)$hhighed(hlabel)          = SUM(h$map_hlabel_h(hlabel,h),resYH(h,%res_dimen%)) ;

 YHpc("LowMedEducation",hlabel,%res_dimen%)$hlowmeded(hlabel)
                                    = SUM(h$map_hlabel_h(hlabel,h),res_p_YH(h,%res_dimen%)) ;
 YHpc("HighEducation",hlabel,%res_dimen%)$hhighed(hlabel)
                                    = SUM(h$map_hlabel_h(hlabel,h),res_p_YH(h,%res_dimen%)) ;
 YHavpc(%res_dimen%)                = (SUM(h,resYH(h,%res_dimen%))/(SUM(h,YH0(h)))-1)*100 ;

 QCDpc("LowMedEducation",cagg,%res_dimen%)$SUM(c$map_cagg_c(cagg,c),SUM(h$LowMedEd(h),QCD0(c,h)))
                                    =(SUM(c$map_cagg_c(cagg,c),SUM(h$LowMedEd(h),resQCD(c,h,%res_dimen%)))/
                                      SUM(c$map_cagg_c(cagg,c),SUM(h$LowMedEd(h),QCD0(c,h)))-1)*100 ;

 QCDpc("HighEducation",cagg,%res_dimen%)$SUM(c$map_cagg_c(cagg,c),SUM(h$HighEd(h),QCD0(c,h)))
                                    =(SUM(c$map_cagg_c(cagg,c),SUM(h$HighEd(h),resQCD(c,h,%res_dimen%)))/
                                      SUM(c$map_cagg_c(cagg,c),SUM(h$HighEd(h),QCD0(c,h)))-1)*100 ;
 QCDtotpc(h,%res_dimen%)            = (SUM(c,resQCD(c,h,%res_dimen%))/(SUM(c,QCD0(c,h)))-1)*100 ;
 QCDavpc(%res_dimen%)               = (SUM(h,SUM(c,resQCD(c,h,%res_dimen%)))/(sum(h,SUM(c,QCD0(c,h))))-1)*100 ;

*Round to remove very small changes
 YHpc(hgrp,hlabel,%res_dimen%)  = ROUND(YHpc(hgrp,hlabel,%res_dimen%),6) ;
 QCDpc(hgrp,cagg,%res_dimen%)   = ROUND(QCDpc(hgrp,cagg,%res_dimen%),6) ;
 YHavpc(%res_dimen%)            = ROUND(YHavpc(%res_dimen%),6) ;
 QCDavpc(%res_dimen%)           = ROUND(QCDavpc(%res_dimen%),6) ;

* - Government
Set gov  "Government indicators" /Income, "Tax Revenue", Consumption, "Real Consumption", Savings/ ;

$onExternalOutput
Parameter
 Govlev(gov,%res_dimen%)     Government indicators
$offExternalOutput
 Govlevp(gov,%res_dimen%)    Government indicators (pc) ;

 Govlev("Income",%res_dimen%)      = resScal("sYG",%res_dimen%) ;
 Govlev("Tax Revenue",%res_dimen%)  = resScal("sMTAX",%res_dimen%) + resScal("sETAX",%res_dimen%)
                                       + resScal("sDTAX",%res_dimen%) + resScal("sFYTAX",%res_dimen%)
                                       + resScal("sITAX",%res_dimen%) + resScal("sSTAX",%res_dimen%) ;
 Govlev("Consumption",%res_dimen%) = resScal("sVGD",%res_dimen%) ;
 Govlev("Real Consumption",%res_dimen%)    = resScal("sEG",%res_dimen%) ;
 Govlev("Savings",%res_dimen%)     = resScal("sKAPGOV",%res_dimen%) ;

 Govlevp("Income",%res_dimen%)     = res_p_Scal("sYG",%res_dimen%) ;
 Govlevp("Tax Revenue",%res_dimen%) = ([resScal("sMTAX",%res_dimen%) + resScal("sETAX",%res_dimen%)
                                        + resScal("sDTAX",%res_dimen%) + resScal("sFYTAX",%res_dimen%)
                                        + resScal("sITAX",%res_dimen%) + resScal("sSTAX",%res_dimen%)]/
                                      [MTAX0 + ETAX0 + DTAX0 + FYTAX0 +ITAX0 + STAX0] -1)*100 ;
 Govlevp("Consumption",%res_dimen%) = res_p_Scal("sVGD",%res_dimen%) ;
 Govlevp("Real Consumption",%res_dimen%)    = res_p_Scal("sEG",%res_dimen%) ;
 Govlevp("Savings",%res_dimen%)     = res_p_Scal("sKAPGOV",%res_dimen%) ;

*Round to remove very small changes
 Govlevp(gov,%res_dimen%)   = ROUND(Govlevp(gov,%res_dimen%),6) ;

$onExternalOutput
Scalar
  gdp
  absorption
  production
  exports
  imports
  govtsavings
  incometaxscaling
  land
  capital
  averagewage
  averagehouseholdconsumption
  averagehouseholdincome;
$offExternalOutput
  gdp =                              MACROperc("GDP","scenario")                           ;
  absorption =                       MACROperc("Absorption","scenario")                    ;
  production =                       MACROperc("Production","scenario")                    ;
  exports =                          MACROperc("Exports","scenario")                       ;
  imports =                          MACROperc("Imports","scenario")                       ;
  govtsavings =                      MACROperc("Govt Savings","scenario")                   ;
  incometaxscaling =                 MACROperc("Income Tax Scaling","scenario")              ;
  land =                             WFfpc("Land","Land","scenario")        ;
  capital =                          WFfpc("Capital","Capital","scenario")     ;
  averagewage =                      WFlabpc("scenario")                                   ;
  averagehouseholdconsumption =      QCDavpc("scenario")                                   ;
  averagehouseholdincome =           YHavpc("scenario")                                    ;


*Export everything to be attached to a MIRO scenario
$ifThen.MIRORUN "x%gams.IDCGDXInput%"=="x%gams.IDCGDXInput%"
$call 'rm -f all_data.gdx'
execute_unload 'all_data.gdx';
$endif.MIRORUN
