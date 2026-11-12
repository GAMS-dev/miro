*--------- stg1 Link to MIRO application ----------------------------------

*################ GENERAL INFORMATION ####################################
$ontext

This code was written by Lindsay Shutes, Elevate Economics
lindsayjshutes@gmail.com

File layout:
* Set global settings
* Input widgets
* Shock model with widget values
* Write out results for use in MIRO
*-- Sets and parameters for results
*-- Write out results
* Transform model results into results parameters for use in MIRO
* - Macro
* - Structural change: production
* - Structural change: factor demand
* - Households
* - SDG 8 Decent Work and Economic Growth

$offText

*################ Set global settings ####################################

* The SETGLOBAL dollar control option set below must not be changed
* IF the percent change calculations are to to be correct
* The matching $DROPGLOBAL appears in the file stg1_anal.inc

$SETGLOBAL  res  res

* Results dimension: use (base,expt) for MIRO instead of (elast,clos,sim) used with loops
$SETGLOBAL res_dimen     sim_miro

*################ Input widgets ##########################################
* Model data and parameters that can be used to shock the model via MIRO
$onExternalInput
* General
Parameter
 ERSIM                      Exchange rate multiplier  / 1 /
 SAM_ZA(sacnt,sacntp)       SAM for South Africa without totals
 SAM_ZA_EXPSH(sacnt,sacntp) Expenditure shares South Africa SAM
 SAM_ZA_INCSH(sacnt,sacntp) Income shares South Africa SAM ;
 ;

$GDXIN stg1.gdx
$LOADdc SAM_ZA = SAM_NT
$LOADdc SAM_ZA_EXPSH = SAM_EXPSH
$LOADdc SAM_ZA_INCSH = SAM_INCSH
$GDXIN
$offExternalInput

$onExternalInput
* World prices
* World import price multiplier by commodity group
Parameter
 PWMSIM_AGR  World agricultural import price scalar      / 1 /
 PWMSIM_NAT  World minerals import price scalar          / 1 /
 PWMSIM_FD   World food import price scalar              / 1 /
* Industry includes petrol and chemicals
 PWMSIM_IND  World industrial import price scalar        / 1 /
 PWMSIM_SER  World services import price scalar          / 1 / ;

* World export price multiplier by commodity group
Parameter
 PWESIM_AGR  World agricultural export price scalar      / 1 /
 PWESIM_NAT  World minerals export price scalar          / 1 /
 PWESIM_FD   World food export price scalar              / 1 /
* Industry includes petrol and chemicals
 PWESIM_IND  World industrial export price scalar        / 1 /
 PWESIM_SER  World services export price scalar          / 1 / ;

* Tax instruments
Parameter
 TMADJ_SIM    Tariff rate Scaling Factor                / 1 /
 TSADJ_SIM    Sales tax rate scaling factor             / 1 /
 TEXADJ_SIM   Excise tax rate scaling factor            / 1 /
 TXADJ_SIM    Indirect Tax Scaling Factor               / 1 /
 TFADJ_SIM    Factor Use Tax Scaling Factor             / 1 /
* TYFADJ_SIM   Factor Tax Scaling Factor                 / 1 /
 TYHADJ_SIM   Household Income Tax Scaling Factor       / 1 /
 TYEADJ_SIM   Enterprise income tax Scaling Factor      / 1 / ;
$offExternalInput

*################ Shock model with widget values ########################
*Exchange rate
ER.FX = ERSIM*ER0 ;

*World import prices
PWM.FX(c)$cagr(c) = PWMSIM_AGR*PWM0(c);
PWM.FX(c)$cnat(c) = PWMSIM_NAT*PWM0(c);
PWM.FX(c)$cfd(c)  = PWMSIM_FD *PWM0(c);
PWM.FX(c)$cind(c) = PWMSIM_IND*PWM0(c);
PWM.FX(c)$cser(c) = PWMSIM_SER*PWM0(c);

*World export prices
PWE.FX(c)$cagr(c) = PWESIM_AGR*PWE0(c);
PWE.FX(c)$cnat(c) = PWESIM_NAT*PWE0(c);
PWE.FX(c)$cfd(c)  = PWESIM_FD *PWE0(c);
PWE.FX(c)$cind(c) = PWESIM_IND*PWE0(c);
PWE.FX(c)$cser(c) = PWESIM_SER*PWE0(c);

*Tax instruments
TMADJ.FX  = TMADJ_SIM  * TMADJ0  ;
TSADJ.FX  = TSADJ_SIM  * TSADJ0  ;
TEXADJ.FX = TEXADJ_SIM * TEXADJ0 ;
TXADJ.FX  = TXADJ_SIM  * TXADJ0  ;
TFADJ.FX  = TFADJ_SIM  * TFADJ0  ;
*TYFADJ.FX = TYFADJ_SIM * TYFADJ0 ;
TYHADJ.FX = TYHADJ_SIM * TYHADJ0 ;
TYEADJ.FX = TYEADJ_SIM * TYEADJ0 ;

put log /'Running model (2/2)... ';
 Solve Models1 Using MCP ;
put log 'Done'/;

*################ Store levels results ##################################

SETS
* Sets for results
 rcons    Set for parameters controlling results flow
 sim_miro Simulations /base, scenario/
 adjres       adjuster results
 scalres      scalar results
  ;

Parameter
 res_cont(rcons)  Values for parameters controlling results flow ;

$GDXIN data_in.gdx
* Sets for results
$LOAD adjres scalres
* Experiment results sets
$LOADdc rcons res_cont

$GDXIn

$INCLUDE stg1_resparm.inc
$INCLUDE stg1_resassign.inc
* Storing base results for MIRO
$INCLUDE stg1_resassign_base.inc
