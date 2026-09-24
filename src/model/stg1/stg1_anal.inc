*--------------- stg1 results analysis -----------------------------------
$ontext
The various files for the analysis of results can be called here or by using
programme analyse.gms to carry out the same actions using results stored
in GDX

EACH COMPONENT IS INDEPENDENT, i.e., each component only makes use of the
results saved into the standard GDX results file that is produced by the
simulations module. This includes the levels values for the model varibales,
all the initial values of variables, all the model parameter values, all the
model and simulation sets
$offtext

*------- 1. Write out Levels results -----------------------------------
$ontext
This $INCLUDE file exports the levels results to GDX as 'stg1_reslevel.gdx'

The results parameters for the level values of the variables are set in
'stg1_resassign.inc'

'stg1_reslevel.gdx' is the default name for the results file
    its name can be changed in the $INCLUDE file
$offtext

If(flow_cont("reslevel") = 1,

$INCLUDE stg1_reslevel.inc

  ) ;


*----------- 2. Macroeconomic Results ------------------------------------
$ontext
This $INCLUDE file generates and exports various summary macroeconomic
totals, shares and proportionate changes to GDX as 'stg1_resmacro.gdx'

'stg1_resmacro.gdx' is the default name for the results file
its name can be changed in the $INCLUDE file
$offtext

*---- 3a. Define Macroeconomic Tables

* This section defines the Macroeconomic tables that are reported

PARAMETER

*Macroeconomic tables - indexed on stab1
 nMACROTOT(stab1,%res_dimen%)     Nominal Macro totals
 nMACROSH1(stab1,%res_dimen%)     Nominal Macro shares in GDP Expenditure
 nMACROSH2(stab1,%res_dimen%)     Nominal Macro shares in Domestic Absorption

 rMACROTOT(stab1,%res_dimen%)     Real Macro totals

*Macroeconomic totals - percentage change
 rperMACROTOT(stab1,%res_dimen%)  Percentage changes in Real Macro totals
 nperMACROTOT(stab1,%res_dimen%)  Percentage changes in Nominal Macro totals

  ;

*---- 3b. Assign and Export Macroeconomic Tables

*If(flow_cont("resmacro") = 1,

$INCLUDE stg1_resmacro.inc

*  ) ;

* ----------- 3. Welfare Results Analysis --------------------------------
$ontext
This $INCLUDE file generates and exports various summary welfare
measures to GDX as 'stg1_reswelf.gdx'

       'stg1_reswelf.gdx' is the default name for the results file
             its name can be changed in the $INCLUDE file
$offtext

*---- 5a. Define Welfare Tables

Parameter

 %res%YH_cap(h,%res_dimen%)     Income per capita to household h
 %res%HEXP_cap(h,%res_dimen%)   H'hold consumption expend per capita

 %res%EV(h,%res_dimen%)         EV by household on consumption
 %res%EV_cap(h,%res_dimen%)     EV per capita by household on consumption

 %res%IC(h,%res_dimen%)         LES Indirect compensation by h'hold on consumption
 %res%IC_cap(h,%res_dimen%)     LES IC per capita by h'hold on consumption

 %res%EV_alt(h,%res_dimen%)     LES Alternative EV by h'hold on consumption
 %res%EV_alt_cap(h,%res_dimen%) LES Alt EV per capita by h'hold on consumption

 pop(h)                         Population by household and region

 %res%EV_H_slut(h,%res_dimen%)  Slutsky approx EV by household on consumption
 %res%EV_R_slut(%res_dimen%)    Slutsky approx EV by region on consumption

   ;

*---- 5b. Assign and Export Welfare Results

If(flow_cont("reswelf") = 1,

$INCLUDE stg1_reswelf.inc

  ) ;

$DROPGLOBAL res

* ------- X. Value Added Results -----------------------------------------



* ------- Y. Percentage Changes in Variables -----------------------------
$ontext
The file respercent.gms calculates the percentages changes in model variables
and sends the results to a GDX file res_percent.gdx
The names for the result parameters are defined using SETGLOBAL below

The $INCLUDE component implements the calculations and sets all non zero
elements to EPS. The results are exported to GDX as res_percent.gdx
The GDXXRW component requires that the input (GDX) (i) and the
output (XLS) (o) files are set
The default input file, res_percent.gdx, is created by the $INCLUDE component
where its name can be changed
The output file does not have a default
$offtext

$SETGLOBAL  res  res_p_

$INCLUDE stg1_resparm2.inc

If(flow_cont("resperc") = 1,

$INCLUDE stg1_resperc.inc

  ) ;

$DROPGLOBAL res

*--------------- end of stg1 results analysis ----------------------------
