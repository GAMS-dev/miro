*-------- smod export level results --------------------------------------
$ontext
* This files writes the level results data to GDX
$offtext

*---- EXPORT OF RESULT FILES TO GDX
$ifThen.MIRORUN "x%gams.IDCGDXInput%"=="x"
Execute_Unload  'results/stg1_reslevel.gdx',
* Data
 SAM, ASAM0,

* A MicroSAM for each Experiment
 %res%SAM,
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
 %res%SHH,
 %res%SEN,
 %res%QINVD

     ;
$endIf.MIRORUN

*-------- end of smod export level results -------------------------------
