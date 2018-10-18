$title Simplified Energy Systems Model
$ eolcom //
Sets
   type   'plant type'      / coal, gas, photovoltaic, wind /
   ttX    'Time Intervals'  / tX0001*tX8760  /
   time_series_hdr          / Demand, Photovoltaic, Wind /
   plant_data_hdr           / CO2, Cost, 'Expansion Cost' /
   location_hdr             / x,  y, lng, lat, pop /

$ifthen not set NBREGIONS
Sets
   rr     'Regions'         / ID_1*ID_16  /

Table rLocData(rr,*) 'Region location information'
      x           y        lng     lat     pop
ID_1  405.2736842 207.7     9.3501 48.6616 0.612185579
ID_2  631.3578947 223.8    11.4979 48.7904 0.722806037
ID_3  832.1052632 690      13.4050 52.52   0.199832309
ID_4  812.2315789 641.425  13.2162 52.1314 0.139463387
ID_5  347.5473684 759.9125  8.8017 53.0793 0.037954164
ID_6  473.0210526 818.8875  9.9937 53.5511 0.10117384
ID_7  385.5157895 456.5125  9.1624 50.6521 0.347288988
ID_8  729.4315789 826.5875 12.4296 53.6127 0.090050307
ID_9  457.3789474 704.5875  9.8451 52.6367 0.444158748
ID_10 227.5368421 554.15    7.6616 51.4332 1
ID_11 190.4210526 389.7875  7.3090 50.1183 0.227277809
ID_12 160.3157895 299.55    7.0230 49.3964 0.055729458
ID_13 651.8210526 618.7875 11.6923 51.9503 0.124986026
ID_14 810.7052632 513.0625 13.2017 51.1045 0.228172163
ID_15 441.6947368 902.425   9.6961 54.2194 0.161095584
ID_16 562.6631579 501.375  10.8453 51.011  0.120626048
;
$else
Sets
   rr     'Regions'         / r1*r%NBREGIONS%  /
Table rLocData(rr,*) 'Region location information'
      x           y         lng    lat     pop
r1    405.2736842 207.7     9.3501 48.6616 0.612185579
;
rLocData(rr,'x') = uniform(0,1000);
rLocData(rr,'y') = uniform(0,1000);
rLocData(rr,'pop') = uniform(0,1);
$endif

$onExternalInput
Table planttypedata(type,plant_data_hdr) 'Plant information'
              CO2  Cost  'Expansion Cost'
coal          390  0.019  1400
gas           200  0.043   700
photovoltaic    0  0       900
wind            0  0       900
;

Table timeseries(ttX,time_series_hdr) 'base case time series [GWh]'
$ifthen exist "%gams.wdir%mytimeseries.csv"
$  ondelim
$  include mytimeseries.csv
$  offdelim
$else
        Demand Photovoltaic Wind
tX0001  42558  0            8657
tX0002  41620  0            8827
$endif
;

$onechoV > webuiconf.json
{
  "GMSPAR_RESOLUTION": { "alias": "Time resolution",
     "dropdown": { "aliases":["hours","days","weeks","months"], "choices":[1,24,168,720] } },
  "GMSPAR_FROM_TO":    { "alias": "Time range",
     "slider"  : { "min":0, "max":100, "default":[5,70], "step":0.001 } }  
}
$offecho
$offExternalInput
$log %sysenv.SIMPLE_RESOLUTION%
$ifthen %GMSWEBUI%==1
$  batInclude loadCSV scalars
$  if setenv SIMPLE_RESOLUTION  $set RESOLUTION  %sysenv.SIMPLE_RESOLUTION%
$  if setenv SIMPLE_FROM_TO_MIN $set FROM_TO_MIN %sysenv.SIMPLE_FROM_TO_MIN%
$  if setenv SIMPLE_FROM_TO_MAX $set FROM_TO_MAX %sysenv.SIMPLE_FROM_TO_MAX%
$endif

$if not set FROM_TO_MIN $set FROM_TO_MIN   0
$if not set FROM_TO_MAX $set FROM_TO_MAX 100
$if not set RESOLUTION  $set RESOLUTION    1

$if  not set NBREGIONS            $eval NBREGIONS         card(rr)
$if  not set FROM                 $eval FROM              %FROM_TO_MIN%/100
$if  not set TO                   $eval TO                %FROM_TO_MAX%/100
                                        
$if  not set LOADFROMXLS          $set  LOADFROMXLS       0
$if  not set XLSID                $set  XLSID             standard
                                        
$if  not set SCALING              $set  SCALING           0
$if  not set NOSLACK              $set  NOSLACK           0
$LOG ### NBREGIONS=%NBREGIONS% FROM=%FROM% TO=%TO% RESOLUTION=%RESOLUTION%


$ife %FROM%>%TO%         $abort 'FROM > TO'
$ife %RESOLUTION%<0      $abort 'Negative RESOLUTION forbidden'

$eval NBTIMESTEPS ceil((%TO%-%FROM%)*365*24/%RESOLUTION%)
$ifthene %NBTIMESTEPS%>=10000
$ set LEADINGZEROES 0000
$elseife %NBTIMESTEPS%>=1000
$ set LEADINGZEROES 000
$elseife %NBTIMESTEPS%>=100
$ set LEADINGZEROES 00
$elseife %NBTIMESTEPS%>=10
$ set LEADINGZEROES 0
$else
$ set LEADINGZEROES
$endif

$include simple_data_gen.gms


*   basic sets
Set rr                          'regions                                       '
    rrUel
    p                           'plants                                        '
    s                           'storages                                      '
    tt                          'time steps                                    ' / t%LEADINGZEROES%1*t%NBTIMESTEPS% /
    ttX                         'time steps in input data                      '
    e                           'emissions                                     '
    type                        'plant type                                    '
;

Alias(rr,rr1,rr2,rr3);

*   Subsets and mappings
set r(rr)                       'active region                                 '
    t(tt)                       'subset of active time steps                   '
    t_fix(tt)                   'subset of time steps to be fixed after        '
    pr(p,rr)                    'plant to region mapping                       '
    rp(rr,p)                    'region to plant mapping                       '
    re(type)                    'renewable energy types                        '
    ptype(rr,p,type)            'plant type mapping                            '
    sr(s,rr)                    'storage to region mapping                     '
    rs(rr,s)                    'region to storage mapping                     '
    net(rr1,rr2)                'transmission links                            '
    netx(rr,rr1,rr2)            'copy of transmission links                    '
    time_map(tt,ttX)            'mapping of model to input time steps          '
    tX(ttX)                     'dynamic subset of data time steps             '
    tlast(tt)                   'last time step                                '
    tXlast(ttX)                 'last hour in base data                        '
;
Alias(r,r1,r2);
$ set SCEN
$ set SCENS

Parameter
    plant_capX(rr,p,ttX)         'plant capacity                           [GW]'
    cost_unserved_demandX(ttX)   'price for unserved demand          [MEUR/GWh]'
    link_capX(rr1,rr2,ttX)       'transmission link capacity per hour     [GWh]'
    link_efficiencyX(rr1,rr2,ttX)'transmission link efficiency factor          '
    demandX(rr,ttX)              'demand for region per time step         [GWh]'
*   time parameters
    start(tt)                    'start hour of model time step                '
    end(tt)                      'end hour of model time step                  '
    startX(ttX)                  'start hour of data time step                 '
    endX(ttX)                    'end hour of data time step                   '
    overlap(tt,ttX)              'overlap of model and input time step         '
*   model parameters
    plant_cap(tt,rr,p)           'plant capacity in time step t           [GWh]'
    plant_cap2(rr,p,tt)          'plant capacity in time step t           [GWh]'
    yearly_plant_cap(rr,p)       'yearly plant capacity                   [GWh]'
    total_plant_cap(rr,p)        'plant capacity over total time span     [GWh]'
    plant_emission(rr,p,e)       'plant emission                     [tons/GWh]'
    cost_power_generation(rr,p)  'electricity production cost        [MEUR/GWh]'
    yearly_emission_cap(e)       'yearly emission cap                    [tons]'
    total_emission_cap(e)        'emission cap for total time span       [tons]'
    cost_emission(e)             'emission costs                     [MEUR/ton]'
    storage_cap(rr,s)            'storage capacity                        [GWh]'
    storage_efficiency(rr,s)     'effieciency factor of storage                '
    storage_efficiency_in(rr,s)  'effieciency factor of storage inflow         '
    storage_efficiency_out(rr,s) 'effieciency factor of storage outflow        '
    storage_max_in(rr,s)         'maximum inflow into storage             [GWh]'
    storage_max_out(rr,s)        'maximum outflow into storage            [GWh]'
    cost_unserved_demand(tt)     'price for unserved demand          [MEUR/GWh]'
    link_cap(tt,rr1,rr2)         'transmission link capacity per time step[GWh]'
    link_cap2(rr1,rr2,tt)        'transmission link capacity per time step[GWh]'
    link_efficiency(tt,rr1,rr2)  'transmission link efficiency factor          '
    link_efficiency2(rr1,rr2,tt) 'transmission link efficiency factor          '
    demand(tt,rr)                'demand for region per time step         [GWh]'
    demand2(rr,tt)               'demand for region per time step         [GWh]'
    plant_max_add_cap(rr,p)      'max additional plant capacity            [GW]'
    storage_max_add_cap(rr,s)    'max additional storage capacity         [GWh]'
    link_max_add_cap(rr1,rr2)    'max additional arc capacity             [GWh]'
    cost_plant_add(rr,p)         'cost for additional plant capacity  [MEUR/GW]'
    cost_storage_add(rr,s)       'cost for additional storage cap    [MEUR/GWh]'
    cost_link_add(rr1,rr2)       'cost for additional link capacity  [MEUR/GWh]'
    type_mult(%SCEN%type)        'generation cost multiplier                   '
    availX(ttX,rr,p)
    avail(tt,rr,p)
    avail2(rr,p,tt)
;

scalar
    tmpStart                     'helper storing time step start times         '
    tmpEnd                       'helper storing time step end times           '
;

tlast(tt)  = tt.last;
* compute mapping and overlap of model time steps to input time steps
start(tt)   = %FROM%*365*24 + (ord(tt)-1) * %RESOLUTION%;
end(tt)     = %FROM%*365*24 +  ord(tt)    * %RESOLUTION%;
end(tlast)  = %TO%*365*24;
startX(ttX) = ord(ttX)-1;
endX(ttX)   = ord(ttX);
tX(ttX)     = no;

loop(ttX$(not card(tX)), tX(ttX) = endX(ttX) > %FROM%*365*24;);
tmpStart = sum(tX, startX(tX));
tmpEnd   = sum(tX, endX(tX));

loop(tt,
  time_map(tt,tX) = yes;
  While(end(tt) > tmpEnd and tmpEnd < %TO%*365*24,
    tX(ttX)  = tX(ttX-1);
    tmpStart = sum(tX, startX(tX));
    tmpEnd   = sum(tX, endX(tX));
    time_map(tt,tX) = yes;
  );
  if(end(tt) = tmpEnd and not tlast(tt),
    tX(ttX)  = tX(ttX-1);
    tmpStart = sum(tX, startX(tX));
    tmpEnd   = sum(tX, endX(tX));
  );
  // if end(t) < tmpEnd we do not need to do anything
);
tXlast(tX) = tX.last;
abort$(not sum(time_map(tlast,tXlast),1)) 'Last time steps of base and model data do not map.';
overlap(time_map(tt,ttX)) = min(1, abs(min(0, max(end(tt),endX(ttX)) - min(start(tt),startX(ttX)) - (%RESOLUTION%+1))));
overlap(time_map(tlast,ttX)) = min(end(tlast),endX(ttX)) - max(start(tlast),startX(ttX));

* Compute parameters according to time span and resolution
plant_cap2(rp(rr,p),tt)      = sum(time_map(tt,ttX), plant_capX(rp,ttX) * overlap(tt,ttX));
total_plant_cap(rp(rr,p))    = (%TO% - %FROM%) * yearly_plant_cap(rp);
total_emission_cap(e)        = (%TO% - %FROM%) * yearly_emission_cap(e);
storage_efficiency(rs(rr,s)) = rPower(storage_efficiency(rs),%RESOLUTION%);
storage_max_in(rs(rr,s))     = %RESOLUTION% * storage_max_in(rs);
storage_max_out(rs(rr,s))    = %RESOLUTION% * storage_max_out(rs);
cost_unserved_demand(tt)     = smax(time_map(tt,ttX), cost_unserved_demandX(ttX));
link_cap2(net,tt)            = sum(time_map(tt,ttX), link_capX(net,ttX) * overlap(tt,ttX));
link_efficiency2(net,tt)     =   sum(time_map(tt,ttX), link_efficiencyX(net,ttX) * overlap(tt,ttX))
                               / sum(time_map(tt,ttX), overlap(tt,ttX));
demand2(rr,tt)               = sum(time_map(tt,ttX), demandX(rr,ttX) * overlap(tt,ttX));
avail2(rr,p,tt)              = sum(time_map(tt,ttX), availX(ttX,rr,p)) / sum(time_map(tt,ttX), 1);
$ifi not %METHOD%==spExplicitDE type_mult(type)= 1;

* projection on parameters with rearranged index sets
option plant_cap<plant_cap2, link_cap<link_cap2, link_efficiency<link_efficiency2, avail<avail2;
option demand<demand2;

* *** ERRORCHECKS
set err01(rr,p) 'consistent region plant type data';
err01(rr,p) = rp(rr,p) xor sum(ptype(rr,p,type), 1);
abort$card(err01) err01, rp, ptype;

Scalar genBlock indicator for generating a block in case of method=PIPS /0/;


Positive variables
    POWER(%SCEN%tt,rr,p)           'power production                   [GWh]'
    FLOW(%SCEN%tt,rr1,rr2)         'power flow                         [GWh]'
    LINK_ADD_CAP(rr1,rr2)          'arc capacity expansion             [GWh]'
    SLACK(%SCEN%tt,rr)             'uncovered demand                   [GWh]'
    STORAGE_LEVEL(%SCEN%tt,rr,s)   'storage level                      [GWh]'
    STORAGE_INFLOW(%SCEN%tt,rr,s)  'power entering storage             [GWh]'
    STORAGE_OUTFLOW(%SCEN%tt,rr,s) 'power taken from storage           [GWh]'
    PLANT_ADD_CAP(rr,p)            'plant capacity expansion            [GW]'
    STORAGE_ADD_CAP(rr,s)          'storage capacity expansion         [GWh]'
    EMISSION_SPLIT(%SCEN%rr,e)     'emission allowance split      [fraction]'
Variable
    EMISSION_COST(%SCEN%rr,e)      'emission cost                     [MEUR]'
    ROBJ(%SCEN%rr)                 'total region cost                 [MEUR]'
    OBJ                            'total                             [MEUR]'
;

* bounds
LINK_ADD_CAP.up(net)            = link_max_add_cap(net)  ;
PLANT_ADD_CAP.up(rp)            = plant_max_add_cap(rp)  ;
STORAGE_ADD_CAP.up(rs)          = storage_max_add_cap(rs);
* The bounds for STORAGE_INFLOW and STORAGE_OUTFLOW are set in the include file spexplicitde.gms

equations
    eq_robj(%SCEN%rr)                     'total cost in region                 '
    eq_power_balance(%SCEN%tt,rr)         'power balance                        '
    eq_plant_capacity(%SCEN%tt,rr,p)      'respect plant capacity               '
    eq_total_plant_capacity(%SCEN%rr,p)   'respect total plant capacity         '
    eq_storage_balance(%SCEN%tt,rr,s)     'storage balance                      '
    eq_storage_capacity(%SCEN%tt,rr,s)    'respect storage capacity             '
    eq_emission_region(%SCEN%rr,e)        'calculate regional emissions         '
    eq_emission_cost(%SCEN%rr,e)          'calculate regional emission costs    '
    eq_emission_cap(%SCEN%e)              'respect emission cap                 '
    eq_link_capacity(%SCEN%tt,rr1,rr2)    'respect link capacity                '
    eq_obj                                'total cost                           '
;

eq_obj..
    OBJ =e=
            sum(r,           ROBJ(%SCENS%r))
          + sum(net(rr1,rr2),  LINK_ADD_CAP(net)   * cost_link_add(net))
          ;
          
eq_robj(%SCENS%r)..
$IFTHENE.scaling NOT %SCALING% == 1
    ROBJ(%SCENS%r)
            =e= sum((t,ptype(rp(r,p),type)), POWER(%SCENS%t,rp) * cost_power_generation(rp) * type_mult(%SCENS%type))
              + sum(t,           SLACK(%SCENS%t,r)              * cost_unserved_demand(t))
              + sum(rp(r,p),     PLANT_ADD_CAP(rp)              * cost_plant_add(rp))
              + sum(rs(r,s),     STORAGE_ADD_CAP(rs)            * cost_storage_add(rs))
              + sum(e,           EMISSION_COST(%SCENS%r,e));
$ELSE.scaling
    [ROBJ(%SCENS%r)
   - ( sum((t,ptype(rp(r,p),type)), POWER(%SCENS%t,rp) * cost_power_generation(rp) * type_mult(%SCENS%type))
     + sum(t,           SLACK(%SCENS%t,r)              * cost_unserved_demand(t))
     + sum(rp(r,p),     PLANT_ADD_CAP(rp)              * cost_plant_add(rp))
     + sum(rs(r,s),     STORAGE_ADD_CAP(rs)            * cost_storage_add(rs))
     + sum(e,           EMISSION_COST(%SCENS%r,e))) ]
   / [10*smin((t,ptype(rp(r,p),type))$cost_power_generation(rp), cost_power_generation(rp) * type_mult(%SCENS%type))]
   =e= 0;

$ENDIF.scaling

eq_power_balance(%SCENS%t,r)..
        sum(rp(r,p),    POWER(%SCENS%t,rp))
      + sum(net(rr2,r), FLOW(%SCENS%t,net) * link_efficiency(t,net))
      - sum(net(r,rr2), FLOW(%SCENS%t,net))
      + sum(rs(r,s),    STORAGE_OUTFLOW(%SCENS%t,rs) - STORAGE_INFLOW(%SCENS%t,rs))
      + SLACK(%SCENS%t,r)
    =g= demand(t,r);

eq_plant_capacity(%SCENS%t,rp(r,p))..
$ifthene.scaling NOT %SCALING% == 1
    POWER(%SCENS%t,rp) =l= (plant_cap(t,rp) + PLANT_ADD_CAP(rp)*%RESOLUTION%) * avail(t,rp) ;
$else.scaling
    [POWER(%SCENS%t,rp) - (plant_cap(t,rp) + PLANT_ADD_CAP(rp)*%RESOLUTION%) * avail(t,rp)] / [1$(avail(t,rp)<1e-6) + ((%RESOLUTION%)*avail(t,rp)*10)$(avail(t,rp)>=1e-6)] =l= 0;
$endif.scaling


eq_total_plant_capacity(%SCENS%rp(r,p))..
    sum(t, POWER(%SCENS%t,rp)) =l= total_plant_cap(rp);

eq_storage_balance(%SCENS%t(tt),rs(r,s))..
    STORAGE_LEVEL(%SCENS%t,rs) =e= STORAGE_LEVEL(%SCENS%tt--1,rs) * storage_efficiency(rs)
                         + STORAGE_INFLOW(%SCENS%t,rs)    * storage_efficiency_in(rs)
                         - STORAGE_OUTFLOW(%SCENS%t,rs)   / storage_efficiency_out(rs) ;

eq_storage_capacity(%SCENS%t,rs(r,s))..
    STORAGE_LEVEL(%SCENS%t,rs) =l= storage_cap(rs) + STORAGE_ADD_CAP(rs);

eq_emission_region(%SCENS%r,e)..
$ifthene.scaling NOT %SCALING% == 1
    sum((rp(r,p),t), POWER(%SCENS%t,rp) * plant_emission(rp,e)) =l= total_emission_cap(e)*EMISSION_SPLIT(%SCENS%r,e);
$else.scaling
    [sum((rp(r,p),t), POWER(%SCENS%t,rp) * plant_emission(rp,e)) - total_emission_cap(e)*EMISSION_SPLIT(%SCENS%r,e)]
    / [10*smin((rp(r,p),t)$plant_emission(rp,e), plant_emission(rp,e))] =l= 0;
$endif.scaling

eq_emission_cost(%SCENS%r,e)..
$ifthene.scaling NOT %SCALING% == 1
    sum((rp(r,p),t), POWER(%SCENS%t,rp) * plant_emission(rp,e)) * cost_emission(e) =e= EMISSION_COST(%SCENS%r,e);
$else.scaling
      [sum((rp(r,p),t), POWER(%SCENS%t,rp) * plant_emission(rp,e)) * cost_emission(e) - EMISSION_COST(%SCENS%r,e)]
    / [10*smin((rp(r,p),t)$plant_emission(rp,e), plant_emission(rp,e)*cost_emission(e))]   =e= 0;
$endif.scaling

eq_emission_cap(%SCENS%e)$(not genBlock)..
    sum(rr, EMISSION_SPLIT(%SCENS%rr,e)) =l= 1;

eq_link_capacity(%SCENS%t,net)$(not genBlock)..
    FLOW(%SCENS%t,net) =l= link_cap(t,net) + LINK_ADD_CAP(net) * %RESOLUTION%;

model simple / all /;

$ifthene.noslack %NOSLACK%==1
  SLACK.fx(%SCENS%tt,rr)  = 0;
  simple.holdfixed = 1;
$endif.noslack

*  Standard LP
t(tt) = yes;
r(rr) = yes;
if (card(rr)>10 or card(tt)>50,
   option limrow=0, limcol=0;
);
simple.optfile = 1;
option limrow=0, limcol=0, solprint=off; 
$onecho > cplex.opt
lpmethod     4
*solutiontype 2
$offecho

solve simple min OBJ use lp;

$onExternalOutput
Set
   emixHdr 'Energy Mix Header'    / 'Lng', 'Lat', 'Total', 'Renewable', 'Fossil' /
   flowHdr 'Flow Header'          / 'Lng0', 'Lat0', 'Lng1', 'Lat1', 'Flow'  /;
Parameter
   rep_emix(tt,rr,emixHdr)     'energy mix report'
   rep_flow(tt,rr,rr,flowHdr)  'flow report'
   obj_rep                     'objective function value';
$offExternalOutput

rep_emix(tt,rr,'lng')       = rLocData(rr,'lng');
rep_emix(tt,rr,'lat')       = rLocData(rr,'lat');
rep_emix(tt,rr,'total')     = sum(rp(rr,p), POWER.l(tt,rp));
rep_emix(tt,rr,'renewable') = sum(rp(rr,p)$[sum(ptype(rp,re),1)], POWER.l(tt,rp));;
rep_emix(tt,rr,'fossil')    = rep_emix(tt,rr,'total') - rep_emix(tt,rr,'renewable');

rep_flow(tt,net(rr1,rr2),'lng0') = rLocData(rr1,'lng');
rep_flow(tt,net(rr1,rr2),'lat0') = rLocData(rr1,'lat');
rep_flow(tt,net(rr1,rr2),'lng1') = rLocData(rr2,'lng');
rep_flow(tt,net(rr1,rr2),'lat1') = rLocData(rr2,'lat');
rep_flow(tt,net(rr1,rr2),'flow') = FLOW.l(tt,rr1,rr2) + eps;

obj_rep = OBJ.l;
$if not exist webui.gms
$if set GMSWEBUI $abort Asked to do webui but can't find webui.gms. Set idir=path/to/webui
$batinclude webui
