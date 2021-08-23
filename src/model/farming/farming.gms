$title Crop plant alloction problem with stochastic crop yield

$ontext

This model helps a farmer to decide how to allocate
his or her land. The yields are uncertain.

Birge, R, and Louveaux, F V, Introduction to Stochastic Programming.
Springer, 1997.

$offtext

Set crop
    ch    'header for data table' /
               yield               'yield [tons/acre]'
               cost                'plant cost [$/acre]'
               pprice              'purchase price [$/ton]'
               minreq              'minimum requirements of crop to feed cattle [tons]' /
    ph    'header for price curve table' /
               price               'crop sales price [$/ton]'
               amount              'max amount of crop sold at sales price [tons]' /
    seq   'price curve segments';
               
$onExternalInput
Table cd(crop<,ch) 'crop data'
              yield   cost  pprice  minreq
wheat           2.5    150     238     200
corn            3      230     210     240
'sugar beets'  20      260
;

Parameter
   yf     'yield factor'          /   1 /
   land   'available land [acre]' / 500 /
   nums   'number of scenarios'   /   3 /
;

Table pricecurve(crop,seq<,ph) 'price curve data'
                 price amount
wheat.s1           170    inf
corn.s1            150    inf
'sugar beets'.s1    36   6000
'sugar beets'.s2    10    inf
;
$offExternalInput

set pcs(crop,seq) relevant segments; option pcs<pricecurve;
alias (crop,c);

* Data checks
file fmiro / 'farming_miro.log' /; put fmiro 'Crop plant alloction problem with stochastic crop yield' //;

put '------------------------------------'/;
put '        Data validation'/;
put '------------------------------------'//;

put 'Validating crop data ...';
if (smin((c,ch), cd(c,ch))<0,
  put 'cd:: No negative entires allowed!'/;
  loop((c,ch)$(cd(c,ch)<0),
      put / ' Negative entry for crop ' c.tl:14 ' in field ' ch.te(ch):30 ': ' cd(c,ch):6:2);
  abort "Data errors detected."
else
  put ' OK'/;
);
put 'Validating price data ...';
if (smin(pcs(c,seq), pricecurve(pcs,'price'))<0,
  put 'pricecurve:: No negative entires allowed!'/;
  loop((pcs(c,seq),ph)$(pricecurve(pcs,'price')<0 and sameas(ph,'price')),
      put / ' Negative entry for crop ' c.tl:14 ' and step ' seq.tl:4 ' in field ' ph.te(ph):30 ': ' pricecurve(pcs,ph):6:2);
  abort "Data errors detected."
);
if (smin(pcs(c,seq), pricecurve(c,seq,'price')-pricecurve(c,seq+1,'price'))<0,
  put 'pricecurve:: Price curve is not concave!'/;
  loop(pcs(c,seq)$(pricecurve(c,seq,'price')-pricecurve(c,seq+1,'price') < 0),
      put / ' Price for crop ' c.tl:14 ' of step ' seq.tl:4 ' to next step increases by $' (pricecurve(c,seq+1,'price')-pricecurve(c,seq,'price')):6:2);
  abort "Data errors detected."
else
  put ' OK'/;
);
put 'Validating purchase data ...';
if (sum(c$(cd(c,'minreq')>0 and cd(c,'pprice')=0),1),
  put 'cd:: Crop for cattlefeed without external purchase ability!'/;
  loop(c$(cd(c,'minreq')>0 and cd(c,'pprice')=0),
      put / ' Crop ' c.tl:14 ' required for feed (min. requirement is ' cd(c,'minreq'):6:2 ' ) cannot be purchased (purchase price is $0)');
  abort "Data errors detected."
else
  put ' OK'/;
);
put / 'No data exceptions.';

Variables
   x(c)     crop planted in acres of land
   w(c,seq) crops sold in segment of cost curve in tons
   y(c)     crops purchased in tons
   profit   objective variable in dollars;
Positive variables x,w,y;

Equations
  profitdef  objective function
  landuse    capacity
  bal(c)     crop balance;

profitdef..    profit =e= sum(pcs, w(pcs)*pricecurve(pcs,'price'))
                        - sum(c, cd(c,'cost')*x(c) + cd(c,'pprice')*y(c));

landuse..      sum(c, x(c)) =l= land;

bal(c)..       yf*cd(c,'yield')*x(c) + y(c) - sum(pcs(c,seq), w(pcs)) =g= cd(c,'minreq');

* No purchase of crops that don't have a purchase price
y.fx(c)$(cd(c,'pprice')=0) = 0;
w.up(pcs) = pricecurve(pcs,'amount');

model farm_emp /all/;

solve farm_emp using lp maximizing profit;
if (farm_emp.modelstat <> 1,
  put 'Data inconsistent which produces a non-optimal model instance. Model status: ' farm_emp.modelstat:0:0;
  abort 'Could not solve model to optimality';
);

set rh 'report header' /
              profit  'profit [$]'
              revenue 'revenue [$]'
              cost    'cost [$]'
              landuse 'land use [%]' /
    rch 'crop report header' /
              planted    'crop planted [acres]'
              seedcost   'seed cost [$]'
              yield      'crop yield [tons]'
              sold       'crop sold [tons]'
              sales      'crop revenue [$]'
              purchased  'crop purchased [tons]'
              pcost      'purchase cost [$]' /
    rcomph 'crop comparison report header' /
              planted_d  'deterministic: crop planted [acres]'
              planted_s  'stochastic: crop planted [acres]' /;            
              
$onExternalOutput
Parameter
    rep(rh)              'report'    
    repc(c,rch)          'crop report'
    repcompare(c,rcomph) 'crop comparison deterministic versus stochastic';
Table repc, repcompare;    
$offExternalOutput

repcompare(c,'planted_d') = x.l(c);

$onEmpty
$if not set SNUM $eval SNUM nums
$if not %SNUM%==0 Set s(s) scenarios / s1*s%SNUM% /;
$if     %SNUM%==0 Set s(s) scenarios /            /;
Parameter
    srep(s,*)         scenario attributes / #s.prob 0 /
    s_yf(s)           yield factor realization by scenario
    s_profit(s)       profit by scenario  /  /
$onExternalOutput
    repfinance(s,rh)  financial report by scenario
$offExternalOutput
    s_w(s,c,seq)      crops sold in segment of cost curve in tons by scenario /  /
    s_y(s,c)          crops purchased in tons by scenario /  /;

Set dict / s     .scenario.''
           ''    .opt.     srep
           yf    .randvar. s_yf
           profit.level.   s_profit
           w     .level.   s_w
           y     .level.   s_y /;

file emp / '%emp.info%' /;
put emp '* problem %gams.i%' / 'randvar yf discrete';
$ifthen %SNUM%==3
put / '0.33 0.8' 
    / '0.33 1.0' 
    / '0.33 1.2'; 
$else
loop(s, put (1/card(s)) ' ' normal(1,0.1) /);
$endif
putclose 'stage 2 yf y w bal profit';

$if not %SNUM%==0 solve farm_emp using emp maximizing profit scenario dict;

$onDotL
repcompare(c,'planted_s') = x(c);
repfinance(s,'profit')    = s_profit(s);
repfinance(s,'cost')      = sum(c, cd(c,'cost')*x(c) + cd(c,'pprice')*s_y(s,c));
repfinance(s,'revenue')   = sum(pcs, s_w(s,pcs)*pricecurve(pcs,'price'));

rep('profit')       = profit;
rep('revenue')      = sum(pcs, w(pcs)*pricecurve(pcs,'price'));
rep('cost')         = sum(c, cd(c,'cost')*x(c) + cd(c,'pprice')*y(c));
rep('landuse')      = sum(c, x(c))/land*100;

repc(c,'planted')   = x(c);
repc(c,'seedcost')  = cd(c,'cost')*x(c);
repc(c,'yield')     = yf*cd(c,'yield')*x(c);
repc(c,'sold')      = sum(pcs(c,seq), w(pcs));
repc(c,'sales')     = sum(pcs(c,seq), w(pcs)*pricecurve(pcs,'price'));
repc(c,'purchased') = y(c);
repc(c,'pcost')     = cd(c,'pprice')*y(c);
$offDotL


