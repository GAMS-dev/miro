$title Indus Basin Model Revised - IBMR (INDUS89,SEQ=181)

$onText
This file contains the basic data and definition of the surface water
system. Data is complete for year 1988. Some parameters could be
computed for future years using growth rates provided in this file,
others had to be estimated and entered. Enter the year for which the
setup is desired in Set isr (Set isr should have only one entry).


Ahmad, M, and Kutcher, G P, Irrigation Planning with Environmental
Considerations - A Case Study of Pakistans's Indus Basin. Tech. rep.,
The World Bank, 1992.

Changes for year 2000 runs:
   Growth of crop yields set to a maximum of 3%
   insert this line after growthcy parameter:
   growthcy(c,z)$(growthcy(c,z) > 3) = 3.0;

Keywords: linear programming, irrigation engineering, agricultural economics,
          resource allocation, water management, surface water system, water
          distribution, agricultural production, irrigation planning
$offText

$sTitle Set Definition
Set
   z           'agroclimatic zones'
               / nwfp    'northwest frontier mixed cropping'
                 pmw     'punjab wheat-mixed cropping'
                 pcw     'punjab cotton-wheat'
                 psw     'punjab sugarcane-wheat'
                 prw     'punjab rice-wheat'
                 scwn    'sind cotton-wheat north'
                 srwn    'sind rice-wheat north'
                 scws    'sind cotton wheat south'
                 srws    'sind rice-wheat south'  /
   pv          'provinces and country' / nwfp, punjab, sind, pakistan /
   pv1(pv)     'provinces'             / nwfp, punjab, sind           /
   pv2(pv)     'punjab and sind'       / punjab, sind                 /
   pvz(pv,z)   'province to zone map'  / nwfp  . nwfp
                                         punjab.(pmw,pcw,psw,prw)
                                         sind  .(scwn,scws,srwn,srws) /
   cq          'crop and livestock products'
               / basmati,  irri,      cotton,  rab-fod
                 gram,     maize,     mus+rap, kha-fod
                 sc-mill,  sc-gur,    wheat,   orchard
                 potatoes, onions,    chilli
                 cow-milk, buff-milk, meat /
   cc(cq)      'consumable commodities'
               / basmati, irri,     gram
                 maize,   mus+rap,  sc-gur
                 wheat,   potatoes, onions
                 chilli                    /
   c(cq)       'crops'
               / basmati  'rice crop'
                 irri     'rice crop'
                 cotton
                 rab-fod  'fodder crop'
                 gram
                 maize
                 mus+rap
                 kha-fod  'fodder crop'
                 sc-gur   'sugarcane processed at the farm'
                 sc-mill  'sugarcane for mill'
                 wheat
                 orchard
                 potatoes
                 onions
                 chilli   /
   cf(c)       'fodder crops'       / rab-fod, kha-fod /
   cnf(c)      'non-fodder crops'
   t           'technology'         / bullock, semi-mech /
   s           'sequence'           / standard 'standard sequence'
                                      la-plant 'late planting'
                                      el-plant 'early planting'
                                      qk-harv  'quick harvesting' /
   w           'water stress level' / standard 'no stress'
                                      light    'light stress'
                                      heavy    'heavy stress'
                                      january  'water stress in january' /
   g           'ground water quality types'   / fresh, saline        /
   gf(g)       'fresh ground water sub-zone'  / fresh                /
   gs(g)       'saline ground water sub-zone' / saline               /
   t1          'sub zones by gw quality'      / fresh, saline, total /
   r1          'resources'
               / cca       'culturable commanded area of the canal'
                 ccap      'canal capacity at the canal head'
                 ceff      'canal efficiency from barrage to the water course head'
                 wce-r     'water course command efficiency in rabi season'
                 wce-k     'water course command efficiency in kharif season'
                 flde      'field efficiency'
                 farmpop   'farm population in the irrigated'
                 farmhh    'number of agricultural households'
                 tractors  'tractor population in the irrigated area'
                 tubewells 'number of tubewells'
                 twc       'existing private tubewell capacity'
                 bullocks
                 cows
                 buffalos  /
   dc(r1)      'characteristics of canal command'
               / cca       'culturable commanded area of the canal'
                 ccap      'canal capacity at the canal head'
                 ceff      'canal efficiency from barrage to the water course head'
                 wce-r     'water course command efficiency in rabi season'
                 wce-k     'water course command efficiency in kharif season'
                 flde      'field efficiency' /
   sa          'subareas'                 / s1*s4        /
   wce(dc)     'watercourse efficiencies' / wce-r, wce-k /
   m1          'months and seasons'       / jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec, rabi, kharif, annual /
   m(m1)       'months'                   / jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec /
   wcem(wce,m) 'mapping from season to months for watercourse efficiencies' / wce-r.(oct,nov,dec,jan,feb,mar), wce-k.(apr,may,jun,jul,aug,sep) /
   sea(m1)     'seasons'                        / rabi, kharif /
   seam(sea,m) 'mapping from seasons to months' / rabi.(oct,nov,dec,jan,feb,mar), kharif.(apr,may,jun,jul,aug,sep) /
   sea1                                / rabi, kharif, annual /
   sea1m(sea1,m)                       / rabi.(oct,nov,dec,jan,feb,mar), kharif.(apr,may,jun,jul,aug,sep) /
   ci          'crop input outputs'    / straw-yld, nitrogen, phosphate, seed /
   p2(ci)                              / nitrogen, phosphate /
   a           'animal types'          / cow, bullock, buffalo /
   ai          'animals input output'  / tdn, dp, labor, cow-milk, buff-milk, meat /
   q(cq)       'livestock commodities' / cow-milk  'milk from cattle cow'
                                         buff-milk 'milk from buffalo cow'
                                         meat      'from cows buffaloes and bullocks' /
   nt          'nutrients for animals' / tdn       'total digestible nutrients'
                                         dp        'digestible protein'               /
   is          'irrigation system scenarios' / 1980*2000 /
   ps          'price scenarios'             /   87-88   /

*  change the set isr to setup data for desired year.
   isr(is)     'irrigation system scenario for this run' / 1988 /;

Scalar baseyear 'base year for crop yields' / 1988 /;
cnf(c)            = yes;
cnf(cf)           =  no;
pvz("pakistan",z) = yes;
sea1m("annual",m) = yes;

* Parameters to export data for zone model
* Set set1 / tdn, dp, labor, cow-milk, buff-milk, meat, fix-cost /;
* Following parameters are to store data for export to the zone models.
* Parameter
*    zone1xxxxx(z,c,t,s,w,m) 'bullock requirements (bullock pair hrs per month)'
*    zone2xxxxx(z,a,set1)    'input output coefficients for livestock'
*    zone3xxxxx(z,cq,*)      'demand data'
*    zone4xxxxx(z,c,ci)      'crop input output';

$sTitle Crop Data
Table land(c,z,t,s,w,m) 'land occupation by month'
                                                                       jan feb mar apr may jun jul aug sep oct nov dec
   (basmati,irri).(pmw,pcw,psw,prw).(bullock,
                          semi-mech).standard.standard                                       1   1   1   1   1  .5
   irri.   scwn.(bullock,semi-mech).standard.standard                                    1   1   1   1   1   1
   irri.   srwn.(bullock,semi-mech).standard.standard                                   .5   1   1   1   1   1  .5
   irri.   scws.(bullock,semi-mech).standard.standard                               .5   1   1   1   1   1   1
   irri.   srws.(bullock,semi-mech).standard.standard                                    1   1   1   1   1   1

   maize.  nwfp.bullock.            standard.standard                                        1   1   1   1   1
   maize. (pcw,psw,prw).bullock.    standard.standard                                       .5   1   1   1   1   1  .5
   maize. (pcw,psw,prw).semi-mech.  standard.standard                                            1   1   1   1   1  .5
   maize.   scwn.      (bullock,semi-mech).standard.standard                                .5   1   1   1   1  .5
   maize.   scws.      (bullock,semi-mech).standard.standard                            .5   1   1   1   1   1  .5
   maize.  nwfp.semi-mech.standard.standard                                                 .5   1   1   1   1

   mus+rap.nwfp.(bullock,semi-mech).       standard.standard                                     1   1   1   1   1   1
   mus+rap.(pmw,pcw).(bullock,semi-mech).  standard.standard             1   1   1                           1   1   1
   mus+rap. psw.     (bullock,semi-mech).  standard.standard             1   1  .5                      .5   1   1   1
   mus+rap. prw.     (bullock,semi-mech).  standard.standard             1  .5                           1   1   1   1
   mus+rap.(scwn,srwn,srws).(bullock,semi-mech).standard.standard        1   1  .5                       1   1   1   1
   mus+rap.scws.(bullock,semi-mech).       standard.standard             1   1   1                      .5   1   1   1

   (sc-gur,sc-mill).(nwfp,pmw,pcw,psw,prw, scwn,srwn,scws,srws).
                   (bullock,semi-mech).standard.standard                 1   1   1   1   1   1   1   1   1   1   1   1
   kha-fod.(nwfp,pmw,pcw).(bullock,semi-mech).standard.standard                 .5  .5  .5   1   1   1  .5  .5
   kha-fod.psw.(bullock,semi-mech).        standard.standard                    .5  .5  .5   1   1  .5  .5
   kha-fod.prw.(bullock,semi-mech).        standard.standard                    .5  .5  .5   1   1   1  .5  .5
   kha-fod.(scwn).     (bullock,semi-mech).standard.standard                    .5  .5   1   1  .5   1   1 .75
   kha-fod.scws.       (bullock,semi-mech).standard.standard                    .5  .5   1   1  .5   1   1   1
   kha-fod.(srwn,srws).(bullock,semi-mech).standard.standard                    .5  .5   1   1   1   1   1   1
   kha-fod.srws.(bullock,semi-mech).       la-plant. standard                            1   1   1   1   1   1

   rab-fod.nwfp.bullock.                   standard.standard             1   1   1   1   1              .5   1   1   1
   rab-fod.(nwfp,pcw).semi-mech.           standard.standard             1   1   1   1   1             .25   1   1   1
   rab-fod.pmw. bullock.                   standard.standard             1   1   1   1   1                   1   1   1
   rab-fod.pcw. bullock.                   standard.standard             1   1   1   1   1              .5   1   1   1
   rab-fod.psw. bullock.                   standard.standard             1   1   1   1  .5                   1   1   1
   rab-fod.prw. bullock.                   standard.standard             1   1   1   1   1                   1   1   1
   rab-fod.scwn.(bullock,semi-mech).       standard.standard             1   1   1   1                  .5   1   1   1
   rab-fod.srwn.(bullock,semi-mech).       standard.standard             1   1   1 .75                  .5   1   1   1
   rab-fod.scws.(bullock,semi-mech).       standard.standard             1   1   1   1 .25              .5   1   1   1
   rab-fod.srws.(bullock,semi-mech).       standard.standard             1   1   1   1  .5              .5   1   1   1
   rab-fod.pmw. semi-mech.                 standard.standard             1   1   1   1   1                   1   1   1
   rab-fod.psw. semi-mech.                 standard.standard             1   1   1   1  .5                 .25   1   1
   rab-fod.prw. semi-mech.                 standard.standard             1   1   1   1   1                  .5   1   1
   rab-fod.(srwn,srws).(bullock,semi-mech).standard.heavy                1   1                          .5   1   1   1
   rab-fod.(srwn,srws).(bullock,semi-mech).standard. light               1   1   1                      .5   1   1   1

   cotton. pmw.(bullock,semi-mech).        standard.standard                             1   1   1   1   1   1  .5
   cotton. (pcw,psw,prw).(bullock,semi-mech).standard.standard                           1   1   1   1   1   1  .5
   cotton. scwn.bullock.                   standard.standard                        .5   1   1   1   1   1   1  .5
   cotton. scws.bullock.                   standard.standard                         1   1   1   1   1   1   1
   cotton. srws.bullock.                   standard.standard                        .5   1   1   1   1   1   1
   cotton. scwn.semi-mech.                 standard.standard                             1   1   1   1   1   1  .5
   cotton. scws.semi-mech.                 standard.standard                        .5   1   1   1   1   1   1
   cotton. srws.semi-mech.                 standard.standard                             1   1   1   1   1   1
   cotton. pcw. bullock.  el-plant. standard                                        .5   1   1   1   1   1   1   1  .5
   cotton. pcw. semi-mech.la-plant. standard                                                 1   1   1   1   1   1  .5

   gram.(nwfp,pmw,pcw).bullock.            standard.standard             1   1   1                      .5   1   1   1
   gram.(nwfp,pmw,pcw).semi-mech.          standard.standard             1   1   1                     .25   1   1   1
   gram.(psw,prw).bullock.                 standard.standard             1   1   1                          .5   1   1
   gram.(psw,prw).semi-mech.               standard.standard             1   1   1                         .25   1   1
   gram.(scwn,srwn,scws,srws).(bullock,semi-mech).standard.standard      1   1   1                     .25   1   1   1

   wheat.  (nwfp,pmw,pcw,psw).bullock.la-plant.(standard,light,
                                                heavy,january)           1   1   1   1                           1   1
   wheat.  prw. bullock.  la-plant.(standard,light,heavy,january)        1   1   1   1                               1
   wheat. (scwn,srwn).bullock.la-plant.(standard,light,heavy,january)    1   1   1  .5                           1   1
   wheat. (scws,srws).bullock.la-plant.(standard,light,heavy,january)    1   1   1 .25                           1   1
   wheat. (nwfp,pmw,pcw,psw).bullock.qk-harv.
           (standard,light,heavy,january)                                1   1   1  .5                      .5   1   1

   wheat.  prw. bullock.  qk-harv.(standard,light,heavy,january)         1   1   1  .5                          .5   1
   wheat.  (scwn,srwn,scws,srws).bullock.qk-harv.(standard,light,
                                                  heavy,january)         1   1   1                          .5   1   1

   wheat.  (nwfp,pmw,pcw,psw).bullock.standard.(standard,light,heavy,
                                                         january)        1   1   1   1                          .5   1
   wheat.  prw. bullock.  standard.(standard,light,heavy,january)        1   1   1   1                          .5   1
   wheat. (scwn,srwn).bullock.standard.(standard,light,heavy,january)    1   1   1  .5                      .5   1   1
   wheat. (scws,srws).bullock.standard.(standard,light,heavy,january)    1   1   1 .25                      .5   1   1

   wheat. (nwfp,pmw,pcw,psw).semi-mech.la-plant.(standard,light,heavy)   1   1   1   1                               1
   wheat. (nwfp,pmw,psw).semi-mech.la-plant.january                      1   1   1   1                               1
   wheat.  pcw.semi-mech.la-plant.january                                1   1   1   1                               1
   wheat.  prw. semi-mech.la-plant.(standard,light,heavy,january)        1   1   1   1                               1
   wheat. (scwn,srwn).semi-mech.la-plant.
           (standard,light,heavy,january)                                1   1   1  .5                          .5   1
   wheat. (scws,srws).semi-mech.la-plant.
           (standard,light,heavy,january)                                1   1   1 .25                          .5   1

   wheat.  (nwfp,pmw,psw).semi-mech.qk-harv.
           (standard,light,heavy,january)                                1   1   1  .5                     .25   1   1
   wheat.  (pcw,prw).semi-mech.qk-harv.
           (standard,light,heavy,january)                                1   1   1  .5                          .5   1
   wheat.  (scwn,srwn,scws,srws).semi-mech.
            qk-harv.(standard,light,heavy,january)                       1   1   1                               1   1

   wheat.(nwfp,pmw,psw).semi-mech.standard.
           (standard,light,heavy,january)                                1   1   1   1                          .5   1

   wheat.  (pcw,prw).semi-mech.standard.(standard,light,heavy,january)   1   1   1   1                          .5   1
   wheat.(scwn,srwn).semi-mech.standard.(standard,light,heavy,january)   1   1   1  .5                           1   1
   wheat.(scws,srws).semi-mech.standard.(standard,light,heavy,january)   1   1   1 .25                           1   1
   (orchard). (nwfp,pmw,pcw,psw,prw, scwn,srwn,scws,srws).
                   (bullock,semi-mech).standard.standard                 1   1   1   1   1   1   1   1   1   1   1   1
   potatoes.(scwn,scws,srwn,srws) .semi-mech.standard.standard           1   1                           .5  1   1   1
   potatoes.(nwfp,pmw,pcw,psw,prw).semi-mech.standard.standard           1   1   1   1
   onions. (nwfp,pmw,pcw,psw,prw).semi-mech.standard.standard            1   1   1   1   1
   onions. (scwn,scws,srwn,srws). semi-mech.standard.standard            1                               1   1   1   1
   chilli. (nwfp,pmw,pcw,psw,prw).semi-mech.standard.standard                    .5  1   1   1   1   1   1   1   .5
   chilli. (scwn,scws,srwn,srws). semi-mech.standard.standard            1   1   1  .5                           .5  1;

Set tech(z,c,t,s,w) 'technology availability indicator';
tech(z,c,t,s,w)$sum(m, land(c,z,t,s,w,m)) = yes;

Table bullock(c,z,t,s,w,m) 'bullock power requirements (bullock pair hours per month)'
                                               jan  feb  mar  apr  may  jun  jul  aug  sep  oct  nov  dec
   basmati.pmw. bullock.  standard.standard                            22.0 17.2                 2.0 15.6
   basmati.pcw. bullock.  standard.standard                            22.0 17.2                 2.0 15.6
   basmati.psw. bullock.  standard.standard                            22.6 17.3                 2.0 17.4
   basmati.prw. bullock.  standard.standard                            32.5 24.6                 2.0 23.0
   basmati.pmw. semi-mech.standard.standard                                                          13.6
   basmati.pcw. semi-mech.standard.standard                                                          13.6
   basmati.psw. semi-mech.standard.standard                                                          15.4
   basmati.prw. semi-mech.standard.standard                                                          20.0

   rab-fod.nwfp.bullock.  standard.standard    3.0  3.0  3.0  3.0  2.0                 8.0 17.9  2.0  2.0
   rab-fod.pmw. bullock.  standard.standard    3.0  3.0  3.0  3.0  2.0                     17.9  7.6  2.0
   rab-fod.pcw. bullock.  standard.standard    2.0  4.0  4.0  2.0  2.0                10.1 13.0  7.6  2.0
   rab-fod.psw. bullock.  standard.standard    4.0  4.0  4.0  2.0  2.0                     16.5  9.7  2.0
   rab-fod.prw. bullock.  standard.standard    4.0  4.0  4.0  2.0  2.0                     12.7  9.4  2.0
   rab-fod.scwn.bullock.  standard.standard    2.0  2.0  2.0  1.0                      9.6 16.4  2.0  2.0
   rab-fod.srwn.bullock.  standard.standard    2.0  2.0  2.0  1.0                      8.9 14.8  2.0  2.0
   rab-fod.scws.bullock.  standard.standard    2.0  2.0  2.0  2.0  1.0                10.3 18.4  2.0  2.0
   rab-fod.srws.bullock.  standard.standard    2.0  2.0  2.0  2.0  1.0                10.7 18.3  2.0  2.0
   rab-fod.scwn.semi-mech.standard.standard    2.0  2.0  2.0  1.0                                2.0  2.0
   rab-fod.srwn.semi-mech.standard.standard    2.0  2.0  2.0  1.0                                2.0  2.0
   rab-fod.scws.semi-mech.standard.standard    2.0  2.0  2.0  2.0  1.0                           2.0  2.0
   rab-fod.srws.semi-mech.standard.standard    2.0  2.0  2.0  2.0  1.0                           2.0  2.0
   cotton. pmw. bullock.  standard.standard                       14.1  6.0            1.0  1.0  1.0  1.0
   cotton. pcw. bullock.  standard.standard                       17.1 15.2            1.0  1.0  1.0  1.0
   cotton. psw. bullock.  standard.standard                        9.0 13.0            1.0  1.0  1.0  1.0
   cotton. prw. bullock.  standard.standard                       10.3 17.1            1.0  1.0  1.0  1.0
   cotton. scwn.bullock.  standard.standard                   7.4 19.0  5.0            2.0  2.0  4.0
   cotton. scws.bullock.  standard.standard                  26.0       5.0            2.0  4.0
   cotton. srws.bullock.  standard.standard                   9.3 21.4  5.0            2.0  4.0
   cotton. (scwn,scws,srws).semi-mech.
                          standard.standard                             5.0
   cotton. pcw. bullock.  el-plant. standard                 16.0  4.0 12.3            1.0  1.0  1.0  1.0
   gram.   nwfp.bullock.  standard.standard                   7.0                      8.9 14.5
   gram.   pmw. bullock.  standard.standard              7.0                          10.7  5.6
   gram.   pcw. bullock.  standard.standard              7.0                          10.8  5.6
   gram.   psw. bullock.  standard.standard              7.0                               10.2  5.2
   gram.   prw. bullock.  standard.standard              7.0                               10.3  5.3
   gram.   scwn.bullock.  standard.standard              4.0  5.7                      6.4  8.1
   gram.   scws.bullock.  standard.standard              4.0  5.7                      6.4  8.1
   gram.   srwn.bullock.  standard.standard              4.9  6.4                      7.1  9.3
   gram.   srws.bullock.  standard.standard              4.9  6.4                      7.1  9.3

   irri.   pmw. bullock.  standard.standard                            18.9 19.2                 1.5 18.4
   irri.   pcw. bullock.  standard.standard                            18.9 19.2                 1.5 18.4
   irri.   psw. bullock.  standard.standard                            16.3 18.4                 1.5 20.2
   irri.   prw. bullock.  standard.standard                            32.6 24.0                 1.5 22.0
   irri.   scwn.bullock.  standard.standard                       10.5 18.3                 3.5 18.4
   irri.   srwn.bullock.  standard.standard                            10.7 18.6                13.4 10.0
   irri.   scws.bullock.  standard.standard                   9.1 10.3 17.1                 3.6 14.7
   irri.   srws.bullock.  standard.standard                       19.0 17.5                 3.7 10.0 10.0
   irri.   pmw. semi-mech.standard.standard                                                          16.4
   irri.   pcw. semi-mech.standard.standard                                                          16.4
   irri.   psw. semi-mech.standard.standard                                                          18.2
   irri.   prw. semi-mech.standard.standard                                                          20.0
   irri.   scwn.semi-mech.standard.standard                                                     16.4
   irri.   srwn.semi-mech.standard.standard                                                     10.0 10.0
   irri.   scws.semi-mech.standard.standard                                                     12.7
   irri.   srws.semi-mech.standard.standard                                                     10.0 10.0

   maize.  nwfp.bullock.  standard.standard                            41.4                 5.0
   maize.  pcw. bullock.  standard.standard                            10.8  4.5 14.2                 5.0
   maize.  psw. bullock.  standard.standard                            10.2  4.1 13.4                 5.0
   maize.  prw. bullock.  standard.standard                            10.2  4.1 13.4                 5.0
   maize.  scwn.bullock.  standard.standard                            15.1 23.9                 3.0
   maize.  scws.bullock.  standard.standard                       14.3 21.7                      3.5
   maize.  nwfp.semi-mech.standard.standard                             3.5
   maize.  pcw. semi-mech.standard.standard                                       3.0
   maize.  psw. semi-mech.standard.standard                                       3.0
   maize.  prw. semi-mech.standard.standard                                       3.0
   maize.  scwn.semi-mech.standard.standard                                                      3.0
   maize.  scws.semi-mech.standard.standard                                                      3.5

   mus+rap.nwfp.bullock.  standard.standard                                  7.4  8.0 14.5  1.0  1.0  1.0
   mus+rap.pmw. bullock.  standard.standard    1.0  1.0  1.5                               15.6  6.2  1.0
   mus+rap.pcw. bullock.  standard.standard    1.0  1.0  1.0                               10.8 10.1  1.0
   mus+rap.psw. bullock.  standard.standard    1.0  1.0  1.0                          10.2  9.3  1.0  1.0
   mus+rap.prw. bullock.  standard.standard    1.0  1.0                               15.6  1.0  1.0  1.0
   mus+rap.scwn.bullock.  standard.standard    1.0  1.0  1.0                          12.9  8.1  1.0  1.0
   mus+rap.srwn.bullock.  standard.standard    1.0  1.0  1.0                          13.7  7.5  1.0  1.0
   mus+rap.srws.bullock.  standard.standard    1.0  1.0  1.0                          13.7  7.5  1.0  1.0
   mus+rap.scws.bullock.  standard.standard    1.0  1.0  1.0                          12.8  6.9  1.0  1.0

   sc-gur. nwfp.bullock.  standard.standard   14.6 11.3 11.0       2.0  2.0            8.0 11.0 14.0 18.0
   sc-gur. (pmw,pcw,psw,prw).bullock.
                          standard.standard   17.8 11.6  8.2       1.8  1.8                     11.5 13.5
   sc-gur. scwn.bullock.  standard.standard    8.2  6.1 10.9            4.9
   sc-gur. srwn.bullock.  standard.standard    8.5  6.3 11.2            5.6
   sc-gur. scws.bullock.  standard.standard    9.1  7.4 12.1            5.8
   sc-gur. srws.bullock.  standard.standard    9.3  7.2 12.5            5.7
   sc-gur. nwfp.semi-mech.standard.standard    7.0  8.0  3.0       2.0  2.0           10.5  6.0 14.0 11.0
   sc-gur. (pmw,pcw,psw,prw).semi-mech.
                          standard.standard   11.0  6.5  2.5       1.8  1.8                     11.5 11.0
   sc-gur. scwn.semi-mech.standard.standard                             4.9
   sc-gur. srwn.semi-mech.standard.standard                             5.6
   sc-gur. scws.semi-mech.standard.standard                             5.8
   sc-gur. srws.semi-mech.standard.standard                             5.7

   sc-mill.nwfp.bullock.  standard.standard   18.0 18.0 14.0 12.0                     15.9 20.0 15.0 16.0
   sc-mill.(pmw,pcw,psw,prw).bullock.
                          standard.standard   16.8 15.1 15.2 12.0  8.0  1.8                     10.0 12.5
   sc-mill.scwn.bullock.  standard.standard    8.2  6.0 10.9            4.9
   sc-mill.srwn.bullock.  standard.standard    8.5  6.3 11.2            5.6
   sc-mill.scws.bullock.  standard.standard    9.1  7.4 12.1            5.8
   sc-mill.srws.bullock.  standard.standard    9.3  7.2 12.5            5.7
   sc-mill.nwfp.semi-mech.standard.standard    2.0  2.0                                     3.0
   sc-mill.(pmw,pcw,psw,prw).semi-mech.
                          standard.standard         2.5            1.8  1.8
   sc-mill.scwn.semi-mech.standard.standard                             4.9
   sc-mill.srwn.semi-mech.standard.standard                             5.6
   sc-mill.scws.semi-mech.standard.standard                             5.8
   sc-mill.srws.semi-mech.standard.standard                             5.7

   kha-fod.nwfp.bullock.  standard.standard             15.4  1.0  1.0 16.0  1.0 15.0  1.0  0.5
   kha-fod.pmw. bullock.  standard.standard             15.0  1.0  1.0 15.5  1.0 16.0  1.0  0.5
   kha-fod.pcw. bullock.  standard.standard             16.0  1.0  1.0 16.0  1.5 15.0  1.0  0.5
   kha-fod.psw. bullock.  standard.standard             17.0  1.0  1.0 16.5  1.0 16.0  1.0  0.5
   kha-fod.prw. bullock.  standard.standard             16.0  1.0  1.2 15.5  1.5 17.0  1.2  0.5
   kha-fod.scwn.bullock.  standard.standard             10.7  2.0  6.3  2.0  2.0  6.3  2.0  2.0
   kha-fod.scws.bullock.  standard.standard             10.3  2.0  6.4  2.0  2.0  6.4  2.0  2.0
   kha-fod.srwn.bullock.  standard.standard             11.3  2.0  8.7  2.0  2.0  5.8  2.0  2.0
   kha-fod.srws.bullock.  standard.standard             11.3  2.0  8.7  2.0  2.0  5.8  2.0  2.0
   kha-fod.srws.bullock.  la-plant. standard                      10.3  2.0  2.0  6.4  2.0  2.0

   rab-fod.srwn.bullock.  standard. heavy      2.0  2.0                                8.9 14.8  2.0  2.0
   rab-fod.srws.bullock.  standard. heavy      2.0  2.0                               10.7 18.3  2.0  2.0
   rab-fod.srwn.bullock.  standard. light      2.0  2.0  2.0                           8.9 14.8  2.0  2.0
   rab-fod.srws.bullock.  standard. light      2.0  2.0  2.0                          10.7 18.3  2.0  2.0
   rab-fod.srwn.semi-mech.standard. heavy      2.0  2.0                                          2.0  2.0
   rab-fod.srws.semi-mech.standard. heavy      2.0  2.0                                          2.0  2.0
   rab-fod.srwn.semi-mech.standard. light      2.0  2.0  2.0                                     2.0  2.0
   rab-fod.srws.semi-mech.standard. light      2.0  2.0  2.0                                     2.0  2.0

   wheat.  nwfp.bullock.  la-plant. heavy                     4.6  4.6                          38.4
   wheat.  pmw. bullock.  la-plant. heavy                     4.0  4.0                          27.1
   wheat.  pcw. bullock.  la-plant. heavy                     4.8  4.8                          39.2
   wheat.  psw. bullock.  la-plant. heavy                     4.7  4.7                          37.9
   wheat.  prw. bullock.  la-plant. heavy                     4.5  4.5                          17.2 12.9
   wheat.  scwn.bullock.  la-plant. heavy                     5.4                               31.2
   wheat.  srwn.bullock.  la-plant. heavy                     5.5                               33.4
   wheat.  scws.bullock.  la-plant. heavy                2.9  3.0                               32.3
   wheat.  srws.bullock.  la-plant. heavy                3.1  3.1                               31.6
   wheat.  nwfp.bullock.  la-plant. january                   4.6  4.6                          38.4
   wheat.  pmw. bullock.  la-plant. january                   4.0  4.0                          27.1
   wheat.  pcw. bullock.  la-plant. january                   4.8  4.8                          39.2
   wheat.  psw. bullock.  la-plant. january                   4.7  4.7                          37.9
   wheat.  prw. bullock.  la-plant. january                   4.5  4.5                          17.2 12.9
   wheat.  scwn.bullock.  la-plant. january                   5.4                               31.2
   wheat.  srwn.bullock.  la-plant. january                   5.5                               33.4
   wheat.  scws.bullock.  la-plant. january              2.9  3.0                               32.3
   wheat.  srws.bullock.  la-plant. january              3.1  3.1                               31.6
   wheat.  nwfp.bullock.  la-plant. light                     6.0  6.0                          38.4
   wheat.  pmw. bullock.  la-plant. light                     5.2  5.2                          27.1
   wheat.  pcw. bullock.  la-plant. light                     6.3  6.3                          39.2
   wheat.  psw. bullock.  la-plant. light                     6.1  6.1                          37.9
   wheat.  prw. bullock.  la-plant. light                     5.9  5.9                          17.2 12.9
   wheat.  scwn.bullock.  la-plant. light                     7.2                               31.2
   wheat.  srwn.bullock.  la-plant. light                     7.2                               33.4
   wheat.  scws.bullock.  la-plant. light                3.8  3.9                               32.3
   wheat.  srws.bullock.  la-plant. light                4.0  4.1                               31.6
   wheat.  nwfp.bullock.  la-plant. standard                  7.1  7.0                          38.4
   wheat.  pmw. bullock.  la-plant. standard                  6.2  6.1                          27.1
   wheat.  pcw. bullock.  la-plant. standard                  7.4  7.4                          39.2
   wheat.  psw. bullock.  la-plant. standard                  7.2  7.2                          37.9
   wheat.  prw. bullock.  la-plant. standard                  6.9  6.9                          17.2 12.9
   wheat.  scwn.bullock.  la-plant. standard                  8.4                               31.2
   wheat.  srwn.bullock.  la-plant. standard                  8.5                               33.4
   wheat.  scws.bullock.  la-plant. standard             4.5  4.6                               32.3
   wheat.  srws.bullock.  la-plant. standard             4.7  4.8                               31.6
   wheat.  nwfp.bullock.  qk-harv.  heavy                    10.4                          22.3 16.0
   wheat.  pmw. bullock.  qk-harv.  heavy                     9.1                          15.9 11.2
   wheat.  pcw. bullock.  qk-harv.  heavy                    10.9                          18.6 20.6
   wheat.  psw. bullock.  qk-harv.  heavy                    10.7                          18.3 19.6
   wheat.  prw. bullock.  qk-harv.  heavy                    10.3                               30.1
   wheat.  scwn.bullock.  qk-harv.  heavy                3.1  3.1                          17.5 13.7
   wheat.  srwn.bullock.  qk-harv.  heavy                3.2  3.2                          18.3 15.1
   wheat.  scws.bullock.  qk-harv.  heavy                6.7                               13.4 18.9
   wheat.  srws.bullock.  qk-harv.  heavy                7.0                               13.7 17.9
   wheat.  nwfp.bullock.  qk-harv.  january                  10.4                          22.3 16.0
   wheat.  pmw. bullock.  qk-harv.  january                   9.1                          15.9 11.2
   wheat.  pcw. bullock.  qk-harv.  january                  10.9                          18.6 20.6
   wheat.  psw. bullock.  qk-harv.  january                  10.7                          18.3 19.6
   wheat.  prw. bullock.  qk-harv.  january                  10.3                               30.1
   wheat.  scwn.bullock.  qk-harv.  january              3.1  3.1                          17.5 13.7
   wheat.  srwn.bullock.  qk-harv.  january              3.2  3.2                          18.3 15.1
   wheat.  scws.bullock.  qk-harv.  january              6.7                               13.4 18.9
   wheat.  srws.bullock.  qk-harv.  january              7.0                               13.7 17.9
   wheat.  nwfp.bullock.  qk-harv.  light                    13.6                          22.3 16.0
   wheat.  pmw. bullock.  qk-harv.  light                    11.9                          15.9 11.2
   wheat.  pcw. bullock.  qk-harv.  light                    14.3                          18.6 20.6
   wheat.  psw. bullock.  qk-harv.  light                    13.9                          18.3 19.6
   wheat.  prw. bullock.  qk-harv.  light                    13.4                               30.1
   wheat.  scwn.bullock.  qk-harv.  light                4.1  4.0                          17.5 13.7
   wheat.  srwn.bullock.  qk-harv.  light                4.1  4.1                          18.3 15.1
   wheat.  scws.bullock.  qk-harv.  light                8.6                               13.4 18.9
   wheat.  srws.bullock.  qk-harv.  light                9.1                               13.7 17.9
   wheat.  nwfp.bullock.  qk-harv.  standard                 16.0                          22.3 16.0
   wheat.  pmw. bullock.  qk-harv.  standard                 14.0                          15.9 11.2
   wheat.  pcw. bullock.  qk-harv.  standard                 16.8                          18.6 20.6
   wheat.  psw. bullock.  qk-harv.  standard                 16.4                          18.3 19.6
   wheat.  prw. bullock.  qk-harv.  standard                 15.8                               30.1
   wheat.  scwn.bullock.  qk-harv.  standard             4.7  4.8                          17.5 13.7
   wheat.  srwn.bullock.  qk-harv.  standard             4.7  4.8                          18.3 15.1
   wheat.  scws.bullock.  qk-harv.  standard            10.3                               13.4 18.9
   wheat.  srws.bullock.  qk-harv.  standard            10.8                               13.7 17.9
   wheat.  nwfp.bullock.  standard. heavy                     5.2  5.2                     22.3 16.0
   wheat.  pmw. bullock.  standard. heavy                     4.5  4.6                     15.9 11.2
   wheat.  pcw. bullock.  standard. heavy                     5.5  5.5                     18.6 20.6
   wheat.  psw. bullock.  standard. heavy                     5.3  5.4                     18.3 19.6
   wheat.  prw. bullock.  standard. heavy                     5.1  5.2                          30.1
   wheat.  scwn.bullock.  standard. heavy                     6.2                          17.5 13.7
   wheat.  srwn.bullock.  standard. heavy                     6.3                          18.3 15.1
   wheat.  scws.bullock.  standard. heavy                3.3  3.4                          13.4 18.9
   wheat.  srws.bullock.  standard. heavy                3.5  3.5                          13.7 17.9
   wheat.  nwfp.bullock.  standard. january                   5.2  5.2                     22.3 16.0
   wheat.  pmw. bullock.  standard. january                   4.5  4.6                     15.9 11.2
   wheat.  pcw. bullock.  standard. january                   5.5  5.5                     18.6 20.6
   wheat.  psw. bullock.  standard. january                   5.3  5.4                     18.3 19.6
   wheat.  prw. bullock.  standard. january                   5.1  5.2                          30.1
   wheat.  scwn.bullock.  standard. january                   6.2                          17.5 13.7
   wheat.  srwn.bullock.  standard. january                   6.3                          18.3 15.1
   wheat.  scws.bullock.  standard. january              3.3  3.4                          13.4 18.9
   wheat.  srws.bullock.  standard. january              3.5  3.5                          13.7 17.9
   wheat.  nwfp.bullock.  standard. light                     6.8  6.8                     22.3 16.0
   wheat.  pmw. bullock.  standard. light                     6.0  6.0                     15.9 11.2
   wheat.  pcw. bullock.  standard. light                     7.1  7.1                     18.6 20.6
   wheat.  psw. bullock.  standard. light                     6.9  7.0                     18.3 19.6
   wheat.  prw. bullock.  standard. light                     6.7  6.7                          30.1
   wheat.  scwn.bullock.  standard. light                     8.1                          17.5 13.7
   wheat.  srwn.bullock.  standard. light                     8.2                          18.3 15.1
   wheat.  scws.bullock.  standard. light                4.3  4.3                          13.4 18.9
   wheat.  srws.bullock.  standard. light                4.5  4.6                          13.7 17.9
   wheat.  nwfp.bullock.  standard. standard                  8.0  8.0                     22.3 16.0
   wheat.  pmw. bullock.  standard. standard                  7.0  7.0                     15.9 11.2
   wheat.  pcw. bullock.  standard. standard                  8.4  8.4                     18.6 20.6
   wheat.  psw. bullock.  standard. standard                  8.2  8.2                     18.3 19.6
   wheat.  prw. bullock.  standard. standard                  7.9  7.9                          30.1
   wheat.  scwn.bullock.  standard. standard                  9.5                          17.5 13.7
   wheat.  srwn.bullock.  standard. standard                  9.7                          18.3 15.1
   wheat.  scws.bullock.  standard. standard             5.1  5.2                          13.4 18.9
   wheat.  srws.bullock.  standard. standard             5.4  5.4                          13.7 17.9
   wheat.  nwfp.semi-mech.la-plant. heavy                     3.2  3.2
   wheat.  pmw. semi-mech.la-plant. heavy                     3.6  3.6
   wheat.  pcw. semi-mech.la-plant. heavy                     3.4  3.4
   wheat.  psw. semi-mech.la-plant. heavy                     3.2  3.2
   wheat.  prw. semi-mech.la-plant. heavy                     6.3  6.3
   wheat.  scwn.semi-mech.la-plant. heavy                     5.4
   wheat.  srwn.semi-mech.la-plant. heavy                     5.5
   wheat.  scws.semi-mech.la-plant. heavy                2.9  3.0
   wheat.  srws.semi-mech.la-plant. heavy                3.1  3.1
   wheat.  nwfp.semi-mech.la-plant. january                   3.2  3.2
   wheat.  pmw. semi-mech.la-plant. january                   3.6  3.6
   wheat.  pcw. semi-mech.la-plant. january                   3.4  3.4
   wheat.  psw. semi-mech.la-plant. january                   3.2  3.2
   wheat.  prw. semi-mech.la-plant. january                   6.3  6.3
   wheat.  scwn.semi-mech.la-plant. january                   5.4
   wheat.  srwn.semi-mech.la-plant. january                   5.5
   wheat.  scws.semi-mech.la-plant. january              2.9  3.0
   wheat.  srws.semi-mech.la-plant. january              3.1  3.1
   wheat.  nwfp.semi-mech.la-plant. light                     4.1  4.2
   wheat.  pmw. semi-mech.la-plant. light                     4.7  4.7
   wheat.  pcw. semi-mech.la-plant. light                     4.4  4.4
   wheat.  psw. semi-mech.la-plant. light                     4.2  4.2
   wheat.  prw. semi-mech.la-plant. light                     8.2  8.3
   wheat.  scwn.semi-mech.la-plant. light                     7.2
   wheat.  srwn.semi-mech.la-plant. light                     7.2
   wheat.  scws.semi-mech.la-plant. light                3.8  3.9
   wheat.  srws.semi-mech.la-plant. light                4.0  4.1
   wheat.  nwfp.semi-mech.la-plant. standard                  4.9  4.9
   wheat.  pmw. semi-mech.la-plant. standard                  5.5  5.5
   wheat.  pcw. semi-mech.la-plant. standard                  5.2  5.2
   wheat.  psw. semi-mech.la-plant. standard                  4.9  4.9
   wheat.  prw. semi-mech.la-plant. standard                  9.7  9.7
   wheat.  scwn.semi-mech.la-plant. standard                  8.4
   wheat.  srwn.semi-mech.la-plant. standard                  8.5
   wheat.  scws.semi-mech.la-plant. standard             4.5  4.6
   wheat.  srws.semi-mech.la-plant. standard             4.7  4.8
   wheat.  nwfp.semi-mech.qk-harv.  heavy                     7.3
   wheat.  pmw. semi-mech.qk-harv.  heavy                     8.2
   wheat.  pcw. semi-mech.qk-harv.  heavy                     7.7
   wheat.  psw. semi-mech.qk-harv.  heavy                     7.3
   wheat.  prw. semi-mech.qk-harv.  heavy                     7.2
   wheat.  scwn.semi-mech.qk-harv.  heavy                3.1  3.1
   wheat.  srwn.semi-mech.qk-harv.  heavy                3.2  3.2
   wheat.  scws.semi-mech.qk-harv.  heavy                6.7
   wheat.  srws.semi-mech.qk-harv.  heavy                7.0
   wheat.  nwfp.semi-mech.qk-harv.  january                   7.3
   wheat.  pmw. semi-mech.qk-harv.  january                   8.2
   wheat.  pcw. semi-mech.qk-harv.  january                   7.7
   wheat.  psw. semi-mech.qk-harv.  january                   7.3
   wheat.  prw. semi-mech.qk-harv.  january                   7.2
   wheat.  scwn.semi-mech.qk-harv.  january              3.1  3.1
   wheat.  srwn.semi-mech.qk-harv.  january              3.2  3.2
   wheat.  scws.semi-mech.qk-harv.  january              6.7
   wheat.  srws.semi-mech.qk-harv.  january              7.0
   wheat.  nwfp.semi-mech.qk-harv.  light                     9.5
   wheat.  pmw. semi-mech.qk-harv.  light                    10.7
   wheat.  pcw. semi-mech.qk-harv.  light                    10.0
   wheat.  psw. semi-mech.qk-harv.  light                     9.5
   wheat.  prw. semi-mech.qk-harv.  light                     9.4
   wheat.  scwn.semi-mech.qk-harv.  light                4.1  4.0
   wheat.  srwn.semi-mech.qk-harv.  light                4.1  4.1
   wheat.  scws.semi-mech.qk-harv.  light                8.6
   wheat.  srws.semi-mech.qk-harv.  light                9.1
   wheat.  nwfp.semi-mech.qk-harv.  standard                 11.2
   wheat.  pmw. semi-mech.qk-harv.  standard                 12.6
   wheat.  pcw. semi-mech.qk-harv.  standard                 11.8
   wheat.  psw. semi-mech.qk-harv.  standard                 11.2
   wheat.  prw. semi-mech.qk-harv.  standard                 11.1
   wheat.  scwn.semi-mech.qk-harv.  standard             4.7  4.8
   wheat.  srwn.semi-mech.qk-harv.  standard             4.7  4.8
   wheat.  scws.semi-mech.qk-harv.  standard            10.3
   wheat.  srws.semi-mech.qk-harv.  standard            10.8
   wheat.  nwfp.semi-mech.standard. heavy                     3.6  3.7
   wheat.  pmw. semi-mech.standard. heavy                     4.1  4.1
   wheat.  pcw. semi-mech.standard. heavy                     3.8  3.8
   wheat.  psw. semi-mech.standard. heavy                     3.6  3.7
   wheat.  prw. semi-mech.standard. heavy                     3.6  3.6
   wheat.  scwn.semi-mech.standard. heavy                     6.2
   wheat.  srwn.semi-mech.standard. heavy                     6.3
   wheat.  scws.semi-mech.standard. heavy                3.3  3.4
   wheat.  srws.semi-mech.standard. heavy                3.5  3.5
   wheat.  nwfp.semi-mech.standard. january                   3.6  3.7
   wheat.  pmw. semi-mech.standard. january                   4.1  4.1
   wheat.  pcw. semi-mech.standard. january                   3.8  3.8
   wheat.  psw. semi-mech.standard. january                   3.6  3.7
   wheat.  prw. semi-mech.standard. january                   3.6  3.6
   wheat.  scwn.semi-mech.standard. january                   6.2
   wheat.  srwn.semi-mech.standard. january                   6.3
   wheat.  scws.semi-mech.standard. january              3.3  3.4
   wheat.  srws.semi-mech.standard. january              3.5  3.5
   wheat.  nwfp.semi-mech.standard. light                     4.7  4.8
   wheat.  pmw. semi-mech.standard. light                     5.4  5.3
   wheat.  pcw. semi-mech.standard. light                     5.0  5.0
   wheat.  psw. semi-mech.standard. light                     4.7  4.8
   wheat.  prw. semi-mech.standard. light                     4.7  4.7
   wheat.  scwn.semi-mech.standard. light                     8.1
   wheat.  srwn.semi-mech.standard. light                     8.2
   wheat.  scws.semi-mech.standard. light                4.3  4.3
   wheat.  srws.semi-mech.standard. light                4.5  4.6
   wheat.  nwfp.semi-mech.standard. standard                  5.6  5.6
   wheat.  pmw. semi-mech.standard. standard                  6.3  6.3
   wheat.  pcw. semi-mech.standard. standard                  5.9  5.9
   wheat.  psw. semi-mech.standard. standard                  5.6  5.6
   wheat.  prw. semi-mech.standard. standard                  5.5  5.6
   wheat.  scwn.semi-mech.standard. standard                  9.5
   wheat.  srwn.semi-mech.standard. standard                  9.7
   wheat.  scws.semi-mech.standard. standard             5.1  5.2
   wheat.  srws.semi-mech.standard. standard             5.4  5.4
   orchard.(nwfp,pmw,pcw,psw,prw,scwn,scws,
       srwn,srws).bullock.standard.standard    5.   5.   3.5  3.5  3.5  3.5  3.5  3.5  3.5  3.5  3.5  3.5
   orchard.(nwfp,pmw,pcw,psw,prw,scwn,scws,
      srwn,srws).semi-mech.standard.standard   3.   3.   3.   2.   1.                  2.   2.   2.   2.

   chilli.(nwfp,pmw,psw,pcw,prw).semi-mech.
                         standard.standard     16.
   chilli.(scwn,scws,srwn,srws).semi-mech.
                         standard.standard                                                             8.;

* convert bullock pair hours to bullock hours
* zone1xxxxx(z,c,t,s,w,m) = bullock(c,z,t,s,w,m);
bullock(c,z,t,s,w,m) = bullock(c,z,t,s,w,m)*2;

Table labor(c,z,t,s,w,m) 'labor requirements for crops (man hours)'
                                                jan   feb   mar   apr   may   jun   jul   aug   sep   oct   nov   dec
   basmati.pmw. bullock.  standard. standard                                 29.1  88.78 65.9   5.6   5.6  47.9  17.6
   basmati.pcw. bullock.  standard. standard                                 29.1  88.78 65.9   5.6   5.6  47.9  17.6
   basmati.psw. bullock.  standard. standard                                 26.9  94.9  74.7   6.2   3.1  50.9  19.4
   basmati.prw. bullock.  standard. standard                                 44.2  91.8  63.9   8.4   5.6  41.9  25.0
   basmati.pmw. semi-mech.standard. standard                                  8.4  71.6  65.9   5.6   5.6  47.4  16.1
   basmati.pcw. semi-mech.standard. standard                                  8.4  71.6  65.9   5.6   5.6  47.4  16.1
   basmati.psw. semi-mech.standard. standard                                  5.6  74.4  74.7   6.2   3.1  50.4  16.9
   basmati.prw. semi-mech.standard. standard                                 11.0  67.7  63.9   8.4   5.6  41.4  22.5

   rab-fod.srwn.bullock.  standard. heavy      27.9  23.4                                       9.1  19.7  19.2  24.8
   rab-fod.srws.bullock.  standard. heavy      27.4  24.8                                      10.7  19.5  16.8  27.9
   rab-fod.srwn.bullock.  standard. light      27.9  23.4  22.5                                 9.1  19.7  19.2  24.8
   rab-fod.srws.bullock.  standard. light      27.4  24.8  19.7                                10.7  19.5  16.8  27.9
   rab-fod.nwfp.bullock.  standard. standard   42.8  42.1  32.6  24.6  10.9                     8.0  14.3  17.8  35.6
   rab-fod.pmw. bullock.  standard. standard   29.3  37.1  36.6  28.1   9.7                          13.35 16.5  34.5
   rab-fod.pcw. bullock.  standard. standard   32.3  41.5  41.8  29.4  18.2                    10.1  15.5  23.5  29.1
   rab-fod.psw. bullock.  standard. standard   31.9  39.8  41.7  28.6  13.4                           8.7  15.7  28.7
   rab-fod.prw. bullock.  standard. standard   31.4  32.0  34.5  23.4  21.9                           4.5  14.4  27.1
   rab-fod.scwn.bullock.  standard. standard   28.0  24.8  24.0  14.3                           9.6  21.5  19.2  24.5
   rab-fod.srwn.bullock.  standard. standard   27.9  23.4  22.5  15.0                           9.1  19.7  19.2  24.8
   rab-fod.scws.bullock.  standard. standard   29.0  23.1  19.8  17.5   8.6                    10.3  19.8  17.5  27.8
   rab-fod.srws.bullock.  standard. standard   27.4  24.8  19.7  18.8   9.3                    10.7  19.5  16.8  27.9
   rab-fod.srwn.semi-mech.standard. heavy      27.9  23.4                                       1.0   8.7  19.2  24.8
   rab-fod.srws.semi-mech.standard. heavy      27.4  24.8                                       1.2   9.3  16.8  27.9
   rab-fod.srwn.semi-mech.standard. light      27.9  23.4  22.5                                 1.0   8.7  19.2  24.8
   rab-fod.srws.semi-mech.standard. light      27.4  24.8  19.7                                 1.2   9.3  16.8  27.9
   rab-fod.nwfp.semi-mech.standard. standard   42.2  43.5  31.2  22.0   9.5                     2.05  8.02 16.29 33.0
   rab-fod.pmw. semi-mech.standard. standard   28.6  36.4  35.6  27.5   8.3                           2.17  7.96 24.08
   rab-fod.pcw. semi-mech.standard. standard   31.1  39.8  40.3  28.7  17.6                     7.5  13.2  21.6  28.4
   rab-fod.psw. semi-mech.standard. standard   30.5  38.4  40.3  27.1  11.9                           7.3  12.7  26.4
   rab-fod.prw. semi-mech.standard. standard   29.6  30.4  29.6  22.5  20.7                           3.4  12.8  25.7
   rab-fod.scwn.semi-mech.standard. standard   26.5  21.4  22.8  12.7                           1.2   8.8  17.5  22.8
   rab-fod.srwn.semi-mech.standard. standard   27.9  23.4  22.5  15.0                           1.0   8.7  19.2  24.8
   rab-fod.scws.semi-mech.standard. standard   29.0  23.1  19.8  17.5   8.6                     1.2   8.9  17.5  27.8
   rab-fod.srws.semi-mech.standard. standard   27.4  24.8  19.7  18.8   9.3                     1.2   9.3  16.8  27.9

   cotton. pcw. bullock.  el-plant. standard                     26.8   6.7  20.5   2.5   7.6  13.2  41.3  57.4  22.1
   cotton. pmw. bullock.  standard. standard                           16.85  9.6         2.9  12.0  50.74 70.5  26.5
   cotton. pcw. bullock.  standard. standard                           35.59 18.4   2.5   7.6  13.2  41.3  57.4  22.1
   cotton. psw. bullock.  standard. standard                           10.2  19.32  2.5   3.9  11.0  40.1  52.9  16.3
   cotton. prw. bullock.  standard. standard                           22.3  20.28        6.7  10.6  40.1  57.1  21.0
   cotton. scwn.bullock.  standard. standard                      7.4  23.5   7.0   2.0   2.0  17.5  39.4  66.2
   cotton. scws.bullock.  standard. standard                     31.2   2.0   7.0   2.0   2.0  38.6  80.4
   cotton. srws.bullock.  standard. standard                      9.5  23.8   7.0   2.0   2.0  28.5  84.1
   cotton. pcw. semi-mech.la-plant. standard                                 14.4   2.5   7.6  12.7  40.8  56.9  21.6
   cotton. pmw. semi-mech.standard. standard                           13.36  5.3         2.9  11.5  50.24 70.0  26.0
   cotton. pcw. semi-mech.standard. standard                            4.99  9.42  2.5   7.6  12.7  40.8  56.9  21.6
   cotton. psw. semi-mech.standard. standard                            4.41  9.28  2.5   3.9  10.5  39.6  52.4  15.8
   cotton. prw. semi-mech.standard. standard                            1.45  9.04        6.7  10.1  39.6  56.6  20.5
   cotton. scwn.semi-mech.standard. standard                           13.2                    15.8  37.4  62.2
   cotton. scws.semi-mech.standard. standard                     12.1         7.0   2.0   2.0  36.6  76.4
   cotton. srws.semi-mech.standard. standard                           12.4   7.0   2.0   2.0  28.5  83.2

   gram.   nwfp.bullock.  standard. standard    3.4              23.2                           8.9  18.0
   gram.   pmw. bullock.  standard. standard               21.5                                10.7   9.1   1.9   2.5
   gram.   pcw. bullock.  standard. standard               21.7                                10.8   8.9   0.7   2.5
   gram.   psw. bullock.  standard. standard    2.5        22.3                                      10.2   8.6   0.8
   gram.   prw. bullock.  standard. standard    2.5        22.6                                      10.3   8.5   0.7
   gram.   scwn.bullock.  standard. standard               23.5   9.2                           6.4  12.0   1.0   1.0
   gram.   scws.bullock.  standard. standard               23.5   9.2                           6.4  12.0   1.0   1.0
   gram.   srwn.bullock.  standard. standard               24.8  10.3                           7.1  13.4   1.0   1.0
   gram.   srws.bullock.  standard. standard               24.8  10.3                           7.1  13.4   1.0   1.0
   gram.   nwfp.semi-mech.standard. standard    3.4              20.5                           1.87  6.0
   gram.   pmw. semi-mech.standard. standard               19.0                                 1.63  4.64  0.9   2.5
   gram.   pcw. semi-mech.standard. standard               20.0                                 1.62  4.41  0.7   2.5
   gram.   psw. semi-mech.standard. standard    2.5        21.4                                       1.54  4.39  0.8
   gram.   prw. semi-mech.standard. standard    2.5        21.9                                       1.45  4.19  0.7
   gram.   scwn.semi-mech.standard. standard               21.4   5.5                           0.81  3.4   1.0   1.0
   gram.   scws.semi-mech.standard. standard               21.4   5.5                           0.81  3.4   1.0   1.0
   gram.   srwn.semi-mech.standard. standard               22.7   9.1                           0.96  3.6   1.0   1.0
   gram.   srws.semi-mech.standard. standard               22.7   9.1                           0.96  3.6   1.0   1.0

   irri.   pmw. bullock.  standard. standard                                 22.9  122.3 35.9   8.4   6.4  43.9  20.4
   irri.   pcw. bullock.  standard. standard                                 22.9  122.3 35.9   8.4   6.4  43.9  20.4
   irri.   psw. bullock.  standard. standard                                 22.3  135.7 37.8   6.2   6.2  51.7  22.2
   irri.   prw. bullock.  standard. standard                                 122.9 86.7  15.4   8.2   6.8  41.0  24.0
   irri.   scwn.bullock.  standard. standard                           13.6  81.0  13.6  32.0  13.6  39.1  20.4
   irri.   srwn.bullock.  standard. standard                            3.0  12.7  91.4  31.5  12.8  13.1  39.0  14.0
   irri.   scws.bullock.  standard. standard                      9.1  10.3  57.9  13.4  35.3  13.3  41.4  16.7
   irri.   srws.bullock.  standard. standard                           19.0  58.5  13.2  34.5  13.1  40.3  13.5  13.5
   irri.   pmw. semi-mech.standard. standard                                  5.3  105.5 29.9   8.4   6.4  43.4  17.9
   irri.   pcw. semi-mech.standard. standard                                  5.3  105.5 29.9   8.4   6.4  43.4  17.9
   irri.   psw. semi-mech.standard. standard                                  7.9  119.2 28.8   6.2   6.2  51.2  19.7
   irri.   prw. semi-mech.standard. standard                                 71.0  80.4  13.7   8.2   6.8  41.5  21.5
   irri.   scwn.semi-mech.standard. standard                            3.3   4.7  13.6  32.0  13.6  36.6  17.9
   irri.   srwn.semi-mech.standard. standard                            3.0   8.7  13.4  31.5  12.8  13.1  36.6  14.0
   irri.   scws.semi-mech.standard. standard                      2.3   2.9  43.5  13.4  35.3  13.3  38.8  14.2
   irri.   srws.semi-mech.standard. standard                            5.0  43.7  13.2  34.5  13.1  37.6  13.5  13.5

   maize.  nwfp.bullock.  standard. standard                                 43.4  38.6   0.7   2.7  46.1
   maize.  pcw. bullock.  standard. standard                                 10.8   4.5  16.7  44.2   2.5  26.8  27.0
   maize.  psw. bullock.  standard. standard                                 10.2   4.1  15.9  42.3   2.0  23.0  23.8
   maize.  prw. bullock.  standard. standard                                 10.2   4.1  15.9  42.3   2.0  23.0  23.8
   maize.  scwn.bullock.  standard. standard                                 18.65 27.4   3.2   3.2   3.2  57.4
   maize.  scws.bullock.  standard. standard                           17.9  26.8   3.0   3.0   3.0   3.0  59.3
   maize.  nwfp.semi-mech.standard. standard                                 14.6  38.6   0.7   2.7  43.8
   maize.  pcw. semi-mech.standard. standard                                        2.41  8.36 44.2   2.5  26.0  26.8
   maize.  psw. semi-mech.standard. standard                                        2.24  8.06 42.3   2.5  22.0  22.5
   maize.  prw. semi-mech.standard. standard                                        2.24  8.06 42.3   2.5  22.0  22.5
   maize.  scwn.semi-mech.standard. standard                                  3.1   5.2   3.2   3.2   3.2  57.4
   maize.  scws.semi-mech.standard. standard                            3.4   5.8   3.0   3.0   3.0   3.0  59.3

   mus+rap.nwfp.bullock.  standard. standard                                        8.9  10.5  18.0  14.6  23.0  23.0
   mus+rap.pmw. bullock.  standard. standard   25.7  15.0  20.5                                      19.3   9.0  23.0
   mus+rap.pcw. bullock.  standard. standard   21.75 20.5   6.0                                      10.8  13.6  13.9
   mus+rap.psw. bullock.  standard. standard   20.5   4.0   3.0                                10.2  12.8  13.2  23.0
   mus+rap.prw. bullock.  standard. standard    3.5   3.0                                      19.1  15.7  25.5  23.0
   mus+rap.scwn.bullock.  standard. standard    1.5   1.5   1.5                                15.1  12.3   1.5   1.5
   mus+rap.srwn.bullock.  standard. standard    1.5   1.5   1.5                                15.2  12.7   1.5   1.5
   mus+rap.srws.bullock.  standard. standard    1.5   1.5   1.5                                15.2  12.7   1.5   1.5
   mus+rap.scws.bullock.  standard. standard    1.5   1.5   1.5                                13.5   7.6   1.5   1.5
   mus+rap.nwfp.semi-mech.standard. standard                                        1.87  3.45  6.0  14.6  23.0  23.0
   mus+rap.pmw. semi-mech.standard. standard   25.7  13.5  18.0                                       3.8   5.5  23.0
   mus+rap.pcw. semi-mech.standard. standard   21.75 20.25  6.0                                       1.62  5.08 13.9
   mus+rap.psw. semi-mech.standard. standard   20.5   3.5   2.5                                 1.54  4.9  13.2  23.0
   mus+rap.prw. semi-mech.standard. standard    4.0   2.0                                       5.94 15.7  25.5  23.0
   mus+rap.scwn.semi-mech.standard. standard    1.0   1.0   1.0                                 3.7   2.8   1.0   1.0
   mus+rap.srwn.semi-mech.standard. standard    1.0   1.0   1.0                                 3.1   2.5   1.0   1.0
   mus+rap.srws.semi-mech.standard. standard    1.0   1.0   1.0                                 3.1   2.5   1.0   1.0
   mus+rap.scws.semi-mech.standard. standard    1.0   1.0   1.0                                 3.3   2.9   1.0   1.0

   sc-gur. nwfp.bullock.  standard. standard   154.6   147 57.4  23.4   5.4   5.74  3.4   3.4  24.0  54.0    216 208.2
   sc-gur.(pmw,pcw,psw,prw).
                bullock.  standard. standard     159 80.4  54.4  22.5   4.75  5.05  3.0   3.0   3.0   1.5  150.3 148.5
   sc-gur. scwn.bullock.  standard. standard   207.6 149.4 53.2  20.8   3.2   8.9   3.2   3.2   3.2   3.2  183.4 195.6
   sc-gur. srwn.bullock.  standard. standard   213.7 145.2 54.4  21.7   3.2   9.0   3.2   3.2   3.2   3.2  187.3   198
   sc-gur. scws.bullock.  standard. standard   207.1 163.5 53.4  31.5   3.2   9.0   3.2   3.2   3.2   3.2  187.4 191.5
   sc-gur. srws.bullock.  standard. standard   208.2 161.4 49.5  37.2   3.2   9.0   3.2   3.2   3.2   3.2  183.2 197.5
   sc-gur. nwfp.semi-mech.standard. standard   143.1 144.1 50.15 23.4   5.4   5.74  3.4   3.4  17.5  54.0    216 201.9
   sc-gur.(pmw,pcw,psw,prw).
                semi-mech.standard. standard   151.2 80.62 48.92 22.5   4.75  5.05  3.0   3.0   3.0   1.5  148.3 142.3
   sc-gur. scwn.semi-mech.standard. standard   201.2 143.4 48.5  16.4   3.2   8.9   3.2   3.2   3.2   3.2  179.2 193.4
   sc-gur. srwn.semi-mech.standard. standard   209.4 139.8 51.4  18.6   3.2   9.0   3.2   3.2   3.2   3.2  184.3 193.7
   sc-gur. scws.semi-mech.standard. standard   201.6 156.5 47.4  28.2   3.2   9.0   3.2   3.2   3.2   3.2  181.5 186.4
   sc-gur. srws.semi-mech.standard. standard   203.5 153.4 44.5  32.6   3.2   9.0   3.2   3.2   3.2   3.2  179.8 191.6

   sc-mill.nwfp.bullock.  standard. standard   120   115   95.0  65.0   5.4   5.74  3.4   3.4  23.4  54.0  107   125
   sc-mill.pmw. bullock.  standard. standard   87.0  85.2  74.0  64.5  44.5   5.05  3.0   3.0   3.0   1.5  74.0  85.5
   sc-mill.pcw. bullock.  standard. standard   90.0  85.0  95.0  72.1  30.0   5.05  3.0   3.0   3.0   1.5  85.0  95.0
   sc-mill.psw. bullock.  standard. standard   88.0  85.0  95.0  70.0  40.0   5.05  3.0   3.0   3.0   1.5  85.0  85.0
   sc-mill.prw. bullock.  standard. standard   90.0  95.0  90.9  65.0  38.5   5.05  3.0   3.0   3.0   1.5  84.0  85.5
   sc-mill.scwn.bullock.  standard. standard   87.5  85.0  92.1  47.5   3.2   8.4  35.4   3.2   3.2   3.2  93.1  87.3
   sc-mill.srwn.bullock.  standard. standard   89.3  87.1  94.2  43.1   3.2   9.2  37.8   3.2   3.2   3.2  94.2  89.1
   sc-mill.scws.bullock.  standard. standard   98.4  95.3  102.4 46.2   3.2   9.8  39.4   3.2   3.2   3.2  92.8  93.7
   sc-mill.srws.bullock.  standard. standard   94.7  96.5  103.4 41.4   3.2   8.9  41.3   3.2   3.2   3.2  91.7  89.8
   sc-mill.nwfp.semi-mech.standard. standard   116   116   90.6  58.0   5.4   5.74  3.4   3.4  22.0  51.0  102   120
   sc-mill.pmw. semi-mech.standard. standard   84.0  81.5  20.0  59.0  59.0  38.5   5.05  3.0   3.0   1.5  69.5  80.5
   sc-mill.pcw. semi-mech.standard. standard   86.5  81.5  90.0  64.0  34.5   5.05  3.0   3.0   3.0   1.5  80.5  90.5
   sc-mill.psw. semi-mech.standard. standard   86.0  82.0  90.5  64.5  35.0   5.05  3.0   3.0   3.0   1.5  81.5  84.0
   sc-mill.prw. semi-mech.standard. standard   87.0  92.5  87.0  62.5  35.0   5.05  3.0   3.0   3.0   1.5  80.0  81.5
   sc-mill.scwn.semi-mech.standard. standard   83.5  81.9  89.2  43.4   3.2   8.4  35.4   3.2   3.2   3.2  89.1  83.8
   sc-mill.srwn.semi-mech.standard. standard   84.1  82.3  90.1  44.5   3.2   9.2  35.7   3.2   3.2   3.2  91.2  84.5
   sc-mill.scws.semi-mech.standard. standard   96.5  92.7  99.4  46.2   3.2   9.8  39.4   3.2   3.2   3.2  89.8  91.1
   sc-mill.srws.semi-mech.standard. standard   92.8  93.7  101.8 39.1   3.2   8.9  40.5   3.2   3.2   3.2  89.5  84.7

   kha-fod.srws.bullock.  la-plant. standard                           13.4   3.0   3.0   8.3   3.0   3.0
   kha-fod.nwfp.bullock.  standard. standard               18.5   2.5   3.0  17.5   2.0  18.0   3.0   1.5
   kha-fod.pmw. bullock.  standard. standard               16.5   3.0   3.5  17.0   4.5  18.5   2.5   0.5
   kha-fod.pcw. bullock.  standard. standard               18.5   2.0   3.0  18.7   4.0  18.0   3.0   1.0
   kha-fod.psw. bullock.  standard. standard               18.5   1.8   2.5  25.5   4.5  27.5   2.5   1.2
   kha-fod.prw. bullock.  standard. standard               17.0   1.5   2.5  20.0   4.0  18.5   3.0   1.5
   kha-fod.scwn.bullock.  standard. standard               10.7   3.0   7.3   3.0   3.0   7.3   3.0   3.0
   kha-fod.scws.bullock.  standard. standard               13.4   3.0   8.2   3.0   3.0   8.3   3.0   3.0
   kha-fod.srwn.bullock.  standard. standard               14.3   3.0  11.4   3.0   3.0   7.5   3.0   3.0
   kha-fod.srws.bullock.  standard. standard               14.3   3.0  11.4   3.0   3.0   7.5   3.0   3.0
   kha-fod.srws.semi-mech.la-plant. standard                            4.0   2.1   0.8   2.3   1.2   1.2
   kha-fod.nwfp.semi-mech.standard. standard                7.0   1.0   2.5   6.0   3.5   7.0   3.0   1.5
   kha-fod.pmw. semi-mech.standard. standard                7.5   1.5   1.0   7.0   1.0   8.0   2.0   1.0
   kha-fod.pcw. semi-mech.standard. standard                6.0   1.3   4.5   6.5   4.2   5.5   2.0   1.5
   kha-fod.psw. semi-mech.standard. standard                5.5   1.2   1.7   7.0   4.5   4.0   2.0   1.5
   kha-fod.prw. semi-mech.standard. standard                6.0   1.5   2.0   8.5   3.8   5.5   2.2   1.5
   kha-fod.scwn.semi-mech.standard. standard                2.1   1.2   2.7   1.2   1.2   2.7   1.2   1.2
   kha-fod.scws.semi-mech.standard. standard                2.0   1.2   2.8   1.2   1.2   2.9   1.2   1.2
   kha-fod.srwn.semi-mech.standard. standard                2.2   1.2   2.1   1.2   1.2   2.3   1.2   1.2
   kha-fod.srws.semi-mech.standard. standard                2.2   1.2   2.1   1.2   1.2   2.3   1.2   1.2

   wheat.  nwfp.bullock.  la-plant. heavy       5.8   3.7   3.7  38.3  14.2                                43.6   3.7
   wheat.  pmw. bullock.  la-plant. heavy       3.4   2.9   1.7  36.3  12.1                                31.9   2.9
   wheat.  pcw. bullock.  la-plant. heavy       4.3   3.9   3.9  33.2  12.3                                45.2   3.9
   wheat.  psw. bullock.  la-plant. heavy       4.4   2.7   2.4  35.9  15.3                                41.4   4.0
   wheat.  prw. bullock.  la-plant. heavy       4.1   2.4   2.4  36.1  15.4                                17.2  19.1
   wheat.  scwn.bullock.  la-plant. heavy       3.2   3.2        24.0                                      34.0   3.2
   wheat.  srwn.bullock.  la-plant. heavy       3.2   3.2        25.4                                      37.5   3.2
   wheat.  scws.bullock.  la-plant. heavy       3.2   3.2  14.0   7.3                                      36.3   3.2
   wheat.  srws.bullock.  la-plant. heavy       3.2   3.2  10.7   7.1                                      36.2   3.2
   wheat.  nwfp.bullock.  la-plant. january     5.8   3.7   3.7  38.3  14.2                                43.6   3.7
   wheat.  pmw. bullock.  la-plant. january     3.4   2.9   1.7  36.3  12.1                                31.9   2.9
   wheat.  pcw. bullock.  la-plant. january     4.3   3.9   3.9  33.2  12.3                                45.2   3.9
   wheat.  psw. bullock.  la-plant. january     4.4   2.7   2.4  35.9  15.3                                41.4   4.0
   wheat.  prw. bullock.  la-plant. january     4.1   2.4   2.4  36.1  15.4                                17.2  19.1
   wheat.  scwn.bullock.  la-plant. january     3.2   3.2        24.0                                      34.0   3.2
   wheat.  srwn.bullock.  la-plant. january     3.2   3.2        25.4                                      37.5   3.2
   wheat.  scws.bullock.  la-plant. january     3.2   3.2  14.0   7.3                                      36.3   3.2
   wheat.  srws.bullock.  la-plant. january     3.2   3.2  10.7   7.1                                      36.2   3.2
   wheat.  nwfp.bullock.  la-plant. light       5.8   3.7   3.7  50.1  18.5                                43.6   3.7
   wheat.  pmw. bullock.  la-plant. light       3.4   2.9   1.7  47.5  15.8                                31.9   2.9
   wheat.  pcw. bullock.  la-plant. light       4.3   3.9   3.9  43.4  16.1                                45.2   3.9
   wheat.  psw. bullock.  la-plant. light       4.4   2.7   2.4  47.0  20.1                                41.4   4.0
   wheat.  prw. bullock.  la-plant. light       4.1   2.4   2.4  47.1  20.1                                17.2  19.1
   wheat.  scwn.bullock.  la-plant. light       3.2   3.2        31.4                                      34.0   3.2
   wheat.  srwn.bullock.  la-plant. light       3.2   3.2        33.2                                      37.5   3.2
   wheat.  scws.bullock.  la-plant. light       3.2   3.2  18.3   9.6                                      36.3   3.2
   wheat.  srws.bullock.  la-plant. light       3.2   3.2  13.9   9.3                                      36.2   3.2
   wheat.  nwfp.bullock.  la-plant. standard    5.8   3.7   3.7  59.0  21.8                                43.6   3.7
   wheat.  pmw. bullock.  la-plant. standard    3.4   2.9   1.7  55.9  18.6                                31.9   2.9
   wheat.  pcw. bullock.  la-plant. standard    4.3   3.9   3.9  51.4  18.9                                45.2   3.9
   wheat.  psw. bullock.  la-plant. standard    4.4   2.7   2.4  55.3  23.6                                41.4   4.0
   wheat.  prw. bullock.  la-plant. standard    4.1   2.4   2.4  55.5  23.7                                17.2  19.1
   wheat.  scwn.bullock.  la-plant. standard    3.2   3.2        37.0                                      34.0   3.2
   wheat.  srwn.bullock.  la-plant. standard    3.2   3.2        39.1                                      37.5   3.2
   wheat.  scws.bullock.  la-plant. standard    3.2   3.2  21.5  11.3                                      36.3   3.2
   wheat.  srws.bullock.  la-plant. standard    3.2   3.2  16.4  10.9                                      36.2   3.2
   wheat.  nwfp.bullock.  qk-harv.  heavy       5.8   3.7   3.7  59.8                                22.3  21.2   3.7
   wheat.  pmw. bullock.  qk-harv.  heavy       3.4   2.9   1.7  55.1                                15.9  16.0   2.9
   wheat.  pcw. bullock.  qk-harv.  heavy       4.3   3.9   3.9  57.3                                18.6  23.9   3.9
   wheat.  psw. bullock.  qk-harv.  heavy       4.4   2.7   2.4  58.4                                18.3  23.1   4.0
   wheat.  prw. bullock.  qk-harv.  heavy       4.1   2.4   2.4  58.6                                      33.9   2.4
   wheat.  scwn.bullock.  qk-harv.  heavy       3.2   3.2  19.2   8.3                                18.9  15.1   3.2
   wheat.  srwn.bullock.  qk-harv.  heavy       3.2   3.2  19.3   9.6                                20.2  17.3   3.2
   wheat.  scws.bullock.  qk-harv.  heavy       3.2   3.2  24.2                                      16.2  20.1   3.2
   wheat.  srws.bullock.  qk-harv.  heavy       3.2   3.2  22.3                                      16.5  19.7   3.2
   wheat.  nwfp.bullock.  qk-harv.  january     5.8   3.7   3.7  59.8                                22.3  21.2   3.7
   wheat.  pmw. bullock.  qk-harv.  january     3.4   2.9   1.7  55.1                                15.9  16.0   2.9
   wheat.  pcw. bullock.  qk-harv.  january     4.3   3.9   3.9  57.3                                18.6  23.9   3.9
   wheat.  psw. bullock.  qk-harv.  january     4.4   2.7   2.4  58.4                                18.3  23.1   4.0
   wheat.  prw. bullock.  qk-harv.  january     4.1   2.4   2.4  58.6                                      33.9   2.4
   wheat.  scwn.bullock.  qk-harv.  january     3.2   3.2  19.2   8.3                                18.9  15.1   3.2
   wheat.  srwn.bullock.  qk-harv.  january     3.2   3.2  19.3   9.6                                20.2  17.3   3.2
   wheat.  scws.bullock.  qk-harv.  january     3.2   3.2  24.2                                      16.2  20.1   3.2
   wheat.  srws.bullock.  qk-harv.  january     3.2   3.2  22.3                                      16.5  19.7   3.2
   wheat.  nwfp.bullock.  qk-harv.  light       5.8   3.7   3.7  78.2                                22.3  21.2   3.7
   wheat.  pmw. bullock.  qk-harv.  light       3.4   2.9   1.7  72.0                                15.9  16.0   2.9
   wheat.  pcw. bullock.  qk-harv.  light       4.3   3.9   3.9  74.9                                18.6  23.9   3.9
   wheat.  psw. bullock.  qk-harv.  light       4.4   2.7   2.4  76.3                                18.3  23.1   4.0
   wheat.  prw. bullock.  qk-harv.  light       4.1   2.4   2.4  76.6                                      33.9   2.4
   wheat.  scwn.bullock.  qk-harv.  light       3.2   3.2  25.1  10.8                                18.9  15.1   3.2
   wheat.  srwn.bullock.  qk-harv.  light       3.2   3.2  25.2  12.6                                20.2  17.3   3.2
   wheat.  scws.bullock.  qk-harv.  light       3.2   3.2  31.7                                      16.2  20.1   3.2
   wheat.  srws.bullock.  qk-harv.  light       3.2   3.2  29.3                                      16.5  19.7   3.2
   wheat.  nwfp.bullock.  qk-harv.  standard    5.8   3.7   3.7  91.9                                22.3  21.2   3.7
   wheat.  pmw. bullock.  qk-harv.  standard    3.4   2.9   1.7  84.8                                15.9  16.0   2.9
   wheat.  pcw. bullock.  qk-harv.  standard    4.3   3.9   3.9  88.1                                18.6  23.9   3.9
   wheat.  psw. bullock.  qk-harv.  standard    4.4   2.7   2.4  89.8                                18.3  23.1   4.0
   wheat.  prw. bullock.  qk-harv.  standard    4.1   2.4   2.4  90.1                                      33.9   2.4
   wheat.  scwn.bullock.  qk-harv.  standard    3.2   3.2  29.5  12.7                                18.9  15.1   3.2
   wheat.  srwn.bullock.  qk-harv.  standard    3.2   3.2  29.7  14.8                                20.2  17.3   3.2
   wheat.  scws.bullock.  qk-harv.  standard    3.2   3.2  37.3                                      16.2  20.1   3.2
   wheat.  srws.bullock.  qk-harv.  standard    3.2   3.2  34.3                                      16.5  19.7   3.2
   wheat.  nwfp.bullock.  standard. heavy       5.8   3.7   3.7  43.6  16.1                          22.3  21.2   3.7
   wheat.  pmw. bullock.  standard. heavy       3.4   2.9   1.7  41.3  13.8                          15.9  16.0   2.9
   wheat.  pcw. bullock.  standard. heavy       4.3   3.9   3.9  41.9  15.5                          18.6  23.9   3.9
   wheat.  psw. bullock.  standard. heavy       4.4   2.7   2.4  40.9  17.5                          18.3  23.1   4.0
   wheat.  prw. bullock.  standard. heavy       4.1   2.4   2.4  41.0  17.6                                33.9   2.4
   wheat.  scwn.bullock.  standard. heavy       3.2   3.2        27.4                                18.9  15.1   3.2
   wheat.  srwn.bullock.  standard. heavy       3.2   3.2        28.9                                20.2  17.3   3.2
   wheat.  scws.bullock.  standard. heavy       3.2   3.2  15.9   8.3                                16.2  20.1   3.2
   wheat.  srws.bullock.  standard. heavy       3.2   3.2  14.2   8.0                                16.5  19.7   3.2
   wheat.  nwfp.bullock.  standard. january     5.8   3.7   3.7  43.6  16.1                          22.3  21.2   3.7
   wheat.  pmw. bullock.  standard. january     3.4   2.9   1.7  41.3  13.8                          15.9  16.0   2.9
   wheat.  pcw. bullock.  standard. january     4.3   3.9   3.9  41.9  15.5                          18.6  23.9   3.9
   wheat.  psw. bullock.  standard. january     4.4   2.7   2.4  40.9  17.5                          18.3  23.1   4.0
   wheat.  prw. bullock.  standard. january     4.1   2.4   2.4  41.0  17.6                                33.9   2.4
   wheat.  scwn.bullock.  standard. january     3.2   3.2        27.4                                18.9  15.1   3.2
   wheat.  srwn.bullock.  standard. january     3.2   3.2        18.9                                20.2  17.3   3.2
   wheat.  scws.bullock.  standard. january     3.2   3.2  15.9   8.3                                16.2  20.1   3.2
   wheat.  srws.bullock.  standard. january     3.2   3.2  14.2   8.0                                16.5  19.7   3.2
   wheat.  nwfp.bullock.  standard. light       5.8   3.7   3.7  57.0  21.1                          22.3  21.2   3.7
   wheat.  pmw. bullock.  standard. light       3.4   2.9   1.7  54.1  18.0                          15.9  16.0   2.9
   wheat.  pcw. bullock.  standard. light       4.3   3.9   3.9  54.7  20.2                          18.6  23.9   3.9
   wheat.  psw. bullock.  standard. light       4.4   2.7   2.4  53.5  22.9                          18.3  23.1   4.0
   wheat.  prw. bullock.  standard. light       4.1   2.4   2.4  53.6  23.0                                33.9   2.4
   wheat.  scwn.bullock.  standard. light       3.2   3.2        35.9                                18.9  15.1   3.2
   wheat.  srwn.bullock.  standard. light       3.2   3.2        37.8                                20.2  17.3   3.2
   wheat.  scws.bullock.  standard. light       3.2   3.2  20.8  10.9                                16.2  20.1   3.2
   wheat.  srws.bullock.  standard. light       3.2   3.2  18.6  10.7                                16.5  19.7   3.2
   wheat.  nwfp.bullock.  standard. standard    5.8   3.7   3.7  67.1  24.8                          22.3  21.2   3.7
   wheat.  pmw. bullock.  standard. standard    3.4   2.9   1.7  63.6  21.2                          15.9  16.0   2.9
   wheat.  pcw. bullock.  standard. standard    4.3   3.9   3.9  64.4  23.8                          18.6  23.9   3.9
   wheat.  psw. bullock.  standard. standard    4.4   2.7   2.4  62.9  26.9                          18.3  23.1   4.0
   wheat.  prw. bullock.  standard. standard    4.1   2.4   2.4  63.1  27.0                                33.9   2.4
   wheat.  scwn.bullock.  standard. standard    3.2   3.2        42.2                                18.9  15.1   3.2
   wheat.  srwn.bullock.  standard. standard    3.2   3.2        44.5                                20.2  17.3   3.2
   wheat.  scws.bullock.  standard. standard    3.2   3.2  24.5  12.8                                16.2  20.1   3.2
   wheat.  srws.bullock.  standard. standard    3.2   3.2  21.9  12.4                                16.5  19.7   3.2
   wheat.  nwfp.semi-mech.la-plant. heavy       5.8   3.7   3.7  37.2  12.4                                13.8   3.7
   wheat.  pmw. semi-mech.la-plant. heavy       3.4   2.9   1.7  33.1  11.1                                 2.3  10.7
   wheat.  pcw. semi-mech.la-plant. heavy       4.3   3.9   3.9  34.3  11.2                                 8.0  11.5
   wheat.  psw. semi-mech.la-plant. heavy       4.4   2.7   2.4  32.4  13.9                                 3.4  15.6
   wheat.  prw. semi-mech.la-plant. heavy       4.1   2.4   2.4  32.3  13.8                                      11.9
   wheat.  scwn.semi-mech.la-plant. heavy       3.2   3.2        22.4                                       3.1   9.1
   wheat.  srwn.semi-mech.la-plant. heavy       3.2   3.2        25.4                                       2.9   9.6
   wheat.  scws.semi-mech.la-plant. heavy       3.2   3.2  14.0   7.3                                       3.2  11.0
   wheat.  srws.semi-mech.la-plant. heavy       3.2   3.2  10.7   7.1                                       3.2  11.0
   wheat.  nwfp.semi-mech.la-plant. january     5.8   3.7   3.7  37.2  12.4                                13.8   3.7
   wheat.  pmw. semi-mech.la-plant. january     3.4   2.9   1.7  33.1  11.1                                 2.3  10.7
   wheat.  pcw. semi-mech.la-plant. january     4.3   3.9   3.9  34.3  11.2                                 8.0  11.5
   wheat.  psw. semi-mech.la-plant. january     4.4   2.7   2.4  32.4  13.9                                 3.4  15.6
   wheat.  prw. semi-mech.la-plant. january     4.1   2.4   2.4  32.3  13.8                                      11.9
   wheat.  scwn.semi-mech.la-plant. january     3.2   3.2        22.9                                       3.1   9.1
   wheat.  srwn.semi-mech.la-plant. january     3.2   3.2        25.4                                       2.9   9.6
   wheat.  scws.semi-mech.la-plant. january     3.2   3.2  14.0   7.3                                       3.2  11.0
   wheat.  srws.semi-mech.la-plant. january     3.2   3.2  10.7   7.1                                       3.2  11.0
   wheat.  nwfp.semi-mech.la-plant. light       5.8   3.7   3.7  48.6  16.2                                13.8   3.7
   wheat.  pmw. semi-mech.la-plant. light       3.4   2.9   1.7  43.3  14.4                                 2.3  10.7
   wheat.  pcw. semi-mech.la-plant. light       4.3   3.9   3.9  44.9  14.6                                 8.0  11.5
   wheat.  psw. semi-mech.la-plant. light       4.4   2.7   2.4  42.4  18.2                                 3.4  15.6
   wheat.  prw. semi-mech.la-plant. light       4.1   2.4   2.4  42.2  18.1                                      11.9
   wheat.  scwn.semi-mech.la-plant. light       3.2   3.2        31.4                                       3.1   9.1
   wheat.  srwn.semi-mech.la-plant. light       3.2   3.2        33.2                                       2.9   9.6
   wheat.  scws.semi-mech.la-plant. light       3.2   3.2  18.3   9.6                                       3.2  11.0
   wheat.  srws.semi-mech.la-plant. light       3.2   3.2  13.9   9.3                                       3.2  11.0
   wheat.  nwfp.semi-mech.la-plant. standard    5.8   3.7   3.7  57.2  19.1                                13.8   3.7
   wheat.  pmw. semi-mech.la-plant. standard    3.4   2.9   1.7  51.0  17.0                                 2.3  10.7
   wheat.  pcw. semi-mech.la-plant. standard    4.3   3.9   3.9  52.8  17.2                                 8.0  11.5
   wheat.  psw. semi-mech.la-plant. standard    4.4   2.7   2.4  49.4  21.4                                 3.4  15.6
   wheat.  prw. semi-mech.la-plant. standard    4.1   2.4   2.4  49.7  21.3                                      11.9
   wheat.  scwn.semi-mech.la-plant. standard    3.2   3.2        37.0                                       3.1   9.1
   wheat.  srwn.semi-mech.la-plant. standard    3.2   3.2        39.1                                       2.9   9.6
   wheat.  scws.semi-mech.la-plant. standard    3.2   3.2  21.5  11.3                                       3.2  11.0
   wheat.  srws.semi-mech.la-plant. standard    3.2   3.2  16.4  10.9                                       3.2  11.0
   wheat.  nwfp.semi-mech.qk-harv.  heavy       5.8   3.7   3.7  56.4                                 4.1   9.7   3.7
   wheat.  pmw. semi-mech.qk-harv.  heavy       3.4   2.9   1.7  50.2                                 2.3   7.8   2.9
   wheat.  pcw. semi-mech.qk-harv.  heavy       4.3   3.9   3.9  44.0                                      15.6   3.9
   wheat.  psw. semi-mech.qk-harv.  heavy       4.4   2.7   2.4  58.4                                 3.4  11.6   4.0
   wheat.  prw. semi-mech.qk-harv.  heavy       4.1   2.4   2.4  52.4                                       9.5   2.4
   wheat.  scwn.semi-mech.qk-harv.  heavy       3.2   3.2  19.2   8.2                                       9.0   3.2
   wheat.  srwn.semi-mech.qk-harv.  heavy       3.2   3.2  19.3   9.6                                       9.3   3.2
   wheat.  scws.semi-mech.qk-harv.  heavy       3.2   3.2  24.2                                            11.0   3.2
   wheat.  srws.semi-mech.qk-harv.  heavy       3.2   3.2  22.3                                            11.0   3.2
   wheat.  nwfp.semi-mech.qk-harv.  january     5.8   3.7   3.7  56.4                                 4.1   9.7   3.7
   wheat.  pmw. semi-mech.qk-harv.  january     3.4   2.9   1.7  50.2                                 2.3   7.8   2.9
   wheat.  pcw. semi-mech.qk-harv.  january     4.3   3.9   3.9  44.0                                      15.6   3.9
   wheat.  psw. semi-mech.qk-harv.  january     4.4   2.7   2.4  58.4                                 3.4  11.6   4.0
   wheat.  prw. semi-mech.qk-harv.  january     4.1   2.4   2.4  52.4                                       9.5   2.4
   wheat.  scwn.semi-mech.qk-harv.  january     3.2   3.2  19.2   8.2                                       9.0   3.2
   wheat.  srwn.semi-mech.qk-harv.  january     3.2   3.2  19.3   9.6                                       9.3   3.2
   wheat.  scws.semi-mech.qk-harv.  january     3.2   3.2  24.2                                            11.0   3.2
   wheat.  srws.semi-mech.qk-harv.  january     3.2   3.2  22.3                                            11.0   3.2
   wheat.  nwfp.semi-mech.qk-harv.  light       5.8   3.7   3.7  73.8                                 4.1   9.7   3.7
   wheat.  pmw. semi-mech.qk-harv.  light       3.4   2.9   1.7  65.7                                 2.3   7.8   2.9
   wheat.  pcw. semi-mech.qk-harv.  light       4.3   3.9   3.9  67.7                                      15.6   3.9
   wheat.  psw. semi-mech.qk-harv.  light       4.4   2.7   2.4  68.9                                 3.4  11.6   4.0
   wheat.  prw. semi-mech.qk-harv.  light       4.1   2.4   2.4  68.6                                       9.5   2.4
   wheat.  scwn.semi-mech.qk-harv.  light       3.2   3.2  25.1  10.8                                       9.0   3.2
   wheat.  srwn.semi-mech.qk-harv.  light       3.2   3.2  25.2  12.6                                       9.3   3.2
   wheat.  scws.semi-mech.qk-harv.  light       3.2   3.2  31.7                                            11.0   3.2
   wheat.  srws.semi-mech.qk-harv.  light       3.2   3.2  29.3                                            11.0   3.2
   wheat.  nwfp.semi-mech.qk-harv.  standard    5.8   3.7   3.7  86.8                                 4.1   9.7   3.7
   wheat.  pmw. semi-mech.qk-harv.  standard    3.4   2.9   1.7  77.3                                 2.3   7.8   2.9
   wheat.  pcw. semi-mech.qk-harv.  standard    4.3   3.9   3.9  79.6                                      15.6   3.9
   wheat.  psw. semi-mech.qk-harv.  standard    4.4   2.7   2.4  81.1                                 3.4  11.6   4.0
   wheat.  prw. semi-mech.qk-harv.  standard    4.1   2.4   2.4  80.7                                       9.5   2.4
   wheat.  scwn.semi-mech.qk-harv.  standard    3.2   3.2  29.5  12.7                                       9.0   3.2
   wheat.  srwn.semi-mech.qk-harv.  standard    3.2   3.2  29.7  14.8                                       9.3   3.2
   wheat.  scws.semi-mech.qk-harv.  standard    3.2   3.2  37.3                                            11.0   3.2
   wheat.  srws.semi-mech.qk-harv.  standard    3.2   3.2  34.3                                            11.0   3.2
   wheat.  nwfp.semi-mech.standard. heavy       5.8   3.7   3.7  42.3  14.1                           4.1   9.7   3.7
   wheat.  pmw. semi-mech.standard. heavy       3.4   2.9   1.7  37.7  12.5                           2.3   7.8   2.9
   wheat.  pcw. semi-mech.standard. heavy       4.3   3.9   3.9  39.1  12.7                                15.6   3.9
   wheat.  psw. semi-mech.standard. heavy       4.4   2.7   2.4  36.9  15.8                           3.4  11.6   4.0
   wheat.  prw. semi-mech.standard. heavy       4.1   2.4   2.4  36.7  15.7                                 9.5   2.4
   wheat.  scwn.semi-mech.standard. heavy       3.2   3.2        27.4                                       9.0   3.2
   wheat.  srwn.semi-mech.standard. heavy       3.2   3.2        28.9                                       9.3   3.2
   wheat.  scws.semi-mech.standard. heavy       3.2   3.2  15.9   8.3                                      11.0   3.2
   wheat.  srws.semi-mech.standard. heavy       3.2   3.2  14.2   8.0                                      11.0   3.2
   wheat.  nwfp.semi-mech.standard. january     5.8   3.7   3.7  42.3  14.1                           4.1   9.7   3.7
   wheat.  pmw. semi-mech.standard. january     3.4   2.9   1.7  37.7  12.5                           2.3   7.8   2.9
   wheat.  pcw. semi-mech.standard. january     4.3   3.9   3.9  39.1  12.7                                15.6   3.9
   wheat.  psw. semi-mech.standard. january     4.4   2.7   2.4  36.9  15.8                           3.4  11.6   4.0
   wheat.  prw. semi-mech.standard. january     4.1   2.4   2.4  36.7  15.7                                 9.5   2.4
   wheat.  scwn.semi-mech.standard. january     3.2   3.2        27.4                                       9.0   3.2
   wheat.  srwn.semi-mech.standard. january     3.2   3.2        28.9                                       9.3   3.2
   wheat.  scws.semi-mech.standard. january     3.2   3.2  15.9   8.3                                      11.0   3.2
   wheat.  srws.semi-mech.standard. january     3.2   3.2  14.2   8.0                                      11.0   3.2
   wheat.  nwfp.semi-mech.standard. light       5.8   3.7   3.7  55.3  18.4                           4.1   9.7   3.7
   wheat.  pmw. semi-mech.standard. light       3.4   2.9   1.7  49.3  16.4                           2.3   7.8   2.9
   wheat.  pcw. semi-mech.standard. light       4.3   3.9   3.9  51.1  16.6                                15.6   3.9
   wheat.  psw. semi-mech.standard. light       4.4   2.7   2.4  48.3  20.7                           3.4  11.6   4.0
   wheat.  prw. semi-mech.standard. light       4.1   2.4   2.4  48.0  20.6                                 9.5   2.4
   wheat.  scwn.semi-mech.standard. light       3.2   3.2        35.9                                       9.0   3.2
   wheat.  srwn.semi-mech.standard. light       3.2   3.2        37.8                                       9.3   3.2
   wheat.  scws.semi-mech.standard. light       3.2   3.2  20.8  10.9                                      11.0   3.2
   wheat.  srws.semi-mech.standard. light       3.2   3.2  18.6  10.7                                      11.0   3.2
   wheat.  nwfp.semi-mech.standard. standard    5.8   3.7   3.7  65.1  21.7                           4.1   9.7   3.7
   wheat.  pmw. semi-mech.standard. standard    3.4   2.9   1.7  58.0  19.3                           2.3   7.8   2.9
   wheat.  pcw. semi-mech.standard. standard    4.3   3.9   3.9  60.1  19.5                                15.6   3.9
   wheat.  psw. semi-mech.standard. standard    4.4   2.7   2.4  56.8  24.3                           3.4  11.6   4.0
   wheat.  prw. semi-mech.standard. standard    4.1   2.4   2.4  56.5  24.2                                 9.5   2.4
   wheat.  scwn.semi-mech.standard. standard    3.2   3.2        42.2                                       9.0   3.2
   wheat.  srwn.semi-mech.standard. standard    3.2   3.2        44.5                                       9.3   3.2
   wheat.  scws.semi-mech.standard. standard    3.2   3.2  24.5  12.8                                      11.0   3.2
   wheat.  srws.semi-mech.standard. standard    3.2   3.2  21.9  12.4                                      11.0   3.2

   orchard.(nwfp,pmw,pcw,psw,prw).
   (bullock,semi-mech).standard.standard         41  41.8  17.3  14.5  14.5   22.   22.    50.  29.   39.  56.5  28.3
   orchard.(scwn,scws,srwn,srws).
   (bullock,semi-mech).standard.standard         31  31.   31.   31.   25.    22.   22.    22.  31.   31.  31.   31.

   potatoes.(scwn,scws,srwn,srws).
                 semi-mech.standard.standard    100   100                                        50    50   100    50
   potatoes.(nwfp,pmw,psw,pcw,prw).semi-mech.
                           standard.standard     70    24         150                                               8
   onions.(nwfp,pmw,psw,pcw,prw).semi-mech.
                           standard.standard    160     8    32    32   160
   onions.(scwn,scws,srwn,srws).semi-mech.
                           standard.standard    160                                             160     8    32    32
   chilli.(nwfp,pmw,pcw,psw,prw).semi-mech.
                           standard.standard                100     2    20    10     20    10   20     8     8
   chilli.(scwn,scws,srwn,srws).semi-mech.
                           standard.standard     16    32    28    28                                             100    ;

Table water(c,z,t,s,w,m) 'water requirements (acre feet per acre)'
                                                                 jan  feb  mar  apr  may  jun  jul  aug  sep  oct  nov  dec
   basmati.(pmw,pcw,psw).(bullock,semi-mech).standard.standard                            .37  .84  .93  .93  .47
   basmati.prw.(bullock,semi-mech).standard.standard                                      .56  .84  .93  .93  .56

   rab-fod.(psw,prw).(bullock,semi-mech).standard.standard       .20  .30  .50  .60  .35                      .25  .15  .15
   rab-fod.(scws,srws).(bullock,semi-mech).standard.standard     .27  .37  .53  .64                      .21  .32  .27  .21
   rab-fod.srwn.(bullock,semi-mech).standard.heavy               .20                                     .20  .25  .15  .15
   rab-fod.srws.(bullock,semi-mech).standard.heavy               .27                                     .21  .32  .27  .21
   rab-fod.srwn.(bullock,semi-mech).standard.light               .20  .30                                .20  .25  .15  .15
   rab-fod.srws.(bullock,semi-mech).standard.light               .27  .37                                .21  .32  .27  .21
   rab-fod.nwfp.(bullock,semi-mech).standard.standard            .20  .25  .40  .45  .15                 .20  .25  .15  .15
   rab-fod.pmw.(bullock,semi-mech).standard.standard             .20  .25  .40  .50  .10                      .25  .15  .15
   rab-fod.pcw.(bullock,semi-mech).standard.standard             .20  .30  .45  .55  .25                 .20  .25  .15  .15
   rab-fod.scwn.(bullock,semi-mech).standard.standard            .21  .32  .48  .59                      .21  .27  .16  .16
   rab-fod.srwn.(bullock,semi-mech).standard.standard            .20  .30  .45  .55                      .20  .25  .15  .15

   cotton. pcw. bullock.  el-plant. standard                                    .19  .09  .37  .33  .37  .47  .33  .09
   cotton.(pmw,psw,prw).(bullock,semi-mech).standard.standard                        .19  .23  .28  .37  .37  .23  .09
   cotton.pcw.(bullock,semi-mech).standard.standard                                  .19  .28  .33  .37  .47  .33  .09
   cotton. scwn.bullock.  standard. standard                                    .10  .20  .30  .40  .40  .40  .30
   cotton.scws.(bullock,semi-mech).standard.standard                            .20  .20  .30  .40  .40  .40  .20
   cotton.srws.(bullock,semi-mech).standard.standard                            .20  .20  .30  .40  .40  .40  .15
   cotton. pcw. semi-mech.la-plant. standard                                              .37  .33  .37  .47  .33  .09
   cotton. scwn.semi-mech.standard. standard                                         .25  .35  .40  .40  .40  .30

   gram.nwfp.(bullock,semi-mech).standard.standard               .10                                     .20  .10  .10  .05
   gram.pmw.(bullock,semi-mech).standard.standard                .05  .05                                .20  .10  .10  .15
   gram.pcw.(bullock,semi-mech).standard.standard                .10  .05                                .25  .15  .10  .10
   gram.(psw,prw).(bullock,semi-mech).standard.standard          .10  .10                                     .20  .15  .10
   gram.(scwn,scws).(bullock,semi-mech).standard.standard        .11  .21                                     .21  .21  .11
   gram.(srwn,srws).(bullock,semi-mech).standard.standard        .10  .20                                     .20  .20  .10

   irri.pmw.(bullock,semi-mech).standard.standard                                         .37  .84  .93  .93  .47
   irri.pcw.(bullock,semi-mech).standard.standard                                         .37  .84  .93  .93  .47
   irri.psw.(bullock,semi-mech).standard.standard                                         .33  .79  .89  .89  .47
   irri.prw.(bullock,semi-mech).standard.standard                                         .56  .84  .93  .93  .65
   irri.scwn.(bullock,semi-mech).standard.standard                                        .36  .90 1.01 1.01  .52
   irri.srwn.(bullock,semi-mech).standard.standard                                        .78  .96  .96  .96  .45
   irri.scws.(bullock,semi-mech).standard.standard                                   .20  .85 1.00 1.00  .75
   irri.srws.(bullock,semi-mech).standard.standard                                   .20 1.15 1.15 1.15  .75

   maize.nwfp.(bullock,semi-mech).standard.standard                                       .15  .35  .30  .25  .20
   maize.(pcw,psw,prw).bullock.   standard.standard                                       .10  .10  .30  .30  .40  .20
   maize.(pcw,psw,prw).semi-mech. standard.standard                                            .15  .30  .30  .40  .25
   maize.scwn.(bullock,semi-mech).standard.standard                                       .21  .32  .32  .43  .43  .21
   maize.scws.(bullock,semi-mech).standard.standard                                  .21  .21  .21  .32  .43  .32  .21

   mus+rap.nwfp.(bullock,semi-mech).standard.standard                                          .20  .20  .25  .25  .25
   mus+rap.pmw.(bullock,semi-mech).standard.standard             .20  .20                                     .25  .20  .20
   mus+rap.pcw.(bullock,semi-mech).standard.standard             .20  .15                                     .30  .20  .20
   mus+rap.psw.(bullock,semi-mech).standard.standard             .20  .20                                .20  .20  .20  .20
   mus+rap.prw.(bullock,semi-mech).standard.standard             .20                                     .20  .20  .20  .20
   mus+rap.scwn.(bullock,semi-mech).standard.standard            .21  .21                                .21  .21  .21  .21
   mus+rap.srwn.(bullock,semi-mech).standard.standard            .20  .20                                .20  .20  .20  .20
   mus+rap.srws.(bullock,semi-mech).standard.standard            .20  .20                                .20  .20  .20  .20
   mus+rap.scws.(bullock,semi-mech).standard.standard            .11  .11                                .27  .21  .21  .27

   sc-gur.nwfp.(bullock,semi-mech).standard.standard             .19  .19  .28  .37  .37  .37  .28  .47  .47  .37  .19  .28
   sc-gur. pmw. (bullock,semi-mech).standard.standard            .14  .09  .23  .33  .33  .28  .23  .37  .37  .19  .09  .19
   sc-gur. pcw. (bullock,semi-mech).standard.standard            .09  .14  .23  .37  .37  .33  .37  .47  .47  .28  .37  .19
   sc-gur. psw. (bullock,semi-mech).standard.standard            .09  .09  .23  .33  .33  .33  .37  .47  .47  .23  .23  .09
   sc-gur. prw. (bullock,semi-mech).standard.standard            .14  .19  .28  .37  .37  .28  .33  .37  .37  .28  .19  .19
   sc-gur. scwn.(bullock,semi-mech).standard.standard            .25  .30  .40  .40  .40  .55  .60  .60  .50  .40  .30  .25
   sc-gur. srwn.(bullock,semi-mech).standard.standard            .23  .28  .37  .37  .37  .51  .56  .56  .47  .37  .28  .23
   sc-gur. scws.(bullock,semi-mech).standard.standard            .25  .35  .40  .40  .40  .55  .65  .65  .60  .50  .35  .25
   sc-gur. srws.(bullock,semi-mech).standard.standard            .25  .35  .40  .40  .50  .55  .65  .65  .60  .50  .40  .20

   sc-mill.nwfp.(bullock,semi-mech).standard.standard            .19  .19  .28  .33  .33  .37  .37  .47  .47  .37  .28  .19
   sc-mill.pmw.(bullock,semi-mech).standard.standard             .14  .09  .23  .33  .33  .28  .23  .37  .37  .19  .09  .19
   sc-mill.pcw. (bullock,semi-mech).standard.standard            .09  .14  .23  .37  .37  .33  .37  .47  .47  .28  .37  .19
   sc-mill.psw. (bullock,semi-mech).standard.standard            .09  .09  .23  .33  .33  .33  .37  .47  .47  .23  .23  .09
   sc-mill.prw. (bullock,semi-mech).standard.standard            .14  .19  .28  .37  .37  .28  .33  .37  .37  .28  .19  .19
   sc-mill.scwn.(bullock,semi-mech).standard.standard            .25  .30  .40  .40  .40  .55  .60  .60  .50  .40  .30  .25
   sc-mill.srwn.(bullock,semi-mech).standard.standard            .23  .28  .37  .37  .37  .51  .56  .56  .47  .37  .28  .23
   sc-mill.scws.(bullock,semi-mech).standard.standard            .25  .35  .40  .40  .40  .55  .65  .65  .60  .50  .35  .25
   sc-mill.srws.(bullock,semi-mech).standard.standard            .25  .35  .40  .40  .50  .55  .65  .65  .60  .50  .40  .20

   kha-fod.srws.(bullock,semi-mech).la-plant.standard                                .21  .27  .32  .32  .16  .11
   kha-fod.nwfp.(bullock,semi-mech).standard.standard                           .05  .15  .20  .20  .25  .25  .05
   kha-fod.pmw. (bullock,semi-mech).standard.standard                      .05  .05  .20  .20  .30  .25  .15
   kha-fod.pcw. (bullock,semi-mech).standard.standard                      .10  .15  .25  .30  .40  .30  .20
   kha-fod.psw. (bullock,semi-mech).standard.standard                      .15  .25  .35  .35  .30  .25  .20
   kha-fod.prw. (bullock,semi-mech).standard.standard                      .10  .20  .30  .30  .30  .30  .30
   kha-fod.scwn.(bullock,semi-mech).standard.standard                      .11  .11  .21  .21  .32  .32  .21  .11
   kha-fod.scws.(bullock,semi-mech).standard.standard                      .11  .21  .21  .27  .32  .32  .16  .05
   kha-fod.srwn.(bullock,semi-mech).standard.standard                      .21  .32  .27  .27  .32       .21  .32
   kha-fod.srws.(bullock,semi-mech).standard.standard                      .21  .32  .27  .27  .32       .21  .32

   wheat.  nwfp.bullock.  la-plant. heavy                        .28                                               .25  .25
   wheat.  pmw. bullock.  la-plant. heavy                        .22                                               .28  .16
   wheat.(pcw,psw,srwn).bullock.la-plant.heavy                   .28                                               .28  .19
   wheat.  prw. bullock.  la-plant. heavy                        .28                                               .09  .28
   wheat.(scwn,srws).bullock.la-plant.heavy                      .30                                               .30  .20
   wheat.  scws.bullock.  la-plant. heavy                        .30                                               .30  .35
   wheat.  nwfp.bullock.  la-plant. january                           .28                                          .25  .25
   wheat.  pmw. bullock.  la-plant. january                           .28                                          .28  .16
   wheat.(pcw,psw,srwn).bullock.la-plant.january                      .28                                          .28  .19
   wheat.  prw. bullock.  la-plant. january                           .28                                          .09  .28
   wheat.(scwn,srws).bullock.la-plant.january                         .30                                          .30  .20
   wheat.  scws.bullock.  la-plant. january                           .30                                          .30  .35
   wheat.  nwfp.bullock.  la-plant. light                        .22  .35                                          .25  .25
   wheat.  pmw. bullock.  la-plant. light                        .22  .28                                          .28  .16
   wheat.(pcw,psw).     bullock.la-plant.light                   .23  .37                                          .28  .19
   wheat.  srwn.bullock.  la-plant. light                        .19  .33                                          .28  .19
   wheat.  prw. bullock.  la-plant. light                        .23  .37                                          .09  .28
   wheat.  scwn.bullock.  la-plant. light                        .20  .40                                          .30  .20
   wheat.  scws.bullock.  la-plant. light                        .25  .40                                          .30  .35
   wheat.  srws.bullock.  la-plant. light                        .20  .35                                          .30  .20
   wheat.  nwfp.bullock.  la-plant. standard                     .22  .35  .41                                     .25  .25
   wheat.  pmw. bullock.  la-plant. standard                     .22  .28  .35                                     .28  .16
   wheat.  pcw. bullock.  la-plant. standard                     .23  .37  .47                                     .28  .19
   wheat.  psw. bullock.  la-plant. standard                     .23  .37  .42                                     .28  .19
   wheat.  prw. bullock.  la-plant. standard                     .23  .37  .47                                     .09  .28
   wheat.  scwn.bullock.  la-plant. standard                     .20  .40  .40                                     .30  .20
   wheat.  srwn.bullock.  la-plant. standard                     .19  .33  .37                                     .28  .19
   wheat.  scws.bullock.  la-plant. standard                     .25  .40  .50                                     .30  .35
   wheat.  srws.bullock.  la-plant. standard                     .20  .35  .40                                     .30  .20

   wheat.nwfp.bullock.(qk-harv,standard).heavy                   .28                                          .19  .16  .16
   wheat.pmw. bullock.(qk-harv,standard).heavy                   .22                                          .19  .16  .16
   wheat.pcw. bullock.(qk-harv,standard).heavy                   .28                                          .19  .19  .19
   wheat.psw. bullock.(qk-harv,standard).heavy                   .28                                          .19  .19  .19
   wheat.prw. bullock.(qk-harv,standard).heavy                   .28                                               .19  .19
   wheat.scwn.bullock.(qk-harv,standard).heavy                   .30                                          .20  .20  .20
   wheat.srwn.bullock.(qk-harv,standard).heavy                   .28                                          .19  .19  .19
   wheat.scws.bullock.(qk-harv,standard).heavy                   .30                                          .20  .30  .25
   wheat.srws.bullock.(qk-harv,standard).heavy                   .30                                          .20  .20  .20
   wheat.nwfp.bullock.(qk-harv,standard).january                      .28                                     .19  .16  .16
   wheat.pmw. bullock.(qk-harv,standard).january                      .28                                     .19  .16  .16
   wheat.pcw. bullock.(qk-harv,standard).january                      .28                                     .19  .19  .19
   wheat.psw. bullock.(qk-harv,standard).january                      .28                                     .19  .19  .19
   wheat.prw. bullock.(qk-harv,standard).january                      .28                                          .19  .19
   wheat.scwn.bullock.(qk-harv,standard).january                      .30                                     .20  .20  .20
   wheat.srwn.bullock.(qk-harv,standard).january                      .28                                     .19  .19  .19
   wheat.scws.bullock.(qk-harv,standard).january                      .30                                     .20  .30  .25
   wheat.srws.bullock.(qk-harv,standard).january                      .30                                     .20  .20  .20
   wheat.nwfp.bullock.(qk-harv,standard).light                   .22  .35                                     .19  .16  .16
   wheat.pmw. bullock.(qk-harv,standard).light                   .22  .28                                     .19  .16  .16
   wheat.pcw. bullock.(qk-harv,standard).light                   .23  .37                                     .19  .19  .19
   wheat.psw. bullock.(qk-harv,standard).light                   .23  .37                                     .19  .19  .19
   wheat.prw. bullock.(qk-harv,standard).light                   .23  .37                                          .19  .19
   wheat.scwn.bullock.(qk-harv,standard).light                   .20  .40                                     .20  .20  .20
   wheat.srwn.bullock.(qk-harv,standard).light                   .19  .33                                     .19  .19  .19
   wheat.scws.bullock.(qk-harv,standard).light                   .25  .40                                     .20  .30  .25
   wheat.srws.bullock.(qk-harv,standard).light                   .20  .35                                     .20  .20  .20
   wheat.nwfp.bullock.(qk-harv,standard).standard                .22  .35  .41                                .19  .16  .16
   wheat.pmw. bullock.(qk-harv,standard).standard                .22  .28  .35                                .19  .16  .16
   wheat.pcw. bullock.(qk-harv,standard).standard                .23  .37  .47                                .19  .19  .19
   wheat.psw. bullock.(qk-harv,standard).standard                .23  .37  .42                                .19  .19  .19
   wheat.prw. bullock.(qk-harv,standard).standard                .23  .37  .47                                     .19  .19
   wheat.scwn.bullock.(qk-harv,standard).standard                .20  .40  .40                                .20  .20  .20
   wheat.srwn.bullock.(qk-harv,standard).standard                .19  .33  .37                                .19  .19  .19
   wheat.scws.bullock.(qk-harv,standard).standard                .25  .40  .50                                .20  .30  .25
   wheat.srws.bullock.(qk-harv,standard).standard                .20  .35  .40                                .20  .20  .20

   wheat.nwfp.semi-mech.(qk-harv,standard).heavy                 .28                                          .19  .16  .16
   wheat.pmw. semi-mech.(qk-harv,standard).heavy                 .22                                          .19  .16  .16
   wheat.pcw. semi-mech.(qk-harv,standard).heavy                 .28                                               .28  .19
   wheat.psw. semi-mech.(qk-harv,standard).heavy                 .28                                          .19  .19  .19
   wheat.prw. semi-mech.(qk-harv,standard).heavy                 .28                                               .19  .19
   wheat.srwn.semi-mech.(qk-harv,standard).heavy                 .28                                               .28  .19
   wheat.srws.semi-mech.(qk-harv,standard).heavy                 .30                                               .30  .30
   wheat.nwfp.semi-mech.(qk-harv,standard).january                    .28                                     .19  .16  .16
   wheat.pmw. semi-mech.(qk-harv,standard).january                    .28                                     .19  .16  .16
   wheat.pcw. semi-mech.(qk-harv,standard).january                    .28                                          .28  .19
   wheat.psw. semi-mech.(qk-harv,standard).january                    .28                                     .19  .19  .19
   wheat.prw. semi-mech.(qk-harv,standard).january                    .28                                          .19  .19
   wheat.srwn.semi-mech.(qk-harv,standard).january                    .28                                          .28  .19
   wheat.srws.semi-mech.(qk-harv,standard).january                    .30                                          .30  .30
   wheat.nwfp.semi-mech.(qk-harv,standard).light                 .22  .35                                     .19  .16  .16
   wheat.pmw. semi-mech.(qk-harv,standard).light                 .22  .28                                     .19  .16  .16
   wheat.pcw. semi-mech.(qk-harv,standard).light                 .23  .37                                          .28  .19
   wheat.psw. semi-mech.(qk-harv,standard).light                 .23  .37                                     .19  .19  .19
   wheat.prw. semi-mech.(qk-harv,standard).light                 .23  .37                                          .19  .19
   wheat.srwn.semi-mech.(qk-harv,standard).light                 .19  .33                                          .28  .19
   wheat.srws.semi-mech.(qk-harv,standard).light                 .20  .35                                          .30  .30
   wheat.nwfp.semi-mech.(qk-harv,standard).standard              .22  .35  .41                                .19  .16  .16
   wheat.pmw. semi-mech.(qk-harv,standard).standard              .22  .28  .35                                .19  .16  .16
   wheat.pcw. semi-mech.(qk-harv,standard).standard              .23  .37  .47                                     .28  .19
   wheat.psw. semi-mech.(qk-harv,standard).standard              .23  .37  .42                                .19  .19  .19
   wheat.prw. semi-mech.(qk-harv,standard).standard              .23  .37  .47                                     .19  .19

   wheat.  scwn.semi-mech.qk-harv.  heavy                        .30                                               .30  .20
   wheat.  scws.semi-mech.qk-harv.  heavy                        .30                                               .35  .30
   wheat.  scwn.semi-mech.qk-harv.  january                           .30                                          .30  .20
   wheat.  scws.semi-mech.qk-harv.  january                           .30                                          .35  .30
   wheat.  scwn.semi-mech.qk-harv.  light                        .20  .40                                          .30  .20
   wheat.  scws.semi-mech.qk-harv.  light                        .25  .40                                          .35  .30
   wheat.  scwn.semi-mech.qk-harv.  standard                     .20  .40  .40                                     .30  .20
   wheat.  srwn.semi-mech.qk-harv.  standard                     .19  .33  .37                                     .28  .19
   wheat.  scws.semi-mech.qk-harv.  standard                     .25  .40  .50                                     .35  .30
   wheat.  srws.semi-mech.qk-harv.  standard                     .20  .35  .40                                     .30  .30
   wheat.  scwn.semi-mech.standard. heavy                        .30                                               .30  .25
   wheat.  scws.semi-mech.standard. heavy                        .30                                               .40  .30
   wheat.  scwn.semi-mech.standard. january                           .30                                          .30  .25
   wheat.  scws.semi-mech.standard. january                           .30                                          .40  .30
   wheat.  scwn.semi-mech.standard. light                        .25  .40                                          .30  .25
   wheat.  scws.semi-mech.standard. light                        .30  .40                                          .30  .30
   wheat.  scwn.semi-mech.standard. standard                     .20  .40  .50                                     .30  .20
   wheat.  srwn.semi-mech.standard. standard                     .19  .33  .47                                     .28  .19
   wheat.  scws.semi-mech.standard. standard                     .25  .45  .55                                     .30  .25
   wheat.  srws.semi-mech.standard. standard                     .20  .40  .55                                     .20  .20

   wheat.  nwfp.semi-mech.la-plant. heavy                        .28                                               .25  .25
   wheat.  pmw. semi-mech.la-plant. heavy                        .22                                               .23  .23
   wheat.  pcw. semi-mech.la-plant. heavy                        .28                                               .23  .23
   wheat.  psw. semi-mech.la-plant. heavy                        .28                                               .28  .19
   wheat.  prw. semi-mech.la-plant. heavy                        .28                                               .09  .28
   wheat.  scwn.semi-mech.la-plant. heavy                        .30                                               .30  .25
   wheat.  srwn.semi-mech.la-plant. heavy                        .28                                               .28  .19
   wheat.  scws.semi-mech.la-plant. heavy                        .30                                               .35  .30
   wheat.  srws.semi-mech.la-plant. heavy                        .30                                               .20  .30
   wheat.  nwfp.semi-mech.la-plant. january                           .28                                          .25  .25
   wheat.  pmw. semi-mech.la-plant. january                           .28                                          .23  .23
   wheat.  pcw. semi-mech.la-plant. january                           .28                                          .23  .23
   wheat.  psw. semi-mech.la-plant. january                           .28                                          .28  .19
   wheat.  prw. semi-mech.la-plant. january                           .28                                          .09  .28
   wheat.  scwn.semi-mech.la-plant. january                           .30                                          .30  .25
   wheat.  srwn.semi-mech.la-plant. january                           .28                                          .28  .19
   wheat.  scws.semi-mech.la-plant. january                           .30                                          .35  .30
   wheat.  srws.semi-mech.la-plant. january                           .30                                          .20  .30
   wheat.  nwfp.semi-mech.la-plant. light                        .22  .35                                          .25  .25
   wheat.  pmw. semi-mech.la-plant. light                        .22  .28                                          .23  .23
   wheat.  pcw. semi-mech.la-plant. light                        .23  .37                                          .23  .23
   wheat.  psw. semi-mech.la-plant. light                        .23  .37                                          .28  .19
   wheat.  prw. semi-mech.la-plant. light                        .23  .37                                          .09  .28
   wheat.  scwn.semi-mech.la-plant. light                        .20  .40                                          .30  .25
   wheat.  srwn.semi-mech.la-plant. light                        .19  .33                                          .28  .19
   wheat.  scws.semi-mech.la-plant. light                        .25  .40                                          .35  .30
   wheat.  srws.semi-mech.la-plant. light                        .20  .35                                          .20  .30
   wheat.  nwfp.semi-mech.la-plant. standard                     .22  .35  .41                                     .25  .25
   wheat.  pmw. semi-mech.la-plant. standard                     .22  .28  .35                                     .23  .23
   wheat.  pcw. semi-mech.la-plant. standard                     .23  .37  .47                                     .23  .23
   wheat.  psw. semi-mech.la-plant. standard                     .23  .37  .42                                     .28  .19
   wheat.  prw. semi-mech.la-plant. standard                     .23  .37  .47                                     .09  .28
   wheat.  scwn.semi-mech.la-plant. standard                     .20  .40  .40                                     .30  .25
   wheat.  srwn.semi-mech.la-plant. standard                     .19  .33  .37                                     .28  .19
   wheat.  scws.semi-mech.la-plant. standard                     .25  .40  .50                                     .35  .30
   wheat.  srws.semi-mech.la-plant. standard                     .20  .35  .40                                     .20  .30

   orchard. nwfp.(bullock,semi-mech).standard.standard           .08  .10  .19  .33  .46  .50  .55  .58  .49  .37  .19  .09
   orchard. pmw. (bullock,semi-mech).standard.standard           .10  .13  .23  .39  .53  .56  .60  .60  .59  .45  .23  .11
   orchard. pcw. (bullock,semi-mech).standard.standard           .11  .14  .30  .44  .54  .59  .57  .61  .58  .47  .23  .12
   orchard. psw. (bullock,semi-mech).standard.standard           .08  .12  .24  .40  .49  .49  .46  .50  .51  .40  .18  .10
   orchard. prw. (bullock,semi-mech).standard.standard           .08  .12  .24  .40  .47  .47  .46  .50  .50  .39  .18  .10
   orchard. scwn.(bullock,semi-mech).standard.standard           .17  .17  .32  .45  .52  .54  .56  .60  .60  .45  .26  .14
   orchard. scws.(bullock,semi-mech).standard.standard           .18  .20  .38  .54  .61  .56  .56  .61  .62  .56  .32  .20
   orchard. srwn.(bullock,semi-mech).standard.standard           .15  .16  .32  .45  .54  .52  .54  .59  .57  .47  .26  .15
   orchard. srws.(bullock,semi-mech).standard.standard           .19  .21  .39  .57  .61  .54  .55  .59  .63  .58  .33  .21

   potatoes.(scwn,scws,srwn,srws).
                 semi-mech.standard.standard                                                             .25  .667  .5  .25
   potatoes.(nwfp,pmw,pcw,psw,prw).semi-mech.
                           standard.standard                     .25  .25   .5                                           .5
   onions.(nwfp,pmw,pcw,psw,prw).semi-mech.
                           standard.standard                     .25  .25   .5   .5  .25
   onions.(scwn,scws,srwn,srws).semi-mech.
                           standard.standard                     .25                                     .25  .25   .5   .5
   chilli.(nwfp,pmw,psw,pcw,prw).semi-mech.
                           standard.standard                              .167  .25 .667 .667   .5   .5 .333 .333 .167
   chilli.(scwn,scws,srwn,srws).semi-mech.
                           standard.standard                      .5 .667 .667 .667                                     .25;

Table tractor(c,z,t,s,w,m) 'tractor requirements (tractor hours per acre)'
                                                jan   feb   mar   apr   may   jun   jul   aug   sep   oct   nov   dec
   basmati.pmw. semi-mech.standard.standard                                  2.70  1.70                    1.50  0.50
   basmati.pcw. semi-mech.standard.standard                                  2.70  1.70                    1.50  0.50
   basmati.psw. semi-mech.standard.standard                                  3.30  1.80                    1.50  0.50
   basmati.prw. semi-mech.standard.standard                                  3.10  2.00                    1.50  0.50
   rab-fod.srwn.semi-mech.standard.heavy                                                       1.00  2.80
   rab-fod.srws.semi-mech.standard.heavy                                                       1.50  2.50
   rab-fod.srwn.semi-mech.standard.light                                                       1.00  2.80
   rab-fod.srws.semi-mech.standard.light                                                       1.50  2.50
   rab-fod.nwfp.semi-mech.standard.standard    1.50  1.50  1.50  1.50  1.50                    2.05  1.62  1.50  1.50
   rab-fod.pmw. semi-mech.standard.standard    1.50  1.50  1.50  1.50  1.50                          1.63  0.78  1.50
   rab-fod.pcw. semi-mech.standard.standard    1.50  1.50  1.50  1.50  1.50                    1.26  2.01  2.29  1.50
   rab-fod.psw. semi-mech.standard.standard    3.00  3.00  3.00  1.50  1.50                          1.91  1.23  1.50
   rab-fod.prw. semi-mech.standard.standard    3.00  3.00  3.00  1.50  1.50                          1.49  1.05  1.57
   rab-fod.scwn.semi-mech.standard.standard                                                    1.20  2.60
   rab-fod.srwn.semi-mech.standard.standard                                                    1.00  2.80
   rab-fod.scws.semi-mech.standard.standard                                                    1.50  2.40
   rab-fod.srws.semi-mech.standard.standard                                                    1.50  2.50
   cotton. pcw. semi-mech.la-plant.standard                                  4.70              0.50  0.50  0.50  0.50
   cotton. pmw. semi-mech.standard.standard                            3.06  0.64              0.50  0.50  0.50  0.50
   cotton. pcw. semi-mech.standard.standard                            2.49  2.22              0.50  0.50  0.50  0.50
   cotton. psw. semi-mech.standard.standard                            1.66  1.98              0.50  0.50  0.50  0.50
   cotton. prw. semi-mech.standard.standard                            1.45  1.84              0.50  0.50  0.50  0.50
   cotton. scwn.semi-mech.standard.standard                           12.10                    1.00  1.00  2.00
   cotton. scws.semi-mech.standard.standard                     12.00                          1.00  2.00
   cotton. srws.semi-mech.standard.standard                           12.40                    1.00  2.00
   gram.   nwfp.semi-mech.standard.standard                      3.75                          1.37  2.00
   gram.   pmw. semi-mech.standard.standard                3.75                                1.27  0.78  0.90  2.50
   gram.   pcw. semi-mech.standard.standard                3.75                                1.30  0.79
   gram.   psw. semi-mech.standard.standard                3.75                                      1.25  0.70
   gram.   prw. semi-mech.standard.standard                3.75                                      1.14  0.68
   gram.(scwn,scws).semi-mech.standard.standard                  3.50                          0.81  1.29
   gram.(srwn,srws).semi-mech.standard.standard                  3.80                          0.96  1.39

   irri.   pmw. semi-mech.standard.standard                                  2.60  2.20                    1.00  0.50
   irri.   pcw. semi-mech.standard.standard                                  2.60  2.20                    1.00  0.50
   irri.   psw. semi-mech.standard.standard                                  2.30  1.90                    1.00  0.50
   irri.   prw. semi-mech.standard.standard                                  4.90  1.50                    1.00  0.50
   irri.   scwn.semi-mech.standard.standard                            1.70  2.30                    1.00  0.50
   irri.   srwn.semi-mech.standard.standard                                  4.30                          1.00
   irri.   scws.semi-mech.standard.standard                      2.10  2.30  3.50                    1.00  0.50
   irri.   srws.semi-mech.standard.standard                            4.20  2.50                    1.00

   maize.  nwfp.semi-mech.standard.standard                                  5.40                    2.50
   maize.  pcw. semi-mech.standard.standard                                        1.77  1.58                    2.50
   maize.  psw. semi-mech.standard.standard                                        1.68  1.40                    2.50
   maize.  prw. semi-mech.standard.standard                                        1.68  1.40                    2.50
   maize.  scwn.semi-mech.standard.standard                                  2.60  4.30
   maize.  scws.semi-mech.standard.standard                            2.70  4.50

   mus+rap.nwfp.semi-mech.standard.standard                                        1.37  0.75  2.00  1.00  1.00  1.00
   mus+rap.pmw. semi-mech.standard.standard    1.00  1.00  1.00                                      2.47  1.00  1.00
   mus+rap.pcw. semi-mech.standard.standard    1.00  1.00  1.00                                      1.30  1.26  1.00
   mus+rap.psw. semi-mech.standard.standard    1.00  1.00  1.00                                1.25  1.11  1.00  1.00
   mus+rap.prw. semi-mech.standard.standard    1.00  1.00                                      1.82  1.00  1.00  1.00
   mus+rap.scwn.semi-mech.standard.standard    0.50  0.50  0.50                                2.80  1.90  0.50  0.50
   mus+rap.srwn.semi-mech.standard.standard    0.50  0.50  0.50                                2.40  1.80  0.50  0.50
   mus+rap.srws.semi-mech.standard.standard    0.50  0.50  0.50                                2.40  1.80  0.50  0.50
   mus+rap.scws.semi-mech.standard.standard    0.50  0.50  0.50                                2.40  1.70  0.50  0.50

   sc-gur. nwfp.semi-mech.standard.standard    2.50  0.37  1.25                                                  0.98
   sc-gur.(pmw,pcw,psw,prw).semi-mech.
                          standard.standard    2.41  0.62  0.72                                                  0.25
   sc-gur. scwn.semi-mech.standard.standard    1.70  1.30  2.10
   sc-gur. srwn.semi-mech.standard.standard    1.80  1.20  2.10
   sc-gur. scws.semi-mech.standard.standard    1.90  1.30  2.40
   sc-gur. srws.semi-mech.standard.standard    2.00  1.50  2.30
   sc-mill.nwfp.semi-mech.standard.standard   13.00 13.00 10.00  7.50                          6.00  5.70 12.00 13.00
   sc-mill.(pmw,pcw,psw). semi-mech.
                          standard.standard    6.40  6.60  5.22  4.50  4.00                                5.75  5.75
   sc-mill.prw. semi-mech.standard.standard    6.50  7.00  6.22  4.50  3.00                                5.70  5.75
   sc-mill.scwn.semi-mech.standard.standard    1.10  1.00  2.00
   sc-mill.srwn.semi-mech.standard.standard    1.10  1.00  2.00
   sc-mill.scws.semi-mech.standard.standard    0.40  1.50  2.00
   sc-mill.srws.semi-mech.standard.standard    0.50  1.70  1.80
   kha-fod.srws.semi-mech.la-plant.standard                            1.20  0.50  0.50  0.90  0.50  0.50
   kha-fod.nwfp.semi-mech.standard.standard                3.00  0.50  1.00  0.50  1.00  1.00  1.00  0.50
   kha-fod.pmw. semi-mech.standard.standard                3.50  0.50  2.00  1.00  2.00  1.50  1.00  0.50
   kha-fod.pcw. semi-mech.standard.standard                2.50  0.50  1.80  1.00  2.20  1.50  1.00  0.50
   kha-fod.psw. semi-mech.standard.standard                2.00  0.50  1.50  1.00  2.00  1.20  0.80  0.50
   kha-fod.prw. semi-mech.standard.standard                3.00  0.80  1.50  1.20  2.20  1.50  1.00  0.50
   kha-fod.scwn.semi-mech.standard.standard                1.10  0.50  0.90  0.50  0.50  0.90  0.50  0.50
   kha-fod.scws.semi-mech.standard.standard                1.20  0.50  0.90  0.50  0.50  0.90  0.50  0.50
   kha-fod.srwn.semi-mech.standard.standard                1.30  0.50  0.90  0.50  0.50  0.80  0.50  0.50
   kha-fod.srws.semi-mech.standard.standard                1.30  0.50  0.90  0.50  0.50  0.80  0.50  0.50
   wheat.  nwfp.semi-mech.la-plant.heavy                         0.40  0.40                                8.60

   wheat.  pmw. semi-mech.la-plant.heavy                         0.40  0.50                                2.30  3.00
   wheat.  pcw. semi-mech.la-plant.heavy                         0.40  0.40                                4.00  3.60
   wheat.  prw. semi-mech.la-plant.heavy                         0.80  0.80                                      3.10
   wheat.  scwn.semi-mech.la-plant.heavy                                                                   3.10  3.30
   wheat.  srwn.semi-mech.la-plant.heavy                                                                   2.90  3.00
   wheat. (scws,srws).semi-mech.la-plant.
          (heavy,january,light,standard)                                                                   3.20  4.20
   wheat.  nwfp.semi-mech.la-plant.january                       0.40  0.40                                8.60
   wheat.  pmw. semi-mech.la-plant.january                       0.40  0.50                                2.30  3.00
   wheat.  pcw. semi-mech.la-plant.january                       0.40  0.40                                4.00  3.60
   wheat.  prw. semi-mech.la-plant.january                       0.80  0.80                                      3.10
   wheat.  scwn.semi-mech.la-plant.january                                                                 3.10  3.30
   wheat.  srwn.semi-mech.la-plant.january                                                                 2.90  3.00
   wheat.  nwfp.semi-mech.la-plant.light                         0.50  0.60                                8.60
   wheat.  pmw. semi-mech.la-plant.light                         0.60  0.60                                2.30  3.00
   wheat.  pcw. semi-mech.la-plant.light                         0.50  0.60                                4.00  3.60
   wheat.  prw. semi-mech.la-plant.light                         1.10  1.10                                      3.10
   wheat.  scwn.semi-mech.la-plant.light                                                                   3.10  3.30
   wheat.  srwn.semi-mech.la-plant.light                                                                   2.90  3.00
   wheat.  nwfp.semi-mech.la-plant.standard                      0.60  0.70                                8.60
   wheat.  pmw. semi-mech.la-plant.standard                      0.70  0.70                                2.30  3.00
   wheat.  pcw. semi-mech.la-plant.standard                      0.60  0.70                                4.00  3.60
   wheat.  prw. semi-mech.la-plant.standard                      1.30  1.30                                      3.10
   wheat.  scwn.semi-mech.la-plant.standard                                                                3.10  3.30
   wheat.  srwn.semi-mech.la-plant.standard                                                                2.90  3.00

   wheat.  nwfp.semi-mech.qk-harv. heavy                         1.00                                4.10  4.50
   wheat.  pmw. semi-mech.qk-harv. heavy                         1.00                                2.30  3.00
   wheat.  pcw. semi-mech.qk-harv. heavy                         1.00                                      7.60
   wheat.  psw. semi-mech.qk-harv. heavy                         0.90                                3.40  3.00
   wheat.  prw. semi-mech.qk-harv. heavy                         1.00                                      3.10
   wheat.scwn.semi-mech.(qk-harv,standard).
         (heavy,january,light,standard)                                                                    6.40
   wheat.srwn.semi-mech.(qk-harv,standard).
         (heavy,january,light,standard)                                                                    5.9
   wheat. (scws,srws).semi-mech.
          (qk-harv,standard).
          (heavy,january,light,standard)                                                                   7.40
   wheat.  nwfp.semi-mech.qk-harv. january                       1.00                                4.10  4.50
   wheat.  pmw. semi-mech.qk-harv. january                       1.00                                2.30  3.00
   wheat.  pcw. semi-mech.qk-harv. january                       1.00                                      7.60
   wheat.  psw. semi-mech.qk-harv. january                       0.90                                3.40  3.00
   wheat.  prw. semi-mech.qk-harv. january                       1.00                                      3.10
   wheat.  nwfp.semi-mech.qk-harv. light                         1.30                                4.10  4.50
   wheat.  pmw. semi-mech.qk-harv. light                         1.40                                2.30  3.00
   wheat.  pcw. semi-mech.qk-harv. light                         1.30                                      7.60
   wheat.  psw. semi-mech.qk-harv. light                         1.20                                3.40  3.00
   wheat.  prw. semi-mech.qk-harv. light                         1.30                                      3.10
   wheat.  nwfp.semi-mech.qk-harv. standard                      1.50                                4.10  4.50
   wheat.  pmw. semi-mech.qk-harv. standard                      1.60                                2.30  3.00
   wheat.  pcw. semi-mech.qk-harv. standard                      1.50                                      7.60
   wheat.  psw. semi-mech.qk-harv. standard                      1.40                                3.40  3.00
   wheat.  prw. semi-mech.qk-harv. standard                      1.50                                      3.10
   wheat.  nwfp.semi-mech.standard.heavy                         0.50  0.50                          4.10  4.50
   wheat.  pmw. semi-mech.standard.heavy                         0.50  0.50                          2.30  3.00
   wheat.  pcw. semi-mech.standard.heavy                         0.50  0.50                                7.60
   wheat.  psw. semi-mech.standard.heavy                         0.40  0.50                          3.40  3.00
   wheat.  prw. semi-mech.standard.heavy                         0.50  0.50                                3.10
   wheat.  nwfp.semi-mech.standard.january                       0.50  0.50                          4.10  4.50
   wheat.  pmw. semi-mech.standard.january                       0.50  0.50                          2.30  3.00
   wheat.  pcw. semi-mech.standard.january                       0.50  0.50                                7.60
   wheat.  psw. semi-mech.standard.january                       0.40  0.50                          3.40  3.00
   wheat.  prw. semi-mech.standard.january                       0.50  0.50                                3.10
   wheat.  nwfp.semi-mech.standard.light                         0.60  0.70                          4.10  4.50
   wheat.  pmw. semi-mech.standard.light                         0.70  0.70                          2.30  3.00
   wheat.  pcw. semi-mech.standard.light                         0.60  0.70                                7.60
   wheat.  psw. semi-mech.standard.light                         0.60  0.60                          3.40  3.00
   wheat.  prw. semi-mech.standard.light                         0.60  0.70                                3.10
   wheat.  nwfp.semi-mech.standard.standard                      0.70  0.80                          4.10  4.50
   wheat.  pmw. semi-mech.standard.standard                      0.80  0.80                          2.30  3.00
   wheat.  pcw. semi-mech.standard.standard                      0.70  0.80                                7.60
   wheat.  psw. semi-mech.standard.standard                      0.70  0.70                          3.40  3.00
   wheat.  prw. semi-mech.standard.standard                      0.70  0.80                                3.10
   orchard.(nwfp,pmw,pcw,psw,prw).semi-mech.
                          standard.standard      .1    .2    .1    .1    .1    .1    .1    .1    .1    .2    .3
   orchard.(scwn,scws,srwn,srws).semi-mech.
                          standard.standard      .1    .3    .1    .1    .1    .2    .1    .1    .1    .1    .3
   potatoes.(scwn,scws,srwn,srws).
                 semi-mech.standard.standard                                                     2.    2.
   potatoes.(nwfp,pmw,pcw,psw,prw).semi-mech.
                           standard.standard     2.                                                                2.
   onions.(nwfp,pmw,pcw,psw,prw).semi-mech.
                           standard.standard     4.
   onions.(scwn,scws,srwn,srws). semi-mech.
                           standard.standard                                                    4.
   chilli.(nwfp,pmw,pcw,psw,prw).semi-mech.
                           standard.standard                 1.
   chilli.(scwn,scws,srwn,srws). semi-mech.
                           standard.standard                                                                       1.;

Table sylds(c,z,t,s,w,ci) 'straw yield and seed data'
                                                        straw-yld   seed
*                                                      (proportion  (kg)
*                                                       of yield)
   basmati.(pmw,pcw,prw).(bullock,semi-mech).
                      standard.standard                  2.33        6.4
   basmati.psw. (bullock,semi-mech).
                      standard. standard                 2.12        6.4

   rab-fod.(srwn,srws).(bullock,semi-mech).
                      standard.(light,heavy)             1           6.0
   rab-fod.(nwfp,pmw,pcw,psw,prw).(bullock,semi-mech).
                      standard.standard                  1           2.0
   rab-fod.(scwn,srwn,scws,srws).(bullock,semi-mech).
                      standard. standard                 1           6.0

   cotton. pcw. bullock.            el-plant. standard               9.
   cotton. pcw. semi-mech.          la-plant. standard               9.
   cotton. (pmw,pcw,psw,prw,scwn,scws,srwn,srws).
           (bullock,semi-mech).standard. standard                    9.

   gram.   nwfp.(bullock,semi-mech).standard. standard   1.70       10.
   gram.  (pmw,pcw,psw,prw).
                (bullock,semi-mech).standard. standard   1.52       14.
   gram.  (scwn,scws).(bullock,semi-mech).
                  standard. standard                     1.50       12.8
   gram.  (srwn,srws).(bullock,semi-mech).
                  standard. standard                     2.00       12.0

   irri.   (pmw,pcw,psw,prw).(bullock,semi-mech).
                       standard.standard                 1.80        7.1
   irri.   scwn.(bullock,semi-mech).standard. standard   1.34        4.5
   irri.  (srwn,scws,srws).(bullock,semi-mech).
                  standard. standard                     1.32        4.5

   maize.  nwfp.(bullock,semi-mech).standard. standard   2.09       14.2
   maize.  pcw. (bullock,semi-mech).standard. standard   3.00        6.1
   maize. (psw,prw). (bullock,semi-mech).
                  standard. standard                     2.50        6.1
   maize. (scwn,scws).
                (bullock,semi-mech).standard. standard   2.80        8.1

   mus+rap.nwfp.(bullock,semi-mech).standard. standard   0.50        1.8
   mus+rap.(pmw,pcw,psw,prw,scwn,srwn,srws,scws).
           (bullock,semi-mech).standard. standard        0.65        2.4

   sc-gur. nwfp.(bullock,semi-mech).standard. standard   1.54       3238
   sc-gur. pmw. (bullock,semi-mech).standard. standard   1.81       3238
   sc-gur. pcw. (bullock,semi-mech).standard. standard   1.66       3238
   sc-gur. psw. (bullock,semi-mech).standard. standard   1.54       3238
   sc-gur. prw. (bullock,semi-mech).standard. standard   2.17       3238
   sc-gur. scwn.(bullock,semi-mech).standard. standard   1.81       3238
   sc-gur. srwn.(bullock,semi-mech).standard. standard   3.08       3238
   sc-gur. scws.(bullock,semi-mech).standard. standard   2.17       3238
   sc-gur. srws.(bullock,semi-mech).standard. standard   2.05       3238

   sc-mill.nwfp.(bullock,semi-mech).standard. standard   0.12       3238
   sc-mill.pmw. (bullock,semi-mech).standard. standard   0.14       3238
   sc-mill.pcw. (bullock,semi-mech).standard. standard   0.13       3238
   sc-mill.psw. (bullock,semi-mech).standard. standard   0.10       3238
   sc-mill.prw. (bullock,semi-mech).standard. standard   0.17       3238
   sc-mill.scwn.(bullock,semi-mech).standard. standard   0.14       3238
   sc-mill.srwn.(bullock,semi-mech).standard. standard   0.25       3238
   sc-mill.scws.(bullock,semi-mech).standard. standard   0.17       3238
   sc-mill.srws.(bullock,semi-mech).standard. standard   0.16       3238

   kha-fod.srws.(bullock,semi-mech).la-plant. standard   1           5.7
   kha-fod.nwfp.(bullock,semi-mech).standard. standard   1          20.2
   kha-fod.(pmw,pcw,psw,prw).
           (bullock,semi-mech).standard. standard        1          27.3
   kha-fod.(scwn,scws,srwn,srws).
           (bullock,semi-mech).standard. standard        1           5.7

   wheat.nwfp.(bullock,semi-mech).
              (standard,la-plant,qk-harv).
              (standard,light,heavy,january)             1.3        40.1
   wheat.pmw. (bullock,semi-mech).
              (standard,la-plant,qk-harv).
              (standard,light,heavy,january)             1.3        34.8
   wheat.(pcw,psw). (bullock,semi-mech).
              (standard,la-plant,qk-harv).
              (standard,light,heavy,january)             1.5        34.8
   wheat.prw. (bullock,semi-mech).
              (standard,la-plant,qk-harv).
              (standard,light,heavy,january)             1.6        34.8
   wheat.(scwn,srwn,scws,srws).(bullock,semi-mech).
              (standard,la-plant,qk-harv).
              (standard,light,heavy,january)             1.5        49.8


   orchard.(nwfp,pmw,pcw,psw,prw,scwn,scws,srwn,srws).
              (bullock,semi-mech).standard.standard                    1

   potatoes.(scwn,scws,srwn,srws).
           semi-mech.standard.standard                              1200
   potatoes.(nwfp,pmw,pcw,psw,prw).
           semi-mech.standard.standard                               600

   onions.(nwfp,pmw,pcw,psw,prw,scwn,scws,srwn,srws).
          semi-mech.standard.standard                                 3.

   chilli.(nwfp,pmw,pcw,psw,prw,scwn,scws,srwn,srws).
           semi-mech.standard.standard                                5.;

Table fert(p2,c,z) 'fertilizer applications (kg per acre)'
                       nwfp   pcw   pmw   prw   psw  scwn  scws  srwn  srws
   nitrogen.basmati          26.6  26.6  21.9  23.4
   nitrogen.irri             39.4  39.4  23.3  26.8  68.6  61.5  48.9  41.7
   nitrogen.cotton     26.7  42.3  30.0  30.0  19.7  55.0  54.9  39.6  39.6
   nitrogen.maize      27.0  27.1  27.1  23.6  19.5  42.0  42.0  42.0  42.0
   nitrogen.kha-fod    21.0  25.3  18.1  19.2  18.4  49.0  49.0  49.0  49.0
   nitrogen.wheat      46.8  40.9  36.5  32.2  33.3  54.9  53.5  29.5  39.8
   nitrogen.rab-fod    10.0  25.3  18.1  19.2  18.4  28.0  28.0  28.0  28.0
   nitrogen.sc-mill    83.4  44.8  63.2  33.9  33.9  65.1  65.1  65.1  65.1
   nitrogen.sc-gur     24.0  19.0  19.0  19.0  19.0  28.0  28.0  28.0  28.0
   nitrogen.onions     60.6  48.0  48.0  48.0  48.0  70.7  70.7  70.7  70.7
   nitrogen.potatoes   48.6  38.5  38.5  38.5  38.5  56.7  56.7  56.7  56.7
   nitrogen.mus+rap    33.6  30.8  30.8  30.8  30.8  45.4  45.4  45.4  45.4
   nitrogen.chilli     48.6  38.5  38.5  38.5  38.5  56.7  56.7  56.7  56.7
   nitrogen.orchard    60.0  47.5  47.5  47.5  47.5  70.0  70.0  70.0  70.0

   phosphate.basmati         13.3  13.3  11.4  10.6   0.0   0.0   0.0   0.0
   phosphate.irri             8.0   8.0   6.8   6.4  15.7  16.6  13.5  12.5
   phosphate.cotton    26.2  11.3   5.2   7.0   7.0  12.8  14.4  13.0  13.0
   phosphate.maize     17.6   8.8   6.9   8.6   8.6   7.7   7.7   7.7   7.7
   phosphate.kha-fod   25.9  11.4  11.4  11.4  11.4  15.3  15.3  15.3  15.3
   phosphate.wheat     12.3  12.8   8.8  11.4  10.9  15.1  15.1  11.8  17.3
   phosphate.rab-fod    5.4   7.6   5.4   5.8   5.6   7.7   7.7   7.7   7.7
   phosphate.sc-mill   46.8  14.0  19.8  10.6  10.6  18.5  18.5  18.5  18.5
   phosphate.sc-gur    10.4   5.7   5.7   5.7   5.7   7.7   7.7   7.7   7.7
   phosphate.onions    26.2   7.0   7.0   7.0   7.0   9.3   9.3   9.3   9.3
   phosphate.potatoes  42.0  18.5  18.5  18.5  18.5  24.9  24.9  24.9  24.9
   phosphate.mus+rap   25.9  13.3  13.3  13.3  13.3  17.9  17.9  17.9  17.9
   phosphate.chilli    26.2  11.6  11.6  11.6  11.6  15.5  15.5  15.5  15.5
   phosphate.orchard   13.0   5.7   5.7   5.7   5.7   7.7   7.7   7.7   7.7;

Parameter fertgr(c) 'fertilizer application growth rate  percent'
   /(basmati , irri   , cotton , rab-fod
     gram    , maize  , mus+rap, kha-fod
     sc-gur  , sc-mill, wheat  , orchard) 3
    (potatoes, onions , chilli)           2.4 /;

* zone4xxxxx(z,c,p2) = fert(p2,c,z);

fert(p2,c,z) = fert(p2,c,z)*sum(is$isr(is), (1 + fertgr(c)/100)**(ord(is) + 1979 - baseyear));

$sTitle Crop Yields
Parameter natyield(c) 'national crop yields 1988 for standard technologies (kgs)'
   / basmati    457
     irri       880
     cotton     695
     rab-fod  15000
     gram       183
     maize      534
     mus+rap    307
     kha-fod  10000
     sc-gur    1270
     sc-mill  15870
     wheat      780
     orchard   3400
     potatoes  3980
     onions    4615
     chilli     567 /

Table yldprpv(c,pv) 'province yields proportion of national 1987-88'
*                    note: used 3-year average from latest asp
               nwfp  punjab   sind
   wheat      0.926   0.978  1.103
   basmati        0       1      0
   irri           0   0.859  1.001
   cotton         0   1.153  0.861
   sc-mill    1.090   0.919  1.190
   sc-gur     1.090   0.919  1.190
   maize      1.006   1.029  0.408
   mus+rap    0.622   1.192  0.842
   gram       0.911   0.937  1.465
   rab-fod     .400   1.000  1.000
   kha-fod     .770   1.000  1.300
   orchard     .936   1.070      1
   potatoes    .973    .960   .873
   onions     1.172   1.085  0.843
   chilli     0.786   1.167  0.952;

Table yldprzs(c,z) 'zones yields as proportion of province-standard technologies'
              nwfp   pcw   pmw   prw   psw  scwn  scws  srwn  srws
   wheat         1  0.96  0.84  1.06  1.13  0.96  1.07  0.82  1.30
   basmati       1   .83  0.00  1.26  0.79  0.00  0.00  0.00  0.00
   irri          1  0.83  0.00  1.26  0.00  1.06  0.91  1.10  0.90
   cotton        1  1.10  0.71  0.70  0.80  1.09  0.96  0.78  0.78
   sc-mill       1  1.04  0.93  0.93  0.98  0.95  1.10  0.64  0.87
   sc-gur        1  1.04  0.93  0.93  0.98  0.95  1.10  0.64  0.87
   maize         1  1.03  0.76  1.08  0.97  1.07  1.06  0.87  1.00
   mus+rap       1  1.03  0.76  1.08  0.97  1.07  1.06  0.87  1.00
   gram          1  1.03  0.76  1.08  0.97  1.07  1.06  0.87  1.00
   rab-fod       1  1.23  0.81  1.17  1.23  1.    1.    1.    1.
   kha-fod       1   .83  0.54  0.82  0.82  1.    1.    1.    1.00
   orchard       1  1.    1.    1.    1.0   1.    1.    1.    1.
   potatoes      1  1.03  0.76  1.08  0.97  1.07  1.06  0.87  1.00
   onions        1  1.03  0.76  1.08  0.97  1.07  1.06  0.87  1.00
   chilli        1  1.03  0.76  1.08  0.97  1.07  1.06  0.87  1.00;

Parameter yldprzo(c,s,w) 'yields as proportion of standard technologies'
   / rab-fod. standard         . heavy          0.70
     rab-fod. standard         . light          0.80
     kha-fod. la-plant         . standard       1.05
     wheat  . qk-harv          . standard       1.00
     wheat  . la-plant         .(heavy,january) 0.57
     wheat  . la-plant         . light          0.75
     wheat  . la-plant         . standard       0.88
     wheat  .(qk-harv,standard).(heavy,january) 0.65
     wheat  .(qk-harv,standard). light          0.85 /;

Table growthcy(c,z) 'growth rate of crop yields from 1988 base (percent)'
            (nwfp,pmw,pcw,psw,prw,scwn,scws,srwn,srws)
   cotton    5.00
   maize     0.73
   sc-gur    2.29
   sc-mill   2.29
   wheat     0.41
   onions    1.66

Table weedy(z,sea,c) 'weed yields by crop (tonns per acer)'
                                   basmati  irri  cotton  gram  mus+rap  maize  sc-gur  sc-mill  wheat orchard
   (nwfp,pmw,pcw,psw,prw).rabi                              .3       .3             .3       .3     .4      .3
   (scwn,scws,srwn,srws) .rabi                              .3       .3             .3       .3     .4      .3
   (nwfp,pmw,pcw,psw,prw).kharif         1     1       1                     1       1        1              1
   (scwn,scws,srwn,srws) .kharif               1       1                     1       1        1              1;

Table graz(z,sea) 'grazing from slack land (tonns per acre)'
                          rabi  kharif
   prw                     .15      .5
   pmw                     .1       .2
  (psw,pcw)                .2       .3
  (scwn,scws,srwn,srws)    .2       .5;

* growthcy(c,z)$(growthcy(c,z) > 3) = 3.0;
Parameter
   yield(c,t,s,w,z) 'yield by zone crop technology in metric tonns'
   growthcyf(c,z)   'growth factor for crop yields using growthcy';

yield(c,t,"standard","standard",z) = sum(pv$pvz(pv,z), natyield(c)/1000*yldprpv(c,pv)*yldprzs(c,z));
yield(c,t,s,w,z)$yldprzo(c,s,w)    = yield(c,t,"standard","standard",z)*yldprzo(c,s,w);

growthcyf(c,z)   = sum(is$isr(is),(1 + growthcy(c,z)/100)**(ord(is) + 1979 - baseyear));
yield(c,t,s,w,z) = yield(c,t,s,w,z)*growthcyf(c,z);
display baseyear, growthcyf, fert;

$onText
* Report on input data
Set ic 'crop inputs' / land, labor, bullock, water, tractor /;

Parameter rep1, rep2;
rep1(z,c,t,s,w,"land",m)           = land(c,z,t,s,w,m);
rep1(z,c,t,s,w,"bullock",m)        = bullock(c,z,t,s,w,m);
rep1(z,c,t,s,w,"labor",m)          = labor(c,z,t,s,w,m);
rep1(z,c,t,s,w,"water",m)          = water(c,z,t,s,w,m);
rep1(z,c,t,s,w,"tractor",m)        = tractor(c,z,t,s,w,m);
rep1(z,c,t,s,w,ic,"total")         = sum(m, rep1(z,c,t,s,w,ic,m));
rep2(z,c,t,s,w,ic)                 = rep1(z,c,t,s,w,ic,"total");
rep2(z,c,t,s,w,ci)                 = sylds(c,z,t,s,w,ci);
rep2(z,c,t,s,w,p2)$tech(z,c,t,s,w) = fert(p2,c,z);
rep2(z,c,t,s,w,"yield")            = yield(c,t,s,w,z);

option  rep1:2:4:1, rep2:2:4:1;
display rep1, rep2;
$offText

$sTitle Livestock Data
Table iolive(a,z,*) 'livestock input output coefficients by zones'
* meat and milk yeilds are of 1985.
                                    tdn         dp      labor  cow-milk  buff-milk  meat  fix-cost
*                              (metric tons/season)   man hrs        liters          kgs
*                                                   per month (- - -  per  year   - - - )
   cow. nwfp                        .68       .061       25.1       359             13.4        15
   cow. pmw                         .68       .061       25.1       284             13.4        15
   cow. pcw                         .68       .061       25.1       303             13.4        15
   cow. psw                         .68       .061       25.1       268             13.4        15
   cow. prw                         .68       .061       25.1       332             13.4        15
   cow.scwn                         .68       .046       23.5       460             10.5        17
   cow.srwn                         .68       .083       23.5       383             15.4        17
   cow.scws                         .68       .046       23.5       350             10.5        17
   cow.srws                         .68       .083       23.5       468             15.4        17

   bullock.(nwfp,pmw,pcw,psw,prw)   .635      .058       15.1                       14.3        50
   bullock.(scwn,srwn,scws,srws)    .635      .058       14.8                       14.1        50

   buffalo.nwfp                    1.04       .095       33.6                  602  17.3        50
   buffalo.pmw                     1.04       .095       33.6                  653  17.3        50
   buffalo.pcw                     1.04       .095       33.6                  652  17.3        50
   buffalo.psw                     1.04       .095       33.6                  643  17.3        50
   buffalo.prw                     1.04       .095       33.6                  590  17.3        50
   buffalo.scwn                    1.04       .073       31.4                  946  16.2        50
   buffalo.srwn                    1.04       .128       31.4                  750  22.5        50
   buffalo.scws                    1.04       .073       31.4                  773  16.2        50
   buffalo.srws                    1.04       .128       31.4                 1018  22.5        50;

Table sconv(nt,sea,c) 'tdn and dp conversion factor from crop straw'
                basmati  cotton  rab-fod  gram  irri  kha-fod  maize  mus+rap  sc-mill  sc-gur  wheat
   tdn.kharif                               .6            .14              .5                      .6
   dp. kharif                               .1           .019            .005                    .005
   tdn.rabi         .5               .14          .5              .5               .17     .17
   dp. rabi       .005              .025        .005              .1              .005    .005       ;

Scalar
   repco   'reproductive coefficient'                            / 2.5 /
   gr      'required proportion of green fodder in total fodder' / 0.3 /
   growthq 'growth rate of milk and meat yields (percent)'       / 2.5 /;

Parameter bp(m) 'draft power available per bullock(hours per month)';
bp(m)     = 96;
bp("may") = 77;
bp("jun") = 77;

* zone2xxxxx(z,a,set1) = iolive(a,z,set1);
iolive(a,z,q) = iolive(a,z,q)*sum(is$isr(is), (1 + growthq/100)**(ord(is) + 1979 - baseyear));
options iolive:3;
display iolive;

$sTitle Canal and Agroclimatic Zone Data
Set
   cnl  'irrigation canals in the indus river irrigation system'
        / 01-ud   'upper dipalpur'
          02-cbd  'central bari doab canal'
          03-ray  'raya canal'
          04-uc   'upper chenab canal'
          05-mr   'marala ravi canal'
          06-sad  'sadiqia canal'
          07-for  'fordwah canal'
          08-pak  'upper pakpattan+u-bahawal+qaim+u-mailsi canal'
          09-ld   'lower dipalpur canal'
          10-lbd  'lower bari doab canal'
          11-jha  'jhang canal (lcc)'
          12-gug  'gugera branch canal (lcc)'
          13-uj   'upper jehlum canal'
          14-lj   'lower jehlum canal'
          15-bah  'bahawal canal'
          16-mai  'lower mailsi+ lower pakpattan canal'
          17-sid  'sidhnai canal'
          18-hav  'haveli canal'
          19-ran  'rangpur canal'
          20-pan  'panjnad canal'
          21-abb  'abbasia canal'
          22-usw  'upper swat canal'
          23-lsw  'lower swat canal'
          24-war  'warsak canal'
          25-kab  'kabul river canal'
          26-tha  'thal canal'
          27-pah  'paharpur canal'
          28-muz  'muzffgarh canal'
          29-dgk  'dera ghazi khan canal'
          31-p+d  'pat plus desert canal'
          32-beg  'begari canal'
          33-gho  'ghotki canal'
          34-nw   'north west canal'
          35-ric  'rice canal'
          36-dad  'dadu canal'
          37-kw   'khairpur west canal'
          38-ke   'khairpur east canal'
          39-roh  'rohri canal'
          41-nar  'nara canal'
          42-kal  'kalri canal'
          43-lch  'lined channel'
          44-ful  'fuleli canal'
          45-pin  'pinyari canal' /
   pvcnl(pv,cnl) 'province to canals map'
                 / nwfp.  (22-usw, 23-lsw, 24-war, 25-kab)
                   punjab.(01-ud,  02-cbd, 03-ray, 04-uc,  05-mr,  06-sad, 07-for
                           08-pak, 09-ld,  10-lbd, 11-jha, 12-gug, 13-uj,  14-lj
                           15-bah, 16-mai, 17-sid, 18-hav, 19-ran, 20-pan, 21-abb
                           26-tha, 27-pah, 28-muz, 29-dgk)
*                  sind canals including baluchistan
                   sind.  (31-p+d, 32-beg, 33-gho, 34-nw,  35-ric, 36-dad
                           37-kw,  38-ke,  39-roh, 41-nar, 42-kal, 43-lch
                           44-ful, 45-pin ) /
   gwfg(cnl,sa,g) 'subarea identification by the groundwater quality';

$sTitle Canal Command Data
Table comdef(is,dc,cnl) 'canal command characteristics'
* cca   -- culturable commanded area of the canal (millions of acres)
* ccap  -- canal capacity at the canal head(millions of acre feet)
* ceff  -- canal efficiency from barrage to the water course head
* wce-r -- water course command efficinecy in rabi season
* wce-k -- water course command efficinecy in kharif season
* flde  -- field efficiency
*
* note: ccap for 05-mr, 24-war, 25-kab are set equal to post tarbela average diversion. original capacities were
*                 .166    .044    .047
                01-ud  02-cbd  03-ray   04-uc   05-mr  06-sad  07-for  08-pak   09-ld  10-lbd  11-jha
   1980.cca      .36     .649    .424   1.017    .158    .969    .426   1.049    .615   1.67    1.168
   1980.ccap     .131    .163    .11     .506    .282    .332    .204    .746    .244    .518    .41
   1980.ceff     .70     .80     .80     .76     .80     .72     .70     .61     .80     .72     .70
   1980.wce-r    .57     .57     .45     .57     .57     .51     .51     .57     .57     .57     .55
   1980.wce-k    .57     .57     .45     .57     .57     .51     .51     .57     .57     .57     .55
   1980.flde     .90     .90     .90     .90     .90     .80     .80     .90     .90     .90     .90

   1988.cca      .36     .649    .424   1.017    .158    .969    .426   1.049    .615   1.67    1.168
   1988.ccap     .131    .163    .11     .506    .282    .332    .204    .746    .244    .518    .41
   1988.ceff     .703    .811    .800    .760    .800    .728    .704    .621    .810    .722    .706
   1988.wce-r    .57     .573    .45     .57     .57     .522    .51     .575    .57     .57     .554
   1988.wce-k    .57     .573    .45     .57     .57     .522    .51     .575    .57     .57     .554
   1988.flde     .90     .90     .90     .90     .90     .80     .80     .90     .90     .90     .90

   1993.cca      .36     .649    .424   1.017    .158    .969    .426   1.049    .615   1.67    1.168
   1993.ccap     .131    .163    .11     .506    .282    .332    .204    .746    .244    .518    .41
   1993.ceff     .705    .815    .808    .765    .800    .733    .708    .625    .810    .724    .713
   1993.wce-r    .58     .585    .46     .58     .58     .543    .53     .59     .58     .58     .564
   1993.wce-k    .58     .585    .46     .58     .58     .543    .53     .59     .58     .58     .564
   1993.flde     .90     .90     .90     .90     .90     .80     .80     .90     .90     .90     .90

   2000.cca      .36     .649    .424   1.017    .158    .969    .426   1.049    .615   1.67    1.168
   2000.ccap     .131    .163    .11     .506    .282    .332    .204    .746    .244    .518    .41
   2000.ceff     .705    .815    .808    .765    .800    .733    .708    .625    .810    .724    .713
   2000.wce-r    .59     .595    .47     .59     .59     .553    .54     .60     .59     .59     .574
   2000.wce-k    .59     .595    .47     .59     .59     .553    .54     .60     .59     .59     .574
   2000.flde     .90     .90     .90     .90     .90     .80     .80     .90     .90     .90     .90

   +           12-gug   13-uj   14-lj  15-bah  16-mai  17-sid  18-hav  19-ran  20-pan  21-abb  22-usw
   1980.cca     1.866    .544   1.50     .605    .996    .869    .179    .344   1.348    .154    .279
   1980.ccap     .433    .345    .464    .346    .362    .268    .068    .176    .671    .08     .117
   1980.ceff     .74     .80     .64     .77     .77     .72     .77     .70     .72     .64     .75
   1980.wce-r    .55     .51     .51     .57     .57     .57     .57     .46     .57     .57     .52
   1980.wce-k    .55     .51     .51     .57     .57     .57     .57     .46     .57     .57     .52
   1980.flde     .90     .90     .90     .90     .90     .90     .85     .80     .90     .90     .90

   1988.cca     1.866    .544   1.50     .605    .996    .869    .179    .344   1.348    .154    .279
   1988.ccap     .433    .345    .464    .346    .362    .268    .068    .176    .773    .08     .117
   1988.ceff     .749    .800    .642    .782    .780    .721    .773    .700    .728    .640    .750
   1988.wce-r    .553    .51     .51     .57     .57     .57     .57     .46     .59     .59     .53
   1988.wce-k    .553    .51     .51     .57     .57     .57     .57     .46     .59     .59     .53
   1988.flde     .90     .90     .90     .90     .90     .90     .85     .80     .90     .90     .90

   1993.cca     1.866    .544   1.500    .605    .996    .869    .179    .344   1.348    .154    .279
   1993.ccap     .433    .345    .464    .346    .362    .268    .068    .176    .773    .08     .132
   1993.ceff     .751    .808    .644    .783    .783    .727    .776    .706    .73     .641    .757
   1993.wce-r    .565    .52     .52     .58     .58     .58     .58     .48     .59     .59     .53
   1993.wce-k    .565    .52     .52     .58     .58     .58     .58     .48     .59     .59     .53
   1993.flde     .90     .90     .90     .90     .90     .90     .85     .80     .90     .90     .90

   2000.cca     1.866    .544   1.573    .605    .996    .869    .179    .344   1.348    .154    .326
   2000.ccap     .433    .345    .505    .346    .362    .268    .068    .176    .773    .08     .162
   2000.ceff     .751    .808    .644    .783    .783    .727    .776    .706    .73     .641    .757
   2000.wce-r    .575    .53     .53     .59     .59     .59     .59     .49     .60     .60     .53
   2000.wce-k    .575    .53     .53     .59     .59     .59     .59     .49     .60     .60     .53
   2000.flde     .90     .90     .90     .90     .90     .90     .85     .80     .90     .90     .90

   +           23-lsw  24-war  25-kab  26-tha  27-pah  28-muz  29-dgk  31-p+d  32-beg  33-gho   34-nw
   1980.cca      .182    .119    .048   1.641    .104    .809    .909   1.075   1.002    .858   1.215
   1980.ccap     .06     .049    .047    .577    .037    .495    .529    .799   1.155    .648    .566
   1980.ceff     .75     .80     .72     .65     .73     .70     .70     .83     .82     .76     .80
   1980.wce-r    .52     .52     .52     .48     .51     .46     .54     .55     .55     .45     .55
   1980.wce-k    .52     .52     .52     .48     .51     .46     .54     .60     .65     .45     .60
   1980.flde     .90     .90     .90     .80     .85     .80     .85     .85     .85     .85     .85

   1988.cca      .182    .119    .048   1.641    .570    .809    .909   1.075   1.002    .858   1.215
   1988.ccap     .120    .049    .047    .577    .330    .495    .529    .799   1.155    .648    .566
   1988.ceff     .750    .800    .720    .663    .730    .720    .703    .830    .820    .763    .800
   1988.wce-r    .57     .57     .53     .48     .51     .46     .54     .55     .55     .45     .55
   1988.wce-k    .57     .57     .53     .48     .51     .46     .54     .60     .65     .45     .60
   1988.flde     .90     .90     .90     .80     .85     .80     .85     .85     .85     .85     .85

   1993.cca      .182    .119    .048   1.641    .570    .809    .909   1.075   1.002    .858   1.215
   1993.ccap     .120    .049    .047    .577    .330    .495    .529    .799   1.155    .648    .566
   1993.ceff     .751    .80     .723    .671    .73     .725    .707    .831    .82     .765    .80
   1993.wce-r    .57     .60     .53     .49     .56     .47     .55     .56     .56     .47     .56
   1993.wce-k    .57     .60     .53     .49     .56     .47     .55     .61     .66     .47     .61
   1993.flde     .90     .90     .90     .80     .85     .80     .85     .85     .85     .85     .85

   2000.cca      .182    .119    .048   1.641    .570    .809   1.232   1.215   1.002    .858   1.215
   2000.ccap     .120    .049    .047    .577    .330    .495    .731    .900   1.155    .648    .566
   2000.ceff     .751    .80     .723    .671    .73     .725    .707    .831    .82     .765    .80
   2000.wce-r    .58     .61     .54     .50     .57     .48     .56     .594    .57     .48     .57
   2000.wce-k    .58     .61     .54     .50     .57     .48     .56     .65     .67     .48     .62
   2000.flde     .90     .90     .90     .80     .85     .80     .85     .85     .85     .85     .85

   +           35-ric  36-dad   37-kw   38-ke  39-roh  41-nar  42-kal  43-lch  44-ful  45-pin
   1980.cca      .519    .584    .417    .373   2.561   2.176    .592    .502    .923    .758
   1980.ccap     .829    .319    .157    .207    .981    .873    .546    .205    .894    .831
   1980.ceff     .85     .80     .74     .73     .80     .80     .80     .80     .80     .82
   1980.wce-r    .55     .55     .45     .45     .45     .45     .55     .55     .55     .55
   1980.wce-k    .75     .60     .45     .45     .45     .45     .60     .61     .65     .62
   1980.flde     .85     .85     .85     .85     .85     .85     .85     .85     .85     .85

   1988.cca      .519    .584    .417    .373   2.561   2.176    .592    .502    .923    .758
   1988.ccap     .829    .319    .157    .214    .981    .873    .546    .205    .894    .831
   1988.ceff     .850    .800    .748    .736    .807    .811    .800    .800    .800    .820
   1988.wce-r    .55     .55     .45     .455    .456    .45     .55     .55     .55     .55
   1988.wce-k    .75     .60     .45     .455    .456    .45     .60     .61     .65     .62
   1988.flde     .85     .85     .85     .85     .85     .85     .85     .85     .85     .85

   1993.cca      .519    .584    .417    .373   2.561   2.176    .592    .502    .923    .758
   1993.ccap     .829    .319    .157    .214    .981   1.041    .546    .205    .894    .831
   1993.ceff     .854    .804    .75     .738    .81     .816    .80     .80     .80     .821
   1993.wce-r    .56     .56     .46     .465    .47     .469    .56     .56     .56     .56
   1993.wce-k    .74     .61     .46     .465    .47     .469    .61     .62     .65     .63
   1993.flde     .85     .85     .85     .85     .85     .85     .85     .85     .85     .85

   2000.cca      .519    .584    .417    .373   2.561   2.176    .592    .502    .923    .758
   2000.ccap     .829    .319    .157    .214    .981   1.041    .546    .205    .894    .831
   2000.ceff     .854    .804    .75     .738    .811    .816    .80     .80     .80     .821
   2000.wce-r    .57     .57     .47     .475    .49     .479    .57     .57     .57     .57
   2000.wce-k    .75     .62     .47     .475    .49     .479    .61     .62     .65     .63
   2000.flde     .85     .85     .85     .85     .85     .85     .85     .85     .85     .85 ;

* 1988 parameters are derived considering the following projects;
* command water management project  50% completion
* scarp vi, scarp mardan, khairpur tile, fourth drainage, ofwmi and ii
* irrigation system rehablitation phase i, chasma right bank.

Table subdef(sa,cnl) 'sub-area definition (proportion of cca) by canals'
        01-ud  02-cbd  03-ray   04-uc   05-mr  06-sad  07-for  08-pak   09-ld  10-lbd  11-jha
   s1       1      .5       1       1       1       1       1       1       1      .5     .32
   s2              .5                                                              .5     .49
   s3                                                                                     .19

   +   12-gug   13-uj   14-lj  15-bah  16-mai  17-sid  18-hav  19-ran  20-pan  21-abb  22-usw
   s1     .26       1     .64      .8     .65       1       1       1      .7       1       1
   s2     .53             .36      .2     .35                              .3
   s3     .21

   +   23-lsw  24-war  25-kab  26-tha  27-pah  28-muz  29-dgk  31-p+d  32-beg  33-gho   34-nw
   s1       1       1       1     .35       1     .25       1       1      .5      .5       1
   s2                             .17             .75                      .5      .5
   s3                             .30
   s4                             .18

   +   35-ric  36-dad   37-kw   38-ke  39-roh  41-nar  42-kal  43-lch  44-ful  45-pin
   s1       1       1       1       1     .39      .2       1       1       1       1
   s2                                     .20      .8
   s3                                     .16
   s4                                     .25                                        ;

Set
   zsa(z,cnl,sa) 'canal-subarea to agroclimatic zone mapping'
                 / nwfp.(22-usw.  s1,           23-lsw.  s1,     24-war.  s1,     25-kab.  s1)
                   pmw. (26-tha. (s1,s2,s3,s4), 27-pah.  s1,     28-muz.  s1)
                   pcw. (01-ud.   s1,           02-cbd.  s2,     06-sad.  s1,     07-for.  s1,     08-pak. s1
                         09-ld.   s1,           10-lbd. (s1,s2), 15-bah. (s1,s2), 16-mai. (s1,s2), 17-sid. s1
                         19-ran.  s1,           20-pan. (s1,s2), 21-abb.  s1,     28-muz.  s2,     29-dgk. s1)
                   psw. (11-jha. (s2,s3),       12-gug. (s2,s3), 13-uj.   s1,     14-lj.  (s1,s2), 18-hav. s1)
                   prw. (02-cbd.  s1,           03-ray. s1,      04-uc.   s1,     05-mr.   s1,     11-jha. s1, 12-gug.s1)
                   scwn.(33-gho. (s1,s2),       37-kw. s1,       38-ke. s1,       39-roh. (s1,s2), 41-nar. s1)
                   srwn.(31-p+d. s1,            32-beg. (s1,s2), 34-nw. s1,       35-ric.  s1,     36-dad. s1)
                   scws.(39-roh. (s3,s4),       41-nar.  s2   )
                   srws.(42-kal. s1,            43-lch. s1,      44-ful. s1,      45-pin. s1 ) /
   gwf(cnl,sa)   'subareas with fresh ground water'
                 / 01-ud.  s1,    02-cbd. (s1,s2),03-ray. s1, 04-uc.  s1,     05-mr.   s1
                   07-for. s1,    08-pak.  s1,    09-ld.  s1, 10-lbd.(s1,s2), 11-jha. (s1,s2)
                   12-gug.(s1,s2),13-uj.   s1,    14-lj.  s1, 15-bah. s1,     16-mai.  s1
                   17-sid. s1,    19-ran.  s1,    20-pan. s1, 21-abb. s1,     22-usw.  s1
                   23-lsw. s1,    24-war.  s1,    25-kab. s1, 26-tha.(s1,s3), 27-pah.  s1
                   28-muz. s1,    29-dgk.  s1,    31-p+d. s1, 32-beg. s1,     33-gho.  s1
                   37-kw.  s1,    39-roh. (s1,s3) /;

gwfg(cnl,sa,"saline")$subdef(sa,cnl) = yes;
gwfg(cnl,sa,"saline")$gwf(cnl,sa)    = no;
gwfg(cnl,sa,"fresh")$gwf(cnl,sa)     = yes;

Parameter carea(cnl,*) 'cca classified by groundwater quality for each canal';

loop(isr, carea(cnl,g) = sum(sa$gwfg(cnl,sa,g), subdef(sa,cnl)*comdef(isr,"cca",cnl)));
carea(cnl,"total")     = sum(g, carea(cnl,g));
display carea;

$onText
* Report and check on acz definition
Set zsa1(cnl,sa,z);

Parameter rep3, rep4;
rep3(is,cnl,sa)        = subdef(sa,cnl)*comdef(is,"cca",cnl);
rep3(is,cnl,g)         = sum(sa$gwfg(cnl,sa,g), subdef(sa,cnl)*comdef(is,"cca",cnl));
rep3(is,cnl,"total")   = sum(g, rep3(is,cnl,g));
rep3(is,"total",t1)    = sum(cnl, rep3(is,cnl,t1));
rep3(is,cnl,"cca")     = comdef(is,"cca",cnl);
rep3(is,"total","cca") = sum(cnl, comdef(is,"cca",cnl));
rep4(is,z,g)           = sum((cnl,sa)$(zsa(z,cnl,sa)$gwfg(cnl,sa,g)), comdef(is,"cca",cnl)*subdef(sa,cnl));
rep4(is,z,"total")     = sum((cnl,sa)$zsa(z,cnl,sa), comdef(is,"cca",cnl)*subdef(sa,cnl));
rep4(is,"total",t1)    = sum(z, rep4(is,z,t1));
zsa1(cnl,sa,z)$zsa(z,cnl,sa) = yes;
display zsa1, gwf, gwfg, rep3, rep4;
$offText

$sTitle Climatic Data
Table evap(cnl,m) 'pan evaporation (feet)'
                     jan   feb   mar    apr    may    jun    jul   aug   sep   oct   nov   dec
   01-ud            .244  .314  .545   .786   .982  1.040   .913  .814  .729  .571  .357  .244
   02-cbd           .200  .314  .529   .771   .968   .998   .829  .743  .686  .543  .329  .229
   03-ray           .200  .314  .514   .771   .951   .968   .812  .743  .671  .543  .326  .214
   04-uc            .200  .283  .514   .771   .940   .998   .829  .743  .671  .543  .326  .214
   05-mr            .200  .286  .529   .769   .942   .998   .829  .743  .671  .543  .326  .214
   06-sad           .286  .414  .648   .814  1.139  1.254  1.012  .940  .743  .629  .402  .286
   07-for           .271  .400  .614   .786  1.083  1.196   .968  .893  .714  .564  .386  .286
   08-pak           .271  .343  .588   .829  1.012  1.056   .970  .885  .771  .614  .400  .288
   09-ld            .243  .312  .586   .786   .969  1.056   .955  .871  .771  .586  .371  .286
   10-lbd           .243  .314  .557   .786   .982  1.040   .925  .938  .745  .581  .360  .243
   11-jha           .229  .300  .514   .729   .990  1.012   .870  .757  .700  .486  .343  .217
   12-gug           .214  .312  .526   .769   .965  1.008   .826  .740  .683  .555  .326  .226
   13-uj            .229  .300  .514   .729   .967  1.012   .870  .760  .700  .571  .343  .214
   14-lj            .257  .314  .557   .788   .982  1.026   .930  .843  .743  .600  .371  .260
   15-bah           .271  .360  .657   .860  1.069  1.098  1.011  .899  .771  .657  .400  .271
   16-mai           .271  .340  .629   .842  1.042  1.056   .940  .869  .757  .643  .400  .271
   17-sid           .257  .343  .588   .842  1.026  1.026   .940  .860  .757  .643  .414  .271
   18-hav           .271  .329  .571   .800   .982  1.025   .940  .857  .757  .600  .386  .271
   19-ran           .244  .314  .557   .786   .982  1.040   .913  .814  .729  .571  .357  .244
   20-pan           .200  .314  .529   .771   .968  1.112  1.026  .913  .800  .543  .329  .457
   21-abb           .271  .338  .671   .858  1.085  1.098  1.026  .913  .788  .671  .400  .288
  (22-usw,23-lsw,
   24-war,25-kab)   .214  .257  .429   .643   .913  1.025   .969  .870  .657  .529  .329  .200
   26-tha           .260  .331  .517   .760  1.057  1.143  1.057  .899  .721  .631  .402  .245
   27-pah           .257  .329  .502   .729  1.094  1.225  1.154  .943  .829  .671  .357  .243
   28-muz           .271  .340  .600   .870  1.056  1.039   .968  .885  .771  .657  .414  .271
   29-dgk           .286  .357  .657   .885  1.082  1.098  1.011  .913  .798  .671  .414  .271
   31-p+d           .371  .457  .758   .956  1.142  1.126  1.026  .942  .830  .715  .486  .343
  (32-beg,34-nw,
   35-ric)          .386  .474  .700   .870  1.068   .949   .943  .949  .757  .657  .457  .343
   33-gho           .429  .429  .700   .871  1.040  1.098   .998  .900  .800  .629  .457  .329
   36-dad           .357  .429  .700   .956  1.169  1.185  1.027  .940  .786  .700  .429  .357
   37-kw            .386  .414  .674   .900  1.083  1.112   .998  .899  .786  .643  .429  .343
   38-ke            .429  .443  .745   .926  1.152  1.154  1.025  .955  .829  .700  .486  .371
   39-roh           .371  .414  .700   .940  1.154  1.139   .982  .926  .757  .698  .457  .371
   41-nar           .457  .500  .843  1.054  1.212  1.155   .999  .914  .829  .786  .557  .443
   42-kal           .529  .557  .857   .982  1.094   .970   .814  .757  .786  .800  .614  .529
  (43-lch,44-ful)   .471  .529  .870  1.095  1.210  1.111   .985  .886  .843  .814  .586  .471
   45-pin           .557  .586  .899  1.044  1.140  1.012   .843  .800  .829  .843  .643  .557;

Table rain(cnl,m) 'rain (inches)'
                       jan    feb    mar    apr   may    jun    jul    aug    sep    oct   nov   dec
   01-ud              .770   .520   .660   .470  .450  1.180  4.480  4.070  1.530   .160  .220  .360
   02-cbd             .878   .700   .712   .510  .506  1.364  4.868  4.502  1.798   .192  .172  .404
   03-ray            1.180  1.120  1.040   .740  .710  1.820  5.610  6.450  2.460   .230  .140  .490
   04-uc             1.141  1.089  1.013   .724  .693  1.779  5.444  6.278  2.396   .224  .138  .478
   05-mr             1.180  1.120  1.040   .740  .710  1.820  5.610  6.450  2.460   .230  .140  .490
  (06-sad,07-for)     .500   .520   .520   .360  .310   .720  3.500  3.000   .860   .090  .110  .240
   08-pak             .505   .520   .523   .362  .313   .729  3.520  3.021   .873   .091  .112  .242
   09-ld              .689   .520   .618   .437  .408  1.042  4.186  3.749  1.329   .139  .187  .324
   10-lbd             .564   .522   .543   .380  .360   .778  3.545  3.130  1.018   .111  .122  .278
   11-jha             .627   .686   .662   .509  .465  1.239  3.286  4.035  1.564   .139  .112  .315
   12-gug             .548   .624   .608   .476  .430  1.156  2.954  3.690  1.436   .126  .108  .290
   13-uj             1.210  1.170  1.240   .850  .690  1.500  5.320  5.610  2.030   .230  .160  .480
   14-lj              .920   .889   .942   .646  .524  1.140  4.043  4.264  1.543   .175  .122  .365
   15-bah             .230   .290   .380   .250  .170   .260  2.520  1.930   .190   .020  .120  .130
   16-mai             .402   .434   .463   .316  .270   .553  3.020  2.530   .618   .068  .107  .208
   17-sid             .420   .435   .445   .305  .325   .495  2.570  2.265   .665   .085  .080  .245
   18-hav             .390   .500   .500   .410  .360   .990  2.290  3.000  1.180   .100  .100  .240
   19-ran             .370   .380   .400   .270  .330   .550  2.010  1.820   .540   .080  .060  .240
   20-pan             .220   .252   .296   .184  .140   .184  1.716  1.444   .152   .010  .108  .120
   21-abb             .221   .263   .326   .205  .143   .206  1.998  1.615   .163   .011  .102  .121
  (22-usw,23-lsw,
   24-war,25-kab)    1.440  1.530  2.440  1.760  .770   .310  1.260  2.030   .810   .230  .310  .670
   26-tha             .610   .717   .882   .621  .448   .809  2.946  2.769   .944   .132  .130  .298
   27-pah             .450   .670   .960   .690  .390   .610  2.290  1.900   .630   .110  .150  .240
   28-muz             .375   .400   .435   .298  .335   .555  2.028  1.825   .548   .083  .068  .240
   29-dgk             .260   .330   .270   .220  .180   .380   .740  1.230   .310   .030  .070  .210
   (31-p+d,32-beg)    .199   .282   .204   .179  .100   .257   .447   .905   .187  0.000  .097  .179
   33-gho             .210   .230   .250   .140  .120   .140  1.230  1.160   .130  0.000  .110  .110
   34-nw              .171   .336   .263   .171  .100   .242   .612   .994   .330  0.000  .035  .173
   35-ric             .173   .341   .273   .173  .100   .251   .706  1.020   .346  0.000  .032  .163
   36-dad             .170   .335   .270   .170  .100   .275  1.330  1.170   .400  0.000  .035  .130
   37-kw              .190   .240   .200   .140  .100   .250  1.310  1.210   .270  0.000  .090  .100
   38-ke              .150   .200   .200   .100  .100   .250  1.350  1.450   .350  0.000  .050  .100
   39-roh             .138   .235   .188   .143  .150   .368  2.297  1.876   .564   .034  .051  .095
   41-nar             .091   .151   .119   .103  .159   .397  3.003  2.097   .658   .071  .056  .060
  (42-kal,43-lch,
   44-ful,45-pin)     .140   .300   .120   .060  .140   .720  4.020  2.080   .700   .060  .060  .120;

$sTitle Historic Canal Diversions (maf)
Table divpost(cnl,m1) 'average (1976-77 to 1987-88) canal diversions (maf)'
* source: indus basin irrigation system
*         historic rivers and canals discharge data,
*         water resources management, directorate. wapda lahore
              apr    may    jun    jul    aug    sep    oct    nov    dec    jan    feb    mar    rabi  kharif  annual
   01-ud    .0238  .1028  .1091  .1155  .1183  .1263  .0730  .0236  .0072  .0100  .0186  .0343   .1668   .5958  0.7626
   02-cbd   .1102  .1339  .1372  .1278  .1151  .1202  .1284  .1294  .1113  .0500  .0958  .1010   .6159   .7443  1.3602
   03-ray   .0025  .0370  .0884  .0902  .0820  .0855  .0473  .0025  .0020  .0005  .0024  .0007   .0553   .3856  0.4409
   04-uc    .0984  .1829  .2771  .3155  .2713  .2577  .1928  .0822  .0343  .0428  .0631  .0566   .4719  1.4029  1.8747
   05-mr    .0163  .0964  .1218  .2160  .2366  .1665  .0402  .0122  .0107  .0111  .0176  .0103   .1020   .8536  0.9556
   06-sad   .2686  .2865  .2838  .2762  .2677  .2693  .2538  .2851  .2719  .1106  .2329  .2796  1.4339  1.6522  3.0861
   07-for   .0340  .1224  .1481  .1433  .1415  .1481  .1044  .0102  .0091  .0061  .0137  .0137   .1572   .7375  0.8946
   08-pak   .2183  .3408  .3542  .3536  .3881  .3739  .2978  .2190  .2399  .1134  .1833  .2470  1.3003  2.0289  3.3292
   09-ld    .0479  .1923  .2140  .2295  .2320  .2278  .1535  .0451  .0258  .0256  .0479  .0295   .3273  1.1436  1.4709
   10-lbd   .4055  .4822  .4717  .4564  .4611  .4590  .4244  .4269  .3860  .1539  .3521  .4069  2.1503  2.7360  4.8862
   11-jha   .2838  .3425  .3423  .3177  .3010  .3391  .3284  .3025  .2970  .1279  .1941  .2728  1.5227  1.9265  3.4492
   12-gug   .3186  .3845  .3843  .3567  .3379  .3807  .3687  .3396  .3334  .1436  .2179  .3063  1.7094  2.1626  3.8720
   13-uj    .0791  .1180  .1285  .1486  .1294  .1305  .1117  .0884  .0732  .0461  .0457  .0462   .4114   .7341  1.1454
   14-lj    .2421  .3128  .3155  .3099  .2902  .2901  .2987  .2509  .2151  .1084  .2138  .2118  1.2988  1.7607  3.0594
   15-bah   .1205  .2033  .2293  .2448  .2282  .2359  .2008  .1238  .1212  .0958  .0827  .1475   .7718  1.2619  2.0337
   16-mai   .1441  .2961  .3286  .3163  .3375  .3478  .2875  .1336  .1392  .1309  .0897  .1643   .9452  1.7704  2.7156
   17-sid   .1469  .2226  .2334  .2216  .2264  .2332  .2203  .1631  .1593  .0975  .1124  .1632   .9159  1.2841  2.2000
   18-hav   .0314  .0455  .0474  .0447  .0464  .0513  .0472  .0252  .0282  .0224  .0255  .0318   .1803   .2666  0.4469
   19-ran   .0314  .0809  .0904  .0823  .0689  .0817  .0663  .0174  .0147  .0206  .0076  .0242   .1507   .4356  0.5863
   20-pan   .2684  .4531  .5359  .5554  .5569  .5577  .5056  .2296  .1913  .1355  .1759  .2235  1.4613  2.9275  4.3888
   21-abb   .0408  .0529  .0602  .0599  .0604  .0602  .0567  .0375  .0328  .0202  .0280  .0357   .2109   .3343  0.5453
   22-usw   .1025  .1131  .1101  .1094  .1028  .1048  .1052  .0876  .0802  .0631  .0654  .0869   .4884   .6426  1.1310
   23-lsw   .0413  .0518  .0514  .0461  .0401  .0455  .0487  .0388  .0372  .0051  .0239  .0308   .1845   .2762  0.4607
   24-war   .0377  .0441  .0436  .0385  .0307  .0412  .0454  .0436  .0406  .0021  .0145  .0263   .1725   .2358  0.4083
   25-kab   .0339  .0434  .0466  .0440  .0366  .0425  .0434  .0392  .0328  .0017  .0177  .0341   .1688   .2470  0.4158
   26-tha   .3777  .4206  .3864  .4027  .3991  .4096  .4016  .3934  .3680  .1064  .3175  .3615  1.9483  2.3962  4.3445
   27-pah   .0269  .0340  .0355  .0363  .0307  .0340  .0348  .0301  .0315  .0045  .0259  .0288   .1556   .1976  0.3532
   28-muz   .1041  .3154  .3886  .3937  .3812  .3749  .2665  .1312  .0745  .0707  .0920  .1057   .7407  1.9578  2.6985
   29-dgk   .1874  .3408  .4056  .4062  .3455  .3699  .2960  .1787  .1091  .0935  .1275  .1580   .9629  2.0555  3.0183
   31-p+d   .0020  .0812  .5064  .5786  .4625  .4169  .2224  .0942  .0700  .1628  .0665  .0632   .6791  2.0475  2.7266
   32-beg   .0017  .0798  .7547 1.0590  .6712  .5522  .1528  .0023  .0122  .1344  .0059  .0038   .3115  3.1186  3.4300
   33-gho   .0134  .2218  .4264  .4093  .4541  .4266  .3847  .2007  .1010  .2016  .0720  .1217  1.0817  1.9517  3.0334
   34-nw    .0917  .1070  .3522  .4994  .4373  .3499  .3011  .2055  .1948  .0973  .2070  .1844  1.1900  1.8375  3.0275
   35-ric   .0000  .1199  .5937  .5945  .4428  .3722  .1454  .0000  .0000  .0000  .0000  .0000   .1454  2.1232  2.2686
   36-dad   .0775  .0820  .1964  .2978  .2643  .2269  .1894  .1350  .1378  .0620  .1494  .1400   .8137  1.1449  1.9587
   37-kw    .0904  .1003  .1086  .1123  .1112  .1283  .1187  .1064  .1073  .0342  .0908  .0938   .5512   .6511  1.2024
   38-ke    .1140  .1357  .1464  .1564  .1537  .1504  .1609  .1456  .1456  .0442  .1185  .1186   .7334   .8567  1.5901
   39-roh   .6860  .7981  .7821  .8123  .8474  .8517  .8026  .7187  .7404  .2608  .7123  .7525  3.9873  4.7776  8.7649
   41-nar   .6064  .6819  .6671  .6932  .6901  .6967  .6916  .6246  .6078  .2227  .5419  .5689  3.2576  4.0355  7.2931
   42-kal   .1342  .1644  .2888  .4046  .3561  .3739  .2843  .1374  .0777  .1522  .0963  .0861   .8340  1.7220  2.5559
   43-lch   .1155  .1476  .1645  .1671  .1421  .1451  .1332  .0954  .0612  .0618  .0752  .0780   .5048   .8819  1.3867
   44-ful   .1021  .3667  .6712  .6460  .4545  .4078  .2991  .1375  .0732  .1594  .1005  .0591   .8287  2.6483  3.4770
   45-pin   .0464  .1899  .4162  .5242  .4391  .3334  .2264  .1068  .0469  .1041  .0611  .0349   .5802  1.9493  2.5295;

$sTitle Govt. Tubewell Pumpage and Depth to Water Table
Table gwt(cnl,m) 'public tunewell pumpage (kaf)'
            jan  feb  mar  apr  may  jun  jul  aug  sep  oct  nov  dec
   01-ud      0    0    1    0    1    0    1    1    1    0    0    0
   02-cbd     2    2    3    2    2    2    6    7    8    2    2    2
   03-ray    15   29   41   45   40   50   30   17   48   43   19    7
   04-uc     39   49   49   30   49   49   55   58   58   16   36   15
   05-mr      3    6   20   17    9   11    8    6   12    9    5    2
   09-ld      1    1    1    1    1    1    0    0    0    1    1    1
   10-lbd     2    2    2    2    2    2    2    3    3    1    2    2
   11-jha    38   42   49   57   57   57   50   54   60   57   48   32
   12-gug    50   62   69   78   79   79   69   73   82   85   67   34
   13-uj     67   88  104  119  141  121  114  114  140  155  117    5
   14-lj     51   65   88   70   81   72   87   81   94   91   70   77
   15-bah     0    0    1    0    1    1    2    2    2    0    1    1
   18-hav     6    6    8    6    7    7    6    6    6    5    7    7
   19-ran    55   66   82   84   68   52   36   57   59   52   51   43
   20-pan     6    6    8    6    6    6    6    6    5    5    6    6
   21-abb     1    1    2    1    2    2    1    2    1    1    2    2
   28-muz   114  114  159  147  109   89   80   90   94   45   79  119
   32-beg     2    3    4    2    2    1    0    1    1    2    2    2
   37-kw     18   20   24   19   16   20   18   18   16   10   11   11
   39-roh     6    7    8    6    5    7    6   48   42    3    4    4;

Table dep1(cnl,is, *) 'depth to water table (feet)'
* 1980 depth is from ibm and irrgation supporting report of rap.
* year 1993 and 2000 depths are estimated considering the drainage
* projects under construction or planning.
                                 1980.depth  (1988,1993,2000).depth
   01-ud                               17.0                      17
   02-cbd                              10.5                      10
  (03-ray,05-mr)                       15.0                      15
   04-uc                               15.1                      15
   06-sad                               5.5                       7
  (07-for,22-usw,23-lsw,
   24-war,25-kab)                       8.0                       8
   08-pak                              19.9                      15
   09-ld                               17.9                      17
   10-lbd                              18.1                      18
   11-jha                              13.6                      14
   12-gug                              14.8                      14
   13-uj                               10.0                      10
   14-lj                                8.1                       9
  (15-bah,19-ran,28-muz)                9.0                       9
  (16-mai,17-sid)                      19.0                      19
   18-hav                              10.1                      10
   20-pan                               8.4                       9
   21-abb                               8.2                       9
   26-tha                               8.7                       8
   27-pah                              10.0                      12
   29-dgk                               7.8                       8
  (31-p+d,32-beg,36-dad,39-roh)                                   7
  (34-nw,35-ric,42-kal,43-lch,
   44-ful,45-pin)                                                 6
   33-gho                               5.5                       7
   37-kw                                5.6                       7
   38-ke                                5.4                       7
   41-nar                               4.9                       7;

Table dep2(is,cnl,m) 'depth to water table (feet)'
                                       jan  feb  mar  apr  may  jun  jul  aug  sep  oct  nov  dec
   1980. 31-p+d                        6.9  6.9  6.9  6.9  6.9  4.5  2.5  2.5  2.5  3.5  4.5  6.9
   1980. 32-beg                        6.6  6.6  6.6  6.6  6.6  4.5  2.5  2.5  2.5  3.5  4.5  6.6
   1980.(34-nw,35-ric)                 4.6  4.6  4.6  4.6  4.6  4.5  2.5  2.5  2.5  3.5  4.5  4.6
   1980. 36-dad                        5.6  5.6  5.6  5.6  5.6  4.5  2.5  2.5  2.5  3.5  4.5  5.6
   1980. 39-roh                        5.9  5.9  5.9  5.9  5.9  5.8  5.8  5.8  5.8  5.9  5.9  5.9
   1980.(42-kal,43-lch,44-ful,45-pin)  5.0  5.0  5.0  5.0  5.0  3.5  2.0  2.0  2.5  4.0  5.0  5.0;

Parameter
   depth(cnl,m)  'depth to groundwater (feet)'
   efr(cnl,m)    'effective rainfall in feet'
   eqevap(cnl,m) 'evaporation from the equaifer (feet)'
   subirr(cnl,m) 'water supplied by capillary action from the aquifer'
   subirrfac(z)  'maximum sub-irrigation in saline areas as proportion of crop req. (net of rain)'
                 / (nwfp, pmw, pcw, psw, prw, srwn, scws) .4
                                                    scwn  .3
                                                    srws  .1 /;

Scalar
   drc  'run-off portion of rainfall'                   /  .15 /
   the1 'portion of equaifer evaporation used by crops' / 0.6  /;

* Sub-irrigation and effective rain calculations
loop(isr,
   depth(cnl,m) = dep1(cnl,isr,"depth");
   depth(cnl,m)$(dep2(isr,cnl,m) <> 0 ) = dep2(isr,cnl,m);
   efr(cnl,m)   = (1.0 - drc - (1 - comdef(isr,"flde",cnl)))*rain(cnl,m)/12.0;
);

eqevap(cnl,m) = min(1., 10.637/depth(cnl,m)**2.558)*evap(cnl,m);
subirr(cnl,m) = eqevap(cnl,m)*the1;

option  depth:1;
display depth;

$sTitle Network Nodes and Arc Definition
Set
   n 'nodes of the indus river system'
     / sulem-b    'sulemanki barrage              - sutlej river'
       islam-b    'islam barrage                  - sutlej river'
       panjnad-b  'panjnad barrage                - panjnad river'
       ravi-i     'ravi inflow at madhopur        - ravi river'
       balloki-b  'balloki barrage                - ravi river'
       sidhnai-b  'sidhani barrage                - ravi river'
       marala-b   'marrala barrage                - chenab river'
       khanki-b   'khanki barrage                 - chenab river'
       qadira-b   'qadirabad barrage              - chenab river'
       trimmu-b   'trimmu barrage                 - chenab river'
       mangla-r   'mangla reservoir               - jehlum river'
       rasul-b    'rasul barrage                  - jhelum river'
       tarbela-r  'tarbela reservoir              - indus river'
       amanda-h   'amandara head works            - swat river'
       munda-h    'munda head works               - swat river'
       warsak-d   'warsak reservoir               - kabul river'
       k-s-jct    'kabul and swat river jct       - kabul river'
       kalabagh-r 'kalabagh reservoir             - indus river'
       chasma-r   'chasma reservoir               - indus river'
       taunsa-b   'taunsa barrage                 - indus river'
       gudu-b     'gudu barrage                   - indus river'
       sukkur-b   'sukkur barrage                 - indus river'
       nara-jct   'nara junction                  - nara complex'
       nara-head  'head works for irri. diversion - nara complex'
       chotiari-r 'chotiari reservoir             - nara complex'
       kotri-b    'kotri barrage                  - indus river'
       a-sea      'arabian sea'
       a1         'diversion point for uj uj link and r.p.c'
       a2         'div. for brbd link uc link and uc int.'
       a3         'div. for mr cross link'
       a4         'mr cross tail and brbd link'
       a5         'div. for cbd and ud canals'
       a6         'diversion to lcc feeder'
       a7         'div. for gugera and jhang canals'
       a8         'divesion point for maili+l pakpattan canals'
       a9         'smb link to l-bahawl canal'
       a10        'diversion point for lj' /
   i 'system inflows'
     / swat       'swat river at chakdara'
       kabul      'kabul river at warsak'
       indus      'indus river at tarbela'
       haro       'haro river at gariall'
       soan       'soan river at dhok pathan'
       jehlum     'jehlum river at mangls'
       chenab     'chenab river at marrala'
       ravi       'ravi river below madhopur'
       sutlej     'sutlej river below ferozpur' /
   nc(n,cnl) 'node to canal map'
     / sulem-b.  (06-sad,07-for,08-pak), a8.        16-mai
       a9.        15-bah               , panjnad-b.(20-pan,21-abb)
       balloki-b.(10-lbd,09-ld)        , sidhnai-b. 17-sid
       marala-b.  05-mr                , a2.       (04-uc,03-ray)
       a5.       (02-cbd,01-ud)        , a7.       (11-jha,12-gug)
       trimmu-b. (19-ran,18-hav)
       a1.        13-uj                , a10.       14-lj
       amanda-h.  22-usw               , munda-h.   23-lsw
       warsak-d.  24-war               , warsak-d.  25-kab
       chasma-r. (26-tha,27-pah)
       taunsa-b. (28-muz,29-dgk)       , gudu-b.   (31-p+d,32-beg,33-gho)
       sukkur-b. (34-nw,35-ric,36-dad,37-kw,38-ke,39-roh)
       nara-head. 41-nar
       kotri-b.  (42-kal,43-lch,44-ful,45-pin) /;

Alias (n,n1);

Set
   nn(n,n1) 'water flow system node to node'
            /
*            sutlej ravi system
             sulem-b.    balloki-b
             islam-b.    sulem-b
             a9.        (a8,       islam-b)
             a8.         sidhnai-b
             panjnad-b. (trimmu-b,sidhnai-b, islam-b, taunsa-b)
*            ravi chenab system
             ravi-i.     a3
             a3.         marala-b
             a4.        (a3,a2)
             a5.         a4
             a2.         marala-b
             balloki-b. (ravi-i,   a2,        a6)
             a6.         qadira-b
             a7.        (a6,       khanki-b)
             sidhnai-b. (trimmu-b, balloki-b)
*            chenab jehlum system
             khanki-b.  (a1,       marala-b)
             qadira-b.  (rasul-b,  khanki-b)
             trimmu-b.  (qadira-b, rasul-b, chasma-r)
             a1.         mangla-r
             rasul-b.   (mangla-r, a10)
             a10.       (rasul-b,  a1)
*            kabul swat system
             munda-h.    amanda-h
             k-s-jct.   (munda-h,  warsak-d)
*            indus river
             kalabagh-r.(k-s-jct,  tarbela-r)
             chasma-r.   kalabagh-r
             taunsa-b.   chasma-r
             gudu-b.    (panjnad-b,taunsa-b)
             sukkur-b.   gudu-b
             nara-jct.   sukkur-b
             chotiari-r. nara-jct
             nara-head. (nara-jct,  chotiari-r)
             kotri-b.    sukkur-b
             a-sea.      kotri-b /
   ni(n,i) 'node to rim station inflow map'
           / amanda-h.  swat  , warsak-d.  kabul
             tarbela-r. indus , kalabagh-r.haro
             kalabagh-r.soan  , mangla-r.  jehlum
             marala-b.  chenab, ravi-i.    ravi
             sulem-b.   sutlej /
   nb(n);

nb(n)       = yes;
nb("a-sea") = no;

* flow capacities
Parameter
    ncap(n,n1) 'node to node transfer capacity (maf)'
               / chasma-r. trimmu-b    1.307, taunsa-b.  panjnad-b  .724
                 sukkur-b. nara-jct    1.190, nara-jct.  nara-head 1.190
                 nara-jct. chotiari-r   .416, chotiari-r.nara-head  .135
                 mangla-r. a1           .685, a1.        a10        .199
                 rasul-b.  a10          .328, a1.        khanki-b   .422
                 rasul-b.  qadira-b    1.232,
                 marala-b. a3           .985, a3.        a4         .199
                 a3.       ravi-i       .938, marala-b.  a2         .995
                 a2.       balloki-b    .698, a2.        a4         .310
                 a4.       a5           .310
                 khanki-b. a7           .684, qadira-b.  a6        1.146
                 a6.       a7           .270, a6.        balloki-b  .808
                 balloki-b.sulem-b     1.172, trimmu-b.  sidhnai-b 1.03
                 sidhnai-b.a8           .662, a8.        a9         .640
                 islam-b.  a9           .299 /
   lloss(n,n1) 'link canal loss factors'
               / chasma-r. trimmu-b   .096 , taunsa-b.  panjnad-b .0615
                 sukkur-b. nara-jct   .055 , nara-jct.  nara-head .0001
                 nara-jct. chotiari-r .001 , chotiari-r.nara-head .0001
                 a1.       a10        .004 , a1.        khanki-b  .099
                 rasul-b.  qadira-b   .039 , marala-b.  a3        .0705
                 a3.       a4         .0075, a2.        balloki-b .0705
                 a2.       a4         .097 , a4.        a5        .007
                 khanki-b. a7         .0435, a6.        balloki-b .087
                 balloki-b.sulem-b    .0525, trimmu-b.  sidhnai-b .075
                 sidhnai-b.a8         .0495 /
   lceff(n,n1) 'link canal efficiency from head to tail';

lceff(n,n1)$lloss(n,n1) = 1 - lloss(n,n1);

* display n, nc, nn, ni, ncap, lloss, lceff;

$sTitle Flow Routing Coefficints
Set cd / c, d /;

Table rivercd(n,cd) 'coefficients for river routing'
                     c       d
   rasul-b      .02732  .93766
   trimmu-b     .15348
   khanki-b             2.2113
   balloki-b
   sidhnai-b    .14063  1.0024
   islam-b      .12294
   panjnad-b    .08077
   gudu-b       .10947
   kalabagh-r   .04856
   chasma-r     .04526
   taunsa-b     .07205
   sukkur-b     .02218
   kotri-b      .17054;

Table riverb(n,n1) 'coefficients for river routing'
                mangla-r  rasul-b  qadira-b  marala-b  ravi-i  balloki-b
   rasul-b       1.01841
   trimmu-b                .82803     .9068
   khanki-b                                    .93361
   balloki-b                                           1.2181
   sidhnai-b                                                      .70555

   +             sulem-b  trimmu-b  sidhnai-b  islam-b  taunsa-b  panjnad-b
   islam-b         .9086
   panjnad-b                .81033    1.13626   .80359
   gudu-b                                                 .94003        1.0

   +             k-s-jct  tarbela-r  kalabagh-r  chasma-r  gudu-b  sukkur-b
   kalabagh-r    1.04621    1.08686
   chasma-r                               .9849
   taunsa-b                                         .9497
   sukkur-b                                                 .9908
   kotri-b                                                           .70625;

riverb(n,n1)$(riverb(n,n1)     = 0) = 1;
rivercd(n,"d")$(rivercd(n,"d") = 0) = 1;

$sTitle Rim Station and Tributory Inflows
Set s58 / 50, 80 /;

Table infl5080(s58,i,m1) 'system inflows measured atthe rim stations (maf)'
* 50 - 50% probability flows and 80 are 80 % probability flows.
*
* source: water resouce management directorate, wapda publications and
*         rap irrigation and drainage supporting report
*         historic data used: indus    oct 1936 - march 1988
*                             jehlum april 1922 - march 1988
*                             chenab april 1922 - march 1988
*                    ravi and sutlej april 1966 - march 1988
*                    all others      april 1966 - march 1976

                 apr    may    jun     jul     aug    sep  kharif
   50.indus    1.931  3.984  7.191  16.148  17.437  6.196  52.887
   50.jehlum   3.826  4.355  2.993   3.429   2.236  0.846  17.685
   50.chenab   1.361  2.281  4.036   5.279   5.097  2.540  20.594
   50.ravi     0.226  0.161  0.038   1.089   1.732  0.841   4.088
   50.sutlej   0.031  0.045  0.052   0.483   1.003  1.198   2.810
   50.haro     0.016  0.013  0.014   0.048   0.204  0.073   0.369
   50.soan     0.012  0.007  0.090   0.225   0.426  0.068   0.829
   50.swat     0.483  0.636  1.049   0.863   0.642  0.346   4.020
   50.kabul    1.512  1.883  3.467   3.490   2.619  0.975  13.945

   80.indus    1.871  3.760  9.634  13.135  10.524  5.234  44.159
   80.jehlum   1.526  2.242  2.594   2.632   3.140  1.623  13.757
   80.chenab   1.120  2.064  3.133   4.515   3.537  2.844  17.213
   80.ravi     0.049  0.020  0.011   0.696   1.346  0.313   2.434
   80.sutlej   0.000  0.001  0.000   0.145   0.291  0.004   0.441
   80.haro     0.014  0.019  0.011   0.020   0.101  0.022   0.187
   80.soan     0.006  0.004  0.020   0.252   0.145  0.021   0.448
   80.swat     0.323  0.574  0.848   0.579   0.463  0.448   3.235
   80.kabul    0.963  1.737  2.238   2.157   2.303  1.534  10.934

   +             oct    nov    dec     jan     feb    mar   rabi
   50.indus    2.297  1.279  1.188   0.952   0.973  1.468   8.157
   50.jehlum   0.988  0.521  0.412   0.460   0.621  1.417   4.419
   50.chenab   0.728  0.487  0.407   0.480   0.711  1.079   3.891
   50.ravi     0.188  0.134  0.131   0.090   0.026  0.455   1.023
   50.sutlej   0.129  0.009  0.004   0.060   0.000  0.000   0.202
   50.haro     0.037  0.020  0.016   0.019   0.019  0.051   0.163
   50.soan     0.010  0.005  0.020   0.022   0.028  0.043   0.128
   50.swat     0.195  0.122  0.088   0.079   0.068  0.140   0.692
   50.kabul    0.523  0.405  0.354   0.379   0.440  0.687   2.787

   80.indus    2.133  1.281  1.060   0.898   0.835  1.025   7.232
   80.jehlum   0.405  0.270  0.309   0.306   0.526  1.514   3.331
   80.chenab   0.516  0.325  0.285   0.292   0.804  0.711   2.933
   80.ravi     0.076  0.048  0.074   0.135   0.112  0.135   0.580
   80.sutlej   0.004  0.002  0.004   0.005   0.002  0.000   0.017
   80.haro     0.024  0.016  0.018   0.017   0.018  0.018   0.112
   80.soan     0.012  0.010  0.009   0.010   0.019  0.031   0.091
   80.swat     0.103  0.070  0.061   0.064   0.085  0.212   0.597
   80.kabul    0.273  0.420  0.354   0.331   0.429  0.359   2.167;

Table tri(s58,n1,n,m1) 'tributary inflows (maf)'
* source: irrigation drainage and flood management supporting report of rap. planning division wapda.
                               apr    may    jun    jul    aug    sep  kharif
   50.marala-b. khanki-b     0.004  0.006  0.023  0.120  0.240  0.056   0.449
   50.mangla-r. rasul-b      0.120  0.180  0.150  0.170  0.320  0.210   1.150
   50.chasma-r. taunsa-b     0.051  0.013  0.021  0.048  0.021  0.012   0.166
   50.tarbela-r.kalabagh-r   0.041  0.087  0.182  0.350  0.602  0.180   1.442
   50.balloki-b.sidhnai-b    0.000  0.001  0.009  0.096  0.206  0.041   0.353
   50.amanda-h. munda-h      0.036  0.337  0.637  0.854  0.544  0.062   2.470
   50.munda-h.  k-s-jct      0.015  0.013  0.005  0.008  0.007  0.007   0.054
   50.warsak-d. k-s-jct      0.126  0.102  0.059  0.070  0.056  0.056   0.469

   80.marala-b. khanki-b     0.004  0.005  0.006  0.136  0.131  0.011   0.293
   80.mangla-r. rasul-b      0.120  0.180  0.150  0.170  0.320  0.210   1.150
   80.chasma-r. taunsa-b     0.021  0.014  0.006  0.047  0.008  0.003   0.098
   80.tarbela-r.kalabagh-r   0.052  0.094  0.106  0.126  0.416  0.079   0.873
   80.balloki-b.sidhnai-b    0.000  0.000  0.006  0.075  0.164  0.027   0.271
   80.amanda-h. munda-h      0.121  0.410  0.161  0.132  0.000  0.019   0.844
   80.munda-h.  k-s-jct      0.006  0.009  0.003  0.005  0.005  0.004   0.031
   80.warsak-d. k-s-jct      0.050  0.077  0.027  0.039  0.036  0.032   0.261

   +                           oct    nov    dec    jan    feb    mar    rabi
   50.marala-b. khanki-b     0.017  0.007  0.008  0.007  0.006  0.007   0.052
   50.mangla-r. rasul-b      0.060  0.000  0.000  0.000  0.000  0.000   0.060
   50.chasma-r. taunsa-b     0.004  0.005  0.010  0.006  0.015  0.026   0.065
   50.tarbela-r.kalabagh-r   0.100  0.064  0.038  0.055  0.052  0.130   0.439
   50.balloki-b.sidhnai-b    0.002  0.003  0.008  0.005  0.007  0.006   0.030
   50.amanda-h. munda-h      0.133  0.153  0.150  0.119  0.119  0.221   0.894
   50.munda-h.  k-s-jct      0.003  0.003  0.004  0.004  0.004  0.009   0.028
   50.warsak-d. k-s-jct      0.024  0.027  0.035  0.035  0.038  0.070   0.229

   80.marala-b. khanki-b     0.007  0.006  0.006  0.003  0.011  0.006   0.039
   80.mangla-r. rasul-b      0.060  0.000  0.000  0.000  0.000  0.000   0.060
   80.chasma-r. taunsa-b     0.009  0.006  0.010  0.006  0.008  0.011   0.050
   80.tarbela-r.kalabagh-r   0.088  0.039  0.041  0.040  0.042  0.045   0.295
   80.balloki-b.sidhnai-b    0.000  0.001  0.001  0.000  0.000  0.000   0.002
   80.amanda-h. munda-h      0.090  0.075  0.045  0.047  0.019  0.121   0.397
   80.munda-h.  k-s-jct      0.005  0.004  0.003  0.003  0.000  0.005   0.022
   80.warsak-d. k-s-jct      0.020  0.016  0.020  0.020  0.027  0.065   0.169;

Parameter
   inflow(i,m)  'inflows for this run           (maf)'
   trib(n1,n,m) 'tributary inflows for this run (maf)';

inflow(i,m)  = infl5080("50",i,m);
trib(n1,n,m) = tri("50",n1,n,m);

$sTitle Reservoir Characteristics
Table rrcap(n,is) 'live storage capacity of reservoirs (maf)'
                 1980   1988   2000
   mangla-r     5.053  4.881  4.617
   tarbela-r    9.197  8.861  8.357
   kalabagh-r                 6.100
   chasma-r     0.571   .435   .231
   chotiari-r                 1.100;

* storage capacities are provided by planning division of wapda.
* 1988 capacities are from recent hydrographic surveys.

Table rulelo(n,m) 'lower rule curve'
* end of month contents as percent of live capacity
                jan  feb  mar  apr  may  jun  jul  aug  sep  oct  nov  dec
   mangla-r      16    6    0    7   21   48   74   95   83   55   35   24
   tarbela-r     43   30   14    4    0   10   46  100  100   64   59   53
   kalabagh-r    55   49   34   28    0    0   24   70  100   96   78   61
   chasma-r       0    0    0    0    0    0    0    0    0    0    0    0
   chotiari-r     0    0    0    0    0    0    0    0    0    0    0    0;

Table ruleup(n,m) 'upper rule curve'
* end of month contents as percent of live capacity
                jan  feb  mar  apr  may  jun  jul  aug  sep  oct  nov  dec
   mangla-r      39   28   19   31   42   59   87  100  100   90   68   49
   tarbela-r     69   47   37   26   13   48   73  100  100  100   87   77
   kalabagh-r   100  100  100  100  100  100  100  100  100  100  100  100
   chasma-r     100  100  100  100  100  100  100  100  100  100  100  100
   chotiari-r   100  100  100  100  100  100  100  100  100  100  100  100;

Table revapl(n,m) 'evaporation losses from reservoirs (kaf)'
                jan  feb  mar  apr  may  jun  jul  aug  sep  oct  nov  dec
   mangla-r       1    5    4    5   12   17 -1.9 -2.5    4   22   10    5
   tarbela-r      1    5    7    9   16   28   10    1   12   29   12    5
   kalabagh-r     1    4    6    9   12    5    9    1    9   24    9    5
   chasma-r       3    3    1                   6    3    7   15    6    3;

Set
   pow / r-ele 'reservoir elevation (feet from spd)'
         p-cap 'installed capacity of the power house at r-ele'
         g-cap 'generation capability (kwh per af) at r-ele'
         r-cap 'gross reservoir capacity (maf) at r-ele'/
   pn(n) 'nodes with power house' / mangla-r, tarbela-r /
   v     / 1*27 /;

Table powerchar(n,*,v) 'power generation chractersitics of hrdro stations'
                         1      2      3      4      5      6      7      8      9     10     11     12     13
   tarbela-r.r-ele    1300   1310   1320   1330   1340   1350   1360   1370   1380   1390   1400   1410   1420
   tarbela-r.p-cap     480    552    630    700    780    832    894    942    992   1052   1114   1184   1266
   tarbela-r.g-cap     136    166    183    192    202    209    216    224    233    237    247    257    263
   tarbela-r.r-cap   1.313  1.439  1.571  1.707  1.848  1.995  2.236  2.486  2.745  3.014  3.297  3.631  3.977

   mangla-r .r-ele    1040   1050   1055   1060   1065   1070   1075   1080   1085   1090   1095   1100   1105
   mangla-r .p-cap     384    424    440    464    488    504    528    544    568    584    608    632    648
   mangla-r .g-cap     141    155    164    173    183    192    198    205    209    212    220    228    233
   mangla-r .r-cap   0.417  0.509  0.555  0.601  0.648  0.696  0.744  0.792  0.883  0.973  1.064  1.155  1.271

   +                    14     15     16     17     18     19     20     21     22     23     24     25      26      27
   tarbela-r.r-ele    1430   1440   1450   1460   1470   1480   1490   1500   1510   1520   1530   1540    1550    1550
   tarbela-r.p-cap    1308   1366   1416   1476   1530   1596   1678   1730   1792   1848   1908   1936    1952    1952
   tarbela-r.g-cap     269    281    288    295    303    310    318    336    346    356    356    368     376     376
   tarbela-r.r-cap   4.334  4.705  5.095  5.520  5.960  6.412  6.882  7.369  7.865  8.381  8.917  9.478  10.063  20.000

   mangla-r .r-ele    1110   1115   1120   1130   1135   1140   1145   1155   1160   1165   1170   1180    1202    1202
   mangla-r .p-cap     672    696    712    760    776    800    816    856    880    896    920    960     960     960
   mangla-r .g-cap     237    247    252    263    263    269    275    281    288    295    303    318     356     356
   mangla-r .r-cap   1.386  1.501  1.617  1.933  2.091  2.249  2.453  2.862  3.066  3.316  3.566  4.067   5.364  20.000;

* segment 27 would be used if the
* dam is raised to create additional storage on top of the existing.
* select capacity and adjust the  capacity curve for sediment/dead storage
Parameter rcap(n) 'live capacity of resrvoirs (maf)';
loop(isr, rcap(n) = rrcap(n,isr));

powerchar(pn,"r-cap",v)   = max(0, powerchar(pn,"r-cap",v) - (powerchar(pn,"r-cap","26") - rcap(pn)));
powerchar(pn,"r-cap","1") = 0;

* report on the inflows etc.
Parameter rep7, rep8;
rep7(i,m)        = inflow(i,m);
rep7(i,sea1)     = sum(m$sea1m(sea1,m), inflow(i,m));
rep7("total",m1) = sum(i, rep7(i,m1));
display "system inflows at rim stations (maf)", rep7;

rep8(n,n1,m)    = trib(n,n1,m);
rep8(n,n1,sea1) = sum(m$sea1m(sea1,m), trib(n,n1,m));
display "tributory inflow in (maf)", rep8;

rep8(n,n1,m1)  = 0;
rep8(pn,v,pow) = powerchar(pn,pow,v);
option rep8:3:1:1;
display rcap
        "r-ele  reservoir elevation (feet from spd)"
        "p-cap  installed capacity of the power house at r-ele"
        "g-cap  generation capability (kwh per af) at r-ele"
        "r-cap  live  reservoir capacity (maf) at r-ele ", rep8;

$sTitle Prices
Set p3 / financial, economic, export, import /;

Table prices(ps,cq,p3) '1988  prices'
                     financial  economic  export  import
   87-88.basmati          6.        6.      4.8
   87-88.irri             2.9       2.4     2.3
   87-88.cotton           4.5       6.      4.5
   87-88.gram             3.9       3.8
   87-88.maize            1.6        .6
   87-88.mus+rap          3.75      3.4
   87-88.sc-gur           3.0       3.0
   87-88.sc-mill           .3        .21              .5
   87-88.wheat            2.0       2.2     2.0
   87-88.orchard          4.8       4.3
   87-88.potatoes         2.6       2.6
   87-88.onions           2.9       2.9     2.8
   87-88.chilli          10.       10.
   87-88.cow-milk         5.4       2.46
   87-88.buff-milk        5.4       3.24
   87-88.meat            13.8      14.4 ;

Table finsdwtpr(c,ps,*) 'prices of seed (rs per kg) and water (rs per acre)'
              87-88.seed  87-88.water  87-88.miscc
   basmati           5.           32.          75.
   irri              2.5          32.          75.
   cotton            5.5          34.         500.
   rab-fod           40.          11.
   gram              5.7          16.
   maize             4.           14.
   mus+rap           4.3          22.
   kha-fod           2.5          14.
   sc-gur             .2          64.
   sc-mill            .2          64.
   wheat             2.9          22.
   orchard                        84.
   potatoes          2.6          16.        1650.
   onions            2.9          16.        1020.
   chilli            10.          42.         525.;

Table ecnsdwtpr(c,ps,*) 'prices of seed(rs per kg) and water (rs per acre)'
              87-88.seed  87-88.water  87-88.miscc
   basmati          7.           16.           75.
   irri             3.7          16.           75.
   cotton           6.7          14.4         500.
   rab-fod         36.            6.4
   gram             5.1           8.
   maize            7.            9.6
   mus+rap          5.9          12.
   kha-fod          2.25          6.4
   sc-gur            .43         33.
   sc-mill           .43         33.
   wheat            3.6          10.
   orchard                       64.
   potatoes         2.6          12.         1650.
   onions           2.9          12.         1020.
   chilli          10            32.          525.;

* wages, fertilizer and other input prices
Set
   p1  / nitrogen,  phosphate, protein, twinvt, trinvt, twopc, tropc /
   p11 / financial, economic /;

Table pri1(ps,p11,p1) 'fertilizer tubewell tractor and protein prices'
* fertilizer and protein prices are in rs/kg, twinvt and trinvt are
* annualized cost for a tubewell and tractor (rupees)
* twopc and tropc are cost of tubewell water(rs/acre foot) and
* cost of tractor (rs/tractor hour)
                     nitrogen  phosphate  protein  twinvt  trinvt  twopc  tropc
   87-88.financial        5.8        7.0       9.   10000   25000    225    45.
   87-88.economic         8.3       10.9       9.   10000   25000    170    50.;

Table wageps(ps,p11,m) 'wage rates rs per man hour'
                      jan   feb   mar  apr  may   jun   jul   aug   sep  oct  nov   dec
   87-88.financial   3.    3.    3.      6.  6.  3.    3.    3.    3.     6.   6.  3.
   87-88.economic    2.75  2.75  2.75    5.  5   2.75  2.75  2.75  2.75   5.   5.  2.75;

* miscellaneous parametres
Scalar
   lstd    'standard labor limit (hours per month)'                      / 200      /
   trcap   'tractor capacity in tractor hours per month'                 / 250      /
   twcap   'nameplate capacity of the private tubewell (af per month)'   /  59.5041 /
   ntwucap 'effective capacity of new tubewells (af per month)'
   twefac  'factor to convert wc losses to from private tubewell losses' /   0.5    /
   labfac  'factor to convert wage to the reservation wage'              /   0.5    /;

Parameter twutil(*) 'effective capacity of tubewells (proportion of name plate capacity)' / existing .60, new .75 /;

$sTitle Demand and Consumption Data
Table totprod(z,cq ) "total production 1988 (000's tons)"
* sc-mill is total cane production (including that used for gur)
* sc-gur is refined portion of total cane - same as consumption
           wheat  basmati   irri  maize  sc-mill  sc-gur  cotton  mus+rap   gram  chilli  potatoes  onions
   nwfp    161.8      0.0    0.0   42.1   2473.7    49.5     0.0     14.8   22.4     4.8      29.7    25.2
   pcw    4636.8    182.8  175.7   23.0  10885.2   217.7  3318.4    103.8  132.4    38.7     199.5   191.5
   pmw     747.1      0.0    0.0    0.0   1364.8    27.3   153.6      7.5   50.6     7.1      36.8    35.1
   prw    1720.1    636.0  279.6    5.6   1065.7    21.3    46.9     16.6   43.0    15.2      78.5    75.0
   psw    2327.3    141.2    0.0   27.2   7148.6   143.0   397.0     14.6   36.4    21.6     110.8   106.2
   scwn    830.1      0.0   18.1    0.0   3030.7    60.6   485.5     17.4   16.7     5.9      33.7    27.8
   scws    847.7      0.0  157.3    0.0   4947.9    99.0   437.4      6.8    6.2     7.0      39.9    32.4
   srwn    414.2      0.0  920.3    0.0    289.5     5.8     6.1     27.8   23.1     7.2      41.2    33.6
   srws    112.7      0.0  257.3    0.0   1832.0    36.6    18.9      3.4   10.1     2.2      12.3    10.1;

Table farmcons(z,cq ) "on-farm consumption 1988 (000's tons)"
           wheat  basmati   irri  maize  sc-gur  mus+rap  gram  chilli  potatoes  onions
   nwfp    110.0                   18.9    49.5      6.7  10.1     2.2      13.4    11.3
   pcw    3013.9     27.4   21.1   10.3   217.7     46.7  59.6    17.4      89.8    86.2
   pmw     485.6                           27.3      3.4  22.8     3.2      16.6    15.8
   prw    1118.1     95.4   33.6           21.3      7.5  19.4     6.8      35.3    33.7
   psw    1512.7     21.2          12.3   143.0      6.6  16.4     9.7      49.9    47.8
   scwn    697.3            11.8           60.6      7.8   7.5     2.7      15.2    12.5
   scws    712.1           102.2           99.0      3.1   2.8     3.1      18.0    14.6
   srwn    347.9           171.1            5.8     12.5  10.4     2.9      16.8    13.5
   srws     94.6            27.1           36.6      1.5   4.5     1.3       7.4     6.1;

Table demand(*,cq ) 'market demand by zone (000 tons or million liters)'
* basmati & irri adjusted for expected exports
* sc-mill adjusted for expected imports of refined sugar
           wheat  basmati   irri  maize  sc-mill  cotton  mus+rap  gram  chilli  potatoes  onions  cow-milk  buff-milk
   nwfp     51.8                   23.1   2337.6     0.0      8.1  12.3     2.6      16.3    13.9       167        163
   pcw    1622.9    155.4   49.3         10286.5  3318.4     57.1  72.8    21.3     109.7   105.3       190        267
   pmw     261.5                          1289.8   153.6      4.1  27.8     3.9      20.2    19.3       709       2298
   prw     602.0    290.6   32.2          1007.1              9.1  23.7     8.3      43.2    41.2       260       1591
   psw     814.5    120.2          15.0   6755.5   397.0      8.0  20.0    11.9      61.0    58.4       180       1142
   scwn    132.8             6.3          2864.0   485.5      9.6   9.2     3.3      18.5    15.3       201        374
   scws     66.3            55.0          4675.8   437.4      3.8   3.4     3.8      22.0    17.8       110        216
   srwn    18.03           171.2           273.5             15.3  12.7     3.9      22.7    18.5       187        469
   srws     28.0            27.1          1731.3              1.8   5.5     1.2       6.7     5.5       206        366;

* adujust the demands for cow and buffalo population in the irrigated area
* zone3xxxxx(z,cq, "demand") = demand(z,cq);
Scalar
   cowf 'adjustment factor for cows population in the irrigated areas' / .5 /
   buff 'adjustment factor for buffloes pop.   in the irrigated areas' / .8 /;

demand(z,"cow-milk")  = demand(z,"cow-milk" )*cowf;
demand(z,"buff-milk") = demand(z,"buff-milk")*buff;

Parameter
   elast(cq )   'elasticity of demand for crop and livestock comodities'
                /(basmati,irri)            -.4
                  cotton                   -.9
                  gram                     -.22
                  maize                    -.4
                  mus+rap                  -.35
                  sc-mill                  -.7
                  wheat                    -.23
                 (potatoes,onions,chilli)  -.7
                 (cow-milk,buff-milk)      -.44 /
   growthrd(cq) 'growth rate of reference demand (percent)'
                / basmati  4.5, irri      4.0, cotton  5.0
                  gram     4.7, wheat     4.1, mus+rap 3.4
                  sc-mill  5.5,
                  cow-milk 6.3, buff-milk 6.3, meat    6.6 /;

Table consratio(z,g) 'proportion of consumption by growundwater type'
          fresh
   nwfp    1
   pmw      .85
   pcw      .9
   psw      .83
   prw     1
   scwn     .55
   srwn     .33
   scws     .34
   srws    0   ;

Parameter
   natexp(cq)     'national exports (000 tons)' / basmati 250, irri 1100, cotton 700, onions 10 /
   explimit(z,cq) 'export limits by zone';

Scalar explimitgr 'growth rate of export limits (percent)' / 5 /;

Table exppv(pv,cq) 'provincial exports as proportion of national'
            basmati  irri  cotton  onions
   nwfp                              0.14
   punjab      1.00  0.29    0.84    0.33
   sind              0.71    0.16    0.53;

Table expzo(z,cq) 'zonal exports as proportion of provincial'
          basmati  irri  cotton  onions
   nwfp                            1
   pcw             0.33    1.00    0.52
   prw       1.00  0.67            0.22
   pmw                             0.09
   psw                             0.17
   scwn                    0.74    0.41
   srwn            0.74            0.30
   scws                    0.26    0.22
   srws            0.26            0.08;

growthrd(cq)$(growthrd(cq) = 0) = 3.0;
consratio(z,"saline") = 1 - consratio(z,"fresh");
explimit(z,cq) = natexp(cq)*sum(pv$pvz(pv,z), exppv(pv,cq))*expzo(z,cq);
explimit(z,cq) = sum(is$isr(is),(1 + explimitgr/100)**(ord(is) + 1979 - baseyear)*explimit(z,cq));

* zone3xxxxx(z,cq,"totprod")  = totprod(z,cq);
* zone3xxxxx(z,cq,"farmcons") = farmcons(z,cq);
* zone3xxxxx(z,cq,"elast")    = elast(cq);
* zone3xxxxx(z,cq,"growthrd") = growthrd(cq);
* zone3xxxxx(z,cq,"explimit") = explimit(z,cq);

$sTitle Data Transformation to Zones
Set sr1(dc) / cca, ccap /;

Alias (g,g1);

Parameter
   zwt(z,cnl,sa)    'weighting factor to map rain evap and efficiencies to zones'
   eqevapz(z,m)     'evaporation from the equaifer by acz (feet)'
   subirrz(z,m)     'subirrigation by acz                 (feet)'
   efrz(z,m)        'effective rain  by acz               (feet)'
   resource(z,g,r1) 'endowments by acz and groundwater quality'
   cneff(cnl)       'canal efficiency from canal head to the watercourse head'
   wceff(cnl,m)     'watercourse command delivery efficiency'
   tweff(cnl,m)     'delivery efficiency from private tubewell to the root zone'
   cneffz(z)        'weighted canal delivery efficiency from canal head to watercourse head'
   tweffz(z,m)      'weighted private tubewell delivery efficiency by zone'
   wceffz(z,m)      'weighted water course command delivery efficiency by zone'
   fleffz(z)        'weighted field efficiency by zone'
   canalwz(z,g,m)   'canal water availablility at the canal head (maf)'
   canalwrtz(z,g,m) 'canal water availablility at the root zone (maf)'
   gwtsa(cnl,sa,m)  'government tubewell pumpage by canal and subarea (kaf)'
   gwt1(z,g,m)      'public tubewell pumpage at the root zone (kaf)'
   ratiofs(z,g)     'fresh and saline cca as a proportion off total';

loop(isr,
   resource(z,g,sr1)           = sum((cnl,sa)$(zsa(z,cnl,sa)$gwfg(cnl,sa,g)), comdef(isr,sr1,cnl)*subdef(sa,cnl));
   zwt(z,cnl,sa)$zsa(z,cnl,sa) = comdef(isr,"cca",cnl)*subdef(sa,cnl)/sum(g, resource(z,g,"cca"));
   cneff(cnl)       = comdef(isr,"ceff",cnl);
   wceff(cnl,m)     = sum(wce$wcem(wce,m), comdef(isr,wce,cnl));
   tweff(cnl,m)     = (1 - (1 - wceff(cnl,m)/comdef(isr,"flde",cnl))*twefac)*comdef(isr,"flde",cnl);
   cneffz(z)        = sum((cnl,sa), comdef(isr,"ceff",cnl)*zwt(z,cnl,sa));
   fleffz(z)        = sum((cnl,sa), comdef(isr,"flde",cnl)*zwt(z,cnl,sa));
   canalwrtz(z,g,m) = sum((cnl,sa)$(zsa(z,cnl,sa)$gwfg(cnl,sa,g)), divpost(cnl,m)*subdef(sa,cnl)
                                                                 * comdef(isr,"ceff",cnl)*wceff(cnl,m));
   gwtsa(cnl,sa,m)$carea(cnl,"fresh") = subdef(sa,cnl)$gwf(cnl,sa)
                                      * comdef(isr,"cca",cnl)/carea(cnl,"fresh")*gwt(cnl,m)
);

ratiofs(z,g)      = resource(z,g,"cca")/sum(g1, resource(z,g1,"cca"));
canalwz(z,g,m)    = sum((cnl,sa)$(zsa(z,cnl,sa)$gwfg(cnl,sa,g)), divpost(cnl,m)*subdef(sa,cnl));
eqevapz(z,m)      = sum((cnl,sa), eqevap(cnl,m)*zwt(z,cnl,sa));
subirrz(z,m)      = sum((cnl,sa), subirr(cnl,m)*zwt(z,cnl,sa));
efrz(z,m)         = sum((cnl,sa), efr(cnl,m)*zwt(z,cnl,sa));
tweffz(z,m)       = sum((cnl,sa), tweff(cnl,m)*zwt(z,cnl,sa));
wceffz(z,m)       = sum((cnl,sa), wceff(cnl,m)*zwt(z,cnl,sa));
gwt1(z,"fresh",m) = sum((cnl,sa)$zsa(z,cnl,sa), gwtsa(cnl,sa,m)*wceff(cnl,m));

display wceff, tweff, gwtsa, gwt1;

$onText
*- report water components
Set
   crt / canal, canal-rt, govt-tw, gwt-rt /
   ras / rain, subirr, total /;

Parameter rep5(z,*,*), rep51, rep6;
rep5(z,"rain",m)      = efrz(z,m)*12;
rep5(z,"subirr",m)    = subirrz(z,m)*12;
rep5(z,"total",m)     = sum(ras, rep5(z,ras,m));
rep5(z,ras,sea1)      = sum(m$sea1m(sea1,m), rep5(z,ras,m));

rep51(z,"cnldeleff",m) = cneffz(z)*wceffz(z,m);
rep51(z,"wcdeleff",m)  = wceffz(z,m);
rep51(z,"twdeleff",m)  = tweffz(z,m);

rep6(z,g,"canal",m)          = canalwz(z,g,m);
rep6(z,"total","canal",m)    = sum(g, canalwz(z,g,m));
rep6(z,g,"canal-rt",m)       = canalwrtz(z,g,m);
rep6(z,"total","canal-rt",m) = sum(g, canalwrtz(z,g,m));
rep6(z,"fresh","govt-tw",m)  = sum((cnl,sa)$zsa(z,cnl,sa), gwtsa(cnl,sa,m))/1000;
rep6(z,"total","govt-tw",m)  = rep6(z,"fresh","govt-tw",m);
rep6(z,g,"gwt-rt",m)         = gwt1(z,g,m)/1000;
rep6(z,"total","gwt-rt",m)   = gwt1(z,"fresh",m)/1000;
rep6(z,t1,crt,sea1)          = sum(m$sea1m(sea1,m), rep6(z,t1,crt,m));
display zwt;

display "rain       effective rain(inches) "
        "subirr     water available to the crops by cappilary action from the gw(inches) "
        "total      total water available from rain and subirr(inches) ", rep5;
display "cnldeleff  weighted canal delivery efficiency from canal head to the root zone "
        "wcdeleff   weighted watercourse command  eff. from canal head to the root zone "
        "twdeleff   weighted perivate tubewell eff. from canal head to the root zone ", rep51;
display "canal      canal water available at the canal head  (maf)    "
        "cannal-rt  canal water available at the root zone (maf)      "
        "govt-tw    government tubewell pumpage at the watercourse head(maf)"
        "gwt-rt     government tubewell pumpage at the root zone(maf)", rep6;
 display eqevapz;
$offText

$sTitle Resource Stocks
Set ftt(r1) / farmpop, farmhh, tractors, tubewells, bullocks, cows, buffalos /;

Table res88(r1 ,z) 'available resources 1988'
* farmpop -- thosand workers, farmhh -- thousand households
* bullocks, cows and buffalos are in thousands
* tubewells are 1 cfs tubewells
               nwfp    pmw     pcw    psw    prw  scwn  srwn   scws  srws
   farmpop      371    478    2589   1441   1016   484   589    567   176
   farmhh       174    226    1123    586    513   211   253    275    88
   tractors    7465  12044   86817  45777  44564  8177  4154   8839  2702
   tubewells   2638  12265  109658  37952  49897  9573  4863  10348  3163
   bullocks     151    332    1978    952    368   399   486    444   143
   cows         505    736    2549   1055    590   442   570    630   256
   buffalos     319    487    4146   2911   2279   584   587    557   249;

Table croparea(z,c) "cropped area 1988 (000's acres)"
* sc-mill area includes gur area
          wheat  basmati  irri  maize  sc-mill  cotton  mus+rap  gram  chilli  potatoes  onions  orchard  rab-fod  kha-fod
   nwfp     224        0     0     78      143               52   132       9       8         6       27       47       14
   pcw     6324      396   277     41      722    3820      275   746      55      51        40      300     1174     1332
   pmw     1168       25    29             100     270       27   389      14      13        10       93      158      328
   prw     2141     1189   272      9       79      78       42   233      21      19        15       56      506      374
   psw     2688      391    42     51      495     614       41   218      33      30        24      242      675      698
   scwn    1004        0    19             169     743       63    58      10       9         7       49      131       72
   scws     921        0   196             238     758       25    22      12      11         8      103      137       59
   srwn     590        0   951              24      13      124   100      15      14        11        7      117        4
   srws     101        0   325             111      41       13    38       4       4         3       41       30        7;

Table growthres(r1,z) 'growth rate of farm population tractors and tubwells (percent)'
               (nwfp,pmw,pcw,psw,prw)  (scwn,scws,srwn,srws)
   farmpop
   farmhh
   tractors                     3                      3
   tubewells                    1
   bullocks                     2.35                   2.35
   cows                         1.21                   1.21
   buffalos                     1.21                   1.21;

Parameter
   orcharea(z)   'area under orchards by zone (thousand acres)'
   orchgrowth(z) 'growth rate of orchard area'
                 / (nwfp,pmw,pcw,psw,prw) 6.43
                   (scwn,scws,srwn,srws)  2.591 /;

demand(z,cq)      = demand(z,cq)*sum(is$isr(is), (1 + growthrd(cq)/100)**(ord(is) + 1979 - baseyear));
orcharea(z)       = croparea(z,"orchard")*sum(is$isr(is), (1 + orchgrowth(z)/100)**(ord(is) + 1979 - baseyear));
resource(z,g,ftt) = res88(ftt,z)*ratiofs(z,g)*sum(is$isr(is), (1 + growthres(ftt,z)/100)**(ord(is) + 1979 - baseyear));

ntwucap = twutil("new")*twcap;
resource(z,"fresh","tubewells")$resource(z,"fresh","cca") = sum(g, resource(z,g,"tubewells"));
resource(z,"saline","tubewells") = 0;
resource(z,"fresh","twc")$resource(z,"fresh","cca") = resource(z,"fresh","tubewells")*twcap*twutil("existing")/1000;

Parameter scmillcap(z) 'sugarcane mill capacity (thousand tonns per year)'
                       / nwfp 6937,
                         pmw  5750, pcw  8550, psw  12000, prw  3600
                         scwn 6000, scws 8370, srwn 3800,  srws 3730 /;

option totprod:1, farmcons:1, demand:1, consratio:2;
display resource, totprod, farmcons, consratio, demand, explimit;

Set cnl1(cnl) 'canals excluding nwfp canals';
cnl1(cnl)                   = yes;
cnl1(cnl)$pvcnl("nwfp",cnl) =  no;

Parameter
   postt   'average canal diversions by season'
   protarb 'diversions as proportion of total (punjab and sind) ost tarbela';

postt(cnl,sea)    = sum(m$seam(sea,m), divpost(cnl,m));
postt(pv2,sea)    = sum(cnl$pvcnl(pv2,cnl), postt(cnl,sea));
protarb(cnl1,sea) = 0.999*postt(cnl1,sea)/(postt("punjab",sea) + postt("sind",sea));
protarb(pv2,sea)  = 0.999*postt(pv2,sea) /(postt("punjab",sea) + postt("sind",sea));

option postt:5, protarb:5;
display cnl1, postt, protarb;

$stitle Model Setup
Set
   psr(ps)  'price scenario for the model (financial prices)' / 87-88 /
   psr1(ps) 'price scenario for report (economic prices)'     / 87-88 /
   z1(z)    'zone selection for this run' / nwfp, pmw, pcw, psw, prw, scwn, scws, srwn, srws /

*  total comodities are 18,
*  with endogenous prices = 13, fixed prices = 2, fodder = 2
*  consumption only = 1
   cn(cq)      'comodities  endogenous prices'
               / basmati,  irri,     cotton
                 gram,     maize,    mus+rap
                 sc-mill,  wheat,    potatoes
                 onions,   chilli,
                 cow-milk, buff-milk /
   ccn(cq)     'crop comodities with endogenous prices'
               / basmati, irri,  cotton
                 gram,    maize, mus+rap
                 sc-mill, wheat, potatoes
                 onions,  chilli /
   qn(cq)      'livestock comodities endogenous prices'   / cow-milk, buff-milk /
   ncn(cq)     'crops with fixed prices excluding fodder' / orchard, meat       /
   ce(cq)      'exportable comodities'   / basmati, irri, cotton, onions, wheat /
   cm(cq)      'comodities which could be imported'       / sc-mill             /
   ex(z,g)     'to check fresh or saline area within a zone'
$onExternalOutput
   techc(z,cq) 'comodities by zones';
$offExternalOutput
ex(z1,g)$resource(z1,g,"cca") = yes;
display cq, cn, ccn, qn, ncn, ce, cm, ex, fert;

Table tec(c,t,s,w,z) 'crop technology disabled for 1988 run'
                                                                 scwn  srwn  scws  srws
   wheat.(bullock,semi-mech).la-plant.(heavy,january)               1     1     1     1
   wheat.(bullock,semi-mech).(qk-harv,standard).heavy                     1     1     1
   wheat.(bullock,semi-mech).(qk-harv,standard).january                   1           1
   wheat.(bullock,semi-mech).(qk-harv,standard,la-plant).light                        1;

Scalar
   big   'big number used for artifical production' /   4000 /
   pawat 'big number for artificial water'          / 999999 /
   pafod 'big number for artifical fodder'          /   1000 /;

Parameter
   divnwfp(m)       'monthy diversion to the nwfp zone (maf)'
                    / jan .158, feb .187, mar .270, apr .184
                      may .232, jun .325, jul .333, aug .333
                      sep .332, oct .200, nov .063, dec .133 /
   rval(n)          'value of water stored in the reservoirs'
                    / tarbela-r 1, kalabagh-r .8, mangla-r .6
                      chasma-r .2, chotiari-r .2, a-sea    .1 /
   fsalep(cq)       'financial sale price for crop and livestock comodities (rs per kg or per liter)'
   pp               'financial purchase price of protein                                (rs per kgs)'
   misc(*)          'financial miscellenious prices'
   seedp(c)         'financial seed price                                               (rs per kgs)'
   wage(m)          'financial wage rates                                          (rs per man-hour)'
   miscct(c)        'financial water charges and miscillenious costs                   (rs per acre)'

   esalep(cq)       'economic sale price for crop and livestock comodities  (rs per kg or per liter)'
   epp              'economic price of protein concentrate                              (rs per kgs)'
   emisc(*)         'economic miscellenious prices'
   eseedp(c)        'economic seed price'
   ewage(m)         'economic wage rate                                            (rs per man-hour)'
   emiscct(c)       'economic water charges and miscillenious costs                    (rs per acre)'
   importp(cq)      'import prices for the scenario'
   exportp(cq)      'export prices for the scenario'
   wnr(c,z,t,s,w,m) 'water requirements net of rain';

loop(psr,
   fsalep(cq)  =  prices(psr,cq,"financial");
   pp          =  pri1(psr,"financial","protein");
   misc(p1)    =  pri1(psr,"financial",p1);
   seedp(c)    =  finsdwtpr(c,psr,"seed");
   wage(m)     =  wageps(psr,"financial",m);
   miscct(c)   =  finsdwtpr(c,psr,"water") + finsdwtpr(c,psr,"miscc");
   importp(cq) =  prices(psr,cq,"import");
   exportp(cq) =  prices(psr,cq,"export")
);
loop(psr1,
   esalep(cq)  =  prices(psr1,cq ,"economic");
   epp         =  pri1(psr1,"economic","protein");
   emisc(p1)   =  pri1(psr1,"economic",p1);
   eseedp(c)   =  ecnsdwtpr(c,psr1,"seed");
   ewage(m)    =  wageps(psr1,"economic",m);
   emiscct(c)  =  ecnsdwtpr(c,psr1,"water") + ecnsdwtpr(c,psr1,"miscc");
);

$onText
* economic prices are set to finacial
esalep(cq) = fsalep(cq);
emisc(p1)  = misc(p1);
eseedp(c)  = seedp(c);
epp        = pp;
ewage(m)   = wage(m);
$offText

* if we know that water is positive runs 10 times as fast
* wnr(c,z1,t,s,w,m)$water(c,z1,t,s,w,m) = max(0, (water(c,z1,t,s,w,m) - efrz(z1,m)));
wnr(c,z1,t,s,w,m) = max(0, (water(c,z1,t,s,w,m) - efrz(z1,m)));
* set coton in prw to no
tec("cotton",t,s,w,"prw") = 1;
tec("maize",t,s,w,"prw")  = 1;

tech(z1,c,t,s,w)$tec(c,t,s,w,z1)           =  no;
techc(z1,c)$sum((t,s,w), tech(z1,c,t,s,w)) = yes;
techc(z1,cf) =  no;
techc(z1,q)  = yes;

* display tech;
display techc;

* note: inflows from ravi and sutlej river are set to zero here.
* inflow("ravi",m)   = 0;
* inflow("sutlej",m) = 0;

Scalar
   tolcnl  'allowed deviation from proportional allocation by canal'    / 0.0 /
   tolpr   'allowed deviation from proportional allocation by province' / 0.0 /
   tolnwfp 'nwfp diversion tolerance'                                   / 0.0 /;

Parameter
   beta(cq,z1)  'gradient comodities demand curve'
   alpha(cq,z1) 'demand curve intecept';

Scalar betaf 'beta factor' / .5 /;
beta(cn,z1)$demand(z1,cn) = fsalep(cn)/demand(z1,cn)/elast(cn);
alpha(cn,z1)              = fsalep(cn) - beta(cn,z1)*demand(z1,cn);

* linearization of the demand function.
Set p 'grid points for linearization' / 1*20 /;

Parameter
   pmax(cq,z1)    'maximum price for segments'
   pmin(cq,z1)    'minimum price for segments'
   qmax(cq,z1)    'max national consumption'
   qmin(cq,z1)    'min national consumption'
   incr(cq,z1)    'increment'
   ws(cq,z1,p)    'welfare segments                     (million rupees)'
   rs(cq,z1,p)    'revenue definition                   (million rupees)'
   qs(cq,z1,p)    'quantity definition (thousand tons or million liters)'
   endpr(cq,z1,p) 'price                       (rupees per kgs or liter)';

pmin(cn,z1) = 0.5*fsalep(cn);
pmax(cn,z1) = min(alpha(cn,z1), 2*fsalep(cn));
* pmax(ce,z1) = min(alpha(ce,z1),2*fsalep(ce));
* pmin(ce,z1) = fsalep(ce);

qmin(cn,z1)$beta(cn,z1 ) = (pmax(cn,z1) - alpha(cn,z1))/beta(cn,z1);
qmax(cn,z1)$beta(cn,z1 ) = (pmin(cn,z1) - alpha(cn,z1))/beta(cn,z1);
incr(cn,z1)              = (qmax(cn,z1) - qmin(cn,z1))/(card(p) - 1);

qs(cn,z1,p)    = qmin(cn,z1) + incr(cn,z1)*(ord(p) - 1);
ws(cn,z1,p)    = alpha(cn,z1)*qs(cn,z1,p) + betaf*beta(cn,z1)*sqr(qs(cn,z1,p));
rs(cn,z1,p)    = alpha(cn,z1)*qs(cn,z1,p) + beta(cn,z1)*sqr(qs(cn,z1,p));
endpr(cn,z1,p) = alpha(cn,z1)             + beta(cn,z1)*qs(cn,z1,p);

display pmax, pmin, qmax, qmin,incr, qs, ws, rs, endpr, alpha, beta;

$sTitle Equations and Variables
Variable cps 'consumer plus producers surplus (million rupees)';

Positive Variable
   acost(z,g)         'farm cost in                                                    (million rupees)'
   ppc(z,g,sea)       'purchases of protein concentrates                         (thousand metric tons)'
$onExternalOutput
   x(z,g,c,t,s,w)     'cropped area by technology                                      (thousand acres)'
$offExternalOutput
Positive Variable
   animal(z,g,a)      'production of livestock type a                                       (thousands)'
   prodt(z,g,cq)      'production (crop commodities 000 metric tons livestock comm mill. kgs or liters)'
   proda(z,g,cq )     'artificial supply'
   import(z ,cq )     'import of comodities      (crop comm. 000 m. tons livestock mill. kgs or liters)'
   export(z ,cq )     'export of comodities                                          (000 metric tonns)'
   consump(z,g,cq )   'on farm consumption                                           (000 metric tonns)'
   familyl(z,g,m)     'family labor used                                            (million man hours)'
   hiredl(z,g,m)      'hired labor used                                             (million man hours)'
   itw(z)             'investment in increased private tubewell capacity                (kaf per month)'
   tw(z,m)            'private tubewell water used  by month m                                    (kaf)'
   itr(z,g)           'investment in increased tractor capacity             (000 tractor-hrs per month)'
   ts(z,g,m)          'private tractor services use by month                             (thousand hrs)'
   f(n,n1,m)          'flow to node n from node n1                                                (maf)'
   rcont(n,m)         'end of the month resrvoir contents                                         (maf)'
   canaldiv(cnl,m)    'canal diversion at the canal head                                          (maf)'
   cnldivsea(cnl,sea) 'canal diversion by season                                                  (maf)'
   prsea(pv,sea)      'canal diversion by province (Sind and Punjab)                              (maf)'
   tcdivsea(sea)      'total canal diversion in Sind and Punjab by season                         (maf)'
   wdivrz(z1,g,m)     'surface water diversion at the root zone                                   (kaf)'
   slkland(z,g,m)     'slack land                                                      (thousand acres)'
   slkwater(z,g,m)    'slack water at the root zone                                               (kaf)'
   artfod(z1,g,sea)   'artificial fodder supply  equaivalent of rab-fod                     (000 tonns)'
   artwater(z,g,m)    'water from imaginary source at the root zone                               (kaf)'
   artwaternd(n,m)    'water from imaginary source at nodes                                       (maf)'
   nat(cq,z,p)        'provincial demand linearized'
   natn(cq,z)         'provincial demand non-linear';

Equation
   objz               'objective function for the zone model linear version      (million rupees)'
   objzn              'objective function for the zone model non-linear version  (million rupees)'
   objn               'objective function for the indus model linear version     (million rupees)'
   objnn              'objective function for the indus model non-linear version (million rupees)'
   cost(z,g)          'annual farm cost                                          (million rupees)'
   conv(cq,z)         'convex combination for aggregate consumption'
   demnat(cq,z)       'provincial demand balance linear              (000 tons or million liters)'
   demnatn(cq,z)      'zonal demand balance non-linear               (000 tons or million liters)'
   ccombal(z,g,c)     'commodity balances for crops                                    (000 tons)'
   qcombal(z,g,cq)    'livestock comodity balances                         (000 tons or m liters)'
   consbal(z,g,cq)    'consumption balance                                 (000 tons or m liters)'
   laborc(z,g,m)      'monthly labor constraint                               (million man hours)'
   fodder(z,g,sea)    'seasonal maintenance of fodder supplies                  (000 metric tons)'
   protein(z,g,sea)   'protein requirements of livestock by season              (000 metric tons)'
   grnfdr(z,g,sea)    'green fodder requirements                                (000 metric tons)'
   bdraft(z,g,m)      'bullock draft power constraint                     (million bullock hours)'
   brepco(z,g)        'bullock reproduction constraint'
   bullockc(z1)       'bullock population constraint                               (000 bullocks)'
   tdraft(z,g,m)      'tractor draft power balance                            (000 tractor hours)'
   trcapc(z,m)        'tractor capacity constraint                            (000 tractor hours)'
   twcapc(z,m)        'tubewell capacity constraint                                         (kaf)'
   landc(z,g,m)       'land constraint                                                (000 acres)'
   orchareac(z)       'orchard area constraint                                        (000 acres)'
   scmillc(z)         'sugar cane to mill constraint                                  (000 acres)'
   waterbaln(z,g,m)   'water balance at the root zone                                       (kaf)'
   watalcz(z,g,m)     'surface water by zone                                                (kaf)'
   subirrc(z,g,m)     'subirrigation constraint                                             (kaf)'
   nbal(n,m)          'water balance at a node                                              (maf)'
   watalcsea(cnl,sea) 'water allocations by season                                          (maf)'
   divsea(sea)        'total canal diversions in Sind and Punjab                            (maf)'
   divcnlsea(cnl,sea) 'canal diversion by season                                            (maf)'
   watalcpro(pv,sea)  'water allocation by province                                         (maf)'
   prseaw(pv,sea)     'diversions by province and season                                    (maf)'
   nwfpalc(m)         'water allocations to the nwfp acz                                    (maf)';

objz..
   cps =e= sum(z1, sum(g$ex(z1,g), sum(ncn, fsalep(ncn)*prodt(z1,g,ncn))
                                 - acost(z1,g) - sum(sea, artfod(z1,g,sea))*pafod
                                 - sum(m, artwater(z1,g,m))*pawat
                                 - sum(cq$techc(z1,cq), proda(z1,g,cq)*big)))
        -  sum(z1, sum(cm$techc(z1,cm),     import(z1,cm)*importp(cm)))
        +  sum(z1, sum(ce$techc(z1,ce),     export(z1,ce)*exportp(ce)))
        +  sum(z1, sum((cn,p)$techc(z1,cn), nat(cn,z1,p)*ws(cn,z1 ,p)));

objzn..
   cps =e= sum(z1, sum(g$ex(z1,g), sum(ncn, fsalep(ncn)*prodt(z1,g,ncn))
                                 - acost(z1,g) - sum(sea, artfod(z1,g,sea))*pafod
                                 - sum(m, artwater(z1,g,m))*pawat
                                 - sum(cq$techc(z1,cq), proda(z1,g,cq)*big)))
        -  sum(z1, sum(cm$techc(z1,cm), import(z1,cm)*importp(cm)))
        +  sum(z1, sum(ce$techc(z1,ce), export(z1,ce)*exportp(ce)))
        +  sum(z1, sum(cn$techc(z1,cn), alpha(cn,z1)*natn(cn,z1)
                                      + betaf*beta(cn,z1)*sqr(natn(cn,z1))));

objn..
   cps =e= sum(z1, sum(g$ex(z1,g), sum(ncn, fsalep(ncn)*prodt(z1,g,ncn))
                                 - acost(z1,g) - sum(sea, artfod(z1,g,sea))*pafod
                                 - sum(m, artwater(z1,g,m))*pawat
                                 - sum(cq$techc(z1,cq), proda(z1,g,cq )*big)))
        -  sum(z1, sum(cm$techc(z1,cm),     import(z1,cm)*importp(cm)))
        +  sum(z1, sum(ce$techc(z1,ce),     export(z1,ce)*exportp(ce)))
        +  sum(z1, sum((cn,p)$techc(z1,cn), nat(cn,z1,p)*ws(cn,z1,p)))
        +  sum((n,m), -artwaternd(n,m)*pawat + rval(n)*rcont(n,m)$rcap(n)
                    +  rval("a-sea")*f("a-sea","kotri-b",m));

objnn..
   cps =e= sum(z1, sum(g$ex(z1,g), sum(ncn, fsalep(ncn)*prodt(z1,g,ncn))
                                 - acost(z1,g) - sum(sea, artfod(z1,g,sea))*pafod
                                 - sum(m, artwater(z1,g,m))*pawat
                                 - sum(cq$techc(z1,cq), proda(z1,g,cq )*big)))
        -  sum(z1, sum(cm$techc(z1,cm), import(z1,cm)*importp(cm)))
        +  sum(z1, sum(ce$techc(z1,ce), export(z1,ce)*exportp(ce)))
        +  sum(z1, sum(cn$techc(z1,cn), alpha(cn,z1)*natn(cn,z1)
                                      + betaf*beta(cn,z1)*sqr(natn(cn,z1))))
        +  sum((n,m), -artwaternd(n,m)*pawat + rval(n)*rcont(n,m)$rcap(n)
                    +  rval("a-sea")*f("a-sea","kotri-b",m));

cost(z1,g)$ex(z1,g)..
   acost(z1,g) =e= (sum((c,t,s,w)$tech(z1,c,t,s,w), (sum(p2, fert(p2,c,z1)*misc(p2))
                +   miscct(c) + seedp(c)*sylds(c,z1,t,s,w,"seed"))*x(z1,g,c,t,s,w))
                +   sum(m, misc("twopc")*tw(z1,m)$gf(g) + misc("tropc")*ts(z1,g,m))
                +   misc("twinvt")*itw(z1)$gf(g) + misc("trinvt")*itr(z1,g)
                +   sum(a, iolive(a,z1,"fix-cost")*animal(z1,g,a)))/1000
                +   sum(sea, pp*ppc(z1,g,sea))
                +   sum(m, (familyl(z1,g,m)*labfac + hiredl(z1,g,m))*wage(m));

conv(cn,z1)$techc(z1,cn)..
   sum(p, nat(cn,z1,p)) =l= 1;

demnat(cq,z1)$techc(z1,cq)..
       sum(g$ex(z1,g), prodt(z1,g,cq) - consump(z1,g,cq)$cc(cq) + proda(z1,g,cq))
    -  export(z1,cq)$ce(cq) + import(z1,cq)$cm(cq)
   =g= sum(p, nat(cq,z1,p)*qs(cq,z1 ,p))$cn(cq);

demnatn(cq,z1)$techc(z1,cq)..
       sum(g$ex(z1,g), prodt(z1,g,cq) - consump(z1,g,cq)$cc(cq) + proda(z1,g,cq))
    -  export(z1,cq)$ce(cq) + import(z1,cq)$cm(cq)
   =g= natn(cq,z1)$cn(cq);

ccombal(z1,g,c)$(cnf(c)$ex(z1,g))..
   sum((t,s,w)$tech(z1,c,t,s,w), yield(c,t,s,w,z1)*x(z1,g,c,t,s,w)) =e= prodt(z1,g,c);

qcombal(z1,g,q)$ex(z1,g)..
   sum(a, iolive(a,z1,q)*animal(z1,g,a))/1000 =e= prodt(z1,g,q);

consbal(z1,g,cc)$(ex(z1,g)$techc(z1,cc))..
   prodt(z1,g,cc) + proda(z1,g,cc) =g= consump(z1,g,cc);

laborc(z1,g,m)$ex(z1,g)..
      (sum((c,t,s,w)$tech(z1,c,t,s,w), labor(c,z1,t,s,w,m)*x(z1,g,c,t,s,w))
    +  sum(a, iolive(a,z1,"labor")*animal(z1,g,a)))/1000
   =l= familyl(z1,g,m) + hiredl(z1,g,m);

fodder(z1,g,sea)$ex(z1,g)..
       sum(a, iolive(a,z1,"tdn")*animal(z1,g,a))
   =l= sum((c,t,s,w)$tech(z1,c,t,s,w),
           (yield(c,t,s,w,z1)*sylds(c,z1,t,s,w,"straw-yld")*sconv("tdn",sea,c) + weedy(z1,sea,c)*
            sconv("tdn","rabi","rab-fod"))*x(z1,g,c,t,s,w))
    +  sum(m$seam(sea,m), slkland(z1,g,m))*graz(z1,sea)*sconv("tdn","rabi","rab-fod")
    +  artfod(z1,g,sea)*sconv("tdn","rabi","rab-fod");

protein(z1,g,sea)$ex(z1,g)..
       sum(a, iolive(a,z1,"dp")*animal(z1,g,a))
   =l= ppc(z1,g,sea) + sum((c,t,s,w)$tech(z1,c,t,s,w), (yield(c,t,s,w,z1)*sylds(c,z1,t,s,w,"straw-yld")
                           *sconv("dp",sea,c) + weedy(z1,sea,c)*sconv("dp","rabi","rab-fod"))*x(z1,g,c,t,s,w))
    +  sum(m$seam(sea,m), slkland(z1,g,m))*graz(z1,sea)*sconv("dp","rabi","rab-fod")
    +  artfod(z1,g,sea)*sconv("dp","rabi","rab-fod");

grnfdr(z1,g,sea)$ex(z1,g)..
       gr*sum(a, iolive(a,z1,"tdn")*animal(z1,g,a))
   =l= sum((cf,t,s,w)$tech(z1,cf,t,s,w), yield(cf,t,s,w,z1)*sconv("tdn",sea,cf)*x(z1,g,cf,t,s,w))
    +  sum((c,t,s,w)$tech(z1,c,t,s,w), weedy(z1,sea,c)*sconv("tdn","rabi","rab-fod")*x(z1,g,c,t,s,w))
    +  artfod(z1,g,sea)*sconv("tdn","rabi","rab-fod");

bdraft(z1,g,m)$ex(z1,g)..
       sum((c,t,s,w)$tech(z1,c,t,s,w), bullock(c,z1,t,s,w,m)*x(z1,g,c,t,s,w))/1000
   =l= bp(m)*animal(z1,g,"bullock")/1000;

brepco(z1,g)$ex(z1,g)..
   animal(z1,g,"bullock") =l= repco*animal(z1,g,"cow");

bullockc(z1)..
   sum(g$ex(z1,g), animal(z1,g,"bullock")) =l= res88("bullocks",z1);

tdraft(z1,g,m)$ex(z1,g)..
   sum((c,t,s,w)$tech(z1,c,t,s,w), tractor(c,z1,t,s,w,m)*x(z1,g,c,t,s,w)) =e= ts(z1,g,m);

trcapc(z1,m)..
   sum(g$ex(z1,g), ts(z1,g,m)) =l= sum(g$ex(z1,g), (resource(z1,g,"tractors")/1000 + itr(z1,g))*trcap);

twcapc(z1,m)$ex(z1,"fresh")..
   tw(z1,m) =l= resource(z1,"fresh","twc") + ntwucap*itw(z1);

landc(z1,g,m)$ex(z1,g)..
       sum((c,t,s,w)$tech(z1,c,t,s,w), land(c,z1,t,s,w,m)*x(z1,g,c,t,s,w)) + slkland(z1,g,m)
   =e= resource(z1,g,"cca")*1000;

orchareac(z1)..
   sum((g,t,s,w)$ex(z1,g), x(z1,g,"orchard",t,s,w)$tech(z1,"orchard",t,s,w)) =l= orcharea(z1);

scmillc(z1)..
   sum(g$ex(z1,g), prodt(z1,g,"sc-mill")) =l= scmillcap(z1);

waterbaln(z1,g,m)$ex(z1,g)..
       sum((c,t,s,w)$tech(z1,c,t,s,w), max((wnr(c,z1,t,s,w,m)-subirrz(z1,m)*land(c,z1,t,s,w,m)), 0.0)*x(z1,g,c,t,s,w))
    +  slkwater(z1,g,m)
   =e= tweffz(z1,m)*tw(z1,m)$gf(g) + gwt1(z1,g,m) + artwater(z1,g,m) + wdivrz(z1,g,m);

watalcz(z1,g,m)$ex(z1,g)..
   wdivrz(z1,g,m) =e= sum((cnl,sa)$(zsa(z1,cnl,sa)$gwfg(cnl,sa,g)), cneff(cnl)*wceff(cnl,m)*canaldiv(cnl,m)*subdef(sa,cnl)*1000);

divcnlsea(cnl,sea)..
   cnldivsea(cnl,sea) =e= sum(m$seam(sea,m), canaldiv(cnl,m));

prseaw(pv2,sea)..
   prsea(pv2,sea) =e= sum(cnl1$pvcnl(pv2,cnl1), cnldivsea(cnl1,sea));

divsea(sea)..
   tcdivsea(sea)  =e= sum(pv2, prsea(pv2,sea));

watalcsea(cnl1,sea)..
   protarb(cnl1,sea)*(1 - tolcnl)*tcdivsea(sea) =l= cnldivsea(cnl1,sea);

watalcpro(pv2,sea)..
   protarb(pv2,sea)*(1 - tolpr)*tcdivsea(sea) =l= prsea(pv2,sea);

nwfpalc(m)..
   sum(cnl$pvcnl("nwfp",cnl), canaldiv(cnl,m)) =g= (1 - tolnwfp)*divnwfp(m);

subirrc(z1,g,m)$(ex(z1,g)$gs(g))..
   wdivrz(z1,g,m) =g= (1 - subirrfac(z1))*sum((c,t,s,w)$tech(z1,c,t,s,w), wnr(c,z1,t,s,w,m)*x(z1,g,c,t,s,w));

nbal(n,m)$nb(n)..
       sum(i$ni(n,i), inflow(i,m))
    +  sum(n1, rivercd(n,"d")*trib(n1,n,m) + rivercd(n,"c")*trib(n1,n,m--1))
    +  sum(n1$nn(n,n1), f(n,n1,m)*lceff(n1,n)$lceff(n1,n) + (riverb(n,n1)*f(n,n1,m)
                      + rivercd(n,"c")*f(n,n1,m--1))$(lceff(n1,n) = 0))
    -  sum(n1$nn(n1,n), f(n1,n,m)) + (rcont(n,m--1) - rcont(n,m) - revapl(n,m)/1000)$rcap(n)
    -  sum(cnl$nc(n,cnl), canaldiv(cnl,m)) + artwaternd(n,m)
   =e= 0;

* bounds for the network link canal capacities.
artwaternd.lo(n,m) = 0;
f.up(n,n1,m)       = inf;
f.up(n,n1,m)$( ncap(n1,n) <> 0) = ncap(n1,n);

* bounds on family labor
familyl.up(z1,g,m) = resource(z1,g,"farmpop")*lstd/1000;

* consumption bounds
consump.fx(z1,g,cq) = farmcons(z1,cq)*consratio(z1,g);
export.up(z1,ce)    = explimit(z1,ce);
itr.fx(z1,g) = 0;
itw.fx(z1)   = 0;

Model
   wsisz  'agroclimatic zones model linear objective'
          / objz,      cost,      conv,      demnat,    ccombal,   qcombal
            consbal,   laborc,    fodder,    protein,   grnfdr,    bdraft
            brepco,    tdraft,    trcapc,    twcapc,    landc,     orchareac
            scmillc,   waterbaln, watalcz,   subirrc                         /
   wsiszn 'agroclimatic zones model non-linear objective'
          / objzn,     cost,                 demnatn,   ccombal,   qcombal
            consbal,   laborc,    fodder,    protein,   grnfdr,    bdraft
            brepco,    tdraft,    trcapc,    twcapc,    landc,     orchareac
            scmillc,   waterbaln, watalcz,   subirrc                         /
   wsisn  'ibmr model with water network linear'
          / objn,      cost,      conv,      demnat,    ccombal,   qcombal
            consbal,   laborc,    fodder,    protein,   grnfdr,    bdraft
            brepco,    tdraft,    trcapc,    twcapc,    landc,     orchareac
                       waterbaln, watalcz,   subirrc,   divcnlsea, prseaw,
            divsea,    watalcsea, watalcpro, nwfpalc,   nbal                 /
   wsisnn 'ibmr model with water network non-linear'
          / objnn,     cost,                 demnatn,   ccombal,   qcombal
            consbal,   laborc,    fodder,    protein,   grnfdr,    bdraft
            brepco,    tdraft,    trcapc,    twcapc,    landc,     orchareac
                       waterbaln, watalcz,   subirrc,   divcnlsea, prseaw
            divsea,    watalcsea, watalcpro, nwfpalc,   nbal                 /;

* irrigation canal capicity bounds
canaldiv.up(cnl,m) = sum(isr, comdef(isr,"ccap",cnl));
canaldiv.lo(cnl,m) = divpost(cnl,m);

* reservoir operating rule bounds
rcont.lo(n,m) = rulelo(n,m)*rcap(n)/100;
rcont.up(n,m) = ruleup(n,m)*rcap(n)/100;

tolnwfp = 1;
* river flow tests
trib("chasma-r","taunsa-b",m)    = 0;
trib("tarbela-r","kalabagh-r",m) = 0;
inflow("haro",m) = 0;
inflow("soan",m) = 0;

option limRow = 0, limCol = 0;

solve wsisn maximizing cps using lp;
