$title Stock Selection Optimization
* Optimization model to pick a small subset of the stocks together with
* some weights, such that this portfolio has a similar behavior to our
* overall Dow Jones index.

Set       date                 'date'
          symbol               'stock symbol';
$onExternalInput
Parameter price(date<,symbol<) 'Price';
Scalar    maxstock             'maximum number of stocks to select'  /  2 /
          trainingdays         'number of days for training'         / 99 /;
Set test12345(*, *) 'Multi-dim test set' / bla.blub, bli.blu /;
$setNames "%gams.input%" fp fn fe
$if not set fileName $set fileName %fp%dowjones2016.csv
$call.checkErrorLevel csv2gdx "%fileName%" output=stockdata.gdx ValueDim=0 id=price Index="(1,2)" Value=3 UseHeader=y
$gdxin stockdata
$load price

Singleton Set solver / CPLEX /;
Singleton Set clearValueSet / asd /;
$offExternalInput

Alias (d,date), (s,symbol);
Parameter
    avgprice(symbol)          'average price of stock'
    weight(symbol)            'weight of stock'
    contribution(date,symbol) 'contribution of stock on date'
    index(date)               'Dow Jones index';

Parameter
    fund(date)                'Index fund report parameter'
    error(date)               'Absolute error';

Set td(date)    'training days'
    ntd(date)   'none-training days';

* input validataion
set error01(date, symbol);

error01(date, symbol) = price(date, symbol) < 0;

file log / miro.log /;
put log '------------------------------------'/;
put log '        Data validation'/;
put log '------------------------------------'/;
if(card(error01),
  put log 'price:: No negative prices allowed!'/;
  loop(error01(date, symbol),
      put log / ' Symbol ' symbol.tl:4 ' has negative price at the date: ' date.tl:0;
    );
  abort "Data errors detected."
);
putclose log;

avgprice(s)       = sum(d, price(d,s))/card(d);
weight(symbol)    = avgprice(symbol)/sum(s, avgprice(s));
contribution(d,s) = weight(s)*price(d,s);
index(d)          = sum(s, contribution(d,s));

Variable
    p(symbol)   'is stock included?'
    w(symbol)   'what part of the portfolio'
    slpos(date) 'positive slack'
    slneg(date) 'negative slack'
    obj         'objective';

Positive variables w, slpos, slneg;
Binary variable p;

Equation
    deffit(date)    'fit to Dow Jones index'
    defpick(symbol) 'can only use stock if picked'
    defnumstock     'few stocks allowed'
    defobj          'absolute violation (L1 norm) from index';

deffit(td)..  sum(s, price(td,s)*w(s)) =e= index(td) + slpos(td) - slneg(td);

defpick(s)..  w(s) =l= p(s);

defnumstock.. sum(s, p(s)) =l= maxstock;

defobj..      obj =e= sum(td, slpos(td) + slneg(td));

Model pickStock /all/;

option optCR=0.01;

td(d) = ord(d)<=trainingdays;
ntd(d) = not td(d);

$ifthen.test %sleep%=="true"
$call sleep 2
$exit
$else.test
solve pickStock min obj using mip;
$endif.test

fund(d)  = sum(s, price(d, s)*w.l(s));
error(d) = abs(index(d)-fund(d));

Set fHdr      'fund header'            / dj 'dow jones','index fund'  /
    errHdr    'stock symbol header'    / 'absolute error train', 'absolute error test' /;

$onExternalOutput
Scalar error_train                     'Absolute error in entire training phase'
       error_test                      'Absolute error in entire testing phase'
       error_ratio                     'Ratio between error test and error train'
Parameter
       stock_weight(symbol)            'weight'
       dowVSindex(date,fHdr)           'dow jones vs. index fund'
       abserror(date,errHdr)           'absolute error'
table dowVSindex;
table abserror;
Singleton Set
firstDayTraining(date)   'first date of training period'
lastDayTraining(date)    'last date of training period' ;
$offExternalOutput

stock_weight(s)                        = w.l(s);
dowVSindex(d,'dj')                     = index(d);
dowVSindex(d,'index fund')             = fund(d);
abserror(td, 'absolute error train')   = error(td);
abserror(ntd,'absolute error test')    = error(ntd);
lastDayTraining(td)                    = td.pos=card(td);
firstDayTraining(td)                   = td.pos=1;
error_train                            = obj.l;
error_test                             = sum(ntd, error(ntd));
if(error_train > 0,
   error_ratio = error_test/error_train;
else
   error_ratio = inf;);

* parameter including all stocks and dow jones index
$onExternalOutput
Parameter priceMerge(date,*) 'Price (stocks & dow jones)';
$offExternalOutput
priceMerge(d,symbol)        = price(d,symbol);
priceMerge(d,'DowJones')    = index(d);


Set i
    j
;
Set scheduleHdr 'schedule header' / 'lngP', 'latP', 'lngM', 'latM', 'cap', 'demand', 'quantities' /;
$onExternalOutput
Table schedule(i<, j<,scheduleHdr) 'shipment quantities in cases'
$onDelim
,,lngP,latP,lngM,latM,cap,demand,quantities
Seattle,New-york,-122.335,47.608,-73.9352,40.7306,350,325,50
Seattle,Chicago,-122.335,47.608,-87.6232,41.8818,350,300,300
Seattle,Topeka,-122.335,47.608,-95.6953,39.0562,350,275,
San-Diego,New-york,-117.161,32.7157,-73.9352,40.7306,600,325,275
San-Diego,Chicago,-117.161,32.7157,-87.6232,41.8818,600,300,
San-Diego,Topeka,-117.161,32.7157,-95.6953,39.0562,600,275,275
$offDelim
;
Table mapNoGroup(i, j,scheduleHdr) 'shipment quantities in cases';
$offExternalOutput
mapNoGroup(i, j,scheduleHdr) = schedule(i, j,scheduleHdr);

Set
   id       'gannt_id'      / 1, 2, 3, 4, 5, 6 /
   start    'gannt_start'   / 2016-01-04, 2016-01-05, 2016-01-06, 2016-01-07, 2016-01-08, 2016-01-09 /
   end      'gannt_end'     / 2016-01-05, 2016-01-06, 2016-01-07, 2016-01-08, 2016-01-09, 2016-01-10 /
   content  'gannt_content' / test1, test2, test3, test4, test5, test6 /
   group    'gantt_group'   / a, b /
   ;
$onExternalOutput
Parameter
   gantt(id, start, end, content, group) 'asdasd' ;
$offExternalOutput
*gantt(id, start, end, content, group)$(ord(id) = ord(start) and (ord(id) = ord(end)) and (ord(id) = ord(content))) = 1;
gantt('1', '2016-01-04', '2016-01-05', 'test1', 'a') = 1;
gantt('2', '2016-01-05', '2016-01-06', 'test2', 'a') = 1;
gantt('3', '2016-01-06', '2016-01-07', 'test3', 'a') = 1;
gantt('4', '2016-01-07', '2016-01-08', 'test4', 'b') = 1;
gantt('5', '2016-01-08', '2016-01-09', 'test5', 'b') = 1;
gantt('6', '2016-01-09', '2016-01-10', 'test6', 'b') = 1;


set c
    rch 'crop report header' /
              planted    'crop planted [acres]'
              seedcost   'seed cost [$]'
              yield      'crop yield [tons]'
              sold       'crop sold [tons]'
              sales      'crop revenue [$]'
              purchased  'crop purchased [tons]'
              pcost      'purchase cost [$]' /;

$onExternalOutput
Table
    repc(c<,rch)          'crop report'
$onDelim
,yield,planted,seedcost,sold,sales,purchased,pcost
wheat,425,170,25500,225,38250,,
corn,240,80,18400,16,2400,16,3360
'sugar beets',5000,250,65000,5000,180000,,
$offDelim
;

$offExternalOutput

sets
  phdr 'for pressure output'   / 'x','pressure', 'thickness' /
  J0      / 0, 1 * 100 /
;
$onExternalOutput
table pressureThickness(J0,phdr)
$onDelim
,x,pressure,thickness
0,-3,EPS,9.41112
1,-2.95,0.0021684,9.13811
2,-2.9,0.00447091,8.8651
3,-2.85,0.00691739,8.59772
4,-2.8,0.00951858,8.33595
5,-2.75,0.0122861,8.07979
6,-2.7,0.0152325,7.82925
7,-2.65,0.0183716,7.58431
8,-2.6,0.021718,7.34501
9,-2.55,0.0252879,7.11134
10,-2.5,0.0290988,6.88331
11,-2.45,0.0331695,6.66095
12,-2.4,0.0375205,6.44426
13,-2.35,0.0421742,6.23326
14,-2.3,0.0471545,6.02797
15,-2.25,0.0524875,5.82839
16,-2.2,0.0582016,5.63455
17,-2.15,0.0643272,5.44647
18,-2.1,0.0708973,5.26416
19,-2.05,0.0779474,5.08763
20,-2,0.0855158,4.91691
21,-1.95,0.0936436,4.75201
22,-1.9,0.102375,4.59294
23,-1.85,0.111756,4.43972
24,-1.8,0.121839,4.29236
25,-1.75,0.132675,4.15087
26,-1.7,0.14432,4.01526
27,-1.65,0.156835,3.88553
28,-1.6,0.170279,3.76169
29,-1.55,0.184716,3.64373
30,-1.5,0.200212,3.53165
31,-1.45,0.216832,3.42542
32,-1.4,0.234641,3.32504
33,-1.35,0.253703,3.23046
34,-1.3,0.274079,3.14165
35,-1.25,0.295824,3.05855
36,-1.2,0.318988,2.98112
37,-1.15,0.343608,2.90926
38,-1.1,0.36971,2.84289
39,-1.05,0.397302,2.78189
40,-1,0.426373,2.72615
41,-0.95,0.456886,2.67551
42,-0.9,0.488777,2.62979
43,-0.85,0.52195,2.58882
44,-0.8,0.556275,2.55237
45,-0.75,0.591588,2.52022
46,-0.7,0.627693,2.4921
47,-0.65,0.664371,2.46773
48,-0.6,0.701392,2.44683
49,-0.55,0.73853,2.42909
50,-0.5,0.775592,2.41421
51,-0.45,0.812438,2.40185
52,-0.4,0.849004,2.3917
53,-0.35,0.885303,2.38341
54,-0.3,0.921384,2.37665
55,-0.25,0.95719,2.37101
56,-0.2,0.99224,2.36602
57,-0.15,1.02498,2.36109
58,-0.1,1.05181,2.35549
59,-0.05,1.06639,2.34844
60,,1.06152,2.33945
61,0.05,1.03512,2.32905
62,0.1,0.99569,2.3201
63,0.15,0.964413,2.32158
64,0.2,2.93635,2.35886
65,0.25,0.765488,2.31373
66,0.3,0.676998,2.18986
67,0.35,0.576924,2.10807
68,0.4,0.485623,2.04364
69,0.45,0.406137,1.99035
70,0.5,0.337611,1.94686
71,0.55,0.278599,1.91271
72,0.6,0.227791,1.88768
73,0.65,0.184113,1.87165
74,0.7,0.146705,1.86449
75,0.75,0.11486,1.86609
76,0.8,0.0879859,1.87636
77,0.85,0.0655726,1.89516
78,0.9,0.0471665,1.92235
79,0.95,0.0323584,1.9578
80,1,0.0207734,2.00134
81,1.05,0.0120663,2.0528
82,1.1,0.00591784,2.11201
83,1.15,0.00203378,2.17877
84,1.2,0.000143622,2.25287
85,1.25,,2.33411
86,1.3,,2.42221
87,1.35,,2.51692
88,1.4,,2.61807
89,1.45,,2.72551
90,1.5,,2.83913
91,1.55,,2.95884
92,1.6,,3.08456
93,1.65,,3.2162
94,1.7,,3.35371
95,1.75,,3.49704
96,1.8,,3.64612
97,1.85,,3.80092
98,1.9,,3.96139
99,1.95,,4.1275
100,2,,4.29921
$offDelim
;
$offExternalOutput

set	ind	    Individuals /Alice, Bob/
	time 	Time points /0*200/
	xy	    Cartesian coordinates /x,y/;
$onExternalOutput
table hovercraft(ind,time,xy)
$onDelim
,,x,y
Alice,1,,0.00329559
Alice,11,0.00195602,0.035246
Alice,21,0.00824485,0.0649351
Alice,31,0.0188407,0.0923143
Alice,41,0.0337107,0.117323
Alice,51,0.0528119,0.139887
Alice,61,0.0760857,0.159906
Alice,71,0.103448,0.177248
Alice,81,0.134768,0.191725
Alice,91,0.16982,0.203052
Alice,101,0.208148,0.21077
Alice,111,0.248577,0.214158
Alice,121,0.28821,0.212993
Alice,131,0.324005,0.209071
Alice,141,0.355032,0.204221
Alice,151,0.381179,0.199354
Alice,161,0.402462,0.194945
Alice,171,0.418914,0.191276
Alice,181,0.430568,0.18853
Alice,191,0.43745,0.186838
Bob,1,0.503296,
Bob,11,0.534188,0.000759772
Bob,21,0.560491,0.00320224
Bob,31,0.582202,0.00731723
Bob,41,0.599316,0.0130925
Bob,51,0.611828,0.0205133
Bob,61,0.619732,0.0295609
Bob,71,0.623018,0.0402114
Bob,81,0.621677,0.0524331
Bob,91,0.615693,0.0661815
Bob,101,0.605044,0.0813898
Bob,111,0.589699,0.0979467
Bob,121,0.569599,0.115627
Bob,131,0.544671,0.133698
Bob,141,0.51758,0.148989
Bob,151,0.493797,0.160921
Bob,161,0.474195,0.17033
Bob,171,0.458923,0.177472
Bob,181,0.448042,0.182471
Bob,191,0.441588,0.185397
$offDelim
;
$offExternalOutput

Set
  latitude,
  longitude
;
$onExternalInput
Set mapTest(j,latitude<, longitude<) 'Market location information'
 / 'New-York'.'40.730610'.'-73.935242',
   'Chicago'.'41.881832'.'-87.623177',
   'Topeka'.'39.056198'.'-95.695312' /;
$offExternalInput
