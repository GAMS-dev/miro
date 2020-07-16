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
$setNames "%gams.input%" fp fn fe
$if not set fileName $set fileName %fp%dowjones2016.csv
$call.checkErrorLevel csv2gdx "%fileName%" output=stockdata.gdx ValueDim=0 id=price Index="(1,2)" Value=3 UseHeader=y
$gdxin stockdata
$load price

Singleton Set solver / CPLEX /;
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

solve pickStock min obj using mip;

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
$offExternalOutput


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