$title Stock Selection Optimization
* Optimization model to pick a small subset of the stocks together with
* some weights, such that this portfolio has a similar behavior to our
* overall Dow Jones index.

Set       date                 'date'
          symbol               'stock symbol';
$onExternalInput
Parameter price(date<,symbol<) 'Price';
Scalar    maxstock             'maximum number of stocks to select'  /  8 /
          trainingdays         'number of days for training'         / 102 /;
$setNames "%gams.input%" fp fn fe
$if not set fileName $set fileName %fp%dowjones2016.csv
$call.checkErrorLevel csv2gdx "%fileName%" output=stockdata.gdx ValueDim=0 id=price Index="(1,2)" Value=3 UseHeader=y
$gdxin stockdata
$load price
singleton set selected_symbol(symbol)'selected stock symbol' /INTC/;
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
