$title Stock Selection Optimization
* Optimization model to pick a small subset of the stocks together with
* some weights, such that this portfolio has a similar behavior to our
* overall Dow Jones index.

Set date   'date'
    symbol 'stock symbol';
    
$onExternalInput
Parameter price(date<,symbol<) 'Price';

Scalar maxstock        'maximum number of stocks to select'  / 2  /
       trainingdays    'number of days for training'  / 99  /;
$offExternalInput

$setNames "%gams.input%" fp fn fe
$if not set fileName $set fileName %fp%dowjones2016.csv
$if not exist "%fileName%" $abort CSV file with stock prices missing
$call csv2gdx "%fileName%" output=stockdata.gdx ValueDim=0 id=price Index="(1,2)" Value=3 UseHeader=y
$if errorlevel 1 $abort problems reading CSV data
$gdxin stockdata
$load price

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
    
avgprice(s)       = sum(d, price(d,s))/card(d);
weight(symbol)    = avgprice(symbol)/sum(s, avgprice(s));
contribution(d,s) = weight(s)*price(d,s);
index(d)          = sum(s, contribution(d,s));

Variable
    p(symbol)   'is stock included?'
    w(symbol)   'what part of the portfolio'
    slpos(date) 'positive slack'
    slneg(date) 'negative slack'
    obj         'UIOutput: objective';

Positive variables w, slpos, slneg;
Binary variable p;

Equation
    deffit(date)    'UIOutput: fit to Dow Jones index'
    defpick(symbol) 'can only use stock if picked'
    defnumstock     'few stocks allowed'
    defobj          'absolute violation (L1 norm) from index';

deffit(td)..  sum(s, price(td,s)*w(s)) =e= index(td) + slpos(td) - slneg(td);

defpick(s)..  w(s) =l= p(s);

defnumstock.. sum(s, p(s)) =l= maxstock;

defobj..      obj =e= sum(td, slpos(td) + slneg(td));

Model pickStock /all/;

option optCR=0.01, resLim=6;

td(d) = ord(d)<=trainingdays;
ntd(d) = not td(d);

solve pickStock min obj using mip;

fund(d)  = sum(s, price(d, s)*w.l(s));
error(d) = abs(index(d)-fund(d));

Set fHdr      'fund header'            / dj 'dow jones','index fund'  /
    errHdr    'stock symbol header'    / 'absolute error train', 'absolute error test' /;
    
$onExternalOutput
Parameter
    partOfPortfolio(symbol)            'what part of the portfolio'   
    dowVSindex(date,fHdr)              'dow jones vs. index fund [MIRO:pivot]'     
    abserror(date,errHdr)              'absolute error [MIRO:pivot]'               
Singleton Set lastDayTraining(date)    'last date of training period [MIRO:hidden]' ;
$offExternalOutput

partOfPortfolio(s)                   = w.l(s);
dowVSindex(d,'dj')                   = index(d);
dowVSindex(d,'index fund')           = fund(d);
abserror(td, 'absolute error train') = error(td);
abserror(ntd,'absolute error test')  = error(ntd);
lastDayTraining(td)                  = td.pos=card(td);

$libinclude miro
