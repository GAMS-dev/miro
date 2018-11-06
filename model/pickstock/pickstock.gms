$title Stock Selection Optimization
* Optimization model to pick a small subset of the stocks together with
* some weights, such that this portfolio has a similar behavior to our
* overall Dow Jones index.

$onExternalInput
Set date   'date'
    symbol 'stockSymbol';

Parameter price(date,symbol) 'Price ### {"readonly":true}';

Scalar maxstock        'maximum number of stocks to select ### { "slider":{"min":1, "max":"card(price$stocksymbol)", "default":5,  "step":1 }}'  / 2  /
       trainingdays    'number of days for training        ### { "slider":{"min":1, "max":"card(price$date)", "default":99, "step":1 }}'  / 99  /;
$offExternalInput

$if not set fileName $set fileName dowjones2016.csv
$ifthen exist "%gams.wdir%%fileName%"
$  call csv2gdx "%gams.wdir%%fileName%" output=stockdata.gdx ValueDim=0 id=price Index=(1,2) Value=3 UseHeader=y
$  if errorlevel 1 $abort problems reading CSV data
$  gdxin stockdata
$  load date<price.dim1 symbol<price.dim2 price
$  gdxin
$  hiddencall rm -f stockdata.gdx
$else
Set date / system.empty /, symbol /system.empty /;
Parameter price(date,symbol)  / system.empty.system.empty 0 /;
$endif
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

option optCR=0.01, resLim=6;

td(d) = ord(d)<=trainingdays;
ntd(d) = not td(d);

solve pickStock min obj using mip;

fund(d)  = sum(s, price(d, s)*w.l(s));
error(d) = abs(index(d)-fund(d));

$onExternalOutput
Set fHdr      'fund header'            / 'dow jones','index fund'  /
    errHdr    'stock symbol header'    / 'absolute error train', 'absolute error test' /;
Parameter
    partOfPortfolio(symbol)            'what part of the portfolio'   
    dowVSindex(date,fHdr)              'dow jones vs. index fund'     
    abserror(date,errHdr)              'absolute error'               
Singleton Set lastDayTraining(date)    'last date of training period ### vertical marker in chart' ;
$offExternalOutput

partOfPortfolio(s)                   = w.l(s);
dowVSindex(d,'dow jones')            = index(d);
dowVSindex(d,'index fund')           = fund(d);
abserror(td, 'absolute error train') = error(td);
abserror(ntd,'absolute error test')  = error(ntd);
lastDayTraining(td)                  = td.pos=card(td);

$libInclude webui
