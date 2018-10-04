$title Stock Selection Optimization with live data from the web
* Optimization model to pick a small subset of the stocks together with
* some weights, such that this portfolio has a similar behavior to our
* overall Dow Jones index.

$onExternalInput
Scalar maxstock      'maximum number of stocks to select ### { "slider":{"min":1, "max":29, "default":5, "step":1 }}'  / 2  /
       trainingdays  'number of days for training        ### { "slider":{"min":1, "max":"card(tw)", "default":99, "step":1 }}'  / 99  /;
$set WEBUICONF '{ "GMSPAR_TW":{"alias": "time window","daterange":{"label":"time window","start": "2018-01-01","startview": "year"}} }'
$offExternalInput

Set date   'date'
    symbol 'stockSymbol';

Parameter
    price(date,symbol)        'UIOutput: stock price';

$ifthen %GMSWEBUI%==1
$  batInclude loadCSV scalars
$  if setenv PICKSTOCK_LIVE_TW_min $set TW_MIN %sysenv.PICKSTOCK_LIVE_TW_MIN%
$  if setenv PICKSTOCK_LIVE_TW_max $set TW_MAX %sysenv.PICKSTOCK_LIVE_TW_MAX%
$endif

$if not set TW_MIN $set TW_MIN "2016-02-01"
$if not set TW_MAX $set TW_MAX "2016-03-31"

$ifthen %GMSWEBUI% == 1
$onEmbeddedCode Python:
import pandas as pd
from datetime import datetime
# This is a work-around for a pandas_datareader bug
pd.core.common.is_list_like = pd.api.types.is_list_like
import pandas_datareader.data as web

# stockSymbols in DowJones index
djSymbols  = ["MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DWDP", "XOM","GE","GS","HD","IBM","INTC","JNJ","JPM","MCD","MRK","MSFT","NKE","PFE","PG","TRV","UTX","UNH","VZ","V","WMT"]

start = datetime.strptime("%TW_MIN%", '%Y-%m-%d')
end   = datetime.strptime("%TW_MAX%", '%Y-%m-%d')
price = []
for sym in djSymbols:
   price_sym = web.DataReader(sym, 'iex', start, end)[['close']]
   price_sym['symbol'] = sym
   price.append(price_sym)
price = pd.concat(price)
gams.set("price", [(r[0], r[2], r[1]) for r in price.itertuples()])
$offEmbeddedCode date<price.dim1 symbol<price.dim2 price
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

fund(d) = sum(s, price(d, s)*w.l(s));
error(d) = abs(index(d)-fund(d));

$onExternalOutput
Set 
    fHdr      'fund header'            / 'dow jones','index fund' /
    errHdr    'stock symbol header'    / 'absolute error train', 'absolute error test' /;
Parameter
    partOfPortfolio(symbol)            'what part of the portfolio'
    dowVSindex(date,fHdr)              'dow jones vs. index fund'
    abserror(date,errHdr)              'absolute error';
Singleton Set lastDayTraining(date)    'last date of training period ### vertical marker in chart' ;
$offExternalOutput

partOfPortfolio(s)                   = w.l(s);
dowVSindex(d,'dow jones')            = index(d);
dowVSindex(d,'index fund')           = fund(d);
abserror(td, 'absolute error train') = error(td);
abserror(ntd,'absolute error test')  = error(ntd);
lastDayTraining(td)                  = td.pos=card(td);

$libInclude webui
