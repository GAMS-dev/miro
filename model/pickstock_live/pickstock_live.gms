$title Stock Selection Optimization with live data from the web
* Optimization model to pick a small subset of the stocks together with
* some weights, such that this portfolio has a similar behavior to our
* overall Dow Jones index.

Set date   'date'
    symbol 'stockSymbol';
    
$onExternalInput
Scalar maxstock      'maximum number of stocks to select '  / 2  /
       trainingdays  'number of days for training '  / 99  /;
$offExternalInput

Parameter
    price(date,symbol)        'UIOutput: stock price';

$if not set TW_lo $set TW_lo "2017-01-01"
$if not set TW_up $set TW_up "2017-12-31"

$ifthen %MIRO% == "RUN"
$onEmbeddedCode Python:
try:
   import pandas as pd
except:
   import pip
   if(hasattr(pip, 'main')):
      pip.main(['install', 'pandas'])
   else:
      pip._internal.main(['install', 'pandas'])
   import pandas as pd
   
# This is a work-around for a pandas_datareader bug
pd.core.common.is_list_like = pd.api.types.is_list_like

try:
   import pandas_datareader.data as web
except:
   import pip
   if(hasattr(pip, 'main')):
      pip.main(['install', 'pandas-datareader'])
   else:
      pip._internal.main(['install', 'pandas-datareader'])
   import pandas_datareader.data as web
   
from datetime import datetime

# stockSymbols in DowJones index
djSymbols  = ["MMM", "AXP", "AAPL", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DWDP", "XOM","GE","GS","HD","IBM","INTC","JNJ","JPM","MCD","MRK","MSFT","NKE","PFE","PG","TRV","UTX","UNH","VZ","V","WMT"]

start = datetime.strptime("%TW_lo%", '%Y-%m-%d')
end   = datetime.strptime("%TW_up%", '%Y-%m-%d')
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
    obj         'UIOutput: objective';

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

Set fHdr      'fund header'            / dj 'dow jones','index fund'  /
    errHdr    'stock symbol header'    / 'absolute error train', 'absolute error test' /;
    
$onExternalOutput
Parameter
    partOfPortfolio(symbol)            'what part of the portfolio'   
    dowVSindex(date,fHdr)              'dow jones vs. index fund [MIRO:table]'     
    abserror(date,errHdr)              'absolute error [MIRO:table]'               
Singleton Set lastDayTraining(date)    'last date of training period [MIRO:hidden]' ;
$offExternalOutput

partOfPortfolio(s)                   = w.l(s);
dowVSindex(d,'dj')                   = index(d);
dowVSindex(d,'index fund')           = fund(d);
abserror(td, 'absolute error train') = error(td);
abserror(ntd,'absolute error test')  = error(ntd);
lastDayTraining(td)                  = td.pos=card(td);

$libInclude miro
