import argparse
import sys

from gamspy import Container, Set, Alias, Parameter, Variable, Equation, Model, Sum, Sense, Card, Options, Card
from gamspy.math import abs

import pandas as pd


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--test')
    args = parser.parse_args()
    if args.test == '123':
        sys.exit(123)
    m = Container()

    date = Set(m, "date", description="trading date")
    symbol = Set(m, "symbol", description="stock symbol")
    price = Parameter(
        m,
        "price",
        [date, symbol],
        domain_forwarding=True,
        records=pd.read_csv('./dowjones2016.csv'),
        description="price of stock on date",
        is_miro_input=True
    )
    trainingdays = Parameter(
        m, "trainingdays", records=100, is_miro_input=True)
    maxstock = Parameter(m, "maxstock", records=3, is_miro_input=True)
    d = Alias(m, "d", date)
    s = Alias(m, "s", symbol)

    mip_solver = Set(m, "solver", description="MIP Solver to use",
                     records=["CPLEX"], is_singleton=True, is_miro_input=True)

    avgprice = Parameter(m, "avgprice", symbol,
                         description="average price of stock")

    avgprice[s] = Sum(d, price[d, s]) / Card(d)

    weight = Parameter(m, "weight", symbol, description="weight of stock")
    weight[symbol] = avgprice[symbol] / Sum(s, avgprice[s])

    contribution = Parameter(m, "contribution", [date, symbol])
    contribution[d, s] = weight[s] * price[d, s]

    index = Parameter(m, "index", date, description="Dow Jones index")
    index[d] = Sum(s, contribution[d, s])

    negative_prices = price.records[price.records['value'] < 0]

    # input validataion
    print('------------------------------------')
    print('        Data validation')
    print('------------------------------------\n')
    if len(negative_prices.index):
        print('price:: No negative prices allowed!')
        for index, row in negative_prices.iterrows():
            print(f' Symbol {row["symbol"]:<4} has negative price at the date: {row["date"]}')
        raise Exception("Data errors detected")
    else:
        print('OK')

    td = Set(m, "td", domain=[date], description="training days")
    ntd = Set(m, "ntd", domain=[date], description="none-training days")
    td.setRecords(date.records["uni"][:int(trainingdays.toValue())])
    ntd[:] = ~td[:]

    p = Variable(m, "p", "binary", symbol, description="is stock included?")
    w = Variable(m, "w", "positive", symbol,
                 description="what part of the portfolio")
    slpos = Variable(m, "slpos", "positive", date,
                     description="positive slack")
    slneg = Variable(m, "slneg", "positive", date,
                     description="negative slack")

    fit = Equation(m, name="deffit", domain=[
                   td], description="fit to Dow Jones indeex")

    fit[td] = Sum(s, price[td, s] * w[s]) == index[td] + slpos[td] - slneg[td]

    pick = Equation(
        m, name="defpick", domain=[s], description="can only use stok if picked"
    )
    pick[s] = w[s] <= p[s]

    numstock = Equation(m, name="defnumstock",
                        description="few stocks allowed")
    numstock[...] = Sum(s, p[s]) <= maxstock
    obj = Sum(td, slpos[td] + slneg[td])

    pickstock = Model(
        container=m,
        name="pickstock",
        equations=m.getEquations(),
        problem="MIP",
        sense=Sense.MIN,
        objective=obj,
    )
    pickstock.solve(solver=mip_solver.records.loc[0, "uni"], options=Options(
        relative_optimality_gap=0.01), output=sys.stdout)

    fund = Parameter(m, "fund", [date],
                     description="Index fund report parameter")
    fund[d] = Sum(s, price[d, s] * w.l[s])
    error = Parameter(m, "error", [date], description="Absolute error")
    error[d] = abs(index[d] - fund[d])
    f_hdr = Set(m, "fHdr", description="fund header",
                records=[('dj', 'dow jones'), ('index fund', )])
    err_hdr = Set(m, "errHdr", description="stock symbol header",
                  records=['absolute error train', 'absolute error test'])

    error_train = Parameter(
        m, "error_train", description="Absolute error in entire training phase", is_miro_output=True)
    error_test = Parameter(
        m, "error_test", description="Absolute error in entire testing phase", is_miro_output=True)
    error_ratio = Parameter(
        m, "error_ratio", description="Ratio between error test and error train", is_miro_output=True)

    stock_weight = Parameter(
        m, "stock_weight", domain=[symbol], description="Ratio between error test and error train", is_miro_output=True)
    dow_vs_index = Parameter(
        m, "dowVSindex", domain=[date, f_hdr], description="dow jones vs. index fund", is_miro_output=True, is_miro_table=True)
    abserror = Parameter(m, "abserror", domain=[date, err_hdr], description="absolute error",
                         is_miro_output=True, is_miro_table=True)

    first_day_training = Set(
        m, "firstDayTraining", description="first date of training period", is_singleton=True, is_miro_output=True)
    last_day_training = Set(
        m, "lastDayTraining", description="last date of training period", is_singleton=True, is_miro_output=True)

    stock_weight[s] = w.l[s]

    dow_vs_index[d, 'dj'] = index[d]
    dow_vs_index[d, 'index fund'] = fund[d]
    abserror[td, 'absolute error train'] = error[td]
    abserror[ntd, 'absolute error test'] = error[ntd]
    last_day_training[td] = td.pos == Card(td)
    first_day_training[td] = td.pos == 1
    error_train.setRecords(pickstock.objective_value)
    error_test[:] = Sum(ntd, error[ntd])
    if error_train.toValue() > 0:
        error_ratio[:] = error_test/error_train
    else:
        error_ratio.setRecords(float('inf'))

    # parameter including all stocks and dow jones index
    price_merge = Parameter(m, "priceMerge", domain=[
                            date, "*"], description="Price (stocks & dow jones)", is_miro_output=True)
    price_merge[d, symbol] = price[d, symbol]
    price_merge[d, 'DowJones'] = index[d]


if __name__ == "__main__":
    main()
