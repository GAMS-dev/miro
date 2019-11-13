# Stock Selection Optimization
Pickstock is an optimization model from the financing sector. The goal is to pick a small subset of stocks together with some weights, such that this portfolio has a similar behavior to our overall Dow Jones index.

### Data: 
Performance (price) data of all 30 shares of the Dow Jones index over a period of 1 year: 

| Date          | Stock         | Price   |
| ------------- | ------------- | -------:|
| 2016-01-04    | AAPL          | 105.35  |
| 2016-01-04    | AXP	        | 67.59   |
| 2016-01-04    | BA	        | 140.5   |
| 2016-01-04    | CAT	        |  67.99  |
| 2016-01-04    | CSCO          | 26.41   |
| 2016-01-04    | CVX	        |  88.85  |
| 2016-01-04    | DD	        |  63.07  |
| 2016-01-04    | DIS	        |  102.98 |
| 2016-01-04    | GE	        |  30.71  |
| 2016-01-04    | GS	        |  177.14 |
| [...]         | [...]         | [...]   |


### Goal: 
The goal is to find a small selection of stocks that follows the Dow Jones as good as possible. 
The interesting part is not how good the model performs with the data it knows about but how well it approximates the Dow Jones index in the future. 
In order to simulate this the stock data is devided into two parts: the **training phase** and the **testing phase**. 
The training phase is be the model input data, i.e. the data the model uses to find an index fund to approximate the Dow Jones index as good as possible. 
When the model finds a solution, the results can be evaluated in the testing phase.  

![Training and testing phase](static/training_testing.png =800x276)

The data of the testing phase is not used for the optimization but only for the evaluation.
Since the objective function minimizes the absolute deviation between the DJ index and the selected stocks,the area between the two lines shows how good the solution is. 

![Example](static/example.png =800x389)

### Optimization model: 
Select a subset (≤ maxstock) of Dow Jones stocks, along with weights, so that this portfolio behaves similarly to the overall index (in the training phase).

![Pickstock model](static/model.png =500x217)
