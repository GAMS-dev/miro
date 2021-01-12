# Cutting Stock Problem
Cutting stock is the problem of cutting standard-sized pieces of stock material.
in this example from a roll of paper. The objective is to meet the demand while
minimizing waste.

### Problem:
In this example the goal is to cut paper rolls of standarizedd size into certain widths to satisfy customer's demand. Each paper roll can be cut in certain ways so called patterns. If not all of the paper roll is used waste is produced as a byproduct. Our objective is then to minimize the number of patterns used while simultanouesly satisfying demand.

### Data:
Two input parameters are necessary to calculate a solution.
1. The raw width of the paper roll which patterns are cut.
2. The amount each width is demanded.

The parameter $ r $ is a scalar positive interger which can be entered as a number. The demand parameter contains the paper width that is demanded and the number of times each width is demanded.
| Width          | Demand          |
| :-------------: | :-------------: |
| 14          | 211              |
| 31          | 395             |
| 36          | 610             |
| 45          | 97               |  

### Optimization:
We seek to minimize the number of patterns used while satisfying demand. The problem can be written as 

$$ \min_{p} z = \sum_{p} xp_{p} $$

subject to
$$ demand_i \leq \sum_p a_{i, p} x_p $$
where $ a_{ip} $ is the number of times width $ i $ is in pattern $ p $.