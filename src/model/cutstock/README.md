# Cutting Stock Problem
Cutting stock is the problem of cutting standard-sized pieces of stock material,
in this example from a paper roll. The objective is to meet the demand while
minimizing waste.

### Problem:
In this example, the goal is to cut paper rolls of standardized size into specific widths to meet customer demand. Each paper roll can be cut in a specific way, called patterns. If the entire paper roll is not used, waste is generated as a by-product. Our goal is to minimize the number of patterns used while meeting demand.

### Data:
Two input parameters are required to calculate a solution.
1. The raw width of the paper roll from which patterns are cut.
2. The number that each width is demanded.

The parameter $ r $ is a positive integer which can be entered as a number. The demand parameter determines which paper width is requested and how often.
| Width &nbsp;&nbsp;         | Demand          |
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