# A Battery Energy Storage System (BESS) sizing problem

This model is based on an example from the company [NAG](https://nag.com/), which can be found on their GitHub: [BESS.ipynb](https://github.com/numericalalgorithmsgroup/NAGPythonExamples/blob/master/local_optimization/MILP/BESS_MILP.ipynb).

## Model

<div align="center"> <img src="static_bess/energy_system.png" alt="input section" width="500"/> </div>

A city has a certain demand for energy at each hour. Given several options to meet the city's demand, the goal is to find the cheapest schedule. Since we can buy energy from the external grid or from a number of generators. It is also efficient to buy energy when prices are low, which is usually when demand is low. To take advantage of this, a city can use a Battery Energy Storage System (BESS), which is charged when demand is low and can then be used to power the city when demand is high. The goal of this model is to simultaneously decide which energy source should be used at any given hour and which type of BESS will provide the lowest overall cost. This is done by analyzing the size of the BESS in terms of storage and delivery rate.

### Sets:
$i=$ generators, $j =$ hours, $t =$ alias of $j$

### Decision Variables:
####BESS Variables
$p_j^{battery} \doteq$ power charged or discharged from the battery at hour $j$

$c^{battery} \doteq$ power rate of the BESS (how much can the battery maximal deliver in one hour)

$e^{battery} \doteq$ energy rate of the BESS (how much can the battery maximal store)

####Generator Variables
$p_{ij}^{gen} \doteq$ dispatched power of generator $i$ at hour $j$  ($p_{ij}^{gen}\geq 0$)

$s_{ij} \doteq$ binary variable stating if generator $i$ is active at hour $j$ ($=1$ online, $=0$ offline)

####External Grid Variables
$p_j^{extern} \doteq$ from the external grid imported power at hour $j$

### Given Data (Parameters):
Since the size of the BESS is being optimized, we need to define how expensive it is to give the battery a faster delivery rate, the $cost\_bat\_power$ and the $cost\_bat\_energy$, which determine how expensive the storage size of the battery is.

The generators must be specified. To keep things simple, the cost of each generator is calculated using a linear cost function
$$cost_i(p_{ij}^{gen}) = a_i \cdot p_{ij}^{gen} + b_i$$

where $a_i$ is the cost per unit of the generator $i$ and $b_i$ is the fixed cost of that generator.

Furthermore, when the generator is active, it must remain within its limits given by $pl_i$ the minimum power output and $pu_i$ the maximum power output. Finally, the generators also have a minimum ubtime $ut_i$ and a minimum downtime $dt_i$.

Depending on the time, there are different prices for importing power from the external grid: $cost\_import\_power_j$. There is also a limit to the amount of energy that can be imported from the external grid per hour: $max\_input\_external\_grid$.

Finally, we have a certain load that must be supplied by the energy sources every hour $j$ : $load\_balance_j$.
 
### Constraints:
Ensure that the load balance is achieved every hour:
$$\sum_i p_{ij}^{gen} + p_j^{battery}  + p_j^{extern} = load\_balance_j \quad \forall j.$$

Ensure that if a generator is active, its power remains within its limits: 
$$pl_i \cdot s_{ij} \le p_{ij}^{gen} \le pu_i \cdot s_{ij}  \quad \forall i,j.$$

Ensure that generators have their minimum uptime and downtime:
$$ \sum _{t=j}^{j+ut_i-1} s_{it} \ge ut_i \cdot (s_{ij} - s_{ij-1})  \quad \forall i,j,$$

$$ \sum _{t=j}^{j+dt_i-1}(1- s_{it}) \ge dt_i \cdot (s_{ij-1} - s_{ij}) \quad \forall i,j.$$

Ensure that the battery stays within its limits: 
$$-c^{battery} \le p_j^{battery} \le c^{battery}  \quad \forall j.$$

Same for the energy (storage) capacity of the battery:
$$-e^{battery} \le \sum_{j=0}^{t}  p_j^{battery} \le 0  \quad \forall t.$$

Ensure that the limit for importing power from the external grid is not exceeded:
$$0 \le p_j^{extern} \le max\_input\_external\_grid  \quad \forall j.$$

### Objective Function:
Minimize the overall cost by including the power from the generators, the external grid and the BESS:

$$\min \sum_j \sum_i cost_i(p_{ij}^{gen}) + \sum_j cost\_import\_power_j \cdotp p_j^{extern} + cost\_bat\_power \cdot  c^{battery} + cost\_bat\_energy \cdot e^{battery}$$
