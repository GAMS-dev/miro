# A vehicle routing problem with time windows

This model is based on the work by K.C. Tan et.al. in  [Heuristic methods for vehicle routing problem with time windows](https://doi.org/10.1016/S0954-1810(01)00005-X).

## Model
### Sets:
$i =$ customers, $j =$ alias of $i$, $k =$ vehicles

### Given Data:
Two scalars: $K =$ number of vehicles and $Q = $ capacity of all vehicles (could easily be changed to $q_k$ to give individual capacities for each vehicle).

For each customer, the latitude and longitude values from which the distance $d_{ij}$ is calculated. Furthermore, their demand $q_i$, the service time $s_i$, the ready time $e_i$ (earliest) and the due time $l_i$ (latest) for each of the $N$ customers. 

### Decision Variables:
$x_{ijk} =$ if vehicle $k$ drives from customer $i$ to $j$, $t_i = $ arrival time a customer $i$, $w_i = $ waiting time at customer $i$ ($e_i-t_i$) where $x_{ijk} \in  \{ 0,1\}, t_i, w_i  \ge0$ for all $i,j,k$.

### Constraints:
Ensure that no more than $K$ vehicles start at the depot:

$\sum_{k=1}^K \sum_{j=1}^N x_{ijk} \le K$ for $i=0$

Ensure that every vehicle starts and finishes at the depot:

$\sum_{j=1}^N x_{ijk} = \sum_{j=1}^N x_{jik} \le 1$ for $i=0$ and $k \in \{ 1,...,K \}$

Ensure that the same vehicle enters and leaves a customer:

$\sum_{j=0}^N x_{ijk} =\sum_{j=0}^N x_{jik} $ for $i \in \{ 1,...,N \}$ and $k \in \{ 1,...,K \}$

Ensure that each customer is visited and left once:

$\sum_{k=1}^K \sum_{j=0, j\neq i}^N x_{ijk} = 1 $ for  $i \in \{ 1,...,N \}$

$\sum_{k=1}^K \sum_{i=0, i\neq j}^N x_{ijk} = 1 $ for  $j \in \{ 1,...,N \}$

Ensure that no vehicle is carrying more than the capacity:

$\sum_{i=1}^N q_i \sum_{j=0, j\neq i}^N x_{ijk} \le Q $ for  $k \in \{ 1,...,K \}$

No arrival, waiting or service time at the depot:

$ t_0 = w_0 = s_0 = 0$

Make sure that the arrival time at the next customer takes the distance, service time and waiting time into account. The formula from the paper has been linearized:

$ t_i + d_{ij}+ s_i + w_i - t_j \le  (1-x_{ijk})\cdot M $   for $i\neq j$, $j \in \{ 1,...,N \}$, $i \in \{ 0,...,N \}$, $k \in \{ 1,...,K \}$

Ensure that the time window is met:

$e_i \le (t_i +w_i) \le l_i$  for  $i \in \{ 1,...,N \}$

### Objective Function:

Minimize: $\sum_{i=0}^N \sum_{j=0, j\neq i}^N \sum_{k=1}^K c_{ij} \, x_{ijk}$

## App

To add a customer, click on the map. This will create an entry in the table below. Here you can add the remaining details for this customer, update the name and manually enter the latitude and longitude. The latter can also be changed by dragging the appropriate label on the map. To remove a customer, simply click on the label.

The depot will always be the first customer entered in the table! It can't be removed, but the rest can be changed. Don't forgot to set a reasonable due date, otherwise the solution will be infeasible if the vehicles don't have enough time to return to the depot.

<img src="/static_vehicle_routing_tw/input_demo.png" alt="input section" width="1000"/>


Furthermore, the number of vehicles and their capacity can be changed under the *Input widgets* tab.
