#A Transportation Problem with multiple version LP/MIP/MINLP
This MIRO App is based on the [trnsport](https://www.gams.com/latest/gamslib_ml/libhtml/gamslib_trnsport.html) model from the GAMS Model library.
It finds a least cost shipping schedule that meets
requirements at markets and supplies at factories.


###Indices:

`i` = plants
`j` = markets

###Given Data:

`a(i)` = supply of commodity of plant i (cases)
`b(j)` = demand for commodity at market j (cases)
`d(i,j)` = distance between plant i and market j (thousand miles)
`c(i,j) = F ⋅ d(i,j)` shipping cost per unit shipment between plant i and market j ($/case/thousand miles)

| Distances|  | | | |
| -------- | ---------- | --------- | -------- | -------- :|
| | New York | Chicago | Topeka | Supply |
|Seattle	         |       2.5	           |1.7	            |1.8                   |     350               |
|San Diego	 |      2.5	           |1.8	            |1.4	             |    600                |
|Demand	         |      325	           |300	            |275	             |                         |


`f` = $ per thousand miles


###Decision Variables:
`x(i,j)` = amount of commodity to ship from plant `i` to market `j` (cases)
where `x(i,j) ≥ 0`, for all `i,j`

###Constraints:
Observe supply limit at plant `i`: `∑j x(i,j) ≤ a(i)` for all `i` (cases)
Satisfy demand at market `j`: `∑i x(i,j) ≥ b(j)` for all `j` (cases)

###Objective Function:
`Minimize ∑i∑j c(i,j) ⋅ x(i,j)` ($K)



Dantzig, G B, Chapter 3.3. In Linear Programming and Extensions.
Princeton University Press, Princeton, New Jersey, 1963.
