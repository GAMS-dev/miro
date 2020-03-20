# Traveling Salesman Problem
This model tries to find the shortest route through all cities given by the 
user. There are two ways how the list of cities can be defined and on the tab
`Input Mode` you can switch between the two:

- With `Use Python geoCode package (Input Mode A)` the user specifies just a 
  list of cities (at the tab `Cities (Input Mode A)`) and the model sets the 
  location of these cities automatically using the Python package "geoCoder" 
  (which has to be installed on the machine running the model). In this case 
  also the distances between the cities are calculated in Python. For this 
  step the packages "itertools" and "geopy" are used.
- With `User provides city location (Input Mode B)` the user has to specify 
  latitude and longitude for all cities herself (at the tab 
  `City location information (Input Mode B)`). Distances are calculated in 
  GAMS directly in this case. At the tab `City location information (Input Mode B)`
  one can also switch the view to see the specified locations on a map.
  
At the tab `Advanced Settings` one can specify certain values to limit the 
runtime of the model (at the cost of not finding the solution for all cities)
given.

This app comes with two example datasets:
- `default` lists the capital cities of all 50 U.S. states
- `defaultGermany` lists the capital cities of all 16 states in Germany

Both data sets have the data for both input modes.