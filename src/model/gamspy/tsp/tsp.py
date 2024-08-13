"""
This is based on tsp5 from the GAMS model library.
This direct adaptation is just a sample translation from GAMS to GAMSPy,
Therefore it is not optimised in Python and has a slow performance.

The shortest round trip is calculated for a set of cities. The distance
between these cities is calculated as crow flight distance based on the
latitude and longitude of the cities specified in parameter iiLocData.


Keywords: mixed integer linear programming, traveling salesman problem,
          iterative subtour elimination
"""
import sys

import pandas as pd
import numpy as np

from gamspy import (
    Container,
    Equation,
    Model,
    Parameter,
    Sum,
    Set,
    Alias,
    Variable,
    Ord,
    Sense,
    Options,
    ModelStatus,
)
from gamspy.math import sin, cos, sqrt, atan, Round


def main():
    m = Container()

    with open("miro.log", "w", encoding="utf-8") as f:
        f.writelines(
            [
                "------------------------------------\n",
                "        Options selected\n",
                "------------------------------------\n",
            ]
        )

    loc_hdr = Set(
        m, "locHdr", records=["lat", "lng"], description="Location data header"
    )
    ii = Set(m, "ii", description="Cities used in the model")
    i = Set(m, "i", domain=[ii], description="Subset of cities")

    jj = Alias(m, "jj", ii)
    j = Alias(m, "j", i)

    city_data = pd.DataFrame(
        [
            ["Montgomery, AL", 32.367, -86.3006],
            ["Juneau, AK", 58.3019, -134.4197],
            ["Phoenix, AZ", 33.4484, -112.0741],
            ["Little Rock, AR", 34.7465, -92.2896],
            ["Denver, CO", 39.7348, -104.9653],
            ["Hartford, CT", 41.7646, -72.6909],
            ["Dover, DE", 39.1582, -75.5244],
            ["Tallahassee, FL", 30.4381, -84.2809],
            ["Atlanta, GA", 33.7491, -84.3902],
            ["Honolulu, HI", 21.3045, -157.8557],
            ["Boise, ID", 43.6166, -116.2009],
            ["Springfield, IL", 39.799, -89.644],
            ["Indianapolis, IN", 39.7683, -86.1584],
            ["Des Moines, IA", 41.5911, -93.6037],
            ["Sacramento, CA", 38.5816, -121.4944],
            ["Topeka, KS", 39.049, -95.6776],
            ["Frankfort, KY",38.2009,-84.8733],
            ["Baton Rouge, LA",30.446,-91.1874],
            ["Augusta, ME",44.3106,-69.7797],
            ["Annapolis, MD",38.9786,-76.4928],
            ["Boston, MA",42.3603,-71.0583],
            ["Lansing, MI",42.7338,-84.5554],
            ["Saint Paul, MN",44.9504,-93.1015],
            ["Jackson, MS",32.299,-90.1848],
            ["Jefferson City, MO",38.5774,-92.1724],
            ["Helena, MT",46.5927,-112.0363],
            ["Lincoln, NE",40.8089,-96.7078],
            ["Carson City, NV",39.1649,-119.7666],
            ["Concord, NH",43.2072,-71.5375],
            ["Trenton, NJ",40.2171,-74.7429],
            ["Santa Fe, NM",35.687,-105.9378],
            ["Albany, NY",42.6512,-73.755],
            ["Raleigh, NC",35.7804,-78.6391],
            ["Bismarck, ND",46.8083,-100.7837],
            ["Columbus, OH",39.9623,-83.0007],
            ["Oklahoma City, OK",35.473,-97.5171],
            ["Salem, OR",44.9392,-123.0331],
            ["Harrisburg, PA",40.2663,-76.8861],
            ["Providence, RI",41.824,-71.4128],
            ["Columbia, SC",34.0007,-81.0343],
            ["Pierre, SD",44.3683,-100.3512],
            ["Nashville, TN",36.1622,-86.7744],
            ["Austin, TX",30.2711,-97.7437],
            ["Salt Lake City, UT",40.7596,-111.8868],
        ],
        columns=["ii", "lat", "lng"],
    ).set_index("ii")

    ii_loc_data = Parameter(
        m,
        "iiLocData",
        domain=[ii, loc_hdr],
        domain_forwarding=[True, False],
        uels_on_axes=True,
        records=city_data,
        is_miro_input=True,
        is_miro_table=True,
        description="City location information",
    )

    c = Parameter(
        m,
        "c",
        domain=[ii, jj],
        description="Cost coefficients (distance in 1000 of miles)",
    )

    # Calculating "crow - flight - distances" in miles
    llr = Parameter(
        m,
        "llr",
        domain=[ii, loc_hdr],
        description="Latiutude and longitude of cities (radians)",
    )
    help_cd = Parameter(
        m,
        "helpCD",
        domain=[ii, ii],
        description="Help parameter for crow-distance calcualtion",
    )
    equ_rad = Parameter(
        m, "equRad", records=3963.190, description="Earth radius at equator in miles"
    )

    llr[ii, loc_hdr] = ii_loc_data[ii, loc_hdr] * np.pi / 180

    help_cd[ii, jj].where[~ii.sameAs(jj)] = sin(llr[ii, "lat"]) * sin(
        llr[jj, "lat"]
    ) + cos(llr[ii, "lat"]) * cos(llr[jj, "lat"]) * cos(llr[ii, "lng"] - llr[jj, "lng"])

    c[ii, jj].where[~ii.sameAs(jj)] = (
        (np.pi / 2 - atan(help_cd[ii, jj] / sqrt(1 - help_cd[ii, jj] * help_cd[ii, jj])))
        * equ_rad
        / 1000
    )

    with open("miro.log", "a", encoding="utf-8") as f:
        f.writelines(["- The following cities were selected:\n"])
        f.writelines("\n".join(ii.records["uni"].values.tolist()))
        f.writelines(["\n"])

    # for computational work with simple minded algorithm
    # we can restrict size of problem and define the model
    # over a subset of all cities.

    x = Variable(
        m,
        "x",
        domain=[ii, jj],
        type="binary",
        description="decision variables - leg of trip",
    )
    z = Variable(m, "z", description="objective variable")

    objective = Equation(m, "objective", description="total cost")
    rowsum = Equation(m, "rowsum", domain=[ii], description="leave each city only once")
    colsum = Equation(
        m, "colsum", domain=[jj], description="arrive at each city only once"
    )

    # the assignment problem is a relaxation of the TSP

    objective[...] = z == Sum((i, j), c[i, j] * x[i, j])

    rowsum[i] = Sum(j, x[i, j]) == 1
    colsum[j] = Sum(i, x[i, j]) == 1

    # exclude diagonal
    x.fx[ii, ii] = 0

    max_cities = Parameter(
        m,
        "maxCities",
        records=100,
        is_miro_input=True,
        description="Limit numbers of cities in tour",
    )
    max_cuts = Parameter(
        m,
        "maxCuts",
        records=1000,
        is_miro_input=True,
        description="Limit numbers of cuts",
    )

    with open("miro.log", "a", encoding="utf-8") as f:
        f.writelines(
            [f"- Maximum number of cities in tour          : {max_cities.toValue()}\n"]
        )

        if len(ii) > max_cities.toValue():
            f.writelines(
                [
                    f"Attention: Maximum number of cities in tour is smaller than selected cities ({len(ii)})!\n",
                    f'           Increase the value of "maxCities", if all selected cities should be visited.\n',
                ]
            )
        f.writelines(
            [
                f"- Maximum number of subtour elimiantion cuts: {max_cuts.toValue()}\n",
                "------------------------------------------------------\n",
            ]
        )
        if len(ii) < 2:
            f.writelines(["Need to have at least two cities to calculate a tour\n"])
            raise Exception("Need to have at least two cities to calculate a tour")
        if max_cities.toValue() < 2:
            f.writelines(["MaxCities needs to be at least two to calculate a tour\n"])
            raise Exception("MaxCities needs to be at least two to calculate a tour")
        if max_cuts.toValue() < 1:
            f.writelines(["max_cuts needs to be at least one to run the problem\n"])
            raise Exception("max_cuts needs to be at least one to run the problem")

    i[ii].where[Ord(ii) <= max_cities] = True

    # Dynamic subtour elimination
    ste = Set(
        m,
        "ste",
        records=[("c" + str(i), i) for i in range(1, 100000)],
        description="possible subtour elimination cuts",
    )
    a = Set(m, "a", domain=[ste], description="active cuts")
    tour = Set(m, "tour", domain=[ii, jj], description="possible subtour")
    n = Set(m, "n", domain=[jj], description="nodes visited by subtour")

    cc = Parameter(m, "cc", domain=[ste, ii, jj], description="cut coefficients")
    rhs = Parameter(m, "rhs", domain=[ste], description="right hand side of cut")

    defste = Equation(m, "defste", domain=[ste], description="subtour elimination cut")
    defste[a] = Sum((i, j), cc[a, i, j] * x[i, j]) <= rhs[a]

    dse = Model(
        m,
        name="DSE",
        equations=m.getEquations(),
        problem="MIP",
        sense=Sense.MIN,
        objective=z,
    )

    a[ste] = False
    cc[a, i, j] = 0
    rhs[a] = 0

    solve_options = Options(
        relative_optimality_gap=0,
        time_limit=30,
        equation_listing_limit=0,
        variable_listing_limit=0,
        report_solution=2,
        solver_link_type=5,
    )

    proceed = 1
    sum_levels_subtour = Parameter(m, "sum_levels_subtour")
    sum_levels_subtour[...] = Sum((n, j), x.l[n, j])

    for ste_elem in ste.records["uni"]:
        if proceed == 1:
            dse.solve(options=solve_options, output=sys.stdout)
            if dse.status != ModelStatus.OptimalGlobal:
                with open("miro.log", "a", encoding="utf-8") as f:
                    f.writelines(
                        ["Problems with MIP solver - No optimal solution found.\n"]
                    )
                raise Exception("Problems with MIP solver")
            x.l[i, j] = Round(x.l[i, j])
            proceed = 2

        # Check for subtours
        tour[i, j] = False
        n[j] = False

        # use len() instsead of Card(), beacuse Card(n) is an Expression, which is always evaluated as true!!
        if len(n) == 0 and not x.records[x.records["level"] != 0].empty:
            n[x.records[x.records["level"] != 0]["ii"].iloc[0]] = True

        # Found all subtours, resolve with new cuts
        if len(n) == 0:
            proceed = 1
        else:
            # Construct a single subtour and remove it by setting x.l=0 for its edges
            while x.records[x.records["ii"].isin(n.records["jj"])]["level"].sum():
                for i_loop in i.toList():
                    for j_loop in x.records[
                        (x.records["level"] != 0) & (x.records["ii"] == i_loop)
                    ]["jj"]:
                        if i_loop in n.records["jj"].values:
                            tour[i_loop, j_loop] = True
                            x.l[i_loop, j_loop] = 0
                            n[j_loop] = True

            if len(n) < len(j):
                a[ste_elem] = 1
                rhs[ste_elem] = len(n) - 1
                cc[ste_elem, i, j].where[n[i] & n[j]] = 1
            else:
                proceed = 0
                break

    tour_hdr = Set(
        m,
        "tourHdr",
        records=["lngA", "latA", "lngB", "latB", "Flow"],
        description="Tour header",
    )
    total_cost = Parameter(
        m, "total_cost", is_miro_output=True, description="Total Distance (miles)"
    )
    tour_details = Parameter(
        m,
        "tourDetails",
        domain=[ii, jj, tour_hdr],
        is_miro_output=True,
        is_miro_table=True,
    )

    total_cost[...] = z.l * 1000

    with open("miro.log", "a", encoding="utf-8") as f:
        if proceed == 0:
            f.writelines(
                [f"Optimal tour found - Tour length: {total_cost.toValue()} miles\n"]
            )
        else:
            f.writelines(
                [
                    'Out of subtour cuts, increase value of "max_cuts" to find better solution.\n'
                ]
            )

    for _, row in tour.records.iterrows():
        tour_details[row["ii"], row["jj"], "lngA"] = ii_loc_data[row["ii"], "lng"]
        tour_details[row["ii"], row["jj"], "latA"] = ii_loc_data[row["ii"], "lat"]
        tour_details[row["ii"], row["jj"], "lngB"] = ii_loc_data[row["jj"], "lng"]
        tour_details[row["ii"], row["jj"], "latB"] = ii_loc_data[row["jj"], "lat"]
        tour_details[row["ii"], row["jj"], "Flow"] = 1


if __name__ == "__main__":
    main()
