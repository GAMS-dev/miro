"""
From http://web.cba.neu.edu/~msolomon/problems.htm
R1-type: R101
"""

import pandas as pd
import numpy as np
import sys

from gamspy import (
    Container,
    Alias,
    Equation,
    Model,
    Parameter,
    Sense,
    Set,
    Sum,
    Variable,
    Ord,
    Domain
)
from gamspy.math import Max


def main():
    m = Container()

    data = pd.read_csv("r101_solomon.txt", sep=",")

    # compute euclidean distance matrix between all customers
    coordinates = data[["XCOORD.", "YCOORD."]].to_numpy()
    distance_matrix = np.sqrt(
        np.einsum("ijk->ij", (coordinates[:, None, :] - coordinates) ** 2)
    )

    vehicle_number = 25
    vehicle_capacity = 200

    # Set
    i = Set(m, name="i", records=data["CUSTNO."], description="customers")
    j = Alias(m, name="j", alias_with=i)
    r = Set(
        m,
        name="r",
        records=["r" + str(i) for i in range(vehicle_number)],
        description="vehicles",
    )

    # Data
    q = Parameter(
        m,
        name="q",
        domain=[i],
        records=data[["CUSTNO.", "DEMAND"]],
        description="demand of customer ci",
    )
    s = Parameter(
        m,
        name="s",
        domain=[i],
        records=data[["CUSTNO.", "SERVICETIME"]],
        description="service time of customer ci",
    )
    e = Parameter(
        m,
        name="e",
        domain=[i],
        records=data[["CUSTNO.", "READYTIME"]],
        description="earliest time to start servicing customer ci",
    )
    l = Parameter(
        m,
        name="l",
        domain=[i],
        records=data[["CUSTNO.", "DUEDATE"]],
        description="latest time to start servicing customer ci",
    )
    d = Parameter(
        m,
        name="d",
        domain=[i, j],
        records=distance_matrix,
        description="distance from customer ci to cj",
    )

    # Variable
    a = Variable(
        m,
        name="a",
        type="binary",
        domain=[i, j, r],
        description="states if vehicle r travels from ci to cj",
    )
    b = Variable(
        m,
        name="b",
        type="binary",
        domain=[i, r],
        description="states if customer ci is serviced by vehicle r",
    )
    t = Variable(
        m,
        name="t",
        domain=[i],
        description="start time of service for customer ci",
    )
    w = Variable(
        m,
        name="w",
        type="positive",
        domain=[i],
        description="waiting time of customer ci: ei-ti",
    )

    # Equations
    eq_arc_1 = Equation(
        m,
        name="eq_arc_1",
        domain=[r, j],
        description="exactly one arc leaves and enters each ci",
    )
    eq_arc_2 = Equation(
        m,
        name="eq_arc_2",
        domain=[r, i],
        description="exactly one arc leaves and enters each ci",
    )
    capacity = Equation(
        m,
        name="capacity",
        domain=[r],
        description="each vehicle must not be loaded with more than its capacity",
    )
    only_one_visit = Equation(
        m,
        name="only_one_visit",
        domain=[i],
        description="each ci only served by one vehicle",
    )
    start_at_depot = Equation(
        m, name="start_at_depot", description="all routes start at depot"
    )
    start_time = Equation(
        m,
        name="start_time",
        domain=[i, j],
        description="defines the time at which customer ci starts to be serviced",
    )
    serviced_after_earliest = Equation(
        m,
        name="serviced_after_earliest",
        domain=[i],
        description="check that the start_time is in the timeframe",
    )
    serviced_before_latest = Equation(
        m,
        name="serviced_before_latest",
        domain=[i],
        description="check that the start_time is in the timeframe",
    )
    waiting_time = Equation(
        m,
        name="waiting_time",
        domain=[i],
        description="waiting time of customer ci: ei-ti",
    )

    eq_arc_1[r,j] = Sum(i, a[i, j, r]) == b[j, r]
    eq_arc_2[r,i] = Sum(j, a[i, j, r]) == b[i, r]
    capacity[r] = Sum(i, b[i, r] * q[i]) <= vehicle_capacity
    only_one_visit[i] = Sum(r, b[i, r]) == 1
    start_at_depot[...] = Sum(Domain(r,i).where[Ord(i)==0], b[i, r]) == vehicle_number
    start_time[i,j].where[Ord(i) != Ord(j)] = t[i] +  w[i] + s[i] + d[i, j] == t[j]
    serviced_after_earliest[i] = e[i] <= t[i]
    serviced_before_latest[i] = t[i] <= l[i]
    waiting_time[i] = w[i] == e[i]-t[i]

    obj = Sum([i, j, r], a[i, j, r] * d[i, j])

    vrptw = Model(
        m,
        name="vrptw",
        equations=m.getEquations(),
        problem="MIP",
        sense=Sense.MIN,
        objective=obj
    )
    vrptw.solve(solver="CPLEX", output=sys.stdout)


if __name__ == "__main__":
    main()
