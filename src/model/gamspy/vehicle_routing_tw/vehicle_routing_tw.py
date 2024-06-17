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
    Domain,
)
from gamspy.math import sin, cos, sqrt, sqr, atan2


def main():
    m = Container()

    # data = pd.read_csv("r101_solomon.txt", sep=",")
    data = pd.read_csv("small_data.txt", sep=",", index_col=False)

    # compute euclidean distance matrix between all customers
    coordinates = data[["lat", "lng"]].to_numpy()
    distance_matrix = np.sqrt(
        np.einsum("ijk->ij", (coordinates[:, None, :] - coordinates) ** 2)
    )

    # Set
    i = Set(m, name="i", records=data["i"], description="customers")
    j = Alias(m, name="j", alias_with=i)

    vehicle_number = Parameter(
        m,
        "vehicle_number",
        records=10,
        is_miro_input=True,
    )
    k = Set(
        m,
        name="k",
        records=["k" + str(i) for i in range(int(vehicle_number.toValue()))],
        description="vehicles",
    )

    customerDataHeader = Set(
        m,
        "customerDataHeader",
        records=["lat", "lng", "demand", "readyTime", "dueDate", "serviceTime"],
    )

    # Data
    customerData = Parameter(
        m,
        "customerData",
        domain=[i, customerDataHeader],
        domain_forwarding=[True, False],
        records=data.melt(id_vars="i", var_name="customerDataHeader"),
        is_miro_input=True,
        is_miro_table=True,
        description="Cusotmer information",
    )

    vehicle_capacity = Parameter(
        m,
        "vehicle_capacity",
        records=200,
        is_miro_input=True,
    )

    q = Parameter(
        m,
        name="q",
        domain=[i],
        description="demand of customer ci",
    )
    s = Parameter(
        m,
        name="s",
        domain=[i],
        description="service time of customer ci",
    )
    e = Parameter(
        m,
        name="e",
        domain=[i],
        description="earliest time to start servicing customer ci",
    )
    l = Parameter(
        m,
        name="l",
        domain=[i],
        description="latest time to start servicing customer ci",
    )
    d = Parameter(
        m,
        name="d",
        domain=[i, j],
        description="distance from customer ci to cj",
    )
    help_d = Parameter(
        m,
        name="d",
        domain=[i, j],
        description="help parameter for computing the distance from customer ci to cj",
    )

    q[i] = customerData[i, "demand"]
    s[i] = customerData[i, "serviceTime"]
    e[i] = customerData[i, "readyTime"]
    l[i] = customerData[i, "dueDate"]

    earth_radius = 6371
    help_d[i, j] = sqr(sin((customerData[i, "lat"] - customerData[j, "lat"])*np.pi / (2*180))) + (
        cos(customerData[i, "lat"]*np.pi / 180) * cos(customerData[j, "lat"]*np.pi / 180)
        * sqr(sin((customerData[i, "lng"] - customerData[j, "lng"]) *np.pi / (2*180)))
    )

    d[i, j] = (
        2
        * earth_radius
        * atan2(sqrt(help_d[i,j]), sqrt(1-help_d[i,j]))
    )

    # to lineariz the start time equation, where the slack is the allows timeframe at the depot
    M = (
        customerData.records[customerData.records["customerDataHeader"] == "dueDate"][
            "value"
        ].iloc[0]
        - customerData.records[
            customerData.records["customerDataHeader"] == "readyTime"
        ]["value"].iloc[0]
    )

    # Variable
    x = Variable(
        m,
        name="x",
        type="binary",
        domain=[i, j, k],
        description="states if vehicle k travels from ci to cj",
        is_miro_output=True,
    )
    t = Variable(
        m,
        name="t",
        type="positive",
        domain=[i],
        description="arrival time at customer ci",
        is_miro_output=True,
    )
    w = Variable(
        m,
        name="w",
        type="positive",
        domain=[i],
        description="waiting time at customer ci: ei-ti",
        is_miro_output=True,
    )

    # Equations
    max_K_starts_at_depot = Equation(
        m,
        name="max_K_starts_at_depot",
        domain=[i],
        description="maximum K routes going out of the depot",
    )
    start_at_depot = Equation(
        m, domain=[k], name="start_at_depot", description="all routes start at depot"
    )
    end_at_depot = Equation(
        m, domain=[k], name="end_at_depot", description="all routes end at depot"
    )
    each_node_enterd_once = Equation(
        m,
        name="each_node_enterd_once",
        domain=[i],
        description="exactly one arc enters each ci",
    )
    each_node_left_once = Equation(
        m,
        name="each_node_left_once",
        domain=[j],
        description="exactly one arc leaves each ci",
    )
    capacity = Equation(
        m,
        name="capacity",
        domain=[k],
        description="each vehicle must not be loaded with more than its capacity",
    )
    start_time_depot = Equation(
        m,
        name="start_time_depot",
        domain=[i],
        description="fix start time at depot to zero",
    )
    waiting_time_depot = Equation(
        m,
        name="waiting_time_depot",
        domain=[i],
        description="fix waiting time at depot to zero",
    )
    start_time = Equation(
        m,
        name="start_time",
        domain=[i, j, k],
        description="defines the time windows",
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
    vehicle_does_not_jump = Equation(
        m,
        name="vehicle_does_not_jump",
        domain=[k, i],
        description="if k goes into j it also has to leave j",
    )

    max_K_starts_at_depot[i] = (
        Sum(Domain(j, k).where[Ord(j) > 1], x[i, j, k]) <= vehicle_number
    )
    start_at_depot[k] = Sum(Domain(j, i).where[Ord(i) == 1], x[i, j, k]) <= 1
    end_at_depot[k] = Sum(Domain(j, i).where[Ord(i) == 1], x[j, i, k]) <= 1
    vehicle_does_not_jump[k, i] = Sum(j, x[i, j, k]) == Sum(j, x[j, i, k])
    each_node_enterd_once[i].where[Ord(i) != 1] = (
        Sum(Domain(j, k).where[Ord(i) != Ord(j)], x[i, j, k]) == 1
    )
    each_node_left_once[j].where[Ord(j) != 1] = (
        Sum(Domain(i, k).where[Ord(i) != Ord(j)], x[i, j, k]) == 1
    )
    capacity[k] = (
        Sum(Domain(i, j).where[Ord(i) != Ord(j)], x[i, j, k] * q[i]) <= vehicle_capacity
    )
    start_time_depot[i].where[Ord(i) == 1] = t[i] == 0
    waiting_time_depot[i].where[Ord(i) == 1] = w[i] == 0
    start_time[i, j, k].where[(Ord(i) != Ord(j)) & (Ord(j) != 1)] = (
        t[i] + d[i, j] + s[i] + w[i] - t[j] <= (1 - x[i, j, k]) * M
    )

    serviced_after_earliest[i] = e[i] <= t[i] + w[i]
    serviced_before_latest[i] = t[i] + w[i] <= l[i]

    obj = Sum(Domain(i, j, k).where[Ord(i) != Ord(j)], x[i, j, k] * d[i, j])

    vrptw = Model(
        m,
        name="vrptw",
        equations=m.getEquations(),
        problem="MIP",
        sense=Sense.MIN,
        objective=obj,
    )
    vrptw.solve(solver="CPLEX", output=sys.stdout)


if __name__ == "__main__":
    main()
