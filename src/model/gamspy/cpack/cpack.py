"""
Packing identical size circles in the unit circle (CPACK)

Given the unit circle (of radius 1), find a set of identical
size circles with an optimized (maximal) radius r so that all
such circles are contained by the unit circle, in a non-overlapping
arrangement.

A test example from  the LGO library


Pinter, J D, Nonlinear optimization with GAMS/LGO.
Journal of Global Optimization 38 (2007), 79-101.

Keywords: quadratic constraint programming, circle packing problem, mathematics
"""

import sys

from gamspy import (
    Alias,
    Container,
    Equation,
    Model,
    Options,
    Ord,
    Problem,
    Sense,
    Set,
    Variable,
    Parameter,
    SpecialValues,
)
from gamspy.math import sqr, sqrt
import math


def main():
    c = Container()
    k = Parameter(
        c,
        name="k",
        description="number of circles",
        records=5,
        is_miro_input=True,
    )
    optcr = Parameter(
        c,
        name="optcrVal",
        description="optcr",
        records=0.01,
        is_miro_input=True,
    )
    reslim = Parameter(
        c,
        name="reslimVal",
        description="reslim",
        records=10,
        is_miro_input=True,
    )
    qcp_solver = Set(
        c,
        "qcp",
        description="QCP solver to use",
        is_singleton=True,
        records=["conopt"],
        is_miro_input=True,
    )

    # Set
    i = Set(
        c,
        name="i",
        description="circles",
        records=[str(i) for i in range(int(k.toValue()))],
    )
    j = Alias(c, name="j", alias_with=i)
    ij = Set(c, name="ij", domain=[i, j])
    ij[i, j].where[Ord(i) < Ord(j)] = True

    # Variables
    r = Variable(c, name="r", description="radius of circles")
    x = Variable(c, name="x", domain=[i], description="abscissa of circle")
    y = Variable(c, name="y", domain=[i], description="ordinate of circle")

    # Equations
    circumscribe = Equation(
        c,
        name="circumscribe",
        domain=[i],
        description="enforce circle is enclosed in unit circle",
    )
    circumscribe[i] = sqr(1 - r) >= sqr(x[i]) + sqr(y[i])

    nonoverlap = Equation(
        c,
        name="nonoverlap",
        domain=[i, j],
        description="enforce that circles do not overlap",
    )
    nonoverlap[ij[i, j]] = sqr(x[i] - x[j]) + sqr(y[i] - y[j]) >= 4 * sqr(r)

    x.lo[i] = -1
    x.up[i] = 1
    y.lo[i] = -1
    y.up[i] = 1

    r.l[...] = 1 / k
    x.l[i] = -1 + (2 * Ord(i) - 1) * r.l
    y.l[i] = 0

    r.lo[...] = r.l[...]
    r.up[...] = sqrt(1 / k)

    m = Model(
        c,
        name="cpack",
        equations=c.getEquations(),
        problem=Problem.QCP,
        sense=Sense.MAX,
        objective=r,
    )

    # solve with a good global solver
    qcp_solver_val = qcp_solver.records.loc[0, "uni"]
    if qcp_solver_val == "gurobi":
        solver_options = {"nonconvex": 2}
    else:
        solver_options = None
    m.solve(
        solver=qcp_solver_val,
        options=Options(
            relative_optimality_gap=optcr.toValue(),
            time_limit=reslim.toValue(),
        ),
        output=sys.stdout,
        solver_options=solver_options,
    )
    circle_data_header = Set(
        c, "circleDataHeader", records=["x", "y", "radius"]
    )

    cirlce_data = Parameter(
        c,
        "circleData",
        domain=["*", circle_data_header],
        is_miro_output=True,
        is_miro_table=True,
    )

    cirlce_data["outerCircle", "x"] = SpecialValues.EPS
    cirlce_data["outerCircle", "y"] = SpecialValues.EPS
    cirlce_data["outerCircle", "radius"] = 1
    cirlce_data[i, "x"] = x.l[i] + SpecialValues.EPS
    cirlce_data[i, "y"] = y.l[i] + SpecialValues.EPS
    cirlce_data[i, "radius"] = r.l + SpecialValues.EPS

    with open("cpack_miroLog.dat", "w") as f:
        f.writelines(
            [
                "------------------------------------\n",
                "        Circle Packing MIRO Report\n",
                "------------------------------------\n",
                f"Outer Circle Area          : {round(math.pi, 4)}\n",
                (
                    "Area covered by start point:"
                    f" {round(len(i.records.index) * math.pi*((1/k.toValue())**2), 4)}\n"
                ),
                (
                    "Area covered by solution   :"
                    f" {round(len(i.records.index) * math.pi*(r.records.level.loc[0]**2), 4)}"
                ),
            ]
        )


if __name__ == "__main__":
    main()
