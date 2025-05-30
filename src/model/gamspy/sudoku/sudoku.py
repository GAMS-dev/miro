import sys
import os
import numpy as np

from gamspy import Container, Equation, Model, Parameter, Sense, Set, Sum, Variable, Ord, Number, ModelStatus, SolveStatus
from gamspy.math import mod


def main():
    m = Container()

    # Set
    col = Set(m, "col", records=[("col" + str(i), i) for i in range(1, 10)])
    row = Set(m, "row", records=[("row" + str(i), i) for i in range(1, 10)])
    val = Set(m, "val", records=[(str(i), i) for i in range(1, 10)])
    quad = Set(m, "quad", records=[("quad" + str(i), i) for i in range(1, 10)])
    rcquad_map = Set(m, "rcquad_map_data", domain=[quad, col, row],
                     records=[("quad1",  "col" + str(i), "row" + str(j)) for i in range(1, 4) for j in range(1, 4)] +
                     [("quad2",  "col" + str(i), "row" + str(j)) for i in range(4, 7) for j in range(1, 4)] +
                     [("quad3",  "col" + str(i), "row" + str(j)) for i in range(7, 10) for j in range(1, 4)] +
                     [("quad4",  "col" + str(i), "row" + str(j)) for i in range(1, 4) for j in range(4, 7)] +
                     [("quad5",  "col" + str(i), "row" + str(j)) for i in range(4, 7) for j in range(4, 7)] +
                     [("quad6",  "col" + str(i), "row" + str(j)) for i in range(7, 10) for j in range(4, 7)] +
                     [("quad7",  "col" + str(i), "row" + str(j)) for i in range(1, 4) for j in range(7, 10)] +
                     [("quad8",  "col" + str(i), "row" + str(j)) for i in range(4, 7) for j in range(7, 10)] +
                     [("quad9",  "col" + str(i), "row" + str(j)) for i in range(7, 10) for j in range(7, 10)])

    # Data
    force_unique_sol = Parameter(
        m, "force_unique_sol", records=1, is_miro_input=True)
    initial_state_data = np.array(
        [
            [0, 0, 0, 0, 8, 6, 0, 0, 0],
            [0, 7, 0, 9, 0, 2, 0, 0, 0],
            [6, 9, 0, 0, 0, 0, 2, 0, 8],
            [8, 0, 0, 0, 9, 0, 7, 0, 2],
            [4, 0, 0, 0, 0, 0, 0, 0, 3],
            [2, 0, 9, 0, 1, 0, 0, 0, 4],
            [5, 0, 3, 0, 0, 0, 0, 7, 6],
            [0, 0, 0, 5, 0, 8, 0, 2, 0],
            [0, 0, 0, 3, 7, 0, 0, 0, 0],
        ],
    )

    initial_state = Parameter(m, "initial_state", domain=[
                              row, col], is_miro_input=True, is_miro_table=True, records=initial_state_data)

    error01 = Set(m, "error01", domain=[row, col])
    error01[row, col] = (initial_state[row, col] < 0) | (
        initial_state[row, col] > 9) | (mod(initial_state[row, col], 1) != 0)

    print("""------------------------------------
        Validating data
------------------------------------""")
    if error01.records is not None:
        print("initial_state:: Digits must be integers between 0 and 9!")
        initial_state_data = initial_state.records
        for _, row in error01.records.iterrows():
            value = initial_state_data[(initial_state_data["row"] == row["row"]) & (
                initial_state_data["col"] == row["col"])]["value"].values[0]
            print(f'Cell {row["row"]}:{row["col"]} has invalid value of {value}')
        raise Exception("Data errors detected")
    print("Data ok")

    # Variable
    x = Variable(m, "x", domain=[col, row, val], type="binary")
    z = Variable(m, "z")

    # Equations
    eq_z = Equation(m, "eq_z")
    eq_col = Equation(m, "eq_col", domain=[col, val])
    eq_row = Equation(m, "eq_row", domain=[row, val])
    eq_quad = Equation(m, "eq_quad", domain=[quad, val])
    eq_val = Equation(m, "eq_val", domain=[col, row])

    eq_z[...] = 1 == z
    eq_col[col, val] = Sum(row, x[col, row, val]) == 1
    eq_row[row, val] = Sum(col, x[col, row, val]) == 1
    eq_quad[quad, val] = Sum(rcquad_map[quad, col, row], x[col, row, val]) == 1
    eq_val[col, row] = Sum(val, x[col, row, val]) == 1

    x.fx[col, row, val].where[initial_state[row, col] == Ord(val)] = 1

    sudoku = Model(m, name="sudoku", equations=m.getEquations(),
                   problem="MIP", sense=Sense.MIN, objective=z)

    sudoku.solve(solver="CPLEX", solver_options={"solnpool": "solnpool.gdx",
                                                 "solnpoolintensity": "4",
                                                 "solnpoolpop": "2"})

    if sudoku.solve_status not in [SolveStatus.NormalCompletion, SolveStatus.TerminatedBySolver] or \
            sudoku.status not in [ModelStatus.OptimalGlobal, ModelStatus.Integer]:
        raise Exception("No solution exists for your input data.")

    if force_unique_sol.toValue():
        m_solnpool = Container("solnpool.gdx")
        if len(m_solnpool["index"].records.index) > 1:
            raise Exception(
                "The solution to the input data you provided is not unique!")

    results = Parameter(m, "results", domain=[
                        row, col], is_miro_output=True, is_miro_table=True)
    results[row, col] = Sum(val.where[x.l[col, row, val]], Ord(
        val)) * (1 - Number(2).where[initial_state[row, col] > 0.5])


if __name__ == "__main__":
    main()
