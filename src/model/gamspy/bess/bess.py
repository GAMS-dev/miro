import pandas as pd
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
    Options,
    ModelStatus,
    SolveStatus,
)


def main():
    m = Container()

    # Generator parameters
    generator_specifications_input = pd.DataFrame(
        [
            ["gen0", 1.1, 220, 50, 100, 4, 2],
            ["gen1", 1.3, 290, 80, 190, 4, 2],
            ["gen2", 0.9, 200, 10, 70, 4, 2],
        ],
        columns=[
            "i",
            "cost_per_unit",
            "fixed_cost",
            "min_power_output",
            "max_power_output",
            "min_up_time",
            "min_down_time",
        ],
    )

    # Load demand to be fulfilled by the energy management system
    # combine with cost external grid, to have one source of truth for the hours (Set j)
    timewise_load_demand_and_cost_external_grid_input = pd.DataFrame(
        [
            ["hour00", 200, 1.5],
            ["hour01", 180, 1.0],
            ["hour02", 170, 1.0],
            ["hour03", 160, 1.0],
            ["hour04", 150, 1.0],
            ["hour05", 170, 1.0],
            ["hour06", 190, 1.2],
            ["hour07", 210, 1.8],
            ["hour08", 290, 2.1],
            ["hour09", 360, 1.9],
            ["hour10", 370, 1.8],
            ["hour11", 350, 1.6],
            ["hour12", 310, 1.6],
            ["hour13", 340, 1.6],
            ["hour14", 390, 1.8],
            ["hour15", 400, 1.9],
            ["hour16", 420, 2.1],
            ["hour17", 500, 3.0],
            ["hour18", 440, 2.1],
            ["hour19", 430, 1.9],
            ["hour20", 420, 1.8],
            ["hour21", 380, 1.6],
            ["hour22", 340, 1.2],
            ["hour23", 320, 1.2],
        ],
        columns=["j", "load_demand", "cost_external_grid"],
    )

    # Set
    i = Set(
        m,
        name="i",
        description="generators",
    )
    j = Set(
        m,
        name="j",
        description="hours",
    )
    t = Alias(m, name="t", alias_with=j)

    generator_spec_header = Set(
        m,
        name="generator_spec_header",
        records=[
            "cost_per_unit",
            "fixed_cost",
            "min_power_output",
            "max_power_output",
            "min_up_time",
            "min_down_time",
        ],
    )

    timewise_header = Set(
        m, name="timewise_header", records=["load_demand", "cost_external_grid"]
    )

    # Data
    # Generator parameters
    generator_specifications = Parameter(
        m,
        name="generator_specifications",
        domain=[i, generator_spec_header],
        domain_forwarding=[True, False],
        records=generator_specifications_input.melt(
            id_vars="i", var_name="generator_spec_header"
        ),
        is_miro_input=True,
        is_miro_table=True,
        description="Specifications of each generator",
    )

    # To improve readability of the equations we extract the individual columns.
    # Since we want a single source of truth we combine them for MIRO.
    gen_cost_per_unit = Parameter(
        m,
        name="gen_cost_per_unit",
        domain=[i],
        description="cost per unit of generator i",
    )

    gen_fixed_cost = Parameter(
        m, name="gen_fixed_cost", domain=[i], description="fixed cost of generator i"
    )

    gen_min_power_output = Parameter(
        m,
        name="gen_min_power_output",
        domain=[i],
        description="minimal power output of generator i",
    )

    gen_max_power_output = Parameter(
        m,
        name="gen_max_power_output",
        domain=[i],
        description="maximal power output of generator i",
    )

    gen_min_up_time = Parameter(
        m,
        name="gen_min_up_time",
        domain=[i],
        description="minimal up time of generator i",
    )

    gen_min_down_time = Parameter(
        m,
        name="gen_min_down_time",
        domain=[i],
        description="minimal down time of generator i",
    )

    gen_cost_per_unit[i] = generator_specifications[i, "cost_per_unit"]
    gen_fixed_cost[i] = generator_specifications[i, "fixed_cost"]
    gen_min_power_output[i] = generator_specifications[i, "min_power_output"]
    gen_max_power_output[i] = generator_specifications[i, "max_power_output"]
    gen_min_up_time[i] = generator_specifications[i, "min_up_time"]
    gen_min_down_time[i] = generator_specifications[i, "min_down_time"]

    # Battery parameters
    cost_bat_power = Parameter(m, "cost_bat_power", records=1, is_miro_input=True)
    cost_bat_energy = Parameter(m, "cost_bat_energy", records=2, is_miro_input=True)

    # Load demand and external grid
    timewise_load_demand_and_cost_external_grid_data = Parameter(
        m,
        name="timewise_load_demand_and_cost_external_grid_data",
        domain=[j, timewise_header],
        domain_forwarding=[True, False],
        records=timewise_load_demand_and_cost_external_grid_input.melt(
            id_vars="j", var_name="timewise_header"
        ),
        is_miro_input=True,
        is_miro_table=True,
        description="Timeline for load demand and cost of the external grid.",
    )

    load_demand = Parameter(
        m, name="load_demand", domain=[j], description="load demand at hour j"
    )

    cost_external_grid = Parameter(
        m,
        name="cost_external_grid",
        domain=[j],
        description="cost of the external grid at hour j",
    )

    load_demand[j] = timewise_load_demand_and_cost_external_grid_data[j, "load_demand"]
    cost_external_grid[j] = timewise_load_demand_and_cost_external_grid_data[
        j, "cost_external_grid"
    ]

    max_input_external_grid = Parameter(
        m,
        name="max_input_external_grid",
        records=10,
        is_miro_input=True,
        description="maximal power that can be imported from the external grid every hour",
    )

    no_negative_gen_spec = generator_specifications.records[
        generator_specifications.records["value"] < 0
    ]
    no_negative_load = load_demand.records[load_demand.records["value"] < 0]
    no_negative_cost = cost_external_grid.records[
        cost_external_grid.records["value"] < 0
    ]

    print(
        """------------------------------------\n       Validating data\n------------------------------------\n"""
    )
    errors = False

    if not no_negative_gen_spec.empty:
        print(
            "generator_specifications:: No negative values for the generator specifications allowed!\n"
        )
        for _, row in no_negative_gen_spec.iterrows():
            print(f'{row["i"]} has a negative value.\n')
        errors = True

    if not no_negative_load.empty:
        print(
            "timewise_load_demand_and_cost_external_grid_data:: No negative load demand allowed!\n"
        )
        for _, row in no_negative_load.iterrows():
            print(f'{row["j"]} has negative load demand.\n')
        errors = True

    if not no_negative_cost.empty:
        print(
            "timewise_load_demand_and_cost_external_grid_data:: No negative cost allowed!\n"
        )
        for _, row in no_negative_cost.iterrows():
            print(f'{row["j"]} has negative external grid cost.\n')
        errors = True

    if errors:
        raise Exception("Data errors detected")
    print("Data ok\n")

    # Variable
    # Generator
    gen_power = Variable(
        m,
        name="gen_power",
        type="positive",
        domain=[i, j],
        description="Dispatched power from generator i at hour j",
        is_miro_output=True,
    )

    gen_active = Variable(
        m,
        name="gen_active",
        type="binary",
        domain=[i, j],
        description="is generator i active at hour j",
    )

    # Battery
    battery_power = Variable(
        m,
        name="battery_power",
        domain=[j],
        description="power charged or discharged from the battery at hour j",
        is_miro_output=True,
    )

    battery_delivery_rate = Variable(
        m,
        name="battery_delivery_rate",
        description="power (delivery) rate of the battery energy system",
        is_miro_output=True,
    )

    battery_storage = Variable(
        m,
        name="battery_storage",
        description="energy (storage) rate of the battery energy system",
        is_miro_output=True,
    )

    # External grid
    external_grid_power = Variable(
        m,
        name="external_grid_power",
        type="positive",
        domain=[j],
        description="power imported from the external grid at hour j",
        is_miro_output=True,
    )

    # Equation
    fulfill_load = Equation(
        m,
        name="fulfill_load",
        domain=[j],
        description="load balance needs to be met very hour j",
    )

    gen_above_min_power = Equation(
        m,
        name="gen_above_min_power",
        domain=[i, j],
        description="generators power should be above the minimal output",
    )

    gen_below_max_power = Equation(
        m,
        name="gen_below_max_power",
        domain=[i, j],
        description="generators power should be below the maximal output",
    )

    gen_above_min_down_time = Equation(
        m,
        name="gen_above_min_down_time",
        domain=[i, j],
        description="generators down time should be above the minimal down time",
    )

    gen_above_min_up_time = Equation(
        m,
        name="gen_above_min_up_time",
        domain=[i, j],
        description="generators up time should be above the minimal up time",
    )

    battery_above_min_delivery = Equation(
        m,
        name="battery_above_min_delivery",
        domain=[j],
        description="battery delivery rate (charge rate) above min power rate",
    )

    battery_below_max_delivery = Equation(
        m,
        name="battery_below_max_delivery",
        domain=[j],
        description="battery delivery rate below max power rate",
    )

    battery_above_min_storage = Equation(
        m,
        name="battery_above_min_storage",
        domain=[t],
        description="battery storage above negative energy rate (since negative power charges the battery)",
    )

    battery_below_max_storage = Equation(
        m,
        name="battery_below_max_storage",
        domain=[t],
        description="sum over battery delivery below zero (cant deliver energy that is not stored)",
    )

    external_power_upper_limit = Equation(
        m,
        name="external_power_upper_limit",
        domain=[j],
        description=" input from the external grid is limited",
    )

    fulfill_load[j] = (
        Sum(i, gen_power[i, j]) + battery_power[j] + external_grid_power[j]
        == load_demand[j]
    )

    gen_above_min_power[i, j] = (
        gen_min_power_output[i] * gen_active[i, j] <= gen_power[i, j]
    )

    gen_below_max_power[i, j] = (
        gen_power[i, j] <= gen_max_power_output[i] * gen_active[i, j]
    )

    # if j=0 -> j.lag(1) = 0 which doesn't brake the equation,
    # since generator is of at start, resulting in negative right side, therefore the sum is always above
    gen_above_min_down_time[i, j] = Sum(
        t.where[(Ord(t) >= Ord(j)) & (Ord(t) <= (Ord(j) + gen_min_down_time[i] - 1))],
        1 - gen_active[i, t],
    ) >= gen_min_down_time[i] * (gen_active[i, j.lag(1)] - gen_active[i, j])

    # and for up it correctly starts the check that if its turned on in the first step
    # it has to stay on for the min up time
    gen_above_min_up_time[i, j] = Sum(
        t.where[(Ord(t) >= Ord(j)) & (Ord(t) <= (Ord(j) + gen_min_up_time[i] - 1))],
        gen_active[i, t],
    ) >= gen_min_up_time[i] * (gen_active[i, j] - gen_active[i, j.lag(1)])

    battery_above_min_delivery[j] = -battery_delivery_rate <= battery_power[j]

    battery_below_max_delivery[j] = battery_power[j] <= battery_delivery_rate

    battery_above_min_storage[t] = -battery_storage <= Sum(
        j.where[Ord(j) <= Ord(t)], battery_power[j]
    )

    battery_below_max_storage[t] = Sum(j.where[Ord(j) <= Ord(t)], battery_power[j]) <= 0

    external_power_upper_limit[j] = external_grid_power[j] <= max_input_external_grid

    obj = (
        Sum(
            j,
            Sum(i, gen_cost_per_unit[i] * gen_power[i, j] + gen_fixed_cost[i])
            + cost_external_grid[j] * external_grid_power[j],
        )
        + cost_bat_power * battery_delivery_rate
        + cost_bat_energy * battery_storage
    )

    # Solve
    bess = Model(
        m,
        name="bess",
        equations=m.getEquations(),
        problem="MIP",
        sense=Sense.MIN,
        objective=obj,
    )

    bess.solve(
        solver="CPLEX",
        output=sys.stdout,
        options=Options(equation_listing_limit=1, relative_optimality_gap=0),
    )

    if bess.solve_status not in [
        SolveStatus.NormalCompletion,
        SolveStatus.TerminatedBySolver,
    ] or bess.status not in [ModelStatus.OptimalGlobal, ModelStatus.Integer]:
        print("No solution exists for your input data.\n")
        raise Exception("Infeasible.")

    # Extract the output data

    # Power output
    power_output_header = Set(
        m,
        name="power_output_header",
        records=["battery", "external_grid", "generators", "load_demand"],
    )

    report_output = Parameter(
        m,
        name="report_output",
        domain=[j, power_output_header],
        description="Optimal combination of incoming power flows",
        is_miro_output=True,
    )

    report_output[j, "generators"] = Sum(i, gen_power.l[i, j])
    report_output[j, "battery"] = battery_power.l[j]
    report_output[j, "external_grid"] = external_grid_power.l[j]
    report_output[j, "load_demand"] = load_demand[j]

    # Costs
    total_cost_gen = Parameter(
        m,
        "total_cost_gen",
        is_miro_output=True,
        description="Total cost of the generators",
    )

    total_cost_gen[...] = Sum(
        j, Sum(i, gen_cost_per_unit[i] * gen_power.l[i, j] + gen_fixed_cost[i])
    )

    total_cost_battery = Parameter(
        m,
        "total_cost_battery",
        is_miro_output=True,
        description="Total cost of the BESS",
    )

    total_cost_battery[...] = (
        cost_bat_power * battery_delivery_rate.l + cost_bat_energy * battery_storage.l
    )

    total_cost_extern = Parameter(
        m,
        "total_cost_extern",
        is_miro_output=True,
        description="Total cost for the imported power",
    )

    total_cost_extern[...] = Sum(
        j,
        cost_external_grid[j] * external_grid_power.l[j],
    )

    total_cost = Parameter(
        m,
        "total_cost",
        is_miro_output=True,
        description="Total cost to fulfill the load demand",
    )

    total_cost[...] = total_cost_gen + total_cost_battery + total_cost_extern


if __name__ == "__main__":
    main()
