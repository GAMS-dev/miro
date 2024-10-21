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
)


def main():
    m = Container()

    # Generator parameters
    generator_specifications_input = pd.DataFrame(
        [
            ["gen0", 0.010694, 142.7348, 30, 70, 8, 6],
            ["gen1", 0.018761, 168.9075, 50, 100, 8, 6],
            ["gen2", 0.0076121, 313.9102, 30, 120, 8, 6],
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
            ["hour000", 200, 0.09],
            ["hour001", 180, 0.08],
            ["hour002", 170, 0.08],
            ["hour003", 160, 0.08],
            ["hour004", 150, 0.08],
            ["hour005", 150, 0.08],
            ["hour006", 170, 0.09],
            ["hour007", 250, 0.11],
            ["hour008", 320, 0.13],
            ["hour009", 300, 0.12],
            ["hour010", 280, 0.11],
            ["hour011", 260, 0.10],
            ["hour012", 270, 0.10],
            ["hour013", 280, 0.10],
            ["hour014", 290, 0.11],
            ["hour015", 300, 0.12],
            ["hour016", 320, 0.13],
            ["hour017", 350, 0.14],
            ["hour018", 340, 0.13],
            ["hour019", 330, 0.12],
            ["hour020", 320, 0.11],
            ["hour021", 280, 0.10],
            ["hour022", 240, 0.09],
            ["hour023", 220, 0.09],
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
    cost_bat_power = Parameter(m, "cost_bat_power", records=0.2, is_miro_input=True)

    cost_bat_energy = Parameter(m, "cost_bat_energy", records=0.25, is_miro_input=True)

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
        records=15,
        is_miro_input=True,
        description="maximal power that can be imported from the external grid every hour",
    )

    # Varaible
    # Generator
    gen_power = Variable(
        m,
        name="gen_power",
        type="positive",
        domain=[i, j],
        description="dispatch power from generator i at hour j",
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
    )

    battery_delivery_rate = Variable(
        m,
        name="battery_delivery_rate",
        description="power (delivery) rate of the battery energy system",
        is_miro_output=True,
    )

    battery_stoarge = Variable(
        m,
        name="battery_stoarge",
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
        description="generators power should be above the minimal ouput",
    )

    gen_below_max_power = Equation(
        m,
        name="gen_below_max_power",
        domain=[i, j],
        description="generators power should be below the maximal ouput",
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
        description="battery storage above negative enegry rate (since negative power charges the battery)",
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
        description=" imput from the external grid is limited",
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

    # if j=0 -> j.lag(1) = 0 which dosen't brack the equation,
    # since generator is of at start, resulting in negative right side, therfore the sum is always above
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

    battery_above_min_storage[t] = -battery_stoarge <= Sum(
        j.where[Ord(j) <= Ord(t)], battery_power[j]
    )

    battery_below_max_storage[t] = Sum(j.where[Ord(j) <= Ord(t)], battery_power[j]) <= 0

    external_power_upper_limit[j] = external_grid_power[j] <= max_input_external_grid

    obj = (
        Sum(
            j,
            Sum(i, gen_cost_per_unit[i] * gen_power[i, j])  # + gen_fixed_cost[i])
            + cost_external_grid[j] * external_grid_power[j],
        )
        + cost_bat_power * battery_delivery_rate
        + cost_bat_energy * battery_stoarge
    )

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

    power_output_header = Set(
        m,
        name="power_output_header",
        records=["battery", "external_grid", "generators", "load_demand"],
    )

    power_output = Parameter(
        m,
        name="report_output",
        domain=[j, power_output_header],
        description="optimal combination of incoming power flows",
        is_miro_output=True,
    )

    power_output[j, "generators"] = Sum(i, gen_power.l[i, j])
    power_output[j, "battery"] = battery_power.l[j]
    power_output[j, "external_grid"] = external_grid_power.l[j]
    power_output[j, "load_demand"] = load_demand[j]

if __name__ == "__main__":
    main()
