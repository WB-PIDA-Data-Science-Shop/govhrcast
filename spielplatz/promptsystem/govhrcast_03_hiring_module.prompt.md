# GOVHRCAST Hiring Module -- Implementation Prompt

## Objective

Implement a modular hiring and workforce adjustment system consistent
with the retirement module architecture. This module is meant to follow 
from the retirement module but should be able to run independently. 

Please keep consistent argument names whenever possible. Override the 
naming conventions used here if it conflicts with names already used 
in the retirement module. Feel free to add helper functions when necessary. 

------------------------------------------------------------------------

## Core Design Principles

1.  Use `data.table` everywhere.
2.  Do not copy input data unnecessarily.
3.  Demand estimation must be pure (no state modification).
4.  State modification must happen in exactly one function.
5.  Allow both hiring and downsizing.
6.  Salary assignment must be generic and merge-based.
7.  All grouping must be abstract and defined by
    `policy_params$group_cols`.
8.  No hardcoded column names.
9.  As usual unit test all functions and ensure 0 errors, warnings and notes.

------------------------------------------------------------------------

## High-Level Architecture

simulate_hiring() ├── compute_current_stock() ├──
estimate_hiring_demand() │ ├── compute_flow_demand() │ ├──
compute_stock_demand() │ └── compute_combined_demand() ├──
update_state_with_adjustment() │ ├── generate_new_personnel() │ ├──
generate_new_contracts() │ ├── assign_compensation() │ └──
select_personnel_to_remove() └── compute_hiring_summary()

------------------------------------------------------------------------

## 1️⃣ compute_current_stock()

### Inputs

-   contract_dt
-   personnel_dt
-   ref_date
-   group_cols

### Logic

-   Identify active contracts at `ref_date`
-   Merge with personnel data
-   Aggregate by `group_cols`
-   Return: data.table with `group_cols + current_stock`

------------------------------------------------------------------------

## 2️⃣ estimate_hiring_demand()

### Inputs

-   contract_dt
-   personnel_dt 
(These two represent the current stock)

-   policy_params

### Behavior

Use: switch(policy_params\$mode)

Modes: - "flow" - "stock" - "combined"

------------------------------------------------------------------------

### compute_flow_demand()

Compute the proportion of exits via retirement + other exits in previous period

Input into this function can be the exiting stock from the retirement module or computing
retirement using govhr::detect_retirement() function on the contract level data. The other 
the exits should be computed using event_type = "fire" in the govhr::detect_personnel_event()
Use govhr::add_contract_to_event() to add all the group_cols if necessary to the exits
and retirement data. The resulting object here should be the exiting_stock data which is
counts by group_cols or overall if no group_cols are specified. 

hire_flow = exiting_stock × replacement_rate

Where `replacement_rate` may be: - Scalar - data.table matched on
group_cols

Return: group_cols + total_hires

------------------------------------------------------------------------

### compute_stock_demand()

H_stock = target_stock − current_stock

Allow negative values.

Return: group_cols + total_hires

------------------------------------------------------------------------

### compute_combined_demand()

H_flow = current_stock × replacement_rate
H_stock = target_stock − current_stock

total_hires = H_flow + H_stock

Return: group_cols + total_hires

------------------------------------------------------------------------

## 3️⃣ update_state_with_adjustment()

### Inputs

-   contract_dt
-   personnel_dt
-   adjustment_dt (group_cols + net_change)
-   policy_params
-   ref_date

### Logic

For each grouping cell:

If net_change \> 0: - generate_new_personnel() -
generate_new_contracts() - assign_compensation()

If net_change \< 0: - select_personnel_to_remove() - deactivate
contracts

Default removal strategy: - last_hired_first

State updates occur here and nowhere else.

------------------------------------------------------------------------

## 4️⃣ assign_compensation()

### Inputs

-   new_contract_dt
-   salary_scale_dt
-   join_columns specified in policy_params

### Logic

-   Merge salary scale onto contracts for the new hires
-   Validate no unmatched rows
-   Attach salary column
-   Return updated contracts

Salary scale must be fully generic and merge-based.

------------------------------------------------------------------------

## 5️⃣ compute_hiring_summary()

Return:

-   n_new_hires
-   net_headcount_change
-   total_headcount
-   total_new_salary_cost

Do not compute redundant "before/after" metrics.

------------------------------------------------------------------------

## Required policy_params Structure

policy_params \<- list( mode = "flow" \| "stock" \| "combined",
group_cols = c("department", "grade"), replacement_rate = 1.0 OR
data.table, stock_targets = data.table, salary_scale = data.table,
removal_strategy = "last_hired_first" )

------------------------------------------------------------------------

## Constraints

-   All grouping must be dynamic.
-   All joins must validate integrity. Please do not use merge use the data.table joins.
-   No side effects outside update_state_with_adjustment().
-   Vectorized operations only.
-   Compatible with yearly simulation loop.

------------------------------------------------------------------------

## Design Philosophy

Demand = structural logic\
Adjustment = state change\
Compensation = fiscal mapping\
Summary = fiscal reporting

This module must support:

-   Flow replacement policy
-   Structural stock reform
-   Combined reforms
-   Workforce downsizing
-   AI optimization layer (future)
-   Fiscal constraint layer (future)
