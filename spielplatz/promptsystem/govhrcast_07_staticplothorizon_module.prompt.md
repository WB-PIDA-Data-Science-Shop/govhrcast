
# .prompt.md

**Role:** Senior R Package Developer & Data Visualization Expert.
**Context:** Implementing the S3 object-oriented layer and a static plotting suite for the `govhrcast` package.

## 1. S3 Class Construction: The `horizon` Object
Refactor the `simulate_horizon()` output to return a formal S3 object.
- **Class Name:** `horizon`.
- **Structure:** A list containing:
    - `$comparison`: The primary `data.table` of simulation results (as shown in the shared summary table).
    - `$metadata`: A list containing the policy arguments (`policy_args`) used to generate the simulation.
- **Constructor:** Ensure the class is assigned at the end of the simulation function: `class(out) <- "horizon"`.

## 2. Generic Plotting Method: `plot.horizon()`
Implement the S3 method `plot.horizon(x, type = c("fiscal_basics", "spending_effects", "turnover"), ...)`.
- **Design Philosophy:** Prioritize **Snapshot Sums**. Do not attempt to mathematically reconcile deltas; plot the data as it exists in the `$comparison` table.
- **Dependencies:** Use `ggplot2` for core plotting and `patchwork` (or `facet_wrap`) to manage multi-panel layouts.

## 3. Visualization Specifications

### A. Fiscal Basics (`type = "fiscal_basics"`)
- **Logic:** Evolution of core costs over the horizon using a "Small Multiples" approach.
- **Layout:** Three line charts placed side-by-side (a 1x3 grid).
- **Chart 1 (Wage Bill):** `wage_bill_end` vs. `period_date`.
- **Chart 2 (Pensions):** Both `pension_cost_total` (cumulative stock) and `pension_cost_new` (period flow) on the same Y-axis to visualize the liability build-up.
- **Chart 3 (Inflation):** `inflation_effect` vs. `period_date`.



### B. Movement Spending Effects (`type = "spending_effects"`)
- **Logic:** Magnitude of individual policy drivers.
- **Layout:** Individual bar charts for the following variables:
    - `hiring_effect`
    - `promotion_effect`
    - `transfer_effect`
    - `inflation_effect`
- **Efficiency Metric:** Instead of absolute exit savings, plot `exit_savings_pct_of_end_bill`.
- **Styling:** Use a diverging color palette. Assign one color for positive spending (costs) and a distinct, contrasting color for negative values or savings metrics (efficiency).



### C. Turnover Dynamics (`type = "turnover"`)
- **Logic:** Tracking the volume and size of the workforce.
- **Metrics:** Line charts for `n_hires`, `n_exits`, and `n_headcount_end`.
- **Note:** Use facets or a secondary axis if `n_headcount_end` scale differs significantly from the flow variables (`n_hires`, `n_exits`).



## 4. Metadata & "i" Tooltips (Self-Documentation)
For every plot object generated:
- Attach a `description` attribute to the `ggplot` object.
- **Instruction:** Based on your knowledge of the `govhrcast` logic, write concise, technical definitions for these variables to be used later as tooltips.
    - *Example:* Define the difference between `pension_cost_new` (the check written for new retirees this period) and `pension_cost_total` (the total government pension payroll).

## 5. Technical Constraints
- Use `scales::label_number(scale_cut = cut_short_scale())` for currency and headcount formatting.
- Ensure the plots handle `NA` values gracefully. Ensure this is tested! 
- Return the plot object invisibly so it is compatible with `print()` or assignment to a variable.