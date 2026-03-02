# Module Specification: Universal Scenario Generator & Shiny Integration (govhrcast_06)

## 1. Objective
Create a high-performance batch wrapper, `generate_scenario_matrix()`, that runs `simulate_horizon()` across a "dense grid" of any available policy levers. The output must be a "long-flat" `data.table` optimized for real-time filtering and multi-scenario plotting in a Shiny dashboard.

## 2. Universal Input Mapping (The Grid)
The generator must be agnostic to the specific country data (e.g., Brazil, Zambia, etc.) and focus on the universal Simulation API.

### Logic:
* **Grid Generation**: Use `data.table::CJ()` to create every possible combination of inputs provided in a `param_grid` list.
* **Levers Supported**: The generator must dynamically map grid values to any parameter accepted by the underlying modules:
    * **Orchestrator Args**: `n_periods`, `salary_growth_rate` (COLA).
    * **Module Params**: `retirement` (age, multiplier), `movement` (rate, strategy), `hiring` (target_mode, replacement_rate).
* **Labeling**: For each unique combination, generate a human-readable `scenario_label` (e.g., "Growth: 3% | Hire: 0.5 | Ret_Age: 60") and a unique `scenario_id`.

## 3. High-Performance Batch Execution
To support 1,000s of runs across long horizons (e.g., 20 years):
* **Parallel Processing**: Use `future.apply` or `parallel::mclapply`. 
* **Worker Isolation**: Each worker must `copy()` the base data. Ensure `setDTthreads(1)` is called inside workers to avoid thread-collision.
* **Memory Management**: Drop all microdata (`personnel_dt`, `contract_dt`) inside the worker loop. Only return the summarized "Drivers of Change" table.
* **Dry Run**: Execute the first scenario in the grid as a "smoke test" before launching the full parallel batch.

## 4. Wage Bill Decomposition & Shiny Readiness
The output must preserve the "Financial Identity" of each simulation for side-by-side comparison.

### Output Schema (`all_scenarios_dt`):
* **Metadata**: All input parameters used for that specific run.
* **Time-Series**: All columns from `compute_wage_bill_summary()`:
    * `year`, `base_bill`, `total_wage_bill`.
    * **Drivers**: `exit_savings`, `promotion_effect`, `hiring_effect`, `inflation_effect`.
* **Comparison Metrics**: 
    * `change_share_pct` for each driver.
    * A "Baseline" flag for the scenario where all multipliers are 1.0.

## 5. Examples of Policy Mapping (e.g., Zambia Use-Case)
The function must be flexible enough to simulate specific reforms by passing ranges for the following universal levers:
* **Recruitment Variations**: Map "Hiring Freeze" by setting `hiring_multiplier = 0` or "Mass Recruitment" by setting high `hiring_target` values.
* **Pay Reforms**: Pass modified `salary_scale_dt` objects or varied `salary_growth_rate` vectors.
* **Promotion Policy**: Toggle `promotion_strategy` (tenure vs. wage) and `promotion_multiplier`.
* **Establishment Changes**: Adjust `hiring_target_mode` from "Flow" (replacement) to "Stock" (fixed headcount target).

## 6. Technical Constraints
* **No Hardcoding**: All column names and group variables must remain parameterized.
* **Stochastic Rounding**: Maintain integer-based personnel counts in all aggregated results.
* **Accounting Identity**: Every row in the resulting matrix must pass the validation: 
  `base_bill + exit_savings + promotion_effect + hiring_effect + inflation_effect == total_wage_bill`.

## 7. Example Usage for Shiny
```r
# Define the dense set of levers
grid <- list(
  salary_growth_rate = seq(0.02, 0.10, by = 0.02),
  hiring_multiplier = c(0, 0.5, 1.0),
  retirement_age_min = c(55, 60)
)

# Generate and Plot
results <- generate_scenario_matrix(base_data, grid, n_periods = 20)
# ggplot(results[scenario_id %in% c(1, 5)], aes(x=year, y=total_wage_bill, color=scenario_label))