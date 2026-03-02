# Redesign Plan: `simulate_scenario()` / `simulate_horizon()` / `generate_scenario_matrix()`

The current design mixes wage bill measurement logic into the orchestrator loop and accumulates several correctness issues (wrong headcount filter, lost pension costs, approximated effect calculations). The redesign separates concerns cleanly: one function per period, one that iterates, one that grids.

---

## Steps

### 1. Write `simulate_scenario()` in `R/simulate_horizon.R`

A new exported single-period function with this signature and logic:

- **Snapshot A**: `wage_bill_start = sum(salary_col)` over the full `contract_dt` — no filtering, no type distinctions. If a `pensioner_register` is passed in (accumulated from prior periods), also sum its `pension_amount` and add to `wage_bill_start`. Record `n_headcount_start = nrow(contract_dt)` at this point.
- **Retirement**: call `simulate_retirement()` as-is. Compute `exit_savings` via `compute_exit_effect()` (see Step 3). Extract `pension_cost_new` from `retirees_dt$pension_amount`. Append new retirees to `pensioner_register`, adding a `period_date` column (the simulation date for this period) so `simulate_horizon()` can audit pension cohorts by year. Record `n_exits = nrow(retirees_dt)`.
- **Movements**: call `simulate_promotions_transfers()`. Compute `promotion_effect` and `transfer_effect` via `compute_movement_effect()` (see Step 3). Record `n_promotions` and `n_transfers` from `mov$summary`.
- **Hiring**: call `simulate_hiring()`. Compute `hiring_effect` via `compute_hiring_effect()` (see Step 3). Record `n_hires` from the new-hire contracts count.
- **Aging**: increment `age` and `tenure_years` for active personnel as-is.
- **COLA**: update `salary_scale_dt` and apply `growth_rate` to active contract salaries. Compute `inflation_effect` via `compute_inflation_effect()` (see Step 3).
- **Snapshot B**: `wage_bill_end = sum(salary_col)` over `contract_dt` after all steps, plus `pension_cost_total = sum(pensioner_register$pension_amount)`. Record `n_headcount_end = nrow(contract_dt)`.
- **Returns**: named list — `summary` (one-row `data.table`), `contract_dt`, `personnel_dt`, `salary_scale_dt`, `pensioner_register`. The `summary` columns are:
  - `year`
  - `n_headcount_start` — total rows in `contract_dt` at Snapshot A
  - `wage_bill_start` — sum of `salary_col` at Snapshot A (including prior pension costs)
  - `n_exits` — count of retirees this period
  - `exit_savings` — salary mass removed by retirements
  - `pension_cost_new` — new pension obligations created this period
  - `pension_cost_total` — cumulative pension obligation from all prior + current retirees
  - `n_promotions` — count of promotions applied
  - `n_transfers` — count of transfers applied
  - `promotion_effect` — net salary change from promotions (post minus pre, per mover)
  - `transfer_effect` — net salary change from transfers (post minus pre, per mover)
  - `n_hires` — count of new hire contracts created
  - `hiring_effect` — total salary cost of new hire contracts
  - `inflation_effect` — additional payroll cost from COLA applied to existing staff
  - `n_headcount_end` — total rows in `contract_dt` at Snapshot B
  - `wage_bill_end` — sum of `salary_col` at Snapshot B (including all movements)

  No accounting identity check.

---

### 2. Rewrite `simulate_horizon()` in `R/simulate_horizon.R`

Thin loop over `simulate_scenario()`:

- Validate inputs and expand `salary_growth_rate` to a vector of length `n_periods` if scalar.
- Initialise `pensioner_register` as an empty `data.table` with columns `personnel_id`, `pension_amount`, `period_date`.
- Loop `t in 1:n_periods`: call `simulate_scenario()`, threading updated state (`contract_dt`, `personnel_dt`, `salary_scale_dt`, `pensioner_register`) forward each iteration.
- `rbindlist` the per-period `summary` rows into `summary_dt`. Add `_pct_of_end_bill` share columns for each effect (each effect divided by `wage_bill_end` — clearer fiscal interpretation than fraction of `total_change`).
- Return `list(summary_dt, contract_dt, personnel_dt)` (microdata conditional on `return_microdata`).
- **Drop `compute_wage_bill_summary()`** and the accounting identity check entirely.

---

### 3. Add effect-computation helper functions in `R/simulate_horizon.R`

Each helper is a pure, exported function with no side effects, independently testable. All live in the same file as `simulate_scenario()`.

**`compute_exit_effect(retirees_dt, salary_col)`**
- Returns: scalar numeric — `sum(retirees_dt[[salary_col]])`, or `0` if `retirees_dt` is `NULL` or empty.
- Tests: zero for empty input; correct sum for known retirees; ignores `NA` values.

**`compute_movement_effect(movers_dt, contract_dt_after, personnel_id_col, salary_col)`**
- Joins `movers_dt$salary_before` against post-move salaries in `contract_dt_after` to compute a per-mover salary diff. Splits by `movement_type`.
- Returns: named list `list(promotion = <scalar>, transfer = <scalar>)`.
- Tests: zero when `movers_dt` is empty; correct promotion diff when salary increases; correct transfer diff; handles movers with multiple contracts (sum salary per person).

**`compute_hiring_effect(new_hire_contracts_dt, salary_col)`**
- Returns: scalar numeric — `sum(new_hire_contracts_dt[[salary_col]])`, or `0` if `NULL` or empty.
- Tests: zero for empty input; correct sum; ignores `NA`.

**`compute_inflation_effect(pre_cola_wage_bill, growth_rate)`**
- Returns: `pre_cola_wage_bill * growth_rate`.
- Tests: zero when `growth_rate = 0`; proportional at known rate; handles negative rates.

---

### 4. Rewrite `generate_scenario_matrix()` in `R/generate_scenario_matrix.R`

Call `simulate_horizon()` — the interface is the same, but:

- Drop `validate_identity` parameter and `.finalise()` identity check entirely.
- Update `@return` docs to reflect new column names from `simulate_scenario()`.
- Keep grid expansion, `scenario_label`, `is_baseline`, parallel/sequential execution unchanged.

---

### 5. Update `update_state_with_movement()` in `R/movement_update.R`

Ensure `movers_dt` always carries `salary_before` (pre-move salary per mover, summed across all contracts per person). This is required by `compute_movement_effect()`. Verify it is captured before any salary update is applied; add if missing.

---

### 6. Verify `simulate_hiring()` returns contract-level new-hire data in `R/simulate_hiring.R`

Confirm the returned list includes a `new_hire_contracts_dt` element (not just `new_hires_dt`, which is personnel-only) with the salary column populated. Rename or add the element if needed so `compute_hiring_effect()` can sum it directly.

---

### 7. Rewrite `tests/testthat/test-simulate_horizon.R`

Tests for the four effect helpers (see Step 3 above), plus:

- `simulate_scenario()` returns all expected columns
- `wage_bill_start == sum(salary_col)` over all contracts with no filtering
- `n_headcount_start` and `n_headcount_end` match `nrow(contract_dt)` before/after
- `exit_savings` equals `compute_exit_effect()` applied to the same `retirees_dt`
- `pension_cost_new` equals `sum(retirees_dt$pension_amount)`
- `pensioner_register` gains one row per retiree with correct `period_date`
- `hiring_effect` equals `compute_hiring_effect()` applied to new hire contracts
- `promotion_effect` equals `compute_movement_effect()$promotion`
- `inflation_effect == pre_cola_bill * rate`
- `simulate_horizon()` threads state: period 2 `wage_bill_start` equals period 1 `wage_bill_end`
- `pensioner_register` accumulates across periods (row count grows each period with retirements)

---

## Further Considerations

### 1. Double-counting pensions

`simulate_retirement()` currently moves retirees to `contract_type_code = "pensioner"` and keeps them in `contract_dt` with a `pension_amount` column. Need to confirm in `R/simulate_retirement.R` whether pensioner rows retain a non-zero `salary_col` value or zero it out. This determines whether `sum(salary_col)` at Snapshot A already includes pension costs (no separate register needed) or excludes them (register must be summed explicitly). Resolve before implementing.

### 2. `movers_dt$salary_before` availability

The prior session's plan added `salary_before` to `update_state_with_movement()` but the change errored during testing. Verify whether it was successfully committed to `R/movement_update.R` before implementing `compute_movement_effect()`.

### 3. Removal of `compute_wage_bill_summary()` and `active_wage_bill()`

Both can be deleted from `R/simulate_horizon.R`. Confirm no spielplatz script or test file references them directly before removing.

For each function, always unit test fully and then ensure you write full roxygen documentation. Once all execution is done with 0 errors, warnings and notes, go ahead and rewrite the zambia_scenario_exploration.R script accounting for the changes.
