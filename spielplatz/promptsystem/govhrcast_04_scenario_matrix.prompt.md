````prompt
# govhrcast: Scenario Matrix & Simulation Orchestration – Prompt File

## Purpose

`generate_scenario_matrix()` is the batch wrapper around `simulate_horizon()`.
It expands a grid of policy lever values into all combinations, runs each as a
separate scenario, and stacks the results into a single long flat `data.table`
ready for Shiny dashboarding.

---

## Key Functions & Their Roles

| Function | File | Role |
|---|---|---|
| `simulate_horizon()` | `simulate_horizon.R` | Single-scenario loop: runs N periods, returns `horizon` S3 object |
| `generate_scenario_matrix()` | `generate_scenario_matrix.R` | Batch wrapper: expands grid, calls `simulate_horizon()` per scenario |
| `new_horizon()` | `horizon_class.R` | Constructor for the `horizon` S3 object |
| `validate_horizon()` | `validation.R` | Validates a `horizon` object |
| `plot.horizon()` | `plot_horizon.R` | S3 plot method for `horizon` objects |
| `generate_hrcastapp()` | `generate_hrcastapp.R` | Launches the Shiny dashboard |

---

## `simulate_horizon()` Output: The `horizon` S3 Object

```r
new_horizon(
  comparison = summary_dt,   # data.table: one row per period
  metadata   = list(
    policy_args = list(...),  # the policy parameters used
    param_grid  = NULL        # NULL for single-scenario; set by generate_scenario_matrix
  )
)
```

`summary_dt` columns (one row per simulation period):

| Column | Type | Description |
|---|---|---|
| `period_date` | Date | End date of the simulation period |
| `n_headcount_start` | integer | Headcount at start of period |
| `n_headcount_end` | integer | Headcount at end of period |
| `wage_bill_start` | numeric | Gross payroll at start of period |
| `wage_bill_end` | numeric | Gross payroll at end of period |
| `n_exits` | integer | Retirement exits this period |
| `exit_savings` | numeric | Salary mass freed by retirements |
| `n_non_ret_exits` | integer | Non-retirement exits this period |
| `non_ret_exit_savings` | numeric | Salary mass freed by non-retirement exits |
| `pension_cost_new` | numeric | New pension commitments this period |
| `pension_cost_total` | numeric | Cumulative pension liability (all living retirees) |
| `n_promotions` | integer | Promotion events this period |
| `n_transfers` | integer | Transfer events this period |
| `promotion_effect` | numeric | Net salary change from promotions |
| `transfer_effect` | numeric | Net salary change from transfers |
| `n_hires` | integer | New hires this period |
| `hiring_effect` | numeric | Total salary of new hires |
| `inflation_effect` | numeric | Payroll increment from COLA/salary_growth_rate |
| `exit_savings_pct_of_end_bill` | numeric | exit_savings / wage_bill_end |
| `non_ret_exit_savings_pct_of_end_bill` | numeric | non_ret_exit_savings / wage_bill_end |
| `promotion_effect_pct_of_end_bill` | numeric | promotion_effect / wage_bill_end |
| `transfer_effect_pct_of_end_bill` | numeric | transfer_effect / wage_bill_end |
| `hiring_effect_pct_of_end_bill` | numeric | hiring_effect / wage_bill_end |
| `inflation_effect_pct_of_end_bill` | numeric | inflation_effect / wage_bill_end |

---

## `generate_scenario_matrix()` — Design

### Input: `param_grid`

A named list where each element is a vector of values for one policy lever.
All combinations are expanded with `data.table::CJ()`.

```r
param_grid <- list(
  salary_growth_rate       = c(0.02, 0.05, 0.10),
  retirement_min_age       = c(55, 60),
  movement_transfer_multiplier = c(0.5, 1.0)
)
# Produces 3 * 2 * 2 = 12 scenarios
```

**Lever name mapping rules (checked in order):**
1. Exact match to a `simulate_horizon()` scalar argument
   (`n_periods`, `salary_growth_rate`, `base_year`)
2. Prefix `retirement_` → injected into `retirement_policy$<key>`
3. Prefix `movement_` → injected into `movement_policy$<key>`
4. Prefix `hiring_` → injected into `hiring_policy$<key>`
5. Unknown names → silently ignored (forward-compatible)

### Output: flat `data.table`

One row per **scenario × period**, key `(scenario_id, period_date)`.

Added columns on top of `simulate_horizon()` output:

| Column | Description |
|---|---|
| `scenario_id` | Integer row index of the expanded grid |
| `scenario_label` | Human-readable: `"short_lever_1: val | short_lever_2: val"` |
| `is_baseline` | `TRUE` for `scenario_id == baseline_scenario_id` (default: 1L) |
| `<lever_name>` | One column per lever, recording the value used in that scenario |

### Execution Strategy

1. **Smoke test**: scenario 1 runs synchronously first. If it fails, abort.
2. **Parallel**: if `future.apply` is installed and `future::plan()` is not
   `SequentialFuture`, remaining scenarios run via `future.apply::future_lapply()`.
   Each worker gets deep copies of base data and sets `setDTthreads(1)` to avoid
   thread collisions.
3. **Sequential fallback**: `lapply()` if parallel not available.
4. **Results**: `rbindlist(fill=TRUE)` all outputs; failed scenarios are dropped
   with a warning.

### `scenario_label` format

Lever names are shortened by stripping module prefixes (`retirement_`,
`movement_`, `hiring_`) and replacing underscores with spaces:

```
retirement_min_age: 55 | salary_growth_rate: 0.03
```

---

## Effect Computation Helpers

These pure functions are exported and used inside `simulate_horizon()`.
They can also be used standalone for custom analyses.

### `compute_exit_effect(retirees_dt, salary_col)`
Returns `sum(salary_col)` over all retirees. Returns `0` if input is NULL/empty.

### `compute_movement_effect(movers_dt, contract_dt_after, personnel_id_col, salary_col)`
Returns `list(promotion = <numeric>, transfer = <numeric>)` — net salary
change (post-move minus pre-move) for each movement type.

### `compute_inflation_effect(wage_bill_before, wage_bill_after)`
Returns `wage_bill_after - wage_bill_before`.

---

## Stock-Flow Identity

The simulation enforces at each period `t`:

```
Active_t = Active_{t-1}
           - Retirements_t
           - Other_Exits_t
           + Hires_t
           ± Promotions/Transfers_t (headcount unchanged)
```

`wage_bill_end` is always recomputed from the full contract microdata after all
modules have run (not incrementally), so rounding errors do not compound.

---

## Policy Parameter Structures

These are passed to `simulate_horizon()` and override-able per-scenario via
`param_grid` prefixes.

### `retirement_policy`
```r
list(
  eligibility_type = "age_only",  # or "tenure_only", "age_and_tenure"
  min_age          = 60,          # overrideable via retirement_min_age
  min_tenure       = NULL,        # overrideable via retirement_min_tenure
  pension_type     = "flat",      # "flat", "db", "dc", "hybrid"
  pension_params   = list(flat_amount = 1000)
)
```

### `movement_policy`
```r
list(
  group_cols           = "est_id",
  salary_scale         = ss,
  promotion_multiplier = 1.0,     # overrideable via movement_promotion_multiplier
  transfer_multiplier  = 1.0,     # overrideable via movement_transfer_multiplier
  promotion_strategy   = "tenure",
  transfer_strategy    = "random"
)
```

### `hiring_policy`
```r
list(
  mode         = "flow",          # "flow" = replace exits; "fixed" = fixed count
  group_cols   = "est_id",
  salary_scale = ss
)
```

---

## Using Results in the Dashboard

```r
# Scenario matrix input
results <- generate_scenario_matrix(ct, pt, ss, param_grid = grid, ...)
generate_hrcastapp(results)   # lever-mode: sidebar shows one input per lever

# Single horizon input
hz <- simulate_horizon(ct, pt, ss, ...)
generate_hrcastapp(hz)        # label-mode: sidebar shows scenario name selector
```

The dashboard detects the mode automatically via `hz_lever_cols(flat_dt)`:
- If `length(lever_cols) > 0`: lever-mode
- If `length(lever_cols) == 0`: label-mode

The Scenario Comparator tab is only shown when `nrow(flat_dt) > 1`.

````
