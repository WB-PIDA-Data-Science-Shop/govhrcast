````prompt
# govhrcast Modelling & Simulation Roadmap
# Sequenced Implementation Plan — DEV branch, March 2026
# =============================================================================
# Items are ordered by implementation dependency.  Each phase must be complete
# and tests green before the next phase begins.  Items marked [APPROVED] are
# ready to code.  Items marked [DEFERRED] are on record but not in scope.
# =============================================================================


---

## PHASE 0 — Code Quality & Correctness (no API changes)

These are surgical fixes with no user-facing signature changes.  They must
land first because Phases 1–6 build on a correct baseline.

---

### Phase 0a — Remove redundant data.table::copy() calls [APPROVED]

**Problem**
Several functions do `contract_dt <- data.table::copy(contract_dt)` and then
never pass the original object back to any caller.  The copy allocates a
full O(n) memory block and is immediately abandoned.

**Rule**
Keep `copy()` only when the original object must remain unmodified for a
caller that still holds a reference to it.  Remove all copies that are
immediately followed by `[`, `[[`, or `.SD` operations (those create new
objects already) or where the object was just created by `as.data.table()`.
For every kept `copy()`, add a one-line comment explaining why.

**Files**
`R/simulate_horizon.R`, `R/simulate_hiring.R`, `R/retirement_update.R`,
`R/movement_update.R`

**Tests**
Run `devtools::test()` after removal.  No new tests needed — existing tests
cover correctness.

---

### Phase 0b — Fix headcount: count persons not contracts [APPROVED — Item 10h]

**Problem**
`n_headcount_start` and `n_headcount_end` in `simulate_scenario()` are:

```r
n_headcount_start <- nrow(contract_dt[get(contract_type_col) != "pensioner"])
```

A person with two simultaneous active contracts is counted twice.

**Fix**
Use `data.table::uniqueN()` on the personnel ID column:

```r
n_headcount_start <- uniqueN(
  contract_dt[get(contract_type_col) != "pensioner"],
  by = personnel_id_col
)
```

Apply the same fix to `n_headcount_end` and any headcount reporting in
`compute_movement_summary()` and `simulate_hiring()`.

**Files**
`R/simulate_horizon.R` (inside `simulate_scenario()`), `R/movement_core.R`,
`R/hiring_core.R`

**Tests**
Fixture: one person, two simultaneous active contracts.
Assert `n_headcount_end == 1`, not `2`.

---

### Phase 0c — Fix wage bill: aggregate to person before summing [APPROVED — Item 10a]

**Problem**
`wage_bill_start` and `wage_bill_end` sum `salary_col` across all active
contract *rows*.  A person with two contracts has their salary counted twice.

**What this means:** `wage_bill_end` is a snapshot of contract rows, not
persons.  The fix is to aggregate to `personnel_id` level first, using
`max(salary_col)` per person as the primary-contract proxy (consistent with
the existing `get_primary_contract()` helper).  Document clearly that users
with genuinely additive split-salary contracts should pre-aggregate their data.

**Fix**

```r
wage_bill_start <- contract_dt[
  get(contract_type_col) != "pensioner",
  .(salary = max(get(salary_col), na.rm = TRUE)),
  by = personnel_id_col
][, sum(salary, na.rm = TRUE)]
```

**Files**
`R/simulate_horizon.R` (inside `simulate_scenario()`) — 2 lines for start,
2 lines for end.

**Tests**
Same fixture as Phase 0b.  Assert `wage_bill_end` equals the person's single
salary value, not double.

---

### Phase 0d — Fix multi-contract exit: retire all contracts per person [APPROVED — Item 10i]

**Problem**
`identify_retirees()` returns rows keyed on qualifying contracts.  When a
person has two simultaneous contracts and only one qualifies, only that
contract row is retired.  The second remains active — logically wrong.

**Fix**
In `simulate_retirement()`, after identification, extract the unique retiring
`personnel_id` values and mark **all** their contracts:

```r
retiring_ids <- unique(retirees_dt[[personnel_id_col]])
contract_dt[get(personnel_id_col) %in% retiring_ids,
            (contract_type_col) := "pensioner"]
```

No join needed — a plain membership filter.  Build the same pattern into
`simulate_exits()` from the start (Phase 3).

**Files**
`R/simulate_retirement.R` (or the status-update helper it calls)

**Tests**
Fixture: one person, two active contracts, qualifies on contract 1.
Assert both contracts are marked `"pensioner"` after `simulate_retirement()`.

---

### Phase 0e — Deterministic synthetic IDs for simulated hires [APPROVED — Item 10d]

**Problem**
New-hire `contract_id` and `personnel_id` values are currently random.
Re-running the same simulation with a different seed produces different IDs,
making period-over-period microdata joins impossible and snapshot tests
unstable.

**Fix**
Replace random ID generation with a deterministic scheme keyed on period
date, group, and sequence position.  All column name arguments are used —
nothing is hardcoded:

```r
# Example output: "sim_2026-01-01_ministry_of_finance_001"
paste0(
  "sim_", format(ref_date, "%Y-%m-%d"), "_",
  sanitise_id_key(group_key), "_",
  formatC(seq_len(n), width = 3L, flag = "0")
)
```

**Files**
`R/hiring_core.R` (wherever new IDs are generated)

**Tests**
Run `simulate_hiring()` twice with identical inputs (different seeds).
Assert identical contract ID vectors in both outputs.

---

## PHASE 1 — Tenure & Age Infrastructure

---

### Phase 1a — Rewrite compute_tenure(): vectorised interval union [APPROVED]

**Problem**
Current `compute_tenure()` sums raw contract durations without merging
overlapping intervals.  Concurrent contracts double-count tenure; re-hires
after a gap are only handled correctly when the gap falls between distinct
contract IDs.

**Algorithm: cummax() vectorised sweep — no for loop, no IRanges**

After sorting each person's intervals by `start_date`, `cummax(end_int)`
propagates the "furthest right endpoint seen so far" across the sorted rows.
Lagging that value by one position gives the "furthest reach before this
interval started".  Three cases cover all situations:

```
Case 1: s[i] > lag_cummax[i]         → new span.   Contribution = e[i] - s[i]
Case 2: e[i] > lag_cummax[i] ≥ s[i]  → extension.  Contribution = e[i] - lag_cummax[i]
Case 3: e[i] ≤ lag_cummax[i]         → nested.     Contribution = 0
```

This is O(n log n) for the sort, O(n) for the rest.  The per-person group
boundary reset is handled by data.table's `by =` in C — no R-level loop.

A hybrid dispatch (simple diff for single-spell persons) is not worthwhile:
the `cummax()` path handles the single-spell case in one vectorised step
with no branching, and adding a dispatch branch would itself require a
`.N == 1` count per person that costs as much as running the algorithm.

**Implementation sketch (all column names parameterised — nothing hardcoded)**

```r
compute_tenure <- function(contract_dt,
                           ref_date,
                           personnel_id_col  = "personnel_id",
                           contract_id_col   = "contract_id",
                           start_date_col    = "start_date",
                           end_date_col      = "end_date",
                           contract_type_col = "contract_type_code",
                           inactive_types    = c("inactive", "pensioner")) {

  # 1. Filter inactive types — subset returns new object, no copy() needed
  dt <- contract_dt[!get(contract_type_col) %in% inactive_types]

  # 2. Cap open-ended / future contracts at ref_date
  dt <- dt[get(start_date_col) <= ref_date]
  dt[, .eff_end := data.table::fifelse(
    is.na(get(end_date_col)) | get(end_date_col) > ref_date,
    ref_date,
    get(end_date_col)
  )]

  # 3. Deduplicate panel snapshots: one row per (contract_id, start_date)
  dt <- dt[dt[, .I[1L], by = c(contract_id_col, start_date_col)]$V1]

  # 4. Work on integer days — avoids Date method overhead
  dt[, .s := as.integer(get(start_date_col))]
  dt[, .e := as.integer(.eff_end)]
  dt <- dt[.e > .s]  # drop zero-length contracts

  # 5. Sort by person then start — O(n log n)
  data.table::setorderv(dt, c(personnel_id_col, ".s"))

  # 6. Lagged cummax of end-dates within person
  dt[, .lag_max_e := data.table::shift(cummax(.e), fill = -Inf),
     by = personnel_id_col]

  # 7. Classify each interval and compute its contribution to the union
  dt[, .contrib := data.table::fcase(
    .s >  .lag_max_e,  .e - .s,          # Case 1: new span
    .e >  .lag_max_e,  .e - .lag_max_e,  # Case 2: extension
    default = 0L                          # Case 3: nested
  )]

  # 8. Sum contributions per person
  result <- dt[,
    .(tenure_days = sum(.contrib, na.rm = TRUE)),
    by = .(personnel_id_val = get(personnel_id_col))
  ]
  result[, tenure_years := tenure_days / 365.25]
  data.table::setnames(result, "personnel_id_val", personnel_id_col)

  result[, c(personnel_id_col, "tenure_days", "tenure_years"), with = FALSE]
}
```

**Files**
`R/utils.R` — replace `compute_tenure()` body; signature and default args
unchanged (backward-compatible drop-in replacement).

**Tests — correctness (`tests/testthat/test-compute_tenure.R`)**
- Single contract: tenure = end − start
- Two overlapping contracts: tenure = union, not sum
- Gap between two contracts: tenure = sum of two separate spans (gap excluded)
- Nested contract (B entirely inside A): tenure = A's span only
- Multiple persons in one call: each computed independently
- Zero-length contract (start = end): contributes 0 days

**Tests — performance (`tests/testthat/test-compute_tenure_perf.R`)**
Guarded by an environment variable so normal `devtools::test()` is unaffected:

```r
skip_unless_perf_env <- function() {
  skip_if(
    !identical(Sys.getenv("GOVHRCAST_RUN_PERF_TESTS"), "true"),
    message = "Set GOVHRCAST_RUN_PERF_TESTS=true to run performance tests"
  )
}

test_that("compute_tenure runs in < 2s on 500k contracts (50k people)", {
  skip_unless_perf_env()
  n <- 500000L
  set.seed(42L)
  big_dt <- data.table::data.table(
    personnel_id       = sample(paste0("p", seq_len(50000L)), n, replace = TRUE),
    contract_id        = paste0("c", seq_len(n)),
    start_date         = sample(
      seq(as.Date("1990-01-01"), as.Date("2015-01-01"), by = "day"),
      n, replace = TRUE
    ),
    contract_type_code = "permanent"
  )
  big_dt[, end_date := start_date + sample(365L:3650L, n, replace = TRUE)]

  elapsed <- system.time(
    compute_tenure(big_dt, ref_date = as.Date("2024-01-01"))
  )["elapsed"]
  expect_lt(elapsed, 2)
})
```

Run independently:
```r
Sys.setenv(GOVHRCAST_RUN_PERF_TESTS = "true")
devtools::test(filter = "compute_tenure_perf")
Sys.unsetenv("GOVHRCAST_RUN_PERF_TESTS")
```

---

### Phase 1b — Auto-compute age and tenure inside simulate_horizon() [APPROVED — Item 3]

**Problem**
Users must pre-compute `age` and `tenure_years` columns before calling any
simulation function.  This leaks implementation detail and is error-prone.

**Fix**
Add optional `birth_date_col` parameter (default `"birth_date"`) to
`simulate_horizon()`.  In the pre-loop prologue:

```r
# Compute age once from birth_date (if column is available)
if (!is.null(birth_date_col) && birth_date_col %in% names(personnel_dt)) {
  personnel_dt[, (age_col) := as.numeric(
    difftime(ref_date, get(birth_date_col), units = "days")
  ) / 365.25]
}

# Compute tenure once from contract history
tenure_init <- compute_tenure(
  contract_dt,
  ref_date          = ref_date,
  personnel_id_col  = personnel_id_col,
  contract_id_col   = contract_id_col,
  start_date_col    = start_date_col,
  end_date_col      = end_date_col,
  contract_type_col = contract_type_col
)
personnel_dt[tenure_init, (tenure_col) := i.tenure_years, on = personnel_id_col]
```

Backward-compatible: if `birth_date_col = NULL` (default), the user's
pre-computed `age_col` column is used as before.

**Files**
`R/simulate_horizon.R` — pre-loop prologue

---

## PHASE 2 — Simulation Engine Restructure

---

### Phase 2a — Dynamic period step: year / month / day [APPROVED — Item 1]

**Problem**
`simulate_horizon()` hard-codes annual stepping via `.add_years()`.

**Fix**
Add `period_unit = c("year", "month", "day")` to `simulate_horizon()` and
replace `.add_years()` with `.advance_period()`:

```r
.advance_period <- function(date, n = 1L, unit = "year") {
  switch(unit,
    year  = lubridate::add_with_rollback(date, lubridate::years(n)),
    month = lubridate::add_with_rollback(date, lubridate::months(n)),
    day   = date + n
  )
}
```

**Rate conversion**
Both `salary_growth_rate` and `pension_cola_rate` (Phase 2c) are treated as
*annual* rates.  When `period_unit != "year"`, the prologue converts them
automatically:

```r
period_fraction    <- switch(period_unit, year = 1, month = 1/12, day = 1/365.25)
period_growth_rate <- (1 + salary_growth_rate)^period_fraction - 1
period_cola_rate   <- (1 + pension_cola_rate)^period_fraction  - 1
```

The same `period_fraction` drives the aging increment in each period's AGING
step.

**Files**
`R/simulate_horizon.R` — `.add_years()` → `.advance_period()`, prologue,
aging step

**Tests**
Monthly simulation over 12 periods ≈ annual simulation over 1 period for
wage bill growth (within floating-point tolerance).

---

### Phase 2b — Restructure simulate_horizon() prologue: pre-compute all baselines [APPROVED — Item 4]

**Problem**
`identify_retirees()` calls `compute_tenure()` on every period call.
With 10 periods and 50k contracts, that is 10 full tenure computations when
1 suffices.

**New prologue structure**

```
simulate_horizon() PROLOGUE  (runs once before the period loop)
│
├── P1.  Validate all inputs
├── P2.  Compute tenure → inject into personnel_dt as tenure_col       (Phase 1b)
├── P3.  Compute age → inject into personnel_dt as age_col             (Phase 1b)
├── P4.  Seed pension register from pre-existing retirees              (Phase 2c)
├── P5.  Estimate movement baseline from panel                         (already done)
├── P6.  Estimate historical hiring rates from panel                   (already done)
└── P7.  Estimate historical exit rates from panel                     (Phase 3)

PERIOD LOOP  t = 1 … n_periods
│
├── simulate_scenario(contract_dt, personnel_dt, ...)
│   ├── RETIREMENT   — reads age_col / tenure_col; no recompute
│   ├── EXITS        — Phase 3
│   ├── MOVEMENTS    — reads cached baseline_matrix
│   ├── HIRING
│   ├── AGING        — personnel_dt[active, age_col    += period_fraction]
│   │                   personnel_dt[active, tenure_col += period_fraction]
│   └── COLA         — salary   × (1 + period_growth_rate[t])
│                       pension_register × (1 + period_cola_rate[t])  Phase 2c
└── append summary row
```

`identify_retirees()` will receive `age` and `tenure_years` as pre-computed
columns on `personnel_dt`; its internal `compute_age()` / `compute_tenure()`
calls are removed.  The function signature gains `age_col` and `tenure_col`
pass-through arguments (defaulting to the existing column names).

**Files**
`R/simulate_horizon.R`, `R/retirement_core.R` (`identify_retirees()`),
`R/simulate_retirement.R`

**Tests**
Snapshot test: confirm 10-period simulation results are identical before and
after this refactor.

---

### Phase 2c — Seed pension register + expand schema + pension_cola_rate [APPROVED — Items 8 + 10b]

#### Part A — Seed pension register from pre-existing retirees

**Problem**
`pensioner_register` is initialised as an empty table before the loop.
`pension_cost_total` in period 1 is therefore 0 even when the input data
already contains people with `contract_type_col == pensioner_type_value`.
In virtually every real civil service dataset some people were already
retired at `ref_date`.

**Fix — pre-loop seed step**

```r
existing_retirees_dt <- contract_dt[
  get(contract_type_col) == retirement_policy$pensioner_type_value,
  .(
    personnel_id               = get(personnel_id_col),
    pension_amount             = get(salary_col),
    final_salary               = get(salary_col),
    tenure_years_at_retirement = NA_real_,   # historical, often unavailable
    age_at_retirement          = NA_real_,
    period_date                = ref_date
  )
]
pensioner_register <- existing_retirees_dt
```

Period 1 then appends new retirees on top of this seeded stock.

#### Part B — Expand pension register schema [APPROVED — Item 10b]

Current schema: `(personnel_id, pension_amount, period_date)`.
This makes cohort audit and scenario comparison impossible without re-running
the pension formula.

**Expanded schema**

```r
data.table::data.table(
  personnel_id               = character(0),
  pension_amount             = numeric(0),   # formula output, COLA-adjusted each period
  final_salary               = numeric(0),   # salary_col at moment of retirement
  tenure_years_at_retirement = numeric(0),
  age_at_retirement          = numeric(0),
  period_date                = as.Date(character(0))
)
```

When `new_reg` rows are appended per period, all five columns are populated
from `retirees_dt`.

#### Part C — Separate COLA rates: salary_growth_rate vs pension_cola_rate

**Why two rates matter**
In most civil service systems these are distinct policy instruments:
- Active worker salary growth is set by budget appropriations or collective
  bargaining.
- Pension COLA is typically statutory (often CPI-linked), and is often lower
  than the active rate or zero in austerity years.

Conflating them into one `salary_growth_rate` forces the user to assume
identical adjustment for both groups — almost never true in practice.

**New parameter:** `pension_cola_rate` added to `simulate_horizon()` and
`simulate_scenario()`.  Default: `pension_cola_rate = salary_growth_rate`
(backward-compatible — if not specified, both groups get the same rate).

**Scalar or vector: both supported with one argument**
Both `salary_growth_rate` and `pension_cola_rate` accept either a scalar or
a length-`n_periods` numeric vector.  The prologue normalises both to vectors:

```r
salary_growth_rate <- rep_len(salary_growth_rate, n_periods)
pension_cola_rate  <- rep_len(pension_cola_rate,  n_periods)
# rep_len() recycles a scalar silently; a vector of wrong length raises an error
if (length(salary_growth_rate) != n_periods)
  stop("salary_growth_rate must be length 1 or n_periods = ", n_periods)
```

Per-period COLA application in the loop:

```r
# Active worker salaries (existing step, now period-indexed)
contract_dt[get(contract_type_col) != "pensioner",
            (salary_col) := get(salary_col) * (1 + period_growth_rate[t])]

# Pension register amounts (new step)
pensioner_register[, pension_amount := pension_amount * (1 + period_cola_rate[t])]
```

**Files**
`R/simulate_horizon.R` — prologue seed step, signature, period loop COLA block

**Tests**
- Fixture with 2 pre-existing retirees → period 1 `pension_cost_total > 0`
- `pension_cola_rate = 0` → assert `pension_cost_total` stays constant after
  seeding (no COLA applied to existing register)
- Vector `salary_growth_rate = c(0.03, 0.05, 0.04)` with `n_periods = 3` →
  assert each period's `inflation_effect` matches the corresponding rate
- Scalar input recycled correctly to `n_periods` length

---

## PHASE 3 — Exit Simulation Module [APPROVED — Item 6]

Implements non-retirement attrition (voluntary resignation, dismissal, contract
non-renewal).  Mirrors the hiring module structure exactly.

---

### Phase 3a — estimate_historical_exit_rates()

```r
estimate_historical_exit_rates <- function(
  contract_dt,
  group_cols        = "est_id",
  personnel_id_col  = "personnel_id",
  contract_id_col   = "contract_id",
  start_date_col    = "start_date",
  end_date_col      = "end_date",
  contract_type_col = "contract_type_code",
  ref_date_col      = "ref_date"
) # → data.table keyed on group_cols: group_cols + exit_rate
```

Uses `govhr::detect_personnel_event(event_type = "fire")` to identify
non-retirement exits in the contract panel.  Computes
`exit_rate = n_exits / n_active` per group per panel period, then averages.

**File:** `R/exit_core.R`

---

### Phase 3b — compute_status_quo_exits()

```r
compute_status_quo_exits <- function(
  contract_dt,
  exit_rates_dt,
  group_cols        = "est_id",
  personnel_id_col  = "personnel_id",
  contract_type_col = "contract_type_code",
  active_types      = "active"
) # → data.table of personnel_ids selected to exit this period
```

Applies historical exit rates to current workforce by group.  Selection
within each group follows `exit_strategy` in the policy params (same design
as `promotion_order_col`, Phase 5).

**File:** `R/exit_core.R`

---

### Phase 3c — simulate_exits()

```r
simulate_exits <- function(
  contract_dt,
  personnel_dt,
  policy_params,
  ref_date,
  personnel_id_col  = "personnel_id",
  contract_id_col   = "contract_id",
  contract_type_col = "contract_type_code",
  status_col        = "status",
  salary_col        = "gross_salary_lcu",
  active_types      = "active",
  exited_type       = "inactive"
) # → list(summary, contract_dt, personnel_dt, exits_dt)
```

Orchestrates core → update → summary (identical pattern to `simulate_hiring()`).

**Exit policy params struct**

```r
exit_policy <- list(
  mode            = "status_quo",  # "status_quo" | "fixed_rate"
  group_cols      = "est_id",
  exit_rates_dt   = NULL,          # NULL → estimated from panel in prologue
  exit_strategy   = "random",      # "random" | any numeric col name
  exit_multiplier = 1.0            # scale historical rate up/down
)
```

**Multi-contract handling**
After identifying exiting `personnel_id` values, filter `contract_dt` and
mark ALL contracts for those people as `exited_type` using a plain membership
filter — no join:

```r
exiting_ids <- unique(exits_dt[[personnel_id_col]])
contract_dt[get(personnel_id_col) %in% exiting_ids,
            (contract_type_col) := exited_type]
```

**Integration in `simulate_scenario()`**

```
1. Retirement     (existing)
2. Exits          ← NEW (Phase 3)
3. Movements      (existing)
4. Hiring         (existing)
5. Aging          (existing, now uses period_fraction)
6. COLA           (updated in Phase 2c)
```

`exit_savings` from non-retirement exits is additive with retirement
`exit_savings` in the hiring demand calculation.

**Files:** `R/exit_core.R`, `R/exit_update.R`, `R/simulate_exits.R`
**Tests:** Mirror `tests/testthat/test-simulate_retirement.R` structure.

---

## PHASE 4 — Salary Scale Infrastructure [APPROVED — Item 10f, defer 10g]

---

### Phase 4a — apply_salary_scale_adjustment()

A neutrally-named utility that applies a scalar or vector multiplier to a
salary scale.  Users name the concept (COLA, regrading, compression policy,
etc.) in their own code and documentation.

```r
apply_salary_scale_adjustment <- function(
  salary_scale_dt,
  adjustment,           # scalar OR named numeric vector
  salary_col  = "gross_salary_lcu",
  key_col     = NULL    # column whose values match names(adjustment)
) # → data.table (same structure as input, salary_col updated in place)
```

Behaviour:
- Scalar `1.05` → all entries multiplied by 1.05.
- Named numeric vector → matched against `salary_scale_dt[[key_col]]` values;
  unmatched entries receive multiplier `1.0` with a warning.
- Input validation: all multiplier values must be > 0; warn if any < 0.9 or
  > 1.2 (likely user error).

Where it plugs in: pass a pre-adjusted scale via `hiring_policy$salary_scale`
or `movement_policy$salary_scale`.  Can also be called directly before
`simulate_horizon()`.

**Files:** `R/salary_scale_utils.R` (new file)

**Tests:**
- Scalar multiplier applied to all entries
- Named vector adjusts matched entries only; unmatched stay at original value
- Zero/negative input raises error
- Output schema identical to input (same columns, same key)

#### Deferred: Phase 4b — wage compression ratio constraint [Item 10g]

After Phase 4a is working end-to-end, add `validate_salary_scale_compression()`
as an optional post-adjustment check.  Implement as a standalone function
(not baked into 4a) so users can call it independently.

---

## PHASE 5 — Promotion Strategy Flexibility [APPROVED — Item 10c]

---

### Phase 5a — promotion_order_col in movement_policy

**Problem**
`identify_movers()` hard-codes ranking by `time_in_grade` descending.

**Fix**
Add `promotion_order_col` to the `movement_policy` list
(default `"time_in_grade"`, backward-compatible).  Any numeric column
present in `contract_dt` is valid — no enum, no hardcoded options.

```r
# inside identify_movers()
order_col <- movement_policy$promotion_order_col %||% "time_in_grade"
eligible_dt[order(-get(order_col))][seq_len(n_movers)]
```

**Documented options (not an exhaustive enum — users can pass any numeric col)**

| Value | Meaning |
|---|---|
| `"time_in_grade"` | Default — longest in current grade promoted first |
| `"tenure_years"` | Total service seniority |
| `"salary_ratio"` | `salary / max_grade_salary` — most headroom below ceiling |
| any numeric col | E.g. a user-supplied performance score |

**Granularity decision**
Computing tenure at a specific position or establishment is already available
via `compute_time_in_grade()`.  We will not go more granular — the marginal
policy realism does not justify additional computational cost for a budget
simulation engine.

**Cooldown / re-promotion guard**
Deferred.  Requires threading `periods_since_last_promotion` state through
the period loop.  Log as a future enhancement.

**Files:** `R/movement_core.R` (`identify_movers()`), `R/validate_inputs.R`

**Tests:**
- Two identical eligible pools — one ranked by `tenure_years`, one by
  `salary_ratio`.  Assert that different personnel are selected.

---

## PHASE 6 — data.table Idiom Cleanup [APPROVED — Item 9]

---

### Phase 6a — Switch scalar sums to dt[i, sum(col)] form

The wage-bill snapshot lines in `simulate_scenario()` currently use
vector-extraction:

```r
# Before
sum(contract_dt[get(contract_type_col) != "pensioner"][[salary_col]], na.rm = TRUE)

# After
contract_dt[get(contract_type_col) != "pensioner",
            sum(get(salary_col), na.rm = TRUE)]
```

The `[i, j]` form avoids materialising a filtered column as an intermediate
R vector (one fewer allocation per period).  The difference is < 5 ms on
500k rows but the idiom is consistent with the rest of the codebase.

Note: by Phase 0b/0c the headcount lines will already use `uniqueN()` and
the per-person wage bill aggregation; this phase updates only the remaining
vector-extraction patterns.

**Files:** `R/simulate_horizon.R` (inside `simulate_scenario()`)

---

## DEFERRED ITEMS

---

### Deferred A — Historical actuals overlay on plots [Item 5 — DROPPED]

Risk of diverging from `govhr` computed actuals outweighs the visual benefit.
`govhr` handles historical analysis; `govhrcast` handles projections only.

---

### Deferred B — Confidence / uncertainty bands [Item 10j]

Add a Monte Carlo wrapper around `simulate_horizon()` and a fan-chart plot
once the core engine is stable.

---

### Deferred C — Wage compression ratio constraint [Item 10g]

Implement after Phase 4a (`apply_salary_scale_adjustment()`) is stable.

---

### Deferred D — Promotion cooldown (periods_since_last_promotion) [Item 10c extension]

Requires additional state threading.  Add after Phase 5 is validated.

---

## Implementation Sequence Summary

```
Phase 0a  Remove redundant copy() calls
Phase 0b  Headcount: uniqueN() by personnel_id
Phase 0c  Wage bill: max(salary) per person before summing
Phase 0d  Multi-contract exit: retire ALL contracts via %in% filter
Phase 0e  Deterministic synthetic IDs for simulated hires
Phase 1a  compute_tenure(): cummax() vectorised interval union
Phase 1b  Auto-compute age/tenure in simulate_horizon() prologue
Phase 2a  Dynamic period step (year / month / day)
Phase 2b  Restructure prologue: pre-compute ALL baselines before loop
Phase 2c  Seed pension register + expand schema + pension_cola_rate
Phase 3a  estimate_historical_exit_rates()
Phase 3b  compute_status_quo_exits()
Phase 3c  simulate_exits() + integrate into simulate_scenario()
Phase 4a  apply_salary_scale_adjustment()
Phase 5a  promotion_order_col in movement_policy
Phase 6a  dt[i, sum(col)] idiom cleanup
```
````
