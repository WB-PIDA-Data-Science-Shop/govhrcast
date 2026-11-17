# Allowance Modeling for govhrcast

## Context

`govhrcast` currently models total compensation as a single `salary_col`
(typically `gross_salary_lcu`) that already bakes in whatever lump allowance
amount a contract had in the source snapshot. Real HRMIS allowance data
(`bra_hrmis_allowance`) is richer than that: it's a long-format panel keyed by
`personnel_id`/`contract_id`/`ref_date`/`allowance_type`, and government data
never tells us *why* someone gets a given allowance (no eligibility rules are
shared) — only the observed amounts. The user wants two new capabilities,
modeled on the existing salary-scale machinery:

1. Assign allowances to contracts each simulated period from an allowance
   scale (since we can't model eligibility directly).
2. Apply targeted changes to specific allowance types, or to a person's total
   allowance, for specific portions of the population.

Decisions already made with the user (via prior discussion):
- **Data shape**: a new long-format companion table, `allowance_dt`
  (personnel_id, contract_id, period_date, allowance_type, allowance_lcu) —
  mirrors `bra_hrmis_allowance` and the existing `pensioner_register` pattern.
- **Assignment logic**: deterministic scale (like `salary_scale_dt`) — every
  contract in a matched group gets the group's scale amount for each
  allowance type, every period. Simpler than modeling incidence, at the cost
  of some realism (see caveat below).
- **Fiscal reporting**: allowance cost is tracked as its own line, but it
  *is* part of the real wage bill (unlike pensioner costs, which are
  deliberately excluded). `.active_wage_bill()` (salary-only) stays untouched
  to preserve the test-asserted invariant; a new `total_wage_bill` column
  reports salary + allowance combined.
- **Integration**: a full 5th pipeline module (`allowance_core.R` /
  `allowance_update.R` / `simulate_allowances.R`), following the existing
  three-file convention, wired into `simulate_scenario()`/`simulate_horizon()`
  as an optional step (`allowance_policy = NULL` disables it, like every
  other module).

## Correctness caveat to flag (needs explicit sign-off before coding)

`gross_salary_lcu` in the bundled data already includes the old, coarse
`allowance_lcu` lump sum (checked directly: `gross_salary_lcu ≈
base_salary_lcu + allowance_lcu` for most rows, correlation 0.75). If a user
runs the new allowance module on top of `salary_col = "gross_salary_lcu"`,
`total_wage_bill` would double-count allowances that are already folded into
`gross_salary_lcu`.

Resolution: this is a data-authoring concern, not something the package can
safely auto-detect (heuristics on column correlation would be fragile and
against the "don't validate for scenarios you can't reliably detect" rule).
Instead:
- Document prominently in `simulate_allowances()` and `simulate_horizon()`
  roxygen: when allowance modeling is active, `salary_col` should represent
  *base* compensation only (e.g. `base_salary_lcu`), not a gross figure that
  already includes allowances.
- Show this explicitly in the worked example/vignette-style roxygen example
  for the new module.
No runtime warning/guard — purely a documentation responsibility, consistent
with how the rest of the package treats column-semantics assumptions.

## New files (three-file module pattern)

**`R/allowance_core.R`** — pure computation, no mutation:
- `estimate_historical_allowance_scale(panel_allowance_dt, panel_contract_dt, group_cols, allowance_type_col = "allowance_type", amount_col = "allowance_lcu", personnel_id_col, ref_date_col, ...)` — for each `group_cols × allowance_type`, computes the **unconditional mean** amount across all contracts in the group (including zeros/non-recipients) — this is what makes deterministic per-contract assignment reproduce the group's real aggregate cost without claiming everyone is individually eligible. Document this "unconditional mean" choice explicitly (it's a deliberate simplification, not a bug — a future reader must not "fix" it into a conditional mean). Mirrors the output shape of `estimate_historical_hiring_rates()`/`estimate_movement_baseline()`: a data.table of `group_cols + allowance_type + amount`, usable directly as `allowance_policy$policy_table`.
- `resolve_allowance_amounts(contract_dt, allowance_policy, allowance_types)` — builds a long "working table" (one row per active contract × requested `allowance_type`, via cross-join) and resolves the per-row amount using the **existing** `dispatch_param()`/`resolve_policy_table()` from `R/utils.R` (no new dispatch logic needed — `group_cols` in the policy can already include `allowance_type` itself, so the standard join-based resolver handles per-type-per-group amounts for free). Also resolves a `total_allowance_multiplier` (default `1.0`) dispatched at the per-contract level (group_cols excluding `allowance_type`) for the "change someone's total allowance" case, applied after the per-type amounts are computed.

**`R/allowance_update.R`** — state mutation:
- `update_allowance_register(allowance_dt, resolved_amounts_dt, period_date, contract_dt)` — writes this period's resolved amounts into `allowance_dt` as new rows (keyed by `period_date`), the same accumulate-by-period pattern already used for `pensioner_register` (`data.table::rbindlist()` each period in `simulate_horizon()`). Drops rows for personnel no longer active (retired/exited) going forward, but never rewrites history for past periods.

**`R/simulate_allowances.R`** — thin wrapper:
- `simulate_allowances(contract_dt, personnel_dt, allowance_dt = NULL, allowance_policy, period_date, ...)` — validates inputs via a new `check_allowance_inputs()` in `R/validation.R` (follow the existing `check_retirement_inputs`/`check_hiring_inputs` pattern), calls the core resolver then the update function, returns `list(contract_dt, personnel_dt, allowance_dt, summary)`. `allowance_policy = NULL` short-circuits to a no-op, matching every other module.

## Targeted adjustments (requirement 2) — reuse, don't build new

No new "adjustment" utility function is needed:
- **Scale-wide or single-dimension tweaks** ("cut hazard pay everywhere", "raise all allowances 10% at establishment X"): reuse `apply_salary_scale_adjustment()` from `R/salary_scale_utils.R` *as-is* on `allowance_scale_dt` — it's already parameterized by `salary_col`, so calling it with `salary_col = "amount"` and `key_col = "allowance_type"` (or a population group column) works unchanged.
- **Compound targeting** ("cut hazard pay only at establishment X, leave everything else"): already expressible via the three-slot `allowance_policy$policy_table` with `group_cols = c(<population col>, "allowance_type")` and explicit override rows — this is exactly what `resolve_policy_table()`'s multi-column group join already supports (confirmed against the existing `dispatch_param: multi-column group_cols join works correctly` test).
- **Total-allowance-for-a-population** changes: the `total_allowance_multiplier` default/policy_table slot described above.

## Wiring into `simulate_scenario()` / `simulate_horizon()`

`R/simulate_horizon.R`:
- New optional args: `allowance_policy = NULL`, `allowance_dt = NULL` (prior register, like `pensioner_register`), `allowance_growth_rate = 0` (COLA-style growth for the scale, applied each period via `apply_salary_scale_adjustment(allowance_scale_dt, adjustment = 1 + growth, salary_col = "amount")` — reused again).
- New step in `simulate_scenario()`, after hiring/movements and before aging (so new hires/movers get allowances assigned in the same period their group is finalized):
  ```
  1. retirement
  2. exits
  3. movements
  4. hiring
  5. allowances   <- new (simulate_allowances())
  6. aging
  7. COLA (salary growth, and now allowance_scale growth)
  ```
- New helper `.active_allowance_bill()` next to `.active_wage_bill()` in `R/simulate_horizon.R` — same active-contract filter, summed over the current period's `allowance_dt` rows.
- New `$comparison` columns: `allowance_cost_start`, `allowance_cost_end`, `total_wage_bill_start`, `total_wage_bill_end` (= existing `wage_bill_*` + `allowance_cost_*`). Existing `wage_bill_start/end` columns and `.active_wage_bill()` are **not** redefined — this is additive, so no existing test should need to change.
- `allowance_dt` accumulates across periods in the horizon loop exactly like `pensioner_register`; returned on the output object as `out$allowance_dt` gated behind `return_microdata` (like `contract_dt`/`personnel_dt` panels) since it's row-per-contract-per-type-per-period and can get large — the aggregated `$comparison` columns are always available regardless.

`R/horizon_class.R`: add `total_wage_bill_end`/`allowance_cost_end` to the candidate column list in `summary.horizon()` (currently `intersect()` against a fixed vector) so they show up in the default one-line summary when present.

`R/zzz.R`: add any new NSE-referenced column names (`allowance_type`, `amount`, `allowance_lcu`, etc.) to `utils::globalVariables()` per existing convention.

## Tests

New: `tests/testthat/test-allowance_core.R`, `test-allowance_update.R`,
`test-simulate_allowances.R`, following the existing per-module test file
split. Key cases to cover:
- `estimate_historical_allowance_scale()` unconditional-mean math on a small
  synthetic panel (verify it matches manual `mean()` including zeros).
- `resolve_allowance_amounts()`: scalar default, group-level `policy_table`,
  compound (population × allowance_type) targeting, `total_allowance_multiplier`.
- `simulate_allowances()`: `allowance_policy = NULL` no-op; new hires and
  movers get (re)assigned; register accumulates correctly across periods.
- `simulate_horizon()` integration: `total_wage_bill = wage_bill +
  allowance_cost` reconciles each period; existing wage-bill invariant tests
  still pass unmodified (regression guard that this feature is additive).

## Documentation (after tests are clean, per the required workflow order)

- Roxygen for all new exported functions, written for the analyst audience
  (explain *why* unconditional-mean assignment is used, the double-counting
  caveat, and a worked example building an allowance scale from
  `bra_hrmis_allowance` + assigning it against `bra_hrmis_contract`).
- Wiki: new page (check `Doc-Impact-Map.md` for where this fits alongside
  the other module pages) describing the allowance module's architecture and
  its relationship to the salary-scale pattern.

## Verification plan

1. Per the repo's required workflow, before writing real implementation code
   I'll first come back with the **Propose** step (exact `debugonce()`/test
   calls against realistic inputs, e.g. built from `bra_hrmis_allowance` +
   `bra_hrmis_contract`) for explicit approval — this plan is the design,
   not that step.
2. `devtools::test()` — bar is 0 errors/warnings/skipped, including full
   existing suite (currently 949 passing) to confirm no regressions to the
   wage-bill invariant or other modules.
3. Manual smoke test: build `allowance_scale_dt` from
   `estimate_historical_allowance_scale()` on the bundled
   `bra_hrmis_allowance`/`bra_hrmis_contract`/`bra_hrmis_personnel`, run a
   multi-period `simulate_horizon()` with `allowance_policy` active and
   `salary_col = "base_salary_lcu"`, and manually reconcile
   `total_wage_bill_end` against `wage_bill_end + allowance_cost_end` and
   against a hand-summed check of `allowance_dt`.
4. `devtools::document()` once tests are clean, then spot-check generated
   `man/*.Rd` for the new functions.