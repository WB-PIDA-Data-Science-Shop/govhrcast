# Retirement hazard-rate modeling — implementation plan

## Context

Retirement eligibility in `govhrcast` is currently 100%-deterministic:
`identify_eligibility()` in `R/retirement_core.R` compares each person's
age/tenure against a threshold (`eligibility_type` ∈ `age_only` /
`tenure_only` / `age_and_tenure` / `custom`) and sets `retire = 1` for every
match, always. In reality, most governments have a mandatory retirement age,
but a meaningful share of personnel retire before it — a fixed threshold
can't represent that.

This plan adds a 5th `eligibility_type`, `"hazard_rate"`: retirement becomes
a group-level rate applied to a count of people, who are then selected the
same way `compute_status_quo_exits()` already selects exiting personnel
(random draw or ranked-by-characteristic), with an optional mandatory age
above which everyone is force-retired. The rate table itself is estimated
using `govhr::estimate_decrement_rates()` — an already-built, exposure-
weighted, multi-period actuarial estimator — rather than anything bespoke.

## Decisions already made (via prior discussion — do not re-litigate these)

- Unmatched hazard-rate cells default to `0` unless smoothing is enabled
  (see the one open item below on whether to default `smooth = TRUE`).
- A mandatory retirement age reuses the existing `min_age` slot: for
  `eligibility_type == "hazard_rate"`, `min_age` (when supplied) is a hard
  ceiling — everyone at or above it retires with certainty — not a
  threshold that turns the hazard mechanism on.
- Retirement selection reuses the exit module's group-count-then-select
  framework (`compute_status_quo_exits()` / `.select_exits()`), not a
  per-row independent Bernoulli draw. `n_retire = round(n_active * rate)`
  per group, then individuals are selected via `.select_exits()`'s existing
  `"random"` or rank-by-column strategies.
- `simulate_scenario()` / `simulate_horizon()` get an explicit `seed`
  argument for reproducibility — we are not relying on the caller to
  `set.seed()` themselves. The seed governs the `"random"` selection
  strategy specifically (rank-based selection is already deterministic
  given the data, so it needs no seed).
- Hazard rates are estimated from the full historical panel (not a single
  baseline snapshot, unlike the allowance scale) — retirement rates need
  multiple periods of exposure to be stable at the per-age level.

## 1. `govhr::estimate_decrement_rates()` — verified, do not reimplement

Confirmed directly in the `govhr` source (`R/demographics.R`). This already
does the hard part:

- Walks **every** consecutive snapshot pair in the panel via
  `roll_snapshot_pairs()`, tracking individuals by ID from T0 → T1 (not
  independent per-snapshot aggregation).
- Pools exposure and exits **across all pairs before dividing**
  (exposure-weighted pooling, not a mean of per-period rates) — a thin
  period-pair doesn't swing the estimate as much as a large one.
- Derives the outcome vocabulary directly from whatever `employment_status`
  values are present in the data. `govhrcast` already uses `"pensioner"` as
  the retirement status value (confirmed in `R/retirement_update.R`), so no
  status-value mapping is needed — it falls out for free.
- Requires ≥ 2 distinct snapshot dates (errors otherwise) — matches the
  existing `.has_panel_` gate already used for exit/movement rate
  estimation in `simulate_horizon()`.
- Companion `smooth_decrement_rates()` (loess, falling back to linear
  interpolation / flat-fill for thin data) graduates and gap-fills the raw
  per-age curve — this is a real alternative to zero-filling unmatched
  ages (see open item #1).
- `compute_service_table()` (full `px`/`lx`/`Tx`/`ex` life table) is
  available but **not needed** for this feature — the retirement draw only
  needs `decrement_rate` per age/group, not a chained survivorship curve.
  Not in scope for this plan.

**Signature** (for reference):
```r
govhr::estimate_decrement_rates(
  personnel, age_col, status_col, personnel_id_col, ref_date_col, group_cols
)
# returns: age_col, group_cols, status_col, pop, exits, decrement_rate, n_periods
# one row per age / group_cols / outcome type (status_col value)

govhr::smooth_decrement_rates(
  decrements, age_col, status_col, group_cols, span = 0.75
)
```

## 2. New function: `estimate_retirement_hazard_rates()` — thin adapter only

Location: `R/retirement_core.R` (or a new `retirement_hazard.R` if the file
convention prefers splitting — follow whatever the codebase's existing
one-concern-per-file pattern suggests).

```r
estimate_retirement_hazard_rates(
  panel_personnel_dt,
  group_cols        = NULL,      # e.g. "age_yr_int", or c("age_yr_int", "est_id")
  age_col           = "age",     # fractional age column on panel_personnel_dt
  personnel_id_col  = "personnel_id",
  ref_date_col      = "ref_date",
  status_col        = "employment_status",
  retire_status     = "pensioner",
  smooth            = FALSE      # see open item #1
)
```

Steps:
1. Add an integer age column (`age_yr_int := as.integer(age)` — matches
   `govhr::estimate_decrement_rates()`'s own internal truncation via
   `as.integer(get(age_col))`, so no separate flooring convention needs to
   be invented).
2. Call `govhr::estimate_decrement_rates()` with `group_cols` including
   `age_yr_int` (this is the caller's `group_cols`, not a new concept —
   `age_yr_int` must be one of the columns the caller passes in).
3. Filter to `status_col == retire_status` (default `"pensioner"`).
4. If `smooth = TRUE`, pass through `govhr::smooth_decrement_rates()` before
   filtering (smoothing needs the full outcome vocabulary per age, not just
   the retirement rows, to graduate correctly — check the exact call order
   against `compute_service_table()`'s usage in `govhr` as a reference,
   since it calls smoothing before extracting a single outcome row).
5. Rename `decrement_rate` → `hazard_rate`.
6. Return `group_cols + hazard_rate`, directly usable as `policy_table`.

This replaces the custom per-snapshot age-enrichment/stock-counting
estimator sketched in an earlier draft of this plan — unnecessary now that
`govhr::estimate_decrement_rates()` already does correct cohort-tracked,
exposure-weighted pooling.

## 3. Promote `.select_exits()` to shared infrastructure

Currently a private function in `R/exit_core.R` (verified: not referenced
by any test file, only used internally within that file — safe to move
without breaking test coverage). It is already fully generic (pool of ids,
target count, ranking/random strategy, no exit-specific logic), so:

- Move `.select_exits()` from `R/exit_core.R` to `R/utils.R`, alongside
  `resolve_policy_table()` / `dispatch_param()` — the existing home for
  cross-module shared mechanics.
- Update `R/exit_core.R`'s call site — no behavior change, pure relocation.
- Consider whether a small rename makes the shared home clearer (e.g.
  `.select_personnel_subset()`), since "exits" is no longer accurate once
  retirement uses it too. Not required, but worth deciding now rather than
  leaving a misleadingly-named shared helper.

## 4. New function: `compute_status_quo_retirements()`

Location: `R/retirement_core.R`. Structurally mirrors
`compute_status_quo_exits()` in `R/exit_core.R` closely enough to use it as
a direct template — read that function in full before writing this one.

```r
compute_status_quo_retirements(
  contract_dt,
  personnel_dt,
  policy_params,       # canonical three-slot: group_cols, policy_table (hazard_rate), defaults
  ref_date,
  personnel_id_col  = "personnel_id",
  contract_type_col = "contract_type",
  age_col           = "age",
  birth_date_col    = "birth_date"
)
```

Logic:
1. Restrict to active, non-pensioner candidates — reuse
   `.filter_active_candidate_pool()` (already exists in
   `retirement_core.R`, used by `identify_eligibility()`) rather than
   duplicating the active-contract filter.
2. Compute age (prefer pre-computed `age_col` on `personnel_dt`, else
   `compute_age()` — same precedence `identify_eligibility()` already
   follows), then `age_yr_int := as.integer(age)`.
3. **Mandatory-age pre-filter**: resolve `min_age` per row/group (via
   `resolve_policy_table()`, same as today). Anyone with
   `!is.na(min_age) & age >= min_age` is force-selected — `retire = 1`,
   skip the rate/selection step for them entirely. This must happen
   *before* step 4's grouping, so forced retirees aren't double-counted
   against a group's `n_retire` budget.
4. For the remaining pool, group by `policy_params$group_cols` (must
   include `age_yr_int`, matching the hazard table's keys), resolve
   `hazard_rate` per group via `resolve_policy_table()` (default `0` for
   unmatched — see open item #1), compute
   `n_retire = round(n_active * hazard_rate)` per group — **plain
   `round()`, not `stochastic_round()`** (mirrors `exit_core.R`'s
   `compute_status_quo_exits()` convention, not `movement_core.R`'s
   fractional-count convention — confirm this reads correctly against
   `compute_status_quo_exits()`'s actual grouped-path code before
   implementing, since that's the parity target).
5. Select individuals per group via the now-shared `.select_exits()` (see
   §3), using `policy_params$defaults$retire_strategy %||% "random"` —
   name the default slot `retire_strategy` (not `exit_strategy`) to keep
   the policy vocabulary retirement-specific even though the mechanism is
   shared.
6. Return `data.table(personnel_id = c(forced_ids, selected_ids))`,
   deduplicated.

## 5. Dispatch in `simulate_retirement()`

`R/simulate_retirement.R`, around the single existing call to
`identify_eligibility()` (line ~485): branch on
`policy_params$defaults$eligibility_type`:
- `"age_only"` / `"tenure_only"` / `"age_and_tenure"` / `"custom"` →
  existing `identify_eligibility()` path, unchanged.
- `"hazard_rate"` → new `compute_status_quo_retirements()` path.

Downstream consumers of the eligibility result (pension computation,
`retirement_update.R`'s reclassification to `"pensioner"`) should not need
to change, provided `compute_status_quo_retirements()`'s return shape
(`personnel_id` of retirees) matches what those consumers already expect
from the `identify_eligibility()` path — verify this against
`simulate_retirement.R`'s post-eligibility code before assuming it's a
drop-in.

## 6. Wiring into `simulate_scenario()` / `simulate_horizon()`

- Mirror the existing exit/movement auto-estimation pattern exactly: when
  `retirement_policy$defaults$eligibility_type == "hazard_rate"` and
  `retirement_policy$policy_table` is `NULL`, call
  `estimate_retirement_hazard_rates()` on the panel in the prologue, gated
  behind the existing `.has_panel_` check (`≥ 2` snapshots — same variable
  already computed at `R/simulate_horizon.R:1078`). If `.has_panel_` is
  `FALSE`, error explicitly (do not silently fall back) — a single
  snapshot cannot estimate a stable per-age hazard rate the way the
  allowance scale can be estimated from one snapshot.
- **Reproducibility**: add `seed = NULL` to `simulate_scenario()` and
  `simulate_horizon()`. In `simulate_horizon()`, call `set.seed(seed)`
  once before entering the period loop (not once per period — the RNG
  stream should continue naturally across periods so each period draws
  fresh values; re-seeding every period would create correlated/repeated
  patterns across periods). `simulate_scenario()` called standalone
  (single period) should also accept and apply its own `seed` for
  independent reproducibility. Only the `"random"` strategy branch of
  `.select_exits()` consumes randomness here — rank-based selection is
  already deterministic given the data and needs no seed.

## 7. `R/validation.R`

Extend `check_retirement_inputs()`:
- `"hazard_rate"` joins the valid `eligibility_type` set.
- When `eligibility_type == "hazard_rate"` is explicitly set, the existing
  `min_age`/`min_tenure`-required checks (currently gated on
  `age_only`/`tenure_only`/`age_and_tenure`) do **not** apply —
  `min_age` is optional (mandatory-ceiling semantics) and `min_tenure` is
  unused by this type.
- If `policy_table` is supplied, validate it contains a `hazard_rate`
  column and whatever `group_cols` were declared.

## 8. Tests

- `test-retirement_core.R`: `compute_status_quo_retirements()` — mandatory
  age forces retirement regardless of hazard rate; group-count rounding;
  `"random"` vs. rank-by-column selection; reproducibility under a fixed
  seed (two calls, same seed, same result).
- New `test-estimate_retirement_hazard_rates.R`: on a small synthetic
  multi-snapshot panel with known retirement counts, verify the adapter's
  output matches a hand-computed rate from `govhr::estimate_decrement_rates()`
  directly (i.e. the test should call `govhr::estimate_decrement_rates()`
  itself on the same fixture and compare, not hardcode an expected number —
  this catches drift if `govhr`'s pooling logic ever changes).
- `test-utils.R`: relocated `.select_exits()` — confirm existing exit-side
  test coverage (if any exists indirectly via `compute_status_quo_exits()`
  tests) still passes after the move; add direct unit tests for it now
  that it's shared infrastructure rather than a private exit-only helper.
- `simulate_horizon()` integration: `eligibility_type = "hazard_rate"` end
  to end, confirming auto-estimation fires when `policy_table = NULL`,
  errors on a single-snapshot panel, and the `seed` argument produces
  identical retiree sets across repeated runs.

## Open items requiring sign-off before implementation

1. **Zero-fill vs. smoothing for unmatched/thin age cells.** Prior answer
   was "default to 0." Now that `govhr::smooth_decrement_rates()` exists
   and interpolates/graduates rather than zero-filling, should
   `estimate_retirement_hazard_rates()` default to `smooth = TRUE`
   (better-behaved curve, but changes rates at ages with thin data) or
   keep `smooth = FALSE` as the default with `smooth = TRUE` available as
   an opt-in? This plan currently defaults to `FALSE` (least surprising /
   most consistent with other estimators in the package being unsmoothed
   by default) but flags it as reversible.
2. **Confirm `compute_status_quo_retirements()`'s output shape** matches
   what `simulate_retirement.R`'s post-eligibility code (pension
   computation, `retirement_update.R`) expects from the existing
   `identify_eligibility()` path, before assuming it's a drop-in.
3. **`.select_exits()` rename** — keep the name as-is post-relocation, or
   rename to something retirement-neutral now that it's shared? Purely
   cosmetic, but worth a deliberate choice rather than an accidental
   exit-flavored name living in `utils.R`.