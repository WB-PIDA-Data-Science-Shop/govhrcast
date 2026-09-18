# CLAUDE.md

# CLAUDE.md — govhrcast

This file provides guidance to Claude Code when working with code in this repository.

## What this is
`govhrcast` is an R package that simulates public-sector workforce and
wage-bill dynamics from HRMIS microdata (contract- and personnel-level). It
is used by World Bank country teams to model retirement, hiring,
promotion/transfer, and attrition policy scenarios and to project their
fiscal impact. Companion package `govhr` (not in this repo, listed under
`Remotes:` in `DESCRIPTION`, a `Suggests` dependency used only in
examples/vignettes) produces the source-data harmonization step that
generates this package's expected input schema. Deployment target is Posit
Connect on World Bank infrastructure. Not yet published — currently
single-user.

## Environment & common commands
- R package, dependency-managed via `renv` (`renv.lock`, `.Rprofile` sources
  `renv/activate.R`). Run `renv::restore()` after cloning/pulling if
  dependencies drift.
- Core runtime dependency is `data.table`; nearly all functions operate on
  `data.table` objects with heavy use of non-standard evaluation (`get()`,
  `:=`, `on = group_cols`), not `dplyr`/base data.frame idioms. After
  writing `data.table` code, it's fine to point out where a `fastverse`
  package (e.g. `collapse`, `kit`) could improve performance — flag it as a
  suggestion rather than substituting it in, so it can be reviewed and
  tried deliberately rather than silently swapped in.
- Roxygen2 (markdown mode) generates `man/` and `NAMESPACE` — never
  hand-edit `NAMESPACE` or `man/*.Rd`.

```r
# Install/sync dependencies
renv::restore()

# Regenerate NAMESPACE and man/ pages after changing roxygen comments
devtools::document()

# Full test suite
devtools::test()
# or
testthat::test_dir("tests/testthat")

# Single test file
devtools::test_active_file("tests/testthat/test-simulate_retirement.R")
# or
testthat::test_file("tests/testthat/test-simulate_retirement.R")

# Load package for interactive/manual exploration without installing
devtools::load_all()

# Full CMD check (what CI runs)
devtools::check()

# Rebuild example datasets from source-raw scripts
source("data-raw/bra_hrmis_data.R")
```

CI (`.github/workflows/R-CMD-check.yaml`) runs `rcmdcheck` across
macOS/Windows/Ubuntu (release, devel, oldrel-1). `test-coverage.yaml` runs
`covr` and uploads to Codecov.

## Before touching any code: do your own audit
Don't assume you already know this codebase's architecture, conventions, or
outstanding issues — including from the summary below, which may drift out
of date. On first engagement with this repo (or whenever asked to
"re-audit"), read the R source yourself and confirm the model below still
holds: which modules use the three-slot policy pattern, where shared
validation helpers are and aren't applied, actual naming conventions in use
in the data, and function dependencies across files — and, just as
important, the actual simulation model logic and rules themselves (e.g.
retirement eligibility conditions, how historical rates are estimated for
each module, what each hiring `mode` actually does, pension formula
mechanics). Structural familiarity with the code isn't enough — a fix that
respects the patterns but misunderstands the underlying rule is still
wrong. Summarize findings — architecture, domain logic, inconsistencies,
likely bugs — before proposing any fix. Don't fix opportunistically while
auditing.

## Development workflow (required order — do not skip or reorder)
1. **Propose.** Before modifying or writing a function's real implementation,
   show the exact call(s) you'd use to exercise it (e.g.
   `debugonce(fn); fn(test_args)`, or a short script with realistic inputs).
   Wait for explicit approval before proceeding.
2. **Implement** the change.
3. **Test.** Run `devtools::test()`. Bar: 0 errors, 0 warnings, 0 skipped.
   Do not move to the next step until this is clean. If a test failure
   reveals a design problem rather than a simple bug, stop — do not patch
   around it. Propose a fix and wait for approval before implementing it.
4. **Document.** Write/update roxygen only after step 3 is clean — roxygen
   should never describe untested behavior. Write roxygen so it's usable by
   both R developers and non-advanced R users with an analytics background
   (the package's actual user base) — explain what an argument does and why
   it matters, not just its type.
5. **Wiki.** Update the relevant GitHub wiki page(s) last. Check
   `Doc-Impact-Map.md` for which wiki pages a given source file maps to.

## Architecture

### The four-module simulation pipeline
The package's entire purpose is expressed through one call chain:

```
simulate_horizon()          # multi-period orchestrator (loops n_periods times)
  └─ simulate_scenario()    # single-period orchestrator, fixed step order:
       1. simulate_retirement()
       2. simulate_exits()                (non-retirement attrition)
       3. simulate_promotions_transfers()
       4. simulate_hiring()
       5. aging (age_col / tenure_col += period_fraction)
       6. COLA (salary_scale_dt and contract salaries scaled by growth rate)
```

Each module is optional — pass its policy list as `NULL` to hold that
process at zero/inactive and isolate the fiscal effect of reforming only
the others. `simulate_horizon()` returns an S3 `horizon` object
(`R/horizon_class.R`, `print.horizon`/`summary.horizon`) whose `$comparison`
(aka `$summary_dt`) has one row per period decomposing wage-bill change into
exit savings, movement effect, hiring effect, and inflation effect.

### Per-module file layout
Every simulation module is split into three files following the same
pattern — look here first when tracing a bug in one process:
- `*_core.R` — pure computation: eligibility/demand estimation, rate
  estimation from historical panels (`estimate_historical_exit_rates`,
  `estimate_movement_baseline`, `estimate_historical_hiring_rates`),
  selection logic. No mutation of input tables.
- `*_update.R` — state mutation: applies the selected changes to
  `contract_dt`/`personnel_dt` (e.g. `update_contracts_for_retirees`,
  `identify_movers`, `generate_new_contracts`).
- `simulate_<module>.R` — thin public-facing wrapper (`simulate_retirement`,
  `simulate_exits`, `simulate_promotions_transfers`, `simulate_hiring`) that
  validates inputs, calls the core/update functions, and returns a summary
  + updated tables.

`R/retirement_pension.R` is the exception with a single-purpose file: it
holds `compute_pension()` dispatching to `compute_db_pension`,
`compute_dc_pension`, `compute_flat_pension`, `compute_hybrid_pension`,
`compute_rate_pension`, and `compute_custom_pension` (user-supplied formula
string).

### The three-slot policy pattern
Every module's policy argument (`retirement_policy`, `exit_policy`,
`movement_policy`) follows the same shape, resolved centrally in
`R/utils.R`:

```r
list(
  group_cols   = "est_id",     # NULL => scalar/uniform policy
  policy_table = exit_dt,      # NULL => use defaults for every row
  defaults     = list(...)     # fallback values, used for unmatched groups
)
```

`dispatch_param()` resolves a single parameter to a per-row vector (scalar
repeat, or left-join against `policy_table` with `defaults` filling
unmatched rows/missing columns); `resolve_policy_table()` resolves an entire
`defaults` list at once. `hiring_policy` does not use this exact 3-slot
shape — it takes a `mode` (`"status_quo"`, `"flow"`, `"stock"`,
`"combined"`) plus mode-specific keys (see `simulate_hiring.R` roxygen).
`make_status_quo_policies()` (`R/utils.R`) is a convenience constructor that
estimates baseline policies for all four modules directly from panel data.

This pattern is the default shape for any new configurable/policy-like
feature — reach for it first. If a different pattern seems more efficient
or maintainable for a specific case, propose it explicitly with the
tradeoffs — don't deviate silently.

### User-supplied formulas: sandboxed, not `eval(parse())`
`eligibility_type = "custom"` (retirement) and `pension_type = "custom"`
accept a user-written R expression string (e.g.
`"age >= 60 & tenure_years >= 5"`, `"salary * 0.02 * tenure_years"`). These
are **not** evaluated with raw `eval(parse())`. `validate_eligibility_rule()`
/ `validate_pension_formula()` in `R/validation.R` parse the string with
`str2lang()` and walk the AST via `.check_safe_calls()` against an explicit
allow-list (`.ALLOWED_RULE_CALLS` / `.ALLOWED_FORMULA_CALLS`), rejecting any
call not on the list and any non-call/non-atomic/non-name node. When adding
new formula/rule features, extend the allow-list deliberately rather than
loosening the AST walk.

### Wage bill accounting invariant
`.active_wage_bill()` (internal, `R/simulate_horizon.R`) is the single
source of truth for what counts as payroll: sum of `salary_col` over
contract rows where `contract_type_col` is not `"pensioner"`/`"inactive"`
and salary is non-NA. Pensioner costs live only in `pensioner_register` (a
separate ledger tracking `pension_amount`, keyed by
`personnel_id`/`period_date`) and must never be folded into wage-bill
totals. Preserve this separation in any change that touches wage-bill
computation — it's asserted throughout the test suite.

### Validation layer
`R/validation.R` centralizes input checking: generic helpers
(`validate_datatable`, `validate_columns_exist`, `validate_date_format`,
`validate_choice`, ...) plus per-module composite checkers
(`check_retirement_inputs`, `check_hiring_inputs`, `check_movement_inputs`)
called at the top of each `simulate_*()` wrapper. Add new parameter
validation here rather than inline in module code.

### Global variables / NSE
`R/zzz.R` declares `utils::globalVariables()` for every symbol referenced
via `data.table` NSE (bare column names inside `[...]`/`:=`) to keep
`R CMD check` clean. When introducing a new column name used in NSE
context, add it to this list.

## Known hard constraints (don't relitigate these without asking)
- Custom pension formulas are **arithmetic-only** — no `ifelse`/comparison
  logic.
- Time-varying policy (e.g. front-loaded severance) is handled by the user
  chaining multiple `simulate_horizon()` calls, not by making
  `simulate_horizon()` itself period-aware.
- `pensioner_register` across chained calls is combined by the user via
  `rbindlist()` on each call's output register — `simulate_horizon()` does
  not accept a register as input.

## Data
Bundled example datasets (`data/*.rda`, loaded lazily): `bra_hrmis_contract`,
`bra_hrmis_personnel`, `bra_hrmis_allowance` — real government payroll data
from Alagoas, Brazil, keyed by `personnel_id` (person), `contract_id`
(contract), `est_id` (establishment), and snapshotted by `ref_date`.
Regenerated from `data-raw/bra_hrmis_data.R` and
`data-raw/prep_allowance_module.R`. Real analyses join the `contract`,
`personnel`, and `allowance` modules on
`personnel_id`/`contract_id`/`ref_date`. Treat data governance and handling
accordingly even in test/dev work.

## Documentation surfaces — three, kept separate
- **Vignettes** (pkgdown site): user-facing, for analysts in developing
  countries, many new to R (often coming from Stata/SPSS). Not yet written —
  the plan is to draft them collaboratively with Claude's help. Keep that
  same audience in mind for vignette prose as for roxygen: usable by
  non-advanced R users with an analytics background, not just R developers.
- **GitHub wiki**: contributor-facing (architecture, patterns, validation
  conventions, contribution workflow).
- **roxygen**: function-level reference, written last per the workflow above.

## Longer-term direction
- The ultimate goal is a dashboard to visualize `simulate_horizon()` results,
  and to run a wide range of policies and compare their results side by side
  in that same dashboard. Keep this in mind when shaping function outputs —
  they should stay dashboard-friendly (e.g. tidy, combinable across runs),
  not just correct for a single ad hoc call.
- Some relevant tooling already exists in the `govhrapp` repo (same GitHub
  org) — worth checking before building dashboard-adjacent pieces from
  scratch.
- This capability is intended to eventually be used in, and built on
  further within, a different project — so avoid overfitting design choices
  to govhrcast alone where a more general shape is just as easy.

## Notes
- `spielplatz/` is a scratch/experimentation directory (excluded from the
  build via `.Rbuildignore`) — not part of the package surface.