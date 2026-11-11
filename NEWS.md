# govhrcast 0.1.0

* Initial CRAN submission.
## Breaking changes

- `column_spec_args` now comes from the new `define_column_spec()`, for example
  `define_column_spec(salary_col = "base_salary_lcu")`. A plain list such as
  `list(salary_col = "base_salary_lcu")` now stops with an error that points to it.
  With harmonized data nothing changes: the default is `define_column_spec()`.
- `output_control_args` now comes from the new `define_output_control()`, for example
  `define_output_control(return_microdata = TRUE)`. A plain list now stops with an
  error that points to it. The defaults are unchanged.
- `simulate_horizon()`: the start date is now called `baseline_date`, and it defaults to
  `NULL`, which starts from the latest
  snapshot both tables hold, instead of today's date. Data with no snapshot date column
  must now give `baseline_date`. `design_allowance_policy_status_quo()` and
  `estimate_baseline_allowance_scale()` use the same name.
- The module functions (`simulate_retirement()`, `simulate_exits()`,
  `simulate_promotions_transfers()`, `simulate_hiring()`, `simulate_allowances()`) now
  call their date `as_of_date`. The harmonized `ref_date` column is unchanged.
- Status-quo movement rates now count someone with several contracts in the group of their
  main contract (latest start date, then highest salary, then lowest contract ID), not
  whichever row came first, so they no longer depend on row order. Rates change, and the
  estimate now also reads `contract_id_col` and `salary_col`. A blank salary or start date
  no longer wins the main contract.
- `simulate_horizon()`: the `base_year` argument is removed. It was never used.

## New features

- `compute_tenure()` gains a `group_cols` argument to compute tenure within
  any grouping (contract, paygrade, establishment) using the same
  interval-union algorithm.
- `setnafill_personnel_attr()` added for backfilling missing personnel
  attributes across panel snapshots using LOCF for time-invariant fields
  and delta correction for age and tenure.

## Bug fixes

- `get_active_contracts()` no longer filters on `start_date <= ref_date`,
  fixing silent zero-retirement and zero-hiring results for snapshots where
  `start_date` is missing.
- `simulate_horizon()` with `return_microdata = TRUE` now returns a full
  stacked panel (one snapshot per period) for both `contract_dt` and
  `personnel_dt`, with `ref_date` correctly stamped per period.

## Documentation

- `simulate_retirement()` policy_params documentation moved from `@param`
  into a dedicated `@section Policy Parameters:` block to fix rendering
  in Positron and pkgdown.
- `compute_tenure()` and `compute_tenure_panel()` roxygen updated to
  document `group_cols`.