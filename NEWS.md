# govhrcast 0.1.0

* Initial CRAN submission.
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