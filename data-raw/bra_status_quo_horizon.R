## A five-year status-quo projection of the bundled Alagoas data, saved as
## package data (bra_status_quo_horizon) so that a dashboard can show a
## baseline without running the model at start-up.
##
## Status quo: retirement, other exits, promotions and hiring continue at the
## rates seen in the panel up to the 2017 snapshot. Pay does not grow
## (salary_growth_rate = 0), so the results are in 2017 money.
##
## Exits and hiring go together: the historical hiring rates include the hires
## that replaced people who left, so hiring at those rates without exits makes
## the payroll grow far faster than it ever did.
##
## Rerun this script whenever the model changes: the saved object is a snapshot
## of what simulate_horizon() returned when it was built.

library(data.table)
devtools::load_all()

baseline_date <- as.Date("2017-09-01")   # the last snapshot with pay recorded
groups        <- c("est_id", "paygrade")  # establishment x pay grade

# ---------------------------------------------------------------------------
# 1. Pay table: mean pay of each establishment and grade at the baseline
# ---------------------------------------------------------------------------
# Pensions sit in the same pay column, and in this data pensioners have a blank
# contract type, so they are dropped here by their status. Hires and people
# promoted into a group are paid that group's value.
pensioners <- bra_hrmis_personnel[ref_date == baseline_date &
                                    employment_status %in% "pensioner", personnel_id]

salary_scale_dt <- prepare_salary_scale(
  bra_hrmis_contract[!personnel_id %in% pensioners],
  baseline_date, groups, mean, na.rm = TRUE
)

# ---------------------------------------------------------------------------
# 2. Status-quo policies; their rates are estimated from the panel inside
#    simulate_horizon()
# ---------------------------------------------------------------------------
retirement_policy <- design_retirement_policy_status_quo(
  pension = pension_db(accrual_rate = 0.02, max_years = 35, replacement_cap = 0.80)
)
exit_policy     <- design_exit_policy_status_quo(group_cols = groups)
movement_policy <- design_movement_policy_status_quo(groups, movement_type = "promotion")
hiring_policy   <- design_hiring_policy_status_quo(group_cols = groups)

# ---------------------------------------------------------------------------
# 3. Five years from the 2017 baseline
# ---------------------------------------------------------------------------
bra_status_quo_horizon <- simulate_horizon(
  contract_dt       = bra_hrmis_contract,
  personnel_dt      = bra_hrmis_personnel,
  salary_scale_dt   = salary_scale_dt,
  n_periods         = 5L,
  retirement_policy = retirement_policy,
  exit_policy       = exit_policy,
  movement_policy   = movement_policy,
  hiring_policy     = hiring_policy,
  baseline_date     = baseline_date,
  output_control_args = define_output_control(
    scenario_name    = "Status quo",
    is_baseline      = TRUE,
    return_microdata = TRUE               # keep the contract and personnel tables
  ),
  seed = 20170901L                        # who retires, moves and is hired is drawn at random
)

bra_status_quo_horizon

usethis::use_data(bra_status_quo_horizon, overwrite = TRUE, compress = "xz")
