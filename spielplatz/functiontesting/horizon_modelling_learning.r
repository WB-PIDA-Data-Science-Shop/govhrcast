library(data.table)
library(govhrcast)

panel <- bra_hrmis_contract
pt    <- bra_hrmis_personnel

sal_scale <- panel[
  ref_date == as.Date("2016-09-01") &
  !is.na(gross_salary_lcu) & gross_salary_lcu > 0,
  .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
  keyby = "est_id"
]

hz <- simulate_horizon(
  contract_dt        = panel,
  personnel_dt       = pt,
  salary_scale_dt    = sal_scale,
  n_periods          = 10L,
  ref_date           = as.Date("2016-09-01"),
  period_unit        = "year",
  birth_date_col     = "birth_date",
  scenario_name      = "Baseline projection",
  is_baseline        = TRUE,
  salary_growth_rate = 0.03,
  pension_cola_rate  = 0.02,
  retirement_policy  = list(
    eligibility_type  = "age_only",
    min_age           = 60,
    pension_type      = "db",
    pension_params    = list(
      accrual_rate    = 0.02,
      ref_wage_col    = "gross_salary_lcu",
      max_years       = 35,
      replacement_cap = 0.80
    )
  ),
  exit_policy = list(
    mode         = "fixed_rate",
    fixed_rate   = 0.04,
    active_types = c("permanent", "temporary", "fterm")
  ),
  movement_policy = list(
    group_cols           = "est_id",
    promotion_multiplier = 1.0,
    promotion_strategy   = "tenure",
    tenure_col           = "tenure_years",
    salary_scale         = sal_scale
  ),
  hiring_policy = list(
    mode             = "flow",
    group_cols       = "est_id",
    replacement_rate = 1.0
  )
)

hz                  # print.horizon — summary header
summary(hz)         # terminal-year numbers

# Year-by-year trajectory
hz$comparison[, .(
  year         = format(period_date, "%Y"),
  headcount    = n_headcount_end,
  wage_bill    = round(wage_bill_end / 1e6, 2),   # in millions
  pensions     = round(pension_cost_total / 1e6, 2),
  retirements  = n_retirements,
  hires        = n_hires,
  cola_pct     = round(cola_effect_pct_of_end_bill * 100, 1)
)]