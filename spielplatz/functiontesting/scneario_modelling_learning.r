library(data.table)
library(govhrcast)

ct <- bra_hrmis_contract[ref_date == as.Date("2016-09-01")]
pt <- bra_hrmis_personnel[ref_date == as.Date("2016-09-01")]

sal_scale <- ct[
  !is.na(gross_salary_lcu) & gross_salary_lcu > 0,
  .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
  keyby = "est_id"
]

baseline_mat <- estimate_movement_baseline(
  contract_dt       = bra_hrmis_contract,
  group_cols        = "est_id",
  personnel_id_col  = "personnel_id",
  ref_date_col      = "ref_date",
  contract_type_col = "contract_type_code",
  start_date_col    = "start_date",
  end_date_col      = "end_date"
)

# Run a single period manually
result <- simulate_scenario(
  contract_dt        = ct,
  personnel_dt       = pt,
  salary_scale_dt    = sal_scale,
  pensioner_register = data.table(),
  period_date        = as.Date("2017-09-01"),
  salary_growth_rate = 0.03,
  pension_cola_rate  = 0.02,
  period_fraction    = 1,
  scenario_id        = 1L,
  scenario_label     = "baseline",
  is_baseline        = TRUE,
  retirement_policy  = list(
    eligibility_type = "age_only",
    min_age          = 60,
    pension_type     = "flat",
    pension_params   = list(flat_amount = 1000)
  ),
  exit_policy = list(
    mode         = "fixed_rate",
    fixed_rate   = 0.04,
    active_types = c("permanent", "temporary", "fterm")
  ),
  movement_policy = list(
    group_cols           = "est_id",
    baseline_matrix      = baseline_mat,
    promotion_multiplier = 1.0,
    promotion_strategy   = "tenure",
    tenure_col           = "tenure_years",
    salary_scale         = sal_scale
  ),
  hiring_policy = list(
    mode             = "flow",
    group_cols       = "est_id",
    replacement_rate = 1.0,
    salary_scale     = sal_scale
  )
)

result$summary
# One row: period_date, wage_bill_start, wage_bill_end,
#          n_retirements, n_non_ret_exits, n_hires,
#          pension_cost_total, inflation_effect, ...

# Net headcount change
result$summary[, n_headcount_end - n_headcount_start]

# What was the COLA cost as % of end bill?
result$summary$cola_effect_pct_of_end_bill