library(data.table)
library(govhrcast)

ct <- bra_hrmis_contract[ref_date == as.Date("2016-09-01")]
pt <- bra_hrmis_personnel[ref_date == as.Date("2016-09-01")]

# --- Mode 1: fixed_rate ---
res_fixed <- simulate_exits(
  contract_dt   = ct,
  personnel_dt  = pt,
  ref_date      = as.Date("2016-09-01"),
  policy_params = list(
    mode         = "fixed_rate",
    fixed_rate   = 0.05,        # 5% of active workforce exits
    active_types = unique(ct$contract_type_code)
  )
)
res_fixed$summary   # n_exits, exit_savings

# --- Mode 2: status_quo, ungrouped ---
rates <- estimate_historical_exit_rates(
  panel_contract_dt  = bra_hrmis_contract,
  panel_personnel_dt = bra_hrmis_personnel,
  group_cols         = NULL
)
# rates: data.table(exit_rate = 0.047...)

res_sq <- simulate_exits(
  contract_dt   = ct,
  personnel_dt  = pt,
  ref_date      = as.Date("2016-09-01"),
  policy_params = list(
    mode          = "status_quo",
    exit_rates_dt = rates,
    active_types  = unique(ct$contract_type_code)
  )
)
res_sq$summary

# --- Mode 3: status_quo, grouped by est_id, lowest salary exits first ---
rates_grp <- estimate_historical_exit_rates(
  panel_contract_dt  = bra_hrmis_contract,
  panel_personnel_dt = bra_hrmis_personnel,
  group_cols         = "est_id"
)

res_grp <- simulate_exits(
  contract_dt   = ct,
  personnel_dt  = pt,
  ref_date      = as.Date("2016-09-01"),
  policy_params = list(
    mode          = "status_quo",
    exit_rates_dt = rates_grp,
    group_cols    = "est_id",
    exit_strategy = "gross_salary_lcu",  # lowest paid exits first
    active_types  = unique(ct$contract_type_code)
  )
)
res_grp$exits_dt[, .(personnel_id, gross_salary_lcu)]
# should be the lowest-salary people from each est