library(data.table)
library(govhrcast)

ct <- bra_hrmis_contract[ref_date == as.Date("2016-09-01")]
pt <- bra_hrmis_personnel[ref_date == as.Date("2016-09-01")]

# --- compute_current_stock: correct signature ---
compute_current_stock(
  contract_dt      = ct,
  group_cols       = NULL,
  personnel_id_col = "personnel_id",
  contract_type_col = "contract_type_code",
  ref_date         = as.Date("2016-09-01")
)
# n_active: total unique active personnel

compute_current_stock(
  contract_dt       = ct,
  group_cols        = "est_id",
  personnel_id_col  = "personnel_id",
  contract_type_col = "contract_type_code",
  ref_date          = as.Date("2016-09-01")
)
# one row per establishment

# --- simulate_hiring: the public API (wraps internal demand functions) ---
sal_scale <- ct[
  !is.na(gross_salary_lcu) & gross_salary_lcu > 0,
  .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
  keyby = "est_id"
]

ret_res <- simulate_retirement(
  contract_dt   = ct,
  personnel_dt  = pt,
  ref_date      = as.Date("2016-09-01"),
  policy_params = list(
    eligibility_type = "age_only",
    min_age          = 60,
    pension_type     = "flat",
    pension_params   = list(flat_amount = 1000)
  )
)

hire_res <- simulate_hiring(
  contract_dt   = ret_res$contract_dt,
  personnel_dt  = ret_res$personnel_dt,
  retirees_dt   = ret_res$retirees_dt,
  exits_dt      = data.table(personnel_id = character(0)),
  ref_date      = as.Date("2016-09-01"),
  policy_params = list(
    mode             = "flow",
    group_cols       = "est_id",
    replacement_rate = 1.0,
    salary_scale     = sal_scale,
    salary_col       = "gross_salary_lcu"
  )
)

hire_res$summary
hire_res$contract_dt[grepl("^P_", personnel_id), .(personnel_id, est_id, gross_salary_lcu)]