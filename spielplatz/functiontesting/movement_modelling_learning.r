library(data.table)
library(govhrcast)

panel <- bra_hrmis_contract
ct    <- panel[ref_date == as.Date("2016-09-01")]
pt    <- bra_hrmis_personnel[ref_date == as.Date("2016-09-01")]

# Step 1: estimate baseline from full panel
baseline_mat <- estimate_movement_baseline(
  contract_dt      = panel,
  group_cols       = "est_id",
  personnel_id_col = "personnel_id",
  ref_date_col     = "ref_date",
  contract_type_col = "contract_type_code",
  start_date_col   = "start_date",
  end_date_col     = "end_date"
)

# Show top 10 most common transitions
baseline_mat[from_group != to_group][order(-avg_prob)][1:10]

# Step 2: simulate movements for one period
move_res <- simulate_promotions_transfers(
  contract_dt  = ct,
  personnel_dt = pt,
  ref_date     = as.Date("2016-09-01"),
  policy_params = list(
    group_cols           = "est_id",
    baseline_matrix      = baseline_mat,
    promotion_multiplier = 1.0,
    promotion_strategy   = "tenure",
    tenure_col           = "tenure_years",
    salary_scale         = ct[
      !is.na(gross_salary_lcu),
      .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
      keyby = "est_id"
    ]
  )
)

move_res$summary
# n_movers, n_promotions, n_transfers, avg_salary_change

# Who moved and what did it cost?
move_res$movers_dt[, .(personnel_id, from_group, to_group,
                        salary_before, gross_salary_lcu,
                        movement_type)]