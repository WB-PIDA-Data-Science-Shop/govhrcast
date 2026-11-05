#############################################################################
############## QUICK SCRIPT TO SHOW GOVHRCAST TO BRAZIL TEAM ################
#############################################################################


### place some of the lazyloads in the global environment

### lets do some status quo runs and show some of the flexibility and power
### of the govhrcast system

baseline_date <- as.Date("2017-09-01")

### lets load the harmonized modules
contract_dt  <- bra_hrmis_contract[ref_date <= baseline_date]
personnel_dt <- bra_hrmis_personnel[ref_date <= baseline_date]
allowance_dt <- bra_hrmis_allowance[ref_date <= baseline_date]

### pay table: the salary column must carry the same name as salary_col
### ("base_salary_lcu" below), otherwise new hires get an NA salary
salary_scale_dt <- contract_dt[ref_date == baseline_date & !is.na(base_salary_lcu),
                               .(base_salary_lcu = mean(base_salary_lcu)),
                               by = .(est_id, paygrade)]

### lets design some policies
ret_policy <- design_retirement_policy(eligibility = eligibility_status_quo(mandatory_age = 65),
                                       pension = pension_db(accrual_rate = 0.02, ref_wage_col = "gross_salary_lcu"))

exit_policy <- design_exit_policy_status_quo(group_cols = c("est_id", "paygrade"))


# movement_policy <- design_movement_policy_status_quo(group_cols = c("est_id", "paygrade"), movement_type = "transfer")

hiring_policy <- design_hiring_policy_status_quo(group_cols = c("est_id", "paygrade"), 
                                                 salary_scale = salary_scale_dt)

allowance_policy <- design_allowance_policy_status_quo(panel_allowance_dt = allowance_dt,
                                                       panel_contract_dt = contract_dt,
                                                       group_cols = c("est_id", "paygrade"),
                                                       ref_date = baseline_date)


sq_model <- simulate_horizon(contract_dt  = contract_dt,
                             personnel_dt = personnel_dt,
                             salary_scale_dt = salary_scale_dt,
                             n_periods = 5,
                             ref_date = baseline_date,   # must equal a snapshot date in the data
                             retirement_policy = ret_policy,
                             exit_policy = exit_policy,
                             hiring_policy = hiring_policy,
                             allowance_policy = allowance_policy,
                             salary_growth_rate = 0.03,
                             column_spec_args = list(age_col = "age", 
                                                     tenure_col = "personnel_tenure",
                                                     salary_col = "base_salary_lcu"),
                             output_control_args = list(return_microdata = TRUE, 
                                                        is_baseline = TRUE, 
                                                        scenario_name = "status-quo"),
                             seed = 123)