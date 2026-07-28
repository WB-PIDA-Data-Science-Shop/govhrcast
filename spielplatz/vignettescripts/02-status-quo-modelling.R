#########################################################################
######## A SCRIPT TO RUN STATUS QUO SIMULATIONS FOR THE VIGNETTE ########
#########################################################################

#### lets project the wage bill for the next 5 years, assuming that 
#### retirement at 60 is mandatory, keep the rates of hiring and
#### attrition constant, and promotion and transfer rates remain the
#### same as in the historical data and also keeping the salary
#### growth rates the same as in the historical data.

### how have salaries changed over time? lets look at the average salary 
### by year and by contract_id i.e. for the same contract, how has
### the salary changed over time then we can aggregate by paygrade and year
### and overall

library(dplyr)

### lets deflate compensation to 2018 values for each year
contract_dt <- 
bra_hrmis_contract |>
  mutate(gross_salary_cpi = govhr::deflate_to_real(gross_salary_lcu, ref_date, base_year = 2018, country_code = "BRA"),
         allowance_cpi = govhr::deflate_to_real(allowance_lcu, ref_date, base_year = 2018, country_code = "BRA")) 

### lets figure out how much salary has changed on average between years for the entire
### public sector
salary_dt <- 
contract_dt[, .(avg_salary = sum(gross_salary_cpi, na.rm = TRUE)), 
  by = .(contract_id, ref_date)]

salary_dt[, year := year(ref_date)]

salary_dt <- salary_dt[, .(median_salary = median(avg_salary, na.rm = TRUE)), by = .(year)][order(year)]

salary_dt[, salary_change := median_salary / shift(median_salary) - 1]

personnel_dt <- bra_hrmis_personnel

### lets compute exit rates by year from the public sector
exit_dt <- 
estimate_historical_exit_rates(panel_contract_dt = contract_dt,
                               panel_personnel_dt = personnel_dt,
                               group_cols = "est_id")

movement_dt <- 
  estimate_movement_baseline(contract_dt = contract_dt,
                             group_cols = c("est_id", "paygrade"))


### lets compute the pseudo salary scale
salary_scale_dt <- 
  contract_dt[ref_date == "2017-09-01", .(gross_salary_cpi = mean(gross_salary_cpi, na.rm = TRUE)), 
              by = .(est_id, paygrade)]

statusquo_sim <- 
simulate_horizon(contract_dt = contract_dt,
                 personnel_dt = personnel_dt,
                 salary_scale_dt =  salary_scale_dt,
                 retirement_policy = list(group_cols   = NULL,
                                          policy_table = NULL,
                                          defaults     = list(
                                            eligibility_type = "age_only",
                                            min_age          = 60,
                                            pension_type     = "rate",
                                            pension_rate     = 0.15,          # GoB DC scheme: 15% of final salary (Albertus, 2026-06-08)
                                            ref_wage_col     = "gross_salary_cpi",
                                            active_types     = "active")),
                 exit_policy = list(group_cols = "est_id",
                                    policy_table = exit_dt,
                                    defaults = list(exit_rate = mean(exit_dt$exit_rate, na.rm = TRUE),
                                                    active_types = "active")),
                 movement_policy = list(group_cols = c("est_id", "paygrade"),
                                        policy_table = movement_dt,
                                        defaults = list(movement_rate = 0,
                                                        movement_strategy = "tenure",
                                                        active_types = "permanent",
                                                        salary_update_rule = "scale")),
                 hiring_policy = list(mode         = "status_quo",
                                      group_cols   = c("est_id", "paygrade"),
                                      salary_scale = salary_scale_dt,
                                      rate_mult    = 1),
                 salary_growth_rate = salary_dt$salary_change[salary_dt$year == 2017],
                 ref_date = as.Date("2017-09-01"),
                 age_col = "age",
                 tenure_col = "personnel_tenure",
                 scenario_name = "status-quo-sim",
                 hire_date_col = "first_employment_date",
                 n_periods = 5)
