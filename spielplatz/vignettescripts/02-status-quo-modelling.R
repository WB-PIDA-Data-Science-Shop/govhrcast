#########################################################################
######## A SCRIPT TO RUN SEVERAL SIMULATIONS FOR THE VIGNETTE ###########
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
                                            eligibility_type = "custom",
                                            eligibility_rule = "age >= 60 & contract_type == 'permanent' & 
                                              employment_status == 'active' & personnel_tenure >= 5",
                                            pension_type     = "custom",
                                            pension_formula  = "0.15 * gross_salary_cpi * tenure_years",          # GoB DC scheme: 15% of final salary (Albertus, 2026-06-08)
                                            ref_wage_col     = "gross_salary_cpi",
                                            active_types     = "active")),
                 exit_policy = list(group_cols = "est_id",
                                    policy_table = exit_dt,
                                    defaults = list(exit_rate = mean(exit_dt$exit_rate, na.rm = TRUE),
                                                    active_types = c("permanent", "short-term", "fixed-term"))),
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
                 n_periods = 5,
                 salary_col = "gross_salary_cpi",
                 return_microdata = TRUE)


### ok lets run other simulations now
### -------------- SIMULATING RETIREMENT POLICIES ----------------- ###

##### lets try group level policies

### lets simulate a different compensation structure for different paygrades

rate_dt <- data.table(paygrade = c("A", "B", "C", "D", "E", "1", "2", "3", "4"),
                      pension_rate = c(0.10, 0.12, 0.15, 0.18, 0.20, 0.10, 0.12, 0.15, 0.18),
                      min_age = c(60, 60, 61, 65, 60, 60, 60, 60, 62))

retirement_group_pensionrate_sim <- 
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
                                                    active_types = c("permanent", "short-term", "fixed-term"))),
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
                 scenario_name = "retirement-group-paygrade-varybypensionrate-minage-sim",
                 hire_date_col = "first_employment_date",
                 n_periods = 5,
                 salary_col = "gross_salary_cpi")


### lets simulate different pension types for different paygrades with different age limits

mixed_pension_dt <- data.table(
  paygrade     = c("A",    "B",    "C",    "D",    "E",    "1",    "2",    "3",    "4"),
  min_age      = c(60,     60,     61,     65,     60,     60,     60,     60,     62),
  pension_type = c("rate", "rate", "db",   "db",   "flat", "rate", "db",   "rate", "flat"),
  pension_rate = c(0.10,   0.12,   NA,     NA,     NA,     0.10,   NA,     0.15,   NA),
  accrual_rate    = c(NA,  NA,     0.02,   0.025,  NA,     NA,     0.022,  NA,     NA),
  max_years       = c(NA,  NA,     35,     35,     NA,     NA,     35,     NA,     NA),
  replacement_cap = c(NA,  NA,     0.80,   0.85,   NA,     NA,     0.80,   NA,     NA),
  flat_amount  = c(NA,     NA,     NA,     NA,     5000,   NA,     NA,     NA,     3500)
)

retirement_mixed_sim <-
  simulate_horizon(
    contract_dt        = contract_dt,
    personnel_dt       = personnel_dt,
    salary_scale_dt    = salary_scale_dt,
    retirement_policy  = list(
      group_cols   = "paygrade",
      policy_table = mixed_pension_dt,
      defaults     = list(
        eligibility_type = "age_only",
        min_age          = 60,
        active_types     = c("permanent", "short-term", "fixed-term"),
        pension_type     = "rate",
        pension_rate     = 0.15,
        ref_wage_col     = "gross_salary_cpi",
        accrual_rate     = NA_real_,
        max_years        = NA_real_,
        replacement_cap  = NA_real_,
        flat_amount      = NA_real_
      )
    ),
    exit_policy = list(
      group_cols   = "est_id",
      policy_table = exit_dt,
      defaults     = list(
        exit_rate    = mean(exit_dt$exit_rate, na.rm = TRUE),
        active_types = c("permanent", "short-term", "fixed-term")
      )
    ),
    movement_policy = list(
      group_cols   = c("est_id", "paygrade"),
      policy_table = movement_dt,
      defaults     = list(
        movement_rate      = 0,
        movement_strategy  = "tenure",
        active_types       = "permanent",
        salary_update_rule = "scale"
      )
    ),
    hiring_policy = list(
      mode         = "status_quo",
      group_cols   = c("est_id", "paygrade"),
      salary_scale = salary_scale_dt,
      rate_mult    = 1
    ),
    salary_growth_rate = salary_dt$salary_change[salary_dt$year == 2017],
    ref_date           = as.Date("2017-09-01"),
    age_col            = "age",
    tenure_col         = "personnel_tenure",
    scenario_name      = "retirement-mixed-pension-type-by-paygrade",
    hire_date_col      = "first_employment_date",
    n_periods          = 5,
    salary_col         = "gross_salary_cpi"
  )


### ----------------- SIMULATING EXIT POLICIES -------------------- ###

#### here is how we simulate exits by groups
exit_dt <- 
estimate_historical_exit_rates(panel_contract_dt = contract_dt,
                               panel_personnel_dt = personnel_dt,
                               group_cols = c("est_id", "paygrade"))


exit_group_paygrade_estid_sim <- 
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
                 exit_policy = list(group_cols = c("est_id", "paygrade"),
                                    policy_table = exit_dt,
                                    defaults = list(exit_rate = mean(exit_dt$exit_rate, na.rm = TRUE),
                                                    active_types = c("permanent", "short-term", "fixed-term"))),
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
                 scenario_name = "exit-group-paygrade-est-id-sim",
                 hire_date_col = "first_employment_date",
                 n_periods = 5,
                 salary_col = "gross_salary_cpi")


### ok, the previous simulation used the default exit strategy which is "random", go ahead and try the others: 

exit_statusquo_sim <-
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
                 exit_policy = list(group_cols = c("est_id", "paygrade"),
                                    policy_table = NULL,
                                    defaults = list(exit_rate = mean(exit_dt$exit_rate, na.rm = TRUE),
                                                    active_types = c("permanent", "short-term", "fixed-term"),
                                                    exit_strategy = "random")),
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
                 scenario_name = "exit-group-paygrade-est-id-sim",
                 hire_date_col = "first_employment_date",
                 n_periods = 5,
                 salary_col = "gross_salary_cpi")

### lets implement fixed rate exits at the group level
exit_fixed_rate_sim <-
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
                 exit_policy = list(group_cols = NULL,
                                    policy_table = NULL,
                                    defaults = list(exit_rate = 0.01,
                                                    active_types = c("permanent", "short-term", "fixed-term"),
                                                    exit_strategy = "random")),
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
                 scenario_name = "exit-group-paygrade-est-id-sim",
                 hire_date_col = "first_employment_date",
                 n_periods = 5,
                 salary_col = "gross_salary_cpi")

### we can also implement exit rate multiplier as well by computing a policy table and doubling rates
### of specific groups


### ------------------ SIMULATING HIRING POLICIES --------------------- ###

### simulate status quo policies for hiring, i.e. keep the same hiring rates as in the historical data
hiring_statusquo_sim <-
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
                                                    active_types = c("permanent", "short-term", "fixed-term"))),
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
                 n_periods = 5,
                 salary_col = "gross_salary_cpi")

### simulate hiring flow policies for hiring i.e. applying hiring rates and replacement rates to 
### the number of exits in the previous period to determine the number of hires in the current period
hiring_flow_sim <-
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
                                                    active_types = c("permanent", "short-term", "fixed-term"))),
                 movement_policy = list(group_cols = c("est_id", "paygrade"),
                                        policy_table = movement_dt,
                                        defaults = list(movement_rate = 0,
                                                        movement_strategy = "tenure",
                                                        active_types = "permanent",
                                                        salary_update_rule = "scale")),
                 hiring_policy = list(mode         = "flow",
                                      group_cols   = c("est_id", "paygrade"),
                                      salary_scale = salary_scale_dt,
                                      replacement_rate    = 1),
                 salary_growth_rate = salary_dt$salary_change[salary_dt$year == 2017],
                 ref_date = as.Date("2017-09-01"),
                 age_col = "age",
                 tenure_col = "personnel_tenure",
                 scenario_name = "status-quo-sim",
                 hire_date_col = "first_employment_date",
                 n_periods = 5,
                 salary_col = "gross_salary_cpi")

# Group-varying replacement rate via data.table
replacement_dt <- contract_dt[, c("est_id", "paygrade")] |> unique()
replacement_dt[, replacement_rate := 1.0]  # Default replacement rate

replacement_dt[paygrade %in% c("A", "B"), replacement_rate := 0.7]  # Lower replacement rate for paygrades A and B

hiring_policy = list(
  mode             = "flow",
  group_cols       = c("est_id", "paygrade"),
  replacement_rate = replacement_dt,
  salary_scale     = salary_scale_dt
)

hiring_group_flow_sim <-
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
                                                    active_types = c("permanent", "short-term", "fixed-term"))),
                 movement_policy = list(group_cols = c("est_id", "paygrade"),
                                        policy_table = movement_dt,
                                        defaults = list(movement_rate = 0,
                                                        movement_strategy = "tenure",
                                                        active_types = "permanent",
                                                        salary_update_rule = "scale")),
                 hiring_policy = hiring_policy,
                 salary_growth_rate = salary_dt$salary_change[salary_dt$year == 2017],
                 ref_date = as.Date("2017-09-01"),
                 age_col = "age",
                 tenure_col = "personnel_tenure",
                 scenario_name = "group_flow_sim",
                 hire_date_col = "first_employment_date",
                 n_periods = 5,
                 salary_col = "gross_salary_cpi")


## lets simulate some stock hiring policies, i.e. hiring to reach a target stock of employees in each group
# Build targets from current snapshot as a starting point
stock_targets <- contract_dt[
  ref_date == as.Date("2017-09-01") & !is.na(contract_type),
  .(target_stock = uniqueN(personnel_id)),
  by = c("est_id", "paygrade")
][!is.na(est_id) & !is.na(paygrade)]

# Freeze headcount at 2017 levels — hire exactly enough to offset all exits
hiring_policy = list(
  mode         = "stock",
  group_cols   = c("est_id", "paygrade"),
  stock_targets = stock_targets,
  salary_scale  = salary_scale_dt
)

# Reform scenario — increase headcount by 10% relative to 2017
stock_targets_reform <- data.table::copy(stock_targets)
stock_targets_reform[, target_stock := as.integer(round(target_stock * 1.1))]

hiring_policy = list(
  mode          = "stock",
  group_cols    = c("est_id", "paygrade"),
  stock_targets = stock_targets_reform,
  salary_scale  = salary_scale_dt
)


hiring_group_stock_sim <-
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
                                                    active_types = c("permanent", "short-term", "fixed-term"))),
                 movement_policy = list(group_cols = c("est_id", "paygrade"),
                                        policy_table = movement_dt,
                                        defaults = list(movement_rate = 0,
                                                        movement_strategy = "tenure",
                                                        active_types = "permanent",
                                                        salary_update_rule = "scale")),
                 hiring_policy = hiring_policy,
                 salary_growth_rate = salary_dt$salary_change[salary_dt$year == 2017],
                 ref_date = as.Date("2017-09-01"),
                 age_col = "age",
                 tenure_col = "personnel_tenure",
                 scenario_name = "group_flow_sim",
                 hire_date_col = "first_employment_date",
                 n_periods = 5,
                 salary_col = "gross_salary_cpi")



#### --------------- SIMULATING MOVEMENT POLICIES --------------------- ####

############################################################
## Example 1: Status quo — empirical baseline estimated
##            from panel data
##
## policy_table = NULL with >= 2 snapshots triggers automatic
## estimation of the transition matrix from consecutive pairs.
## This is the natural baseline for status quo projections.
############################################################

movement_policy <- list(
  group_cols   = c("est_id", "paygrade"),
  policy_table = NULL,
  defaults = list(
    movement_rate      = 0,
    movement_strategy  = "tenure",
    active_types       = "permanent",
    salary_update_rule = "scale"
  )
)

movement_statusquo_sim <-
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
                                                    active_types = c("permanent", "short-term", "fixed-term"))),
                 movement_policy = movement_policy,
                 hiring_policy = list(mode         = "status_quo",
                                      group_cols   = c("est_id", "paygrade"),
                                      salary_scale = salary_scale_dt,
                                      rate_mult    = 1),
                 salary_growth_rate = salary_dt$salary_change[salary_dt$year == 2017],
                 ref_date = as.Date("2017-09-01"),
                 age_col = "age",
                 tenure_col = "personnel_tenure",
                 scenario_name = "group_flow_sim",
                 hire_date_col = "first_employment_date",
                 n_periods = 5,
                 salary_col = "gross_salary_cpi")


############################################################
## Example 2: Reform scenario — user-supplied transition
##            matrix doubling promotion rates into senior grades
##
## Estimate the historical baseline first, then scale up
## promotion probabilities for a targeted reform scenario.
############################################################

baseline <- estimate_movement_baseline(
  contract_dt       = contract_dt,
  group_cols        = c("est_id", "paygrade"),
  personnel_id_col  = "personnel_id",
  ref_date_col      = "ref_date",
  start_date_col    = "start_date",
  end_date_col      = "end_date",
  contract_type_col = "contract_type"
)

# Double movement rates into the two most senior paygrades
reform_matrix <- data.table::copy(baseline)
reform_matrix[
  grepl("Grade C|Grade D", to_group),
  movement_rate := pmin(movement_rate * 2, 1.0)
]

movement_policy_reform <- list(
  group_cols   = c("est_id", "paygrade"),
  policy_table = reform_matrix,
  defaults = list(
    movement_rate      = 0,
    movement_strategy  = "tenure",
    active_types       = "permanent",
    salary_update_rule = "scale"
  )
)

movement_policyreform_sim <-
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
                                                    active_types = c("permanent", "short-term", "fixed-term"))),
                 movement_policy = movement_policy,
                 hiring_policy = list(mode         = "status_quo",
                                      group_cols   = c("est_id", "paygrade"),
                                      salary_scale = salary_scale_dt,
                                      rate_mult    = 1),
                 salary_growth_rate = salary_dt$salary_change[salary_dt$year == 2017],
                 ref_date = as.Date("2017-09-01"),
                 age_col = "age",
                 tenure_col = "personnel_tenure",
                 scenario_name = "group_flow_sim",
                 hire_date_col = "first_employment_date",
                 n_periods = 5,
                 salary_col = "gross_salary_cpi",
                 return_microdata = TRUE)