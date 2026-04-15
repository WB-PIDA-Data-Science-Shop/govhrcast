###################################################################################################
################### SANDBOXING simulate_exits() — STEP BY STEP ###################################
###################################################################################################
# The non-retirement exit module handles voluntary resignations, dismissals, and
# contract non-renewals. It is structurally parallel to simulate_retirement().
#
# Two modes:
#   "fixed_rate"  — apply a flat scalar rate to all active workers each period
#   "status_quo"  — apply historically estimated rates (from estimate_historical_exit_rates())
#
# Workflow:
#   1. estimate_historical_exit_rates()   — learn rates from the full panel
#   2. simulate_exits() mode="status_quo" — apply those rates
#   3. simulate_exits() mode="fixed_rate" — apply a user-specified rate instead
#   4. Inspect outputs: summary, exits_dt, updated contract/personnel state
###################################################################################################

devtools::load_all()
library(data.table)

# Working copies of the full panel (all ref_dates)
contract_panel  <- data.table::copy(bra_hrmis_contract)
personnel_panel <- data.table::copy(bra_hrmis_personnel)

# Snapshot ref_date we will use as the "current period" in the simulation
REF_DATE <- as.Date("2016-09-01")

# Single-snapshot working copies for the simulation step
contract_snap  <- data.table::copy(contract_panel[ref_date == REF_DATE])
personnel_snap <- data.table::copy(personnel_panel[ref_date == REF_DATE])

# Quick orientation
cat("Panel date range:", format(range(contract_panel$ref_date)), "\n")
cat("Snapshot headcount:", uniqueN(contract_snap$personnel_id), "workers\n")
cat("Active contracts in snapshot:",
    nrow(contract_snap[contract_type_code == "active"]), "\n")


###################################################################################################
# PART 1: estimate_historical_exit_rates() -------------------------------------------------------
# Learns the non-retirement attrition rate from the full historical panel.
# Returns mean(exit_rate) per group across all observed snapshots.
###################################################################################################

# --- 1a. Overall rate (no grouping) ---
rates_overall <- estimate_historical_exit_rates(
  panel_contract_dt  = contract_panel,
  panel_personnel_dt = personnel_panel,
  group_cols         = NULL
)

rates_overall
# expect: one-row table with a single exit_rate column


# --- 1b. Grouped by establishment (est_id) ---
rates_by_est <- estimate_historical_exit_rates(
  panel_contract_dt  = contract_panel,
  panel_personnel_dt = personnel_panel,
  group_cols         = "est_id"
)

rates_by_est[order(-exit_rate)]
nrow(rates_by_est)   # one row per est_id


###################################################################################################
# PART 2: simulate_exits() — mode = "status_quo" ------------------------------------------------
# Apply the historically estimated rates to the current snapshot.
# exit_rates_dt must be pre-computed and passed in via policy_params.
###################################################################################################

exit_policy_sq <- list(
  mode          = "status_quo",
  exit_rates_dt = rates_overall,   # overall rate, no grouping
  exit_strategy = "random",
  exit_multiplier = 1.0,           # no scaling
  active_types  = "active",
  exited_type   = "inactive"
)

res_sq <- simulate_exits(
  contract_dt   = contract_snap,
  personnel_dt  = personnel_snap,
  policy_params = exit_policy_sq,
  ref_date      = REF_DATE
)

# Summary: how many exited and how much salary was freed?
res_sq$summary

# Who exited?
res_sq$exits_dt[, .(personnel_id, gross_salary_lcu)]

# Check contract state update: exited workers should now be "inactive"
res_sq$contract_dt[personnel_id %in% res_sq$exits_dt$personnel_id,
                   .(personnel_id, contract_type_code, end_date)]

# Check personnel state update
res_sq$personnel_dt[personnel_id %in% res_sq$exits_dt$personnel_id,
                    .(personnel_id, status)]


###################################################################################################
# PART 3: simulate_exits() — mode = "status_quo" with establishment grouping --------------------
# Rates vary by est_id, so high-turnover establishments lose more workers.
###################################################################################################

exit_policy_sq_grp <- list(
  mode            = "status_quo",
  exit_rates_dt   = rates_by_est,   # group-specific rates
  group_cols      = "est_id",
  exit_strategy   = "random",
  exit_multiplier = 1.0,
  active_types    = "active",
  exited_type     = "inactive"
)

res_sq_grp <- simulate_exits(
  contract_dt   = data.table::copy(contract_snap),
  personnel_dt  = data.table::copy(personnel_snap),
  policy_params = exit_policy_sq_grp,
  ref_date      = REF_DATE
)

res_sq_grp$summary

# Compare exits per establishment vs. the estimated rate
exits_by_est <- res_sq_grp$exits_dt[
  , .(n_exits = .N),
  by = est_id
]
merge(exits_by_est, rates_by_est, by = "est_id")[order(-exit_rate)]


###################################################################################################
# PART 4: simulate_exits() — mode = "status_quo" with exit multiplier --------------------------
# Multiplier scales the historical rate: 2.0 = double the historical attrition.
###################################################################################################

exit_policy_2x <- list(
  mode            = "status_quo",
  exit_rates_dt   = rates_overall,
  exit_strategy   = "random",
  exit_multiplier = 2.0,           # double attrition
  active_types    = "active",
  exited_type     = "inactive"
)

res_2x <- simulate_exits(
  contract_dt   = data.table::copy(contract_snap),
  personnel_dt  = data.table::copy(personnel_snap),
  policy_params = exit_policy_2x,
  ref_date      = REF_DATE
)

# Should show roughly 2x exits vs. status quo (subject to rounding)
cat("Status quo exits:", res_sq$summary$n_exits, "\n")
cat("2x multiplier exits:", res_2x$summary$n_exits, "\n")


###################################################################################################
# PART 5: simulate_exits() — mode = "fixed_rate" -----------------------------------------------
# Override historical rates entirely with a user-specified scalar.
# Useful for policy scenarios: "what if attrition drops to 2%?"
###################################################################################################

# --- 5a. Low attrition (2%) ---
exit_policy_low <- list(
  mode          = "fixed_rate",
  fixed_rate    = 0.02,
  exit_strategy = "random",
  active_types  = "active",
  exited_type   = "inactive"
)

res_low <- simulate_exits(
  contract_dt   = data.table::copy(contract_snap),
  personnel_dt  = data.table::copy(personnel_snap),
  policy_params = exit_policy_low,
  ref_date      = REF_DATE
)

res_low$summary


# --- 5b. High attrition (15%) ---
exit_policy_high <- list(
  mode          = "fixed_rate",
  fixed_rate    = 0.15,
  exit_strategy = "random",
  active_types  = "active",
  exited_type   = "inactive"
)

res_high <- simulate_exits(
  contract_dt   = data.table::copy(contract_snap),
  personnel_dt  = data.table::copy(personnel_snap),
  policy_params = exit_policy_high,
  ref_date      = REF_DATE
)

res_high$summary


###################################################################################################
# PART 6: exit_strategy — lowest salary exits first -------------------------------------------
# Instead of random selection, the lowest-paid workers exit first.
# Pass the salary column name as exit_strategy.
###################################################################################################

exit_policy_lowest_sal <- list(
  mode          = "fixed_rate",
  fixed_rate    = 0.05,
  exit_strategy = "gross_salary_lcu",   # ascending: lowest salary exits first
  active_types  = "active",
  exited_type   = "inactive"
)

res_lowest <- simulate_exits(
  contract_dt   = data.table::copy(contract_snap),
  personnel_dt  = data.table::copy(personnel_snap),
  policy_params = exit_policy_lowest_sal,
  ref_date      = REF_DATE
)

# Verify: exited workers should have lower salaries than survivors
cat("Mean salary — exited:", mean(res_lowest$exits_dt$gross_salary_lcu, na.rm = TRUE), "\n")
cat("Mean salary — remaining active:",
    mean(res_lowest$contract_dt[contract_type_code == "active", gross_salary_lcu],
         na.rm = TRUE), "\n")


###################################################################################################
# PART 7: Cross-scenario comparison -----------------------------------------------------------
###################################################################################################

scenario_summary <- rbindlist(list(
  cbind(scenario = "status_quo",    res_sq$summary),
  cbind(scenario = "status_quo_2x", res_2x$summary),
  cbind(scenario = "fixed_2pct",    res_low$summary),
  cbind(scenario = "fixed_15pct",   res_high$summary),
  cbind(scenario = "lowest_sal_5pct", res_lowest$summary)
))

scenario_summary[order(-n_exits)]
