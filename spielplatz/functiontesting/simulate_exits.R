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

contract_dt <- copy(bra_hrmis_contract)
personnel_dt <- copy(bra_hrmis_personnel)

exit_rates_dt <- 
estimate_historical_exit_rates(panel_contract_dt = contract_dt,
                               panel_personnel_dt = personnel_dt,
                               group_cols = c("est_id", "paygrade", "seniority"))

exit_policy <- list(group_cols   = NULL,
                    policy_table = NULL,
                    defaults = list(exit_rate            = 0.1,
                                    exit_strategy   = "random",
                                    active_types    = c("perm", "fterm", "temp"),
                                    exited_type     = "inactive"))


simulate_exits(contract_dt = bra_hrmis_contract,
               personnel_dt = bra_hrmis_personnel,
               policy_params = exit_policy,
               ref_date = "2015-09-01")