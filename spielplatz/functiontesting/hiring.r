#############################################################################################################
######################### TESTING OUT THE HIRING FUNCTIONS TO SEE HOW THEY WORK #############################
#############################################################################################################

library(govhrcast)
library(data.table)

# Load the Brazil HRMIS data (lazy loaded from the package)
contract_dt <- copy(bra_hrmis_contract)
personnel_dt <- copy(bra_hrmis_personnel)

# Inspect the data
cat("Contract data dimensions:", nrow(contract_dt), "rows x", ncol(contract_dt), "cols\n")
cat("Personnel data dimensions:", nrow(personnel_dt), "rows x", ncol(personnel_dt), "cols\n\n")

# Since this is panel data, let's work with a single snapshot
# Select the most recent ref_date
latest_date <- "2016-09-01"
cat("Using snapshot from:", as.character(latest_date), "\n\n")

contract_dt <- contract_dt[ref_date == latest_date]
personnel_dt <- personnel_dt[ref_date == latest_date]

cat("After filtering to latest date:\n")
cat("  Contracts:", nrow(contract_dt), "\n")
cat("  Personnel:", nrow(personnel_dt), "\n\n")

# =============================================================================
# EXAMPLE 1: Flow-based hiring (replacement rate)
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("EXAMPLE 1: Flow-based hiring with 100% replacement rate\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# First, let's identify retirees to know how many exits we're replacing
policy_params_retirement <- list(
  eligibility_type = "age_and_tenure",
  min_age = 60,
  min_tenure = 20,
  pension_type = "db",
  pension_params = list(
    accrual_rate = 0.02,
    ref_wage_col = "gross_salary_lcu",
    max_years = 35,
    replacement_cap = 0.8
  )
)

retirees_all <- identify_retirees(
  contract_dt = contract_dt,
  personnel_dt = personnel_dt,
  policy_params = policy_params_retirement,
  ref_date = "2016-09-01"  # Can pass as string or Date object
)

# Filter to only actual retirees (retiree == 1)
retirees_dt <- retirees_all[retiree == 1]

cat("Number of retirees identified:", nrow(retirees_dt), "\n\n")

# Now set up hiring policy with 100% replacement
salary_scale <- data.table(
  gross_salary_lcu = 5000  # Default salary for new hires
)

policy_params_flow <- list(
  mode = "flow",
  replacement_rate = 1.0,  # Replace 100% of exits
  group_cols = NULL,       # Overall hiring (not by group)
  salary_scale = salary_scale
)

results_flow <- simulate_hiring(
  contract_dt = contract_dt,
  personnel_dt = personnel_dt,
  policy_params = policy_params_flow,
  retirees_dt = retirees_dt[retire == 1,],  # Pass identified retirees
  ref_date = as.Date(latest_date)
)

cat("FLOW MODE RESULTS:\n")
print(results_flow$summary)
cat("\n")
cat("New hires preview:\n")
print(head(results_flow$new_hires_dt, 3))
cat("\n\n")

# =============================================================================
# EXAMPLE 2: Stock-based hiring (target headcount levels)
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("EXAMPLE 2: Stock-based hiring to reach target headcount\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Reset data
contract_dt <- copy(bra_hrmis_contract[ref_date == latest_date])
personnel_dt <- copy(bra_hrmis_personnel[ref_date == latest_date])

# Compute current headcount
current_stock <- compute_current_stock(
  contract_dt = contract_dt,
  personnel_dt = personnel_dt,
  ref_date = "2016-09-01",
  group_cols = NULL
)

cat("Current active headcount:", current_stock$current_stock, "\n")

# Set target 10% higher than current
target_headcount <- ceiling(current_stock$current_stock * 1.10)
cat("Target headcount (+10%):", target_headcount, "\n\n")

stock_targets <- data.table(
  target_stock = target_headcount
)

policy_params_stock <- list(
  mode = "stock",
  stock_targets = stock_targets,
  group_cols = NULL,
  salary_scale = salary_scale
)

results_stock <- simulate_hiring(
  contract_dt = contract_dt,
  personnel_dt = personnel_dt,
  policy_params = policy_params_stock,
  ref_date = "2016-09-01"
)

cat("STOCK MODE RESULTS:\n")
print(results_stock$summary)
cat("\n\n")

# =============================================================================
# EXAMPLE 3: Combined mode with grouped hiring
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("EXAMPLE 3: Combined mode (flow + stock) by establishment\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Reset data
contract_dt <- copy(bra_hrmis_contract[ref_date == latest_date])
personnel_dt <- copy(bra_hrmis_personnel[ref_date == latest_date])

# Look at current distribution by establishment
current_by_est <- compute_current_stock(
  contract_dt = contract_dt,
  personnel_dt = personnel_dt,
  ref_date = "2016-09-01",
  group_cols = "est_id"
)

cat("Current headcount by establishment (top 5):\n")
print(head(current_by_est[order(-current_stock)], 5))
cat("\n")

# Set replacement rate by establishment (data.table format)
# 80% replacement for all establishments
replacement_rate_dt <- data.table(
  est_id = unique(contract_dt$est_id),
  replacement_rate = 0.8
)

# Set stock targets - maintain current + 5 per establishment
stock_targets_by_est <- current_by_est[, .(
  est_id,
  target_stock = current_stock + 5
)]

# Different salary scales by establishment (simplified example)
# In reality, you'd have more granular scales
salary_scale_by_est <- data.table(
  est_id = unique(contract_dt$est_id),
  gross_salary_lcu = 5000
)

policy_params_combined <- list(
  mode = "combined",
  replacement_rate = replacement_rate_dt,
  stock_targets = stock_targets_by_est,
  group_cols = "est_id",
  salary_scale = salary_scale_by_est
)

results_combined <- simulate_hiring(
  contract_dt = contract_dt,
  personnel_dt = personnel_dt,
  policy_params = policy_params_combined,
  retirees_dt = retirees_dt,
  ref_date = "2016-09-01"
)

cat("COMBINED MODE RESULTS:\n")
print(results_combined$summary)
cat("\n")
cat("Adjustment by establishment (top 10):\n")
print(head(results_combined$adjustment_dt[order(-net_change)], 10))
cat("\n\n")

# =============================================================================
# EXAMPLE 4: Downsizing scenario (negative net change)
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("EXAMPLE 4: Downsizing scenario with 'last hired first' strategy\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Reset data
contract_dt <- copy(bra_hrmis_contract[ref_date == latest_date])
personnel_dt <- copy(bra_hrmis_personnel[ref_date == latest_date])

# Current headcount
current <- compute_current_stock(
  contract_dt = contract_dt,
  personnel_dt = personnel_dt,
  ref_date = "2016-09-01",
  group_cols = NULL
)$current_stock

# Set target 5% lower (downsizing)
target_lower <- ceiling(current * 0.95)

cat("Current headcount:", current, "\n")
cat("Target headcount (-5%):", target_lower, "\n")
cat("Expected downsizing:", current - target_lower, "personnel\n\n")

stock_targets_down <- data.table(
  target_stock = target_lower
)

policy_params_downsize <- list(
  mode = "stock",
  stock_targets = stock_targets_down,
  group_cols = NULL,
  removal_strategy = "last_hired_first",  # Remove most recent hires
  salary_scale = salary_scale
)

results_downsize <- simulate_hiring(
  contract_dt = contract_dt,
  personnel_dt = personnel_dt,
  policy_params = policy_params_downsize,
  ref_date = "2016-09-01"
)

cat("DOWNSIZING RESULTS:\n")
print(results_downsize$summary)
cat("\n")

# Check terminated contracts
n_terminated <- results_downsize$contract_dt[
  contract_type_code == "terminated", .N
]
cat("Number of contracts terminated:", n_terminated, "\n\n")

# =============================================================================
# EXAMPLE 5: Integration with retirement module
# =============================================================================
cat("=" , rep("=", 70), "\n", sep = "")
cat("EXAMPLE 5: Integrated retirement + hiring workflow\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Reset data
contract_dt <- copy(bra_hrmis_contract[ref_date == latest_date])
personnel_dt <- copy(bra_hrmis_personnel[ref_date == latest_date])

cat("STEP 1: Process retirements\n")
cat("----------------------------\n")

retirement_results <- simulate_retirement(
  contract_dt = contract_dt,
  personnel_dt = personnel_dt,
  policy_params = policy_params_retirement,
  ref_date = "2016-09-01"
)

cat("Retirements processed:", retirement_results$summary$n_retired, "\n")
cat("Pension cost:", retirement_results$summary$total_pension, "\n\n")

cat("STEP 2: Hire replacements (80% replacement rate)\n")
cat("---------------------------------------------------\n")

policy_params_replace <- list(
  mode = "flow",
  replacement_rate = 0.8,
  group_cols = NULL,
  salary_scale = salary_scale
)

# Use updated contract_dt and personnel_dt from retirement simulation
# Filter retirees_dt to only those with retiree == 1
retirees_only <- retirement_results$retirees_dt[retiree == 1]

hiring_results <- simulate_hiring(
  contract_dt = retirement_results$contract_dt,
  personnel_dt = retirement_results$personnel_dt,
  policy_params = policy_params_replace,
  retirees_dt = retirees_only,  # Only actual retirees
  ref_date = "2016-09-01"
)

cat("New hires:", hiring_results$summary$n_new_hires, "\n")
cat("Net headcount change:", hiring_results$summary$net_headcount_change, "\n")
cat("Final headcount:", hiring_results$summary$total_headcount, "\n\n")

cat("INTEGRATED SUMMARY:\n")
cat("  Exits (retired):", retirement_results$summary$n_retirements, "\n")
cat("  Entries (hired):", hiring_results$summary$n_new_hires, "\n")
cat("  Net change:", hiring_results$summary$net_headcount_change, "\n")
cat("  Final headcount:", hiring_results$summary$total_headcount, "\n\n")

cat("=" , rep("=", 70), "\n", sep = "")
cat("All examples completed successfully!\n")
cat("=" , rep("=", 70), "\n", sep = "")

