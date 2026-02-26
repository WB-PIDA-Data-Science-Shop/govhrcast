###############################################################################
############ TESTING OUT THE MOVEMENTS (PROMOTIONS & TRANSFERS) FUNCTIONS ####
###############################################################################
#
# This script walks through every function in the movements module in order:
#
#   PART 1 – Pure demand estimation (movement_core.R)
#     1a. compute_time_in_grade()
#     1b. estimate_movement_baseline()
#     1c. compute_movement_demand()
#
#   PART 2 – Individual selection & state modification (movement_update.R)
#     2a. stochastic_round()
#     2b. identify_movers()
#     2c. update_state_with_movement()
#
#   PART 3 – Main orchestrator (simulate_promotions_transfers.R)
#     3a. Default run (status-quo multipliers)
#     3b. Accelerated promotions (promotion_multiplier > 1)
#     3c. Suppressed transfers  (transfer_multiplier = 0)
#     3d. Wage-based promotion strategy
#     3e. Chained with simulate_retirement()
#
###############################################################################

library(govhrcast)
library(data.table)

# ── Data ──────────────────────────────────────────────────────────────────────
# bra_hrmis_contract is a panel: one row per person per ref_date
# Columns: personnel_id, contract_id, ref_date, start_date, end_date,
#          contract_type_code, gross_salary_lcu, est_id, ... (paygrade absent;
#          we use est_id as the movement group)
contract_dt  <- copy(bra_hrmis_contract)
personnel_dt <- copy(bra_hrmis_personnel)

cat("Panel snapshots available:\n")
print(sort(unique(contract_dt$ref_date)))

cat("\nContract data:", nrow(contract_dt), "rows x", ncol(contract_dt), "cols\n")
cat("Personnel data:", nrow(personnel_dt), "rows x", ncol(personnel_dt), "cols\n\n")

# Reference date for the simulation
ref_date <- as.Date("2016-09-01")

# Salary scale: one row per est_id (used for salary assignment after movement)
salary_scale_dt <- contract_dt[
  ref_date == as.Date("2016-09-01") & !is.na(gross_salary_lcu),
  .(gross_salary_lcu = median(gross_salary_lcu)),
  by = c("est_id", "paygrade")
]

###############################################################################
# PART 1 – PURE DEMAND ESTIMATION FUNCTIONS
###############################################################################

# =============================================================================
# 1a. compute_time_in_grade()
# -----------------------------------------------------------------------------
# How long has each person been continuously in their current est_id?
# Uses panel snapshots to find the earliest snapshot where the person
# was already in their current state.
# =============================================================================
cat(strrep("=", 70), "\n")
cat("1a. compute_time_in_grade()\n")
cat(strrep("=", 70), "\n\n")

tig <- compute_time_in_grade(
  contract_dt      = contract_dt,
  ref_date         = ref_date,
  group_cols       = "est_id",
  personnel_id_col = "personnel_id",
  ref_date_col     = "ref_date",
  start_date_col   = "start_date",
  end_date_col     = "end_date",
  contract_type_col = "contract_type_code"
)

cat("Rows returned (one per active person):", nrow(tig), "\n")
cat("Distribution of time-in-grade (years):\n")
print(summary(tig$time_in_grade))

cat("\nTop 10 longest-serving personnel (by time in current est_id):\n")
print(tig[order(-time_in_grade)][1:10])

cat("\n")

# =============================================================================
# 1b. estimate_movement_baseline()
# -----------------------------------------------------------------------------
# Computes empirical transition probabilities from the full panel.
# For each consecutive pair of snapshots (T0→T1, T1→T2, …) it counts
# how many people moved from state i to state j, divides by the T0 population
# in state i, and averages across periods.
# =============================================================================
cat(strrep("=", 70), "\n")
cat("1b. estimate_movement_baseline()\n")
cat(strrep("=", 70), "\n\n")

baseline_matrix <- estimate_movement_baseline(
  contract_dt      = contract_dt,
  group_cols       = c("est_id", "paygrade"),
  personnel_id_col = "personnel_id",
  ref_date_col     = "ref_date",
  start_date_col   = "start_date",
  end_date_col     = "end_date",
  contract_type_col = "contract_type_code"
)

cat("Baseline matrix dimensions:", nrow(baseline_matrix), "rows x",
    ncol(baseline_matrix), "cols\n")
cat("Columns:", paste(names(baseline_matrix), collapse = ", "), "\n\n")

# Off-diagonal rows = actual movements (from_group != to_group)
movers_baseline <- baseline_matrix[from_group != to_group]
cat("Number of observed transition types (movements):", nrow(movers_baseline), "\n\n")

cat("Top 10 highest-probability movement transitions:\n")
print(movers_baseline[order(-avg_prob)][1:min(10, .N)])

cat("\nHistorical summary per origin group (avg mobility rate):\n")
mobility_by_group <- movers_baseline[,
  .(total_move_prob = sum(avg_prob), n_destinations = .N),
  by = from_group
][order(-total_move_prob)]
print(mobility_by_group[1:min(10, .N)])

cat("\n")

# =============================================================================
# 1c. compute_movement_demand()
# -----------------------------------------------------------------------------
# Applies policy multipliers to baseline probabilities and converts to integer
# mover counts using the current workforce as the denominator.
# Stochastic rounding handles fractional counts.
# =============================================================================
cat(strrep("=", 70), "\n")
cat("1c. compute_movement_demand()\n")
cat(strrep("=", 70), "\n\n")

# Snapshot at ref_date
snap_contract  <- contract_dt[ref_date == ref_date]
snap_personnel <- personnel_dt[ref_date == ref_date]

policy_params_sq <- list(
  group_cols           = "est_id",
  salary_scale         = salary_scale_dt,
  promotion_multiplier = 1.0,   # Status quo
  transfer_multiplier  = 1.0,
  promotion_strategy   = "tenure",
  transfer_strategy    = "random"
)

set.seed(42)
demand_dt <- compute_movement_demand(
  contract_dt       = snap_contract,
  personnel_dt      = snap_personnel,
  baseline_matrix   = baseline_matrix,
  policy_params     = policy_params_sq,
  salary_scale_dt   = salary_scale_dt,
  ref_date          = ref_date,
  personnel_id_col  = "personnel_id",
  start_date_col    = "start_date",
  end_date_col      = "end_date",
  contract_type_col = "contract_type_code",
  status_col        = "status"
)

cat("Demand table dimensions:", nrow(demand_dt), "rows\n")
cat("Columns:", paste(names(demand_dt), collapse = ", "), "\n\n")

cat("Movements demanded (from_group → to_group):\n")
print(demand_dt[n_movers > 0][order(-n_movers)][1:min(15, .N)])

cat("\nTotal movers demanded:\n")
demand_summary <- demand_dt[n_movers > 0, .(n_transitions = .N, total_movers = sum(n_movers)),
                             by = movement_type]
print(demand_summary)

cat("\n")

###############################################################################
# PART 2 – INDIVIDUAL SELECTION & STATE MODIFICATION
###############################################################################

# =============================================================================
# 2a. stochastic_round()
# -----------------------------------------------------------------------------
# floor(x) + Bernoulli(x - floor(x))
# Ensures fractional demand counts are converted to integers in an unbiased way.
# =============================================================================
cat(strrep("=", 70), "\n")
cat("2a. stochastic_round()\n")
cat(strrep("=", 70), "\n\n")

cat("Single call examples:\n")
set.seed(1)
for (x in c(0.0, 0.3, 0.5, 0.7, 1.0, 2.4, 2.9, 10.5)) {
  result <- stochastic_round(x)
  cat(sprintf("  stochastic_round(%.1f) = %d\n", x, result))
}

cat("\nLaw of large numbers check (mean of 10000 rounds of 0.3 should ≈ 0.3):\n")
set.seed(99)
means <- vapply(1:10000, function(i) stochastic_round(0.3), integer(1))
cat(sprintf("  Empirical mean = %.4f (expected ≈ 0.3)\n\n", mean(means)))

# =============================================================================
# 2b. identify_movers()
# -----------------------------------------------------------------------------
# For each from_group → to_group pair in demand_dt, selects exactly n_movers
# individuals from the from_group using the specified ranking strategy.
# Guarantees no person is selected twice across all transitions.
# =============================================================================
cat(strrep("=", 70), "\n")
cat("2b. identify_movers()\n")
cat(strrep("=", 70), "\n\n")

# Only use demand rows with n_movers > 0
demand_nonzero <- demand_dt[n_movers > 0]

set.seed(42)
movers_dt <- identify_movers(
  contract_dt   = snap_contract,
  personnel_dt  = snap_personnel,
  demand_dt     = demand_nonzero,
  policy_params = policy_params_sq,
  ref_date      = ref_date
)

cat("Movers identified:", nrow(movers_dt), "\n")
cat("Columns:", paste(names(movers_dt), collapse = ", "), "\n\n")

cat("Breakdown by movement_type:\n")
print(movers_dt[, .N, by = movement_type])

cat("\nSample of identified movers (first 10):\n")
print(movers_dt[1:min(10, .N)])

cat("\nUniqueness check (no person selected twice):",
    data.table::uniqueN(movers_dt$personnel_id) == nrow(movers_dt), "\n\n")

# =============================================================================
# 2c. update_state_with_movement()
# -----------------------------------------------------------------------------
# Applies the mover assignments to contract_dt: updates est_id (group_col)
# and gross_salary_lcu from salary_scale_dt for each mover.
# Non-movers are untouched. Returns updated contract_dt, personnel_dt,
# and the annotated movers_dt.
# =============================================================================
cat(strrep("=", 70), "\n")
cat("2c. update_state_with_movement()\n")
cat(strrep("=", 70), "\n\n")

set.seed(42)
state_result <- govhrcast:::update_state_with_movement(
  contract_dt   = copy(snap_contract),
  personnel_dt  = copy(snap_personnel),
  movers_dt     = movers_dt,
  policy_params = policy_params_sq,
  ref_date      = ref_date
)

cat("Result list elements:", paste(names(state_result), collapse = ", "), "\n\n")

# Compare group distribution before and after for moved personnel
before_groups <- snap_contract[
  personnel_id %in% movers_dt$personnel_id,
  .(personnel_id, est_id_before = est_id, salary_before = gross_salary_lcu)
]
after_groups <- state_result$contract_dt[
  personnel_id %in% movers_dt$personnel_id,
  .(personnel_id, est_id_after = est_id, salary_after = gross_salary_lcu)
]
changes <- before_groups[after_groups, on = "personnel_id"]

cat("Group / salary changes for movers (first 10):\n")
print(changes[1:min(10, .N)])

cat("\nCount actually moved (est_id changed):",
    changes[est_id_before != est_id_after, .N], "of", nrow(movers_dt), "\n\n")

###############################################################################
# PART 3 – MAIN ORCHESTRATOR: simulate_promotions_transfers()
###############################################################################

# =============================================================================
# 3a. Default run: status-quo multipliers (1.0 / 1.0)
# -----------------------------------------------------------------------------
# Uses full panel to estimate baseline, then applies it to ref_date snapshot.
# Returns: summary, contract_dt (snapshot), personnel_dt, movers_dt,
#          baseline_matrix, demand_dt
# =============================================================================
cat(strrep("=", 70), "\n")
cat("3a. simulate_promotions_transfers() – status quo\n")
cat(strrep("=", 70), "\n\n")

set.seed(42)
res_sq <- simulate_promotions_transfers(
  contract_dt   = contract_dt,
  personnel_dt  = personnel_dt,
  policy_params = list(
    group_cols           = "est_id",
    salary_scale         = salary_scale_dt,
    promotion_multiplier = 1.0,
    transfer_multiplier  = 1.0,
    promotion_strategy   = "tenure",
    transfer_strategy    = "random"
  ),
  ref_date = ref_date
)

cat("Return list elements:", paste(names(res_sq), collapse = ", "), "\n\n")
cat("Summary:\n")
print(res_sq$summary)

cat("\nMovers (first 10):\n")
print(res_sq$movers_dt[1:min(10, .N)])

cat("\nBaseline matrix (top transitions by probability):\n")
print(res_sq$baseline_matrix[from_group != to_group][order(-avg_prob)][1:min(10, .N)])

cat("\n")

# =============================================================================
# 3b. Accelerated promotions: promotion_multiplier = 2.0
# =============================================================================
cat(strrep("=", 70), "\n")
cat("3b. simulate_promotions_transfers() – 2x promotion multiplier\n")
cat(strrep("=", 70), "\n\n")

set.seed(42)
res_2x <- simulate_promotions_transfers(
  contract_dt   = contract_dt,
  personnel_dt  = personnel_dt,
  policy_params = list(
    group_cols           = "est_id",
    salary_scale         = salary_scale_dt,
    promotion_multiplier = 2.0,   # Double historical promotion rates
    transfer_multiplier  = 1.0,
    promotion_strategy   = "tenure",
    transfer_strategy    = "random"
  ),
  ref_date = ref_date
)

cat("Summary comparison (status quo vs 2x promotions):\n")
comparison <- data.table::rbindlist(list(
  res_sq$summary[, scenario := "status_quo"],
  res_2x$summary[, scenario := "2x_promo"]
), use.names = TRUE, fill = TRUE)
print(comparison[, .(scenario, n_promotions, n_transfers, n_total_movers,
                     headcount_before, headcount_after)])

cat("\n")

# =============================================================================
# 3c. Suppressed transfers: transfer_multiplier = 0
# =============================================================================
cat(strrep("=", 70), "\n")
cat("3c. simulate_promotions_transfers() – no transfers (multiplier = 0)\n")
cat(strrep("=", 70), "\n\n")

set.seed(42)
res_notrans <- simulate_promotions_transfers(
  contract_dt   = contract_dt,
  personnel_dt  = personnel_dt,
  policy_params = list(
    group_cols           = "est_id",
    salary_scale         = salary_scale_dt,
    promotion_multiplier = 1.0,
    transfer_multiplier  = 0.0,   # No transfers
    promotion_strategy   = "tenure",
    transfer_strategy    = "random"
  ),
  ref_date = ref_date
)

cat("n_transfers (should be 0):", res_notrans$summary$n_transfers, "\n")
cat("n_promotions:              ", res_notrans$summary$n_promotions, "\n\n")

# =============================================================================
# 3d. Wage-based promotion strategy
# -----------------------------------------------------------------------------
# Instead of longest-serving first, promotes those with the lowest salary
# ratio relative to the maximum in their grade.
# =============================================================================
cat(strrep("=", 70), "\n")
cat("3d. simulate_promotions_transfers() – wage_based promotion strategy\n")
cat(strrep("=", 70), "\n\n")

set.seed(42)
res_wage <- simulate_promotions_transfers(
  contract_dt   = contract_dt,
  personnel_dt  = personnel_dt,
  policy_params = list(
    group_cols           = "est_id",
    salary_scale         = salary_scale_dt,
    promotion_multiplier = 1.0,
    transfer_multiplier  = 1.0,
    promotion_strategy   = "wage_based",   # ← changed
    transfer_strategy    = "random"
  ),
  ref_date = ref_date
)

cat("Summary (wage_based strategy):\n")
print(res_wage$summary)

cat("\nSalary of promoted individuals (wage_based):\n")
promoted_wage <- snap_contract[
  personnel_id %in% res_wage$movers_dt[movement_type == "promotion", personnel_id],
  .(personnel_id, gross_salary_lcu)
][order(gross_salary_lcu)]
print(promoted_wage[1:min(10, .N)])

cat("\n")

# =============================================================================
# 3e. Chaining: simulate_retirement() → simulate_promotions_transfers()
# -----------------------------------------------------------------------------
# After retirements thin the workforce, simulate internal movements to
# fill gaps before external hiring.
# =============================================================================
cat(strrep("=", 70), "\n")
cat("3e. Chained workflow: retirement → promotions/transfers\n")
cat(strrep("=", 70), "\n\n")

# Step 1: Process retirements
policy_retirement <- list(
  eligibility_type = "age_and_tenure",
  min_age          = 60,
  min_tenure       = 20,
  pension_type     = "db",
  pension_params   = list(
    accrual_rate    = 0.02,
    ref_wage_col    = "gross_salary_lcu",
    max_years       = 35,
    replacement_cap = 0.8
  )
)

retirement_results <- simulate_retirement(
  contract_dt   = contract_dt,
  personnel_dt  = personnel_dt,
  policy_params = policy_retirement,
  ref_date      = ref_date
)

cat("After retirement:\n")
cat("  Retirees:             ", retirement_results$summary$n_retirees, "\n")
cat("  Remaining active:     ", retirement_results$summary$headcount_after, "\n\n")

# Step 2: Internal movement on post-retirement snapshot
set.seed(42)
movement_results <- simulate_promotions_transfers(
  contract_dt   = retirement_results$contract_dt,
  personnel_dt  = retirement_results$personnel_dt,
  policy_params = list(
    group_cols           = "est_id",
    salary_scale         = salary_scale_dt,
    promotion_multiplier = 1.5,   # Slightly elevated post-retirement
    transfer_multiplier  = 1.0,
    promotion_strategy   = "tenure",
    transfer_strategy    = "reverse_tenure"  # LIFO for transfers
  ),
  ref_date = ref_date
)

cat("After internal movements:\n")
print(movement_results$summary)

cat("\nFull movement log (first 15 rows):\n")
print(movement_results$movers_dt[1:min(15, .N)])

cat("\nDone. All movement functions demonstrated.\n")
