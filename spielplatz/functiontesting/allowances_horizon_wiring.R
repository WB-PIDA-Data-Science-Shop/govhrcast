# =============================================================================
# Demo/scratchpad: Stage 2 -- simulate_horizon()/simulate_scenario() wiring
#
# Verifies the allowance module's integration into the horizon pipeline:
#   1. allowance_policy = NULL must reproduce pre-existing simulate_horizon()
#      output identically (regression safety).
#   2. An active allowance_policy reconciles total_wage_bill = wage_bill +
#      allowance_cost every period.
#   3. Stayers' allowances grow at allowance_growth_rate; movers pick up
#      their new group's current amount instead of a stale one.
#
# Run with: devtools::load_all() then source this file.
# =============================================================================

library(data.table)

baseline_date <- as.Date("2010-09-01")
n_periods <- 5

ct <- copy(bra_hrmis_contract)
pt <- copy(bra_hrmis_personnel)
ss <- ct[ref_date == baseline_date, .(gross_salary_lcu = mean(gross_salary_lcu, na.rm = TRUE)), by = est_id]

retirement_policy <- list(
  group_cols = NULL, policy_table = NULL,
  defaults = list(eligibility_type = "age_and_tenure", 
                  pension_type = "db",
                  min_age = 60, 
                  min_tenure = 10, 
                  accrual_rate = 0.02,
                  ref_wage_col = "gross_salary_lcu", 
                  max_years = 35, 
                  replacement_cap = 0.80)
)

# -----------------------------------------------------------------------------
# 1. Regression: allowance_policy = NULL must not change existing behavior
# -----------------------------------------------------------------------------
before <- simulate_horizon(
  contract_dt = ct, personnel_dt = pt, salary_scale_dt = ss, n_periods = n_periods,
  retirement_policy = retirement_policy, ref_date = baseline_date, salary_growth_rate = 0.03
)
after <- simulate_horizon(
  contract_dt = ct, personnel_dt = pt, salary_scale_dt = ss, n_periods = n_periods,
  retirement_policy = retirement_policy, ref_date = baseline_date, salary_growth_rate = 0.03,
  allowance_policy = NULL
)
existing_cols <- names(before$comparison)
cat("existing columns identical:", isTRUE(all.equal(
  before$comparison[, ..existing_cols], after$comparison[, ..existing_cols]
)), "\n")
cat("new columns present with allowance_policy=NULL:",
    all(c("allowance_cost_start", "allowance_cost_end", "total_wage_bill_start", "total_wage_bill_end")
        %in% names(after$comparison)), "\n")
cat("allowance_cost_end all zero when unused:", all(after$comparison$allowance_cost_end == 0), "\n")
cat("total_wage_bill_end == wage_bill_end when unused:",
    isTRUE(all.equal(after$comparison$total_wage_bill_end, after$comparison$wage_bill_end)), "\n")

# -----------------------------------------------------------------------------
# 2. Active policy: reconcile total_wage_bill against components
# -----------------------------------------------------------------------------
allowance_scale_dt <- estimate_baseline_allowance_scale(
  panel_allowance_dt = bra_hrmis_allowance, panel_contract_dt = bra_hrmis_contract,
  group_cols = "est_id", ref_date = baseline_date
)
allowance_policy <- list(
  group_cols = c("est_id", "allowance_type"), policy_table = allowance_scale_dt,
  defaults = list(amount = 0)
)

out <- simulate_horizon(
  contract_dt = ct, personnel_dt = pt, salary_scale_dt = ss, n_periods = n_periods,
  retirement_policy = retirement_policy, ref_date = baseline_date, salary_growth_rate = 0.03,
  allowance_policy = allowance_policy, allowance_growth_rate = 0.02,
  return_microdata = TRUE
)

cat("\n--- comparison (allowance/total_wage_bill columns) ---\n")
print(out$comparison[, .(period_date, wage_bill_end, allowance_cost_start, allowance_cost_end,
                          total_wage_bill_start, total_wage_bill_end)])

cat("\ntotal_wage_bill_end reconciles:", isTRUE(all.equal(
  out$comparison$total_wage_bill_end, out$comparison$wage_bill_end + out$comparison$allowance_cost_end
)), "\n")
cat("allowance_cost_start[t] == allowance_cost_end[t-1]:", isTRUE(all.equal(
  out$comparison$allowance_cost_start[-1], out$comparison$allowance_cost_end[-n_periods]
)), "\n")
cat("period 1 allowance_cost_start is 0:", out$comparison$allowance_cost_start[1] == 0, "\n")

cat("\nnrow allowance_dt (return_microdata=TRUE):", nrow(out$allowance_dt), "\n")
cat("distinct period_dates in allowance_dt:", uniqueN(out$allowance_dt$period_date), " (expect", n_periods, ")\n")

# -----------------------------------------------------------------------------
# 3. Growth check for a stayer: does their allowance grow at allowance_growth_rate?
# -----------------------------------------------------------------------------
cand <- out$allowance_dt[amount > 0, .N, by = .(contract_id, allowance_type)][N == n_periods][1]
stayer_rows <- out$allowance_dt[contract_id == cand$contract_id & allowance_type == cand$allowance_type][order(period_date)]
cat("\n--- one stayer's allowance across periods (should grow ~2%/period) ---\n")
print(stayer_rows[, .(period_date, allowance_type, amount)])
n <- nrow(stayer_rows)
if (n >= 2) {
  cat("period-over-period ratios (should be ~1.02):\n")
  print(stayer_rows$amount[2:n] / stayer_rows$amount[1:(n - 1)])
}

# -----------------------------------------------------------------------------
# 4. Allowance group_cols independent of, and finer than, the salary scale's
#    own group_cols. Salary is scaled by est_id x paygrade only; allowance
#    additionally distinguishes contract_type -- e.g. permanent staff getting
#    a different hazard/transport allowance than fixed-term staff in the SAME
#    est_id x paygrade. Nothing about the allowance module requires its
#    grouping to match salary's.
# -----------------------------------------------------------------------------
cat("\n\n=== 4. Allowance group_cols richer than salary scale's group_cols ===\n")

salary_scale_fine <- ct[ref_date == baseline_date,
                         .(gross_salary_lcu = mean(gross_salary_lcu, na.rm = TRUE)),
                         by = .(est_id, paygrade)]
cat("salary scale grouped by: est_id x paygrade (", nrow(salary_scale_fine), "rows )\n")

allowance_scale_fine <- estimate_baseline_allowance_scale(
  panel_allowance_dt = bra_hrmis_allowance,
  panel_contract_dt  = bra_hrmis_contract,
  group_cols         = c("est_id", "paygrade", "contract_type"),  # finer than salary's grouping
  ref_date           = baseline_date
)
cat("allowance scale grouped by: est_id x paygrade x contract_type (", nrow(allowance_scale_fine), "rows )\n")

allowance_policy_fine <- list(
  group_cols   = c("est_id", "paygrade", "contract_type", "allowance_type"),
  policy_table = allowance_scale_fine,
  defaults     = list(amount = 0)
)

out_fine <- simulate_horizon(
  contract_dt = ct, personnel_dt = pt, salary_scale_dt = salary_scale_fine, n_periods = 1,
  retirement_policy = retirement_policy, ref_date = baseline_date,
  allowance_policy = allowance_policy_fine, return_microdata = TRUE
)

# Show that, within the SAME est_id x paygrade, contract_type actually
# differentiates the allowance amount -- proving the finer grouping took
# effect, independent of the coarser salary scale.
# Find an (est_id, paygrade, allowance_type) combo where the amount actually
# DIFFERS across contract_type -- e.g. permanent gets a nonzero amount, but
# fixed-term/short-term don't (or get a different nonzero amount). Don't
# require amount > 0 for all rows -- a 0-vs-positive contrast still proves
# the finer grouping took effect.
# paygrade has real NAs in this data -- filter those out here, since `==` on
# an NA value never matches (NA == NA is NA, not TRUE) and a join is the
# NA-safe way to filter, but simplest is to just exclude NA-paygrade combos
# from this particular illustration.
varying <- allowance_scale_fine[!is.na(paygrade),
                                 .(n_distinct = data.table::uniqueN(amount)),
                                 by = .(est_id, paygrade, allowance_type)][n_distinct > 1][1]
if (!is.null(varying) && nrow(varying) > 0) {
  cat("\n--- same est_id x paygrade x allowance_type, different contract_type -> different allowance ---\n")
  # Join (NA-safe), not == (fails silently on NA), to select the matching rows.
  print(allowance_scale_fine[varying[, .(est_id, paygrade, allowance_type)], on = c("est_id", "paygrade", "allowance_type")])
} else {
  cat("\n(no est_id x paygrade x allowance_type combo varied by contract_type in this baseline)\n")
}

# -----------------------------------------------------------------------------
# 5. Targeted increase to a SPECIFIC allowance type by a specific fraction --
#    reusing apply_salary_scale_adjustment() as-is on the allowance scale, per
#    the original design doc ("reuse, don't build new"). A named-vector
#    adjustment keyed by allowance_type leaves every other type untouched.
# -----------------------------------------------------------------------------
cat("\n\n=== 5. Targeted increase to one allowance type (e.g. +20% hazard-equivalent pay) ===\n")

target_type <- allowance_scale_dt[amount > 0][1]$allowance_type
cat("targeting allowance_type:", target_type, "\n")

allowance_scale_boosted <- apply_salary_scale_adjustment(
  allowance_scale_dt,
  adjustment = stats::setNames(1.20, target_type),  # +20% for this type only
  salary_col = "amount",
  key_col    = "allowance_type"
)

cat("\n--- amounts before vs after targeted +20% adjustment ---\n")
comparison_dt <- merge(
  allowance_scale_dt[, .(est_id, allowance_type, amount_before = amount)],
  allowance_scale_boosted[, .(est_id, allowance_type, amount_after = amount)],
  by = c("est_id", "allowance_type")
)
print(comparison_dt[allowance_type == target_type][amount_before > 0][1:3])
print(comparison_dt[allowance_type != target_type][amount_before > 0][1:3])
cat("\nratio for targeted type (should be 1.20):",
    unique(round(comparison_dt[allowance_type == target_type & amount_before > 0]$amount_after /
                 comparison_dt[allowance_type == target_type & amount_before > 0]$amount_before, 6)), "\n")
cat("ratio for other types (should be 1.0, unchanged):",
    unique(comparison_dt[allowance_type != target_type & amount_before > 0]$amount_after /
           comparison_dt[allowance_type != target_type & amount_before > 0]$amount_before), "\n")

# Feed the boosted (one-time policy-change) scale into a horizon run, exactly
# like any other allowance_policy$policy_table.
allowance_policy_boosted <- list(
  group_cols = c("est_id", "allowance_type"), policy_table = allowance_scale_boosted,
  defaults = list(amount = 0)
)
out_boosted <- simulate_horizon(
  contract_dt = ct, personnel_dt = pt, salary_scale_dt = ss, n_periods = 1,
  retirement_policy = retirement_policy, ref_date = baseline_date,
  allowance_policy = allowance_policy_boosted
)
cat("\nallowance_cost_end with +20% on", target_type, ":", out_boosted$comparison$allowance_cost_end, "\n")
cat("allowance_cost_end with unmodified scale:            ",
    simulate_horizon(contract_dt = ct, personnel_dt = pt, salary_scale_dt = ss, n_periods = 1,
                     retirement_policy = retirement_policy, ref_date = baseline_date,
                     allowance_policy = allowance_policy)$comparison$allowance_cost_end, "\n")
