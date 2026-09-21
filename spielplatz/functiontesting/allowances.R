# =============================================================================
# Demo/scratchpad: Allowance Module
#
# Exercises the new allowance module (R/allowance_core.R, R/allowance_update.R,
# R/simulate_allowances.R) against the lazy-loaded Brazil HRMIS data. Not a
# formal test suite (see tests/testthat/test-allowance_*.R for that) -- this
# is meant to be run interactively and tweaked freely while exploring the
# functions or debugging something new.
#
# Run with: devtools::load_all() then source this file (or run section by
# section).
# =============================================================================

library(data.table)

# -----------------------------------------------------------------------------
# 0. Data prep
# -----------------------------------------------------------------------------
# NOTE: deliberately NOT called `ref_date` here. bra_hrmis_contract/
# bra_hrmis_allowance both have a column literally named "ref_date" -- inside
# a data.table `[...]` expression, a bare symbol resolves to a same-named
# COLUMN before it resolves to a variable in this scope, so `ref_date ==
# ref_date` would silently become a no-op filter (every row equals itself)
# and return the ENTIRE unfiltered table. This is the exact bug that was
# caught and fixed in estimate_baseline_allowance_scale() itself -- see the
# `select_nearest_ref_date()` usage there.
baseline_date <- as.Date("2018-09-01")

ct <- bra_hrmis_contract[ref_date == baseline_date & !is.na(est_id)]
al <- bra_hrmis_allowance[ref_date == baseline_date]

# -----------------------------------------------------------------------------
# 1. estimate_baseline_allowance_scale() -- single-snapshot unconditional mean
# -----------------------------------------------------------------------------
# Pass the FULL multi-period panel; the function resolves ref_date to the
# nearest available snapshot internally (via select_nearest_ref_date()).
scale_dt <- estimate_baseline_allowance_scale(
  panel_allowance_dt = bra_hrmis_allowance,
  panel_contract_dt  = bra_hrmis_contract,
  group_cols         = "est_id",
  ref_date           = baseline_date
)

cat("\n--- scale_dt (head) ---\n")
print(head(scale_dt))
cat("nrow:", nrow(scale_dt), "\n")

# Hand-check one non-zero est_id x allowance_type combo against the raw data.
nonzero_row <- scale_dt[amount > 0][1]
est1  <- nonzero_row$est_id
type1 <- nonzero_row$allowance_type
contracts1 <- unique(ct[est_id == est1]$contract_id)
hand_calc  <- sum(al[contract_id %in% contracts1 & allowance_type == type1]$allowance_lcu) / length(contracts1)
cat("\nhand_calc:", hand_calc, " pkg_calc:", nonzero_row$amount,
    " match:", isTRUE(all.equal(hand_calc, nonzero_row$amount)), "\n")

# Try a ref_date that doesn't land exactly on a snapshot -- should snap to the
# nearest one on/before it (2018-09-01) rather than erroring or blending.
scale_dt_near <- estimate_baseline_allowance_scale(
  panel_allowance_dt = bra_hrmis_allowance,
  panel_contract_dt  = bra_hrmis_contract,
  group_cols         = "est_id",
  ref_date           = as.Date("2018-11-15")
)
cat("\nnearest-snapshot result identical to exact-date result:",
    isTRUE(all.equal(scale_dt_near, scale_dt)), "\n")

# -----------------------------------------------------------------------------
# 2. resolve_allowance_amounts() -- per-contract assignment from a policy
# -----------------------------------------------------------------------------
policy_flat <- list(
  group_cols   = c("est_id", "allowance_type"),
  policy_table = scale_dt,
  defaults     = list(amount = 0)
)

resolved <- resolve_allowance_amounts(
  contract_dt     = ct,
  policy_params   = policy_flat,
  allowance_types = unique(al$allowance_type)
)

cat("\n--- resolved (head) ---\n")
print(head(resolved))
cat("nrow resolved:", nrow(resolved), " (expect n_contracts x n_types = ",
    data.table::uniqueN(ct$contract_id), " x ", data.table::uniqueN(al$allowance_type), ")\n")

# Try a total_allowance_multiplier on top -- e.g. a 20% across-the-board bump.
policy_boosted <- c(policy_flat, list(multiplier_default = 1.2))
resolved_boosted <- resolve_allowance_amounts(
  contract_dt     = ct,
  policy_params   = policy_boosted,
  allowance_types = unique(al$allowance_type)
)
cat("multiplier applied correctly:",
    isTRUE(all.equal(resolved_boosted$amount, resolved$amount * 1.2)), "\n")

# -----------------------------------------------------------------------------
# 3. simulate_allowances() -- single period, NULL policy, and accumulation
# -----------------------------------------------------------------------------
pt <- bra_hrmis_personnel[ref_date == baseline_date]

# NULL policy_params is a no-op.
out_noop <- simulate_allowances(
  contract_dt = ct, personnel_dt = pt, policy_params = NULL, ref_date = baseline_date
)
cat("\n--- NULL policy summary ---\n")
print(out_noop$summary)

# Active policy, first period.
out1 <- simulate_allowances(
  contract_dt = ct, personnel_dt = pt, policy_params = policy_flat, ref_date = baseline_date
)
cat("\n--- period 1 summary ---\n")
print(out1$summary)

# Second period: ledger should accumulate, not overwrite.
ref_date_2 <- as.Date("2019-09-01")
out2 <- simulate_allowances(
  contract_dt = ct, personnel_dt = pt, allowance_dt = out1$allowance_dt,
  policy_params = policy_flat, ref_date = ref_date_2
)
cat("\n--- period 2 summary ---\n")
print(out2$summary)
cat("allowance_dt rows after 2 periods:", nrow(out2$allowance_dt),
    " (expect 2x period 1's ", nrow(out1$allowance_dt), ")\n")
cat("distinct period_date values in ledger:", data.table::uniqueN(out2$allowance_dt$period_date), "\n")

# -----------------------------------------------------------------------------
# Scratch space -- tweak below as needed
# -----------------------------------------------------------------------------
