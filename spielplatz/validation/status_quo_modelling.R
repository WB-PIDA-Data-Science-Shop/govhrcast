# =============================================================================
# Status-Quo Validation: 2007â€“2012 calibration â†’ 2013â€“2015 out-of-sample
# =============================================================================
#
# Strategy
# --------
# Use 2007-09-01 through 2012-09-01 as the historical panel to calibrate all
# status-quo policies automatically, then run simulate_horizon() forward 3
# periods (2013, 2014, 2015).  Compare simulated headcount and wage bill
# against the actual values observed in the data.
#
# Two simulation variants are compared side-by-side:
#   Run A  â€” Eligibility-rule retirement + rate-based exits (classic status-quo)
#   Run B  â€” GLM hazard model for retirement AND exits (data-driven take-up)
#
# Policy assumptions (all status-quo / data-driven)
# -------------------------------------------------
# Retirement  : age_only, min_age = 62, pension_type = "flat", flat_amount = 0.
#               No pension formula parameters available in bra_hrmis.
#               Note: eligibility does NOT guarantee retirement â€” in Run B the
#               hazard model governs actual take-up probability.
#
# Exits       : estimate_historical_exit_rates() calibrated from 2007-2012
#               panel, grouped by est_id.  Run B replaces this with a GLM.
#
# Hiring      : status_quo mode â€” estimate_historical_hiring_rates() calibrated
#               from the 2007-2012 panel, grouped by est_id.
#
# Movement    : status_quo mode â€” estimate_movement_baseline() calibrated from
#               the 2007-2012 panel, grouped by est_id.
#
# Salary COLA : geometric mean of annual growth 2007-2012 â‰ˆ 10%.
# =============================================================================

library(data.table)
devtools::load_all()          # load govhrcast from the project root

# ---------------------------------------------------------------------------
# 0. Partition data
# ---------------------------------------------------------------------------
HIST_START  <- as.Date("2007-09-01")
HIST_END    <- as.Date("2012-09-01")   # last historical snapshot = sim start
N_PERIODS   <- 3L                      # 2013, 2014, 2015

panel_c_hist <- bra_hrmis_contract[ref_date >= HIST_START & ref_date <= HIST_END]
panel_p_hist <- bra_hrmis_personnel[ref_date >= HIST_START & ref_date <= HIST_END]

# Actual out-of-sample observations for comparison
actual_years <- as.Date(c("2013-09-01", "2014-09-01", "2015-09-01"))
actual_hc    <- bra_hrmis_contract[
  ref_date %in% actual_years & contract_type_code %in% c("perm", "fterm", "temp"),
  .(actual_headcount = uniqueN(personnel_id),
    actual_wage_bill  = sum(gross_salary_lcu, na.rm = TRUE)),
  by = ref_date
][order(ref_date)]

# ---------------------------------------------------------------------------
# 1. Starting snapshot (2012-09-01)
# ---------------------------------------------------------------------------
ct_start <- bra_hrmis_contract[ref_date == HIST_END][, ref_date := NULL]
pt_start <- bra_hrmis_personnel[ref_date == HIST_END][, ref_date := NULL]

# ---------------------------------------------------------------------------
# 2. Salary scale: mean gross salary per est_id at the start snapshot
# ---------------------------------------------------------------------------
salary_scale <- ct_start[
  contract_type_code %in% c("perm", "fterm", "temp") & !is.na(gross_salary_lcu),
  .(gross_salary_lcu = mean(gross_salary_lcu, na.rm = TRUE)),
  by = est_id
]

# ---------------------------------------------------------------------------
# 3. Salary COLA: geometric mean of annual growth 2007-2012
# ---------------------------------------------------------------------------
sal_by_year <- bra_hrmis_contract[
  ref_date >= HIST_START & ref_date <= HIST_END &
    contract_type_code %in% c("perm", "fterm", "temp") &
    !is.na(gross_salary_lcu),
  .(mean_sal = mean(gross_salary_lcu)),
  by = ref_date
][order(ref_date)]

n_years   <- nrow(sal_by_year) - 1L
cola_rate <- (sal_by_year$mean_sal[nrow(sal_by_year)] /
                sal_by_year$mean_sal[1L])^(1 / n_years) - 1
cat(sprintf("Estimated annual COLA rate (2007-2012 geometric mean): %.1f%%\n",
            cola_rate * 100))

# ---------------------------------------------------------------------------
# 4. Shared policies (identical across both runs)
# ---------------------------------------------------------------------------
active_types <- c("perm", "fterm", "temp")

# Retirement policy â€” flat pension (no formula data available in bra_hrmis;
# pension_type = "db" would require a ref_wage_col).
retirement_policy <- list(
  group_cols   = NULL,
  policy_table = NULL,
  defaults = list(
    eligibility_type = "age_only",
    pension_type     = "flat",
    flat_amount      = 0,
    min_age          = 62,
    active_types     = active_types
  )
)

# Exit policy â€” rate-based, auto-estimated per est_id from the panel.
exit_policy <- list(
  group_cols   = "est_id",
  policy_table = NULL,
  defaults = list(
    exit_rate     = 0.04,
    exit_strategy = "random",
    active_types  = active_types,
    exited_type   = "inactive"
  )
)

# Hiring policy â€” status_quo, auto-estimated per est_id from the panel.
hiring_policy <- list(
  mode               = "status_quo",
  group_cols         = "est_id",
  rate_mult          = 1.0,
  salary_scale       = salary_scale,
  panel_contract_dt  = panel_c_hist,
  panel_personnel_dt = panel_p_hist
)

# Movement policy â€” status_quo, auto-estimated per est_id from the panel.
movement_policy <- list(
  group_cols   = "est_id",
  policy_table = NULL,
  defaults = list(
    movement_rate      = 0.03,
    movement_strategy  = "random",
    active_types       = active_types,
    salary_update_rule = "scale"
  )
)

# Common simulate_horizon() args (everything except hazard options)
common_args <- list(
  contract_dt        = panel_c_hist,
  personnel_dt       = panel_p_hist,
  salary_scale_dt    = salary_scale,
  n_periods          = 4,
  retirement_policy  = retirement_policy,
  exit_policy        = exit_policy,
  movement_policy    = movement_policy,
  hiring_policy      = hiring_policy,
  salary_growth_rate = cola_rate,
  ref_date           = HIST_END,
  birth_date_col     = "birth_date",
  return_microdata   = FALSE
)

# ---------------------------------------------------------------------------
# 5. Run A â€” eligibility-rule retirement + rate-based exits
# ---------------------------------------------------------------------------
cat("\n--- Run A: eligibility + rate-based exits ---\n")
set.seed(42L)
result_a <- do.call(simulate_horizon, c(common_args, list(
  retirement_hazard_options = list(use_hazard_model = FALSE),
  exit_hazard_options       = list(use_hazard_model = FALSE)
)))

# ---------------------------------------------------------------------------
# 6. Run B â€” GLM hazard model for retirement AND exits
# ---------------------------------------------------------------------------
cat("\n--- Run B: hazard models for retirement and exits ---\n")
set.seed(42L)
result_b <- suppressWarnings(do.call(simulate_horizon, c(common_args, list(
  retirement_hazard_options = list(use_hazard_model = TRUE),
  exit_hazard_options       = list(use_hazard_model = TRUE)
))))

# ---------------------------------------------------------------------------
# 7. Helper: extract headcount + wage bill from a horizon result
# ---------------------------------------------------------------------------
extract_summary <- function(result, label) {
  result$comparison[, .(
    ref_date      = period_date,
    sim_headcount = n_headcount_end,
    sim_wage_bill = wage_bill_end,
    run           = label
  )]
}

# ---------------------------------------------------------------------------
# 8. Three-way comparison: Run A, Run B, Actual
# ---------------------------------------------------------------------------
cat("\n=== Headcount & Wage Bill Comparison ===\n")

make_comparison <- function(sim_dt, label) {
  out <- sim_dt[actual_hc, on = "ref_date", nomatch = NA]
  out[, headcount_error_pct := round(
    (sim_headcount - actual_headcount) / actual_headcount * 100, 1)]
  out[, wage_bill_error_pct := round(
    (sim_wage_bill - actual_wage_bill) / actual_wage_bill * 100, 1)]
  out[, run := label]
  out
}

comp_a <- make_comparison(extract_summary(result_a, "A_eligibility"), "A_eligibility")
comp_b <- make_comparison(extract_summary(result_b, "B_hazard"),      "B_hazard")
comparison <- rbindlist(list(comp_a, comp_b))[order(run, ref_date)]

print(comparison[, .(
  Run               = run,
  Year              = format(ref_date, "%Y"),
  Sim_HC            = sim_headcount,
  Actual_HC         = actual_headcount,
  HC_Err_pct        = headcount_error_pct,
  Sim_WB            = round(sim_wage_bill),
  Actual_WB         = round(actual_wage_bill),
  WB_Err_pct        = wage_bill_error_pct
)])

# ---------------------------------------------------------------------------
# 9. Per-period decomposition (both runs)
# ---------------------------------------------------------------------------
cat("\n=== Period Decomposition ===\n")

make_decomp <- function(result, label) {
  result$comparison[, .(
    Run            = label,
    Year           = format(period_date, "%Y"),
    HC_start       = n_headcount_start,
    n_retirements  = n_exits,
    n_vol_exits    = n_non_ret_exits,
    n_hires        = n_hires,
    HC_end         = n_headcount_end,
    WageBill_end   = round(wage_bill_end),
    COLA_effect    = round(inflation_effect),
    Exit_savings   = round(exit_savings)
  )]
}

decomp <- rbindlist(list(
  make_decomp(result_a, "A_eligibility"),
  make_decomp(result_b, "B_hazard")
))[order(Run, Year)]
print(decomp)

# ---------------------------------------------------------------------------
# 10. Actual observed period-over-period changes (context)
# ---------------------------------------------------------------------------
actual_all <- bra_hrmis_contract[
  ref_date %in% c(HIST_END, actual_years) &
    contract_type_code %in% c("perm", "fterm", "temp"),
  .(headcount = uniqueN(personnel_id),
    wage_bill  = sum(gross_salary_lcu, na.rm = TRUE)),
  by = ref_date
][order(ref_date)]
actual_all[, hc_change := headcount - shift(headcount)]
actual_all[, wb_growth := round((wage_bill / shift(wage_bill) - 1) * 100, 1)]

cat("\n=== Actual Observed Changes (including 2012 baseline) ===\n")
print(actual_all)


