# =============================================================================
# Status-Quo Stress Test: 2007-09-01..2012-09-01 calibration -> 2013-2015
# projection, exercising ALL FIVE modules (retirement, exits, movement,
# hiring, allowances) plus the new hazard_rate retirement path, a custom
# eligibility rule + custom pension formula, and allowances grouped by a
# contract_dt-native dimension (paygrade).
#
# This is a rebuild of status_quo_modelling.R against the REAL package API --
# that script references simulate_horizon() arguments
# (retirement_hazard_options / exit_hazard_options / use_hazard_model) and
# data columns (contract_type_code, "perm"/"fterm"/"temp") that do not exist
# in this package or in bra_hrmis_contract's actual schema (real column is
# contract_type, values "permanent"/"fixed-term"/"short-term"/"pensioner").
#
# Two retirement variants, mirroring the original script's Run A / Run B
# comparison structure, both real:
#   Run A -- eligibility_type = "hazard_rate" (this session's new feature),
#            calibrated per integer age from the historical panel.
#   Run B -- eligibility_type = "custom" (sandboxed rule string) +
#            pension_type = "custom" (sandboxed formula string).
#
# =============================================================================

library(data.table)
devtools::load_all()

# ---------------------------------------------------------------------------
# 0. Partition data
# ---------------------------------------------------------------------------
HIST_START <- as.Date("2007-09-01")
HIST_END   <- as.Date("2012-09-01")   # last historical snapshot = sim start
N_PERIODS  <- 3L                      # 2013, 2014, 2015

panel_c_hist <- data.table::copy(bra_hrmis_contract[ref_date >= HIST_START & ref_date <= HIST_END])
panel_p_hist <- data.table::copy(bra_hrmis_personnel[ref_date >= HIST_START & ref_date <= HIST_END])

active_types <- c("permanent", "fixed-term", "short-term")

# Actual out-of-sample observations for comparison (base_salary_lcu, matching
# salary_col below -- allowances are tracked separately from base wage bill
# per simulate_allowances()'s explicit double-counting warning).
actual_years <- as.Date(c("2013-09-01", "2014-09-01", "2015-09-01"))
actual_hc <- bra_hrmis_contract[
  ref_date %in% actual_years & contract_type %in% active_types,
  .(actual_headcount = uniqueN(personnel_id),
    actual_wage_bill  = sum(base_salary_lcu, na.rm = TRUE)),
  by = ref_date
][order(ref_date)]

actual_allowance <- bra_hrmis_allowance[
  ref_date %in% actual_years,
  .(actual_allowance_total = sum(allowance_lcu, na.rm = TRUE)),
  by = ref_date
][order(ref_date)]

# ---------------------------------------------------------------------------
# 1. Salary scale: mean base_salary_lcu per est_id at the start snapshot
# ---------------------------------------------------------------------------
ct_start <- panel_c_hist[ref_date == HIST_END]
salary_scale <- ct_start[
  contract_type %in% active_types & !is.na(base_salary_lcu),
  .(base_salary_lcu = mean(base_salary_lcu, na.rm = TRUE)),
  by = est_id
]

# ---------------------------------------------------------------------------
# 2. Baseline allowance scale, grouped by est_id x paygrade x allowance_type
# ---------------------------------------------------------------------------
allowance_scale_dt <- estimate_baseline_allowance_scale(
  panel_allowance_dt = bra_hrmis_allowance,
  panel_contract_dt  = panel_c_hist,
  group_cols          = c("est_id", "paygrade"),
  ref_date             = HIST_END
)
cat("Allowance scale rows (est_id x paygrade x allowance_type):", nrow(allowance_scale_dt), "\n")

allowance_policy <- list(
  group_cols   = c("est_id", "paygrade", "allowance_type"),
  policy_table = allowance_scale_dt,
  defaults     = list(amount = 0)
)

# ---------------------------------------------------------------------------
# 3. Shared policies (identical across both runs)
# ---------------------------------------------------------------------------

exit_policy <- list(
  group_cols   = "est_id",
  policy_table = NULL,   # auto-estimated from the panel by simulate_horizon()
  defaults = list(
    exit_strategy = "random",
    active_types  = active_types
  )
)

hiring_policy <- list(
  mode               = "status_quo",
  group_cols         = "est_id",
  rate_mult          = 1.0,
  salary_scale       = salary_scale,
  panel_contract_dt  = panel_c_hist,
  panel_personnel_dt = panel_p_hist
)

movement_policy <- list(
  group_cols   = "est_id",
  policy_table = NULL,   # auto-estimated from the panel by simulate_horizon()
  defaults = list(
    movement_strategy  = "random",
    active_types       = active_types,
    salary_update_rule = "scale",
    movement_type       = "transfer"  # est_id change is a transfer, not a promotion
  )
)

# ---------------------------------------------------------------------------
# 4. Retirement -- Run A (hazard_rate) vs Run B (custom rule + custom formula)
# ---------------------------------------------------------------------------

retirement_policy_a <- list(
  group_cols   = "age_yr_int",
  policy_table = NULL,   # auto-estimated by simulate_horizon() from the panel
  defaults = list(
    eligibility_type = "hazard_rate",
    min_age          = 75,             # mandatory ceiling
    retire_strategy  = "random",
    active_types     = active_types,
    pension_type     = "rate",
    pension_rate     = 0.15,
    ref_wage_col     = "base_salary_lcu"
  )
)

retirement_policy_b <- list(
  group_cols   = NULL,
  policy_table = NULL,
  defaults = list(
    eligibility_type = "custom",
    eligibility_rule = "age >= 62 | (tenure_years >= 30 & age >= 55)",
    active_types     = active_types,
    pension_type     = "custom",
    pension_formula  = "pmin(base_salary_lcu * 0.02 * tenure_years, base_salary_lcu * 0.8)"
  )
)




# ---------------------------------------------------------------------------
# 5. Common simulate_horizon() args
# ---------------------------------------------------------------------------
common_args <- list(
  contract_dt         = panel_c_hist,
  personnel_dt        = panel_p_hist,
  salary_scale_dt     = salary_scale,
  n_periods           = N_PERIODS,
  exit_policy         = exit_policy,
  movement_policy     = movement_policy,
  hiring_policy       = hiring_policy,
  allowance_policy    = allowance_policy,
  salary_growth_rate  = 0.03,
  allowance_growth_rate = 0.03,
  ref_date            = HIST_END,
  column_spec_args    = list(salary_col = "base_salary_lcu", birth_date_col = "birth_date"),
  output_control_args = list(return_microdata = TRUE),
  seed                = 42L
)

# ---------------------------------------------------------------------------
# 6. Run A -- hazard_rate retirement
# ---------------------------------------------------------------------------
cat("\n--- Run A: hazard_rate retirement ---\n")
result_a <- do.call(simulate_horizon, c(common_args, list(retirement_policy = retirement_policy_a)))
cat("Run A complete.\n")

# ---------------------------------------------------------------------------
# 7. Run B -- custom eligibility rule + custom pension formula
# ---------------------------------------------------------------------------
cat("\n--- Run B: custom eligibility + custom pension formula ---\n")
result_b <- do.call(simulate_horizon, c(common_args, list(retirement_policy = retirement_policy_b)))
cat("Run B complete.\n")

# ---------------------------------------------------------------------------
# 8. Comparison
# ---------------------------------------------------------------------------
# Vectorized "+1 calendar year" (period_unit = "year" throughout this
# script), matching .advance_period()'s own year-advance semantics.
advance_year <- function(d) {
  as.Date(ISOdate(as.integer(format(d, "%Y")) + 1L, format(d, "%m"), format(d, "%d")))
}

extract_summary <- function(result, label) {
  # period_date is the reference/eligibility date at the START of the
  # period (t=1's period_date == ref_date itself -- see the
  # ".advance_period(ref_date, t - 1L, ...)" call in the period loop); the
  # _end columns are the state AFTER that period's transitions, i.e.
  # effectively one period_unit LATER than period_date. Advance by 1 year so
  # the join below lines up with actual_hc's real observation dates.
  result$comparison[, .(
    ref_date              = advance_year(period_date),
    sim_headcount         = n_headcount_end,
    sim_wage_bill         = wage_bill_end,
    sim_allowance_total   = allowance_cost_end,
    run                   = label
  )]
}

make_comparison <- function(sim_dt, label) {
  out <- sim_dt[actual_hc, on = "ref_date", nomatch = NA]
  out <- out[actual_allowance, on = "ref_date", nomatch = NA]
  out[, headcount_error_pct   := round((sim_headcount - actual_headcount) / actual_headcount * 100, 1)]
  out[, wage_bill_error_pct   := round((sim_wage_bill - actual_wage_bill) / actual_wage_bill * 100, 1)]
  out[, allowance_error_pct   := round((sim_allowance_total - actual_allowance_total) / actual_allowance_total * 100, 1)]
  out[, run := label]
  out
}

comp_a <- make_comparison(extract_summary(result_a, "A_hazard_rate"), "A_hazard_rate")
comp_b <- make_comparison(extract_summary(result_b, "B_custom"), "B_custom")
comparison <- rbindlist(list(comp_a, comp_b))[order(run, ref_date)]

cat("\n=== Headcount, Wage Bill & Allowance Comparison ===\n")
print(comparison[, .(
  Run          = run,
  Year         = format(ref_date, "%Y"),
  Sim_HC       = sim_headcount,
  Actual_HC    = actual_headcount,
  HC_Err_pct   = headcount_error_pct,
  Sim_WB       = round(sim_wage_bill),
  Actual_WB    = round(actual_wage_bill),
  WB_Err_pct   = wage_bill_error_pct,
  Sim_Allow    = round(sim_allowance_total),
  Actual_Allow = round(actual_allowance_total),
  Allow_Err_pct = allowance_error_pct
)])

cat("\n=== Period Decomposition ===\n")
make_decomp <- function(result, label) {
  result$comparison[, .(
    Run           = label,
    Year          = format(period_date, "%Y"),
    HC_start      = n_headcount_start,
    n_retirements = n_exits,
    n_vol_exits   = n_non_ret_exits,
    n_promotions  = n_promotions,
    n_transfers   = n_transfers,
    n_hires       = n_hires,
    HC_end        = n_headcount_end,
    WageBill_end  = round(wage_bill_end),
    Allowance_end = round(allowance_cost_end),
    COLA_effect   = round(inflation_effect),
    Exit_savings  = round(exit_savings),
    Pension_new   = round(pension_cost_new)
  )]
}
decomp <- rbindlist(list(make_decomp(result_a, "A_hazard_rate"), make_decomp(result_b, "B_custom")))[order(Run, Year)]
print(decomp)

cat("\n=== Retiree ledger sanity checks ===\n")
cat("Run A pensioner_register rows:", nrow(result_a$pensioner_register), "\n")
cat("Run A pension_amount range:", paste(round(range(result_a$pensioner_register$pension_amount)), collapse = " - "), "\n")
cat("Run B pensioner_register rows:", nrow(result_b$pensioner_register), "\n")
cat("Run B pension_amount range:", paste(round(range(result_b$pensioner_register$pension_amount)), collapse = " - "), "\n")

cat("\n=== Reproducibility check (seed) ===\n")
result_a2 <- do.call(simulate_horizon, c(common_args, list(retirement_policy = retirement_policy_a)))
cat("Run A identical on repeat with same seed:", identical(result_a$comparison, result_a2$comparison), "\n")
