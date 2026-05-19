# =============================================================================
# Status-Quo Validation: 2007–2012 calibration → 2013–2015 out-of-sample
# =============================================================================
#
# Strategy
# --------
# Use 2007-09-01 through 2012-09-01 as the historical panel to calibrate all
# status-quo policies automatically, then run simulate_horizon() forward 3
# periods (2013, 2014, 2015).  Compare simulated headcount and wage bill
# against the actual values observed in the data.
#
# Policy assumptions (all status-quo / data-driven)
# -------------------------------------------------
# Retirement  : age_only, min_age = 60, pension_type = "flat" (no pension
#               formula data available), flat_amount = 0.
#               Rationale: median active age is 47, 90th pct is 65 at 2012;
#               age 60 sits between the 75th and 90th pct and matches the
#               ~5-7 new pensioners per year visible in the panel.
#
# Exits       : status_quo mode — estimate_historical_exit_rates() calibrated
#               from the 2007-2012 panel, grouped by est_id.
#
# Hiring      : status_quo mode — estimate_historical_hiring_rates() calibrated
#               from the 2007-2012 panel, grouped by est_id.
#
# Movement    : status_quo mode — estimate_movement_baseline() calibrated from
#               the 2007-2012 panel, grouped by est_id.
#
# Salary COLA : mean annual growth 2007-2012 ≈ 9.2% (geometric mean).
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

# Actual observations for comparison
actual_years  <- as.Date(c("2013-09-01", "2014-09-01", "2015-09-01"))
actual_hc     <- bra_hrmis_contract[
  ref_date %in% actual_years & contract_type_code %in% c("perm", "fterm", "temp"),
  .(actual_headcount = uniqueN(personnel_id),
    actual_wage_bill  = sum(gross_salary_lcu, na.rm = TRUE)),
  by = ref_date
][order(ref_date)]

# ---------------------------------------------------------------------------
# 1. Starting snapshot (2012-09-01, single period)
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
# Annual mean salary by year
sal_by_year <- bra_hrmis_contract[
  ref_date >= HIST_START & ref_date <= HIST_END &
    contract_type_code %in% c("perm", "fterm", "temp") &
    !is.na(gross_salary_lcu),
  .(mean_sal = mean(gross_salary_lcu)),
  by = ref_date
][order(ref_date)]

n_years <- nrow(sal_by_year) - 1L
cola_rate <- (sal_by_year$mean_sal[nrow(sal_by_year)] /
                sal_by_year$mean_sal[1L])^(1 / n_years) - 1
cat(sprintf("Estimated annual COLA rate (2007-2012 geometric mean): %.1f%%\n",
            cola_rate * 100))

# ---------------------------------------------------------------------------
# 4. Retirement policy
# ---------------------------------------------------------------------------
# Active types in this dataset
active_types <- c("perm", "fterm", "temp")

retirement_policy <- list(
  group_cols   = NULL,
  policy_table = NULL,
  defaults = list(
    eligibility_type = "age_only",
    pension_type     = "flat",
    min_age          = 62,
    flat_amount      = 0,      # no pension formula data — track exits only
    active_types     = active_types
  )
)

# ---------------------------------------------------------------------------
# 5. Exit policy (status-quo)
# ---------------------------------------------------------------------------
exit_policy <- list(
  group_cols   = "est_id",
  policy_table = NULL,        # will be auto-estimated from panel in simulate_horizon
  defaults = list(
    exit_rate     = 0.04,     # fallback if estimation fails for any group
    exit_strategy = "random",
    active_types  = active_types,
    exited_type   = "inactive"
  )
)

# ---------------------------------------------------------------------------
# 6. Hiring policy (status-quo)
# ---------------------------------------------------------------------------
hiring_policy <- list(
  mode               = "status_quo",
  group_cols         = "est_id",
  rate_mult          = 1.0,
  salary_scale       = salary_scale,
  panel_contract_dt  = panel_c_hist,   # injected explicitly
  panel_personnel_dt = panel_p_hist
)

# ---------------------------------------------------------------------------
# 7. Movement policy (status-quo)
# ---------------------------------------------------------------------------
movement_policy <- list(
  group_cols   = "est_id",
  policy_table = NULL,        # will be auto-estimated from panel in simulate_horizon
  defaults = list(
    movement_rate      = 0.03,    # fallback
    movement_strategy  = "random",
    active_types       = active_types,
    salary_update_rule = "scale"
  )
)

# ---------------------------------------------------------------------------
# 8. Run simulation
# ---------------------------------------------------------------------------
cat("\nRunning simulate_horizon() for 3 periods (2013, 2014, 2015)...\n")
sim_result <- simulate_horizon(
  contract_dt        = panel_c_hist,   # full panel — horizon strips to HIST_END
  personnel_dt       = panel_p_hist,
  salary_scale_dt    = salary_scale,
  n_periods          = N_PERIODS,
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
# 9. Extract simulated summary and build comparison table
# ---------------------------------------------------------------------------
sim_summary <- sim_result$comparison[, .(
  period_date,
  sim_headcount = n_headcount_end,
  sim_wage_bill = wage_bill_end
)]

# Add a year column for joining
sim_summary[, ref_date := period_date]
actual_years_dt <- actual_hc[, .(ref_date, actual_headcount, actual_wage_bill)]

comparison <- sim_summary[actual_years_dt, on = "ref_date", nomatch = NA]
comparison[, headcount_error_pct := round(
  (sim_headcount - actual_headcount) / actual_headcount * 100, 1)]
comparison[, wage_bill_error_pct := round(
  (sim_wage_bill - actual_wage_bill) / actual_wage_bill * 100, 1)]

cat("\n=== Headcount & Wage Bill Comparison ===\n")
print(comparison[, .(
  Year              = format(ref_date, "%Y"),
  Sim_Headcount     = sim_headcount,
  Actual_Headcount  = actual_headcount,
  HC_Error_pct      = headcount_error_pct,
  Sim_WageBill      = round(sim_wage_bill),
  Actual_WageBill   = round(actual_wage_bill),
  WB_Error_pct      = wage_bill_error_pct
)])

# ---------------------------------------------------------------------------
# 10. Per-period decomposition from simulation
# ---------------------------------------------------------------------------
cat("\n=== Simulation Period Decomposition ===\n")
decomp <- sim_result$comparison[, .(
  Year             = format(period_date, "%Y"),
  HC_start         = n_headcount_start,
  n_retirements    = n_exits,
  n_non_ret_exits  = n_non_ret_exits,
  n_hires          = n_hires,
  HC_end           = n_headcount_end,
  WageBill_end     = round(wage_bill_end),
  COLA_effect      = round(inflation_effect),
  Exit_savings     = round(exit_savings)
)]
print(decomp)

# ---------------------------------------------------------------------------
# 11. Actual observed period-over-period changes (context)
# ---------------------------------------------------------------------------
actual_all <- bra_hrmis_contract[
  ref_date %in% c(HIST_END, actual_years) &
    contract_type_code %in% c("perm", "fterm", "temp"),
  .(headcount = uniqueN(personnel_id),
    wage_bill  = sum(gross_salary_lcu, na.rm = TRUE)),
  by = ref_date
][order(ref_date)]
actual_all[, hc_change   := headcount - shift(headcount)]
actual_all[, wb_growth   := round((wage_bill / shift(wage_bill) - 1) * 100, 1)]

cat("\n=== Actual Observed Changes (including 2012 baseline) ===\n")
print(actual_all)
