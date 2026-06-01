# =============================================================================
# Zambia Wage Bill Scenario Exploration
# govhrcast spielplatz — March 2026
# =============================================================================
#
# PURPOSE
# -------
# Demonstrate govhrcast's scenario simulation capabilities against the policy
# questions raised by the Zambian government.  We use the Brazilian HRMIS
# teaching dataset (bra_hrmis_*) as a structural proxy — the scenarios and
# parameter levers are the same as those we would apply to actual Zambian
# payroll data once it is loaded.
#
# ZAMBIA POLICY INTERESTS (mapped to model levers)
# -------------------------------------------------
#  1. Wage bill projection over short/medium/long horizons
#        → n_periods = 5 (short), 10 (medium), 20 (long)
#        → salary_growth_rate lever
#
#  2. Staffing categories, grades, allowances, statutory contributions
#        → group_cols = c("est_id", "paygrade") for grade-stratified analysis
#        → salary_scale_grade built from actual grade medians
#        → allowance_lcu column present in contract data
#
#  3. Recruitment variations
#        → hiring_policy: mode = "status_quo" for historical rate replication
#        → mode = "flow" (retirement-linked backfill)
#        → replacement_rate lever (0 = freeze, 1 = full backfill, 2 = expansion)
#
#  4. Across-the-board salary increases / selective pay reforms
#        → salary_growth_rate lever (uniform COLA)
#        → salary_scale override (grade-selective bump via modified salary_scale_grade)
#
#  5. Restructuring of allowances
#        → modify allowance_lcu in the salary scale before simulation
#        → tracked via gross_salary_lcu = base_salary_lcu + allowance_lcu
#
#  6. Hiring freezes
#        → hiring_policy = NULL  OR  replacement_rate = 0
#
#  7. Promotion policies / regrading exercises
#        → movement_policy with group_cols = c("est_id", "paygrade")
#        → promotion_multiplier lever
#
#  8. Policy reforms affecting establishment size
#        → hiring_policy mode = "stock" with stock_targets
#
# NEW OUTPUT SCHEMA (govhrcast 0.0.0.9000+)
# ------------------------------------------
# simulate_horizon() summary_dt now produces per-period columns:
#   period_date                         — Date at start of each period
#   wage_bill_start, wage_bill_end      — payroll at start/end of period
#   n_headcount_start, n_headcount_end  — active (non-pensioner) contract rows
#   n_exits, exit_savings               — retirement count & salary mass removed
#   pension_cost_new, pension_cost_total — new & cumulative pension obligations
#   n_promotions, n_transfers           — movement counts
#   promotion_effect, transfer_effect   — net salary change per mover type
#   n_hires, hiring_effect              — new hire count & total new-hire salary
#   inflation_effect                    — payroll increment from COLA
#   *_pct_of_end_bill                   — each effect as share of wage_bill_end
#
# (Old columns base_bill, total_wage_bill, n_active, total_change,
#  year (integer), exit_savings_pct, promotion_effect_pct, etc. are REMOVED.)
#
# STATUS_QUO HIRING
# -----------------
# Pass the full panel (all ref_dates) as contract_dt / personnel_dt.
# simulate_horizon() automatically captures the panel before stripping ref_date,
# so status_quo hiring estimation works without any manual data preparation:
#
#   hire_policy_status_quo <- list(
#     mode         = "status_quo",
#     group_cols   = "est_id",
#     rate_mult    = 1,            # 1 = replicate history; 2 = double the rate
#     salary_scale = salary_scale_est
#   )
#
# =============================================================================

suppressPackageStartupMessages(library(data.table))
devtools::load_all(quiet = TRUE)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 0: Data preparation
# ─────────────────────────────────────────────────────────────────────────────

REF_DATE <- as.Date("2015-09-01")   # project from 2015-09-01 onwards

# Full panels — keep ALL ref_dates; simulate_horizon() strips ref_date internally
# and uses the panel for status_quo hiring rate estimation.
bra_ct_panel <- bra_hrmis_contract[
  contract_type_code %in% c("perm", "fterm", "temp")
]
bra_pt_panel <- bra_hrmis_personnel[status == "active"]

# ── Base snapshot (REF_DATE only) — for age/tenure derivation ────────────────
ct_base <- bra_ct_panel[ref_date == REF_DATE]
pt_raw  <- bra_pt_panel[ref_date == REF_DATE]

pt_raw[, age := as.integer(difftime(REF_DATE, birth_date, units = "days") / 365.25)]

tenure_dt <- ct_base[,
  .(tenure_years = max(
      as.integer(difftime(REF_DATE, start_date, units = "days") / 365.25),
      na.rm = TRUE
    )),
  by = personnel_id
]
pt_base <- merge(pt_raw[, !"ref_date", with = FALSE], tenure_dt,
                 by = "personnel_id", all.x = TRUE)
pt_base[is.na(tenure_years), tenure_years := 0L]
pt_base <- unique(pt_base, by = "personnel_id")

# ── Add age/tenure columns to the full panel ──────────────────────────────
# (simulate_horizon needs these on each snapshot row)
bra_pt_panel[, age := as.integer(difftime(ref_date, birth_date, units = "days") / 365.25)]
bra_pt_panel[is.na(age), age := 0L]

# For tenure we use the REF_DATE baseline tenure and add elapsed years per snapshot
bra_pt_panel <- merge(
  bra_pt_panel[, !"tenure_years", with = FALSE],
  tenure_dt,
  by = "personnel_id", all.x = TRUE
)
bra_pt_panel[is.na(tenure_years), tenure_years := 0L]
bra_pt_panel[, tenure_years := tenure_years +
               as.integer(difftime(ref_date, REF_DATE, units = "days") / 365.25)]

# ── Salary scales ──────────────────────────────────────────────────────────

# By establishment (for est_id-based hiring / transfers)
salary_scale_est <- ct_base[,
  .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
  keyby = est_id
]

# By paygrade (for promotion / grade-stratified movement analysis)
salary_scale_grade <- ct_base[
  !is.na(paygrade),
  .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
  keyby = paygrade
]

# By establishment × paygrade (for two-dimensional movement analysis)
salary_scale_est_grade <- ct_base[
  !is.na(paygrade),
  .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
  keyby = .(est_id, paygrade)
]

cat(sprintf(
  "Base workforce: %d personnel | %d contracts | annual wage bill: %s LCU\n",
  nrow(pt_base), nrow(ct_base),
  format(round(ct_base[, sum(gross_salary_lcu, na.rm = TRUE)]), big.mark = ",")
))
cat(sprintf(
  "Age 60+: %d eligible for retirement | Paygrades: %s\n",
  pt_base[age >= 60, .N],
  paste(sort(unique(ct_base$paygrade[!is.na(ct_base$paygrade)])), collapse = ", ")
))

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 1: Baseline — status quo projection (10 years, 3 % COLA)
# ─────────────────────────────────────────────────────────────────────────────
# Historical hiring rates are estimated automatically from the panel.
# This is the true status-quo: retirements occur, and hires replace them at the
# observed historical rate.

cat("\n====  SECTION 1: Baseline — status quo projection + 3 % COLA  ====\n")

ret_policy_base <- list(
  eligibility_type = "age_only",
  min_age          = 60,
  pension_type     = "flat",
  pension_params   = list(flat_amount = 500)
)

hire_policy_status_quo <- list(
  mode         = "status_quo",
  group_cols   = c("est_id", "paygrade"),
  rate_mult    = 1,
  salary_scale = data.table::copy(salary_scale_est_grade)
)

movement_status_quo <- list(
  group_cols = c("est_id", "paygrade"),
  salary_scale = data.table::copy(salary_scale_est_grade),
  salary_update_rule = "scale",
  promotion_strategy = "tenure",
  transfer_strategy = "reverse_tenure"
)

res_baseline <- simulate_horizon(
  contract_dt        = data.table::copy(bra_ct_panel),
  personnel_dt       = data.table::copy(bra_pt_panel),
  salary_scale_dt    = data.table::copy(salary_scale_est),
  n_periods          = 10L,
  retirement_policy  = ret_policy_base,
  movement_policy    = movement_status_quo,
  hiring_policy      = hire_policy_status_quo,
  salary_growth_rate = 0.03,
  ref_date           = REF_DATE,
  age_col            = "age",
  tenure_col         = "tenure_years"
)

# res_baseline$summary_dt[, .(
#   period_date,
#   n_headcount_end,
#   n_exits,
#   wage_bill_start    = round(wage_bill_start),
#   exit_savings       = round(exit_savings),
#   pension_cost_total = round(pension_cost_total),
#   inflation_effect   = round(inflation_effect),
#   wage_bill_end      = round(wage_bill_end),
#   yoy_pct            = round((wage_bill_end / shift(wage_bill_end, 1) - 1) * 100, 1)
# )]

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 2: Hiring freeze vs full backfill vs mass recruitment
# ─────────────────────────────────────────────────────────────────────────────
# Scenario 2a: Hiring freeze (no replacements, no COLA)
# Scenario 2b: Hiring freeze (no replacements, 3 % COLA)
# Scenario 2c: Status-quo hiring (historical rate, 3 % COLA)
# Scenario 2d: Full backfill (1:1 replacement of every retiree, 3 % COLA)
# Scenario 2e: Mass recruitment (2× flow replacement rate, 3 % COLA)

cat("\n====  SECTION 2: Recruitment policy variations  ====\n")

hire_policy_full <- list(
  mode             = "flow",
  group_cols       = "est_id",
  replacement_rate = 1.0,
  salary_scale     = data.table::copy(salary_scale_est)
)

hire_policy_mass <- list(
  mode             = "flow",
  group_cols       = "est_id",
  replacement_rate = 2.0,
  salary_scale     = data.table::copy(salary_scale_est)
)

run_s2 <- function(hire_pol, label, cola = 0.03) {
  res <- suppressWarnings(simulate_horizon(
    contract_dt        = data.table::copy(bra_ct_panel),
    personnel_dt       = data.table::copy(bra_pt_panel),
    salary_scale_dt    = data.table::copy(salary_scale_est),
    n_periods          = 10L,
    retirement_policy  = ret_policy_base,
    movement_policy    = movement_status_quo,
    hiring_policy      = hire_pol,
    salary_growth_rate = cola,
    ref_date           = REF_DATE,
    age_col            = "age",
    tenure_col         = "tenure_years"
  ))
  res$summary_dt[, scenario := label]
  res$summary_dt
}

final_date_10yr <- .add_years(REF_DATE, 9L)   # 10th period start date

s2_results <- rbindlist(list(
  run_s2(NULL,                 "freeze (0 COLA)",     cola = 0),
  run_s2(NULL,                 "freeze (3% COLA)",    cola = 0.03),
  run_s2(hire_policy_status_quo, "status quo hiring", cola = 0.03),
  run_s2(hire_policy_full,     "full backfill",       cola = 0.03),
  run_s2(hire_policy_mass,     "mass recruitment 2x", cola = 0.03)
))

cat("\n10-year results by recruitment scenario (final period):\n")
s2_results[period_date == final_date_10yr, .(
  scenario,
  n_headcount_end,
  n_hires,
  wage_bill_end              = round(wage_bill_end),
  hiring_effect_pct_of_end_bill = round(hiring_effect_pct_of_end_bill * 100, 1)
)]

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 3: Across-the-board salary increases vs selective grade-based pay reform
# ─────────────────────────────────────────────────────────────────────────────
# Scenario 3a: Moderate COLA (3 %)
# Scenario 3b: Selective reform — lower grades +8 %, upper grades +2 %
# Scenario 3c: Across-the-board 10 % rise

cat("\n====  SECTION 3: Salary increase scenarios  ====\n")

# Selective uplift: lower grades (A, B, C) get 8 %, upper grades get 2 %
salary_scale_selective <- data.table::copy(salary_scale_grade)
salary_scale_selective[paygrade %in% c("A", "B", "C"),
                       gross_salary_lcu := gross_salary_lcu * 1.08]
salary_scale_selective[!paygrade %in% c("A", "B", "C"),
                       gross_salary_lcu := gross_salary_lcu * 1.02]

ct_selective <- data.table::copy(ct_base)
ct_selective <- merge(
  ct_selective[, !"gross_salary_lcu", with = FALSE],
  salary_scale_selective[, .(paygrade, new_salary = gross_salary_lcu)],
  by = "paygrade", all.x = TRUE
)
ct_selective[!is.na(new_salary),  gross_salary_lcu := new_salary]
ct_selective[is.na(new_salary),   gross_salary_lcu := ct_base$gross_salary_lcu[
  match(ct_selective$contract_id[is.na(ct_selective$new_salary)], ct_base$contract_id)
]]
ct_selective[, new_salary := NULL]

# Build a full panel with the selective salary applied at the base snapshot only
bra_ct_selective_panel <- data.table::copy(bra_ct_panel)
bra_ct_selective_panel[
  ref_date == REF_DATE & contract_id %in% ct_selective$contract_id,
  gross_salary_lcu := ct_selective$gross_salary_lcu[
    match(bra_ct_selective_panel$contract_id[
            bra_ct_selective_panel$ref_date == REF_DATE &
              bra_ct_selective_panel$contract_id %in% ct_selective$contract_id
          ],
          ct_selective$contract_id)
  ]
]

run_s3 <- function(ct_panel, ss, label, cola = 0) {
  res <- simulate_horizon(
    contract_dt        = data.table::copy(ct_panel),
    personnel_dt       = data.table::copy(bra_pt_panel),
    salary_scale_dt    = data.table::copy(ss),
    n_periods          = 10L,
    retirement_policy  = ret_policy_base,
    movement_policy    = movement_status_quo,
    hiring_policy      = hire_policy_status_quo,
    salary_growth_rate = cola,
    ref_date           = REF_DATE,
    age_col            = "age",
    tenure_col         = "tenure_years"
  )
  res$summary_dt[, scenario := label]
  res$summary_dt
}

date_yr1  <- REF_DATE
date_yr10 <- .add_years(REF_DATE, 9L)

s3_results <- rbindlist(list(
  run_s3(bra_ct_panel,           salary_scale_est_grade, "3% uniform COLA",                   cola = 0.03),
  run_s3(bra_ct_panel,           salary_scale_est_grade, "10% across-the-board",               cola = 0.10),
  run_s3(bra_ct_selective_panel, salary_scale_est_grade, "selective: lower +8%, upper +2%",    cola = 0.03)
))

cat("\nYear-1 and Year-10 wage bill by salary scenario:\n")
s3_results[period_date %in% c(date_yr1, date_yr10), .(
  scenario,
  period_date,
  wage_bill_start            = round(wage_bill_start),
  wage_bill_end              = round(wage_bill_end),
  inflation_effect_pct_of_end_bill = round(inflation_effect_pct_of_end_bill * 100, 1)
)][order(period_date, scenario)]

# # ─────────────────────────────────────────────────────────────────────────────
# # SECTION 4: Hiring freeze — detailed decomposition (5 years, no COLA)
# # ─────────────────────────────────────────────────────────────────────────────

# cat("\n====  SECTION 4: Hiring freeze — detailed decomposition  ====\n")

# res_freeze <- simulate_horizon(
#   contract_dt        = data.table::copy(bra_ct_panel),
#   personnel_dt       = data.table::copy(bra_pt_panel),
#   salary_scale_dt    = data.table::copy(salary_scale_est),
#   n_periods          = 5L,
#   retirement_policy  = ret_policy_base,
#   movement_policy    = movement_status_quo,
#   hiring_policy      = hire_policy_status_quo,
#   salary_growth_rate = 0,
#   ref_date           = REF_DATE,
#   age_col            = "age",
#   tenure_col         = "tenure_years"
# )

# res_freeze$summary_dt[, .(
#   period_date,
#   n_headcount_start,
#   n_headcount_end,
#   n_exits,
#   wage_bill_end      = round(wage_bill_end),
#   exit_savings       = round(exit_savings),
#   pension_cost_new   = round(pension_cost_new),
#   pension_cost_total = round(pension_cost_total),
#   cumulative_exit_saving = round(cumsum(exit_savings))
# )]

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 5: Promotion policy / regrading — by establishment × paygrade
# ─────────────────────────────────────────────────────────────────────────────

# cat("\n====  SECTION 5: Promotion and regrading policy  ====\n")

# make_move_policy <- function(promo_mult = 1.0, transfer_mult = 0.5) {
#   list(
#     group_cols           = c("est_id", "paygrade"),
#     salary_scale         = data.table::copy(salary_scale_est_grade),
#     promotion_multiplier = promo_mult,
#     transfer_multiplier  = transfer_mult,
#     promotion_strategy   = "tenure",
#     transfer_strategy    = "random",
#     salary_update_rule   = "scale"
#   )
# }

# date_yr5 <- .add_years(REF_DATE, 4L)

# run_s5 <- function(move_pol, label) {
#   res <- simulate_horizon(
#     contract_dt        = data.table::copy(bra_ct_panel),
#     personnel_dt       = data.table::copy(bra_pt_panel),
#     salary_scale_dt    = data.table::copy(salary_scale_est),  # est_id key matches group_cols
#     n_periods          = 5L,
#     retirement_policy  = ret_policy_base,
#     movement_policy    = move_pol,
#     hiring_policy      = hire_policy_status_quo,
#     salary_growth_rate = 0.03,
#     ref_date           = REF_DATE,
#     age_col            = "age",
#     tenure_col         = "tenure_years"
#   )
#   res$summary_dt[, scenario := label]
#   res$summary_dt
# }

# s5_results <- rbindlist(list(
#   run_s5(NULL,                  "no promotions (policy = NULL)"),
#   run_s5(make_move_policy(0),   "promotion freeze (multiplier = 0)"),
#   run_s5(make_move_policy(1.0), "normal pace (multiplier = 1x)"),
#   run_s5(make_move_policy(3.0), "regrading exercise (multiplier = 3x)")
# ))

# cat("\nYear-5 wage bill by promotion scenario:\n")
# s5_results[period_date == date_yr5, .(
#   scenario,
#   n_promotions,
#   n_transfers,
#   wage_bill_end                     = round(wage_bill_end),
#   promotion_effect                  = round(promotion_effect),
#   transfer_effect                   = round(transfer_effect),
#   promotion_effect_pct_of_end_bill  = round(promotion_effect_pct_of_end_bill * 100, 1),
#   transfer_effect_pct_of_end_bill   = round(transfer_effect_pct_of_end_bill * 100, 1)
# )][order(scenario)]

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 6: Establishment size reform — stock-based headcount targets
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====  SECTION 6: Establishment size reform (stock targets)  ====\n")

current_stock <- ct_base[, .(current_n = .N), keyby = est_id]

stock_targets_reduce <- current_stock[, .(
  est_id,
  target_stock = as.integer(ceiling(current_n * 0.90))
)]

top5_est <- current_stock[order(-current_n)][1:5, est_id]
stock_targets_expand <- current_stock[, .(
  est_id,
  target_stock = as.integer(ceiling(
    data.table::fifelse(est_id %in% top5_est, current_n * 1.20, current_n * 1.0)
  ))
)]

make_stock_hire_policy <- function(targets) {
  list(
    mode          = "stock",
    group_cols    = "est_id",
    stock_targets = targets,
    salary_scale  = data.table::copy(salary_scale_est)
  )
}

run_s6 <- function(hire_pol, label) {
  res <- suppressWarnings(simulate_horizon(
    contract_dt        = data.table::copy(bra_ct_panel),
    personnel_dt       = data.table::copy(bra_pt_panel),
    salary_scale_dt    = data.table::copy(salary_scale_est),
    n_periods          = 10L,
    retirement_policy  = ret_policy_base,
    movement_policy    = movement_status_quo,
    hiring_policy      = hire_pol,
    salary_growth_rate = 0.03,
    ref_date           = REF_DATE,
    age_col            = "age",
    tenure_col         = "tenure_years"
  ))
  res$summary_dt[, scenario := label]
  res$summary_dt
}

s6_results <- rbindlist(list(
  run_s6(hire_policy_status_quo,                       "status quo hiring"),
  run_s6(make_stock_hire_policy(stock_targets_reduce), "10% reduction"),
  run_s6(make_stock_hire_policy(stock_targets_expand), "20% expansion (top 5 ests)")
))

# cat("\nYear-5 wage bill by establishment size scenario:\n")
# s6_results[period_date == date_yr5, .(
#   scenario,
#   n_headcount_end,
#   n_hires,
#   wage_bill_end                  = round(wage_bill_end),
#   hiring_effect                  = round(hiring_effect),
#   hiring_effect_pct_of_end_bill  = round(hiring_effect_pct_of_end_bill * 100, 1)
# )][order(scenario)]

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 7: Allowance restructuring
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====  SECTION 7: Allowance restructuring  ====\n")

cat("Current allowance share of gross salary:\n")
ct_base[, .(
  mean_base  = round(mean(base_salary_lcu,   na.rm = TRUE)),
  mean_allow = round(mean(allowance_lcu,     na.rm = TRUE)),
  mean_gross = round(mean(gross_salary_lcu,  na.rm = TRUE)),
  allow_pct  = round(mean(allowance_lcu / gross_salary_lcu, na.rm = TRUE) * 100, 1)
)]

ct_allow_capped <- data.table::copy(ct_base)
ct_allow_capped[, allowance_cap  := base_salary_lcu * 0.30]
ct_allow_capped[, allowance_lcu  := pmin(allowance_lcu, allowance_cap, na.rm = TRUE)]
ct_allow_capped[, gross_salary_lcu := base_salary_lcu + allowance_lcu]
ct_allow_capped[, allowance_cap  := NULL]

ct_no_allow <- data.table::copy(ct_base)
ct_no_allow[, allowance_lcu    := 0]
ct_no_allow[, gross_salary_lcu := base_salary_lcu]

# Build allow-capped and no-allow panels (only the REF_DATE snapshot is modified)
make_allow_panel <- function(ct_snap) {
  panel_out <- data.table::copy(bra_ct_panel)
  panel_out[ref_date == REF_DATE, gross_salary_lcu :=
    ct_snap$gross_salary_lcu[match(panel_out$contract_id[panel_out$ref_date == REF_DATE],
                                   ct_snap$contract_id)]]
  panel_out
}

bra_ct_allow_capped_panel <- make_allow_panel(ct_allow_capped)
bra_ct_no_allow_panel     <- make_allow_panel(ct_no_allow)

run_s7 <- function(ct_panel, label) {
  ss <- ct_panel[ref_date == REF_DATE,
                 .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
                 keyby = c("est_id", "paygrade")]
  res <- simulate_horizon(
    contract_dt        = data.table::copy(ct_panel),
    personnel_dt       = data.table::copy(bra_pt_panel),
    salary_scale_dt    = data.table::copy(ss),
    n_periods          = 10L,
    retirement_policy  = ret_policy_base,
    movement_policy    = movement_status_quo,
    hiring_policy      = list(
      mode         = "status_quo",
      group_cols   = c("est_id", "paygrade"),
      rate_mult    = 1,
      salary_scale = ss
    ),
    salary_growth_rate = 0.03,
    ref_date           = REF_DATE,
    age_col            = "age",
    tenure_col         = "tenure_years"
  )
  res$summary_dt[, scenario := label]
  res$summary_dt
}

s7_results <- rbindlist(list(
  run_s7(bra_ct_panel,             "current allowances"),
  run_s7(bra_ct_allow_capped_panel, "capped at 30% of base"),
  run_s7(bra_ct_no_allow_panel,    "allowances eliminated")
))

# cat("\nYear-1 and Year-5 wage bill by allowance scenario:\n")
# s7_results[period_date %in% c(date_yr1, date_yr5), .(
#   scenario,
#   period_date,
#   wage_bill_start  = round(wage_bill_start),
#   wage_bill_end    = round(wage_bill_end),
#   inflation_effect = round(inflation_effect)
# )][order(period_date, scenario)]

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 8: Combined policy scenario — "fiscal reform package"
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====  SECTION 8: Combined fiscal reform package  ====\n")

ret_policy_early <- list(
  eligibility_type = "age_only",
  min_age          = 58,
  pension_type     = "flat",
  pension_params   = list(flat_amount = 500)
)

hire_policy_backfill <- list(
  mode             = "flow",
  group_cols       = "est_id",
  replacement_rate = 1.0,
  salary_scale     = data.table::copy(salary_scale_est)
)

ss_reform <- ct_allow_capped[,
  .(gross_salary_lcu = min(gross_salary_lcu, na.rm = TRUE)),
  keyby = c("est_id", "paygrade")
]

res_reform <- suppressWarnings(simulate_horizon(
  contract_dt        = data.table::copy(bra_ct_allow_capped_panel),
  personnel_dt       = data.table::copy(bra_pt_panel),
  salary_scale_dt    = data.table::copy(ss_reform),
  n_periods          = 10L,
  retirement_policy  = ret_policy_early,
  movement_policy    = movement_status_quo,
  hiring_policy      = hire_policy_backfill,
  salary_growth_rate = 0.05,
  ref_date           = REF_DATE,
  age_col            = "age",
  tenure_col         = "tenure_years"
))

res_status_quo <- simulate_horizon(
  contract_dt        = data.table::copy(bra_ct_panel),
  personnel_dt       = data.table::copy(bra_pt_panel),
  salary_scale_dt    = data.table::copy(salary_scale_est),
  n_periods          = 10L,
  retirement_policy  = ret_policy_base,
  movement_policy    = movement_status_quo,
  hiring_policy      = hire_policy_status_quo,
  salary_growth_rate = 0.03,
  ref_date           = REF_DATE,
  age_col            = "age",
  tenure_col         = "tenure_years"
)

comparison <- rbind(
  res_reform$summary_dt[,
    scenario := "reform (early retire + backfill + 5% COLA + allow cap)"],
  res_status_quo$summary_dt[,
    scenario := "status quo (retire 60 + status_quo hire + 3% COLA)"]
)

# cat("\nFiscal reform package vs status quo — 10-year trajectory:\n")
# comparison[, .(
#   scenario,
#   period_date,
#   n_headcount_end,
#   n_exits,
#   n_hires,
#   pension_cost_total = round(pension_cost_total),
#   wage_bill_end      = round(wage_bill_end)
# )][order(scenario, period_date)]

# # ─────────────────────────────────────────────────────────────────────────────
# # SECTION 9: Horizon sensitivity — 5, 10, 20 years
# # ─────────────────────────────────────────────────────────────────────────────

# cat("\n====  SECTION 9: Projection horizon sensitivity  ====\n")

# run_horizon <- function(n_per, label) {
#   res <- suppressWarnings(simulate_horizon(
#     contract_dt        = data.table::copy(bra_ct_panel),
#     personnel_dt       = data.table::copy(bra_pt_panel),
#     salary_scale_dt    = data.table::copy(salary_scale_est),
#     n_periods          = n_per,
#     retirement_policy  = ret_policy_base,
#     movement_policy    = NULL,
#     hiring_policy      = hire_policy_full,
#     salary_growth_rate = 0.03,
#     ref_date           = REF_DATE,
#     age_col            = "age",
#     tenure_col         = "tenure_years"
#   ))
#   dt <- res$summary_dt
#   dt[period_date == max(period_date), .(
#     horizon          = label,
#     final_period     = period_date,
#     wb_year1         = round(dt$wage_bill_end[1]),
#     wb_final         = round(wage_bill_end),
#     cumul_growth_pct = round((wage_bill_end / dt$wage_bill_end[1] - 1) * 100, 1)
#   )]
# }

# rbindlist(list(
#   run_horizon(5L,  "short  (5 yr)"),
#   run_horizon(10L, "medium (10 yr)"),
#   run_horizon(20L, "long   (20 yr)")
# ))

# # ─────────────────────────────────────────────────────────────────────────────
# # SECTION 10: Full generate_scenario_matrix() — dense scenario grid
# # ─────────────────────────────────────────────────────────────────────────────

# if (FALSE) {
#   param_grid <- list(
#     salary_growth_rate            = c(0, 0.03, 0.05, 0.08, 0.10),
#     retirement_min_age            = c(55L, 58L, 60L, 65L),
#     hiring_replacement_rate       = c(0, 0.5, 1.0, 1.5, 2.0),
#     movement_promotion_multiplier = c(0, 0.5, 1.0, 2.0, 3.0)
#   )

#   results_matrix <- generate_scenario_matrix(
#     contract_dt        = bra_ct_panel,
#     personnel_dt       = bra_pt_panel,
#     salary_scale_dt    = salary_scale_est,
#     param_grid         = param_grid,
#     n_periods          = 10L,
#     retirement_policy  = ret_policy_base,
#     movement_policy    = make_move_policy(1.0),
#     hiring_policy      = hire_policy_full,
#     salary_growth_rate = 0.03,
#     ref_date           = REF_DATE
#   )

#   cat("Scenario matrix rows:", nrow(results_matrix), "\n")
#   cat("Scenarios:", data.table::uniqueN(results_matrix$scenario_id), "\n")

#   results_matrix[
#     period_date == max(period_date),
#     .(scenario_label,
#       wage_bill_end = round(wage_bill_end),
#       exit_savings_pct_of_end_bill      = round(exit_savings_pct_of_end_bill * 100, 1),
#       hiring_effect_pct_of_end_bill     = round(hiring_effect_pct_of_end_bill * 100, 1),
#       inflation_effect_pct_of_end_bill  = round(inflation_effect_pct_of_end_bill * 100, 1))
#   ][order(-wage_bill_end)]
# }

# # govhrcast spielplatz — March 2026
# # =============================================================================
# #
# # PURPOSE
# # -------
# # Demonstrate govhrcast's scenario simulation capabilities against the policy
# # questions raised by the Zambian government.  We use the Brazilian HRMIS
# # teaching dataset (bra_hrmis_*) as a structural proxy — the scenarios and
# # parameter levers are the same as those we would apply to actual Zambian
# # payroll data once it is loaded.
# #
# # ZAMBIA POLICY INTERESTS (mapped to model levers)
# # -------------------------------------------------
# #  1. Wage bill projection over short/medium/long horizons
# #        → n_periods = 5 (short), 10 (medium), 20 (long)
# #        → salary_growth_rate lever
# #
# #  2. Staffing categories, grades, allowances, statutory contributions
# #        → group_cols = c("est_id", "paygrade") for grade-stratified analysis
# #        → salary_scale_grade built from actual grade medians
# #        → allowance_lcu column present in contract data
# #
# #  3. Recruitment variations
# #        → hiring_policy: replacement_rate lever (0 = freeze, 1 = full backfill, 2 = expansion)
# #        → mode = "flow" (retirement-linked backfill)
# #
# #  4. Across-the-board salary increases / selective pay reforms
# #        → salary_growth_rate lever (uniform COLA)
# #        → salary_scale override (grade-selective bump via modified salary_scale_grade)
# #
# #  5. Restructuring of allowances
# #        → modify allowance_lcu in the salary scale before simulation
# #        → tracked via gross_salary_lcu = base_salary_lcu + allowance_lcu
# #
# #  6. Hiring freezes
# #        → hiring_policy = NULL  OR  replacement_rate = 0
# #
# #  7. Promotion policies / regrading exercises
# #        → movement_policy with group_cols = c("est_id", "paygrade")
# #        → promotion_multiplier lever
# #
# #  8. Policy reforms affecting establishment size
# #        → hiring_policy mode = "stock" with stock_targets
# #
# # NEW OUTPUT SCHEMA (govhrcast 0.0.0.9000+)
# # ------------------------------------------
# # simulate_horizon() summary_dt now produces per-period columns:
# #   wage_bill_start, wage_bill_end      — payroll at start/end of period
# #   n_headcount_start, n_headcount_end  — contract rows at start/end
# #   n_exits, exit_savings               — retirement count & salary mass removed
# #   pension_cost_new, pension_cost_total — new & cumulative pension obligations
# #   n_promotions, n_transfers           — movement counts
# #   promotion_effect, transfer_effect   — net salary change per mover type
# #   n_hires, hiring_effect              — new hire count & total new-hire salary
# #   inflation_effect                    — payroll increment from COLA
# #   *_pct_of_end_bill                   — each effect as share of wage_bill_end
# #
# # (Old columns base_bill, total_wage_bill, n_active, total_change,
# #  exit_savings_pct, promotion_effect_pct, hiring_effect_pct,
# #  inflation_effect_pct are REMOVED.)
# #
# # =============================================================================
