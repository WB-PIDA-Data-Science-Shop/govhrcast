# =============================================================================
# The Gambia — Wage Bill Scenarios & 10-Year Horizon Projections
# govhrcast spielplatz — March 2026
# =============================================================================
#
# PURPOSE
# -------
# Walk through how to use govhrcast's simulation engine with Gambia-specific
# policy levers.  We use the Brazilian HRMIS teaching dataset (bra_hrmis_*)
# as a structural proxy — same levers, same workflow as you would apply to
# actual Gambian HRMIS data once it is loaded.
#
# GAMBIA POLICY CONTEXT (from TOR)
# ---------------------------------
# The Gambia Civil Service Commission faces four core fiscal pressures:
#
#  1. RETIREMENT AGE REFORM
#     The mandatory retirement age has historically been 55 (established grades)
#     and 60 (senior/professional grades).  There is policy interest in moving
#     to a unified 60 or 63 to retain experienced staff and smooth outflows.
#
#  2. WAGE BILL / COLA POLICY
#     Nominal salary increments have ranged from 0% (freeze years) to ~8% pa.
#     The IMF/World Bank fiscal framework flags 3–5% as the sustainable band.
#
#  3. RECRUITMENT & BACKFILL POLICY
#     A freeze was imposed in 2021–22; pressure to resume hiring at
#     replacement rate (1:1) or above to address service delivery gaps.
#     Option: expand to 1.5× in priority ministries only (proxied by est_id).
#
#  4. PROMOTION / REGRADING
#     A regrading exercise was proposed to align pay with ECOWAS comparators.
#     This is modelled as a promotion multiplier applied to the historical
#     promotion rate (2× = double the pace; 3× = one-time regrading burst).
#
# SCRIPT STRUCTURE
# ----------------
#  SECTION 0 — Data preparation
#  SECTION 1 — Policy templates (building blocks)
#  SECTION 2 — Single-scenario horizon: baseline (60, 3% COLA, full backfill)
#  SECTION 3 — Direct comparisons: two named scenarios side-by-side
#  SECTION 4 — Scenario matrix: 4 × 4 grid (retirement age × COLA, 16 runs)
#  SECTION 5 — Full policy matrix: all 4 axes (256 scenarios, ~5–10 min)
#  SECTION 6 — Launch Shiny dashboard
#
# HOW TO RUN
# ----------
# Source section by section (Ctrl+Enter lines) to understand each step, or
# source the whole file for the full analysis.  Section 5 takes ~5–10 minutes.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
devtools::load_all(quiet = TRUE)


# =============================================================================
# SECTION 0: Data preparation
# =============================================================================
# Using bra_hrmis_* (built-in teaching dataset) as a structural proxy.
# When you have real Gambian HRMIS data, replace this block with your loader.

REF_DATE <- as.Date("2015-09-01")   # Start of projection horizon

# Keep relevant contract types across the full panel (needed for status_quo hiring)
ct_panel <- bra_hrmis_contract[
  contract_type_code %in% c("perm", "fterm", "temp")
]
pt_panel <- bra_hrmis_personnel[status == "active"]

# Single-snapshot base data for the direct simulate_horizon() calls
ct_base <- ct_panel[ref_date == REF_DATE]
pt_base <- pt_panel[ref_date == REF_DATE]

# Drop the panel ref_date column so simulate_horizon() sees a clean snapshot.
# Age and tenure will be computed internally from birth_date + contract history.
pt_base <- pt_base[, !"ref_date", with = FALSE]
pt_base <- unique(pt_base, by = "personnel_id")

# Salary scales — built from the REF_DATE snapshot only.
#   est_level  (salary_scale_est):   one row per est_id   → used by hiring module
#   grade_level (salary_scale_grade): one row per (est_id, paygrade) → used by movement module
# Building from ct_base (single snapshot) guarantees unique keys with no
# cross-period averaging.  The full panel is only used in Sections 4–5 where
# generate_scenario_matrix() needs historical exit rates.
salary_scale_est <- ct_base[
  !is.na(gross_salary_lcu),
  .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
  keyby = est_id
]
salary_scale_grade <- ct_base[
  !is.na(paygrade) & !is.na(gross_salary_lcu),
  .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
  keyby = .(est_id, paygrade)
]

cat(sprintf(
  "\nBase workforce: %s personnel | %s contracts | Annual wage bill: %s LCU\n",
  format(data.table::uniqueN(pt_base$personnel_id), big.mark = ","),
  format(nrow(ct_base), big.mark = ","),
  format(round(ct_base[, sum(gross_salary_lcu, na.rm = TRUE)]), big.mark = ",")
))
cat(sprintf(
  "Paygrades present: %s\n  (Age and tenure will be computed inside simulate_horizon() from birth_date + contract history)\n\n",
  paste(sort(unique(ct_base$paygrade[!is.na(ct_base$paygrade)])), collapse = ", ")
))


# =============================================================================
# SECTION 1: Policy templates
# =============================================================================
# These are the building blocks.  Each section below modifies or selects from
# these templates to represent specific Gambia policy scenarios.

# ── Retirement policy ────────────────────────────────────────────────────────
# Gambia baseline: mandatory retirement at 60, flat pension = 50% of final salary
ret_policy_baseline <- list(
  eligibility_type    = "age_only",
  min_age             = 60,           # lever: also test 55, 58, 63
  pension_type        = "flat",
  pension_params      = list(flat_amount = 500)
)

# ── Hiring policy ─────────────────────────────────────────────────────────────
# Flow mode: each retiree opens a vacancy; replacement_rate controls backfill.
#   0   = full hiring freeze (no backfill)
#   0.5 = half of vacancies filled (austerity)
#   1.0 = one-for-one replacement (status quo)
#   1.5 = 150% (catch-up after freeze years)
hire_policy_baseline <- list(
  mode             = "flow",
  group_cols       = "est_id",
  replacement_rate = 1.0,            # lever
  salary_scale     = data.table::copy(salary_scale_est)  # est-level only: new hires have no paygrade
)

# ── Movement / promotion policy ──────────────────────────────────────────────
# promotion_multiplier scales the historical promotion rate:
#   0   = freeze all promotions
#   1.0 = historical pace (status quo)
#   2.0 = double pace (incremental regrading)
#   3.0 = triple pace (one-time regrading exercise)
move_policy_baseline <- list(
  group_cols           = c("est_id", "paygrade"),
  salary_scale         = data.table::copy(salary_scale_grade),
  salary_update_rule   = "scale",
  promotion_strategy   = "tenure",        # longest in grade promoted first
  transfer_strategy    = "reverse_tenure",
  promotion_multiplier = 1.0              # lever
)

# ── Exit (non-retirement attrition) policy ────────────────────────────────────
# fixed_rate mode: applies a flat annual attrition rate regardless of history.
# 5% represents typical civil service voluntary resignation + dismissal rate.
# Remove this policy (pass exit_policy = NULL) to model a zero-attrition world.
exit_policy_baseline <- list(
  mode          = "fixed_rate",
  fixed_rate    = 0.05,    # 5% annual non-retirement attrition
  exit_strategy = "random",
  active_types  = c("perm", "fterm", "temp"),
  exited_type   = "inactive"
)


# =============================================================================
# SECTION 2: Single-scenario 10-year horizon (baseline)
# =============================================================================
# This is the most direct use of the engine:
#   simulate_horizon() → one scenario → per-period data.table of results

cat("====  SECTION 2: Baseline 10-year horizon  ====\n")

# bra_hrmis_contract / bra_hrmis_personnel: full panel so simulate_horizon()
# has complete history for computing rates.  The salary_scale_dt (grade-level)
# is only used by the movement module; the hiring module uses its own
# hire_policy$salary_scale (est-level) and will never see the grade-level table.
baseline_result <- simulate_horizon(
  contract_dt        = bra_hrmis_contract,
  personnel_dt       = bra_hrmis_personnel,
  salary_scale_dt    = data.table::copy(salary_scale_grade),
  n_periods          = 10L,
  ref_date           = REF_DATE,
  period_unit        = "year",          # annual steps
  birth_date_col     = "birth_date",    # age computed internally from this
  retirement_policy  = ret_policy_baseline,
  exit_policy        = exit_policy_baseline,
  movement_policy    = move_policy_baseline,
  hiring_policy      = hire_policy_baseline,
  salary_growth_rate = 0.03,            # 3% annual COLA
  pension_cola_rate  = 0.02             # 2% pension COLA (different from active)
)

# Summary table: one row per year
summary_dt <- baseline_result$comparison
cat("\nBaseline scenario — key metrics by year:\n")
print(
  summary_dt[, .(
    year               = format(period_date, "%Y"),
    headcount          = n_headcount_end,
    wage_bill_end      = round(wage_bill_end),
    n_retirements      = n_exits,
    n_non_ret_exits    = n_non_ret_exits,
    n_hires,
    pension_total      = round(pension_cost_total),
    cola_effect_pct    = round(inflation_effect_pct_of_end_bill * 100, 2)
  )]
)

# Terminal-year summary
terminal <- summary_dt[period_date == max(period_date)]
cat(sprintf(
  "\nYear 10 snapshot:  headcount = %d  |  wage bill = %s  |  pension liability = %s\n\n",
  terminal$n_headcount_end,
  format(round(terminal$wage_bill_end), big.mark = ","),
  format(round(terminal$pension_cost_total), big.mark = ",")
))


# =============================================================================
# SECTION 3: Named scenario comparisons
# =============================================================================
# Run a handful of named scenarios manually with simulate_horizon() and bind
# the results — useful when you want full control over exactly what changes.

cat("====  SECTION 3: Named scenario comparisons  ====\n")

run_scenario <- function(label, ret_min_age, cola, replace_rate, promo_mult) {
  rp <- modifyList(ret_policy_baseline,  list(min_age             = ret_min_age))
  hp <- modifyList(hire_policy_baseline, list(replacement_rate    = replace_rate,
                                               salary_scale        = data.table::copy(salary_scale_est)))
  mp <- modifyList(move_policy_baseline, list(promotion_multiplier = promo_mult,
                                               salary_scale        = data.table::copy(salary_scale_grade)))

  res <- simulate_horizon(
    contract_dt        = data.table::copy(ct_base),
    personnel_dt       = data.table::copy(pt_base),
    salary_scale_dt    = data.table::copy(salary_scale_grade),
    n_periods          = 10L,
    ref_date           = REF_DATE,
    period_unit        = "year",
    birth_date_col     = "birth_date",
    retirement_policy  = rp,
    exit_policy        = exit_policy_baseline,
    movement_policy    = mp,
    hiring_policy      = hp,
    salary_growth_rate = cola,
    pension_cola_rate  = cola * 0.67   # pension COLA = 2/3 of active COLA
  )

  res$comparison[period_date == max(period_date), .(
    scenario           = label,
    ret_age            = ret_min_age,
    cola_pct           = cola * 100,
    replace_rate,
    promo_mult,
    headcount          = n_headcount_end,
    wage_bill_Y10      = round(wage_bill_end),
    pension_Y10        = round(pension_cost_total),
    total_cost_Y10     = round(wage_bill_end + pension_cost_total)
  )]
}

named_scenarios <- rbindlist(list(
  # Baseline
  run_scenario("Baseline (status quo)",     60L, 0.03, 1.0, 1.0),
  # Retire later → fewer exits → lower short-run cost but same long-run
  run_scenario("Later retirement (63)",     63L, 0.03, 1.0, 1.0),
  # Earlier retirement → faster turnover
  run_scenario("Earlier retirement (55)",   55L, 0.03, 1.0, 1.0),
  # Wage freeze
  run_scenario("Wage freeze (0% COLA)",     60L, 0.00, 1.0, 1.0),
  # High COLA
  run_scenario("High COLA (8%)",            60L, 0.08, 1.0, 1.0),
  # Hiring freeze
  run_scenario("Hiring freeze",             60L, 0.03, 0.0, 1.0),
  # Expansion hiring
  run_scenario("Expansion (150% backfill)", 60L, 0.03, 1.5, 1.0),
  # Regrading exercise
  run_scenario("Regrading (3x promo)",      60L, 0.03, 1.0, 3.0),
  # Austerity package
  run_scenario("Austerity (freeze+0COLA)",  60L, 0.00, 0.5, 0.5),
  # Reform package
  run_scenario("Reform (63+5%+expand)",     63L, 0.05, 1.5, 1.0)
))

cat("\nYear-10 comparison across named scenarios:\n")
print(named_scenarios[order(total_cost_Y10)])


# =============================================================================
# SECTION 4: Scenario matrix — retirement age × COLA (4 × 4 = 16 scenarios)
# =============================================================================
# generate_scenario_matrix() runs all combinations automatically in parallel
# and returns a long-format data.table ready for the Shiny dashboard.

cat("\n====  SECTION 4: Retirement age × COLA matrix (16 scenarios)  ====\n")

param_grid_lite <- list(
  retirement_min_age = c(55L, 58L, 60L, 63L),
  salary_growth_rate = c(0, 0.03, 0.05, 0.08)
)

results_lite <- generate_scenario_matrix(
  contract_dt          = data.table::copy(ct_panel),
  personnel_dt         = data.table::copy(pt_panel),
  salary_scale_dt      = data.table::copy(salary_scale_grade),
  param_grid           = param_grid_lite,
  n_periods            = 10L,
  ref_date             = REF_DATE,
  retirement_policy    = ret_policy_baseline,
  movement_policy      = move_policy_baseline,
  hiring_policy        = hire_policy_baseline,
  salary_growth_rate   = 0.03,
  baseline_scenario_id = 3L    # min_age=60, COLA=3% is the "policy baseline"
)

cat(sprintf(
  "Lite grid: %d scenarios × %d periods = %d rows\n",
  data.table::uniqueN(results_lite$scenario_id),
  data.table::uniqueN(results_lite$period_date),
  nrow(results_lite)
))

cat("\nYear-10 wage bill by retirement age × COLA scenario:\n")
print(
  results_lite[
    period_date == max(period_date),
    .(scenario_label,
      headcount          = n_headcount_end,
      wage_bill_end      = round(wage_bill_end),
      pension_total      = round(pension_cost_total),
      cola_effect_pct    = round(inflation_effect_pct_of_end_bill * 100, 1))
  ][order(wage_bill_end)]
)


# =============================================================================
# SECTION 5: Full policy matrix — all 4 Gambia axes (4^4 = 256 scenarios)
# =============================================================================
# NOTE: This takes ~5–10 minutes on the bra dataset.
# Comment out if you only need the lite grid.

cat("\n====  SECTION 5: Full 4-axis matrix (256 scenarios) — running...  ====\n")

param_grid_full <- list(
  retirement_min_age            = c(55L, 58L, 60L, 63L),
  salary_growth_rate            = c(0, 0.03, 0.05, 0.08),
  hiring_replacement_rate       = c(0, 0.5, 1.0, 1.5),
  movement_promotion_multiplier = c(0, 0.5, 1.0, 3.0)
)

# Identify which grid row corresponds to the policy baseline
baseline_idx <- which(
  do.call(data.table::CJ, c(param_grid_full, list(sorted = FALSE)))[,
    retirement_min_age            == 60L  &
    salary_growth_rate            == 0.03 &
    hiring_replacement_rate       == 1.0  &
    movement_promotion_multiplier == 1.0
  ]
)

results_full <- generate_scenario_matrix(
  contract_dt          = data.table::copy(ct_panel),
  personnel_dt         = data.table::copy(pt_panel),
  salary_scale_dt      = data.table::copy(salary_scale_grade),
  param_grid           = param_grid_full,
  n_periods            = 10L,
  ref_date             = REF_DATE,
  retirement_policy    = ret_policy_baseline,
  movement_policy      = move_policy_baseline,
  hiring_policy        = hire_policy_baseline,
  salary_growth_rate   = 0.03,
  baseline_scenario_id = baseline_idx
)

cat(sprintf(
  "Full grid: %d scenarios × %d periods = %d rows\n",
  data.table::uniqueN(results_full$scenario_id),
  data.table::uniqueN(results_full$period_date),
  nrow(results_full)
))

terminal_full <- results_full[period_date == max(period_date)]

cat("\nTop 5 most expensive scenarios (Year 10 total wage bill):\n")
print(
  terminal_full[order(-wage_bill_end)][1:5, .(
    scenario_label,
    headcount          = n_headcount_end,
    wage_bill_end      = round(wage_bill_end),
    pension_total      = round(pension_cost_total)
  )]
)

cat("\nTop 5 least expensive scenarios (Year 10 total wage bill):\n")
print(
  terminal_full[order(wage_bill_end)][1:5, .(
    scenario_label,
    headcount          = n_headcount_end,
    wage_bill_end      = round(wage_bill_end),
    pension_total      = round(pension_cost_total)
  )]
)

cat(sprintf(
  "\nWage-bill range across all 256 scenarios at Year 10:\n  Min: %s  |  Max: %s  |  Range: %.1f%%\n",
  format(round(terminal_full[, min(wage_bill_end)]), big.mark = ","),
  format(round(terminal_full[, max(wage_bill_end)]), big.mark = ","),
  (terminal_full[, max(wage_bill_end) / min(wage_bill_end)] - 1) * 100
))


# =============================================================================
# SECTION 6: Launch Shiny dashboard
# =============================================================================
# generate_hrcastapp() opens an interactive browser-based dashboard.
# Pass any results data.table from generate_scenario_matrix().
# Use results_lite for a fast first look; results_full for the full analysis.

cat("\n====  SECTION 6: Launching govhrcast dashboard  ====\n")

# Lite grid (fast — 16 scenarios):
# generate_hrcastapp(results_lite)

# Full grid (256 scenarios):
generate_hrcastapp(results_full)
