# =============================================================================
# The Gambia — Wage Bill Scenarios & 10-Year Horizon Projections
# govhrcast spielplatz — March 2026
# =============================================================================
#
# PURPOSE
# -------
# Demonstrates the full govhrcast simulation workflow for Gambia-specific
# policy levers.  Uses the Brazilian HRMIS teaching dataset (bra_hrmis_*) as a
# structural proxy — the same workflow applies once real Gambian HRMIS data is
# loaded.
#
# GAMBIA POLICY CONTEXT (from TOR)
# ---------------------------------
#  1. RETIREMENT AGE REFORM       — test 55 / 58 / 60 / 63 mandatory age
#  2. WAGE BILL / COLA POLICY     — test 0% / 3% / 5% / 8% annual increment
#  3. RECRUITMENT & BACKFILL      — test 0 / 0.5× / 1× / 1.5× replacement
#  4. PROMOTION / REGRADING       — test 0 / 0.5× / 1× / 3× historical rate
#
# WHAT IS NEW (March 2026)
# ------------------------
# simulate_horizon() now accepts:
#   scenario_name  — human-readable label stamped onto $comparison as
#                    scenario_id + scenario_label (Shiny-ready)
#   is_baseline    — logical flag; the baseline row is highlighted in the
#                    Shiny comparator
#
# generate_hrcastapp() accepts either:
#   (a) a single horizon object     → single-scenario view
#   (b) a flat data.table from rbindlist(lapply(..., `[[`, "comparison"))
#       → multi-scenario comparator view
#   (c) output of generate_scenario_matrix() → full grid explorer
#
# SCRIPT STRUCTURE
# ----------------
#  SECTION 0 — Data preparation
#  SECTION 1 — Policy templates (building blocks)
#  SECTION 2 — Baseline horizon: a single named scenario
#  SECTION 3 — Named scenario comparisons → flat table → Shiny
#  SECTION 4 — Scenario matrix: retirement age × COLA (16 scenarios)
#  SECTION 5 — Full policy matrix: all 4 axes (256 scenarios, ~5–10 min)
#  SECTION 6 — Launch Shiny dashboard
#
# RUN SECTION BY SECTION
# ----------------------
# Sections 0–3 run in under a minute.
# Section 4 takes ~30 seconds.  Section 5 takes ~5–10 minutes.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
devtools::load_all(quiet = TRUE)


# =============================================================================
# SECTION 0: Data preparation
# =============================================================================

REF_DATE <- as.Date("2015-09-01")   # Start of projection horizon

# Full panel — needed for simulate_horizon() to estimate the movement baseline
# (requires >= 2 ref_date snapshots) and for generate_scenario_matrix().
ct_panel <- bra_hrmis_contract[contract_type_code %in% c("perm", "fterm", "temp")]
pt_panel <- bra_hrmis_personnel[status == "active"]

# Single REF_DATE snapshot — used for the manual Section 3 scenarios.
# simulate_horizon() will still see the full panel for movement baseline
# estimation via the ref_date column; we pass bra_hrmis_contract (full panel)
# for Section 2 and data.table::copy(ct_base) for Section 3.
ct_base  <- ct_panel[ref_date == REF_DATE]
pt_base  <- unique(pt_panel[ref_date == REF_DATE][, !"ref_date", with = FALSE],
                   by = "personnel_id")

# Salary scales built from the REF_DATE snapshot (unique keys guaranteed).
#   salary_scale_est   — one row per est_id         → hiring module
#   salary_scale_grade — one row per (est_id, paygrade) → movement module
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
  "\nBase workforce : %s personnel | %s contracts\n",
  format(data.table::uniqueN(pt_base$personnel_id), big.mark = ","),
  format(nrow(ct_base), big.mark = ",")
))
cat(sprintf(
  "Annual wage bill: %s LCU\n",
  format(round(ct_base[, sum(gross_salary_lcu, na.rm = TRUE)]), big.mark = ",")
))
cat(sprintf(
  "Panel snapshots : %d  (%s → %s)\n\n",
  data.table::uniqueN(ct_panel$ref_date),
  format(min(ct_panel$ref_date)), format(max(ct_panel$ref_date))
))


# =============================================================================
# SECTION 1: Policy templates
# =============================================================================

# ── Retirement ────────────────────────────────────────────────────────────────
ret_policy_baseline <- list(
  eligibility_type = "age_only",
  min_age          = 60,        # lever: 55 / 58 / 60 / 63
  pension_type     = "flat",
  pension_params   = list(flat_amount = 500)
)

# ── Hiring ─────────────────────────────────────────────────────────────────────
# flow mode: vacancies = retirements × replacement_rate
hire_policy_baseline <- list(
  mode             = "flow",
  group_cols       = "est_id",
  replacement_rate = 1.0,        # lever: 0 / 0.5 / 1.0 / 1.5
  salary_scale     = data.table::copy(salary_scale_est)
)

# ── Movement / promotion ──────────────────────────────────────────────────────
# promotion_multiplier scales the historical promotion probability
move_policy_baseline <- list(
  group_cols           = c("est_id", "paygrade"),
  salary_scale         = data.table::copy(salary_scale_grade),
  salary_update_rule   = "scale",
  promotion_strategy   = "tenure",          # longest in grade promoted first
  transfer_strategy    = "reverse_tenure",
  promotion_multiplier = 1.0                # lever: 0 / 0.5 / 1.0 / 3.0
)

# ── Non-retirement attrition ──────────────────────────────────────────────────
# 5% flat annual attrition (voluntary resignation + contract non-renewals).
# Pass exit_policy = NULL to model zero attrition.
exit_policy_baseline <- list(
  mode          = "fixed_rate",
  fixed_rate    = 0.05,
  exit_strategy = "random",
  active_types  = c("perm", "fterm", "temp"),
  exited_type   = "inactive"
)


# =============================================================================
# SECTION 2: Baseline 10-year horizon (single named scenario)
# =============================================================================
# simulate_horizon() with scenario_name = "Baseline" stamps scenario_id,
# scenario_label, and is_baseline = TRUE onto every row of $comparison.
# The returned horizon object can be passed directly to generate_hrcastapp().

cat("====  SECTION 2: Baseline 10-year horizon  ====\n")

baseline_hz <- simulate_horizon(
  contract_dt        = data.table::copy(bra_hrmis_contract),  # full panel for movement baseline
  personnel_dt       = data.table::copy(bra_hrmis_personnel),
  salary_scale_dt    = data.table::copy(salary_scale_grade),
  n_periods          = 10L,
  ref_date           = REF_DATE,
  period_unit        = "year",
  birth_date_col     = "birth_date",
  retirement_policy  = ret_policy_baseline,
  exit_policy        = exit_policy_baseline,
  movement_policy    = move_policy_baseline,
  hiring_policy      = hire_policy_baseline,
  salary_growth_rate = 0.03,
  pension_cola_rate  = 0.02,
  scenario_name      = "Baseline",     # NEW: stamps scenario_id + scenario_label
  is_baseline        = TRUE            # NEW: flagged for Shiny comparator
)

cat("\nBaseline — period-by-period summary:\n")
print(
  baseline_hz$comparison[, .(
    year            = format(period_date, "%Y"),
    headcount       = n_headcount_end,
    wage_bill       = round(wage_bill_end),
    n_retirements   = n_exits,
    n_attrition     = n_non_ret_exits,
    n_hires,
    pension_total   = round(pension_cost_total),
    cola_pct_of_bill = round(inflation_effect_pct_of_end_bill * 100, 1)
  )]
)

terminal_bau <- baseline_hz$comparison[period_date == max(period_date)]
cat(sprintf(
  "\nYear 10: headcount = %d | wage bill = %s | pension liability = %s\n\n",
  terminal_bau$n_headcount_end,
  format(round(terminal_bau$wage_bill_end),       big.mark = ","),
  format(round(terminal_bau$pension_cost_total),  big.mark = ",")
))

# The baseline horizon can be passed directly to the Shiny app:
#   generate_hrcastapp(baseline_hz)


# =============================================================================
# SECTION 3: Named scenario comparisons → flat table → Shiny
# =============================================================================
# Each call to simulate_horizon() produces a horizon object with scenario_id,
# scenario_label, and is_baseline stamped on $comparison.
# rbindlist() across $comparison tables produces a single flat data.table
# that generate_hrcastapp() can dashboard directly.

cat("====  SECTION 3: Named scenario comparisons  ====\n")

# Helper: run one scenario and return its horizon object
run_named_hz <- function(label,
                         ret_min_age  = 60L,
                         cola         = 0.03,
                         replace_rate = 1.0,
                         promo_mult   = 1.0,
                         baseline     = FALSE) {

  rp <- modifyList(ret_policy_baseline,  list(min_age              = ret_min_age))
  hp <- modifyList(hire_policy_baseline, list(replacement_rate     = replace_rate,
                                              salary_scale         = data.table::copy(salary_scale_est)))
  mp <- modifyList(move_policy_baseline, list(promotion_multiplier = promo_mult,
                                              salary_scale         = data.table::copy(salary_scale_grade)))

  simulate_horizon(
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
    pension_cola_rate  = cola * 0.67,   # pension COLA = 2/3 of active COLA
    scenario_name      = label,         # stamped onto $comparison
    is_baseline        = baseline
  )
}

# Run all named scenarios
scenario_list <- list(
  run_named_hz("Baseline (60, 3% COLA, full backfill)", baseline = TRUE),
  run_named_hz("Later retirement (63)",     ret_min_age = 63L),
  run_named_hz("Earlier retirement (55)",   ret_min_age = 55L),
  run_named_hz("Wage freeze (0% COLA)",     cola        = 0.00),
  run_named_hz("High COLA (8%)",            cola        = 0.08),
  run_named_hz("Hiring freeze",             replace_rate = 0.0),
  run_named_hz("Expansion (150% backfill)", replace_rate = 1.5),
  run_named_hz("Regrading (3x promo)",      promo_mult   = 3.0),
  run_named_hz("Austerity (0COLA+50%hire)", cola = 0.00, replace_rate = 0.5),
  run_named_hz("Reform (63+5%+expand)",     ret_min_age = 63L, cola = 0.05,
                                            replace_rate = 1.5)
)

# Bind all $comparison tables into one flat data.table.
# Each row already carries scenario_id, scenario_label, is_baseline from
# simulate_horizon(), so no post-processing needed.
named_scenarios_flat <- data.table::rbindlist(
  lapply(scenario_list, `[[`, "comparison"),
  use.names = TRUE, fill = TRUE
)

# Year-10 terminal-year summary for a quick console comparison
terminal_named <- named_scenarios_flat[
  period_date == max(period_date),
  .(scenario_label,
    headcount       = n_headcount_end,
    wage_bill_Y10   = round(wage_bill_end),
    pension_Y10     = round(pension_cost_total),
    total_Y10       = round(wage_bill_end + pension_cost_total))
][order(total_Y10)]

cat("\nYear-10 comparison across named scenarios (sorted by total cost):\n")
print(terminal_named)

# Pass the flat table directly to the Shiny app for a 10-scenario dashboard:
#   generate_hrcastapp(named_scenarios_flat)


# =============================================================================
# SECTION 4: Scenario matrix — retirement age × COLA (4 × 4 = 16 scenarios)
# =============================================================================
# generate_scenario_matrix() runs all grid combinations automatically and
# returns a long-format data.table with scenario_id, scenario_label, and
# is_baseline already set — identical column structure to named_scenarios_flat.

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
  baseline_scenario_id = 3L   # row 3 = min_age 60 + COLA 3% (the policy baseline)
)

cat(sprintf(
  "Lite grid: %d scenarios × %d periods = %d rows\n",
  data.table::uniqueN(results_lite$scenario_id),
  data.table::uniqueN(results_lite$period_date),
  nrow(results_lite)
))

cat("\nYear-10 wage bill by retirement age × COLA:\n")
print(
  results_lite[
    period_date == max(period_date),
    .(scenario_label,
      headcount        = n_headcount_end,
      wage_bill_end    = round(wage_bill_end),
      pension_total    = round(pension_cost_total),
      cola_effect_pct  = round(inflation_effect_pct_of_end_bill * 100, 1))
  ][order(wage_bill_end)]
)

# Dashboard the 16-scenario grid:
#   generate_hrcastapp(results_lite)


# =============================================================================
# SECTION 5: Full policy matrix — all 4 Gambia axes (4^4 = 256 scenarios)
# =============================================================================
# ~5–10 minutes on the bra dataset.  Comment out if not needed.

cat("\n====  SECTION 5: Full 4-axis matrix (256 scenarios)  ====\n")

param_grid_full <- list(
  retirement_min_age            = c(55L, 58L, 60L, 63L),
  salary_growth_rate            = c(0, 0.03, 0.05, 0.08),
  hiring_replacement_rate       = c(0, 0.5, 1.0, 1.5),
  movement_promotion_multiplier = c(0, 0.5, 1.0, 3.0)
)

# Locate the policy baseline row in the Cartesian grid
baseline_idx <- which(
  data.table::CJ(
    retirement_min_age            = param_grid_full$retirement_min_age,
    salary_growth_rate            = param_grid_full$salary_growth_rate,
    hiring_replacement_rate       = param_grid_full$hiring_replacement_rate,
    movement_promotion_multiplier = param_grid_full$movement_promotion_multiplier
  )[,
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

terminal_full <- results_full[period_date == max(period_date)]

cat(sprintf(
  "Full grid: %d scenarios × %d periods = %d rows\n",
  data.table::uniqueN(results_full$scenario_id),
  data.table::uniqueN(results_full$period_date),
  nrow(results_full)
))

cat("\nTop 5 most expensive (Year 10 wage bill):\n")
print(terminal_full[order(-wage_bill_end)][1:5,
  .(scenario_label, headcount = n_headcount_end,
    wage_bill_end = round(wage_bill_end), pension = round(pension_cost_total))])

cat("\nTop 5 least expensive (Year 10 wage bill):\n")
print(terminal_full[order(wage_bill_end)][1:5,
  .(scenario_label, headcount = n_headcount_end,
    wage_bill_end = round(wage_bill_end), pension = round(pension_cost_total))])

cat(sprintf(
  "\nWage-bill range at Year 10:  Min = %s  |  Max = %s  |  Spread = %.1f%%\n",
  format(round(terminal_full[, min(wage_bill_end)]), big.mark = ","),
  format(round(terminal_full[, max(wage_bill_end)]), big.mark = ","),
  (terminal_full[, max(wage_bill_end) / min(wage_bill_end)] - 1) * 100
))


# =============================================================================
# SECTION 6: Launch Shiny dashboard
# =============================================================================
# generate_hrcastapp() accepts any of the three result objects produced above.
# All share the same flat-table column structure (scenario_id, scenario_label,
# is_baseline, period_date, wage_bill_end, ...) so the app works identically
# regardless of which you pass.
#
# Choose based on what you want to explore:

cat("\n====  SECTION 6: Launch dashboard  ====\n")

# Option A — single baseline scenario (Section 2):
# generate_hrcastapp(baseline_hz)

# Option B — 10 named hand-crafted scenarios (Section 3) — RECOMMENDED for demos
generate_hrcastapp(named_scenarios_flat)

# Option C — 16-scenario retirement × COLA grid (Section 4):
# generate_hrcastapp(results_lite)

# Option D — full 256-scenario grid (Section 5):
# generate_hrcastapp(results_full)
