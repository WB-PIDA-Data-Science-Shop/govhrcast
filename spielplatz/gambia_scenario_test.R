# =============================================================================
# Gambia Wage Bill Scenario Test
# govhrcast spielplatz — March 2026
# =============================================================================
#
# PURPOSE
# -------
# Test generate_scenario_matrix() and generate_hrcastapp() against the
# policy questions raised for The Gambia. We use the Brazilian HRMIS
# teaching dataset (bra_hrmis_*) as a structural proxy — the scenarios and
# levers are the same as those applied to actual Gambian payroll data.
#
# GAMBIA POLICY INTERESTS (mapped to generate_scenario_matrix param_grid)
# -----------------------------------------------------------------------
#  1. Retirement age reform
#        → retirement_min_age: 55, 58, 60, 63
#
#  2. COLA / salary increment policy
#        → salary_growth_rate: 0, 0.03, 0.05, 0.08
#
#  3. Hiring / recruitment policy
#        → hiring_replacement_rate: 0 (freeze), 0.5, 1.0, 2.0
#          (using flow mode; mode set on the base hiring_policy template)
#
#  4. Promotion / regrading intensity
#        → movement_promotion_multiplier: 0 (freeze), 0.5, 1.0, 3.0
#          (using status_quo-style movement; multiplier varies across scenarios)
#
# MATRIX DESIGN
# -------------
# Four param_grid axes above × all combinations = 4×4×4×4 = 256 scenarios.
# That is within the feasible range for a synchronous run (~3–5 min with
# the bra dataset at 10 periods). A targeted 2-axis cross (retirement age
# × COLA) is also available as the "lite" grid (16 scenarios).
#
# =============================================================================

suppressPackageStartupMessages(library(data.table))
devtools::load_all(quiet = TRUE)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 0: Data preparation (mirrors zambia_scenario_exploration.R)
# ─────────────────────────────────────────────────────────────────────────────

REF_DATE <- as.Date("2015-09-01")

# Full contract + personnel panels (all ref_dates kept for status_quo hiring)
bra_ct_panel <- bra_hrmis_contract[
  contract_type_code %in% c("perm", "fterm", "temp")
]
bra_pt_panel <- bra_hrmis_personnel[status == "active"]

# Base snapshot
ct_base <- bra_ct_panel[ref_date == REF_DATE]
pt_raw  <- bra_pt_panel[ref_date == REF_DATE]
pt_raw[, age := as.integer(
  difftime(REF_DATE, birth_date, units = "days") / 365.25
)]

tenure_dt <- ct_base[,
  .(tenure_years = max(
      as.integer(difftime(REF_DATE, start_date, units = "days") / 365.25),
      na.rm = TRUE
    )),
  by = personnel_id
]
pt_base <- merge(
  pt_raw[, !"ref_date", with = FALSE],
  tenure_dt,
  by  = "personnel_id",
  all.x = TRUE
)
pt_base[is.na(tenure_years), tenure_years := 0L]
pt_base <- unique(pt_base, by = "personnel_id")

# Age + tenure on the full panel
bra_pt_panel[, age := as.integer(
  difftime(ref_date, birth_date, units = "days") / 365.25
)]
bra_pt_panel[is.na(age), age := 0L]
bra_pt_panel <- merge(
  bra_pt_panel[, !"tenure_years", with = FALSE],
  tenure_dt,
  by    = "personnel_id",
  all.x = TRUE
)
bra_pt_panel[is.na(tenure_years), tenure_years := 0L]
bra_pt_panel[, tenure_years := tenure_years +
               as.integer(difftime(ref_date, REF_DATE, units = "days") / 365.25)]

# Salary scales
salary_scale_est <- ct_base[,
  .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
  keyby = est_id
]
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
# SECTION 1: Base policy templates
# ─────────────────────────────────────────────────────────────────────────────
# These serve as the *defaults* that generate_scenario_matrix() will mutate
# via the param_grid keys (e.g. retirement_min_age, hiring_replacement_rate,
# movement_promotion_multiplier).

base_retirement_policy <- list(
  eligibility_type = "age_only",
  min_age          = 60,          # overridden by retirement_min_age lever
  pension_type     = "flat",
  pension_params   = list(flat_amount = 500)
)

# Flow-mode: each retired post is partially or fully backfilled.
# hiring_replacement_rate lever (0, 0.5, 1.0, 2.0) scales the flow.
base_hiring_policy <- list(
  mode             = "flow",
  group_cols       = "est_id",
  replacement_rate = 1.0,         # overridden by hiring_replacement_rate lever
  salary_scale     = data.table::copy(salary_scale_est)
)

# Movement policy: promotion multiplier varies across scenarios.
# promotion_multiplier = 0  → freeze promotions
# promotion_multiplier = 1  → historical pace
# promotion_multiplier = 3  → regrading exercise
base_movement_policy <- list(
  group_cols             = c("est_id", "paygrade"),
  salary_scale           = data.table::copy(salary_scale_est_grade),
  salary_update_rule     = "scale",
  promotion_strategy     = "tenure",
  transfer_strategy      = "reverse_tenure",
  promotion_multiplier   = 1.0    # overridden by movement_promotion_multiplier
)


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 2: LITE grid — retirement age × COLA (16 scenarios, ~30 sec)
# ─────────────────────────────────────────────────────────────────────────────
# Good for a quick sanity check of the app rendering.

cat("\n====  LITE GRID: retirement age × COLA (16 scenarios)  ====\n")

param_grid_lite <- list(
  retirement_min_age = c(55L, 58L, 60L, 63L),
  salary_growth_rate = c(0, 0.03, 0.05, 0.08)
)

results_lite <- generate_scenario_matrix(
  contract_dt        = data.table::copy(bra_ct_panel),
  personnel_dt       = data.table::copy(bra_pt_panel),
  salary_scale_dt    = data.table::copy(salary_scale_est_grade),
  param_grid         = param_grid_lite,
  n_periods          = 10L,
  retirement_policy  = base_retirement_policy,
  movement_policy    = base_movement_policy,
  hiring_policy      = base_hiring_policy,
  salary_growth_rate = 0.03,
  ref_date           = REF_DATE,
  age_col            = "age",
  tenure_col         = "tenure_years",
  baseline_scenario_id = 3L        # min_age=60, COLA=3% is the "baseline"
)

cat(sprintf(
  "Lite grid: %d scenarios × %d periods = %d rows\n",
  data.table::uniqueN(results_lite$scenario_id),
  data.table::uniqueN(results_lite$period_date),
  nrow(results_lite)
))

# Quick terminal-year snapshot
cat("\nTerminal-year wage bill by scenario (lite grid):\n")
results_lite[
  period_date == max(period_date),
  .(scenario_label,
    n_exits,
    n_headcount_end,
    wage_bill_end              = round(wage_bill_end),
    pension_cost_total         = round(pension_cost_total),
    inflation_effect_pct       = round(inflation_effect_pct_of_end_bill * 100, 1))
][order(wage_bill_end)]


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 3: FULL grid — all 4 Gambia policy axes (256 scenarios, ~5–10 min)
# ─────────────────────────────────────────────────────────────────────────────
# Covers: retirement age reform, COLA policy, hiring/recruitment policy,
# and promotion / regrading intensity.

cat("\n====  FULL GRID: 4 axes × 4 levels = 256 scenarios  ====\n")

param_grid_full <- list(
  retirement_min_age            = c(55L, 58L, 60L, 63L),
  salary_growth_rate            = c(0, 0.03, 0.05, 0.08),
  hiring_replacement_rate       = c(0, 0.5, 1.0, 2.0),
  movement_promotion_multiplier = c(0, 0.5, 1.0, 3.0)
)

results_full <- generate_scenario_matrix(
  contract_dt        = data.table::copy(bra_ct_panel),
  personnel_dt       = data.table::copy(bra_pt_panel),
  salary_scale_dt    = data.table::copy(salary_scale_est_grade),
  param_grid         = param_grid_full,
  n_periods          = 10L,
  retirement_policy  = base_retirement_policy,
  movement_policy    = base_movement_policy,
  hiring_policy      = base_hiring_policy,
  salary_growth_rate = 0.03,
  ref_date           = REF_DATE,
  age_col            = "age",
  tenure_col         = "tenure_years",
  # Baseline: retire 60 | 3% COLA | 100% backfill | normal promotions
  baseline_scenario_id = which(
    do.call(data.table::CJ, c(param_grid_full, list(sorted = FALSE)))[,
      retirement_min_age            == 60L &
      salary_growth_rate            == 0.03 &
      hiring_replacement_rate       == 1.0  &
      movement_promotion_multiplier == 1.0
    ]
  )
)

cat(sprintf(
  "Full grid: %d scenarios × %d periods = %d rows\n",
  data.table::uniqueN(results_full$scenario_id),
  data.table::uniqueN(results_full$period_date),
  nrow(results_full)
))

# Highest- and lowest-cost terminal-year scenarios
terminal_full <- results_full[period_date == max(period_date)]

cat("\nTop 5 highest wage bill scenarios (terminal year):\n")
terminal_full[order(-wage_bill_end)][1:5, .(
  scenario_label,
  n_headcount_end,
  wage_bill_end      = round(wage_bill_end),
  pension_cost_total = round(pension_cost_total)
)]

cat("\nTop 5 lowest wage bill scenarios (terminal year):\n")
terminal_full[order(wage_bill_end)][1:5, .(
  scenario_label,
  n_headcount_end,
  wage_bill_end      = round(wage_bill_end),
  pension_cost_total = round(pension_cost_total)
)]

cat("\nWage-bill range across all 256 scenarios (terminal year):\n")
terminal_full[, .(
  min_wb   = round(min(wage_bill_end)),
  max_wb   = round(max(wage_bill_end)),
  range_pct = round((max(wage_bill_end) / min(wage_bill_end) - 1) * 100, 1)
)]


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 4: Launch the Shiny dashboard
# ─────────────────────────────────────────────────────────────────────────────
# Pass the full 256-scenario matrix.  In interactive sessions this opens the
# browser immediately.  Use results_lite for a faster first look.

cat("\n====  Launching govhrcast dashboard (full grid)  ====\n")
cat("Tip: switch to results_lite for a faster first load.\n\n")

# --- Full grid app (256 scenarios) ---
generate_hrcastapp(results_full)

# --- Lite grid app (16 scenarios) — uncomment to use instead ---
# generate_hrcastapp(results_lite)
