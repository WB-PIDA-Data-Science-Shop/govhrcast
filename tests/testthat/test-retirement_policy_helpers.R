# Unit tests for R/retirement_policy_helpers.R

library(testthat)
library(data.table)

# =============================================================================
# Rules: who meets them
# =============================================================================

test_that("retirement_rule_age builds the rule in the three-slot shape", {
  expect_equal(retirement_rule_age(60),
               list(group_cols = NULL, policy_table = NULL,
                    defaults = list(kind = "age", min_age = 60)))
})

test_that("retirement_rule_age rejects an invalid min_age", {
  expect_error(retirement_rule_age(-1), "min_age")
  expect_error(retirement_rule_age("60"), "min_age")
})

test_that("retirement_rule_age keeps a per-group table and checks its keys", {
  tbl <- data.table(occupation = "teacher", min_age = 70)
  rule <- retirement_rule_age(75, group_cols = "occupation", policy_table = tbl)
  expect_identical(rule$group_cols, "occupation")
  expect_identical(rule$policy_table, tbl)
  expect_error(retirement_rule_age(75, group_cols = "grade", policy_table = tbl),
               "missing the group column\\(s\\) grade")
  expect_error(retirement_rule_age(75, group_cols = "occupation", policy_table = list()),
               "policy_table must be a data.table")
  expect_error(retirement_rule_age(75, group_cols = 1), "group_cols must be a character vector")
})

test_that("retirement_rule_tenure builds the rule, and allows zero years", {
  expect_equal(retirement_rule_tenure(30)$defaults, list(kind = "tenure", min_tenure = 30))
  expect_equal(retirement_rule_tenure(0)$defaults$min_tenure, 0)
})

test_that("retirement_rule_age_tenure builds the rule with both thresholds", {
  expect_equal(retirement_rule_age_tenure(60, 10)$defaults,
               list(kind = "age_tenure", min_age = 60, min_tenure = 10))
})

test_that("retirement_rule_custom checks the rule when it is built", {
  rule <- retirement_rule_custom("age >= 65 | tenure_years >= 35")
  expect_equal(rule$defaults, list(kind = "custom", custom_rule = "age >= 65 | tenure_years >= 35"))
  expect_error(retirement_rule_custom("system('ls')"), "eligibility_rule")
  expect_error(retirement_rule_custom("age >= "), "could not be parsed")
})

test_that("retirement_rule_custom carries only the thresholds it is given", {
  rule <- retirement_rule_custom("age >= min_age | tenure_years >= min_tenure",
                                 min_age = 65, min_tenure = 35)
  expect_equal(rule$defaults$min_age, 65)
  expect_equal(rule$defaults$min_tenure, 35)
  expect_false("min_age" %in% names(retirement_rule_custom("age >= 65")$defaults))
})

# =============================================================================
# Rates: as in the history
# =============================================================================

test_that("retirement_rates_status_quo keys the rates by age by default", {
  expect_equal(retirement_rates_status_quo(),
               list(group_cols = "age_yr_int", policy_table = NULL,
                    defaults = list(hazard_rate = 0, retire_strategy = "random")))
})

test_that("retirement_rates_status_quo turns 'by' into whole-year keys, then group_cols", {
  expect_identical(retirement_rates_status_quo(by = "tenure")$group_cols, "tenure_yr_int")
  expect_identical(retirement_rates_status_quo(by = "age_tenure", group_cols = "est_id")$group_cols,
                   c("age_yr_int", "tenure_yr_int", "est_id"))
  # A key the caller repeats is kept once
  expect_identical(retirement_rates_status_quo(group_cols = "age_yr_int")$group_cols, "age_yr_int")
  expect_error(retirement_rates_status_quo(by = "grade"), "by")
})

test_that("retirement_rates_status_quo checks your own rate table against the keys", {
  ok <- data.table(age_yr_int = 60:61, est_id = "E1", hazard_rate = 0.1)
  expect_identical(retirement_rates_status_quo(group_cols = "est_id", hazard_table = ok)$policy_table, ok)
  expect_error(retirement_rates_status_quo(group_cols = "est_id",
                                           hazard_table = data.table(age_yr_int = 60L, hazard_rate = 0.1)),
               "missing column\\(s\\) est_id")
  expect_error(retirement_rates_status_quo(hazard_table = data.table(age_yr_int = 60L)),
               "missing column\\(s\\) hazard_rate")
  expect_error(retirement_rates_status_quo(hazard_table = list()), "must be a data.table")
  expect_error(retirement_rates_status_quo(retire_strategy = c("a", "b")), "retire_strategy")
})

# =============================================================================
# Pension constructors
# =============================================================================

test_that("pension_db builds the correct fragment with defaults", {
  result <- pension_db(accrual_rate = 0.02)
  expect_equal(result$defaults$pension_type, "db")
  expect_equal(result$defaults$accrual_rate, 0.02)
  expect_true(is.na(result$defaults$max_years))
  expect_true(is.na(result$defaults$replacement_cap))
})

test_that("pension_db validates inputs at construction", {
  expect_error(pension_db(accrual_rate = "x"), regexp = "numeric")
})

test_that("a pension helper no longer takes a wage column", {
  expect_error(pension_db(accrual_rate = 0.02, ref_wage_col = "w"), "unused argument")
  expect_error(pension_rate(pension_rate = 0.15, ref_wage_col = "w"), "unused argument")
})

test_that("pension_dc builds the correct fragment", {
  result <- pension_dc(balance_col = "dc_balance", annuity_factor = 12)
  expect_equal(result$defaults$pension_type, "dc")
  expect_equal(result$defaults$balance_col, "dc_balance")
  expect_equal(result$defaults$annuity_factor, 12)
  expect_true(is.na(result$defaults$notional_rate))
})

test_that("pension_dc rejects a zero or negative annuity_factor", {
  expect_error(pension_dc(balance_col = "b", annuity_factor = 0))
})

test_that("pension_flat builds the correct fragment", {
  expect_equal(pension_flat(500), list(defaults = list(pension_type = "flat", flat_amount = 500)))
})

test_that("pension_rate builds the correct fragment", {
  result <- pension_rate(pension_rate = 0.15)
  expect_equal(
    result,
    list(defaults = list(pension_type = "rate", pension_rate = 0.15))
  )
})

test_that("pension_hybrid unions db and dc fields under pension_type = hybrid", {
  result <- pension_hybrid(
    db = pension_db(accrual_rate = 0.015),
    dc = pension_dc(balance_col = "dc_balance", annuity_factor = 12)
  )
  expect_equal(result$defaults$pension_type, "hybrid")
  expect_equal(result$defaults$accrual_rate, 0.015)
  expect_equal(result$defaults$balance_col, "dc_balance")
  expect_equal(result$defaults$annuity_factor, 12)
})

test_that("pension_hybrid requires its arguments to be pension_db()/pension_dc() results", {
  expect_error(pension_hybrid(db = pension_flat(500), dc = pension_dc("b", 12)), regexp = "pension_db")
  expect_error(pension_hybrid(db = pension_db(0.02), dc = pension_flat(500)), regexp = "pension_dc")
})

test_that("pension_custom validates the formula at construction time", {
  result <- pension_custom("pmin(base_salary_lcu * 0.02 * tenure_years, base_salary_lcu * 0.8)")
  expect_equal(result$defaults$pension_type, "custom")

  expect_error(pension_custom("base_salary_lcu; unlink('x')"), regexp = "could not be parsed")
  expect_error(pension_custom("system('id')"), regexp = "disallowed")
})

# =============================================================================
# Policies: one per kind
# =============================================================================

test_that("design_retirement_policy_status_quo holds the rates and the pension, no rule", {
  policy <- design_retirement_policy_status_quo(pension_flat(500))
  expect_identical(policy$type, "status_quo")
  expect_identical(policy$rates, retirement_rates_status_quo())
  expect_identical(policy$pension, pension_flat(500))
  expect_null(policy$rule)
  expect_null(policy$active_types)
})

test_that("design_retirement_policy_eligibility holds the rule and the pension, no rates", {
  policy <- design_retirement_policy_eligibility(retirement_rule_age(60), pension_flat(500),
                                                 active_types = "permanent")
  expect_identical(policy$type, "eligibility")
  expect_identical(policy$rule, retirement_rule_age(60))
  expect_identical(policy$active_types, "permanent")
  expect_null(policy$rates)
})

test_that("design_retirement_policy_mandatory holds the rule, the rates and the pension", {
  rates  <- retirement_rates_status_quo(by = "age_tenure")
  policy <- design_retirement_policy_mandatory(retirement_rule_age(75), pension_flat(500), rates)
  expect_identical(policy$type, "mandatory")
  expect_identical(policy$rule, retirement_rule_age(75))
  expect_identical(policy$rates, rates)
})

test_that("each part must come from its own constructor", {
  expect_error(design_retirement_policy_eligibility(list(defaults = list(min_age = 60)), pension_flat(500)),
               "'rule' must come from a retirement_rule_")
  expect_error(design_retirement_policy_status_quo(pension_flat(500), rates = list(group_cols = "est_id")),
               "'rates' must come from retirement_rates_status_quo")
  expect_error(design_retirement_policy_eligibility(retirement_rule_age(60), list(defaults = list())),
               "'pension' must come from a pension_")
  expect_error(design_retirement_policy_eligibility(retirement_rule_age(60), pension_flat(500),
                                                    active_types = NA_character_),
               "active_types")
})

# =============================================================================
# Integration: constructed policies are true drop-ins
# =============================================================================

test_that("a constructed policy retires and pays as the rule and pension say", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001", start_date = as.Date("2000-01-01"),
    end_date = as.Date(NA), contract_type = "permanent", base_salary_lcu = 5000
  )
  personnel_dt <- data.table(
    personnel_id = "P001", birth_date = as.Date("1960-01-01"), age = 65,
    employment_status = "active"
  )
  ref_date <- as.Date("2025-01-01")

  constructed <- design_retirement_policy_eligibility(retirement_rule_age(60), pension_rate(0.15))
  res_constructed <- simulate_retirement(contract_dt, personnel_dt, as_of_date = ref_date,
                                         policy_params = constructed, column_spec_args = define_column_spec(salary_col = "base_salary_lcu"))

  expect_equal(res_constructed$summary$n_retired, 1L)
  expect_equal(res_constructed$retirees_dt$pension, 750)
})

test_that("design_retirement_policy_mandatory works end-to-end through simulate_horizon()", {
  snap1 <- as.Date("2019-01-01")
  snap2 <- as.Date("2020-01-01")
  ids <- paste0("P", 1:6)
  contract_panel <- data.table(
    ref_date          = c(rep(snap1, 6), rep(snap2, 6)),
    contract_id       = paste0("C", c(1:6, 1:6)),
    personnel_id      = rep(ids, 2),
    start_date        = as.Date("2010-01-01"),
    end_date          = as.Date(NA),
    contract_type     = c(rep("permanent", 6),
                          c("pensioner", "pensioner", "permanent", "permanent", "permanent", "permanent")),
    base_salary_lcu   = 10000
  )
  personnel_panel <- data.table(
    ref_date          = c(rep(snap1, 6), rep(snap2, 6)),
    personnel_id      = rep(ids, 2),
    employment_status = c(rep("active", 6),
                          c("pensioner", "pensioner", "active", "active", "active", "active")),
    age               = 60,
    birth_date        = as.Date("1959-01-01"),
    tenure_years      = 20
  )
  salary_scale <- data.table(est_id = "E1", base_salary_lcu = 10000)

  policy <- design_retirement_policy_mandatory(
    retirement_rule_age(75),
    pension = pension_custom("pmin(base_salary_lcu * 0.02 * tenure_years, base_salary_lcu * 0.8)")
  )

  res <- simulate_horizon(
    contract_dt = contract_panel, personnel_dt = personnel_panel, salary_scale_dt = salary_scale,
    n_periods = 1, retirement_policy = policy,
    column_spec_args = define_column_spec(salary_col = "base_salary_lcu"),
    baseline_date = snap2, seed = 1
  )

  # 4 active candidates (P3-P6) at snap2, estimated hazard_rate = 2/6:
  # 1.33 expected, so 1 or 2 retirees (rounded at random, D6).
  expect_true(res$comparison$n_exits[1] %in% 1:2)
})
