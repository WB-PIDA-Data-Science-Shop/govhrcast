# Unit tests for R/retirement_policy_helpers.R

library(testthat)
library(data.table)

# =============================================================================
# Eligibility constructors
# =============================================================================

test_that("eligibility_age builds the correct fragment", {
  result <- eligibility_age(min_age = 60)
  expect_equal(result, list(defaults = list(eligibility_type = "age_only", min_age = 60)))
})

test_that("eligibility_age rejects invalid min_age", {
  expect_error(eligibility_age("60"), regexp = "numeric")
  expect_error(eligibility_age(c(60, 65)), regexp = "single")
  expect_error(eligibility_age(NA_real_), regexp = "NA")
})

test_that("eligibility_tenure builds the correct fragment", {
  result <- eligibility_tenure(min_tenure = 30)
  expect_equal(result, list(defaults = list(eligibility_type = "tenure_only", min_tenure = 30)))
})

test_that("eligibility_tenure allows zero", {
  expect_no_error(eligibility_tenure(min_tenure = 0))
})

test_that("eligibility_age_tenure builds the correct fragment", {
  result <- eligibility_age_tenure(min_age = 60, min_tenure = 10)
  expect_equal(
    result,
    list(defaults = list(eligibility_type = "age_and_tenure", min_age = 60, min_tenure = 10))
  )
})

test_that("eligibility_status_quo defaults to auto-estimation trigger shape", {
  result <- eligibility_status_quo(mandatory_age = 75)
  expect_null(result$policy_table)
  expect_equal(result$group_cols, "age_yr_int")
  expect_equal(result$defaults$eligibility_type, "hazard_rate")
  expect_equal(result$defaults$min_age, 75)
  expect_equal(result$defaults$retire_strategy, "random")
})

test_that("eligibility_status_quo accepts a supplied hazard_table", {
  tbl <- data.table(age_yr_int = 60:65, hazard_rate = seq(0.1, 0.6, length.out = 6))
  result <- eligibility_status_quo(hazard_table = tbl)
  expect_identical(result$policy_table, tbl)
  expect_null(result$defaults$min_age)
})

test_that("eligibility_status_quo never requires the caller to reference age_yr_int", {
  result <- eligibility_status_quo(mandatory_age = 75)
  expect_equal(result$group_cols, "age_yr_int")
})

test_that("eligibility_status_quo unions group_cols with the implicit age_yr_int", {
  result <- eligibility_status_quo(mandatory_age = 75, group_cols = "est_id")
  expect_equal(result$group_cols, c("age_yr_int", "est_id"))
})

test_that("eligibility_status_quo de-duplicates a redundant age_yr_int in group_cols", {
  result <- eligibility_status_quo(mandatory_age = 75, group_cols = "age_yr_int")
  expect_equal(result$group_cols, "age_yr_int")
})

test_that("eligibility_status_quo rejects a non-character group_cols", {
  expect_error(eligibility_status_quo(group_cols = 1), regexp = "character vector")
})

test_that("eligibility_status_quo validates a supplied hazard_table against the full group_cols", {
  # missing the "est_id" column that group_cols asks for
  tbl <- data.table(age_yr_int = 60:65, hazard_rate = 0.1)
  expect_error(
    eligibility_status_quo(hazard_table = tbl, group_cols = "est_id"),
    regexp = "est_id"
  )
})

test_that("eligibility_status_quo requires hazard_table to be a data.table with hazard_rate", {
  expect_error(eligibility_status_quo(hazard_table = data.frame(age_yr_int = 60)), regexp = "data.table")
  expect_error(eligibility_status_quo(hazard_table = data.table(age_yr_int = 60)), regexp = "hazard_rate")
})

test_that("eligibility_custom validates the rule at construction time", {
  result <- eligibility_custom("age >= 62 | (tenure_years >= 30 & age >= 55)")
  expect_equal(result$defaults$eligibility_type, "custom")
  expect_equal(result$defaults$eligibility_rule, "age >= 62 | (tenure_years >= 30 & age >= 55)")

  expect_error(eligibility_custom("system('rm -rf /')"), regexp = "disallowed")
  expect_error(eligibility_custom("age >="), regexp = "could not be parsed")
})

# =============================================================================
# Pension constructors
# =============================================================================

test_that("pension_db builds the correct fragment with defaults", {
  result <- pension_db(accrual_rate = 0.02, ref_wage_col = "base_salary_lcu")
  expect_equal(result$defaults$pension_type, "db")
  expect_equal(result$defaults$accrual_rate, 0.02)
  expect_equal(result$defaults$ref_wage_col, "base_salary_lcu")
  expect_true(is.na(result$defaults$max_years))
  expect_true(is.na(result$defaults$replacement_cap))
})

test_that("pension_db validates inputs at construction", {
  expect_error(pension_db(accrual_rate = "x", ref_wage_col = "y"), regexp = "numeric")
  expect_error(pension_db(accrual_rate = 0.02, ref_wage_col = 5), regexp = "character string")
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
  result <- pension_rate(pension_rate = 0.15, ref_wage_col = "base_salary_lcu")
  expect_equal(
    result,
    list(defaults = list(pension_type = "rate", pension_rate = 0.15, ref_wage_col = "base_salary_lcu"))
  )
})

test_that("pension_hybrid unions db and dc fields under pension_type = hybrid", {
  result <- pension_hybrid(
    db = pension_db(accrual_rate = 0.015, ref_wage_col = "base_salary_lcu"),
    dc = pension_dc(balance_col = "dc_balance", annuity_factor = 12)
  )
  expect_equal(result$defaults$pension_type, "hybrid")
  expect_equal(result$defaults$accrual_rate, 0.015)
  expect_equal(result$defaults$ref_wage_col, "base_salary_lcu")
  expect_equal(result$defaults$balance_col, "dc_balance")
  expect_equal(result$defaults$annuity_factor, 12)
})

test_that("pension_hybrid requires its arguments to be pension_db()/pension_dc() results", {
  expect_error(pension_hybrid(db = pension_flat(500), dc = pension_dc("b", 12)), regexp = "pension_db")
  expect_error(pension_hybrid(db = pension_db(0.02, "x"), dc = pension_flat(500)), regexp = "pension_dc")
})

test_that("pension_custom validates the formula at construction time", {
  result <- pension_custom("pmin(base_salary_lcu * 0.02 * tenure_years, base_salary_lcu * 0.8)")
  expect_equal(result$defaults$pension_type, "custom")

  expect_error(pension_custom("base_salary_lcu; unlink('x')"), regexp = "could not be parsed")
  expect_error(pension_custom("system('id')"), regexp = "disallowed")
})

# =============================================================================
# design_retirement_policy() combiner
# =============================================================================

test_that("design_retirement_policy merges eligibility and pension defaults", {
  result <- design_retirement_policy(
    eligibility = eligibility_age_tenure(60, 10),
    pension     = pension_db(accrual_rate = 0.02, ref_wage_col = "base_salary_lcu", max_years = 35, replacement_cap = 0.80)
  )
  expect_null(result$group_cols)
  expect_null(result$policy_table)
  expect_equal(result$defaults$eligibility_type, "age_and_tenure")
  expect_equal(result$defaults$min_age, 60)
  expect_equal(result$defaults$min_tenure, 10)
  expect_equal(result$defaults$pension_type, "db")
  expect_equal(result$defaults$accrual_rate, 0.02)
})

test_that("design_retirement_policy passes through explicit group_cols/policy_table", {
  tbl <- data.table(grade = c("A", "B"), accrual_rate = c(0.02, 0.03))
  result <- design_retirement_policy(
    eligibility_age(60),
    pension_db(accrual_rate = 0.02, ref_wage_col = "base_salary_lcu"),
    group_cols = "grade", policy_table = tbl
  )
  expect_equal(result$group_cols, "grade")
  expect_identical(result$policy_table, tbl)
})

test_that("design_retirement_policy uses eligibility's own group_cols/policy_table when set", {
  hazard_tbl <- data.table(age_yr_int = 60:65, hazard_rate = 0.1)
  result <- design_retirement_policy(
    eligibility_status_quo(hazard_table = hazard_tbl),
    pension_flat(500)
  )
  expect_equal(result$group_cols, "age_yr_int")
  expect_identical(result$policy_table, hazard_tbl)
})

test_that("design_retirement_policy errors on conflicting policy_table", {
  hazard_tbl <- data.table(age_yr_int = 60, hazard_rate = 0.1)
  other_tbl  <- data.table(age_yr_int = 61, hazard_rate = 0.2)
  expect_error(
    design_retirement_policy(
      eligibility_status_quo(hazard_table = hazard_tbl),
      pension_flat(500),
      policy_table = other_tbl
    ),
    regexp = "ambiguous"
  )
})

test_that("design_retirement_policy errors on conflicting group_cols", {
  expect_error(
    design_retirement_policy(
      eligibility_status_quo(mandatory_age = 75),  # group_cols = "age_yr_int"
      pension_flat(500),
      group_cols = "est_id"
    ),
    regexp = "ambiguous"
  )
})

test_that("design_retirement_policy allows matching group_cols without conflict", {
  expect_no_error(
    design_retirement_policy(
      eligibility_status_quo(mandatory_age = 75),
      pension_flat(500),
      group_cols = "age_yr_int"
    )
  )
})

test_that("design_retirement_policy supports the mixed eligibility_type/pension_type escape hatch", {
  result <- design_retirement_policy(
    eligibility_age(60), pension_flat(500),
    group_cols = "grade",
    policy_table = data.table(
      grade = c("A", "B"),
      eligibility_type = c("age_only", "hazard_rate"),
      pension_type     = c("flat", "db")
    )
  )
  expect_equal(nrow(result$policy_table), 2L)
  expect_true(all(c("eligibility_type", "pension_type") %in% names(result$policy_table)))
})

test_that("design_retirement_policy rejects non-constructor inputs", {
  expect_error(design_retirement_policy(list(foo = 1), pension_flat(500)), regexp = "eligibility_\\*\\(\\)")
  expect_error(design_retirement_policy(eligibility_age(60), list(foo = 1)), regexp = "pension_\\*\\(\\)")
})

# =============================================================================
# Presets
# =============================================================================

test_that("design_retirement_policy_age matches manual composition", {
  expect_equal(
    design_retirement_policy_age(60, pension_flat(500)),
    design_retirement_policy(eligibility_age(60), pension_flat(500))
  )
})

test_that("design_retirement_policy_age forwards group_cols/policy_table", {
  tbl <- data.table(grade = c("A", "B"), min_age = c(60, 65))
  result <- design_retirement_policy_age(60, pension_flat(500), group_cols = "grade", policy_table = tbl)
  expect_equal(result$group_cols, "grade")
  expect_identical(result$policy_table, tbl)
})

test_that("design_retirement_policy_tenure matches manual composition", {
  expect_equal(
    design_retirement_policy_tenure(30, pension_flat(500)),
    design_retirement_policy(eligibility_tenure(30), pension_flat(500))
  )
})

test_that("design_retirement_policy_age_tenure matches manual composition", {
  expect_equal(
    design_retirement_policy_age_tenure(60, 10, pension_flat(500)),
    design_retirement_policy(eligibility_age_tenure(60, 10), pension_flat(500))
  )
})

test_that("design_retirement_policy_status_quo matches manual composition", {
  expect_equal(
    design_retirement_policy_status_quo(pension_flat(500), mandatory_age = 75),
    design_retirement_policy(eligibility_status_quo(mandatory_age = 75), pension_flat(500))
  )
})

# =============================================================================
# Integration: constructed policies are true drop-ins for simulate_retirement()
# =============================================================================

test_that("a constructed policy produces identical output to the equivalent hand-written list", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001", start_date = as.Date("2000-01-01"),
    end_date = as.Date(NA), contract_type = "permanent", base_salary_lcu = 5000
  )
  personnel_dt <- data.table(
    personnel_id = "P001", birth_date = as.Date("1960-01-01"), employment_status = "active"
  )
  ref_date <- as.Date("2025-01-01")

  constructed <- design_retirement_policy_age(min_age = 60, pension = pension_rate(0.15, "base_salary_lcu"))
  handwritten <- list(
    group_cols = NULL, policy_table = NULL,
    defaults = list(eligibility_type = "age_only", min_age = 60,
                     pension_type = "rate", pension_rate = 0.15, ref_wage_col = "base_salary_lcu")
  )

  res_constructed <- simulate_retirement(contract_dt, personnel_dt, ref_date = ref_date,
                                         policy_params = constructed, salary_col = "base_salary_lcu")
  res_handwritten <- simulate_retirement(contract_dt, personnel_dt, ref_date = ref_date,
                                         policy_params = handwritten, salary_col = "base_salary_lcu")

  expect_identical(res_constructed$retirees_dt, res_handwritten$retirees_dt)
  expect_equal(res_constructed$summary$n_retired, 1L)
  expect_equal(res_constructed$retirees_dt$pension, 750)
})

test_that("design_retirement_policy_status_quo works end-to-end through simulate_horizon()", {
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

  policy <- design_retirement_policy_status_quo(
    mandatory_age = 75,
    pension = pension_custom("pmin(base_salary_lcu * 0.02 * tenure_years, base_salary_lcu * 0.8)")
  )

  res <- simulate_horizon(
    contract_dt = contract_panel, personnel_dt = personnel_panel, salary_scale_dt = salary_scale,
    n_periods = 1, retirement_policy = policy,
    column_spec_args = list(salary_col = "base_salary_lcu"),
    ref_date = snap2, seed = 1
  )

  # 4 active candidates (P3-P6) at snap2, estimated hazard_rate = 2/6 -> round(4 * 1/3) = 1 retiree.
  expect_equal(res$comparison$n_exits[1], 1L)
})
