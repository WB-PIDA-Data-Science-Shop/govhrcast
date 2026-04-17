# Integration tests for simulate_retirement()
# Testing the complete retirement simulation workflow

library(testthat)
library(data.table)

# =============================================================================
# Test data setup
# =============================================================================

create_integration_test_data <- function() {
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003", "C004", "C005"),
    personnel_id = c("P001", "P002", "P003", "P003", "P004"),
    ref_date = as.Date("2024-01-01"),
    start_date = as.Date(c("1995-01-01", "2000-01-01", "2000-01-01", "2010-01-01", "2015-01-01")),
    # C003 ends 2015-01-01; C004 starts 2010-01-01 (overlapping).
    # Union for P003: [2000-01-01, 2025-01-01] = 25 years.
    end_date = as.Date(c(NA, NA, "2015-01-01", NA, NA)),
    contract_type_code = c("perm", "perm", "perm", "perm", "perm"),
    gross_salary_lcu = c(12000, 10000, 8000, 11000, 9000)
  )

  personnel_dt <- data.table(
    personnel_id = c("P001", "P002", "P003", "P004"),
    birth_date = as.Date(c("1960-01-01", "1965-01-01", "1970-01-01", "1980-01-01")),
    status = c("active", "active", "active", "active")
  )

  list(contract_dt = contract_dt, personnel_dt = personnel_dt)
}

# =============================================================================
# simulate_retirement() - age_only eligibility
# =============================================================================

test_that("simulate_retirement works with age_only eligibility", {
  test_data <- create_integration_test_data()

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_only",
      pension_type     = "flat",
      min_age          = 60,
      flat_amount      = 15000
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- simulate_retirement(
    contract_dt  = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("summary", "contract_dt", "personnel_dt", "retirees_dt"))

  # P001 (age 65) and P002 (age 60) should retire
  expect_equal(result$summary$n_retired, 2)
  expect_equal(result$summary$total_pension, 30000)
  expect_equal(result$summary$avg_pension, 15000)

  # Check retirees_dt
  expect_s3_class(result$retirees_dt, "data.table")
  expect_true(all(c("P001", "P002") %in% result$retirees_dt$personnel_id))
  expect_equal(nrow(result$retirees_dt), 2)

  # Check contract updates
  expect_equal(result$contract_dt[contract_id == "C001"]$contract_type_code, "pensioner")
  expect_equal(result$contract_dt[contract_id == "C002"]$contract_type_code, "pensioner")
  expect_equal(result$contract_dt[contract_id == "C004"]$contract_type_code, "perm")

  # Check personnel updates
  expect_equal(result$personnel_dt[personnel_id == "P001"]$status, "inactive")
  expect_equal(result$personnel_dt[personnel_id == "P002"]$status, "inactive")
  expect_equal(result$personnel_dt[personnel_id == "P003"]$status, "active")
  expect_equal(result$personnel_dt[personnel_id == "P004"]$status, "active")
})

# =============================================================================
# simulate_retirement() - tenure_only eligibility
# =============================================================================

test_that("simulate_retirement works with tenure_only eligibility", {
  test_data <- create_integration_test_data()

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "tenure_only",
      pension_type     = "flat",
      min_tenure       = 25,
      flat_amount      = 12000
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- simulate_retirement(
    contract_dt  = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  # P001 (30 yrs), P002 (25 yrs), P003 (union = 25 yrs) should retire
  expect_equal(result$summary$n_retired, 3)
  expect_true(all(c("P001", "P002", "P003") %in% result$retirees_dt$personnel_id))
})

# =============================================================================
# simulate_retirement() - age_and_tenure eligibility
# =============================================================================

test_that("simulate_retirement works with age_and_tenure eligibility", {
  test_data <- create_integration_test_data()

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_and_tenure",
      pension_type     = "flat",
      min_age          = 60,
      min_tenure       = 20,
      flat_amount      = 10000
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- simulate_retirement(
    contract_dt  = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  # Only P001 (age 65, tenure 30) and P002 (age 60, tenure 25) meet BOTH criteria
  expect_equal(result$summary$n_retired, 2)
  expect_true(all(c("P001", "P002") %in% result$retirees_dt$personnel_id))
})

# =============================================================================
# simulate_retirement() - DB pension
# =============================================================================

test_that("simulate_retirement works with DB pension", {
  test_data <- create_integration_test_data()

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "tenure_only",
      pension_type     = "db",
      min_tenure       = 20,
      accrual_rate     = 0.02,
      ref_wage_col     = "gross_salary_lcu",
      max_years        = 30,
      replacement_cap  = 0.70
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- simulate_retirement(
    contract_dt  = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  # P001, P002, P003 should retire (all >= 20 years tenure)
  expect_equal(result$summary$n_retired, 3)

  # P001: min(0.02 * min(~30, 30) * 12000, 0.70 * 12000) ≈ 7200
  expect_equal(result$retirees_dt[personnel_id == "P001"]$pension, 7200, tolerance = 5)

  # P002: min(0.02 * min(25.002, 30) * 10000, 0.70 * 10000) ≈ 5000.4
  expect_equal(result$retirees_dt[personnel_id == "P002"]$pension, 5000, tolerance = 1)
})

# =============================================================================
# simulate_retirement() - DC pension
# =============================================================================

test_that("simulate_retirement works with DC pension", {
  test_data <- create_integration_test_data()
  test_data$contract_dt[, account_balance := c(600000, 500000, 400000, 450000, 300000)]

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_only",
      pension_type     = "dc",
      min_age          = 60,
      balance_col      = "account_balance",
      annuity_factor   = 20
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- simulate_retirement(
    contract_dt  = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  expect_equal(result$summary$n_retired, 2)
  # P001: 600000 / 20 = 30000
  expect_equal(result$retirees_dt[personnel_id == "P001"]$pension, 30000)
  # P002: 500000 / 20 = 25000
  expect_equal(result$retirees_dt[personnel_id == "P002"]$pension, 25000)
})

# =============================================================================
# simulate_retirement() - hybrid pension
# =============================================================================

test_that("simulate_retirement works with hybrid pension", {
  test_data <- create_integration_test_data()
  test_data$contract_dt[, account_balance := c(400000, 350000, 300000, 320000, 250000)]

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_and_tenure",
      pension_type     = "hybrid",
      min_age          = 60,
      min_tenure       = 20,
      # DB component
      accrual_rate     = 0.01,
      ref_wage_col     = "gross_salary_lcu",
      # DC component
      balance_col      = "account_balance",
      annuity_factor   = 25
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- simulate_retirement(
    contract_dt  = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  expect_equal(result$summary$n_retired, 2)

  # P001: DB = 0.01 * 30.001 * 12000 ≈ 3600.16, DC = 400000 / 25 = 16000, Total ≈ 19600
  expect_equal(result$retirees_dt[personnel_id == "P001"]$pension, 19600, tolerance = 1)

  # P002: DB = 0.01 * 25.002 * 10000 ≈ 2500.21, DC = 350000 / 25 = 14000, Total ≈ 16500
  expect_equal(result$retirees_dt[personnel_id == "P002"]$pension, 16500, tolerance = 1)
})

# =============================================================================
# simulate_retirement() - edge cases
# =============================================================================

test_that("simulate_retirement handles no eligible retirees", {
  test_data <- create_integration_test_data()

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_only",
      pension_type     = "flat",
      min_age          = 70,  # No one is 70 yet
      flat_amount      = 15000
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- simulate_retirement(
    contract_dt  = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  expect_equal(result$summary$n_retired, 0)
  expect_equal(result$summary$total_pension, 0)
  expect_true(is.na(result$summary$avg_pension))
  expect_equal(nrow(result$retirees_dt), 0)
  expect_true(all(result$contract_dt$contract_type_code == "perm"))
  expect_true(all(result$personnel_dt$status == "active"))
})

test_that("simulate_retirement handles retiree with multiple contracts", {
  test_data <- create_integration_test_data()

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "tenure_only",
      pension_type     = "flat",
      min_tenure       = 20,
      flat_amount      = 10000
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- simulate_retirement(
    contract_dt  = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  # P003 has 2 contracts; all contracts for a retiring person are marked 'pensioner'
  expect_equal(result$contract_dt[contract_id == "C004"]$contract_type_code, "pensioner")
  expect_equal(result$contract_dt[contract_id == "C003"]$contract_type_code, "pensioner")
})

test_that("simulate_retirement does not modify original data", {
  test_data <- create_integration_test_data()
  original_contract_dt  <- copy(test_data$contract_dt)
  original_personnel_dt <- copy(test_data$personnel_dt)

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_only",
      pension_type     = "flat",
      min_age          = 60,
      flat_amount      = 15000
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- simulate_retirement(
    contract_dt  = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  expect_equal(test_data$contract_dt,  original_contract_dt)
  expect_equal(test_data$personnel_dt, original_personnel_dt)
})

test_that("simulate_retirement validates inputs correctly", {
  test_data <- create_integration_test_data()

  # min_age intentionally omitted — validation should catch it
  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_only",
      # min_age deliberately missing
      pension_type = "flat",
      flat_amount  = 15000
    )
  )

  ref_date <- as.Date("2025-01-01")

  expect_error(
    simulate_retirement(
      contract_dt  = test_data$contract_dt,
      personnel_dt = test_data$personnel_dt,
      ref_date     = ref_date,
      policy_params = policy_params
    ),
    "min_age is required"
  )
})

test_that("simulate_retirement handles missing birth dates gracefully", {
  test_data <- create_integration_test_data()
  test_data$personnel_dt[personnel_id == "P001", birth_date := NA]

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_only",
      pension_type     = "flat",
      min_age          = 60,
      flat_amount      = 15000
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- simulate_retirement(
    contract_dt  = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  # Only P002 should retire (P001 has NA birth_date)
  expect_equal(result$summary$n_retired, 1)
  expect_equal(result$retirees_dt$personnel_id, "P002")
})

test_that("simulate_retirement works with package data", {
  data(bra_hrmis_contract,  package = "govhrcast")
  data(bra_hrmis_personnel, package = "govhrcast")

  policy_params <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "tenure_only",
      pension_type     = "db",
      min_tenure       = 20,
      accrual_rate     = 0.02,
      ref_wage_col     = "gross_salary_lcu",
      max_years        = 35,
      replacement_cap  = 0.80
    )
  )

  ref_date <- as.Date("2020-01-01")

  result <- simulate_retirement(
    contract_dt  = bra_hrmis_contract,
    personnel_dt = bra_hrmis_personnel,
    ref_date     = ref_date,
    policy_params = policy_params
  )

  expect_type(result, "list")
  expect_s3_class(result$summary,      "data.table")
  expect_s3_class(result$contract_dt,  "data.table")
  expect_s3_class(result$personnel_dt, "data.table")
  expect_s3_class(result$retirees_dt,  "data.table")
  expect_true(result$summary$n_retired > 0)
})


test_that("simulate_retirement: group-level accrual_rate via policy_table produces different pensions per grade", {
  ref_date <- as.Date("2025-01-01")
  contract_dt <- data.table::data.table(
    personnel_id       = c("P001", "P002"),
    contract_id        = c("C001", "C002"),
    start_date         = as.Date("1990-01-01"),
    end_date           = as.Date(NA),
    contract_type_code = "perm",
    gross_salary_lcu   = 5000,
    grade              = c("A", "B")
  )
  personnel_dt <- data.table::data.table(
    personnel_id = c("P001", "P002"),
    birth_date   = as.Date(c("1955-01-01", "1955-01-01")),  # both age 70
    status       = "active"
  )
  accrual_tbl <- data.table::data.table(
    grade        = c("A", "B"),
    accrual_rate = c(0.02, 0.03)
  )
  policy_params <- list(
    group_cols   = "grade",
    policy_table = accrual_tbl,
    defaults = list(
      eligibility_type = "age_only",
      pension_type     = "db",
      min_age          = 60,
      accrual_rate     = 0.02,
      ref_wage_col     = "gross_salary_lcu",
      replacement_cap  = 1.0
    )
  )
  result <- simulate_retirement(
    contract_dt  = contract_dt,
    personnel_dt = personnel_dt,
    ref_date     = ref_date,
    policy_params = policy_params
  )
  # Both retire; grade B should have higher pension due to higher accrual rate
  expect_equal(result$summary$n_retired, 2L)
  p_A <- result$retirees_dt[personnel_id == "P001"]$pension
  p_B <- result$retirees_dt[personnel_id == "P002"]$pension
  expect_true(p_B > p_A)
})


# =============================================================================
# simulate_retirement() - age_only eligibility
# =============================================================================

test_that("simulate_retirement works with age_only eligibility", {
  test_data <- create_integration_test_data()
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "age_only",
      min_age = 60,
      pension_type = "flat",
      flat_amount = 15000
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- simulate_retirement(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # Check structure
  expect_type(result, "list")
  expect_named(result, c("summary", "contract_dt", "personnel_dt", "retirees_dt"))
  
  # P001 (age 65) and P002 (age 60) should retire
  expect_equal(result$summary$n_retired, 2)
  expect_equal(result$summary$total_pension, 30000)
  expect_equal(result$summary$avg_pension, 15000)
  
  # Check retirees_dt
  expect_s3_class(result$retirees_dt, "data.table")
  expect_true(all(c("P001", "P002") %in% result$retirees_dt$personnel_id))
  expect_equal(nrow(result$retirees_dt), 2)
  
  # Check contract updates
  expect_equal(result$contract_dt[contract_id == "C001"]$contract_type_code, "pensioner")
  expect_equal(result$contract_dt[contract_id == "C002"]$contract_type_code, "pensioner")
  expect_equal(result$contract_dt[contract_id == "C004"]$contract_type_code, "perm")
  
  # Check personnel updates
  expect_equal(result$personnel_dt[personnel_id == "P001"]$status, "inactive")
  expect_equal(result$personnel_dt[personnel_id == "P002"]$status, "inactive")
  expect_equal(result$personnel_dt[personnel_id == "P003"]$status, "active")
  expect_equal(result$personnel_dt[personnel_id == "P004"]$status, "active")
})

# =============================================================================
# simulate_retirement() - tenure_only eligibility
# =============================================================================

test_that("simulate_retirement works with tenure_only eligibility", {
  test_data <- create_integration_test_data()
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "tenure_only",
      min_tenure = 25,
      pension_type = "flat",
      flat_amount = 12000
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- simulate_retirement(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # P001 (30 yrs), P002 (25 yrs), P003 (union of overlapping spells = 25 yrs) should retire
  expect_equal(result$summary$n_retired, 3)
  
  # Check correct personnel retired
  expect_true(all(c("P001", "P002", "P003") %in% result$retirees_dt$personnel_id))
})

# =============================================================================
# simulate_retirement() - age_and_tenure eligibility
# =============================================================================

test_that("simulate_retirement works with age_and_tenure eligibility", {
  test_data <- create_integration_test_data()
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "age_and_tenure",
      min_age = 60,
      min_tenure = 20,
      pension_type = "flat",
      flat_amount = 10000
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- simulate_retirement(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # Only P001 (age 65, tenure 30) and P002 (age 60, tenure 25) meet BOTH criteria
  expect_equal(result$summary$n_retired, 2)
  expect_true(all(c("P001", "P002") %in% result$retirees_dt$personnel_id))
})

# =============================================================================
# simulate_retirement() - DB pension
# =============================================================================

test_that("simulate_retirement works with DB pension", {
  test_data <- create_integration_test_data()
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "tenure_only",
      min_tenure = 20,
      pension_type = "db",
      accrual_rate = 0.02,
      ref_wage_col = "gross_salary_lcu",
      max_years = 30,
      replacement_cap = 0.70
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- simulate_retirement(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # P001, P002, P003 should retire (all >= 20 years tenure)
  expect_equal(result$summary$n_retired, 3)
  
  # Check pension calculations
  # P001: min(0.02 * min(~30, 30) * 12000, 0.70 * 12000) ≈ 7199 (30 yrs ± leap-year rounding)
  expect_equal(result$retirees_dt[personnel_id == "P001"]$pension, 7200, tolerance = 5)
  
  # P002: min(0.02 * min(25.002, 30) * 10000, 0.70 * 10000) ≈ min(5000.4, 7000) ≈ 5000.4
  expect_equal(result$retirees_dt[personnel_id == "P002"]$pension, 5000, tolerance = 1)
})

# =============================================================================
# simulate_retirement() - DC pension
# =============================================================================

test_that("simulate_retirement works with DC pension", {
  # Add account balance column
  test_data <- create_integration_test_data()
  test_data$contract_dt[, account_balance := c(600000, 500000, 400000, 450000, 300000)]
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "age_only",
      min_age = 60,
      pension_type = "dc",
      balance_col = "account_balance",
      annuity_factor = 20
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- simulate_retirement(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # P001 and P002 retire
  expect_equal(result$summary$n_retired, 2)
  
  # Check pension calculations
  # P001: 600000 / 20 = 30000
  expect_equal(result$retirees_dt[personnel_id == "P001"]$pension, 30000)
  
  # P002: 500000 / 20 = 25000
  expect_equal(result$retirees_dt[personnel_id == "P002"]$pension, 25000)
})

# =============================================================================
# simulate_retirement() - hybrid pension
# =============================================================================

test_that("simulate_retirement works with hybrid pension", {
  # Add account balance column
  test_data <- create_integration_test_data()
  test_data$contract_dt[, account_balance := c(400000, 350000, 300000, 320000, 250000)]
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "age_and_tenure",
      min_age = 60,
      min_tenure = 20,
      pension_type = "hybrid",
      accrual_rate   = 0.01,
      ref_wage_col   = "gross_salary_lcu",
      balance_col    = "account_balance",
      annuity_factor = 25
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- simulate_retirement(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # P001 and P002 retire
  expect_equal(result$summary$n_retired, 2)
  
  # Check pension calculations
  # P001: DB = 0.01 * 30.001 * 12000 ≈ 3600.16, DC = 400000 / 25 = 16000, Total ≈ 19600.16
  expect_equal(result$retirees_dt[personnel_id == "P001"]$pension, 19600, tolerance = 1)
  
  # P002: DB = 0.01 * 25.002 * 10000 ≈ 2500.21, DC = 350000 / 25 = 14000, Total ≈ 16500.21
  expect_equal(result$retirees_dt[personnel_id == "P002"]$pension, 16500, tolerance = 1)
})

# =============================================================================
# simulate_retirement() - edge cases
# =============================================================================

test_that("simulate_retirement handles no eligible retirees", {
  test_data <- create_integration_test_data()
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "age_only",
      min_age = 70,  # No one is 70 yet
      pension_type = "flat",
      flat_amount = 15000
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- simulate_retirement(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # No retirements
  expect_equal(result$summary$n_retired, 0)
  expect_equal(result$summary$total_pension, 0)
  expect_true(is.na(result$summary$avg_pension))
  expect_equal(nrow(result$retirees_dt), 0)
  
  # All contracts and personnel unchanged
  expect_true(all(result$contract_dt$contract_type_code == "perm"))
  expect_true(all(result$personnel_dt$status == "active"))
})

test_that("simulate_retirement handles retiree with multiple contracts", {
  test_data <- create_integration_test_data()
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "tenure_only",
      min_tenure = 20,
      pension_type = "flat",
      flat_amount = 10000
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- simulate_retirement(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # P003 has 2 contracts; Phase 0d ensures ALL contracts for a retiring person
  # are marked 'pensioner' (not just the primary contract)
  expect_equal(result$contract_dt[contract_id == "C004"]$contract_type_code, "pensioner")
  expect_equal(result$contract_dt[contract_id == "C003"]$contract_type_code, "pensioner")
})

test_that("simulate_retirement does not modify original data", {
  test_data <- create_integration_test_data()
  original_contract_dt <- copy(test_data$contract_dt)
  original_personnel_dt <- copy(test_data$personnel_dt)
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "age_only",
      min_age = 60,
      pension_type = "flat",
      flat_amount = 15000
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- simulate_retirement(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # Original data should be unchanged
  expect_equal(test_data$contract_dt, original_contract_dt)
  expect_equal(test_data$personnel_dt, original_personnel_dt)
})

test_that("simulate_retirement validates inputs correctly", {
  test_data <- create_integration_test_data()
  
  # Missing required parameter
  policy_params <- list(
    defaults = list(
      eligibility_type = "age_only",
      # Missing min_age
      pension_type = "flat",
      flat_amount = 15000
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  expect_error(
    simulate_retirement(
      contract_dt = test_data$contract_dt,
      personnel_dt = test_data$personnel_dt,
      policy_params = policy_params,
      ref_date = ref_date
    ),
    "min_age is required"
  )
})

test_that("simulate_retirement handles missing birth dates gracefully", {
  test_data <- create_integration_test_data()
  test_data$personnel_dt[personnel_id == "P001", birth_date := NA]
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "age_only",
      min_age = 60,
      pension_type = "flat",
      flat_amount = 15000
    )
  )
  
  ref_date <- as.Date("2025-01-01")
  
  result <- simulate_retirement(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # Only P002 should retire (P001 has NA birth_date)
  expect_equal(result$summary$n_retired, 1)
  expect_equal(result$retirees_dt$personnel_id, "P002")
})

test_that("simulate_retirement works with package data", {
  # Load package example data
  data(bra_hrmis_contract, package = "govhrcast")
  data(bra_hrmis_personnel, package = "govhrcast")
  
  policy_params <- list(
    defaults = list(
      eligibility_type = "tenure_only",
      min_tenure = 20,
      pension_type = "db",
      accrual_rate = 0.02,
      ref_wage_col = "gross_salary_lcu",
      max_years = 35,
      replacement_cap = 0.80
    )
  )
  
  ref_date <- as.Date("2020-01-01")
  
  # Should run without errors
  result <- simulate_retirement(
    contract_dt = bra_hrmis_contract,
    personnel_dt = bra_hrmis_personnel,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # Check structure
  expect_type(result, "list")
  expect_s3_class(result$summary, "data.table")
  expect_s3_class(result$contract_dt, "data.table")
  expect_s3_class(result$personnel_dt, "data.table")
  expect_s3_class(result$retirees_dt, "data.table")
  
  # Should have some retirees
  expect_true(result$summary$n_retired > 0)
})


test_that("simulate_retirement: group-level accrual_rate produces different pensions per grade", {
  ref_date <- as.Date("2025-01-01")
  contract_dt <- data.table::data.table(
    personnel_id       = c("P001", "P002"),
    contract_id        = c("C001", "C002"),
    start_date         = as.Date("1990-01-01"),
    end_date           = as.Date(NA),
    contract_type_code = "perm",
    gross_salary_lcu   = 5000,
    grade              = c("A", "B")
  )
  personnel_dt <- data.table::data.table(
    personnel_id = c("P001", "P002"),
    birth_date   = as.Date(c("1955-01-01", "1955-01-01")),  # both age 70
    status       = "active"
  )
  accrual_tbl <- data.table::data.table(
    grade        = c("A", "B"),
    accrual_rate = c(0.02, 0.03)
  )
  policy_params <- list(
    group_cols   = "grade",
    policy_table = accrual_tbl,
    defaults = list(
      eligibility_type = "age_only",
      min_age          = 60,
      pension_type     = "db",
      ref_wage_col     = "gross_salary_lcu",
      replacement_cap  = 1.0
    )
  )
  result <- simulate_retirement(
    contract_dt  = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date     = ref_date
  )
  # Both retire; grade B should have higher pension due to higher accrual rate
  expect_equal(result$summary$n_retired, 2L)
  p_A <- result$retirees_dt[personnel_id == "P001"]$pension
  p_B <- result$retirees_dt[personnel_id == "P002"]$pension
  expect_true(p_B > p_A)
})
