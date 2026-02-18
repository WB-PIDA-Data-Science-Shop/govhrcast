# Unit tests for retirement_pension.R functions
# Testing all pension calculation methods with edge cases

library(testthat)
library(data.table)

# =============================================================================
# Test data setup
# =============================================================================

create_test_retirees_dt <- function() {
  data.table(
    personnel_id = c("P001", "P002", "P003", "P004"),
    contract_id = c("C001", "C002", "C003", "C004"),
    gross_salary_lcu = c(10000, 8000, 12000, 9000),
    tenure_years = c(30, 20, 35, 15),
    account_balance = c(500000, 400000, 600000, 300000),
    notional_balance = c(450000, 380000, 570000, 285000),
    age = c(65, 60, 68, 62)
  )
}

# =============================================================================
# compute_db_pension()
# =============================================================================

test_that("compute_db_pension calculates correctly without caps", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    accrual_rate = 0.02,
    ref_wage_col = "gross_salary_lcu"
  )
  
  result <- compute_db_pension(dt, params)
  
  # P001: 0.02 * 30 * 10000 = 6000
  expect_equal(result[1], 6000)
  
  # P002: 0.02 * 20 * 8000 = 3200
  expect_equal(result[2], 3200)
  
  # P003: 0.02 * 35 * 12000 = 8400
  expect_equal(result[3], 8400)
  
  # P004: 0.02 * 15 * 9000 = 2700
  expect_equal(result[4], 2700)
})

test_that("compute_db_pension applies max_years cap", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    accrual_rate = 0.02,
    ref_wage_col = "gross_salary_lcu",
    max_years = 25
  )
  
  result <- compute_db_pension(dt, params)
  
  # P001: 0.02 * min(30, 25) * 10000 = 0.02 * 25 * 10000 = 5000
  expect_equal(result[1], 5000)
  
  # P002: 0.02 * min(20, 25) * 8000 = 3200 (under cap)
  expect_equal(result[2], 3200)
  
  # P003: 0.02 * min(35, 25) * 12000 = 0.02 * 25 * 12000 = 6000
  expect_equal(result[3], 6000)
})

test_that("compute_db_pension applies replacement_cap", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    accrual_rate = 0.02,
    ref_wage_col = "gross_salary_lcu",
    replacement_cap = 0.50
  )
  
  result <- compute_db_pension(dt, params)
  
  # P001: min(0.02 * 30 * 10000, 0.50 * 10000) = min(6000, 5000) = 5000
  expect_equal(result[1], 5000)
  
  # P002: min(0.02 * 20 * 8000, 0.50 * 8000) = min(3200, 4000) = 3200
  expect_equal(result[2], 3200)
  
  # P003: min(0.02 * 35 * 12000, 0.50 * 12000) = min(8400, 6000) = 6000
  expect_equal(result[3], 6000)
})

test_that("compute_db_pension applies both caps correctly", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    accrual_rate = 0.02,
    ref_wage_col = "gross_salary_lcu",
    max_years = 25,
    replacement_cap = 0.40
  )
  
  result <- compute_db_pension(dt, params)
  
  # P001: 
  # - years = min(30, 25) = 25
  # - gross = 0.02 * 25 * 10000 = 5000
  # - capped = min(5000, 0.40 * 10000) = min(5000, 4000) = 4000
  expect_equal(result[1], 4000)
  
  # P003:
  # - years = min(35, 25) = 25
  # - gross = 0.02 * 25 * 12000 = 6000
  # - capped = min(6000, 0.40 * 12000) = min(6000, 4800) = 4800
  expect_equal(result[3], 4800)
})

test_that("compute_db_pension handles NA salaries", {
  dt <- data.table(
    personnel_id = c("P001", "P002"),
    gross_salary_lcu = c(10000, NA),
    tenure_years = c(25, 20)
  )
  
  params <- list(
    accrual_rate = 0.02,
    ref_wage_col = "gross_salary_lcu"
  )
  
  result <- compute_db_pension(dt, params)
  
  expect_equal(result[1], 5000)
  expect_true(is.na(result[2]))
})

test_that("compute_db_pension handles NA tenure", {
  dt <- data.table(
    personnel_id = c("P001", "P002"),
    gross_salary_lcu = c(10000, 8000),
    tenure_years = c(25, NA)
  )
  
  params <- list(
    accrual_rate = 0.02,
    ref_wage_col = "gross_salary_lcu"
  )
  
  result <- compute_db_pension(dt, params)
  
  expect_equal(result[1], 5000)
  expect_true(is.na(result[2]))
})

test_that("compute_db_pension requires accrual_rate", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    ref_wage_col = "gross_salary_lcu"
  )
  
  expect_error(
    compute_db_pension(dt, params),
    "Missing DB pension parameters: accrual_rate"
  )
})

test_that("compute_db_pension requires ref_wage_col", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    accrual_rate = 0.02
  )
  
  expect_error(
    compute_db_pension(dt, params),
    "Missing DB pension parameters: ref_wage_col"
  )
})

test_that("compute_db_pension handles empty data.table", {
  dt <- data.table(
    personnel_id = character(),
    gross_salary_lcu = numeric(),
    tenure_years = numeric()
  )
  
  params <- list(
    accrual_rate = 0.02,
    ref_wage_col = "gross_salary_lcu"
  )
  
  result <- compute_db_pension(dt, params)
  
  expect_equal(length(result), 0)
})

# =============================================================================
# compute_dc_pension()
# =============================================================================

test_that("compute_dc_pension calculates standard DC correctly", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    balance_col = "account_balance",
    annuity_factor = 20
  )
  
  result <- compute_dc_pension(dt, params)
  
  # P001: 500000 / 20 = 25000
  expect_equal(result[1], 25000)
  
  # P002: 400000 / 20 = 20000
  expect_equal(result[2], 20000)
  
  # P003: 600000 / 20 = 30000
  expect_equal(result[3], 30000)
  
  # P004: 300000 / 20 = 15000
  expect_equal(result[4], 15000)
})

test_that("compute_dc_pension calculates NDC correctly", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    balance_col = "notional_balance",
    annuity_factor = 18,
    type = "NDC",
    notional_rate = 0.02
  )
  
  result <- compute_dc_pension(dt, params)
  
  # P001: (450000 * 1.02) / 18 = 459000 / 18 = 25500
  expect_equal(result[1], 25500)
  
  # P002: (380000 * 1.02) / 18 = 387600 / 18 = 21533.33...
  expect_equal(result[2], 387600 / 18)
})

test_that("compute_dc_pension handles NA balances", {
  dt <- data.table(
    personnel_id = c("P001", "P002"),
    account_balance = c(500000, NA)
  )
  
  params <- list(
    balance_col = "account_balance",
    annuity_factor = 20
  )
  
  result <- compute_dc_pension(dt, params)
  
  expect_equal(result[1], 25000)
  expect_true(is.na(result[2]))
})

test_that("compute_dc_pension requires balance_col", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    annuity_factor = 20
  )
  
  expect_error(
    compute_dc_pension(dt, params),
    "Missing DC pension parameters: balance_col"
  )
})

test_that("compute_dc_pension requires annuity_factor", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    balance_col = "account_balance"
  )
  
  expect_error(
    compute_dc_pension(dt, params),
    "Missing DC pension parameters: annuity_factor"
  )
})

test_that("compute_dc_pension requires notional_rate for NDC", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    balance_col = "notional_balance",
    annuity_factor = 18,
    type = "NDC"
  )
  
  expect_error(
    compute_dc_pension(dt, params),
    "notional_rate required for NDC pension type"
  )
})

test_that("compute_dc_pension handles empty data.table", {
  dt <- data.table(
    personnel_id = character(),
    account_balance = numeric()
  )
  
  params <- list(
    balance_col = "account_balance",
    annuity_factor = 20
  )
  
  result <- compute_dc_pension(dt, params)
  
  expect_equal(length(result), 0)
})

# =============================================================================
# compute_flat_pension()
# =============================================================================

test_that("compute_flat_pension assigns uniform amount", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    flat_amount = 15000
  )
  
  result <- compute_flat_pension(dt, params)
  
  expect_equal(length(result), 4)
  expect_true(all(result == 15000))
})

test_that("compute_flat_pension works with single retiree", {
  dt <- data.table(personnel_id = "P001")
  
  params <- list(
    flat_amount = 12500
  )
  
  result <- compute_flat_pension(dt, params)
  
  expect_equal(result, 12500)
})

test_that("compute_flat_pension requires flat_amount", {
  dt <- create_test_retirees_dt()
  
  params <- list()
  
  expect_error(
    compute_flat_pension(dt, params),
    "flat_amount is required for flat pension type"
  )
})

test_that("compute_flat_pension handles empty data.table", {
  dt <- data.table(personnel_id = character())
  
  params <- list(
    flat_amount = 10000
  )
  
  result <- compute_flat_pension(dt, params)
  
  expect_equal(length(result), 0)
})

test_that("compute_flat_pension handles zero amount", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    flat_amount = 0
  )
  
  result <- compute_flat_pension(dt, params)
  
  expect_true(all(result == 0))
})

# =============================================================================
# compute_hybrid_pension()
# =============================================================================

test_that("compute_hybrid_pension combines DB and DC correctly", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    db_params = list(
      accrual_rate = 0.015,
      ref_wage_col = "gross_salary_lcu"
    ),
    dc_params = list(
      balance_col = "account_balance",
      annuity_factor = 25
    )
  )
  
  result <- compute_hybrid_pension(dt, params)
  
  # P001: 
  # DB = 0.015 * 30 * 10000 = 4500
  # DC = 500000 / 25 = 20000
  # Total = 24500
  expect_equal(result[1], 24500)
  
  # P002:
  # DB = 0.015 * 20 * 8000 = 2400
  # DC = 400000 / 25 = 16000
  # Total = 18400
  expect_equal(result[2], 18400)
})

test_that("compute_hybrid_pension handles NA in DB component", {
  dt <- data.table(
    personnel_id = c("P001", "P002"),
    gross_salary_lcu = c(10000, NA),
    tenure_years = c(25, 20),
    account_balance = c(500000, 400000)
  )
  
  params <- list(
    db_params = list(
      accrual_rate = 0.02,
      ref_wage_col = "gross_salary_lcu"
    ),
    dc_params = list(
      balance_col = "account_balance",
      annuity_factor = 20
    )
  )
  
  result <- compute_hybrid_pension(dt, params)
  
  # P001: DB=5000, DC=25000, Total=30000
  expect_equal(result[1], 30000)
  
  # P002: DB=NA, DC=20000, Total=NA (NA + 20000 = NA)
  expect_true(is.na(result[2]))
})

test_that("compute_hybrid_pension handles NA in DC component", {
  dt <- data.table(
    personnel_id = c("P001", "P002"),
    gross_salary_lcu = c(10000, 8000),
    tenure_years = c(25, 20),
    account_balance = c(500000, NA)
  )
  
  params <- list(
    db_params = list(
      accrual_rate = 0.02,
      ref_wage_col = "gross_salary_lcu"
    ),
    dc_params = list(
      balance_col = "account_balance",
      annuity_factor = 20
    )
  )
  
  result <- compute_hybrid_pension(dt, params)
  
  # P001: DB=5000, DC=25000, Total=30000
  expect_equal(result[1], 30000)
  
  # P002: DB=3200, DC=NA, Total=NA
  expect_true(is.na(result[2]))
})

test_that("compute_hybrid_pension requires db_params", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    dc_params = list(
      balance_col = "account_balance",
      annuity_factor = 20
    )
  )
  
  expect_error(
    compute_hybrid_pension(dt, params),
    "Both db_params and dc_params are required for hybrid pension type"
  )
})

test_that("compute_hybrid_pension requires dc_params", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    db_params = list(
      accrual_rate = 0.02,
      ref_wage_col = "gross_salary_lcu"
    )
  )
  
  expect_error(
    compute_hybrid_pension(dt, params),
    "Both db_params and dc_params are required for hybrid pension type"
  )
})

test_that("compute_hybrid_pension handles empty data.table", {
  dt <- data.table(
    personnel_id = character(),
    gross_salary_lcu = numeric(),
    tenure_years = numeric(),
    account_balance = numeric()
  )
  
  params <- list(
    db_params = list(
      accrual_rate = 0.02,
      ref_wage_col = "gross_salary_lcu"
    ),
    dc_params = list(
      balance_col = "account_balance",
      annuity_factor = 20
    )
  )
  
  result <- compute_hybrid_pension(dt, params)
  
  expect_equal(length(result), 0)
})

# =============================================================================
# compute_pension() - dispatcher
# =============================================================================

test_that("compute_pension dispatches to DB correctly", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    accrual_rate = 0.02,
    ref_wage_col = "gross_salary_lcu"
  )
  
  result <- compute_pension(dt, "db", params)
  
  expect_equal(result[1], 6000)
})

test_that("compute_pension dispatches to DC correctly", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    balance_col = "account_balance",
    annuity_factor = 20
  )
  
  result <- compute_pension(dt, "dc", params)
  
  expect_equal(result[1], 25000)
})

test_that("compute_pension dispatches to flat correctly", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    flat_amount = 15000
  )
  
  result <- compute_pension(dt, "flat", params)
  
  expect_true(all(result == 15000))
})

test_that("compute_pension dispatches to hybrid correctly", {
  dt <- create_test_retirees_dt()
  
  params <- list(
    db_params = list(
      accrual_rate = 0.01,
      ref_wage_col = "gross_salary_lcu"
    ),
    dc_params = list(
      balance_col = "account_balance",
      annuity_factor = 20
    )
  )
  
  result <- compute_pension(dt, "hybrid", params)
  
  # P001: DB = 0.01 * 30 * 10000 = 3000, DC = 25000, Total = 28000
  expect_equal(result[1], 28000)
})

test_that("compute_pension rejects invalid policy type", {
  dt <- create_test_retirees_dt()
  
  params <- list(flat_amount = 10000)
  
  expect_error(
    compute_pension(dt, "invalid_type", params),
    "Unknown pension policy type: invalid_type"
  )
})
