# Unit tests for retirement_pension.R functions
# Testing all pension calculation methods with edge cases
#
# NOTE: In the new unified design, pension parameters (accrual_rate, ref_wage_col,
# flat_amount, balance_col, annuity_factor, etc.) are columns on the retirees_dt
# data.table -- resolved upstream by resolve_policy_table().  Sub-functions
# compute_db_pension(), compute_dc_pension(), compute_flat_pension(), and
# compute_hybrid_pension() accept only `dt`; compute_pension() dispatches on
# the `pension_type` column.

library(testthat)
library(data.table)

# =============================================================================
# Test data setup helpers
# =============================================================================

# Base retirees_dt with DB param columns
create_db_retirees_dt <- function(accrual_rate    = 0.02,
                                  ref_wage_col    = "gross_salary_lcu",
                                  max_years       = NA_real_,
                                  replacement_cap = NA_real_) {
  data.table(
    personnel_id     = c("P001", "P002", "P003", "P004"),
    contract_id      = c("C001", "C002", "C003", "C004"),
    gross_salary_lcu = c(10000,   8000,  12000,   9000),
    tenure_years     = c(30,      20,     35,      15),
    age              = c(65,      60,     68,      62),
    pension_type     = "db",
    accrual_rate     = accrual_rate,
    ref_wage_col     = ref_wage_col,
    max_years        = max_years,
    replacement_cap  = replacement_cap
  )
}

# Base retirees_dt with DC param columns
create_dc_retirees_dt <- function(balance_col    = "account_balance",
                                  annuity_factor = 20,
                                  notional_rate  = NA_real_) {
  data.table(
    personnel_id     = c("P001", "P002", "P003", "P004"),
    contract_id      = c("C001", "C002", "C003", "C004"),
    account_balance  = c(500000, 400000, 600000, 300000),
    notional_balance = c(450000, 380000, 570000, 285000),
    age              = c(65, 60, 68, 62),
    pension_type     = "dc",
    balance_col      = balance_col,
    annuity_factor   = annuity_factor,
    notional_rate    = notional_rate
  )
}

# Base retirees_dt with flat param columns
create_flat_retirees_dt <- function(flat_amount = 15000) {
  data.table(
    personnel_id     = c("P001", "P002", "P003", "P004"),
    contract_id      = c("C001", "C002", "C003", "C004"),
    gross_salary_lcu = c(10000,   8000,  12000,   9000),
    tenure_years     = c(30,      20,     35,      15),
    age              = c(65,      60,     68,      62),
    pension_type     = "flat",
    flat_amount      = flat_amount
  )
}

# Base retirees_dt with hybrid param columns (DB + DC)
create_hybrid_retirees_dt <- function(accrual_rate    = 0.015,
                                      ref_wage_col    = "gross_salary_lcu",
                                      max_years       = NA_real_,
                                      replacement_cap = NA_real_,
                                      balance_col     = "account_balance",
                                      annuity_factor  = 25,
                                      notional_rate   = NA_real_) {
  data.table(
    personnel_id     = c("P001", "P002", "P003", "P004"),
    contract_id      = c("C001", "C002", "C003", "C004"),
    gross_salary_lcu = c(10000,   8000,  12000,   9000),
    account_balance  = c(500000, 400000, 600000, 300000),
    tenure_years     = c(30,      20,     35,      15),
    age              = c(65,      60,     68,      62),
    pension_type     = "hybrid",
    accrual_rate     = accrual_rate,
    ref_wage_col     = ref_wage_col,
    max_years        = max_years,
    replacement_cap  = replacement_cap,
    balance_col      = balance_col,
    annuity_factor   = annuity_factor,
    notional_rate    = notional_rate
  )
}

# =============================================================================
# compute_db_pension()
# =============================================================================

test_that("compute_db_pension calculates correctly without caps", {
  dt <- create_db_retirees_dt()
  result <- compute_db_pension(dt)
  expect_equal(result[1], 6000)   # 0.02 * 30 * 10000
  expect_equal(result[2], 3200)   # 0.02 * 20 * 8000
  expect_equal(result[3], 8400)   # 0.02 * 35 * 12000
  expect_equal(result[4], 2700)   # 0.02 * 15 * 9000
})

test_that("compute_db_pension applies max_years cap", {
  dt <- create_db_retirees_dt(max_years = 25)
  result <- compute_db_pension(dt)
  expect_equal(result[1], 5000)   # 0.02 * 25 * 10000
  expect_equal(result[2], 3200)   # 0.02 * 20 * 8000 (under cap)
  expect_equal(result[3], 6000)   # 0.02 * 25 * 12000
})

test_that("compute_db_pension applies replacement_cap", {
  dt <- create_db_retirees_dt(replacement_cap = 0.50)
  result <- compute_db_pension(dt)
  expect_equal(result[1], 5000)   # min(6000, 5000)
  expect_equal(result[2], 3200)   # min(3200, 4000)
  expect_equal(result[3], 6000)   # min(8400, 6000)
})

test_that("compute_db_pension applies both caps correctly", {
  dt <- create_db_retirees_dt(max_years = 25, replacement_cap = 0.40)
  result <- compute_db_pension(dt)
  expect_equal(result[1], 4000)   # min(0.02*25*10000, 0.40*10000) = min(5000,4000)
  expect_equal(result[3], 4800)   # min(0.02*25*12000, 0.40*12000) = min(6000,4800)
})

test_that("compute_db_pension handles NA salaries", {
  dt <- data.table(
    personnel_id     = c("P001", "P002"),
    gross_salary_lcu = c(10000, NA),
    tenure_years     = c(25, 20),
    pension_type     = "db",
    accrual_rate     = 0.02,
    ref_wage_col     = "gross_salary_lcu",
    max_years        = NA_real_,
    replacement_cap  = NA_real_
  )
  result <- compute_db_pension(dt)
  expect_equal(result[1], 5000)
  expect_true(is.na(result[2]))
})

test_that("compute_db_pension handles NA tenure", {
  dt <- data.table(
    personnel_id     = c("P001", "P002"),
    gross_salary_lcu = c(10000, 8000),
    tenure_years     = c(25, NA),
    pension_type     = "db",
    accrual_rate     = 0.02,
    ref_wage_col     = "gross_salary_lcu",
    max_years        = NA_real_,
    replacement_cap  = NA_real_
  )
  result <- compute_db_pension(dt)
  expect_equal(result[1], 5000)
  expect_true(is.na(result[2]))
})

test_that("compute_db_pension errors when ref_wage_col is NA", {
  dt <- create_db_retirees_dt()
  dt[, ref_wage_col := NA_character_]
  expect_error(compute_db_pension(dt), "ref_wage_col")
})

test_that("compute_db_pension errors when accrual_rate column absent", {
  dt <- create_db_retirees_dt()
  dt[, accrual_rate := NULL]
  expect_error(compute_db_pension(dt), "accrual_rate")
})

test_that("compute_db_pension handles empty data.table", {
  dt <- data.table(
    personnel_id     = character(),
    gross_salary_lcu = numeric(),
    tenure_years     = numeric(),
    pension_type     = character(),
    accrual_rate     = numeric(),
    ref_wage_col     = character(),
    max_years        = numeric(),
    replacement_cap  = numeric()
  )
  result <- compute_db_pension(dt)
  expect_equal(length(result), 0)
})

# =============================================================================
# compute_dc_pension()
# =============================================================================

test_that("compute_dc_pension calculates standard DC correctly", {
  dt <- create_dc_retirees_dt()
  result <- compute_dc_pension(dt)
  expect_equal(result[1], 25000)   # 500000 / 20
  expect_equal(result[2], 20000)   # 400000 / 20
  expect_equal(result[3], 30000)   # 600000 / 20
  expect_equal(result[4], 15000)   # 300000 / 20
})

test_that("compute_dc_pension calculates NDC (notional_rate) correctly", {
  dt <- create_dc_retirees_dt(
    balance_col    = "notional_balance",
    annuity_factor = 18,
    notional_rate  = 0.02
  )
  result <- compute_dc_pension(dt)
  # P001: (450000 * 1.02) / 18 = 459000 / 18 = 25500
  expect_equal(result[1], 25500)
  expect_equal(result[2], 387600 / 18)
})

test_that("compute_dc_pension handles NA balances", {
  dt <- data.table(
    personnel_id    = c("P001", "P002"),
    account_balance = c(500000, NA),
    pension_type    = "dc",
    balance_col     = "account_balance",
    annuity_factor  = 20,
    notional_rate   = NA_real_
  )
  result <- compute_dc_pension(dt)
  expect_equal(result[1], 25000)
  expect_true(is.na(result[2]))
})

test_that("compute_dc_pension errors when balance_col is NA", {
  dt <- create_dc_retirees_dt()
  dt[, balance_col := NA_character_]
  expect_error(compute_dc_pension(dt), "balance_col")
})

test_that("compute_dc_pension errors when annuity_factor column absent", {
  dt <- create_dc_retirees_dt()
  dt[, annuity_factor := NULL]
  expect_error(compute_dc_pension(dt), "annuity_factor")
})

test_that("compute_dc_pension handles empty data.table", {
  dt <- data.table(
    personnel_id    = character(),
    account_balance = numeric(),
    pension_type    = character(),
    balance_col     = character(),
    annuity_factor  = numeric(),
    notional_rate   = numeric()
  )
  result <- compute_dc_pension(dt)
  expect_equal(length(result), 0)
})

# =============================================================================
# compute_flat_pension()
# =============================================================================

test_that("compute_flat_pension assigns uniform amount", {
  dt <- create_flat_retirees_dt(flat_amount = 15000)
  result <- compute_flat_pension(dt)
  expect_equal(length(result), 4)
  expect_true(all(result == 15000))
})

test_that("compute_flat_pension works with single retiree", {
  dt <- data.table(
    personnel_id = "P001",
    pension_type = "flat",
    flat_amount  = 12500
  )
  result <- compute_flat_pension(dt)
  expect_equal(result, 12500)
})

test_that("compute_flat_pension errors when flat_amount column absent", {
  dt <- create_flat_retirees_dt()
  dt[, flat_amount := NULL]
  expect_error(compute_flat_pension(dt), "flat_amount")
})

test_that("compute_flat_pension errors when flat_amount is all NA", {
  dt <- create_flat_retirees_dt()
  dt[, flat_amount := NA_real_]
  expect_error(compute_flat_pension(dt), "flat_amount")
})

test_that("compute_flat_pension handles empty data.table", {
  dt <- data.table(
    personnel_id = character(),
    pension_type = character(),
    flat_amount  = numeric()
  )
  result <- compute_flat_pension(dt)
  expect_equal(length(result), 0)
})

test_that("compute_flat_pension handles zero amount", {
  dt <- create_flat_retirees_dt(flat_amount = 0)
  result <- compute_flat_pension(dt)
  expect_true(all(result == 0))
})

# =============================================================================
# compute_hybrid_pension()
# =============================================================================

test_that("compute_hybrid_pension combines DB and DC correctly", {
  dt <- create_hybrid_retirees_dt(
    accrual_rate   = 0.015,
    ref_wage_col   = "gross_salary_lcu",
    balance_col    = "account_balance",
    annuity_factor = 25
  )
  result <- compute_hybrid_pension(dt)
  # P001: DB = 0.015 * 30 * 10000 = 4500; DC = 500000 / 25 = 20000; Total = 24500
  expect_equal(result[1], 24500)
  # P002: DB = 0.015 * 20 * 8000 = 2400; DC = 400000 / 25 = 16000; Total = 18400
  expect_equal(result[2], 18400)
})

test_that("compute_hybrid_pension handles NA in DB component", {
  dt <- data.table(
    personnel_id     = c("P001", "P002"),
    gross_salary_lcu = c(10000, NA),
    account_balance  = c(500000, 400000),
    tenure_years     = c(25, 20),
    pension_type     = "hybrid",
    accrual_rate     = 0.02,
    ref_wage_col     = "gross_salary_lcu",
    max_years        = NA_real_,
    replacement_cap  = NA_real_,
    balance_col      = "account_balance",
    annuity_factor   = 20,
    notional_rate    = NA_real_
  )
  result <- compute_hybrid_pension(dt)
  expect_equal(result[1], 30000)   # DB=5000, DC=25000
  expect_true(is.na(result[2]))    # DB=NA, DC=20000 -> NA
})

test_that("compute_hybrid_pension handles NA in DC component", {
  dt <- data.table(
    personnel_id     = c("P001", "P002"),
    gross_salary_lcu = c(10000, 8000),
    account_balance  = c(500000, NA),
    tenure_years     = c(25, 20),
    pension_type     = "hybrid",
    accrual_rate     = 0.02,
    ref_wage_col     = "gross_salary_lcu",
    max_years        = NA_real_,
    replacement_cap  = NA_real_,
    balance_col      = "account_balance",
    annuity_factor   = 20,
    notional_rate    = NA_real_
  )
  result <- compute_hybrid_pension(dt)
  expect_equal(result[1], 30000)   # DB=5000, DC=25000
  expect_true(is.na(result[2]))    # DB=3200, DC=NA -> NA
})

test_that("compute_hybrid_pension errors when ref_wage_col missing", {
  dt <- create_hybrid_retirees_dt()
  dt[, ref_wage_col := NA_character_]
  expect_error(compute_hybrid_pension(dt), "ref_wage_col")
})

test_that("compute_hybrid_pension errors when balance_col missing", {
  dt <- create_hybrid_retirees_dt()
  dt[, balance_col := NA_character_]
  expect_error(compute_hybrid_pension(dt), "balance_col")
})

test_that("compute_hybrid_pension handles empty data.table", {
  dt <- data.table(
    personnel_id     = character(),
    gross_salary_lcu = numeric(),
    account_balance  = numeric(),
    tenure_years     = numeric(),
    pension_type     = character(),
    accrual_rate     = numeric(),
    ref_wage_col     = character(),
    max_years        = numeric(),
    replacement_cap  = numeric(),
    balance_col      = character(),
    annuity_factor   = numeric(),
    notional_rate    = numeric()
  )
  result <- compute_hybrid_pension(dt)
  expect_equal(length(result), 0)
})

# =============================================================================
# compute_pension() - dispatcher
# =============================================================================

test_that("compute_pension dispatches to DB correctly", {
  dt <- create_db_retirees_dt()
  result <- compute_pension(dt)
  expect_equal(result[1], 6000)
})

test_that("compute_pension dispatches to DC correctly", {
  dt <- create_dc_retirees_dt()
  result <- compute_pension(dt)
  expect_equal(result[1], 25000)
})

test_that("compute_pension dispatches to flat correctly", {
  dt <- create_flat_retirees_dt(flat_amount = 15000)
  result <- compute_pension(dt)
  expect_true(all(result == 15000))
})

test_that("compute_pension dispatches to hybrid correctly", {
  dt <- create_hybrid_retirees_dt(accrual_rate = 0.01, annuity_factor = 20)
  result <- compute_pension(dt)
  # P001: DB = 0.01 * 30 * 10000 = 3000; DC = 500000 / 20 = 25000; Total = 28000
  expect_equal(result[1], 28000)
})

test_that("compute_pension handles mixed pension types in one cohort", {
  dt_db   <- create_db_retirees_dt()[1:2]
  dt_flat <- create_flat_retirees_dt(flat_amount = 5000)[1:2]
  all_cols <- union(names(dt_db), names(dt_flat))
  for (col in setdiff(all_cols, names(dt_db)))  dt_db[[col]]   <- NA
  for (col in setdiff(all_cols, names(dt_flat))) dt_flat[[col]] <- NA
  dt <- rbind(dt_db, dt_flat)

  result <- compute_pension(dt)

  expect_equal(result[1], 6000)   # DB: 0.02 * 30 * 10000
  expect_equal(result[2], 3200)   # DB: 0.02 * 20 * 8000
  expect_equal(result[3], 5000)   # flat
  expect_equal(result[4], 5000)   # flat
})

test_that("compute_pension rejects invalid policy type", {
  dt <- create_db_retirees_dt()[1]
  dt[, pension_type := "invalid_type"]
  expect_error(compute_pension(dt), "Unknown pension policy type: invalid_type")
})
