# Unit tests for R/hiring_policy_helpers.R

library(testthat)
library(data.table)

# =============================================================================
# design_hiring_policy_flow()
# =============================================================================

test_that("design_hiring_policy_flow builds the correct scalar-rate fragment", {
  result <- design_hiring_policy_flow(replacement_rate = 1.0)
  expect_equal(
    result,
    list(mode = "flow", group_cols = NULL, replacement_rate = 1.0, salary_scale = NULL)
  )
})

test_that("design_hiring_policy_flow accepts a group-varying replacement_rate table", {
  tbl <- data.table(est_id = c("E1", "E2"), replacement_rate = c(1.0, 0.5))
  result <- design_hiring_policy_flow(replacement_rate = tbl, group_cols = "est_id")
  expect_identical(result$replacement_rate, tbl)
  expect_equal(result$group_cols, "est_id")
})

test_that("design_hiring_policy_flow requires group_cols when replacement_rate is a data.table", {
  tbl <- data.table(est_id = "E1", replacement_rate = 1.0)
  expect_error(design_hiring_policy_flow(replacement_rate = tbl), regexp = "group_cols")
})

test_that("design_hiring_policy_flow requires a replacement_rate column in the table form", {
  tbl <- data.table(est_id = "E1", rate = 1.0)
  expect_error(
    design_hiring_policy_flow(replacement_rate = tbl, group_cols = "est_id"),
    regexp = "replacement_rate"
  )
})

test_that("design_hiring_policy_flow requires the table to contain group_cols columns", {
  tbl <- data.table(replacement_rate = 1.0)
  expect_error(
    design_hiring_policy_flow(replacement_rate = tbl, group_cols = "est_id"),
    regexp = "est_id"
  )
})

test_that("design_hiring_policy_flow validates a scalar replacement_rate", {
  expect_no_error(design_hiring_policy_flow(replacement_rate = 0))  # 0 is a valid, if inert, policy
  expect_error(design_hiring_policy_flow(replacement_rate = -1))
  expect_error(design_hiring_policy_flow(replacement_rate = "1"), regexp = "numeric")
})

test_that("design_hiring_policy_flow validates salary_scale type", {
  expect_error(design_hiring_policy_flow(replacement_rate = 1, salary_scale = data.frame(x = 1)),
              regexp = "data.table")
})

# =============================================================================
# design_hiring_policy_stock()
# =============================================================================

test_that("design_hiring_policy_stock builds the correct overall-target fragment", {
  tbl <- data.table(target_stock = 100)
  result <- design_hiring_policy_stock(stock_targets = tbl)
  expect_equal(
    result,
    list(mode = "stock", group_cols = NULL, stock_targets = tbl,
         salary_scale = NULL, removal_strategy = "last_hired_first")
  )
})

test_that("design_hiring_policy_stock accepts group-varying targets", {
  tbl <- data.table(est_id = c("E1", "E2"), target_stock = c(100, 150))
  result <- design_hiring_policy_stock(stock_targets = tbl, group_cols = "est_id")
  expect_identical(result$stock_targets, tbl)
})

test_that("design_hiring_policy_stock requires a target_stock column", {
  expect_error(
    design_hiring_policy_stock(stock_targets = data.table(est_id = "E1")),
    regexp = "target_stock"
  )
})

test_that("design_hiring_policy_stock requires exactly 1 row when group_cols is NULL", {
  expect_error(
    design_hiring_policy_stock(stock_targets = data.table(target_stock = c(100, 150))),
    regexp = "exactly 1 row"
  )
})

test_that("design_hiring_policy_stock requires the table to contain group_cols columns", {
  expect_error(
    design_hiring_policy_stock(stock_targets = data.table(target_stock = 100), group_cols = "est_id"),
    regexp = "est_id"
  )
})

test_that("design_hiring_policy_stock validates removal_strategy as a strict enum", {
  expect_no_error(design_hiring_policy_stock(data.table(target_stock = 1), removal_strategy = "random"))
  expect_error(
    design_hiring_policy_stock(data.table(target_stock = 1), removal_strategy = "seniority"),
    regexp = "Invalid removal_strategy"
  )
})

# =============================================================================
# design_hiring_policy_status_quo()
# =============================================================================

test_that("design_hiring_policy_status_quo builds the correct default fragment", {
  expect_equal(
    design_hiring_policy_status_quo(),
    list(mode = "status_quo", group_cols = NULL, rate_mult = 1, salary_scale = NULL)
  )
})

test_that("design_hiring_policy_status_quo accepts rate_mult and group_cols", {
  result <- design_hiring_policy_status_quo(rate_mult = 1.5, group_cols = "est_id")
  expect_equal(result$rate_mult, 1.5)
  expect_equal(result$group_cols, "est_id")
})

test_that("design_hiring_policy_status_quo validates rate_mult", {
  expect_no_error(design_hiring_policy_status_quo(rate_mult = 0))
  expect_error(design_hiring_policy_status_quo(rate_mult = -1))
})

test_that("design_hiring_policy_status_quo never asks for panel data or hire_date_col", {
  # These are auto-injected by simulate_horizon() -- confirm the function
  # signature genuinely has no such parameters (would error if passed).
  expect_error(
    design_hiring_policy_status_quo(panel_contract_dt = data.table()),
    regexp = "unused argument"
  )
  expect_error(
    design_hiring_policy_status_quo(hire_date_col = "first_employment_date"),
    regexp = "unused argument"
  )
})

# =============================================================================
# Integration: constructed policies are true drop-ins
# =============================================================================

test_that("a constructed flow policy produces identical output to the equivalent hand-written list", {
  contract_dt <- data.table(
    contract_id = paste0("C", 1:5), personnel_id = paste0("P", 1:5),
    start_date = as.Date("2010-01-01"), end_date = as.Date(NA),
    contract_type = "permanent", gross_salary_lcu = 5000
  )
  personnel_dt <- data.table(personnel_id = paste0("P", 1:5), employment_status = "active")
  ref_date <- as.Date("2025-01-01")
  retirees_dt <- data.table(personnel_id = c("P1", "P2"))

  constructed <- design_hiring_policy_flow(replacement_rate = 1.0, salary_scale = data.table(gross_salary_lcu = 5000))
  handwritten <- list(mode = "flow", group_cols = NULL, replacement_rate = 1.0,
                      salary_scale = data.table(gross_salary_lcu = 5000))

  res_c <- simulate_hiring(contract_dt, personnel_dt, policy_params = constructed, retirees_dt = retirees_dt, ref_date = ref_date)
  res_h <- simulate_hiring(contract_dt, personnel_dt, policy_params = handwritten, retirees_dt = retirees_dt, ref_date = ref_date)

  expect_identical(res_c$new_hire_contracts_dt, res_h$new_hire_contracts_dt)
  expect_equal(res_c$summary$n_new_hires, 2L)
})

test_that("design_hiring_policy_stock triggers downsizing correctly", {
  contract_dt <- data.table(
    contract_id = paste0("C", 1:5), personnel_id = paste0("P", 1:5),
    start_date = as.Date("2010-01-01"), end_date = as.Date(NA),
    contract_type = "permanent", gross_salary_lcu = 5000
  )
  personnel_dt <- data.table(personnel_id = paste0("P", 1:5), employment_status = "active")

  policy <- design_hiring_policy_stock(stock_targets = data.table(target_stock = 3), removal_strategy = "random")
  res <- simulate_hiring(contract_dt, personnel_dt, policy_params = policy, ref_date = as.Date("2025-01-01"))

  expect_equal(res$summary$n_new_hires, 0L)
  expect_equal(res$summary$net_headcount_change, -2L)
})

test_that("design_hiring_policy_status_quo works end-to-end through simulate_horizon()", {
  snap1 <- as.Date("2019-01-01"); snap2 <- as.Date("2020-01-01")
  contract_panel <- rbindlist(list(
    data.table(ref_date = snap1, contract_id = paste0("C", 1:4), personnel_id = paste0("P", 1:4), est_id = "E1",
               start_date = as.Date("2015-01-01"), end_date = as.Date(NA), contract_type = "permanent", gross_salary_lcu = 5000),
    data.table(ref_date = snap2, contract_id = paste0("C", 1:7), personnel_id = paste0("P", 1:7), est_id = "E1",
               start_date = as.Date("2015-01-01"), end_date = as.Date(NA), contract_type = "permanent", gross_salary_lcu = 5000)
  ))
  personnel_panel <- rbindlist(list(
    data.table(ref_date = snap1, personnel_id = paste0("P", 1:4), employment_status = "active"),
    data.table(ref_date = snap2, personnel_id = paste0("P", 1:7), employment_status = "active")
  ))
  salary_scale <- data.table(est_id = "E1", gross_salary_lcu = 5000)

  policy <- design_hiring_policy_status_quo(group_cols = "est_id")
  res <- simulate_horizon(
    contract_dt = contract_panel, personnel_dt = personnel_panel, salary_scale_dt = salary_scale,
    n_periods = 1, retirement_policy = NULL, hiring_policy = policy, ref_date = snap2
  )

  expect_gt(res$comparison$n_hires[1], 0L)
})
