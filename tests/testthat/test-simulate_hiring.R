# Unit tests for simulate_hiring.R
# Testing the main orchestrator function

library(testthat)
library(data.table)

# =============================================================================
# Test data setup
# =============================================================================

create_complete_test_data <- function() {
  contract_dt <- data.table(
    contract_id = paste0("C", 1:30),
    personnel_id = paste0("P", 1:30),
    start_date = as.Date("2020-01-01"),
    end_date = as.Date(c(rep(NA, 25), rep("2023-12-31", 5))),
    gross_salary_lcu = rep(c(50000, 60000, 70000), 10),
    department = rep(c("HR", "IT", "Finance"), 10),
    paygrade = rep(c("G5", "G6"), 15),
    contract_type_code = c(rep("permanent", 25), rep("terminated", 5))
  )
  
  personnel_dt <- data.table(
    personnel_id = paste0("P", 1:30),
    birth_date = as.Date("1965-01-01"),  # Age 59 at ref_date 2024-06-01
    status = c(rep("active", 25), rep("inactive", 5))
  )
  
  list(contract_dt = contract_dt, personnel_dt = personnel_dt)
}

# =============================================================================
# Test simulate_hiring() with stock mode
# =============================================================================

test_that("simulate_hiring works with stock mode", {
  test_data <- create_complete_test_data()
  
  stock_targets <- data.table(
    department = c("HR", "IT", "Finance"),
    target_stock = c(10, 12, 8)
  )
  
  salary_scale <- data.table(
    department = c("HR", "IT", "Finance"),
    gross_salary_lcu = c(50000, 60000, 55000)
  )
  
  policy_params <- list(
    mode = "stock",
    group_cols = c("department"),
    stock_targets = stock_targets,
    salary_scale = salary_scale
  )
  
  results <- simulate_hiring(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(results, "list")
  expect_true(all(c("summary", "contract_dt", "personnel_dt", 
                    "adjustment_dt", "new_hires_dt") %in% names(results)))
  
  expect_s3_class(results$summary, "data.table")
  expect_s3_class(results$contract_dt, "data.table")
  expect_s3_class(results$personnel_dt, "data.table")
  expect_s3_class(results$adjustment_dt, "data.table")
  
  # Check summary structure
  expect_true(all(c("n_new_hires", "net_headcount_change", "total_headcount",
                    "total_new_salary_cost") %in% names(results$summary)))
})

test_that("simulate_hiring works with flow mode", {
  test_data <- create_complete_test_data()
  
  # Create retirees data
  retirees_dt <- data.table(
    personnel_id = paste0("P", 1:5),
    department = c("HR", "HR", "IT", "IT", "Finance")
  )
  
  salary_scale <- data.table(
    department = c("HR", "IT", "Finance"),
    gross_salary_lcu = c(50000, 60000, 55000)
  )
  
  policy_params <- list(
    mode = "flow",
    group_cols = c("department"),
    replacement_rate = 1.0,
    salary_scale = salary_scale
  )
  
  results <- simulate_hiring(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    retirees_dt = retirees_dt,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(results, "list")
  expect_s3_class(results$summary, "data.table")
  
  # Should hire to replace retirees
  expect_true(results$summary$n_new_hires > 0)
})

test_that("simulate_hiring works with combined mode", {
  test_data <- create_complete_test_data()
  
  stock_targets <- data.table(
    department = c("HR", "IT", "Finance"),
    target_stock = c(10, 15, 10)
  )
  
  salary_scale <- data.table(
    department = c("HR", "IT", "Finance"),
    gross_salary_lcu = c(50000, 60000, 55000)
  )
  
  policy_params <- list(
    mode = "combined",
    group_cols = c("department"),
    replacement_rate = 0.1,
    stock_targets = stock_targets,
    salary_scale = salary_scale
  )
  
  results <- simulate_hiring(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(results, "list")
  expect_s3_class(results$summary, "data.table")
  expect_true(all(is.finite(results$summary$n_new_hires)))
})

test_that("simulate_hiring handles no adjustments needed", {
  test_data <- create_complete_test_data()
  
  # Set targets equal to current stock
  stock_targets <- data.table(
    department = c("HR", "IT", "Finance"),
    target_stock = c(8, 9, 8)  # Approximately current levels
  )
  
  salary_scale <- data.table(
    department = c("HR", "IT", "Finance"),
    gross_salary_lcu = c(50000, 60000, 55000)
  )
  
  policy_params <- list(
    mode = "stock",
    group_cols = c("department"),
    stock_targets = stock_targets,
    salary_scale = salary_scale
  )
  
  results <- simulate_hiring(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(results, "list")
  expect_s3_class(results$summary, "data.table")
  
  # Should handle gracefully even if no hires needed
  expect_true(is.numeric(results$summary$n_new_hires))
})

test_that("simulate_hiring handles downsizing", {
  test_data <- create_complete_test_data()
  
  # Set very low targets
  stock_targets <- data.table(
    department = c("HR", "IT", "Finance"),
    target_stock = c(2, 3, 2)
  )
  
  policy_params <- list(
    mode = "stock",
    group_cols = c("department"),
    stock_targets = stock_targets,
    salary_scale = NULL,
    removal_strategy = "last_hired_first"
  )
  
  results <- simulate_hiring(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(results, "list")
  expect_s3_class(results$summary, "data.table")
  
  # Net headcount change should be negative
  expect_true(results$summary$net_headcount_change < 0)
  expect_equal(results$summary$n_new_hires, 0)  # No new hires in downsizing
})

test_that("simulate_hiring preserves input data (copy-once pattern)", {
  test_data <- create_complete_test_data()
  original_contract_nrow <- nrow(test_data$contract_dt)
  original_personnel_nrow <- nrow(test_data$personnel_dt)
  
  stock_targets <- data.table(
    department = c("HR", "IT", "Finance"),
    target_stock = c(10, 15, 10)
  )
  
  salary_scale <- data.table(
    department = c("HR", "IT", "Finance"),
    gross_salary_lcu = c(50000, 60000, 55000)
  )
  
  policy_params <- list(
    mode = "stock",
    group_cols = c("department"),
    stock_targets = stock_targets,
    salary_scale = salary_scale
  )
  
  results <- simulate_hiring(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  # Original data should be unchanged
  expect_equal(nrow(test_data$contract_dt), original_contract_nrow)
  expect_equal(nrow(test_data$personnel_dt), original_personnel_nrow)
  
  # Returned data should be modified
  expect_true(nrow(results$contract_dt) >= original_contract_nrow)
  expect_true(nrow(results$personnel_dt) >= original_personnel_nrow)
})

test_that("simulate_hiring works without grouping (overall)", {
  test_data <- create_complete_test_data()
  
  stock_targets <- data.table(
    target_stock = 30
  )
  
  salary_scale <- data.table(
    gross_salary_lcu = 55000
  )
  
  policy_params <- list(
    mode = "stock",
    group_cols = NULL,
    stock_targets = stock_targets,
    salary_scale = salary_scale
  )
  
  results <- simulate_hiring(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(results, "list")
  expect_s3_class(results$summary, "data.table")
  expect_true(results$summary$total_headcount >= 25)  # At least current stock
})

test_that("simulate_hiring integrates with retirement module output", {
  test_data <- create_complete_test_data()
  
  # Simulate some retirements first (simplified)
  retirees_dt <- data.table(
    personnel_id = paste0("P", 1:3),
    department = c("HR", "IT", "Finance"),
    pension = c(40000, 45000, 42000)
  )
  
  salary_scale <- data.table(
    department = c("HR", "IT", "Finance"),
    gross_salary_lcu = c(50000, 60000, 55000)
  )
  
  policy_params <- list(
    mode = "flow",
    group_cols = c("department"),
    replacement_rate = 1.0,
    salary_scale = salary_scale
  )
  
  results <- simulate_hiring(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    retirees_dt = retirees_dt,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(results, "list")
  expect_s3_class(results$summary, "data.table")
  
  # Should hire 3 people to replace retirees
  expect_equal(results$summary$n_new_hires, 3)
})

test_that("simulate_hiring handles panel data with ref_date_col", {
  # Create panel data
  contract_panel <- rbindlist(list(
    create_complete_test_data()$contract_dt[, ref_date := as.Date("2023-01-01")],
    create_complete_test_data()$contract_dt[, ref_date := as.Date("2024-01-01")]
  ))
  
  personnel_panel <- rbindlist(list(
    create_complete_test_data()$personnel_dt[, ref_date := as.Date("2023-01-01")],
    create_complete_test_data()$personnel_dt[, ref_date := as.Date("2024-01-01")]
  ))
  
  stock_targets <- data.table(
    target_stock = 30
  )
  
  salary_scale <- data.table(
    gross_salary_lcu = 55000
  )
  
  policy_params <- list(
    mode = "stock",
    group_cols = NULL,
    stock_targets = stock_targets,
    salary_scale = salary_scale
  )
  
  results <- simulate_hiring(
    contract_dt = contract_panel,
    personnel_dt = personnel_panel,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01"),
    ref_date_col = "ref_date"
  )
  
  expect_type(results, "list")
  expect_s3_class(results$summary, "data.table")
  
  # Should select 2024-01-01 snapshot (closest to but not after 2024-06-01)
  expect_true(all(results$contract_dt$ref_date == as.Date("2024-01-01")))
})

# =============================================================================
# Test validation through simulate_hiring()
# =============================================================================

test_that("simulate_hiring validates inputs correctly", {
  test_data <- create_complete_test_data()
  
  # Missing mode
  policy_params_invalid <- list(
    group_cols = c("department")
  )
  
  expect_error(
    simulate_hiring(
      contract_dt = test_data$contract_dt,
      personnel_dt = test_data$personnel_dt,
      policy_params = policy_params_invalid,
      ref_date = as.Date("2024-06-01")
    ),
    "must contain 'mode'"
  )
  
  # Invalid mode
  policy_params_invalid2 <- list(
    mode = "invalid_mode"
  )
  
  expect_error(
    simulate_hiring(
      contract_dt = test_data$contract_dt,
      personnel_dt = test_data$personnel_dt,
      policy_params = policy_params_invalid2,
      ref_date = as.Date("2024-06-01")
    ),
    "Invalid mode"
  )
})

test_that("simulate_hiring requires stock_targets for stock mode", {
  test_data <- create_complete_test_data()
  
  policy_params <- list(
    mode = "stock",
    group_cols = c("department"),
    salary_scale = data.table(department = "HR", gross_salary_lcu = 50000)
  )
  
  expect_error(
    simulate_hiring(
      contract_dt = test_data$contract_dt,
      personnel_dt = test_data$personnel_dt,
      policy_params = policy_params,
      ref_date = as.Date("2024-06-01")
    ),
    "stock_targets is required"
  )
})

test_that("simulate_hiring requires replacement_rate for flow mode", {
  test_data <- create_complete_test_data()
  
  policy_params <- list(
    mode = "flow",
    group_cols = c("department"),
    salary_scale = data.table(department = "HR", gross_salary_lcu = 50000)
  )
  
  expect_error(
    simulate_hiring(
      contract_dt = test_data$contract_dt,
      personnel_dt = test_data$personnel_dt,
      policy_params = policy_params,
      ref_date = as.Date("2024-06-01")
    ),
    "replacement_rate is required"
  )
})
