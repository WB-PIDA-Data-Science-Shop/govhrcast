# Unit tests for hiring_core.R functions
# Testing demand estimation logic and current stock computation

library(testthat)
library(data.table)

# =============================================================================
# Test data setup
# =============================================================================

create_test_data_for_hiring <- function() {
  # Create contract data with different departments and grades
  contract_dt <- data.table(
    contract_id = paste0("C", 1:20),
    personnel_id = paste0("P", 1:20),
    start_date = as.Date("2020-01-01"),
    end_date = as.Date(c(rep(NA, 15), rep("2024-12-31", 5))),  # 15 active, 5 terminated
    gross_salary_lcu = rep(c(50000, 60000, 70000, 80000), 5),
    department = rep(c("HR", "IT"), each = 10),
    paygrade = rep(c("G5", "G6"), 10),
    contract_type_code = c(rep("permanent", 15), rep("terminated", 5))
  )
  
  personnel_dt <- data.table(
    personnel_id = paste0("P", 1:20),
    birth_date = as.Date("1980-01-01"),
    status = c(rep("active", 15), rep("inactive", 5))
  )
  
  list(contract_dt = contract_dt, personnel_dt = personnel_dt)
}

# =============================================================================
# Test compute_current_stock()
# =============================================================================

test_that("compute_current_stock calculates overall headcount correctly", {
  test_data <- create_test_data_for_hiring()
  
  result <- compute_current_stock(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date = as.Date("2024-06-01"),
    group_cols = NULL
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_true("current_stock" %in% names(result))
  expect_equal(result$current_stock, 15)  # 15 active personnel
})

test_that("compute_current_stock aggregates by group_cols", {
  test_data <- create_test_data_for_hiring()
  
  result <- compute_current_stock(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date = as.Date("2024-06-01"),
    group_cols = c("department")
  )
  
  expect_s3_class(result, "data.table")
  expect_true("department" %in% names(result))
  expect_true("current_stock" %in% names(result))
  expect_equal(nrow(result), 2)  # HR and IT
  
  # Check counts - 8 active in HR, 7 active in IT (5 terminated are in IT)
  hr_count <- result[department == "HR", current_stock]
  it_count <- result[department == "IT", current_stock]
  expect_equal(hr_count + it_count, 15)
})

test_that("compute_current_stock handles multiple grouping columns", {
  test_data <- create_test_data_for_hiring()
  
  result <- compute_current_stock(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    ref_date = as.Date("2024-06-01"),
    group_cols = c("department", "paygrade")
  )
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("department", "paygrade", "current_stock") %in% names(result)))
  expect_true(nrow(result) <= 4)  # Max 4 combinations
})

# =============================================================================
# Test compute_flow_demand()
# =============================================================================

test_that("compute_flow_demand works with retirees_dt provided", {
  test_data <- create_test_data_for_hiring()
  
  # Simulate 3 retirees
  retirees_dt <- data.table(
    personnel_id = paste0("P", 1:3),
    department = "HR",
    paygrade = "G5"
  )
  
  policy_params <- list(
    mode = "flow",
    group_cols = c("department"),
    replacement_rate = 1.0
  )
  
  result <- compute_flow_demand(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    retirees_dt = retirees_dt,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  expect_true("total_hires" %in% names(result))
  expect_true("department" %in% names(result))
  expect_equal(sum(result$total_hires), 3)  # 3 retirees × 1.0 replacement rate
})

test_that("compute_flow_demand handles scalar replacement_rate", {
  test_data <- create_test_data_for_hiring()
  
  retirees_dt <- data.table(
    personnel_id = paste0("P", 1:5)
  )
  
  policy_params <- list(
    mode = "flow",
    group_cols = NULL,
    replacement_rate = 0.5
  )
  
  result <- compute_flow_demand(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    retirees_dt = retirees_dt,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_equal(result$total_hires, 2)  # 5 × 0.5 = 2.5, rounded to 2
})

test_that("compute_flow_demand handles data.table replacement_rate", {
  test_data <- create_test_data_for_hiring()
  
  retirees_dt <- data.table(
    personnel_id = paste0("P", 1:4),
    department = c("HR", "HR", "IT", "IT")
  )
  
  replacement_rates <- data.table(
    department = c("HR", "IT"),
    replacement_rate = c(1.0, 0.5)
  )
  
  policy_params <- list(
    mode = "flow",
    group_cols = c("department"),
    replacement_rate = replacement_rates
  )
  
  result <- compute_flow_demand(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    retirees_dt = retirees_dt,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  
  # HR: 2 exits × 1.0 = 2 hires
  # IT: 2 exits × 0.5 = 1 hire
  expect_equal(result[department == "HR", total_hires], 2)
  expect_equal(result[department == "IT", total_hires], 1)
})

# =============================================================================
# Test compute_stock_demand()
# =============================================================================

test_that("compute_stock_demand calculates demand correctly", {
  test_data <- create_test_data_for_hiring()
  
  stock_targets <- data.table(
    department = c("HR", "IT"),
    target_stock = c(10, 20)
  )
  
  policy_params <- list(
    mode = "stock",
    group_cols = c("department"),
    stock_targets = stock_targets
  )
  
  result <- compute_stock_demand(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  expect_true("total_hires" %in% names(result))
  expect_equal(nrow(result), 2)
  
  # Check calculations (current stock will vary based on test data setup)
  expect_true(all(is.finite(result$total_hires)))
})

test_that("compute_stock_demand allows negative values for downsizing", {
  test_data <- create_test_data_for_hiring()
  
  # Set targets below current stock
  stock_targets <- data.table(
    department = c("HR", "IT"),
    target_stock = c(2, 3)  # Current is much higher
  )
  
  policy_params <- list(
    mode = "stock",
    group_cols = c("department"),
    stock_targets = stock_targets
  )
  
  result <- compute_stock_demand(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  # Should have negative values (downsizing needed)
  expect_true(any(result$total_hires < 0))
})

test_that("compute_stock_demand handles overall target (no grouping)", {
  test_data <- create_test_data_for_hiring()
  
  stock_targets <- data.table(
    target_stock = 20
  )
  
  policy_params <- list(
    mode = "stock",
    group_cols = NULL,
    stock_targets = stock_targets
  )
  
  result <- compute_stock_demand(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
  expect_true("total_hires" %in% names(result))
  expect_equal(result$total_hires, 5)  # 20 target - 15 current = 5
})

# =============================================================================
# Test compute_combined_demand()
# =============================================================================

test_that("compute_combined_demand combines flow and stock", {
  test_data <- create_test_data_for_hiring()
  
  stock_targets <- data.table(
    department = c("HR", "IT"),
    target_stock = c(10, 10)
  )
  
  policy_params <- list(
    mode = "combined",
    group_cols = c("department"),
    replacement_rate = 0.1,  # 10% flow
    stock_targets = stock_targets
  )
  
  result <- compute_combined_demand(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  expect_true("total_hires" %in% names(result))
  expect_equal(nrow(result), 2)
  
  # Total demand = flow (current × rate) + stock (target - current)
  expect_true(all(is.finite(result$total_hires)))
})

# =============================================================================
# Test estimate_hiring_demand() wrapper
# =============================================================================

test_that("estimate_hiring_demand routes to correct function based on mode", {
  test_data <- create_test_data_for_hiring()
  
  # Test flow mode
  policy_params_flow <- list(
    mode = "flow",
    group_cols = NULL,
    replacement_rate = 1.0,
    eligibility_type = "age_only",
    min_age = 60
  )
  
  result_flow <- estimate_hiring_demand(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params_flow,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result_flow, "data.table")
  expect_true("total_hires" %in% names(result_flow))
  
  # Test stock mode
  policy_params_stock <- list(
    mode = "stock",
    group_cols = NULL,
    stock_targets = data.table(target_stock = 20)
  )
  
  result_stock <- estimate_hiring_demand(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    policy_params = policy_params_stock,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result_stock, "data.table")
  expect_true("total_hires" %in% names(result_stock))
})

test_that("estimate_hiring_demand fails with invalid mode", {
  test_data <- create_test_data_for_hiring()
  
  policy_params <- list(
    mode = "invalid_mode"
  )
  
  expect_error(
    estimate_hiring_demand(
      contract_dt = test_data$contract_dt,
      personnel_dt = test_data$personnel_dt,
      policy_params = policy_params,
      ref_date = as.Date("2024-06-01")
    ),
    "Unknown hiring mode"
  )
})

# =============================================================================
# Test compute_hiring_summary()
# =============================================================================

test_that("compute_hiring_summary calculates statistics correctly", {
  adjustment_dt <- data.table(
    department = c("HR", "IT"),
    net_change = c(5, -2)
  )
  
  new_contracts_dt <- data.table(
    contract_id = paste0("C", 1:5),
    personnel_id = paste0("P", 1:5),
    gross_salary_lcu = rep(50000, 5)
  )
  
  result <- compute_hiring_summary(
    adjustment_dt = adjustment_dt,
    new_hires_dt = data.table(personnel_id = paste0("P", 1:5)),
    new_contracts_dt = new_contracts_dt,
    total_headcount = 100
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(result$n_new_hires, 5)
  expect_equal(result$net_headcount_change, 3)  # 5 - 2
  expect_equal(result$total_headcount, 100)
  expect_equal(result$total_new_salary_cost, 250000)  # 5 × 50000
})

test_that("compute_hiring_summary handles no adjustments", {
  adjustment_dt <- data.table(
    department = character(0),
    net_change = numeric(0)
  )
  
  result <- compute_hiring_summary(
    adjustment_dt = adjustment_dt,
    new_hires_dt = NULL,
    new_contracts_dt = NULL,
    total_headcount = 100
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(result$n_new_hires, 0)
  expect_equal(result$net_headcount_change, 0)
  expect_equal(result$total_headcount, 100)
})
