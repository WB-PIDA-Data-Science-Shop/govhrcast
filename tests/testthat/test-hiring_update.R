# Unit tests for hiring_update.R functions
# Testing state modification functions

library(testthat)
library(data.table)

# =============================================================================
# Test data setup
# =============================================================================

create_test_data_for_update <- function() {
  contract_dt <- data.table(
    contract_id = paste0("C", 1:10),
    personnel_id = paste0("P", 1:10),
    start_date = as.Date("2020-01-01"),
    end_date = as.Date(NA),
    gross_salary_lcu = seq(50000, 95000, by = 5000),
    department = rep(c("HR", "IT"), 5),
    paygrade = rep(c("G5", "G6"), each = 5),
    contract_type_code = "permanent"
  )
  
  personnel_dt <- data.table(
    personnel_id = paste0("P", 1:10),
    birth_date = as.Date("1980-01-01"),
    status = "active"
  )
  
  list(contract_dt = contract_dt, personnel_dt = personnel_dt)
}

# =============================================================================
# Test generate_new_personnel()
# =============================================================================

test_that("generate_new_personnel creates correct number of records", {
  result <- generate_new_personnel(
    n = 5,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 5)
  expect_true("personnel_id" %in% names(result))
  expect_true("status" %in% names(result))
  expect_true(all(result$status == "active"))
  expect_true(all(grepl("^P_", result$personnel_id)))
})

test_that("generate_new_personnel adds group values", {
  result <- generate_new_personnel(
    n = 3,
    ref_date = as.Date("2024-06-01"),
    group_vals = list(department = "HR", paygrade = "G5")
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_true("department" %in% names(result))
  expect_true("paygrade" %in% names(result))
  expect_true(all(result$department == "HR"))
  expect_true(all(result$paygrade == "G5"))
})

test_that("generate_new_personnel returns empty for n <= 0", {
  result <- generate_new_personnel(
    n = 0,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("generate_new_personnel respects custom column names", {
  result <- generate_new_personnel(
    n = 2,
    ref_date = as.Date("2024-06-01"),
    personnel_id_col = "emp_id",
    status_col = "emp_status"
  )
  
  expect_true("emp_id" %in% names(result))
  expect_true("emp_status" %in% names(result))
  expect_false("personnel_id" %in% names(result))
})

# =============================================================================
# Test generate_new_contracts()
# =============================================================================

test_that("generate_new_contracts creates correct number of records", {
  personnel_ids <- paste0("P_NEW_", 1:5)
  
  result <- generate_new_contracts(
    personnel_ids = personnel_ids,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 5)
  expect_true(all(c("contract_id", "personnel_id", "start_date", 
                    "end_date", "contract_type_code") %in% names(result)))
  expect_true(all(result$start_date == as.Date("2024-06-01")))
  expect_true(all(is.na(result$end_date)))
  expect_true(all(result$contract_type_code == "permanent"))
})

test_that("generate_new_contracts adds group values", {
  personnel_ids <- paste0("P_NEW_", 1:3)
  
  result <- generate_new_contracts(
    personnel_ids = personnel_ids,
    ref_date = as.Date("2024-06-01"),
    group_vals = list(department = "IT", paygrade = "G6")
  )
  
  expect_true("department" %in% names(result))
  expect_true("paygrade" %in% names(result))
  expect_true(all(result$department == "IT"))
  expect_true(all(result$paygrade == "G6"))
})

test_that("generate_new_contracts returns empty for empty personnel_ids", {
  result <- generate_new_contracts(
    personnel_ids = character(0),
    ref_date = as.Date("2024-06-01")
  )
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

# =============================================================================
# Test assign_compensation()
# =============================================================================

test_that("assign_compensation merges salary scale correctly", {
  new_contracts <- data.table(
    contract_id = paste0("C", 1:4),
    personnel_id = paste0("P", 1:4),
    department = c("HR", "HR", "IT", "IT"),
    paygrade = c("G5", "G6", "G5", "G6")
  )
  
  salary_scale <- data.table(
    department = c("HR", "HR", "IT", "IT"),
    paygrade = c("G5", "G6", "G5", "G6"),
    gross_salary_lcu = c(50000, 60000, 55000, 65000)
  )
  
  result <- assign_compensation(
    new_contracts_dt = new_contracts,
    salary_scale_dt = salary_scale,
    join_cols = c("department", "paygrade")
  )
  
  expect_s3_class(result, "data.table")
  expect_true("gross_salary_lcu" %in% names(result))
  expect_false(any(is.na(result$gross_salary_lcu)))
  expect_equal(result[department == "HR" & paygrade == "G5", gross_salary_lcu], 50000)
  expect_equal(result[department == "IT" & paygrade == "G6", gross_salary_lcu], 65000)
})

test_that("assign_compensation warns about unmatched rows", {
  new_contracts <- data.table(
    contract_id = "C1",
    personnel_id = "P1",
    department = "Finance"  # Not in salary scale
  )
  
  salary_scale <- data.table(
    department = c("HR", "IT"),
    gross_salary_lcu = c(50000, 60000)
  )
  
  # Should issue warning, not error
  expect_warning(
    result <- assign_compensation(
      new_contracts_dt = new_contracts,
      salary_scale_dt = salary_scale,
      join_cols = "department"
    ),
    "could not be matched to salary_scale"
  )
  
  # Result should have NA for unmatched salary
  expect_true(is.na(result$gross_salary_lcu[1]))
})

test_that("assign_compensation validates join columns exist", {
  new_contracts <- data.table(
    contract_id = "C1",
    personnel_id = "P1"
  )
  
  salary_scale <- data.table(
    department = "HR",
    gross_salary_lcu = 50000
  )
  
  expect_error(
    assign_compensation(
      new_contracts_dt = new_contracts,
      salary_scale_dt = salary_scale,
      join_cols = "department"
    ),
    "Join columns not found in new_contracts_dt"
  )
})

# =============================================================================
# Test select_personnel_to_remove()
# =============================================================================

test_that("select_personnel_to_remove with last_hired_first strategy", {
  test_data <- create_test_data_for_update()
  
  # Modify dates so we have clear last hired
  test_data$contract_dt[1:3, start_date := as.Date("2024-01-01")]
  test_data$contract_dt[4:10, start_date := as.Date("2020-01-01")]
  
  result <- select_personnel_to_remove(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    n_remove = 3,
    strategy = "last_hired_first",
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(result, "character")
  expect_equal(length(result), 3)
  # Should select P1, P2, P3 (most recent hires)
  expect_true(all(result %in% paste0("P", 1:3)))
})

test_that("select_personnel_to_remove respects group filtering", {
  test_data <- create_test_data_for_update()
  
  result <- select_personnel_to_remove(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    n_remove = 2,
    strategy = "last_hired_first",
    group_vals = list(department = "HR"),
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(result, "character")
  expect_equal(length(result), 2)
  
  # Verify selected personnel are from HR
  selected_dept <- test_data$contract_dt[personnel_id %in% result, unique(department)]
  expect_true(all(selected_dept == "HR"))
})

test_that("select_personnel_to_remove with random strategy", {
  test_data <- create_test_data_for_update()
  
  result <- select_personnel_to_remove(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    n_remove = 3,
    strategy = "random",
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(result, "character")
  expect_equal(length(result), 3)
  expect_true(all(result %in% test_data$personnel_dt$personnel_id))
})

test_that("select_personnel_to_remove returns empty for n_remove <= 0", {
  test_data <- create_test_data_for_update()
  
  result <- select_personnel_to_remove(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    n_remove = 0,
    strategy = "last_hired_first",
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(result, "character")
  expect_equal(length(result), 0)
})

# =============================================================================
# Test update_state_with_adjustment()
# =============================================================================

test_that("update_state_with_adjustment handles hiring (positive net_change)", {
  test_data <- create_test_data_for_update()
  
  adjustment_dt <- data.table(
    department = "HR",
    net_change = 3
  )
  
  salary_scale <- data.table(
    department = "HR",
    gross_salary_lcu = 50000
  )
  
  policy_params <- list(
    group_cols = c("department"),
    salary_scale = salary_scale
  )
  
  initial_n_personnel <- nrow(test_data$personnel_dt)
  initial_n_contracts <- nrow(test_data$contract_dt)
  
  result <- update_state_with_adjustment(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    adjustment_dt = adjustment_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_type(result, "list")
  expect_true("contract_dt" %in% names(result))
  expect_true("personnel_dt" %in% names(result))
  expect_true("new_personnel_dt" %in% names(result))
  expect_true("new_contracts_dt" %in% names(result))
  
  # Check new hires were added
  expect_equal(nrow(result$new_personnel_dt), 3)
  expect_equal(nrow(result$new_contracts_dt), 3)
  
  # Verify state was updated (returned objects have new rows)
  expect_equal(nrow(result$personnel_dt), initial_n_personnel + 3)
  expect_equal(nrow(result$contract_dt), initial_n_contracts + 3)
  
  # Check salary was assigned
  expect_true(all(result$new_contracts_dt$gross_salary_lcu == 50000))
})

test_that("update_state_with_adjustment handles downsizing (negative net_change)", {
  test_data <- create_test_data_for_update()
  
  adjustment_dt <- data.table(
    department = "IT",
    net_change = -2
  )
  
  policy_params <- list(
    group_cols = c("department"),
    salary_scale = NULL,
    removal_strategy = "last_hired_first"
  )
  
  initial_n_active <- test_data$personnel_dt[status == "active", .N]
  
  result <- update_state_with_adjustment(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    adjustment_dt = adjustment_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  # Check no new hires
  expect_equal(nrow(result$new_personnel_dt), 0)
  expect_equal(nrow(result$new_contracts_dt), 0)
  
  # Verify personnel were deactivated
  final_n_active <- test_data$personnel_dt[status == "active", .N]
  expect_equal(final_n_active, initial_n_active - 2)
  
  # Verify contracts were terminated
  n_terminated <- test_data$contract_dt[contract_type_code == "terminated", .N]
  expect_equal(n_terminated, 2)
})

test_that("update_state_with_adjustment handles multiple groups", {
  test_data <- create_test_data_for_update()
  
  adjustment_dt <- data.table(
    department = c("HR", "IT"),
    net_change = c(2, -1)
  )
  
  salary_scale <- data.table(
    department = c("HR", "IT"),
    gross_salary_lcu = c(50000, 60000)
  )
  
  policy_params <- list(
    group_cols = c("department"),
    salary_scale = salary_scale,
    removal_strategy = "last_hired_first"
  )
  
  initial_n_personnel <- nrow(test_data$personnel_dt)
  
  result <- update_state_with_adjustment(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    adjustment_dt = adjustment_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  # Total rows increase by 2 (new hires), downsizing doesn't remove rows
  expect_equal(nrow(result$personnel_dt), initial_n_personnel + 2)
  
  # Net active personnel: +2 hires, -1 downsized = +1
  n_active <- result$personnel_dt[status == "active", .N]
  expect_equal(n_active, initial_n_personnel + 1)
  
  # Check 2 hires in HR
  expect_equal(nrow(result$new_personnel_dt), 2)
  expect_true(all(result$new_contracts_dt$department == "HR"))
})

test_that("update_state_with_adjustment handles zero net_change", {
  test_data <- create_test_data_for_update()
  
  adjustment_dt <- data.table(
    department = "HR",
    net_change = 0
  )
  
  policy_params <- list(
    group_cols = c("department"),
    salary_scale = NULL
  )
  
  initial_n_personnel <- nrow(test_data$personnel_dt)
  initial_n_contracts <- nrow(test_data$contract_dt)
  
  result <- update_state_with_adjustment(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    adjustment_dt = adjustment_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  # No changes should occur
  expect_equal(nrow(test_data$personnel_dt), initial_n_personnel)
  expect_equal(nrow(test_data$contract_dt), initial_n_contracts)
  expect_equal(nrow(result$new_personnel_dt), 0)
})

test_that("update_state_with_adjustment handles overall adjustment (no grouping)", {
  test_data <- create_test_data_for_update()
  
  adjustment_dt <- data.table(
    net_change = 5
  )
  
  salary_scale <- data.table(
    gross_salary_lcu = 55000
  )
  
  policy_params <- list(
    group_cols = NULL,
    salary_scale = salary_scale
  )
  
  initial_n_personnel <- nrow(test_data$personnel_dt)
  
  result <- update_state_with_adjustment(
    contract_dt = test_data$contract_dt,
    personnel_dt = test_data$personnel_dt,
    adjustment_dt = adjustment_dt,
    policy_params = policy_params,
    ref_date = as.Date("2024-06-01")
  )
  
  expect_equal(nrow(result$new_personnel_dt), 5)
  expect_equal(nrow(result$personnel_dt), initial_n_personnel + 5)
  expect_true(all(result$new_contracts_dt$gross_salary_lcu == 55000))
})
