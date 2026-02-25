# Unit tests for validation.R functions
# Testing all validation helpers with edge cases and missing values

library(testthat)
library(data.table)

# =============================================================================
# validate_datatable()
# =============================================================================

test_that("validate_datatable accepts valid data.table", {
  dt <- data.table(a = 1:5, b = letters[1:5])
  expect_invisible(validate_datatable(dt, "test_dt"))
})

test_that("validate_datatable rejects non-data.table", {
  df <- data.frame(a = 1:5, b = letters[1:5])
  expect_error(validate_datatable(df, "test_dt"), "must be a data.table")
})

test_that("validate_datatable rejects empty data.table", {
  dt <- data.table(a = integer(), b = character())
  expect_error(validate_datatable(dt, "test_dt"), "cannot be empty")
})

test_that("validate_datatable rejects NULL", {
  expect_error(validate_datatable(NULL, "test_dt"), "must be a data.table")
})

# =============================================================================
# validate_column_exists()
# =============================================================================

test_that("validate_column_exists accepts existing column", {
  dt <- data.table(id = 1:5, name = letters[1:5])
  expect_invisible(validate_column_exists(dt, "id", "test_dt"))
})

test_that("validate_column_exists rejects missing column", {
  dt <- data.table(id = 1:5, name = letters[1:5])
  expect_error(
    validate_column_exists(dt, "missing_col", "test_dt"),
    "Column 'missing_col' not found in test_dt"
  )
})

test_that("validate_column_exists is case sensitive", {
  dt <- data.table(ID = 1:5, NAME = letters[1:5])
  expect_error(
    validate_column_exists(dt, "id", "test_dt"),
    "Column 'id' not found"
  )
})

# =============================================================================
# validate_columns_exist()
# =============================================================================

test_that("validate_columns_exist accepts all existing columns", {
  dt <- data.table(id = 1:5, name = letters[1:5], value = rnorm(5))
  expect_invisible(validate_columns_exist(dt, c("id", "name", "value"), "test_dt"))
})

test_that("validate_columns_exist rejects when some columns missing", {
  dt <- data.table(id = 1:5, name = letters[1:5])
  expect_error(
    validate_columns_exist(dt, c("id", "missing1", "missing2"), "test_dt"),
    "Columns not found in test_dt: missing1, missing2"
  )
})

test_that("validate_columns_exist handles empty column vector", {
  dt <- data.table(id = 1:5, name = letters[1:5])
  expect_invisible(validate_columns_exist(dt, character(), "test_dt"))
})

# =============================================================================
# validate_date_format()
# =============================================================================

test_that("validate_date_format accepts valid Date", {
  date <- as.Date("2025-01-01")
  result <- validate_date_format(date, "test_date")
  expect_true(inherits(result, "Date"))
  expect_equal(result, date)
})

test_that("validate_date_format accepts character string and converts", {
  result <- validate_date_format("2025-01-01", "test_date")
  expect_true(inherits(result, "Date"))
  expect_equal(result, as.Date("2025-01-01"))
})

test_that("validate_date_format rejects numeric", {
  expect_error(
    validate_date_format(19723, "test_date"),
    "must be a Date object"
  )
})

test_that("validate_date_format rejects vector of dates", {
  dates <- as.Date(c("2025-01-01", "2025-01-02"))
  expect_error(
    validate_date_format(dates, "test_date"),
    "must be a single Date value"
  )
})

test_that("validate_date_format rejects NA Date", {
  date <- as.Date(NA)
  expect_error(
    validate_date_format(date, "test_date"),
    "cannot be NA"
  )
})

# =============================================================================
# validate_positive_number()
# =============================================================================

test_that("validate_positive_number accepts positive number", {
  expect_invisible(validate_positive_number(10, "test_num"))
  expect_invisible(validate_positive_number(0.001, "test_num"))
})

test_that("validate_positive_number accepts zero when allow_zero=TRUE", {
  expect_invisible(validate_positive_number(0, "test_num", allow_zero = TRUE))
})

test_that("validate_positive_number rejects zero by default", {
  expect_error(
    validate_positive_number(0, "test_num"),
    "must be > 0"
  )
})

test_that("validate_positive_number rejects negative number", {
  expect_error(
    validate_positive_number(-5, "test_num"),
    "must be > 0"
  )
})

test_that("validate_positive_number rejects negative with allow_zero=TRUE", {
  expect_error(
    validate_positive_number(-5, "test_num", allow_zero = TRUE),
    "must be >= 0"
  )
})

test_that("validate_positive_number rejects character", {
  expect_error(
    validate_positive_number("10", "test_num"),
    "must be numeric"
  )
})

test_that("validate_positive_number rejects vector", {
  expect_error(
    validate_positive_number(c(1, 2, 3), "test_num"),
    "must be a single numeric value"
  )
})

test_that("validate_positive_number rejects NA", {
  expect_error(
    validate_positive_number(NA_real_, "test_num"),
    "cannot be NA"
  )
})

# =============================================================================
# validate_number_range()
# =============================================================================

test_that("validate_number_range accepts value in range", {
  expect_invisible(validate_number_range(50, "test_num", 0, 100))
  expect_invisible(validate_number_range(0, "test_num", 0, 100))  # min boundary
  expect_invisible(validate_number_range(100, "test_num", 0, 100))  # max boundary
})

test_that("validate_number_range rejects value below min", {
  expect_error(
    validate_number_range(-1, "test_num", 0, 100),
    "must be between 0 and 100"
  )
})

test_that("validate_number_range rejects value above max", {
  expect_error(
    validate_number_range(101, "test_num", 0, 100),
    "must be between 0 and 100"
  )
})

test_that("validate_number_range rejects non-numeric", {
  expect_error(
    validate_number_range("50", "test_num", 0, 100),
    "must be numeric"
  )
})

test_that("validate_number_range rejects NA", {
  expect_error(
    validate_number_range(NA_real_, "test_num", 0, 100),
    "cannot be NA"
  )
})

# =============================================================================
# validate_character_string()
# =============================================================================

test_that("validate_character_string accepts valid string", {
  expect_invisible(validate_character_string("hello", "test_str"))
})

test_that("validate_character_string rejects numeric", {
  expect_error(
    validate_character_string(123, "test_str"),
    "must be a single character string"
  )
})

test_that("validate_character_string rejects character vector", {
  expect_error(
    validate_character_string(c("a", "b"), "test_str"),
    "must be a single character string"
  )
})

test_that("validate_character_string rejects NA by default", {
  expect_error(
    validate_character_string(NA_character_, "test_str"),
    "cannot be NA"
  )
})

test_that("validate_character_string accepts NA when allow_na=TRUE", {
  expect_invisible(validate_character_string(NA_character_, "test_str", allow_na = TRUE))
})

test_that("validate_character_string accepts empty string", {
  expect_invisible(validate_character_string("", "test_str"))
})

# =============================================================================
# validate_choice()
# =============================================================================

test_that("validate_choice accepts valid choice", {
  expect_invisible(validate_choice("red", c("red", "blue", "green"), "color"))
})

test_that("validate_choice rejects invalid choice", {
  expect_error(
    validate_choice("yellow", c("red", "blue", "green"), "color"),
    "Invalid color: 'yellow'. Valid options: red, blue, green"
  )
})

test_that("validate_choice is case sensitive", {
  expect_error(
    validate_choice("RED", c("red", "blue", "green"), "color"),
    "Invalid color: 'RED'"
  )
})

test_that("validate_choice rejects numeric", {
  expect_error(
    validate_choice(1, c("red", "blue", "green"), "color"),
    "must be a single character string"
  )
})

test_that("validate_choice rejects character vector", {
  expect_error(
    validate_choice(c("red", "blue"), c("red", "blue", "green"), "color"),
    "must be a single character string"
  )
})

# =============================================================================
# validate_required_params()
# =============================================================================

test_that("validate_required_params accepts all required params", {
  params <- list(min_age = 60, min_tenure = 15, pension_type = "db")
  expect_invisible(
    validate_required_params(params, c("min_age", "min_tenure"), "test context")
  )
})

test_that("validate_required_params rejects missing params", {
  params <- list(min_age = 60)
  expect_error(
    validate_required_params(params, c("min_age", "min_tenure"), "test context"),
    "Missing required parameters for test context: min_tenure"
  )
})

test_that("validate_required_params handles multiple missing params", {
  params <- list(min_age = 60)
  expect_error(
    validate_required_params(params, c("min_age", "min_tenure", "pension_type"), "test context"),
    "Missing required parameters for test context: min_tenure, pension_type"
  )
})

test_that("validate_required_params rejects non-list", {
  expect_error(
    validate_required_params("not a list", c("min_age"), "test context"),
    "policy_params must be a list"
  )
})

test_that("validate_required_params accepts empty required params", {
  params <- list(min_age = 60, min_tenure = 15)
  expect_invisible(
    validate_required_params(params, character(), "test context")
  )
})

# =============================================================================
# check_retirement_inputs()
# =============================================================================

test_that("check_retirement_inputs accepts valid complete inputs", {
  contract_dt <- data.table(
    contract_id = "C001",
    personnel_id = "P001",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  
  personnel_dt <- data.table(
    personnel_id = "P001",
    status = "active",
    birth_date = as.Date("1970-01-01")
  )
  
  policy_params <- list(
    eligibility_type = "age_and_tenure",
    min_age = 60,
    min_tenure = 15,
    pension_type = "db",
    pension_params = list(accrual_rate = 0.02)
  )
  
  ref_date <- as.Date("2025-01-01")
  
  expect_invisible(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date)
  )
})

test_that("check_retirement_inputs validates contract_dt", {
  personnel_dt <- data.table(personnel_id = "P001", status = "active")
  policy_params <- list(
    eligibility_type = "tenure_only", 
    min_tenure = 15,
    pension_type = "flat",
    pension_params = list(amount = 1000)
  )
  ref_date <- as.Date("2025-01-01")
  
  expect_error(
    check_retirement_inputs(NULL, personnel_dt, policy_params, ref_date),
    "contract_dt must be a data.table"
  )
})

test_that("check_retirement_inputs validates personnel_dt", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001", 
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  policy_params <- list(
    eligibility_type = "tenure_only",
    min_tenure = 15,
    pension_type = "flat",
    pension_params = list(amount = 1000)
  )
  ref_date <- as.Date("2025-01-01")
  
  expect_error(
    check_retirement_inputs(contract_dt, NULL, policy_params, ref_date),
    "personnel_dt must be a data.table"
  )
})

test_that("check_retirement_inputs validates ref_date", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  personnel_dt <- data.table(personnel_id = "P001", status = "active")
  policy_params <- list(
    eligibility_type = "tenure_only",
    min_tenure = 15,
    pension_type = "flat",
    pension_params = list(amount = 1000)
  )
  
  # Invalid date string should error
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, "not-a-date"),
    "must be a valid date"
  )
  
  # Valid date string should be accepted
  expect_no_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, "2025-01-01")
  )
})

test_that("check_retirement_inputs validates policy_params structure", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  personnel_dt <- data.table(personnel_id = "P001", status = "active")
  ref_date <- as.Date("2025-01-01")
  
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, "not a list", ref_date),
    "policy_params must be a list"
  )
})

test_that("check_retirement_inputs validates required policy params", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  personnel_dt <- data.table(personnel_id = "P001", status = "active")
  ref_date <- as.Date("2025-01-01")
  
  # Missing pension_type
  policy_params <- list(eligibility_type = "tenure_only")
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date),
    "Missing required parameters for retirement policy: pension_type"
  )
})

test_that("check_retirement_inputs validates eligibility_type value", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  personnel_dt <- data.table(personnel_id = "P001", status = "active")
  ref_date <- as.Date("2025-01-01")
  
  policy_params <- list(
    eligibility_type = "invalid_type",
    pension_type = "flat",
    pension_params = list(amount = 1000)
  )
  
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date),
    "Invalid eligibility_type: 'invalid_type'. Valid options: age_only, tenure_only, age_and_tenure"
  )
})

test_that("check_retirement_inputs validates pension_type value", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  personnel_dt <- data.table(personnel_id = "P001", status = "active")
  ref_date <- as.Date("2025-01-01")
  
  policy_params <- list(
    eligibility_type = "tenure_only",
    min_tenure = 15,
    pension_type = "invalid_pension",
    pension_params = list()
  )
  
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date),
    "Invalid pension_type: 'invalid_pension'. Valid options: db, dc, flat, hybrid"
  )
})

test_that("check_retirement_inputs requires min_age for age_only eligibility", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  personnel_dt <- data.table(personnel_id = "P001", status = "active", birth_date = as.Date("1970-01-01"))
  ref_date <- as.Date("2025-01-01")
  
  policy_params <- list(
    eligibility_type = "age_only",
    pension_type = "flat",
    pension_params = list(amount = 1000)
  )
  
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date),
    "min_age is required for eligibility_type 'age_only'"
  )
})

test_that("check_retirement_inputs requires min_tenure for tenure_only eligibility", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  personnel_dt <- data.table(personnel_id = "P001", status = "active")
  ref_date <- as.Date("2025-01-01")
  
  policy_params <- list(
    eligibility_type = "tenure_only",
    pension_type = "flat",
    pension_params = list(amount = 1000)
  )
  
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date),
    "min_tenure is required for eligibility_type 'tenure_only'"
  )
})

test_that("check_retirement_inputs requires birth_date column for age-based eligibility", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  # Missing birth_date column
  personnel_dt <- data.table(personnel_id = "P001", status = "active")
  ref_date <- as.Date("2025-01-01")
  
  policy_params <- list(
    eligibility_type = "age_only",
    min_age = 60,
    pension_type = "flat",
    pension_params = list(amount = 1000)
  )
  
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date),
    "Column 'birth_date' not found in personnel_dt"
  )
})

test_that("check_retirement_inputs validates required contract columns", {
  # Missing contract_type_code column
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA)
  )
  personnel_dt <- data.table(personnel_id = "P001", status = "active")
  ref_date <- as.Date("2025-01-01")
  
  policy_params <- list(
    eligibility_type = "tenure_only",
    min_tenure = 15,
    pension_type = "flat",
    pension_params = list(amount = 1000)
  )
  
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date),
    "Columns not found in contract_dt: contract_type_code"
  )
})

test_that("check_retirement_inputs validates required personnel columns", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  # Missing status column
  personnel_dt <- data.table(personnel_id = "P001")
  ref_date <- as.Date("2025-01-01")
  
  policy_params <- list(
    eligibility_type = "tenure_only",
    min_tenure = 15,
    pension_type = "flat",
    pension_params = list(amount = 1000)
  )
  
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date),
    "Columns not found in personnel_dt: status"
  )
})

test_that("check_retirement_inputs requires pension_params", {
  contract_dt <- data.table(
    contract_id = "C001", personnel_id = "P001",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA),
    contract_type_code = "perm"
  )
  personnel_dt <- data.table(personnel_id = "P001", status = "active")
  ref_date <- as.Date("2025-01-01")
  
  policy_params <- list(
    eligibility_type = "tenure_only",
    min_tenure = 15,
    pension_type = "flat"
    # Missing pension_params
  )
  
  expect_error(
    check_retirement_inputs(contract_dt, personnel_dt, policy_params, ref_date),
    "pension_params must be specified in policy_params"
  )
})
