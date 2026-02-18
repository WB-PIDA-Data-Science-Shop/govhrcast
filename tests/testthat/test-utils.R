# Tests for utility functions in utils.R

library(testthat)
library(data.table)

test_that("compute_years calculates time differences correctly", {
  # Basic calculation
  start <- as.Date("2000-01-01")
  end <- as.Date("2025-01-01")
  result <- compute_years(start, end)
  expect_equal(result, 25, tolerance = 0.01)
  
  # Vectorized operation
  starts <- as.Date(c("2000-01-01", "2010-06-15", "2015-12-31"))
  ends <- as.Date(c("2020-01-01", "2020-06-15", "2025-12-31"))
  results <- compute_years(starts, ends)
  expect_length(results, 3)
  expect_equal(results[1], 20, tolerance = 0.01)
  expect_equal(results[2], 10, tolerance = 0.01)
  expect_equal(results[3], 10, tolerance = 0.01)
  
  # Leap year handling
  start_leap <- as.Date("2020-02-29")
  end_leap <- as.Date("2024-02-29")
  result_leap <- compute_years(start_leap, end_leap)
  expect_equal(result_leap, 4, tolerance = 0.01)
  
  # Same date should be 0
  same_date <- as.Date("2020-01-01")
  expect_equal(compute_years(same_date, same_date), 0)
  
  # Negative time (end before start)
  expect_lt(compute_years(end, start), 0)
})

test_that("compute_years handles edge cases", {
  # NA values
  start_na <- as.Date(c("2000-01-01", NA, "2010-01-01"))
  end_valid <- as.Date(c("2020-01-01", "2020-01-01", "2020-01-01"))
  result_na <- compute_years(start_na, end_valid)
  expect_true(is.na(result_na[2]))
  expect_false(is.na(result_na[1]))
  
  # Empty vectors
  empty_start <- as.Date(character(0))
  empty_end <- as.Date(character(0))
  result_empty <- compute_years(empty_start, empty_end)
  expect_length(result_empty, 0)
})


test_that("compute_age calculates ages correctly", {
  # Create test personnel data
  personnel_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    birth_date = as.Date(c("1970-01-01", "1985-06-15", "1990-12-31"))
  )
  
  ref_date <- as.Date("2025-01-01")
  result <- compute_age(personnel_dt, ref_date)
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_true("age" %in% names(result))
  expect_true("personnel_id" %in% names(result))
  
  # Check age calculations
  expect_equal(result[personnel_id == "P001"]$age, 55, tolerance = 0.1)
  expect_equal(result[personnel_id == "P002"]$age, 39.5, tolerance = 0.1)
  expect_equal(result[personnel_id == "P003"]$age, 34, tolerance = 0.1)
})

test_that("compute_age handles missing birth_dates", {
  personnel_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    birth_date = as.Date(c("1970-01-01", NA, "1990-12-31"))
  )
  
  ref_date <- as.Date("2025-01-01")
  result <- compute_age(personnel_dt, ref_date)
  
  expect_true(is.na(result[personnel_id == "P002"]$age))
  expect_false(is.na(result[personnel_id == "P001"]$age))
})

test_that("compute_age uses custom column names", {
  personnel_dt <- data.table(
    emp_id = c("E001", "E002"),
    dob = as.Date(c("1980-01-01", "1990-01-01"))
  )
  
  ref_date <- as.Date("2025-01-01")
  result <- compute_age(personnel_dt, ref_date, 
                       birth_date_col = "dob",
                       personnel_id_col = "emp_id")
  
  expect_true("personnel_id" %in% names(result))
  expect_equal(nrow(result), 2)
})


test_that("compute_tenure calculates service years correctly", {
  # Create test contract data (non-panel)
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003"),
    personnel_id = c("P001", "P001", "P002"),
    start_date = as.Date(c("2010-01-01", "2015-01-01", "2018-06-01")),
    end_date = as.Date(c("2015-01-01", NA, NA)),
    contract_type_code = c("perm", "perm", "perm")
  )
  
  ref_date <- as.Date("2025-01-01")
  result <- compute_tenure(contract_dt, ref_date)
  
  expect_s3_class(result, "data.table")
  expect_true("tenure_years" %in% names(result))
  expect_true("tenure_days" %in% names(result))
  expect_true("personnel_id" %in% names(result))
  
  # P001 has two contracts: 5 years + 10 years = 15 years
  p001_tenure <- result[personnel_id == "P001"]$tenure_years
  expect_equal(p001_tenure, 15, tolerance = 0.1)
  
  # P002 has one ongoing contract: ~6.5 years
  p002_tenure <- result[personnel_id == "P002"]$tenure_years
  expect_equal(p002_tenure, 6.5, tolerance = 0.1)
})

test_that("compute_tenure handles panel data correctly", {
  # Create panel data: same contract repeated across ref_dates
  contract_dt <- data.table(
    ref_date = rep(as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")), each = 2),
    contract_id = rep(c("C001", "C002"), 3),
    personnel_id = rep(c("P001", "P001"), 3),
    start_date = rep(as.Date(c("2010-01-01", "2015-01-01")), 3),
    end_date = rep(as.Date(c("2015-01-01", NA)), 3),
    contract_type_code = rep("perm", 6)
  )
  
  ref_date <- as.Date("2025-01-01")
  result <- compute_tenure(contract_dt, ref_date)
  
  # Should deduplicate and get: 5 years + 10 years = 15 years
  expect_equal(nrow(result), 1)  # Only one personnel
  expect_equal(result$tenure_years, 15, tolerance = 0.1)
})

test_that("compute_tenure excludes inactive contracts", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003"),
    personnel_id = c("P001", "P001", "P001"),
    start_date = as.Date(c("2010-01-01", "2015-01-01", "2020-01-01")),
    end_date = as.Date(c("2015-01-01", "2020-01-01", NA)),
    contract_type_code = c("perm", "inactive", "perm")
  )
  
  ref_date <- as.Date("2025-01-01")
  result <- compute_tenure(contract_dt, ref_date)
  
  # Should exclude C002 (inactive): 5 years (C001) + 5 years (C003) = 10 years
  expect_equal(result$tenure_years, 10, tolerance = 0.1)
})

test_that("compute_tenure handles contracts starting after ref_date", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P001"),
    start_date = as.Date(c("2010-01-01", "2030-01-01")),  # C002 in future
    end_date = as.Date(c(NA, NA)),
    contract_type_code = c("perm", "perm")
  )
  
  ref_date <- as.Date("2025-01-01")
  result <- compute_tenure(contract_dt, ref_date)
  
  # Should only count C001: 15 years
  expect_equal(result$tenure_years, 15, tolerance = 0.1)
})

test_that("compute_tenure handles empty contract data", {
  contract_dt <- data.table(
    contract_id = character(0),
    personnel_id = character(0),
    start_date = as.Date(character(0)),
    end_date = as.Date(character(0)),
    contract_type_code = character(0)
  )
  
  ref_date <- as.Date("2025-01-01")
  result <- compute_tenure(contract_dt, ref_date)
  
  expect_equal(nrow(result), 0)
})


test_that("get_active_contracts filters correctly", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003", "C004", "C005"),
    personnel_id = c("P001", "P002", "P003", "P004", "P005"),
    start_date = as.Date(c("2010-01-01", "2020-01-01", "2030-01-01", "2015-01-01", "2020-01-01")),
    end_date = as.Date(c("2020-01-01", NA, NA, "2022-01-01", NA)),
    contract_type_code = c("perm", "perm", "perm", "perm", "inactive")
  )
  
  ref_date <- as.Date("2025-01-01")
  result <- get_active_contracts(contract_dt, ref_date)
  
  # Should include: C002 (ongoing), exclude: C001 (ended), C003 (future), C004 (ended), C005 (inactive)
  expect_equal(nrow(result), 1)
  expect_equal(result$contract_id, "C002")
})


test_that("get_primary_contract selects correctly", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003"),
    personnel_id = c("P001", "P001", "P001"),
    start_date = as.Date(c("2010-01-01", "2020-01-01", "2020-01-01")),
    gross_salary_lcu = c(5000, 7000, 6000)
  )
  
  result <- get_primary_contract(contract_dt)
  
  # Should select C002: same start_date as C003, but higher salary
  expect_equal(nrow(result), 1)
  expect_equal(result$contract_id, "C002")
})

test_that("get_primary_contract uses contract_id as tiebreaker", {
  contract_dt <- data.table(
    contract_id = c("C003", "C001", "C002"),
    personnel_id = c("P001", "P001", "P001"),
    start_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01")),
    gross_salary_lcu = c(7000, 7000, 7000)
  )
  
  result <- get_primary_contract(contract_dt)
  
  # Should select C001: lowest contract_id when all else equal
  expect_equal(result$contract_id, "C001")
})


test_that("generate_new_ids creates unique IDs", {
  ids <- generate_new_ids(5, as.Date("2025-01-15"))
  
  expect_length(ids, 5)
  expect_true(all(grepl("^NEW_20250115_", ids)))
  expect_equal(length(unique(ids)), 5)
})


test_that("create_empty_events returns correct structure", {
  result <- create_empty_events("retirement")
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
  expect_true("personnel_id" %in% names(result))
  expect_true("event_type" %in% names(result))
  expect_true("event_date" %in% names(result))
})


test_that("format_date_stamp formats correctly", {
  dates <- as.Date(c("2025-01-15", "2020-12-31"))
  result <- format_date_stamp(dates)
  
  expect_equal(result, c("20250115", "20201231"))
  expect_type(result, "character")
})
