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


# =============================================================================
# Tests for select_nearest_ref_date()
# =============================================================================

test_that("select_nearest_ref_date returns closest date not exceeding ref_date", {
  # Basic case
  dates <- as.Date(c("2015-01-01", "2016-01-01", "2017-01-01"))
  result <- select_nearest_ref_date(dates, as.Date("2016-06-01"))
  expect_equal(result, as.Date("2016-01-01"))
  
  # Exact match
  result_exact <- select_nearest_ref_date(dates, as.Date("2016-01-01"))
  expect_equal(result_exact, as.Date("2016-01-01"))
  
  # ref_date after all dates - should return max
  result_max <- select_nearest_ref_date(dates, as.Date("2020-01-01"))
  expect_equal(result_max, as.Date("2017-01-01"))
  
  # ref_date between two dates
  result_between <- select_nearest_ref_date(dates, as.Date("2015-06-15"))
  expect_equal(result_between, as.Date("2015-01-01"))
})

test_that("select_nearest_ref_date handles duplicate dates", {
  # Duplicates should be handled by unique()
  dates <- as.Date(c("2015-01-01", "2016-01-01", "2016-01-01", "2017-01-01"))
  result <- select_nearest_ref_date(dates, as.Date("2016-06-01"))
  expect_equal(result, as.Date("2016-01-01"))
  expect_length(result, 1)
})

test_that("select_nearest_ref_date handles unordered dates", {
  # Dates not in chronological order
  dates <- as.Date(c("2017-01-01", "2015-01-01", "2016-01-01"))
  result <- select_nearest_ref_date(dates, as.Date("2016-06-01"))
  expect_equal(result, as.Date("2016-01-01"))
})

test_that("select_nearest_ref_date returns single value", {
  dates <- as.Date(c("2015-01-01", "2016-01-01", "2017-01-01"))
  result <- select_nearest_ref_date(dates, as.Date("2016-06-01"))
  expect_length(result, 1)
  expect_type(result, "double")
  expect_s3_class(result, "Date")
})

test_that("select_nearest_ref_date handles single date input", {
  single_date <- as.Date("2015-01-01")
  
  # ref_date after single date
  result <- select_nearest_ref_date(single_date, as.Date("2016-01-01"))
  expect_equal(result, as.Date("2015-01-01"))
  
  # ref_date exactly the single date
  result_exact <- select_nearest_ref_date(single_date, as.Date("2015-01-01"))
  expect_equal(result_exact, as.Date("2015-01-01"))
})

test_that("select_nearest_ref_date errors when no valid dates", {
  dates <- as.Date(c("2015-01-01", "2016-01-01", "2017-01-01"))
  
  # ref_date before all dates
  expect_error(
    select_nearest_ref_date(dates, as.Date("2014-01-01")),
    "No dates found on or before"
  )
})

test_that("select_nearest_ref_date handles empty input", {
  empty_dates <- as.Date(character(0))
  
  expect_error(
    select_nearest_ref_date(empty_dates, as.Date("2016-01-01")),
    "No dates found on or before"
  )
})

test_that("select_nearest_ref_date handles NA values appropriately", {
  # NA values should be excluded by the <= comparison
  dates_with_na <- as.Date(c("2015-01-01", NA, "2016-01-01", "2017-01-01"))
  result <- select_nearest_ref_date(dates_with_na, as.Date("2016-06-01"))
  expect_equal(result, as.Date("2016-01-01"))
  expect_false(is.na(result))
})

test_that("select_nearest_ref_date works with monthly panel data", {
  # Realistic scenario: monthly snapshots
  monthly_dates <- seq(as.Date("2015-01-01"), as.Date("2017-12-01"), by = "month")
  
  # Should select 2016-06-01 for ref_date 2016-06-15
  result <- select_nearest_ref_date(monthly_dates, as.Date("2016-06-15"))
  expect_equal(result, as.Date("2016-06-01"))
  
  # Should select 2016-12-01 for ref_date 2016-12-31
  result2 <- select_nearest_ref_date(monthly_dates, as.Date("2016-12-31"))
  expect_equal(result2, as.Date("2016-12-01"))
})


# =============================================================================
# Tests for compute_age()
# =============================================================================


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
  
  # Should deduplicate and get: one span 2010-01-01 → 2025-01-01 = 15 years
  # (C001: 2010-01-01 to 2015-01-01 = 5 yrs; C002: 2015-01-01 to open → capped at ref_date
  #  = 10 yrs; no overlap → sum = 15 yrs)
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


# ===========================================================================
# dispatch_param()
# ===========================================================================

test_that("dispatch_param: bare scalar is repeated for every row", {
  dt <- data.table::data.table(grade = c("A", "B", "A"))
  result <- dispatch_param(60, dt, "min_age")
  expect_equal(result, c(60, 60, 60))
})

test_that("dispatch_param: three-slot list with group_cols NULL uses default", {
  dt <- data.table::data.table(grade = c("A", "B", "A"))
  result <- dispatch_param(list(default = 55, group_cols = NULL, dt = NULL), dt, "min_age")
  expect_equal(result, c(55, 55, 55))
})

test_that("dispatch_param: group-level dispatch returns per-row values", {
  dt     <- data.table::data.table(grade = c("A", "B", "A"))
  lookup <- data.table::data.table(grade = c("A", "B"), min_age = c(60, 55))
  result <- dispatch_param(
    list(default = NULL, group_cols = "grade", policy_table = lookup),
    dt, "min_age"
  )
  expect_equal(result, c(60, 55, 60))
})

test_that("dispatch_param: unmatched rows filled with default", {
  dt     <- data.table::data.table(grade = c("A", "B", "C"))
  lookup <- data.table::data.table(grade = c("A", "B"), min_age = c(60, 55))
  result <- dispatch_param(
    list(default = 50, group_cols = "grade", policy_table = lookup),
    dt, "min_age"
  )
  expect_equal(result, c(60, 55, 50))
})

test_that("dispatch_param: unmatched rows with no default raises error", {
  dt     <- data.table::data.table(grade = c("A", "B", "C"))
  lookup <- data.table::data.table(grade = c("A", "B"), min_age = c(60, 55))
  expect_error(
    dispatch_param(
      list(default = NULL, group_cols = "grade", policy_table = lookup),
      dt, "min_age"
    ),
    regexp = "unmatched group"
  )
})

test_that("dispatch_param: group_cols set but policy_table NULL raises error", {
  dt <- data.table::data.table(grade = c("A", "B"))
  expect_error(
    dispatch_param(
      list(default = 60, group_cols = "grade", policy_table = NULL),
      dt, "min_age"
    ),
    regexp = "group_cols but policy_table is NULL"
  )
})

test_that("dispatch_param: policy_table set but group_cols NULL raises error", {
  dt     <- data.table::data.table(grade = c("A", "B"))
  lookup <- data.table::data.table(grade = c("A", "B"), min_age = c(60, 55))
  expect_error(
    dispatch_param(
      list(default = NULL, group_cols = NULL, policy_table = lookup),
      dt, "min_age"
    ),
    regexp = "has policy_table but group_cols is NULL"
  )
})

test_that("dispatch_param: group_cols not found in working_dt raises error", {
  dt     <- data.table::data.table(grade = c("A", "B"))
  lookup <- data.table::data.table(employment_type = c("permanent"), min_age = c(60))
  expect_error(
    dispatch_param(
      list(default = NULL, group_cols = "employment_type", policy_table = lookup),
      dt, "min_age"
    ),
    regexp = "group_cols not found in working_dt"
  )
})

test_that("dispatch_param: param_name column missing from policy_table raises error", {
  dt     <- data.table::data.table(grade = c("A", "B"))
  lookup <- data.table::data.table(grade = c("A", "B"), retirement_age = c(60, 55))
  expect_error(
    dispatch_param(
      list(default = NULL, group_cols = "grade", policy_table = lookup),
      dt, "min_age"
    ),
    regexp = "column not found in dt"
  )
})

test_that("dispatch_param: multi-column group_cols join works correctly", {
  dt <- data.table::data.table(
    grade           = c("A", "A", "B"),
    employment_type = c("permanent", "contract", "permanent")
  )
  lookup <- data.table::data.table(
    grade           = c("A",    "A",        "B"),
    employment_type = c("permanent", "contract", "permanent"),
    min_age         = c(60,    55,         58)
  )
  result <- dispatch_param(
    list(default = NULL, group_cols = c("grade", "employment_type"), policy_table = lookup),
    dt, "min_age"
  )
  expect_equal(result, c(60, 55, 58))
})