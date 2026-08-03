# Unit tests for retirement_core.R functions
# Testing retirement eligibility logic with various scenarios.

library(testthat)
library(data.table)

# Access internal helpers for direct unit tests.
determine_required_metrics <- getFromNamespace(
  ".determine_required_metrics",
  "govhrcast"
)
filter_active_candidate_pool <- getFromNamespace(
  ".filter_active_candidate_pool",
  "govhrcast"
)
enrich_working_table <- getFromNamespace(".enrich_working_table", "govhrcast")
eval_per_row_expression <- getFromNamespace(
  ".eval_per_row_expression",
  "govhrcast"
)

# =============================================================================
# Test data setup helpers
# =============================================================================

create_test_contract_dt <- function() {
  data.table(
    ref_date = as.Date("2025-01-01"),
    contract_id = c("C001", "C002", "C003", "C004"),
    personnel_id = c("P001", "P002", "P003", "P004"),
    start_date = as.Date(c(
      "2000-01-01",
      "2010-01-01",
      "2015-01-01",
      "2005-01-01"
    )),
    end_date = as.Date(c(NA, NA, NA, NA)),
    contract_type = c("perm", "perm", "perm", "perm"),
    gross_salary_lcu = c(5000, 6000, 4000, 5500)
  )
}

create_test_personnel_dt <- function() {
  data.table(
    personnel_id = c("P001", "P002", "P003", "P004"),
    birth_date = as.Date(c(
      "1960-01-01",
      "1975-01-01",
      "1980-01-01",
      "1965-01-01"
    )),
    employment_status = c("active", "active", "active", "active")
  )
}

# =============================================================================
# Internal helper coverage
# =============================================================================

test_that(".determine_required_metrics flags required metrics correctly", {
  age_only <- determine_required_metrics(list(
    defaults = list(eligibility_type = "age_only")
  ))
  expect_true(age_only$needs_age)
  expect_false(age_only$needs_tenure)

  tenure_only <- determine_required_metrics(
    list(defaults = list(eligibility_type = "tenure_only"))
  )
  expect_false(tenure_only$needs_age)
  expect_true(tenure_only$needs_tenure)

  custom_rule <- determine_required_metrics(
    list(
      defaults = list(
        eligibility_type = "custom",
        eligibility_rule = "score >= 10"
      )
    )
  )
  expect_true("score" %in% custom_rule$rule_all_cols)
})

test_that(".filter_active_candidate_pool keeps active non-pensioner personnel", {
  contract_dt <- data.table(
    contract_id = c("C1", "C2", "C3"),
    personnel_id = c("P1", "P2", "P3"),
    start_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01")),
    end_date = as.Date(c(NA, NA, "2021-01-01")),
    contract_type = c("perm", "perm", "perm")
  )
  personnel_dt <- data.table(
    personnel_id = c("P1", "P2", "P3"),
    employment_status = c("active", "pensioner", "active")
  )

  result <- filter_active_candidate_pool(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    ref_date = as.Date("2025-01-01"),
    personnel_id_col = "personnel_id",
    start_date_col = "start_date",
    end_date_col = "end_date",
    contract_type_col = "contract_type",
    status_col = "employment_status"
  )

  expect_setequal(result$all_pid, c("P1", "P2", "P3"))
  expect_setequal(result$active_pid, c("P1", "P2"))
  expect_setequal(result$personnel_dt$personnel_id, "P1")
})

test_that(".enrich_working_table adds needed columns from both sources", {
  working_dt <- data.table(personnel_id = c("P1", "P2"))
  contract_dt <- data.table(
    contract_id = c("C1", "C2"),
    personnel_id = c("P1", "P2"),
    start_date = as.Date(c("2020-01-01", "2020-01-01")),
    end_date = as.Date(c(NA, NA)),
    contract_type = c("perm", "perm"),
    gross_salary_lcu = c(5000, 6000),
    grade = c("A", "B")
  )
  personnel_dt <- data.table(
    personnel_id = c("P1", "P2"),
    region = c("North", "South")
  )

  enrich_working_table(
    working_dt = working_dt,
    needed_cols = c("grade", "region"),
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    ref_date = as.Date("2025-01-01"),
    personnel_id_col = "personnel_id",
    contract_id_col = "contract_id",
    start_date_col = "start_date",
    end_date_col = "end_date",
    contract_type_col = "contract_type"
  )

  expect_equal(working_dt[personnel_id == "P1", grade], "A")
  expect_equal(working_dt[personnel_id == "P2", region], "South")
})

test_that(".eval_per_row_expression evaluates expressions for masked rows", {
  dt <- data.table(
    age = c(61, 55, 70),
    tenure_years = c(12, 20, 8),
    eligibility_rule = c("age >= 60", "tenure_years >= 15", "age >= 65")
  )

  eval_per_row_expression(
    dt = dt,
    mask = c(TRUE, TRUE, FALSE),
    expr_col = "eligibility_rule",
    result_col = "custom_eligible"
  )

  expect_identical(dt$custom_eligible[[1]], TRUE)
  expect_identical(dt$custom_eligible[[2]], TRUE)
  expect_true(is.na(dt$custom_eligible[[3]]))
})

# =============================================================================
# identify_eligibility() - age_only eligibility
# =============================================================================

test_that("identify_eligibility works with age_only eligibility", {
  contract_dt <- create_test_contract_dt()
  personnel_dt <- create_test_personnel_dt()

  policy_params <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(eligibility_type = "age_only", min_age = 60)
  )

  ref_date <- as.Date("2025-01-01")

  result <- identify_eligibility(
    contract_dt,
    personnel_dt,
    policy_params,
    ref_date
  )

  # Check structure
  expect_s3_class(result, "data.table")
  expect_named(result, c("personnel_id", "retire", "age", "tenure_years"))
  expect_equal(nrow(result), 4)

  # Check eligibility: P001 (age 65) and P004 (age 60) should be eligible
  expect_equal(result[personnel_id == "P001"]$retire, 1)
  expect_equal(result[personnel_id == "P004"]$retire, 1)
  expect_equal(result[personnel_id == "P002"]$retire, 0) # age 50
  expect_equal(result[personnel_id == "P003"]$retire, 0) # age 45

  # Check that tenure is NA for age_only
  expect_true(all(is.na(result$tenure_years)))
})

test_that("identify_eligibility respects exact min_age boundary", {
  contract_dt <- create_test_contract_dt()
  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    birth_date = as.Date(c("1965-01-01", "1965-01-02")), # Born Jan 1 vs Jan 2
    employment_status = c("active", "active")
  )

  policy_params <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(eligibility_type = "age_only", min_age = 60)
  )

  ref_date <- as.Date("2025-01-01") # P001 is exactly 60.00, P002 is 59.997

  result <- identify_eligibility(
    contract_dt[1:2],
    personnel_dt,
    policy_params,
    ref_date
  )

  # Only P001 should be eligible (>= 60), P002 is 59.997 years
  expect_equal(sum(result$retire), 1)
  expect_equal(result[personnel_id == "P001"]$retire, 1)
  expect_equal(result[personnel_id == "P002"]$retire, 0)
})

# =============================================================================
# identify_eligibility() - tenure_only eligibility
# =============================================================================

test_that("identify_eligibility works with tenure_only eligibility", {
  contract_dt <- create_test_contract_dt()
  personnel_dt <- create_test_personnel_dt()

  policy_params <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(eligibility_type = "tenure_only", min_tenure = 15)
  )

  ref_date <- as.Date("2025-01-01")

  result <- identify_eligibility(
    contract_dt,
    personnel_dt,
    policy_params,
    ref_date
  )

  # Check structure
  expect_s3_class(result, "data.table")
  expect_named(result, c("personnel_id", "retire", "age", "tenure_years"))

  # Check eligibility: P001 (25 years), P002 (15.0 years), and P004 (20 years) should be eligible
  expect_equal(result[personnel_id == "P001"]$retire, 1)
  expect_equal(result[personnel_id == "P004"]$retire, 1)
  expect_equal(result[personnel_id == "P002"]$retire, 1) # 15.0 years - at boundary (eligible)
  expect_equal(result[personnel_id == "P003"]$retire, 0) # 10 years

  # Check that age is NA for tenure_only
  expect_true(all(is.na(result$age)))
})

test_that("identify_eligibility respects exact min_tenure boundary", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P002"),
    start_date = as.Date(c("2010-01-01", "2010-01-02")),
    end_date = as.Date(c(NA, NA)),
    contract_type = c("perm", "perm")
  )

  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    employment_status = c("active", "active")
  )

  policy_params <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(eligibility_type = "tenure_only", min_tenure = 15)
  )

  ref_date <- as.Date("2025-01-01") # P001 has exactly 15.000 years, P002 slightly less

  result <- identify_eligibility(
    contract_dt,
    personnel_dt,
    policy_params,
    ref_date
  )

  # P001 should be eligible (>= 15), P002 should not (< 15)
  expect_equal(result[personnel_id == "P001"]$retire, 1)
  expect_equal(result[personnel_id == "P002"]$retire, 0)
})

# =============================================================================
# identify_eligibility() - age_and_tenure eligibility
# =============================================================================

test_that("identify_eligibility works with age_and_tenure eligibility", {
  contract_dt <- create_test_contract_dt()
  personnel_dt <- create_test_personnel_dt()

  policy_params <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_and_tenure",
      min_age = 60,
      min_tenure = 15
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- identify_eligibility(
    contract_dt,
    personnel_dt,
    policy_params,
    ref_date
  )

  # Check structure
  expect_s3_class(result, "data.table")
  expect_named(result, c("personnel_id", "retire", "age", "tenure_years"))

  # Check eligibility: Only P001 (age 65, tenure 25) and P004 (age 60, tenure 20) meet both
  expect_equal(result[personnel_id == "P001"]$retire, 1)
  expect_equal(result[personnel_id == "P004"]$retire, 1)
  expect_equal(result[personnel_id == "P002"]$retire, 0) # age 50 < 60 (even though tenure >= 15)
  expect_equal(result[personnel_id == "P003"]$retire, 0) # tenure 10 < 15 (even though age condition not met)

  # Check that both age and tenure are populated
  expect_false(any(is.na(result$age)))
  expect_false(any(is.na(result$tenure_years)))
})

test_that("identify_eligibility AND logic works correctly", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003"),
    personnel_id = c("P001", "P002", "P003"),
    start_date = as.Date(c("2000-01-01", "2015-01-01", "2000-01-01")),
    end_date = as.Date(c(NA, NA, NA)),
    contract_type = c("perm", "perm", "perm")
  )

  personnel_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    birth_date = as.Date(c("1960-01-01", "1960-01-01", "1975-01-01")), # ages 65, 65, 50
    employment_status = c("active", "active", "active")
  )

  policy_params <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_and_tenure",
      min_age = 60,
      min_tenure = 15
    )
  )

  ref_date <- as.Date("2025-01-01")

  result <- identify_eligibility(
    contract_dt,
    personnel_dt,
    policy_params,
    ref_date
  )

  # P001: age 65 (✓), tenure 25 (✓) -> eligible
  expect_equal(result[personnel_id == "P001"]$retire, 1)

  # P002: age 65 (✓), tenure 10 (✗) -> not eligible
  expect_equal(result[personnel_id == "P002"]$retire, 0)

  # P003: age 50 (✗), tenure 25 (✓) -> not eligible
  expect_equal(result[personnel_id == "P003"]$retire, 0)
})

# =============================================================================
# identify_eligibility() - edge cases
# =============================================================================

test_that("identify_eligibility handles missing birth_date for tenure_only", {
  contract_dt <- create_test_contract_dt()
  personnel_dt <- data.table(
    personnel_id = c("P001", "P002", "P003", "P004"),
    birth_date = as.Date(c(NA, NA, NA, NA)), # All NA
    employment_status = c("active", "active", "active", "active")
  )

  policy_params <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(eligibility_type = "tenure_only", min_tenure = 15)
  )

  ref_date <- as.Date("2025-01-01")

  # Should not error - age not needed for tenure_only
  result <- identify_eligibility(
    contract_dt,
    personnel_dt,
    policy_params,
    ref_date
  )

  expect_s3_class(result, "data.table")
  expect_true(all(is.na(result$age)))
})

test_that("identify_eligibility handles empty personnel list", {
  contract_dt <- data.table(
    contract_id = character(),
    personnel_id = character(),
    start_date = as.Date(character()),
    end_date = as.Date(character()),
    contract_type = character()
  )

  personnel_dt <- data.table(
    personnel_id = character(),
    birth_date = as.Date(character()),
    employment_status = character()
  )

  policy_params <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(eligibility_type = "tenure_only", min_tenure = 15)
  )

  ref_date <- as.Date("2025-01-01")

  result <- identify_eligibility(
    contract_dt,
    personnel_dt,
    policy_params,
    ref_date
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("identify_eligibility handles NA in computed age", {
  contract_dt <- create_test_contract_dt()[1:2]
  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    birth_date = as.Date(c("1960-01-01", NA)), # P002 has NA birth_date
    employment_status = c("active", "active")
  )

  policy_params <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(eligibility_type = "age_only", min_age = 60)
  )

  ref_date <- as.Date("2025-01-01")

  result <- identify_eligibility(
    contract_dt,
    personnel_dt,
    policy_params,
    ref_date
  )

  # P001 should be eligible
  expect_equal(result[personnel_id == "P001"]$retire, 1)

  # P002 should not be eligible (NA age treated as 0)
  expect_equal(result[personnel_id == "P002"]$retire, 0)
})

test_that("identify_eligibility handles NA in computed tenure", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P002"),
    start_date = as.Date(c("2000-01-01", NA)), # P002 has NA start_date
    end_date = as.Date(c(NA, NA)),
    contract_type = c("perm", "perm")
  )

  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    employment_status = c("active", "active")
  )

  policy_params <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(eligibility_type = "tenure_only", min_tenure = 15)
  )

  ref_date <- as.Date("2025-01-01")

  result <- identify_eligibility(
    contract_dt,
    personnel_dt,
    policy_params,
    ref_date
  )

  # P001 should be eligible (25 years)
  expect_equal(result[personnel_id == "P001"]$retire, 1)

  # P002 should not be eligible (NA tenure treated as 0)
  expect_equal(result[personnel_id == "P002"]$retire, 0)
})

# =============================================================================
# compute_retirement_summary()
# =============================================================================

test_that("compute_retirement_summary calculates correct statistics", {
  retirees_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    pension = c(2000, 2500, 1800),
    age = c(65, 62, 68),
    tenure_years = c(25, 20, 30)
  )

  result <- compute_retirement_summary(retirees_dt)

  expect_s3_class(result, "data.table")
  expect_equal(result$n_retired, 3)
  expect_equal(result$total_pension, 6300)
  expect_equal(result$avg_pension, 2100)
  expect_equal(result$avg_age, 65)
  expect_equal(result$avg_tenure, 25)
})

test_that("compute_retirement_summary handles empty retirees", {
  retirees_dt <- data.table(
    personnel_id = character(),
    pension = numeric(),
    age = numeric(),
    tenure_years = numeric()
  )

  result <- compute_retirement_summary(retirees_dt)

  expect_equal(result$n_retired, 0)
  expect_equal(result$total_pension, 0)
  expect_true(is.na(result$avg_pension))
  expect_true(is.na(result$avg_age))
  expect_true(is.na(result$avg_tenure))
})

test_that("compute_retirement_summary handles NA pensions", {
  retirees_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    pension = c(2000, NA, 1800),
    age = c(65, 62, 68),
    tenure_years = c(25, 20, 30)
  )

  result <- compute_retirement_summary(retirees_dt)

  expect_equal(result$n_retired, 3)
  expect_equal(result$total_pension, 3800) # Excludes NA
  expect_equal(result$avg_pension, 1900) # Mean of 2000 and 1800
})

test_that("compute_retirement_summary handles NA ages", {
  retirees_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    pension = c(2000, 2500, 1800),
    age = c(65, NA, 68),
    tenure_years = c(25, 20, 30)
  )

  result <- compute_retirement_summary(retirees_dt)

  expect_equal(result$n_retired, 3)
  expect_equal(result$avg_age, 66.5) # Mean of 65 and 68
})

test_that("compute_retirement_summary handles all NA values", {
  retirees_dt <- data.table(
    personnel_id = c("P001", "P002"),
    pension = c(NA_real_, NA_real_),
    age = c(NA_real_, NA_real_),
    tenure_years = c(NA_real_, NA_real_)
  )

  result <- compute_retirement_summary(retirees_dt)

  expect_equal(result$n_retired, 2)
  expect_equal(result$total_pension, 0)
  expect_true(is.nan(result$avg_pension))
  expect_true(is.nan(result$avg_age))
  expect_true(is.nan(result$avg_tenure))
})

# =============================================================================
# prepare_retiree_data()
# =============================================================================

test_that("prepare_retiree_data enriches eligibility data correctly", {
  contract_dt <- data.table(
    ref_date = as.Date("2025-01-01"),
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P002"),
    start_date = as.Date(c("2000-01-01", "2010-01-01")),
    end_date = as.Date(c(NA, NA)),
    contract_type = c("perm", "perm"),
    gross_salary_lcu = c(5000, 6000),
    position_id = c("POS1", "POS2")
  )

  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    employment_status = c("active", "active")
  )

  eligibility_dt <- data.table(
    personnel_id = c("P001", "P002"),
    retire = c(1L, 0L),
    age = c(65, 50),
    tenure_years = c(25, 15)
  )

  ref_date <- as.Date("2025-01-01")

  result <- prepare_retiree_data(
    eligibility_dt,
    contract_dt,
    personnel_dt,
    ref_date
  )

  # Should only include P001 (retire = 1)
  expect_equal(nrow(result), 1)
  expect_equal(result$personnel_id, "P001")
  expect_equal(result$gross_salary_lcu, 5000)
  expect_equal(result$age, 65)
  expect_equal(result$tenure_years, 25)
})

test_that("prepare_retiree_data returns empty when no retirees", {
  contract_dt <- create_test_contract_dt()
  personnel_dt <- create_test_personnel_dt()

  eligibility_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    retire = c(0L, 0L, 0L),
    age = c(45, 50, 40),
    tenure_years = c(5, 10, 8)
  )

  ref_date <- as.Date("2025-01-01")

  result <- prepare_retiree_data(
    eligibility_dt,
    contract_dt,
    personnel_dt,
    ref_date
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("prepare_retiree_data handles multiple contracts per retiree", {
  contract_dt <- data.table(
    ref_date = as.Date("2025-01-01"),
    contract_id = c("C001", "C002", "C003"),
    personnel_id = c("P001", "P001", "P002"), # P001 has 2 contracts
    start_date = as.Date(c("2000-01-01", "2010-01-01", "2015-01-01")),
    end_date = as.Date(c(NA, NA, NA)),
    contract_type = c("perm", "perm", "perm"),
    gross_salary_lcu = c(5000, 7000, 6000) # C002 is primary (highest salary)
  )

  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    employment_status = c("active", "active")
  )

  eligibility_dt <- data.table(
    personnel_id = c("P001", "P002"),
    retire = c(1L, 0L),
    age = c(65, 50),
    tenure_years = c(25, 10)
  )

  ref_date <- as.Date("2025-01-01")

  result <- prepare_retiree_data(
    eligibility_dt,
    contract_dt,
    personnel_dt,
    ref_date
  )

  # Should select C002 as primary (highest salary)
  expect_equal(nrow(result), 1)
  expect_equal(result$personnel_id, "P001")
  expect_equal(result$contract_id, "C002")
  expect_equal(result$gross_salary_lcu, 7000)
})

test_that("prepare_retiree_data filters inactive contracts", {
  contract_dt <- data.table(
    ref_date = as.Date("2025-01-01"),
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P001"),
    start_date = as.Date(c("2000-01-01", "2010-01-01")),
    end_date = as.Date(c("2020-01-01", NA)), # C001 ended
    contract_type = c("inactive", "perm"),
    gross_salary_lcu = c(9000, 5000)
  )

  personnel_dt <- data.table(
    personnel_id = "P001",
    employment_status = "active"
  )

  eligibility_dt <- data.table(
    personnel_id = "P001",
    retire = 1L,
    age = 65,
    tenure_years = 25
  )

  ref_date <- as.Date("2025-01-01")

  result <- prepare_retiree_data(
    eligibility_dt,
    contract_dt,
    personnel_dt,
    ref_date
  )

  # Should only include active contract C002
  expect_equal(nrow(result), 1)
  expect_equal(result$contract_id, "C002")
  expect_equal(result$gross_salary_lcu, 5000)
})

test_that("prepare_retiree_data preserves all contract columns", {
  contract_dt <- data.table(
    ref_date = as.Date("2025-01-01"),
    contract_id = "C001",
    personnel_id = "P001",
    start_date = as.Date("2000-01-01"),
    end_date = as.Date(NA),
    contract_type = "perm",
    gross_salary_lcu = 5000,
    position_id = "POS1",
    paygrade_code = "G10",
    org_unit_id = "ORG1"
  )

  personnel_dt <- data.table(
    personnel_id = "P001",
    employment_status = "active"
  )

  eligibility_dt <- data.table(
    personnel_id = "P001",
    retire = 1L,
    age = 65,
    tenure_years = 25
  )

  ref_date <- as.Date("2025-01-01")

  result <- prepare_retiree_data(
    eligibility_dt,
    contract_dt,
    personnel_dt,
    ref_date
  )

  # Check all columns are preserved
  expect_true("position_id" %in% names(result))
  expect_true("paygrade_code" %in% names(result))
  expect_true("org_unit_id" %in% names(result))
  expect_equal(result$position_id, "POS1")
  expect_equal(result$paygrade_code, "G10")
  expect_equal(result$org_unit_id, "ORG1")
})


# ===========================================================================
# identify_eligibility() — group-level policy params via policy_table
# ===========================================================================

test_that("identify_eligibility: group-level min_age via policy_table dispatches correctly", {
  contract_dt <- data.table::data.table(
    personnel_id = c("P001", "P002", "P003"),
    contract_id = c("C001", "C002", "C003"),
    start_date = as.Date("2000-01-01"),
    end_date = as.Date(NA),
    contract_type = "perm",
    gross_salary_lcu = c(5000, 4000, 3000),
    grade = c("A", "B", "A")
  )
  personnel_dt <- data.table::data.table(
    personnel_id = c("P001", "P002", "P003"),
    # P001/P002: age 65 at ref_date; P003: age 50
    birth_date = as.Date(c("1960-01-01", "1960-01-01", "1975-01-01"))
  )
  # Grade A: min_age 60 → P001 (65) retires, P003 (50) does not
  # Grade B: min_age 68 → P002 (65) does NOT retire
  grade_tbl <- data.table::data.table(
    grade = c("A", "B"),
    min_age = c(60, 68)
  )
  policy_params <- list(
    group_cols = "grade",
    policy_table = grade_tbl,
    defaults = list(eligibility_type = "age_only", min_age = 60)
  )
  result <- identify_eligibility(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2025-01-01")
  )
  expect_equal(result[personnel_id == "P001"]$retire, 1L)
  expect_equal(result[personnel_id == "P002"]$retire, 0L)
  expect_equal(result[personnel_id == "P003"]$retire, 0L)
})

test_that("identify_eligibility: group-level min_tenure via policy_table dispatches correctly", {
  contract_dt <- data.table::data.table(
    personnel_id = c("P001", "P002"),
    contract_id = c("C001", "C002"),
    start_date = as.Date(c("1995-01-01", "2010-01-01")),
    end_date = as.Date(NA),
    contract_type = "perm",
    gross_salary_lcu = c(5000, 4000),
    grade = c("A", "B")
  )
  personnel_dt <- data.table::data.table(
    personnel_id = c("P001", "P002"),
    birth_date = as.Date(c("1970-01-01", "1970-01-01"))
  )
  # P001: tenure ~30 yrs, grade A (min_tenure 25) → retires
  # P002: tenure ~15 yrs, grade B (min_tenure 20) → does NOT retire
  grade_tbl <- data.table::data.table(
    grade = c("A", "B"),
    min_tenure = c(25, 20)
  )
  policy_params <- list(
    group_cols = "grade",
    policy_table = grade_tbl,
    defaults = list(eligibility_type = "tenure_only", min_tenure = 25)
  )
  result <- identify_eligibility(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2025-01-01")
  )
  expect_equal(result[personnel_id == "P001"]$retire, 1L)
  expect_equal(result[personnel_id == "P002"]$retire, 0L)
})

test_that("identify_eligibility: unmatched grade uses default min_age from defaults", {
  contract_dt <- data.table::data.table(
    personnel_id = c("P001", "P002"),
    contract_id = c("C001", "C002"),
    start_date = as.Date("2000-01-01"),
    end_date = as.Date(NA),
    contract_type = "perm",
    gross_salary_lcu = c(5000, 4000),
    grade = c("A", "C") # grade C not in policy_table
  )
  personnel_dt <- data.table::data.table(
    personnel_id = c("P001", "P002"),
    birth_date = as.Date(c("1960-01-01", "1960-01-01")) # both age 65
  )
  # Grade C unmatched → falls back to default min_age = 62; age 65 >= 62 → retires
  grade_tbl <- data.table::data.table(
    grade = c("A"),
    min_age = c(60)
  )
  policy_params <- list(
    group_cols = "grade",
    policy_table = grade_tbl,
    defaults = list(eligibility_type = "age_only", min_age = 62)
  )
  result <- identify_eligibility(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = as.Date("2025-01-01")
  )
  expect_equal(result[personnel_id == "P001"]$retire, 1L) # grade A, min_age 60
  expect_equal(result[personnel_id == "P002"]$retire, 1L) # grade C, default 62
})
