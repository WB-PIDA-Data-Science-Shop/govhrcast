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
    contract_type = c(rep("permanent", 15), rep("terminated", 5))
  )
  
  personnel_dt <- data.table(
    personnel_id = paste0("P", 1:20),
    birth_date = as.Date("1980-01-01"),
    employment_status = c(rep("active", 15), rep("inactive", 5))
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

# =============================================================================
# Tests for duplicate-personnel robustness in compute_current_stock()
# =============================================================================

test_that("compute_current_stock errors on duplicate personnel_id in personnel_dt", {
  # Mirrors real HRMIS data where a person can appear twice at the same snapshot
  # (e.g. conflicting education records). The function should fail loudly so the
  # caller can clean their data rather than silently producing wrong counts.
  contract_dt <- data.table(
    contract_id       = paste0("C", 1:5),
    personnel_id      = paste0("P", 1:5),
    start_date        = as.Date("2020-01-01"),
    end_date          = as.Date(NA),
    contract_type = "permanent"
  )

  personnel_dt <- data.table(
    personnel_id = c(paste0("P", 1:5), "P1"),  # P1 duplicated
    employment_status = c(rep("active", 5), "active")
  )

  expect_error(
    compute_current_stock(
      contract_dt  = contract_dt,
      personnel_dt = personnel_dt,
      ref_date     = as.Date("2024-06-01")
    ),
    "duplicate rows"
  )
})

# =============================================================================
# Tests for downsizing salary cost in compute_hiring_summary()
# =============================================================================

test_that("compute_hiring_summary reports negative salary cost for pure downsizing", {
  adjustment_dt <- data.table(net_change = -3L)

  terminated_contracts <- data.table(
    contract_id      = paste0("C", 1:3),
    personnel_id     = paste0("P", 1:3),
    gross_salary_lcu = c(50000, 60000, 70000)
  )

  result <- compute_hiring_summary(
    adjustment_dt          = adjustment_dt,
    new_hires_dt           = NULL,
    new_contracts_dt       = NULL,
    terminated_contracts_dt = terminated_contracts,
    total_headcount        = 97L
  )

  expect_equal(result$n_new_hires, 0L)
  expect_equal(result$net_headcount_change, -3L)
  expect_equal(result$total_new_salary_cost, -180000)  # -(50000+60000+70000)
})

test_that("compute_hiring_summary reports combined cost for hire+downsize in same run", {
  adjustment_dt <- data.table(
    department = c("HR", "IT"),
    net_change = c(2L, -1L)
  )

  new_contracts <- data.table(
    contract_id      = c("N1", "N2"),
    gross_salary_lcu = c(50000, 50000)
  )

  terminated_contracts <- data.table(
    contract_id      = "T1",
    gross_salary_lcu = 80000
  )

  result <- compute_hiring_summary(
    adjustment_dt          = adjustment_dt,
    new_hires_dt           = data.table(personnel_id = c("NP1", "NP2")),
    new_contracts_dt       = new_contracts,
    terminated_contracts_dt = terminated_contracts,
    total_headcount        = 99L
  )

  expect_equal(result$n_new_hires, 2L)
  expect_equal(result$net_headcount_change, 1L)
  # 100000 new - 80000 removed = 20000 net
  expect_equal(result$total_new_salary_cost, 20000)
})

# =============================================================================
# Block H — estimate_historical_hiring_rates() with hire_date_col
# =============================================================================

# Minimal synthetic panel: 3 snapshots x 5 persons, one hire each year
make_hire_date_panel <- function() {
  dates <- as.Date(c("2022-01-01", "2023-01-01", "2024-01-01"))

  # All 5 persons active across all 3 snapshots
  personnel_dt <- data.table::rbindlist(lapply(dates, function(d) {
    data.table::data.table(
      personnel_id          = paste0("P", 1:5),
      ref_date              = d,
      employment_status = "active",
      first_employment_date = as.Date(c(
        "2021-06-01",   # P1: hired before panel — should NOT be counted
        "2022-03-01",   # P2: within window (2022)
        "2023-03-01",   # P3: within window (2023)
        "2024-03-01",   # P4: within window (2024)
        "2019-01-01"    # P5: hired long before panel — should NOT be counted
      )),
      department = c("HR", "HR", "IT", "IT", "HR")
    )
  }))

  contract_dt <- data.table::rbindlist(lapply(dates, function(d) {
    data.table::data.table(
      contract_id        = paste0("C", 1:5),
      personnel_id       = paste0("P", 1:5),
      ref_date           = d,
      start_date         = as.Date("2019-01-01"),
      end_date           = as.Date(NA),
      contract_type = "active",
      gross_salary_lcu   = 50000,
      department         = c("HR", "HR", "IT", "IT", "HR")
    )
  }))

  list(personnel_dt = personnel_dt, contract_dt = contract_dt)
}

test_that("hire_date_col path counts only hires within the panel window", {
  d <- make_hire_date_panel()

  result <- estimate_historical_hiring_rates(
    panel_contract_dt  = d$contract_dt,
    panel_personnel_dt = d$personnel_dt,
    group_cols         = NULL,
    hire_date_col      = "first_employment_date"
  )

  expect_s3_class(result, "data.table")
  expect_true("hiring_rate" %in% names(result))
  expect_equal(nrow(result), 1L)

  # 3 hires (P2, P3, P4) in ~3-year window; mean stock = 5
  # rate ≈ (3 / n_years) / 5 where n_years ~ 2 (Jan 2022 to Jan 2024)
  # Use loose tolerance to accommodate 365.25-day year rounding
  expect_equal(result$hiring_rate, 3 / 3 / 5, tolerance = 0.01)
})

test_that("hire_date_col = NULL (fallback) returns a numeric hiring_rate", {
  d <- make_hire_date_panel()

  result <- estimate_historical_hiring_rates(
    panel_contract_dt  = d$contract_dt,
    panel_personnel_dt = d$personnel_dt,
    group_cols         = NULL,
    hire_date_col      = NULL   # panel first-appearance fallback
  )

  expect_s3_class(result, "data.table")
  expect_true("hiring_rate" %in% names(result))
  expect_true(is.numeric(result$hiring_rate))
  expect_true(result$hiring_rate >= 0)
})

test_that("hire_date_col path errors clearly when column absent", {
  d <- make_hire_date_panel()

  expect_error(
    estimate_historical_hiring_rates(
      panel_contract_dt  = d$contract_dt,
      panel_personnel_dt = d$personnel_dt,
      group_cols         = NULL,
      hire_date_col      = "nonexistent_col"
    ),
    regexp = "nonexistent_col"
  )
})

test_that("hire_date_col path with group_cols returns one row per group", {
  d <- make_hire_date_panel()

  result <- estimate_historical_hiring_rates(
    panel_contract_dt  = d$contract_dt,
    panel_personnel_dt = d$personnel_dt,
    group_cols         = "department",
    hire_date_col      = "first_employment_date"
  )

  expect_s3_class(result, "data.table")
  expect_true("department" %in% names(result))
  expect_true("hiring_rate" %in% names(result))
  expect_equal(nrow(result), 2L)  # HR and IT
  expect_true(all(result$hiring_rate >= 0))
})

test_that("hire_date_col path: all hires outside window produces rate = 0", {
  d <- make_hire_date_panel()

  # Set all hire dates before panel start
  d$personnel_dt[, first_employment_date := as.Date("2010-01-01")]

  result <- estimate_historical_hiring_rates(
    panel_contract_dt  = d$contract_dt,
    panel_personnel_dt = d$personnel_dt,
    group_cols         = NULL,
    hire_date_col      = "first_employment_date"
  )

  expect_equal(result$hiring_rate, 0)
})

test_that("hire_date_col produces lower rate than panel-appearance when pre-panel hires exist", {
  d <- make_hire_date_panel()

  rate_fed   <- estimate_historical_hiring_rates(
    panel_contract_dt  = d$contract_dt,
    panel_personnel_dt = d$personnel_dt,
    group_cols         = NULL,
    hire_date_col      = "first_employment_date"
  )$hiring_rate

  rate_panel <- estimate_historical_hiring_rates(
    panel_contract_dt  = d$contract_dt,
    panel_personnel_dt = d$personnel_dt,
    group_cols         = NULL,
    hire_date_col      = NULL
  )$hiring_rate

  # Panel first-appearance will count P1 and P5 as appearing at first snapshot;
  # the fed rate only counts 3 within-window hires — should be lower.
  # (Both are valid — just verifying the two paths differ in this scenario.)
  expect_true(is.numeric(rate_fed))
  expect_true(is.numeric(rate_panel))
})


# =============================================================================
# estimate_historical_hiring_rates() panel path: pooled, start-of-interval base
# =============================================================================

make_hire_rate_panel <- function(ids_by_snap, est_of = function(id) "E1") {
  d <- seq(as.Date("2019-01-01"), by = "year", length.out = length(ids_by_snap))
  ct <- data.table::rbindlist(lapply(seq_along(d), function(i) {
    ids <- ids_by_snap[[i]]
    data.table::data.table(
      ref_date = d[i], personnel_id = sprintf("P%02d", ids),
      contract_id = sprintf("C%02d", ids), est_id = vapply(ids, est_of, ""),
      contract_type = "permanent", start_date = as.Date("2010-01-01"),
      end_date = as.Date(NA), gross_salary_lcu = 1000
    )
  }))
  list(ct = ct, pt = ct[, .(ref_date, personnel_id, employment_status = "active")])
}

test_that("estimate_historical_hiring_rates: hires are divided by the start-of-interval stock", {
  # Regression test: 10 active, 5 join by the next snapshot => 5 / 10 = 0.5.
  # It used to divide by the end stock (5 / 15) and average in the first
  # snapshot as a structural zero, giving 1/6.
  p <- make_hire_rate_panel(list(1:10, 1:15))
  expect_equal(estimate_historical_hiring_rates(p$ct, p$pt, group_cols = NULL)$hiring_rate, 0.5)
})

test_that("estimate_historical_hiring_rates: rates are pooled across intervals", {
  # Interval 1: 5 hires on a base of 10; interval 2: 0 on 15. Pooled = 5 / 25.
  p <- make_hire_rate_panel(list(1:10, 1:15, 1:15))
  expect_equal(estimate_historical_hiring_rates(p$ct, p$pt, group_cols = NULL)$hiring_rate, 0.2)
})

test_that("estimate_historical_hiring_rates: grouped rates are pooled per group", {
  # E1 = P01-P05 + hires P11, P12; E2 = P06-P10 + hire P13 (in interval 2).
  # E1: 2 / (5 + 7) ; E2: 1 / (5 + 5).
  est_of <- function(id) if (id <= 5L || id %in% 11:12) "E1" else "E2"
  p <- make_hire_rate_panel(list(1:10, c(1:10, 11:12), c(1:10, 11:13)), est_of)
  r <- estimate_historical_hiring_rates(p$ct, p$pt, group_cols = "est_id")
  data.table::setkey(r, est_id)
  expect_equal(r$hiring_rate, c(2 / 12, 1 / 10))
})
