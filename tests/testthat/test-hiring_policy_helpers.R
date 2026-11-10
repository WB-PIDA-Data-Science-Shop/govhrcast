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
    list(mode = "flow", group_cols = NULL, replacement_rate = 1.0, unmatched_rate = 0,
         salary_scale = NULL)
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

  res_c <- simulate_hiring(contract_dt, personnel_dt, policy_params = constructed, leavers_dt = retirees_dt, as_of_date = ref_date)
  res_h <- simulate_hiring(contract_dt, personnel_dt, policy_params = handwritten, leavers_dt = retirees_dt, as_of_date = ref_date)

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
  res <- simulate_hiring(contract_dt, personnel_dt, policy_params = policy, as_of_date = as.Date("2025-01-01"))

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
    n_periods = 1, retirement_policy = NULL, hiring_policy = policy, baseline_date = snap2
  )

  expect_gt(res$comparison$n_hires[1], 0L)
})


# =============================================================================
# design_hiring_policy_adjustment()
# =============================================================================

# Ten active people in each of three establishments
three_groups <- function() {
  ids <- paste0("P", 1:30)
  list(
    ct = data.table(
      contract_id = paste0("C", 1:30), personnel_id = ids, est_id = rep(c("E1", "E2", "E3"), each = 10),
      start_date = as.Date("2010-01-01"), end_date = as.Date(NA),
      contract_type = "permanent", gross_salary_lcu = 100
    ),
    pt = data.table(personnel_id = ids, employment_status = "active"),
    scale = data.table(est_id = c("E1", "E2", "E3"), gross_salary_lcu = 100)
  )
}

hires_by_group <- function(policy) {
  d <- three_groups()
  res <- simulate_hiring(d$ct, d$pt, policy_params = policy, as_of_date = as.Date("2020-01-01"))
  counts <- res$new_hire_contracts_dt[, .N, by = est_id]
  vapply(c("E1", "E2", "E3"), function(g) sum(counts[est_id == g, N]), integer(1))
}

test_that("design_hiring_policy_adjustment gives each group its own rate", {
  rates <- data.table(est_id = c("E1", "E2"), hiring_rate = c(0.5, 0))
  policy <- design_hiring_policy_adjustment(rates, group_cols = "est_id", salary_scale = three_groups()$scale)
  expect_equal(hires_by_group(policy), c(E1 = 5L, E2 = 0L, E3 = 0L))
})

test_that("design_hiring_policy_adjustment uses unmatched_rate for groups not in the table", {
  rates <- data.table(est_id = "E1", hiring_rate = 0.5)
  policy <- design_hiring_policy_adjustment(rates, group_cols = "est_id", unmatched_rate = 0.2,
                                            salary_scale = three_groups()$scale)
  expect_equal(hires_by_group(policy), c(E1 = 5L, E2 = 2L, E3 = 2L))
})

test_that("design_hiring_policy_adjustment accepts one overall rate", {
  policy <- design_hiring_policy_adjustment(data.table(hiring_rate = 0.1))
  expect_null(policy$group_cols)
  expect_equal(policy$squorate_dt$hiring_rate, 0.1)
})

test_that("design_hiring_policy_adjustment allows rates above 100%", {
  # A small group can really hire more than its starting staff in a year
  rates <- data.table(est_id = c("E1", "E2"), hiring_rate = c(1.5, 0))
  policy <- design_hiring_policy_adjustment(rates, group_cols = "est_id", salary_scale = three_groups()$scale)
  expect_equal(hires_by_group(policy), c(E1 = 15L, E2 = 0L, E3 = 0L))
})

test_that("design_hiring_policy_adjustment keeps only the columns it needs, in its own copy", {
  # A column such as current_stock in the user's table must not reach the join
  rates <- data.table(est_id = "E1", hiring_rate = 0.5, current_stock = 999, note = "reform")
  policy <- design_hiring_policy_adjustment(rates, group_cols = "est_id", salary_scale = three_groups()$scale)
  expect_identical(names(policy$squorate_dt), c("est_id", "hiring_rate"))
  expect_equal(hires_by_group(policy)[["E1"]], 5L)

  rates[, hiring_rate := 0]
  expect_equal(policy$squorate_dt$hiring_rate, 0.5)
})

test_that("design_hiring_policy_adjustment checks the table as soon as it is called", {
  adjust <- design_hiring_policy_adjustment
  expect_error(adjust(data.frame(hiring_rate = 0.1)), "must be a data.table")
  expect_error(adjust(data.table(rate = 0.1)), "'hiring_rate' column")
  expect_error(adjust(data.table(hiring_rate = numeric(0))), "no rows")
  expect_error(adjust(data.table(hiring_rate = NA_real_)), "hiring_rate must be")
  expect_error(adjust(data.table(hiring_rate = -0.1)), "hiring_rate must be")
  expect_error(adjust(data.table(hiring_rate = "0.1")), "hiring_rate must be")
  expect_error(adjust(data.table(hiring_rate = c(0.1, 0.2))), "exactly one row")
  expect_error(adjust(data.table(hiring_rate = 0.1), group_cols = "est_id"), "missing group_cols")
  expect_error(adjust(data.table(est_id = c("E1", "E1"), hiring_rate = 0.1), group_cols = "est_id"),
               "more than one row for the same group")
  expect_error(adjust(data.table(hiring_rate = 0.1), unmatched_rate = -1))
  expect_error(adjust(data.table(hiring_rate = 0.1), salary_scale = data.frame(x = 1)), "salary_scale")
})

# Through simulate_horizon(), on panel data, where rates are otherwise estimated
adjusted_horizon <- function(policy, n_periods = 2L) {
  start <- as.Date("2012-09-01")
  suppressMessages(simulate_horizon(
    bra_hrmis_contract[ref_date <= start], bra_hrmis_personnel[ref_date <= start],
    bra_hrmis_contract[ref_date == start & !is.na(gross_salary_lcu),
                       .(gross_salary_lcu = mean(gross_salary_lcu)), by = est_id],
    n_periods = n_periods, baseline_date = start, retirement_policy = NULL, hiring_policy = policy,
    output_control_args = define_output_control(return_microdata = TRUE), seed = 1L
  ))
}
historical_rates <- function() {
  start <- as.Date("2012-09-01")
  estimate_historical_hiring_rates(bra_hrmis_contract[ref_date <= start],
                                   bra_hrmis_personnel[ref_date <= start], group_cols = "est_id")
}

test_that("simulate_horizon uses your table instead of estimating, even on panel data", {
  frozen <- historical_rates()[, hiring_rate := 0]
  run <- adjusted_horizon(design_hiring_policy_adjustment(frozen, group_cols = "est_id"))
  expect_equal(sum(run$comparison$n_hires), 0L)
})

test_that("an unedited table reproduces the status quo exactly", {
  status_quo <- adjusted_horizon(design_hiring_policy_status_quo(group_cols = "est_id"))
  expect_gt(sum(status_quo$comparison$n_hires), 0L)
  unedited <- adjusted_horizon(design_hiring_policy_adjustment(historical_rates(), group_cols = "est_id"))
  expect_equal(unedited$comparison, status_quo$comparison)
})

test_that("status quo for most groups and a hiring freeze for one", {
  rates <- historical_rates()
  status_quo <- adjusted_horizon(design_hiring_policy_status_quo(group_cols = "est_id"), n_periods = 1L)
  new_status_quo <- status_quo$contract_dt[!contract_id %in% bra_hrmis_contract$contract_id]
  busiest <- new_status_quo[, .N, by = est_id][order(-N)]$est_id[1]

  rates[est_id == busiest, hiring_rate := 0]
  frozen <- adjusted_horizon(design_hiring_policy_adjustment(rates, group_cols = "est_id"), n_periods = 1L)
  new_frozen <- frozen$contract_dt[!contract_id %in% bra_hrmis_contract$contract_id]

  expect_gt(new_status_quo[est_id == busiest, .N], 0L)
  expect_equal(new_frozen[est_id == busiest, .N], 0L)
  # Every other group hires what it hired under the status quo
  others <- function(new) new[est_id != busiest, .N, keyby = est_id]
  expect_equal(others(new_frozen), others(new_status_quo))
})

test_that("simulate_horizon still stops when there is no panel and no table", {
  start <- as.Date("2012-09-01")
  expect_error(
    simulate_horizon(bra_hrmis_contract[ref_date == start], bra_hrmis_personnel[ref_date == start],
                     data.table(est_id = "E1", gross_salary_lcu = 100), n_periods = 1L, baseline_date = start,
                     retirement_policy = NULL,
                     hiring_policy = design_hiring_policy_status_quo(group_cols = "est_id")),
    "status-quo hiring needs a panel"
  )
})


# =============================================================================
# Groups missing from a replacement_rate table (B-89)
# =============================================================================

flow_retirees <- data.table(personnel_id = c("P1", "P11"), est_id = c("E1", "E2"))

test_that("flow hiring: a group with leavers but no rate replaces nobody by default", {
  d <- three_groups()
  rates <- data.table(est_id = "E1", replacement_rate = 1)
  policy <- design_hiring_policy_flow(rates, group_cols = "est_id", salary_scale = d$scale)
  res <- simulate_hiring(d$ct, d$pt, policy_params = policy, leavers_dt = flow_retirees,
                         as_of_date = as.Date("2020-01-01"))
  expect_equal(res$new_hire_contracts_dt[, .N, keyby = est_id], data.table(est_id = "E1", N = 1L, key = "est_id"))
})

test_that("flow hiring: unmatched_rate sets the rate for groups not in the table", {
  d <- three_groups()
  rates <- data.table(est_id = "E1", replacement_rate = 0)
  policy <- design_hiring_policy_flow(rates, group_cols = "est_id", salary_scale = d$scale, unmatched_rate = 1)
  res <- simulate_hiring(d$ct, d$pt, policy_params = policy, leavers_dt = flow_retirees,
                         as_of_date = as.Date("2020-01-01"))
  expect_equal(res$new_hire_contracts_dt[, .N, keyby = est_id], data.table(est_id = "E2", N = 1L, key = "est_id"))
})

test_that("design_hiring_policy_flow checks unmatched_rate", {
  expect_error(design_hiring_policy_flow(1, unmatched_rate = -1))
  expect_error(design_hiring_policy_flow(1, unmatched_rate = c(0, 1)))
})

