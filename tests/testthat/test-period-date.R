# The projection uses the date it is given, not a column of that name (B-86)
#
# Inside data.table brackets a bare `ref_date` reads a column called ref_date
# when the table has one. During a projection that column stays at the start
# date, so reading it means every year is judged as if it were the first: hires
# from year 2 on never count as staff, and leavers get the wrong end date.

library(data.table)

start_snapshot <- as.Date("2020-01-01")
this_year      <- as.Date("2022-01-01")

# Four people on the start snapshot; the date column stays at the start date
frozen_contracts <- function(start_date = as.Date("2010-01-01"), end_date = as.Date(NA)) {
  data.table(
    ref_date = start_snapshot, contract_id = paste0("C", 1:4), personnel_id = paste0("P", 1:4),
    est_id = "E1", start_date = start_date, end_date = end_date,
    contract_type = "permanent", gross_salary_lcu = 100
  )
}
frozen_personnel <- function() {
  data.table(ref_date = start_snapshot, personnel_id = paste0("P", 1:4),
             employment_status = "active")
}

test_that("get_active_contracts(): someone hired after the start snapshot is active", {
  hired_last_year <- frozen_contracts(start_date = as.Date("2021-01-01"))
  expect_equal(nrow(get_active_contracts(hired_last_year, this_year)), 4L)
  # ... and not yet active before they started
  expect_equal(nrow(get_active_contracts(hired_last_year, as.Date("2020-06-01"))), 0L)
})

test_that("get_active_contracts(): a contract that ended before this year is not active", {
  ended_last_year <- frozen_contracts(end_date = as.Date("2021-01-01"))
  expect_equal(nrow(get_active_contracts(ended_last_year, this_year)), 0L)
  expect_equal(nrow(get_active_contracts(ended_last_year, as.Date("2020-06-01"))), 4L)
})

test_that("update_contracts_for_exits(): leavers end on this year's date", {
  ct <- frozen_contracts()
  update_contracts_for_exits(ct, data.table(personnel_id = "P1"), this_year)
  expect_equal(ct[personnel_id == "P1", end_date], this_year)
  expect_true(all(is.na(ct[personnel_id != "P1", end_date])))
})

test_that("update_contracts_for_retirees(): retirees end on this year's date", {
  ct <- frozen_contracts()
  update_contracts_for_retirees(ct, data.table(personnel_id = "P1"), this_year)
  expect_equal(ct[personnel_id == "P1", end_date], this_year)
})

test_that("compute_flow_demand(): a retiree hired after the start snapshot still counts", {
  # No retirees table, so it works them out; their contract only counts as
  # active at this year's date
  ct <- frozen_contracts(start_date = as.Date("2021-01-01"))
  pt <- frozen_personnel()[, age := c(65, 40, 40, 40)]
  demand <- compute_flow_demand(
    ct, pt,
    policy_params = list(group_cols = "est_id", replacement_rate = 1,
                         defaults = list(eligibility_type = "age_only", min_age = 60)),
    leavers_dt = NULL, ref_date = this_year
  )
  expect_equal(demand[est_id == "E1", total_hires], 1L)
})

test_that("downsizing ends contracts on this year's date, with and without groups", {
  scale <- data.table(est_id = "E1", gross_salary_lcu = 100)
  grouped <- simulate_hiring(
    frozen_contracts(), frozen_personnel(), ref_date = this_year,
    policy_params = list(mode = "stock", group_cols = "est_id", salary_scale = scale,
                         stock_targets = data.table(est_id = "E1", target_stock = 2L))
  )
  expect_equal(grouped$contract_dt[contract_type == "inactive", unique(end_date)], this_year)

  ungrouped <- simulate_hiring(
    frozen_contracts(), frozen_personnel(), ref_date = this_year,
    policy_params = list(mode = "stock", group_cols = NULL, salary_scale = scale,
                         stock_targets = data.table(target_stock = 2L))
  )
  expect_equal(ungrouped$contract_dt[contract_type == "inactive", unique(end_date)], this_year)
})

test_that("simulate_horizon(): hires from year 2 on count as staff, and leavers get their own year", {
  start <- as.Date("2012-09-01")
  hc <- bra_hrmis_contract[ref_date <= start]
  hp <- bra_hrmis_personnel[ref_date <= start]
  scale <- hc[ref_date == start & !is.na(gross_salary_lcu),
              .(gross_salary_lcu = mean(gross_salary_lcu)), by = est_id]
  run <- suppressMessages(simulate_horizon(
    hc, hp, scale, n_periods = 5L, ref_date = start,
    retirement_policy = NULL,
    exit_policy   = design_exit_policy_status_quo(group_cols = "est_id"),
    hiring_policy = design_hiring_policy_status_quo(group_cols = "est_id"),
    output_control_args = list(return_microdata = TRUE), seed = 1L
  ))
  last_year <- run$contract_dt[ref_date == max(ref_date)]

  # Someone hired after the start has since left: they were counted as staff
  later_hires <- last_year[start_date > start]
  expect_gt(nrow(later_hires[contract_type == "inactive"]), 0L)

  # Leavers end on the year they left, not all on the start date
  expect_gt(uniqueN(last_year[contract_type == "inactive" & start_date <= start, end_date]), 1L)
  expect_gt(last_year[contract_type == "inactive", max(end_date)], start)
})
