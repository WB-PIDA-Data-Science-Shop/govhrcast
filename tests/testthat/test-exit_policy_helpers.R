# Unit tests for R/exit_policy_helpers.R

library(testthat)
library(data.table)

.at <- c("permanent", "fixed-term", "short-term")

make_eph_staff <- function(n = 100L, est = "E1") {
  ids <- sprintf("P%03d", seq_len(n))
  list(
    ct = data.table(contract_id = sub("P", "C", ids), personnel_id = ids,
                    est_id = rep_len(est, n), start_date = as.Date("2010-01-01"),
                    end_date = as.Date(NA), gross_salary_lcu = 1000,
                    contract_type = "permanent"),
    pt = data.table(personnel_id = ids, employment_status = "active")
  )
}

n_exits_with <- function(policy, d = make_eph_staff()) {
  simulate_exits(d$ct, d$pt, policy, ref_date = as.Date("2020-01-01"))$summary$n_exits
}

# 10 active at 2019, 3 gone by 2020: historical rate 0.30.
make_eph_panel <- function() {
  ids1 <- sprintf("P%02d", 1:10); ids2 <- sprintf("P%02d", 1:7)
  ct <- data.table(
    ref_date = c(rep(as.Date("2019-01-01"), 10), rep(as.Date("2020-01-01"), 7)),
    personnel_id = c(ids1, ids2), contract_id = sub("P", "C", c(ids1, ids2)),
    est_id = "E1", contract_type = "permanent", start_date = as.Date("2010-01-01"),
    end_date = as.Date(NA), gross_salary_lcu = 1000
  )
  list(ct = ct, pt = ct[, .(ref_date, personnel_id, employment_status = "active",
                            birth_date = as.Date("1980-01-01"))])
}

horizon_exits <- function(exit_policy) {
  p <- make_eph_panel()
  simulate_horizon(
    contract_dt = p$ct, personnel_dt = p$pt,
    salary_scale_dt = data.table(gross_salary_lcu = 1000),
    n_periods = 1L, exit_policy = exit_policy, salary_growth_rate = 0,
    ref_date = as.Date("2020-01-01"), seed = 1L
  )$summary_dt$n_non_ret_exits
}

# =============================================================================
# design_exit_policy_flat()
# =============================================================================

test_that("design_exit_policy_flat builds the three-slot list", {
  expect_equal(
    design_exit_policy_flat(exit_rate = 0.05),
    list(group_cols = NULL, policy_table = NULL,
         defaults = list(exit_rate = 0.05, exit_strategy = "random", active_types = .at))
  )
})

test_that("design_exit_policy_flat drives simulate_exits()", {
  expect_equal(n_exits_with(design_exit_policy_flat(0.10)), 10L)
})

test_that("design_exit_policy_flat validates its arguments", {
  expect_error(design_exit_policy_flat(1.5), regexp = "between 0 and 1")
  expect_error(design_exit_policy_flat(-0.1), regexp = "between 0 and 1")
  expect_error(design_exit_policy_flat(c(0.1, 0.2)), regexp = "single number")
  expect_error(design_exit_policy_flat("0.1"), regexp = "single number")
  expect_error(design_exit_policy_flat(0.1, exit_strategy = c("a", "b")), regexp = "exit_strategy")
  expect_error(design_exit_policy_flat(0.1, active_types = character(0)), regexp = "active_types")
  expect_no_error(design_exit_policy_flat(0))
})

# =============================================================================
# design_exit_policy_status_quo()
# =============================================================================

test_that("design_exit_policy_status_quo builds the three-slot list with policy_table = NULL", {
  expect_equal(
    design_exit_policy_status_quo(group_cols = "est_id", exit_multiplier = 1.2),
    list(group_cols = "est_id", policy_table = NULL,
         defaults = list(exit_multiplier = 1.2, exit_strategy = "random", active_types = .at))
  )
})

test_that("design_exit_policy_status_quo validates its arguments", {
  expect_error(design_exit_policy_status_quo(group_cols = 1), regexp = "group_cols")
  expect_error(design_exit_policy_status_quo(exit_multiplier = -1), regexp = "exit_multiplier")
  expect_error(design_exit_policy_status_quo(exit_multiplier = NA_real_), regexp = "exit_multiplier")
})

test_that("design_exit_policy_status_quo is estimated by simulate_horizon() and scaled by exit_multiplier", {
  # Starting stock 7, historical rate 0.30: round(7 * 0.3) = 2; x2 => round(4.2) = 4.
  expect_equal(horizon_exits(design_exit_policy_status_quo()), 2L)
  expect_equal(horizon_exits(design_exit_policy_status_quo(exit_multiplier = 2)), 4L)
  expect_equal(horizon_exits(design_exit_policy_status_quo(group_cols = "est_id")), 2L)
})

# =============================================================================
# design_exit_policy_adjustment()
# =============================================================================

test_that("design_exit_policy_adjustment accepts estimate_historical_exit_rates() output", {
  p <- make_eph_panel()
  rates <- estimate_historical_exit_rates(p$ct, p$pt, group_cols = "est_id")
  rates[, exit_rate := exit_rate * 2]   # 0.60
  ep <- design_exit_policy_adjustment(rates, group_cols = "est_id")
  expect_equal(ep$group_cols, "est_id")
  expect_equal(ep$defaults, list(exit_rate = 0, exit_strategy = "random", active_types = .at))
  expect_equal(horizon_exits(ep), 4L)   # round(7 * 0.6) = 4
})

test_that("design_exit_policy_adjustment accepts an ungrouped one-row table", {
  ep <- design_exit_policy_adjustment(data.table(exit_rate = 0.25))
  expect_null(ep$group_cols)
  expect_equal(n_exits_with(ep), 25L)
})

test_that("design_exit_policy_adjustment applies unmatched_rate to groups missing from the table", {
  d <- make_eph_staff(est = c("E1", "E2"))   # 50 in each
  ep <- design_exit_policy_adjustment(data.table(est_id = "E1", exit_rate = 0.2),
                                      group_cols = "est_id", unmatched_rate = 0.1)
  expect_equal(n_exits_with(ep, d), 15L)   # 10 from E1 + 5 from E2
})

test_that("design_exit_policy_adjustment honours an exit_multiplier column", {
  d <- make_eph_staff(est = c("E1", "E2"))
  tbl <- data.table(est_id = c("E1", "E2"), exit_rate = 0.2, exit_multiplier = c(1, 0.5))
  expect_equal(n_exits_with(design_exit_policy_adjustment(tbl, "est_id"), d), 15L)
})

test_that("design_exit_policy_adjustment stores a copy, not a reference", {
  tbl <- data.table(exit_rate = 0.25)
  ep <- design_exit_policy_adjustment(tbl)
  tbl[, exit_rate := 0.9]
  expect_equal(ep$policy_table$exit_rate, 0.25)
})

test_that("design_exit_policy_adjustment rejects malformed tables", {
  expect_error(design_exit_policy_adjustment(data.frame(exit_rate = 0.1)), regexp = "data.table")
  expect_error(design_exit_policy_adjustment(data.table(rate = 0.1)), regexp = "exit_rate")
  expect_error(design_exit_policy_adjustment(data.table(exit_rate = numeric(0))), regexp = "no rows")
  expect_error(design_exit_policy_adjustment(data.table(exit_rate = c(0.1, 0.2))),
               regexp = "exactly one row")
  expect_error(design_exit_policy_adjustment(data.table(exit_rate = 0.1), group_cols = "est_id"),
               regexp = "est_id")
  expect_error(design_exit_policy_adjustment(data.table(est_id = c("E1", "E1"), exit_rate = 0.1),
                                             group_cols = "est_id"),
               regexp = "more than one row")
  expect_error(design_exit_policy_adjustment(data.table(exit_rate = 1.2)), regexp = "between 0 and 1")
  expect_error(design_exit_policy_adjustment(data.table(exit_rate = NA_real_)), regexp = "no NA")
  expect_error(design_exit_policy_adjustment(data.table(exit_rate = 0.1, exit_multiplier = -1)),
               regexp = "exit_multiplier")
  expect_error(design_exit_policy_adjustment(data.table(exit_rate = 0.1), unmatched_rate = 2),
               regexp = "unmatched_rate")
})
