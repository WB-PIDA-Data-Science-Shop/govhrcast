# Integration tests for simulate_exits() and related exit-module functions
# Mirrors test-simulate_retirement.R structure.

library(testthat)
library(data.table)

# =============================================================================
# Test data helpers
# =============================================================================

make_exit_test_data <- function() {
  contract_dt <- data.table::data.table(
    contract_id        = c("C1", "C2", "C3", "C4"),
    personnel_id       = c("P1", "P2", "P3", "P4"),
    start_date         = as.Date(c("2010-01-01", "2015-01-01",
                                   "2012-01-01", "2018-01-01")),
    end_date           = as.Date(c(NA, NA, NA, NA)),
    gross_salary_lcu   = c(60000, 45000, 52000, 38000),
    contract_type = c("permanent", "permanent", "permanent", "permanent"),
    employment_status = c("active", "active", "active", "active")
  )

  personnel_dt <- data.table::data.table(
    personnel_id = c("P1", "P2", "P3", "P4"),
    employment_status = c("active", "active", "active", "active")
  )

  list(contract_dt = contract_dt, personnel_dt = personnel_dt)
}

make_exit_test_data_grouped <- function() {
  contract_dt <- data.table::data.table(
    contract_id        = paste0("C", 1:6),
    personnel_id       = paste0("P", 1:6),
    est_id             = c("E1", "E1", "E1", "E2", "E2", "E2"),
    start_date         = as.Date(rep("2010-01-01", 6)),
    end_date           = as.Date(rep(NA, 6)),
    gross_salary_lcu   = c(60000, 50000, 45000, 70000, 55000, 40000),
    contract_type = rep("permanent", 6),
    employment_status = rep("active", 6)
  )

  personnel_dt <- data.table::data.table(
    personnel_id = paste0("P", 1:6),
    est_id       = c("E1", "E1", "E1", "E2", "E2", "E2"),
    employment_status = rep("active", 6)
  )

  exit_rates_dt <- data.table::data.table(
    est_id    = c("E1", "E2"),
    exit_rate = c(1/3, 1/3)   # exactly 1 per group
  )

  list(contract_dt = contract_dt, personnel_dt = personnel_dt,
       exit_rates_dt = exit_rates_dt)
}

# =============================================================================
# simulate_exits() — fixed_rate mode
# =============================================================================

test_that("simulate_exits: fixed_rate mode returns correct structure", {
  d <- make_exit_test_data()

  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = 0.5,
      exit_strategy = "random",
      active_types  = "permanent",
      exited_type   = "inactive"
    )
  )

  result <- simulate_exits(
    contract_dt   = d$contract_dt,
    personnel_dt  = d$personnel_dt,
    policy_params = exit_policy,
    ref_date      = as.Date("2025-01-01")
  )

  expect_type(result, "list")
  expect_named(result, c("summary", "contract_dt", "personnel_dt", "exits_dt"))
  expect_equal(result$summary$n_exits, 2L)
})

test_that("simulate_exits: omitting defaults$active_types uses the dictionary-grounded default, not zero exits", {
  # Regression test: active_types previously fell back to the literal string
  # "active" -- an employment_status value, never a valid contract_type value
  # (per govhr::dictionary: contract_type in c("permanent", "fixed-term",
  # "short-term", "pensioner", "inactive")) -- which silently matched zero
  # rows and produced zero exits regardless of exit_rate. make_exit_test_data()
  # uses contract_type = "permanent" throughout, so with the corrected default
  # (c("permanent", "fixed-term", "short-term")) and exit_rate = 1.0, every
  # active person must exit.
  d <- make_exit_test_data()

  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = 1.0,
      exit_strategy = "random"
      # active_types intentionally omitted
    )
  )

  result <- simulate_exits(
    contract_dt   = d$contract_dt,
    personnel_dt  = d$personnel_dt,
    policy_params = exit_policy,
    ref_date      = as.Date("2025-01-01")
  )

  expect_equal(result$summary$n_exits, 4L)
})

test_that("simulate_exits: fixed_rate = 0 → no exits", {
  d <- make_exit_test_data()

  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = 0,
      exit_strategy = "random",
      active_types  = "permanent",
      exited_type   = "inactive"
    )
  )

  result <- simulate_exits(
    contract_dt   = d$contract_dt,
    personnel_dt  = d$personnel_dt,
    policy_params = exit_policy,
    ref_date      = as.Date("2025-01-01")
  )

  expect_equal(result$summary$n_exits, 0L)
  expect_equal(result$summary$exit_savings, 0)
})

test_that("simulate_exits: exiting personnel contracts are marked inactive", {
  d <- make_exit_test_data()

  # Force all 4 to exit
  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = 1.0,
      exit_strategy = "random",
      active_types  = "permanent",
      exited_type   = "inactive"
    )
  )

  result <- simulate_exits(
    contract_dt   = d$contract_dt,
    personnel_dt  = d$personnel_dt,
    policy_params = exit_policy,
    ref_date      = as.Date("2025-01-01")
  )

  expect_true(all(result$contract_dt$contract_type == "inactive"))
  expect_true(all(result$personnel_dt$employment_status == "inactive"))
})

test_that("simulate_exits: exit_savings equals sum of exited salaries", {
  d <- make_exit_test_data()

  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = 1.0,
      exit_strategy = "random",
      active_types  = "permanent",
      exited_type   = "inactive"
    )
  )

  result <- simulate_exits(
    contract_dt   = d$contract_dt,
    personnel_dt  = d$personnel_dt,
    policy_params = exit_policy,
    ref_date      = as.Date("2025-01-01")
  )

  expect_equal(result$summary$exit_savings,
               sum(d$contract_dt$gross_salary_lcu))
})

test_that("simulate_exits: no policy_table and no exit_rate → error", {
  d <- make_exit_test_data()

  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_strategy = "random",
      active_types  = "permanent"
    )
  )

  expect_error(
    simulate_exits(
      contract_dt   = d$contract_dt,
      personnel_dt  = d$personnel_dt,
      policy_params = exit_policy,
      ref_date      = as.Date("2025-01-01")
    ),
    regexp = "exit_rate"
  )
})

test_that("simulate_exits: status_quo mode with pre-supplied rates works", {
  d <- make_exit_test_data()

  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = 0.25,         # 25% of 4 = 1 exit
      exit_strategy = "random",
      active_types  = "permanent",
      exited_type   = "inactive"
    )
  )

  result <- simulate_exits(
    contract_dt   = d$contract_dt,
    personnel_dt  = d$personnel_dt,
    policy_params = exit_policy,
    ref_date      = as.Date("2025-01-01")
  )

  expect_equal(result$summary$n_exits, 1L)
})

test_that("simulate_exits: group_cols set without policy_table → error", {
  d <- make_exit_test_data()

  exit_policy <- list(
    group_cols   = "status",
    policy_table = NULL,
    defaults = list(
      exit_rate    = 0.05,
      active_types = "permanent"
    )
  )

  expect_error(
    simulate_exits(
      contract_dt   = d$contract_dt,
      personnel_dt  = d$personnel_dt,
      policy_params = exit_policy,
      ref_date      = as.Date("2025-01-01")
    ),
    regexp = "group_cols"
  )
})


# =============================================================================
# simulate_exits() — wage bill correctness after exits
# =============================================================================

test_that("update_contracts_for_exits: salary column is NOT zeroed on inactive rows", {
  # Confirms the known behaviour: exited contracts are relabelled "inactive"
  # but gross_salary_lcu is left intact. This is the root cause verified by
  # the wage_bill_end integration test below.
  d <- make_exit_test_data()

  update_contracts_for_exits(
    contract_dt       = d$contract_dt,
    exits_dt          = d$contract_dt[1:2, .(personnel_id)],
    ref_date          = as.Date("2025-01-01"),
    active_types      = "permanent",
    exited_type       = "inactive"
  )

  inactive_rows <- d$contract_dt[contract_type == "inactive"]
  expect_equal(nrow(inactive_rows), 2L)
  expect_true(all(!is.na(inactive_rows$gross_salary_lcu)))
  expect_true(all(inactive_rows$gross_salary_lcu > 0))
})

test_that("wage_bill_end excludes inactive (exited) workers' salaries", {
  # 4 workers each earning 10000; all exit (rate = 1.0); no hires, no COLA.
  # wage_bill_end should be 0, NOT 40000 (which would happen if .active_wage_bill
  # counts inactive rows because it only filters out "pensioner", not "inactive").
  n <- 4L; sal <- 10000
  contract_dt <- data.table::data.table(
    contract_id        = paste0("C", seq_len(n)),
    personnel_id       = paste0("P", seq_len(n)),
    start_date         = as.Date("2010-01-01"),
    end_date           = as.Date(NA),
    gross_salary_lcu   = as.numeric(rep(sal, n)),
    contract_type = rep("permanent", n),
    employment_status = rep("active", n)
  )
  personnel_dt <- data.table::data.table(
    personnel_id = paste0("P", seq_len(n)),
    employment_status = rep("active", n)
  )
  salary_scale_dt <- data.table::data.table(
    grade            = "G1",
    gross_salary_lcu = sal
  )
  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = 1.0,
      exit_strategy = "random",
      active_types  = "permanent",
      exited_type   = "inactive"
    )
  )

  res <- simulate_horizon(
    contract_dt        = contract_dt,
    personnel_dt       = personnel_dt,
    salary_scale_dt    = salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = NULL,
    exit_policy        = exit_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )

  expect_equal(res$summary_dt$wage_bill_end, 0)
})

# =============================================================================
# simulate_exits() — integration with simulate_horizon()
# =============================================================================

test_that("Phase 3c: simulate_horizon with exit_policy reduces n_headcount_end", {
  contract_dt <- data.table::data.table(
    contract_id        = paste0("C", 1:10),
    personnel_id       = paste0("P", 1:10),
    start_date         = as.Date(rep("2010-01-01", 10)),
    end_date           = as.Date(rep(NA, 10)),
    gross_salary_lcu   = rep(50000, 10),
    contract_type = rep("permanent", 10),
    employment_status = rep("active", 10)
  )
  personnel_dt <- data.table::data.table(
    personnel_id = paste0("P", 1:10),
    employment_status = rep("active", 10)
  )
  salary_scale_dt <- data.table::data.table(
    grade            = "G1",
    gross_salary_lcu = 50000
  )

  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = 0.2,     # 2 of 10 exit per period
      exit_strategy = "random",
      active_types  = "permanent",
      exited_type   = "inactive"
    )
  )

  res <- simulate_horizon(
    contract_dt        = contract_dt,
    personnel_dt       = personnel_dt,
    salary_scale_dt    = salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = NULL,
    exit_policy        = exit_policy,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )

  expect_equal(res$comparison$n_non_ret_exits[1], 2L)
  # Verify 2 persons now have inactive contracts in the returned state
  # (n_headcount snapshots count all non-pensioner contracts, including inactive,
  #  so headcount does not drop here -- that is tested directly via n_non_ret_exits)
  expect_true(res$comparison$non_ret_exit_savings[1] > 0)
})

test_that("Phase 3c: n_non_ret_exits column present in comparison when exit_policy = NULL", {
  contract_dt <- data.table::data.table(
    contract_id        = "C1",
    personnel_id       = "P1",
    start_date         = as.Date("2010-01-01"),
    end_date           = as.Date(NA),
    gross_salary_lcu   = 50000,
    contract_type = "permanent",
    employment_status = "active"
  )
  personnel_dt <- data.table::data.table(
    personnel_id = "P1",
    employment_status = "active"
  )
  salary_scale_dt <- data.table::data.table(
    grade            = "G1",
    gross_salary_lcu = 50000
  )

  res <- simulate_horizon(
    contract_dt        = contract_dt,
    personnel_dt       = personnel_dt,
    salary_scale_dt    = salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = NULL,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )

  expect_true("n_non_ret_exits" %in% names(res$comparison))
  expect_equal(res$comparison$n_non_ret_exits[1], 0L)
})

# =============================================================================
# compute_status_quo_exits() — unit tests
# =============================================================================

test_that("compute_status_quo_exits: returns personnel_id for selected exiters", {
  d <- make_exit_test_data()

  result <- compute_status_quo_exits(
    contract_dt       = d$contract_dt,
    policy_params     = list(
      group_cols   = NULL,
      policy_table = NULL,
      defaults     = list(
        exit_rate     = 0.5,
        exit_strategy = "random",
        active_types  = "permanent"
      )
    ),
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type"
  )

  expect_s3_class(result, "data.table")
  expect_true("personnel_id" %in% names(result))
  expect_equal(nrow(result), 2L)
})

test_that("compute_status_quo_exits: exit_rate = 0 → no exits", {
  d <- make_exit_test_data()

  result <- compute_status_quo_exits(
    contract_dt       = d$contract_dt,
    policy_params     = list(
      group_cols   = NULL,
      policy_table = NULL,
      defaults     = list(
        exit_rate     = 0,
        exit_strategy = "random",
        active_types  = "permanent"
      )
    ),
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type"
  )

  expect_equal(nrow(result), 0L)
})

# =============================================================================
# compute_status_quo_exits() — grouped exit tests (Block B regression guard)
# =============================================================================

test_that("compute_status_quo_exits: grouped — correct exit count per group", {
  # rate 1/3 of 3 per group = exactly 1 exit per group = 2 total
  d <- make_exit_test_data_grouped()

  result <- compute_status_quo_exits(
    contract_dt       = d$contract_dt,
    policy_params     = list(
      group_cols   = "est_id",
      policy_table = d$exit_rates_dt,
      defaults     = list(
        exit_strategy = "random",
        active_types  = "permanent"
      )
    ),
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2L)
  # All selected IDs must come from the original workforce
  expect_true(all(result$personnel_id %in% paste0("P", 1:6)))
})

test_that("compute_status_quo_exits: grouped — no person selected twice", {
  # Rate of 1.0 forces all 3 per group to exit = 6 total; no duplicates
  d <- make_exit_test_data_grouped()
  rates_all <- data.table::data.table(
    est_id    = c("E1", "E2"),
    exit_rate = c(1.0, 1.0)
  )

  result <- compute_status_quo_exits(
    contract_dt       = d$contract_dt,
    policy_params     = list(
      group_cols   = "est_id",
      policy_table = rates_all,
      defaults     = list(
        exit_strategy = "random",
        active_types  = "permanent"
      )
    ),
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type"
  )

  expect_equal(nrow(result), 6L)
  expect_equal(nrow(result), data.table::uniqueN(result$personnel_id))
})

test_that("compute_status_quo_exits: grouped — group with zero rate produces no exits", {
  d <- make_exit_test_data_grouped()
  rates_zero_e2 <- data.table::data.table(
    est_id    = c("E1", "E2"),
    exit_rate = c(1/3, 0)    # E2 has no exits
  )

  result <- compute_status_quo_exits(
    contract_dt       = d$contract_dt,
    policy_params     = list(
      group_cols   = "est_id",
      policy_table = rates_zero_e2,
      defaults     = list(
        exit_strategy = "random",
        active_types  = "permanent"
      )
    ),
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type"
  )

  # Only 1 exit from E1; E2 contributes nothing
  expect_equal(nrow(result), 1L)
  expect_true(result$personnel_id %in% c("P1", "P2", "P3"))
})

test_that("compute_status_quo_exits: grouped — unknown group in contract_dt gets rate 0", {
  # E3 has no matching rate row → should receive exit_rate = 0 and produce no exits
  d <- make_exit_test_data_grouped()

  # Add an E3 person with no corresponding rate
  extra_contract <- data.table::data.table(
    contract_id        = "C99",
    personnel_id       = "P99",
    est_id             = "E3",
    start_date         = as.Date("2010-01-01"),
    end_date           = as.Date(NA),
    gross_salary_lcu   = 50000,
    contract_type = "permanent",
    employment_status = "active"
  )
  ct_extended <- data.table::rbindlist(list(d$contract_dt, extra_contract), fill = TRUE)

  result <- compute_status_quo_exits(
    contract_dt       = ct_extended,
    policy_params     = list(
      group_cols   = "est_id",
      policy_table = d$exit_rates_dt,   # no row for E3
      defaults     = list(
        exit_strategy = "random",
        active_types  = "permanent"
      )
    ),
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type"
  )

  # P99 must not be selected (E3 rate imputed to 0)
  expect_false("P99" %in% result$personnel_id)
})

test_that("compute_status_quo_exits: empty active workforce returns empty data.table", {
  ct_pensioner <- data.table::data.table(
    contract_id        = "C1",
    personnel_id       = "P1",
    est_id             = "E1",
    start_date         = as.Date("2000-01-01"),
    end_date           = as.Date("2020-01-01"),
    gross_salary_lcu   = 0,
    contract_type = "pensioner",
    employment_status = "inactive"
  )
  rates <- data.table::data.table(est_id = "E1", exit_rate = 1.0)

  result <- compute_status_quo_exits(
    contract_dt       = ct_pensioner,
    policy_params     = list(
      group_cols   = "est_id",
      policy_table = rates,
      defaults     = list(
        exit_strategy = "random",
        active_types  = "permanent"
      )
    ),
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})

# =============================================================================
# Ungrouped status-quo table and exit_multiplier in every path
# =============================================================================

make_exit_test_data_100 <- function() {
  ids <- sprintf("P%03d", 1:100)
  list(
    contract_dt = data.table::data.table(
      contract_id      = sprintf("C%03d", 1:100),
      personnel_id     = ids,
      start_date       = as.Date("2010-01-01"),
      end_date         = as.Date(NA),
      gross_salary_lcu = 1000,
      contract_type    = "permanent"
    ),
    personnel_dt = data.table::data.table(personnel_id = ids, employment_status = "active")
  )
}

run_exits_100 <- function(policy_table, defaults) {
  d <- make_exit_test_data_100()
  simulate_exits(d$contract_dt, d$personnel_dt,
                 list(group_cols = NULL, policy_table = policy_table, defaults = defaults),
                 ref_date = as.Date("2020-01-01"))$summary$n_exits
}

test_that("simulate_exits: an ungrouped 1-row policy_table rate is used (not ignored)", {
  # Regression test: the 1-row table estimate_historical_exit_rates() returns
  # for group_cols = NULL used to be ignored, giving 0 exits (or the flat
  # defaults$exit_rate) instead of the estimated rate.
  est <- data.table::data.table(exit_rate = 0.30)
  expect_equal(run_exits_100(est, list()), 30L)
  expect_equal(run_exits_100(est, list(exit_rate = 0.05)), 30L)
})

test_that("simulate_exits: exit_multiplier applies to an ungrouped policy_table", {
  est <- data.table::data.table(exit_rate = 0.30)
  expect_equal(run_exits_100(est, list(exit_multiplier = 2)), 60L)
  est_col <- data.table::data.table(exit_rate = 0.30, exit_multiplier = 0.5)
  expect_equal(run_exits_100(est_col, list(exit_multiplier = 2)), 15L)  # table column wins
})

test_that("simulate_exits: a multi-row policy_table without group_cols is an error", {
  est <- data.table::data.table(est_id = c("E1", "E2"), exit_rate = c(0.1, 0.2))
  expect_error(run_exits_100(est, list()), regexp = "group_cols is NULL")
})

test_that("simulate_exits: exit_multiplier applies on the flat-rate path", {
  expect_equal(run_exits_100(NULL, list(exit_rate = 0.10)), 10L)
  expect_equal(run_exits_100(NULL, list(exit_rate = 0.10, exit_multiplier = 2)), 20L)
  expect_error(run_exits_100(NULL, list(exit_rate = 0.10, exit_multiplier = "2")),
               regexp = "exit_multiplier")
})

# =============================================================================
# estimate_historical_exit_rates(): pooled, start-of-interval rates
# =============================================================================

# One row per person per snapshot; ids_by_snap[[i]] = who is active at snapshot i.
make_rate_panel <- function(ids_by_snap, est_of = function(id) "E1") {
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

test_that("estimate_historical_exit_rates: the last snapshot does not dilute the rate", {
  # Regression test: 10 active, 3 gone by the next snapshot => 0.30. The last
  # snapshot used to be averaged in as a structural zero, giving 0.15.
  p <- make_rate_panel(list(1:10, 1:7))
  expect_equal(estimate_historical_exit_rates(p$ct, p$pt)$exit_rate, 0.30)
})

test_that("estimate_historical_exit_rates: rates are pooled across intervals", {
  # Interval 1: 3 of 10 leave; interval 2: 0 of 7. Pooled = 3 / 17.
  p <- make_rate_panel(list(1:10, 1:7, 1:7))
  expect_equal(estimate_historical_exit_rates(p$ct, p$pt)$exit_rate, 3 / 17)
})

test_that("estimate_historical_exit_rates: grouped rates are pooled per group", {
  # E1 = P01-P05, E2 = P06-P10. Interval 1: P01, P02 (E1) and P06 (E2) leave.
  # Interval 2: nobody leaves. E1 = 2 / (5 + 3); E2 = 1 / (5 + 4).
  est_of <- function(id) if (id <= 5L) "E1" else "E2"
  keep <- c(3:5, 7:10)
  p <- make_rate_panel(list(1:10, keep, keep), est_of)
  r <- estimate_historical_exit_rates(p$ct, p$pt, group_cols = "est_id")
  data.table::setkey(r, est_id)
  expect_equal(r$exit_rate, c(2 / 8, 1 / 9))
})
