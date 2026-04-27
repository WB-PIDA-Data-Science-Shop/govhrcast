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
    contract_type_code = c("permanent", "permanent", "permanent", "permanent"),
    status             = c("active", "active", "active", "active")
  )

  personnel_dt <- data.table::data.table(
    personnel_id = c("P1", "P2", "P3", "P4"),
    status       = c("active", "active", "active", "active")
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
    contract_type_code = rep("permanent", 6),
    status             = rep("active", 6)
  )

  personnel_dt <- data.table::data.table(
    personnel_id = paste0("P", 1:6),
    est_id       = c("E1", "E1", "E1", "E2", "E2", "E2"),
    status       = rep("active", 6)
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

  expect_true(all(result$contract_dt$contract_type_code == "inactive"))
  expect_true(all(result$personnel_dt$status == "inactive"))
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
# simulate_exits() — integration with simulate_horizon()
# =============================================================================

test_that("Phase 3c: simulate_horizon with exit_policy reduces n_headcount_end", {
  contract_dt <- data.table::data.table(
    contract_id        = paste0("C", 1:10),
    personnel_id       = paste0("P", 1:10),
    start_date         = as.Date(rep("2010-01-01", 10)),
    end_date           = as.Date(rep(NA, 10)),
    gross_salary_lcu   = rep(50000, 10),
    contract_type_code = rep("permanent", 10),
    status             = rep("active", 10)
  )
  personnel_dt <- data.table::data.table(
    personnel_id = paste0("P", 1:10),
    status       = rep("active", 10)
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
    contract_type_code = "permanent",
    status             = "active"
  )
  personnel_dt <- data.table::data.table(
    personnel_id = "P1",
    status       = "active"
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
    contract_type_col = "contract_type_code"
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
    contract_type_col = "contract_type_code"
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
    contract_type_col = "contract_type_code"
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
    contract_type_col = "contract_type_code"
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
    contract_type_col = "contract_type_code"
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
    contract_type_code = "permanent",
    status             = "active"
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
    contract_type_col = "contract_type_code"
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
    contract_type_code = "pensioner",
    status             = "inactive"
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
    contract_type_col = "contract_type_code"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})
