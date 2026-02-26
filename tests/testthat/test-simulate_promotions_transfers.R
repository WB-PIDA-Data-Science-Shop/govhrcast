# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
make_full_panel <- function() {
  # 2-period panel: 6 people, 2 grades
  data.table::rbindlist(list(
    data.table::data.table(
      ref_date          = as.Date("2015-01-01"),
      personnel_id      = paste0("P", 1:6),
      contract_id       = paste0("C", 1:6),
      paygrade          = c("G1", "G1", "G1", "G2", "G2", "G2"),
      gross_salary_lcu  = c(3000, 3000, 3000, 5000, 5000, 5000),
      start_date        = as.Date(c("2010-01-01", "2012-01-01", "2014-01-01",
                                    "2008-01-01", "2011-01-01", "2013-01-01")),
      end_date          = as.Date(NA),
      contract_type_code = "permanent"
    ),
    data.table::data.table(
      ref_date          = as.Date("2016-01-01"),
      personnel_id      = paste0("P", 1:6),
      contract_id       = paste0("C", 1:6),
      paygrade          = c("G2", "G1", "G1", "G2", "G2", "G1"),  # P1: G1->G2, P6: G2->G1
      gross_salary_lcu  = c(5000, 3000, 3000, 5000, 5000, 3000),
      start_date        = as.Date(c("2010-01-01", "2012-01-01", "2014-01-01",
                                    "2008-01-01", "2011-01-01", "2013-01-01")),
      end_date          = as.Date(NA),
      contract_type_code = "permanent"
    )
  ))
}

make_personnel <- function() {
  data.table::data.table(
    ref_date     = as.Date(c(rep("2015-01-01", 6), rep("2016-01-01", 6))),
    personnel_id = rep(paste0("P", 1:6), 2),
    status       = "active"
  )
}

make_salary_scale <- function() {
  data.table::data.table(
    paygrade         = c("G1", "G2"),
    gross_salary_lcu = c(3000, 5000)
  )
}

make_policy <- function(promo_mult = 1.0, transfer_mult = 1.0,
                         promo_strat = "tenure", transfer_strat = "random") {
  list(
    group_cols           = "paygrade",
    salary_scale         = make_salary_scale(),
    promotion_multiplier = promo_mult,
    transfer_multiplier  = transfer_mult,
    promotion_strategy   = promo_strat,
    transfer_strategy    = transfer_strat
  )
}

# ---------------------------------------------------------------------------
# simulate_promotions_transfers - return structure
# ---------------------------------------------------------------------------
test_that("simulate_promotions_transfers returns required list elements", {
  set.seed(1)
  result <- simulate_promotions_transfers(
    contract_dt  = make_full_panel(),
    personnel_dt = make_personnel(),
    policy_params = make_policy(),
    ref_date     = "2016-01-01"
  )
  expect_true(is.list(result))
  expected_names <- c("summary", "contract_dt", "personnel_dt",
                      "movers_dt", "baseline_matrix", "demand_dt")
  expect_true(all(expected_names %in% names(result)))
})

test_that("simulate_promotions_transfers summary has required columns", {
  set.seed(1)
  result <- simulate_promotions_transfers(
    contract_dt  = make_full_panel(),
    personnel_dt = make_personnel(),
    policy_params = make_policy(),
    ref_date     = "2016-01-01"
  )
  expected_cols <- c("n_promotions", "n_transfers", "n_total_movers",
                     "headcount_before", "headcount_after")
  expect_true(all(expected_cols %in% names(result$summary)))
})

test_that("simulate_promotions_transfers returns baseline_matrix with correct structure", {
  set.seed(1)
  result <- simulate_promotions_transfers(
    contract_dt  = make_full_panel(),
    personnel_dt = make_personnel(),
    policy_params = make_policy(),
    ref_date     = "2016-01-01"
  )
  bm <- result$baseline_matrix
  expect_true(data.table::is.data.table(bm))
  expect_true(all(c("from_group", "to_group", "avg_prob") %in% names(bm)))
})

test_that("simulate_promotions_transfers headcount unchanged after movements", {
  set.seed(1)
  result <- simulate_promotions_transfers(
    contract_dt  = make_full_panel(),
    personnel_dt = make_personnel(),
    policy_params = make_policy(),
    ref_date     = "2016-01-01"
  )
  # Promotions/transfers don't change headcount
  expect_equal(result$summary$headcount_before, result$summary$headcount_after)
})

test_that("simulate_promotions_transfers movers_dt has correct columns", {
  set.seed(1)
  result <- simulate_promotions_transfers(
    contract_dt  = make_full_panel(),
    personnel_dt = make_personnel(),
    policy_params = make_policy(),
    ref_date     = "2016-01-01"
  )
  if (nrow(result$movers_dt) > 0) {
    expect_true(all(c("personnel_id", "from_group", "to_group", "movement_type") %in%
                      names(result$movers_dt)))
  }
})

test_that("simulate_promotions_transfers accepts string ref_date", {
  set.seed(1)
  expect_no_error(
    simulate_promotions_transfers(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = make_policy(),
      ref_date     = "2016-01-01"
    )
  )
})

test_that("simulate_promotions_transfers accepts Date ref_date", {
  set.seed(1)
  expect_no_error(
    simulate_promotions_transfers(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = make_policy(),
      ref_date     = as.Date("2016-01-01")
    )
  )
})

test_that("simulate_promotions_transfers: promotion_multiplier = 0 gives 0 promotions", {
  set.seed(1)
  result <- simulate_promotions_transfers(
    contract_dt  = make_full_panel(),
    personnel_dt = make_personnel(),
    policy_params = make_policy(promo_mult = 0, transfer_mult = 0),
    ref_date     = "2016-01-01"
  )
  expect_equal(result$summary$n_total_movers, 0L)
})

test_that("simulate_promotions_transfers: contract_dt returned is snapshot only", {
  set.seed(1)
  result <- simulate_promotions_transfers(
    contract_dt  = make_full_panel(),
    personnel_dt = make_personnel(),
    policy_params = make_policy(),
    ref_date     = "2016-01-01"
  )
  # Should be single-period data
  if ("ref_date" %in% names(result$contract_dt)) {
    expect_equal(data.table::uniqueN(result$contract_dt$ref_date), 1L)
  }
})

test_that("simulate_promotions_transfers: contract_dt rows unchanged (no new/removed)", {
  set.seed(1)
  ct_full <- make_full_panel()
  snap_rows <- nrow(ct_full[ref_date == as.Date("2016-01-01")])
  result <- simulate_promotions_transfers(
    contract_dt  = ct_full,
    personnel_dt = make_personnel(),
    policy_params = make_policy(),
    ref_date     = "2016-01-01"
  )
  expect_equal(nrow(result$contract_dt), snap_rows)
})

# ---------------------------------------------------------------------------
# simulate_promotions_transfers - single snapshot (no baseline)
# ---------------------------------------------------------------------------
test_that("simulate_promotions_transfers handles single snapshot gracefully", {
  set.seed(1)
  ct_single <- make_full_panel()[ref_date == as.Date("2016-01-01")]
  # expect_message returns the condition, not the function's return value;
  # capture return value separately then assert the message was emitted
  result <- NULL
  expect_message(
    {
      result <- simulate_promotions_transfers(
        contract_dt  = ct_single,
        personnel_dt = make_personnel()[ref_date == as.Date("2016-01-01")],
        policy_params = make_policy(),
        ref_date     = "2016-01-01"
      )
    },
    "No movement baseline"
  )
  expect_equal(result$summary$n_total_movers, 0L)
})

# ---------------------------------------------------------------------------
# simulate_promotions_transfers - validation
# ---------------------------------------------------------------------------
test_that("simulate_promotions_transfers errors on missing group_cols", {
  pp <- make_policy()
  pp$group_cols <- NULL
  expect_error(
    simulate_promotions_transfers(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = pp,
      ref_date     = "2016-01-01"
    ),
    "group_cols"
  )
})

test_that("simulate_promotions_transfers errors on missing salary_scale", {
  pp <- make_policy()
  pp$salary_scale <- NULL
  expect_error(
    simulate_promotions_transfers(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = pp,
      ref_date     = "2016-01-01"
    ),
    "salary_scale"
  )
})

test_that("simulate_promotions_transfers errors on invalid promotion_strategy", {
  pp <- make_policy()
  pp$promotion_strategy <- "invalid_strategy"
  expect_error(
    simulate_promotions_transfers(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = pp,
      ref_date     = "2016-01-01"
    ),
    "promotion_strategy"
  )
})

test_that("simulate_promotions_transfers errors on invalid transfer_strategy", {
  pp <- make_policy()
  pp$transfer_strategy <- "bad"
  expect_error(
    simulate_promotions_transfers(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = pp,
      ref_date     = "2016-01-01"
    ),
    "transfer_strategy"
  )
})

test_that("simulate_promotions_transfers errors on non-datatable input", {
  pp <- make_policy()
  expect_error(
    simulate_promotions_transfers(
      contract_dt  = as.data.frame(make_full_panel()),  # data.frame not dt
      personnel_dt = make_personnel(),
      policy_params = pp,
      ref_date     = "2016-01-01"
    )
  )
})

test_that("simulate_promotions_transfers errors on invalid ref_date", {
  pp <- make_policy()
  expect_error(
    simulate_promotions_transfers(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = pp,
      ref_date     = "not-a-date"
    )
  )
})

# ---------------------------------------------------------------------------
# simulate_promotions_transfers - integration with hiring/retirement results
# ---------------------------------------------------------------------------
test_that("simulate_promotions_transfers works on subset contract_dt", {
  set.seed(1)
  # Simulate a post-retirement snapshot (subset of personnel)
  ct <- make_full_panel()[personnel_id %in% c("P1", "P2", "P3", "P4")]
  pt <- make_personnel()[personnel_id %in% c("P1", "P2", "P3", "P4")]
  expect_no_error(
    simulate_promotions_transfers(
      contract_dt  = ct,
      personnel_dt = pt,
      policy_params = make_policy(),
      ref_date     = "2016-01-01"
    )
  )
})

# ---------------------------------------------------------------------------
# check_movement_inputs
# ---------------------------------------------------------------------------
test_that("check_movement_inputs passes with valid inputs", {
  expect_invisible(
    check_movement_inputs(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = make_policy(),
      ref_date     = "2016-01-01"
    )
  )
})

test_that("check_movement_inputs errors on non-list policy_params", {
  expect_error(
    check_movement_inputs(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = "not a list",
      ref_date     = "2016-01-01"
    ),
    "list"
  )
})

test_that("check_movement_inputs errors when group_cols not in contract_dt", {
  pp <- make_policy()
  pp$group_cols <- c("nonexistent_col")
  expect_error(
    check_movement_inputs(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = pp,
      ref_date     = "2016-01-01"
    )
  )
})

test_that("check_movement_inputs errors when salary_scale lacks salary column", {
  pp <- make_policy()
  pp$salary_scale <- data.table::data.table(
    paygrade = c("G1", "G2"),
    bonus = c(100, 200)  # no salary column
  )
  expect_error(
    check_movement_inputs(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = pp,
      ref_date     = "2016-01-01"
    ),
    "salary"
  )
})

test_that("check_movement_inputs errors on invalid promotion_multiplier", {
  pp <- make_policy()
  pp$promotion_multiplier <- -1
  expect_error(
    check_movement_inputs(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = pp,
      ref_date     = "2016-01-01"
    )
  )
})

test_that("check_movement_inputs accepts zero multipliers", {
  pp <- make_policy(promo_mult = 0.0, transfer_mult = 0.0)
  expect_invisible(
    check_movement_inputs(
      contract_dt  = make_full_panel(),
      personnel_dt = make_personnel(),
      policy_params = pp,
      ref_date     = "2016-01-01"
    )
  )
})
