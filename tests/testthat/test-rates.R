# Rates turned into people, and rates estimated from history
#
# The rules, from the backlog decisions and the user's decisions:
#   * Expected counts are rounded at random (D6): a group expecting 0.3 people
#     sometimes loses one, so small groups are not stuck at zero and totals match
#     what the rates imply. The same seed gives the same result.
#   * Movement rates are pooled: moves over everyone exposed, in every pair of
#     snapshots, including pairs with no move.
#   * Hire-date hiring rates use the same base as first-appearance rates.
#   * Ages above the oldest age with a retirement rate take that age's rate.

library(data.table)

start <- as.Date("2020-01-01")

# n_groups groups of 5 staff each. The ages are 40 unless given.
small_groups <- function(n_groups = 40L, age = 40) {
  n <- n_groups * 5L
  list(
    c = data.table(
      contract_id = paste0("C", seq_len(n)),
      personnel_id = paste0("P", seq_len(n)),
      est_id = rep(paste0("E", seq_len(n_groups)), each = 5L),
      start_date = as.Date("2010-01-01"),
      end_date = as.Date(NA),
      contract_type = "permanent",
      gross_salary_lcu = 100
    ),
    p = data.table(
      personnel_id = paste0("P", seq_len(n)),
      employment_status = "active",
      birth_date = start - round(365.25 * age),
      age = age
    )
  )
}

# The average of f() over 30 seeds. Set up so that plain rounding gives 0.
average_over_seeds <- function(f) {
  mean(vapply(1:30, function(seed) { set.seed(seed); as.numeric(f()) }, numeric(1)))
}

# -- Rounding ----------------------------------------------------------------

test_that("stochastic_round works on a vector, and each value rounds down or up", {
  set.seed(1)
  x <- c(0, 0.3, 2.5, 7)
  r <- stochastic_round(x)
  expect_type(r, "integer")
  expect_length(r, 4L)
  expect_true(all(r >= floor(x) & r <= ceiling(x)))
  expect_equal(r[c(1, 4)], c(0L, 7L))
})

test_that("stochastic_round is right on average", {
  set.seed(1)
  expect_equal(mean(stochastic_round(rep(0.3, 20000))), 0.3, tolerance = 0.05)
})

test_that("grouped exit rates give the expected total, however small the groups", {
  d <- small_groups()
  policy <- list(group_cols = "est_id",
                 policy_table = data.table(est_id = unique(d$c$est_id), exit_rate = 0.05),
                 defaults = list(exit_strategy = "random"))
  # 5 x 0.05 = 0.25 in each of 40 groups: plain rounding says nobody ever exits
  n <- average_over_seeds(function() nrow(compute_status_quo_exits(d$c, policy)))
  expect_gt(n, 8.5)
  expect_lt(n, 11.5)
})

test_that("flat and ungrouped exit rates are rounded the same way", {
  d <- small_groups(n_groups = 1L)
  flat <- list(defaults = list(exit_rate = 0.05, exit_strategy = "random"))
  expect_gt(average_over_seeds(function() nrow(compute_fixed_rate_exits(d$c, flat))), 0)
  one_row <- list(group_cols = NULL, policy_table = data.table(exit_rate = 0.05),
                  defaults = list(exit_strategy = "random"))
  expect_gt(average_over_seeds(function() nrow(compute_status_quo_exits(d$c, one_row))), 0)
})

test_that("retirement rates give the expected total, however small the groups", {
  d <- small_groups(age = 60)
  policy <- eligibility_status_quo(
    hazard_table = data.table(age_yr_int = 60L, est_id = unique(d$c$est_id), hazard_rate = 0.05),
    group_cols = "est_id"
  )
  n <- average_over_seeds(function() {
    nrow(compute_status_quo_retirements(d$c, d$p, policy, start))
  })
  expect_gt(n, 8.5)
  expect_lt(n, 11.5)
})

test_that("status-quo hiring gives the expected total, however small the groups", {
  d <- small_groups()
  policy <- list(group_cols = "est_id",
                 squorate_dt = data.table(est_id = unique(d$c$est_id), hiring_rate = 0.05))
  n <- average_over_seeds(function() {
    sum(compute_status_quo_hiring(d$c, d$p, policy, start)$total_hires)
  })
  expect_gt(n, 8.5)
  expect_lt(n, 11.5)
})

test_that("flow hiring gives the expected total when replacing part of one leaver", {
  d <- small_groups()
  leavers <- d$c[, .SD[1L], by = est_id][, .(personnel_id, est_id)]
  policy <- list(group_cols = "est_id", replacement_rate = 0.25)
  n <- average_over_seeds(function() {
    sum(compute_flow_demand(d$c, d$p, policy, leavers, start)$total_hires)
  })
  expect_gt(n, 8.5)
  expect_lt(n, 11.5)
})

test_that("the same seed gives the same people and the same totals", {
  d <- small_groups()
  policy <- design_exit_policy_flat(exit_rate = 0.05)
  scale <- data.table(est_id = unique(d$c$est_id), gross_salary_lcu = 100)
  run <- function() {
    simulate_horizon(d$c, d$p, scale, n_periods = 3L, baseline_date = start,
                     retirement_policy = NULL, exit_policy = policy, seed = 7L)$comparison
  }
  expect_equal(run(), run())
})

# -- Movement rates ----------------------------------------------------------

# 10 people in A at each of 4 snapshots. One moves to B, in one pair only.
one_move_in_three_pairs <- function() {
  dates <- as.Date(c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01"))
  rbindlist(lapply(seq_along(dates), function(i) {
    data.table(
      ref_date = dates[i], contract_id = "C1", gross_salary_lcu = 100,
      personnel_id = paste0("P", 1:10),
      paygrade = c(if (i <= 2L) "A" else "B", rep("A", 9L)),
      start_date = as.Date("2010-01-01"),
      end_date = as.Date(NA),
      contract_type = "permanent"
    )
  }))
}

test_that("a movement rate counts the pairs of snapshots with no move", {
  rates <- estimate_historical_movement_rates(one_move_in_three_pairs(), group_cols = "paygrade")
  # 1 move, over 10 + 10 + 9 people in A at the start of the three pairs
  expect_equal(rates[from_group == "A" & to_group == "B", movement_rate], 1 / 29)
  expect_equal(rates[from_group == "A" & to_group == "B", n_periods], 1L)
})

test_that("the movement rate is what the history shows, when applied to the history", {
  panel <- bra_hrmis_contract[ref_date <= as.Date("2012-09-01")]
  rates <- estimate_historical_movement_rates(panel, group_cols = "est_id")
  moves <- function(snap, nxt) {
    a <- unique(panel[ref_date == snap & contract_type != "inactive" & !is.na(est_id),
                      .(personnel_id, from = est_id)], by = "personnel_id")
    b <- unique(panel[ref_date == nxt & contract_type != "inactive" & !is.na(est_id),
                      .(personnel_id, to = est_id)], by = "personnel_id")
    a[b, on = "personnel_id", nomatch = NULL][from != to, .N]
  }
  dates <- sort(unique(panel$ref_date))
  per_year <- mean(mapply(moves, dates[-length(dates)], dates[-1L]))
  expected_from <- function(snap) {
    stock <- panel[ref_date == snap & contract_type != "inactive" & !is.na(est_id),
                   .(n = uniqueN(personnel_id)), by = .(from_group = est_id)]
    rates[stock, on = "from_group", nomatch = NULL][, sum(movement_rate * n)]
  }
  expected <- mean(sapply(dates[-length(dates)], expected_from))
  expect_lt(abs(expected / per_year - 1), 0.05)
})

# -- Hire-date hiring rates --------------------------------------------------

test_that("a hire-date rate is hires over the staff at the start of each interval", {
  dates <- as.Date(c("2020-01-01", "2021-01-01", "2022-01-01"))
  people <- data.table(
    personnel_id = paste0("P", 1:12),
    first_employment_date = as.Date(c(rep("2010-01-01", 10), "2020-06-01", "2021-06-01"))
  )
  on_snapshot <- list(paste0("P", 1:10), paste0("P", 1:11), paste0("P", 1:12))
  hp <- rbindlist(lapply(1:3, function(i) {
    people[personnel_id %in% on_snapshot[[i]]][, `:=`(ref_date = dates[i], employment_status = "active")]
  }))
  hc <- hp[, .(personnel_id, ref_date, contract_id = paste0("C", personnel_id),
               start_date = as.Date("2010-01-01"), end_date = as.Date(NA),
               contract_type = "permanent", est_id = "E1")]
  spec <- define_column_spec(hire_date_col = "first_employment_date")
  # 2 hires, on 10 and then 11 people at the start of the intervals
  expect_equal(estimate_historical_hiring_rates(hc, hp, NULL, spec)$hiring_rate, 2 / 21)
  expect_equal(estimate_historical_hiring_rates(hc, hp, "est_id", spec)$hiring_rate, 2 / 21)
  # and the same as counting first appearances
  expect_equal(estimate_historical_hiring_rates(hc, hp, NULL)$hiring_rate, 2 / 21)
})

test_that("a person shown before their hire date is dated to the first snapshot after it", {
  dates <- as.Date(c("2020-01-01", "2021-01-01", "2022-01-01"))
  hp <- rbindlist(lapply(dates, function(d) data.table(
    personnel_id = paste0("P", 1:10), ref_date = d, employment_status = "active",
    first_employment_date = as.Date(c(rep("2010-01-01", 9), "2021-06-01"))
  )))
  hc <- hp[, .(personnel_id, ref_date, contract_id = paste0("C", personnel_id),
               start_date = as.Date("2010-01-01"), end_date = as.Date(NA),
               contract_type = "permanent")]
  # Dated to the first snapshot instead, it would fall outside every interval
  expect_equal(
    estimate_historical_hiring_rates(hc, hp, NULL, define_column_spec(hire_date_col = "first_employment_date"))$hiring_rate,
    1 / 20
  )
})

test_that("on the bundled data the two hiring estimators agree within 10%", {
  panel_end <- as.Date("2012-09-01")
  hc <- bra_hrmis_contract[ref_date <= panel_end]
  hp <- bra_hrmis_personnel[ref_date <= panel_end]
  from_hire_date <- estimate_historical_hiring_rates(
    hc, hp, NULL, define_column_spec(hire_date_col = "first_employment_date")
  )$hiring_rate
  from_appearance <- estimate_historical_hiring_rates(hc, hp, NULL)$hiring_rate
  expect_lt(abs(from_hire_date / from_appearance - 1), 0.10)
})

# -- Ages above the oldest retirement age ------------------------------------

older_than_the_table <- function() {
  d <- small_groups(n_groups = 2L, age = 70)
  d$c[, est_id := rep(c("E1", "E2"), each = 5L)]
  d
}

hazard_with_ceiling <- function(table, group_cols = NULL) {
  eligibility_status_quo(hazard_table = table, group_cols = group_cols)
}

test_that("ages above the oldest retirement age take that age's rate", {
  d <- older_than_the_table()
  policy <- hazard_with_ceiling(data.table(age_yr_int = c(60L, 65L), hazard_rate = c(0.1, 1)))
  expect_equal(nrow(compute_status_quo_retirements(d$c, d$p, policy, start)), 10L)
})

test_that("the oldest age is worked out for each group", {
  d <- older_than_the_table()
  table <- data.table(est_id = c("E1", "E2"), age_yr_int = c(60L, 69L), hazard_rate = c(1, 0))
  policy <- hazard_with_ceiling(table, "est_id")
  out <- compute_status_quo_retirements(d$c, d$p, policy, start)
  # E2's only rate is zero, so E2 has no oldest age with a rate
  expect_setequal(out$personnel_id, d$c[est_id == "E1", personnel_id])
})

test_that("the oldest age is the oldest with a rate above zero", {
  d <- older_than_the_table()
  policy <- hazard_with_ceiling(data.table(age_yr_int = c(60L, 75L), hazard_rate = c(1, 0)))
  expect_equal(nrow(compute_status_quo_retirements(d$c, d$p, policy, start)), 10L)
})

test_that("ages below or between the ages in the table are not filled in", {
  d <- small_groups(n_groups = 1L, age = 50)
  policy <- hazard_with_ceiling(data.table(age_yr_int = 60L, hazard_rate = 1))
  expect_equal(nrow(compute_status_quo_retirements(d$c, d$p, policy, start)), 0L)
  d <- small_groups(n_groups = 1L, age = 62)
  policy <- hazard_with_ceiling(data.table(age_yr_int = c(60L, 65L), hazard_rate = c(1, 1)))
  expect_equal(nrow(compute_status_quo_retirements(d$c, d$p, policy, start)), 0L)
})

test_that("a rate you set for unmatched groups still applies above the oldest age", {
  d <- older_than_the_table()
  policy <- hazard_with_ceiling(data.table(age_yr_int = 60L, hazard_rate = 1))
  policy$defaults$hazard_rate <- 0
  expect_equal(nrow(compute_status_quo_retirements(d$c, d$p, policy, start)), 0L)
})

test_that("a retirement table with no rates retires nobody", {
  d <- older_than_the_table()
  policy <- hazard_with_ceiling(data.table(age_yr_int = integer(0), hazard_rate = numeric(0)))
  expect_equal(nrow(compute_status_quo_retirements(d$c, d$p, policy, start)), 0L)
})
