# Unit tests for R/movement_policy_helpers.R

library(testthat)
library(data.table)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------
# Two snapshots, 4 people. Only P1 moves: E1||G1 -> E1||G2 (1 of the 2 people
# in E1||G1 at T0), so the estimated rate for that pair is 0.5.
make_mph_panel <- function() {
  snap1 <- as.Date("2019-01-01")
  snap2 <- as.Date("2020-01-01")
  data.table(
    ref_date         = c(rep(snap1, 4), rep(snap2, 4)),
    personnel_id     = rep(paste0("P", 1:4), 2),
    contract_id      = rep(paste0("C", 1:4), 2),
    est_id           = rep(c("E1", "E1", "E1", "E2"), 2),
    paygrade         = c("G1", "G1", "G2", "G1",
                         "G2", "G1", "G2", "G1"),
    gross_salary_lcu = c(3000, 3000, 5000, 3000,
                         5000, 3000, 5000, 3000),
    start_date       = as.Date("2010-01-01"),
    end_date         = as.Date(NA),
    contract_type    = "permanent"
  )
}

make_mph_personnel <- function() {
  data.table(
    ref_date          = rep(as.Date(c("2019-01-01", "2020-01-01")), each = 4),
    personnel_id      = rep(paste0("P", 1:4), 2),
    employment_status = "active",
    birth_date        = as.Date("1980-01-01")
  )
}

make_mph_salary_scale <- function() {
  data.table(
    est_id           = c("E1", "E1", "E2", "E2"),
    paygrade         = c("G1", "G2", "G1", "G2"),
    gross_salary_lcu = c(3000, 5000, 3000, 5000)
  )
}

make_mph_static_panel <- function() {
  p <- make_mph_panel()[ref_date == as.Date("2019-01-01")]
  rbindlist(list(p, copy(p)[, ref_date := as.Date("2020-01-01")]))
}

.gc <- c("est_id", "paygrade")

# =============================================================================
# movement_policy_template()
# =============================================================================

test_that("movement_policy_template returns observed transitions with split columns", {
  tpl <- movement_policy_template(make_mph_panel(), group_cols = .gc)
  expect_true(is.data.table(tpl))
  expect_equal(
    names(tpl),
    c("from_est_id", "from_paygrade", "to_est_id", "to_paygrade",
      "from_group", "to_group", "n_periods", "status_quo_rate", "movement_rate")
  )
  expect_equal(nrow(tpl), 1L)
  expect_equal(tpl$from_group, "E1||G1")
  expect_equal(tpl$to_group, "E1||G2")
  expect_equal(tpl$from_paygrade, "G1")
  expect_equal(tpl$to_paygrade, "G2")
  expect_equal(tpl$status_quo_rate, 0.5)
  expect_identical(tpl$movement_rate, tpl$status_quo_rate)
})

test_that("movement_policy_template works with a single group column", {
  tpl <- movement_policy_template(make_mph_panel(), group_cols = "paygrade")
  expect_true(all(c("from_paygrade", "to_paygrade") %in% names(tpl)))
  expect_equal(tpl[from_group == "G1" & to_group == "G2", status_quo_rate], 1 / 3)
})

test_that("movement_policy_template returns an empty, correctly shaped table when nothing moved", {
  expect_message(
    tpl <- movement_policy_template(make_mph_static_panel(), group_cols = .gc),
    "no transitions observed"
  )
  expect_equal(nrow(tpl), 0L)
  expect_true(all(c("from_est_id", "to_paygrade", "movement_rate") %in% names(tpl)))
})

test_that("movement_policy_template does not re-sort or re-key the caller's table", {
  panel <- make_mph_panel()[order(-personnel_id)]
  before <- copy(panel)
  movement_policy_template(panel, group_cols = .gc)
  expect_identical(panel, before)
  expect_null(key(panel))
})

test_that("movement_policy_template reports missing columns", {
  expect_error(
    movement_policy_template(make_mph_panel()[, contract_type := NULL], group_cols = .gc),
    regexp = "contract_type"
  )
})

test_that("movement_policy_template requires a multi-snapshot panel", {
  expect_error(
    movement_policy_template(make_mph_panel()[ref_date == as.Date("2020-01-01")], group_cols = .gc),
    regexp = "At least 2 panel snapshots"
  )
})

test_that("movement_policy_template validates group_cols", {
  expect_error(movement_policy_template(make_mph_panel(), group_cols = NULL), regexp = "group_cols")
  expect_error(movement_policy_template(make_mph_panel(), group_cols = 1), regexp = "group_cols")
})

test_that("movement_policy_template rejects group_cols that clash with its own columns", {
  panel <- make_mph_panel()[, group := paygrade]
  expect_error(movement_policy_template(panel, group_cols = "group"), regexp = "clash")
})

# =============================================================================
# design_movement_policy_status_quo()
# =============================================================================

test_that("design_movement_policy_status_quo builds the three-slot list with policy_table = NULL", {
  result <- design_movement_policy_status_quo(group_cols = "est_id", movement_type = "transfer")
  expect_equal(
    result,
    list(
      group_cols   = "est_id",
      policy_table = NULL,
      defaults = list(
        movement_strategy  = "tenure",
        salary_update_rule = "scale",
        movement_type      = "transfer"
      )
    )
  )
})

test_that("design_movement_policy_status_quo validates its arguments", {
  expect_error(design_movement_policy_status_quo(group_cols = "est_id"), regexp = "movement_type")
  expect_error(design_movement_policy_status_quo(group_cols = NULL, movement_type = "transfer"),
               regexp = "group_cols")
  expect_error(design_movement_policy_status_quo("est_id", movement_type = "lateral"),
               regexp = "movement_type")
  expect_error(design_movement_policy_status_quo("est_id", "transfer", movement_strategy = "fifo"),
               regexp = "movement_strategy")
  expect_error(design_movement_policy_status_quo("est_id", "transfer", salary_update_rule = "raise"),
               regexp = "salary_update_rule")
})

# =============================================================================
# design_movement_policy_adjustment()
# =============================================================================

make_adj_table <- function() {
  data.table(from_group = c("E1||G1", "E2||G1"),
             to_group   = c("E1||G2", "E2||G2"),
             movement_rate = c(0.2, 0.1))
}

test_that("design_movement_policy_adjustment accepts an edited template", {
  tpl <- movement_policy_template(make_mph_panel(), group_cols = .gc)
  tpl[to_paygrade == "G2", movement_rate := movement_rate * 2]
  result <- design_movement_policy_adjustment(tpl, group_cols = .gc, movement_type = "promotion")
  expect_equal(result$group_cols, .gc)
  expect_equal(result$policy_table$movement_rate, 1)
  expect_equal(result$policy_table$status_quo_rate, 0.5)
  expect_equal(result$defaults,
               list(movement_strategy = "tenure", salary_update_rule = "scale",
                    movement_type = "promotion"))
})

test_that("design_movement_policy_adjustment accepts a hand-built table without split columns", {
  expect_no_error(
    design_movement_policy_adjustment(make_adj_table(), group_cols = .gc, movement_type = "promotion")
  )
})

test_that("design_movement_policy_adjustment stores a copy, not a reference to the input", {
  tbl <- make_adj_table()
  result <- design_movement_policy_adjustment(tbl, group_cols = .gc, movement_type = "promotion")
  tbl[, movement_rate := 0.9]
  expect_equal(result$policy_table$movement_rate, c(0.2, 0.1))
})

test_that("design_movement_policy_adjustment rejects malformed tables", {
  adj <- function(tbl) design_movement_policy_adjustment(tbl, group_cols = .gc, movement_type = "promotion")

  expect_error(adj(as.data.frame(make_adj_table())), regexp = "data.table")
  expect_error(adj(make_adj_table()[, movement_rate := NULL]), regexp = "movement_rate")
  expect_error(adj(make_adj_table()[0L]), regexp = "no rows")
  expect_error(adj(make_adj_table()[1L, from_group := NA_character_]), regexp = "NA")
  expect_error(adj(make_adj_table()[1L, to_group := "E1||G1"]), regexp = "from_group == to_group")
  expect_error(adj(rbindlist(list(make_adj_table(), make_adj_table()[1L]))), regexp = "duplicate")
  expect_error(adj(make_adj_table()[1L, to_group := "G2"]), regexp = "one part per")
})

test_that("design_movement_policy_adjustment validates movement_rate values", {
  adj <- function(tbl) design_movement_policy_adjustment(tbl, group_cols = .gc, movement_type = "promotion")
  expect_error(adj(make_adj_table()[1L, movement_rate := 1.5]), regexp = "between 0 and 1")
  expect_error(adj(make_adj_table()[1L, movement_rate := -0.1]), regexp = "between 0 and 1")
  expect_error(adj(make_adj_table()[1L, movement_rate := NA_real_]), regexp = "no NA")
  expect_no_error(adj(make_adj_table()[1L, movement_rate := 0]))
})

test_that("design_movement_policy_adjustment warns when a group's total outflow exceeds 1", {
  tbl <- rbindlist(list(
    make_adj_table(),
    data.table(from_group = "E1||G1", to_group = "E2||G1", movement_rate = 0.9)
  ))
  expect_warning(
    design_movement_policy_adjustment(tbl, group_cols = .gc, movement_type = "transfer"),
    regexp = "total outflow above 1"
  )
})

test_that("design_movement_policy_adjustment catches split columns that disagree with the keys", {
  tpl <- movement_policy_template(make_mph_panel(), group_cols = .gc)
  tpl[, to_paygrade := "G3"]   # edited the readable column but not to_group
  expect_error(
    design_movement_policy_adjustment(tpl, group_cols = .gc, movement_type = "promotion"),
    regexp = "disagree with to_group"
  )

  tpl2 <- movement_policy_template(make_mph_panel(), group_cols = .gc)
  tpl2[, from_est_id := NULL]
  expect_error(
    design_movement_policy_adjustment(tpl2, group_cols = .gc, movement_type = "promotion"),
    regexp = "some but not all"
  )
})

test_that("design_movement_policy_adjustment validates the shared arguments", {
  expect_error(design_movement_policy_adjustment(make_adj_table(), group_cols = .gc),
               regexp = "movement_type")
  expect_error(design_movement_policy_adjustment(make_adj_table(), group_cols = .gc,
                                                 movement_type = "promotion",
                                                 movement_strategy = "fifo"),
               regexp = "movement_strategy")
})

# =============================================================================
# End to end through simulate_horizon()
# =============================================================================

test_that("an adjustment policy drives simulate_horizon() movements", {
  # At the 2020 start snapshot only P2 is in E1||G1; a rate of 1 moves them.
  tpl <- movement_policy_template(make_mph_panel(), group_cols = .gc)
  tpl[, movement_rate := 1]
  mp <- design_movement_policy_adjustment(tpl, group_cols = .gc, movement_type = "promotion")

  res <- simulate_horizon(
    contract_dt        = make_mph_panel(),
    personnel_dt       = make_mph_personnel(),
    salary_scale_dt    = make_mph_salary_scale(),
    n_periods          = 1L,
    movement_policy    = mp,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01"),
    seed               = 1L
  )
  expect_equal(res$summary_dt$n_promotions, 1L)
  expect_equal(res$summary_dt$n_transfers, 0L)
  expect_equal(res$summary_dt$promotion_effect, 2000)
})

test_that("a status-quo policy runs through simulate_horizon() and reports under its movement_type", {
  mp <- design_movement_policy_status_quo(group_cols = .gc, movement_type = "transfer",
                                          movement_strategy = "random")
  res <- simulate_horizon(
    contract_dt        = make_mph_panel(),
    personnel_dt       = make_mph_personnel(),
    salary_scale_dt    = make_mph_salary_scale(),
    n_periods          = 2L,
    movement_policy    = mp,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01"),
    seed               = 1L
  )
  expect_equal(nrow(res$summary_dt), 2L)
  expect_equal(sum(res$summary_dt$n_promotions), 0L)
  expect_true(all(res$summary_dt$n_transfers >= 0L))
})
