# Unit tests for estimate_retirement_hazard_rates() (R/retirement_core.R)

library(testthat)
library(data.table)

# =============================================================================
# Test data setup helper
# =============================================================================

# Two-snapshot panel, 6 personnel, hand-traceable transitions:
#   age 40 (t0): p4                        -> stays active            (0/1 exit)
#   age 59 (t0): p1                        -> stays active            (0/1 exit)
#   age 60 (t0): p2 (north), p6 (south)    -> p2 retires, p6 stays    (1/1 north, 0/1 south)
#   age 61 (t0): p3 (north), p5 (south)    -> p3 stays, p5 retires    (0/1 north, 1/1 south)
create_hazard_test_panel <- function() {
  t0 <- data.table(
    personnel_id      = paste0("p", 1:6),
    ref_date          = as.Date("2020-01-01"),
    age               = c(59, 60, 61, 40, 61, 60),
    region            = c("north", "north", "north", "south", "south", "south"),
    employment_status = "active"
  )
  t1 <- data.table(
    personnel_id      = paste0("p", 1:6),
    ref_date          = as.Date("2021-01-01"),
    age               = c(60, 61, 62, 41, 62, 61),
    region            = c("north", "north", "north", "south", "south", "south"),
    employment_status = c(
      "active", "pensioner", "active", "active", "pensioner", "active"
    )
  )
  rbindlist(list(t0, t1))
}

# =============================================================================
# estimate_retirement_hazard_rates()
# =============================================================================

test_that("estimate_retirement_hazard_rates requires age_yr_int in group_cols", {
  panel <- create_hazard_test_panel()

  expect_error(
    estimate_retirement_hazard_rates(panel, group_cols = NULL),
    regexp = "age_yr_int"
  )
  expect_error(
    estimate_retirement_hazard_rates(panel, group_cols = "region"),
    regexp = "age_yr_int"
  )
})

test_that("estimate_retirement_hazard_rates matches a direct govhr::estimate_decrement_rates() call, age_yr_int only", {
  panel <- create_hazard_test_panel()

  result <- estimate_retirement_hazard_rates(
    panel_personnel_dt = panel,
    group_cols          = "age_yr_int"
  )

  # Independently reproduce the adapter's expected output by calling
  # govhr::estimate_decrement_rates() directly on the same fixture, rather
  # than hardcoding expected numbers -- catches drift if govhr's pooling
  # logic ever changes.
  panel_check <- copy(panel)
  panel_check[, age_yr_int := as.integer(age)]
  expected <- govhr::estimate_decrement_rates(
    personnel         = panel_check,
    age_col           = "age_yr_int",
    status_col        = "employment_status",
    personnel_id_col  = "personnel_id",
    ref_date_col      = "ref_date",
    group_cols        = character(0)
  )
  expected <- expected[employment_status == "pensioner"]
  setnames(expected, "decrement_rate", "hazard_rate")
  expected <- expected[, .(age_yr_int, hazard_rate)]

  setkey(result, age_yr_int)
  setkey(expected, age_yr_int)
  expect_equal(result, expected)

  # Hand-verified against the fixture's known transitions (see helper comment).
  expect_equal(result[age_yr_int == 40]$hazard_rate, 0)
  expect_equal(result[age_yr_int == 59]$hazard_rate, 0)
  expect_equal(result[age_yr_int == 60]$hazard_rate, 0.5)
  expect_equal(result[age_yr_int == 61]$hazard_rate, 0.5)
})

test_that("estimate_retirement_hazard_rates matches a direct govhr::estimate_decrement_rates() call, grouped by region", {
  panel <- create_hazard_test_panel()

  result <- estimate_retirement_hazard_rates(
    panel_personnel_dt = panel,
    group_cols          = c("age_yr_int", "region")
  )

  panel_check <- copy(panel)
  panel_check[, age_yr_int := as.integer(age)]
  expected <- govhr::estimate_decrement_rates(
    personnel         = panel_check,
    age_col           = "age_yr_int",
    status_col        = "employment_status",
    personnel_id_col  = "personnel_id",
    ref_date_col      = "ref_date",
    group_cols        = "region"
  )
  expected <- expected[employment_status == "pensioner"]
  setnames(expected, "decrement_rate", "hazard_rate")
  expected <- expected[, .(age_yr_int, region, hazard_rate)]

  setkey(result, age_yr_int, region)
  setkey(expected, age_yr_int, region)
  expect_equal(result, expected)

  # Hand-verified: region distinguishes who exits at ages 60/61.
  expect_equal(result[age_yr_int == 60 & region == "north"]$hazard_rate, 1)
  expect_equal(result[age_yr_int == 60 & region == "south"]$hazard_rate, 0)
  expect_equal(result[age_yr_int == 61 & region == "north"]$hazard_rate, 0)
  expect_equal(result[age_yr_int == 61 & region == "south"]$hazard_rate, 1)
})

test_that("estimate_retirement_hazard_rates smooth = TRUE matches a direct govhr::smooth_decrement_rates() call", {
  panel <- create_hazard_test_panel()

  # This fixture only has 4 distinct ages -- deliberately thin so the raw
  # rate is hand-traceable (see helper comment) -- which is exactly the
  # "thin data" case loess() warns about (span too small, near-singular
  # fit, etc.). Those are expected numerical warnings from govhr's own
  # loess() call on a tiny synthetic fixture, not a defect here, so both
  # the adapter call and the direct comparison call are wrapped.
  result <- suppressWarnings(estimate_retirement_hazard_rates(
    panel_personnel_dt = panel,
    group_cols          = "age_yr_int",
    smooth               = TRUE
  ))

  panel_check <- copy(panel)
  panel_check[, age_yr_int := as.integer(age)]
  raw <- govhr::estimate_decrement_rates(
    personnel         = panel_check,
    age_col           = "age_yr_int",
    status_col        = "employment_status",
    personnel_id_col  = "personnel_id",
    ref_date_col      = "ref_date",
    group_cols        = character(0)
  )
  smoothed <- suppressWarnings(govhr::smooth_decrement_rates(
    decrements = raw,
    age_col    = "age_yr_int",
    status_col = "employment_status",
    group_cols = character(0)
  ))
  expected <- smoothed[employment_status == "pensioner"]
  setnames(expected, "decrement_rate", "hazard_rate")
  expected <- expected[, .(age_yr_int, hazard_rate)]

  setkey(result, age_yr_int)
  setkey(expected, age_yr_int)
  expect_equal(result, expected)

  # Smoothing should differ from the raw (noisy, thin-data) rate.
  raw_result <- estimate_retirement_hazard_rates(
    panel_personnel_dt = panel, group_cols = "age_yr_int", smooth = FALSE
  )
  expect_false(isTRUE(all.equal(result$hazard_rate, raw_result$hazard_rate)))
})

test_that("estimate_retirement_hazard_rates errors on a single-snapshot panel", {
  panel <- create_hazard_test_panel()[ref_date == min(ref_date)]

  expect_error(
    estimate_retirement_hazard_rates(panel, group_cols = "age_yr_int")
  )
})

test_that("estimate_retirement_hazard_rates respects custom status_col/retire_status", {
  panel <- create_hazard_test_panel()
  setnames(panel, "employment_status", "status")
  panel[, status := ifelse(status == "pensioner", "retired", status)]

  result <- estimate_retirement_hazard_rates(
    panel_personnel_dt = panel,
    group_cols          = "age_yr_int",
    status_col          = "status",
    retire_status        = "retired"
  )

  expect_equal(result[age_yr_int == 60]$hazard_rate, 0.5)
  expect_equal(result[age_yr_int == 61]$hazard_rate, 0.5)
})
