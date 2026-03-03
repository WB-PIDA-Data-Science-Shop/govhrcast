# tests/testthat/test-horizon_class.R
# =============================================================================
# Tests for:
#   new_horizon()
#   validate_horizon()
#   is.horizon()
#   print.horizon()
#   summary.horizon()
#   plot.horizon()  — fiscal_basics, spending_effects, turnover
# =============================================================================

library(data.table)

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

make_minimal_comparison <- function(n_periods = 5L,
                                    n_scenarios = 1L,
                                    ref_date = as.Date("2020-01-01")) {
  periods <- seq(ref_date, by = "year", length.out = n_periods)
  if (n_scenarios == 1L) {
    dt <- data.table::data.table(
      period_date              = periods,
      n_headcount_start        = 100L,
      wage_bill_start          = 1e6,
      n_exits                  = 5L,
      exit_savings             = 50000,
      pension_cost_new         = 5000,
      pension_cost_total       = cumsum(rep(5000, n_periods)),
      n_promotions             = 3L,
      n_transfers              = 2L,
      promotion_effect         = 3000,
      transfer_effect          = -500,
      n_hires                  = 5L,
      hiring_effect            = 50000,
      inflation_effect         = 30000,
      n_headcount_end          = 100L,
      wage_bill_end            = 1e6 + 30000,
      exit_savings_pct_of_end_bill      = 0.048,
      promotion_effect_pct_of_end_bill  = 0.003,
      transfer_effect_pct_of_end_bill   = -0.0005,
      hiring_effect_pct_of_end_bill     = 0.048,
      inflation_effect_pct_of_end_bill  = 0.029
    )
  } else {
    rows <- lapply(seq_len(n_scenarios), function(i) {
      dt_i <- data.table::data.table(
        scenario                 = paste0("Scenario_", i),
        period_date              = periods,
        n_headcount_start        = 100L,
        wage_bill_start          = 1e6 * i,
        n_exits                  = 5L,
        exit_savings             = 50000 * i,
        pension_cost_new         = 5000,
        pension_cost_total       = cumsum(rep(5000, n_periods)),
        n_promotions             = 3L,
        n_transfers              = 2L,
        promotion_effect         = 3000 * i,
        transfer_effect          = -500,
        n_hires                  = 5L,
        hiring_effect            = 50000 * i,
        inflation_effect         = 30000 * i,
        n_headcount_end          = 100L,
        wage_bill_end            = (1e6 + 30000) * i,
        exit_savings_pct_of_end_bill      = 0.048,
        promotion_effect_pct_of_end_bill  = 0.003,
        transfer_effect_pct_of_end_bill   = -0.0005,
        hiring_effect_pct_of_end_bill     = 0.048,
        inflation_effect_pct_of_end_bill  = 0.029
      )
      dt_i
    })
    dt <- data.table::rbindlist(rows)
  }
  dt
}

make_horizon <- function(n_periods = 5L, n_scenarios = 1L) {
  new_horizon(
    comparison = make_minimal_comparison(n_periods = n_periods,
                                         n_scenarios = n_scenarios),
    metadata   = list(policy_args = list(salary_growth_rate = 0.03))
  )
}

# ---------------------------------------------------------------------------
# new_horizon() and is.horizon()
# ---------------------------------------------------------------------------

test_that("new_horizon: returns object of class 'horizon'", {
  h <- make_horizon()
  expect_s3_class(h, "horizon")
})

test_that("new_horizon: contains $comparison data.table", {
  h <- make_horizon()
  expect_true(data.table::is.data.table(h$comparison))
})

test_that("new_horizon: $summary_dt is a backward-compatible alias for $comparison", {
  h <- make_horizon()
  expect_identical(h$summary_dt, h$comparison)
})

test_that("new_horizon: contains $metadata list", {
  h <- make_horizon()
  expect_type(h$metadata, "list")
  expect_true("policy_args" %in% names(h$metadata))
})

test_that("is.horizon: TRUE for horizon, FALSE for list/data.table", {
  h <- make_horizon()
  expect_true(is.horizon(h))
  expect_false(is.horizon(list(a = 1)))
  expect_false(is.horizon(data.table::data.table(x = 1)))
})

test_that("new_horizon: errors if comparison is not a data.table", {
  expect_error(new_horizon(comparison = data.frame(period_date = Sys.Date(),
                                                    wage_bill_end = 1)))
})

# ---------------------------------------------------------------------------
# validate_horizon()
# ---------------------------------------------------------------------------

test_that("validate_horizon: passes for valid object", {
  h <- make_horizon()
  expect_no_error(validate_horizon(h))
})

test_that("validate_horizon: errors if not a horizon", {
  expect_error(validate_horizon(list(comparison = data.table::data.table())),
               "not of class 'horizon'")
})

test_that("validate_horizon: errors if $comparison missing required columns", {
  bad_dt <- data.table::data.table(x = 1)
  bad_h  <- structure(list(comparison = bad_dt, summary_dt = bad_dt, metadata = list()),
                      class = "horizon")
  expect_error(validate_horizon(bad_h), "missing required columns")
})

# ---------------------------------------------------------------------------
# print.horizon()
# ---------------------------------------------------------------------------

test_that("print.horizon: returns x invisibly", {
  h   <- make_horizon()
  out <- capture.output(ret <- print(h))
  expect_identical(ret, h)
})

test_that("print.horizon: output contains '<horizon>'", {
  h   <- make_horizon()
  out <- capture.output(print(h))
  expect_true(any(grepl("<horizon>", out, fixed = TRUE)))
})

test_that("print.horizon: multi-scenario output lists scenario count", {
  h   <- make_horizon(n_scenarios = 3L)
  out <- capture.output(print(h))
  expect_true(any(grepl("3", out)))
})

# ---------------------------------------------------------------------------
# summary.horizon()
# ---------------------------------------------------------------------------

test_that("summary.horizon: returns data.table invisibly", {
  h   <- make_horizon()
  out <- capture.output(ret <- summary(h))
  expect_true(data.table::is.data.table(ret))
})

test_that("summary.horizon: single-scenario returns one row (final period)", {
  h   <- make_horizon(n_periods = 5L)
  out <- capture.output(ret <- summary(h))
  expect_equal(nrow(ret), 1L)
})

test_that("summary.horizon: multi-scenario returns one row per scenario", {
  h   <- make_horizon(n_scenarios = 3L, n_periods = 5L)
  out <- capture.output(ret <- summary(h))
  expect_equal(nrow(ret), 3L)
})

# ---------------------------------------------------------------------------
# simulate_horizon() integration: returns horizon
# ---------------------------------------------------------------------------

make_sim_state <- function(n = 4L, ages = rep(40L, 4L)) {
  ref <- as.Date("2020-01-01")
  ct  <- data.table::data.table(
    contract_id        = paste0("C", seq_len(n)),
    personnel_id       = paste0("P", seq_len(n)),
    est_id             = rep(c("E1", "E2"), length.out = n),
    start_date         = ref - 365L * 10L,
    end_date           = as.Date(NA),
    contract_type_code = "permanent",
    gross_salary_lcu   = 10000
  )
  pt <- data.table::data.table(
    personnel_id = paste0("P", seq_len(n)),
    birth_date   = ref - 365L * as.integer(ages),
    status       = "active",
    age          = as.numeric(ages),
    tenure_years = 10
  )
  ss <- data.table::data.table(est_id = c("E1", "E2"), gross_salary_lcu = 10000)
  list(ct = ct, pt = pt, ss = ss, ref = ref)
}

test_that("simulate_horizon: returns a horizon object", {
  s   <- make_sim_state()
  res <- simulate_horizon(
    contract_dt        = s$ct,
    personnel_dt       = s$pt,
    salary_scale_dt    = s$ss,
    n_periods          = 2L,
    retirement_policy  = list(eligibility_type = "age_only", min_age = 999,
                               pension_type = "flat",
                               pension_params = list(flat_amount = 500)),
    salary_growth_rate = 0,
    ref_date           = s$ref,
    age_col            = "age",
    tenure_col         = "tenure_years"
  )
  expect_s3_class(res, "horizon")
})

test_that("simulate_horizon: $summary_dt alias still works for backward compat", {
  s   <- make_sim_state()
  res <- simulate_horizon(
    contract_dt        = s$ct,
    personnel_dt       = s$pt,
    salary_scale_dt    = s$ss,
    n_periods          = 2L,
    retirement_policy  = list(eligibility_type = "age_only", min_age = 999,
                               pension_type = "flat",
                               pension_params = list(flat_amount = 500)),
    salary_growth_rate = 0,
    ref_date           = s$ref,
    age_col            = "age",
    tenure_col         = "tenure_years"
  )
  expect_true(data.table::is.data.table(res$summary_dt))
  expect_equal(nrow(res$summary_dt), 2L)
})

test_that("simulate_horizon: metadata contains policy_args", {
  s   <- make_sim_state()
  ret_pol <- list(eligibility_type = "age_only", min_age = 999,
                  pension_type = "flat", pension_params = list(flat_amount = 500))
  res <- simulate_horizon(
    contract_dt        = s$ct,
    personnel_dt       = s$pt,
    salary_scale_dt    = s$ss,
    n_periods          = 2L,
    retirement_policy  = ret_pol,
    salary_growth_rate = 0.05,
    ref_date           = s$ref,
    age_col            = "age",
    tenure_col         = "tenure_years"
  )
  expect_type(res$metadata, "list")
  expect_equal(res$metadata$policy_args$salary_growth_rate, 0.05)
})

# ---------------------------------------------------------------------------
# plot.horizon() — structural tests (no rendering needed)
# ---------------------------------------------------------------------------
# We test that plot.horizon() returns an object with the right classes
# and the description attribute, without actually rendering to a device.

test_that("plot.horizon fiscal_basics: returns patchwork/gg object invisibly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  h   <- make_horizon()
  out <- plot(h, type = "fiscal_basics")
  expect_true(inherits(out, "patchwork") || inherits(out, "gg"))
})

test_that("plot.horizon spending_effects: returns patchwork/gg object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  h   <- make_horizon()
  out <- plot(h, type = "spending_effects")
  expect_true(inherits(out, "patchwork") || inherits(out, "gg"))
})

test_that("plot.horizon turnover: returns patchwork/gg object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  h   <- make_horizon()
  out <- plot(h, type = "turnover")
  expect_true(inherits(out, "patchwork") || inherits(out, "gg"))
})

test_that("plot.horizon: description attribute is a non-empty string", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  for (tp in c("fiscal_basics", "spending_effects", "turnover")) {
    h   <- make_horizon()
    out <- plot(h, type = tp)
    desc <- attr(out, "description")
    expect_type(desc, "character")
    expect_gt(nchar(desc), 20L)
  }
})

test_that("plot.horizon: plot is returned invisibly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  h  <- make_horizon()
  # withVisible() captures visibility
  vis <- withVisible(plot(h, type = "fiscal_basics"))
  expect_false(vis$visible)
})

test_that("plot.horizon: errors with unknown type", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  h <- make_horizon()
  expect_error(plot(h, type = "bogus_type"))
})

test_that("plot.horizon: multi-scenario comparison renders without error", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  h <- make_horizon(n_scenarios = 3L)
  expect_no_error(plot(h, type = "fiscal_basics"))
  expect_no_error(plot(h, type = "spending_effects"))
  expect_no_error(plot(h, type = "turnover"))
})

# ---------------------------------------------------------------------------
# NA handling
# ---------------------------------------------------------------------------

test_that("plot.horizon fiscal_basics: handles NAs in wage_bill_end gracefully", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  dt <- make_minimal_comparison(n_periods = 5L)
  dt[3L, wage_bill_end := NA_real_]   # inject NA in middle period
  h  <- new_horizon(comparison = dt)
  expect_no_error(plot(h, type = "fiscal_basics"))
})

test_that("plot.horizon spending_effects: handles all-NA effect column gracefully", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  dt <- make_minimal_comparison(n_periods = 5L)
  dt[, promotion_effect := NA_real_]   # entire column NA
  h  <- new_horizon(comparison = dt)
  expect_no_error(plot(h, type = "spending_effects"))
})

test_that("plot.horizon turnover: handles NAs in n_hires and n_exits", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  dt <- make_minimal_comparison(n_periods = 5L)
  dt[c(1L, 4L), n_hires := NA_integer_]
  dt[c(2L, 5L), n_exits := NA_integer_]
  h  <- new_horizon(comparison = dt)
  expect_no_error(plot(h, type = "turnover"))
})

test_that("plot.horizon fiscal_basics: missing pension columns uses fallback gracefully", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("scales")
  dt <- make_minimal_comparison(n_periods = 5L)
  dt[, pension_cost_total := NULL]
  h  <- new_horizon(comparison = dt)
  expect_no_error(plot(h, type = "fiscal_basics"))
})
