# tests/testthat/test-generate_hrcastapp.R
# Unit tests for app_shinyutils.R helper functions.
# All functions tested here are pure / side-effect-free and do NOT require
# a running Shiny session.
#
# Shiny testing strategy note:
# ----------------------------
# - Pure helpers (hz_fmt_big, hz_lever_cols, etc.)  → tested here with testthat
# - Server reactive logic (compare_dt, sid resolution) → shiny::testServer()
#   (no browser required, ships with Shiny)
# - Full UI/browser flows → shinytest2 (optional, requires Chrome, use sparingly)
#   A shinytest2 suite would live in tests/testthat/test-shinytest2.R and is
#   NOT included here to keep CI fast and dependency-free.

# =============================================================================
# Test fixture
# =============================================================================

make_scenario_dt <- function() {
  data.table::data.table(
    scenario_id        = c(1L, 1L, 2L, 2L),
    scenario_label     = c("baseline", "baseline", "alt", "alt"),
    is_baseline        = c(TRUE, TRUE, FALSE, FALSE),
    period_date        = as.Date(c(
      "2015-09-01", "2016-09-01",
      "2015-09-01", "2016-09-01"
    )),
    n_headcount_start  = c(100L, 95L,  100L, 100L),
    n_headcount_end    = c(95L,  90L,  100L, 110L),
    wage_bill_start    = c(1e6,  1.05e6, 1e6, 1.1e6),
    wage_bill_end      = c(1.05e6, 1.1e6, 1.1e6, 1.2e6),
    n_exits            = c(5L,  5L,  0L, 0L),
    exit_savings       = c(5e4, 5e4, 0,  0),
    pension_cost_new   = c(2e3, 2e3, 0,  0),
    pension_cost_total = c(5e4, 6e4, 5e4, 7e4),
    n_promotions       = c(2L,  2L,  0L,  0L),
    n_transfers        = c(1L,  1L,  0L,  0L),
    promotion_effect   = c(3e3, 3e3, 0,   0),
    transfer_effect    = c(1e3, 1e3, 0,   0),
    n_hires            = c(0L,  0L,  5L,  5L),
    hiring_effect      = c(0,   0,   8e3, 8e3),
    inflation_effect   = c(1e4, 1e4, 1e4, 1.1e4),
    exit_savings_pct_of_end_bill          = c(0.05, 0.05, 0, 0),
    promotion_effect_pct_of_end_bill      = c(0.003, 0.003, 0, 0),
    transfer_effect_pct_of_end_bill       = c(0.001, 0.001, 0, 0),
    hiring_effect_pct_of_end_bill         = c(0, 0, 0.007, 0.007),
    inflation_effect_pct_of_end_bill      = c(0.010, 0.010, 0.009, 0.009),
    salary_growth_rate = c(0.03, 0.03, 0.05, 0.05)   # lever column
  )
}

# =============================================================================
# hz_fmt_big
# =============================================================================

test_that("hz_fmt_big returns N/A for NA input", {
  expect_equal(hz_fmt_big(NA_real_), "N/A")
})

test_that("hz_fmt_big returns N/A for NULL input", {
  expect_equal(hz_fmt_big(NULL), "N/A")
})

test_that("hz_fmt_big formats millions correctly", {
  result <- hz_fmt_big(1e6)
  expect_match(result, "M")  # accepts "1M", "1.0M", etc.
})

test_that("hz_fmt_big formats thousands correctly", {
  result <- hz_fmt_big(1500)
  expect_match(result, "1\\.5K|1,500", perl = TRUE)
})

test_that("hz_fmt_big handles zero", {
  result <- hz_fmt_big(0)
  expect_false(is.na(result))
  expect_false(result == "N/A")
})

# =============================================================================
# hz_lever_cols
# =============================================================================

test_that("hz_lever_cols returns only non-reserved columns", {
  dt     <- make_scenario_dt()
  levers <- hz_lever_cols(dt)
  expect_equal(levers, "salary_growth_rate")
})

test_that("hz_lever_cols returns character(0) when no lever columns", {
  dt <- make_scenario_dt()[, salary_growth_rate := NULL]
  expect_equal(hz_lever_cols(dt), character(0))
})

# =============================================================================
# hz_terminal_row
# =============================================================================

test_that("hz_terminal_row returns last-period row for known scenario", {
  dt  <- make_scenario_dt()
  row <- hz_terminal_row(dt, 1L)
  expect_equal(nrow(row), 1L)
  expect_equal(row$period_date, as.Date("2016-09-01"))
  expect_equal(row$scenario_id, 1L)
})

test_that("hz_terminal_row returns zero-row table for unknown scenario", {
  dt  <- make_scenario_dt()
  row <- hz_terminal_row(dt, 999L)
  expect_equal(nrow(row), 0L)
})

# =============================================================================
# hz_scenario_choices
# =============================================================================

test_that("hz_scenario_choices returns a named list with correct length", {
  dt  <- make_scenario_dt()
  ch  <- hz_scenario_choices(dt)
  expect_type(ch, "list")
  expect_equal(length(ch), 2L)
})

test_that("hz_scenario_choices has labels as names and ids as values", {
  dt  <- make_scenario_dt()
  ch  <- hz_scenario_choices(dt)
  expect_setequal(names(ch), c("baseline", "alt"))
  expect_setequal(unlist(ch), c(1L, 2L))
})

# =============================================================================
# hz_ensure_scenario_cols
# =============================================================================

test_that("hz_ensure_scenario_cols adds missing columns", {
  dt <- make_scenario_dt()[, c("scenario_id", "scenario_label",
                               "is_baseline") := NULL]
  expect_false("scenario_id" %in% names(dt))
  hz_ensure_scenario_cols(dt)
  expect_true("scenario_id"    %in% names(dt))
  expect_true("scenario_label" %in% names(dt))
  expect_true("is_baseline"    %in% names(dt))
})

test_that("hz_ensure_scenario_cols does not overwrite existing columns", {
  dt <- make_scenario_dt()
  original_ids <- copy(dt$scenario_id)
  hz_ensure_scenario_cols(dt)
  expect_equal(dt$scenario_id, original_ids)
})

# =============================================================================
# hz_dt_to_horizon
# =============================================================================

test_that("hz_dt_to_horizon returns a horizon object", {
  dt  <- make_scenario_dt()
  out <- hz_dt_to_horizon(dt)
  expect_s3_class(out, "horizon")
})

test_that("hz_dt_to_horizon$comparison equals the input data.table", {
  dt  <- make_scenario_dt()
  out <- hz_dt_to_horizon(dt)
  expect_true(data.table::is.data.table(out$comparison))
  expect_equal(nrow(out$comparison), nrow(dt))
  expect_equal(names(out$comparison), names(dt))
})

test_that("hz_dt_to_horizon errors on non-data.table input", {
  expect_error(hz_dt_to_horizon(data.frame(a = 1)))
})

# =============================================================================
# hz_delta_card_html
# =============================================================================

test_that("hz_delta_card_html returns a shiny tag list", {
  dt  <- make_scenario_dt()
  out <- hz_delta_card_html(1e6, 1.2e6, "Scenario A", "Scenario B")
  expect_true(inherits(out, c("shiny.tag.list", "shiny.tag", "list")))
})

test_that("hz_delta_card_html positive delta contains up arrow", {
  out  <- hz_delta_card_html(1e6, 1.2e6, "A", "B")
  html <- paste(as.character(out), collapse = "")
  expect_match(html, "\u25b2")  # ▲
})

test_that("hz_delta_card_html negative delta contains down arrow", {
  out  <- hz_delta_card_html(1.2e6, 1e6, "A", "B")
  html <- paste(as.character(out), collapse = "")
  expect_match(html, "\u25bc")  # ▼
})

test_that("hz_delta_card_html handles NA val_a gracefully", {
  out  <- hz_delta_card_html(NA_real_, 1e6, "A", "B")
  html <- paste(as.character(out), collapse = "")
  expect_match(html, "N/A")
})

test_that("hz_delta_card_html handles NA val_b gracefully", {
  out  <- hz_delta_card_html(1e6, NA_real_, "A", "B")
  html <- paste(as.character(out), collapse = "")
  expect_match(html, "N/A")
})

# =============================================================================
# hz_app_theme
# =============================================================================

test_that("hz_app_theme returns a bslib bs_theme object", {
  th <- hz_app_theme()
  expect_true(inherits(th, "bs_theme") || inherits(th, "sass_layer"))
})

# =============================================================================
# Scenario Comparator — server reactive logic (shiny::testServer)
# =============================================================================

# Helper: build a minimal hz_server app for testServer
.make_test_app <- function(dt) {
  hz_obj  <- hz_dt_to_horizon(dt)
  levers  <- hz_lever_cols(dt)
  sch     <- hz_scenario_choices(dt)
  server  <- hz_server(dt, hz_obj, sch, levers)
  list(server = server, levers = levers, flat_dt = dt)
}

test_that("comparator: cmp_sid_a resolves to baseline scenario on load (lever-mode)", {
  dt <- make_scenario_dt()   # has salary_growth_rate as lever
  app <- .make_test_app(dt)

  shiny::testServer(app$server, {
    # Simulate clicking Compare without changing inputs — should resolve
    # to whichever sid has salary_growth_rate matching its default (baseline)
    session$setInputs(
      show_results    = 1L,
      cmp_show_results = 1L,
      cmp_a_lv_salary_growth_rate = 0.03,
      cmp_b_lv_salary_growth_rate = 0.05
    )
    sid_a <- cmp_sid_a()
    sid_b <- cmp_sid_b()
    # sid_a should be scenario 1 (salary_growth_rate = 0.03, baseline)
    expect_equal(sid_a, 1L)
    # sid_b should be scenario 2 (salary_growth_rate = 0.05)
    expect_equal(sid_b, 2L)
  })
})

test_that("comparator: compare_dt contains rows for both scenario ids", {
  dt  <- make_scenario_dt()
  app <- .make_test_app(dt)

  shiny::testServer(app$server, {
    session$setInputs(
      show_results     = 1L,
      cmp_show_results = 1L,
      cmp_a_lv_salary_growth_rate = 0.03,
      cmp_b_lv_salary_growth_rate = 0.05
    )
    cdt <- compare_dt()
    expect_true(data.table::is.data.table(cdt))
    expect_true(nrow(cdt) > 0L)
    # Both scenario ids present
    expect_true(1L %in% cdt$scenario_id)
    expect_true(2L %in% cdt$scenario_id)
  })
})

test_that("comparator: compare_dt has scenario column with 'Scenario A' and 'Scenario B'", {
  dt  <- make_scenario_dt()
  app <- .make_test_app(dt)

  shiny::testServer(app$server, {
    session$setInputs(
      show_results     = 1L,
      cmp_show_results = 1L,
      cmp_a_lv_salary_growth_rate = 0.03,
      cmp_b_lv_salary_growth_rate = 0.05
    )
    cdt <- compare_dt()
    expect_true("scenario" %in% names(cdt))
    expect_setequal(unique(cdt$scenario), c("Scenario A", "Scenario B"))
  })
})

test_that("comparator: diff_wage_bill renders without error", {
  dt  <- make_scenario_dt()
  app <- .make_test_app(dt)

  shiny::testServer(app$server, {
    session$setInputs(
      show_results     = 1L,
      cmp_show_results = 1L,
      cmp_a_lv_salary_growth_rate = 0.03,
      cmp_b_lv_salary_growth_rate = 0.05
    )
    html_tag <- output$diff_wage_bill
    # Should produce a shiny tag, not NULL / error
    expect_false(is.null(html_tag))
  })
})

test_that("comparator: cmp_sid_a == cmp_sid_b when same lever values chosen", {
  dt  <- make_scenario_dt()
  app <- .make_test_app(dt)

  shiny::testServer(app$server, {
    session$setInputs(
      show_results     = 1L,
      cmp_show_results = 1L,
      cmp_a_lv_salary_growth_rate = 0.03,
      cmp_b_lv_salary_growth_rate = 0.03   # same as A
    )
    expect_equal(cmp_sid_a(), cmp_sid_b())
  })
})
