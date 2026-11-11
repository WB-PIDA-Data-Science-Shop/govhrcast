# Column-name and output-option arguments are validated before any work starts
#
# simulate_horizon() takes column_spec_args (which column holds what) and
# output_control_args (labelling and output size). A typo, a wrong type, a
# missing column or two names for one column must stop the run with a message
# that names the argument -- never be ignored or surface later as an unrelated
# error.

library(data.table)

spec_fixture <- function() {
  start <- as.Date("2012-09-01")
  hc <- bra_hrmis_contract[ref_date <= start]
  hp <- bra_hrmis_personnel[ref_date <= start]
  scale <- hc[ref_date == start,
              .(gross_salary_lcu = mean(gross_salary_lcu, na.rm = TRUE)), by = est_id]
  list(hc = hc, hp = hp, scale = scale, start = start)
}

run_with <- function(column_spec_args = define_column_spec(), output_control_args = define_output_control()) {
  d <- spec_fixture()
  suppressMessages(simulate_horizon(
    d$hc, d$hp, d$scale, n_periods = 1L, baseline_date = d$start,
    retirement_policy = NULL,
    column_spec_args = column_spec_args, output_control_args = output_control_args))
}

test_that("define_column_spec() with no arguments gives the govhr names", {
  expected <- list(
    personnel_id_col  = "personnel_id",
    contract_id_col   = "contract_id",
    ref_date_col      = "ref_date",
    start_date_col    = "start_date",
    end_date_col      = "end_date",
    salary_col        = "gross_salary_lcu",
    contract_type_col = "contract_type",
    status_col        = "employment_status",
    age_col           = "age",
    tenure_col        = "personnel_tenure",
    birth_date_col    = "birth_date",
    hire_date_col     = NULL,
    gender_col        = "gender"
  )
  spec <- define_column_spec()
  expect_length(spec, length(expected))
  expect_identical(spec[names(expected)], expected)
})

test_that("define_column_spec() changes only the columns it is given", {
  spec <- define_column_spec(personnel_id_col = "personnel_id2",
                             contract_id_col  = "contract_id2")
  expect_identical(spec$personnel_id_col, "personnel_id2")
  expect_identical(spec$contract_id_col, "contract_id2")
  expect_identical(spec$salary_col, "gross_salary_lcu")
  expect_identical(spec$status_col, "employment_status")
})

test_that("an optional column set to NULL stays in the spec, empty", {
  spec <- define_column_spec(age_col = NULL)
  expect_length(spec, 13L)
  expect_true("age_col" %in% names(spec))
  expect_null(spec$age_col)
})

test_that("a mistyped column argument stops rather than being ignored", {
  expect_error(define_column_spec(salary_column = "base_salary_lcu"), "salary_column")
})

test_that("each column must be named by a single non-empty string", {
  expect_error(define_column_spec(salary_col = 5), "salary_col must be a single column name")
  expect_error(define_column_spec(salary_col = NA_character_), "salary_col must be a single column name")
  expect_error(define_column_spec(salary_col = ""), "salary_col must be a single column name")
  expect_error(define_column_spec(salary_col = c("a", "b")), "salary_col must be a single column name")
})

test_that("a column the model cannot run without cannot be NULL", {
  expect_error(define_column_spec(salary_col = NULL), "salary_col must be a single column name")
})

test_that("two arguments cannot name the same column", {
  expect_error(define_column_spec(start_date_col = "start_date", end_date_col = "start_date"),
               "start_date_col and end_date_col both name column 'start_date'")
})

# One call per exported function that takes column_spec_args, each with arguments
# that get as far as reading the columns. `spec` is what the test passes in.
entry_points <- function(spec) {
  d <- spec_fixture()
  allowance <- bra_hrmis_allowance[ref_date <= d$start]
  flat_exits <- list(group_cols = NULL, policy_table = NULL, defaults = list(exit_rate = 0.1))
  list(
    simulate_horizon = function() simulate_horizon(
      d$hc, d$hp, d$scale, n_periods = 1L, baseline_date = d$start,
      retirement_policy = NULL, column_spec_args = spec),
    simulate_scenario = function() simulate_scenario(
      d$hc, d$hp, d$scale, period_date = d$start, column_spec_args = spec),
    simulate_retirement = function() simulate_retirement(
      d$hc, d$hp, as_of_date = d$start, column_spec_args = spec),
    simulate_exits = function() simulate_exits(
      d$hc, d$hp, policy_params = flat_exits, as_of_date = d$start, column_spec_args = spec),
    simulate_promotions_transfers = function() simulate_promotions_transfers(
      d$hc, d$hp, d$scale,
      policy_params = list(group_cols = "est_id", defaults = list(movement_rate = 0.1)),
      as_of_date = d$start, column_spec_args = spec),
    simulate_hiring = function() simulate_hiring(
      d$hc, d$hp, policy_params = list(mode = "flow", replacement_rate = 0,
                           defaults = list(eligibility_type = "age_only", min_age = 60)),
      as_of_date = d$start, column_spec_args = spec),
    simulate_allowances = function() simulate_allowances(
      d$hc, d$hp, policy_params = list(defaults = list(amount = 0)),
      as_of_date = d$start, allowance_types = "food", column_spec_args = spec),
    estimate_historical_exit_rates = function() estimate_historical_exit_rates(
      d$hc, d$hp, column_spec_args = spec),
    estimate_historical_hiring_rates = function() estimate_historical_hiring_rates(
      d$hc, d$hp, group_cols = NULL, column_spec_args = spec),
    estimate_new_hire_profile = function() estimate_new_hire_profile(
      d$hc, d$hp, column_spec_args = spec),
    estimate_retirement_hazard_rates = function() estimate_retirement_hazard_rates(
      d$hp, group_cols = "age_yr_int", column_spec_args = spec),
    estimate_baseline_allowance_scale = function() estimate_baseline_allowance_scale(
      allowance, d$hc, group_cols = "est_id", baseline_date = d$start, column_spec_args = spec),
    design_allowance_policy_status_quo = function() design_allowance_policy_status_quo(
      allowance, d$hc, group_cols = "est_id", baseline_date = d$start, column_spec_args = spec),
    movement_policy_template = function() movement_policy_template(
      d$hc, group_cols = "est_id", column_spec_args = spec),
    make_status_quo_policies = function() suppressWarnings(make_status_quo_policies(
      d$hc, d$hp, d$scale, column_spec_args = spec))
  )
}

test_that("the list of entry points covers every exported function that takes a spec", {
  ns <- asNamespace("govhrcast")
  takes_spec <- Filter(
    function(name) is.function(ns[[name]]) && "column_spec_args" %in% names(formals(ns[[name]])),
    getNamespaceExports("govhrcast")
  )
  expect_setequal(names(entry_points(list())), takes_spec)
})

test_that("every entry point refuses a plain list and points to define_column_spec()", {
  for (run in entry_points(list(salary_col = "gross_salary_lcu"))) {
    expect_error(suppressMessages(run()), "define_column_spec")
  }
})

test_that("every entry point refuses an empty spec and a NULL spec", {
  for (spec in list(list(), NULL)) {
    for (run in entry_points(spec)) {
      expect_error(suppressMessages(run()), "define_column_spec")
    }
  }
})

test_that("a spec edited by hand after define_column_spec() is checked again", {
  spec <- define_column_spec()
  spec$salary_col <- 5
  for (run in entry_points(spec)) {
    expect_error(suppressMessages(run()), "salary_col must be a single column name")
  }
})

test_that("define_output_control() with no arguments labels nothing and returns no microdata", {
  expected <- list(scenario_name = NULL, is_baseline = FALSE, return_microdata = FALSE)
  out <- define_output_control()
  expect_length(out, length(expected))
  expect_identical(out[names(expected)], expected)
})

test_that("define_output_control() changes only the options it is given", {
  out <- define_output_control(return_microdata = TRUE)
  expect_true(out$return_microdata)
  expect_false(out$is_baseline)
  expect_null(out$scenario_name)
  expect_length(out, 3L)
})

test_that("a mistyped output option stops rather than being ignored", {
  expect_error(define_output_control(microdata = TRUE), "microdata")
})

test_that("a scenario name must be one string or NULL", {
  message <- "scenario_name must be a single character string or NULL"
  expect_error(define_output_control(scenario_name = 42L), message)
  expect_error(define_output_control(scenario_name = NA_character_), message)
  expect_error(define_output_control(scenario_name = c("a", "b")), message)
})

test_that("each output switch must be a single TRUE or FALSE", {
  expect_error(define_output_control(is_baseline = "yes"), "is_baseline must be TRUE or FALSE")
  expect_error(define_output_control(is_baseline = NA), "is_baseline must be TRUE or FALSE")
  expect_error(define_output_control(return_microdata = 1), "return_microdata must be TRUE or FALSE")
  expect_error(define_output_control(return_microdata = c(TRUE, FALSE)),
               "return_microdata must be TRUE or FALSE")
})

test_that("simulate_horizon() refuses output options that did not come from define_output_control()", {
  expect_error(run_with(output_control_args = list(return_microdata = TRUE)), "define_output_control")
  expect_error(run_with(output_control_args = list()), "define_output_control")
  expect_error(run_with(output_control_args = NULL), "define_output_control")
})

test_that("output options edited by hand after define_output_control() are checked again", {
  out <- define_output_control()
  out$is_baseline <- "yes"
  expect_error(run_with(output_control_args = out), "is_baseline must be TRUE or FALSE")
})

test_that("define_output_control() decides what simulate_horizon() returns", {
  with_micro <- run_with(output_control_args = define_output_control(return_microdata = TRUE))
  without    <- run_with(output_control_args = define_output_control())
  expect_true(all(c("contract_dt", "personnel_dt") %in% names(with_micro)))
  expect_false(any(c("contract_dt", "personnel_dt") %in% names(without)))

  labelled <- run_with(output_control_args = define_output_control(scenario_name = "Reform"))
  expect_identical(unique(labelled$comparison$scenario_label), "Reform")
  expect_false("scenario_label" %in% names(without$comparison))
})

test_that("a mapped column missing from the data is named with its argument and table", {
  expect_error(run_with(column_spec_args = define_column_spec(contract_type_col = "ctype")),
               "contract_type_col = 'ctype'.*not found in contract_dt")
})
