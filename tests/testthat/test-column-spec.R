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

run_with <- function(column_spec_args = list(), output_control_args = list()) {
  d <- spec_fixture()
  suppressMessages(simulate_horizon(
    d$hc, d$hp, d$scale, n_periods = 1L, ref_date = d$start,
    retirement_policy = NULL,
    column_spec_args = column_spec_args, output_control_args = output_control_args))
}

test_that("an unknown column_spec_args key is an error that lists the valid keys", {
  expect_error(run_with(column_spec_args = list(salary_column = "base_salary_lcu")),
               "unknown key.*salary_column.*Valid keys.*salary_col")
})

test_that("an unknown output_control_args key is an error that lists the valid keys", {
  expect_error(run_with(output_control_args = list(return_micro = TRUE)),
               "unknown key.*return_micro.*Valid keys.*return_microdata")
})

test_that("a column name must be a single string", {
  expect_error(run_with(column_spec_args = list(salary_col = 5)),
               "salary_col must be a single column name")
})

test_that("a mapped column missing from the data is named with its argument and table", {
  expect_error(run_with(column_spec_args = list(contract_type_col = "ctype")),
               "contract_type_col = 'ctype'.*not found in contract_dt")
})

test_that("two arguments cannot name the same column", {
  expect_error(run_with(column_spec_args = list(start_date_col = "start_date",
                                                end_date_col   = "start_date")),
               "start_date_col and end_date_col both name column 'start_date'")
})

test_that("output switches must be a single TRUE or FALSE", {
  expect_error(run_with(output_control_args = list(is_baseline = "yes")),
               "is_baseline must be TRUE or FALSE")
})
