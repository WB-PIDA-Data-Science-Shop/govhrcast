# prepare_salary_scale(): a pay table from the staff on the payroll at the
# baseline date, one value per group

library(data.table)

# Two snapshots. At 2021 grade G1 pays 100 and 300 (and one blank), G2 pays 50;
# a pension and an inactive contract sit in the same column and must be ignored.
scale_fixture <- function() {
  dt <- fread(text = "
personnel_id,contract_id,ref_date,paygrade,contract_type,gross_salary_lcu
A,A1,2020-01-01,G1,permanent,10
B,B1,2020-01-01,G2,permanent,10
A,A1,2021-01-01,G1,permanent,100
C,C1,2021-01-01,G1,temporary,300
D,D1,2021-01-01,G1,permanent,
B,B1,2021-01-01,G2,permanent,50
E,E1,2021-01-01,G1,pensioner,9000
F,F1,2021-01-01,G2,inactive,9000
")
  dt[, ref_date := as.Date(ref_date)]
  dt[]
}

test_that("one row per group, from the baseline snapshot's staff only", {
  out <- prepare_salary_scale(scale_fixture(), "2021-01-01", "paygrade", mean, na.rm = TRUE)
  expect_identical(names(out), c("paygrade", "gross_salary_lcu"))
  expect_equal(out[order(paygrade), gross_salary_lcu], c(200, 50))
})

test_that("arguments after fn are passed on to it", {
  dt <- scale_fixture()
  # Without na.rm, the blank pay makes G1's mean blank
  expect_true(is.na(prepare_salary_scale(dt, "2021-01-01", "paygrade", mean)[paygrade == "G1", gross_salary_lcu]))
  q75 <- prepare_salary_scale(dt, "2021-01-01", "paygrade", stats::quantile,
                              probs = 0.75, na.rm = TRUE, names = FALSE)
  expect_equal(q75[paygrade == "G1", gross_salary_lcu], 250)
})

test_that("fn can be a function, its name, or one of your own", {
  dt <- scale_fixture()
  by_fn   <- prepare_salary_scale(dt, "2021-01-01", "paygrade", median, na.rm = TRUE)
  by_name <- prepare_salary_scale(dt, "2021-01-01", "paygrade", "median", na.rm = TRUE)
  expect_identical(by_fn, by_name)
  own <- prepare_salary_scale(dt, "2021-01-01", "paygrade", function(x) max(x, na.rm = TRUE))
  expect_equal(own[order(paygrade), gross_salary_lcu], c(300, 50))
})

test_that("no groups gives a single overall value", {
  out <- prepare_salary_scale(scale_fixture(), "2021-01-01", NULL, mean, na.rm = TRUE)
  expect_equal(out$gross_salary_lcu, 150)
})

test_that("fn must give one value per group", {
  expect_error(prepare_salary_scale(scale_fixture(), "2021-01-01", "paygrade", range, na.rm = TRUE),
               "fn must return one value per group")
  # ... with no groups too
  expect_error(prepare_salary_scale(scale_fixture(), "2021-01-01", NULL, range, na.rm = TRUE),
               "fn must return one value per group")
})

test_that("a date with no staff, or a missing group column, stops with a clear message", {
  dt <- scale_fixture()
  expect_error(prepare_salary_scale(dt, "2019-01-01", "paygrade"), "nobody on the payroll at 2019-01-01")
  expect_error(prepare_salary_scale(dt, "2021-01-01", "grade"), "Columns not found in contract_dt: grade")
})

test_that("the pay column follows column_spec_args", {
  dt <- setnames(scale_fixture(), "gross_salary_lcu", "base_salary_lcu")
  out <- prepare_salary_scale(dt, "2021-01-01", "paygrade", mean, na.rm = TRUE,
                              column_spec_args = define_column_spec(salary_col = "base_salary_lcu"))
  expect_identical(names(out), c("paygrade", "base_salary_lcu"))
})

test_that("the caller's table is not changed", {
  dt <- scale_fixture()
  before <- copy(dt)
  prepare_salary_scale(dt, "2021-01-01", "paygrade", mean, na.rm = TRUE)
  expect_identical(dt, before)
})
