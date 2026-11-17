# estimate_replacement_rates(): each retiree's first pension against their last salary

library(data.table)

# Four yearly snapshots, six people (pension rows have a blank contract type and
# establishment, as in the bundled data):
#   A  active 2020-2021 on two contracts (E1 from 2000, E2 from 2010), pensioner 2022
#   B  active 2020, missing 2021-2022, pensioner 2023: three snapshots apart
#   C  a pensioner throughout: no salary to compare
#   D  active 2020, pensioner 2021 with a blank pension
#   E  active 2020, pensioner 2021, back at work 2022: the later pay is not used
#   F  active throughout: not a retiree
replacement_fixture <- function() {
  contracts <- fread(text = "
personnel_id,ref_date,contract_id,start_date,contract_type,gross_salary_lcu,est_id
A,2020-01-01,A1,2000-01-01,permanent,100,E1
A,2020-01-01,A2,2010-01-01,permanent,50,E2
A,2021-01-01,A1,2000-01-01,permanent,100,E1
A,2021-01-01,A2,2010-01-01,permanent,50,E2
A,2022-01-01,AP,2022-01-01,,120,
B,2020-01-01,B1,2005-01-01,permanent,200,E1
B,2023-01-01,BP,2023-01-01,,220,
C,2020-01-01,CP,2000-01-01,,300,
C,2021-01-01,CP,2000-01-01,,300,
D,2020-01-01,D1,2005-01-01,permanent,100,E1
D,2021-01-01,DP,2021-01-01,,,
E,2020-01-01,E1,2005-01-01,permanent,100,E1
E,2021-01-01,EP,2021-01-01,,90,
E,2022-01-01,E2,2022-01-01,permanent,500,E2
F,2020-01-01,F1,2005-01-01,permanent,100,E2
F,2021-01-01,F1,2005-01-01,permanent,100,E2
")
  personnel <- fread(text = "
personnel_id,ref_date,employment_status,age,gender
A,2020-01-01,active,63.5,f
A,2021-01-01,active,64.5,f
A,2022-01-01,pensioner,65.5,f
B,2020-01-01,active,59.2,m
B,2023-01-01,pensioner,62.2,m
C,2020-01-01,pensioner,70,m
C,2021-01-01,pensioner,71,m
D,2020-01-01,active,61,f
D,2021-01-01,pensioner,62,f
E,2020-01-01,active,60,m
E,2021-01-01,pensioner,61,m
E,2022-01-01,active,62,m
F,2020-01-01,active,40,f
F,2021-01-01,active,41,f
")
  for (col in c("ref_date", "start_date")) contracts[, (col) := as.Date(get(col))]
  personnel[, ref_date := as.Date(ref_date)]
  contracts[, end_date := as.Date(NA)]
  list(c = contracts, p = personnel)
}

test_that("a retirement is a first pension after an earlier active snapshot, however far back", {
  d <- replacement_fixture()
  out <- estimate_replacement_rates(d$c, d$p)
  # C was a pensioner throughout and F never retired
  expect_setequal(out$personnel_id, c("A", "B", "D", "E"))
  expect_equal(out[order(personnel_id), periods_apart], c(1L, 3L, 1L, 1L))
  expect_equal(out[personnel_id == "B", last_date], as.Date("2020-01-01"))
})

test_that("pay is the total on all contracts, and the rate is pension over last salary", {
  d <- replacement_fixture()
  out <- estimate_replacement_rates(d$c, d$p)
  expect_equal(out[personnel_id == "A", c(last_salary, first_pension, replacement_rate)],
               c(150, 120, 0.8))
  expect_equal(out[personnel_id == "B", replacement_rate], 1.1)
})

test_that("a blank pension keeps the retiree but leaves the rate blank", {
  d <- replacement_fixture()
  out <- estimate_replacement_rates(d$c, d$p)
  expect_true(is.na(out[personnel_id == "D", replacement_rate]))
})

test_that("pay after the pension starts is not the last salary", {
  d <- replacement_fixture()
  out <- estimate_replacement_rates(d$c, d$p)
  expect_equal(out[personnel_id == "E", c(last_salary, replacement_rate)], c(100, 0.9))
})

test_that("groups are taken at the last active snapshot, from the main contract", {
  d <- replacement_fixture()
  out <- estimate_replacement_rates(d$c, d$p, group_cols = c("est_id", "gender", "age"))
  expect_identical(names(out)[1:4], c("personnel_id", "est_id", "gender", "age"))
  # A's main contract in 2021 is the newer one, at E2; A is counted once; age
  # is as at 2021, A's last active snapshot
  expect_equal(out[personnel_id == "A", .(est_id, gender, age)],
               data.table(est_id = "E2", gender = "f", age = 64.5))
  expect_equal(nrow(out), 4L)
})

test_that("a group column neither table has stops with a clear message", {
  d <- replacement_fixture()
  expect_error(estimate_replacement_rates(d$c, d$p, group_cols = "grade"),
               "Columns not found in contract_dt or personnel_dt: grade")
})

test_that("the caller's tables are not changed", {
  d <- replacement_fixture()
  before <- copy(d)
  estimate_replacement_rates(d$c, d$p, group_cols = c("est_id", "age"))
  expect_identical(d, before)
})
