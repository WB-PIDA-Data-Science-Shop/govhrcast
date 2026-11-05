# Unit tests for allowance_update.R
# Testing update_allowance_register()

make_resolved_amounts <- function(amount_val = 100) {
  data.table::data.table(
    contract_id    = c("C1", "C2"),
    personnel_id   = c("P1", "P2"),
    allowance_type = c("hazard", "hazard"),
    amount         = c(amount_val, amount_val)
  )
}

test_that("update_allowance_register requires resolved_amounts_dt to be a data.table", {
  expect_error(
    update_allowance_register(NULL, list(), ref_date = as.Date("2020-01-01")),
    "data.table"
  )
})

test_that("update_allowance_register initializes the ledger when allowance_dt is NULL", {
  resolved <- make_resolved_amounts()
  out <- update_allowance_register(NULL, resolved, ref_date = as.Date("2020-01-01"))

  expect_equal(nrow(out), nrow(resolved))
  expect_true("period_date" %in% names(out))
  expect_true(all(out$period_date == as.Date("2020-01-01")))
})

test_that("update_allowance_register rejects a non-data.table allowance_dt", {
  resolved <- make_resolved_amounts()
  expect_error(
    update_allowance_register(list(), resolved, ref_date = as.Date("2020-01-01")),
    "data.table"
  )
})

test_that("update_allowance_register accumulates across periods without rewriting history", {
  resolved1 <- make_resolved_amounts(amount_val = 100)
  ledger1   <- update_allowance_register(NULL, resolved1, ref_date = as.Date("2020-01-01"))

  resolved2 <- make_resolved_amounts(amount_val = 150)
  ledger2   <- update_allowance_register(ledger1, resolved2, ref_date = as.Date("2021-01-01"))

  expect_equal(nrow(ledger2), nrow(resolved1) + nrow(resolved2))
  expect_equal(data.table::uniqueN(ledger2$period_date), 2L)

  # Period 1's rows must be untouched -- still amount 100, still dated 2020-01-01
  period1_rows <- ledger2[period_date == as.Date("2020-01-01")]
  expect_true(all(period1_rows$amount == 100))
  period2_rows <- ledger2[period_date == as.Date("2021-01-01")]
  expect_true(all(period2_rows$amount == 150))
})

test_that("update_allowance_register stamps the ref_date value under the period_date column", {
  resolved <- make_resolved_amounts()
  out <- update_allowance_register(NULL, resolved, ref_date = as.Date("2022-06-01"))
  expect_true(all(out$period_date == as.Date("2022-06-01")))
})
