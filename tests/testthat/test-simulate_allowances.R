# Unit tests for simulate_allowances.R
# Testing simulate_allowances()

make_sim_contract_dt <- function() {
  data.table::data.table(
    contract_id   = c("C1", "C2", "C3"),
    personnel_id  = c("P1", "P2", "P3"),
    est_id        = c("E1", "E1", "E2"),
    contract_type = "permanent"
  )
}

make_sim_personnel_dt <- function() {
  data.table::data.table(
    personnel_id       = c("P1", "P2", "P3"),
    employment_status  = "active"
  )
}

make_scale_dt <- function() {
  data.table::data.table(
    est_id         = c("E1", "E1", "E2", "E2"),
    allowance_type = c("hazard", "transport", "hazard", "transport"),
    amount         = c(100, 20, 5, 5)
  )
}

test_that("simulate_allowances: policy_params = NULL is a no-op", {
  ct <- make_sim_contract_dt()
  pt <- make_sim_personnel_dt()

  out <- simulate_allowances(
    contract_dt = ct, personnel_dt = pt, policy_params = NULL, ref_date = as.Date("2020-01-01")
  )

  expect_identical(out$contract_dt, ct)
  expect_identical(out$personnel_dt, pt)
  expect_equal(out$summary$n_contracts, 0L)
  expect_equal(out$summary$allowance_cost_total, 0)
  expect_equal(nrow(out$allowance_dt), 0L)
})

test_that("simulate_allowances: NULL policy_params passes an existing allowance_dt through unchanged", {
  ct <- make_sim_contract_dt()
  pt <- make_sim_personnel_dt()
  existing_ledger <- data.table::data.table(
    contract_id = "C1", personnel_id = "P1", allowance_type = "hazard",
    amount = 100, period_date = as.Date("2019-01-01")
  )

  out <- simulate_allowances(
    contract_dt = ct, personnel_dt = pt, allowance_dt = existing_ledger,
    policy_params = NULL, ref_date = as.Date("2020-01-01")
  )

  expect_identical(out$allowance_dt, existing_ledger)
})

test_that("simulate_allowances never mutates contract_dt/personnel_dt with an active policy", {
  ct <- make_sim_contract_dt()
  pt <- make_sim_personnel_dt()
  scale_dt <- make_scale_dt()

  out <- simulate_allowances(
    contract_dt = ct, personnel_dt = pt,
    policy_params = list(group_cols = c("est_id", "allowance_type"), policy_table = scale_dt, defaults = list(amount = 0)),
    ref_date = as.Date("2020-01-01")
  )

  expect_identical(out$contract_dt, ct)
  expect_identical(out$personnel_dt, pt)
})

test_that("simulate_allowances resolves amounts and summarizes correctly", {
  ct <- make_sim_contract_dt()
  pt <- make_sim_personnel_dt()
  scale_dt <- make_scale_dt()

  out <- simulate_allowances(
    contract_dt = ct, personnel_dt = pt,
    policy_params = list(group_cols = c("est_id", "allowance_type"), policy_table = scale_dt, defaults = list(amount = 0)),
    ref_date = as.Date("2020-01-01")
  )

  # C1, C2 in E1 (hazard 100 + transport 20 each); C3 in E2 (hazard 5 + transport 5)
  expected_total <- 2 * (100 + 20) + (5 + 5)
  expect_equal(out$summary$allowance_cost_total, expected_total)
  expect_equal(out$summary$n_contracts, 3L)
  expect_equal(out$summary$period_date, as.Date("2020-01-01"))
  expect_equal(nrow(out$allowance_dt), 3L * 2L)
})

test_that("simulate_allowances derives allowance_types from policy_table when not supplied", {
  ct <- make_sim_contract_dt()
  pt <- make_sim_personnel_dt()
  scale_dt <- make_scale_dt()

  out <- simulate_allowances(
    contract_dt = ct, personnel_dt = pt,
    policy_params = list(group_cols = c("est_id", "allowance_type"), policy_table = scale_dt, defaults = list(amount = 0)),
    ref_date = as.Date("2020-01-01")
  )
  expect_setequal(unique(out$allowance_dt$allowance_type), c("hazard", "transport"))
})

test_that("simulate_allowances errors when allowance_types cannot be derived", {
  ct <- make_sim_contract_dt()
  pt <- make_sim_personnel_dt()

  expect_error(
    simulate_allowances(
      contract_dt = ct, personnel_dt = pt,
      policy_params = list(group_cols = NULL, policy_table = NULL, defaults = list(amount = 50)),
      ref_date = as.Date("2020-01-01")
    ),
    "allowance_types"
  )
})

test_that("simulate_allowances accumulates the ledger across sequential periods", {
  ct <- make_sim_contract_dt()
  pt <- make_sim_personnel_dt()
  scale_dt <- make_scale_dt()
  policy <- list(group_cols = c("est_id", "allowance_type"), policy_table = scale_dt, defaults = list(amount = 0))

  out1 <- simulate_allowances(contract_dt = ct, personnel_dt = pt, policy_params = policy, ref_date = as.Date("2020-01-01"))
  out2 <- simulate_allowances(
    contract_dt = ct, personnel_dt = pt, allowance_dt = out1$allowance_dt,
    policy_params = policy, ref_date = as.Date("2021-01-01")
  )

  expect_equal(nrow(out2$allowance_dt), nrow(out1$allowance_dt) * 2L)
  expect_equal(data.table::uniqueN(out2$allowance_dt$period_date), 2L)
})

test_that("simulate_allowances validates ref_date format", {
  ct <- make_sim_contract_dt()
  pt <- make_sim_personnel_dt()
  scale_dt <- make_scale_dt()
  expect_error(
    simulate_allowances(
      contract_dt = ct, personnel_dt = pt,
      policy_params = list(group_cols = "allowance_type", policy_table = scale_dt, defaults = list(amount = 0)),
      ref_date = "not-a-date"
    )
  )
})
