# Unit tests for R/allowance_policy_helpers.R

library(testthat)
library(data.table)

make_aph_contracts <- function() {
  data.table(
    contract_id   = paste0("C", 1:4),
    personnel_id  = paste0("P", 1:4),
    est_id        = c("E1", "E1", "E2", "E2"),
    contract_type = c("permanent", "permanent", "permanent", "pensioner")
  )
}
make_aph_personnel <- function() {
  data.table(personnel_id = paste0("P", 1:4), employment_status = "active")
}
allowance_total <- function(policy, ct = make_aph_contracts()) {
  simulate_allowances(ct, make_aph_personnel(), policy_params = policy,
                      ref_date = as.Date("2020-01-01"))$summary$allowance_cost_total
}

# =============================================================================
# design_allowance_policy_adjustment()
# =============================================================================

test_that("design_allowance_policy_adjustment builds the policy shape resolve_allowance_amounts() expects", {
  tbl <- data.table(est_id = c("E1", "E2"), allowance_type = "hazard", amount = c(100, 50))
  ap <- design_allowance_policy_adjustment(tbl, group_cols = "est_id")
  expect_equal(ap$group_cols, c("est_id", "allowance_type"))
  expect_equal(ap$defaults, list(amount = 0))
  expect_null(ap$multiplier_group_cols)
  expect_null(ap$multiplier_policy_table)
  expect_equal(ap$multiplier_default, 1)
  # 2 active E1 contracts x 100 + 1 active E2 contract x 50 (the pensioner is excluded)
  expect_equal(allowance_total(ap), 250)
})

test_that("design_allowance_policy_adjustment: a type-only table gives everyone the same amount", {
  ap <- design_allowance_policy_adjustment(data.table(allowance_type = "transport", amount = 200))
  expect_equal(ap$group_cols, "allowance_type")
  expect_equal(allowance_total(ap), 600)   # 3 active contracts
})

test_that("design_allowance_policy_adjustment accepts group_cols that already include allowance_type", {
  tbl <- data.table(est_id = "E1", allowance_type = "hazard", amount = 100)
  ap <- design_allowance_policy_adjustment(tbl, group_cols = c("allowance_type", "est_id"))
  expect_equal(ap$group_cols, c("est_id", "allowance_type"))
})

test_that("design_allowance_policy_adjustment applies unmatched_amount to groups missing from the table", {
  tbl <- data.table(est_id = "E1", allowance_type = "hazard", amount = 100)
  ap <- design_allowance_policy_adjustment(tbl, group_cols = "est_id", unmatched_amount = 10)
  expect_equal(allowance_total(ap), 210)   # 2 x 100 in E1 + 1 x 10 in E2
})

test_that("design_allowance_policy_adjustment: a scalar total_multiplier scales everyone", {
  tbl <- data.table(est_id = c("E1", "E2"), allowance_type = "hazard", amount = c(100, 50))
  ap <- design_allowance_policy_adjustment(tbl, group_cols = "est_id", total_multiplier = 0.9)
  expect_equal(ap$multiplier_default, 0.9)
  expect_equal(allowance_total(ap), 225)
})

test_that("design_allowance_policy_adjustment: a total_multiplier table scales by group", {
  tbl  <- data.table(est_id = c("E1", "E2"), allowance_type = "hazard", amount = c(100, 50))
  mult <- data.table(est_id = "E2", total_allowance_multiplier = 2)
  ap <- design_allowance_policy_adjustment(tbl, group_cols = "est_id",
                                           total_multiplier = mult,
                                           multiplier_group_cols = "est_id")
  expect_equal(ap$multiplier_default, 1)
  expect_equal(allowance_total(ap), 300)   # E1: 2 x 100 (unmatched -> 1); E2: 1 x 50 x 2
})

test_that("design_allowance_policy_adjustment stores copies, not references", {
  tbl <- data.table(allowance_type = "transport", amount = 200)
  ap <- design_allowance_policy_adjustment(tbl)
  tbl[, amount := 1]
  expect_equal(ap$policy_table$amount, 200)
})

test_that("design_allowance_policy_adjustment rejects malformed amount tables", {
  adj <- function(tbl, ...) design_allowance_policy_adjustment(tbl, ...)
  expect_error(adj(data.frame(allowance_type = "a", amount = 1)), regexp = "data.table")
  expect_error(adj(data.table(amount = 1)), regexp = "allowance_type")
  expect_error(adj(data.table(allowance_type = "a")), regexp = "amount")
  expect_error(adj(data.table(allowance_type = "a", amount = 1), group_cols = "est_id"),
               regexp = "est_id")
  expect_error(adj(data.table(allowance_type = character(0), amount = numeric(0))),
               regexp = "no rows")
  expect_error(adj(data.table(allowance_type = NA_character_, amount = 1)), regexp = "NA")
  expect_error(adj(data.table(allowance_type = c("a", "a"), amount = 1)), regexp = "more than one row")
  expect_error(adj(data.table(allowance_type = "a", amount = -1)), regexp = "0 or more")
  expect_error(adj(data.table(allowance_type = "a", amount = NA_real_)), regexp = "no NA")
  expect_error(adj(data.table(allowance_type = "a", amount = 1), unmatched_amount = -5),
               regexp = "unmatched_amount")
})

test_that("design_allowance_policy_adjustment rejects malformed multipliers", {
  tbl <- data.table(allowance_type = "a", amount = 1)
  mult <- data.table(est_id = "E1", total_allowance_multiplier = 1.1)
  adj <- function(...) design_allowance_policy_adjustment(tbl, ...)
  expect_error(adj(total_multiplier = -1), regexp = "total_multiplier")
  expect_error(adj(total_multiplier = c(1, 2)), regexp = "total_multiplier")
  expect_error(adj(total_multiplier = 1, multiplier_group_cols = "est_id"), regexp = "only applies")
  expect_error(adj(total_multiplier = mult), regexp = "multiplier_group_cols must be set")
  expect_error(adj(total_multiplier = mult, multiplier_group_cols = "allowance_type"),
               regexp = "must not include")
  expect_error(adj(total_multiplier = mult, multiplier_group_cols = "paygrade"), regexp = "paygrade")
  expect_error(adj(total_multiplier = mult[, total_allowance_multiplier := -1],
                   multiplier_group_cols = "est_id"),
               regexp = "0 or more")
})

# =============================================================================
# design_allowance_policy_status_quo()
# =============================================================================

test_that("design_allowance_policy_status_quo reproduces the actual allowance total at the baseline snapshot", {
  d  <- as.Date("2012-09-01")
  ct <- bra_hrmis_contract[ref_date == d]
  ap <- design_allowance_policy_status_quo(bra_hrmis_allowance, bra_hrmis_contract,
                                           group_cols = "est_id", ref_date = d)
  expect_equal(ap$group_cols, c("est_id", "allowance_type"))

  modelled <- simulate_allowances(ct, bra_hrmis_personnel[ref_date == d],
                                  policy_params = ap, ref_date = d)$summary$allowance_cost_total
  active_ids <- ct[!contract_type %in% c("pensioner", "inactive"), contract_id]
  actual <- bra_hrmis_allowance[ref_date == d & contract_id %in% active_ids,
                                sum(allowance_lcu, na.rm = TRUE)]
  expect_equal(modelled, actual)

  ap_cut <- design_allowance_policy_status_quo(bra_hrmis_allowance, bra_hrmis_contract,
                                               group_cols = "est_id", ref_date = d,
                                               total_multiplier = 0.9)
  modelled_cut <- simulate_allowances(ct, bra_hrmis_personnel[ref_date == d],
                                      policy_params = ap_cut, ref_date = d)$summary$allowance_cost_total
  expect_equal(modelled_cut, 0.9 * actual)
})

test_that("design_allowance_policy_status_quo validates group_cols", {
  expect_error(
    design_allowance_policy_status_quo(bra_hrmis_allowance, bra_hrmis_contract,
                                       group_cols = NULL, ref_date = as.Date("2012-09-01")),
    regexp = "group_cols"
  )
})

test_that("an allowance policy from the helpers runs through simulate_horizon()", {
  d  <- as.Date("2012-09-01")
  ap <- design_allowance_policy_status_quo(bra_hrmis_allowance, bra_hrmis_contract,
                                           group_cols = "est_id", ref_date = d)
  ct <- bra_hrmis_contract[ref_date == d]
  res <- simulate_horizon(
    contract_dt      = ct,
    personnel_dt     = bra_hrmis_personnel[ref_date == d],
    salary_scale_dt  = data.table(base_salary_lcu = 1000),
    n_periods        = 1L,
    retirement_policy = NULL,   # single snapshot; this test is about allowances only
    allowance_policy = ap,
    salary_growth_rate = 0,
    ref_date         = d,
    column_spec_args = list(salary_col = "base_salary_lcu"),
    seed             = 1L
  )
  expect_gt(res$summary_dt$allowance_cost_end, 0)
})
