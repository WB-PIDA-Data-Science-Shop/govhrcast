# Characterisation tests: pin today's full outputs
#
# These tests record what the package CURRENTLY produces for two complete
# status-quo projections on the bundled data. They are not evidence that the
# numbers are right -- known bugs on the errors list are baked in until fixed. Their
# job is to make any change to the outputs
# visible during refactoring: a refactor must leave the snapshots unchanged; a
# fix that changes numbers on purpose updates the snapshot in the same change,
# with the reason recorded in the commit and the backlog.
#
# Snapshots live in tests/testthat/_snaps/characterisation.md.

library(data.table)

# A 5-year status-quo projection from 2012 with all five modules on.
characterisation_run <- function(salary_col, explicit_hiring_scale) {
  start <- as.Date("2012-09-01")
  hc <- bra_hrmis_contract[ref_date <= start]
  hp <- bra_hrmis_personnel[ref_date <= start]
  scale <- hc[ref_date == start & !is.na(get(salary_col)),
              .(pay = mean(get(salary_col))), by = est_id]
  setnames(scale, "pay", salary_col)
  suppressMessages(suppressWarnings(simulate_horizon(
    contract_dt = hc, personnel_dt = hp, salary_scale_dt = scale,
    n_periods = 5L, ref_date = start,
    retirement_policy = design_retirement_policy_status_quo(
      pension = pension_db(0.02, 35, 0.80), mandatory_age = 75),
    exit_policy     = design_exit_policy_status_quo(group_cols = "est_id"),
    movement_policy = design_movement_policy_status_quo(
      group_cols = "est_id", movement_type = "transfer", salary_update_rule = "keep"),
    hiring_policy   = design_hiring_policy_status_quo(
      group_cols = "est_id", salary_scale = if (explicit_hiring_scale) scale),
    allowance_policy = design_allowance_policy_status_quo(
      bra_hrmis_allowance, hc, group_cols = "est_id", ref_date = start),
    salary_growth_rate = 0.05,
    column_spec_args   = list(salary_col = salary_col),
    seed = 1L
  )))
}

# The parts of a run to pin: the comparison table and a register summary.
characterise <- function(res) {
  cmp <- as.data.frame(res$comparison)
  cmp$period_date <- as.character(cmp$period_date)
  reg <- res$pensioner_register
  list(
    comparison = cmp,
    register = list(rows = nrow(reg),
                    pension_total = sum(reg$pension_amount, na.rm = TRUE),
                    final_salary_total = sum(reg$final_salary, na.rm = TRUE))
  )
}

test_that("characterisation A: base salary as pay, hiring scale derived automatically", {
  res <- characterisation_run("base_salary_lcu", explicit_hiring_scale = FALSE)
  expect_snapshot_value(characterise(res), style = "deparse")
})

test_that("characterisation B: default names throughout, gross salary as pay", {
  res <- characterisation_run("gross_salary_lcu", explicit_hiring_scale = FALSE)
  expect_snapshot_value(characterise(res), style = "deparse")
})
