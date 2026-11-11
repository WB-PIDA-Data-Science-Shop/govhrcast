# Who flow hiring replaces, and the other hiring modes
#
# The rules, from decisions D2 and D4:
#   * Flow hiring replaces everyone who left this year: retirees and other exits.
#     A replacement rate of 1 holds headcount.
#   * A year when nobody leaves is a year with nothing to replace.
#   * The "combined" mode is gone.

library(data.table)

start <- as.Date("2020-01-01")

# E1 has an older person who retires; E2 has someone a flat rate lets go.
two_units <- function() {
  list(
    c = data.table(
      contract_id = paste0("C", 1:6),
      personnel_id = paste0("P", 1:6),
      est_id = c("E1", "E1", "E1", "E2", "E2", "E2"),
      start_date = as.Date("2010-01-01"),
      end_date = as.Date(NA),
      contract_type = "permanent",
      gross_salary_lcu = 100
    ),
    p = data.table(
      personnel_id = paste0("P", 1:6),
      employment_status = "active",
      birth_date = start - round(365.25 * c(70, 30, 30, 30, 30, 30)),
      age = c(70, 30, 30, 30, 30, 30),
      personnel_tenure = 10
    ),
    s = data.table(est_id = c("E1", "E2"), gross_salary_lcu = 100)
  )
}

retire_at_60 <- function() design_retirement_policy_age(60, pension_rate(0.5))
leave_e2 <- function() {
  list(group_cols = "est_id", policy_table = data.table(est_id = "E2", exit_rate = 1 / 3),
       defaults = list(exit_strategy = "random"))
}

flow_run <- function(d, replacement_rate = 1, retirement = retire_at_60(), exits = leave_e2(),
                     n_periods = 1L, group_cols = "est_id") {
  simulate_horizon(d$c, d$p, d$s, n_periods = n_periods, baseline_date = start,
                   retirement_policy = retirement, exit_policy = exits,
                   hiring_policy = design_hiring_policy_flow(replacement_rate, group_cols = group_cols),
                   seed = 1L, output_control_args = define_output_control(return_microdata = TRUE))
}

test_that("flow hiring replaces retirees and other exits, each in their own group", {
  run <- suppressMessages(flow_run(two_units()))
  expect_equal(run$comparison$n_exits, 1L)
  expect_equal(run$comparison$n_non_ret_exits, 1L)
  expect_equal(run$comparison$n_hires, 2L)
  new <- run$contract_dt[!contract_id %in% paste0("C", 1:6)]
  expect_setequal(new$est_id, c("E1", "E2"))
})

test_that("a replacement rate of 1 holds headcount", {
  run <- suppressMessages(flow_run(two_units(), n_periods = 3L,
                                   retirement = NULL))
  expect_equal(run$comparison$n_headcount_end, run$comparison$n_headcount_start)
  expect_equal(unique(run$comparison$n_headcount_end), 6L)
})

test_that("flow hiring replaces other exits when nobody retires, and ungrouped too", {
  run <- suppressMessages(flow_run(two_units(), retirement = NULL))
  expect_equal(run$comparison$n_hires, 1L)
  d <- two_units()
  d$s <- data.table(gross_salary_lcu = 100)
  ungrouped <- suppressMessages(flow_run(d, group_cols = NULL))
  expect_equal(ungrouped$comparison$n_hires, 2L)
})

test_that("a year when nobody leaves is a year with nothing to replace", {
  d <- two_units()
  # Eligibility would pick someone, but the run has its own record of who left
  run <- suppressMessages(flow_run(d, retirement = design_retirement_policy_age(90, pension_rate(0.5)),
                                   exits = NULL))
  expect_equal(run$comparison$n_hires, 0L)
  # An empty table passed in says the same
  out <- simulate_hiring(d$c, d$p, list(mode = "flow", group_cols = "est_id", replacement_rate = 1,
                                        defaults = list(eligibility_type = "age_only", min_age = 60),
                                        salary_scale = d$s),
                         leavers_dt = data.table(), as_of_date = start)
  expect_equal(out$summary$n_new_hires, 0L)
})

test_that("with no table of leavers, flow hiring needs an eligibility rule in defaults", {
  d <- two_units()
  policy <- list(mode = "flow", group_cols = "est_id", replacement_rate = 1, salary_scale = d$s)
  # At the top level, where the old help page said to put them, they do nothing
  expect_error(
    simulate_hiring(d$c, d$p, c(policy, list(eligibility_type = "age_only", min_age = 60)), as_of_date = start),
    "defaults\\$eligibility_type"
  )
  out <- simulate_hiring(d$c, d$p,
                         c(policy, list(defaults = list(eligibility_type = "age_only", min_age = 60))),
                         as_of_date = start)
  expect_equal(out$summary$n_new_hires, 1L)
})

test_that("a stock target for a group with no staff yet hires up to the target", {
  d <- two_units()
  d$s <- data.table(est_id = c("E1", "E2", "E3"), gross_salary_lcu = 100)
  policy <- list(mode = "stock", group_cols = "est_id",
                 stock_targets = data.table(est_id = c("E1", "E3"), target_stock = c(3L, 4L)),
                 salary_scale = d$s)
  out <- simulate_hiring(d$c, d$p, policy, as_of_date = start)
  expect_equal(out$new_hire_contracts_dt[, .N, by = est_id], data.table(est_id = "E3", N = 4L))
  # E2 has no target, so stays as it is
  expect_equal(out$summary$n_downsized, 0L)
})

test_that("the combined mode is gone", {
  d <- two_units()
  expect_error(
    simulate_hiring(d$c, d$p, list(mode = "combined", group_cols = "est_id", replacement_rate = 0.1,
                                   stock_targets = data.table(est_id = "E1", target_stock = 3L),
                                   salary_scale = d$s), as_of_date = start),
    "mode"
  )
  expect_false(exists("compute_combined_demand", envir = asNamespace("govhrcast"), inherits = FALSE))
})
