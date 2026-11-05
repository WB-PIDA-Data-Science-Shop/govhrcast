# Smaller errors: starting dates, pension totals, who a policy covers, what the
# user is told, and what happens when a status-quo estimate fails.

library(data.table)

start <- as.Date("2020-01-01")

small_run_data <- function() {
  list(
    c = data.table(
      contract_id = paste0("C", 1:4),
      personnel_id = paste0("P", 1:4),
      est_id = c("E1", "E1", "E2", "E2"),
      paygrade = "A",
      start_date = as.Date("2010-01-01"),
      end_date = as.Date(NA),
      contract_type = c("permanent", "fixed-term", NA, "permanent"),
      gross_salary_lcu = 100
    ),
    p = data.table(
      personnel_id = paste0("P", 1:4),
      employment_status = "active",
      birth_date = start - round(365.25 * 70),
      age = 70,
      personnel_tenure = 10
    ),
    s = data.table(est_id = c("E1", "E2"), paygrade = "A", gross_salary_lcu = 100)
  )
}

two_snapshots <- function(d = small_run_data()) {
  stamp <- function(x, date) copy(x)[, ref_date := date]
  list(c = rbind(stamp(d$c, start - 366), stamp(d$c, start)),
       p = rbind(stamp(d$p, start - 366), stamp(d$p, start)),
       s = d$s)
}

# -- Where the projection starts ---------------------------------------------

test_that("a ref_date between snapshots stops, and says which snapshots there are", {
  d <- two_snapshots()
  expect_error(
    simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start - 100, retirement_policy = NULL),
    "2020-01-01"
  )
  # The default date, today, is the usual way to hit this
  expect_error(
    simulate_horizon(d$c, d$p, d$s, n_periods = 1L, retirement_policy = NULL),
    "ref_date"
  )
})

test_that("a ref_date on a snapshot, or with no snapshot column, runs as before", {
  d <- two_snapshots()
  run <- simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL)
  expect_equal(run$comparison$n_headcount_start, 4L)
  s <- small_run_data()
  run <- simulate_horizon(s$c, s$p, s$s, n_periods = 1L, ref_date = start, retirement_policy = NULL)
  expect_equal(run$comparison$n_headcount_start, 4L)
})

# -- Pension totals ----------------------------------------------------------

test_that("the year's pension total matches the register after pension growth", {
  d <- small_run_data()
  run <- simulate_horizon(d$c, d$p, d$s, n_periods = 2L, ref_date = start, seed = 1L,
                          retirement_policy = design_retirement_policy_age(60, pension_flat(500)),
                          pension_cola_rate = 0.10)
  expect_equal(run$comparison$pension_cost_new, c(2000, 0))
  expect_equal(run$comparison$pension_cost_total, c(2200, 2420))
  expect_equal(run$comparison$pension_cost_total[2], sum(run$pensioner_register$pension_amount))
})

# -- Who a policy covers -----------------------------------------------------

test_that("retirement applies active_types, and a blank type still counts as staff", {
  d <- small_run_data()
  policy <- design_retirement_policy_age(60, pension_flat(500))
  policy$defaults$active_types <- "permanent"
  out <- simulate_retirement(d$c, d$p, start, policy)
  expect_setequal(out$retirees_dt$personnel_id, c("P1", "P3", "P4"))
  hazard <- eligibility_status_quo(hazard_table = data.table(age_yr_int = 70L, hazard_rate = 1))
  hazard$defaults$active_types <- "fixed-term"
  picked <- compute_status_quo_retirements(d$c, d$p, hazard, start)
  expect_setequal(picked$personnel_id, c("P2", "P3"))
})

test_that("with no active_types, retirement covers everyone on the payroll", {
  d <- small_run_data()
  out <- simulate_retirement(d$c, d$p, start, design_retirement_policy_age(60, pension_flat(500)))
  expect_setequal(out$retirees_dt$personnel_id, paste0("P", 1:4))
})

test_that("movement applies active_types, and a blank type still counts as staff", {
  d <- small_run_data()
  d$s <- data.table(paygrade = c("A", "B"), gross_salary_lcu = c(100, 200))
  policy <- list(group_cols = "paygrade",
                 policy_table = data.table(from_group = "A", to_group = "B", movement_rate = 1),
                 defaults = list(movement_strategy = "random", active_types = "permanent"))
  out <- simulate_promotions_transfers(d$c, d$p, d$s, policy_params = policy, ref_date = start)
  expect_setequal(out$movers_dt$personnel_id, c("P1", "P3", "P4"))
  expect_equal(out$contract_dt[personnel_id == "P2", paygrade], "A")
  flat <- list(group_cols = "paygrade", policy_table = NULL,
               defaults = list(movement_rate = 1, movement_strategy = "random", active_types = "fixed-term"))
  demand <- compute_fixed_rate_movements(d$c, d$p, d$s, flat, start)
  expect_equal(demand$current_stock, 2L)
})

test_that("a mover's main contract is one the policy covers", {
  d <- small_run_data()
  d$c <- rbind(d$c, d$c[1L][, `:=`(contract_id = "C5", contract_type = "fixed-term",
                                   start_date = as.Date("2015-01-01"))])
  d$s <- data.table(paygrade = c("A", "B"), gross_salary_lcu = c(100, 200))
  policy <- list(group_cols = "paygrade",
                 policy_table = data.table(from_group = "A", to_group = "B", movement_rate = 1),
                 defaults = list(movement_strategy = "random", active_types = "permanent"))
  ct <- simulate_promotions_transfers(d$c, d$p, d$s, policy_params = policy, ref_date = start)$contract_dt
  expect_equal(ct[contract_id == "C1", paygrade], "B")
  expect_equal(ct[contract_id == "C5", paygrade], "A")
})

# -- What the user is told ---------------------------------------------------

test_that("print and summary tell scenarios apart by their label", {
  d <- small_run_data()
  run <- function(name) {
    simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL,
                     output_control_args = list(scenario_name = name))
  }
  out <- capture.output(print(run("Baseline")))
  expect_true(any(grepl("Baseline", out)))
  stacked <- run("Baseline")
  stacked$comparison <- rbind(stacked$comparison, run("Reform")$comparison)
  out <- capture.output(print(stacked))
  expect_true(any(grepl("Scenarios : 2", out)))
  expect_true(any(grepl("Periods   : 1", out)))
  expect_true(any(grepl("Reform", out)))
  capture.output(smry <- summary(stacked))
  expect_equal(nrow(smry), 2L)
  expect_setequal(smry$scenario_label, c("Baseline", "Reform"))
})

test_that("groups with no rate in a table are reported once, not silently frozen", {
  d <- small_run_data()
  exits <- list(group_cols = "est_id", policy_table = data.table(est_id = "E1", exit_rate = 0.1),
                defaults = list(exit_strategy = "random"))
  expect_message(
    simulate_horizon(d$c, d$p, d$s, n_periods = 2L, ref_date = start, retirement_policy = NULL,
                     exit_policy = exits, seed = 1L),
    "exit.*1 group.*E2"
  )
  msgs <- capture_messages(
    simulate_horizon(d$c, d$p, d$s, n_periods = 2L, ref_date = start, retirement_policy = NULL,
                     exit_policy = exits, seed = 1L)
  )
  expect_length(grep("E2", msgs), 1L)
})

test_that("hiring and retirement tables report their missing groups too", {
  d <- small_run_data()
  expect_message(
    simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL,
                     hiring_policy = design_hiring_policy_adjustment(
                       data.table(est_id = "E1", hiring_rate = 0.1), group_cols = "est_id"), seed = 1L),
    "hiring.*1 group.*E2"
  )
  policy <- design_retirement_policy_status_quo(pension = pension_flat(1), group_cols = "est_id")
  policy$policy_table <- data.table(age_yr_int = 70L, est_id = "E1", hazard_rate = 0)
  expect_message(
    simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = policy, seed = 1L),
    "retirement.*1 group.*E2"
  )
})

test_that("no message when every group has a rate, or the rate is flat", {
  d <- small_run_data()
  exits <- list(group_cols = "est_id", policy_table = data.table(est_id = c("E1", "E2"), exit_rate = 0.1),
                defaults = list(exit_strategy = "random"))
  expect_no_message(
    simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL,
                     exit_policy = exits, seed = 1L)
  )
  expect_no_message(
    simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL,
                     exit_policy = design_exit_policy_flat(0.1), seed = 1L)
  )
})

# -- When a status-quo estimate cannot be made -------------------------------

test_that("a flat exit rate on panel data is used, not replaced by the history", {
  d <- two_snapshots()
  run <- simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL,
                          exit_policy = design_exit_policy_flat(0.5), seed = 1L)
  # The two snapshots show nobody leaving, so history alone would give none
  expect_equal(run$comparison$n_non_ret_exits, 2L)
})

test_that("a failed exit estimate stops the run instead of using a flat default", {
  d <- two_snapshots()
  policy <- list(group_cols = "no_such_column", policy_table = NULL,
                 defaults = list(exit_rate = 0.05, exit_strategy = "random"))
  expect_error(
    simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL,
                     exit_policy = policy),
    "could not estimate status-quo exit rates"
  )
})

test_that("status-quo exits on one snapshot stop, like hiring and retirement", {
  d <- small_run_data()
  expect_error(
    simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL,
                     exit_policy = design_exit_policy_status_quo()),
    "status-quo exits need a panel"
  )
})

test_that("make_status_quo_policies() is deprecated, and points to the design helpers", {
  d <- two_snapshots()
  expect_warning(make_status_quo_policies(d$c, d$p, d$s), class = "deprecatedWarning")
  expect_warning(make_status_quo_policies(d$c, d$p, d$s), "design_\\*_status_quo")
})
