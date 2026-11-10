# Who is staff, who is a pensioner, and who has left
#
# The rules, from govhr::dictionary and the user's decisions:
#   * A row is a pension if its contract type is "pensioner" or its person's
#     status is "pensioner". Every other row is staff, so someone with both a
#     pension and a working contract is on the register and on the payroll.
#   * "pensioner" and "inactive" contract types are off the payroll.
#   * A blank contract type is missing data: staff if the person's status is
#     active, off the payroll otherwise.
#   * Contracts are renewed past their end date in the projection (decision D3).
#   * Someone let go by downsizing leaves the payroll, and the wage bill says so.

library(data.table)

start <- as.Date("2020-01-01")

# Every kind of person the rules name, one row each unless stated.
state_fixture <- function() {
  c_dt <- data.table(
    contract_id = paste0("C", 1:10),
    personnel_id = c("staff", "blank_type", "two_rows", "two_rows", "both", "both",
                     "no_pay", "typed_row", "type_only", "inactive_blank"),
    est_id = "E1",
    start_date = as.Date("2000-01-01"),
    end_date = as.Date(NA),
    contract_type = c("permanent", NA, NA, NA, "pensioner", "permanent",
                      NA, "permanent", "pensioner", NA),
    gross_salary_lcu = c(1000, 800, 500, 100, 300, 700, NA, 400, 250, 600)
  )
  p_dt <- data.table(
    personnel_id = unique(c_dt$personnel_id),
    employment_status = c("active", "active", "pensioner", "active", "pensioner",
                          "pensioner", "active", "inactive"),
    birth_date = as.Date("1980-01-01"),
    gender = "F"
  )
  list(c = c_dt, p = p_dt, s = data.table(est_id = "E1", gross_salary_lcu = 900))
}

horizon_run <- function(d, ..., n_periods = 1L) {
  simulate_horizon(d$c, d$p, d$s, n_periods = n_periods, baseline_date = start,
                   retirement_policy = NULL, seed = 1L,
                   output_control_args = define_output_control(return_microdata = TRUE), ...)
}

# -- Who is who at the start of a projection ---------------------------------

test_that("the payroll holds staff only, whichever way a pensioner is recorded", {
  run <- suppressMessages(horizon_run(state_fixture()))
  # staff 1000 + blank_type 800 + both's working contract 700
  expect_equal(run$comparison$n_headcount_start, 3L)
  expect_equal(run$comparison$wage_bill_start, 2500)
  expect_equal(run$comparison$wage_bill_end, 2500)
})

test_that("existing pensioners are seeded on the register, and several rows add up", {
  reg <- suppressMessages(horizon_run(state_fixture()))$pensioner_register
  amount <- function(id) reg[personnel_id == id, pension_amount]
  expect_setequal(reg$personnel_id, c("two_rows", "both", "no_pay", "typed_row", "type_only"))
  expect_equal(nrow(reg), 5L)
  expect_equal(amount("two_rows"), 600)   # 500 + 100, not the larger row
  expect_equal(amount("both"), 300)       # the pension row only
  expect_equal(amount("typed_row"), 400)  # status wins over a working type
  expect_equal(amount("type_only"), 250)  # type alone is enough
  expect_equal(reg[personnel_id == "two_rows", final_salary], 600)
})

test_that("a pensioner with no pay stays on the register with a blank amount, and the user is told", {
  d <- state_fixture()
  expect_message(run <- horizon_run(d), "1 existing pensioner")
  reg <- run$pensioner_register
  expect_true(is.na(reg[personnel_id == "no_pay", pension_amount]))
  expect_equal(run$comparison$pension_cost_total, 600 + 300 + 400 + 250)
})

test_that("no message when every pensioner has a pension amount", {
  d <- state_fixture()
  d$c[personnel_id == "no_pay", gross_salary_lcu := 50]
  expect_no_message(horizon_run(d), message = "existing pensioner")
})

test_that("someone with a pension and a working contract stays a person in the projection", {
  run <- suppressMessages(horizon_run(state_fixture()))
  expect_true("both" %in% run$personnel_dt$personnel_id)
  expect_equal(run$contract_dt[personnel_id == "both", contract_type], "permanent")
  # Pure pensioners leave the working tables
  gone <- c("two_rows", "no_pay", "typed_row", "type_only")
  expect_false(any(gone %in% run$personnel_dt$personnel_id))
  expect_false(any(gone %in% run$contract_dt$personnel_id))
})

test_that("a blank contract type on someone who is not active is off the payroll", {
  run <- suppressMessages(horizon_run(state_fixture()))
  expect_equal(run$contract_dt[personnel_id == "inactive_blank", contract_type], "inactive")
})

test_that("the caller's tables keep their blank types", {
  d <- state_fixture()
  before <- copy(d$c)
  suppressMessages(horizon_run(d))
  expect_identical(d$c, before)
})

# -- Blank contract type: staff in every module ------------------------------

test_that("staff with a blank contract type get salary growth", {
  run <- suppressMessages(horizon_run(state_fixture(), salary_growth_rate = 0.05))
  expect_equal(run$comparison$wage_bill_end, 2500 * 1.05)
  expect_equal(run$contract_dt[personnel_id == "blank_type", gross_salary_lcu], 800 * 1.05)
})

test_that("staff with a blank contract type can exit", {
  run <- suppressMessages(horizon_run(state_fixture(),
                                      exit_policy = design_exit_policy_flat(exit_rate = 1)))
  expect_equal(run$comparison$n_non_ret_exits, 3L)
  expect_equal(run$comparison$non_ret_exit_savings, 2500)
  expect_equal(run$comparison$n_headcount_end, 0L)
  expect_equal(run$comparison$wage_bill_end, 0)
  expect_equal(run$contract_dt[personnel_id == "blank_type", contract_type], "inactive")
})

test_that("staff with a blank contract type get allowances; pensioners and leavers do not", {
  allowance <- design_allowance_policy_adjustment(data.table(allowance_type = "a", amount = 10))
  run <- suppressMessages(horizon_run(state_fixture(), allowance_policy = allowance))
  expect_equal(run$comparison$allowance_cost_end, 30)
})

test_that("on_payroll: pensioner and inactive are off, everything else including blank is on", {
  expect_identical(.on_payroll(c("permanent", "fixed-term", "short-term", NA, "pensioner", "inactive")),
                   c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

# -- Contract end dates: renewed in the projection (D3) ----------------------

ended_fixture <- function() {
  list(
    c = data.table(contract_id = c("C1", "C2"), personnel_id = c("ended", "open"),
                   est_id = "E1", start_date = as.Date("1990-01-01"),
                   end_date = as.Date(c("2015-01-01", NA)), contract_type = "permanent",
                   gross_salary_lcu = c(1000, 500)),
    p = data.table(personnel_id = c("ended", "open"), employment_status = "active",
                   birth_date = as.Date(c("1950-01-01", "1990-01-01")), gender = "F"),
    s = data.table(est_id = "E1", gross_salary_lcu = 900)
  )
}

test_that("someone whose contract end date has passed can still retire", {
  d <- ended_fixture()
  run <- simulate_horizon(d$c, d$p, d$s, n_periods = 1L, baseline_date = start, seed = 1L,
                          retirement_policy = design_retirement_policy_age_tenure(
                            min_age = 60, min_tenure = 1, pension = pension_flat(100)))
  expect_equal(run$comparison$n_exits, 1L)
  expect_equal(run$comparison$exit_savings, 1000)
  expect_equal(run$comparison$wage_bill_end, 500)
})

test_that("someone whose contract end date has passed can still be moved out by exits", {
  d <- ended_fixture()
  run <- simulate_horizon(d$c, d$p, d$s, n_periods = 1L, baseline_date = start, seed = 1L,
                          retirement_policy = NULL,
                          exit_policy = design_exit_policy_flat(exit_rate = 1),
                          output_control_args = define_output_control(return_microdata = TRUE))
  expect_equal(run$comparison$wage_bill_end, 0)
})

test_that("end dates are cleared for staff at the start of the projection", {
  d <- ended_fixture()
  run <- simulate_horizon(d$c, d$p, d$s, n_periods = 1L, baseline_date = start, seed = 1L,
                          retirement_policy = NULL,
                          output_control_args = define_output_control(return_microdata = TRUE))
  expect_true(all(is.na(run$contract_dt$end_date)))
  expect_equal(d$c[personnel_id == "ended", end_date], as.Date("2015-01-01"))  # caller's table untouched
})

# -- Downsizing ---------------------------------------------------------------

downsize_fixture <- function() {
  list(
    c = data.table(contract_id = paste0("C", 1:4), personnel_id = paste0("P", 1:4),
                   est_id = "E1",
                   start_date = as.Date(c("2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01")),
                   end_date = as.Date(NA), contract_type = "permanent",
                   gross_salary_lcu = c(1000, 900, 800, 700)),
    p = data.table(personnel_id = paste0("P", 1:4), employment_status = "active",
                   birth_date = as.Date("1980-01-01"), gender = "F"),
    s = data.table(est_id = "E1", gross_salary_lcu = 900)
  )
}
cut_to <- function(target) design_hiring_policy_stock(
  stock_targets = data.table(est_id = "E1", target_stock = target), group_cols = "est_id")

test_that("downsizing takes people off the payroll and the wage bill says so", {
  d <- downsize_fixture()
  run <- simulate_horizon(d$c, d$p, d$s, n_periods = 2L, baseline_date = start, seed = 1L,
                          retirement_policy = NULL, hiring_policy = cut_to(2L),
                          output_control_args = define_output_control(return_microdata = TRUE))
  cmp <- run$comparison
  # The two most recent hires go in year 1, and nobody in year 2
  expect_equal(cmp$n_downsized, c(2L, 0L))
  expect_equal(cmp$downsizing_savings, c(800 + 700, 0))
  expect_equal(cmp$n_headcount_end, c(2L, 2L))
  expect_equal(cmp$wage_bill_end, c(1900, 1900))
  expect_equal(cmp$n_hires, c(0L, 0L))
  expect_equal(cmp$hiring_effect, c(0, 0))
  year1 <- run$contract_dt[ref_date == start & personnel_id %in% c("P3", "P4")]
  expect_true(all(year1$contract_type == "inactive"))
  expect_true(all(year1$end_date == start))
})

test_that("the wage bill decomposition adds up with downsizing and growth", {
  d <- downsize_fixture()
  cmp <- simulate_horizon(d$c, d$p, d$s, n_periods = 2L, baseline_date = start, seed = 1L,
                          retirement_policy = NULL, hiring_policy = cut_to(3L),
                          salary_growth_rate = 0.05)$comparison
  expect_equal(cmp$wage_bill_end,
               cmp$wage_bill_start - cmp$exit_savings - cmp$non_ret_exit_savings +
                 cmp$promotion_effect + cmp$transfer_effect + cmp$hiring_effect -
                 cmp$downsizing_savings + cmp$inflation_effect)
  expect_equal(cmp$downsizing_savings_pct_of_end_bill,
               cmp$downsizing_savings / cmp$wage_bill_end)
})

test_that("simulate_hiring reports downsizing in its summary, with nothing to do and without", {
  d <- downsize_fixture()
  policy <- list(mode = "stock", group_cols = "est_id", salary_scale = d$s,
                 stock_targets = data.table(est_id = "E1", target_stock = 3L))
  cut <- simulate_hiring(d$c, d$p, policy, as_of_date = start)
  expect_equal(cut$summary$n_downsized, 1L)
  expect_equal(cut$summary$downsizing_savings, 700)
  expect_equal(cut$summary$total_headcount, 3L)

  policy$stock_targets[, target_stock := 4L]
  nothing <- simulate_hiring(d$c, d$p, policy, as_of_date = start)
  expect_equal(nothing$summary$n_downsized, 0L)
  expect_equal(nothing$summary$downsizing_savings, 0)
})

test_that("downsizing leaves a person's pension row alone", {
  d <- downsize_fixture()
  pension_row <- copy(d$c[personnel_id == "P4"])[, `:=`(contract_id = "C5", contract_type = "pensioner",
                                                        gross_salary_lcu = 300)]
  d$c <- rbind(d$c, pension_row)
  policy <- list(mode = "stock", group_cols = "est_id", salary_scale = d$s,
                 stock_targets = data.table(est_id = "E1", target_stock = 3L))
  cut <- simulate_hiring(d$c, d$p, policy, as_of_date = start)
  expect_equal(cut$contract_dt[contract_id == "C4", contract_type], "inactive")
  expect_equal(cut$contract_dt[contract_id == "C5", contract_type], "pensioner")
  expect_equal(cut$summary$downsizing_savings, 700)  # the working contract only
})

# -- Simulated retirees ------------------------------------------------------

test_that("simulated retirees get the status pensioner and stop ageing", {
  d <- ended_fixture()
  d$p[, age := c(70, 30)]
  run <- simulate_horizon(d$c, d$p, d$s, n_periods = 1L, baseline_date = start, seed = 1L,
                          retirement_policy = design_retirement_policy_age_tenure(
                            min_age = 60, min_tenure = 1, pension = pension_flat(100)),
                          output_control_args = define_output_control(return_microdata = TRUE))
  expect_equal(run$personnel_dt[personnel_id == "ended", employment_status], "pensioner")
  expect_equal(run$personnel_dt[personnel_id == "ended", age], 70)
  expect_equal(run$personnel_dt[personnel_id == "open", employment_status], "active")
})

test_that("simulate_retirement does not retire someone only because they hold a pension row", {
  d <- ended_fixture()
  d$c[, end_date := as.Date(NA)]  # only simulate_horizon() renews contracts
  d$c <- rbind(d$c, data.table(contract_id = "C3", personnel_id = "open", est_id = "E1",
                               start_date = as.Date("1990-01-01"), end_date = as.Date(NA),
                               contract_type = "pensioner", gross_salary_lcu = 200))
  d$p[, age := c(70, 30)]
  d$p[, personnel_tenure := c(30, 10)]
  res <- simulate_retirement(d$c, d$p, as_of_date = start,
                             policy_params = design_retirement_policy_age_tenure(
                               min_age = 60, min_tenure = 1, pension = pension_flat(100)))
  expect_equal(res$personnel_dt[personnel_id == "ended", employment_status], "pensioner")
  expect_equal(res$personnel_dt[personnel_id == "open", employment_status], "active")
})

# -- Allowances --------------------------------------------------------------

test_that("allowances grow in the same year as pay", {
  d <- downsize_fixture()
  allowance <- design_allowance_policy_adjustment(data.table(allowance_type = "a", amount = 10))
  cmp <- simulate_horizon(d$c, d$p, d$s, n_periods = 3L, baseline_date = start, seed = 1L,
                          retirement_policy = NULL, allowance_policy = allowance,
                          salary_growth_rate = 0.10, allowance_growth_rate = 0.10)$comparison
  expect_equal(cmp$allowance_cost_start[1], 40)
  expect_equal(cmp$allowance_cost_end, 40 * 1.10^(1:3))
  expect_equal(cmp$allowance_cost_end / cmp$allowance_cost_end[1],
               cmp$wage_bill_end / cmp$wage_bill_end[1])
})

test_that("the amount for unmatched groups grows too", {
  d <- downsize_fixture()
  allowance <- design_allowance_policy_adjustment(
    data.table(est_id = "E9", allowance_type = "a", amount = 10),
    group_cols = "est_id", unmatched_amount = 5)
  cmp <- simulate_horizon(d$c, d$p, d$s, n_periods = 2L, baseline_date = start, seed = 1L,
                          retirement_policy = NULL, allowance_policy = allowance,
                          allowance_growth_rate = 0.10)$comparison
  expect_equal(cmp$allowance_cost_end, 4 * 5 * 1.10^(1:2))
})
