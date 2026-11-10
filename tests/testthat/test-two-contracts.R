# People with two contracts on the payroll
#
# The rules, from the user's decisions:
#   * A retiree leaves both jobs: the wage bill falls by both, the savings say so,
#     and the pension wage is both contracts added up.
#   * A mover moves on their main contract only. A second job stays as it is.

library(data.table)

start <- as.Date("2020-01-01")

# P1 holds two jobs (1000 is the main one: it started last); P2 holds one.
two_jobs <- function(p1_age = 70) {
  list(
    c = data.table(
      contract_id = c("C1", "C2", "C3"),
      personnel_id = c("P1", "P1", "P2"),
      paygrade = c("A", "A", "A"),
      start_date = as.Date(c("2005-01-01", "2000-01-01", "2005-01-01")),
      end_date = as.Date(NA),
      contract_type = "permanent",
      gross_salary_lcu = c(1000, 500, 800)
    ),
    p = data.table(
      personnel_id = c("P1", "P2"),
      employment_status = "active",
      birth_date = start - round(365.25 * c(p1_age, 30)),
      age = c(p1_age, 30),
      personnel_tenure = c(20, 5)
    ),
    s = data.table(paygrade = c("A", "B"), gross_salary_lcu = c(900, 2200))
  )
}

retire_at_60 <- function(pension = pension_rate(0.5)) {
  design_retirement_policy_age(min_age = 60, pension = pension)
}

everyone_moves <- function(rule = "scale") {
  list(group_cols = "paygrade",
       policy_table = data.table(from_group = "A", to_group = "B", movement_rate = 1),
       defaults = list(movement_strategy = "random", salary_update_rule = rule))
}

# -- Retirees ----------------------------------------------------------------

test_that("a retiree with two contracts: savings equal what leaves the wage bill", {
  d <- two_jobs()
  cmp <- simulate_horizon(d$c, d$p, d$s, n_periods = 1L, baseline_date = start,
                          retirement_policy = retire_at_60(), seed = 1L)$comparison
  expect_equal(cmp$wage_bill_start, 2300)
  expect_equal(cmp$exit_savings, 1500)
  expect_equal(cmp$wage_bill_end, 800)
  expect_equal(cmp$n_exits, 1L)
})

test_that("the pension wage of a retiree with two contracts is both added up", {
  d <- two_jobs()
  out <- simulate_retirement(d$c, d$p, start, retire_at_60())
  expect_equal(nrow(out$retirees_dt), 1L)
  expect_equal(out$retirees_dt$gross_salary_lcu, 1500)
  expect_equal(out$retirees_dt$pension, 750)
})

test_that("the pension counts the pay on both contracts, and sits on the main one", {
  d <- two_jobs()
  run <- simulate_horizon(d$c, d$p, d$s, n_periods = 1L, baseline_date = start,
                          retirement_policy = retire_at_60(), seed = 1L,
                          output_control_args = define_output_control(return_microdata = TRUE))
  reg <- run$pensioner_register
  expect_equal(nrow(reg), 1L)
  expect_equal(reg$pension_amount, 750)   # half of 1000 + 500
  # In the returned tables the main contract carries the pension; the other ended
  later <- run$contract_dt[ref_date > start & personnel_id == reg$personnel_id]
  expect_equal(later$contract_id, reg$contract_id)
  expect_equal(later$gross_salary_lcu, 750)
  expect_equal(later$contract_type, "pensioner")
})

test_that("a retiree with no pay on any contract keeps a blank pension wage", {
  d <- two_jobs()
  d$c[personnel_id == "P1", gross_salary_lcu := NA_real_]
  out <- simulate_retirement(d$c, d$p, start, retire_at_60())
  expect_true(is.na(out$retirees_dt$gross_salary_lcu))
})

test_that("a retiree's other columns still come from the main contract", {
  d <- two_jobs()
  d$c[contract_id == "C1", paygrade := "main"]
  d$c[contract_id == "C2", paygrade := "second"]
  out <- simulate_retirement(d$c, d$p, start, retire_at_60())
  expect_equal(out$retirees_dt$paygrade, "main")
  expect_equal(out$retirees_dt$contract_id, "C1")
})

# -- Movers ------------------------------------------------------------------

mover_run <- function(d, policy = everyone_moves()) {
  simulate_promotions_transfers(d$c, d$p, d$s, policy_params = policy, as_of_date = start)
}

test_that("a mover with two contracts moves on the main one only", {
  out <- mover_run(two_jobs())
  ct <- out$contract_dt
  expect_equal(ct[contract_id == "C1", paygrade], "B")
  expect_equal(ct[contract_id == "C1", gross_salary_lcu], 2200)
  expect_equal(ct[contract_id == "C2", paygrade], "A")
  expect_equal(ct[contract_id == "C2", gross_salary_lcu], 500)
})

test_that("the cost of moving a person with two contracts is the main contract's change", {
  d <- two_jobs()
  cmp <- simulate_horizon(d$c, d$p, d$s, n_periods = 1L, baseline_date = start,
                          retirement_policy = NULL, movement_policy = everyone_moves(),
                          seed = 1L)$comparison
  # P1: 1000 to 2200. P2: 800 to 2200.
  expect_equal(cmp$promotion_effect, (2200 - 1000) + (2200 - 800))
  expect_equal(cmp$wage_bill_end, 2200 + 500 + 2200)
})

test_that("salary rule keep: the main contract changes group and no pay changes", {
  ct <- mover_run(two_jobs(), everyone_moves("keep"))$contract_dt
  expect_equal(ct[contract_id == "C1", .(paygrade, gross_salary_lcu)],
               data.table(paygrade = "B", gross_salary_lcu = 1000))
  expect_equal(ct[contract_id == "C2", .(paygrade, gross_salary_lcu)],
               data.table(paygrade = "A", gross_salary_lcu = 500))
})

test_that("a mover's closed contracts and pension rows are left alone", {
  d <- two_jobs()
  extra <- d$c[c(2L, 2L)][, `:=`(contract_id = c("C4", "C5"),
                                 contract_type = c("inactive", "pensioner"))]
  d$c <- rbind(d$c, extra)
  ct <- mover_run(d)$contract_dt
  expect_equal(ct[contract_id %in% c("C4", "C5"), unique(paygrade)], "A")
  expect_equal(ct[contract_id %in% c("C4", "C5"), unique(gross_salary_lcu)], 500)
})

test_that("group columns keep their type when someone moves", {
  d <- two_jobs()
  d$c[, paygrade := c(1L, 1L, 1L)]
  d$s <- data.table(paygrade = c(1L, 2L), gross_salary_lcu = c(900, 2200))
  policy <- everyone_moves()
  policy$policy_table <- data.table(from_group = "1", to_group = "2", movement_rate = 1)
  expect_no_warning(out <- mover_run(d, policy))
  expect_type(out$contract_dt$paygrade, "integer")
  expect_equal(out$contract_dt[contract_id == "C1", paygrade], 2L)
})

test_that("the decomposition holds on the bundled data, with two-contract staff", {
  sim_start <- as.Date("2012-09-01")
  hc <- bra_hrmis_contract[ref_date <= sim_start]
  hp <- bra_hrmis_personnel[ref_date <= sim_start]
  scale <- hc[ref_date == sim_start & !is.na(gross_salary_lcu),
              .(gross_salary_lcu = mean(gross_salary_lcu)), by = est_id]
  cmp <- suppressMessages(suppressWarnings(simulate_horizon(
    hc, hp, scale, n_periods = 5L, baseline_date = sim_start,
    retirement_policy = design_retirement_policy_status_quo(
      pension = pension_db(0.02, 35, 0.80), mandatory_age = 75
    ),
    exit_policy = design_exit_policy_status_quo(group_cols = "est_id"),
    movement_policy = design_movement_policy_status_quo(
      group_cols = "est_id", movement_type = "transfer", salary_update_rule = "keep"
    ),
    hiring_policy = design_hiring_policy_status_quo(group_cols = "est_id"),
    salary_growth_rate = 0.05, seed = 1L
  )))$comparison
  explained <- cmp[, wage_bill_start - exit_savings - non_ret_exit_savings +
                     promotion_effect + transfer_effect + hiring_effect -
                     downsizing_savings + inflation_effect]
  expect_equal(cmp$wage_bill_end, explained)
})
