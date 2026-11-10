# One pay column, with no guessing (WP3 stage 3: B-11, B-31, B-32)
#
# The pay table's pay column has the same name as salary_col. New hires and
# movers are paid in that column, and pensions are always calculated from it.

library(data.table)

sim_start <- as.Date("2012-09-01")
history_contracts <- function() bra_hrmis_contract[ref_date <= sim_start]
history_personnel <- function() bra_hrmis_personnel[ref_date <= sim_start]

# Mean pay per establishment at the start, in the given column
pay_table <- function(col) {
  scale <- bra_hrmis_contract[ref_date == sim_start & !is.na(get(col)), .(pay = mean(get(col))), by = est_id]
  setnames(scale, "pay", col)
}

base_pay_run <- function(hiring_policy, n_periods = 3L, retirement_policy = NULL) {
  suppressMessages(simulate_horizon(
    history_contracts(), history_personnel(), pay_table("base_salary_lcu"), n_periods = n_periods,
    baseline_date = sim_start, retirement_policy = retirement_policy, hiring_policy = hiring_policy,
    salary_growth_rate = 0.05, column_spec_args = define_column_spec(salary_col = "base_salary_lcu"),
    output_control_args = define_output_control(return_microdata = TRUE), seed = 1L
  ))
}
new_contracts <- function(run) run$contract_dt[!contract_id %in% bra_hrmis_contract$contract_id]


# ---- New hires are paid in salary_col (B-11) ----------------------------------

test_that("new hires are paid in salary_col when hiring derives its own pay table", {
  run <- base_pay_run(design_hiring_policy_status_quo(group_cols = "est_id"))
  expect_true(all(run$comparison$hiring_effect > 0))

  hires <- new_contracts(run)
  expect_gt(nrow(hires), 0L)
  expect_false(anyNA(hires$base_salary_lcu))
  # Nothing is written to a column the run does not use as pay
  expect_true(all(is.na(hires$gross_salary_lcu)))
})

test_that("the derived pay table is the mean of the group's pay in the pay table", {
  run <- base_pay_run(design_hiring_policy_status_quo(group_cols = "est_id"), n_periods = 1L)
  hires <- new_contracts(run)
  # This year's pay table, grown once by the 5% salary growth at the end of the year
  expected <- pay_table("base_salary_lcu")[est_id %in% hires$est_id][order(est_id)]
  paid <- unique(hires[, .(est_id, base_salary_lcu)])[order(est_id)]
  expect_equal(paid$base_salary_lcu, expected$base_salary_lcu * 1.05)
})

test_that("the wage bill follows: hires add their pay to it", {
  run <- base_pay_run(design_hiring_policy_status_quo(group_cols = "est_id"))
  cmp <- run$comparison
  expect_equal(cmp$wage_bill_end,
               (cmp$wage_bill_start + cmp$hiring_effect) * 1.05, tolerance = 1e-8)
})


# ---- No guessing: the pay table's column is salary_col (B-31) ------------------

# A pay table with a tempting column first, and both kinds of pay
tempting_pay_table <- function() {
  bra_hrmis_contract[ref_date == sim_start & !is.na(base_salary_lcu),
                     .(allowance_lcu = 1, wage_note = 2, base_salary_lcu = mean(base_salary_lcu)),
                     by = est_id][, gross_salary_lcu := base_salary_lcu + 1000][]
}

test_that("movers are paid from salary_col in a pay table holding several pay columns", {
  hc <- bra_hrmis_contract[ref_date == sim_start]
  hp <- bra_hrmis_personnel[ref_date == sim_start]
  table <- tempting_pay_table()[order(-base_salary_lcu)]
  to_group <- table$est_id[2]
  policy <- list(
    group_cols = "est_id", salary_scale = table,
    policy_table = data.table(from_group = table$est_id[1], to_group = to_group, movement_rate = 0.5),
    defaults = list(movement_strategy = "random", salary_update_rule = "scale", active_types = "permanent")
  )
  set.seed(1L)
  run <- suppressMessages(simulate_promotions_transfers(
    hc, hp, table, policy_params = policy, as_of_date = sim_start,
    column_spec_args = define_column_spec(salary_col = "base_salary_lcu")
  ))
  ids <- run$movers_dt$personnel_id
  expect_gt(length(ids), 0L)
  paid <- unique(run$contract_dt[personnel_id %in% ids, base_salary_lcu])
  expect_equal(paid, table[est_id == to_group, base_salary_lcu])
})

test_that("new hires are paid from salary_col in a pay table holding several pay columns", {
  hc <- bra_hrmis_contract[ref_date == sim_start]
  hp <- bra_hrmis_personnel[ref_date == sim_start]
  table <- tempting_pay_table()
  group <- table$est_id[1]
  stock <- compute_current_stock(hc, hp, sim_start, group_cols = "est_id")[est_id == group]
  run <- simulate_hiring(
    hc, hp, as_of_date = sim_start, column_spec_args = define_column_spec(salary_col = "base_salary_lcu"),
    policy_params = list(mode = "stock", group_cols = "est_id", salary_scale = table,
                         stock_targets = stock[, .(est_id, target_stock = current_stock + 5L)])
  )
  hires <- run$new_hire_contracts_dt
  expect_equal(nrow(hires), 5L)
  expect_equal(unique(hires$base_salary_lcu), table[est_id == group, base_salary_lcu])
  expect_true(all(is.na(hires$gross_salary_lcu)))
})

test_that("a pay table without a column named salary_col stops with a clear message", {
  hc <- bra_hrmis_contract[ref_date == sim_start]
  hp <- bra_hrmis_personnel[ref_date == sim_start]
  gross_table <- pay_table("gross_salary_lcu")
  spec <- define_column_spec(salary_col = "base_salary_lcu")
  message <- "must have a column named 'base_salary_lcu'"

  expect_error(simulate_horizon(hc, hp, gross_table, n_periods = 1L, baseline_date = sim_start,
                                retirement_policy = NULL, column_spec_args = spec), message)
  expect_error(simulate_scenario(hc, hp, gross_table, period_date = sim_start,
                                 column_spec_args = spec), message)
  expect_error(simulate_hiring(hc, hp, as_of_date = sim_start, column_spec_args = spec,
                               policy_params = list(mode = "stock", group_cols = "est_id", salary_scale = gross_table,
                                                    stock_targets = gross_table[, .(est_id, target_stock = 1L)])),
               message)
  expect_error(check_movement_inputs(
    hc, hp, gross_table, as_of_date = sim_start, column_spec_args = spec,
    policy_params = list(group_cols = "est_id", policy_table = data.table(from_group = "a", to_group = "b", movement_rate = 0.1),
                         defaults = list(movement_strategy = "random"))
  ), message)
})

test_that("a pay column that is not numeric stops with a clear message", {
  bad <- data.table(est_id = "E1", gross_salary_lcu = "1000")
  expect_error(.check_pay_column(bad, "gross_salary_lcu", "salary_scale"), "must be numeric")
  expect_no_error(.check_pay_column(data.table(est_id = "E1", gross_salary_lcu = 1000),
                                    "gross_salary_lcu", "salary_scale"))
})

test_that("package code never guesses the pay column from its name", {
  strings_in <- function(e) {
    if (is.character(e)) return(e)
    if (!is.call(e)) return(character(0))
    unlist(lapply(as.list(e), function(a) if (missing(a)) NULL else strings_in(a)))
  }
  ns <- asNamespace("govhrcast")
  guessing <- Filter(function(n) {
    f <- get(n, ns)
    is.function(f) && !is.primitive(f) && any(grepl("salary|wage", strings_in(body(f)), fixed = TRUE))
  }, ls(ns, all.names = TRUE))
  expect_identical(guessing, character(0))
})


# ---- Hiring with no groups (B-32) ----------------------------------------------

three_people <- function() {
  list(
    ct = data.table(contract_id = paste0("C", 1:3), personnel_id = paste0("P", 1:3), est_id = "E1",
                    start_date = as.Date("2010-01-01"), end_date = as.Date(NA),
                    contract_type = "permanent", base_salary_lcu = 100),
    pt = data.table(personnel_id = paste0("P", 1:3), employment_status = "active")
  )
}

test_that("hiring with no groups pays new hires from a one-row pay table, in salary_col", {
  d <- three_people()
  run <- simulate_hiring(
    d$ct, d$pt, as_of_date = as.Date("2020-01-01"), column_spec_args = define_column_spec(salary_col = "base_salary_lcu"),
    policy_params = list(mode = "stock", group_cols = NULL, stock_targets = data.table(target_stock = 5L),
                         salary_scale = data.table(base_salary_lcu = 250))
  )
  expect_equal(run$new_hire_contracts_dt$base_salary_lcu, c(250, 250))
  expect_equal(run$summary$total_new_salary_cost, 500)
})

test_that("hiring with no groups stops clearly on a pay table with several rows", {
  d <- three_people()
  expect_error(
    simulate_hiring(
      d$ct, d$pt, as_of_date = as.Date("2020-01-01"), column_spec_args = define_column_spec(salary_col = "base_salary_lcu"),
      policy_params = list(mode = "stock", group_cols = NULL, stock_targets = data.table(target_stock = 5L),
                           salary_scale = data.table(paygrade = c("A", "B"), base_salary_lcu = c(100, 200)))
    ),
    "several rows|No common columns"
  )
})

test_that("simulate_horizon hires with no groups, at the mean of the pay table", {
  run <- base_pay_run(design_hiring_policy_status_quo(group_cols = NULL), n_periods = 2L)
  expect_true(all(run$comparison$n_hires > 0))
  expect_true(all(run$comparison$hiring_effect > 0))
  # Returned microdata stacks the years; this is the first year's snapshot
  first_year <- new_contracts(run)[ref_date == sim_start]
  expect_equal(unique(first_year$base_salary_lcu),
               mean(pay_table("base_salary_lcu")$base_salary_lcu) * 1.05)
})


# ---- Pensions are always calculated from salary_col ------------------------------

one_person <- function() {
  s <- as.Date("2010-01-01")
  list(
    ct = data.table(contract_id = "C1", personnel_id = "P1", est_id = "E1", start_date = s, end_date = as.Date(NA),
                    contract_type = "permanent", base_salary_lcu = 500, gross_salary_lcu = 1000, net_salary_lcu = 400),
    pt = data.table(personnel_id = "P1", employment_status = "active", age = 59, personnel_tenure = 10,
                    birth_date = as.Date("1960-01-01")),
    scale = data.table(est_id = "E1", base_salary_lcu = 500, gross_salary_lcu = 1000)
  )
}

# The person turns 60 and retires in year 2
two_years <- function(growth, pay_col = "base_salary_lcu") {
  d <- one_person()
  policy <- design_retirement_policy_age(min_age = 60, pension = pension_rate(0.1))
  spec <- define_column_spec(salary_col = pay_col)
  year1 <- simulate_scenario(d$ct, d$pt, d$scale, period_date = as.Date("2020-01-01"), retirement_policy = policy,
                             salary_growth_rate = growth, column_spec_args = spec)
  year2 <- simulate_scenario(year1$contract_dt, year1$personnel_dt, year1$salary_scale_dt,
                             period_date = as.Date("2021-01-01"), retirement_policy = policy,
                             salary_growth_rate = 0, column_spec_args = spec)
  list(year1 = year1, pension = year2$pensioner_register$pension_amount)
}

test_that("a pension is calculated from salary_col, whichever column that is", {
  expect_equal(two_years(0, "base_salary_lcu")$pension, 50)
  expect_equal(two_years(0, "gross_salary_lcu")$pension, 100)
})

test_that("a pension is based on the wage at retirement, not the start-year wage", {
  expect_equal(two_years(0.10)$pension, 55)
})

test_that("salary growth grows salary_col, and other columns stay as they were", {
  ct <- two_years(0.10)$year1$contract_dt
  expect_equal(ct$base_salary_lcu, 550)
  expect_equal(ct$gross_salary_lcu, 1000)
  expect_equal(ct$net_salary_lcu, 400)
})

test_that("a hand-written policy still naming ref_wage_col stops with a clear message", {
  d <- one_person()
  message <- "ref_wage_col is no longer used"
  in_defaults <- list(group_cols = NULL, policy_table = NULL,
                      defaults = list(eligibility_type = "age_only", min_age = 60, pension_type = "rate",
                                      pension_rate = 0.1, ref_wage_col = "gross_salary_lcu"))
  in_table <- list(group_cols = "est_id",
                   policy_table = data.table(est_id = "E1", ref_wage_col = "gross_salary_lcu"),
                   defaults = list(eligibility_type = "age_only", min_age = 60, pension_type = "rate",
                                   pension_rate = 0.1))
  expect_error(simulate_retirement(d$ct, d$pt, policy_params = in_defaults, as_of_date = as.Date("2020-01-01")), message)
  expect_error(simulate_retirement(d$ct, d$pt, policy_params = in_table, as_of_date = as.Date("2020-01-01")), message)
})

default_retirement_run <- function(contract, pay_col, table) {
  suppressMessages(simulate_horizon(
    contract, history_personnel(), table, n_periods = 2L, baseline_date = sim_start,
    column_spec_args = define_column_spec(salary_col = pay_col), seed = 1L
  ))
}

test_that("the default retirement policy calculates pensions from salary_col", {
  # 2% a year, at most 80% of pay: pensions on gross would break that ceiling on base pay
  reg <- default_retirement_run(history_contracts(), "base_salary_lcu", pay_table("base_salary_lcu"))$pensioner_register
  reg <- reg[!is.na(tenure_years_at_retirement)]
  expect_gt(nrow(reg), 0L)
  expect_true(all(reg$pension_amount <= 0.8 * reg$final_salary + 1e-8))
})

test_that("the default retirement policy runs on a renamed pay column", {
  hc <- setnames(copy(history_contracts()), "gross_salary_lcu", "bruto")
  table <- setnames(pay_table("gross_salary_lcu"), "gross_salary_lcu", "bruto")
  reg <- default_retirement_run(hc, "bruto", table)$pensioner_register
  reg <- reg[!is.na(tenure_years_at_retirement)]
  expect_gt(nrow(reg), 0L)
  expect_true(all(reg$pension_amount <= 0.8 * reg$final_salary + 1e-8))
})


# ---- The pay column is checked wherever a pay table is read ----------------------

test_that("the derived pay table reads salary_col, not the table's first number", {
  table <- pay_table("base_salary_lcu")
  numbered <- copy(table)[, headcount := 1]
  setcolorder(numbered, c("est_id", "headcount"))
  run <- function(scale) suppressMessages(simulate_horizon(
    history_contracts(), history_personnel(), scale, n_periods = 1L, baseline_date = sim_start,
    retirement_policy = NULL, hiring_policy = design_hiring_policy_status_quo(group_cols = "est_id"),
    column_spec_args = define_column_spec(salary_col = "base_salary_lcu"), seed = 1L
  ))
  expect_equal(run(numbered)$comparison$hiring_effect, run(table)$comparison$hiring_effect)
})

test_that("assign_compensation() and update_state_with_movement() check the pay column themselves", {
  no_pay <- data.table(est_id = "E1", other = 1)
  expect_error(
    assign_compensation(data.table(contract_id = "C1", est_id = "E1"), no_pay, join_cols = "est_id",
                        salary_col = "base_salary_lcu"),
    "must have a column named 'base_salary_lcu'"
  )
  d <- one_person()
  movers <- data.table(personnel_id = "P1", from_group = "E1", to_group = "E2")
  expect_error(
    update_state_with_movement(d$ct, d$pt, movers, list(group_cols = "est_id"), salary_scale_dt = no_pay,
                               as_of_date = as.Date("2020-01-01"),
                               column_spec_args = define_column_spec(salary_col = "base_salary_lcu")),
    "must have a column named 'base_salary_lcu'"
  )
})


# ---- A pay table passed to hiring grows with salary growth (B-12) -----------------

# Ten people in one group, hiring half their number each year
hires_each_year <- function(table, growth) {
  people <- data.table(contract_id = paste0("C", 1:10), personnel_id = paste0("P", 1:10), est_id = "E1",
                       start_date = as.Date("2010-01-01"), end_date = as.Date(NA),
                       contract_type = "permanent", base_salary_lcu = 100)
  staff <- data.table(personnel_id = paste0("P", 1:10), employment_status = "active")
  suppressMessages(simulate_horizon(
    people, staff, data.table(est_id = "E1", base_salary_lcu = 100), n_periods = length(growth),
    baseline_date = as.Date("2020-01-01"), retirement_policy = NULL,
    hiring_policy = design_hiring_policy_adjustment(
      data.table(est_id = "E1", hiring_rate = 0.5), group_cols = "est_id", salary_scale = table
    ),
    salary_growth_rate = growth, column_spec_args = define_column_spec(salary_col = "base_salary_lcu"), seed = 1L
  ))
}

test_that("a passed pay table gives the same results as the same table built from salary_scale_dt", {
  table <- pay_table("base_salary_lcu")
  built  <- base_pay_run(design_hiring_policy_status_quo(group_cols = "est_id"))
  passed <- base_pay_run(design_hiring_policy_status_quo(group_cols = "est_id", salary_scale = table))
  expect_gt(sum(built$comparison$n_hires), 0L)
  expect_equal(passed$comparison, built$comparison)
})

test_that("each year's hires start at the pay table grown by the years before them", {
  cmp <- hires_each_year(data.table(est_id = "E1", base_salary_lcu = 250), growth = c(0.05, 0.10, 0.02))$comparison
  expect_true(all(cmp$n_hires > 0L))
  expect_equal(cmp$hiring_effect / cmp$n_hires, c(250, 250 * 1.05, 250 * 1.05 * 1.10))
})

test_that("with no salary growth a passed pay table stays as it is", {
  cmp <- hires_each_year(data.table(est_id = "E1", base_salary_lcu = 250), growth = c(0, 0, 0))$comparison
  expect_equal(cmp$hiring_effect / cmp$n_hires, c(250, 250, 250))
})

test_that("the table you pass in is never changed, and $metadata records it as supplied", {
  table <- data.table(est_id = "E1", base_salary_lcu = 250)
  run <- hires_each_year(table, growth = c(0.05, 0.05))
  expect_equal(table$base_salary_lcu, 250)
  expect_equal(run$metadata$policy_args$hiring_policy$salary_scale$base_salary_lcu, 250)
})

test_that("simulate_scenario() returns the grown table to pass back in next year", {
  d <- one_person()
  table <- data.table(est_id = "E1", base_salary_lcu = 250, other = 7)
  policy <- design_hiring_policy_adjustment(data.table(est_id = "E1", hiring_rate = 1), group_cols = "est_id",
                                            salary_scale = table)
  year1 <- simulate_scenario(d$ct, d$pt, d$scale, period_date = as.Date("2020-01-01"), retirement_policy = NULL,
                             hiring_policy = policy, salary_growth_rate = 0.10,
                             column_spec_args = define_column_spec(salary_col = "base_salary_lcu"), seed = 1L)
  expect_equal(year1$hiring_policy$salary_scale$base_salary_lcu, 275)
  # Only the pay column grows
  expect_equal(year1$hiring_policy$salary_scale$other, 7)
  expect_equal(table$base_salary_lcu, 250)
})

test_that("a pay table built from salary_scale_dt is not kept for the next year", {
  d <- one_person()
  policy <- design_hiring_policy_adjustment(data.table(est_id = "E1", hiring_rate = 1), group_cols = "est_id")
  year1 <- simulate_scenario(d$ct, d$pt, d$scale, period_date = as.Date("2020-01-01"), retirement_policy = NULL,
                             hiring_policy = policy, salary_growth_rate = 0.10,
                             column_spec_args = define_column_spec(salary_col = "base_salary_lcu"), seed = 1L)
  expect_gt(year1$summary$n_hires, 0L)
  expect_null(year1$hiring_policy$salary_scale)
})
