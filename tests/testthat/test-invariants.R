# Invariants of a multi-period projection
#
# Rules that must hold for any run, whatever the numbers: they are checked on
# the bundled Alagoas panel, projected 3 years with all five modules on
# (status-quo retirement, exits, transfers, hiring and allowances). Each test
# states its rule in its name. Exact values are not asserted here -- the
# component tests elsewhere do that; these catch accounting leaks between
# periods and between modules.

library(data.table)

# A status-quo projection from 2012 on the bundled panel. `pension` and `seed`
# are the only levers the tests vary.
run_alagoas <- function(pension = pension_db(accrual_rate = 0.02,
                                             ref_wage_col = "base_salary_lcu",
                                             max_years = 35, replacement_cap = 0.80),
                        seed = 1L,
                        contract_dt = NULL, personnel_dt = NULL) {
  start <- as.Date("2012-09-01")
  hist_c <- contract_dt %||% bra_hrmis_contract[ref_date <= start]
  hist_p <- personnel_dt %||% bra_hrmis_personnel[ref_date <= start]
  # Pay table: mean base salary per establishment at the start; it is also
  # passed to hiring so new hires are paid in the same salary column.
  scale <- bra_hrmis_contract[ref_date == start & !is.na(base_salary_lcu),
                              .(base_salary_lcu = mean(base_salary_lcu)), by = est_id]
  suppressMessages(suppressWarnings(simulate_horizon(
    contract_dt       = hist_c,
    personnel_dt      = hist_p,
    salary_scale_dt   = scale,
    n_periods         = 3L,
    ref_date          = start,
    retirement_policy = design_retirement_policy_status_quo(pension = pension),
    exit_policy       = design_exit_policy_status_quo(group_cols = "est_id"),
    movement_policy   = design_movement_policy_status_quo(
      group_cols = "est_id", movement_type = "transfer", salary_update_rule = "keep"),
    hiring_policy     = design_hiring_policy_status_quo(group_cols = "est_id",
                                                        salary_scale = scale),
    allowance_policy  = design_allowance_policy_status_quo(
      bra_hrmis_allowance, bra_hrmis_contract[ref_date <= start],
      group_cols = "est_id", ref_date = start),
    salary_growth_rate = 0.05,
    column_spec_args   = list(salary_col = "base_salary_lcu"),
    seed               = seed
  )))
}

test_that("headcount flows: end = start - retirements - other exits + hires, every period", {
  cmp <- run_alagoas()$comparison
  expect_true(all(cmp$n_exits + cmp$n_non_ret_exits + cmp$n_hires > 0))  # flows happen
  expect_equal(cmp$n_headcount_end,
               cmp$n_headcount_start - cmp$n_exits - cmp$n_non_ret_exits + cmp$n_hires)
})

test_that("each period starts where the previous one ended (headcount and wage bill)", {
  cmp <- run_alagoas()$comparison
  n <- nrow(cmp)
  expect_equal(cmp$n_headcount_start[-1], cmp$n_headcount_end[-n])
  expect_equal(cmp$wage_bill_start[-1],   cmp$wage_bill_end[-n])
})

test_that("pension amounts never enter the wage bill", {
  # Same seed, same retirees; only the pension formula differs (0 vs 1 million
  # each). Pension costs must move, the wage bill must not.
  low  <- run_alagoas(pension = pension_flat(0))$comparison
  high <- run_alagoas(pension = pension_flat(1e6))$comparison
  expect_identical(low$n_exits, high$n_exits)
  expect_true(all(high$pension_cost_new > low$pension_cost_new))
  expect_identical(low$wage_bill_start, high$wage_bill_start)
  expect_identical(low$wage_bill_end,   high$wage_bill_end)
})

test_that("the same seed reproduces the same projection", {
  a <- run_alagoas(seed = 7L)
  b <- run_alagoas(seed = 7L)
  expect_identical(a$comparison, b$comparison)
  expect_identical(a$pensioner_register, b$pensioner_register)
})

test_that("simulate_horizon() leaves the caller's tables unchanged", {
  start <- as.Date("2012-09-01")
  hist_c <- bra_hrmis_contract[ref_date <= start]
  hist_p <- bra_hrmis_personnel[ref_date <= start]
  before_c <- copy(hist_c); before_p <- copy(hist_p)
  run_alagoas(contract_dt = hist_c, personnel_dt = hist_p)
  expect_identical(hist_c, before_c)
  expect_identical(hist_p, before_p)
})

# ---------------------------------------------------------------------------
# Exported functions never change the tables the caller passes in
# ---------------------------------------------------------------------------

small_state <- function() {
  list(
    c = data.table(contract_id = c("C1", "C2", "C3"), personnel_id = c("P1", "P2", "P3"),
                   est_id = c("E1", "E1", "E2"),
                   start_date = as.Date(c("2000-01-01", "2005-01-01", "2010-01-01")),
                   end_date = as.Date(NA), contract_type = "permanent",
                   gross_salary_lcu = c(1000, 2000, 1500)),
    p = data.table(personnel_id = c("P1", "P2", "P3"),
                   birth_date = as.Date(c("1960-01-01", "1970-01-01", "1980-01-01")),
                   employment_status = "active", age = c(65, 55, 45),
                   personnel_tenure = c(25, 20, 15)),
    s = data.table(est_id = c("E1", "E2"), gross_salary_lcu = c(1500, 1600))
  )
}

test_that("simulate_scenario() leaves the caller's tables unchanged", {
  # No modules on, so aging and COLA act on whatever tables the function holds
  d <- small_state()
  before <- lapply(d, copy)
  reg <- data.table(personnel_id = "X", pension_amount = 100, final_salary = 100,
                    tenure_years_at_retirement = 30, age_at_retirement = 62,
                    period_date = as.Date("2025-01-01"))
  reg_before <- copy(reg)
  simulate_scenario(d$c, d$p, d$s, period_date = as.Date("2025-01-01"),
                    pensioner_register = reg, salary_growth_rate = 0.10,
                    pension_cola_rate = 0.10)
  expect_identical(d$c, before$c)
  expect_identical(d$p, before$p)
  expect_identical(d$s, before$s)
  expect_identical(reg, reg_before)
})

test_that("make_status_quo_policies() leaves the caller's panel unchanged", {
  cc <- copy(bra_hrmis_contract); pp <- copy(bra_hrmis_personnel)
  cc_before <- copy(cc)
  scale <- cc[, .(gross_salary_lcu = mean(gross_salary_lcu, na.rm = TRUE)), by = est_id]
  suppressWarnings(make_status_quo_policies(cc, pp, scale, movement_group_cols = "est_id"))
  expect_identical(cc, cc_before)   # same rows, same order, no key added
})

# ---------------------------------------------------------------------------
# A seeded run never disturbs the caller's random-number stream
# ---------------------------------------------------------------------------

test_that("a seeded simulate_horizon() run leaves the global random-number state unchanged", {
  d <- small_state()
  set.seed(42); invisible(runif(1))
  state_before <- .Random.seed
  simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = as.Date("2025-01-01"),
                   retirement_policy = NULL, seed = 1L)
  expect_identical(.Random.seed, state_before)
})

test_that("a seeded simulate_scenario() run leaves the global random-number state unchanged", {
  d <- small_state()
  set.seed(42); invisible(runif(1))
  state_before <- .Random.seed
  simulate_scenario(d$c, d$p, d$s, period_date = as.Date("2025-01-01"), seed = 1L)
  expect_identical(.Random.seed, state_before)
})

test_that("a seeded run in a session with no random state leaves none behind", {
  # Start from a session that has never drawn a random number, and put the
  # test session's own state back afterwards whatever happens.
  saved <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    get(".Random.seed", envir = globalenv()) else NULL
  tryCatch({
    if (!is.null(saved)) rm(".Random.seed", envir = globalenv())
    d <- small_state()
    simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = as.Date("2025-01-01"),
                     retirement_policy = NULL, seed = 1L)
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  }, finally = {
    if (!is.null(saved)) assign(".Random.seed", saved, envir = globalenv())
  })
})

# ---------------------------------------------------------------------------
# The result's metadata carries no person-level data
# ---------------------------------------------------------------------------

# Paths of every table inside a nested list that has a personnel_id column.
person_level_tables <- function(x, path = "metadata") {
  if (is.data.frame(x)) return(if ("personnel_id" %in% names(x)) path else character(0))
  if (!is.list(x) || length(x) == 0L) return(character(0))
  nms <- names(x)
  if (is.null(nms)) nms <- as.character(seq_along(x))
  unlist(Map(person_level_tables, x, paste0(path, "$", nms)), use.names = FALSE)
}

test_that("the result's metadata contains no person-level tables", {
  res <- run_alagoas()
  expect_identical(person_level_tables(res$metadata), character(0))
})

test_that("status-quo hiring rates are the historical estimate from the input panel", {
  start <- as.Date("2012-09-01")
  res <- run_alagoas()
  fresh <- estimate_historical_hiring_rates(bra_hrmis_contract[ref_date <= start],
                                            bra_hrmis_personnel[ref_date <= start],
                                            group_cols = "est_id")
  expect_equal(res$metadata$policy_args$hiring_policy$squorate_dt, fresh)
})
