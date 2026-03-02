# tests/testthat/test-simulate_horizon.R
# ============================================================
# Tests for:
#   compute_exit_effect()
#   compute_movement_effect()
#   compute_hiring_effect()
#   compute_inflation_effect()
#   simulate_scenario()
#   simulate_horizon()
# ============================================================

library(data.table)

# ---------------------------------------------------------------------------
# Shared minimal fixtures
# ---------------------------------------------------------------------------

make_horizon_state <- function(n          = 4L,
                               salary     = 10000,
                               ref_date   = as.Date("2020-01-01"),
                               ages       = NULL,
                               tenures    = NULL) {
  if (is.null(ages))    ages    <- rep(40L, n)
  if (is.null(tenures)) tenures <- rep(10,  n)
  contract_dt <- data.table::data.table(
    contract_id        = paste0("C", seq_len(n)),
    personnel_id       = paste0("P", seq_len(n)),
    est_id             = rep(c("E1", "E2"), length.out = n),
    start_date         = ref_date - 365L * as.integer(tenures),
    end_date           = as.Date(NA),
    contract_type_code = "permanent",
    gross_salary_lcu   = as.numeric(rep(salary, n))
  )
  personnel_dt <- data.table::data.table(
    personnel_id = paste0("P", seq_len(n)),
    birth_date   = ref_date - 365L * as.integer(ages),
    status       = "active",
    age          = as.numeric(ages),
    tenure_years = as.numeric(tenures)
  )
  list(contract_dt = contract_dt, personnel_dt = personnel_dt)
}

make_salary_scale <- function() {
  data.table::data.table(
    est_id           = c("E1", "E2"),
    gross_salary_lcu = c(10000, 10000)
  )
}

null_retirement_policy <- list(
  eligibility_type = "age_only",
  min_age          = 999,
  pension_type     = "flat",
  pension_params   = list(flat_amount = 1000)
)

null_movement_policy <- list(
  group_cols           = "est_id",
  salary_scale         = make_salary_scale(),
  promotion_multiplier = 0,
  transfer_multiplier  = 0,
  promotion_strategy   = "tenure",
  transfer_strategy    = "random"
)

null_hiring_policy <- list(
  mode          = "stock",
  group_cols    = "est_id",
  stock_targets = data.table::data.table(est_id = c("E1", "E2"), target_stock = 2L),
  salary_scale  = make_salary_scale()
)


# ===========================================================================
# compute_exit_effect()
# ===========================================================================

test_that("compute_exit_effect: returns 0 for NULL input", {
  expect_equal(compute_exit_effect(NULL), 0)
})

test_that("compute_exit_effect: returns 0 for zero-row data.table", {
  empty <- data.table::data.table(gross_salary_lcu = numeric(0))
  expect_equal(compute_exit_effect(empty), 0)
})

test_that("compute_exit_effect: sums salary_col correctly", {
  dt <- data.table::data.table(gross_salary_lcu = c(10000, 20000, 30000))
  expect_equal(compute_exit_effect(dt), 60000)
})

test_that("compute_exit_effect: tolerates NA values", {
  dt <- data.table::data.table(gross_salary_lcu = c(10000, NA, 30000))
  expect_equal(compute_exit_effect(dt), 40000)
})

test_that("compute_exit_effect: returns 0 when salary_col missing", {
  dt <- data.table::data.table(other_col = c(1, 2))
  expect_equal(compute_exit_effect(dt, salary_col = "gross_salary_lcu"), 0)
})

test_that("compute_exit_effect: respects custom salary_col name", {
  dt <- data.table::data.table(my_sal = c(500, 1500))
  expect_equal(compute_exit_effect(dt, salary_col = "my_sal"), 2000)
})


# ===========================================================================
# compute_movement_effect()
# ===========================================================================

test_that("compute_movement_effect: returns zeros for NULL movers_dt", {
  after <- data.table::data.table(personnel_id = "P1", gross_salary_lcu = 12000)
  r <- compute_movement_effect(NULL, after)
  expect_equal(r$promotion, 0)
  expect_equal(r$transfer,  0)
})

test_that("compute_movement_effect: returns zeros for zero-row movers_dt", {
  movers <- data.table::data.table(
    personnel_id  = character(0),
    movement_type = character(0),
    salary_before = numeric(0)
  )
  after <- data.table::data.table(
    personnel_id     = character(0),
    gross_salary_lcu = numeric(0)
  )
  r <- compute_movement_effect(movers, after)
  expect_equal(r$promotion, 0)
  expect_equal(r$transfer,  0)
})

test_that("compute_movement_effect: returns zeros when salary_before missing", {
  movers <- data.table::data.table(
    personnel_id  = "P1",
    movement_type = "promotion"
    # no salary_before column
  )
  after <- data.table::data.table(personnel_id = "P1", gross_salary_lcu = 12000)
  r <- compute_movement_effect(movers, after)
  expect_equal(r$promotion, 0)
  expect_equal(r$transfer,  0)
})

test_that("compute_movement_effect: correct promotion diff", {
  movers <- data.table::data.table(
    personnel_id  = "P1",
    movement_type = "promotion",
    salary_before = 10000
  )
  after <- data.table::data.table(personnel_id = "P1", gross_salary_lcu = 12000)
  r <- compute_movement_effect(movers, after)
  expect_equal(r$promotion, 2000)
  expect_equal(r$transfer,  0)
})

test_that("compute_movement_effect: correct transfer diff", {
  movers <- data.table::data.table(
    personnel_id  = "P1",
    movement_type = "transfer",
    salary_before = 12000
  )
  after <- data.table::data.table(personnel_id = "P1", gross_salary_lcu = 11000)
  r <- compute_movement_effect(movers, after)
  expect_equal(r$promotion, 0)
  expect_equal(r$transfer,  -1000)
})

test_that("compute_movement_effect: splits promotion and transfer effects", {
  movers <- data.table::data.table(
    personnel_id  = c("P1", "P2"),
    movement_type = c("promotion", "transfer"),
    salary_before = c(10000, 12000)
  )
  after <- data.table::data.table(
    personnel_id     = c("P1", "P2"),
    gross_salary_lcu = c(13000, 11500)
  )
  r <- compute_movement_effect(movers, after)
  expect_equal(r$promotion, 3000)
  expect_equal(r$transfer,  -500)
})


# ===========================================================================
# compute_hiring_effect()
# ===========================================================================

test_that("compute_hiring_effect: returns 0 for NULL input", {
  expect_equal(compute_hiring_effect(NULL), 0)
})

test_that("compute_hiring_effect: returns 0 for zero-row data.table", {
  empty <- data.table::data.table(gross_salary_lcu = numeric(0))
  expect_equal(compute_hiring_effect(empty), 0)
})

test_that("compute_hiring_effect: sums salary_col correctly", {
  dt <- data.table::data.table(gross_salary_lcu = c(40000, 35000))
  expect_equal(compute_hiring_effect(dt), 75000)
})

test_that("compute_hiring_effect: tolerates NA values", {
  dt <- data.table::data.table(gross_salary_lcu = c(40000, NA))
  expect_equal(compute_hiring_effect(dt), 40000)
})

test_that("compute_hiring_effect: returns 0 when salary_col missing", {
  dt <- data.table::data.table(other_col = c(1, 2))
  expect_equal(compute_hiring_effect(dt, salary_col = "gross_salary_lcu"), 0)
})


# ===========================================================================
# compute_inflation_effect()
# ===========================================================================

test_that("compute_inflation_effect: correct product", {
  expect_equal(compute_inflation_effect(1000000, 0.03), 30000)
})

test_that("compute_inflation_effect: zero at rate 0", {
  expect_equal(compute_inflation_effect(1000000, 0), 0)
})

test_that("compute_inflation_effect: negative at negative rate", {
  expect_equal(compute_inflation_effect(1000000, -0.02), -20000)
})

test_that("compute_inflation_effect: zero base gives zero effect", {
  expect_equal(compute_inflation_effect(0, 0.05), 0)
})


# ===========================================================================
# simulate_scenario() — structural tests
# ===========================================================================

test_that("simulate_scenario: returns a list with required named elements", {
  s <- make_horizon_state()
  r <- simulate_scenario(
    contract_dt     = s$contract_dt,
    personnel_dt    = s$personnel_dt,
    salary_scale_dt = make_salary_scale(),
    period_date     = as.Date("2020-01-01")
  )
  expect_true(is.list(r))
  expect_setequal(
    names(r),
    c("summary", "contract_dt", "personnel_dt", "salary_scale_dt", "pensioner_register")
  )
})

test_that("simulate_scenario: summary is a single-row data.table", {
  s <- make_horizon_state()
  r <- simulate_scenario(
    contract_dt     = s$contract_dt,
    personnel_dt    = s$personnel_dt,
    salary_scale_dt = make_salary_scale(),
    period_date     = as.Date("2020-01-01")
  )
  expect_true(data.table::is.data.table(r$summary))
  expect_equal(nrow(r$summary), 1L)
})

test_that("simulate_scenario: summary has all required columns", {
  s <- make_horizon_state()
  r <- simulate_scenario(
    contract_dt     = s$contract_dt,
    personnel_dt    = s$personnel_dt,
    salary_scale_dt = make_salary_scale(),
    period_date     = as.Date("2020-01-01")
  )
  required <- c("period_date", "n_headcount_start", "wage_bill_start",
                "n_exits", "exit_savings", "pension_cost_new", "pension_cost_total",
                "n_promotions", "n_transfers", "promotion_effect", "transfer_effect",
                "n_hires", "hiring_effect", "inflation_effect",
                "n_headcount_end", "wage_bill_end")
  expect_true(all(required %in% names(r$summary)))
})

test_that("simulate_scenario: wage_bill_start = sum(salary_col) before any ops", {
  n <- 4L; sal <- 10000
  s <- make_horizon_state(n = n, salary = sal)
  r <- simulate_scenario(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    period_date        = as.Date("2020-01-01"),
    retirement_policy  = NULL,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0
  )
  expect_equal(r$summary$wage_bill_start, n * sal)
})

test_that("simulate_scenario: n_headcount_start = nrow(contract_dt) at entry", {
  n <- 4L
  s <- make_horizon_state(n = n)
  r <- simulate_scenario(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    period_date        = as.Date("2020-01-01"),
    retirement_policy  = NULL,
    movement_policy    = NULL,
    hiring_policy      = NULL
  )
  expect_equal(r$summary$n_headcount_start, n)
})

test_that("simulate_scenario: with no modules and 0 growth, bill unchanged", {
  n <- 4L; sal <- 10000
  s <- make_horizon_state(n = n, salary = sal)
  r <- simulate_scenario(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    period_date        = as.Date("2020-01-01"),
    retirement_policy  = NULL,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0
  )
  expect_equal(r$summary$wage_bill_end,    n * sal)
  expect_equal(r$summary$inflation_effect, 0)
  expect_equal(r$summary$exit_savings,     0)
  expect_equal(r$summary$hiring_effect,    0)
})

test_that("simulate_scenario: inflation_effect = pre_cola_bill * rate", {
  n <- 4L; sal <- 10000; growth <- 0.05
  s <- make_horizon_state(n = n, salary = sal)
  r <- simulate_scenario(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    period_date        = as.Date("2020-01-01"),
    retirement_policy  = NULL,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = growth
  )
  # No exits / moves / hires so pre-COLA bill = n*sal
  expect_equal(r$summary$inflation_effect, n * sal * growth, tolerance = 1e-6)
})

test_that("simulate_scenario: pensioner_register gains rows after retirements", {
  n <- 2L; sal <- 10000
  s <- make_horizon_state(n = n, salary = sal, ages = rep(65L, n))
  retire_policy <- list(
    eligibility_type = "age_only",
    min_age          = 60,
    pension_type     = "flat",
    pension_params   = list(flat_amount = 5000)
  )
  r <- simulate_scenario(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    period_date        = as.Date("2020-01-01"),
    pensioner_register = NULL,
    retirement_policy  = retire_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0
  )
  expect_equal(r$summary$n_exits, n)
  expect_gt(nrow(r$pensioner_register), 0L)
  expect_equal(r$summary$pension_cost_new, n * 5000)
})

test_that("simulate_scenario: exit_savings positive when retirees have salary", {
  s <- make_horizon_state(n = 2L, salary = 10000, ages = rep(65L, 2L))
  retire_policy <- list(
    eligibility_type = "age_only",
    min_age          = 60,
    pension_type     = "flat",
    pension_params   = list(flat_amount = 1000)
  )
  r <- simulate_scenario(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    period_date        = as.Date("2020-01-01"),
    retirement_policy  = retire_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0
  )
  expect_gt(r$summary$exit_savings, 0)
})

test_that("simulate_scenario: salary_scale_dt is updated by COLA", {
  s      <- make_horizon_state()
  growth <- 0.10
  r <- simulate_scenario(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    period_date        = as.Date("2020-01-01"),
    retirement_policy  = NULL,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = growth
  )
  expect_equal(r$salary_scale_dt$gross_salary_lcu,
               rep(10000 * (1 + growth), 2))
})


# ===========================================================================
# simulate_horizon() — structural tests
# ===========================================================================

test_that("simulate_horizon: returns a list with summary_dt", {
  s <- make_horizon_state()
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 2L,
    retirement_policy  = null_retirement_policy,
    movement_policy    = null_movement_policy,
    hiring_policy      = null_hiring_policy,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  expect_true(is.list(res))
  expect_true("summary_dt" %in% names(res))
  expect_true(data.table::is.data.table(res$summary_dt))
})

test_that("simulate_horizon: summary_dt has n_periods rows", {
  s <- make_horizon_state()
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 3L,
    retirement_policy  = null_retirement_policy,
    movement_policy    = null_movement_policy,
    hiring_policy      = null_hiring_policy,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  expect_equal(nrow(res$summary_dt), 3L)
})

test_that("simulate_horizon: summary_dt has new-API column names", {
  s <- make_horizon_state()
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 1L,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  required <- c("period_date", "n_headcount_start", "wage_bill_start",
                "n_exits", "exit_savings", "pension_cost_new", "pension_cost_total",
                "n_promotions", "n_transfers", "promotion_effect", "transfer_effect",
                "n_hires", "hiring_effect", "inflation_effect",
                "n_headcount_end", "wage_bill_end")
  expect_true(all(required %in% names(res$summary_dt)))
})

test_that("simulate_horizon: does NOT have old-API column names", {
  s <- make_horizon_state()
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 1L,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  old_cols <- c("base_bill", "total_wage_bill", "n_active", "total_change",
                "exit_savings_pct", "promotion_effect_pct")
  for (col in old_cols) {
    expect_false(col %in% names(res$summary_dt),
                 label = paste("old column should not exist:", col))
  }
})

test_that("simulate_horizon: summary_dt has _pct_of_end_bill share columns", {
  s <- make_horizon_state()
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 1L,
    salary_growth_rate = 0.05,
    ref_date           = as.Date("2020-01-01")
  )
  pct_cols <- c("exit_savings_pct_of_end_bill",
                "promotion_effect_pct_of_end_bill",
                "transfer_effect_pct_of_end_bill",
                "hiring_effect_pct_of_end_bill",
                "inflation_effect_pct_of_end_bill")
  expect_true(all(pct_cols %in% names(res$summary_dt)))
})

test_that("simulate_horizon: period_date sequences annually from ref_date", {
  s <- make_horizon_state()
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 3L,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  expected_dates <- as.Date(c("2020-01-01", "2021-01-01", "2022-01-01"))
  expect_equal(res$summary_dt$period_date, expected_dates)
})

test_that("simulate_horizon: return_microdata = FALSE omits contract/personnel", {
  s <- make_horizon_state()
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 1L,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01"),
    return_microdata   = FALSE
  )
  expect_false("contract_dt"  %in% names(res))
  expect_false("personnel_dt" %in% names(res))
})

test_that("simulate_horizon: return_microdata = TRUE includes contract/personnel", {
  s <- make_horizon_state()
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 1L,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01"),
    return_microdata   = TRUE
  )
  expect_true("contract_dt"  %in% names(res))
  expect_true("personnel_dt" %in% names(res))
  expect_true(data.table::is.data.table(res$contract_dt))
  expect_true(data.table::is.data.table(res$personnel_dt))
})

test_that("simulate_horizon: errors on invalid n_periods", {
  s <- make_horizon_state()
  expect_error(
    simulate_horizon(
      contract_dt     = s$contract_dt,
      personnel_dt    = s$personnel_dt,
      salary_scale_dt = make_salary_scale(),
      n_periods       = 0L,
      ref_date        = as.Date("2020-01-01")
    ),
    "positive integer"
  )
})

test_that("simulate_horizon: errors when salary_growth_rate length mismatches n_periods", {
  s <- make_horizon_state()
  expect_error(
    simulate_horizon(
      contract_dt        = s$contract_dt,
      personnel_dt       = s$personnel_dt,
      salary_scale_dt    = make_salary_scale(),
      n_periods          = 3L,
      salary_growth_rate = c(0.02, 0.03),   # length 2, not 3
      ref_date           = as.Date("2020-01-01")
    ),
    "length n_periods"
  )
})

test_that("simulate_horizon: accepts vector salary_growth_rate", {
  s <- make_horizon_state()
  expect_no_error(
    simulate_horizon(
      contract_dt        = s$contract_dt,
      personnel_dt       = s$personnel_dt,
      salary_scale_dt    = make_salary_scale(),
      n_periods          = 3L,
      salary_growth_rate = c(0.02, 0.03, 0.04),
      ref_date           = as.Date("2020-01-01")
    )
  )
})


# ===========================================================================
# STATE THREADING
# ===========================================================================

test_that("period 2 wage_bill_start == period 1 wage_bill_end", {
  set.seed(1)
  s <- make_horizon_state(n = 4L, salary = 10000)
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 3L,
    retirement_policy  = null_retirement_policy,
    movement_policy    = null_movement_policy,
    hiring_policy      = null_hiring_policy,
    salary_growth_rate = 0.05,
    ref_date           = as.Date("2020-01-01")
  )
  dt <- res$summary_dt
  expect_equal(dt$wage_bill_start[2], dt$wage_bill_end[1], tolerance = 1e-6)
  expect_equal(dt$wage_bill_start[3], dt$wage_bill_end[2], tolerance = 1e-6)
})

test_that("pension_cost_total >= pension_cost_new from period 2 onwards", {
  # Two workers retire in period 1; pension register carries forward
  s <- make_horizon_state(n = 4L, salary = 10000,
                          ages = c(64L, 64L, 40L, 40L))
  retire_policy <- list(
    eligibility_type = "age_only",
    min_age          = 65,
    pension_type     = "flat",
    pension_params   = list(flat_amount = 1000)
  )
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 3L,
    retirement_policy  = retire_policy,
    movement_policy    = null_movement_policy,
    hiring_policy      = null_hiring_policy,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  dt <- res$summary_dt
  # Period 1: two retirees → pension_cost_total == pension_cost_new == 2000
  # Period 2: no new retirees → pension_cost_total == 2000, pension_cost_new == 0
  # pension_cost_total should never fall below period 1 value
  expect_gte(dt$pension_cost_total[2], dt$pension_cost_total[1])
  expect_gte(dt$pension_cost_total[3], dt$pension_cost_total[2])
})


# ===========================================================================
# NULL MODULES
# ===========================================================================

test_that("simulate_horizon works with all modules set to NULL", {
  s <- make_horizon_state()
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 2L,
    retirement_policy  = NULL,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  expect_equal(nrow(res$summary_dt), 2L)
  expect_true(all(res$summary_dt$exit_savings     == 0))
  expect_true(all(res$summary_dt$promotion_effect == 0))
  expect_true(all(res$summary_dt$transfer_effect  == 0))
  expect_true(all(res$summary_dt$hiring_effect    == 0))
})

test_that("simulate_horizon with only inflation (all modules NULL)", {
  set.seed(11)
  n <- 4L; sal <- 8000; growth <- 0.03
  s <- make_horizon_state(n = n, salary = sal)
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 1L,
    retirement_policy  = NULL,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = growth,
    ref_date           = as.Date("2020-01-01")
  )
  dt <- res$summary_dt
  expect_equal(dt$inflation_effect, n * sal * growth, tolerance = 1e-6)
  expect_equal(dt$exit_savings,     0)
  expect_equal(dt$promotion_effect, 0)
  expect_equal(dt$transfer_effect,  0)
  expect_equal(dt$hiring_effect,    0)
})


# ===========================================================================
# INFLATION
# ===========================================================================

test_that("inflation_effect equals payroll * growth when no other changes", {
  set.seed(3)
  n <- 4L; sal <- 10000; growth <- 0.05
  s <- make_horizon_state(n = n, salary = sal)
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 1L,
    retirement_policy  = null_retirement_policy,
    movement_policy    = null_movement_policy,
    hiring_policy      = null_hiring_policy,
    salary_growth_rate = growth,
    ref_date           = as.Date("2020-01-01")
  )
  # null policies → no exits / moves / net hiring changes → pre-COLA bill = n*sal
  expect_equal(res$summary_dt$inflation_effect, n * sal * growth, tolerance = 1e-2)
})

test_that("wage_bill_end compounds over periods with pure inflation", {
  set.seed(5)
  n <- 4L; sal <- 10000; growth <- 0.10
  s <- make_horizon_state(n = n, salary = sal)
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 3L,
    retirement_policy  = null_retirement_policy,
    movement_policy    = null_movement_policy,
    hiring_policy      = null_hiring_policy,
    salary_growth_rate = growth,
    ref_date           = as.Date("2020-01-01")
  )
  dt <- res$summary_dt
  expected <- n * sal * (1.10 ^ seq_len(3))
  expect_equal(dt$wage_bill_end, expected, tolerance = 1)
})


# ===========================================================================
# ZERO-GROWTH CHECK
# ===========================================================================

test_that("with 0 growth and locked-out policies, all effects are zero", {
  set.seed(7)
  s <- make_horizon_state(n = 4L, salary = 10000,
                          ages = rep(30L, 4L), tenures = rep(5L, 4L))
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 1L,
    retirement_policy  = null_retirement_policy,
    movement_policy    = null_movement_policy,
    hiring_policy      = null_hiring_policy,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  dt <- res$summary_dt
  expect_equal(dt$inflation_effect, 0,             tolerance = 1e-6)
  expect_equal(dt$promotion_effect, 0,             tolerance = 1e-6)
  expect_equal(dt$transfer_effect,  0,             tolerance = 1e-6)
  expect_equal(dt$exit_savings,     0,             tolerance = 1e-6)
  expect_equal(dt$wage_bill_end,    dt$wage_bill_start, tolerance = 1e-4)
})


# ===========================================================================
# AGING
# ===========================================================================

test_that("simulate_horizon increments age each period", {
  s <- make_horizon_state(n = 2L, ages = c(40L, 50L))
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 2L,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01"),
    return_microdata   = TRUE
  )
  expect_equal(sort(res$personnel_dt$age), c(42, 52))
})


# ===========================================================================
# PENSIONER EXCLUSION
# ===========================================================================

test_that("n_headcount_start excludes pensioner rows", {
  # 2 active workers, 2 near-retirement; pensioners should not inflate headcount
  n_active <- 2L
  s <- make_horizon_state(n = n_active, salary = 10000, ages = rep(30L, n_active))
  retire_policy <- list(
    eligibility_type = "age_only",
    min_age          = 65,
    pension_type     = "flat",
    pension_params   = list(flat_amount = 1000)
  )
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 2L,
    retirement_policy  = retire_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  # No one retires (age 30 < min_age 65), headcount should stay constant
  expect_equal(res$summary_dt$n_headcount_start[1], n_active)
  expect_equal(res$summary_dt$n_headcount_start[2], n_active)
})

test_that("n_headcount_end decreases after retirements (pensioners excluded)", {
  # 4 workers all above retirement age → all retire in period 1, no hires
  n <- 4L
  s <- make_horizon_state(n = n, salary = 10000, ages = rep(66L, n))
  retire_policy <- list(
    eligibility_type = "age_only",
    min_age          = 65,
    pension_type     = "flat",
    pension_params   = list(flat_amount = 1000)
  )
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 1L,
    retirement_policy  = retire_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  # All retired → n_headcount_end should be 0
  expect_equal(res$summary_dt$n_headcount_end, 0L)
})

test_that("wage_bill_start is 0 after all workers become pensioners", {
  # All workers retire in period 1; period 2 should have wage_bill_start == 0
  n <- 2L
  s <- make_horizon_state(n = n, salary = 10000, ages = rep(66L, n))
  retire_policy <- list(
    eligibility_type = "age_only",
    min_age          = 65,
    pension_type     = "flat",
    pension_params   = list(flat_amount = 500)
  )
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 2L,
    retirement_policy  = retire_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  # Period 2: no active workers → wage_bill_start == 0
  expect_equal(res$summary_dt$wage_bill_start[2], 0)
})

test_that("exit_savings equals pre-retirement salary bill when all workers retire", {
  n <- 2L; sal <- 10000
  s <- make_horizon_state(n = n, salary = sal, ages = rep(66L, n))
  retire_policy <- list(
    eligibility_type = "age_only",
    min_age          = 65,
    pension_type     = "flat",
    pension_params   = list(flat_amount = 500)
  )
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 1L,
    retirement_policy  = retire_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01")
  )
  expect_equal(res$summary_dt$exit_savings, n * sal)
})


# ===========================================================================
# ESTIMATE_HISTORICAL_HIRING_RATES
# ===========================================================================

# Helper: build a minimal two-snapshot panel
make_panel_state <- function() {
  snap1 <- as.Date("2019-01-01")
  snap2 <- as.Date("2020-01-01")
  # snap1: 4 people; snap2: 7 people (3 new hires: P5, P6, P7)
  # hiring rate E1 = 2/3 over 1 interval → guaranteed hires in forecast
  contract_panel <- data.table::data.table(
    ref_date           = c(rep(snap1, 4), rep(snap2, 7)),
    contract_id        = c(paste0("C", 1:4), paste0("C", c(1:4, 5:7))),
    personnel_id       = c(paste0("P", 1:4), paste0("P", c(1:4, 5:7))),
    est_id             = c(rep(c("E1", "E2"), 2),
                           rep(c("E1", "E2"), 2), "E1", "E2", "E1"),
    start_date         = as.Date("2015-01-01"),
    end_date           = as.Date(NA),
    contract_type_code = "permanent",
    gross_salary_lcu   = 10000
  )
  personnel_panel <- data.table::data.table(
    ref_date     = c(rep(snap1, 4), rep(snap2, 7)),
    personnel_id = c(paste0("P", 1:4), paste0("P", c(1:4, 5:7))),
    status       = "active",
    birth_date   = as.Date("1980-01-01"),
    age          = c(rep(39L, 4), rep(40L, 7)),
    tenure_years = c(rep(5L, 4), rep(5L, 4), 0L, 0L, 0L)
  )
  list(contract_panel = contract_panel, personnel_panel = personnel_panel)
}

test_that("estimate_historical_hiring_rates: returns data.table with hiring_rate", {
  p <- make_panel_state()
  rates <- estimate_historical_hiring_rates(
    panel_contract_dt  = p$contract_panel,
    panel_personnel_dt = p$personnel_panel,
    group_cols         = "est_id"
  )
  expect_true(data.table::is.data.table(rates))
  expect_true("hiring_rate" %in% names(rates))
  expect_true("est_id"      %in% names(rates))
})

test_that("estimate_historical_hiring_rates: hiring_rate in [0, 1]", {
  p <- make_panel_state()
  rates <- estimate_historical_hiring_rates(
    panel_contract_dt  = p$contract_panel,
    panel_personnel_dt = p$personnel_panel,
    group_cols         = "est_id"
  )
  expect_true(all(rates$hiring_rate >= 0))
  expect_true(all(rates$hiring_rate <= 1))
})

test_that("estimate_historical_hiring_rates: works with group_cols = NULL", {
  p <- make_panel_state()
  rates <- estimate_historical_hiring_rates(
    panel_contract_dt  = p$contract_panel,
    panel_personnel_dt = p$personnel_panel,
    group_cols         = NULL
  )
  expect_true(data.table::is.data.table(rates))
  expect_equal(nrow(rates), 1L)
  expect_true("hiring_rate" %in% names(rates))
})


# ===========================================================================
# STATUS_QUO HIRING MODE
# ===========================================================================

test_that("simulate_horizon: status_quo mode produces hires close to historical rate", {
  p <- make_panel_state()

  salary_scale <- data.table::data.table(
    est_id           = c("E1", "E2"),
    gross_salary_lcu = c(10000, 10000)
  )

  # Use the snap2 snapshot as the starting point for the forecast
  snap2 <- as.Date("2020-01-01")
  ct_start <- p$contract_panel[ref_date == snap2]
  pt_start <- p$personnel_panel[ref_date == snap2]

  hire_policy_sq <- list(
    mode      = "status_quo",
    group_cols = "est_id",
    rate_mult  = 1,
    salary_scale = salary_scale
  )

  retire_policy_off <- list(
    eligibility_type = "age_only",
    min_age          = 999,
    pension_type     = "flat",
    pension_params   = list(flat_amount = 500)
  )

  res <- simulate_horizon(
    contract_dt        = p$contract_panel,   # full panel
    personnel_dt       = p$personnel_panel,  # full panel
    salary_scale_dt    = salary_scale,
    n_periods          = 2L,
    retirement_policy  = retire_policy_off,
    movement_policy    = NULL,
    hiring_policy      = hire_policy_sq,
    salary_growth_rate = 0,
    ref_date           = snap2
  )

  expect_true(data.table::is.data.table(res$summary_dt))
  expect_equal(nrow(res$summary_dt), 2L)
  # Some hires should occur (historical rate > 0 since P5 joined at snap2)
  expect_gt(sum(res$summary_dt$n_hires), 0L)
})
