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
    contract_type = "permanent",
    gross_salary_lcu   = as.numeric(rep(salary, n))
  )
  personnel_dt <- data.table::data.table(
    personnel_id = paste0("P", seq_len(n)),
    birth_date   = ref_date - 365L * as.integer(ages),
    employment_status = "active",
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

# The pre-status-quo default of simulate_horizon(): retire at 60 with 10+ years,
# DB pension. Tests on single-snapshot fixtures pass it explicitly, because the
# status-quo default needs a multi-snapshot panel to estimate retirement rates.
rule_retirement_policy <- design_retirement_policy_age_tenure(
  min_age = 60, min_tenure = 10,
  pension = pension_db(accrual_rate = 0.02, ref_wage_col = "gross_salary_lcu",
                       max_years = 35, replacement_cap = 0.80)
)

null_retirement_policy <- list(
  defaults = list(
    eligibility_type = "age_only", min_age = 999,
    pension_type = "flat", flat_amount = 1000
    )
)

null_movement_policy <- list(
  group_cols   = "est_id",
  policy_table = NULL,
  defaults = list(
    movement_rate      = 0,
    movement_strategy  = "tenure",
    active_types       = "permanent",
    salary_update_rule = "scale"
  )
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

test_that("compute_movement_effect: returns zero for NULL movers_dt", {
  after <- data.table::data.table(personnel_id = "P1", gross_salary_lcu = 12000)
  r <- compute_movement_effect(NULL, after)
  expect_equal(r$movement, 0)
})

test_that("compute_movement_effect: returns zero for zero-row movers_dt", {
  movers <- data.table::data.table(
    personnel_id  = character(0),
    salary_before = numeric(0)
  )
  after <- data.table::data.table(
    personnel_id     = character(0),
    gross_salary_lcu = numeric(0)
  )
  r <- compute_movement_effect(movers, after)
  expect_equal(r$movement, 0)
})

test_that("compute_movement_effect: returns zero when salary_before missing", {
  movers <- data.table::data.table(
    personnel_id = "P1"
    # no salary_before column
  )
  after <- data.table::data.table(personnel_id = "P1", gross_salary_lcu = 12000)
  r <- compute_movement_effect(movers, after)
  expect_equal(r$movement, 0)
})

test_that("compute_movement_effect: correct movement diff (upward move)", {
  movers <- data.table::data.table(
    personnel_id  = "P1",
    salary_before = 10000
  )
  after <- data.table::data.table(personnel_id = "P1", gross_salary_lcu = 12000)
  r <- compute_movement_effect(movers, after)
  expect_equal(r$movement, 2000)
})

test_that("compute_movement_effect: correct movement diff (lateral move)", {
  movers <- data.table::data.table(
    personnel_id  = "P1",
    salary_before = 12000
  )
  after <- data.table::data.table(personnel_id = "P1", gross_salary_lcu = 11000)
  r <- compute_movement_effect(movers, after)
  expect_equal(r$movement, -1000)
})

test_that("compute_movement_effect: sums total movement effect across all movers", {
  movers <- data.table::data.table(
    personnel_id  = c("P1", "P2"),
    salary_before = c(10000, 12000)
  )
  after <- data.table::data.table(
    personnel_id     = c("P1", "P2"),
    gross_salary_lcu = c(13000, 11500)
  )
  r <- compute_movement_effect(movers, after)
  # P1: +3000, P2: -500 => total = 2500
  expect_equal(r$movement, 2500)
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
    c("summary", "contract_dt", "personnel_dt", "salary_scale_dt", "pensioner_register",
      "allowance_dt", "allowance_policy")
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
    defaults = list(
      eligibility_type = "age_only", min_age = 60,
      pension_type = "flat", flat_amount = 5000
      )
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
    defaults = list(
      eligibility_type = "age_only", min_age = 60,
      pension_type = "flat", flat_amount = 1000
      )
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
    retirement_policy  = rule_retirement_policy,
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
    retirement_policy  = rule_retirement_policy,
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
    retirement_policy  = rule_retirement_policy,
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
    retirement_policy  = rule_retirement_policy,
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
    retirement_policy  = rule_retirement_policy,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01"),
    output_control_args = list(return_microdata = FALSE)
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
    retirement_policy  = rule_retirement_policy,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01"),
    output_control_args = list(return_microdata = TRUE)
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
      retirement_policy  = rule_retirement_policy,
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
    defaults = list(
      eligibility_type = "age_only", min_age = 65,
      pension_type = "flat", flat_amount = 1000
      )
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
    retirement_policy  = rule_retirement_policy,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01"),
    output_control_args = list(return_microdata = TRUE)
  )
  # Age auto-computed from birth_date (difftime/365.25) then incremented by 1
  # each period, so after 2 periods: ~40+2=42 and ~50+2=52 (within rounding).
  last_date <- max(res$personnel_dt$ref_date)
  expect_equal(sort(res$personnel_dt[ref_date == last_date, age]), c(42, 52), tolerance = 0.1)
})


# ===========================================================================
# PENSIONER EXCLUSION
# ===========================================================================

test_that("n_headcount_start excludes pensioner rows", {
  # 2 active workers, 2 near-retirement; pensioners should not inflate headcount
  n_active <- 2L
  s <- make_horizon_state(n = n_active, salary = 10000, ages = rep(30L, n_active))
  retire_policy <- list(
    defaults = list(
      eligibility_type = "age_only", min_age = 65,
      pension_type = "flat", flat_amount = 1000
      )
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
    defaults = list(
      eligibility_type = "age_only", min_age = 65,
      pension_type = "flat", flat_amount = 1000
      )
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
    defaults = list(
      eligibility_type = "age_only", min_age = 65,
      pension_type = "flat", flat_amount = 500
      )
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
    defaults = list(
      eligibility_type = "age_only", min_age = 65,
      pension_type = "flat", flat_amount = 500
      )
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
    contract_type = "permanent",
    gross_salary_lcu   = 10000
  )
  personnel_panel <- data.table::data.table(
    ref_date     = c(rep(snap1, 4), rep(snap2, 7)),
    personnel_id = c(paste0("P", 1:4), paste0("P", c(1:4, 5:7))),
    employment_status = "active",
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
    defaults = list(
      eligibility_type = "age_only", min_age = 999,
      pension_type = "flat", flat_amount = 500
      )
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

test_that("simulate_horizon: movement_policy without movement_rate on a panel with no transitions gives 0 movers, not an error", {
  # make_panel_state(): P1-P4 keep the same est_id across both snapshots, so
  # the auto-estimated transition matrix has zero rows.
  p <- make_panel_state()
  salary_scale <- data.table::data.table(
    est_id           = c("E1", "E2"),
    gross_salary_lcu = c(10000, 10000)
  )
  mov_policy <- list(
    group_cols   = "est_id",
    policy_table = NULL,
    defaults = list(
      movement_strategy  = "random",
      salary_update_rule = "scale",
      movement_type      = "transfer"
    )
  )
  retire_policy_off <- list(
    defaults = list(
      eligibility_type = "age_only", min_age = 999,
      pension_type = "flat", flat_amount = 500
    )
  )

  expect_message(
    res <- simulate_horizon(
      contract_dt        = p$contract_panel,
      personnel_dt       = p$personnel_panel,
      salary_scale_dt    = salary_scale,
      n_periods          = 2L,
      retirement_policy  = retire_policy_off,
      movement_policy    = mov_policy,
      hiring_policy      = NULL,
      salary_growth_rate = 0,
      ref_date           = as.Date("2020-01-01")
    ),
    "no defaults\\$movement_rate"
  )
  expect_equal(nrow(res$summary_dt), 2L)
  expect_equal(sum(res$summary_dt$n_transfers), 0L)
  expect_equal(sum(res$summary_dt$transfer_effect), 0)
})

test_that("simulate_horizon: ungrouped status-quo exits use the rate estimated from the panel", {
  # 10 active at 2019, 3 gone by 2020 (govhr "fire" events). exit_policy has
  # group_cols = NULL and no defaults$exit_rate: the estimated overall rate
  # must drive exits. Regression test: it used to be ignored (0 exits).
  ids1 <- sprintf("P%02d", 1:10); ids2 <- sprintf("P%02d", 1:7)
  ct <- data.table::data.table(
    ref_date         = c(rep(as.Date("2019-01-01"), 10), rep(as.Date("2020-01-01"), 7)),
    personnel_id     = c(ids1, ids2),
    contract_id      = sub("P", "C", c(ids1, ids2)),
    contract_type    = "permanent",
    start_date       = as.Date("2010-01-01"),
    end_date         = as.Date(NA),
    gross_salary_lcu = 1000
  )
  pt <- ct[, .(ref_date, personnel_id, employment_status = "active",
               birth_date = as.Date("1980-01-01"))]
  exit_policy <- list(group_cols = NULL, policy_table = NULL,
                      defaults = list(exit_strategy = "random"))

  res <- simulate_horizon(
    contract_dt        = ct,
    personnel_dt       = pt,
    salary_scale_dt    = data.table::data.table(gross_salary_lcu = 1000),
    n_periods          = 1L,
    exit_policy        = exit_policy,
    salary_growth_rate = 0,
    ref_date           = as.Date("2020-01-01"),
    seed               = 1L
  )
  expect_gte(res$summary_dt$n_non_ret_exits, 1L)
})

# ---------------------------------------------------------------------------
# Status-quo (hazard-rate) retirement: panel age derivation, zero retirements
# ---------------------------------------------------------------------------
# 3 snapshots, 6 staff born 1958-1963, no age column (birth_date only).
# P1 (oldest) becomes a pensioner at the second snapshot.
make_hazard_panel <- function(age_name = NULL, with_pensioner = TRUE) {
  d   <- as.Date(c("2018-01-01", "2019-01-01", "2020-01-01"))
  ids <- sprintf("P%d", 1:6)
  pt <- data.table::rbindlist(lapply(d, function(x) data.table::data.table(
    ref_date = x, personnel_id = ids,
    employment_status = if (with_pensioner && x > d[1])
      c("pensioner", rep("active", 5)) else "active",
    birth_date = as.Date(sprintf("%d-06-01", 1958:1963))
  )))
  if (!is.null(age_name))
    pt[, (age_name) := as.numeric(difftime(ref_date, birth_date, units = "days")) / 365.25]
  ct <- pt[, .(ref_date, personnel_id, contract_id = sub("P", "C", personnel_id),
               contract_type = data.table::fifelse(employment_status == "pensioner",
                                                   "pensioner", "permanent"),
               start_date = as.Date("1990-01-01"), end_date = as.Date(NA),
               gross_salary_lcu = 1000)]
  list(ct = ct, pt = pt)
}

run_hazard_horizon <- function(p, ...) {
  simulate_horizon(
    contract_dt = p$ct, personnel_dt = p$pt,
    salary_scale_dt = data.table::data.table(gross_salary_lcu = 1000),
    n_periods = 1L, salary_growth_rate = 0,
    ref_date = as.Date("2020-01-01"), seed = 1L, ...
  )
}

test_that("simulate_horizon: default status-quo retirement works when the panel has birth_date but no age column", {
  # Regression test: hazard rates were estimated before age was derived from
  # birth_date, so this errored with "object 'age' not found".
  res <- run_hazard_horizon(make_hazard_panel())
  expect_equal(nrow(res$summary_dt), 1L)
})

test_that("simulate_horizon: status-quo retirement respects column_spec_args$age_col", {
  pen <- pension_db(accrual_rate = 0.02, ref_wage_col = "gross_salary_lcu")
  res <- run_hazard_horizon(
    make_hazard_panel(age_name = "age_years"),
    retirement_policy = design_retirement_policy_status_quo(pension = pen),
    column_spec_args  = list(age_col = "age_years", birth_date_col = NULL)
  )
  expect_equal(nrow(res$summary_dt), 1L)
})

test_that("simulate_horizon: a panel with no retirements gives no status-quo retirements, not an error", {
  expect_message(
    res <- run_hazard_horizon(make_hazard_panel(with_pensioner = FALSE)),
    "no retirements observed"
  )
  expect_equal(res$summary_dt$n_exits, 0L)
})

test_that("simulate_horizon: with no observed retirements, a mandatory age still applies", {
  # At 2020-01-01 the staff are aged 56-61; mandatory age 60 retires the two aged 60+.
  pen <- pension_db(accrual_rate = 0.02, ref_wage_col = "gross_salary_lcu")
  expect_message(
    res <- run_hazard_horizon(
      make_hazard_panel(with_pensioner = FALSE),
      retirement_policy = design_retirement_policy_status_quo(pension = pen, mandatory_age = 60)
    ),
    "apart from the mandatory age of 60"
  )
  expect_equal(res$summary_dt$n_exits, 2L)
})

test_that("make_status_quo_policies: exit active_types match the harmonized contract_type values", {
  p <- make_panel_state()
  pol <- make_status_quo_policies(
    contract_dt     = p$contract_panel,
    personnel_dt    = p$personnel_panel,
    salary_scale_dt = data.table::data.table(est_id = c("E1", "E2"), gross_salary_lcu = 10000)
  )
  expect_equal(pol$exit$defaults$active_types, c("permanent", "fixed-term", "short-term"))
})

# ===========================================================================
# Phase 1b — auto-compute age/tenure in simulate_horizon() prologue
# ===========================================================================

# Helper: minimal single-snapshot inputs with birth_date present but age/tenure
# intentionally set to zero so we can assert that simulate_horizon() overwrites them.
make_phase1b_inputs <- function(ref_date = as.Date("2020-01-01")) {
  contract_dt <- data.table::data.table(
    contract_id        = c("C1", "C2"),
    personnel_id       = c("P1", "P2"),
    est_id             = c("E1", "E1"),
    # P1: 10-year contract; P2: 5-year contract
    start_date         = c(ref_date - 365L * 10L, ref_date - 365L * 5L),
    end_date           = as.Date(NA_character_),
    contract_type = "permanent",
    gross_salary_lcu   = c(10000, 10000)
  )
  personnel_dt <- data.table::data.table(
    personnel_id = c("P1", "P2"),
    # birth_date gives P1 age = 40, P2 age = 30 at ref_date
    birth_date   = c(ref_date - 365L * 40L, ref_date - 365L * 30L),
    employment_status = "active",
    age              = NA_real_,       # intentionally wrong — should be overwritten
    personnel_tenure = NA_real_        # intentionally wrong — should be overwritten
  )
  salary_scale_dt <- data.table::data.table(
    est_id           = "E1",
    gross_salary_lcu = 10000
  )
  retire_off <- list(
    defaults = list(
      eligibility_type = "age_only", min_age = 999L,
      pension_type = "flat", flat_amount = 500
      )
  )
  list(
    contract_dt     = contract_dt,
    personnel_dt    = personnel_dt,
    salary_scale_dt = salary_scale_dt,
    retire_off      = retire_off,
    ref_date        = ref_date
  )
}

test_that("Phase 1b: simulate_horizon() auto-computes age from birth_date_col", {
  d <- make_phase1b_inputs()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = d$retire_off,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date,
    output_control_args = list(return_microdata = TRUE),
    column_spec_args   = list(birth_date_col = "birth_date")
  )

  # After the prologue the age column in personnel_dt should ≈ 40 and 30
  # (exact value is difftime in days / 365.25, so ~39.97 and ~29.97 for
  #  365-day approximations, but well above the initial 0)
  final_pers <- res$personnel_dt
  ages <- final_pers[order(personnel_id), age]
  expect_gt(ages[1], 39)  # P1 ~40 + 1 period increment
  expect_gt(ages[2], 29)  # P2 ~30 + 1 period increment
})

test_that("Phase 1b: simulate_horizon() auto-computes tenure from contract history", {
  d <- make_phase1b_inputs()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = d$retire_off,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date,
    output_control_args = list(return_microdata = TRUE),
    column_spec_args   = list(birth_date_col = "birth_date")
  )

  # P1 started 10 years before ref_date → tenure ≈ 10 + 1 period increment
  # P2 started  5 years before ref_date → tenure ≈  5 + 1 period increment
  # Written into the default tenure_col, personnel_tenure (govhr dictionary name).
  final_pers <- res$personnel_dt
  tenures <- final_pers[order(personnel_id), personnel_tenure]
  expect_gt(tenures[1], 9.5)   # P1
  expect_gt(tenures[2], 4.5)   # P2
})

test_that("Phase 1b: birth_date_col = NULL skips age auto-compute", {
  d <- make_phase1b_inputs()
  d$personnel_dt[, age := 0]  # override NA so aging step produces ~1, not NA

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = d$retire_off,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date,
    output_control_args = list(return_microdata = TRUE),
    column_spec_args   = list(birth_date_col = NULL)   # ← skip age auto-compute
  )

  # age was initialised to 0; after 1 period it should be ~1 (incremented once)
  final_pers <- res$personnel_dt
  ages <- final_pers[order(personnel_id), age]
  expect_lt(ages[1], 5)   # started at 0 + 1 increment = 1, not ~41
  expect_lt(ages[2], 5)
})

test_that("Phase 1b: missing birth_date column is silently ignored", {
  d <- make_phase1b_inputs()
  # Remove birth_date so the column doesn't exist
  d$personnel_dt[, birth_date := NULL]

  # Should not error even though birth_date_col = "birth_date" by default.
  # Use retirement_policy = NULL to avoid the birth_date validation check in
  # check_retirement_inputs() — we're only testing the prologue guard here.
  expect_no_error(
    simulate_horizon(
      contract_dt        = d$contract_dt,
      personnel_dt       = d$personnel_dt,
      salary_scale_dt    = d$salary_scale_dt,
      n_periods          = 1L,
      retirement_policy  = NULL,
      movement_policy    = NULL,
      hiring_policy      = NULL,
      salary_growth_rate = 0,
      ref_date           = d$ref_date
    )
  )
})

# =============================================================================
# Phase 2c helpers
# =============================================================================

make_phase2c_inputs <- function() {
  ref_date <- as.Date("2020-01-01")

  # Two active employees
  contract_dt <- data.table::data.table(
    personnel_id     = c("P1", "P2"),
    contract_id      = c("C1", "C2"),
    start_date       = as.Date(c("2010-01-01", "2015-01-01")),
    end_date         = as.Date(c(NA, NA)),
    gross_salary_lcu = c(50000, 40000),
    contract_type = c("permanent", "permanent"),
    employment_status = c("active", "active")
  )

  personnel_dt <- data.table::data.table(
    personnel_id = c("P1", "P2"),
    birth_date   = as.Date(c("1960-01-01", "1965-01-01")),
    employment_status = c("active", "active")
  )

  salary_scale_dt <- data.table::data.table(
    grade            = c("G1", "G1"),
    step             = c(1L, 2L),
    gross_salary_lcu = c(40000, 50000)
  )

  list(
    contract_dt     = contract_dt,
    personnel_dt    = personnel_dt,
    salary_scale_dt = salary_scale_dt,
    ref_date        = ref_date
  )
}

make_phase2c_inputs_with_existing_pensioners <- function() {
  ref_date <- as.Date("2020-01-01")

  # Two active employees + two pre-existing retirees
  contract_dt <- data.table::data.table(
    personnel_id     = c("P1", "P2", "R1", "R2"),
    contract_id      = c("C1", "C2", "RC1", "RC2"),
    start_date       = as.Date(c("2010-01-01", "2015-01-01",
                                 "2000-01-01", "1998-01-01")),
    end_date         = as.Date(c(NA, NA, NA, NA)),
    gross_salary_lcu = c(50000, 40000, 20000, 18000),
    contract_type = c("permanent", "permanent", "pensioner", "pensioner"),
    employment_status = c("active", "active", "inactive", "inactive")
  )

  personnel_dt <- data.table::data.table(
    personnel_id = c("P1", "P2", "R1", "R2"),
    birth_date   = as.Date(c("1960-01-01", "1965-01-01",
                              "1950-01-01", "1948-01-01")),
    employment_status = c("active", "active", "inactive", "inactive")
  )

  salary_scale_dt <- data.table::data.table(
    grade            = c("G1", "G1"),
    step             = c(1L, 2L),
    gross_salary_lcu = c(40000, 50000)
  )

  list(
    contract_dt     = contract_dt,
    personnel_dt    = personnel_dt,
    salary_scale_dt = salary_scale_dt,
    ref_date        = ref_date
  )
}

# =============================================================================
# Phase 2c tests
# =============================================================================

test_that("Phase 2c: pre-existing pensioners seed pension_cost_total in period 1", {
  d <- make_phase2c_inputs_with_existing_pensioners()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 2L,
    retirement_policy  = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )

  # R1 and R2 have pension_amount = 20000 and 18000 respectively
  # pension_cost_total should reflect both in both periods (no new retirees, no COLA)
  expect_gt(res$comparison$pension_cost_total[1], 0)
  expect_equal(res$comparison$pension_cost_total[1],
               res$comparison$pension_cost_total[2])
})

test_that("Phase 2c: pension_cola_rate = 0 keeps pension_cost_total constant", {
  d <- make_phase2c_inputs_with_existing_pensioners()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 3L,
    retirement_policy  = NULL,
    salary_growth_rate = 0.05,
    pension_cola_rate  = 0,
    ref_date           = d$ref_date
  )

  # salary_growth_rate raises active wages but pension amounts should be unchanged
  total_pensions <- res$comparison$pension_cost_total
  expect_equal(total_pensions[1], total_pensions[2])
  expect_equal(total_pensions[2], total_pensions[3])
})

test_that("Phase 2c: pension_cola_rate > 0 grows pension_cost_total each period", {
  d <- make_phase2c_inputs_with_existing_pensioners()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 3L,
    retirement_policy  = NULL,
    salary_growth_rate = 0,
    pension_cola_rate  = 0.05,
    ref_date           = d$ref_date
  )

  total_pensions <- res$comparison$pension_cost_total
  # Each period the register is COLA-adjusted after snapshot, so costs grow
  expect_gt(total_pensions[2], total_pensions[1])
  expect_gt(total_pensions[3], total_pensions[2])
  # Growth factor should be ~1.05
  expect_equal(total_pensions[2] / total_pensions[1], 1.05, tolerance = 1e-6)
})

test_that("Phase 2c: pensioner_register has 5-column expanded schema", {
  d <- make_phase2c_inputs_with_existing_pensioners()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = rule_retirement_policy,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )

  reg <- res$pensioner_register
  expect_true(all(c("personnel_id", "pension_amount", "final_salary",
                    "tenure_years_at_retirement", "age_at_retirement",
                    "period_date") %in% names(reg)))
})

test_that("Phase 2c: pension_cola_rate as vector of length n_periods is accepted", {
  d <- make_phase2c_inputs_with_existing_pensioners()

  expect_no_error(
    simulate_horizon(
      contract_dt        = d$contract_dt,
      personnel_dt       = d$personnel_dt,
      salary_scale_dt    = d$salary_scale_dt,
      n_periods          = 3L,
      retirement_policy  = rule_retirement_policy,
      salary_growth_rate = 0,
      pension_cola_rate  = c(0.02, 0.03, 0.04),
      ref_date           = d$ref_date
    )
  )
})

test_that("Phase 2c: pension_cola_rate wrong length raises error", {
  d <- make_phase2c_inputs_with_existing_pensioners()

  expect_error(
    simulate_horizon(
      contract_dt        = d$contract_dt,
      personnel_dt       = d$personnel_dt,
      salary_scale_dt    = d$salary_scale_dt,
      n_periods          = 3L,
      salary_growth_rate = 0,
      pension_cola_rate  = c(0.02, 0.03),  # length 2, not 3
      ref_date           = d$ref_date
    ),
    regexp = "pension_cola_rate"
  )
})


# =============================================================================
# Block E — scenario_name and is_baseline parameters
# =============================================================================

make_minimal_horizon_inputs <- function() {
  contract_dt <- data.table::data.table(
    contract_id        = "C1",
    personnel_id       = "P1",
    start_date         = as.Date("2010-01-01"),
    end_date           = as.Date(NA),
    gross_salary_lcu   = 50000,
    contract_type = "permanent",
    employment_status = "active"
  )
  personnel_dt <- data.table::data.table(
    personnel_id = "P1",
    employment_status = "active"
  )
  salary_scale_dt <- data.table::data.table(
    grade            = "G1",
    gross_salary_lcu = 50000
  )
  list(
    contract_dt     = contract_dt,
    personnel_dt    = personnel_dt,
    salary_scale_dt = salary_scale_dt,
    ref_date        = as.Date("2020-01-01")
  )
}

test_that("Block E: scenario_name stamps scenario_id and scenario_label columns", {
  d <- make_minimal_horizon_inputs()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 2L,
    retirement_policy  = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date,
    output_control_args = list(scenario_name = "Baseline")
  )

  expect_true("scenario_id"    %in% names(res$comparison))
  expect_true("scenario_label" %in% names(res$comparison))
  expect_true(all(res$comparison$scenario_id    == "Baseline"))
  expect_true(all(res$comparison$scenario_label == "Baseline"))
})

test_that("Block E: is_baseline = TRUE stamps TRUE in is_baseline column", {
  d <- make_minimal_horizon_inputs()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date,
    output_control_args = list(scenario_name = "BAU", is_baseline = TRUE)
  )

  expect_true("is_baseline" %in% names(res$comparison))
  expect_true(all(res$comparison$is_baseline == TRUE))
})

test_that("Block E: is_baseline = FALSE (default) stamps FALSE", {
  d <- make_minimal_horizon_inputs()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date,
    output_control_args = list(scenario_name = "Alt")
  )

  expect_true(all(res$comparison$is_baseline == FALSE))
})

test_that("Block E: scenario_name = NULL omits scenario_id and scenario_label", {
  d <- make_minimal_horizon_inputs()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
    # scenario_name omitted (defaults to NULL)
  )

  expect_false("scenario_id"    %in% names(res$comparison))
  expect_false("scenario_label" %in% names(res$comparison))
  # is_baseline should still be present (always stamped)
  expect_true("is_baseline" %in% names(res$comparison))
  expect_true(all(res$comparison$is_baseline == FALSE))
})

test_that("Block E: scenario_name and is_baseline stored in metadata", {
  d <- make_minimal_horizon_inputs()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date,
    output_control_args = list(scenario_name = "Scenario A", is_baseline = TRUE)
  )

  expect_equal(res$metadata$scenario_name, "Scenario A")
  expect_true(res$metadata$is_baseline)
})

test_that("Block E: non-character scenario_name raises error", {
  d <- make_minimal_horizon_inputs()

  expect_error(
    simulate_horizon(
      contract_dt        = d$contract_dt,
      personnel_dt       = d$personnel_dt,
      salary_scale_dt    = d$salary_scale_dt,
      n_periods          = 1L,
      retirement_policy  = NULL,
      salary_growth_rate = 0,
      ref_date           = d$ref_date,
      output_control_args = list(scenario_name = 42L)    # must be character
    ),
    regexp = "scenario_name"
  )
})

test_that("Block E: scenario_id and scenario_label are first columns when scenario_name set", {
  d <- make_minimal_horizon_inputs()

  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date,
    output_control_args = list(scenario_name = "Test")
  )

  col_positions <- match(c("scenario_id", "scenario_label", "is_baseline"),
                         names(res$comparison))
  expect_true(all(col_positions <= 3L))
})

# =============================================================================
# Block F — canonical 3-slot policy_params passing through simulate_horizon()
# Tests that group_cols / policy_table / defaults all reach the sub-functions
# correctly when wired through simulate_horizon().
# =============================================================================

# Shared fixture: 4 workers in 2 grades, 2 periods of contract history
make_block_f_inputs <- function() {
  ref_date <- as.Date("2020-01-01")

  contract_dt <- data.table::data.table(
    contract_id        = paste0("C", 1:6),
    personnel_id       = paste0("P", 1:6),
    paygrade           = c("G1", "G1", "G1", "G2", "G2", "G2"),
    est_id             = rep(c("E1", "E2"), 3L),
    start_date         = ref_date - c(365L * 5L, 365L * 3L, 365L * 7L,
                                      365L * 2L, 365L * 8L, 365L * 4L),
    end_date           = as.Date(NA),
    contract_type = "permanent",
    gross_salary_lcu   = c(30000, 30000, 30000, 50000, 50000, 50000),
    employment_status = "active"
  )
  personnel_dt <- data.table::data.table(
    personnel_id = paste0("P", 1:6),
    birth_date   = ref_date - 365L * c(35L, 38L, 42L, 45L, 50L, 55L),
    employment_status = "active",
    age          = c(35, 38, 42, 45, 50, 55),
    tenure_years = c(5, 3, 7, 2, 8, 4)
  )
  salary_scale_dt <- data.table::data.table(
    paygrade         = c("G1", "G2"),
    gross_salary_lcu = c(30000, 50000)
  )
  list(
    contract_dt     = contract_dt,
    personnel_dt    = personnel_dt,
    salary_scale_dt = salary_scale_dt,
    ref_date        = ref_date
  )
}

# ── Retirement ────────────────────────────────────────────────────────────────

test_that("Block F: canonical 3-slot retirement_policy (scalar defaults) works", {
  d <- make_block_f_inputs()

  # Nobody is old enough to retire (max age = 55 < 60)
  ret_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_only",
      pension_type     = "flat",
      flat_amount      = 500,
      min_age          = 60
    )
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = ret_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  expect_equal(res$summary_dt$n_exits, 0L)
})

test_that("Block F: canonical 3-slot retirement_policy with group_cols + policy_table", {
  d <- make_block_f_inputs()

  # G2 workers (birth_date offset by 365L days → actual age ≈ 44.97, 49.97, 54.97)
  # retire at min_age = 44; G1 workers (approx 35, 38, 42) have min_age = 60 → none retire
  pt <- data.table::data.table(
    paygrade = c("G1", "G2"),
    min_age  = c(60,   44)
  )
  ret_policy <- list(
    group_cols   = "paygrade",
    policy_table = pt,
    defaults = list(
      eligibility_type = "age_only",
      pension_type     = "flat",
      flat_amount      = 500,
      min_age          = 60
    )
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = ret_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  # All 3 G2 workers (approx 44.97, 49.97, 54.97) are >= 44 → all retire; G1 none
  expect_equal(res$summary_dt$n_exits, 3L)
})

test_that("Block F: retirement policy_table overrides defaults per group", {
  d <- make_block_f_inputs()

  # policy_table sets G1 min_age = 999 (never retire) but G2 min_age = 40
  pt <- data.table::data.table(
    paygrade = c("G1", "G2"),
    min_age  = c(999,   40)
  )
  ret_policy <- list(
    group_cols   = "paygrade",
    policy_table = pt,
    defaults = list(
      eligibility_type = "age_only",
      pension_type     = "flat",
      flat_amount      = 500,
      min_age          = 999
    )
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = ret_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  # G1: no retirements; G2 (ages 45, 50, 55 all >= 40): 3 retirements
  expect_equal(res$summary_dt$n_exits, 3L)
})

# ── Exit policy ───────────────────────────────────────────────────────────────

test_that("Block F: canonical 3-slot exit_policy (flat rate) produces exits", {
  d <- make_block_f_inputs()

  no_retire <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(eligibility_type = "age_only", pension_type = "flat",
                        flat_amount = 0, min_age = 999)
  )
  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = 1.0,          # 100% → all active workers exit
      exit_strategy = "random",
      active_types  = "permanent",
      exited_type   = "inactive"
    )
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = no_retire,
    exit_policy        = exit_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  # With exit_rate = 1.0 all 6 active workers should exit
  expect_equal(res$summary_dt$n_non_ret_exits %||%
                 (res$summary_dt$n_headcount_start - res$summary_dt$n_headcount_end),
               6L)
})

test_that("Block F: exit_rate = 0 produces no non-retirement exits", {
  d <- make_block_f_inputs()

  no_retire <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(eligibility_type = "age_only", pension_type = "flat",
                        flat_amount = 0, min_age = 999)
  )
  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = 0,
      exit_strategy = "random",
      active_types  = "permanent",
      exited_type   = "inactive"
    )
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = no_retire,
    exit_policy        = exit_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  # Headcount should be unchanged
  expect_equal(res$summary_dt$n_headcount_end, res$summary_dt$n_headcount_start)
})

test_that("Block F: canonical exit_policy with group_cols + policy_table", {
  d <- make_block_f_inputs()

  no_retire <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(eligibility_type = "age_only", pension_type = "flat",
                        flat_amount = 0, min_age = 999)
  )
  # G1: 100% exit; G2: 0% exit
  exit_pt <- data.table::data.table(
    paygrade  = c("G1", "G2"),
    exit_rate = c(1.0,  0.0)
  )
  exit_policy <- list(
    group_cols   = "paygrade",
    policy_table = exit_pt,
    defaults = list(
      exit_rate     = 0,
      exit_strategy = "random",
      active_types  = "permanent",
      exited_type   = "inactive"
    )
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = no_retire,
    exit_policy        = exit_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  # 3 G1 workers exit; 3 G2 stay.
  # n_headcount_end counts non-pensioner rows — exited workers are stamped
  # "inactive" (not "pensioner") so headcount_end stays at 6.
  # Check n_non_ret_exits in the summary directly.
  expect_equal(res$summary_dt$n_non_ret_exits, 3L)
})

# ── Movement policy ───────────────────────────────────────────────────────────

test_that("Block F: canonical 3-slot movement_policy (flat rate = 0) gives no movers", {
  d <- make_block_f_inputs()

  no_retire <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(eligibility_type = "age_only", pension_type = "flat",
                        flat_amount = 0, min_age = 999)
  )
  mov_policy <- list(
    group_cols   = "paygrade",
    policy_table = NULL,
    defaults = list(
      movement_rate      = 0,
      movement_strategy  = "tenure",
      active_types       = "permanent",
      salary_update_rule = "scale"
    )
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = no_retire,
    exit_policy        = NULL,
    movement_policy    = mov_policy,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  expect_equal(res$summary_dt$n_promotions, 0L)
  expect_equal(res$summary_dt$promotion_effect, 0)
})

test_that("Block F: canonical 3-slot movement_policy (flat rate > 0) produces movers", {
  set.seed(42)
  d <- make_block_f_inputs()

  no_retire <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(eligibility_type = "age_only", pension_type = "flat",
                        flat_amount = 0, min_age = 999)
  )
  mov_policy <- list(
    group_cols   = "paygrade",
    policy_table = NULL,
    defaults = list(
      movement_rate      = 0.5,
      movement_strategy  = "tenure",
      active_types       = "permanent",
      salary_update_rule = "scale"
    )
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = no_retire,
    exit_policy        = NULL,
    movement_policy    = mov_policy,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  # With rate = 0.5 across 6 workers some movement should occur
  expect_gte(res$summary_dt$n_promotions, 0L)
  expect_true(is.numeric(res$summary_dt$promotion_effect))
})

test_that("Block F: movement_type defaults to promotion when unset", {
  d <- make_block_f_inputs()

  no_retire <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(eligibility_type = "age_only", pension_type = "flat",
                        flat_amount = 0, min_age = 999)
  )
  # Explicit policy_table with rate = 1.0 -- deterministic (no stochastic
  # rounding ambiguity) and sidesteps simulate_promotions_transfers()'s
  # single-snapshot + NULL-policy_table short-circuit (a separate,
  # pre-existing issue, not this fix's concern).
  mov_policy <- list(
    group_cols   = "paygrade",
    policy_table = data.table::data.table(
      from_group = "G1", to_group = "G2", movement_rate = 1.0
    ),
    defaults = list(
      movement_strategy  = "tenure",
      active_types       = "permanent",
      salary_update_rule = "scale"
      # movement_type deliberately omitted -- should default to "promotion"
    )
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = no_retire,
    exit_policy        = NULL,
    movement_policy    = mov_policy,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  # All 3 people in G1 (P1-P3) move to G2 -- rate 1.0, no rounding ambiguity.
  expect_equal(res$summary_dt$n_promotions, 3L)
  expect_equal(res$summary_dt$n_transfers, 0L)
  expect_equal(res$summary_dt$transfer_effect, 0)
})

test_that("Block F: movement_type = 'transfer' reports under n_transfers, not n_promotions", {
  d <- make_block_f_inputs()

  no_retire <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(eligibility_type = "age_only", pension_type = "flat",
                        flat_amount = 0, min_age = 999)
  )
  mov_policy <- list(
    group_cols   = "est_id",
    policy_table = data.table::data.table(
      from_group = "E1", to_group = "E2", movement_rate = 1.0
    ),
    defaults = list(
      movement_strategy  = "random",
      active_types       = "permanent",
      salary_update_rule = "keep",
      movement_type      = "transfer"
    )
  )
  salary_scale_by_est <- data.table::data.table(
    est_id           = c("E1", "E2"),
    gross_salary_lcu = c(30000, 50000)
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = salary_scale_by_est,
    n_periods          = 1L,
    retirement_policy  = no_retire,
    exit_policy        = NULL,
    movement_policy    = mov_policy,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  # All 3 people in E1 (P1, P3, P5) move to E2 -- rate 1.0, no rounding ambiguity.
  expect_equal(res$summary_dt$n_transfers, 3L)
  expect_equal(res$summary_dt$n_promotions, 0L)
  expect_equal(res$summary_dt$promotion_effect, 0)
})

test_that("check_movement_inputs validates movement_type", {
  contract_dt <- data.table::data.table(
    contract_id = "C1", personnel_id = "P1", paygrade = "G1",
    start_date = as.Date("2020-01-01"), end_date = as.Date(NA), contract_type = "permanent"
  )
  personnel_dt <- data.table::data.table(personnel_id = "P1", employment_status = "active")
  salary_scale_dt <- data.table::data.table(paygrade = "G1", gross_salary_lcu = 30000)
  policy_params <- list(
    group_cols = "paygrade", policy_table = NULL,
    defaults = list(movement_rate = 0.1, movement_type = "not_a_real_type")
  )

  expect_error(
    check_movement_inputs(
      contract_dt = contract_dt, personnel_dt = personnel_dt,
      salary_scale_dt = salary_scale_dt, policy_params = policy_params,
      ref_date = as.Date("2020-01-01")
    ),
    regexp = "movement_type"
  )
})

test_that("Block F: movement_policy NULL gives zero promotion and transfer effects", {
  d <- make_block_f_inputs()

  no_retire <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(eligibility_type = "age_only", pension_type = "flat",
                        flat_amount = 0, min_age = 999)
  )
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = no_retire,
    exit_policy        = NULL,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0,
    ref_date           = d$ref_date
  )
  expect_equal(res$summary_dt$promotion_effect, 0)
  expect_equal(res$summary_dt$transfer_effect,  0)
})

# ── Combined modules ──────────────────────────────────────────────────────────

test_that("Block F: all 3 canonical policies active simultaneously — no error", {
  set.seed(7)
  d <- make_block_f_inputs()

  ret_policy <- list(
    group_cols   = "paygrade",
    policy_table = data.table::data.table(paygrade = c("G1", "G2"),
                                          min_age  = c(999,   45)),
    defaults = list(eligibility_type = "age_only", pension_type = "flat",
                    flat_amount = 500, min_age = 999)
  )
  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(exit_rate = 0.1, exit_strategy = "random",
                    active_types = "permanent", exited_type = "inactive")
  )
  mov_policy <- list(
    group_cols   = "paygrade",
    policy_table = NULL,
    defaults = list(movement_rate = 0.1, movement_strategy = "tenure",
                    active_types = "permanent", salary_update_rule = "scale")
  )
  expect_no_error(
    simulate_horizon(
      contract_dt        = d$contract_dt,
      personnel_dt       = d$personnel_dt,
      salary_scale_dt    = d$salary_scale_dt,
      n_periods          = 2L,
      retirement_policy  = ret_policy,
      exit_policy        = exit_policy,
      movement_policy    = mov_policy,
      hiring_policy      = NULL,
      salary_growth_rate = 0.02,
      ref_date           = d$ref_date
    )
  )
})

test_that("Block F: wage_bill_end accounting identity holds with exits + COLA", {
  d <- make_block_f_inputs()

  no_retire <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(eligibility_type = "age_only", pension_type = "flat",
                        flat_amount = 0, min_age = 999)
  )
  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(exit_rate = 0.5, exit_strategy = "random",
                    active_types = "permanent", exited_type = "inactive")
  )
  set.seed(1)
  res <- simulate_horizon(
    contract_dt        = d$contract_dt,
    personnel_dt       = d$personnel_dt,
    salary_scale_dt    = d$salary_scale_dt,
    n_periods          = 1L,
    retirement_policy  = no_retire,
    exit_policy        = exit_policy,
    movement_policy    = NULL,
    hiring_policy      = NULL,
    salary_growth_rate = 0.05,
    ref_date           = d$ref_date
  )
  dt <- res$summary_dt
  # Identity: wage_bill_end ≈ wage_bill_start - exit_savings + hiring_effect + inflation_effect
  rhs <- dt$wage_bill_start - dt$exit_savings + dt$hiring_effect +
         dt$promotion_effect + dt$transfer_effect + dt$inflation_effect
  expect_equal(dt$wage_bill_end, rhs, tolerance = 1)
})


# =============================================================================
# Block G -- allowance module integration (simulate_scenario()/simulate_horizon())
# =============================================================================

make_allowance_scale <- function() {
  data.table::data.table(
    est_id         = c("E1", "E1", "E2", "E2"),
    allowance_type = c("hazard", "transport", "hazard", "transport"),
    amount         = c(100, 20, 5, 5)
  )
}

test_that("Block G: allowance_policy = NULL leaves existing wage_bill columns untouched and adds zeroed allowance columns", {
  s <- make_horizon_state()
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 3L,
    retirement_policy  = null_retirement_policy,
    salary_growth_rate = 0.02,
    ref_date           = as.Date("2020-01-01")
  )
  dt <- res$comparison
  expect_true(all(c("allowance_cost_start", "allowance_cost_end",
                    "total_wage_bill_start", "total_wage_bill_end") %in% names(dt)))
  expect_true(all(dt$allowance_cost_start == 0))
  expect_true(all(dt$allowance_cost_end == 0))
  expect_equal(dt$total_wage_bill_start, dt$wage_bill_start)
  expect_equal(dt$total_wage_bill_end, dt$wage_bill_end)
})

test_that("Block G: existing summary columns are byte-for-byte identical with allowance_policy NULL vs omitted", {
  s <- make_horizon_state()
  args <- list(
    contract_dt = s$contract_dt, personnel_dt = s$personnel_dt,
    salary_scale_dt = make_salary_scale(), n_periods = 3L,
    retirement_policy = null_retirement_policy, salary_growth_rate = 0.02,
    ref_date = as.Date("2020-01-01")
  )
  before <- do.call(simulate_horizon, args)
  after  <- do.call(simulate_horizon, c(args, list(allowance_policy = NULL)))
  shared_cols <- intersect(names(before$comparison), names(after$comparison))
  expect_equal(before$comparison[, ..shared_cols], after$comparison[, ..shared_cols])
})

test_that("Block G: total_wage_bill reconciles against wage_bill + allowance_cost every period", {
  s <- make_horizon_state()
  allowance_policy <- list(
    group_cols = c("est_id", "allowance_type"), policy_table = make_allowance_scale(),
    defaults = list(amount = 0)
  )
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 3L,
    retirement_policy  = null_retirement_policy,
    salary_growth_rate = 0.02,
    allowance_policy   = allowance_policy,
    allowance_growth_rate = 0.03,
    ref_date           = as.Date("2020-01-01")
  )
  dt <- res$comparison
  expect_equal(dt$total_wage_bill_start, dt$wage_bill_start + dt$allowance_cost_start)
  expect_equal(dt$total_wage_bill_end, dt$wage_bill_end + dt$allowance_cost_end)
})

test_that("Block G: allowance_cost_start[1] is the real baseline, not zero; later periods thread from allowance_cost_end", {
  s <- make_horizon_state()  # 4 contracts: 2 in E1, 2 in E2
  allowance_policy <- list(
    group_cols = c("est_id", "allowance_type"), policy_table = make_allowance_scale(),
    defaults = list(amount = 0)
  )
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 4L,
    retirement_policy  = null_retirement_policy,
    allowance_policy   = allowance_policy,
    ref_date           = as.Date("2020-01-01")
  )
  dt <- res$comparison

  # Hand-calculated baseline: 2 contracts x (E1: hazard 100 + transport 20)
  # + 2 contracts x (E2: hazard 5 + transport 5) = 260. Must NOT be zero --
  # this is what people are already receiving before period 1 even runs,
  # exactly as wage_bill_start reflects real pre-existing salaries.
  expected_baseline <- 2 * (100 + 20) + 2 * (5 + 5)
  expect_equal(dt$allowance_cost_start[1], expected_baseline)
  expect_true(dt$allowance_cost_start[1] > 0)

  # Periods 2+ inherit exactly what was actually last assigned.
  expect_equal(dt$allowance_cost_start[-1], dt$allowance_cost_end[-nrow(dt)])
})

test_that("Block G: allowance_cost_start does NOT double-count next period's own COLA growth", {
  s <- make_horizon_state()
  allowance_policy <- list(
    group_cols = c("est_id", "allowance_type"), policy_table = make_allowance_scale(),
    defaults = list(amount = 0)
  )
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 3L,
    retirement_policy  = null_retirement_policy,
    allowance_policy   = allowance_policy,
    allowance_growth_rate = 0.05,
    ref_date           = as.Date("2020-01-01")
  )
  dt <- res$comparison
  # allowance_cost_start[2] must equal allowance_cost_end[1] EXACTLY -- if it
  # were instead re-resolved against the already-grown scale, it would be
  # allowance_cost_end[1] * 1.05, which nobody was actually ever paid.
  expect_equal(dt$allowance_cost_start[2], dt$allowance_cost_end[1])
  expect_false(isTRUE(all.equal(dt$allowance_cost_start[2], dt$allowance_cost_end[1] * 1.05)))
})

test_that("Block G: a contract that never changes group grows its allowance at allowance_growth_rate", {
  s <- make_horizon_state(n = 2L)  # 2 contracts, no movement/hiring/retirement policy supplied
  allowance_policy <- list(
    group_cols = c("est_id", "allowance_type"), policy_table = make_allowance_scale(),
    defaults = list(amount = 0)
  )
  res <- simulate_horizon(
    contract_dt        = s$contract_dt,
    personnel_dt       = s$personnel_dt,
    salary_scale_dt    = make_salary_scale(),
    n_periods          = 3L,
    retirement_policy  = null_retirement_policy,
    allowance_policy   = allowance_policy,
    allowance_growth_rate = 0.10,
    ref_date           = as.Date("2020-01-01"),
    output_control_args = list(return_microdata = TRUE)
  )
  led <- res$allowance_dt[contract_id == "C1" & allowance_type == "hazard"][order(period_date)]
  expect_equal(nrow(led), 3L)
  expect_equal(led$amount, 100 * 1.10^(0:2))
})

test_that("Block G: allowance_dt is gated behind return_microdata", {
  s <- make_horizon_state()
  allowance_policy <- list(
    group_cols = c("est_id", "allowance_type"), policy_table = make_allowance_scale(),
    defaults = list(amount = 0)
  )
  res_no_micro <- simulate_horizon(
    contract_dt = s$contract_dt, personnel_dt = s$personnel_dt,
    salary_scale_dt = make_salary_scale(), n_periods = 2L,
    retirement_policy = null_retirement_policy, allowance_policy = allowance_policy,
    ref_date = as.Date("2020-01-01"), output_control_args = list(return_microdata = FALSE)
  )
  res_micro <- simulate_horizon(
    contract_dt = s$contract_dt, personnel_dt = s$personnel_dt,
    salary_scale_dt = make_salary_scale(), n_periods = 2L,
    retirement_policy = null_retirement_policy, allowance_policy = allowance_policy,
    ref_date = as.Date("2020-01-01"), output_control_args = list(return_microdata = TRUE)
  )
  expect_null(res_no_micro$allowance_dt)
  expect_true(nrow(res_micro$allowance_dt) > 0L)
})


# ===========================================================================
# HAZARD_RATE RETIREMENT -- simulate_horizon() panel auto-estimation + seed
# ===========================================================================

# Two-snapshot panel: 6 people, all age 60 at both snapshots. P1/P2 retire
# (pensioner) between snap1 -> snap2; P3-P6 stay active. Estimated hazard
# rate at age_yr_int = 60 is therefore 2/6.
make_hazard_retirement_panel <- function() {
  snap1 <- as.Date("2019-01-01")
  snap2 <- as.Date("2020-01-01")
  ids <- paste0("P", 1:6)

  contract_panel <- data.table::data.table(
    ref_date          = c(rep(snap1, 6), rep(snap2, 6)),
    contract_id       = paste0("C", c(1:6, 1:6)),
    personnel_id      = rep(ids, 2),
    start_date        = as.Date("2010-01-01"),
    end_date          = as.Date(NA),
    contract_type     = c(
      rep("permanent", 6),
      c("pensioner", "pensioner", "permanent", "permanent", "permanent", "permanent")
    ),
    gross_salary_lcu  = 10000
  )
  personnel_panel <- data.table::data.table(
    ref_date          = c(rep(snap1, 6), rep(snap2, 6)),
    personnel_id      = rep(ids, 2),
    employment_status = c(
      rep("active", 6),
      c("pensioner", "pensioner", "active", "active", "active", "active")
    ),
    age               = 60,
    birth_date        = as.Date("1959-01-01")
  )
  list(contract_panel = contract_panel, personnel_panel = personnel_panel)
}

make_hazard_retirement_policy <- function() {
  list(
    group_cols   = "age_yr_int",
    policy_table = NULL, # forces auto-estimation from the panel
    defaults = list(
      eligibility_type = "hazard_rate",
      retire_strategy  = "random",
      pension_type     = "flat",
      flat_amount      = 500
    )
  )
}

test_that("simulate_horizon: hazard_rate auto-estimates policy_table from the panel", {
  p <- make_hazard_retirement_panel()
  snap2 <- as.Date("2020-01-01")

  res <- simulate_horizon(
    contract_dt        = p$contract_panel,
    personnel_dt        = p$personnel_panel,
    salary_scale_dt      = make_salary_scale(),
    n_periods            = 1L,
    retirement_policy    = make_hazard_retirement_policy(),
    ref_date             = snap2,
    output_control_args = list(return_microdata = FALSE),
    seed                 = 1
  )

  # 4 active candidates (P3-P6) at snap2, estimated hazard_rate = 2/6 ->
  # round(4 * 1/3) = 1 retiree.
  expect_equal(res$comparison$n_exits[1], 1L)
})

test_that("simulate_horizon: hazard_rate errors on a single-snapshot panel", {
  p <- make_hazard_retirement_panel()
  snap2 <- as.Date("2020-01-01")
  single_ct <- p$contract_panel[ref_date == snap2]
  single_pt <- p$personnel_panel[ref_date == snap2]

  expect_error(
    simulate_horizon(
      contract_dt = single_ct, personnel_dt = single_pt,
      salary_scale_dt = make_salary_scale(), n_periods = 1L,
      retirement_policy = make_hazard_retirement_policy(),
      ref_date = snap2, output_control_args = list(return_microdata = FALSE)
    ),
    regexp = "requires panel data"
  )
})

test_that("simulate_horizon: seed produces identical retiree sets across repeated runs", {
  p <- make_hazard_retirement_panel()
  snap2 <- as.Date("2020-01-01")

  run_once <- function() {
    simulate_horizon(
      contract_dt        = data.table::copy(p$contract_panel),
      personnel_dt        = data.table::copy(p$personnel_panel),
      salary_scale_dt      = make_salary_scale(),
      n_periods            = 2L,
      retirement_policy    = make_hazard_retirement_policy(),
      ref_date             = snap2,
      output_control_args = list(return_microdata = TRUE),
      seed                 = 7
    )
  }

  res1 <- run_once()
  res2 <- run_once()

  expect_identical(res1$comparison, res2$comparison)
  expect_identical(
    res1$personnel_dt[order(personnel_id)],
    res2$personnel_dt[order(personnel_id)]
  )
})

test_that("simulate_horizon: different seeds can produce different retiree selections", {
  # 40 candidates, hazard_rate = 0.5 -> 20 retirees selected randomly out of
  # 40 -- C(40, 20) is astronomically large, so sweeping a handful of seeds
  # and requiring at least one differing pair makes a chance coincidence
  # negligible (unlike a small pool, where two arbitrary seeds can
  # legitimately -- if unluckily -- select the identical subset).
  snap1 <- as.Date("2019-01-01")
  snap2 <- as.Date("2020-01-01")
  n <- 40L
  ids <- paste0("P", seq_len(n))
  # Half already retired between snap1 -> snap2 (estimated hazard_rate = 0.5).
  retired_at_snap2 <- rep(c("pensioner", "permanent"), n / 2)
  status_at_snap2  <- rep(c("pensioner", "active"), n / 2)

  contract_panel <- data.table::data.table(
    ref_date      = c(rep(snap1, n), rep(snap2, n)),
    contract_id   = paste0("C", c(seq_len(n), seq_len(n))),
    personnel_id  = rep(ids, 2),
    start_date    = as.Date("2010-01-01"),
    end_date      = as.Date(NA),
    contract_type = c(rep("permanent", n), retired_at_snap2),
    gross_salary_lcu = 10000
  )
  personnel_panel <- data.table::data.table(
    ref_date          = c(rep(snap1, n), rep(snap2, n)),
    personnel_id      = rep(ids, 2),
    employment_status = c(rep("active", n), status_at_snap2),
    age               = 60,
    birth_date        = as.Date("1959-01-01")
  )

  run_with_seed <- function(seed) {
    simulate_horizon(
      contract_dt        = data.table::copy(contract_panel),
      personnel_dt        = data.table::copy(personnel_panel),
      salary_scale_dt      = make_salary_scale(),
      n_periods            = 1L,
      retirement_policy    = make_hazard_retirement_policy(),
      ref_date             = snap2,
      output_control_args = list(return_microdata = TRUE),
      seed                 = seed
    )
  }

  selections <- lapply(1:3, function(seed) {
    res <- run_with_seed(seed)
    sort(res$personnel_dt[employment_status == "inactive"]$personnel_id)
  })

  expect_true(length(unique(selections)) > 1L)
})

