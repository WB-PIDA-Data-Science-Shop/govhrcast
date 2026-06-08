# # tests/testthat/test-generate_scenario_matrix.R
# # ============================================================
# # Tests for generate_scenario_matrix()
# # ============================================================

# # ---------------------------------------------------------------------------
# # Shared fixtures (reused from test-simulate_horizon.R pattern)
# # ---------------------------------------------------------------------------

# make_gsm_state <- function(n = 4L, salary = 10000,
#                             ref_date = as.Date("2020-01-01")) {
#   contract_dt <- data.table::data.table(
#     contract_id        = paste0("C", seq_len(n)),
#     personnel_id       = paste0("P", seq_len(n)),
#     est_id             = rep(c("E1", "E2"), length.out = n),
#     start_date         = ref_date - 365L * 10L,
#     end_date           = as.Date(NA),
#     contract_type_code = "permanent",
#     gross_salary_lcu   = as.numeric(rep(salary, n))
#   )
#   personnel_dt <- data.table::data.table(
#     personnel_id = paste0("P", seq_len(n)),
#     birth_date   = ref_date - 365L * 40L,
#     status       = "active",
#     age          = 40,
#     tenure_years = 10
#   )
#   list(contract_dt = contract_dt, personnel_dt = personnel_dt)
# }

# make_gsm_scale <- function() {
#   data.table::data.table(
#     est_id           = c("E1", "E2"),
#     gross_salary_lcu = c(10000, 10000)
#   )
# }

# # Null policies that produce no effects (for clean isolation)
# gsm_ret_policy <- list(
#   defaults = list(
#     eligibility_type = "age_only",
#     min_age          = 999,
#     pension_type     = "flat",
#     flat_amount      = 1000
#   )
# )

# gsm_mov_policy <- list(
#   group_cols   = "est_id",
#   policy_table = NULL,
#   defaults = list(
#     movement_rate      = 0,
#     movement_strategy  = "tenure",
#     active_types       = "permanent",
#     salary_update_rule = "scale"
#   )
# )

# gsm_hire_policy <- list(
#   mode          = "stock",
#   group_cols    = "est_id",
#   stock_targets = data.table::data.table(est_id = c("E1", "E2"), target_stock = 2L),
#   salary_scale  = make_gsm_scale()
# )


# # ===========================================================================
# # INPUT VALIDATION
# # ===========================================================================

# test_that("generate_scenario_matrix errors on empty param_grid", {
#   s <- make_gsm_state()
#   expect_error(
#     generate_scenario_matrix(
#       contract_dt     = s$contract_dt,
#       personnel_dt    = s$personnel_dt,
#       salary_scale_dt = make_gsm_scale(),
#       param_grid      = list(),
#       n_periods       = 1L,
#       ref_date        = as.Date("2020-01-01")
#     ),
#     "non-empty"
#   )
# })

# test_that("generate_scenario_matrix errors on unnamed param_grid elements", {
#   s <- make_gsm_state()
#   expect_error(
#     generate_scenario_matrix(
#       contract_dt     = s$contract_dt,
#       personnel_dt    = s$personnel_dt,
#       salary_scale_dt = make_gsm_scale(),
#       param_grid      = list(0.02, 0.05),   # no names
#       n_periods       = 1L,
#       ref_date        = as.Date("2020-01-01")
#     ),
#     "named"
#   )
# })

# test_that("generate_scenario_matrix errors on invalid n_periods", {
#   s <- make_gsm_state()
#   expect_error(
#     generate_scenario_matrix(
#       contract_dt     = s$contract_dt,
#       personnel_dt    = s$personnel_dt,
#       salary_scale_dt = make_gsm_scale(),
#       param_grid      = list(salary_growth_rate = 0.02),
#       n_periods       = 0L,
#       ref_date        = as.Date("2020-01-01")
#     ),
#     "positive integer"
#   )
# })


# # ===========================================================================
# # GRID EXPANSION
# # ===========================================================================

# test_that("output has n_scenarios * n_periods rows", {
#   set.seed(1)
#   s <- make_gsm_state()
#   grid <- list(salary_growth_rate = c(0.02, 0.05))  # 2 scenarios

#   out <- generate_scenario_matrix(
#     contract_dt       = s$contract_dt,
#     personnel_dt      = s$personnel_dt,
#     salary_scale_dt   = make_gsm_scale(),
#     param_grid        = grid,
#     n_periods         = 2L,
#     retirement_policy = gsm_ret_policy,
#     movement_policy   = gsm_mov_policy,
#     hiring_policy     = gsm_hire_policy,
#     ref_date          = as.Date("2020-01-01")
#   )
#   expect_equal(nrow(out), 2L * 2L)  # 2 scenarios × 2 periods
# })

# test_that("scenario_id ranges from 1 to n_scenarios", {
#   set.seed(2)
#   s <- make_gsm_state()
#   grid <- list(salary_growth_rate = c(0, 0.03, 0.06))  # 3 scenarios

#   out <- generate_scenario_matrix(
#     contract_dt       = s$contract_dt,
#     personnel_dt      = s$personnel_dt,
#     salary_scale_dt   = make_gsm_scale(),
#     param_grid        = grid,
#     n_periods         = 1L,
#     ref_date          = as.Date("2020-01-01")
#   )
#   expect_equal(sort(unique(out$scenario_id)), 1:3)
# })

# test_that("CJ produces all combinations across two grid axes", {
#   set.seed(3)
#   s <- make_gsm_state()
#   grid <- list(
#     salary_growth_rate = c(0.02, 0.05),
#     retirement_min_age = c(60, 65)
#   )  # 2 × 2 = 4 scenarios

#   out <- generate_scenario_matrix(
#     contract_dt       = s$contract_dt,
#     personnel_dt      = s$personnel_dt,
#     salary_scale_dt   = make_gsm_scale(),
#     param_grid        = grid,
#     n_periods         = 1L,
#     retirement_policy = gsm_ret_policy,
#     ref_date          = as.Date("2020-01-01")
#   )
#   expect_equal(data.table::uniqueN(out$scenario_id), 4L)
#   # Both lever columns present in output
#   expect_true("salary_growth_rate" %in% names(out))
#   expect_true("retirement_min_age" %in% names(out))
# })


# # ===========================================================================
# # RETURN STRUCTURE
# # ===========================================================================

# test_that("output is a data.table", {
#   set.seed(4)
#   s <- make_gsm_state()
#   out <- generate_scenario_matrix(
#     contract_dt     = s$contract_dt,
#     personnel_dt    = s$personnel_dt,
#     salary_scale_dt = make_gsm_scale(),
#     param_grid      = list(salary_growth_rate = 0.03),
#     n_periods       = 1L,
#     ref_date        = as.Date("2020-01-01")
#   )
#   expect_true(data.table::is.data.table(out))
# })

# test_that("output has required metadata and time-series columns", {
#   set.seed(5)
#   s <- make_gsm_state()
#   out <- generate_scenario_matrix(
#     contract_dt     = s$contract_dt,
#     personnel_dt    = s$personnel_dt,
#     salary_scale_dt = make_gsm_scale(),
#     param_grid      = list(salary_growth_rate = 0.03),
#     n_periods       = 1L,
#     ref_date        = as.Date("2020-01-01")
#   )
#   required <- c("scenario_id", "scenario_label", "is_baseline",
#                 "period_date", "wage_bill_start", "wage_bill_end",
#                 "n_exits", "exit_savings", "pension_cost_new", "pension_cost_total",
#                 "n_promotions", "n_transfers", "promotion_effect", "transfer_effect",
#                 "n_hires", "hiring_effect", "inflation_effect",
#                 "n_headcount_start", "n_headcount_end")
#   expect_true(all(required %in% names(out)))
# })

# test_that("microdata columns are NOT present in output", {
#   set.seed(6)
#   s <- make_gsm_state()
#   out <- generate_scenario_matrix(
#     contract_dt     = s$contract_dt,
#     personnel_dt    = s$personnel_dt,
#     salary_scale_dt = make_gsm_scale(),
#     param_grid      = list(salary_growth_rate = 0.03),
#     n_periods       = 1L,
#     ref_date        = as.Date("2020-01-01")
#   )
#   expect_false("contract_dt"  %in% names(out))
#   expect_false("personnel_dt" %in% names(out))
# })


# # ===========================================================================
# # SCENARIO LABELING
# # ===========================================================================

# test_that("scenario_label is a non-empty character string", {
#   set.seed(7)
#   s <- make_gsm_state()
#   out <- generate_scenario_matrix(
#     contract_dt     = s$contract_dt,
#     personnel_dt    = s$personnel_dt,
#     salary_scale_dt = make_gsm_scale(),
#     param_grid      = list(salary_growth_rate = c(0.02, 0.05)),
#     n_periods       = 1L,
#     ref_date        = as.Date("2020-01-01")
#   )
#   labels <- unique(out$scenario_label)
#   expect_equal(length(labels), 2L)
#   expect_true(all(nchar(labels) > 0L))
#   expect_true(is.character(out$scenario_label))
# })

# test_that("scenario_labels differ across different grid combinations", {
#   set.seed(8)
#   s <- make_gsm_state()
#   out <- generate_scenario_matrix(
#     contract_dt     = s$contract_dt,
#     personnel_dt    = s$personnel_dt,
#     salary_scale_dt = make_gsm_scale(),
#     param_grid      = list(salary_growth_rate = c(0.02, 0.05, 0.10)),
#     n_periods       = 1L,
#     ref_date        = as.Date("2020-01-01")
#   )
#   expect_equal(data.table::uniqueN(out$scenario_label), 3L)
# })


# # ===========================================================================
# # BASELINE FLAG
# # ===========================================================================

# test_that("exactly one scenario is flagged is_baseline = TRUE (default = scenario 1)", {
#   set.seed(9)
#   s <- make_gsm_state()
#   out <- generate_scenario_matrix(
#     contract_dt     = s$contract_dt,
#     personnel_dt    = s$personnel_dt,
#     salary_scale_dt = make_gsm_scale(),
#     param_grid      = list(salary_growth_rate = c(0, 0.03, 0.06)),
#     n_periods       = 2L,
#     ref_date        = as.Date("2020-01-01")
#   )
#   baseline_scenarios <- unique(out[is_baseline == TRUE, scenario_id])
#   expect_equal(length(baseline_scenarios), 1L)
#   expect_equal(baseline_scenarios, 1L)
# })

# test_that("baseline_scenario_id overrides default baseline", {
#   set.seed(10)
#   s <- make_gsm_state()
#   out <- generate_scenario_matrix(
#     contract_dt          = s$contract_dt,
#     personnel_dt         = s$personnel_dt,
#     salary_scale_dt      = make_gsm_scale(),
#     param_grid           = list(salary_growth_rate = c(0, 0.03, 0.06)),
#     n_periods            = 1L,
#     baseline_scenario_id = 2L,
#     ref_date             = as.Date("2020-01-01")
#   )
#   baseline_scenarios <- unique(out[is_baseline == TRUE, scenario_id])
#   expect_equal(baseline_scenarios, 2L)
# })


# # ===========================================================================
# # LEVER MAPPING
# # ===========================================================================

# test_that("retirement_ prefix maps to retirement_policy sub-key", {
#   set.seed(11)
#   s <- make_gsm_state()
#   # retirement_min_age = 999 forces 0 retirees; = 0 forces all to retire
#   # We just verify the lever is accepted and the run completes
#   expect_no_error(
#     generate_scenario_matrix(
#       contract_dt       = s$contract_dt,
#       personnel_dt      = s$personnel_dt,
#       salary_scale_dt   = make_gsm_scale(),
#       param_grid        = list(retirement_min_age = c(999, 998)),
#       n_periods         = 1L,
#       retirement_policy = gsm_ret_policy,
#       ref_date          = as.Date("2020-01-01")
#     )
#   )
# })

# test_that("movement_ prefix maps to movement_policy sub-key", {
#   set.seed(12)
#   s <- make_gsm_state()
#   # Inject an arbitrary key via movement_ prefix; unknown keys are silently ignored
#   expect_no_error(
#     generate_scenario_matrix(
#       contract_dt     = s$contract_dt,
#       personnel_dt    = s$personnel_dt,
#       salary_scale_dt = make_gsm_scale(),
#       param_grid      = list(movement_group_cols = "est_id"),
#       n_periods       = 1L,
#       movement_policy = gsm_mov_policy,
#       ref_date        = as.Date("2020-01-01")
#     )
#   )
# })

# test_that("hiring_ prefix maps to hiring_policy sub-key", {
#   set.seed(13)
#   s <- make_gsm_state()
#   expect_no_error(
#     generate_scenario_matrix(
#       contract_dt     = s$contract_dt,
#       personnel_dt    = s$personnel_dt,
#       salary_scale_dt = make_gsm_scale(),
#       param_grid      = list(hiring_mode = c("stock")),
#       n_periods       = 1L,
#       hiring_policy   = gsm_hire_policy,
#       ref_date        = as.Date("2020-01-01")
#     )
#   )
# })

# test_that("n_periods as a grid lever varies horizon length per scenario", {
#   set.seed(14)
#   s <- make_gsm_state()
#   out <- generate_scenario_matrix(
#     contract_dt     = s$contract_dt,
#     personnel_dt    = s$personnel_dt,
#     salary_scale_dt = make_gsm_scale(),
#     param_grid      = list(n_periods = c(1L, 3L)),
#     n_periods       = 1L,   # base value overridden by grid
#     ref_date        = as.Date("2020-01-01")
#   )
#   rows_per_scenario <- out[, .N, by = scenario_id]
#   expect_equal(sort(rows_per_scenario$N), c(1L, 3L))
# })


# # ===========================================================================
# # ACCOUNTING IDENTITY
# # ===========================================================================

# test_that("WAGE BILL STATE: wage_bill_start[2] == wage_bill_end[1] within scenario", {
#   set.seed(20)
#   s <- make_gsm_state(n = 6L, salary = 8000)
#   out <- generate_scenario_matrix(
#     contract_dt       = s$contract_dt,
#     personnel_dt      = s$personnel_dt,
#     salary_scale_dt   = make_gsm_scale(),
#     param_grid        = list(salary_growth_rate = c(0, 0.05, 0.10)),
#     n_periods         = 2L,
#     retirement_policy = gsm_ret_policy,
#     movement_policy   = gsm_mov_policy,
#     hiring_policy     = NULL,
#     ref_date          = as.Date("2020-01-01")
#   )
#   # Within each scenario, period 2 start bill should equal period 1 end bill
#   for (sid in unique(out$scenario_id)) {
#     sc <- out[scenario_id == sid]
#     if (nrow(sc) == 2L) {
#       expect_equal(sc$wage_bill_start[2], sc$wage_bill_end[1], tolerance = 1e-4,
#                    label = paste0("scenario ", sid, ": state threading"))
#     }
#   }
# })


# # ===========================================================================
# # SINGLE-SCENARIO GRID
# # ===========================================================================

# test_that("single-scenario grid returns n_periods rows", {
#   set.seed(21)
#   s <- make_gsm_state()
#   out <- generate_scenario_matrix(
#     contract_dt     = s$contract_dt,
#     personnel_dt    = s$personnel_dt,
#     salary_scale_dt = make_gsm_scale(),
#     param_grid      = list(salary_growth_rate = 0.03),
#     n_periods       = 3L,
#     ref_date        = as.Date("2020-01-01")
#   )
#   expect_equal(nrow(out), 3L)
#   expect_equal(data.table::uniqueN(out$scenario_id), 1L)
# })


# # ===========================================================================
# # OUTPUT IS KEYED BY (scenario_id, period_date)
# # ===========================================================================

# test_that("output is keyed by (scenario_id, period_date)", {
#   set.seed(22)
#   s <- make_gsm_state()
#   out <- generate_scenario_matrix(
#     contract_dt     = s$contract_dt,
#     personnel_dt    = s$personnel_dt,
#     salary_scale_dt = make_gsm_scale(),
#     param_grid      = list(salary_growth_rate = c(0.02, 0.04)),
#     n_periods       = 2L,
#     ref_date        = as.Date("2020-01-01")
#   )
#   expect_equal(data.table::key(out), c("scenario_id", "period_date"))
# })


# # ===========================================================================
# # VALIDATE_IDENTITY WARNING
# # ===========================================================================

# test_that("output does NOT have old-API column names", {
#   set.seed(23)
#   s <- make_gsm_state(n = 4L, salary = 5000)
#   out <- generate_scenario_matrix(
#     contract_dt       = s$contract_dt,
#     personnel_dt      = s$personnel_dt,
#     salary_scale_dt   = make_gsm_scale(),
#     param_grid        = list(salary_growth_rate = 0),
#     n_periods         = 1L,
#     retirement_policy = gsm_ret_policy,
#     movement_policy   = gsm_mov_policy,
#     hiring_policy     = gsm_hire_policy,
#     ref_date          = as.Date("2020-01-01")
#   )
#   old_cols <- c("base_bill", "total_wage_bill", "total_change",
#                 "exit_savings_pct", "promotion_effect_pct")
#   for (col in old_cols) {
#     expect_false(col %in% names(out),
#                  label = paste("old column should not exist:", col))
#   }
# })


# # ===========================================================================
# # PARALLEL / SEQUENTIAL PARITY
# # ===========================================================================

# test_that("sequential results match across identical seeds", {
#   # Two independent sequential runs with the same seed should be identical
#   # (stochastic rounding is seeded)
#   s <- make_gsm_state(n = 4L, salary = 10000)
#   grid <- list(salary_growth_rate = c(0, 0.03))

#   run <- function() {
#     set.seed(42)
#     generate_scenario_matrix(
#       contract_dt       = s$contract_dt,
#       personnel_dt      = s$personnel_dt,
#       salary_scale_dt   = make_gsm_scale(),
#       param_grid        = grid,
#       n_periods         = 2L,
#       retirement_policy = gsm_ret_policy,
#       movement_policy   = gsm_mov_policy,
#       hiring_policy     = gsm_hire_policy,
#       ref_date          = as.Date("2020-01-01")
#     )
#   }
#   r1 <- run()
#   r2 <- run()
#   expect_equal(r1$wage_bill_end, r2$wage_bill_end)
# })
