# The tables simulate_horizon() returns continue your data (ADR 0017): up to the
# baseline they are your own rows; after it, each year reads like a govhr snapshot,
# so govhr and govhrapp give the same numbers on history and carry on through the
# projection.

start <- as.Date("2012-09-01")
years <- seq(start, by = "year", length.out = 4L)[-1]   # the three simulated snapshots

continuity_run <- local({
  res <- NULL
  function() {
    if (is.null(res)) {
      hc <- bra_hrmis_contract[ref_date <= start]
      hp <- bra_hrmis_personnel[ref_date <= start]
      scale <- hc[ref_date == start & !is.na(gross_salary_lcu),
                  .(gross_salary_lcu = mean(gross_salary_lcu)), by = est_id]
      res <<- suppressMessages(suppressWarnings(simulate_horizon(
        contract_dt         = hc,
        personnel_dt        = hp,
        salary_scale_dt     = scale,
        n_periods           = 3L,
        baseline_date       = start,
        retirement_policy   = design_retirement_policy_status_quo(
          pension = pension_db(accrual_rate = 0.02, max_years = 35, replacement_cap = 0.80)),
        exit_policy         = design_exit_policy_status_quo(group_cols = "est_id"),
        hiring_policy       = design_hiring_policy_status_quo(group_cols = "est_id"),
        pension_cola_rate   = 0.05,
        output_control_args = define_output_control(return_microdata = TRUE),
        seed                = 1L
      )))
    }
    res
  }
})

sorted <- function(x) data.table::setorderv(data.table::copy(data.table::as.data.table(x)), names(x))

test_that("up to the baseline, the returned tables are your own rows, unchanged", {
  res <- continuity_run()
  expect_identical(names(res$contract_dt), names(bra_hrmis_contract))
  expect_identical(names(res$personnel_dt), names(bra_hrmis_personnel))
  expect_equal(sorted(res$contract_dt[ref_date <= start]),
               sorted(bra_hrmis_contract[ref_date <= start]), ignore_attr = TRUE)
  expect_equal(sorted(res$personnel_dt[ref_date <= start]),
               sorted(bra_hrmis_personnel[ref_date <= start]), ignore_attr = TRUE)
})

test_that("govhr's wage bill analytics give the same numbers on history as on your data", {
  res <- continuity_run()
  est <- unique(bra_hrmis_contract[, .(est_id)])[, country_code := "BRA"]
  on_input <- suppressWarnings(govhr::compute_wagebill_analytics(
    bra_hrmis_contract[ref_date <= start], bra_hrmis_personnel[ref_date <= start], est))
  on_output <- suppressWarnings(govhr::compute_wagebill_analytics(
    res$contract_dt, res$personnel_dt, est))
  for (part in c("wagebill", "wagebill_by_est", "wage", "wage_by_est", "composition")) {
    a <- data.table::as.data.table(on_output[[part]])[ref_date <= start]
    b <- data.table::as.data.table(on_input[[part]])
    expect_equal(sorted(a), sorted(b), ignore_attr = TRUE, info = part)
  }
})

test_that("each simulated year is dated a year after the one before, starting after the baseline", {
  res <- continuity_run()
  expect_equal(res$comparison$period_date, years)
  expect_equal(sort(unique(res$contract_dt[ref_date > start, ref_date])), years)
  expect_equal(sort(unique(res$personnel_dt[ref_date > start, ref_date])), years)
  expect_equal(anyDuplicated(res$contract_dt[ref_date > start], by = c("contract_id", "ref_date")), 0L)
  expect_equal(anyDuplicated(res$personnel_dt[ref_date > start], by = c("personnel_id", "ref_date")), 0L)
})

test_that("people already retired at the baseline carry on as pensioners, their pensions growing by the COLA", {
  res <- continuity_run()
  base_p <- bra_hrmis_personnel[ref_date == start]
  base_c <- bra_hrmis_contract[ref_date == start]
  pensioners <- base_p[employment_status %in% "pensioner", personnel_id]
  expect_gt(length(pensioners), 0L)
  for (t in seq_along(years)) {
    yp <- res$personnel_dt[ref_date == years[t] & personnel_id %in% pensioners]
    expect_setequal(yp$personnel_id, pensioners)
    expect_true(all(yp$employment_status == "pensioner"))
    aged <- yp[base_p[personnel_id %in% pensioners, .(personnel_id, age0 = age)], on = "personnel_id"]
    expect_equal(aged$age, aged$age0 + t)

    was <- base_c[personnel_id %in% pensioners, .(contract_id, pay0 = gross_salary_lcu)]
    now <- res$contract_dt[ref_date == years[t] & personnel_id %in% pensioners, .(contract_id, gross_salary_lcu)]
    both <- now[was, on = "contract_id"]
    expect_equal(nrow(now), nrow(was))
    expect_equal(both$gross_salary_lcu, both$pay0 * 1.05^t)
  }
})

test_that("people retired in the projection turn from active to pensioner, with their pension as pay", {
  res <- continuity_run()
  reg <- res$pensioner_register
  base_ids <- reg[ref_date == years[1] & personnel_id %in%
                    bra_hrmis_personnel[ref_date == start & employment_status %in% "pensioner", personnel_id],
                  unique(personnel_id)]
  first <- reg[!personnel_id %in% base_ids, .(ref_date = min(ref_date)), by = personnel_id]
  expect_gt(nrow(first), 0L)
  dates <- c(start, years)
  for (i in seq_len(nrow(first))) {
    id <- first$personnel_id[i]; d <- first$ref_date[i]; before <- dates[match(d, dates) - 1L]
    expect_identical(res$personnel_dt[personnel_id == id & ref_date == before, employment_status], "active")
    expect_identical(res$personnel_dt[personnel_id == id & ref_date == d, employment_status], "pensioner")
    pension <- reg[personnel_id == id & ref_date == d]
    row <- res$contract_dt[contract_id == pension$contract_id & ref_date == d]
    expect_identical(row$contract_type, "pensioner")
    expect_equal(row$gross_salary_lcu, pension$pension_amount)
  }
})

test_that("govhr counts the simulated retirements and exits, year by year, as the summary does", {
  res <- continuity_run()
  people <- res$personnel_dt
  flows <- govhr::detect_movement(people)
  dates <- c(start, years)
  for (t in seq_along(years)) {
    active_before <- people[ref_date == dates[t] & employment_status %in% "active", personnel_id]
    active_after  <- people[ref_date == dates[t + 1L] & employment_status %in% "active", personnel_id]
    n_left <- length(setdiff(active_before, active_after))
    n_separations <- flows[ref_date == dates[t] & separation %in% TRUE, data.table::uniqueN(personnel_id)]
    expect_equal(n_separations, res$comparison$n_non_ret_exits[t], info = format(years[t]))
    expect_equal(n_left - n_separations, res$comparison$n_exits[t], info = format(years[t]))
  }
})

test_that("people who leave are absent from the following years, as in government data", {
  res <- continuity_run()
  expect_false(any(res$personnel_dt[ref_date > start, employment_status] %in% "inactive"))
  expect_false(any(res$contract_dt[ref_date > start, contract_type] %in% "inactive"))
})

test_that("the pensioner register holds each pension contract's amount for every year", {
  res <- continuity_run()
  reg <- res$pensioner_register
  expect_identical(names(reg), c("personnel_id", "contract_id", "ref_date", "pension_amount"))
  expect_equal(sort(unique(reg$ref_date)), years)
  expect_equal(anyDuplicated(reg, by = c("contract_id", "ref_date")), 0L)
  by_year <- reg[, .(total = sum(pension_amount, na.rm = TRUE)), keyby = ref_date]
  expect_equal(by_year$total, res$comparison$pension_cost_total)
})
