# tests/testthat/test-hazard_models_utils.R
# Tests for build_retirement_hazard_data(), fit_hazard_model(),
# and select_hazard_threshold()

# =============================================================================
# Helper: synthetic panel datasets for build_retirement_hazard_data()
# =============================================================================

make_panel <- function(n_persons = 50L, n_snaps = 3L, seed = 1L) {
  set.seed(seed)

  persons    <- paste0("P", seq_len(n_persons))
  snap_dates <- as.Date("2019-01-01") + (seq_len(n_snaps) - 1L) * 365L

  # personnel panel: one row per person per snapshot
  personnel_rows <- lapply(snap_dates, function(d) {
    data.table::data.table(
      personnel_id = persons,
      ref_date     = d,
      birth_date   = as.Date("1965-01-01") +
                       as.integer(stats::runif(n_persons, 0, 5 * 365)),
      status       = "active"
    )
  })
  panel_personnel_dt <- data.table::rbindlist(personnel_rows)

  # contract panel: 10% of persons become pensioner at the last snapshot
  contract_rows <- lapply(seq_along(snap_dates), function(i) {
    d    <- snap_dates[[i]]
    type <- rep("permanent", n_persons)
    if (i == n_snaps) {
      retire_idx  <- seq_len(max(1L, round(n_persons * 0.1)))
      type[retire_idx] <- "pensioner"
    }
    data.table::data.table(
      personnel_id       = persons,
      contract_id        = paste0("C", seq_len(n_persons), "_", i),
      ref_date           = d,
      contract_type_code = type,
      start_date         = as.Date("2010-01-01"),
      end_date           = as.Date("2030-12-31"),
      paygrade           = sample(c("A", "B", "C"), n_persons, replace = TRUE),
      gross_salary_lcu   = round(stats::runif(n_persons, 30000, 80000))
    )
  })
  panel_contract_dt <- data.table::rbindlist(contract_rows)

  list(
    panel_contract_dt  = panel_contract_dt,
    panel_personnel_dt = panel_personnel_dt
  )
}

# =============================================================================
# build_retirement_hazard_data — structure tests
# =============================================================================

# =============================================================================
# Helper: synthetic person-period dataset
# =============================================================================

make_reg_dt <- function(n = 200L, seed = 42L) {
  set.seed(seed)
  age          <- round(stats::runif(n, 45, 65))
  tenure_years <- round(stats::runif(n, 5, 35))
  # log-odds increases with age and tenure
  log_odds     <- -8 + 0.08 * age + 0.05 * tenure_years
  prob         <- 1 / (1 + exp(-log_odds))
  outcome      <- as.integer(stats::rbinom(n, 1L, prob))
  data.frame(
    retired      = outcome,
    age          = age,
    tenure_years = tenure_years,
    paygrade     = sample(c("A", "B", "C"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# fit_hazard_model — structure tests
# =============================================================================

test_that("fit_hazard_model returns a hazard_model with correct names", {
  dt <- make_reg_dt()
  hm <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))

  expect_s3_class(hm, "hazard_model")
  expect_named(hm, c("model", "outcome_col", "threshold"),
               ignore.order = TRUE)
})

test_that("fit_hazard_model model slot is a glm", {
  dt <- make_reg_dt()
  hm <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  expect_s3_class(hm$model, "glm")
})

test_that("fit_hazard_model records outcome_col; covariates recoverable from formula", {
  dt  <- make_reg_dt()
  hm  <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  expect_identical(hm$outcome_col, "retired")
  # covariates live in the model formula
  recovered_covs <- all.vars(stats::formula(hm$model))[-1L]
  expect_setequal(recovered_covs, c("age", "tenure_years"))
})

test_that("fit_hazard_model records family and link inside the glm object", {
  dt <- make_reg_dt()
  hm <- fit_hazard_model(dt, "retired", c("age", "tenure_years"),
                         family = binomial(link = "cloglog"))
  expect_identical(stats::family(hm$model)$family, "binomial")
  expect_identical(stats::family(hm$model)$link,   "cloglog")
})

test_that("fit_hazard_model n_obs and n_events accessible via model$y", {
  dt <- make_reg_dt(n = 100L)
  hm <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  expect_equal(length(hm$model$y), 100L)
  expect_equal(as.integer(sum(hm$model$y)), sum(dt$retired == 1L))
})

test_that("fit_hazard_model sets threshold to NA_real_", {
  dt <- make_reg_dt()
  hm <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  expect_true(is.na(hm$threshold))
  expect_type(hm$threshold, "double")
})

test_that("fit_hazard_model works with categorical covariates (factor)", {
  dt <- make_reg_dt()
  # Should not error when paygrade (char coerced to factor) is included
  expect_no_error(
    fit_hazard_model(dt, "retired", c("age", "paygrade"))
  )
})

test_that("fit_hazard_model works with data.table input", {
  dt <- data.table::as.data.table(make_reg_dt())
  expect_no_error(
    fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  )
})

# =============================================================================
# fit_hazard_model — complete-case handling
# =============================================================================

test_that("fit_hazard_model drops rows with NA values silently", {
  dt         <- make_reg_dt(n = 100L)
  dt$age[1L] <- NA_real_   # introduce one incomplete row
  hm <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  # glm should have been fitted on 99 complete cases
  expect_equal(length(hm$model$y), 99L)
})

test_that("fit_hazard_model errors when no complete cases remain", {
  dt      <- make_reg_dt(n = 50L)
  dt$age  <- NA_real_   # all-NA covariate leaves no complete rows
  expect_error(
    fit_hazard_model(dt, "retired", c("age", "tenure_years")),
    regexp = "No complete cases"
  )
})

# =============================================================================
# fit_hazard_model — error conditions
# =============================================================================

test_that("fit_hazard_model errors when outcome_col missing", {
  dt <- make_reg_dt()
  expect_error(
    fit_hazard_model(dt, "not_a_col", c("age", "tenure_years")),
    regexp = "not found in reg_dt"
  )
})

test_that("fit_hazard_model errors when covariate missing", {
  dt <- make_reg_dt()
  expect_error(
    fit_hazard_model(dt, "retired", c("age", "nonexistent")),
    regexp = "Covariates not found"
  )
})

test_that("fit_hazard_model errors on non-binary outcome", {
  dt         <- make_reg_dt()
  dt$retired <- dt$retired + 2L   # values 2 and 3
  expect_error(
    fit_hazard_model(dt, "retired", c("age", "tenure_years")),
    regexp = "must contain only 0, 1"
  )
})

test_that("fit_hazard_model errors when outcome is entirely NA", {
  dt         <- make_reg_dt()
  dt$retired <- NA_integer_
  expect_error(
    fit_hazard_model(dt, "retired", c("age", "tenure_years")),
    regexp = "entirely NA"
  )
})

test_that("fit_hazard_model errors on non-data.frame input", {
  expect_error(
    fit_hazard_model(list(a = 1), "retired", "age"),
    regexp = "data.frame or data.table"
  )
})

# =============================================================================
# select_hazard_threshold — basic function
# =============================================================================

test_that("select_hazard_threshold returns updated hazard_model", {
  dt <- make_reg_dt()
  hm <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  hm2 <- select_hazard_threshold(hm, dt, method = "youden")
  expect_s3_class(hm2, "hazard_model")
})

test_that("select_hazard_threshold populates threshold in (0, 1)", {
  dt  <- make_reg_dt()
  hm  <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  hm2 <- select_hazard_threshold(hm, dt, method = "youden")
  expect_true(!is.na(hm2$threshold))
  expect_gt(hm2$threshold, 0)
  expect_lt(hm2$threshold, 1)
})

test_that("select_hazard_threshold records threshold_method", {
  dt  <- make_reg_dt()
  hm  <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  hm2 <- select_hazard_threshold(hm, dt, method = "youden")
  expect_identical(hm2$threshold_method, "youden")
})

test_that("select_hazard_threshold f1 method works", {
  dt  <- make_reg_dt()
  hm  <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  hm2 <- select_hazard_threshold(hm, dt, method = "f1")
  expect_identical(hm2$threshold_method, "f1")
  expect_true(!is.na(hm2$threshold))
  expect_gt(hm2$threshold, 0)
  expect_lt(hm2$threshold, 1)
})

test_that("select_hazard_threshold threshold_diagnostics is a data.table with 99 rows", {
  dt  <- make_reg_dt()
  hm  <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  hm2 <- select_hazard_threshold(hm, dt)
  expect_s3_class(hm2$threshold_diagnostics, "data.table")
  expect_equal(nrow(hm2$threshold_diagnostics), 99L)
})

test_that("select_hazard_threshold diagnostics contains expected columns", {
  dt  <- make_reg_dt()
  hm  <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  hm2 <- select_hazard_threshold(hm, dt)
  expect_true(all(c("threshold", "youden", "f1",
                    "sensitivity", "specificity",
                    "precision", "recall")
                  %in% names(hm2$threshold_diagnostics)))
})

test_that("youden and f1 methods can give different thresholds", {
  dt   <- make_reg_dt(n = 500L, seed = 7L)
  hm   <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  hm_y <- select_hazard_threshold(hm, dt, method = "youden")
  hm_f <- select_hazard_threshold(hm, dt, method = "f1")
  # We don't require they differ but both should be valid; just check both run
  expect_true(!is.na(hm_y$threshold))
  expect_true(!is.na(hm_f$threshold))
})

# =============================================================================
# select_hazard_threshold — error conditions
# =============================================================================

test_that("select_hazard_threshold errors on non-hazard_model input", {
  dt <- make_reg_dt()
  expect_error(
    select_hazard_threshold(list(model = NULL), dt),
    regexp = "fit_hazard_model"
  )
})

test_that("select_hazard_threshold errors on invalid method", {
  dt <- make_reg_dt()
  hm <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  expect_error(
    select_hazard_threshold(hm, dt, method = "roc"),
    regexp = "arg"
  )
})

test_that("select_hazard_threshold errors when outcome col missing in reg_dt", {
  dt <- make_reg_dt()
  hm <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  dt2 <- dt[, c("age", "tenure_years")]   # drop outcome
  expect_error(
    select_hazard_threshold(hm, dt2),
    regexp = "not found in reg_dt"
  )
})

test_that("select_hazard_threshold errors when covariate missing in reg_dt", {
  dt <- make_reg_dt()
  hm <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  dt2 <- dt[, c("retired", "age")]   # drop tenure_years
  expect_error(
    select_hazard_threshold(hm, dt2),
    regexp = "not found in reg_dt"
  )
})

test_that("select_hazard_threshold errors when outcome entirely NA", {
  dt         <- make_reg_dt()
  hm         <- fit_hazard_model(dt, "retired", c("age", "tenure_years"))
  dt$retired <- NA_integer_
  expect_error(
    select_hazard_threshold(hm, dt),
    regexp = "entirely NA"
  )
})

# =============================================================================
# Edge case: no events in the outcome column
# =============================================================================

test_that("fit_hazard_model accepts (but warns implicitly) all-zero outcome", {
  dt         <- make_reg_dt(n = 100L)
  dt$retired <- 0L   # no events
  # glm will produce a convergence warning; we suppress and just check structure
  expect_no_error(
    suppressWarnings(
      fit_hazard_model(dt, "retired", c("age", "tenure_years"))
    )
  )
})

# =============================================================================
# build_retirement_hazard_data — structure tests
# =============================================================================

test_that("build_retirement_hazard_data returns a data.table", {
  p  <- make_panel()
  rd <- build_retirement_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  expect_s3_class(rd, "data.table")
})

test_that("build_retirement_hazard_data contains required columns", {
  p  <- make_panel()
  rd <- build_retirement_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  expect_true(all(c("personnel_id", "ref_date", "retired",
                    "age", "tenure_years") %in% names(rd)))
})

test_that("build_retirement_hazard_data outcome is binary 0/1", {
  p  <- make_panel()
  rd <- build_retirement_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  expect_true(all(rd$retired %in% c(0L, 1L)))
})

test_that("build_retirement_hazard_data produces at least one retirement event", {
  # make_panel() forces 10% of persons to pensioner on the last snapshot
  p  <- make_panel(n_persons = 50L, n_snaps = 2L)
  rd <- build_retirement_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  expect_gt(sum(rd$retired), 0L)
})

test_that("build_retirement_hazard_data respects outcome_col argument", {
  p  <- make_panel()
  rd <- build_retirement_hazard_data(p$panel_contract_dt, p$panel_personnel_dt,
                                     outcome_col = "exited_retirement")
  expect_true("exited_retirement" %in% names(rd))
  expect_false("retired" %in% names(rd))
})

test_that("build_retirement_hazard_data attaches extra_covariates", {
  p  <- make_panel()
  rd <- build_retirement_hazard_data(p$panel_contract_dt, p$panel_personnel_dt,
                                     extra_covariates = c("paygrade",
                                                          "gross_salary_lcu"))
  expect_true("paygrade" %in% names(rd))
  expect_true("gross_salary_lcu" %in% names(rd))
})

test_that("build_retirement_hazard_data includes all n_snaps ref_dates", {
  # New approach: all snapshots appear (retirees get retired=1 on their
  # first pensioner snapshot; non-retirees appear on all snapshots)
  p  <- make_panel(n_snaps = 3L)
  rd <- build_retirement_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  expect_equal(length(unique(rd$ref_date)), 3L)
})

test_that("build_retirement_hazard_data no row after a person's first retirement", {
  p  <- make_panel(n_persons = 30L, n_snaps = 3L)
  rd <- build_retirement_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  # For each person: if they ever have retired=1, no subsequent rows should exist
  rd[, .check := {
    first_ret <- suppressWarnings(min(get("ref_date")[get("retired") == 1L], na.rm = TRUE))
    if (is.finite(first_ret)) all(get("ref_date") <= first_ret) else TRUE
  }, by = "personnel_id"]
  expect_true(all(rd$.check))
  rd[, .check := NULL]
})

# =============================================================================
# build_retirement_hazard_data — error conditions
# =============================================================================

test_that("build_retirement_hazard_data errors on missing ref_date_col", {
  p  <- make_panel()
  bad <- data.table::copy(p$panel_personnel_dt)
  data.table::setnames(bad, "ref_date", "snapshot")
  expect_error(
    build_retirement_hazard_data(p$panel_contract_dt, bad),
    regexp = "ref_date"
  )
})

test_that("build_retirement_hazard_data errors on missing extra covariate", {
  p <- make_panel()
  expect_error(
    build_retirement_hazard_data(p$panel_contract_dt, p$panel_personnel_dt,
                                 extra_covariates = "nonexistent_col"),
    regexp = "extra_covariates not found"
  )
})

test_that("build_retirement_hazard_data errors with only one snapshot", {
  p  <- make_panel(n_snaps = 1L)
  expect_error(
    build_retirement_hazard_data(p$panel_contract_dt, p$panel_personnel_dt),
    regexp = "at least 2"
  )
})

# =============================================================================
# build_exit_hazard_data — fixture
# =============================================================================

# 50 persons, 3 snapshots.  At snapshot 2: 10% disappear (exits).
# At snapshot 3: another 10% become "pensioner" (retirements, excluded from
# exit outcome).  The remaining persons stay active throughout.
make_exit_panel <- function(n_persons = 50L, n_snaps = 3L, seed = 42L) {
  set.seed(seed)

  persons    <- paste0("Q", seq_len(n_persons))
  snap_dates <- as.Date("2019-01-01") + (seq_len(n_snaps) - 1L) * 365L

  n_exit    <- max(1L, round(n_persons * 0.10))
  n_retire  <- max(1L, round(n_persons * 0.10))
  exit_idx  <- seq_len(n_exit)
  ret_idx   <- seq_len(n_retire) + n_exit   # non-overlapping

  # personnel panel — exiting persons drop out after snapshot 1
  personnel_rows <- lapply(seq_along(snap_dates), function(i) {
    d       <- snap_dates[[i]]
    keep    <- if (i == 1L) persons else persons[!(persons %in% persons[exit_idx])]
    data.table::data.table(
      personnel_id = keep,
      ref_date     = d,
      birth_date   = as.Date("1965-01-01") +
                       as.integer(stats::runif(length(keep), 0, 5 * 365)),
      status       = "active"
    )
  })
  panel_personnel_dt <- data.table::rbindlist(personnel_rows)

  # contract panel — same drop-out; snapshot 3 retirements become "pensioner"
  contract_rows <- lapply(seq_along(snap_dates), function(i) {
    d    <- snap_dates[[i]]
    keep <- if (i == 1L) persons else persons[!(persons %in% persons[exit_idx])]
    type <- rep("permanent", length(keep))
    if (i == n_snaps) {
      ret_keep_idx <- which(keep %in% persons[ret_idx])
      type[ret_keep_idx] <- "pensioner"
    }
    data.table::data.table(
      personnel_id       = keep,
      contract_id        = paste0("C", seq_along(keep), "_", i),
      ref_date           = d,
      contract_type_code = type,
      start_date         = as.Date("2010-01-01"),
      end_date           = as.Date("2030-12-31"),
      paygrade           = sample(c("A", "B", "C"), length(keep), replace = TRUE),
      gross_salary_lcu   = round(stats::runif(length(keep), 30000, 80000))
    )
  })
  panel_contract_dt <- data.table::rbindlist(contract_rows)

  list(
    panel_contract_dt  = panel_contract_dt,
    panel_personnel_dt = panel_personnel_dt,
    exit_ids           = persons[exit_idx],
    retire_ids         = persons[ret_idx]
  )
}

# =============================================================================
# build_exit_hazard_data — structure tests
# =============================================================================

test_that("build_exit_hazard_data returns a data.table", {
  p  <- make_exit_panel()
  rd <- build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  expect_true(data.table::is.data.table(rd))
})

test_that("build_exit_hazard_data contains required columns", {
  p  <- make_exit_panel()
  rd <- build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  expect_true(all(c("personnel_id", "ref_date", "exited", "age", "tenure_years")
                  %in% names(rd)))
})

test_that("build_exit_hazard_data outcome is binary 0/1", {
  p  <- make_exit_panel()
  rd <- build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  expect_true(all(rd$exited %in% c(0L, 1L)))
})

test_that("build_exit_hazard_data produces at least one exit event", {
  p  <- make_exit_panel()
  rd <- build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  expect_gt(sum(rd$exited), 0L)
})

test_that("build_exit_hazard_data retirements are not flagged as exits", {
  p  <- make_exit_panel()
  rd <- build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  # persons in retire_ids should never appear with exited = 1
  retired_exits <- rd[personnel_id %in% p$retire_ids & exited == 1L]
  expect_equal(nrow(retired_exits), 0L)
})

test_that("build_exit_hazard_data no rows after first exit per person", {
  p  <- make_exit_panel()
  rd <- build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt)
  rd[, .check := {
    first_ex <- suppressWarnings(min(ref_date[exited == 1L], na.rm = TRUE))
    if (is.finite(first_ex)) all(ref_date <= first_ex) else TRUE
  }, by = "personnel_id"]
  expect_true(all(rd$.check))
  rd[, .check := NULL]
})

test_that("build_exit_hazard_data respects outcome_col argument", {
  p  <- make_exit_panel()
  rd <- build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt,
                               outcome_col = "left")
  expect_true("left" %in% names(rd))
  expect_false("exited" %in% names(rd))
})

test_that("build_exit_hazard_data attaches extra_covariates", {
  p  <- make_exit_panel()
  rd <- build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt,
                               extra_covariates = "paygrade")
  expect_true("paygrade" %in% names(rd))
})

test_that("build_exit_hazard_data errors on missing contract column", {
  p  <- make_exit_panel()
  expect_error(
    build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt,
                           ref_date_col = "bad_col"),
    regexp = "missing required column"
  )
})

test_that("build_exit_hazard_data errors on missing extra covariate", {
  p  <- make_exit_panel()
  expect_error(
    build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt,
                           extra_covariates = "nonexistent_col"),
    regexp = "extra_covariates not found"
  )
})

test_that("build_exit_hazard_data errors with only one snapshot", {
  p  <- make_exit_panel(n_snaps = 1L)
  expect_error(
    build_exit_hazard_data(p$panel_contract_dt, p$panel_personnel_dt),
    regexp = "at least 2"
  )
})


# =============================================================================
# predict_hazard — helpers
# =============================================================================

# Fit a calibrated hazard_model on make_panel() data for use across tests.
make_calibrated_model <- function(seed = 1L) {
  p      <- make_panel(n_persons = 200L, n_snaps = 5L, seed = seed)
  reg_dt <- build_retirement_hazard_data(p$panel_contract_dt,
                                         p$panel_personnel_dt)
  # Use logit link on small toy data to avoid cloglog convergence issues
  hm     <- suppressWarnings(
    fit_hazard_model(reg_dt, "retired", c("age", "tenure_years"),
                     family = binomial(link = "logit"))
  )
  hm     <- select_hazard_threshold(hm, reg_dt)
  list(hm = hm, panel = p)
}

# Single-snapshot slices (last snapshot of make_panel())
make_snapshot <- function(panel, snap_idx = NULL) {
  all_dates <- sort(unique(panel$panel_personnel_dt$ref_date))
  d         <- all_dates[if (is.null(snap_idx)) length(all_dates) else snap_idx]
  list(
    contract_dt  = panel$panel_contract_dt[ref_date == d],
    personnel_dt = panel$panel_personnel_dt[ref_date == d],
    snap_date    = d
  )
}

# =============================================================================
# predict_hazard — structure tests
# =============================================================================

test_that("predict_hazard returns a data.table", {
  cm   <- make_calibrated_model()
  snap <- make_snapshot(cm$panel)
  out  <- predict_hazard(cm$hm, snap$contract_dt, snap$personnel_dt,
                         ref_date = snap$snap_date)
  expect_true(data.table::is.data.table(out))
})

test_that("predict_hazard contains exactly personnel_id, prob, event columns", {
  cm   <- make_calibrated_model()
  snap <- make_snapshot(cm$panel)
  out  <- predict_hazard(cm$hm, snap$contract_dt, snap$personnel_dt,
                         ref_date = snap$snap_date)
  expect_equal(names(out), c("personnel_id", "prob", "event"))
})

test_that("predict_hazard returns one row per active person", {
  cm   <- make_calibrated_model()
  snap <- make_snapshot(cm$panel)
  # active = non-inactive, non-pensioner
  n_active <- snap$contract_dt[
    !contract_type_code %in% c("inactive", "pensioner"),
    uniqueN(personnel_id)
  ]
  out <- predict_hazard(cm$hm, snap$contract_dt, snap$personnel_dt,
                        ref_date = snap$snap_date)
  expect_equal(nrow(out), n_active)
})

test_that("predict_hazard event column is binary 0/1 integer", {
  cm   <- make_calibrated_model()
  snap <- make_snapshot(cm$panel)
  out  <- predict_hazard(cm$hm, snap$contract_dt, snap$personnel_dt,
                         ref_date = snap$snap_date)
  expect_true(is.integer(out$event))
  expect_true(all(out$event %in% c(0L, 1L)))
})

test_that("predict_hazard prob is numeric in [0, 1]", {
  cm   <- make_calibrated_model()
  snap <- make_snapshot(cm$panel)
  out  <- predict_hazard(cm$hm, snap$contract_dt, snap$personnel_dt,
                         ref_date = snap$snap_date)
  non_na <- out$prob[!is.na(out$prob)]
  expect_true(all(non_na >= 0 & non_na <= 1))
})

test_that("predict_hazard event=1 iff prob >= threshold", {
  cm   <- make_calibrated_model()
  snap <- make_snapshot(cm$panel)
  out  <- predict_hazard(cm$hm, snap$contract_dt, snap$personnel_dt,
                         ref_date = snap$snap_date)
  thr  <- cm$hm$threshold
  expected_event <- as.integer(!is.na(out$prob) & out$prob >= thr)
  expect_equal(out$event, expected_event)
})

test_that("predict_hazard excludes inactive and pensioner contracts", {
  cm   <- make_calibrated_model()
  snap <- make_snapshot(cm$panel)
  # Manually mark one person as inactive
  snap$contract_dt[1L, contract_type_code := "inactive"]
  out  <- predict_hazard(cm$hm, snap$contract_dt, snap$personnel_dt,
                         ref_date = snap$snap_date)
  expect_false(snap$contract_dt$personnel_id[1L] %in% out$personnel_id)
})

test_that("predict_hazard works with pre-computed age on personnel_dt", {
  cm   <- make_calibrated_model()
  snap <- make_snapshot(cm$panel)
  snap$personnel_dt[, age := 50]   # add pre-computed age column
  out  <- predict_hazard(cm$hm, snap$contract_dt, snap$personnel_dt,
                         age_col = "age", ref_date = snap$snap_date)
  expect_true(data.table::is.data.table(out))
  expect_equal(names(out), c("personnel_id", "prob", "event"))
})

# =============================================================================
# predict_hazard — error conditions
# =============================================================================

test_that("predict_hazard errors on non-hazard_model input", {
  snap <- make_snapshot(make_calibrated_model()$panel)
  expect_error(
    predict_hazard(list(model = NULL), snap$contract_dt, snap$personnel_dt),
    regexp = "hazard_model"
  )
})

test_that("predict_hazard errors when threshold is NA", {
  cm   <- make_calibrated_model()
  cm$hm$threshold <- NA_real_
  snap <- make_snapshot(cm$panel)
  expect_error(
    predict_hazard(cm$hm, snap$contract_dt, snap$personnel_dt),
    regexp = "threshold is NA"
  )
})

test_that("predict_hazard errors on missing required contract column", {
  cm   <- make_calibrated_model()
  snap <- make_snapshot(cm$panel)
  bad  <- data.table::copy(snap$contract_dt)
  data.table::setnames(bad, "personnel_id", "pid")
  expect_error(
    predict_hazard(cm$hm, bad, snap$personnel_dt),
    regexp = "missing required column"
  )
})
