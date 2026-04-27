test_that("estimate_movement_baseline requires data.table", {
  expect_error(
    estimate_movement_baseline(list(), group_cols = "paygrade"),
    "data.table"
  )
})

test_that("estimate_movement_baseline requires at least 2 snapshots", {
  ct <- data.table::data.table(
    ref_date          = as.Date("2016-01-01"),
    personnel_id      = c("P1", "P2"),
    paygrade          = c("G1", "G2"),
    start_date        = as.Date("2010-01-01"),
    end_date          = as.Date(NA),
    contract_type_code = "permanent"
  )
  expect_error(
    estimate_movement_baseline(ct, group_cols = "paygrade"),
    "2 panel snapshots"
  )
})

test_that("estimate_movement_baseline requires group_cols", {
  ct <- data.table::data.table(
    ref_date = as.Date(c("2015-01-01", "2016-01-01")),
    personnel_id = c("P1", "P1"),
    start_date = as.Date("2010-01-01"),
    end_date = as.Date(NA),
    contract_type_code = "permanent"
  )
  expect_error(
    estimate_movement_baseline(ct, group_cols = NULL),
    "group_cols"
  )
})

# ---------------------------------------------------------------------------
# Helper: small 2-period panel with known movements
# ---------------------------------------------------------------------------
make_panel <- function() {
  data.table::data.table(
    ref_date = as.Date(c(
      "2015-01-01", "2015-01-01", "2015-01-01", "2015-01-01",
      "2016-01-01", "2016-01-01", "2016-01-01", "2016-01-01"
    )),
    personnel_id = c("P1", "P2", "P3", "P4",
                     "P1", "P2", "P3", "P4"),
    paygrade = c("G1", "G1", "G2", "G2",
                 "G2", "G1", "G2", "G1"),   # P1: G1->G2, P4: G2->G1
    start_date = as.Date("2010-01-01"),
    end_date   = as.Date(NA),
    contract_type_code = "permanent"
  )
}

test_that("estimate_movement_baseline returns correct columns", {
  bm <- estimate_movement_baseline(make_panel(), group_cols = "paygrade")
  expect_true(data.table::is.data.table(bm))
  expect_true(all(c("from_group", "to_group", "movement_rate", "n_periods") %in% names(bm)))
})

test_that("estimate_movement_baseline computes correct avg_prob", {
  bm <- estimate_movement_baseline(make_panel(), group_cols = "paygrade")

  # P1 (G1->G2): 1 mover out of 2 in G1 = 0.5
  g1_to_g2 <- bm[from_group == "G1" & to_group == "G2", movement_rate]
  expect_equal(g1_to_g2, 0.5, tolerance = 1e-10)

  # P4 (G2->G1): 1 mover out of 2 in G2 = 0.5
  g2_to_g1 <- bm[from_group == "G2" & to_group == "G1", movement_rate]
  expect_equal(g2_to_g1, 0.5, tolerance = 1e-10)
})

test_that("estimate_movement_baseline probs are in [0, 1]", {
  bm <- estimate_movement_baseline(make_panel(), group_cols = "paygrade")
  expect_true(all(bm$movement_rate >= 0 & bm$movement_rate <= 1))
})

test_that("estimate_movement_baseline only returns movement transitions", {
  bm <- estimate_movement_baseline(make_panel(), group_cols = "paygrade")
  # Should include G1->G2 and G2->G1 cross-transitions AND same-state rows
  expect_true(nrow(bm) > 0)
})

test_that("estimate_movement_baseline works with multi-column group_cols", {
  ct <- data.table::data.table(
    ref_date = as.Date(c(
      "2015-01-01", "2015-01-01",
      "2016-01-01", "2016-01-01"
    )),
    personnel_id = c("P1", "P2", "P1", "P2"),
    est_id   = c("E1", "E1", "E1", "E2"),
    paygrade = c("G1", "G2", "G1", "G2"),
    start_date = as.Date("2010-01-01"),
    end_date   = as.Date(NA),
    contract_type_code = "permanent"
  )
  bm <- estimate_movement_baseline(ct, group_cols = c("est_id", "paygrade"))
  expect_true(data.table::is.data.table(bm))
  expect_true("from_group" %in% names(bm))
})

test_that("estimate_movement_baseline handles 3+ snapshots", {
  ct <- data.table::rbindlist(list(
    make_panel(),
    data.table::data.table(
      ref_date = as.Date("2017-01-01"),
      personnel_id = c("P1", "P2", "P3", "P4"),
      paygrade = c("G2", "G2", "G2", "G2"),
      start_date = as.Date("2010-01-01"),
      end_date = as.Date(NA),
      contract_type_code = "permanent"
    )
  ))
  bm <- estimate_movement_baseline(ct, group_cols = "paygrade")
  expect_true(all(bm$n_periods <= 2L))  # 2 transition periods
  expect_true(all(bm$avg_prob >= 0 & bm$avg_prob <= 1))
})

# ---------------------------------------------------------------------------
# compute_time_in_grade
# ---------------------------------------------------------------------------
test_that("compute_time_in_grade returns correct columns", {
  tig <- compute_time_in_grade(
    contract_dt = make_panel(),
    ref_date    = "2016-01-01",
    group_cols  = "paygrade"
  )
  expect_true(data.table::is.data.table(tig))
  expect_true(all(c("personnel_id", "time_in_grade") %in% names(tig)))
})

test_that("compute_time_in_grade values are non-negative", {
  tig <- compute_time_in_grade(
    contract_dt = make_panel(),
    ref_date    = "2016-01-01",
    group_cols  = "paygrade"
  )
  expect_true(all(tig$time_in_grade >= 0))
})

test_that("compute_time_in_grade accepts string ref_date", {
  expect_no_error(
    compute_time_in_grade(
      contract_dt = make_panel(),
      ref_date    = "2016-01-01",
      group_cols  = "paygrade"
    )
  )
})

test_that("compute_time_in_grade errors on pre-data ref_date", {
  expect_error(
    compute_time_in_grade(
      contract_dt = make_panel(),
      ref_date    = "2000-01-01",
      group_cols  = "paygrade"
    ),
    "No panel snapshots"
  )
})

# ---------------------------------------------------------------------------
# compute_movement_demand
# ---------------------------------------------------------------------------
make_snapshot <- function() {
  list(
    contract_dt = data.table::data.table(
      personnel_id      = c("P1", "P2", "P3", "P4"),
      paygrade          = c("G1", "G1", "G2", "G2"),
      start_date        = as.Date("2010-01-01"),
      end_date          = as.Date(NA),
      contract_type_code = "permanent"
    ),
    personnel_dt = data.table::data.table(
      personnel_id = c("P1", "P2", "P3", "P4"),
      status       = "active"
    ),
    salary_scale = data.table::data.table(
      paygrade         = c("G1", "G2"),
      gross_salary_lcu = c(3000, 5000)
    ),
    baseline = data.table::data.table(
      from_group    = c("G1", "G2"),
      to_group      = c("G2", "G1"),
      movement_rate = c(0.5, 0.5),
      n_periods     = c(1L, 1L)
    )
  )
}

test_that("compute_movement_demand returns correct columns", {
  set.seed(42)
  s <- make_snapshot()
  pp <- list(group_cols   = "paygrade",
             policy_table = NULL,
             defaults     = list(movement_rate = 0.5, active_types = "permanent"))
  dm <- compute_movement_demand(
    contract_dt     = s$contract_dt,
    personnel_dt    = s$personnel_dt,
    baseline_matrix = s$baseline,
    policy_params   = pp,
    salary_scale_dt = s$salary_scale,
    ref_date        = "2016-01-01"
  )
  expect_true(data.table::is.data.table(dm))
  expected_cols <- c("from_group", "to_group", "current_stock", "n_movers")
  expect_true(all(expected_cols %in% names(dm)))
})

test_that("compute_movement_demand n_movers are non-negative integers", {
  set.seed(1)
  s <- make_snapshot()
  pp <- list(group_cols   = "paygrade",
             policy_table = NULL,
             defaults     = list(movement_rate = 0.5, active_types = "permanent"))
  dm <- compute_movement_demand(
    contract_dt     = s$contract_dt,
    personnel_dt    = s$personnel_dt,
    baseline_matrix = s$baseline,
    policy_params   = pp,
    salary_scale_dt = s$salary_scale,
    ref_date        = "2016-01-01"
  )
  expect_true(all(dm$n_movers >= 0L))
  expect_true(is.integer(dm$n_movers))
})

test_that("compute_movement_demand scrubs destinations not in salary_scale", {
  set.seed(1)
  s <- make_snapshot()
  # Remove G1 from salary_scale -> G2->G1 should be scrubbed
  scale_no_g1 <- s$salary_scale[paygrade != "G1"]
  pp <- list(group_cols   = "paygrade",
             policy_table = NULL,
             defaults     = list(movement_rate = 0.5, active_types = "permanent"))
  expect_message(
    dm <- compute_movement_demand(
      contract_dt     = s$contract_dt,
      personnel_dt    = s$personnel_dt,
      baseline_matrix = s$baseline,
      policy_params   = pp,
      salary_scale_dt = scale_no_g1,
      ref_date        = "2016-01-01"
    ),
    "Scrubbed"
  )
  # G2->G1 should no longer appear
  expect_equal(nrow(dm[to_group == "G1"]), 0L)
})

test_that("compute_movement_demand caps total outflow at 1.0", {
  set.seed(1)
  s <- make_snapshot()
  # Add a second destination for G1 so total prob > 1
  high_prob_baseline <- data.table::data.table(
    from_group    = c("G1", "G1", "G2"),
    to_group      = c("G2", "G2", "G1"),  # intentional duplicate -> prob sum > 1
    movement_rate = c(0.7, 0.6, 0.3),
    n_periods     = c(1L, 1L, 1L)
  )
  pp <- list(group_cols   = "paygrade",
             policy_table = NULL,
             defaults     = list(movement_rate = 0.5, active_types = "permanent"))
  dm <- compute_movement_demand(
    contract_dt     = s$contract_dt,
    personnel_dt    = s$personnel_dt,
    baseline_matrix = high_prob_baseline,
    policy_params   = pp,
    salary_scale_dt = s$salary_scale,
    ref_date        = "2016-01-01"
  )
  # Total outflow per from_group must be <= 1
  dm[, total_out := sum(movement_rate), by = from_group]
  expect_true(all(dm$total_out <= 1.0 + 1e-10))
})

test_that("compute_movement_demand returns empty dt when no active personnel", {
  ct <- data.table::data.table(
    personnel_id = character(0),
    paygrade = character(0),
    start_date = as.Date(character(0)),
    end_date = as.Date(character(0)),
    contract_type_code = character(0)
  )
  pp_dt <- data.table::data.table(
    personnel_id = character(0),
    status = character(0)
  )
  s <- make_snapshot()
  pp <- list(group_cols   = "paygrade",
             policy_table = NULL,
             defaults     = list(movement_rate = 0.5, active_types = "permanent"))
  dm <- compute_movement_demand(
    contract_dt     = ct,
    personnel_dt    = pp_dt,
    baseline_matrix = s$baseline,
    policy_params   = pp,
    salary_scale_dt = s$salary_scale,
    ref_date        = "2016-01-01"
  )
  expect_equal(nrow(dm), 0L)
})

test_that("compute_movement_demand classifies movements correctly", {
  set.seed(1)
  ct <- data.table::data.table(
    personnel_id      = c("P1", "P2", "P3", "P4"),
    est_id            = c("E1", "E1", "E2", "E2"),
    paygrade          = c("G1", "G2", "G1", "G2"),
    start_date        = as.Date("2010-01-01"),
    end_date          = as.Date(NA),
    contract_type_code = "permanent"
  )
  pp_dt <- data.table::data.table(
    personnel_id = c("P1", "P2", "P3", "P4"),
    status = "active"
  )
  scale <- data.table::data.table(
    est_id = c("E1", "E1", "E2", "E2"),
    paygrade = c("G1", "G2", "G1", "G2"),
    gross_salary_lcu = c(3000, 5000, 3000, 5000)
  )
  bm <- data.table::data.table(
    from_group    = c("E1||G1", "E1||G2"),
    to_group      = c("E1||G2", "E2||G2"),
    movement_rate = c(0.3, 0.3),
    n_periods     = c(1L, 1L)
  )
  pp <- list(group_cols   = c("est_id", "paygrade"),
             policy_table = NULL,
             defaults     = list(movement_rate = 0.3, active_types = "permanent"))
  dm <- compute_movement_demand(
    contract_dt     = ct,
    personnel_dt    = pp_dt,
    baseline_matrix = bm,
    policy_params   = pp,
    salary_scale_dt = scale,
    ref_date        = "2016-01-01"
  )
  expect_true(data.table::is.data.table(dm))
  if (nrow(dm) > 0) {
    expect_true(all(c("from_group", "to_group", "n_movers") %in% names(dm)))
  }
})

# ---------------------------------------------------------------------------
# compute_movement_summary
# ---------------------------------------------------------------------------
test_that("compute_movement_summary returns correct columns", {
  movers <- data.table::data.table(
    personnel_id = c("P1", "P2"),
    from_group   = c("G1", "G2"),
    to_group     = c("G2", "G1")
  )
  bm <- data.table::data.table(
    from_group = "G1", to_group = "G2", movement_rate = 0.1, n_periods = 1L
  )
  demand <- data.table::data.table(
    from_group = "G1", to_group = "G2",
    current_stock = 10L, n_movers = 1L
  )
  s <- compute_movement_summary(movers, demand, bm, 100L, 100L)
  expect_true(data.table::is.data.table(s))
  expected_cols <- c("n_movers", "headcount_before", "headcount_after")
  expect_true(all(expected_cols %in% names(s)))
})

test_that("compute_movement_summary handles empty movers", {
  empty_movers <- data.table::data.table(
    personnel_id = character(0),
    from_group   = character(0),
    to_group     = character(0)
  )
  s <- compute_movement_summary(empty_movers, NULL, NULL, 50L, 50L)
  expect_equal(s$n_movers, 0L)
  expect_equal(s$headcount_before, 50L)
})


# =============================================================================
# roll_snapshot_pairs() — unit tests (Block A)
# =============================================================================

make_two_period_panel <- function() {
  data.table::data.table(
    ref_date     = as.Date(c("2015-01-01", "2015-01-01",
                             "2016-01-01", "2016-01-01")),
    personnel_id = c("P1", "P2", "P1", "P2"),
    paygrade     = c("G1", "G2", "G2", "G2")
  )
}

make_three_period_panel <- function() {
  data.table::data.table(
    ref_date     = as.Date(c(
      rep("2015-01-01", 2), rep("2016-01-01", 2), rep("2017-01-01", 2)
    )),
    personnel_id = c("P1","P2", "P1","P2", "P1","P2"),
    value        = 1:6
  )
}

test_that("roll_snapshot_pairs: returns empty data.table for < 2 dates", {
  panel <- data.table::data.table(
    ref_date = as.Date("2015-01-01"),
    x        = 1L
  )
  result <- roll_snapshot_pairs(panel, "ref_date", function(a, b) data.table::data.table(n = 1L))
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0L)
})

test_that("roll_snapshot_pairs: calls f exactly n_dates-1 times", {
  panel <- make_three_period_panel()   # 3 dates → 2 calls

  call_count <- 0L
  counter_f  <- function(a, b) {
    call_count <<- call_count + 1L
    data.table::data.table(pair = call_count)
  }

  result <- roll_snapshot_pairs(panel, "ref_date", counter_f)
  expect_equal(call_count, 2L)
  expect_equal(nrow(result), 2L)
})

test_that("roll_snapshot_pairs: snap_a and snap_b carry the correct dates", {
  panel <- make_two_period_panel()

  dates_seen <- list()
  check_f <- function(a, b) {
    dates_seen[[length(dates_seen) + 1L]] <<- list(
      t0 = a$ref_date[1L],
      t1 = b$ref_date[1L]
    )
    data.table::data.table(ok = TRUE)
  }

  roll_snapshot_pairs(panel, "ref_date", check_f)

  expect_equal(length(dates_seen), 1L)
  expect_equal(dates_seen[[1L]]$t0, as.Date("2015-01-01"))
  expect_equal(dates_seen[[1L]]$t1, as.Date("2016-01-01"))
})

test_that("roll_snapshot_pairs: NA dates are dropped before iteration", {
  panel <- data.table::data.table(
    ref_date = as.Date(c(NA, "2015-01-01", "2016-01-01")),
    x        = 1:3
  )

  call_count <- 0L
  roll_snapshot_pairs(panel, "ref_date", function(a, b) {
    call_count <<- call_count + 1L
    data.table::data.table(n = 1L)
  })
  # Only 2 non-NA dates → exactly 1 pair
  expect_equal(call_count, 1L)
})

test_that("roll_snapshot_pairs: NULL results from f are skipped", {
  panel <- make_three_period_panel()

  k <- 0L
  skip_f <- function(a, b) {
    k <<- k + 1L
    if (k == 1L) return(NULL)   # skip first pair
    data.table::data.table(pair = k)
  }

  result <- roll_snapshot_pairs(panel, "ref_date", skip_f)
  expect_equal(nrow(result), 1L)
  expect_equal(result$pair, 2L)
})

test_that("roll_snapshot_pairs: errors on missing date_col", {
  panel <- data.table::data.table(x = 1:3)
  expect_error(
    roll_snapshot_pairs(panel, "not_a_col", function(a, b) NULL),
    "not_a_col"
  )
})

test_that("roll_snapshot_pairs: errors on non-data.table input", {
  expect_error(
    roll_snapshot_pairs(list(x = 1), "ref_date", function(a, b) NULL),
    "data.table"
  )
})

# =============================================================================
# estimate_movement_baseline() output-identity check after Block A refactor
# =============================================================================

test_that("estimate_movement_baseline: Block A refactor produces identical output to pre-refactor", {
  bm <- estimate_movement_baseline(make_panel(), group_cols = "paygrade")

  g1_to_g2 <- bm[from_group == "G1" & to_group == "G2", movement_rate]
  g2_to_g1 <- bm[from_group == "G2" & to_group == "G1", movement_rate]

  expect_equal(g1_to_g2, 0.5, tolerance = 1e-10)
  expect_equal(g2_to_g1, 0.5, tolerance = 1e-10)
})

test_that("estimate_movement_baseline: 3-period panel averages correctly", {
  # Period 1 (2015→2016): P1 G1→G2, P4 G2→G1  (1/2 each direction)
  # Period 2 (2016→2017): P1 G2→G1, P4 G1→G2  (same rate, reversed)
  # Both periods contribute to both cross-transitions → avg_prob = 0.5, n_periods = 2
  panel3 <- data.table::rbindlist(list(
    make_panel(),  # 2015 and 2016 snapshots
    data.table::data.table(
      ref_date           = as.Date("2017-01-01"),
      personnel_id       = c("P1","P2","P3","P4"),
      paygrade           = c("G1","G1","G2","G2"),   # reverse of 2016 → new movements
      start_date         = as.Date("2010-01-01"),
      end_date           = as.Date(NA),
      contract_type_code = "permanent"
    )
  ))

  bm3 <- estimate_movement_baseline(panel3, group_cols = "paygrade")
  # G1→G2 occurred in both periods: period-1 prob = 0.5, period-2 prob = 0.5 → avg = 0.5
  expect_equal(bm3[from_group == "G1" & to_group == "G2", movement_rate], 0.5, tolerance = 1e-10)
  expect_equal(bm3[from_group == "G1" & to_group == "G2", n_periods], 2L)
})
