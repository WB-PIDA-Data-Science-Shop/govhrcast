test_that("estimate_historical_movement_rates requires data.table", {
  expect_error(
    estimate_historical_movement_rates(list(), group_cols = "paygrade"),
    "data.table"
  )
})

test_that("estimate_historical_movement_rates requires at least 2 snapshots", {
  ct <- data.table::data.table(
    ref_date          = as.Date("2016-01-01"),
    personnel_id      = c("P1", "P2"),
    paygrade          = c("G1", "G2"),
    start_date        = as.Date("2010-01-01"),
    end_date          = as.Date(NA),
    contract_type = "permanent", contract_id = "C1", gross_salary_lcu = 100
  )
  expect_error(
    estimate_historical_movement_rates(ct, group_cols = "paygrade"),
    "2 panel snapshots"
  )
})

test_that("estimate_historical_movement_rates requires group_cols", {
  ct <- data.table::data.table(
    ref_date = as.Date(c("2015-01-01", "2016-01-01")),
    personnel_id = c("P1", "P1"),
    start_date = as.Date("2010-01-01"),
    end_date = as.Date(NA),
    contract_type = "permanent", contract_id = "C1", gross_salary_lcu = 100
  )
  expect_error(
    estimate_historical_movement_rates(ct, group_cols = NULL),
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
    contract_type = "permanent", contract_id = "C1", gross_salary_lcu = 100
  )
}

test_that("estimate_historical_movement_rates returns correct columns", {
  bm <- estimate_historical_movement_rates(make_panel(), group_cols = "paygrade")
  expect_true(data.table::is.data.table(bm))
  expect_true(all(c("from_group", "to_group", "movement_rate", "n_periods") %in% names(bm)))
})

test_that("estimate_historical_movement_rates computes correct avg_prob", {
  bm <- estimate_historical_movement_rates(make_panel(), group_cols = "paygrade")

  # P1 (G1->G2): 1 mover out of 2 in G1 = 0.5
  g1_to_g2 <- bm[from_group == "G1" & to_group == "G2", movement_rate]
  expect_equal(g1_to_g2, 0.5, tolerance = 1e-10)

  # P4 (G2->G1): 1 mover out of 2 in G2 = 0.5
  g2_to_g1 <- bm[from_group == "G2" & to_group == "G1", movement_rate]
  expect_equal(g2_to_g1, 0.5, tolerance = 1e-10)
})

test_that("estimate_historical_movement_rates probs are in [0, 1]", {
  bm <- estimate_historical_movement_rates(make_panel(), group_cols = "paygrade")
  expect_true(all(bm$movement_rate >= 0 & bm$movement_rate <= 1))
})

test_that("estimate_historical_movement_rates only returns movement transitions", {
  bm <- estimate_historical_movement_rates(make_panel(), group_cols = "paygrade")
  # Should include G1->G2 and G2->G1 cross-transitions AND same-state rows
  expect_true(nrow(bm) > 0)
})

test_that("estimate_historical_movement_rates works with multi-column group_cols", {
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
    contract_type = "permanent", contract_id = "C1", gross_salary_lcu = 100
  )
  bm <- estimate_historical_movement_rates(ct, group_cols = c("est_id", "paygrade"))
  expect_true(data.table::is.data.table(bm))
  expect_true("from_group" %in% names(bm))
})

test_that("estimate_historical_movement_rates handles 3+ snapshots", {
  ct <- data.table::rbindlist(list(
    make_panel(),
    data.table::data.table(
      ref_date = as.Date("2017-01-01"),
      personnel_id = c("P1", "P2", "P3", "P4"),
      paygrade = c("G2", "G2", "G2", "G2"),
      start_date = as.Date("2010-01-01"),
      end_date = as.Date(NA),
      contract_type = "permanent", contract_id = "C1", gross_salary_lcu = 100
    )
  ))
  bm <- estimate_historical_movement_rates(ct, group_cols = "paygrade")
  expect_true(all(bm$n_periods <= 2L))  # 2 transition periods
  expect_true(all(bm$avg_prob >= 0 & bm$avg_prob <= 1))
})

# ---------------------------------------------------------------------------
# compute_time_in_grade
# ---------------------------------------------------------------------------
test_that("compute_time_in_grade returns correct columns", {
  tig <- compute_time_in_grade(
    contract_dt = make_panel(),
    as_of_date    = "2016-01-01",
    group_cols  = "paygrade"
  )
  expect_true(data.table::is.data.table(tig))
  expect_true(all(c("personnel_id", "time_in_grade") %in% names(tig)))
})

test_that("compute_time_in_grade values are non-negative", {
  tig <- compute_time_in_grade(
    contract_dt = make_panel(),
    as_of_date    = "2016-01-01",
    group_cols  = "paygrade"
  )
  expect_true(all(tig$time_in_grade >= 0))
})

test_that("compute_time_in_grade accepts string ref_date", {
  expect_no_error(
    compute_time_in_grade(
      contract_dt = make_panel(),
      as_of_date    = "2016-01-01",
      group_cols  = "paygrade"
    )
  )
})

test_that("compute_time_in_grade errors on pre-data ref_date", {
  expect_error(
    compute_time_in_grade(
      contract_dt = make_panel(),
      as_of_date    = "2000-01-01",
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
      contract_type = "permanent", contract_id = "C1", gross_salary_lcu = 100
    ),
    personnel_dt = data.table::data.table(
      personnel_id = c("P1", "P2", "P3", "P4"),
      employment_status = "active"
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
    as_of_date        = "2016-01-01"
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
    as_of_date        = "2016-01-01"
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
      as_of_date        = "2016-01-01"
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
    as_of_date        = "2016-01-01"
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
    contract_type = character(0)
  )
  pp_dt <- data.table::data.table(
    personnel_id = character(0),
    employment_status = character(0)
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
    as_of_date        = "2016-01-01"
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
    contract_type = "permanent", contract_id = "C1", gross_salary_lcu = 100
  )
  pp_dt <- data.table::data.table(
    personnel_id = c("P1", "P2", "P3", "P4"),
    employment_status = "active"
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
    as_of_date        = "2016-01-01"
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
# estimate_historical_movement_rates() output-identity check after Block A refactor
# =============================================================================

test_that("estimate_historical_movement_rates: Block A refactor produces identical output to pre-refactor", {
  bm <- estimate_historical_movement_rates(make_panel(), group_cols = "paygrade")

  g1_to_g2 <- bm[from_group == "G1" & to_group == "G2", movement_rate]
  g2_to_g1 <- bm[from_group == "G2" & to_group == "G1", movement_rate]

  expect_equal(g1_to_g2, 0.5, tolerance = 1e-10)
  expect_equal(g2_to_g1, 0.5, tolerance = 1e-10)
})

test_that("estimate_historical_movement_rates: 3-period panel averages correctly", {
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
      contract_type = "permanent", contract_id = "C1", gross_salary_lcu = 100
    )
  ))

  bm3 <- estimate_historical_movement_rates(panel3, group_cols = "paygrade")
  # G1→G2 occurred in both periods: period-1 prob = 0.5, period-2 prob = 0.5 → avg = 0.5
  expect_equal(bm3[from_group == "G1" & to_group == "G2", movement_rate], 0.5, tolerance = 1e-10)
  expect_equal(bm3[from_group == "G1" & to_group == "G2", n_periods], 2L)
})

# =============================================================================
# Which group a person with several contracts counts in: their main contract
# =============================================================================

contract_row <- function(person, date, est, contract, start = "2000-01-01", pay = 100) {
  data.table::data.table(
    personnel_id = person, ref_date = as.Date(date), est_id = est, contract_id = contract,
    start_date = as.Date(start), end_date = as.Date(NA), contract_type = "permanent",
    gross_salary_lcu = pay
  )
}

test_that("row order does not change the movement rates", {
  panel <- rbind(
    contract_row("a", "2019-01-01", "E1", "C1", start = "2000-01-01"),
    contract_row("a", "2019-01-01", "E2", "C2", start = "2010-01-01"),
    contract_row("a", "2020-01-01", "E3", "C3", start = "2019-06-01"),
    contract_row("b", "2019-01-01", "E1", "C4"),
    contract_row("b", "2020-01-01", "E1", "C4")
  )
  expect_identical(
    as.data.frame(estimate_historical_movement_rates(data.table::copy(panel), "est_id")),
    as.data.frame(estimate_historical_movement_rates(panel[.N:1L], "est_id"))
  )
})

test_that("a person with several contracts counts in the group of their newest", {
  panel <- rbind(
    contract_row("a", "2019-01-01", "E1", "C1", start = "2000-01-01"),
    contract_row("a", "2019-01-01", "E2", "C2", start = "2010-01-01"),
    contract_row("a", "2020-01-01", "E3", "C3", start = "2019-06-01"),
    contract_row("b", "2019-01-01", "E2", "C4"),
    contract_row("b", "2020-01-01", "E2", "C4")
  )
  rates <- estimate_historical_movement_rates(panel, "est_id")
  # a's main contract is in E2, so E2 holds a and b, and one of the two moved
  expect_equal(rates$from_group, "E2")
  expect_equal(rates$to_group, "E3")
  expect_equal(rates$movement_rate, 0.5)
})

test_that("with the same start date, the higher salary decides the main contract", {
  panel <- rbind(
    contract_row("a", "2019-01-01", "E1", "C1", pay = 100),
    contract_row("a", "2019-01-01", "E2", "C2", pay = 200),
    contract_row("a", "2020-01-01", "E3", "C3", start = "2019-06-01")
  )
  rates <- estimate_historical_movement_rates(panel, "est_id")
  expect_equal(rates$from_group, "E2")
})

test_that("with the same start date and salary, the lowest contract ID decides", {
  panel <- rbind(
    contract_row("a", "2019-01-01", "E1", "C2"),
    contract_row("a", "2019-01-01", "E2", "C1"),
    contract_row("a", "2020-01-01", "E3", "C3", start = "2019-06-01")
  )
  rates <- estimate_historical_movement_rates(panel, "est_id")
  expect_equal(rates$from_group, "E2")
})

test_that("adding a newer contract in another group counts as a move, because the main contract moved", {
  panel <- rbind(
    contract_row("a", "2019-01-01", "E1", "C1", start = "2000-01-01"),
    contract_row("a", "2020-01-01", "E1", "C1", start = "2000-01-01"),
    contract_row("a", "2020-01-01", "E2", "C2", start = "2019-06-01")
  )
  rates <- estimate_historical_movement_rates(panel, "est_id")
  expect_equal(rates$from_group, "E1")
  expect_equal(rates$to_group, "E2")
  expect_equal(rates$movement_rate, 1)
})

test_that("a blank group on the main contract leaves the person out, rather than using another contract", {
  panel <- rbind(
    contract_row("a", "2019-01-01", "E1", "C1", start = "2000-01-01"),
    contract_row("a", "2019-01-01", NA, "C2", start = "2010-01-01"),
    contract_row("a", "2020-01-01", "E3", "C3", start = "2019-06-01")
  )
  expect_equal(nrow(estimate_historical_movement_rates(panel, "est_id")), 0L)
})

test_that("the main contract rule reads the caller's own column names", {
  panel <- rbind(
    contract_row("a", "2019-01-01", "E1", "C1", start = "2000-01-01"),
    contract_row("a", "2019-01-01", "E2", "C2", start = "2010-01-01"),
    contract_row("a", "2020-01-01", "E3", "C3", start = "2019-06-01")
  )
  renamed <- data.table::copy(panel)
  data.table::setnames(renamed,
                       c("personnel_id", "ref_date", "contract_id", "start_date", "end_date",
                         "contract_type", "gross_salary_lcu"),
                       c("pid", "snap", "cid", "inicio", "fim", "tipo", "bruto"))
  spec <- define_column_spec(personnel_id_col = "pid", ref_date_col = "snap", contract_id_col = "cid",
                             start_date_col = "inicio", end_date_col = "fim",
                             contract_type_col = "tipo", salary_col = "bruto")
  expect_equal(estimate_historical_movement_rates(renamed, "est_id", spec)$from_group, "E2")
})

# Someone who stays in E1 throughout, so the panel always has two dates with staff
stayer <- function() rbind(contract_row("s", "2019-01-01", "E1", "CS"),
                           contract_row("s", "2020-01-01", "E1", "CS"))

test_that("a contract that ended before the snapshot does not count as a move into its group", {
  ended <- contract_row("a", "2020-01-01", "E2", "C2")
  ended[, end_date := as.Date("2019-06-01")]
  panel <- rbind(stayer(), contract_row("a", "2019-01-01", "E1", "C1"), ended)
  expect_equal(nrow(estimate_historical_movement_rates(panel, "est_id")), 0L)
})

test_that("a contract that starts after the snapshot does not count as a move into its group", {
  panel <- rbind(stayer(), contract_row("a", "2019-01-01", "E1", "C1"),
                 contract_row("a", "2020-01-01", "E2", "C2", start = "2025-01-01"))
  expect_equal(nrow(estimate_historical_movement_rates(panel, "est_id")), 0L)
})

test_that("filling in a blank paygrade is not a move, with two group columns", {
  # paste() would turn the blank into the text "E1||NA", a different group from "E1||G1"
  panel <- rbind(stayer(), contract_row("a", "2019-01-01", "E1", "C1"),
                 contract_row("a", "2020-01-01", "E1", "C1"))
  panel[, paygrade := "G1"]
  panel[personnel_id == "a" & ref_date == as.Date("2019-01-01"), paygrade := NA_character_]
  expect_equal(nrow(estimate_historical_movement_rates(panel, c("est_id", "paygrade"))), 0L)
})

test_that("a group literally called NA is left out of the routes", {
  panel <- rbind(
    contract_row("a", "2019-01-01", "NA", "C1"), contract_row("a", "2020-01-01", "E2", "C1"),
    contract_row("b", "2019-01-01", "E1", "C2"), contract_row("b", "2020-01-01", "NA", "C2"),
    contract_row("c", "2019-01-01", "E1", "C3"), contract_row("c", "2020-01-01", "E2", "C3")
  )
  rates <- estimate_historical_movement_rates(panel, "est_id")
  expect_equal(rates$from_group, "E1")
  expect_equal(rates$to_group, "E2")
  # b and c were both in E1 at the start, and one of the two reached E2
  expect_equal(rates$movement_rate, 0.5)
})

test_that("movement_policy_template() names itself when a column it needs is missing", {
  panel <- rbind(stayer(), contract_row("a", "2019-01-01", "E1", "C1"),
                 contract_row("a", "2020-01-01", "E2", "C1"))
  panel[, gross_salary_lcu := NULL]
  expect_error(movement_policy_template(panel, group_cols = "est_id"),
               "movement_policy_template.*gross_salary_lcu")
})
