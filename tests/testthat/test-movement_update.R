# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
make_workforce <- function() {
  list(
    contract_dt = data.table::data.table(
      personnel_id      = c("P1", "P2", "P3", "P4", "P5", "P6"),
      contract_id       = paste0("C", 1:6),
      paygrade          = c("G1", "G1", "G1", "G2", "G2", "G2"),
      gross_salary_lcu  = c(3000, 3000, 3000, 5000, 5000, 5000),
      start_date        = as.Date(c("2010-01-01", "2012-01-01", "2014-01-01",
                                    "2008-01-01", "2011-01-01", "2013-01-01")),
      end_date          = as.Date(NA),
      contract_type_code = "permanent"
    ),
    personnel_dt = data.table::data.table(
      personnel_id = c("P1", "P2", "P3", "P4", "P5", "P6"),
      status       = "active"
    ),
    salary_scale = data.table::data.table(
      paygrade         = c("G1", "G2"),
      gross_salary_lcu = c(3000, 5000)
    ),
    demand_dt = data.table::data.table(
      from_group    = c("G1", "G2"),
      to_group      = c("G2", "G1"),
      movement_type = c("promotion", "transfer"),
      adj_prob      = c(0.33, 0.33),
      current_stock = c(3L, 3L),
      n_movers      = c(1L, 1L)
    )
  )
}

# ---------------------------------------------------------------------------
# stochastic_round
# ---------------------------------------------------------------------------
test_that("stochastic_round returns integer", {
  set.seed(1)
  r <- stochastic_round(2.7)
  expect_true(is.integer(r))
})

test_that("stochastic_round is in {floor(x), floor(x)+1}", {
  set.seed(1)
  for (x in c(0.0, 0.3, 0.7, 1.0, 2.5, 10.9)) {
    r <- stochastic_round(x)
    expect_true(r >= floor(x) && r <= ceiling(x))
  }
})

test_that("stochastic_round(0) = 0", {
  expect_equal(stochastic_round(0), 0L)
})

test_that("stochastic_round(n) = n for integer n", {
  expect_equal(stochastic_round(3), 3L)
  expect_equal(stochastic_round(5), 5L)
})

# ---------------------------------------------------------------------------
# identify_movers
# ---------------------------------------------------------------------------
test_that("identify_movers returns correct columns", {
  set.seed(42)
  w <- make_workforce()
  pp <- list(group_cols = "paygrade",
             promotion_strategy = "tenure",
             transfer_strategy  = "random")
  movers <- identify_movers(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    demand_dt    = w$demand_dt,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  expect_true(data.table::is.data.table(movers))
  expect_true(all(c("personnel_id", "from_group", "to_group", "movement_type") %in%
                    names(movers)))
})

test_that("identify_movers selects correct number of movers", {
  set.seed(42)
  w <- make_workforce()
  pp <- list(group_cols = "paygrade",
             promotion_strategy = "tenure",
             transfer_strategy  = "random")
  movers <- identify_movers(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    demand_dt    = w$demand_dt,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  # demand asks for 1 promotion (G1->G2) and 1 transfer (G2->G1)
  expect_equal(movers[movement_type == "promotion", .N], 1L)
  expect_equal(movers[movement_type == "transfer",  .N], 1L)
})

test_that("identify_movers with promotion_strategy=tenure selects longest time-in-grade", {
  set.seed(1)
  w <- make_workforce()
  # Add ref_date for panel tig computation
  ct <- data.table::copy(w$contract_dt)
  ct[, ref_date := as.Date("2016-01-01")]

  pp <- list(group_cols = "paygrade",
             promotion_strategy = "tenure",
             transfer_strategy  = "random")
  # Only ask for 1 promotion from G1
  demand <- data.table::data.table(
    from_group = "G1", to_group = "G2",
    movement_type = "promotion", adj_prob = 0.33,
    current_stock = 3L, n_movers = 1L
  )
  movers <- identify_movers(
    contract_dt  = ct,
    personnel_dt = w$personnel_dt,
    demand_dt    = demand,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  # P1 has earliest start_date (2010) -> longest tenure -> should be selected
  expect_equal(movers$personnel_id, "P1")
})

test_that("identify_movers with promotion_strategy=wage_based selects lowest salary ratio", {
  set.seed(1)
  # P1 has the lowest salary (2000) relative to max in G1 (5000)
  ct <- data.table::data.table(
    personnel_id      = c("P1", "P2", "P3"),
    contract_id       = c("C1", "C2", "C3"),
    paygrade          = c("G1", "G1", "G1"),
    gross_salary_lcu  = c(2000, 3500, 5000),
    start_date        = as.Date("2010-01-01"),
    end_date          = as.Date(NA),
    contract_type_code = "permanent"
  )
  pt <- data.table::data.table(
    personnel_id = c("P1", "P2", "P3"),
    status = "active"
  )
  demand <- data.table::data.table(
    from_group = "G1", to_group = "G2",
    movement_type = "promotion", adj_prob = 0.33,
    current_stock = 3L, n_movers = 1L
  )
  pp <- list(group_cols = "paygrade",
             promotion_strategy = "wage_based",
             transfer_strategy  = "random")
  movers <- identify_movers(
    contract_dt  = ct,
    personnel_dt = pt,
    demand_dt    = demand,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  # P1 has lowest salary ratio -> should be selected
  expect_equal(movers$personnel_id, "P1")
})

test_that("identify_movers with transfer_strategy=reverse_tenure selects shortest tenure", {
  set.seed(1)
  w <- make_workforce()
  demand <- data.table::data.table(
    from_group = "G2", to_group = "G1",
    movement_type = "transfer", adj_prob = 0.33,
    current_stock = 3L, n_movers = 1L
  )
  pp <- list(group_cols = "paygrade",
             promotion_strategy = "tenure",
             transfer_strategy  = "reverse_tenure")
  movers <- identify_movers(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    demand_dt    = demand,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  # P6 has latest start_date in G2 (2013) -> shortest tenure -> selected for LIFO
  expect_equal(movers$personnel_id, "P6")
})

test_that("identify_movers returns empty dt when demand is empty", {
  w <- make_workforce()
  pp <- list(group_cols = "paygrade",
             promotion_strategy = "tenure",
             transfer_strategy  = "random")
  empty_demand <- data.table::data.table(
    from_group = character(0), to_group = character(0),
    movement_type = character(0), adj_prob = numeric(0),
    current_stock = integer(0), n_movers = integer(0)
  )
  movers <- identify_movers(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    demand_dt    = empty_demand,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  expect_equal(nrow(movers), 0L)
})

test_that("identify_movers: no person is selected twice", {
  set.seed(1)
  w <- make_workforce()
  # Ask for 2 movers from G1 in different transitions
  demand <- data.table::data.table(
    from_group    = c("G1", "G1"),
    to_group      = c("G2", "G2"),   # same dest (edge case)
    movement_type = c("promotion", "promotion"),
    adj_prob      = c(0.3, 0.3),
    current_stock = c(3L, 3L),
    n_movers      = c(1L, 1L)
  )
  pp <- list(group_cols = "paygrade",
             promotion_strategy = "tenure",
             transfer_strategy  = "random")
  movers <- identify_movers(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    demand_dt    = demand,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  # personnel_ids should be unique
  expect_equal(
    data.table::uniqueN(movers$personnel_id),
    nrow(movers)
  )
})

test_that("identify_movers caps selection at pool size", {
  set.seed(1)
  w <- make_workforce()
  # Ask for 10 movers but only 3 in pool
  demand <- data.table::data.table(
    from_group = "G1", to_group = "G2",
    movement_type = "promotion", adj_prob = 1.0,
    current_stock = 3L, n_movers = 10L
  )
  pp <- list(group_cols = "paygrade",
             promotion_strategy = "tenure",
             transfer_strategy  = "random")
  movers <- identify_movers(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    demand_dt    = demand,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  expect_lte(nrow(movers), 3L)
})

# ---------------------------------------------------------------------------
# update_state_with_movement
# ---------------------------------------------------------------------------
test_that("update_state_with_movement returns list with required elements", {
  w <- make_workforce()
  movers <- data.table::data.table(
    personnel_id  = c("P1"),
    from_group    = c("G1"),
    to_group      = c("G2"),
    movement_type = c("promotion")
  )
  pp <- list(group_cols = "paygrade", salary_scale = w$salary_scale)
  result <- update_state_with_movement(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    movers_dt    = movers,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  expect_true(is.list(result))
  expect_true(all(c("contract_dt", "personnel_dt", "movers_dt") %in% names(result)))
})

test_that("update_state_with_movement changes paygrade for mover", {
  w <- make_workforce()
  movers <- data.table::data.table(
    personnel_id  = "P1",
    from_group    = "G1",
    to_group      = "G2",
    movement_type = "promotion"
  )
  pp <- list(group_cols = "paygrade", salary_scale = w$salary_scale)
  result <- update_state_with_movement(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    movers_dt    = movers,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  # P1 should now be in G2
  p1_grade <- result$contract_dt[personnel_id == "P1", paygrade]
  expect_equal(p1_grade, "G2")
})

test_that("update_state_with_movement assigns new salary", {
  w <- make_workforce()
  movers <- data.table::data.table(
    personnel_id  = "P1",
    from_group    = "G1",
    to_group      = "G2",
    movement_type = "promotion"
  )
  pp <- list(group_cols = "paygrade", salary_scale = w$salary_scale)
  result <- update_state_with_movement(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    movers_dt    = movers,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  p1_salary <- result$contract_dt[personnel_id == "P1", gross_salary_lcu]
  # G2 salary = 5000
  expect_equal(p1_salary, 5000)
})

test_that("update_state_with_movement does not change non-movers", {
  w <- make_workforce()
  movers <- data.table::data.table(
    personnel_id  = "P1",
    from_group    = "G1",
    to_group      = "G2",
    movement_type = "promotion"
  )
  pp <- list(group_cols = "paygrade", salary_scale = w$salary_scale)
  result <- update_state_with_movement(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    movers_dt    = movers,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  # P2 and P3 (G1, not moved) should still be G1
  unchanged <- result$contract_dt[personnel_id %in% c("P2", "P3"), unique(paygrade)]
  expect_equal(unchanged, "G1")
})

test_that("update_state_with_movement returns unchanged data when no movers", {
  w <- make_workforce()
  empty_movers <- data.table::data.table(
    personnel_id  = character(0),
    from_group    = character(0),
    to_group      = character(0),
    movement_type = character(0)
  )
  pp <- list(group_cols = "paygrade", salary_scale = w$salary_scale)
  result <- update_state_with_movement(
    contract_dt  = w$contract_dt,
    personnel_dt = w$personnel_dt,
    movers_dt    = empty_movers,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  expect_equal(nrow(result$contract_dt), nrow(w$contract_dt))
  expect_equal(result$contract_dt$paygrade, w$contract_dt$paygrade)
})

test_that("update_state_with_movement errors on duplicate salary_scale keys", {
  w <- make_workforce()
  dup_scale <- data.table::rbindlist(list(w$salary_scale, w$salary_scale))
  movers <- data.table::data.table(
    personnel_id  = "P1", from_group = "G1",
    to_group = "G2", movement_type = "promotion"
  )
  pp <- list(group_cols = "paygrade", salary_scale = dup_scale)
  expect_error(
    update_state_with_movement(
      contract_dt  = w$contract_dt,
      personnel_dt = w$personnel_dt,
      movers_dt    = movers,
      policy_params = pp,
      ref_date     = "2016-01-01"
    ),
    "[Dd]uplicate"
  )
})

test_that("update_state_with_movement errors when salary_scale missing", {
  w <- make_workforce()
  movers <- data.table::data.table(
    personnel_id = "P1", from_group = "G1",
    to_group = "G2", movement_type = "promotion"
  )
  pp <- list(group_cols = "paygrade", salary_scale = NULL)
  expect_error(
    update_state_with_movement(
      contract_dt  = w$contract_dt,
      personnel_dt = w$personnel_dt,
      movers_dt    = movers,
      policy_params = pp,
      ref_date     = "2016-01-01"
    ),
    "salary_scale"
  )
})

test_that("update_state_with_movement handles multi-column group_cols", {
  ct <- data.table::data.table(
    personnel_id      = c("P1", "P2"),
    contract_id       = c("C1", "C2"),
    est_id            = c("E1", "E2"),
    paygrade          = c("G1", "G2"),
    gross_salary_lcu  = c(3000, 5000),
    start_date        = as.Date("2010-01-01"),
    end_date          = as.Date(NA),
    contract_type_code = "permanent"
  )
  pt <- data.table::data.table(
    personnel_id = c("P1", "P2"),
    status = "active"
  )
  scale <- data.table::data.table(
    est_id = c("E1", "E2"),
    paygrade = c("G1", "G2"),
    gross_salary_lcu = c(3000, 5000)
  )
  movers <- data.table::data.table(
    personnel_id  = "P1",
    from_group    = "E1||G1",
    to_group      = "E2||G2",
    movement_type = "transfer"
  )
  pp <- list(group_cols = c("est_id", "paygrade"), salary_scale = scale)
  result <- update_state_with_movement(
    contract_dt  = ct,
    personnel_dt = pt,
    movers_dt    = movers,
    policy_params = pp,
    ref_date     = "2016-01-01"
  )
  # P1 should now have est_id=E2 and paygrade=G2
  expect_equal(result$contract_dt[personnel_id == "P1", est_id],   "E2")
  expect_equal(result$contract_dt[personnel_id == "P1", paygrade], "G2")
  expect_equal(result$contract_dt[personnel_id == "P1", gross_salary_lcu], 5000)
})

test_that("update_state_with_movement warns on unmatched salary", {
  w <- make_workforce()
  # mover to a group not in salary_scale
  movers <- data.table::data.table(
    personnel_id  = "P1",
    from_group    = "G1",
    to_group      = "G99",  # Not in salary_scale
    movement_type = "promotion"
  )
  pp <- list(group_cols = "paygrade", salary_scale = w$salary_scale)
  expect_warning(
    update_state_with_movement(
      contract_dt  = w$contract_dt,
      personnel_dt = w$personnel_dt,
      movers_dt    = movers,
      policy_params = pp,
      ref_date     = "2016-01-01"
    )
  )
})
