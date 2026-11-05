# Unit tests for allowance_core.R functions
# Testing estimate_baseline_allowance_scale() and resolve_allowance_amounts()

# =============================================================================
# estimate_baseline_allowance_scale()
# =============================================================================

# Small single-snapshot panel: C4 has NO row in the allowance table at all
# (for either type) -- this is what makes the "unconditional mean" test
# meaningful, since C4 must still count as a zero in the denominator.
make_allowance_panel <- function() {
  data.table::data.table(
    contract_id    = rep(c("C1", "C2", "C3"), each = 2),
    ref_date       = as.Date("2020-01-01"),
    allowance_type = rep(c("hazard", "transport"), times = 3),
    allowance_lcu  = c(100, 0,    0, 50,    0, 0),
    personnel_id   = rep(c("P1", "P2", "P3"), each = 2)
  )
}

make_contract_panel <- function() {
  data.table::data.table(
    contract_id   = c("C1", "C2", "C3", "C4"),
    ref_date      = as.Date("2020-01-01"),
    personnel_id  = c("P1", "P2", "P3", "P4"),
    est_id        = c("E1", "E1", "E2", "E1"),
    contract_type = "permanent"
  )
}

test_that("estimate_baseline_allowance_scale requires data.table inputs", {
  ct <- make_contract_panel()
  al <- make_allowance_panel()
  expect_error(
    estimate_baseline_allowance_scale(list(), ct, group_cols = "est_id", ref_date = as.Date("2020-01-01")),
    "data.table"
  )
  expect_error(
    estimate_baseline_allowance_scale(al, list(), group_cols = "est_id", ref_date = as.Date("2020-01-01")),
    "data.table"
  )
})

test_that("estimate_baseline_allowance_scale requires group_cols", {
  ct <- make_contract_panel()
  al <- make_allowance_panel()
  expect_error(
    estimate_baseline_allowance_scale(al, ct, group_cols = NULL, ref_date = as.Date("2020-01-01")),
    "group_cols"
  )
})

test_that("estimate_baseline_allowance_scale requires amount_col to be numeric", {
  ct <- make_contract_panel()
  al <- make_allowance_panel()
  al[, allowance_lcu := as.character(allowance_lcu)]
  expect_error(
    estimate_baseline_allowance_scale(al, ct, group_cols = "est_id", ref_date = as.Date("2020-01-01")),
    "numeric"
  )
})

test_that("estimate_baseline_allowance_scale computes the unconditional mean, including contracts absent from the allowance table", {
  ct <- make_contract_panel()
  al <- make_allowance_panel()

  scale_dt <- estimate_baseline_allowance_scale(al, ct, group_cols = "est_id", ref_date = as.Date("2020-01-01"))

  # E1 = {C1, C2, C4}; C4 has no allowance rows at all -> counts as 0 for both types
  expect_equal(scale_dt[est_id == "E1" & allowance_type == "hazard"]$amount, (100 + 0 + 0) / 3)
  expect_equal(scale_dt[est_id == "E1" & allowance_type == "transport"]$amount, (0 + 50 + 0) / 3)

  # E2 = {C3} only, both types 0
  expect_equal(scale_dt[est_id == "E2" & allowance_type == "hazard"]$amount, 0)
  expect_equal(scale_dt[est_id == "E2" & allowance_type == "transport"]$amount, 0)

  expect_equal(sort(names(scale_dt)), sort(c("est_id", "allowance_type", "amount")))
})

test_that("estimate_baseline_allowance_scale does NOT silently compute a conditional mean", {
  ct <- make_contract_panel()
  al <- make_allowance_panel()
  scale_dt <- estimate_baseline_allowance_scale(al, ct, group_cols = "est_id", ref_date = as.Date("2020-01-01"))

  # Conditional mean (only over contracts appearing in the allowance table) would give
  # 100/2 = 50 for E1 x hazard -- the correct unconditional mean must differ from this.
  conditional_wrong <- 100 / 2
  expect_false(isTRUE(all.equal(scale_dt[est_id == "E1" & allowance_type == "hazard"]$amount, conditional_wrong)))
})

test_that("estimate_baseline_allowance_scale resolves ref_date to the nearest prior snapshot, not a blend across periods", {
  al <- data.table::data.table(
    contract_id    = rep(c("C1", "C2"), times = 2),
    ref_date       = rep(as.Date(c("2019-01-01", "2020-01-01")), each = 2),
    allowance_type = "hazard",
    allowance_lcu  = c(999, 999, 100, 0),   # 2019 rows are decoys
    personnel_id   = rep(c("P1", "P2"), times = 2)
  )
  ct <- data.table::data.table(
    contract_id   = rep(c("C1", "C2"), times = 2),
    ref_date      = rep(as.Date(c("2019-01-01", "2020-01-01")), each = 2),
    personnel_id  = rep(c("P1", "P2"), times = 2),
    est_id        = "E1",
    contract_type = "permanent"
  )

  # ref_date falls after the 2020-01-01 snapshot but should snap to it, not to
  # 2019-01-01, and must not blend the two periods together.
  scale_dt <- estimate_baseline_allowance_scale(al, ct, group_cols = "est_id", ref_date = as.Date("2020-06-01"))
  expect_equal(scale_dt$amount, (100 + 0) / 2)
})

test_that("estimate_baseline_allowance_scale errors when no snapshot exists on or before ref_date", {
  ct <- make_contract_panel()
  al <- make_allowance_panel()
  expect_error(
    estimate_baseline_allowance_scale(al, ct, group_cols = "est_id", ref_date = as.Date("1999-01-01")),
    "No dates found"
  )
})

# =============================================================================
# resolve_allowance_amounts()
# =============================================================================

make_resolve_contract_dt <- function() {
  data.table::data.table(
    contract_id   = c("C1", "C2", "C3"),
    personnel_id  = c("P1", "P2", "P3"),
    est_id        = c("E1", "E1", "E2"),
    contract_type = "permanent"
  )
}

test_that("resolve_allowance_amounts requires a data.table", {
  expect_error(
    resolve_allowance_amounts(list(), policy_params = list(defaults = list(amount = 0)),
                              allowance_types = "hazard"),
    "data.table"
  )
})

test_that("resolve_allowance_amounts requires non-empty allowance_types", {
  ct <- make_resolve_contract_dt()
  expect_error(
    resolve_allowance_amounts(ct, policy_params = list(defaults = list(amount = 0)), allowance_types = NULL),
    "allowance_types"
  )
})

test_that("resolve_allowance_amounts rejects multiplier_group_cols containing allowance_type", {
  ct <- make_resolve_contract_dt()
  expect_error(
    resolve_allowance_amounts(
      ct,
      policy_params = list(defaults = list(amount = 0), multiplier_group_cols = "allowance_type"),
      allowance_types = "hazard"
    ),
    "multiplier_group_cols"
  )
})

test_that("resolve_allowance_amounts: scalar default applies uniformly", {
  ct <- make_resolve_contract_dt()
  resolved <- resolve_allowance_amounts(
    ct, policy_params = list(group_cols = NULL, policy_table = NULL, defaults = list(amount = 50)),
    allowance_types = c("hazard", "transport")
  )
  expect_equal(nrow(resolved), 3L * 2L)
  expect_true(all(resolved$amount == 50))
})

test_that("resolve_allowance_amounts: group-level policy_table resolves per allowance_type", {
  ct <- make_resolve_contract_dt()
  pt <- data.table::data.table(allowance_type = c("hazard", "transport"), amount = c(100, 20))
  resolved <- resolve_allowance_amounts(
    ct, policy_params = list(group_cols = "allowance_type", policy_table = pt, defaults = list(amount = 0)),
    allowance_types = c("hazard", "transport")
  )
  expect_true(all(resolved[allowance_type == "hazard"]$amount == 100))
  expect_true(all(resolved[allowance_type == "transport"]$amount == 20))
})

test_that("resolve_allowance_amounts: compound targeting (est_id x allowance_type)", {
  ct <- make_resolve_contract_dt()
  pt <- data.table::data.table(
    est_id         = c("E1", "E1", "E2", "E2"),
    allowance_type = c("hazard", "transport", "hazard", "transport"),
    amount         = c(100, 20, 5, 5)
  )
  resolved <- resolve_allowance_amounts(
    ct, policy_params = list(group_cols = c("est_id", "allowance_type"), policy_table = pt, defaults = list(amount = 0)),
    allowance_types = c("hazard", "transport")
  )
  expect_equal(resolved[contract_id == "C1" & allowance_type == "hazard"]$amount, 100)
  expect_equal(resolved[contract_id == "C3" & allowance_type == "hazard"]$amount, 5)
})

test_that("resolve_allowance_amounts: scalar total_allowance_multiplier scales every type", {
  ct <- make_resolve_contract_dt()
  pt <- data.table::data.table(allowance_type = c("hazard", "transport"), amount = c(100, 20))
  base <- resolve_allowance_amounts(
    ct, policy_params = list(group_cols = "allowance_type", policy_table = pt, defaults = list(amount = 0)),
    allowance_types = c("hazard", "transport")
  )
  scaled <- resolve_allowance_amounts(
    ct, policy_params = list(group_cols = "allowance_type", policy_table = pt, defaults = list(amount = 0),
                             multiplier_default = 1.5),
    allowance_types = c("hazard", "transport")
  )
  expect_equal(scaled$amount, base$amount * 1.5)
})

test_that("resolve_allowance_amounts: group-level multiplier applies per contract, not per type", {
  ct <- make_resolve_contract_dt()
  pt <- data.table::data.table(allowance_type = c("hazard", "transport"), amount = c(100, 20))
  mult_pt <- data.table::data.table(est_id = "E1", total_allowance_multiplier = 2.0)
  resolved <- resolve_allowance_amounts(
    ct,
    policy_params = list(
      group_cols = "allowance_type", policy_table = pt, defaults = list(amount = 0),
      multiplier_group_cols = "est_id", multiplier_policy_table = mult_pt, multiplier_default = 1.0
    ),
    allowance_types = c("hazard", "transport")
  )
  # E1 (C1, C2) doubled; E2 (C3) falls back to multiplier_default = 1.0
  expect_equal(resolved[contract_id == "C1" & allowance_type == "hazard"]$amount, 200)
  expect_equal(resolved[contract_id == "C3" & allowance_type == "hazard"]$amount, 100)
})

test_that("resolve_allowance_amounts: output has one row per contract x allowance_type", {
  ct <- make_resolve_contract_dt()
  resolved <- resolve_allowance_amounts(
    ct, policy_params = list(defaults = list(amount = 0)),
    allowance_types = c("hazard", "transport", "meal")
  )
  expect_equal(nrow(resolved), nrow(ct) * 3L)
  expect_equal(data.table::uniqueN(resolved$contract_id), nrow(ct))
})

test_that("resolve_allowance_amounts excludes pensioner/inactive rows -- retirement relabels in place, it does not remove the row", {
  # simulate_retirement() does NOT delete a retiree's contract row -- it
  # relabels contract_type to "pensioner" and leaves it in contract_dt. If
  # resolve_allowance_amounts() didn't filter on contract_type, a retiree's
  # leftover row would still get assigned a live allowance.
  ct <- data.table::data.table(
    contract_id   = c("C1", "C2", "C3", "C4"),
    personnel_id  = c("P1", "P2", "P3", "P4"),
    est_id        = c("E1", "E1", "E1", "E1"),
    contract_type = c("permanent", "permanent", "pensioner", "inactive")
  )
  pt <- data.table::data.table(allowance_type = "hazard", amount = 100)
  resolved <- resolve_allowance_amounts(
    ct, policy_params = list(group_cols = "allowance_type", policy_table = pt, defaults = list(amount = 0)),
    allowance_types = "hazard"
  )
  expect_equal(sort(resolved$contract_id), c("C1", "C2"))
  expect_equal(nrow(resolved), 2L)
  expect_equal(sum(resolved$amount), 200)
})
