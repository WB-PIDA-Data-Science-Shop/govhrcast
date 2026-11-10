# Renaming the input columns does not change the results (ADR 0012)
#
# Every exported function reads the caller's column names from
# column_spec_args and uses them all the way down. So a run on data whose
# columns carry other names must match the run on default-named data exactly,
# once the names are mapped back -- and the caller's names are what come out.

library(data.table)

# The caller's name for each default column (default name = caller name)
user_names <- c(
  personnel_id      = "pid",
  contract_id       = "cid",
  ref_date          = "snap",
  start_date        = "inicio",
  end_date          = "fim",
  gross_salary_lcu  = "bruto",
  contract_type     = "tipo",
  employment_status = "situacao",
  age               = "idade",
  personnel_tenure  = "tempo",
  birth_date        = "nasc"
)

# The same mapping, as the column_spec_args a caller would pass
user_spec <- define_column_spec(
  personnel_id_col  = "pid",
  contract_id_col   = "cid",
  ref_date_col      = "snap",
  start_date_col    = "inicio",
  end_date_col      = "fim",
  salary_col        = "bruto",
  contract_type_col = "tipo",
  status_col        = "situacao",
  age_col           = "idade",
  tenure_col        = "tempo",
  birth_date_col    = "nasc"
)

# Columns the package creates itself keep their own names in every run
package_made <- "age"

to_user_names <- function(dt) {
  setnames(copy(dt), names(user_names), user_names, skip_absent = TRUE)
}

# Back to default names
to_default_names <- function(dt) {
  setnames(copy(dt), user_names, names(user_names), skip_absent = TRUE)
}

# Two Alagoas snapshots (so the run must pick one via ref_date_col), in
# default and in renamed form
renamed_fixture <- function(earlier = as.Date("2011-09-01"), snap = as.Date("2012-09-01")) {
  keep <- c(earlier, snap)
  hc <- bra_hrmis_contract[ref_date %in% keep]
  hp <- bra_hrmis_personnel[ref_date %in% keep]
  list(hc = hc, hp = hp, hc_user = to_user_names(hc), hp_user = to_user_names(hp),
       ref_date = snap)
}

# Run simulate_retirement() on default names and on renamed data
retire_both_ways <- function(policy_default, policy_user, seed = 1L) {
  d <- renamed_fixture()
  set.seed(seed)
  default_run <- simulate_retirement(d$hc, d$hp, as_of_date = d$ref_date,
                                     policy_params = policy_default)
  set.seed(seed)
  user_run <- simulate_retirement(d$hc_user, d$hp_user, as_of_date = d$ref_date,
                                  policy_params = policy_user,
                                  column_spec_args = user_spec)
  list(default = default_run, user = user_run)
}

expect_same_results_in_user_names <- function(runs, result_table = "retirees_dt") {
  for (tbl in c("contract_dt", "personnel_dt", result_table)) {
    out <- runs$user[[tbl]]
    expect_true("pid" %in% names(out), info = tbl)
    expect_length(setdiff(intersect(names(user_names), names(out)), package_made), 0L)
    expect_equal(to_default_names(out), runs$default[[tbl]], info = tbl)
  }
  expect_equal(runs$user$summary, runs$default$summary)
}


# ---- simulate_retirement() ----------------------------------------------------

test_that("simulate_retirement(): renamed columns give the default-name results", {
  accrual_by_type <- function(type_col) {
    setnames(data.table(x = c("permanent", "fixed-term", "short-term"),
                        accrual_rate = c(0.02, 0.015, 0.01)), "x", type_col)
  }
  policy <- function(type_col) list(
    group_cols   = type_col,
    policy_table = accrual_by_type(type_col),
    defaults     = list(eligibility_type = "age_and_tenure", min_age = 60,
                        min_tenure = 10, pension_type = "db", accrual_rate = 0.02,
                        max_years = 35, replacement_cap = 0.8)
  )
  runs <- retire_both_ways(policy("contract_type"), policy("tipo"))
  expect_gt(nrow(runs$default$retirees_dt), 0L)
  expect_same_results_in_user_names(runs)
})

test_that("simulate_retirement(): custom rules and formulas use the caller's names", {
  policy <- function(rule, formula) list(
    group_cols = NULL, policy_table = NULL,
    defaults = list(eligibility_type = "custom", eligibility_rule = rule,
                    pension_type = "custom", pension_formula = formula)
  )
  # A pension formula sees the retiree's contract columns plus the package's
  # own age and tenure_years (age at ref_date), which keep their names
  default_formula <- "gross_salary_lcu * (age - 40) / 100"
  user_formula    <- "bruto * (age - 40) / 100"
  runs <- retire_both_ways(
    policy("age >= 60 & personnel_tenure >= 10", default_formula),
    policy("idade >= 60 & tempo >= 10",          user_formula)
  )
  expect_gt(nrow(runs$default$retirees_dt), 0L)

  # The formula carried on retirees_dt is exactly the caller's text
  expect_identical(unique(runs$user$retirees_dt$pension_formula), user_formula)
  runs$user$retirees_dt[, pension_formula := default_formula]
  expect_same_results_in_user_names(runs)
})

test_that("simulate_retirement(): hazard-rate retirement works on renamed columns", {
  policy <- list(
    group_cols   = "age_yr_int",
    policy_table = data.table(age_yr_int = 55:75, hazard_rate = 0.2),
    defaults     = list(eligibility_type = "hazard_rate", pension_type = "rate",
                        pension_rate = 0.5)
  )
  runs <- retire_both_ways(policy, policy)
  expect_gt(nrow(runs$default$retirees_dt), 0L)
  expect_same_results_in_user_names(runs)
})

test_that("simulate_retirement(): the caller's tables are left unchanged", {
  # Guard: the run works on copies, never on the caller's tables.
  d <- renamed_fixture()
  hc_before <- copy(d$hc_user)
  hp_before <- copy(d$hp_user)
  simulate_retirement(
    d$hc_user, d$hp_user, as_of_date = d$ref_date,
    policy_params = list(group_cols = NULL, policy_table = NULL,
                         defaults = list(eligibility_type = "age_only", min_age = 60,
                                         pension_type = "rate", pension_rate = 0.5)),
    column_spec_args = user_spec)
  expect_identical(d$hc_user, hc_before)
  expect_identical(d$hp_user, hp_before)
})

test_that("simulate_retirement(): age_col = NULL means 'compute age from birth dates'", {
  # Guard: with age_col and tenure_col set to NULL, precomputed age and
  # tenure columns are ignored, even when they exist.
  d <- renamed_fixture()
  policy <- list(group_cols = NULL, policy_table = NULL,
                 defaults = list(eligibility_type = "age_only", min_age = 60,
                                 pension_type = "rate", pension_rate = 0.5))
  run <- function(hp) simulate_retirement(
    d$hc, hp, as_of_date = d$ref_date, policy_params = policy,
    column_spec_args = define_column_spec(age_col = NULL, tenure_col = NULL))
  # A precomputed age ten years off: ignored, so the results must not move
  with_age    <- run(copy(d$hp)[, age := age + 10])
  without_age <- run(copy(d$hp)[, c("age", "personnel_tenure") := NULL])
  expect_equal(with_age$retirees_dt, without_age$retirees_dt)
})

test_that("simulate_retirement(): age from birth dates works on renamed columns", {
  # No age column used (age_col = NULL) and tenure-only eligibility: age is
  # computed from birth dates both for eligibility and for the retiree table
  policy <- list(
    group_cols = NULL, policy_table = NULL,
    defaults = list(eligibility_type = "tenure_only", min_tenure = 25,
                    pension_type = "rate", pension_rate = 0.5)
  )
  d <- renamed_fixture()
  no_age <- define_column_spec(age_col = NULL)
  user_no_age <- user_spec
  user_no_age["age_col"] <- list(NULL)
  runs <- list(
    default = simulate_retirement(d$hc, d$hp, as_of_date = d$ref_date,
                                  policy_params = policy,
                                  column_spec_args = no_age),
    user    = simulate_retirement(d$hc_user, d$hp_user, as_of_date = d$ref_date,
                                  policy_params = policy,
                                  column_spec_args = user_no_age)
  )
  expect_gt(nrow(runs$default$retirees_dt), 0L)
  expect_false(anyNA(runs$user$retirees_dt$age))
  expect_same_results_in_user_names(runs)
})

test_that("identify_eligibility(): every person is listed under the caller's ID column", {
  # People without an active contract are listed with retire = 0 under the
  # same ID column as everyone else -- callers join on that column.
  d <- renamed_fixture()
  hc <- d$hc_user[snap == d$ref_date]
  hp <- d$hp_user[snap == d$ref_date]
  hc <- hc[!pid %in% hp$pid[1:5]]   # five people lose their contracts
  out <- identify_eligibility(
    hc, hp, as_of_date = d$ref_date,
    policy_params = list(group_cols = NULL, policy_table = NULL,
                         defaults = list(eligibility_type = "age_only", min_age = 60)),
    column_spec_args = user_spec
  )
  expect_identical(names(out), c("pid", "retire", "age", "tenure_years"))
  expect_setequal(out$pid, hp$pid)
  expect_true(all(out[pid %in% hp$pid[1:5], retire] == 0L))
})


# ---- simulate_exits() ---------------------------------------------------------

# simulate_exits() works on one snapshot, so the fixture is cut to ref_date
exit_fixture <- function() {
  d <- renamed_fixture()
  list(hc = d$hc[ref_date == d$ref_date], hp = d$hp[ref_date == d$ref_date],
       hc_user = d$hc_user[snap == d$ref_date], hp_user = d$hp_user[snap == d$ref_date],
       ref_date = d$ref_date)
}

exit_both_ways <- function(policy_default, policy_user, seed = 1L) {
  d <- exit_fixture()
  set.seed(seed)
  default_run <- simulate_exits(d$hc, d$hp, policy_params = policy_default,
                                as_of_date = d$ref_date)
  set.seed(seed)
  user_run <- simulate_exits(d$hc_user, d$hp_user, policy_params = policy_user,
                             as_of_date = d$ref_date, column_spec_args = user_spec)
  list(default = default_run, user = user_run)
}

test_that("simulate_exits(): a flat rate on renamed columns gives the default-name results", {
  policy <- list(group_cols = NULL, policy_table = NULL,
                 defaults = list(exit_rate = 0.1))
  runs <- exit_both_ways(policy, policy)
  expect_gt(nrow(runs$default$exits_dt), 0L)
  expect_gt(runs$default$summary$exit_savings, 0)
  expect_same_results_in_user_names(runs, "exits_dt")
})

test_that("simulate_exits(): a one-row policy table on renamed columns gives the default-name results", {
  policy <- list(group_cols = NULL, policy_table = data.table(exit_rate = 0.1),
                 defaults = list())
  runs <- exit_both_ways(policy, policy)
  expect_gt(nrow(runs$default$exits_dt), 0L)
  expect_same_results_in_user_names(runs, "exits_dt")
})

test_that("simulate_exits(): rates grouped on a renamed column give the default-name results", {
  policy <- function(type_col) list(
    group_cols   = type_col,
    policy_table = setnames(data.table(x = c("permanent", "fixed-term", "short-term"),
                                       exit_rate = c(0.05, 0.2, 0.3)), "x", type_col),
    defaults     = list(exit_rate = 0)
  )
  runs <- exit_both_ways(policy("contract_type"), policy("tipo"))
  expect_gt(nrow(runs$default$exits_dt), 0L)
  expect_same_results_in_user_names(runs, "exits_dt")
})

test_that("simulate_exits(): a ranking exit strategy reads the caller's column", {
  # Lowest-paid leave first. With random selection (what an unrecognised name
  # falls back to) the leavers would differ from the lowest-paid.
  policy <- function(pay_col) list(
    group_cols = NULL, policy_table = NULL,
    defaults = list(exit_rate = 0.1, exit_strategy = pay_col)
  )
  runs <- exit_both_ways(policy("gross_salary_lcu"), policy("bruto"))
  expect_gt(nrow(runs$default$exits_dt), 0L)
  expect_same_results_in_user_names(runs, "exits_dt")
})

test_that("simulate_exits(): the caller's tables are left unchanged", {
  # Guard: the run works on copies, never on the caller's tables.
  d <- exit_fixture()
  hc_before <- copy(d$hc_user)
  hp_before <- copy(d$hp_user)
  simulate_exits(d$hc_user, d$hp_user,
                 policy_params = list(group_cols = NULL, policy_table = NULL,
                                      defaults = list(exit_rate = 0.1)),
                 as_of_date = d$ref_date, column_spec_args = user_spec)
  expect_identical(d$hc_user, hc_before)
  expect_identical(d$hp_user, hp_before)
})

test_that("simulate_exits(): a column the run needs must exist, and unused keys are not checked", {
  d <- exit_fixture()
  policy <- list(group_cols = NULL, policy_table = NULL, defaults = list(exit_rate = 0.1))
  run <- function(spec) simulate_exits(d$hc_user, d$hp_user, policy_params = policy,
                                       as_of_date = d$ref_date, column_spec_args = spec)

  for (key in c("personnel_id_col", "contract_type_col", "salary_col", "end_date_col", "status_col")) {
    bad <- modifyList(user_spec, setNames(list("not_a_column"), key))
    expect_error(run(bad), paste0(key, " = 'not_a_column'"), fixed = TRUE)
  }

  # simulate_exits() never reads birth dates or contract IDs, so it does not ask for them
  unused <- modifyList(user_spec, list(birth_date_col = "not_a_column", contract_id_col = "also_not"))
  expect_no_error(run(unused))
})


# ---- simulate_promotions_transfers() ------------------------------------------

# The pay table's pay column has the same name as salary_col, in each run's names
movement_scale <- function(hc, ref) {
  hc[ref_date == ref & !is.na(paygrade),
     .(gross_salary_lcu = mean(gross_salary_lcu, na.rm = TRUE)), by = paygrade][!is.na(gross_salary_lcu)]
}

# People change paygrade on their main contract in this pair of snapshots
moves_fixture <- function() renamed_fixture(as.Date("2013-09-01"), as.Date("2014-09-01"))

move_both_ways <- function(policy_default, policy_user, d = moves_fixture(), seed = 1L) {
  scale <- movement_scale(d$hc, d$ref_date)
  set.seed(seed)
  default_run <- simulate_promotions_transfers(
    d$hc, d$hp, salary_scale_dt = scale, policy_params = policy_default,
    as_of_date = d$ref_date)
  set.seed(seed)
  user_run <- simulate_promotions_transfers(
    d$hc_user, d$hp_user, salary_scale_dt = to_user_names(scale), policy_params = policy_user,
    as_of_date = d$ref_date, column_spec_args = user_spec)
  list(default = default_run, user = user_run)
}

expect_same_movement <- function(runs) {
  expect_same_results_in_user_names(runs, "movers_dt")
  expect_equal(runs$user$demand_dt, runs$default$demand_dt)
  expect_equal(runs$user$baseline_matrix, runs$default$baseline_matrix)
}

movement_policy <- function(strategy = "random", rule = "scale", rate = 0, table = NULL) list(
  group_cols   = "paygrade",
  policy_table = table,
  defaults     = list(movement_rate = rate, movement_strategy = strategy,
                      salary_update_rule = rule)
)

test_that("simulate_promotions_transfers(): every selection strategy works on renamed columns", {
  for (strategy in c("tenure", "reverse_tenure", "wage_based", "random")) {
    runs <- move_both_ways(movement_policy(strategy), movement_policy(strategy))
    expect_gt(nrow(runs$default$movers_dt), 0L)
    expect_identical(names(runs$user$movers_dt)[[1L]], "pid", label = strategy)
    expect_same_movement(runs)
  }
})

test_that("simulate_promotions_transfers(): the 'keep' pay rule works on renamed columns", {
  runs <- move_both_ways(movement_policy(rule = "keep"), movement_policy(rule = "keep"))
  expect_gt(nrow(runs$default$movers_dt), 0L)
  expect_same_movement(runs)
})

test_that("simulate_promotions_transfers(): a ranking column reads the caller's name", {
  policy <- function(col) c(movement_policy(), list(promotion_order_col = col))
  runs <- move_both_ways(policy("gross_salary_lcu"), policy("bruto"))
  expect_gt(nrow(runs$default$movers_dt), 0L)
  expect_same_movement(runs)
})

test_that("simulate_promotions_transfers(): a supplied rate table works on renamed columns", {
  d <- moves_fixture()
  rates <- estimate_historical_movement_rates(d$hc, group_cols = "paygrade")
  rates <- rates[, .(from_group, to_group, movement_rate = pmin(movement_rate * 2, 0.5))]
  runs <- move_both_ways(movement_policy(table = rates), movement_policy(table = rates), d)
  expect_gt(nrow(runs$default$movers_dt), 0L)
  expect_same_movement(runs)
})

test_that("simulate_promotions_transfers(): the flat-rate fallback works on renamed columns", {
  # Both snapshots hold the same people in the same grades, so nothing can be
  # estimated and defaults$movement_rate drives the run
  d <- renamed_fixture()
  earlier <- as.Date("2011-09-01")
  stack <- function(dt, date_col) {
    now <- dt[get(date_col) == d$ref_date]
    rbind(copy(now)[, (date_col) := earlier], now)
  }
  d$hc <- stack(d$hc, "ref_date");  d$hp <- stack(d$hp, "ref_date")
  d$hc_user <- stack(d$hc_user, "snap");  d$hp_user <- stack(d$hp_user, "snap")

  policy <- movement_policy(rate = 0.2)
  runs <- suppressMessages(move_both_ways(policy, policy, d))
  expect_gt(nrow(runs$default$movers_dt), 0L)
  expect_same_movement(runs)
})

test_that("simulate_promotions_transfers(): an empty result is under the caller's ID column too", {
  d <- renamed_fixture()
  scale <- to_user_names(movement_scale(d$hc, d$ref_date))

  # One snapshot and no rate table: returns before any selection
  single <- suppressMessages(simulate_promotions_transfers(
    d$hc_user[snap == d$ref_date], d$hp_user[snap == d$ref_date], scale,
    movement_policy(rate = 0.1), as_of_date = d$ref_date, column_spec_args = user_spec))
  expect_identical(names(single$movers_dt), c("pid", "from_group", "to_group"))

  # A rate table that matches nobody: demand is empty, so nobody is selected
  rates <- data.table(from_group = "x", to_group = "y", movement_rate = 0)
  none <- simulate_promotions_transfers(
    d$hc_user, d$hp_user, scale, movement_policy(table = rates),
    as_of_date = d$ref_date, column_spec_args = user_spec)
  expect_identical(names(none$movers_dt), c("pid", "from_group", "to_group"))
})

test_that("simulate_promotions_transfers(): the caller's tables are left unchanged", {
  # Guard: the run works on copies, never on the caller's tables.
  d <- renamed_fixture()
  hc_before <- copy(d$hc_user)
  hp_before <- copy(d$hp_user)
  simulate_promotions_transfers(d$hc_user, d$hp_user, to_user_names(movement_scale(d$hc, d$ref_date)),
                                movement_policy(), as_of_date = d$ref_date,
                                column_spec_args = user_spec)
  expect_identical(d$hc_user, hc_before)
  expect_identical(d$hp_user, hp_before)
})

test_that("simulate_promotions_transfers(): a column the run needs must exist, and unused keys are not checked", {
  d <- renamed_fixture()
  run <- function(spec) simulate_promotions_transfers(
    d$hc_user, d$hp_user, to_user_names(movement_scale(d$hc, d$ref_date)), movement_policy(),
    as_of_date = d$ref_date, column_spec_args = spec)

  for (key in c("personnel_id_col", "contract_id_col", "start_date_col", "end_date_col",
                "salary_col", "contract_type_col", "status_col")) {
    bad <- modifyList(user_spec, setNames(list("not_a_column"), key))
    expect_error(run(bad), paste0(key, " = 'not_a_column'"), fixed = TRUE)
  }

  # Movement never reads birth dates or a stored age, so it does not ask for them
  unused <- modifyList(user_spec, list(birth_date_col = "not_a_column", age_col = "also_not"))
  expect_no_error(run(unused))
})

test_that("simulate_promotions_transfers(): matched movers raise no false 'unmatched' warning", {
  # Every destination is in the pay table, so nobody can be unmatched
  d <- renamed_fixture()
  expect_no_warning(simulate_promotions_transfers(
    d$hc_user, d$hp_user, to_user_names(movement_scale(d$hc, d$ref_date)), movement_policy(),
    as_of_date = d$ref_date, column_spec_args = user_spec))
})

test_that("identify_movers(): time in grade uses the caller's snapshot column", {
  # Called directly on a panel, so time in grade comes from the snapshots.
  # If the snapshot column were not found it would come from contract start
  # dates instead, and rank people differently.
  d <- moves_fixture()
  demand <- move_both_ways(movement_policy(), movement_policy(), d)$default$demand_dt
  policy <- movement_policy("tenure")
  default_movers <- identify_movers(d$hc, d$hp[ref_date == d$ref_date], demand, policy,
                                    d$ref_date)
  user_movers <- identify_movers(d$hc_user, d$hp_user[snap == d$ref_date], demand, policy,
                                 d$ref_date, column_spec_args = user_spec)
  expect_gt(nrow(default_movers), 0L)
  expect_equal(setnames(copy(user_movers), "pid", "personnel_id"), default_movers)
})

test_that("compute_time_in_grade(): every path answers under the caller's ID column", {
  d <- renamed_fixture()
  spec <- user_spec
  to_default <- function(dt) setnames(copy(dt), "pid", "personnel_id")

  # Several snapshots: earliest snapshot in the current grade
  many_default <- compute_time_in_grade(d$hc, d$ref_date, "paygrade")
  many_user    <- compute_time_in_grade(d$hc_user, d$ref_date, "paygrade", column_spec_args = spec)
  expect_identical(names(many_user), c("pid", "time_in_grade"))
  expect_equal(to_default(many_user), many_default)

  # One snapshot: falls back to the contract start date
  one_default <- compute_time_in_grade(d$hc[ref_date == d$ref_date], d$ref_date, "paygrade")
  one_user    <- compute_time_in_grade(d$hc_user[snap == d$ref_date], d$ref_date, "paygrade",
                                       column_spec_args = spec)
  expect_identical(names(one_user), c("pid", "time_in_grade"))
  expect_equal(to_default(one_user), one_default)

  # Nobody active
  gone <- copy(d$hc_user)[, tipo := "inactive"]
  empty <- compute_time_in_grade(gone, d$ref_date, "paygrade", column_spec_args = spec)
  expect_identical(names(empty), c("pid", "time_in_grade"))
  expect_equal(nrow(empty), 0L)
})


# ---- simulate_hiring() --------------------------------------------------------

hiring_scale <- function(d) {
  # Includes the bundled data's missing paygrade: staff there can be replaced
  # too, and new hires must find a row in the pay table
  d$hc[ref_date == d$ref_date, .(gross_salary_lcu = mean(gross_salary_lcu, na.rm = TRUE)), by = paygrade]
}

# The same policy in the caller's names: its pay table's pay column follows salary_col
user_policy <- function(policy) {
  if (!is.null(policy$salary_scale)) policy$salary_scale <- to_user_names(policy$salary_scale)
  policy
}

# Retirees retiring at the fixture's snapshot, under each run's own names
retirees_of <- function(hc, hp, ref, spec = define_column_spec()) {
  simulate_retirement(hc, hp, as_of_date = ref, column_spec_args = spec, policy_params = list(
    group_cols = NULL, policy_table = NULL,
    defaults = list(eligibility_type = "age_only", min_age = 60, pension_type = "rate",
                    pension_rate = 0.5)
  ))$retirees_dt
}

hire_both_ways <- function(policy, retirees_default = NULL, retirees_user = NULL,
                           d = renamed_fixture(), seed = 1L) {
  set.seed(seed)
  default_run <- simulate_hiring(d$hc, d$hp, policy_params = policy,
                              leavers_dt = retirees_default, as_of_date = d$ref_date)
  set.seed(seed)
  user_run <- simulate_hiring(d$hc_user, d$hp_user, policy_params = user_policy(policy),
                           leavers_dt = retirees_user, as_of_date = d$ref_date,
                           column_spec_args = user_spec)
  list(default = default_run, user = user_run)
}

expect_same_hiring <- function(runs, new_hires = TRUE) {
  tables <- if (new_hires) c("new_hires_dt", "new_hire_contracts_dt") else character(0)
  expect_same_results_in_user_names(runs, tables)
  expect_equal(runs$user$adjustment_dt, runs$default$adjustment_dt)
}

# Current staff per grade, so targets can sit above or below it
grade_stock <- function(d) {
  compute_current_stock(d$hc[ref_date == d$ref_date], d$hp[ref_date == d$ref_date],
                        d$ref_date, group_cols = "paygrade")[!is.na(paygrade)]
}

test_that("simulate_hiring(): flow mode with a retirees table works on renamed columns", {
  d <- renamed_fixture()
  policy <- list(mode = "flow", group_cols = "paygrade", replacement_rate = 1,
                 salary_scale = hiring_scale(d))
  runs <- hire_both_ways(policy,
                         retirees_of(d$hc, d$hp, d$ref_date),
                         retirees_of(d$hc_user, d$hp_user, d$ref_date, user_spec))
  expect_gt(runs$default$summary$n_new_hires, 0L)
  expect_same_hiring(runs)
})

test_that("simulate_hiring(): flow mode recovers the groups of a bare retirees table", {
  # A retirees table holding only IDs: the groups come from the contract table
  d <- renamed_fixture()
  policy <- list(mode = "flow", group_cols = "paygrade", replacement_rate = 1,
                 salary_scale = hiring_scale(d))
  ids_default <- retirees_of(d$hc, d$hp, d$ref_date)[, .(personnel_id)]
  ids_user    <- retirees_of(d$hc_user, d$hp_user, d$ref_date, user_spec)[, .(pid)]
  runs <- hire_both_ways(policy, ids_default, ids_user)
  expect_gt(runs$default$summary$n_new_hires, 0L)
  expect_same_hiring(runs)
})

test_that("simulate_hiring(): flow mode finds retirees itself on renamed columns", {
  # No retirees table: eligibility is worked out from the policy
  d <- renamed_fixture()
  policy <- list(mode = "flow", group_cols = "paygrade", replacement_rate = 1,
                 salary_scale = hiring_scale(d),
                 defaults = list(eligibility_type = "age_only", min_age = 60))
  d$hc <- d$hc[ref_date == d$ref_date];  d$hp <- d$hp[ref_date == d$ref_date]
  d$hc_user <- d$hc_user[snap == d$ref_date];  d$hp_user <- d$hp_user[snap == d$ref_date]
  runs <- hire_both_ways(policy, d = d)
  expect_gt(runs$default$summary$n_new_hires, 0L)
  expect_same_hiring(runs)
})

test_that("simulate_hiring(): stock mode hires and downsizes on renamed columns", {
  # Every other grade grows or shrinks, so both branches run. Pay saved by
  # downsizing is read from the caller's pay column.
  d <- renamed_fixture()
  for (strategy in c("last_hired_first", "random")) {
    stock <- grade_stock(d)
    targets <- stock[, .(paygrade, target_stock = fifelse(seq_len(.N) %% 2L == 0L,
                                                          current_stock + 2L,
                                                          pmax(current_stock - 2L, 0L)))]
    policy <- list(mode = "stock", group_cols = "paygrade", stock_targets = targets,
                   salary_scale = hiring_scale(d), removal_strategy = strategy)
    runs <- hire_both_ways(policy)
    expect_gt(runs$default$summary$n_new_hires, 0L)
    expect_true(any(runs$default$adjustment_dt$net_change < 0L))
    expect_same_hiring(runs)
  }
})

test_that("simulate_hiring(): downsizing with no groups reads the caller's columns", {
  # Hiring with no groups is a separate open bug (B-32); shrinking works
  d <- renamed_fixture()
  stock <- compute_current_stock(d$hc[ref_date == d$ref_date], d$hp[ref_date == d$ref_date],
                                 d$ref_date)$current_stock
  policy <- list(mode = "stock", group_cols = NULL,
                 stock_targets = data.table(target_stock = stock - 5L))
  runs <- hire_both_ways(policy)
  expect_equal(sum(runs$default$adjustment_dt$net_change), -5L)
  expect_lt(runs$default$summary$total_new_salary_cost, 0)
  expect_same_hiring(runs, new_hires = FALSE)
})

test_that("simulate_hiring(): status-quo mode works on renamed columns", {
  d <- renamed_fixture()
  rates <- grade_stock(d)[, .(paygrade, hiring_rate = 0.1)]
  policy <- list(mode = "status_quo", group_cols = "paygrade", squorate_dt = rates,
                 salary_scale = hiring_scale(d))
  runs <- hire_both_ways(policy)
  expect_gt(runs$default$summary$n_new_hires, 0L)
  expect_same_hiring(runs)
})

test_that("design_hiring_policy_adjustment(): your own rates work on renamed columns", {
  d <- renamed_fixture()
  rates <- grade_stock(d)[, .(paygrade, hiring_rate = 0.1)]
  policy <- design_hiring_policy_adjustment(rates, group_cols = "paygrade",
                                            salary_scale = hiring_scale(d))
  runs <- hire_both_ways(policy)
  expect_gt(runs$default$summary$n_new_hires, 0L)
  expect_same_hiring(runs)
})

test_that("simulate_hiring(): a run with no change reports headcount on renamed columns", {
  d <- renamed_fixture()
  policy <- list(mode = "flow", group_cols = NULL, replacement_rate = 0)
  retirees_default <- retirees_of(d$hc, d$hp, d$ref_date)
  retirees_user    <- retirees_of(d$hc_user, d$hp_user, d$ref_date, user_spec)
  runs <- hire_both_ways(policy, retirees_default, retirees_user)
  expect_gt(runs$default$summary$total_headcount, 0L)
  expect_equal(runs$user$summary, runs$default$summary)
  expect_equal(nrow(runs$user$new_hires_dt), 0L)
})

test_that("simulate_hiring(): the caller's tables are left unchanged", {
  # Guard: the run works on copies, never on the caller's tables.
  d <- renamed_fixture()
  hc_before <- copy(d$hc_user)
  hp_before <- copy(d$hp_user)
  retirees <- retirees_of(d$hc_user, d$hp_user, d$ref_date, user_spec)
  retirees_before <- copy(retirees)
  simulate_hiring(d$hc_user, d$hp_user,
               policy_params = list(mode = "flow", group_cols = "paygrade", replacement_rate = 1,
                                    salary_scale = to_user_names(hiring_scale(d))),
               leavers_dt = retirees, as_of_date = d$ref_date, column_spec_args = user_spec)
  expect_identical(d$hc_user, hc_before)
  expect_identical(d$hp_user, hp_before)
  expect_identical(retirees, retirees_before)
})

test_that("simulate_hiring(): tables with no snapshot column are left unchanged by downsizing", {
  # Guard: with no snapshot column nothing is subset first, so only the entry
  # copies keep downsizing, which edits in place, away from the caller's tables.
  d <- exit_fixture()
  hc <- copy(d$hc_user)[, snap := NULL]
  hp <- copy(d$hp_user)[, snap := NULL]
  hc_before <- copy(hc)
  hp_before <- copy(hp)
  stock <- compute_current_stock(d$hc, d$hp, d$ref_date)$current_stock
  simulate_hiring(hc, hp,
               policy_params = list(mode = "stock", group_cols = NULL,
                                    stock_targets = data.table(target_stock = stock - 5L)),
               as_of_date = d$ref_date, column_spec_args = user_spec)
  expect_identical(hc, hc_before)
  expect_identical(hp, hp_before)
})

test_that("simulate_hiring(): a column the run needs must exist, and unused keys are not checked", {
  d <- renamed_fixture()
  policy <- list(mode = "stock", group_cols = "paygrade",
                 stock_targets = grade_stock(d)[, .(paygrade, target_stock = current_stock)],
                 salary_scale = to_user_names(hiring_scale(d)))
  run <- function(spec) simulate_hiring(d$hc_user, d$hp_user, policy_params = policy,
                                     as_of_date = d$ref_date, column_spec_args = spec)

  for (key in c("personnel_id_col", "contract_id_col", "start_date_col", "end_date_col",
                "salary_col", "contract_type_col", "status_col")) {
    bad <- modifyList(user_spec, setNames(list("not_a_column"), key))
    expect_error(run(bad), paste0(key, " = 'not_a_column'"), fixed = TRUE)
  }

  # Without flow-mode eligibility, hiring never reads birth dates or a stored age
  unused <- modifyList(user_spec, list(birth_date_col = "not_a_column", age_col = "also_not"))
  expect_no_error(run(unused))
})


# ---- simulate_allowances() ----------------------------------------------------

allowances_both_ways <- function(policy_default, policy_user, ledger_default = NULL,
                                 ledger_user = NULL, types = c("hazard", "transport")) {
  d <- exit_fixture()
  list(
    default = simulate_allowances(d$hc, d$hp, allowance_dt = ledger_default,
                                  policy_params = policy_default, as_of_date = d$ref_date,
                                  allowance_types = types),
    user    = simulate_allowances(d$hc_user, d$hp_user, allowance_dt = ledger_user,
                                  policy_params = policy_user, as_of_date = d$ref_date,
                                  allowance_types = types, column_spec_args = user_spec)
  )
}

test_that("simulate_allowances(): a scalar amount works on renamed columns", {
  policy <- list(group_cols = NULL, policy_table = NULL, defaults = list(amount = 100))
  runs <- allowances_both_ways(policy, policy)
  expect_gt(runs$default$summary$allowance_cost_total, 0)
  expect_same_results_in_user_names(runs, "allowance_dt")
})

test_that("simulate_allowances(): amounts and multipliers grouped on a renamed column", {
  policy <- function(type_col) list(
    group_cols   = c(type_col, "allowance_type"),
    policy_table = setnames(data.table(x = "permanent", allowance_type = "hazard", amount = 250),
                            "x", type_col),
    defaults     = list(amount = 10),
    multiplier_group_cols   = type_col,
    multiplier_policy_table = setnames(data.table(x = "fixed-term", total_allowance_multiplier = 2),
                                       "x", type_col)
  )
  runs <- allowances_both_ways(policy("contract_type"), policy("tipo"))
  expect_true(all(c(10, 20, 250) %in% runs$default$allowance_dt$amount))
  expect_same_results_in_user_names(runs, "allowance_dt")
})

test_that("simulate_allowances(): pensioner and inactive rows are left out under a renamed type column", {
  d <- exit_fixture()
  hc <- copy(d$hc_user)
  hc[1:10, tipo := "pensioner"]
  run <- simulate_allowances(hc, d$hp_user,
                             policy_params = list(group_cols = NULL, policy_table = NULL,
                                                  defaults = list(amount = 100)),
                             as_of_date = d$ref_date, allowance_types = "hazard",
                             column_spec_args = user_spec)
  expect_false(any(run$allowance_dt$cid %in% hc$cid[1:10]))
})

test_that("simulate_allowances(): an empty ledger uses the caller's ID columns", {
  # No policy: nothing is resolved, but the ledger still has the caller's names
  d <- exit_fixture()
  run <- simulate_allowances(d$hc_user, d$hp_user, policy_params = NULL, as_of_date = d$ref_date,
                             column_spec_args = user_spec)
  expect_identical(names(run$allowance_dt),
                   c("cid", "pid", "allowance_type", "amount", "period_date"))
  expect_equal(nrow(run$allowance_dt), 0L)
})

test_that("simulate_allowances(): the ledger carries forward under the caller's names", {
  policy <- list(group_cols = NULL, policy_table = NULL, defaults = list(amount = 100))
  first <- allowances_both_ways(policy, policy)
  runs  <- allowances_both_ways(policy, policy, first$default$allowance_dt, first$user$allowance_dt)
  expect_equal(nrow(runs$user$allowance_dt), 2L * nrow(first$user$allowance_dt))
  expect_same_results_in_user_names(runs, "allowance_dt")
})

test_that("simulate_allowances(): a column the run needs must exist, and unused keys are not checked", {
  d <- exit_fixture()
  policy <- list(group_cols = NULL, policy_table = NULL, defaults = list(amount = 100))
  run <- function(spec) simulate_allowances(d$hc_user, d$hp_user, policy_params = policy,
                                            as_of_date = d$ref_date, allowance_types = "hazard",
                                            column_spec_args = spec)
  for (key in c("personnel_id_col", "contract_id_col", "contract_type_col")) {
    bad <- modifyList(user_spec, setNames(list("not_a_column"), key))
    expect_error(run(bad), paste0(key, " = 'not_a_column'"), fixed = TRUE)
  }

  # Allowances never read the pay column or contract dates, so they do not ask for them
  unused <- modifyList(user_spec, list(salary_col = "not_a_column", start_date_col = "also_not"))
  expect_no_error(run(unused))
})


# ---- The govhr calls that need fixed names (ADR 0012, exception) ---------------

# Enough history for exits, hires and moves to occur, in both sets of names
history_fixture <- function() {
  start <- as.Date("2012-09-01")
  hc <- bra_hrmis_contract[ref_date <= start]
  hp <- bra_hrmis_personnel[ref_date <= start]
  list(hc = hc, hp = hp, hc_user = to_user_names(hc), hp_user = to_user_names(hp),
       ref_date = start)
}

test_that(".govhr_detect_movement(): events come back under the caller's names", {
  d <- history_fixture()
  default_events <- .govhr_detect_movement(d$hp, "personnel_id", "ref_date", "employment_status")
  user_events    <- .govhr_detect_movement(d$hp_user, "pid", "snap", "situacao")
  expect_identical(names(user_events), c("pid", "snap", "prev_date", "next_date", "hire", "separation"))
  expect_equal(to_default_names(user_events), default_events)
  expect_equal(default_events, govhr::detect_movement(d$hp))
})

test_that(".govhr_add_contract_to_event(): the join comes back under the caller's names", {
  d <- history_fixture()
  default_events <- .govhr_detect_movement(d$hp, "personnel_id", "ref_date", "employment_status")
  user_events    <- .govhr_detect_movement(d$hp_user, "pid", "snap", "situacao")
  default_joined <- .govhr_add_contract_to_event(default_events, d$hc, "est_id", "personnel_id", "ref_date")
  user_joined    <- .govhr_add_contract_to_event(user_events, d$hc_user, "est_id", "pid", "snap")
  expect_true(all(c("pid", "snap", "est_id") %in% names(user_joined)))
  expect_equal(to_default_names(user_joined), default_joined)
  expect_equal(default_joined,
               govhr::add_contract_to_event(default_events, d$hc, keep_vars = "est_id"))
})

test_that("the govhr wrappers leave the caller's tables and their order unchanged", {
  d <- history_fixture()
  hp <- copy(d$hp_user)[order(pid, decreasing = TRUE)]
  hc <- copy(d$hc_user)[order(cid, decreasing = TRUE)]
  hp_before <- copy(hp); hc_before <- copy(hc)
  events <- .govhr_detect_movement(hp, "pid", "snap", "situacao")
  events_before <- copy(events)
  .govhr_add_contract_to_event(events, hc, "est_id", "pid", "snap")
  expect_identical(hp, hp_before)
  expect_identical(hc, hc_before)
  expect_identical(events, events_before)
})

test_that(".panel_snapshot(): reads the date argument even when a column has the same name", {
  # The date column here is called `snap`. Inside a data.table filter a bare
  # `snap` would mean the column, the filter would keep every row, and the
  # estimate would stop on "duplicate rows".
  d <- history_fixture()
  snap <- as.Date("2011-09-01")
  out <- .panel_snapshot(d$hp_user, "snap", snap)
  expect_false("snap" %in% names(out))
  expect_equal(nrow(out), d$hp_user[d$hp_user$snap == as.Date("2011-09-01"), .N])
  expect_gt(nrow(out), 0L)
  expect_lt(nrow(out), nrow(d$hp_user))

  # The same when the column has the name of the function's own argument
  hp_date <- setnames(copy(d$hp_user), "snap", "date")
  expect_equal(nrow(.panel_snapshot(hp_date, "date", snap)), nrow(out))
})

test_that("the govhr wrappers ignore a caller's other columns that carry govhr's names", {
  # The caller's ID is `pid`; their own `personnel_id` and `ref_date` columns
  # mean something else, and must neither clash nor be read
  d <- history_fixture()
  hp <- copy(d$hp_user)[, `:=`(personnel_id = "unrelated", ref_date = as.Date("1900-01-01"))]
  hc <- copy(d$hc_user)[, `:=`(personnel_id = "unrelated", ref_date = as.Date("1900-01-01"))]
  # First in the table, where a lookup by name would find them first
  setcolorder(hp, c("personnel_id", "ref_date"))
  setcolorder(hc, c("personnel_id", "ref_date"))
  expect_equal(estimate_historical_exit_rates(hc, hp, group_cols = "est_id", column_spec_args = user_spec),
               estimate_historical_exit_rates(d$hc, d$hp, group_cols = "est_id"))
})


# ---- estimate_*() --------------------------------------------------------------

test_that("estimate_historical_exit_rates(): renamed columns give the default-name rates", {
  d <- history_fixture()
  ungrouped <- estimate_historical_exit_rates(d$hc, d$hp)
  expect_gt(ungrouped$exit_rate, 0)
  expect_equal(estimate_historical_exit_rates(d$hc_user, d$hp_user, column_spec_args = user_spec),
               ungrouped)
  expect_equal(estimate_historical_exit_rates(d$hc_user, d$hp_user, group_cols = "est_id",
                                              column_spec_args = user_spec),
               estimate_historical_exit_rates(d$hc, d$hp, group_cols = "est_id"))
})

test_that("estimate_historical_hiring_rates(): first-appearance rates on renamed columns", {
  d <- history_fixture()
  ungrouped <- estimate_historical_hiring_rates(d$hc, d$hp, group_cols = NULL)
  expect_gt(ungrouped$hiring_rate, 0)
  expect_equal(estimate_historical_hiring_rates(d$hc_user, d$hp_user, group_cols = NULL,
                                                column_spec_args = user_spec),
               ungrouped)
  expect_equal(estimate_historical_hiring_rates(d$hc_user, d$hp_user, group_cols = "est_id",
                                                column_spec_args = user_spec),
               estimate_historical_hiring_rates(d$hc, d$hp, group_cols = "est_id"))
})

test_that("estimate_historical_hiring_rates(): administrative hire dates on renamed columns", {
  d <- history_fixture()
  hp_user <- setnames(copy(d$hp_user), "first_employment_date", "admissao")
  spec_user    <- modifyList(user_spec, list(hire_date_col = "admissao"))
  spec_default <- define_column_spec(hire_date_col = "first_employment_date")
  for (groups in list(NULL, "est_id")) {
    default_rate <- estimate_historical_hiring_rates(d$hc, d$hp, group_cols = groups,
                                                     column_spec_args = spec_default)
    expect_gt(sum(default_rate$hiring_rate), 0)
    expect_equal(estimate_historical_hiring_rates(d$hc_user, hp_user, group_cols = groups,
                                                  column_spec_args = spec_user),
                 default_rate)
  }
})

test_that("estimate_historical_movement_rates(): renamed columns give the default-name rates", {
  d <- history_fixture()
  default_rates <- estimate_historical_movement_rates(d$hc, group_cols = "est_id")
  expect_gt(nrow(default_rates), 0L)
  expect_equal(estimate_historical_movement_rates(d$hc_user, group_cols = "est_id",
                                                  column_spec_args = user_spec),
               default_rates)
})

test_that("movement_policy_template(): renamed columns give the default-name template", {
  d <- history_fixture()
  default_template <- movement_policy_template(d$hc, group_cols = "est_id")
  expect_gt(nrow(default_template), 0L)
  expect_equal(movement_policy_template(d$hc_user, group_cols = "est_id", column_spec_args = user_spec),
               default_template)
})

test_that("estimate_retirement_hazard_rates(): renamed columns give the default-name rates", {
  d <- history_fixture()
  for (groups in list("age_yr_int", c("age_yr_int", "gender"))) {
    default_rates <- estimate_retirement_hazard_rates(d$hp, group_cols = groups)
    expect_gt(sum(default_rates$hazard_rate), 0)
    expect_equal(estimate_retirement_hazard_rates(d$hp_user, group_cols = groups,
                                                  column_spec_args = user_spec),
                 default_rates)
  }
  expect_equal(
    estimate_retirement_hazard_rates(d$hp_user, group_cols = "age_yr_int", smooth = TRUE,
                                     column_spec_args = user_spec),
    estimate_retirement_hazard_rates(d$hp, group_cols = "age_yr_int", smooth = TRUE)
  )
})

test_that("estimate_retirement_hazard_rates(): age_col = NULL means the standard age column", {
  d <- history_fixture()
  expect_equal(
    estimate_retirement_hazard_rates(d$hp, group_cols = "age_yr_int",
                                     column_spec_args = define_column_spec(age_col = NULL)),
    estimate_retirement_hazard_rates(d$hp, group_cols = "age_yr_int")
  )
})

# The allowance table's own columns are named separately from the spec
allowance_user <- function(d) {
  al <- to_user_names(bra_hrmis_allowance[ref_date == d$ref_date])
  setnames(al, c("allowance_type", "allowance_lcu"), c("tipo_aux", "valor"))
}

test_that("estimate_baseline_allowance_scale(): renamed columns give the default-name scale", {
  d <- history_fixture()
  default_scale <- estimate_baseline_allowance_scale(bra_hrmis_allowance, d$hc, group_cols = "est_id",
                                                     baseline_date = d$ref_date)
  expect_gt(sum(default_scale$amount), 0)
  expect_equal(
    estimate_baseline_allowance_scale(allowance_user(d), d$hc_user, group_cols = "est_id",
                                      baseline_date = d$ref_date, allowance_type_col = "tipo_aux",
                                      amount_col = "valor", column_spec_args = user_spec),
    default_scale
  )
})

test_that("design_allowance_policy_status_quo(): renamed columns give the default-name policy", {
  d <- history_fixture()
  expect_equal(
    design_allowance_policy_status_quo(allowance_user(d), d$hc_user, group_cols = "est_id",
                                       baseline_date = d$ref_date, allowance_type_col = "tipo_aux",
                                       amount_col = "valor", column_spec_args = user_spec),
    design_allowance_policy_status_quo(bra_hrmis_allowance, d$hc, group_cols = "est_id",
                                       baseline_date = d$ref_date)
  )
})

test_that("make_status_quo_policies(): rates are estimated, not defaulted, on renamed columns", {
  d <- history_fixture()
  scale <- d$hc[, .(gross_salary_lcu = mean(gross_salary_lcu, na.rm = TRUE)), by = est_id]
  # It is deprecated, so that warning is expected. A failed estimate would warn as
  # well and fall back to a flat rate, so no other warning is the check.
  ignore_deprecation <- function(code) {
    withCallingHandlers(code, deprecatedWarning = function(w) invokeRestart("muffleWarning"))
  }
  default_run <- ignore_deprecation(
    make_status_quo_policies(d$hc, d$hp, scale, movement_group_cols = "est_id",
                             exit_group_cols = "est_id", hiring_group_cols = "est_id")
  )
  expect_no_warning(
    user_run <- ignore_deprecation(
      make_status_quo_policies(d$hc_user, d$hp_user, scale, movement_group_cols = "est_id",
                               exit_group_cols = "est_id", hiring_group_cols = "est_id",
                               column_spec_args = user_spec)
    )
  )
  expect_gt(nrow(user_run$exit$policy_table), 0L)
  expect_equal(user_run$exit, default_run$exit)
  expect_equal(user_run$movement, default_run$movement)
  expect_equal(user_run$hiring, default_run$hiring)
})

test_that("estimate_*(): the caller's tables are left unchanged", {
  d <- history_fixture()
  hc <- copy(d$hc_user); hp <- copy(d$hp_user); al <- allowance_user(d)
  hc_before <- copy(hc); hp_before <- copy(hp); al_before <- copy(al)
  estimate_historical_exit_rates(hc, hp, group_cols = "est_id", column_spec_args = user_spec)
  estimate_historical_hiring_rates(hc, hp, group_cols = "est_id", column_spec_args = user_spec)
  estimate_historical_movement_rates(hc, group_cols = "est_id", column_spec_args = user_spec)
  movement_policy_template(hc, group_cols = "est_id", column_spec_args = user_spec)
  estimate_retirement_hazard_rates(hp, group_cols = "age_yr_int", column_spec_args = user_spec)
  estimate_baseline_allowance_scale(al, hc, group_cols = "est_id", baseline_date = d$ref_date,
                                    allowance_type_col = "tipo_aux", amount_col = "valor",
                                    column_spec_args = user_spec)
  expect_identical(hc, hc_before)
  expect_identical(hp, hp_before)
  expect_identical(al, al_before)
})

test_that("estimate_*(): a column the estimate needs must exist, and unused keys are not checked", {
  d <- history_fixture()
  needs <- list(
    function(spec) estimate_historical_exit_rates(d$hc_user, d$hp_user, column_spec_args = spec),
    function(spec) estimate_historical_hiring_rates(d$hc_user, d$hp_user, group_cols = NULL, column_spec_args = spec)
  )
  for (run in needs) {
    for (key in c("personnel_id_col", "ref_date_col", "start_date_col", "end_date_col",
                  "contract_type_col", "status_col")) {
      bad <- modifyList(user_spec, setNames(list("not_a_column"), key))
      expect_error(run(bad), paste0(key, " = 'not_a_column'"), fixed = TRUE)
    }
    # Rates never read pay, so they do not ask for it
    expect_no_error(run(modifyList(user_spec, list(salary_col = "not_a_column"))))
  }

  for (key in c("age_col", "personnel_id_col", "ref_date_col", "status_col")) {
    bad <- modifyList(user_spec, setNames(list("not_a_column"), key))
    expect_error(estimate_retirement_hazard_rates(d$hp_user, group_cols = "age_yr_int", column_spec_args = bad),
                 paste0(key, " = 'not_a_column'"), fixed = TRUE)
  }
  expect_no_error(estimate_retirement_hazard_rates(
    d$hp_user, group_cols = "age_yr_int",
    column_spec_args = modifyList(user_spec, list(salary_col = "not_a_column"))
  ))
})


# ---- simulate_scenario() and simulate_horizon() --------------------------------

# The pay table at the start, per establishment, under the default names
start_pay_table <- function(d) {
  d$hc[ref_date == d$ref_date, .(gross_salary_lcu = mean(gross_salary_lcu, na.rm = TRUE)), by = est_id]
}

# Column names held inside policies and the pay table follow the run's own names.
horizon_run <- function(d, spec = define_column_spec(), contract = d$hc,
                        personnel = d$hp, allowance = bra_hrmis_allowance, n_periods = 3L,
                        movement_rule = "keep", seed = 1L, hiring_pay_table = TRUE) {
  date_col <- spec$ref_date_col
  scale <- start_pay_table(d)
  if (spec$salary_col != "gross_salary_lcu") scale <- to_user_names(scale)
  allowance_args <- list(allowance, contract, group_cols = "est_id", baseline_date = d$ref_date,
                         column_spec_args = spec)
  if (date_col != "ref_date") {
    allowance_args$allowance_type_col <- "tipo_aux"
    allowance_args$amount_col <- "valor"
  }
  suppressMessages(simulate_horizon(
    contract_dt = contract, personnel_dt = personnel, salary_scale_dt = scale,
    n_periods = n_periods, baseline_date = d$ref_date,
    retirement_policy = design_retirement_policy_status_quo(
      pension = pension_db(0.02, 35, 0.80), mandatory_age = 75
    ),
    exit_policy     = design_exit_policy_status_quo(group_cols = "est_id"),
    movement_policy = design_movement_policy_status_quo(
      group_cols = "est_id", movement_type = "transfer", salary_update_rule = movement_rule
    ),
    hiring_policy   = design_hiring_policy_status_quo(
      group_cols = "est_id", salary_scale = if (hiring_pay_table) scale
    ),
    allowance_policy = do.call(design_allowance_policy_status_quo, allowance_args),
    salary_growth_rate = 0.05,
    column_spec_args   = spec,
    output_control_args = define_output_control(return_microdata = TRUE),
    seed = seed
  ))
}

expect_same_horizon <- function(user_run, default_run) {
  expect_equal(user_run$comparison, default_run$comparison)
  for (tbl in c("contract_dt", "personnel_dt", "pensioner_register", "allowance_dt")) {
    out <- user_run[[tbl]]
    expect_true("pid" %in% names(out), info = tbl)
    expect_length(setdiff(intersect(names(user_names), names(out)), package_made), 0L)
    expect_equal(to_default_names(out), default_run[[tbl]], info = tbl)
  }
}

test_that("simulate_horizon(): every module on renamed columns gives the default-name results", {
  d <- history_fixture()
  default_run <- horizon_run(d)
  user_run <- horizon_run(d, user_spec, d$hc_user, d$hp_user, allowance_user(d))
  # Every flow actually happened, so the comparison below means something
  cmp <- default_run$comparison
  expect_gt(sum(cmp$n_exits), 0L)
  expect_gt(sum(cmp$n_non_ret_exits), 0L)
  expect_gt(sum(cmp$n_transfers), 0L)
  expect_gt(sum(cmp$n_hires), 0L)
  expect_gt(sum(cmp$allowance_cost_end), 0)
  expect_same_horizon(user_run, default_run)
})

test_that("simulate_horizon(): the pay table hiring derives for itself works on renamed columns", {
  d <- history_fixture()
  default_run <- horizon_run(d, hiring_pay_table = FALSE)
  user_run <- horizon_run(d, user_spec, d$hc_user, d$hp_user, allowance_user(d),
                          hiring_pay_table = FALSE)
  expect_true(all(default_run$comparison$hiring_effect > 0))
  expect_same_horizon(user_run, default_run)
})

test_that("simulate_horizon(): the panel is cut to the starting snapshot", {
  # Returned microdata holds each person once per projected year, not their
  # history. All modules off, since each module also cuts the tables it reads.
  d <- history_fixture()
  scale <- to_user_names(start_pay_table(d))
  run <- simulate_horizon(d$hc_user, d$hp_user, scale, n_periods = 2L, baseline_date = d$ref_date,
                          retirement_policy = NULL, column_spec_args = user_spec,
                          output_control_args = define_output_control(return_microdata = TRUE))
  expect_equal(uniqueN(run$personnel_dt$snap), 2L)
  expect_equal(anyDuplicated(run$personnel_dt, by = c("pid", "snap")), 0L)
  expect_equal(anyDuplicated(run$contract_dt, by = c("cid", "snap")), 0L)
})

test_that("simulate_horizon(): movers paid from the pay table on renamed columns", {
  d <- history_fixture()
  expect_same_horizon(
    horizon_run(d, user_spec, d$hc_user, d$hp_user, allowance_user(d), movement_rule = "scale"),
    horizon_run(d, movement_rule = "scale")
  )
})

test_that("simulate_horizon(): age_col = NULL and tenure_col = NULL mean the standard columns", {
  # They are still aged each year, so the run matches one that names them
  d <- history_fixture()
  default_run <- horizon_run(d)
  null_run <- horizon_run(d, define_column_spec(age_col = NULL, tenure_col = NULL))
  expect_equal(null_run$comparison, default_run$comparison)
  expect_equal(null_run$personnel_dt, default_run$personnel_dt)
})

test_that("simulate_horizon(): age and tenure are calculated on renamed columns", {
  d <- history_fixture()
  default_run <- horizon_run(d, personnel = copy(d$hp)[, c("age", "personnel_tenure") := NULL])
  user_run <- horizon_run(d, user_spec, d$hc_user,
                          copy(d$hp_user)[, c("idade", "tempo") := NULL], allowance_user(d))
  expect_gt(sum(default_run$comparison$n_exits), 0L)
  expect_same_horizon(user_run, default_run)
})

test_that("simulate_horizon(): staff already pensioned at the start seed the register on renamed columns", {
  d <- history_fixture()
  already <- unique(d$hc[ref_date == d$ref_date, personnel_id])[1:5]
  hc <- copy(d$hc)[ref_date == d$ref_date & personnel_id %in% already, contract_type := "pensioner"]
  default_run <- horizon_run(d, contract = hc, n_periods = 1L)
  user_run <- horizon_run(d, user_spec, to_user_names(hc), d$hp_user, allowance_user(d),
                          n_periods = 1L)
  seeded <- user_run$pensioner_register[pid %in% already]
  expect_equal(nrow(seeded), 5L)
  expect_false(anyNA(seeded$age_at_retirement))
  expect_same_horizon(user_run, default_run)
})

test_that("simulate_horizon(): hiring rates can use a renamed hire-date column", {
  d <- history_fixture()
  hp_user <- setnames(copy(d$hp_user), "first_employment_date", "admissao")
  default_run <- horizon_run(d, define_column_spec(hire_date_col = "first_employment_date"))
  user_run <- horizon_run(d, modifyList(user_spec, list(hire_date_col = "admissao")),
                          d$hc_user, hp_user, allowance_user(d))
  # Different from the first-appearance method, so the column was really read
  expect_false(isTRUE(all.equal(default_run$comparison$n_hires,
                                horizon_run(d)$comparison$n_hires)))
  expect_equal(user_run$comparison, default_run$comparison)
})

test_that("simulate_horizon(): a snapshot without a date column is stamped in the caller's", {
  # One snapshot, no date column: microdata still comes back dated, under the caller's name
  d <- history_fixture()
  hc <- d$hc_user[snap == d$ref_date][, snap := NULL]
  hp <- d$hp_user[snap == d$ref_date][, snap := NULL]
  scale <- to_user_names(start_pay_table(d))
  run <- simulate_horizon(hc, hp, scale, n_periods = 2L, baseline_date = d$ref_date,
                          retirement_policy = NULL, column_spec_args = user_spec,
                          output_control_args = define_output_control(return_microdata = TRUE))
  expect_true("snap" %in% names(run$contract_dt))
  expect_true("snap" %in% names(run$personnel_dt))
  expect_false("ref_date" %in% names(run$contract_dt))
  expect_equal(uniqueN(run$contract_dt$snap), 2L)
})

test_that("simulate_scenario(): one period on renamed columns gives the default-name results", {
  d <- renamed_fixture()
  run <- function(hc, hp, spec, register = NULL) {
    scale <- start_pay_table(d)
    if (spec$salary_col != "gross_salary_lcu") scale <- to_user_names(scale)
    set.seed(1L)
    simulate_scenario(
      hc, hp, scale, period_date = d$ref_date, pensioner_register = register,
      retirement_policy = design_retirement_policy_age(
        min_age = 60, pension = pension_db(0.02, 35, 0.80)
      ),
      exit_policy = design_exit_policy_flat(exit_rate = 0.02),
      salary_growth_rate = 0.05, pension_cola_rate = 0.03, column_spec_args = spec
    )
  }
  default_run <- run(d$hc, d$hp, define_column_spec())
  user_run <- run(d$hc_user, d$hp_user, user_spec)
  expect_gt(default_run$summary$n_exits, 0L)
  expect_equal(user_run$summary, default_run$summary)
  for (tbl in c("contract_dt", "personnel_dt", "pensioner_register")) {
    expect_equal(to_default_names(user_run[[tbl]]), default_run[[tbl]], info = tbl)
  }
  expect_true("pid" %in% names(user_run$pensioner_register))

  # The register carries forward into a second period under the caller's name
  second <- run(user_run$contract_dt, user_run$personnel_dt, user_spec,
                user_run$pensioner_register)
  expect_true("pid" %in% names(second$pensioner_register))
  expect_false("personnel_id" %in% names(second$pensioner_register))
  expect_gte(nrow(second$pensioner_register), nrow(user_run$pensioner_register))
})

test_that("simulate_scenario(): an empty register and a run with no modules use the caller's names", {
  d <- renamed_fixture()
  scale <- to_user_names(start_pay_table(d))
  run <- simulate_scenario(d$hc_user[snap == d$ref_date], d$hp_user[snap == d$ref_date], scale,
                           period_date = d$ref_date, column_spec_args = user_spec)
  expect_true("pid" %in% names(run$pensioner_register))
  expect_equal(nrow(run$pensioner_register), 0L)
  expect_gt(run$summary$n_headcount_start, 0L)
})

test_that("simulate_scenario(): a column the run needs must exist, and unused keys are not checked", {
  d <- renamed_fixture()
  scale <- to_user_names(start_pay_table(d))
  run <- function(spec) simulate_scenario(d$hc_user[snap == d$ref_date], d$hp_user[snap == d$ref_date],
                                          scale, period_date = d$ref_date, column_spec_args = spec)
  for (key in c("personnel_id_col", "contract_type_col", "salary_col")) {
    bad <- modifyList(user_spec, setNames(list("not_a_column"), key))
    expect_error(run(bad), paste0(key, " = 'not_a_column'"), fixed = TRUE)
  }
  # With no modules on, contract dates and birth dates are not read
  expect_no_error(run(modifyList(user_spec, list(start_date_col = "not_a_column",
                                                 birth_date_col = "also_not"))))
})
