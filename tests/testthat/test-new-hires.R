# The state a new hire starts with
#
# The rules, from decision D5 and the user's decisions:
#   * Entry age: the median entry age of recent hires in their group.
#   * Tenure 0, and they age and accrue tenure like everyone else.
#   * Contract type: the most common among recent hires in their group.
#   * Gender: drawn at random from the gender shares among recent hires in their
#     group, so a group is not all one gender. The same seed gives the same draw.
#   * Their group columns and pay, as before.
#   * A group with no recent hires uses the shares and medians across all groups.

library(data.table)

dates <- as.Date(c("2018-01-01", "2019-01-01", "2020-01-01"))
start <- dates[3]

# Everyone in E1 and E2 at the start; hires join at the second and third snapshots.
# E1's hires: 25, 30, 35, all "fixed-term" bar one, 2 women and 1 man.
# E2's hires: 40 and 50, "permanent", all men.
# E3 has staff but no hires.
history <- function() {
  staff <- data.table(
    personnel_id = paste0("S", 1:6),
    est_id = c("E1", "E1", "E2", "E2", "E3", "E3"),
    birth_date = as.Date("1970-01-01"),
    gender = c("F", "M", "F", "M", "F", "M"),
    contract_type = "permanent",
    joined = 1L
  )
  hires <- data.table(
    personnel_id = paste0("H", 1:5),
    est_id = c("E1", "E1", "E1", "E2", "E2"),
    birth_date = c(dates[2] - round(365.25 * 25), dates[2] - round(365.25 * 30), dates[3] - round(365.25 * 35),
                   dates[2] - round(365.25 * 40), dates[3] - round(365.25 * 50)),
    gender = c("F", "F", "M", "M", "M"),
    contract_type = c("fixed-term", "fixed-term", "permanent", "permanent", "permanent"),
    joined = c(2L, 2L, 3L, 2L, 3L)
  )
  everyone <- rbind(staff, hires)
  on_snapshot <- rbindlist(lapply(seq_along(dates), function(i) {
    everyone[joined <= i][, ref_date := dates[i]]
  }))
  list(
    c = on_snapshot[, .(contract_id = paste0("C", personnel_id), personnel_id, est_id, contract_type,
                        start_date = as.Date("2010-01-01"), end_date = as.Date(NA),
                        gross_salary_lcu = 100, ref_date)],
    p = on_snapshot[, .(personnel_id, birth_date, gender, employment_status = "active", ref_date)],
    s = data.table(est_id = c("E1", "E2", "E3"), gross_salary_lcu = c(100, 200, 300))
  )
}

hire_into <- function(est, n, d = history()) {
  targets <- d$c[ref_date == start, .(target_stock = uniqueN(personnel_id)), by = est_id]
  targets[est_id == est, target_stock := target_stock + n]
  design_hiring_policy_stock(targets, group_cols = "est_id")
}

hire_run <- function(est, n, n_periods = 1L, d = history(), ...) {
  simulate_horizon(d$c, d$p, d$s, n_periods = n_periods, ref_date = start, retirement_policy = NULL,
                   hiring_policy = hire_into(est, n, d), seed = 1L,
                   output_control_args = list(return_microdata = TRUE), ...)
}

# -- The profile -------------------------------------------------------------

test_that("the profile gives median entry age, most common contract type and gender shares", {
  d <- history()
  profile <- estimate_new_hire_profile(d$c, d$p, group_cols = "est_id")
  e1 <- profile[est_id == "E1"]
  expect_equal(e1[gender == "F", share], 2 / 3)
  expect_equal(e1[gender == "M", share], 1 / 3)
  expect_equal(unique(e1$entry_age), 30, tolerance = 0.01)
  expect_equal(unique(e1$contract_type), "fixed-term")
  e2 <- profile[est_id == "E2"]
  expect_equal(e2$gender, "M")
  expect_equal(e2$share, 1)
  expect_equal(e2$entry_age, 45, tolerance = 0.01)
  expect_equal(e2$contract_type, "permanent")
  expect_false("E3" %in% profile$est_id)
})

test_that("with no groups the profile covers all hires", {
  d <- history()
  profile <- estimate_new_hire_profile(d$c, d$p)
  expect_equal(profile[gender == "F", share], 2 / 5)
  expect_equal(unique(profile$entry_age), 35, tolerance = 0.01)
  expect_equal(unique(profile$contract_type), "permanent")
})

test_that("the profile reads a filled-in age column when there is one", {
  d <- history()
  d$p[, age := 20][, birth_date := as.Date(NA)]
  profile <- estimate_new_hire_profile(d$c, d$p)
  expect_equal(unique(profile$entry_age), 20)
})

test_that("with no gender column the profile has one row per group, and hires have no gender", {
  d <- history()
  d$p[, gender := NULL]
  profile <- estimate_new_hire_profile(d$c, d$p, group_cols = "est_id")
  expect_equal(nrow(profile), 2L)
  expect_equal(unique(profile$share), 1)
  run <- hire_run("E1", 2L, d = d)
  expect_false("gender" %in% names(run$personnel_dt))
})

test_that("a blank gender is left out of the shares", {
  d <- history()
  d$p[personnel_id == "H3", gender := NA_character_]
  profile <- estimate_new_hire_profile(d$c, d$p, group_cols = "est_id")
  expect_equal(profile[est_id == "E1", gender], "F")
  expect_equal(profile[est_id == "E1", share], 1)
})

# -- What a hire gets --------------------------------------------------------

test_that("a hire starts with the group's entry age and contract type, and tenure 0", {
  run <- hire_run("E1", 3L)
  people <- run$personnel_dt[grepl("^P_", personnel_id)]
  expect_equal(nrow(people), 3L)
  # One period: they aged one year and served one year by its end
  expect_equal(unique(people$age), 31, tolerance = 0.01)
  expect_equal(unique(people$personnel_tenure), 1)
  contracts <- run$contract_dt[personnel_id %in% people$personnel_id]
  expect_equal(unique(contracts$contract_type), "fixed-term")
  expect_equal(unique(contracts$est_id), "E1")
  expect_equal(unique(contracts$gross_salary_lcu), 100)
})

test_that("standalone: age, tenure and birth date are set at entry", {
  d <- history()
  snap_c <- d$c[ref_date == start]
  snap_p <- d$p[ref_date == start]
  snap_p[, `:=`(age = 40, personnel_tenure = 10)]
  policy <- hire_into("E2", 2L, d)
  policy$new_hire_profile <- estimate_new_hire_profile(d$c, d$p, group_cols = "est_id")
  policy$salary_scale <- d$s
  out <- simulate_hiring(snap_c, snap_p, policy, ref_date = start)
  expect_equal(unique(out$new_hires_dt$age), 45, tolerance = 0.01)
  expect_equal(unique(out$new_hires_dt$personnel_tenure), 0)
  expect_equal(unique(out$new_hires_dt$birth_date), start - round(365.25 * 45), tolerance = 5)
  expect_equal(unique(out$new_hires_dt$gender), "M")
})

test_that("a hire date column, when named, is set to the day they start", {
  d <- history()
  d$p[, first_employment_date := as.Date("2005-01-01")]
  d$p[grepl("^H", personnel_id), first_employment_date := dates[2] - 30]
  run <- hire_run("E1", 1L, d = d, column_spec_args = list(hire_date_col = "first_employment_date"))
  expect_equal(run$personnel_dt[grepl("^P_", personnel_id), first_employment_date], start)
})

test_that("genders are drawn from the group's shares, not all the most common", {
  set.seed(1)
  d <- history()
  run <- hire_run("E1", 300L, d = d)
  people <- run$personnel_dt[grepl("^P_", personnel_id)]
  expect_gt(mean(people$gender == "F"), 0.55)
  expect_lt(mean(people$gender == "F"), 0.78)
  expect_setequal(people$gender, c("F", "M"))
})

test_that("the same seed gives the same genders", {
  expect_equal(hire_run("E1", 20L)$personnel_dt, hire_run("E1", 20L)$personnel_dt)
})

test_that("a group with no recent hires uses the profile across all groups", {
  run <- hire_run("E3", 3L)
  people <- run$personnel_dt[grepl("^P_", personnel_id)]
  expect_equal(unique(people$age), 36, tolerance = 0.01)
  contracts <- run$contract_dt[personnel_id %in% people$personnel_id]
  expect_equal(unique(contracts$contract_type), "permanent")
})

test_that("flow and status-quo hires get the same state", {
  d <- history()
  run <- simulate_horizon(
    d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL, seed = 1L,
    exit_policy = list(group_cols = "est_id", policy_table = data.table(est_id = "E1", exit_rate = 1),
                       defaults = list(exit_strategy = "random")),
    hiring_policy = design_hiring_policy_flow(1, group_cols = "est_id"),
    output_control_args = list(return_microdata = TRUE)
  )
  people <- run$personnel_dt[grepl("^P_", personnel_id)]
  expect_gt(nrow(people), 0L)
  expect_false(anyNA(people$age))
  expect_false(anyNA(people$gender))
  run <- simulate_horizon(
    d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL, seed = 1L,
    hiring_policy = design_hiring_policy_status_quo(group_cols = "est_id"),
    output_control_args = list(return_microdata = TRUE)
  )
  people <- run$personnel_dt[grepl("^P_", personnel_id)]
  expect_gt(nrow(people), 0L)
  expect_false(anyNA(people$age))
})

test_that("hires age, and can retire, in later years", {
  d <- history()
  # Everyone is hired at 59 or so, so a hire in year 1 is 60 by year 2
  d$p[grepl("^H", personnel_id), birth_date := start - round(365.25 * 59.5)]
  targets <- d$c[ref_date == start, .(target_stock = uniqueN(personnel_id) + 2L), by = est_id]
  run <- simulate_horizon(
    d$c, d$p, d$s, n_periods = 3L, ref_date = start, seed = 1L,
    retirement_policy = design_retirement_policy_age(60, pension_rate(0.5)),
    hiring_policy = design_hiring_policy_stock(targets, group_cols = "est_id"),
    output_control_args = list(return_microdata = TRUE)
  )
  expect_true(any(grepl("^P_", run$pensioner_register$personnel_id)))
})

test_that("your own profile takes the place of the estimate", {
  d <- history()
  policy <- hire_into("E1", 2L, d)
  policy$new_hire_profile <- data.table(est_id = "E1", gender = "X", share = 1, entry_age = 22,
                                        contract_type = "short-term")
  run <- simulate_horizon(d$c, d$p, d$s, n_periods = 1L, ref_date = start, retirement_policy = NULL,
                          hiring_policy = policy, seed = 1L,
                          output_control_args = list(return_microdata = TRUE))
  people <- run$personnel_dt[grepl("^P_", personnel_id)]
  expect_equal(unique(people$gender), "X")
  expect_equal(unique(people$age), 23)
  expect_equal(unique(run$contract_dt[personnel_id %in% people$personnel_id, contract_type]), "short-term")
})

test_that("with no history of hires, hires have no age, and the user is told", {
  d <- history()
  one <- list(c = d$c[ref_date == start], p = d$p[ref_date == start], s = d$s)
  expect_message(
    run <- hire_run("E1", 2L, d = one),
    "no recent hires"
  )
  people <- run$personnel_dt[grepl("^P_", personnel_id)]
  expect_true(all(is.na(people$age)))
  expect_equal(unique(people$personnel_tenure), 1)
  expect_equal(unique(run$contract_dt[personnel_id %in% people$personnel_id, contract_type]), "permanent")
})

test_that("renamed columns give the same hires", {
  d <- history()
  default_run <- hire_run("E1", 3L, d = d)
  u <- copy(d)
  setnames(u$p, c("personnel_id", "gender", "birth_date", "employment_status", "ref_date"),
           c("pid", "sexo", "nasc", "situacao", "snap"))
  setnames(u$c, c("personnel_id", "contract_id", "contract_type", "start_date", "end_date",
                  "gross_salary_lcu", "ref_date"),
           c("pid", "cid", "tipo", "inicio", "fim", "bruto", "snap"))
  setnames(u$s, "gross_salary_lcu", "bruto")
  spec <- list(personnel_id_col = "pid", contract_id_col = "cid", contract_type_col = "tipo",
               start_date_col = "inicio", end_date_col = "fim", salary_col = "bruto",
               ref_date_col = "snap", status_col = "situacao", birth_date_col = "nasc",
               gender_col = "sexo", age_col = "idade", tenure_col = "tempo")
  profile <- estimate_new_hire_profile(u$c, u$p, group_cols = "est_id", column_spec_args = spec)
  expect_true(all(c("sexo", "tipo") %in% names(profile)))
  targets <- u$c[snap == start, .(target_stock = uniqueN(pid)), by = est_id]
  targets[est_id == "E1", target_stock := target_stock + 3L]
  user_run <- simulate_horizon(u$c, u$p, u$s, n_periods = 1L, ref_date = start, retirement_policy = NULL,
                               hiring_policy = design_hiring_policy_stock(targets, group_cols = "est_id"),
                               seed = 1L, column_spec_args = spec,
                               output_control_args = list(return_microdata = TRUE))
  user <- user_run$personnel_dt[grepl("^P_", pid)]
  default <- default_run$personnel_dt[grepl("^P_", personnel_id)]
  expect_equal(user$sexo, default$gender)
  expect_equal(user$idade, default$age)
  expect_equal(user$tempo, default$personnel_tenure)
})
