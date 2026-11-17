# Who retires: select_retirees() and its three kinds of policy (status quo,
# eligibility rule, mandatory rule on top of the status quo).

library(data.table)

start <- as.Date("2020-01-01")

# Six people, one contract each. P3 has a blank status and a blank contract type
# (both count as staff); P6 already draws a pension but still holds a contract.
retiree_data <- function() {
  list(
    c = data.table(
      contract_id   = paste0("C", 1:6),
      personnel_id  = paste0("P", 1:6),
      occupation    = c("teacher", "teacher", "clerk", "teacher", "clerk", "clerk"),
      start_date    = as.Date("2000-01-01"),
      end_date      = as.Date(NA),
      contract_type = c("permanent", "fixed-term", NA, "permanent", "permanent", "permanent"),
      gross_salary_lcu = 100
    ),
    p = data.table(
      personnel_id      = paste0("P", 1:6),
      employment_status = c("active", "active", NA, "active", "active", "pensioner"),
      age               = c(76, 72, 61, 59, 40, 80),
      personnel_tenure  = c(30, 25, 20, 15, 5, 35)
    )
  )
}

retire <- function(policy, d = retiree_data()) {
  select_retirees(d$c, d$p, policy, start)
}

# Who retired, and why, in ID order
who <- function(out) out[order(personnel_id), .(personnel_id, retire_reason)]

flat <- pension_flat(1)   # select_retirees() ignores the pension; the policy needs one

test_that("an age rule retires everyone at or over the age, blank status included", {
  out <- retire(design_retirement_policy_eligibility(retirement_rule_age(60), flat))
  expect_equal(who(out), data.table(personnel_id = c("P1", "P2", "P3"), retire_reason = "rule"))
})

test_that("the output is the shape simulate_retirement() passes on", {
  out <- retire(design_retirement_policy_eligibility(retirement_rule_age(60), flat))
  expect_named(out, c("personnel_id", "retire", "age", "tenure_years", "retire_reason"))
  expect_equal(out$retire, rep(1L, 3))
  expect_equal(out[order(personnel_id), tenure_years], c(30, 25, 20))
})

test_that("a rule can set a different age by group, from a contract column", {
  rule <- retirement_rule_age(75, group_cols = "occupation",
                              policy_table = data.table(occupation = "teacher", min_age = 70))
  out <- retire(design_retirement_policy_eligibility(rule, flat))
  expect_equal(who(out)$personnel_id, c("P1", "P2"))
})

test_that("status quo retires at each age's rate; ages with no rate retire nobody", {
  rates <- retirement_rates_status_quo(hazard_table = data.table(age_yr_int = c(72L, 76L), hazard_rate = 1))
  out <- retire(design_retirement_policy_status_quo(flat, rates))
  expect_equal(who(out), data.table(personnel_id = c("P1", "P2"), retire_reason = "status_quo"))
})

test_that("ages past the oldest rate take that rate", {
  rates <- retirement_rates_status_quo(hazard_table = data.table(age_yr_int = 72L, hazard_rate = 1))
  out <- retire(design_retirement_policy_status_quo(flat, rates))
  expect_equal(who(out)$personnel_id, c("P1", "P2"))
})

test_that("mandatory retires by the rule first, then by rate among the rest", {
  rates <- retirement_rates_status_quo(hazard_table = data.table(age_yr_int = c(61L, 76L), hazard_rate = 1))
  out <- retire(design_retirement_policy_mandatory(retirement_rule_age(75), flat, rates))
  # P1 is 76: retired by the rule, and not drawn again at the age-76 rate
  expect_equal(who(out), data.table(personnel_id = c("P1", "P3"), retire_reason = c("rule", "status_quo")))
})

test_that("active_types limits the pool; a blank contract type counts as staff", {
  out <- retire(design_retirement_policy_eligibility(retirement_rule_age(60), flat, active_types = "permanent"))
  expect_equal(who(out)$personnel_id, c("P1", "P3"))
})

test_that("nobody in the pool gives an empty table of the same shape", {
  d <- retiree_data()
  d$p[, employment_status := "pensioner"]
  out <- retire(design_retirement_policy_eligibility(retirement_rule_age(60), flat), d)
  expect_equal(nrow(out), 0L)
  expect_named(out, c("personnel_id", "retire", "age", "tenure_years", "retire_reason"))
})
