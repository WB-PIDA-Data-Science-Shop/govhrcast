# Custom formulas and rules never run code outside the allow-list
#
# Users may write a custom pension formula (pension_type = "custom") or a
# custom eligibility rule (eligibility_type = "custom") as an R expression in
# a string. Whatever path the string arrives by -- a helper, a hand-written
# policy list, or a policy_table column -- it must be checked against the
# allow-list before anything evaluates it. Each harmful string below tries to
# set an environment variable; the tests check it is rejected AND never ran.

library(data.table)

probe_var <- "GOVHRCAST_SANDBOX_PROBE"
harmful   <- sprintf('Sys.setenv(%s = "ran")', probe_var)

sandbox_fixture <- function() {
  list(
    contract_dt = data.table(
      contract_id = c("C1", "C2"), personnel_id = c("P1", "P2"),
      est_id = c("E1", "E2"), start_date = as.Date("2000-01-01"),
      end_date = as.Date(NA), contract_type = "permanent",
      gross_salary_lcu = c(1000, 2000)
    ),
    personnel_dt = data.table(
      personnel_id = c("P1", "P2"),
      birth_date = as.Date(c("1955-01-01", "1990-01-01")),
      employment_status = "active"
    ),
    ref_date = as.Date("2025-01-01")
  )
}

# A hand-written retirement policy: P1 (age 70) retires under age_only 60.
custom_pension_policy <- function(formula, policy_table = NULL, group_cols = NULL) {
  list(group_cols = group_cols, policy_table = policy_table,
       defaults = list(eligibility_type = "age_only", min_age = 60,
                       pension_type = "custom", pension_formula = formula))
}

test_that("a hand-written policy with a disallowed pension_formula is rejected before it runs", {
  Sys.unsetenv(probe_var); on.exit(Sys.unsetenv(probe_var), add = TRUE)
  d <- sandbox_fixture()
  expect_error(
    simulate_retirement(d$contract_dt, d$personnel_dt, ref_date = d$ref_date,
                        policy_params = custom_pension_policy(harmful)),
    "pension_formula.*Sys.setenv"
  )
  expect_identical(Sys.getenv(probe_var), "")
})

test_that("a disallowed pension_formula in a policy_table column is rejected before it runs", {
  Sys.unsetenv(probe_var); on.exit(Sys.unsetenv(probe_var), add = TRUE)
  d <- sandbox_fixture()
  pt <- data.table(est_id = c("E1", "E2"),
                   pension_formula = c(harmful, "gross_salary_lcu * 0.5"))
  expect_error(
    simulate_retirement(d$contract_dt, d$personnel_dt, ref_date = d$ref_date,
                        policy_params = custom_pension_policy("gross_salary_lcu * 0.5",
                                                              policy_table = pt,
                                                              group_cols = "est_id")),
    "pension_formula.*Sys.setenv"
  )
  expect_identical(Sys.getenv(probe_var), "")
})

test_that("every supplied pension_formula is checked, even for a group with no retirees", {
  d <- sandbox_fixture()
  # Only P1 (E1) retires; E2's formula would never be evaluated this period,
  # but a policy carrying it must still be rejected up front.
  pt <- data.table(est_id = c("E1", "E2"),
                   pension_formula = c("gross_salary_lcu * 0.5", harmful))
  expect_error(
    simulate_retirement(d$contract_dt, d$personnel_dt, ref_date = d$ref_date,
                        policy_params = custom_pension_policy("gross_salary_lcu * 0.5",
                                                              policy_table = pt,
                                                              group_cols = "est_id")),
    "pension_formula.*Sys.setenv"
  )
})

test_that("a disallowed eligibility_rule reached through flow hiring is rejected before it runs", {
  Sys.unsetenv(probe_var); on.exit(Sys.unsetenv(probe_var), add = TRUE)
  d <- sandbox_fixture()
  # With no retirees passed in, flow hiring re-derives leavers from the
  # eligibility rule on the hiring policy -- a path that bypasses
  # check_retirement_inputs().
  hiring <- list(mode = "flow", replacement_rate = 1, group_cols = NULL,
                 defaults = list(eligibility_type = "custom",
                                 eligibility_rule = paste("age >= 60 |", harmful)))
  expect_error(
    simulate_hiring(d$contract_dt, d$personnel_dt, policy_params = hiring,
                    ref_date = d$ref_date),
    "eligibility_rule.*Sys.setenv"
  )
  expect_identical(Sys.getenv(probe_var), "")
})

test_that("pension_custom() names pension_formula, not eligibility_rule, when it rejects", {
  expect_error(pension_custom(harmful), "^pension_formula.*Sys.setenv")
})

test_that("namespaced and anonymous calls are rejected with a clear message", {
  expect_error(eligibility_custom("base::system('ls')"),
               "eligibility_rule.*only plain function names")
  expect_error(eligibility_custom("(function() TRUE)()"),
               "eligibility_rule.*only plain function names")
  expect_error(pension_custom("base::abs(gross_salary_lcu)"),
               "pension_formula.*only plain function names")
})

test_that("an allowed custom pension_formula still computes the pension", {
  d <- sandbox_fixture()
  res <- simulate_retirement(d$contract_dt, d$personnel_dt, ref_date = d$ref_date,
                             policy_params = custom_pension_policy("gross_salary_lcu * 0.5"))
  # P1 (born 1955, age 70) retires; half of a 1,000 salary
  expect_identical(res$retirees_dt$personnel_id, "P1")
  expect_equal(res$retirees_dt$pension, 500)
})
