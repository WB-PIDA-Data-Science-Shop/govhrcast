# Unit tests for retirement_core.R functions
# Testing who a retirement rule or rate retires, and the retiree data after.

library(testthat)
library(data.table)

# Access internal helpers for direct unit tests.
eval_per_row_expression <- getFromNamespace(
  ".eval_per_row_expression",
  "govhrcast"
)

# =============================================================================
# Test data setup helpers
# =============================================================================

create_test_contract_dt <- function() {
  data.table(
    ref_date = as.Date("2025-01-01"),
    contract_id = c("C001", "C002", "C003", "C004"),
    personnel_id = c("P001", "P002", "P003", "P004"),
    start_date = as.Date(c(
      "2000-01-01",
      "2010-01-01",
      "2015-01-01",
      "2005-01-01"
    )),
    end_date = as.Date(c(NA, NA, NA, NA)),
    contract_type = c("perm", "perm", "perm", "perm"),
    gross_salary_lcu = c(5000, 6000, 4000, 5500)
  )
}

create_test_personnel_dt <- function() {
  data.table(
    personnel_id = c("P001", "P002", "P003", "P004"),
    birth_date = as.Date(c(
      "1960-01-01",
      "1975-01-01",
      "1980-01-01",
      "1965-01-01"
    )),
    employment_status = c("active", "active", "active", "active")
  )
}

# Age and tenure at ref, as simulate_horizon() fills them in: age from birth
# date, tenure from contract history. Retirement reads them.
add_age_tenure <- function(contract_dt, personnel_dt, ref = as.Date("2025-01-01")) {
  personnel_dt <- copy(personnel_dt)
  if (!"employment_status" %in% names(personnel_dt)) personnel_dt[, employment_status := "active"]
  personnel_dt[, age := as.numeric(ref - birth_date) / 365.25]
  personnel_dt[, personnel_tenure := NA_real_]
  personnel_dt[compute_tenure(contract_dt, ref), personnel_tenure := i.tenure_years, on = "personnel_id"]
  personnel_dt
}

# Who a rule retires at ref, in ID order
retired_by <- function(rule, contract_dt, personnel_dt, ref = as.Date("2025-01-01")) {
  policy <- design_retirement_policy_eligibility(rule, pension_flat(1))
  out <- select_retirees(contract_dt, add_age_tenure(contract_dt, personnel_dt, ref), policy, ref)
  sort(out$personnel_id)
}

# =============================================================================
# Internal helper coverage
# =============================================================================

test_that(".eval_per_row_expression evaluates expressions for masked rows", {
  dt <- data.table(
    age = c(61, 55, 70),
    tenure_years = c(12, 20, 8),
    eligibility_rule = c("age >= 60", "tenure_years >= 15", "age >= 65")
  )

  eval_per_row_expression(
    dt = dt,
    mask = c(TRUE, TRUE, FALSE),
    expr_col = "eligibility_rule",
    result_col = "custom_eligible"
  )

  expect_identical(dt$custom_eligible[[1]], TRUE)
  expect_identical(dt$custom_eligible[[2]], TRUE)
  expect_true(is.na(dt$custom_eligible[[3]]))
})

# =============================================================================
# An age rule
# =============================================================================

test_that("an age rule retires everyone at or over the age", {
  # P001 (age 65) and P004 (age 60) retire; P002 (50) and P003 (45) do not
  out <- select_retirees(create_test_contract_dt(),
                         add_age_tenure(create_test_contract_dt(), create_test_personnel_dt()),
                         design_retirement_policy_eligibility(retirement_rule_age(60), pension_flat(1)),
                         as.Date("2025-01-01"))
  expect_s3_class(out, "data.table")
  expect_named(out, c("personnel_id", "retire", "age", "tenure_years", "retire_reason"))
  expect_equal(sort(out$personnel_id), c("P001", "P004"))
})

test_that("an age rule respects the exact min_age boundary", {
  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    birth_date = as.Date(c("1965-01-01", "1965-01-02")), # Born Jan 1 vs Jan 2
    employment_status = c("active", "active")
  )
  # At 2025-01-01, P001 is exactly 60.00 and P002 is 59.997
  expect_equal(retired_by(retirement_rule_age(60), create_test_contract_dt()[1:2], personnel_dt), "P001")
})

# =============================================================================
# A tenure rule
# =============================================================================

test_that("a tenure rule retires everyone with the years of service", {
  # P001 (25 years), P002 (15.0 years, at the boundary) and P004 (20 years) retire; P003 (10) does not
  expect_equal(retired_by(retirement_rule_tenure(15), create_test_contract_dt(), create_test_personnel_dt()),
               c("P001", "P002", "P004"))
})

test_that("a tenure rule respects the exact min_tenure boundary", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P002"),
    start_date = as.Date(c("2010-01-01", "2010-01-02")),
    end_date = as.Date(c(NA, NA)),
    contract_type = c("perm", "perm")
  )
  personnel_dt <- data.table(personnel_id = c("P001", "P002"),
                             birth_date = as.Date("1970-01-01"))
  # At 2025-01-01, P001 has exactly 15.000 years, P002 slightly less
  expect_equal(retired_by(retirement_rule_tenure(15), contract_dt, personnel_dt), "P001")
})

# =============================================================================
# An age and tenure rule
# =============================================================================

test_that("an age and tenure rule retires only those who meet both", {
  # Only P001 (age 65, tenure 25) and P004 (age 60, tenure 20) meet both
  expect_equal(retired_by(retirement_rule_age_tenure(60, 15), create_test_contract_dt(),
                          create_test_personnel_dt()),
               c("P001", "P004"))
})

test_that("an age and tenure rule is AND, not OR", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002", "C003"),
    personnel_id = c("P001", "P002", "P003"),
    start_date = as.Date(c("2000-01-01", "2015-01-01", "2000-01-01")),
    end_date = as.Date(c(NA, NA, NA)),
    contract_type = c("perm", "perm", "perm")
  )
  personnel_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    birth_date = as.Date(c("1960-01-01", "1960-01-01", "1975-01-01")) # ages 65, 65, 50
  )
  # P001: age and tenure met; P002: tenure 10 < 15; P003: age 50 < 60
  expect_equal(retired_by(retirement_rule_age_tenure(60, 15), contract_dt, personnel_dt), "P001")
})

# =============================================================================
# Missing values
# =============================================================================

test_that("a tenure rule works when nobody has an age", {
  personnel_dt <- create_test_personnel_dt()[, birth_date := as.Date(NA)]
  out <- select_retirees(create_test_contract_dt(),
                         add_age_tenure(create_test_contract_dt(), personnel_dt),
                         design_retirement_policy_eligibility(retirement_rule_tenure(15), pension_flat(1)),
                         as.Date("2025-01-01"))
  expect_equal(sort(out$personnel_id), c("P001", "P002", "P004"))
  expect_true(all(is.na(out$age)))
})

test_that("someone with no age does not meet an age rule", {
  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    birth_date = as.Date(c("1960-01-01", NA)) # P002 has no birth date, so no age
  )
  expect_equal(retired_by(retirement_rule_age(60), create_test_contract_dt()[1:2], personnel_dt), "P001")
})

test_that("someone with no tenure does not meet a tenure rule", {
  contract_dt <- data.table(
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P002"),
    start_date = as.Date(c("2000-01-01", NA)), # P002 has no start date, so no tenure
    end_date = as.Date(c(NA, NA)),
    contract_type = c("perm", "perm")
  )
  personnel_dt <- data.table(personnel_id = c("P001", "P002"), birth_date = as.Date("1970-01-01"))
  expect_equal(retired_by(retirement_rule_tenure(15), contract_dt, personnel_dt), "P001")
})

# =============================================================================
# compute_retirement_summary()
# =============================================================================

test_that("compute_retirement_summary calculates correct statistics", {
  retirees_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    pension = c(2000, 2500, 1800),
    age = c(65, 62, 68),
    tenure_years = c(25, 20, 30)
  )

  result <- compute_retirement_summary(retirees_dt)

  expect_s3_class(result, "data.table")
  expect_equal(result$n_retired, 3)
  expect_equal(result$total_pension, 6300)
  expect_equal(result$avg_pension, 2100)
  expect_equal(result$avg_age, 65)
  expect_equal(result$avg_tenure, 25)
})

test_that("compute_retirement_summary handles empty retirees", {
  retirees_dt <- data.table(
    personnel_id = character(),
    pension = numeric(),
    age = numeric(),
    tenure_years = numeric()
  )

  result <- compute_retirement_summary(retirees_dt)

  expect_equal(result$n_retired, 0)
  expect_equal(result$total_pension, 0)
  expect_true(is.na(result$avg_pension))
  expect_true(is.na(result$avg_age))
  expect_true(is.na(result$avg_tenure))
})

test_that("compute_retirement_summary handles NA pensions", {
  retirees_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    pension = c(2000, NA, 1800),
    age = c(65, 62, 68),
    tenure_years = c(25, 20, 30)
  )

  result <- compute_retirement_summary(retirees_dt)

  expect_equal(result$n_retired, 3)
  expect_equal(result$total_pension, 3800) # Excludes NA
  expect_equal(result$avg_pension, 1900) # Mean of 2000 and 1800
})

test_that("compute_retirement_summary handles NA ages", {
  retirees_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    pension = c(2000, 2500, 1800),
    age = c(65, NA, 68),
    tenure_years = c(25, 20, 30)
  )

  result <- compute_retirement_summary(retirees_dt)

  expect_equal(result$n_retired, 3)
  expect_equal(result$avg_age, 66.5) # Mean of 65 and 68
})

test_that("compute_retirement_summary handles all NA values", {
  retirees_dt <- data.table(
    personnel_id = c("P001", "P002"),
    pension = c(NA_real_, NA_real_),
    age = c(NA_real_, NA_real_),
    tenure_years = c(NA_real_, NA_real_)
  )

  result <- compute_retirement_summary(retirees_dt)

  expect_equal(result$n_retired, 2)
  expect_equal(result$total_pension, 0)
  expect_true(is.nan(result$avg_pension))
  expect_true(is.nan(result$avg_age))
  expect_true(is.nan(result$avg_tenure))
})

# =============================================================================
# prepare_retiree_data()
# =============================================================================

test_that("prepare_retiree_data enriches eligibility data correctly", {
  contract_dt <- data.table(
    ref_date = as.Date("2025-01-01"),
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P002"),
    start_date = as.Date(c("2000-01-01", "2010-01-01")),
    end_date = as.Date(c(NA, NA)),
    contract_type = c("perm", "perm"),
    gross_salary_lcu = c(5000, 6000),
    position_id = c("POS1", "POS2")
  )

  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    employment_status = c("active", "active")
  )

  eligibility_dt <- data.table(
    personnel_id = c("P001", "P002"),
    retire = c(1L, 0L),
    age = c(65, 50),
    tenure_years = c(25, 15)
  )

  ref_date <- as.Date("2025-01-01")

  result <- prepare_retiree_data(
    eligibility_dt,
    contract_dt,
    personnel_dt,
    ref_date
  )

  # Should only include P001 (retire = 1)
  expect_equal(nrow(result), 1)
  expect_equal(result$personnel_id, "P001")
  expect_equal(result$gross_salary_lcu, 5000)
  expect_equal(result$age, 65)
  expect_equal(result$tenure_years, 25)
})

test_that("prepare_retiree_data returns empty when no retirees", {
  contract_dt <- create_test_contract_dt()
  personnel_dt <- create_test_personnel_dt()

  eligibility_dt <- data.table(
    personnel_id = c("P001", "P002", "P003"),
    retire = c(0L, 0L, 0L),
    age = c(45, 50, 40),
    tenure_years = c(5, 10, 8)
  )

  ref_date <- as.Date("2025-01-01")

  result <- prepare_retiree_data(
    eligibility_dt,
    contract_dt,
    personnel_dt,
    ref_date
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

test_that("prepare_retiree_data handles multiple contracts per retiree", {
  contract_dt <- data.table(
    ref_date = as.Date("2025-01-01"),
    contract_id = c("C001", "C002", "C003"),
    personnel_id = c("P001", "P001", "P002"), # P001 has 2 contracts
    start_date = as.Date(c("2000-01-01", "2010-01-01", "2015-01-01")),
    end_date = as.Date(c(NA, NA, NA)),
    contract_type = c("perm", "perm", "perm"),
    gross_salary_lcu = c(5000, 7000, 6000) # C002 is primary (highest salary)
  )

  personnel_dt <- data.table(
    personnel_id = c("P001", "P002"),
    employment_status = c("active", "active")
  )

  eligibility_dt <- data.table(
    personnel_id = c("P001", "P002"),
    retire = c(1L, 0L),
    age = c(65, 50),
    tenure_years = c(25, 10)
  )

  ref_date <- as.Date("2025-01-01")

  result <- prepare_retiree_data(
    eligibility_dt,
    contract_dt,
    personnel_dt,
    ref_date
  )

  # Should select C002 as primary (highest salary)
  expect_equal(nrow(result), 1)
  expect_equal(result$personnel_id, "P001")
  expect_equal(result$contract_id, "C002")
  # Its pay is added up across the retiree's contracts (decision on B-15)
  expect_equal(result$gross_salary_lcu, 12000)
})

test_that("prepare_retiree_data filters inactive contracts", {
  contract_dt <- data.table(
    ref_date = as.Date("2025-01-01"),
    contract_id = c("C001", "C002"),
    personnel_id = c("P001", "P001"),
    start_date = as.Date(c("2000-01-01", "2010-01-01")),
    end_date = as.Date(c("2020-01-01", NA)), # C001 ended
    contract_type = c("inactive", "perm"),
    gross_salary_lcu = c(9000, 5000)
  )

  personnel_dt <- data.table(
    personnel_id = "P001",
    employment_status = "active"
  )

  eligibility_dt <- data.table(
    personnel_id = "P001",
    retire = 1L,
    age = 65,
    tenure_years = 25
  )

  ref_date <- as.Date("2025-01-01")

  result <- prepare_retiree_data(
    eligibility_dt,
    contract_dt,
    personnel_dt,
    ref_date
  )

  # Should only include active contract C002
  expect_equal(nrow(result), 1)
  expect_equal(result$contract_id, "C002")
  expect_equal(result$gross_salary_lcu, 5000)
})

test_that("prepare_retiree_data preserves all contract columns", {
  contract_dt <- data.table(
    ref_date = as.Date("2025-01-01"),
    contract_id = "C001",
    personnel_id = "P001",
    start_date = as.Date("2000-01-01"),
    end_date = as.Date(NA),
    contract_type = "perm",
    gross_salary_lcu = 5000,
    position_id = "POS1",
    paygrade_code = "G10",
    org_unit_id = "ORG1"
  )

  personnel_dt <- data.table(
    personnel_id = "P001",
    employment_status = "active"
  )

  eligibility_dt <- data.table(
    personnel_id = "P001",
    retire = 1L,
    age = 65,
    tenure_years = 25
  )

  ref_date <- as.Date("2025-01-01")

  result <- prepare_retiree_data(
    eligibility_dt,
    contract_dt,
    personnel_dt,
    ref_date
  )

  # Check all columns are preserved
  expect_true("position_id" %in% names(result))
  expect_true("paygrade_code" %in% names(result))
  expect_true("org_unit_id" %in% names(result))
  expect_equal(result$position_id, "POS1")
  expect_equal(result$paygrade_code, "G10")
  expect_equal(result$org_unit_id, "ORG1")
})



# ===========================================================================
# A rule with thresholds by group, from a contract column
# ===========================================================================

test_that("a rule's min_age can differ by group", {
  contract_dt <- data.table::data.table(
    personnel_id = c("P001", "P002", "P003"),
    contract_id = c("C001", "C002", "C003"),
    start_date = as.Date("2000-01-01"),
    end_date = as.Date(NA),
    contract_type = "perm",
    gross_salary_lcu = c(5000, 4000, 3000),
    grade = c("A", "B", "A")
  )
  personnel_dt <- data.table::data.table(
    personnel_id = c("P001", "P002", "P003"),
    # P001/P002: age 65 at ref_date; P003: age 50
    birth_date = as.Date(c("1960-01-01", "1960-01-01", "1975-01-01"))
  )
  # Grade A: min_age 60 -> P001 (65) retires, P003 (50) does not
  # Grade B: min_age 68 -> P002 (65) does NOT retire
  grade_tbl <- data.table::data.table(grade = c("A", "B"), min_age = c(60, 68))
  rule <- retirement_rule_age(60, group_cols = "grade", policy_table = grade_tbl)
  expect_equal(retired_by(rule, contract_dt, personnel_dt), "P001")
})

test_that("a rule's min_tenure can differ by group", {
  contract_dt <- data.table::data.table(
    personnel_id = c("P001", "P002"),
    contract_id = c("C001", "C002"),
    start_date = as.Date(c("1995-01-01", "2010-01-01")),
    end_date = as.Date(NA),
    contract_type = "perm",
    gross_salary_lcu = c(5000, 4000),
    grade = c("A", "B")
  )
  personnel_dt <- data.table::data.table(
    personnel_id = c("P001", "P002"),
    birth_date = as.Date(c("1970-01-01", "1970-01-01"))
  )
  # P001: tenure ~30 yrs, grade A (min_tenure 25) -> retires
  # P002: tenure ~15 yrs, grade B (min_tenure 20) -> does NOT retire
  grade_tbl <- data.table::data.table(grade = c("A", "B"), min_tenure = c(25, 20))
  rule <- retirement_rule_tenure(25, group_cols = "grade", policy_table = grade_tbl)
  expect_equal(retired_by(rule, contract_dt, personnel_dt), "P001")
})

test_that("a group the rule table does not list uses the rule's default", {
  contract_dt <- data.table::data.table(
    personnel_id = c("P001", "P002"),
    contract_id = c("C001", "C002"),
    start_date = as.Date("2000-01-01"),
    end_date = as.Date(NA),
    contract_type = "perm",
    gross_salary_lcu = c(5000, 4000),
    grade = c("A", "C") # grade C not in policy_table
  )
  personnel_dt <- data.table::data.table(
    personnel_id = c("P001", "P002"),
    birth_date = as.Date(c("1960-01-01", "1960-01-01")) # both age 65
  )
  # Grade C unmatched -> falls back to default min_age = 62; age 65 >= 62 -> retires
  rule <- retirement_rule_age(62, group_cols = "grade",
                              policy_table = data.table::data.table(grade = "A", min_age = 60))
  expect_equal(retired_by(rule, contract_dt, personnel_dt), c("P001", "P002"))
})

# =============================================================================
# Status-quo rates, with and without a mandatory rule
# =============================================================================

create_hazard_contract_dt <- function(n = 10) {
  data.table(
    ref_date          = as.Date("2024-01-01"),
    contract_id       = paste0("c", seq_len(n)),
    personnel_id      = paste0("p", seq_len(n)),
    start_date        = as.Date("2010-01-01"),
    end_date          = as.Date(NA),
    contract_type     = "permanent",
    gross_salary_lcu  = 1000
  )
}

# Who a status-quo or mandatory policy retires at 2024-01-01
retire_at_rates <- function(policy, contract_dt, personnel_dt) {
  select_retirees(contract_dt, personnel_dt, policy, as.Date("2024-01-01"))
}

rates_by_age <- function(table, retire_strategy = "random") {
  retirement_rates_status_quo(hazard_table = table, retire_strategy = retire_strategy)
}

test_that("a mandatory rule retires everyone who meets it, whatever the rate", {
  contract_dt  <- create_hazard_contract_dt(3)
  personnel_dt <- data.table(
    personnel_id      = c("p1", "p2", "p3"),
    ref_date          = as.Date("2024-01-01"),
    age               = c(58, 70, 71),
    employment_status = "active"
  )
  # hazard_rate = 0 everywhere -- only the age-70 rule should retire anyone.
  policy <- design_retirement_policy_mandatory(
    retirement_rule_age(70), pension_flat(1),
    rates_by_age(data.table(age_yr_int = 58:71, hazard_rate = 0)))

  result <- retire_at_rates(policy, contract_dt, personnel_dt)

  expect_setequal(result$personnel_id, c("p2", "p3"))
})

test_that("status quo retires the exact expected number per age when it is whole", {
  contract_dt  <- create_hazard_contract_dt(4)
  personnel_dt <- data.table(
    personnel_id      = c("p1", "p2", "p3", "p4"),
    ref_date          = as.Date("2024-01-01"),
    age               = c(60, 60, 61, 61),
    employment_status = "active"
  )
  # age 60: hazard_rate = 0.5 -> 2 * 0.5 = 1 retiree out of {p1, p2}
  # age 61: hazard_rate = 1.0 -> 2 * 1.0 = 2 retirees, i.e. both {p3, p4}
  policy <- design_retirement_policy_status_quo(
    pension_flat(1), rates_by_age(data.table(age_yr_int = c(60L, 61L), hazard_rate = c(0.5, 1.0))))

  result <- retire_at_rates(policy, contract_dt, personnel_dt)

  expect_equal(sum(result$personnel_id %in% c("p1", "p2")), 1L)
  expect_setequal(result$personnel_id[result$personnel_id %in% c("p3", "p4")], c("p3", "p4"))
  expect_equal(nrow(result), 3L)
})

test_that("status quo: ages the rate table does not cover retire nobody, without an error", {
  contract_dt  <- create_hazard_contract_dt(2)
  personnel_dt <- data.table(
    personnel_id      = c("p1", "p2"),
    ref_date          = as.Date("2024-01-01"),
    age               = c(45, 50), # neither age appears in the rate table
    employment_status = "active"
  )
  policy <- design_retirement_policy_status_quo(
    pension_flat(1), rates_by_age(data.table(age_yr_int = 60:65, hazard_rate = 1)))

  expect_equal(nrow(retire_at_rates(policy, contract_dt, personnel_dt)), 0L)
})

test_that("status quo: a ranking retire_strategy picks the lowest values first", {
  contract_dt <- create_hazard_contract_dt(4)
  contract_dt[, gross_salary_lcu := c(500, 100, 300, 200)]
  personnel_dt <- data.table(
    personnel_id      = c("p1", "p2", "p3", "p4"),
    ref_date          = as.Date("2024-01-01"),
    age               = 60,
    employment_status = "active"
  )
  # n_active = 4, hazard_rate = 0.5 -> n_retire = 2; ranking picks the
  # two lowest gross_salary_lcu (a contract column): p2 (100), p4 (200).
  policy <- design_retirement_policy_status_quo(
    pension_flat(1),
    rates_by_age(data.table(age_yr_int = 60L, hazard_rate = 0.5), retire_strategy = "gross_salary_lcu"))

  expect_setequal(retire_at_rates(policy, contract_dt, personnel_dt)$personnel_id, c("p2", "p4"))
})

test_that("status quo: a random draw is reproducible under a fixed seed", {
  contract_dt  <- create_hazard_contract_dt(10)
  personnel_dt <- data.table(
    personnel_id      = paste0("p", 1:10),
    ref_date          = as.Date("2024-01-01"),
    age               = 60,
    employment_status = "active"
  )
  policy <- design_retirement_policy_status_quo(
    pension_flat(1), rates_by_age(data.table(age_yr_int = 60L, hazard_rate = 0.5)))

  set.seed(42)
  result1 <- retire_at_rates(policy, contract_dt, personnel_dt)
  set.seed(42)
  result2 <- retire_at_rates(policy, contract_dt, personnel_dt)

  expect_identical(sort(result1$personnel_id), sort(result2$personnel_id))
})

test_that("status quo: rates keyed by neither age nor tenure stop with an error", {
  contract_dt  <- create_hazard_contract_dt(2)
  personnel_dt <- data.table(
    personnel_id      = c("p1", "p2"),
    ref_date          = as.Date("2024-01-01"),
    age               = c(60, 61),
    employment_status = "active"
  )
  policy <- list(type = "status_quo",
                 rates = list(group_cols = "grade", policy_table = data.table(grade = "A", hazard_rate = 1)),
                 pension = pension_flat(1))

  expect_error(retire_at_rates(policy, contract_dt, personnel_dt), regexp = "age or tenure")
})

test_that("a mandatory rule does not retire someone with no age", {
  # A candidate with no age (e.g. no birth date in the data) must not be
  # retired by the rule, nor add a phantom NA ID to the result.
  contract_dt  <- create_hazard_contract_dt(3)
  personnel_dt <- data.table(
    personnel_id      = c("p1", "p2", "p3"),
    ref_date          = as.Date("2024-01-01"),
    age               = c(80, NA, 60),
    employment_status = "active"
  )
  policy <- design_retirement_policy_mandatory(
    retirement_rule_age(75), pension_flat(1),
    rates_by_age(data.table(age_yr_int = c(60L, 80L), hazard_rate = 0)))

  result <- retire_at_rates(policy, contract_dt, personnel_dt)

  expect_false(anyNA(result$personnel_id))
  expect_setequal(result$personnel_id, "p1")  # only p1 (age 80) meets the rule
})
