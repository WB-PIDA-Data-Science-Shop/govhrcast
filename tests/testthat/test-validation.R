# Unit tests for validation.R functions
# Testing validation helpers with edge cases and missing values.

library(testthat)
library(data.table)

# Access internal helper for direct tests.
check_safe_calls <- getFromNamespace(".check_safe_calls", "govhrcast")

# =============================================================================
# validate_datatable()
# =============================================================================

test_that("validate_datatable accepts valid data.table", {
  dt <- data.table(a = 1:5, b = letters[1:5])
  expect_invisible(validate_datatable(dt, "test_dt"))
})

test_that("validate_datatable rejects non-data.table", {
  df <- data.frame(a = 1:5, b = letters[1:5])
  expect_error(validate_datatable(df, "test_dt"), "must be a data.table")
})

test_that("validate_datatable rejects empty data.table", {
  dt <- data.table(a = integer(), b = character())
  expect_error(validate_datatable(dt, "test_dt"), "cannot be empty")
})

test_that("validate_datatable rejects NULL", {
  expect_error(validate_datatable(NULL, "test_dt"), "must be a data.table")
})

# =============================================================================
# validate_column_exists()
# =============================================================================

test_that("validate_column_exists accepts existing column", {
  dt <- data.table(id = 1:5, name = letters[1:5])
  expect_invisible(validate_column_exists(dt, "id", "test_dt"))
})

test_that("validate_column_exists rejects missing column", {
  dt <- data.table(id = 1:5, name = letters[1:5])
  expect_error(
    validate_column_exists(dt, "missing_col", "test_dt"),
    "Column 'missing_col' not found in test_dt"
  )
})

test_that("validate_column_exists is case sensitive", {
  dt <- data.table(ID = 1:5, NAME = letters[1:5])
  expect_error(
    validate_column_exists(dt, "id", "test_dt"),
    "Column 'id' not found"
  )
})

# =============================================================================
# validate_columns_exist()
# =============================================================================

test_that("validate_columns_exist accepts all existing columns", {
  dt <- data.table(id = 1:5, name = letters[1:5], value = rnorm(5))
  expect_invisible(validate_columns_exist(
    dt,
    c("id", "name", "value"),
    "test_dt"
  ))
})

test_that("validate_columns_exist rejects when some columns missing", {
  dt <- data.table(id = 1:5, name = letters[1:5])
  expect_error(
    validate_columns_exist(dt, c("id", "missing1", "missing2"), "test_dt"),
    "Columns not found in test_dt: missing1, missing2"
  )
})

test_that("validate_columns_exist handles empty column vector", {
  dt <- data.table(id = 1:5, name = letters[1:5])
  expect_invisible(validate_columns_exist(dt, character(), "test_dt"))
})

# =============================================================================
# validate_date_format()
# =============================================================================

test_that("validate_date_format accepts valid Date", {
  date <- as.Date("2025-01-01")
  result <- validate_date_format(date, "test_date")
  expect_true(inherits(result, "Date"))
  expect_equal(result, date)
})

test_that("validate_date_format accepts character string and converts", {
  result <- validate_date_format("2025-01-01", "test_date")
  expect_true(inherits(result, "Date"))
  expect_equal(result, as.Date("2025-01-01"))
})

test_that("validate_date_format rejects numeric", {
  expect_error(
    validate_date_format(19723, "test_date"),
    "must be a Date object"
  )
})

test_that("validate_date_format rejects vector of dates", {
  dates <- as.Date(c("2025-01-01", "2025-01-02"))
  expect_error(
    validate_date_format(dates, "test_date"),
    "must be a single Date value"
  )
})

test_that("validate_date_format rejects NA Date", {
  date <- as.Date(NA)
  expect_error(
    validate_date_format(date, "test_date"),
    "cannot be NA"
  )
})

# =============================================================================
# validate_positive_number()
# =============================================================================

test_that("validate_positive_number accepts positive number", {
  expect_invisible(validate_positive_number(10, "test_num"))
  expect_invisible(validate_positive_number(0.001, "test_num"))
})

test_that("validate_positive_number accepts zero when allow_zero=TRUE", {
  expect_invisible(validate_positive_number(0, "test_num", allow_zero = TRUE))
})

test_that("validate_positive_number rejects zero by default", {
  expect_error(
    validate_positive_number(0, "test_num"),
    "must be > 0"
  )
})

test_that("validate_positive_number rejects negative number", {
  expect_error(
    validate_positive_number(-5, "test_num"),
    "must be > 0"
  )
})

test_that("validate_positive_number rejects negative with allow_zero=TRUE", {
  expect_error(
    validate_positive_number(-5, "test_num", allow_zero = TRUE),
    "must be >= 0"
  )
})

test_that("validate_positive_number rejects character", {
  expect_error(
    validate_positive_number("10", "test_num"),
    "must be numeric"
  )
})

test_that("validate_positive_number rejects vector", {
  expect_error(
    validate_positive_number(c(1, 2, 3), "test_num"),
    "must be a single numeric value"
  )
})

test_that("validate_positive_number rejects NA", {
  expect_error(
    validate_positive_number(NA_real_, "test_num"),
    "cannot be NA"
  )
})

# =============================================================================
# validate_number_range()
# =============================================================================

test_that("validate_number_range accepts value in range", {
  expect_invisible(validate_number_range(50, "test_num", 0, 100))
  expect_invisible(validate_number_range(0, "test_num", 0, 100)) # min boundary
  expect_invisible(validate_number_range(100, "test_num", 0, 100)) # max boundary
})

test_that("validate_number_range rejects value below min", {
  expect_error(
    validate_number_range(-1, "test_num", 0, 100),
    "must be between 0 and 100"
  )
})

test_that("validate_number_range rejects value above max", {
  expect_error(
    validate_number_range(101, "test_num", 0, 100),
    "must be between 0 and 100"
  )
})

test_that("validate_number_range rejects non-numeric", {
  expect_error(
    validate_number_range("50", "test_num", 0, 100),
    "must be numeric"
  )
})

test_that("validate_number_range rejects NA", {
  expect_error(
    validate_number_range(NA_real_, "test_num", 0, 100),
    "cannot be NA"
  )
})

# =============================================================================
# validate_character_string()
# =============================================================================

test_that("validate_character_string accepts valid string", {
  expect_invisible(validate_character_string("hello", "test_str"))
})

test_that("validate_character_string rejects numeric", {
  expect_error(
    validate_character_string(123, "test_str"),
    "must be a single character string"
  )
})

test_that("validate_character_string rejects character vector", {
  expect_error(
    validate_character_string(c("a", "b"), "test_str"),
    "must be a single character string"
  )
})

test_that("validate_character_string rejects NA by default", {
  expect_error(
    validate_character_string(NA_character_, "test_str"),
    "cannot be NA"
  )
})

test_that("validate_character_string accepts NA when allow_na=TRUE", {
  expect_invisible(validate_character_string(
    NA_character_,
    "test_str",
    allow_na = TRUE
  ))
})

test_that("validate_character_string accepts empty string", {
  expect_invisible(validate_character_string("", "test_str"))
})

# =============================================================================
# validate_choice()
# =============================================================================

test_that("validate_choice accepts valid choice", {
  expect_invisible(validate_choice("red", c("red", "blue", "green"), "color"))
})

test_that("validate_choice rejects invalid choice", {
  expect_error(
    validate_choice("yellow", c("red", "blue", "green"), "color"),
    "Invalid color: 'yellow'. Valid options: red, blue, green"
  )
})

test_that("validate_choice is case sensitive", {
  expect_error(
    validate_choice("RED", c("red", "blue", "green"), "color"),
    "Invalid color: 'RED'"
  )
})

test_that("validate_choice rejects numeric", {
  expect_error(
    validate_choice(1, c("red", "blue", "green"), "color"),
    "must be a single character string"
  )
})

test_that("validate_choice rejects character vector", {
  expect_error(
    validate_choice(c("red", "blue"), c("red", "blue", "green"), "color"),
    "must be a single character string"
  )
})

# =============================================================================
# validate_required_params()
# =============================================================================

test_that("validate_required_params accepts all required params", {
  params <- list(min_age = 60, min_tenure = 15, pension_type = "db")
  expect_invisible(
    validate_required_params(params, c("min_age", "min_tenure"), "test context")
  )
})

test_that("validate_required_params rejects missing params", {
  params <- list(min_age = 60)
  expect_error(
    validate_required_params(
      params,
      c("min_age", "min_tenure"),
      "test context"
    ),
    "Missing required parameters for test context: min_tenure"
  )
})

test_that("validate_required_params handles multiple missing params", {
  params <- list(min_age = 60)
  expect_error(
    validate_required_params(
      params,
      c("min_age", "min_tenure", "pension_type"),
      "test context"
    ),
    "Missing required parameters for test context: min_tenure, pension_type"
  )
})

test_that("validate_required_params rejects non-list", {
  expect_error(
    validate_required_params("not a list", c("min_age"), "test context"),
    "policy_params must be a list"
  )
})

test_that("validate_required_params accepts empty required params", {
  params <- list(min_age = 60, min_tenure = 15)
  expect_invisible(
    validate_required_params(params, character(), "test context")
  )
})

# =============================================================================
# check_retirement_selection_inputs() and check_retirement_pension_inputs()
# =============================================================================

# One person aged 70 with 30 years' service, on one permanent teacher contract
selection_data <- function() {
  list(
    c = data.table(contract_id = "C1", personnel_id = "P1", occupation = "teacher",
                   start_date = as.Date("2000-01-01"), end_date = as.Date(NA),
                   contract_type = "permanent", gross_salary_lcu = 100),
    p = data.table(personnel_id = "P1", employment_status = "active", age = 70,
                   personnel_tenure = 30)
  )
}

check_selection <- function(policy, d = selection_data(), date = as.Date("2025-01-01")) {
  check_retirement_selection_inputs(d$c, d$p, policy, date)
}

age_60 <- design_retirement_policy_eligibility(retirement_rule_age(60), pension_flat(1))

test_that("check_retirement_selection_inputs accepts a usable policy", {
  expect_invisible(check_selection(age_60))
  # A rule keyed by a column held on contracts, not on people
  by_occupation <- retirement_rule_age(75, "occupation", data.table(occupation = "teacher", min_age = 70))
  expect_invisible(check_selection(design_retirement_policy_eligibility(by_occupation, pension_flat(1))))
  # An empty rate table is fine: the history showed no retirements
  no_rates <- retirement_rates_status_quo(
    hazard_table = data.table(age_yr_int = integer(), hazard_rate = numeric()))
  expect_invisible(check_selection(design_retirement_policy_status_quo(pension_flat(1), no_rates)))
})

test_that("check_retirement_selection_inputs checks the data and the date", {
  d <- selection_data()
  expect_error(check_retirement_selection_inputs(NULL, d$p, age_60, as.Date("2025-01-01")),
               "contract_dt must be a data.table")
  expect_error(check_retirement_selection_inputs(d$c, NULL, age_60, as.Date("2025-01-01")),
               "personnel_dt must be a data.table")
  expect_error(check_selection(age_60, date = "not-a-date"), "must be a valid date")
  expect_no_error(check_selection(age_60, date = "2025-01-01"))
  d$c[, contract_type := NULL]
  expect_error(check_selection(age_60, d),
               "contract_type_col = .contract_type.: column not found in contract_dt")
  d <- selection_data()
  d$p[, employment_status := NULL]
  expect_error(check_selection(age_60, d),
               "status_col = .employment_status.: column not found in personnel_dt")
})

test_that("check_retirement_selection_inputs stops on a policy with no type", {
  old_style <- list(defaults = list(eligibility_type = "age_only", min_age = 60))
  expect_error(check_selection(old_style), "has no 'type'")
  expect_error(check_selection("age 60"), "has no 'type'")
  expect_error(check_selection(list(type = "early")), "retirement policy type")
})

test_that("check_retirement_selection_inputs stops on status-quo rates it cannot use", {
  expect_error(check_selection(design_retirement_policy_status_quo(pension_flat(1))),
               "has no rates.*estimate_retirement_hazard_rates")
  no_age_or_tenure <- list(type = "status_quo",
                           rates = list(group_cols = "est_id", policy_table = data.table(est_id = "E1", hazard_rate = 0)))
  expect_error(check_selection(no_age_or_tenure), "must vary by age or tenure")
  no_rate_column <- list(type = "status_quo",
                         rates = list(group_cols = "age_yr_int", policy_table = data.table(age_yr_int = 60L)))
  expect_error(check_selection(no_rate_column), "columns age_yr_int, hazard_rate")
})

test_that("check_retirement_selection_inputs stops on a rule it cannot use", {
  expect_error(check_selection(list(type = "eligibility", rule = list(defaults = list(kind = "age")))),
               "'age' retirement rule needs min_age")
  expect_error(check_selection(list(type = "eligibility", rule = list(defaults = list(kind = "tenure")))),
               "'tenure' retirement rule needs min_tenure")
  expect_error(check_selection(list(type = "eligibility", rule = list(defaults = list(kind = "early")))),
               "retirement rule kind")
})

test_that("check_retirement_selection_inputs stops on a column neither table has", {
  by_grade <- retirement_rule_age(75, "grade", data.table(grade = "A", min_age = 70))
  expect_error(check_selection(design_retirement_policy_eligibility(by_grade, pension_flat(1))),
               "neither personnel_dt nor contract_dt has: grade")
})

test_that("check_retirement_selection_inputs needs the age or tenure column the policy uses", {
  d <- selection_data()
  d$p[, age := NULL]
  expect_error(check_selection(age_60, d), "uses age, but personnel_dt has no 'age' column")
  d <- selection_data()
  d$p[, personnel_tenure := NULL]
  on_tenure <- design_retirement_policy_eligibility(retirement_rule_custom("tenure_years >= 35"), pension_flat(1))
  expect_error(check_selection(on_tenure, d), "uses tenure, but personnel_dt has no 'personnel_tenure' column")
  # A policy by age only does not need the tenure column
  expect_invisible(check_selection(age_60, d))
})

test_that("check_retirement_pension_inputs accepts a usable pension, by group too", {
  expect_invisible(check_retirement_pension_inputs(age_60))
  pension <- pension_db(0.02)
  pension$group_cols   <- "paygrade"
  pension$policy_table <- data.table(paygrade = "D", accrual_rate = 0.025)
  expect_invisible(check_retirement_pension_inputs(
    design_retirement_policy_eligibility(retirement_rule_age(60), pension)))
})

test_that("check_retirement_pension_inputs stops on a pension it cannot pay", {
  expect_error(check_retirement_pension_inputs(list(type = "eligibility")), "has no pension")
  expect_error(check_retirement_pension_inputs(list(pension = list(defaults = list(pension_type = "gold")))),
               "pension_type")
  by_group <- list(pension = list(group_cols = "paygrade",
                                  policy_table = data.table(paygrade = "A", pension_type = "gold"),
                                  defaults = list(pension_type = "flat", flat_amount = 1)))
  expect_error(check_retirement_pension_inputs(by_group), "Unknown pension_type in the pension table: gold")
  group_cols_without_table <- list(pension = list(group_cols = "paygrade", defaults = list(pension_type = "flat")))
  expect_error(check_retirement_pension_inputs(group_cols_without_table), "policy_table is NULL")
})

# ===========================================================================
# validate_param_spec()
# ===========================================================================

test_that("validate_param_spec: bare positive scalar passes", {
  expect_invisible(validate_param_spec(60, "min_age"))
})

test_that("validate_param_spec: bare scalar zero or negative raises error", {
  expect_error(validate_param_spec(0, "min_age"), "must be > 0")
  expect_error(validate_param_spec(-1, "min_age"), "must be > 0")
})

test_that("validate_param_spec: bare non-numeric raises error", {
  expect_error(
    validate_param_spec("60", "min_age"),
    "single non-NA numeric scalar"
  )
})

test_that("validate_param_spec: bare NA raises error", {
  expect_error(
    validate_param_spec(NA_real_, "min_age"),
    "single non-NA numeric scalar"
  )
})

test_that("validate_param_spec: scalar list (group_cols NULL) passes", {
  expect_invisible(
    validate_param_spec(
      list(default = 60, group_cols = NULL, policy_table = NULL),
      "min_age"
    )
  )
})

test_that("validate_param_spec: valid group-level spec passes", {
  tbl <- data.table::data.table(grade = c("A", "B"), min_age = c(60, 55))
  expect_invisible(
    validate_param_spec(
      list(default = 58, group_cols = "grade", policy_table = tbl),
      "min_age"
    )
  )
})

test_that("validate_param_spec: group_cols set but policy_table NULL raises error", {
  expect_error(
    validate_param_spec(
      list(default = 60, group_cols = "grade", policy_table = NULL),
      "min_age"
    ),
    "group_cols supplied but policy_table is NULL"
  )
})

test_that("validate_param_spec: policy_table set but group_cols NULL raises error", {
  tbl <- data.table::data.table(grade = "A", min_age = 60)
  expect_error(
    validate_param_spec(
      list(default = NULL, group_cols = NULL, policy_table = tbl),
      "min_age"
    ),
    "policy_table supplied but group_cols is NULL"
  )
})

test_that("validate_param_spec: policy_table missing group_cols column raises error", {
  tbl <- data.table::data.table(employment_type = "permanent", min_age = 60)
  expect_error(
    validate_param_spec(
      list(default = NULL, group_cols = "grade", policy_table = tbl),
      "min_age"
    ),
    "missing group_cols column"
  )
})

test_that("validate_param_spec: policy_table missing param_name column raises error", {
  tbl <- data.table::data.table(grade = "A", retirement_age = 60)
  expect_error(
    validate_param_spec(
      list(default = NULL, group_cols = "grade", policy_table = tbl),
      "min_age"
    ),
    "must contain a column named 'min_age'"
  )
})

test_that("validate_param_spec: empty policy_table raises error", {
  tbl <- data.table::data.table(grade = character(0), min_age = numeric(0))
  expect_error(
    validate_param_spec(
      list(default = 60, group_cols = "grade", policy_table = tbl),
      "min_age"
    ),
    "0 rows"
  )
})

test_that("validate_param_spec: negative default raises error", {
  tbl <- data.table::data.table(grade = "A", min_age = 60)
  expect_error(
    validate_param_spec(
      list(default = -5, group_cols = "grade", policy_table = tbl),
      "min_age"
    ),
    "default must be > 0"
  )
})

# ===========================================================================
# validate_policy_table()
# ===========================================================================

test_that("validate_policy_table: valid scalar-only spec passes", {
  pp <- list(
    group_cols = NULL,
    policy_table = NULL,
    defaults = list(
      eligibility_type = "age_only",
      pension_type = "db",
      min_age = 60
    )
  )
  expect_invisible(validate_policy_table(pp))
})

test_that("validate_policy_table: valid group-level spec passes", {
  tbl <- data.table::data.table(
    grade = c("A", "B"),
    accrual_rate = c(0.02, 0.025)
  )
  pp <- list(
    group_cols = "grade",
    policy_table = tbl,
    defaults = list(
      eligibility_type = "age_only",
      pension_type = "db",
      min_age = 60,
      accrual_rate = 0.02
    )
  )
  expect_invisible(validate_policy_table(pp))
})

test_that("validate_policy_table: not a list raises error", {
  expect_error(
    validate_policy_table("not a list"),
    "policy_params must be a list"
  )
})

test_that("validate_policy_table: missing defaults raises error", {
  expect_error(
    validate_policy_table(list(group_cols = NULL, policy_table = NULL)),
    "must contain a 'defaults' list"
  )
})

test_that("validate_policy_table: group_cols without policy_table raises error", {
  pp <- list(
    group_cols = "grade",
    policy_table = NULL,
    defaults = list(min_age = 60)
  )
  expect_error(
    validate_policy_table(pp),
    "group_cols supplied but policy_table is NULL"
  )
})

test_that("validate_policy_table: policy_table without group_cols raises error", {
  tbl <- data.table::data.table(grade = "A", min_age = 60)
  pp <- list(
    group_cols = NULL,
    policy_table = tbl,
    defaults = list(min_age = 60)
  )
  expect_error(
    validate_policy_table(pp),
    "policy_table supplied but group_cols is NULL"
  )
})

test_that("validate_policy_table: empty policy_table raises error", {
  tbl <- data.table::data.table(grade = character(0), min_age = numeric(0))
  pp <- list(
    group_cols = "grade",
    policy_table = tbl,
    defaults = list(min_age = 60)
  )
  expect_error(validate_policy_table(pp), "0 rows")
})

test_that("validate_policy_table: policy_table missing group_cols column raises error", {
  tbl <- data.table::data.table(dept = "A", min_age = 60)
  pp <- list(
    group_cols = "grade",
    policy_table = tbl,
    defaults = list(min_age = 60)
  )
  expect_error(validate_policy_table(pp), "missing group_cols")
})

# =============================================================================
# .check_safe_calls() and validate_eligibility_rule()
# =============================================================================

test_that(".check_safe_calls accepts allow-listed operators", {
  expr <- str2lang("age >= 60 & tenure_years >= 15")
  expect_no_error(check_safe_calls(expr))
})

test_that(".check_safe_calls rejects disallowed function calls", {
  expr <- str2lang("system('echo hi')")
  expect_error(
    check_safe_calls(expr),
    "disallowed function/operator 'system'"
  )
})

test_that("validate_eligibility_rule parses and returns expression invisibly", {
  parsed <- validate_eligibility_rule("age >= 60 & tenure_years >= 15")
  expect_type(parsed, "language")
  expect_equal(all.vars(parsed), c("age", "tenure_years"))
})

test_that("validate_eligibility_rule rejects parse errors", {
  expect_error(
    validate_eligibility_rule("age >="),
    "could not be parsed"
  )
})

test_that("validate_eligibility_rule rejects disallowed calls", {
  expect_error(
    validate_eligibility_rule("system('echo x')"),
    "disallowed function/operator"
  )
})

test_that("validate_eligibility_rule validates referenced columns", {
  expect_no_error(
    validate_eligibility_rule(
      "age >= 60 & tenure_years >= 15",
      available_cols = c("age", "tenure_years", "grade")
    )
  )

  expect_error(
    validate_eligibility_rule(
      "age >= 60 & service_years >= 15",
      available_cols = c("age", "tenure_years")
    ),
    "references column\\(s\\) not found: service_years"
  )
})
