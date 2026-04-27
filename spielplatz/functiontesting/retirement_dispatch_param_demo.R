# =============================================================================
# Demo: Group-Level Policy Parameters in simulate_retirement()
#
# Shows the new unified policy_params design in action using the lazy-loaded
# Brazil HRMIS data. Three scenarios are run:
#
#   1. SCALAR  — classic usage, all params in `defaults` (no policy_table)
#   2. GROUP   — min_age varies by paygrade via policy_table
#   3. PENSION — accrual_rate varies by paygrade via policy_table
#
# Run with: devtools::load_all() then source this file.
# =============================================================================

library(data.table)

# -----------------------------------------------------------------------------
# 0. Data prep
# -----------------------------------------------------------------------------
contract_dt  <- data.table::copy(bra_hrmis_contract)
personnel_dt <- data.table::copy(bra_hrmis_personnel)

ref_date <- as.Date("2014-01-01")


# -----------------------------------------------------------------------------
# 1. SCALAR — every param is in defaults, no policy_table
# -----------------------------------------------------------------------------
policy_scalar <- list(
  group_cols   = NULL,
  policy_table = NULL,
  defaults = list(
    eligibility_type = "age_and_tenure",
    pension_type     = "db",
    min_age          = 55,
    min_tenure       = 10,
    accrual_rate     = 0.02,
    ref_wage_col     = "gross_salary_lcu",
    max_years        = 35,
    replacement_cap  = 0.80
  )
)

result_scalar <- simulate_retirement(
  contract_dt  = contract_dt,
  personnel_dt = personnel_dt,
  ref_date     = ref_date,
  policy_params = policy_scalar
)

cat("\n--- SCALAR result ---\n")
print(result_scalar$summary)


# -----------------------------------------------------------------------------
# 2. GROUP — min_age varies by paygrade
#    Grades A/B retire later (60), C/D earlier (55), E earliest (50).
#    Unmatched paygrades (NA or numeric codes) fall back to defaults$min_age.
# -----------------------------------------------------------------------------
min_age_tbl <- data.table::data.table(
  paygrade = c("A", "B", "C", "D", "E"),
  min_age  = c(  60,   60,   55,   55,   50)
)

policy_group_age <- list(
  group_cols   = "paygrade",
  policy_table = min_age_tbl,
  defaults = list(
    eligibility_type = "age_and_tenure",
    pension_type     = "db",
    min_age          = 55,   # fallback for unmatched paygrades
    min_tenure       = 10,
    accrual_rate     = 0.02,
    ref_wage_col     = "gross_salary_lcu",
    max_years        = 35,
    replacement_cap  = 0.80
  )
)

result_group_age <- simulate_retirement(
  contract_dt  = contract_dt,
  personnel_dt = personnel_dt,
  ref_date     = ref_date,
  policy_params = policy_group_age
)

cat("\n--- GROUP (min_age by paygrade) result ---\n")
print(result_group_age$summary)

cat("\nRetirees by paygrade:\n")
print(
  result_group_age$retirees_dt[
    contract_dt[, .(personnel_id, paygrade)],
    on = "personnel_id",
    nomatch = 0L
  ][, .N, by = paygrade][order(paygrade)]
)


# -----------------------------------------------------------------------------
# 3. PENSION — accrual_rate varies by paygrade
#    Senior grades (D/E) have a higher accrual rate; others use default.
# -----------------------------------------------------------------------------
accrual_tbl <- data.table::data.table(
  paygrade     = c("D",    "E"),
  accrual_rate = c(0.025,  0.03)
)

policy_group_pension <- list(
  group_cols   = "paygrade",
  policy_table = accrual_tbl,
  defaults = list(
    eligibility_type = "age_and_tenure",
    pension_type     = "db",
    min_age          = 55,
    min_tenure       = 10,
    accrual_rate     = 0.02,   # fallback for A, B, C and unmatched paygrades
    ref_wage_col     = "gross_salary_lcu",
    max_years        = 35,
    replacement_cap  = 0.80
  )
)

result_group_pension <- simulate_retirement(
  contract_dt  = contract_dt,
  personnel_dt = personnel_dt,
  ref_date     = ref_date,
  policy_params = policy_group_pension
)

cat("\n--- GROUP (accrual_rate by paygrade) result ---\n")
print(result_group_pension$summary)

# Compare average pension across scenarios
cat("\nAverage pension comparison:\n")
print(data.table::data.table(
  scenario      = c("scalar", "group_age", "group_pension"),
  n_retired     = c(result_scalar$summary$n_retired,
                    result_group_age$summary$n_retired,
                    result_group_pension$summary$n_retired),
  avg_pension   = round(c(result_scalar$summary$avg_pension,
                          result_group_age$summary$avg_pension,
                          result_group_pension$summary$avg_pension), 0),
  total_pension = round(c(result_scalar$summary$total_pension,
                          result_group_age$summary$total_pension,
                          result_group_pension$summary$total_pension), 0)
))


library(data.table)

# -----------------------------------------------------------------------------
# 0. Data prep
# -----------------------------------------------------------------------------
contract_dt  <- data.table::copy(bra_hrmis_contract)
personnel_dt <- data.table::copy(bra_hrmis_personnel)

ref_date <- as.Date("2014-01-01")


# -----------------------------------------------------------------------------
# 1. SCALAR — every param is a bare scalar (unchanged from old API)
# -----------------------------------------------------------------------------
policy_scalar <- list(
  eligibility_type = "age_and_tenure",
  min_age          = 55,
  min_tenure       = 10,
  pension_type     = "db",
  pension_params   = list(
    accrual_rate    = 0.02,
    ref_wage_col    = "gross_salary_lcu",
    max_years       = 35,
    replacement_cap = 0.80
  )
)

result_scalar <- simulate_retirement(
  contract_dt  = contract_dt,
  personnel_dt = personnel_dt,
  policy_params = policy_scalar,
  ref_date     = ref_date
)

cat("\n--- SCALAR result ---\n")
print(result_scalar$summary)


# -----------------------------------------------------------------------------
# 2. GROUP — min_age varies by paygrade
#    Grades A/B retire later (60), C/D earlier (55), E earliest (50).
#    Unmatched paygrades (NA or numeric codes) fall back to default = 55.
# -----------------------------------------------------------------------------
min_age_tbl <- data.table::data.table(
  paygrade = c("A", "B", "C", "D", "E"),
  min_age  = c(  60,   60,   55,   55,   50)
)

policy_group_age <- list(
  eligibility_type = "age_and_tenure",
  min_age = list(
    default      = 55,
    group_cols   = "paygrade",
    policy_table = min_age_tbl
  ),
  min_tenure   = 10,
  pension_type = "db",
  pension_params = list(
    accrual_rate    = 0.02,
    ref_wage_col    = "gross_salary_lcu",
    max_years       = 35,
    replacement_cap = 0.80
  )
)

result_group_age <- simulate_retirement(
  contract_dt   = contract_dt,
  personnel_dt  = personnel_dt,
  policy_params = policy_group_age,
  ref_date      = ref_date
)

cat("\n--- GROUP (min_age by paygrade) result ---\n")
print(result_group_age$summary)

# How many retirees per paygrade?
cat("\nRetirees by paygrade:\n")
print(
  result_group_age$retirees_dt[
    contract_dt[, .(personnel_id, paygrade)],
    on = c("personnel_id", "paygrade"),
    nomatch = 0L
  ][, .N, by = paygrade][order(paygrade)]
)


