##############################################################################
## 00-pre-simchecks.R
## Botswana HRMIS — pre-simulation diagnostics
## Purpose: Establish all simulation parameters from observed data before
##          writing the simulation runs in 01-simulation-runs.R
##############################################################################

library(data.table)
library(govhrcast)
library(qs2)

dir <- paste0(
  "C:/Users/wb559885/OneDrive - WBG/",
  "Documents - datascienceprojectsforworldbankprojects/DICE/GHRMD/BWA/",
  "data-clean/micro/simulation"
)

contract_dt  <- qs2::qs_read(paste0(dir, "/bwa_contract.qs2"))
personnel_dt <- qs2::qs_read(paste0(dir, "/bwa_personnel.qs2"))



# ----------------------------------------------------------------------------
# 0. Add end_date column (absent in BWA panel — activity encoded in
#    contract_type_code).  Required by get_active_contracts() internals.
# ----------------------------------------------------------------------------
contract_active <- contract_dt[contract_type_code == "active"]
contract_active[, end_date := as.Date(NA)]

panel_start <- min(contract_active$ref_date)
panel_end   <- max(contract_active$ref_date)

cat("\n=== Panel date range ===\n")
cat("Start:", format(panel_start), " End:", format(panel_end),
    " Snapshots:", uniqueN(contract_active$ref_date), "\n")

# ----------------------------------------------------------------------------
# 1. Contract type landscape
# ----------------------------------------------------------------------------
cat("\n=== 1. Contract type frequency ===\n")
print(contract_dt[, .N, by = contract_type_code][order(-N)])

# ----------------------------------------------------------------------------
# 2. Year-on-year salary growth (same contract, same month, consecutive years)
# ----------------------------------------------------------------------------
cat("\n=== 2. Year-on-year salary growth (gross & base) ===\n")

sg <- contract_active[!is.na(gross_salary_lcu),
  .(personnel_id, contract_id, ref_date, gross_salary_lcu, base_salary_lcu,
    yr = data.table::year(ref_date), mo = data.table::month(ref_date))
]
setorder(sg, contract_id, mo, yr)
sg[, gross_lag := shift(gross_salary_lcu), by = .(contract_id, mo)]
sg[, base_lag  := shift(base_salary_lcu),  by = .(contract_id, mo)]
sg[, yr_lag    := shift(yr),               by = .(contract_id, mo)]
sg <- sg[!is.na(gross_lag) & (yr - yr_lag) == 1L]
sg[, gross_growth := gross_salary_lcu / gross_lag - 1]
sg[, base_growth  := base_salary_lcu  / base_lag  - 1]

salary_growth_by_yr <- sg[, .(
  n_pairs      = .N,
  median_gross = round(median(gross_growth, na.rm = TRUE), 4),
  p25_gross    = round(quantile(gross_growth, 0.25, na.rm = TRUE), 4),
  p75_gross    = round(quantile(gross_growth, 0.75, na.rm = TRUE), 4),
  median_base  = round(median(base_growth,  na.rm = TRUE), 4)
), by = yr][order(yr)]
print(salary_growth_by_yr)

# Recommended forward rate: use most recent full year as anchor
SALARY_GROWTH_RATE <- salary_growth_by_yr[yr == max(yr) - 1L, median_gross]
cat("\nRecommended salary_growth_rate (most recent full year):", SALARY_GROWTH_RATE, "\n")

# ----------------------------------------------------------------------------
# 3. Completeness of key person-level columns
# ----------------------------------------------------------------------------
cat("\n=== 3. Column completeness ===\n")
print(personnel_dt[, .(
  n                          = .N,
  pct_first_employment_na    = round(mean(is.na(first_employment_date)) * 100, 1),
  pct_birth_date_na          = round(mean(is.na(birth_date)) * 100, 1),
  pct_tenure_col_na          = round(mean(is.na(tenure)) * 100, 1)
)])

print(contract_active[, .(
  n                   = .N,
  pct_start_date_na   = round(mean(is.na(start_date)) * 100, 1)
)])

# ----------------------------------------------------------------------------
# 4. Retirement age — last-appearance analysis
# ----------------------------------------------------------------------------
cat("\n=== 4. Retirement age (last-appearance analysis) ===\n")

person_last  <- contract_active[, .(last_date = max(ref_date)), by = personnel_id]
exits_p      <- person_last[last_date < panel_end]
exits_p      <- personnel_dt[exits_p, on = .(personnel_id, ref_date = last_date), nomatch = 0L]

cat("\nExit age summary (all exits):\n")
print(exits_p[!is.na(age), .(
  n        = .N,
  mean_age = round(mean(age), 1),
  p25      = quantile(age, 0.25),
  p50      = median(age),
  p75      = quantile(age, 0.75),
  p90      = quantile(age, 0.90),
  p95      = quantile(age, 0.95)
)])

cat("\nExit counts near mandatory retirement age (57-65):\n")
print(exits_p[!is.na(age) & age %in% 57:65, .N, by = age][order(age)])

# Strong spike at 60 → mandatory retirement age = 60
MIN_RETIREMENT_AGE <- 60L
cat("\nRecommended min_age:", MIN_RETIREMENT_AGE, "\n")
cat("Note: start_date is 36.7% missing — do NOT use min_tenure gate.\n")

# ----------------------------------------------------------------------------
# 5. Aggregate annual exit rate (non-retirement attrition)
#    Uses estimate_historical_exit_rates() from govhrcast
# ----------------------------------------------------------------------------
cat("\n=== 5. Aggregate annual exit rate ===\n")

exit_rates <- estimate_historical_exit_rates(
  panel_contract_dt  = contract_active,
  panel_personnel_dt = personnel_dt,
  group_cols         = NULL,
  freq               = "year"
)
print(exit_rates)
EXIT_RATE <- exit_rates$exit_rate
cat("\nAggregate exit_rate:", round(EXIT_RATE, 4), "\n")

# ----------------------------------------------------------------------------
# 6. Hiring rate — using first_employment_date (avoids left-censoring bias)
# ----------------------------------------------------------------------------
cat("\n=== 6. Annual hiring rate from first_employment_date ===\n")

persons_unique <- unique(personnel_dt[status == "active",
  .(personnel_id, first_employment_date)])

hires_fed <- persons_unique[
  !is.na(first_employment_date) &
  first_employment_date >= panel_start &
  first_employment_date <= panel_end,
  .(n_hires = .N),
  by = .(yr = data.table::year(first_employment_date))
]

stock_by_yr <- personnel_dt[status == "active",
  .(n_stock = uniqueN(personnel_id)),
  by = .(yr = data.table::year(ref_date))
]

hire_rate_tbl <- merge(stock_by_yr, hires_fed, by = "yr", all.x = TRUE)[
  , hire_rate := round(n_hires / n_stock, 4)][order(yr)]
print(hire_rate_tbl)

# Monthly seasonality of hiring
cat("\nMonthly hiring counts (first_employment_date):\n")
print(persons_unique[
  !is.na(first_employment_date) &
  first_employment_date >= panel_start &
  first_employment_date <= panel_end,
  .(n_hires = .N),
  by = .(mo = data.table::month(first_employment_date))
][order(mo)])

# ----------------------------------------------------------------------------
# 7. Summary of recommended simulation parameters
# ----------------------------------------------------------------------------
cat("\n")
cat("===========================================================\n")
cat("RECOMMENDED SIMULATION PARAMETERS\n")
cat("===========================================================\n")
cat(sprintf("salary_growth_rate : %.4f  (most recent full-year median)\n", SALARY_GROWTH_RATE))
cat(sprintf("min_retirement_age : %d      (mandatory; start_date too sparse for tenure gate)\n", MIN_RETIREMENT_AGE))
cat(sprintf("exit_rate          : %.4f  (aggregate non-retirement attrition)\n", EXIT_RATE))
cat("hire_date_col      : \"first_employment_date\"\n")
cat("hiring_policy$mode : \"status_quo\"  (replay historical rates per month)\n")
cat("pension_type       : \"flat\", flat_amount = 0  (wage-bill focus; no pension formula)\n")
cat("n_periods          : 5   (5 future years of the same calendar month)\n")
cat("period_unit        : \"year\"\n")
cat("===========================================================\n")
