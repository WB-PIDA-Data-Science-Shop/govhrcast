##############################################################################
## 01-simulation-runs.R
## Botswana HRMIS — status-quo simulation (month-stratified, 5-year horizon)
##
## Strategy: run 12 independent simulate_horizon() calls — one per calendar
## month — each using only same-month historical slices to estimate rates
## and the most recent same-month snapshot as the starting state.
## Runs are executed sequentially via purrr::map.
## Output: assembled data.table of 12 x 5 = 60 projected rows.
##############################################################################

library(data.table)
# library(govhrcast)
library(qs2)
library(purrr)

# ----------------------------------------------------------------------------
# 0. Load data
# ----------------------------------------------------------------------------
dir <- paste0(
  "C:/Users/wb559885/OneDrive - WBG/",
  "Documents - datascienceprojectsforworldbankprojects/DICE/GHRMD/BWA/",
  "data-clean/micro/simulation"
)

contract_dt  <- qs2::qs_read(paste0(dir, "/bwa_contract.qs2"))
personnel_dt <- qs2::qs_read(paste0(dir, "/bwa_personnel.qs2"))

# ----------------------------------------------------------------------------
# 1. Pre-processing
#    - Add end_date (absent in BWA panel; required by get_active_contracts)
#    - Recompute tenure_years from first_employment_date (more complete than
#      start_date which is 36.7% missing)
#    - Keep only active contracts/personnel
# ----------------------------------------------------------------------------
contract_active <- contract_dt[contract_type_code == "active"]
contract_active[, end_date := as.Date(NA)]

### drop salaries from the pensioner rows
contract_dt[contract_type_code == "pensioner",
            `:=`(base_salary_lcu = NA_real_, 
                 gross_salary_lcu = NA_real_)]
# ----------------------------------------------------------------------------
# 2. Simulation parameters (from 00-pre-simchecks.R)
# ----------------------------------------------------------------------------
SALARY_GROWTH_RATE <- 0.0488   # most recent full-year median (2024)
MIN_RETIREMENT_AGE <- 60L
EXIT_RATE          <- 0.0076   # aggregate annual non-retirement attrition
N_PERIODS          <- 5L
HIRE_DATE_COL      <- "first_employment_date"

# Salary scale is computed per month inside run_month_sim using each month's
# most recent snapshot, so there is no single global salary_scale object here.

# ----------------------------------------------------------------------------
# 3. Helper: run one month's simulation
# ----------------------------------------------------------------------------
run_month_sim <- function(mo, contract_active, personnel_dt,
                          salary_growth_rate,
                          min_retirement_age, exit_rate,
                          n_periods, hire_date_col) {

  # --- Subset panel to this calendar month only ---
  ct_mo <- contract_active[data.table::month(ref_date) == mo]
  pt_mo <- personnel_dt[data.table::month(ref_date) == mo]

  if (nrow(ct_mo) == 0L || nrow(pt_mo) == 0L) {
    warning("No data for month ", mo, " — skipping.")
    return(NULL)
  }

  # Starting snapshot: most recent same-month observation
  base_date <- max(ct_mo$ref_date)
  base_yr   <- data.table::year(base_date)

  # Salary scale: median gross salary per est_id at this month's latest snapshot
  salary_scale <- ct_mo[ref_date == base_date,
    .(gross_salary_lcu = median(gross_salary_lcu, na.rm = TRUE)),
    by = c("est_id", "paygrade")
  ]

  # Policy objects -----------------------------------------------------------

  retirement_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(
      eligibility_type = "age_only",
      min_age          = min_retirement_age,
      pension_type     = "flat",
      flat_amount      = 0,
      active_types     = "active"
    )
  )

  exit_policy <- list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(
      exit_rate     = exit_rate,
      exit_strategy = "random",
      active_types  = "active",
      exited_type   = "inactive"
    )
  )

  hiring_policy <- list(
    mode         = "flow",
    group_cols   = c("est_id", "paygrade"),
    salary_scale = salary_scale,
    rate_mult    = 1
    # panel_contract_dt and panel_personnel_dt injected automatically
    # by simulate_horizon when mode = "status_quo"
  )

  # Run simulation -----------------------------------------------------------
  result <- tryCatch(
    simulate_horizon(
      contract_dt        = ct_mo,
      personnel_dt       = pt_mo,
      salary_scale_dt    = salary_scale,
      n_periods          = n_periods,
      retirement_policy  = retirement_policy,
      exit_policy        = exit_policy,
      hiring_policy      = hiring_policy,
      salary_growth_rate = salary_growth_rate,
      ref_date           = base_date,
      period_unit        = "year",
      birth_date_col     = "birth_date",
      age_col            = "age",
      tenure_col         = "tenure",
      hire_date_col      = hire_date_col,
      scenario_name      = paste0("month_", sprintf("%02d", mo))
    ),
    error = function(e) {
      warning("Month ", mo, " failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result)) return(NULL)

  # Tag the comparison output with month metadata
  comp <- data.table::copy(result$comparison)
  comp[, calendar_month := mo]
  comp[, base_year      := base_yr]
  comp[, projected_year := base_yr + seq_len(n_periods)]
  comp
}

# ----------------------------------------------------------------------------
# 4. Sequential execution — purrr::map (avoids worker package bootstrap issue
#    when govhrcast is loaded via devtools::load_all() rather than installed)
# ----------------------------------------------------------------------------
cat("\nRunning 12 month simulations sequentially...\n")

month_results <- purrr::map(
  .x = 1:12,
  .f = run_month_sim,
  contract_active    = contract_active,
  personnel_dt       = personnel_dt,
  salary_growth_rate = SALARY_GROWTH_RATE,
  min_retirement_age = MIN_RETIREMENT_AGE,
  exit_rate          = EXIT_RATE,
  n_periods          = N_PERIODS,
  hire_date_col      = HIRE_DATE_COL
)

# ----------------------------------------------------------------------------
# 5. Assemble results
# ----------------------------------------------------------------------------
failed_months <- which(sapply(month_results, is.null))
if (length(failed_months) > 0L)
  warning("Months failed and excluded: ", paste(failed_months, collapse = ", "))

simulation_results <- data.table::rbindlist(
  Filter(Negate(is.null), month_results),
  use.names = TRUE,
  fill      = TRUE
)

setorder(simulation_results, calendar_month, period_date)

cat(sprintf("\nAssembled %d projection rows across %d months.\n",
            nrow(simulation_results),
            uniqueN(simulation_results$calendar_month)))

# ----------------------------------------------------------------------------
# 6. Quick sanity checks
# ----------------------------------------------------------------------------
cat("\n=== Headcount by calendar month and projected year ===\n")
print(simulation_results[, .(
  n_headcount_start = first(n_headcount_start),
  n_headcount_end   = last(n_headcount_end)
), by = .(calendar_month, projected_year)][order(calendar_month, projected_year)])

cat("\n=== Wage bill trend (mean across months) by projected year ===\n")
print(simulation_results[, .(
  mean_wage_bill_end = round(mean(wage_bill_end,   na.rm = TRUE), 0),
  mean_n_headcount   = round(mean(n_headcount_end, na.rm = TRUE), 1),
  mean_n_exits       = round(mean(n_exits,         na.rm = TRUE), 1),
  mean_n_hires       = round(mean(n_hires,         na.rm = TRUE), 1)
), by = projected_year][order(projected_year)])


#### lets plot the wage bill overtime including both actual and simulated data
# Historical: annual wage bill + headcount from the contract panel
# Use Jan snapshot each year as the annual anchor (most complete coverage)
hist_dt <- contract_dt[contract_type_code == "active",
  .(
    wage_bill  = sum(gross_salary_lcu, na.rm = TRUE),
    headcount  = uniqueN(personnel_id)
  ),
  by = ref_date
][order(ref_date)]

hist_dt[, ref_year := data.table::year(ref_date)]

# --- Simulated: period_date is period START; wage_bill_end is the END value.
#     Plot at period_date + 1 year so simulated points appear after actuals. ---
sim_dt <- simulation_results[, .(
  ref_date  = period_date + 365L,
  wage_bill = wage_bill_start,
  headcount = n_headcount_start
)][order(ref_date)]

# --- Combine ---
hist_plot <- hist_dt[, .(ref_date, wage_bill, headcount, type = "Actual")]
sim_plot  <- sim_dt[,  .(ref_date, wage_bill, headcount, type = "Simulated")]
plot_dt   <- rbind(hist_plot, sim_plot)

wb_scale  <- 1e9
hc_scale  <- 1e3

p_wb <- ggplot(plot_dt, aes(x = ref_date, y = wage_bill / wb_scale,
                             linetype = type, colour = type)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2025-09-01"), linetype = "dotted",
             colour = "grey50", linewidth = 0.5) +
  annotate("text", x = as.Date("2025-09-01"), y = Inf,
           label = " last actual", hjust = 0, vjust = 1.5,
           size = 3, colour = "grey40") +
  scale_linetype_manual(values = c("Actual" = "solid", "Simulated" = "dashed")) +
  scale_colour_manual(values  = c("Actual" = "#1f77b4", "Simulated" = "#ff7f0e")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Botswana — Wage Bill: Actual vs Simulated (status quo)",
       x = NULL, y = "Gross wage bill (BWP billion)",
       linetype = NULL, colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

p_hc <- ggplot(plot_dt, aes(x = ref_date, y = headcount / hc_scale,
                             linetype = type, colour = type)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2025-09-01"), linetype = "dotted",
             colour = "grey50", linewidth = 0.5) +
  scale_linetype_manual(values = c("Actual" = "solid", "Simulated" = "dashed")) +
  scale_colour_manual(values  = c("Actual" = "#1f77b4", "Simulated" = "#ff7f0e")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Botswana — Active Headcount: Actual vs Simulated (status quo)",
       x = NULL, y = "Active employees (thousands)",
       linetype = NULL, colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))



# ----------------------------------------------------------------------------
# 7. Save
# ----------------------------------------------------------------------------
qs2::qs_save(simulation_results, "spielplatz/botswana/results/simulation_results.qs2")
