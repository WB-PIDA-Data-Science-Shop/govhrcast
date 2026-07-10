###############################################################################################
## A SCRIPT TO SHOW WHAT WE NEED TO KNOW ABOUT THE PUBLIC SECTOR BEFORE RUNNING SIMULATIONS ###
###############################################################################################

# library(govhr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)

### This script will show what we need to understand about the public sector before we start to
### run simulations

#### the idea is that between govhr and govhrapp you are already able to compute a bunch of things
#### this should be more in-depth analysis previewing the data prior to simulation

###### lets starting with compensation

### first let's deflate the compensation variables to a specific year

bra_hrmis_contract <- as.data.table(bra_hrmis_contract)
bra_hrmis_personnel <- as.data.table(bra_hrmis_personnel)

comp_vars <- colnames(bra_hrmis_contract)[grepl("_lcu", colnames(bra_hrmis_contract))]
def_vars <- gsub(pattern = "_lcu", replacement = "_def", x = comp_vars) 

bra_hrmis_contract[, (def_vars) := lapply(.SD, 
                                         govhr::deflate_to_real, 
                                         ref_date = ref_date, 
                                         country_code = "BRA",
                                         base_year = year(max(bra_hrmis_contract$ref_date))),
                                         .SDcols = comp_vars]

# how have wages changed over time overall and at the organization level

bra_hrmis_contract[, year := year(ref_date)]

change_dt <- 
  rbind(
      govhr::compute_fastsummary(data = bra_hrmis_contract,
                             cols = c("gross_salary_def"),
                             fn = "mean",
                             groups = "ref_date") |>
      mutate(year = year(ref_date)) |>
      govhr::compute_fastchange(col = "value", 
                                date_col = "year") |> 
      mutate(indicator = "gross_salary_def"),
      govhr::compute_fastsummary(data = bra_hrmis_contract,
                                 cols = c("allowance_def"),
                                 fn = "mean",
                                 groups = "ref_date") |>
      mutate(year = year(ref_date)) |>
      govhr::compute_fastchange(col = "value", 
                                date_col = "year") |> 
      mutate(indicator = "allowance_def"))

### lets see the share of wage bill in each organization

wagebill_decomp_dt <- 
  left_join(x = govhr::compute_fastsummary(data = bra_hrmis_contract,
                             cols = c("gross_salary_def", "allowance_def"),
                             fns = "sum",
                             groups = c("est_id", "ref_date")) %>%
                setnames(., old = "value", new = "group_value"),
            y = govhr::compute_fastsummary(data = bra_hrmis_contract,
                             cols = c("gross_salary_def", "allowance_def"),
                             fns = "sum",
                             groups = c("ref_date")),
            by = c("ref_date", "indicator")) %>%
  .[, est_share := group_value / value]
  


plot_top_establishments <- function(data,
                                    indicator = "gross_salary_def_sum",
                                    n = 10,
                                    year = 2017) {
  ind <- indicator

 

  plot_dt <- data[
    get("indicator") == ind &
      year(ref_date) == year
  ][
    order(-est_share)
  ] |>
    head(n)

  ggplot(
    plot_dt,
    aes(
      x = reorder(est_id, est_share),
      y = est_share
    )
  ) +
    geom_col(fill = "#2C7FB8") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = NULL,
      y = "Share of wage bill",
      title = sprintf(
        "Top %d establishments (%s)",
        n,
        year
      )
    ) +
    theme_minimal(base_size = 12)
}

plot_top_establishments(data = wagebill_decomp_dt) ### for gross salary
plot_top_establishments(data = wagebill_decomp_dt, 
                        indicator = "allowance_def_sum")


### Pension cost - wage bill ratio
pension_rate_dt <- 
bra_hrmis_contract |>
  filter(contract_type_native == "PENSIONISTA") %>%
  govhr::compute_fastsummary(data = .,
                             cols = c("gross_salary_def", "allowance_def"),
                             fns = "sum",
                             groups = "ref_date") |>
  left_join(y = govhr::compute_fastsummary(data = bra_hrmis_contract,
                             cols = c("gross_salary_def", "allowance_def"),
                             fns = "sum",
                             groups = "ref_date") %>%
                setnames(., "value", "sum"),
            by = c("ref_date", "indicator")) %>%
  .[, pension_rate := value / sum]


### plot pension rate dt to see how it has changed over time
pension_rate_dt |>
  ggplot(aes(x = ref_date, y = pension_rate)) +
  geom_col(color = "#2C7FB8", size = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Pension cost / Wage bill",
    title = "Pension cost to wage bill ratio over time"
  ) +
  theme_minimal(base_size = 12)


### ----------------- STARTING WITH RETIREMENT -------------------- ###

### let us compare the salary of those who just retire to their last salary to get a sense
### for what might be saved by retiring people
retiree_ids <- 
  bra_hrmis_personnel |>
  dplyr::filter(employment_status == "pensioner") |>
  pull(personnel_id) |>
  unique()

# Tag retiree contracts with employment_status from personnel table
retiree_tagged <- bra_hrmis_contract |>
  filter(personnel_id %in% retiree_ids) |>
  left_join(
    bra_hrmis_personnel |> select(personnel_id, employment_status, ref_date),
    by = c("personnel_id", "ref_date")
  )

# Last active contract (non-pensioner) per person
last_active <- retiree_tagged |>
  filter(employment_status != "pensioner", !is.na(gross_salary_def)) |>
  slice_max(ref_date, by = personnel_id, n = 1, with_ties = FALSE) |>
  select(personnel_id, ref_date, gross_salary_def, allowance_def) |>
  mutate(status = "last_active")

# First pension contract per person
first_pension <- retiree_tagged |>
  filter(employment_status == "pensioner", !is.na(gross_salary_def)) |>
  slice_min(ref_date, by = personnel_id, n = 1, with_ties = FALSE) |>
  select(personnel_id, ref_date, gross_salary_def, allowance_def) |>
  mutate(status = "first_pension")

# add contract details
last_active <- 
  govhr::add_contract_to_event(event_dt = last_active,
                               contract_dt = bra_hrmis_contract,
                               keep_vars = c("est_id", "paygrade", "seniority", 
                                             "occupation_native")) |>
  setorder(personnel_id, ref_date, -gross_salary_def, allowance_def) %>%
  .[, .SD[1L], by = .(personnel_id, ref_date)]

first_pension <- 
  govhr::add_contract_to_event(event_dt = first_pension,
                               contract_dt = bra_hrmis_contract,
                               keep_vars = c("est_id", "paygrade", "seniority", 
                                             "occupation_native")) |>
  setorder(personnel_id, ref_date, -gross_salary_def, allowance_def) %>%
  .[, .SD[1L], by = .(personnel_id, ref_date)]


# --- Compute replacement rate ratio ---
lastfirst_dt <- bind_rows(last_active, first_pension)
active  <- lastfirst_dt[status == "last_active",  .(personnel_id, ref_date, last_salary  = gross_salary_def)]
pension <- lastfirst_dt[status == "first_pension", .(personnel_id, ref_date, first_pension = gross_salary_def)]

# Inner join: keeps only workers with both records in the same ref_date
ratio_dt <- merge(active, pension, by = "personnel_id")
ratio_dt[, replacement_rate := first_pension / last_salary]

# Drop non-finite values (zero salaries, NAs)
ratio_dt <- ratio_dt[is.finite(replacement_rate)]


# --- CDF plotting function ---
plot_replacement_cdf <- function(data, ref_date_filter = NULL) {
  
  dt <- copy(data)
  
  if (!is.null(ref_date_filter)) {
    dt <- dt[ref_date.y == as.Date(ref_date_filter)]
    if (nrow(dt) == 0) stop("No data found for the specified ref_date.")
    title_label <- paste0("Replacement Rate CDF — ", format(as.Date(ref_date_filter), "%B %Y"))
  } else {
    title_label <- "Replacement Rate CDF — All Periods"
  }
  
  ggplot(dt, aes(x = replacement_rate)) +
    stat_ecdf(geom = "step", color = "#2166ac", linewidth = 0.8) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "firebrick", linewidth = 0.7) +
    annotate("text", x = 1.02, y = 0.1, label = "Rate = 1",
             color = "firebrick", hjust = 0, size = 3.5) +
    scale_x_continuous(
      name   = "Replacement Rate (First Pension / Last Active Salary)",
      limits = c(0, quantile(dt$replacement_rate, 0.99))  # trims display outliers
    ) +
    scale_y_continuous(name = "Cumulative Proportion", labels = scales::percent) +
    labs(
      title    = title_label,
      subtitle = paste0("n = ", nrow(dt), " workers with matched active-to-pension transition"),
      caption  = "Dashed line marks a 1:1 replacement rate"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
}


# --- Usage ---

# All periods pooled
plot_replacement_cdf(ratio_dt)

# Single ref_date
plot_replacement_cdf(ratio_dt, ref_date_filter = "2014-09-01")

# Optional: faceted across all periods
ggplot(ratio_dt, aes(x = replacement_rate)) +
  stat_ecdf(geom = "step", color = "#2166ac", linewidth = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "firebrick") +
  scale_x_continuous(
    name   = "Replacement Rate (First Pension / Last Active Salary)",
    limits = c(0, quantile(ratio_dt$replacement_rate, 0.99))
  ) +
  scale_y_continuous(name = "Cumulative Proportion", labels = scales::percent) +
  facet_wrap(~ ref_date.y, labeller = label_both) +
  labs(title = "Replacement Rate CDF by Reference Period") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

### lets look at the age distribution of the public sector as well with facet
### wrap for each ref_date and add a vertical line for retirement age of 60 years old
bra_hrmis_personnel |>
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "#2C7FB8", color = "white") +
  labs(
    x = "Age (years)",
    y = "Count",
    title = "Age distribution of public sector personnel"
  ) +
  geom_vline(xintercept = 60, linetype = "dashed", color = "firebrick", size = 1) +
  facet_wrap(~ ref_date, labeller = label_both) +
  theme_minimal(base_size = 12)

### lets look at tenure as well
bra_hrmis_personnel |>
  ggplot(aes(x = personnel_tenure)) +
  geom_histogram(binwidth = 1, fill = "#2C7FB8", color = "white") +
  labs(
    x = "Tenure",
    y = "Count",
    title = "Tenure distribution of public sector personnel"
  ) + 
  facet_wrap(~ ref_date, labeller = label_both) +
  theme_minimal(base_size = 12)


### Lets look at the salary distribution by age and tenure to see if there are any patterns

## first include age and tenure into the contract module

all_dt <- bra_hrmis_personnel[bra_hrmis_contract, on = .(personnel_id, ref_date)]
  
### total salary distribution by integer age
all_dt[, age_int := floor(age)]

salaryage_dt <- 
  all_dt[employment_status != "pensioner", .(total_salary = sum(gross_salary_def, na.rm = TRUE)), by = .(age_int, ref_date)]

### plot salary distribution by age with red vertical line for retirement age of 55-60 years old

salaryage_dt |>
  ggplot(aes(x = age_int, y = total_salary)) +
  geom_col(fill = "#2C7FB8") +
  labs(
    x = "Age (years)",
    y = "Total Salary (deflated)",
    title = "Total salary distribution by age"
  ) +
  geom_vline(xintercept = 55, linetype = "dashed", color = "firebrick", size = 0.5) +
  facet_wrap(~ ref_date, labeller = label_both) +
  geom_vline(xintercept = 60, linetype = "dashed", color = "firebrick", size = 0.5) +
  facet_wrap(~ ref_date, labeller = label_both) +
  theme_minimal(base_size = 12)

### lets look at the total wage bill
salaryage_dt[, totalwagebill := sum(total_salary, na.rm = TRUE), by = "ref_date"]
salaryage_dt[, shares := total_salary / totalwagebill]

salaryage_dt |>
  ggplot(aes(x = age_int, y = shares)) +
  geom_col(fill = "#2C7FB8") +
  labs(
    x = "Age (years)",
    y = "Wage Bill Share",
    title = "Share of Wage Bill by Age"
  ) +
  geom_vline(xintercept = 55, linetype = "dashed", color = "firebrick", size = 0.5) +
  facet_wrap(~ ref_date, labeller = label_both) +
  geom_vline(xintercept = 60, linetype = "dashed", color = "firebrick", size = 0.5) +
  facet_wrap(~ ref_date, labeller = label_both) +
  theme_minimal(base_size = 12)


### -------------------- NON-RETIREMENT EXITS -------------------------- ###

### lets take a look at the non-retirement exits i.e. fires, resignings etc
### the goal is to look at the rates of exits in each period, ie that is 
### people we no longer observe in the data and how much the government saves
### by their exiting and we can do this by groups of interest

## compute exit rates for each year

exits_dt <- 
  govhr::detect_personnel_event(data = bra_hrmis_personnel,
                                id_col = "personnel_id",
                                event_type = "fire",
                                start_date = min(bra_hrmis_personnel$ref_date, na.rm = TRUE),
                                end_date = max(bra_hrmis_personnel$ref_date, na.rm = TRUE),
                                status_col = "employment_status") |>
  govhr::add_contract_to_event(contract_dt = bra_hrmis_contract, 
                               keep_vars = c("est_id", "paygrade", "seniority", 
                               "gross_salary_def", "allowance_def"))

### compute exit rates, exit savings for each year

### plot the total gross_salary_def by ref_date 
exits_dt |>
  group_by(ref_date) |>
  summarise(total_exit_salary = sum(gross_salary_def, na.rm = TRUE)) |>
  ggplot(aes(x = ref_date, y = total_exit_salary)) +
  geom_col(fill = "#2C7FB8") +
  labs(
    x = NULL,
    y = "Total Exit Salary (deflated)",
    title = "Total Exit Salary by Reference Date"
  ) +
  theme_minimal(base_size = 12)

### same plot but as a share of total wage bill with x-axis as factor ref_date and label rotated 45 degrees
exits_dt |>
  group_by(ref_date) |>
  summarise(total_exit_salary = sum(gross_salary_def, na.rm = TRUE)) |>
  left_join(salaryage_dt |> distinct(ref_date, totalwagebill), by = "ref_date") |>
  mutate(exit_share = total_exit_salary / totalwagebill) |>
  ggplot(aes(x = as.factor(year(ref_date)), y = exit_share)) + 
  geom_col(fill = "#2C7FB8") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Exit Salary Share of Total Wage Bill",
    title = "Exit Salary Share of Total Wage Bill by Reference Date"
  ) +
  theme_minimal(base_size = 12) 

### same plot but now in terms of the counts of exits by ref_date
exits_dt |>
  group_by(ref_date) |>
  summarise(total_exits = n()) |>
  ggplot(aes(x = as.factor(year(ref_date)), y = total_exits)) +
  geom_col(fill = "#2C7FB8") +
  labs(
    x = NULL,
    y = "Total Exits",
    title = "Total Exits by Reference Date"
  ) +
  theme_minimal(base_size = 12)

### same plot but now as a share of the total headcount by ref_date
exits_dt |>
  group_by(ref_date) |>
  summarise(total_exits = n()) |>
  left_join(bra_hrmis_personnel |> group_by(ref_date) |> summarise(total_headcount = n()), by = "ref_date") |>
  mutate(exit_share = total_exits / total_headcount) |>
  ggplot(aes(x = as.factor(year(ref_date)), y = exit_share)) +
  geom_col(fill = "#2C7FB8") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Exit Share of Total Headcount",
    title = "Exit Share of Total Headcount by Reference Date"
  ) +
  theme_minimal(base_size = 12)


### lets create a summary table total exits, exit rates (i.e. exits / total headcount), total exit salary savings from exit, 
### and exit salary savings as a share of total wage bill by est_id, paygrade and seniority

exits_summary_dt <- 
  exits_dt |>
  group_by(ref_date, est_id, paygrade, seniority) |>
  summarise(total_exits = n(),
            total_exit_salary = sum(gross_salary_def, na.rm = TRUE)) |>
  left_join(bra_hrmis_contract[, uniqueN(personnel_id), by = .(ref_date, est_id, paygrade, seniority)] %>%
            setnames(., old = "V1", new = "total_headcount"), 
            by = c("ref_date", "est_id", "paygrade", "seniority")) |>
  left_join(salaryage_dt |> distinct(ref_date, totalwagebill), by = "ref_date") |>
  mutate(exit_rate = total_exits / total_headcount,
         exit_salary_share = total_exit_salary / totalwagebill) |>
  ungroup()



### ------------------------ PROMOTIONS AND TRANSFERS -------------------------- ###

##### lets compute the promotion and transfer rates for each year and by est_id, paygrade and seniority

movement_dt <- estimate_movement_baseline(contract_dt = bra_hrmis_contract,
                                          group_cols = c("est_id", "paygrade")) 

### lets look at the costs of promotions and transfers in terms of the total gross_salary_def and allowance_def by ref_date

### figure out the movers i.e. those who have a change in paygrade or est_id between two consecutive ref_dates inside the
### bra_hrmis_contract dataset and compute the total gross_salary_def and allowance_def for those movers by ref_date

all_dt[, prev_paygrade := shift(paygrade, type = "lag"), by = personnel_id]
all_dt[, prev_est_id := shift(est_id, type = "lag"), by = personnel_id]
all_dt[, prev_ref_date := shift(ref_date, type = "lag"), by = personnel_id]
all_dt[, prev_gross_salary_def := shift(gross_salary_def, type = "lag"), by = personnel_id]
all_dt[, prev_allowance_def := shift(allowance_def, type = "lag"), by = personnel_id]

### keep only the movers i.e. those who have a change in paygrade or est_id between two consecutive ref_dates
movers_dt <- all_dt[!is.na(prev_paygrade) & !is.na(prev_est_id) & 
                      (paygrade != prev_paygrade | est_id != prev_est_id)]

### drop the pensioners
movers_dt <- movers_dt[employment_status != "pensioner"]

movers_dt[, salary_change := gross_salary_def - prev_gross_salary_def]
movers_dt[, allowance_change := allowance_def - prev_allowance_def]
movers_dt[, salary_changerate := salary_change / prev_gross_salary_def]
movers_dt[, allowance_changerate := allowance_change / prev_allowance_def]



### compute the total movers, total mover salary and total mover allowance by ref_date and 
### compute the share of mover salary and mover allowance as a share of total wage bill by ref_date
### also compute the change in salary and allowance for each mover between two consecutive ref_dates

summary_movers_dt <- 
  movers_dt[, .(total_movers = .N,
            total_mover_salary = sum(gross_salary_def, na.rm = TRUE),
            total_mover_allowance = sum(allowance_def, na.rm = TRUE),
            total_mover_salary_change = sum(salary_change, na.rm = TRUE),
            total_mover_allowance_change = sum(allowance_change, na.rm = TRUE),
            total_mover_salary_changerate = sum(salary_changerate, na.rm = TRUE),
            total_mover_allowance_changerate = sum(allowance_changerate, na.rm = TRUE)), 
        by = ref_date] |>
  left_join(salaryage_dt |> distinct(ref_date, totalwagebill), by = "ref_date") %>%
  .[, `:=`(
    mover_salary_share    = total_mover_salary / totalwagebill,
    mover_allowance_share = total_mover_allowance / totalwagebill
  )]
  


### ---------------------------------------- HIRING --------------------------------------------- ###

### lets take a look at the hiring rates and costs for each year and by est_id, paygrade and seniority

### lets see the time distribution of hiring annually using the first_employment_date in bra_hrmis_personnel
### that is when during the year did the government hire the most people and what is the distribution of hiring by month
### but we only want to do it for those between 2007 and 2018 and this needs to be faceted by year
  
bra_hrmis_personnel |>
  filter(!is.na(first_employment_date)) |>
  mutate(hire_year = lubridate::year(first_employment_date),
         hire_month = lubridate::month(first_employment_date, label = TRUE, abbr = TRUE)) |>
  filter(hire_year >= 2007 & hire_year <= 2018) |>
  group_by(hire_year, hire_month) |>
  summarise(total_hires = n()) |>
  ggplot(aes(x = hire_month, y = total_hires, fill = as.factor(hire_year))) +
  geom_col(position = "dodge") +
  labs(
    x = "Month",
    y = "Total Hires",
    title = "Hiring Distribution by Month and Year"
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_brewer(palette = "Set1", name = "Year") + 
  facet_wrap(~ hire_year, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### lets compute the hiring costs for each year and by est_id, paygrade and seniority and share of the total wage bill
### by year
all_dt[, hire_year := lubridate::year(first_employment_date)] 

hire_dt <- all_dt[hire_year == lubridate::year(ref_date),]

### overall hiring costs as a share of total wage bill by ref_date
hire_dt |>
  group_by(ref_date) |>
  summarise(total_hire_salary = sum(gross_salary_def, na.rm = TRUE),
            total_hire_allowance = sum(allowance_def, na.rm = TRUE)) |>
  left_join(salaryage_dt |> distinct(ref_date, totalwagebill), by = "ref_date") |>
  mutate(hire_salary_share = total_hire_salary / totalwagebill,
         hire_allowance_share = total_hire_allowance / totalwagebill) |>
  ggplot(aes(x = ref_date)) +
  geom_col(aes(y = hire_salary_share), fill = "#2C7FB8", alpha = 0.7) +
  geom_col(aes(y = hire_allowance_share), fill = "#D95F02", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Share of Total Wage Bill",
    title = "Hiring Costs as Share of Total Wage Bill"
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c("#2C7FB8", "#D95F02"), 
                    name = "Cost Type", 
                    labels = c("Salary", "Allowance"))

### lets compute the hiring costs for each year and by est_id, paygrade and seniority and share of the total wage bill
hire_summary_dt <-
  hire_dt |>
  group_by(ref_date, est_id, paygrade, seniority) |>
  summarise(total_hires = n(),
            total_hire_salary = sum(gross_salary_def, na.rm = TRUE),
            total_hire_allowance = sum(allowance_def, na.rm = TRUE)) |>
  left_join(salaryage_dt |> distinct(ref_date, totalwagebill), by = "ref_date") |>
  mutate(hire_salary_share = total_hire_salary / totalwagebill,
         hire_allowance_share = total_hire_allowance / totalwagebill) |>
  ungroup()