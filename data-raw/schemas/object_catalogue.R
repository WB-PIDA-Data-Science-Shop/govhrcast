# What the main objects in a simulation hold, for the flow diagrams that
# build_flows.R draws. This is the one part of the map written by hand: the code
# cannot say what one row of a table is, and tables use the caller's column names.
#
# kind    "table" (unit = what one row is, columns = main columns, govhr dictionary
#         names), "list" (parts, which can nest) or "vector".
# Objects are matched by the name they have in the code. A list part with the same
# name as an entry here (contract_dt inside ret_result) takes that entry's unit.
#
# Rerun source("data-raw/schemas/build_code_map.R") after editing.

tbl <- function(unit, columns) list(kind = "table", unit = unit, columns = columns)
lst <- function(...) list(kind = "list", parts = list(...))

object_catalogue <- list(
  # ---- The state carried from year to year --------------------------------
  contract_dt = tbl("one row per contract",
    c("contract_id", "personnel_id", "est_id", "contract_type", "gross_salary_lcu",
      "base_salary_lcu", "start_date", "end_date", "paygrade")),
  personnel_dt = tbl("one row per person",
    c("personnel_id", "employment_status", "age", "personnel_tenure", "birth_date", "gender")),
  salary_scale_dt = tbl("one row per pay group",
    c("the group columns, e.g. est_id", "gross_salary_lcu")),
  pensioner_register = tbl("one row per pension contract (returned: per contract per year)",
    c("personnel_id", "contract_id", "pension_amount", "ref_date (returned only)")),
  allowance_dt = tbl("one row per contract, allowance type and year",
    c("contract_id", "personnel_id", "allowance_type", "amount", "period_date")),

  # ---- What the modules hand back ------------------------------------------
  retirees_dt = tbl("one row per retiree, on their main contract, pay summed over all their contracts",
    c("contract and group columns", "age", "tenure_years", "pension rule columns", "pension")),
  exits_dt = tbl("one row per person leaving", c("personnel_id", "gross_salary_lcu")),
  movers_dt = tbl("one row per person moving", c("personnel_id", "salary_before", "from_group", "to_group")),
  new_hire_contracts_dt = tbl("one row per new contract", c("the contract columns, for new hires")),
  new_hires_dt = tbl("one row per new hire", c("the personnel columns, for new hires")),
  baseline_matrix = tbl("one row per move between groups", c("from_group", "to_group", "movement_rate", "n_periods")),
  demand_dt = tbl("one row per move between groups", c("from_group", "to_group", "movement_rate", "current_stock", "n_movers")),
  summary = tbl("one row: this step's counts and costs", c("depends on the step")),
  comparison = tbl("one row per year", c("period_date", "wage bill start and end", "one column per effect", "headcounts")),
  summary_dt = tbl("one row per year", c("the same table as comparison")),

  # ---- Policies ---------------------------------------------------------------
  retirement_policy = lst(
    group_cols   = "the columns rates vary by, or NULL",
    policy_table = "table: hazard_rate by age_yr_int (x group)",
    defaults     = lst(eligibility_type = "e.g. hazard_rate", min_age = "mandatory age",
                       retire_strategy = "who goes first",
                       pension_type = "e.g. db", "accrual_rate, max_years, replacement_cap" = "the pension rule")),
  exit_policy = lst(
    group_cols   = "the columns rates vary by, or NULL",
    policy_table = "table: exit_rate by group",
    defaults     = lst(exit_rate = "a flat rate, or", exit_multiplier = "x the status-quo rate",
                       exit_strategy = "who goes first", active_types = "contract types that can exit")),
  movement_policy = lst(
    group_cols   = "the columns that make a group",
    policy_table = "table: movement_rate from_group -> to_group",
    defaults     = lst(movement_type = "promotion or transfer", movement_strategy = "who moves first",
                       salary_update_rule = "what happens to pay")),
  hiring_policy = lst(
    mode         = "status_quo, flow or stock",
    group_cols   = "the columns rates vary by",
    squorate_dt  = "table: hiring_rate by group (status quo)",
    rate_mult    = "x the status-quo rate",
    salary_scale = "table: pay for new hires, by group",
    new_hire_profile = "table: gender share, entry_age, contract_type by group",
    new_hire_profile_overall = "the same, across all groups"),
  allowance_policy = lst(
    group_cols   = "the group columns, with allowance_type",
    policy_table = "table: amount by group x allowance_type",
    defaults     = lst(amount = "for groups without a row")),
  policy_params = list(kind = "list", parts = list(), note = "the module's policy"),

  # ---- Settings that are not drawn: passed almost everywhere ----------------
  column_spec_args    = list(kind = "hidden"),
  output_control_args = list(kind = "hidden"),
  seed                = list(kind = "hidden")
)

# Functions left out of the diagrams: input checks and one-line helpers that
# would clutter every picture. Every file in validation.R is left out too.
flow_skip <- c("%||%", ".on_payroll", ".total_pay", ".save_rng_state", ".restore_rng_state",
               "expand_rate_vector", ".advance_period", ".report_groups_without_rate",
               "validate_date_format", ".latest_snapshot")
