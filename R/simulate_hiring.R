#' Simulate Hiring Module
#'
#' @description
#' Main user-facing function for simulating hiring and workforce adjustments.
#' Orchestrates the entire hiring workflow: demand estimation, state updates,
#' and summary reporting. Supports three policy modes: flow-based (replacement of
#' the people who left), stock-based (target levels) and status quo (historical
#' rates).
#'
#' @import data.table
#'
#' @param contract_dt data.table. Contract data in govhr harmonized format
#' @param personnel_dt data.table. Personnel data in govhr harmonized format
#' @param policy_params List. Hiring policy parameters including:
#'   \describe{
#'     \item{mode}{Character. One of: "flow", "stock", "status_quo"}
#'     \item{group_cols}{Character vector. Grouping columns (e.g., c("department", "grade")). NULL for overall.}
#'     \item{replacement_rate}{Numeric scalar or data.table. For "flow" mode: 1 replaces everyone who left.}
#'     \item{stock_targets}{data.table. For "stock" mode. Must contain group_cols + target_stock. A group with
#'       no target stays as it is; a target for a group with no staff yet is hired up to.}
#'     \item{salary_scale}{data.table. Salary scale with group_cols + a numeric
#'       column with the same name as salary_col. With no group_cols, one row.}
#'     \item{removal_strategy}{Character. For downsizing. Options: "last_hired_first", "random" (default: "last_hired_first")}
#'     \item{defaults}{List. Only for flow mode with no \code{leavers_dt}: the rule that says who
#'       retires, e.g. \code{list(eligibility_type = "age_and_tenure", min_age = 60, min_tenure = 20)}.
#'       See [simulate_retirement()].}
#'     \item{new_hire_profile}{data.table. Optional. What new hires start with: entry age, contract
#'       type and gender shares, by group, from [estimate_new_hire_profile()] (edit it to set your own).
#'       [simulate_horizon()] estimates this for you from the panel. Without one, hires here are
#'       \code{"permanent"} with a blank age and gender, and tenure 0.}
#'     \item{new_hire_profile_overall}{data.table. Optional. The same with no groups, used for a group
#'       with no rows in \code{new_hire_profile}.}
#'   }
#' @param leavers_dt data.table or \code{NULL}. For flow mode: the people to replace, one row per
#'   person, with the person ID column, e.g. \code{retirees_dt} from [simulate_retirement()]. They are
#'   replaced in the group of their main contract. An empty table means nobody left, so nobody is hired.
#'   \code{NULL} (default) works out retirees from the rule in \code{policy_params$defaults}.
#' @param ref_date Date. Reference date for hiring simulation
#' @param column_spec_args A named list saying what your columns are called,
#'   so the function can read your data as it is. List only the names that
#'   differ from the **govhr** harmonized defaults; for example
#'   `list(personnel_id_col = "pid", salary_col = "bruto")`. Your names are used
#'   throughout, so the returned tables keep them, and you write the same names
#'   in `group_cols`, `stock_targets`, `replacement_rate` and `salary_scale`.
#'   The same list can be passed to every **govhrcast** function; keys a function
#'   does not use are ignored, and an unknown key is an error.
#'
#'   Keys used here (default in brackets):
#'
#'   * `ref_date_col` (`"ref_date"`): snapshot date of a panel. When the contract
#'     table holds several snapshots, the one nearest (but not later than)
#'     `ref_date` is used.
#'   * `personnel_id_col` (`"personnel_id"`): person identifier, in both tables.
#'   * `contract_id_col` (`"contract_id"`): contract identifier, generated for
#'     new contracts.
#'   * `start_date_col`, `end_date_col` (`"start_date"`, `"end_date"`): contract
#'     dates, used to find active contracts. New hires start on `ref_date` with
#'     an open end date; contracts ended by downsizing get `ref_date`.
#'   * `salary_col` (`"gross_salary_lcu"`): pay column. Picks each person's main
#'     contract and gives the pay saved by downsizing.
#'   * `contract_type_col` (`"contract_type"`): new hires get the type in the new
#'     hire profile, or `"permanent"` without one; contracts ended by downsizing
#'     become `"inactive"`.
#'   * `status_col` (`"employment_status"`): only `"active"` people count towards
#'     headcount. New hires are `"active"`; people let go become `"inactive"`.
#'   * `age_col` (`"age"`), `birth_date_col` (`"birth_date"`), `tenure_col`
#'     (`"personnel_tenure"`), `gender_col` (`"gender"`) and `hire_date_col`
#'     (`NULL`): what new hires start with, in the columns `personnel_dt` has. In
#'     flow mode with no `leavers_dt`, the first three also work out retirees, as
#'     in [simulate_retirement()].
#'
#' @return List containing:
#'   \describe{
#'     \item{summary}{data.table with hiring summary statistics, including
#'       \code{n_downsized} (people let go) and \code{downsizing_savings} (their
#'       pay, a positive number)}
#'     \item{contract_dt}{Updated contract data with new hires}
#'     \item{personnel_dt}{Updated personnel data with new hires}
#'     \item{adjustment_dt}{data.table of adjustments by group}
#'     \item{new_hires_dt}{data.table of new hire personnel records (empty if downsizing only)}
#'     \item{new_hire_contracts_dt}{data.table of new hire contract records with salary column populated
#'       (empty if no new hires). Used by \code{compute_hiring_effect()}.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(govhr)
#'
#' # Load example data
#' contract_dt <- as.data.table(govhr::bra_hrmis_contract)
#' personnel_dt <- as.data.table(govhr::bra_hrmis_personnel)
#'
#' # Example 1: Flow-based hiring (replacement)
#' policy_params_flow <- list(
#'   mode = "flow",
#'   group_cols = c("department", "paygrade"),
#'   replacement_rate = 1.0,
#'   defaults = list(
#'     eligibility_type = "age_and_tenure",
#'     min_age = 60,
#'     min_tenure = 20
#'   ),
#'   salary_scale = data.table(
#'     department = c("HR", "IT"),
#'     paygrade = c("G5", "G6"),
#'     gross_salary_lcu = c(50000, 60000)
#'   )
#' )
#'
#' results_flow <- simulate_hiring(
#'   contract_dt = contract_dt,
#'   personnel_dt = personnel_dt,
#'   policy_params = policy_params_flow,
#'   ref_date = as.Date("2025-01-01")
#' )
#'
#' # Example 2: Stock-based hiring (target levels)
#' policy_params_stock <- list(
#'   mode = "stock",
#'   group_cols = c("department"),
#'   stock_targets = data.table(
#'     department = c("HR", "IT", "Finance"),
#'     target_stock = c(100, 150, 80)
#'   ),
#'   salary_scale = data.table(
#'     department = c("HR", "IT", "Finance"),
#'     gross_salary_lcu = c(50000, 60000, 55000)
#'   )
#' )
#'
#' results_stock <- simulate_hiring(
#'   contract_dt = contract_dt,
#'   personnel_dt = personnel_dt,
#'   policy_params = policy_params_stock,
#'   ref_date = as.Date("2025-01-01")
#' )
#'
#' # Example 3: Using retirement module output
#' retirement_results <- simulate_retirement(
#'   contract_dt = contract_dt,
#'   personnel_dt = personnel_dt,
#'   policy_params = list(
#'     eligibility_type = "age_and_tenure",
#'     min_age = 60,
#'     min_tenure = 20,
#'     pension_type = "db",
#'     pension_params = list(accrual_rate = 0.02)
#'   ),
#'   ref_date = as.Date("2025-01-01")
#' )
#'
#' hiring_results <- simulate_hiring(
#'   contract_dt = retirement_results$contract_dt,
#'   personnel_dt = retirement_results$personnel_dt,
#'   policy_params = policy_params_flow,
#'   leavers_dt = retirement_results$retirees_dt,
#'   ref_date = as.Date("2025-01-01")
#' )
#'
#' # View results
#' results_flow$summary
#' results_flow$new_hires_dt
#' }
#'
#' @export
simulate_hiring <- function(contract_dt,
                           personnel_dt,
                           policy_params,
                           leavers_dt = NULL,
                           ref_date,
                           column_spec_args = list()) {

  # ========================================
  # 1. Input Validation
  # ========================================
  # The caller's column names, with defaults for any not given; the same
  # list is passed to every step below
  cols <- .column_spec(column_spec_args)
  check_hiring_inputs(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date,
    column_spec_args = cols
  )
  ref_date_col <- cols$ref_date_col

  # Convert to data.table and create working copies
  # We copy here to avoid modifying the user's input data
  if (!data.table::is.data.table(contract_dt)) {
    contract_dt <- data.table::as.data.table(contract_dt)
  } else {
    contract_dt <- data.table::copy(contract_dt)
  }

  if (!data.table::is.data.table(personnel_dt)) {
    personnel_dt <- data.table::as.data.table(personnel_dt)
  } else {
    personnel_dt <- data.table::copy(personnel_dt)
  }

  # ========================================
  # 2. Select Nearest Reference Date
  # ========================================
  # Find the reference date in the data closest to (but not after) the specified ref_date
  selected_ref_date <- NULL  # Track for later use
  if (ref_date_col %in% names(contract_dt)) {
    selected_ref_date <- select_nearest_ref_date(contract_dt[[ref_date_col]], ref_date)

    # Subset both datasets to the selected reference date
    contract_dt <- contract_dt[get(ref_date_col) == selected_ref_date]
    if (ref_date_col %in% names(personnel_dt)) {
      personnel_dt <- personnel_dt[get(ref_date_col) == selected_ref_date]
    }
  }

  # ========================================
  # 3. Estimate Hiring Demand
  # ========================================
  # Pure function - no state modification
  adjustment_dt <- estimate_hiring_demand(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    leavers_dt = leavers_dt,
    ref_date = ref_date,
    column_spec_args = cols
  )

  # Rename total_hires to net_change for clarity in update function
  data.table::setnames(adjustment_dt, "total_hires", "net_change", skip_absent = TRUE)

  # Handle case of no adjustments needed
  if (nrow(adjustment_dt) == 0 || all(adjustment_dt$net_change == 0)) {
    # Compute current headcount
    current_headcount <- compute_current_stock(
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      ref_date = ref_date,
      group_cols = NULL,  # Overall
      column_spec_args = cols
    )

    summary_tbl <- data.table::data.table(
      n_new_hires = 0L,
      net_headcount_change = 0L,
      total_headcount = current_headcount$current_stock,
      total_new_salary_cost = 0,
      n_downsized = 0L,
      downsizing_savings = 0
    )

    return(list(
      summary = summary_tbl,
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      adjustment_dt = adjustment_dt,
      new_hires_dt = data.table::data.table(),
      new_hire_contracts_dt = data.table::data.table()
    ))
  }

  # ========================================
  # 4. Update State with Adjustments
  # ========================================
  # This is the ONLY place where state modification occurs
  update_results <- update_state_with_adjustment(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    adjustment_dt = adjustment_dt,
    policy_params = policy_params,
    ref_date = ref_date,
    column_spec_args = cols
  )

  # Extract updated data.tables and new hire information
  contract_dt <- update_results$contract_dt
  personnel_dt <- update_results$personnel_dt
  new_hires_dt <- update_results$new_personnel_dt
  new_contracts_dt <- update_results$new_contracts_dt
  terminated_contracts_dt <- update_results$terminated_contracts_dt

  # If working with panel data, add ref_date column to new records
  if (!is.null(selected_ref_date)) {
    # Add ref_date to new hire records in the main data.tables
    if (ref_date_col %in% names(contract_dt)) {
      # Find rows with NA ref_date (newly added)
      contract_dt[is.na(get(ref_date_col)), (ref_date_col) := selected_ref_date]
    }
    if (ref_date_col %in% names(personnel_dt)) {
      personnel_dt[is.na(get(ref_date_col)), (ref_date_col) := selected_ref_date]
    }

    # Also add to summary tables
    if (nrow(new_hires_dt) > 0 && !ref_date_col %in% names(new_hires_dt)) {
      new_hires_dt[, (ref_date_col) := selected_ref_date]
    }
    if (nrow(new_contracts_dt) > 0 && !ref_date_col %in% names(new_contracts_dt)) {
      new_contracts_dt[, (ref_date_col) := selected_ref_date]
    }
  }

  # ========================================
  # 5. Compute Summary Statistics
  # ========================================
  # Compute final headcount
  final_headcount <- compute_current_stock(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    ref_date = ref_date,
    group_cols = NULL,  # Overall
    column_spec_args = cols
  )

  summary_tbl <- compute_hiring_summary(
    adjustment_dt = adjustment_dt,
    new_hires_dt = new_hires_dt,
    new_contracts_dt = new_contracts_dt,
    terminated_contracts_dt = terminated_contracts_dt,
    total_headcount = final_headcount$current_stock,
    salary_col = cols$salary_col,
    personnel_id_col = cols$personnel_id_col
  )

  # ========================================
  # 6. Return Results
  # ========================================
  return(list(
    summary = summary_tbl,
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    adjustment_dt = adjustment_dt,
    new_hires_dt = new_hires_dt,
    new_hire_contracts_dt = new_contracts_dt
  ))
}
