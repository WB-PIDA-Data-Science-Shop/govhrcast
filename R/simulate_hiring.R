#' Simulate Hiring Module
#'
#' @description
#' Main user-facing function for simulating hiring and workforce adjustments.
#' Orchestrates the entire hiring workflow: demand estimation, state updates,
#' and summary reporting. Supports three policy modes: flow-based (replacement),
#' stock-based (target levels), and combined.
#'
#' @import data.table
#'
#' @param contract_dt data.table. Contract data in govhr harmonized format
#' @param personnel_dt data.table. Personnel data in govhr harmonized format
#' @param policy_params List. Hiring policy parameters including:
#'   \describe{
#'     \item{mode}{Character. One of: "flow", "stock", "combined"}
#'     \item{group_cols}{Character vector. Grouping columns (e.g., c("department", "grade")). NULL for overall.}
#'     \item{replacement_rate}{Numeric scalar or data.table. For "flow" and "combined" modes.}
#'     \item{stock_targets}{data.table. For "stock" and "combined" modes. Must contain group_cols + target_stock.}
#'     \item{salary_scale}{data.table. Salary scale with group_cols + salary column.}
#'     \item{removal_strategy}{Character. For downsizing. Options: "last_hired_first", "random" (default: "last_hired_first")}
#'     \item{eligibility_type}{Character. For flow mode with retirement calculation. See retirement module.}
#'     \item{min_age}{Numeric. For flow mode with retirement calculation.}
#'     \item{min_tenure}{Numeric. For flow mode with retirement calculation.}
#'   }
#' @param retirees_dt data.table. Optional. Output from simulate_retirement (default: NULL)
#' @param ref_date Date. Reference date for hiring simulation
#' @param ref_date_col Character. Name of reference date column in panel data (default: "ref_date")
#' @param personnel_id_col Character. Name of personnel ID column (default: "personnel_id")
#' @param birth_date_col Character. Name of birth date column (default: "birth_date")
#' @param contract_id_col Character. Name of contract ID column (default: "contract_id")
#' @param start_date_col Character. Name of start date column (default: "start_date")
#' @param end_date_col Character. Name of end date column (default: "end_date")
#' @param salary_col Character. Name of salary column (default: "gross_salary_lcu")
#' @param contract_type_col Character. Name of contract type column (default: "contract_type_code")
#' @param status_col Character. Name of status column (default: "status")
#'
#' @return List containing:
#'   \describe{
#'     \item{summary}{data.table with hiring summary statistics}
#'     \item{contract_dt}{Updated contract data with new hires}
#'     \item{personnel_dt}{Updated personnel data with new hires}
#'     \item{adjustment_dt}{data.table of adjustments by group}
#'     \item{new_hires_dt}{data.table of new hires (empty if downsizing only)}
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
#'   eligibility_type = "age_and_tenure",
#'   min_age = 60,
#'   min_tenure = 20,
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
#'   retirees_dt = retirement_results$retirees_dt,
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
                           retirees_dt = NULL,
                           ref_date,
                           ref_date_col = "ref_date",
                           personnel_id_col = "personnel_id",
                           birth_date_col = "birth_date",
                           contract_id_col = "contract_id",
                           start_date_col = "start_date",
                           end_date_col = "end_date",
                           salary_col = "gross_salary_lcu",
                           contract_type_col = "contract_type_code",
                           status_col = "status") {
  
  # ========================================
  # 1. Input Validation
  # ========================================
  check_hiring_inputs(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date,
    personnel_id_col = personnel_id_col,
    birth_date_col = birth_date_col,
    contract_id_col = contract_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col,
    status_col = status_col
  )
  
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
    retirees_dt = retirees_dt,
    ref_date = ref_date,
    personnel_id_col = personnel_id_col,
    birth_date_col = birth_date_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col,
    status_col = status_col
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
      personnel_id_col = personnel_id_col,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      contract_type_col = contract_type_col,
      status_col = status_col
    )
    
    summary_tbl <- data.table::data.table(
      n_new_hires = 0L,
      net_headcount_change = 0L,
      total_headcount = current_headcount$current_stock,
      total_new_salary_cost = 0
    )
    
    return(list(
      summary = summary_tbl,
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      adjustment_dt = adjustment_dt,
      new_hires_dt = data.table::data.table()
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
    personnel_id_col = personnel_id_col,
    contract_id_col = contract_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    salary_col = salary_col,
    contract_type_col = contract_type_col,
    status_col = status_col
  )
  
  # Extract updated data.tables and new hire information
  contract_dt <- update_results$contract_dt
  personnel_dt <- update_results$personnel_dt
  new_hires_dt <- update_results$new_personnel_dt
  new_contracts_dt <- update_results$new_contracts_dt
  
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
    personnel_id_col = personnel_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col,
    status_col = status_col
  )
  
  summary_tbl <- compute_hiring_summary(
    adjustment_dt = adjustment_dt,
    new_hires_dt = new_hires_dt,
    new_contracts_dt = new_contracts_dt,
    total_headcount = final_headcount$current_stock
  )
  
  # ========================================
  # 6. Return Results
  # ========================================
  return(list(
    summary = summary_tbl,
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    adjustment_dt = adjustment_dt,
    new_hires_dt = new_hires_dt
  ))
}
