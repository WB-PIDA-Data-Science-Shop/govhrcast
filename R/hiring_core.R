#' Core Hiring Logic Functions
#'
#' @description
#' Functions for computing current workforce stock, estimating hiring demand
#' based on different policy modes (flow, stock, combined), and calculating
#' hiring-related statistics.
#'
#' @import data.table
#' @name hiring_core
#' @keywords internal
NULL

#' Compute Current Workforce Stock
#'
#' @description
#' Aggregates active personnel counts at a reference date, optionally by
#' grouping variables (e.g., department, grade, etc.). Returns total headcount
#' if no grouping variables are specified.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param ref_date Date. Reference date for stock calculation
#' @param group_cols Character vector. Columns to group by (default: NULL for overall count)
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param start_date_col Character. Start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#' @param status_col Character. Status column (default: "status")
#'
#' @return data.table with group_cols (if specified) and current_stock column
#' @keywords internal
compute_current_stock <- function(contract_dt,
                                  personnel_dt,
                                  ref_date,
                                  group_cols = NULL,
                                  personnel_id_col = "personnel_id",
                                  start_date_col = "start_date",
                                  end_date_col = "end_date",
                                  contract_type_col = "contract_type_code",
                                  status_col = "status") {
  
  # Get active contracts at ref_date
  active_contracts <- get_active_contracts(
    contract_dt = contract_dt,
    ref_date = ref_date,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col
  )
  
  # Merge with personnel to get active personnel only
  active_personnel <- active_contracts[
    personnel_dt[get(status_col) == "active"],
    on = personnel_id_col,
    nomatch = NULL
  ]
  
  # If group_cols specified, ensure they exist in active_personnel
  if (!is.null(group_cols) && length(group_cols) > 0) {
    missing_cols <- setdiff(group_cols, names(active_personnel))
    if (length(missing_cols) > 0) {
      stop("Grouping columns not found in data: ", paste(missing_cols, collapse = ", "), 
           call. = FALSE)
    }
    
    # Aggregate by group_cols - count unique personnel
    stock_dt <- active_personnel[, 
      .(current_stock = data.table::uniqueN(get(personnel_id_col))),
      by = group_cols
    ]
  } else {
    # Overall count - no grouping
    stock_dt <- data.table::data.table(
      current_stock = data.table::uniqueN(active_personnel[[personnel_id_col]])
    )
  }
  
  return(stock_dt)
}


#' Compute Flow-Based Hiring Demand
#'
#' @description
#' Calculates hiring needs based on exits (retirements) from previous period.
#' Demand = exits × replacement_rate. Replacement rate can be scalar or data.table
#' matched on group_cols.
#'
#' @param contract_dt data.table. Current contract data
#' @param personnel_dt data.table. Current personnel data
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item replacement_rate: Scalar or data.table with group_cols + replacement_rate
#'     \item group_cols: Character vector of grouping columns (can be NULL)
#'   }
#' @param retirees_dt data.table. Optional. Output from simulate_retirement (default: NULL)
#' @param ref_date Date. Reference date for calculations
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param birth_date_col Character. Birth date column (default: "birth_date")
#' @param start_date_col Character. Start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#'
#' @return data.table with group_cols (if specified) and total_hires column
#' @keywords internal
compute_flow_demand <- function(contract_dt,
                                personnel_dt,
                                policy_params,
                                retirees_dt = NULL,
                                ref_date,
                                personnel_id_col = "personnel_id",
                                birth_date_col = "birth_date",
                                start_date_col = "start_date",
                                end_date_col = "end_date",
                                contract_type_col = "contract_type_code") {
  
  group_cols <- policy_params$group_cols
  replacement_rate <- policy_params$replacement_rate
  
  # If retirees_dt not provided, compute it using identify_retirees
  if (is.null(retirees_dt) || nrow(retirees_dt) == 0) {
    # Use retirement module's eligibility logic
    eligibility_dt <- identify_retirees(
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      policy_params = policy_params,  # Must contain eligibility_type, min_age, min_tenure
      ref_date = ref_date,
      personnel_id_col = personnel_id_col,
      birth_date_col = birth_date_col,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      contract_type_col = contract_type_col
    )
    
    # Filter to actual retirees
    exits_dt <- eligibility_dt[retire == 1]
    
    # If group_cols specified, need to merge with contract_dt to get grouping variables
    if (!is.null(group_cols) && length(group_cols) > 0) {
      # Get active contracts for retirees
      active_retiree_contracts <- contract_dt[
        get(personnel_id_col) %in% exits_dt$personnel_id
      ][
        get(start_date_col) <= ref_date &
        (is.na(get(end_date_col)) | get(end_date_col) >= ref_date)
      ]
      
      # Get primary contract per retiree
      primary_contracts <- get_primary_contract(
        contract_dt = active_retiree_contracts,
        personnel_id_col = personnel_id_col,
        start_date_col = start_date_col,
        salary_col = "gross_salary_lcu"
      )
      
      exits_dt <- primary_contracts[, c(personnel_id_col, group_cols), with = FALSE]
    }
  } else {
    # Use provided retirees_dt
    exits_dt <- retirees_dt
    
    # Ensure personnel_id column is named correctly
    if (!"personnel_id" %in% names(exits_dt) && personnel_id_col %in% names(exits_dt)) {
      data.table::setnames(exits_dt, personnel_id_col, "personnel_id", skip_absent = TRUE)
    }
    
    # If group_cols specified but not in retirees_dt, merge from contract_dt
    if (!is.null(group_cols) && length(group_cols) > 0) {
      missing_cols <- setdiff(group_cols, names(exits_dt))
      if (length(missing_cols) > 0) {
        # Get grouping info from contract_dt
        contract_info <- contract_dt[
          get(personnel_id_col) %in% exits_dt$personnel_id,
          c(personnel_id_col, group_cols),
          with = FALSE
        ]
        
        exits_dt <- contract_info[exits_dt, on = c(personnel_id = "personnel_id")]
      }
    }
  }
  
  # Compute exiting stock by group (or overall)
  if (!is.null(group_cols) && length(group_cols) > 0) {
    exiting_stock <- exits_dt[, 
      .(exit_count = .N),
      by = group_cols
    ]
  } else {
    exiting_stock <- data.table::data.table(
      exit_count = nrow(exits_dt)
    )
  }
  
  # Apply replacement rate
  if (is.data.table(replacement_rate)) {
    # Merge replacement rates by group_cols
    if (is.null(group_cols) || length(group_cols) == 0) {
      stop("replacement_rate is a data.table but no group_cols specified", call. = FALSE)
    }
    
    demand_dt <- replacement_rate[exiting_stock, on = group_cols]
    demand_dt[, total_hires := exit_count * replacement_rate]
  } else {
    # Scalar replacement rate
    demand_dt <- data.table::copy(exiting_stock)
    demand_dt[, total_hires := exit_count * replacement_rate]
  }
  
  # Round to integers and ensure non-negative
  demand_dt[, total_hires := pmax(0, round(total_hires))]
  
  # Return with group_cols and total_hires
  if (!is.null(group_cols) && length(group_cols) > 0) {
    result <- demand_dt[, c(group_cols, "total_hires"), with = FALSE]
  } else {
    result <- demand_dt[, .(total_hires)]
  }
  
  return(result)
}


#' Compute Stock-Based Hiring Demand
#'
#' @description
#' Calculates hiring needs to achieve target workforce levels.
#' Demand = target_stock - current_stock. Allows negative values (downsizing).
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item stock_targets: data.table with group_cols + target_stock
#'     \item group_cols: Character vector of grouping columns (can be NULL)
#'   }
#' @param ref_date Date. Reference date
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param start_date_col Character. Start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#' @param status_col Character. Status column (default: "status")
#'
#' @return data.table with group_cols (if specified) and total_hires column
#' @keywords internal
compute_stock_demand <- function(contract_dt,
                                 personnel_dt,
                                 policy_params,
                                 ref_date,
                                 personnel_id_col = "personnel_id",
                                 start_date_col = "start_date",
                                 end_date_col = "end_date",
                                 contract_type_col = "contract_type_code",
                                 status_col = "status") {
  
  group_cols <- policy_params$group_cols
  stock_targets <- policy_params$stock_targets
  
  # Validate stock_targets
  if (!is.data.table(stock_targets)) {
    stop("policy_params$stock_targets must be a data.table", call. = FALSE)
  }
  
  if (!"target_stock" %in% names(stock_targets)) {
    stop("stock_targets must contain 'target_stock' column", call. = FALSE)
  }
  
  # Compute current stock
  current_stock_dt <- compute_current_stock(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    ref_date = ref_date,
    group_cols = group_cols,
    personnel_id_col = personnel_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col,
    status_col = status_col
  )
  
  # Merge targets with current stock
  if (!is.null(group_cols) && length(group_cols) > 0) {
    demand_dt <- stock_targets[current_stock_dt, on = group_cols]
    
    # Handle missing matches (groups in targets but not in current data)
    demand_dt[is.na(current_stock), current_stock := 0L]
  } else {
    # Overall demand (no grouping)
    if (nrow(stock_targets) != 1) {
      stop("For overall demand (no group_cols), stock_targets must have exactly 1 row", 
           call. = FALSE)
    }
    demand_dt <- data.table::copy(stock_targets)
    demand_dt[, current_stock := current_stock_dt$current_stock]
  }
  
  # Compute demand (allow negative values for downsizing)
  demand_dt[, total_hires := target_stock - current_stock]
  
  # Return with group_cols and total_hires
  if (!is.null(group_cols) && length(group_cols) > 0) {
    result <- demand_dt[, c(group_cols, "total_hires"), with = FALSE]
  } else {
    result <- demand_dt[, .(total_hires)]
  }
  
  return(result)
}


#' Compute Combined Hiring Demand
#'
#' @description
#' Calculates hiring needs using both flow and stock considerations.
#' Flow component: current_stock × replacement_rate
#' Stock component: target_stock - current_stock
#' Total demand: flow + stock
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item replacement_rate: Scalar or data.table with group_cols + replacement_rate
#'     \item stock_targets: data.table with group_cols + target_stock
#'     \item group_cols: Character vector of grouping columns (can be NULL)
#'   }
#' @param ref_date Date. Reference date
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param start_date_col Character. Start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#' @param status_col Character. Status column (default: "status")
#'
#' @return data.table with group_cols (if specified) and total_hires column
#' @keywords internal
compute_combined_demand <- function(contract_dt,
                                    personnel_dt,
                                    policy_params,
                                    ref_date,
                                    personnel_id_col = "personnel_id",
                                    start_date_col = "start_date",
                                    end_date_col = "end_date",
                                    contract_type_col = "contract_type_code",
                                    status_col = "status") {
  
  group_cols <- policy_params$group_cols
  replacement_rate <- policy_params$replacement_rate
  stock_targets <- policy_params$stock_targets
  
  # Validate inputs
  if (!is.data.table(stock_targets)) {
    stop("policy_params$stock_targets must be a data.table", call. = FALSE)
  }
  
  # Compute current stock
  current_stock_dt <- compute_current_stock(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    ref_date = ref_date,
    group_cols = group_cols,
    personnel_id_col = personnel_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col,
    status_col = status_col
  )
  
  # Compute flow demand: current_stock × replacement_rate
  if (is.data.table(replacement_rate)) {
    # Merge replacement rates by group_cols
    if (is.null(group_cols) || length(group_cols) == 0) {
      stop("replacement_rate is a data.table but no group_cols specified", call. = FALSE)
    }
    
    flow_dt <- replacement_rate[current_stock_dt, on = group_cols]
    flow_dt[, flow_demand := current_stock * replacement_rate]
  } else {
    # Scalar replacement rate
    flow_dt <- data.table::copy(current_stock_dt)
    flow_dt[, flow_demand := current_stock * replacement_rate]
  }
  
  # Compute stock demand: target_stock - current_stock
  if (!is.null(group_cols) && length(group_cols) > 0) {
    demand_dt <- stock_targets[flow_dt, on = group_cols]
    
    # Handle missing matches
    demand_dt[is.na(target_stock), target_stock := current_stock]
  } else {
    # Overall demand (no grouping)
    if (nrow(stock_targets) != 1) {
      stop("For overall demand (no group_cols), stock_targets must have exactly 1 row", 
           call. = FALSE)
    }
    demand_dt <- data.table::copy(stock_targets)
    demand_dt[, current_stock := flow_dt$current_stock]
    demand_dt[, flow_demand := flow_dt$flow_demand]
  }
  
  demand_dt[, stock_demand := target_stock - current_stock]
  
  # Combine demands
  demand_dt[, total_hires := flow_demand + stock_demand]
  
  # Round to integers
  demand_dt[, total_hires := round(total_hires)]
  
  # Return with group_cols and total_hires
  if (!is.null(group_cols) && length(group_cols) > 0) {
    result <- demand_dt[, c(group_cols, "total_hires"), with = FALSE]
  } else {
    result <- demand_dt[, .(total_hires)]
  }
  
  return(result)
}


#' Estimate Hiring Demand
#'
#' @description
#' Wrapper function that routes to appropriate demand calculation based on
#' policy mode: "flow", "stock", or "combined". Pure function - no state modification.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item mode: "flow", "stock", or "combined"
#'     \item Other parameters depending on mode (see specific functions)
#'   }
#' @param retirees_dt data.table. Optional. Output from simulate_retirement (default: NULL)
#' @param ref_date Date. Reference date
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param birth_date_col Character. Birth date column (default: "birth_date")
#' @param start_date_col Character. Start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#' @param status_col Character. Status column (default: "status")
#'
#' @return data.table with group_cols (if specified) and total_hires column
#' @keywords internal
estimate_hiring_demand <- function(contract_dt,
                                   personnel_dt,
                                   policy_params,
                                   retirees_dt = NULL,
                                   ref_date,
                                   personnel_id_col = "personnel_id",
                                   birth_date_col = "birth_date",
                                   start_date_col = "start_date",
                                   end_date_col = "end_date",
                                   contract_type_col = "contract_type_code",
                                   status_col = "status") {
  
  mode <- policy_params$mode
  
  demand_dt <- switch(
    mode,
    
    "flow" = compute_flow_demand(
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      policy_params = policy_params,
      retirees_dt = retirees_dt,
      ref_date = ref_date,
      personnel_id_col = personnel_id_col,
      birth_date_col = birth_date_col,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      contract_type_col = contract_type_col
    ),
    
    "stock" = compute_stock_demand(
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      policy_params = policy_params,
      ref_date = ref_date,
      personnel_id_col = personnel_id_col,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      contract_type_col = contract_type_col,
      status_col = status_col
    ),
    
    "combined" = compute_combined_demand(
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      policy_params = policy_params,
      ref_date = ref_date,
      personnel_id_col = personnel_id_col,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      contract_type_col = contract_type_col,
      status_col = status_col
    ),
    
    stop("Unknown hiring mode: ", mode, ". Must be 'flow', 'stock', or 'combined'", 
         call. = FALSE)
  )
  
  return(demand_dt)
}


#' Compute Hiring Summary Statistics
#'
#' @description
#' Aggregates key statistics about hiring including new hires count,
#' net headcount change, total headcount, and total new salary cost.
#' Uses data.table for efficient computation.
#'
#' @param adjustment_dt data.table. Adjustment data with net_change column
#' @param new_hires_dt data.table. New hire personnel data (optional)
#' @param new_contracts_dt data.table. New hire contract data with salary (optional)
#' @param total_headcount Integer. Current total headcount after adjustment
#'
#' @return data.table with summary statistics
#' @keywords internal
compute_hiring_summary <- function(adjustment_dt,
                                   new_hires_dt = NULL,
                                   new_contracts_dt = NULL,
                                   total_headcount = NA_integer_) {
  
  # Calculate statistics
  n_new_hires <- if (!is.null(new_hires_dt)) nrow(new_hires_dt) else 0L
  net_change <- if (nrow(adjustment_dt) > 0) sum(adjustment_dt$net_change, na.rm = TRUE) else 0L
  
  # Calculate new salary cost if contracts provided
  if (!is.null(new_contracts_dt) && nrow(new_contracts_dt) > 0 && 
      "gross_salary_lcu" %in% names(new_contracts_dt)) {
    new_salary_cost <- sum(new_contracts_dt$gross_salary_lcu, na.rm = TRUE)
  } else {
    new_salary_cost <- NA_real_
  }
  
  summary_tbl <- data.table::data.table(
    n_new_hires = as.integer(n_new_hires),
    net_headcount_change = as.integer(net_change),
    total_headcount = as.integer(total_headcount),
    total_new_salary_cost = new_salary_cost
  )
  
  return(summary_tbl)
}
