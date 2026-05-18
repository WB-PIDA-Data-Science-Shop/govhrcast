#' State Update Functions for Hiring
#'
#' @description
#' Functions for updating contract_dt and personnel_dt after hiring/downsizing events.
#' Handles new personnel generation, contract creation, salary assignment, and
#' personnel removal.
#'
#' @import data.table
#' @name hiring_update
#' @keywords internal
NULL

#' Generate New Personnel Records
#'
#' @description
#' Creates new personnel records for new hires. Generates unique personnel IDs
#' and sets status to "active".
#'
#' @param n Integer. Number of new personnel to generate
#' @param ref_date Date. Reference date for ID generation
#' @param group_vals Named list. Values for grouping columns (e.g., list(department = "HR", grade = "G5"))
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param status_col Character. Status column (default: "status")
#'
#' @return data.table with new personnel records
#' @keywords internal
generate_new_personnel <- function(n,
                                   ref_date,
                                   group_vals = NULL,
                                   personnel_id_col = "personnel_id",
                                   status_col = "status") {
  
  if (n <= 0) {
    return(data.table::data.table())
  }
  
  # Generate unique personnel IDs — deterministic per (date, group, position)
  group_key <- if (!is.null(group_vals) && length(group_vals) > 0L) {
    paste(unlist(group_vals), collapse = "_")
  } else {
    NULL
  }
  new_ids <- generate_new_ids(n = n, ref_date = ref_date, prefix = "P", group_key = group_key)
  
  # Create base personnel records
  new_personnel <- data.table::data.table(
    personnel_id = new_ids,
    status = "active"
  )
  
  # Rename columns to match user's schema
  data.table::setnames(new_personnel, 
                       old = c("personnel_id", "status"),
                       new = c(personnel_id_col, status_col))
  
  # Add grouping columns if specified
  if (!is.null(group_vals) && length(group_vals) > 0) {
    for (col_name in names(group_vals)) {
      new_personnel[, (col_name) := group_vals[[col_name]]]
    }
  }
  
  return(new_personnel)
}


#' Generate New Contract Records
#'
#' @description
#' Creates new contract records for new hires. Generates unique contract IDs
#' and sets start_date to ref_date.
#'
#' @param personnel_ids Character vector. Personnel IDs for new hires
#' @param ref_date Date. Contract start date
#' @param group_vals Named list. Values for grouping columns
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param contract_id_col Character. Contract ID column (default: "contract_id")
#' @param start_date_col Character. Start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#'
#' @return data.table with new contract records
#' @keywords internal
generate_new_contracts <- function(personnel_ids,
                                   ref_date,
                                   group_vals = NULL,
                                   personnel_id_col = "personnel_id",
                                   contract_id_col = "contract_id",
                                   start_date_col = "start_date",
                                   end_date_col = "end_date",
                                   contract_type_col = "contract_type_code") {
  
  n <- length(personnel_ids)
  
  if (n <= 0) {
    return(data.table::data.table())
  }
  
  # Generate unique contract IDs — deterministic per (date, group, position)
  group_key <- if (!is.null(group_vals) && length(group_vals) > 0L) {
    paste(unlist(group_vals), collapse = "_")
  } else {
    NULL
  }
  contract_ids <- generate_new_ids(n = n, ref_date = ref_date, prefix = "C", group_key = group_key)
  
  # Create base contract records
  new_contracts <- data.table::data.table(
    contract_id = contract_ids,
    personnel_id = personnel_ids,
    start_date = ref_date,
    end_date = as.Date(NA),
    contract_type_code = "permanent"
  )
  
  # Rename columns to match user's schema
  data.table::setnames(new_contracts,
                       old = c("contract_id", "personnel_id", "start_date", 
                               "end_date", "contract_type_code"),
                       new = c(contract_id_col, personnel_id_col, start_date_col,
                               end_date_col, contract_type_col))
  
  # Add grouping columns if specified
  if (!is.null(group_vals) && length(group_vals) > 0) {
    for (col_name in names(group_vals)) {
      new_contracts[, (col_name) := group_vals[[col_name]]]
    }
  }
  
  return(new_contracts)
}


#' Assign Compensation to New Hires
#'
#' @description
#' Merges salary scale onto new hire contracts. Automatically detects join columns
#' and salary column if not specified. Issues warning (not error) if some contracts
#' cannot be matched to salary scale.
#'
#' @param new_contracts_dt data.table. New hire contracts
#' @param salary_scale_dt data.table. Salary scale with join columns + salary column
#' @param join_cols Character vector. Columns to join on. If NULL (default), uses
#'   intersection of column names (excluding salary columns)
#' @param salary_col Character. Salary column name. If NULL (default), auto-detects
#'   first column matching pattern "salary|wage|pay|compensation"
#'
#' @return Updated new_contracts_dt with salary column
#' @keywords internal
assign_compensation <- function(new_contracts_dt,
                                salary_scale_dt,
                                join_cols = NULL,
                                salary_col = NULL) {
  
  if (nrow(new_contracts_dt) == 0) {
    return(new_contracts_dt)
  }
  
  # Auto-detect salary column if not specified
  if (is.null(salary_col)) {
    salary_pattern <- "salary|wage|pay|compensation"
    salary_candidates <- grep(salary_pattern, names(salary_scale_dt), 
                              value = TRUE, ignore.case = TRUE)
    
    if (length(salary_candidates) == 0) {
      stop("Could not auto-detect salary column in salary_scale_dt. ",
           "Please specify salary_col explicitly.", call. = FALSE)
    }
    
    # Prefer gross_salary, then base_salary, then first match
    if ("gross_salary_lcu" %in% salary_candidates) {
      salary_col <- "gross_salary_lcu"
    } else if ("base_salary_lcu" %in% salary_candidates) {
      salary_col <- "base_salary_lcu"
    } else {
      salary_col <- salary_candidates[1]
    }
  }
  
  if (!salary_col %in% names(salary_scale_dt)) {
    stop("Salary column '", salary_col, "' not found in salary_scale_dt", call. = FALSE)
  }
  
  # Auto-detect join columns if not specified
  if (is.null(join_cols)) {
    # Use intersection of column names, excluding the salary column
    common_cols <- intersect(names(new_contracts_dt), names(salary_scale_dt))
    join_cols <- setdiff(common_cols, salary_col)
    
    if (length(join_cols) == 0) {
      # No join key — only valid if salary_scale_dt is a single row (flat/universal
      # salary). In that case broadcast the single value directly and return early.
      if (nrow(salary_scale_dt) == 1L) {
        new_contracts_dt[, (salary_col) := salary_scale_dt[[salary_col]]]
        return(new_contracts_dt)
      }
      stop("No common columns found for joining. ",
           "salary_scale_dt has ", nrow(salary_scale_dt), " rows so a flat broadcast ",
           "is ambiguous. Please specify join_cols explicitly.", call. = FALSE)
    }
  }
  
  # Validate join columns exist in both tables
  missing_in_contracts <- setdiff(join_cols, names(new_contracts_dt))
  missing_in_scale <- setdiff(join_cols, names(salary_scale_dt))
  
  if (length(missing_in_contracts) > 0) {
    stop("Join columns not found in new_contracts_dt: ", 
         paste(missing_in_contracts, collapse = ", "), call. = FALSE)
  }
  
  if (length(missing_in_scale) > 0) {
    stop("Join columns not found in salary_scale_dt: ", 
         paste(missing_in_scale, collapse = ", "), call. = FALSE)
  }
  
  # Check for duplicate keys in salary_scale_dt to avoid cartesian join
  if (anyDuplicated(salary_scale_dt, by = join_cols)) {
    dup_keys <- salary_scale_dt[duplicated(salary_scale_dt, by = join_cols), 
                                join_cols, with = FALSE]
    stop("Duplicate keys found in salary_scale_dt. This would cause a cartesian join.\n",
         "Duplicated join key values:\n",
         paste(utils::capture.output(print(head(unique(dup_keys), 10))), collapse = "\n"),
         "\nPlease ensure salary_scale_dt has unique keys for: ",
         paste(join_cols, collapse = ", "), call. = FALSE)
  }
  
  # Store original row count
  n_original <- nrow(new_contracts_dt)
  
  # Perform join
  # Select only join_cols and salary_col from salary_scale_dt
  scale_subset <- unique(salary_scale_dt[, c(join_cols, salary_col), with = FALSE])
  
  # Join salary onto contracts
  new_contracts_dt <- scale_subset[new_contracts_dt, on = join_cols]
  
  # Check for unmatched rows and issue warning (not error)
  if (any(is.na(new_contracts_dt[[salary_col]]))) {
    n_unmatched <- sum(is.na(new_contracts_dt[[salary_col]]))
    unmatched <- new_contracts_dt[is.na(get(salary_col))]
    unmatched_vals <- unique(unmatched[, join_cols, with = FALSE])
    
    warning("WARNING: ", n_unmatched, " out of ", n_original, 
            " contracts could not be matched to salary_scale_dt.\n",
            "These contracts will have NA salary values.\n",
            "Unmatched join key combinations (showing up to 10):\n",
            paste(utils::capture.output(print(head(unmatched_vals, 10))), collapse = "\n"),
            call. = FALSE, immediate. = TRUE)
  }
  
  return(new_contracts_dt)
}


#' Select Personnel to Remove for Downsizing
#'
#' @description
#' Identifies personnel to remove when net_change is negative. Supports
#' different removal strategies.
#'
#' @param contract_dt data.table. Current contract data
#' @param personnel_dt data.table. Current personnel data
#' @param n_remove Integer. Number of personnel to remove (positive number)
#' @param strategy Character. Removal strategy (default: "last_hired_first")
#'   Options: "last_hired_first", "random"
#' @param group_vals Named list. Values for grouping columns to filter on
#' @param ref_date Date. Reference date
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param start_date_col Character. Start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#' @param status_col Character. Status column (default: "status")
#'
#' @return Character vector of personnel_ids to remove
#' @keywords internal
select_personnel_to_remove <- function(contract_dt,
                                       personnel_dt,
                                       n_remove,
                                       strategy = "last_hired_first",
                                       group_vals = NULL,
                                       ref_date,
                                       personnel_id_col = "personnel_id",
                                       start_date_col = "start_date",
                                       end_date_col = "end_date",
                                       contract_type_col = "contract_type_code",
                                       status_col = "status") {
  
  if (n_remove <= 0) {
    return(character(0))
  }
  
  # Get active personnel
  active_contracts <- get_active_contracts(
    contract_dt = contract_dt,
    ref_date = ref_date,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col
  )
  
  active_personnel <- active_contracts[
    personnel_dt[get(status_col) == "active"],
    on = personnel_id_col,
    nomatch = NULL
  ]
  
  # Filter by group if specified
  if (!is.null(group_vals) && length(group_vals) > 0) {
    for (col_name in names(group_vals)) {
      active_personnel <- active_personnel[get(col_name) == group_vals[[col_name]]]
    }
  }
  
  if (nrow(active_personnel) == 0) {
    warning("No active personnel found to remove", call. = FALSE)
    return(character(0))
  }
  
  # Get primary contract per personnel for start_date
  primary_contracts <- get_primary_contract(
    contract_dt = active_personnel,
    personnel_id_col = personnel_id_col,
    start_date_col = start_date_col,
    salary_col = "gross_salary_lcu"
  )
  
  # Apply removal strategy
  personnel_to_remove <- switch(
    strategy,
    
    "last_hired_first" = {
      # Order by start_date descending (most recent first)
      data.table::setorderv(primary_contracts, start_date_col, order = -1)
      head(primary_contracts[[personnel_id_col]], n_remove)
    },
    
    "random" = {
      # Random selection
      if (nrow(primary_contracts) <= n_remove) {
        primary_contracts[[personnel_id_col]]
      } else {
        sample(primary_contracts[[personnel_id_col]], size = n_remove)
      }
    },
    
    stop("Unknown removal strategy: ", strategy, 
         ". Supported: 'last_hired_first', 'random'", call. = FALSE)
  )
  
  return(personnel_to_remove)
}


#' Update State with Hiring/Downsizing Adjustments
#'
#' @description
#' Main state update function. Handles both hiring (positive net_change) and
#' downsizing (negative net_change). Returns modified contract_dt and personnel_dt.
#' This is the ONLY function that modifies state in the hiring module.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param adjustment_dt data.table. Adjustment targets with group_cols + net_change column
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item salary_scale: data.table with salary information
#'     \item removal_strategy: Character for downsizing (default: "last_hired_first")
#'     \item group_cols: Character vector of grouping columns (can be NULL)
#'   }
#' @param ref_date Date. Reference date for adjustments
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param contract_id_col Character. Contract ID column (default: "contract_id")
#' @param start_date_col Character. Start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param salary_col Character. Salary column (default: "gross_salary_lcu")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#' @param status_col Character. Status column (default: "status")
#'
#' @return List containing:
#'   \itemize{
#'     \item contract_dt: Updated contract data
#'     \item personnel_dt: Updated personnel data
#'     \item new_personnel_dt: New personnel added (for reporting)
#'     \item new_contracts_dt: New contracts added (for reporting)
#'     \item terminated_contracts_dt: Contracts terminated in downsizing (salary reflects cost saved)
#'   }
#' @section Data Integrity:
#'   Modifies \code{contract_dt} and \code{personnel_dt} **in place** via
#'   data.table reference semantics.  Pass \code{data.table::copy()} on
#'   each if the originals must be preserved.
#' @keywords internal
update_state_with_adjustment <- function(contract_dt,
                                         personnel_dt,
                                         adjustment_dt,
                                         policy_params,
                                         ref_date,
                                         personnel_id_col = "personnel_id",
                                         contract_id_col = "contract_id",
                                         start_date_col = "start_date",
                                         end_date_col = "end_date",
                                         salary_col = "gross_salary_lcu",
                                         contract_type_col = "contract_type_code",
                                         status_col = "status") {
  
  group_cols <- policy_params$group_cols
  salary_scale <- policy_params$salary_scale
  removal_strategy <- if (!is.null(policy_params$removal_strategy)) {
    policy_params$removal_strategy
  } else {
    "last_hired_first"
  }
  
  # Initialize collectors for new hires and terminations
  all_new_personnel <- list()
  all_new_contracts <- list()
  all_terminated_contracts <- list()
  
  # Handle each adjustment group
  if (!is.null(group_cols) && length(group_cols) > 0) {
    # Process each grouping cell
    for (i in seq_len(nrow(adjustment_dt))) {
      row <- adjustment_dt[i]
      net_change <- row$net_change
      
      # Extract group values
      group_vals <- as.list(row[, group_cols, with = FALSE])
      
      if (net_change > 0) {
        # HIRING
        # Generate new personnel
        new_personnel <- generate_new_personnel(
          n = net_change,
          ref_date = ref_date,
          group_vals = group_vals,
          personnel_id_col = personnel_id_col,
          status_col = status_col
        )
        
        # Generate new contracts
        new_contracts <- generate_new_contracts(
          personnel_ids = new_personnel[[personnel_id_col]],
          ref_date = ref_date,
          group_vals = group_vals,
          personnel_id_col = personnel_id_col,
          contract_id_col = contract_id_col,
          start_date_col = start_date_col,
          end_date_col = end_date_col,
          contract_type_col = contract_type_col
        )
        
        # Assign compensation (auto-detects join cols and salary col)
        new_contracts <- assign_compensation(
          new_contracts_dt = new_contracts,
          salary_scale_dt = salary_scale,
          join_cols = group_cols,  # Use group_cols for joining
          salary_col = NULL  # Auto-detect salary column
        )
        
        # Append new records to data.tables
        personnel_dt <- data.table::rbindlist(
          list(personnel_dt, new_personnel), 
          fill = TRUE, 
          use.names = TRUE
        )
        
        contract_dt <- data.table::rbindlist(
          list(contract_dt, new_contracts),
          fill = TRUE,
          use.names = TRUE
        )
        
        # Collect for summary
        all_new_personnel[[i]] <- new_personnel
        all_new_contracts[[i]] <- new_contracts
        
      } else if (net_change < 0) {
        # DOWNSIZING
        n_remove <- abs(net_change)
        
        # Select personnel to remove
        personnel_to_remove <- select_personnel_to_remove(
          contract_dt = contract_dt,
          personnel_dt = personnel_dt,
          n_remove = n_remove,
          strategy = removal_strategy,
          group_vals = group_vals,
          ref_date = ref_date,
          personnel_id_col = personnel_id_col,
          start_date_col = start_date_col,
          end_date_col = end_date_col,
          contract_type_col = contract_type_col,
          status_col = status_col
        )
        
        # Capture active contracts for terminated personnel (for salary cost reporting)
        terminated_contracts <- get_active_contracts(
          contract_dt = contract_dt,
          ref_date = ref_date,
          start_date_col = start_date_col,
          end_date_col = end_date_col,
          contract_type_col = contract_type_col
        )[get(personnel_id_col) %in% personnel_to_remove]
        all_terminated_contracts[[i]] <- terminated_contracts
        
        # Deactivate contracts
        contract_dt[
          get(personnel_id_col) %in% personnel_to_remove,
          c(end_date_col, contract_type_col) := list(ref_date, "terminated")
        ]
        
        # Deactivate personnel
        personnel_dt[
          get(personnel_id_col) %in% personnel_to_remove,
          (status_col) := "inactive"
        ]
      }
      # net_change == 0: no action
    }
  } else {
    # Overall adjustment (no grouping)
    if (nrow(adjustment_dt) != 1) {
      stop("For overall adjustment (no group_cols), adjustment_dt must have exactly 1 row",
           call. = FALSE)
    }
    
    net_change <- adjustment_dt$net_change[1]
    
    if (net_change > 0) {
      # HIRING
      new_personnel <- generate_new_personnel(
        n = net_change,
        ref_date = ref_date,
        group_vals = NULL,
        personnel_id_col = personnel_id_col,
        status_col = status_col
      )
      
      new_contracts <- generate_new_contracts(
        personnel_ids = new_personnel[[personnel_id_col]],
        ref_date = ref_date,
        group_vals = NULL,
        personnel_id_col = personnel_id_col,
        contract_id_col = contract_id_col,
        start_date_col = start_date_col,
        end_date_col = end_date_col,
        contract_type_col = contract_type_col
      )
      
      # For overall hiring without grouping, salary scale should have 1 row or join on other columns
      # User must ensure salary_scale_dt is compatible
      if (!is.null(salary_scale) && nrow(salary_scale) > 0) {
        # Detect salary column
        salary_pattern <- "salary|wage|pay|compensation"
        salary_col_detected <- grep(salary_pattern, names(salary_scale), 
                                   value = TRUE, ignore.case = TRUE)[1]
        
        # Check for common columns (excluding salary)
        common_cols <- intersect(names(new_contracts), names(salary_scale))
        join_cols_overall <- setdiff(common_cols, salary_col_detected)
        
        if (length(join_cols_overall) > 0) {
          # Join on common columns
          new_contracts <- assign_compensation(
            new_contracts_dt = new_contracts,
            salary_scale_dt = salary_scale,
            join_cols = join_cols_overall,
            salary_col = NULL  # Auto-detect
          )
        } else {
          # No common columns - apply single salary value
          if (nrow(salary_scale) == 1) {
            new_contracts[, (salary_col_detected) := salary_scale[[salary_col_detected]][1]]
          } else {
            stop("salary_scale has multiple rows but no common columns with contracts. ",
                 "Cannot determine how to assign salaries.", call. = FALSE)
          }
        }
      }
      
      # Append to data
      personnel_dt <- data.table::rbindlist(
        list(personnel_dt, new_personnel),
        fill = TRUE,
        use.names = TRUE
      )
      
      contract_dt <- data.table::rbindlist(
        list(contract_dt, new_contracts),
        fill = TRUE,
        use.names = TRUE
      )
      
      all_new_personnel[[1]] <- new_personnel
      all_new_contracts[[1]] <- new_contracts
      
    } else if (net_change < 0) {
      # DOWNSIZING
      n_remove <- abs(net_change)
      
      personnel_to_remove <- select_personnel_to_remove(
        contract_dt = contract_dt,
        personnel_dt = personnel_dt,
        n_remove = n_remove,
        strategy = removal_strategy,
        group_vals = NULL,
        ref_date = ref_date,
        personnel_id_col = personnel_id_col,
        start_date_col = start_date_col,
        end_date_col = end_date_col,
        contract_type_col = contract_type_col,
        status_col = status_col
      )
      
      # Capture active contracts for terminated personnel (for salary cost reporting)
      terminated_contracts <- get_active_contracts(
        contract_dt = contract_dt,
        ref_date = ref_date,
        start_date_col = start_date_col,
        end_date_col = end_date_col,
        contract_type_col = contract_type_col
      )[get(personnel_id_col) %in% personnel_to_remove]
      all_terminated_contracts[[1]] <- terminated_contracts
      
      # Deactivate
      contract_dt[
        get(personnel_id_col) %in% personnel_to_remove,
        c(end_date_col, contract_type_col) := list(ref_date, "terminated")
      ]
      
      personnel_dt[
        get(personnel_id_col) %in% personnel_to_remove,
        (status_col) := "inactive"
      ]
    }
  }
  
  # Combine all new personnel and contracts for summary
  new_personnel_all <- if (length(all_new_personnel) > 0) {
    data.table::rbindlist(all_new_personnel, fill = TRUE, use.names = TRUE)
  } else {
    data.table::data.table()
  }
  
  new_contracts_all <- if (length(all_new_contracts) > 0) {
    data.table::rbindlist(all_new_contracts, fill = TRUE, use.names = TRUE)
  } else {
    data.table::data.table()
  }
  
  terminated_contracts_all <- if (length(all_terminated_contracts) > 0) {
    data.table::rbindlist(all_terminated_contracts, fill = TRUE, use.names = TRUE)
  } else {
    data.table::data.table()
  }
  
  return(list(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    new_personnel_dt = new_personnel_all,
    new_contracts_dt = new_contracts_all,
    terminated_contracts_dt = terminated_contracts_all
  ))
}
