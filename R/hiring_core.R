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
  
  # Merge with personnel to get active personnel only.
  # NOTE: both contract_dt and personnel_dt must already be single-snapshot data.
  # The orchestrators (simulate_hiring, simulate_retirement) pre-filter to the
  # nearest ref_date before calling this function. If calling directly with raw
  # panel data, subset to a single snapshot first.
  #
  # personnel_dt is expected to be unique at the personnel_id level. If it is not,
  # the join below will produce a cartesian product. Check upfront and error clearly.
  if (anyDuplicated(personnel_dt, by = personnel_id_col)) {
    stop(
      "personnel_dt has duplicate rows for the same ", personnel_id_col, ". ",
      "Ensure personnel_dt is unique at the ", personnel_id_col,
      " level before calling compute_current_stock().",
      call. = FALSE
    )
  }

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
  
  # If retirees_dt not provided, compute it using identify_eligibility
  if (is.null(retirees_dt) || nrow(retirees_dt) == 0) {
    # Use retirement module's eligibility logic
    eligibility_dt <- identify_eligibility(
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
        # Get one primary contract per exiting person to recover group_cols.
        # get_primary_contract() deduplicates to one row per person, preventing
        # row duplication for multi-contract individuals.
        primary_info <- get_primary_contract(
          contract_dt      = contract_dt[
            get(personnel_id_col) %in% exits_dt$personnel_id
          ],
          personnel_id_col = personnel_id_col,
          start_date_col   = start_date_col
        )[, c(personnel_id_col, group_cols), with = FALSE]

        exits_dt <- primary_info[exits_dt, on = c(personnel_id = "personnel_id")]
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


#' Estimate Historical Hiring Rates from Panel Data
#'
#' @description
#' Estimates per-group hiring rates (hires / active stock) from historical panel
#' data.  Two methods are supported, selected by the \code{hire_date_col}
#' argument:
#'
#' \describe{
#'   \item{Panel first-appearance (default, \code{hire_date_col = NULL})}{
#'     Uses \code{govhr::detect_personnel_event()} to detect the first time a
#'     person appears in the panel.  Left-censored at the panel start date —
#'     pre-existing staff present at the first snapshot are not counted as hires.}
#'   \item{Administrative hire date (\code{hire_date_col} supplied)}{
#'     Uses a person-level hire-date column (e.g. \code{"first_employment_date"})
#'     read directly from \code{panel_personnel_dt}.  Hire events are counted
#'     within the \code{[min(panel_dates), max(panel_dates)]} window.  This
#'     method avoids left-censoring bias and is preferred when the column is
#'     available.}
#' }
#'
#' @param panel_contract_dt data.table. Full panel of contract data (all ref_dates).
#' @param panel_personnel_dt data.table. Full panel of personnel data (all ref_dates).
#' @param group_cols Character vector. Columns to group by (e.g. \code{"est_id"}).
#'   Pass \code{NULL} for overall (ungrouped) rates.
#' @param hire_date_col Character or \code{NULL}.  Name of a person-level column
#'   in \code{panel_personnel_dt} that holds the true administrative hire date
#'   (e.g. \code{"first_employment_date"}).  When supplied, the function counts
#'   hires directly from this column instead of using panel first-appearance
#'   detection.  Default \code{NULL} (panel first-appearance method).
#' @param freq Character. Frequency passed to \code{govhr::detect_personnel_event()}
#'   when \code{hire_date_col = NULL}.  Default \code{"year"}.
#' @param personnel_id_col Character. Personnel ID column. Default \code{"personnel_id"}.
#' @param ref_date_col Character. Reference-date column in both panel tables.
#'   Default \code{"ref_date"}.
#' @param start_date_col Character. Contract start-date column. Default \code{"start_date"}.
#' @param end_date_col Character. Contract end-date column. Default \code{"end_date"}.
#' @param contract_type_col Character. Contract-type column. Default \code{"contract_type_code"}.
#' @param status_col Character. Personnel status column. Default \code{"status"}.
#'
#' @return data.table with \code{group_cols} (if specified) and \code{hiring_rate} column.
#' @export
estimate_historical_hiring_rates <- function(panel_contract_dt,
                                             panel_personnel_dt,
                                             group_cols,
                                             hire_date_col     = NULL,
                                             freq              = "year",
                                             personnel_id_col  = "personnel_id",
                                             ref_date_col      = "ref_date",
                                             start_date_col    = "start_date",
                                             end_date_col      = "end_date",
                                             contract_type_col = "contract_type_code",
                                             status_col        = "status") {

  panel_dates <- sort(unique(panel_personnel_dt[[ref_date_col]]))
  panel_start <- min(panel_dates, na.rm = TRUE)
  panel_end   <- max(panel_dates, na.rm = TRUE)

  # ------------------------------------------------------------------
  # PATH A: Administrative hire date column (preferred when available)
  # ------------------------------------------------------------------
  if (!is.null(hire_date_col)) {
    if (!hire_date_col %in% names(panel_personnel_dt))
      stop("hire_date_col '", hire_date_col, "' not found in panel_personnel_dt.",
           call. = FALSE)
    
    # One row per person — hire date does not vary across snapshots
    persons_dt <- unique(panel_personnel_dt[,
      c(personnel_id_col, hire_date_col, ref_date_col), with = FALSE
    ])

    # Restrict to hires that fall within the observable panel window
    hire_events <- persons_dt[
      !is.na(get(hire_date_col)) &
      get(hire_date_col) >= panel_start &
      get(hire_date_col) <= panel_end
    ]

    # assuming we only see people getting hired once
    .fhire_list <- hire_events[, .I[which.min(get(ref_date_col))], by = personnel_id_col]$V1

    hire_events <- hire_events[.fhire_list]

    # Attach group_cols from the most recent contract snapshot for each person
    if (!is.null(group_cols) && length(group_cols) > 0) {
      # Use the latest available snapshot per person to recover group_cols;
      # govhr::add_contract_to_event() deduplicates to one row per person.
      # We create a synthetic event table keyed on hire date so the helper
      # can match each hire to its contract context.
      hire_event_dt <- data.table::copy(hire_events)
      data.table::setnames(hire_event_dt, hire_date_col, "hire_date")

      hire_event_dt <- govhr::add_contract_to_event(
        event_dt    = hire_event_dt,
        contract_dt = panel_contract_dt,
        keep_vars   = group_cols
      )


      hire_counts <- hire_event_dt[
        !is.na(get(group_cols[[1]])),
        .(n_hires = .N),
        by = c(group_cols, ref_date_col)
      ]


      hire_counts <- hire_counts[, mean(n_hires, na.rm = TRUE), by = group_cols]
      setnames(hire_counts, "V1", "n_hires")

    } else {
      hire_counts <- hire_events[,.(n_hires = .N), by = ref_date_col]
    }

    # Stock denominator: mean active stock across all snapshots (same denominator
    # as in the panel first-appearance path)
    # stock_list <- lapply(panel_dates, function(snap) {
    #   ct_snap <- panel_contract_dt[get(ref_date_col) == snap]
    #   pt_snap <- panel_personnel_dt[get(ref_date_col) == snap]
    #   ct_snap <- ct_snap[, !ref_date_col, with = FALSE]
    #   pt_snap <- pt_snap[, !ref_date_col, with = FALSE]

    #   s <- compute_current_stock(
    #     contract_dt       = ct_snap,
    #     personnel_dt      = pt_snap,
    #     ref_date          = snap,
    #     group_cols        = group_cols,
    #     personnel_id_col  = personnel_id_col,
    #     start_date_col    = start_date_col,
    #     end_date_col      = end_date_col,
    #     contract_type_col = contract_type_col,
    #     status_col        = status_col
    #   )
    #   s
    # })
    # stock_dt <- data.table::rbindlist(stock_list, fill = TRUE)

    stock_dt <- panel_contract_dt[, data.table::uniqueN(personnel_id), 
                                    by = c(group_cols, ref_date_col)]
    setnames(stock_dt, "V1", "current_stock")


    # Average stock across snapshots (denominator = mean annual stock)
    if (!is.null(group_cols) && length(group_cols) > 0) {
      mean_stock <- stock_dt[,
        .(current_stock = mean(current_stock, na.rm = TRUE)),
        by = group_cols
      ]
      result_dt <- hire_counts[mean_stock, on = group_cols]
    } else {
      mean_stock <- data.table::data.table(
        current_stock = mean(stock_dt$current_stock, na.rm = TRUE)
      )
      result_dt <- cbind(hire_counts, mean_stock)
    }

    # Rate = total hires in window / mean annual stock / n_years in window
    n_years <- as.numeric(difftime(panel_end, panel_start, units = "days")) / 365.25
    result_dt[is.na(n_hires), n_hires := 0L]
    result_dt[, hiring_rate := data.table::fifelse(
      current_stock > 0,
      n_hires / current_stock,
      0
    )]

    if (!is.null(group_cols) && length(group_cols) > 0) {
      return(result_dt[, c(group_cols, "hiring_rate"), with = FALSE])
    } else {
      return(result_dt[, .(hiring_rate)])
    }
  }

  # ------------------------------------------------------------------
  # PATH B: Panel first-appearance (fallback when hire_date_col = NULL)
  # ------------------------------------------------------------------
  start_str <- format(panel_start)
  end_str   <- format(panel_end)

  # Detect hire events across the full panel
  hire_events <- govhr::detect_personnel_event(
    data       = panel_personnel_dt,
    id_col     = personnel_id_col,
    event_type = "hire",
    start_date = start_str,
    end_date   = end_str,
    freq       = freq
  )
  # hire_events columns: personnel_id_col, ref_date, type_event

  # Join to contract panel to retrieve group_cols per hire.
  # govhr::add_contract_to_event() deduplicates to one row per
  # person x ref_date x keep_vars before joining, preventing row
  # duplication for multi-contract individuals.
  if (!is.null(group_cols) && length(group_cols) > 0) {
    hire_events <- govhr::add_contract_to_event(
      event_dt    = hire_events,
      contract_dt = panel_contract_dt,
      keep_vars   = group_cols
    )

    hire_counts <- hire_events[
      !is.na(get(group_cols[[1]])),   # drop hires without a matching group
      .(n_hires = .N),
      by = c(ref_date_col, group_cols)
    ]
  } else {
    hire_counts <- hire_events[, .(n_hires = .N), by = ref_date_col]
  }

  # Compute active stock per snapshot per group (re-uses compute_current_stock)
  stock_list <- lapply(panel_dates, function(snap) {
    ct_snap <- panel_contract_dt[get(ref_date_col) == snap]
    pt_snap <- panel_personnel_dt[get(ref_date_col) == snap]
    ct_snap <- ct_snap[, !ref_date_col, with = FALSE]
    pt_snap <- pt_snap[, !ref_date_col, with = FALSE]

    s <- compute_current_stock(
      contract_dt       = ct_snap,
      personnel_dt      = pt_snap,
      ref_date          = snap,
      group_cols        = group_cols,
      personnel_id_col  = personnel_id_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      contract_type_col = contract_type_col,
      status_col        = status_col
    )
    s[, (ref_date_col) := snap]
    s
  })
  stock_dt <- data.table::rbindlist(stock_list, fill = TRUE)

  # Merge hire counts with stock to compute per-snapshot hiring rates
  join_keys <- if (!is.null(group_cols) && length(group_cols) > 0) {
    c(ref_date_col, group_cols)
  } else {
    ref_date_col
  }

  rate_dt <- hire_counts[stock_dt, on = join_keys]
  rate_dt[is.na(n_hires), n_hires := 0L]
  rate_dt[, hiring_rate := data.table::fifelse(
    current_stock > 0,
    n_hires / current_stock,
    0
  )]

  # Average rate across snapshots per group
  if (!is.null(group_cols) && length(group_cols) > 0) {
    result <- rate_dt[, .(hiring_rate = mean(hiring_rate, na.rm = TRUE)), by = group_cols]
  } else {
    result <- data.table::data.table(hiring_rate = mean(rate_dt$hiring_rate, na.rm = TRUE))
  }

  result
}


#' Compute Status-Quo Hiring Demand
#'
#' @description
#' Replicates the historical hiring rate observed in the panel to generate
#' period-specific hiring demand.  The rate is estimated once via
#' \code{estimate_historical_hiring_rates()} (using panel data injected into
#' \code{policy_params} by \code{simulate_horizon()}), then scaled by
#' \code{policy_params$rate_mult} (default 1), and applied to the current
#' active stock.
#'
#' @param contract_dt data.table. Current-period contract snapshot (no ref_date column).
#' @param personnel_dt data.table. Current-period personnel snapshot (no ref_date column).
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item panel_contract_dt: Full historical contract panel (injected by simulate_horizon).
#'     \item panel_personnel_dt: Full historical personnel panel (injected by simulate_horizon).
#'     \item group_cols: Character vector or NULL.
#'     \item rate_mult: Numeric scalar (default 1). Scales the estimated historical rate.
#'     \item salary_scale: data.table used for salary assignment of new hires.
#'   }
#' @param ref_date Date. Reference date for current-period stock calculation.
#' @param hire_date_col Character or \code{NULL}.  Forwarded to
#'   \code{estimate_historical_hiring_rates()}.  When non-\code{NULL}, the
#'   administrative hire-date column is used instead of panel first-appearance
#'   detection.  Default \code{NULL}.
#' @param personnel_id_col Character. Default \code{"personnel_id"}.
#' @param start_date_col Character. Default \code{"start_date"}.
#' @param end_date_col Character. Default \code{"end_date"}.
#' @param contract_type_col Character. Default \code{"contract_type_code"}.
#' @param status_col Character. Default \code{"status"}.
#'
#' @return data.table with \code{group_cols} (if specified) and \code{total_hires} column.
#' @keywords internal
compute_status_quo_hiring <- function(contract_dt,
                                      personnel_dt,
                                      policy_params,
                                      ref_date,
                                      hire_date_col     = NULL,
                                      personnel_id_col  = "personnel_id",
                                      start_date_col    = "start_date",
                                      end_date_col      = "end_date",
                                      contract_type_col = "contract_type_code",
                                      status_col        = "status") {

  group_cols <- policy_params$group_cols
  rate_mult  <- if (!is.null(policy_params$rate_mult)) policy_params$rate_mult else 1

  # Estimate historical rates from panel (panel was injected by simulate_horizon)
  # rates <- estimate_historical_hiring_rates(
  #   panel_contract_dt  = policy_params$panel_contract_dt,
  #   panel_personnel_dt = policy_params$panel_personnel_dt,
  #   group_cols         = group_cols,
  #   hire_date_col      = hire_date_col,
  #   personnel_id_col   = personnel_id_col,
  #   start_date_col     = start_date_col,
  #   end_date_col       = end_date_col,
  #   contract_type_col  = contract_type_col,
  #   status_col         = status_col
  # )
  rates <- policy_params$squorate_dt
  rates[, hiring_rate := hiring_rate * rate_mult]

  # Current active stock in this period
  stock <- compute_current_stock(
    contract_dt       = contract_dt,
    personnel_dt      = personnel_dt,
    ref_date          = ref_date,
    group_cols        = group_cols,
    personnel_id_col  = personnel_id_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    contract_type_col = contract_type_col,
    status_col        = status_col
  )

  # Merge rates onto stock
  if (!is.null(group_cols) && length(group_cols) > 0) {
    demand <- rates[stock, on = group_cols]
  } else {
    demand <- cbind(stock, rates)
  }

  demand[is.na(hiring_rate), hiring_rate := 0]
  demand[, total_hires := pmax(0L, as.integer(round(current_stock * hiring_rate)))]

  if (!is.null(group_cols) && length(group_cols) > 0) {
    demand[, c(group_cols, "total_hires"), with = FALSE]
  } else {
    demand[, .(total_hires)]
  }
}


#' Estimate Hiring Demand
#'
#' @description
#' Wrapper function that routes to appropriate demand calculation based on
#' policy mode: \code{"flow"}, \code{"stock"}, \code{"combined"}, or
#' \code{"status_quo"}. Pure function — no state modification.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item mode: \code{"flow"}, \code{"stock"}, \code{"combined"}, or \code{"status_quo"}
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
                                   hire_date_col     = NULL,
                                   personnel_id_col  = "personnel_id",
                                   birth_date_col    = "birth_date",
                                   start_date_col    = "start_date",
                                   end_date_col      = "end_date",
                                   contract_type_col = "contract_type_code",
                                   status_col        = "status") {
  
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

    "status_quo" = compute_status_quo_hiring(
      contract_dt       = contract_dt,
      personnel_dt      = personnel_dt,
      policy_params     = policy_params,
      ref_date          = ref_date,
      hire_date_col     = hire_date_col,
      personnel_id_col  = personnel_id_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      contract_type_col = contract_type_col,
      status_col        = status_col
    ),

    stop("Unknown hiring mode: ", mode, ". Must be 'flow', 'stock', 'combined', or 'status_quo'",
         call. = FALSE)
  )
  
  return(demand_dt)
}


#' Compute Hiring Summary Statistics
#'
#' @description
#' Aggregates key statistics about hiring including new hires count,
#' net headcount change, total headcount, and total salary cost impact.
#' Uses data.table for efficient computation.
#'
#' For hiring scenarios, `total_new_salary_cost` is the annual salary bill added
#' (sum of new hire salaries). For downsizing scenarios it is the annual salary
#' bill removed (sum of terminated worker salaries, reported as a negative number
#' to indicate savings). Mixed scenarios sum both.
#'
#' @param adjustment_dt data.table. Adjustment data with net_change column
#' @param new_hires_dt data.table. New hire personnel data (optional)
#' @param new_contracts_dt data.table. New hire contract data with salary (optional)
#' @param terminated_contracts_dt data.table. Terminated contract data with salary (optional)
#' @param total_headcount Integer. Current total headcount after adjustment
#'
#' @return data.table with summary statistics
#' @keywords internal
compute_hiring_summary <- function(adjustment_dt,
                                   new_hires_dt = NULL,
                                   new_contracts_dt = NULL,
                                   terminated_contracts_dt = NULL,
                                   total_headcount = NA_integer_) {
  
  # Calculate statistics
  n_new_hires <- if (!is.null(new_hires_dt)) nrow(new_hires_dt) else 0L
  net_change <- if (nrow(adjustment_dt) > 0) sum(adjustment_dt$net_change, na.rm = TRUE) else 0L
  
  salary_col <- "gross_salary_lcu"
  
  # New hire salary cost (positive)
  hire_cost <- if (!is.null(new_contracts_dt) && nrow(new_contracts_dt) > 0 &&
                   salary_col %in% names(new_contracts_dt)) {
    sum(new_contracts_dt[[salary_col]], na.rm = TRUE)
  } else {
    0
  }
  
  # Terminated salary savings (reported as negative — cost removed from payroll)
  downsize_cost <- if (!is.null(terminated_contracts_dt) && nrow(terminated_contracts_dt) > 0 &&
                        salary_col %in% names(terminated_contracts_dt)) {
    -sum(terminated_contracts_dt[[salary_col]], na.rm = TRUE)
  } else {
    0
  }
  
  # Combined: positive = net new cost, negative = net savings
  total_salary_impact <- hire_cost + downsize_cost
  new_salary_cost <- if (hire_cost == 0 && downsize_cost == 0) NA_real_ else total_salary_impact
  
  summary_tbl <- data.table::data.table(
    n_new_hires = as.integer(n_new_hires),
    net_headcount_change = as.integer(net_change),
    total_headcount = as.integer(total_headcount),
    total_new_salary_cost = new_salary_cost
  )
  
  return(summary_tbl)
}
