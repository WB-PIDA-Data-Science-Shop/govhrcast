#' Utility Helper Functions
#'
#' @description
#' Collection of utility functions used across all simulation modules.
#' Includes date calculations, tenure computation, and other common operations.
#'
#' @import data.table
#' @name utils
#' @keywords internal
NULL

#' Calculate Years Between Two Dates
#'
#' @description
#' Vectorized function to compute time elapsed in years between two date vectors.
#' Uses 365.25 days per year to account for leap years.
#'
#' @param start_date Date vector. Start dates
#' @param end_date Date vector. End dates
#'
#' @return Numeric vector of years elapsed
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' compute_years(as.Date("2000-01-01"), as.Date("2025-01-01"))
#' # Returns: 25
#' }
compute_years <- function(start_date, end_date) {
  days_diff <- as.numeric(difftime(end_date, start_date, units = "days"))
  years <- days_diff / 365.25
  
  return(years)
}


#' Calculate Age from Birth Date
#'
#' @description
#' Computes age in years for each personnel at a reference date.
#'
#' @param personnel_dt data.table. Personnel data
#' @param ref_date Date. Reference date for age calculation
#' @param birth_date_col Character. Name of birth date column (default: "birth_date")
#' @param personnel_id_col Character. Name of personnel ID column (default: "personnel_id")
#'
#' @return data.table with personnel_id and age columns
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' age_dt <- compute_age(
#'   personnel_dt = personnel_data,
#'   ref_date = as.Date("2025-01-01"),
#'   birth_date_col = "birth_date"
#' )
#' }
compute_age <- function(personnel_dt,
                        ref_date,
                        birth_date_col = "birth_date",
                        personnel_id_col = "personnel_id") {
  
  # Compute age directly without copying
  # Return only ID and age - no modification to input
  age_dt <- personnel_dt[, .(
    personnel_id = get(personnel_id_col),
    age = compute_years(start_date = get(birth_date_col), end_date = ref_date)
  )]
  
  return(age_dt)
}


#' Calculate Tenure from Contract History
#'
#' @description
#' Computes total years of service for each personnel as of a reference date.
#' Handles panel/time-series data by deduplicating contracts using contract_id + start_date.
#' For each unique contract, calculates time between start_date and the earlier of
#' (end_date, ref_date), then sums across all contracts per personnel.
#'
#' @param contract_dt data.table. Contract data (may contain panel observations)
#' @param ref_date Date. Reference date for tenure calculation
#' @param personnel_id_col Character. Name of personnel ID column (default: "personnel_id")
#' @param contract_id_col Character. Name of contract ID column (default: "contract_id")
#' @param start_date_col Character. Name of start date column (default: "start_date")
#' @param end_date_col Character. Name of end date column (default: "end_date")
#' @param contract_type_col Character. Name of contract type column (default: "contract_type_code")
#'
#' @return data.table with personnel_id, tenure_years, and tenure_days columns
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' tenure_dt <- compute_tenure(
#'   contract_dt = contract_data,
#'   ref_date = as.Date("2025-01-01")
#' )
#' }
compute_tenure <- function(contract_dt,
                           ref_date,
                           personnel_id_col = "personnel_id",
                           contract_id_col = "contract_id",
                           start_date_col = "start_date",
                           end_date_col = "end_date",
                           contract_type_col = "contract_type_code") {
  
  # Filter out inactive contracts and pensioners (read-only operation, no copy needed)
  active_dt <- contract_dt[!get(contract_type_col) %in% c("inactive", "pensioner")]
  
  # Deduplicate panel observations: one row per unique contract
  # Use contract_id + start_date as the unique key
  # Select only the needed columns to avoid carrying forward panel ref_date
  unique_contracts <- active_dt[, .SD[1], 
                                by = c(contract_id_col, start_date_col)]
  
  # Keep only essential columns for tenure calculation
  keep_cols <- c(personnel_id_col, contract_id_col, start_date_col, end_date_col)
  unique_contracts <- unique_contracts[, ..keep_cols]
  
  # For each contract, determine the effective end date as of ref_date
  # If contract ended before ref_date, use end_date; otherwise use ref_date
  unique_contracts[, effective_end := fifelse(
    is.na(get(end_date_col)) | get(end_date_col) > ref_date,
    ref_date,
    get(end_date_col)
  )]
  
  # Only count contracts that started on or before ref_date
  unique_contracts <- unique_contracts[get(start_date_col) <= ref_date]
  
  # Calculate contract duration in days
  unique_contracts[, contract_days := as.numeric(
    difftime(effective_end, get(start_date_col), units = "days")
  )]
  
  # Ensure non-negative durations
  unique_contracts[contract_days < 0, contract_days := 0]
  
  # Sum total days per personnel
  tenure_dt <- unique_contracts[, 
    .(tenure_days = sum(contract_days, na.rm = TRUE)),
    by = .(personnel_id = get(personnel_id_col))
  ]
  
  # Convert to years
  tenure_dt[, tenure_years := tenure_days / 365.25]
  
  return(tenure_dt)
}


#' Get Active Contracts at Reference Date
#'
#' @description
#' Filters contracts that are active at a given reference date.
#' A contract is active if it has started and has not ended before ref_date.
#'
#' @param contract_dt data.table. Contract data
#' @param ref_date Date. Reference date
#' @param start_date_col Character. Name of start date column (default: "start_date")
#' @param end_date_col Character. Name of end date column (default: "end_date")
#' @param contract_type_col Character. Name of contract type column (default: "contract_type_code")
#'
#' @return data.table of active contracts
#' @keywords internal
get_active_contracts <- function(contract_dt,
                                 ref_date,
                                 start_date_col = "start_date",
                                 end_date_col = "end_date",
                                 contract_type_col = "contract_type_code") {
  
  # Filter: started before/on ref_date AND (no end date OR ended after ref_date) AND not inactive
  active <- contract_dt[
    get(start_date_col) <= ref_date &
    (is.na(get(end_date_col)) | get(end_date_col) >= ref_date) &
    get(contract_type_col) != "inactive"
  ]
  
  return(active)
}


#' Get Primary Contract for Each Personnel
#'
#' @description
#' For personnel with multiple active contracts, selects the primary contract
#' based on priority: (1) latest start_date, (2) highest salary, (3) lowest contract_id.
#'
#' @param contract_dt data.table. Contract data (should be pre-filtered for active contracts)
#' @param personnel_id_col Character. Name of personnel ID column (default: "personnel_id")
#' @param contract_id_col Character. Name of contract ID column (default: "contract_id")
#' @param start_date_col Character. Name of start date column (default: "start_date")
#' @param salary_col Character. Name of salary column for prioritization (default: "gross_salary_lcu")
#'
#' @return data.table with one row per personnel (their primary contract)
#' @keywords internal
get_primary_contract <- function(contract_dt,
                                 personnel_id_col = "personnel_id",
                                 contract_id_col = "contract_id",
                                 start_date_col = "start_date",
                                 salary_col = "gross_salary_lcu") {
  
  # Order by priority: start_date DESC, salary DESC, contract_id ASC
  data.table::setorderv(
    contract_dt,
    cols = c(start_date_col, salary_col, contract_id_col),
    order = c(-1, -1, 1)  # DESC, DESC, ASC
  )
  
  # Take first row per personnel (highest priority)
  primary <- contract_dt[, .SD[1], by = .(personnel_id = get(personnel_id_col))]
  
  return(primary)
}


#' Generate New Personnel IDs
#'
#' @description
#' Creates unique personnel IDs for new hires using a timestamp-based format.
#'
#' @param n Integer. Number of IDs to generate
#' @param ref_date Date. Reference date for ID stamping
#' @param prefix Character. Prefix for IDs (default "NEW")
#'
#' @return Character vector of new IDs in format: PREFIX_YYYYMMDD_N
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' generate_new_ids(3, as.Date("2025-01-15"))
#' # Returns: c("NEW_20250115_1", "NEW_20250115_2", "NEW_20250115_3")
#' }
generate_new_ids <- function(n, ref_date, prefix = "NEW") {
  ref_date_str <- gsub("-", "", as.character(ref_date))
  new_ids <- paste0(prefix, "_", ref_date_str, "_", seq_len(n))
  
  return(new_ids)
}


#' Create Empty Event Data Table
#'
#' @description
#' Creates an empty data.table with standard event structure.
#' Used when no events occur in a simulation step.
#'
#' @param event_type Character. Type of event (e.g., "retirement", "hire")
#'
#' @return Empty data.table with standard event columns
#' @keywords internal
create_empty_events <- function(event_type = "event") {
  empty_events <- data.table::data.table(
    personnel_id = character(0),
    event_type = character(0),
    event_date = as.Date(character(0))
  )
  
  return(empty_events)
}


#' Format Date as YYYYMMDD String
#'
#' @description
#' Converts dates to compact string format for IDs and timestamps.
#'
#' @param date Date vector
#'
#' @return Character vector in YYYYMMDD format
#' @keywords internal
format_date_stamp <- function(date) {
  date_str <- gsub("-", "", as.character(date))
  
  return(date_str)
}
