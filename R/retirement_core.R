#' Core Retirement Logic Functions
#'
#' @description
#' Functions for identifying retirement-eligible personnel and computing
#' retirement eligibility based on age and/or tenure.
#'
#' @import data.table
#' @name retirement_core
#' @keywords internal
NULL

#' Identify Personnel Eligible for Retirement
#'
#' @description
#' Determines which personnel are eligible for retirement based on age and/or
#' tenure criteria specified in policy parameters. Uses switch() to handle
#' different eligibility types efficiently.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data with birth dates
#' @param policy_params List. Policy parameters including:
#'   \itemize{
#'     \item eligibility_type: "age_only", "tenure_only", or "age_and_tenure"
#'     \item min_age: Minimum retirement age (required for age-based eligibility)
#'     \item min_tenure: Minimum years of service (required for tenure-based eligibility)
#'   }
#' @param ref_date Date. Reference date for eligibility calculation
#'
#' @return data.table with columns: personnel_id, retire (0/1), age, tenure_years
#' @keywords internal
identify_retirees <- function(contract_dt,
                              personnel_dt,
                              policy_params,
                              ref_date) {
  
  eligibility_type <- policy_params$eligibility_type
  
  # Compute age if needed
  if (eligibility_type %in% c("age_only", "age_and_tenure")) {
    age_dt <- compute_age(
      personnel_dt = personnel_dt,
      ref_date = ref_date,
      birth_date_col = "birth_date",
      personnel_id_col = "personnel_id"
    )
  } else {
    # Create placeholder with NA age
    age_dt <- data.table::data.table(
      personnel_id = unique(personnel_dt$personnel_id),
      age = NA_real_
    )
  }
  
  # Compute tenure if needed
  if (eligibility_type %in% c("tenure_only", "age_and_tenure")) {
    tenure_dt <- compute_tenure(
      contract_dt = contract_dt,
      ref_date = ref_date,
      personnel_id_col = "personnel_id",
      start_date_col = "start_date",
      end_date_col = "end_date",
      contract_type_col = "contract_type_code"
    )
  } else {
    # Create placeholder with NA tenure
    tenure_dt <- data.table::data.table(
      personnel_id = unique(contract_dt$personnel_id),
      tenure_years = NA_real_,
      tenure_days = NA_real_
    )
  }
  
  # Merge age and tenure
  eligibility_dt <- merge(
    age_dt,
    tenure_dt,
    by = "personnel_id",
    all = TRUE
  )
  
  # Determine eligibility using switch
  eligibility_dt[, retire := switch(
    eligibility_type,
    
    "age_only" = as.integer(age >= policy_params$min_age),
    
    "tenure_only" = as.integer(tenure_years >= policy_params$min_tenure),
    
    "age_and_tenure" = as.integer(
      age >= policy_params$min_age & 
      tenure_years >= policy_params$min_tenure
    ),
    
    stop("Unknown eligibility type: ", eligibility_type, call. = FALSE)
  )]
  
  # Handle NA values (set to 0 - not eligible)
  eligibility_dt[is.na(retire), retire := 0L]
  
  return(eligibility_dt[, .(personnel_id, retire, age, tenure_years)])
}


#' Compute Retirement Summary Statistics
#'
#' @description
#' Aggregates key statistics about retirees including count, total pension cost,
#' average age, and average tenure.
#'
#' @param retirees_dt data.table. Retiree data with pension amounts
#' @param contract_dt data.table. Original contract data (for wage bill comparison)
#'
#' @return data.table with summary statistics
#' @keywords internal
compute_retirement_summary <- function(retirees_dt, contract_dt = NULL) {
  
  # Handle case of no retirees
  if (nrow(retirees_dt) == 0) {
    summary_tbl <- data.table::data.table(
      n_retired = 0L,
      total_pension = 0,
      avg_pension = NA_real_,
      avg_age = NA_real_,
      avg_tenure = NA_real_
    )
    return(summary_tbl)
  }
  
  # Compute summary statistics
  summary_tbl <- data.table::data.table(
    n_retired = nrow(retirees_dt),
    total_pension = sum(retirees_dt$pension, na.rm = TRUE),
    avg_pension = mean(retirees_dt$pension, na.rm = TRUE),
    avg_age = mean(retirees_dt$age, na.rm = TRUE),
    avg_tenure = mean(retirees_dt$tenure_years, na.rm = TRUE)
  )
  
  return(summary_tbl)
}


#' Prepare Retiree Data for Pension Calculation
#'
#' @description
#' Enriches retiree eligibility data with salary and contract information
#' needed for pension calculations. Merges age/tenure with contract data.
#'
#' @param eligibility_dt data.table. Output from identify_retirees()
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param ref_date Date. Reference date
#'
#' @return data.table with retiree information ready for pension calculation
#' @keywords internal
prepare_retiree_data <- function(eligibility_dt,
                                 contract_dt,
                                 personnel_dt,
                                 ref_date) {
  
  # Filter to eligible retirees only
  retirees_only <- eligibility_dt[retire == 1]
  
  # If no retirees, return empty
  if (nrow(retirees_only) == 0) {
    return(data.table::data.table())
  }
  
  # Get active contracts for these retirees
  active_contracts <- get_active_contracts(
    contract_dt = contract_dt,
    ref_date = ref_date,
    start_date_col = "start_date",
    end_date_col = "end_date",
    contract_type_col = "contract_type_code"
  )
  
  # Filter to retirees only
  retiree_contracts <- active_contracts[personnel_id %in% retirees_only$personnel_id]
  
  # Get primary contract for each retiree
  primary_contracts <- get_primary_contract(
    contract_dt = retiree_contracts,
    personnel_id_col = "personnel_id",
    contract_id_col = "contract_id",
    start_date_col = "start_date",
    salary_col = "gross_salary_lcu"
  )
  
  # Merge with eligibility data (age, tenure)
  retirees_dt <- merge(
    primary_contracts,
    retirees_only[, .(personnel_id, age, tenure_years)],
    by = "personnel_id",
    all.x = TRUE
  )
  
  return(retirees_dt)
}
