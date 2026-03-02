#' State Update Functions for Retirement
#'
#' @description
#' Functions for updating contract_dt and personnel_dt after retirement events.
#' Handles primary contract selection and status updates.
#'
#' @import data.table
#' @name retirement_update
#' @keywords internal
NULL

#' Update Contracts for Retirees
#'
#' @description
#' Updates contract_dt to reflect retirements by:
#' 1. Identifying active contracts for retirees
#' 2. Selecting primary contract (latest start, highest salary, lowest ID)
#' 3. Setting primary contract to "pensioner" status
#' 4. Setting other contracts to "closed_due_to_retirement" status
#' 5. Setting end_date to ref_date for all affected contracts
#'
#' Uses efficient data.table operations without loops.
#'
#' @param contract_dt data.table. Contract data
#' @param retirees_dt data.table. Retiree data with personnel_id
#' @param ref_date Date. Reference date for retirement
#' @param personnel_id_col Character. Personnel ID column name (default: "personnel_id")
#' @param contract_id_col Character. Contract ID column name (default: "contract_id")
#' @param start_date_col Character. Start date column name (default: "start_date")
#' @param end_date_col Character. End date column name (default: "end_date")
#' @param salary_col Character. Salary column for prioritization (default: "gross_salary_lcu")
#' @param contract_type_col Character. Contract type column name (default: "contract_type_code")
#'
#' @return Updated contract_dt with retirement status changes
#' @keywords internal
update_contracts_for_retirees <- function(contract_dt,
                                          retirees_dt,
                                          ref_date,
                                          personnel_id_col = "personnel_id",
                                          contract_id_col = "contract_id",
                                          start_date_col = "start_date",
                                          end_date_col = "end_date",
                                          salary_col = "gross_salary_lcu",
                                          contract_type_col = "contract_type_code") {
  
  # If no retirees, return unchanged
  if (nrow(retirees_dt) == 0) {
    return(contract_dt)
  }
  
  # Extract retiree IDs
  retiree_ids <- unique(retirees_dt[[personnel_id_col]])
  
  # Create retire flag in contract_dt
  contract_dt[, retire := fifelse(get(personnel_id_col) %in% retiree_ids, 1L, 0L)]
  
  # Identify active contracts
  contract_dt[, active_flag := fifelse(
    is.na(get(end_date_col)) & get(contract_type_col) != "inactive",
    1L,
    0L
  )]
  
  # Filter to active contracts of retirees
  retiree_active <- contract_dt[retire == 1L & active_flag == 1L]
  
  # If no active contracts to update, return unchanged
  if (nrow(retiree_active) == 0) {
    contract_dt[, c("retire", "active_flag") := NULL]
    return(contract_dt)
  }
  
  # Rank contracts within each personnel by priority
  # Priority: start_date DESC, salary DESC, contract_id ASC
  retiree_active[, priority_rank := frank(
    list(-as.numeric(get(start_date_col)), 
         -get(salary_col), 
         get(contract_id_col)),
    ties.method = "first"
  ), by = .(personnel_id = get(personnel_id_col))]
  
  # Identify primary contracts (rank = 1)
  primary_contract_ids <- retiree_active[priority_rank == 1][[contract_id_col]]
  
  # Update contract types and end dates
  contract_dt[get(contract_id_col) %in% primary_contract_ids & retire == 1L & active_flag == 1L,
     c(contract_type_col, end_date_col) := list("pensioner", ref_date)]

  # Zero out salary on primary pensioner contracts.
  # Pension cost is tracked separately in the pensioner_register via pension_amount.
  # Zeroing here ensures sum(salary_col) never double-counts pension obligations.
  contract_dt[get(contract_id_col) %in% primary_contract_ids & retire == 1L & active_flag == 1L,
     (salary_col) := 0]

  # Get non-primary active retiree contracts
  non_primary_contract_ids <- retiree_active[priority_rank > 1][[contract_id_col]]
  
  # Update non-primary contracts
  if (length(non_primary_contract_ids) > 0) {
    contract_dt[get(contract_id_col) %in% non_primary_contract_ids & retire == 1L & active_flag == 1L,
       c(contract_type_col, end_date_col) := list("closed_due_to_retirement", ref_date)]
    # Also zero salary on closed secondary contracts
    contract_dt[get(contract_id_col) %in% non_primary_contract_ids & retire == 1L & active_flag == 1L,
       (salary_col) := 0]
  }
  
  # Clean temporary columns
  contract_dt[, c("retire", "active_flag") := NULL]
  
  return(contract_dt)
}


#' Update Personnel Status for Retirees
#'
#' @description
#' Updates personnel_dt to set status = "inactive" for all personnel whose
#' contracts are now marked as "pensioner".
#'
#' @param personnel_dt data.table. Personnel data
#' @param contract_dt data.table. Updated contract data (after update_contracts_for_retirees)
#' @param personnel_id_col Character. Personnel ID column name (default: "personnel_id")
#' @param contract_type_col Character. Contract type column name (default: "contract_type_code")
#' @param status_col Character. Status column name (default: "status")
#'
#' @return Updated personnel_dt with retirement status changes
#' @keywords internal
update_personnel_for_retirees <- function(personnel_dt,
                                          contract_dt,
                                          personnel_id_col = "personnel_id",
                                          contract_type_col = "contract_type_code",
                                          status_col = "status") {
  
  # Get unique personnel_ids with pensioner contracts
  pensioner_ids <- unique(
    contract_dt[get(contract_type_col) == "pensioner"][[personnel_id_col]]
  )
  
  # Update status for pensioners
  if (length(pensioner_ids) > 0) {
    personnel_dt[get(personnel_id_col) %in% pensioner_ids, 
       (status_col) := "inactive"]
  }
  
  return(personnel_dt)
}
