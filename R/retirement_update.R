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
#' Updates contract_dt to reflect retirements: ALL active contracts for each
#' retiring person are marked "pensioner", their salaries are zeroed, and their
#' end_date is set to ref_date.  A plain membership filter on personnel_id
#' (no join) ensures every contract row for a retiring person is captured,
#' regardless of how many simultaneous contracts they hold.
#'
#' Pension cost is tracked separately in the pensioner_register via
#' pension_amount; zeroing salary here prevents double-counting.
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

  # Unique set of retiring personnel — plain membership filter, no join needed
  retiring_ids <- unique(retirees_dt[[personnel_id_col]])

  # Mark ALL active contracts for retiring persons as "pensioner" in a single pass.
  # Using %in% ensures every contract row is captured even when a person holds
  # multiple simultaneous active contracts.
  # Inactive/terminated/closed contracts are left untouched.
  active_types_to_update <- c("inactive", "pensioner", "closed_due_to_retirement",
                               "terminated")
  contract_dt[
    get(personnel_id_col) %in% retiring_ids &
      !get(contract_type_col) %in% active_types_to_update,
    c(contract_type_col, end_date_col, salary_col) := list("pensioner", ref_date, 0)
  ]

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
