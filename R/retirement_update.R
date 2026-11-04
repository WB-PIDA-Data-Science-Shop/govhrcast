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
#' Applies retirement state changes to \code{contract_dt} in a single
#' \code{:=} expression.  All open contracts held by retiring personnel are
#' simultaneously reclassified as \code{"pensioner"}, their closing date is
#' stamped to \code{ref_date}, and their salary is zeroed.
#'
#' @param contract_dt data.table.  Contract register (modified in place by
#'   \code{:=}).  Required columns: \code{personnel_id_col},
#'   \code{contract_type_col}, \code{end_date_col}, \code{salary_col}.
#' @param retirees_dt data.table.  Table of confirmed retirees (\code{retire == 1})
#'   produced by \code{\link{prepare_retiree_data}}.  Only the
#'   \code{personnel_id_col} column is used here.
#' @param ref_date Date.  The simulation period date.  Stamped as
#'   \code{end_date_col} on all closing contracts.
#' @param column_spec_args Complete column specification
#'   (\code{.column_spec()}), in the caller's column names. Keys read:
#'   \code{personnel_id_col} (in both tables); \code{end_date_col} (set to
#'   \code{ref_date} for all closing contracts); \code{salary_col} (set to
#'   \code{0} to prevent double-counting: pension cost is tracked separately
#'   via \code{pension_amount} in the pensioner register);
#'   \code{contract_type_col} (set to \code{"pensioner"} for closing
#'   contracts; contracts already in \code{c("inactive", "pensioner",
#'   "closed_due_to_retirement", "terminated")} are skipped).
#'
#' @section Data Integrity:
#' \code{contract_dt} is modified in place via \code{data.table} \code{:=}.
#' The caller's object is mutated.  Pass \code{data.table::copy(contract_dt)}
#' if the original must be preserved (\code{\link{simulate_retirement}} does
#' this automatically at entry).
#'
#' @return \code{contract_dt} (invisibly; the same object, modified in place).
#' @keywords internal
update_contracts_for_retirees <- function(contract_dt,
                                          retirees_dt,
                                          ref_date,
                                          column_spec_args = .column_spec()) {
  personnel_id_col  <- column_spec_args$personnel_id_col
  end_date_col      <- column_spec_args$end_date_col
  salary_col        <- column_spec_args$salary_col
  contract_type_col <- column_spec_args$contract_type_col

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
#' Sets \code{status_col} to \code{"inactive"} in \code{personnel_dt} for
#' every person whose contracts have been reclassified as \code{"pensioner"}
#' by \code{\link{update_contracts_for_retirees}}.  Derives the retiree set
#' from \code{contract_dt} (not from the eligibility or pension tables) to
#' guarantee consistency with the already-committed contract state.
#'
#' @param personnel_dt data.table.  Personnel register (modified in place).
#'   Required columns: \code{personnel_id_col}, \code{status_col}.
#' @param contract_dt data.table.  Contract register \emph{after}
#'   \code{\link{update_contracts_for_retirees}} has been called.
#'   The function reads \code{contract_type_col} to derive retiring
#'   \code{personnel_id}s.
#' @param personnel_id_col Character.  Personnel identifier column in both
#'   \code{personnel_dt} and \code{contract_dt}.
#'   (default: \code{"personnel_id"}).
#' @param contract_type_col Character.  Contract classification column in
#'   \code{contract_dt}.  Rows with \code{contract_type_col == "pensioner"}
#'   identify retiring personnel.  (default: \code{"contract_type"}).
#' @param status_col Character.  Employment status column in
#'   \code{personnel_dt}.  Set to \code{"inactive"} for retirees.
#'   (default: \code{"status"}).
#'
#' @section Data Integrity:
#' \code{personnel_dt} is modified in place via \code{data.table} \code{:=}.
#' The caller's object is mutated.  \code{\link{simulate_retirement}} passes a
#' deep copy so the caller's original is unaffected.
#'
#' @return \code{personnel_dt} (invisibly; the same object, modified in place).
#' @keywords internal
update_personnel_for_retirees <- function(personnel_dt,
                                          contract_dt,
                                          personnel_id_col = "personnel_id",
                                          contract_type_col = "contract_type",
                                          status_col         = "employment_status") {
  
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
