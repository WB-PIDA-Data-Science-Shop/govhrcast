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
#' stamped to \code{as_of_date}, and their salary is zeroed.
#'
#' @param contract_dt data.table.  Contract register (modified in place by
#'   \code{:=}).  Required columns: \code{personnel_id_col},
#'   \code{contract_type_col}, \code{end_date_col}, \code{salary_col}.
#' @param retirees_dt data.table.  Table of confirmed retirees (\code{retire == 1})
#'   produced by \code{\link{prepare_retiree_data}}.  Only the
#'   \code{personnel_id_col} column is used here.
#' @param as_of_date Date.  The simulation period date.  Stamped as
#'   \code{end_date_col} on all closing contracts.
#' @param column_spec_args Complete column specification
#'   (\code{define_column_spec()}), in the caller's column names. Keys read:
#'   \code{personnel_id_col} (in both tables); \code{end_date_col} (set to
#'   \code{as_of_date} for all closing contracts); \code{salary_col} (set to
#'   \code{0} to prevent double-counting: pension cost is tracked separately
#'   via \code{pension_amount} in the pensioner register);
#'   \code{contract_type_col} (set to \code{"pensioner"} for closing
#'   contracts; contracts already off the payroll are skipped).
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
                                          as_of_date,
                                          column_spec_args = define_column_spec()) {
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

  # Mark every contract a retiring person holds on the payroll as "pensioner", in
  # a single pass, even when they hold several. Rows already off the payroll are
  # left untouched.
  contract_dt[
    get(personnel_id_col) %in% retiring_ids &
      .on_payroll(get(contract_type_col)),
    c(contract_type_col, end_date_col, salary_col) := list("pensioner", as_of_date, 0)
  ]

  return(contract_dt)
}


#' Update Personnel Status for Retirees
#'
#' @description
#' Sets \code{status_col} to \code{"pensioner"} in \code{personnel_dt} for
#' every person in \code{retirees_dt}. It goes by who retired this period, not
#' by who holds a pension row, because someone can hold a pension and a working
#' contract without retiring from the job.
#'
#' @param personnel_dt data.table.  Personnel register (modified in place).
#'   Required columns: \code{personnel_id_col}, \code{status_col}.
#' @param retirees_dt data.table.  Table of confirmed retirees.  Only
#'   \code{personnel_id_col} is used.
#' @param personnel_id_col Character.  Personnel identifier column in both
#'   tables.  (default: \code{"personnel_id"}).
#' @param status_col Character.  Employment status column in
#'   \code{personnel_dt}.  (default: \code{"employment_status"}).
#'
#' @section Data Integrity:
#' \code{personnel_dt} is modified in place via \code{data.table} \code{:=}.
#' The caller's object is mutated.  \code{\link{simulate_retirement}} passes a
#' deep copy so the caller's original is unaffected.
#'
#' @return \code{personnel_dt} (invisibly; the same object, modified in place).
#' @keywords internal
update_personnel_for_retirees <- function(personnel_dt,
                                          retirees_dt,
                                          personnel_id_col = "personnel_id",
                                          status_col       = "employment_status") {
  personnel_dt[get(personnel_id_col) %in% retirees_dt[[personnel_id_col]],
               (status_col) := "pensioner"]
  invisible(personnel_dt)
}
