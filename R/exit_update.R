#' State Update Functions for Non-Retirement Exits
#'
#' @description
#' Functions for updating \code{contract_dt} and \code{personnel_dt} after
#' non-retirement attrition events (voluntary resignation, dismissal,
#' contract non-renewal).  Mirrors \code{retirement_update.R} structure.
#'
#' @import data.table
#' @name exit_update
#' @keywords internal
NULL


#' Update Contracts for Non-Retirement Exits
#'
#' @description
#' Marks ALL active contracts for each exiting person as \code{exited_type}
#' and sets \code{end_date_col} to \code{ref_date}.  Uses a plain membership
#' filter on \code{personnel_id} so that multi-contract persons are handled
#' correctly in one pass.
#'
#' @param contract_dt data.table.  Contract data (modified by reference).
#' @param exits_dt data.table.  Table of exiting personnel — must contain
#'   \code{personnel_id_col}.
#' @param ref_date Date.  Reference date; becomes the exit date.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_type_col Character.  Default \code{"contract_type_code"}.
#' @param end_date_col Character.  Default \code{"end_date"}.
#' @param active_types Character vector.  Contract type values to update.
#' @param exited_type Character.  Value to write into \code{contract_type_col}.
#'   Default \code{"inactive"}.
#'
#' @return \code{contract_dt} (invisibly; modified by reference in place).
#' @section Data Integrity:
#'   Modifies \code{contract_dt} **in place** via data.table reference semantics.
#'   The caller's table is changed; pass \code{data.table::copy()} if the
#'   original must be preserved.
#' @keywords internal
update_contracts_for_exits <- function(contract_dt,
                                       exits_dt,
                                       ref_date,
                                       personnel_id_col  = "personnel_id",
                                       contract_type_col = "contract_type_code",
                                       end_date_col      = "end_date",
                                       active_types      = "active",
                                       exited_type       = "inactive") {
  if (is.null(exits_dt) || nrow(exits_dt) == 0L) return(invisible(contract_dt))

  exiting_ids <- unique(exits_dt[[personnel_id_col]])

  contract_dt[
    get(personnel_id_col) %in% exiting_ids &
      get(contract_type_col) %in% active_types,
    (contract_type_col) := exited_type
  ]
  contract_dt[
    get(personnel_id_col) %in% exiting_ids &
      get(contract_type_col) == exited_type,
    (end_date_col) := as.Date(ref_date)
  ]

  invisible(contract_dt)
}


#' Update Personnel Status for Non-Retirement Exits
#'
#' @description
#' Sets \code{status_col} to \code{"inactive"} for all exiting personnel.
#'
#' @param personnel_dt data.table.  Personnel data (modified by reference).
#' @param exits_dt data.table.  Table of exiting personnel.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param status_col Character.  Default \code{"status"}.
#'
#' @return \code{personnel_dt} (invisibly; modified by reference in place).
#' @section Data Integrity:
#'   Modifies \code{personnel_dt} **in place** via data.table reference semantics.
#'   The caller's table is changed; pass \code{data.table::copy()} if the
#'   original must be preserved.
#' @keywords internal
update_personnel_for_exits <- function(personnel_dt,
                                       exits_dt,
                                       personnel_id_col = "personnel_id",
                                       status_col       = "status") {
  if (is.null(exits_dt) || nrow(exits_dt) == 0L) return(invisible(personnel_dt))

  exiting_ids <- unique(exits_dt[[personnel_id_col]])
  personnel_dt[get(personnel_id_col) %in% exiting_ids,
               (status_col) := "inactive"]

  invisible(personnel_dt)
}


#' Compute Non-Retirement Exit Effect on Wage Bill
#'
#' @description
#' Returns the wage-bill reduction from non-retirement exits as a positive
#' number (savings).  Mirrors \code{compute_exit_effect()} in
#' \code{retirement_core.R}.
#'
#' @param exits_dt data.table or \code{NULL}.  Table returned by
#'   \code{compute_status_quo_exits()} after the salary join.
#' @param salary_col Character.  Name of the salary column.  Default
#'   \code{"gross_salary_lcu"}.
#'
#' @return Numeric scalar.
#' @keywords internal
compute_non_retirement_exit_effect <- function(exits_dt,
                                                salary_col = "gross_salary_lcu") {
  if (is.null(exits_dt) || nrow(exits_dt) == 0L) return(0)
  if (!salary_col %in% names(exits_dt))           return(0)
  sum(exits_dt[[salary_col]], na.rm = TRUE)
}
