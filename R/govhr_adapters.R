# Calls into govhr functions that fix the names of two columns.
#
# ADR 0012 keeps the caller's column names throughout the package and renames
# nothing. These two wrappers are the only exception: govhr::detect_movement()
# and govhr::add_contract_to_event() always look for columns called
# `personnel_id` and `ref_date`, and take no argument to say otherwise. So each
# wrapper hands govhr a small copy of just the columns it reads, under govhr's
# names, and renames the two columns back on the way out. The caller's tables
# are never touched.
#
# Delete this file once govhr takes the two names as arguments.

.GOVHR_ID_COLS <- c("personnel_id", "ref_date")

#' Detect Hires and Separations Under the Caller's Column Names
#'
#' @param panel_personnel_dt data.table.  Personnel panel (all snapshots).
#' @param personnel_id_col,ref_date_col,status_col Character.  The caller's
#'   names for the person ID, the snapshot date and the employment status.
#' @return The \code{govhr::detect_movement()} table, with its ID and date
#'   columns under the caller's names.
#' @noRd
.govhr_detect_movement <- function(panel_personnel_dt, personnel_id_col, ref_date_col, status_col) {
  caller_ids <- c(personnel_id_col, ref_date_col)
  personnel <- panel_personnel_dt[, c(caller_ids, status_col), with = FALSE]
  data.table::setnames(personnel, caller_ids, .GOVHR_ID_COLS)

  events <- govhr::detect_movement(personnel, status_col = status_col)
  data.table::setnames(events, .GOVHR_ID_COLS, caller_ids)[]
}

#' Attach Contract Columns to Events Under the Caller's Column Names
#'
#' @param events data.table.  Events carrying the caller's ID and date columns.
#' @param panel_contract_dt data.table.  Contract panel (all snapshots).
#' @param keep_vars Character vector.  Contract columns to attach.
#' @param personnel_id_col,ref_date_col Character.  The caller's names for the
#'   person ID and the snapshot date.
#' @return \code{events} with \code{keep_vars} attached, one row per event and
#'   contract value, under the caller's names.
#' @noRd
.govhr_add_contract_to_event <- function(events, panel_contract_dt, keep_vars,
                                         personnel_id_col, ref_date_col) {
  caller_ids <- c(personnel_id_col, ref_date_col)
  contracts <- panel_contract_dt[, c(caller_ids, keep_vars), with = FALSE]
  data.table::setnames(contracts, caller_ids, .GOVHR_ID_COLS)
  events <- data.table::setnames(data.table::copy(events), caller_ids, .GOVHR_ID_COLS)

  joined <- govhr::add_contract_to_event(events = events, contracts = contracts,
                                         keep_vars = keep_vars)
  data.table::setnames(joined, .GOVHR_ID_COLS, caller_ids)[]
}
