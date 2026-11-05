#' Update the Allowance Register with This Period's Resolved Amounts
#'
#' @description
#' Appends this period's resolved per-contract, per-type allowance amounts
#' to the accumulated \code{allowance_dt} ledger, stamped with
#' \code{period_date}. Mirrors the accumulate-by-period pattern already used
#' for \code{pensioner_register}: past periods are never rewritten, and a
#' contract that becomes inactive simply stops appearing in future periods'
#' \code{resolved_amounts_dt} (\code{\link{resolve_allowance_amounts}}
#' already excludes pensioner/inactive contracts upstream).
#'
#' @param allowance_dt data.table or \code{NULL}. Accumulated ledger from
#'   prior periods. \code{NULL} initializes the ledger with this period's
#'   rows.
#' @param resolved_amounts_dt data.table. This period's resolved amounts, as
#'   returned by \code{\link{resolve_allowance_amounts}}.
#' @param ref_date Date. Stamped onto the new rows as \code{period_date},
#'   matching \code{pensioner_register}'s column (the parameter itself is
#'   named \code{ref_date} for consistency with
#'   \code{simulate_retirement()}/\code{simulate_exits()}/
#'   \code{simulate_promotions_transfers()}/\code{simulate_hiring()}, which
#'   all take the current period's date under this same name).
#'
#' @return Updated \code{allowance_dt}.
#' @keywords internal
update_allowance_register <- function(allowance_dt, resolved_amounts_dt, ref_date) {

  if (!data.table::is.data.table(resolved_amounts_dt))
    stop("resolved_amounts_dt must be a data.table.", call. = FALSE)

  new_rows <- data.table::copy(resolved_amounts_dt)
  new_rows[, period_date := ref_date]

  if (is.null(allowance_dt)) return(new_rows)

  if (!data.table::is.data.table(allowance_dt))
    stop("allowance_dt must be a data.table or NULL.", call. = FALSE)

  data.table::rbindlist(list(allowance_dt, new_rows), use.names = TRUE, fill = TRUE)
}
