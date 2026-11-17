# Internal helpers used only by simulate_scenario() / simulate_horizon().

#' Say Which Payroll Groups Have No Rate
#'
#' @description
#' Called once per module by `prepare_horizon_inputs()`, so a group that a rate
#' table leaves out (and that therefore never exits, moves or retires by rate)
#' is reported rather than frozen silently. Groups are compared by the rate
#' table's keys other than whole-year age and tenure, which are not on
#' contracts. Prints a message naming up to five such groups; prints nothing
#' when every group has a rate, when there is no table, or when the table has
#' no other keys.
#'
#' @param module Character. The module's name, for the message.
#' @param consequence Character. What happens to those groups, for the message.
#' @param rate_table data.table or `NULL`. The module's rate table.
#' @param group_cols Character. The rate table's keys.
#' @param contract_dt data.table. The contracts at the baseline date.
#' @param contract_type_col Character. The contract type column, to find who is
#'   on the payroll.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
.report_groups_without_rate <- function(module, consequence, rate_table, group_cols,
                                        contract_dt, contract_type_col) {
  # Whole-year age and tenure are not on contracts: the check is by the other keys
  group_cols <- setdiff(group_cols, c("age_yr_int", "tenure_yr_int"))
  if (is.null(rate_table) || length(group_cols) == 0L) return(invisible(NULL))

  group_key <- function(dt) do.call(paste, c(dt[, group_cols, with = FALSE], sep = "||"))
  staffed <- stats::na.omit(
    unique(contract_dt[.on_payroll(get(contract_type_col)), group_cols, with = FALSE])
  )
  rated <- if ("from_group" %in% names(rate_table)) rate_table$from_group else group_key(rate_table)
  missing_groups <- setdiff(group_key(staffed), rated)

  if (length(missing_groups) > 0L) {
    shown <- paste(utils::head(missing_groups, 5L), collapse = ", ")
    if (length(missing_groups) > 5L) shown <- paste0(shown, ", ...")
    message("simulate_horizon: ", module, ": ", length(missing_groups),
            " group(s) on the payroll have no rate in the table (", shown, "). ", consequence)
  }
  invisible(NULL)
}

# Internal helper: the latest snapshot date that both tables hold, which is where
# a projection starts when the caller names no date. Personnel may have no date
# column, in which case the contract table decides. Stops if no date is found.
.latest_snapshot <- function(contract_dt, personnel_dt, ref_date_col) {
  dates <- contract_dt[[ref_date_col]]
  if (ref_date_col %in% names(personnel_dt)) {
    dates <- dates[dates %in% personnel_dt[[ref_date_col]]]
  }
  if (all(is.na(dates))) {
    stop(
      "simulate_horizon: found no snapshot date in '", ref_date_col, "' that both ",
      "contract_dt and personnel_dt hold, so the projection has no starting point. ",
      "Pass baseline_date, the date your data describe.",
      call. = FALSE
    )
  }
  as.Date(max(dates, na.rm = TRUE))
}

# Internal helper: the pensioner register before anyone is on it, keyed by the
# caller's person ID column.
.empty_pensioner_register <- function(personnel_id_col) {
  register <- data.table::data.table(
    .id                        = character(0),
    pension_amount             = numeric(0),   # COLA-adjusted each period
    final_salary               = numeric(0),   # salary at moment of retirement
    tenure_years_at_retirement = numeric(0),
    age_at_retirement          = numeric(0),
    retire_reason              = character(0), # "rule" or "status_quo"; blank for people retired before the projection
    period_date                = as.Date(character(0))
  )
  data.table::setnames(register, ".id", personnel_id_col)
}

# ---------------------------------------------------------------------------
# Helper: advance Date by n units (year / month / day)
# ---------------------------------------------------------------------------
.advance_period <- function(date, n = 1L, unit = "year") {
  switch(
    unit,
    year  = seq(date, by = "year",  length.out = n + 1L)[n + 1L],
    month = seq(date, by = "month", length.out = n + 1L)[n + 1L],
    day   = date + as.integer(n),
    stop(".advance_period: unknown unit '", unit, "'", call. = FALSE)
  )
}
