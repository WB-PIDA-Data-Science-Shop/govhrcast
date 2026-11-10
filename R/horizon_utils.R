# Internal helpers used only by simulate_scenario() / simulate_horizon().

# Internal helper: tells the user once, per module, which groups on the payroll
# have no row in a rate table, so a frozen group is not a silent one.
.report_groups_without_rate <- function(module, consequence, rate_table, group_cols,
                                        contract_dt, contract_type_col) {
  group_cols <- setdiff(group_cols, "age_yr_int")
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

# Internal helper: the pensioner register before anyone is on it. One row per
# pension contract, keyed by the caller's person and contract ID columns; the
# amount is this year's pension, grown by the pension COLA every year.
.empty_pensioner_register <- function(personnel_id_col, contract_id_col) {
  register <- data.table::data.table(
    .id            = character(0),
    .cid           = character(0),
    pension_amount = numeric(0)
  )
  data.table::setnames(register, c(".id", ".cid"), c(personnel_id_col, contract_id_col))
}

# Internal helper: your own rows up to baseline_date, unchanged, which the returned
# tables continue (ADR 0017). A single snapshot without a date column is dated at
# baseline_date.
.history_up_to <- function(dt, baseline_date, ref_date_col) {
  dt <- data.table::as.data.table(dt)
  if (ref_date_col %in% names(dt)) return(dt[get(ref_date_col) <= baseline_date])
  data.table::copy(dt)[, (ref_date_col) := baseline_date][]
}

# Internal helper: history followed by the simulated years, with your columns in
# your order. Columns the simulation adds for its own work are left out, except
# `also_keep` (age and tenure, which it fills in when your data lack them): those
# come last, blank on the history rows.
.continue_table <- function(history, years, also_keep = character(0)) {
  all_rows <- data.table::rbindlist(c(list(history), years), use.names = TRUE, fill = TRUE)
  all_rows[, c(names(history), setdiff(intersect(also_keep, names(all_rows)), names(history))), with = FALSE]
}
