#' Core Exit Logic Functions
#'
#' @description
#' Functions for estimating historical non-retirement attrition rates from
#' panel data, and for applying those rates to the current workforce to
#' select individuals to exit each simulation period.  Mirrors the structure
#' of \code{hiring_core.R}.
#'
#' @import data.table
#' @name exit_core
#' @keywords internal
NULL


# =============================================================================
# Phase 3a — estimate_historical_exit_rates()
# =============================================================================

#' Estimate Historical Non-Retirement Exit Rates from Panel Data
#'
#' @description
#' Uses \code{govhr::detect_personnel_event(event_type = "fire")} to identify
#' non-retirement attrition events (voluntary resignation, dismissal, contract
#' non-renewal) across the full historical panel.  Computes
#' \code{exit_rate = n_exits / n_active} per group per panel snapshot, then
#' returns the mean rate per group.
#'
#' @param panel_contract_dt data.table.  Full panel of contract data (all
#'   \code{ref_date} snapshots).
#' @param panel_personnel_dt data.table.  Full panel of personnel data.
#' @param group_cols Character vector or \code{NULL}.  Columns to group by
#'   (e.g. \code{"est_id"}).  Pass \code{NULL} for an overall (ungrouped) rate.
#' @param freq Character.  Frequency passed to
#'   \code{govhr::detect_personnel_event()}.  Default \code{"year"}.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param ref_date_col Character.  Default \code{"ref_date"}.
#' @param start_date_col Character.  Default \code{"start_date"}.
#' @param end_date_col Character.  Default \code{"end_date"}.
#' @param contract_type_col Character.  Default \code{"contract_type_code"}.
#' @param status_col Character.  Default \code{"status"}.
#'
#' @return data.table with \code{group_cols} (if specified) and
#'   \code{exit_rate} column.
#' @export
estimate_historical_exit_rates <- function(
    panel_contract_dt,
    panel_personnel_dt,
    group_cols        = NULL,
    freq              = "year",
    personnel_id_col  = "personnel_id",
    ref_date_col      = "ref_date",
    start_date_col    = "start_date",
    end_date_col      = "end_date",
    contract_type_col = "contract_type_code",
    status_col        = "status") {

  if (!data.table::is.data.table(panel_contract_dt))
    panel_contract_dt <- data.table::as.data.table(panel_contract_dt)
  if (!data.table::is.data.table(panel_personnel_dt))
    panel_personnel_dt <- data.table::as.data.table(panel_personnel_dt)

  panel_dates <- sort(unique(panel_personnel_dt[[ref_date_col]]))
  start_str   <- format(min(panel_dates, na.rm = TRUE))
  end_str     <- format(max(panel_dates, na.rm = TRUE))

  # Detect non-retirement exit events across the full panel
  fire_events <- govhr::detect_personnel_event(
    data       = panel_personnel_dt,
    id_col     = personnel_id_col,
    event_type = "fire",
    start_date = start_str,
    end_date   = end_str,
    freq       = freq
  )
  # fire_events columns: personnel_id_col, ref_date, type_event

  # Join to contract panel to retrieve group_cols per exit
  if (!is.null(group_cols) && length(group_cols) > 0) {
    contract_groups <- unique(
      panel_contract_dt[, c(personnel_id_col, ref_date_col, group_cols), with = FALSE]
    )
    fire_events <- contract_groups[fire_events, on = c(personnel_id_col, ref_date_col)]

    exit_counts <- fire_events[
      !is.na(get(group_cols[[1]])),
      .(n_exits = .N),
      by = c(ref_date_col, group_cols)
    ]
  } else {
    exit_counts <- fire_events[, .(n_exits = .N), by = ref_date_col]
  }

  # Compute active stock per snapshot per group (re-uses compute_current_stock)
  stock_list <- lapply(panel_dates, function(snap) {
    ct_snap <- panel_contract_dt[get(ref_date_col) == snap]
    pt_snap <- panel_personnel_dt[get(ref_date_col) == snap]
    ct_snap <- ct_snap[, !ref_date_col, with = FALSE]
    pt_snap <- pt_snap[, !ref_date_col, with = FALSE]

    s <- compute_current_stock(
      contract_dt       = ct_snap,
      personnel_dt      = pt_snap,
      ref_date          = snap,
      group_cols        = group_cols,
      personnel_id_col  = personnel_id_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      contract_type_col = contract_type_col,
      status_col        = status_col
    )
    s[, (ref_date_col) := snap]
    s
  })
  stock_dt <- data.table::rbindlist(stock_list, fill = TRUE)

  join_keys <- if (!is.null(group_cols) && length(group_cols) > 0)
    c(ref_date_col, group_cols) else ref_date_col

  rate_dt <- exit_counts[stock_dt, on = join_keys]
  rate_dt[is.na(n_exits), n_exits := 0L]
  rate_dt[, exit_rate := data.table::fifelse(
    current_stock > 0,
    n_exits / current_stock,
    0
  )]

  if (!is.null(group_cols) && length(group_cols) > 0) {
    result <- rate_dt[, .(exit_rate = mean(exit_rate, na.rm = TRUE)), by = group_cols]
  } else {
    result <- data.table::data.table(exit_rate = mean(rate_dt$exit_rate, na.rm = TRUE))
  }

  result
}


# =============================================================================
# Phase 3b — compute_status_quo_exits()
# =============================================================================

#' Select Personnel for Non-Retirement Exit Under Status-Quo Policy
#'
#' @description
#' Applies historical exit rates to the current workforce by group.
#' Returns a data.table of personnel IDs selected to exit this period.
#' Selection within each group follows \code{exit_strategy} (random or any
#' numeric column in \code{contract_dt}).
#'
#' @param contract_dt data.table.  Current (single-snapshot) contract data.
#' @param exit_rates_dt data.table.  Output of
#'   \code{estimate_historical_exit_rates()} — must contain \code{group_cols}
#'   and \code{exit_rate}.
#' @param group_cols Character vector or \code{NULL}.  Grouping columns.
#'   If \code{NULL}, a single overall exit rate is applied.
#' @param exit_strategy Character.  \code{"random"} (default) or the name of
#'   a numeric column in \code{contract_dt} to rank by (ascending — lowest
#'   values exit first).
#' @param exit_multiplier Numeric.  Scale the historical rate up or down.
#'   Default \code{1.0}.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_type_col Character.  Default \code{"contract_type_code"}.
#' @param active_types Character vector.  Contract type values treated as
#'   active (non-pensioner) eligible for exit.  Default \code{"active"}.
#'
#' @return data.table with at least \code{personnel_id_col} column identifying
#'   personnel selected to exit this period.
#' @keywords internal
compute_status_quo_exits <- function(
    contract_dt,
    exit_rates_dt,
    group_cols        = NULL,
    exit_strategy     = "random",
    exit_multiplier   = 1.0,
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type_code",
    active_types      = "active") {

  if (!data.table::is.data.table(contract_dt))
    contract_dt <- data.table::as.data.table(contract_dt)

  # Active (non-pensioner) workforce only
  active_dt <- contract_dt[
    get(contract_type_col) %in% active_types
  ]

  if (nrow(active_dt) == 0L) {
    return(data.table::data.table())
  }

  if (is.null(group_cols) || length(group_cols) == 0L) {
    # Overall rate — single group
    n_active <- data.table::uniqueN(active_dt[[personnel_id_col]])
    rate     <- if (!is.null(exit_rates_dt) && "exit_rate" %in% names(exit_rates_dt))
      exit_rates_dt$exit_rate[1L] else 0
    n_exit   <- min(round(n_active * rate * exit_multiplier), n_active)

    if (n_exit <= 0L) return(data.table::data.table())

    # One row per person (primary contract)
    pool_ids <- unique(active_dt[[personnel_id_col]])

    selected_ids <- .select_exits(pool_ids, active_dt, n_exit,
                                   exit_strategy, personnel_id_col)
    return(data.table::data.table(personnel_id = selected_ids))
  }

  # Grouped exit selection
  # Left-join rates into active contracts so every active row carries its group rate.
  # Use a copy to avoid modifying exit_rates_dt by reference.
  active_with_rate <- exit_rates_dt[active_dt, on = group_cols]
  active_with_rate[is.na(exit_rate), exit_rate := 0]

  selected_per_group <- active_with_rate[, .compute_group_exits(
    .SD,
    exit_multiplier   = exit_multiplier,
    exit_strategy     = exit_strategy,
    personnel_id_col  = personnel_id_col
  ), by = group_cols]

  if (nrow(selected_per_group) == 0L || !("personnel_id" %in% names(selected_per_group))) {
    return(data.table::data.table())
  }

  # Keep only the ID column; deduplicate (a person appears in at most one group,
  # but guard against edge cases where group definitions overlap).
  result <- unique(
    selected_per_group[, .(personnel_id)],
    by = "personnel_id"
  )

  data.table::setnames(result, "personnel_id", personnel_id_col)
  result
}


# ---------------------------------------------------------------------------
# Internal: compute exits for one group slice (called from [, by = group_cols]).
# Receiving all parameters explicitly avoids closure-capture NOTEs from R CMD check.
# @param group_dt   data.table. Rows for this group (.SD from calling context).
# @param exit_multiplier Numeric. Scale factor applied to the group exit rate.
# @param exit_strategy Character. "random" or a column name for ranked exit.
# @param personnel_id_col Character. Name of the personnel ID column.
# @return list with one element `personnel_id` (character vector of selected IDs).
# @keywords internal
.compute_group_exits <- function(group_dt,
                                  exit_multiplier,
                                  exit_strategy,
                                  personnel_id_col) {
  n_active_grp <- data.table::uniqueN(group_dt[[personnel_id_col]])
  rate         <- group_dt$exit_rate[1L]
  n_exit_grp   <- as.integer(
    min(round(n_active_grp * rate * exit_multiplier), n_active_grp)
  )
  if (n_exit_grp <= 0L) {
    return(list(personnel_id = character(0)))
  }
  pool_ids <- unique(group_dt[[personnel_id_col]])
  list(
    personnel_id = .select_exits(pool_ids, group_dt, n_exit_grp,
                                  exit_strategy, personnel_id_col)
  )
}


# ---------------------------------------------------------------------------
# Internal: select exits from a pool using strategy
# ---------------------------------------------------------------------------
.select_exits <- function(pool_ids, pool_dt, n_exit, strategy, personnel_id_col) {
  n_exit <- as.integer(min(n_exit, length(pool_ids)))
  if (n_exit <= 0L) return(character(0))

  if (strategy == "random") {
    return(sample(pool_ids, size = n_exit, replace = FALSE))
  }

  # Treat strategy as a column name — rank ascending, lowest exits first
  if (strategy %in% names(pool_dt)) {
    primary <- pool_dt[
      data.table::uniqueN(get(personnel_id_col)) > 0,
      .(rank_val = max(get(strategy), na.rm = TRUE)),
      by = c(personnel_id_col)
    ]
    primary <- primary[order(rank_val)]
    return(head(primary[[personnel_id_col]], n_exit))
  }

  # Fallback to random
  sample(pool_ids, size = n_exit, replace = FALSE)
}
