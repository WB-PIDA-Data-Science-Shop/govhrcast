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

# Suppress R CMD check NOTEs for data.table column names.
utils::globalVariables(c(
  "n_exits",   # estimate_historical_exit_rates: column created by := and used in ratio
  "exit_rate", # resolve_policy_table output stamped onto active_dt
  "rank_val"   # .select_exits: ordering column built from dynamic strategy col
))

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
estimate_historical_exit_rates <- function(panel_contract_dt,
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

#' Select Personnel for Non-Retirement Exit
#'
#' @description
#' Resolves per-row \code{exit_rate} and \code{exit_multiplier} from
#' \code{policy_params} via \code{\link{resolve_policy_table}}, then selects
#' personnel to exit this period.  Supports scalar-only and group-level rate
#' dispatch through the unified three-slot \code{policy_params} format.
#'
#' @param contract_dt data.table.  Current (single-snapshot) contract data.
#' @param policy_params List.  Canonical three-slot exit policy specification.
#'   See \code{\link{simulate_exits}} for the full \code{\describe{}} block.
#'   Keys consumed here: \code{group_cols}, \code{policy_table} (with
#'   \code{exit_rate} column), \code{defaults$exit_rate},
#'   \code{defaults$exit_strategy}, \code{defaults$active_types}.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_type_col Character.  Default \code{"contract_type_code"}.
#'
#' @return data.table with at least \code{personnel_id_col} identifying
#'   personnel selected to exit this period.  Empty \code{data.table()} when
#'   there are no active contracts or the resolved rate produces zero exits.
#' @keywords internal
compute_status_quo_exits <- function(
    contract_dt,
    policy_params,
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type_code") {

  .defaults     <- policy_params$defaults %||% list()
  exit_strategy <- .defaults$exit_strategy %||% "random"
  active_types  <- .defaults$active_types  %||% "active"
  group_cols    <- policy_params$group_cols

  if (!data.table::is.data.table(contract_dt))
    contract_dt <- data.table::as.data.table(contract_dt)

  # Active (non-pensioner) workforce only
  active_dt <- contract_dt[
    get(contract_type_col) %in% active_types
  ]

  if (nrow(active_dt) == 0L) {
    return(data.table::data.table())
  }

  # Resolve exit_rate and exit_multiplier to per-row columns via a single
  # resolve_policy_table() call.  For scalar-only dispatch (group_cols = NULL)
  # this simply repeats the defaults.  For group-level, it left-joins
  # policy_table onto active_dt and fills unmatched cells from defaults.
  resolved <- resolve_policy_table(
    policy_params,
    active_dt,
    "exit_rate"
  )
  active_dt[, exit_rate := resolved$exit_rate %||% rep(0, .N)]
  active_dt[is.na(exit_rate), exit_rate := 0]

  if (is.null(group_cols) || length(group_cols) == 0L) {
    # Scalar path — single overall rate
    n_active <- data.table::uniqueN(active_dt[[personnel_id_col]])
    rate     <- active_dt$exit_rate[1L]
    n_exit   <- min(round(n_active * rate), n_active)

    if (n_exit <= 0L) return(data.table::data.table())

    pool_ids <- unique(active_dt[[personnel_id_col]])
    selected_ids <- .select_exits(pool_ids, active_dt, n_exit,
                                   exit_strategy, personnel_id_col)
    return(data.table::data.table(personnel_id = selected_ids))
  }

  # Grouped exit selection — exit_rate and exit_multiplier already stamped
  # as columns on active_dt; .compute_group_exits() reads them from .SD.
  selected_per_group <- active_dt[, .compute_group_exits(
    .SD,
    exit_strategy    = exit_strategy,
    personnel_id_col = personnel_id_col
  ), by = group_cols]

  if (nrow(selected_per_group) == 0L || !("personnel_id" %in% names(selected_per_group))) {
    return(data.table::data.table())
  }

  # Keep only the ID column; deduplicate in case group definitions overlap.
  result <- unique(
    selected_per_group[, .(personnel_id)],
    by = "personnel_id"
  )

  data.table::setnames(result, "personnel_id", personnel_id_col)
  result
}


# =============================================================================
# Phase 3b-alt — compute_fixed_rate_exits()
# =============================================================================

#' Select Personnel for Non-Retirement Exit — Flat Rate
#'
#' @description
#' Applies a single scalar \code{exit_rate} from \code{policy_params$defaults}
#' uniformly across the entire active workforce.  Called by
#' \code{\link{simulate_exits}} when \code{policy_params$policy_table} is
#' \code{NULL}.  No group join, no rate lookup — three steps: count active
#' persons, compute \code{n_exit}, select who exits.
#'
#' @param contract_dt data.table.  Current (single-snapshot) contract data.
#' @param policy_params List.  Canonical three-slot exit policy specification.
#'   Keys consumed: \code{defaults$exit_rate}, \code{defaults$exit_strategy},
#'   \code{defaults$active_types}.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_type_col Character.  Default \code{"contract_type_code"}.
#'
#' @return data.table with \code{personnel_id_col} identifying personnel
#'   selected to exit.  Empty \code{data.table()} when there are no active
#'   contracts or the rate produces zero exits.
#' @keywords internal
compute_fixed_rate_exits <- function(
    contract_dt,
    policy_params,
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type_code") {

  .defaults     <- policy_params$defaults %||% list()
  exit_rate     <- .defaults$exit_rate
  exit_strategy <- .defaults$exit_strategy %||% "random"
  active_types  <- .defaults$active_types  %||% "active"

  if (is.null(exit_rate) || !is.numeric(exit_rate) || length(exit_rate) != 1L)
    stop(
      "defaults$exit_rate must be a numeric scalar when policy_table is NULL.",
      call. = FALSE
    )

  if (!data.table::is.data.table(contract_dt))
    contract_dt <- data.table::as.data.table(contract_dt)

  active_dt <- contract_dt[get(contract_type_col) %in% active_types]

  if (nrow(active_dt) == 0L) return(data.table::data.table())

  n_active <- data.table::uniqueN(active_dt[[personnel_id_col]])
  n_exit   <- min(round(n_active * exit_rate), n_active)

  if (n_exit <= 0L) return(data.table::data.table())

  pool_ids     <- unique(active_dt[[personnel_id_col]])
  selected_ids <- .select_exits(pool_ids, active_dt, n_exit,
                                 exit_strategy, personnel_id_col)

  result <- data.table::data.table(personnel_id = selected_ids)
  data.table::setnames(result, "personnel_id", personnel_id_col)
  result
}


# ---------------------------------------------------------------------------
# Internal: compute exits for one group slice (called from [, by = group_cols]).
# exit_rate is read from per-row columns stamped onto group_dt by
# resolve_policy_table() in compute_status_quo_exits().
# @param group_dt        data.table. Rows for this group (.SD from calling context).
# @param exit_strategy   Character. "random" or a column name for ranked exit.
# @param personnel_id_col Character. Name of the personnel ID column.
# @return list with one element `personnel_id` (character vector of selected IDs).
# @keywords internal
.compute_group_exits <- function(group_dt,
                                 exit_strategy,
                                 personnel_id_col) {
  n_active_grp <- data.table::uniqueN(group_dt[[personnel_id_col]])
  rate         <- group_dt$exit_rate[1L] %||% 0
  n_exit_grp   <- as.integer(min(round(n_active_grp * rate), n_active_grp))
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
.select_exits <- function(pool_ids, 
                          pool_dt, 
                          n_exit, 
                          strategy, 
                          personnel_id_col) {
  
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
