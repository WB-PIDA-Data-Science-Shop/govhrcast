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
  "exit_rate", # groups_dt: resolved rate column in compute_status_quo_exits grouped path
  "n_exit"     # groups_dt: exit count column in compute_status_quo_exits grouped path
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
#' non-renewal) across the full historical panel, and turns them into one
#' historical exit rate per group.
#'
#' Each pair of consecutive snapshots is one interval.  Someone active at the
#' start of an interval and no longer active at its end counts as an exit in
#' that interval.  The rate is \strong{pooled} across intervals:
#'
#' \code{exit_rate = total exits over all intervals / total active staff at
#' the start of each interval}
#'
#' e.g. 3 exits from 10 staff in the first interval and 0 from 7 in the
#' second gives \code{3 / 17}.  Pooling gives steadier rates for small
#' groups than averaging each interval's rate, and weights busier years by
#' their size.  The last snapshot starts no interval, so it only ever serves
#' as an end point.  Exits and staff are counted in the group the person
#' was in at the start of the interval.
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
#' @param contract_type_col Character.  Default \code{"contract_type"}.
#' @param status_col Character.  Default \code{"employment_status"}.
#'
#' @return data.table with \code{group_cols} (if specified) and
#'   \code{exit_rate} column -- one row per group, or a single row when
#'   \code{group_cols = NULL}.  A group with no active staff at the start of
#'   any interval gets a rate of 0.
#' @export
estimate_historical_exit_rates <- function(panel_contract_dt,
                                           panel_personnel_dt,
                                           group_cols        = NULL,
                                           freq              = "year",
                                           personnel_id_col  = "personnel_id",
                                           ref_date_col      = "ref_date",
                                           start_date_col    = "start_date",
                                           end_date_col      = "end_date",
                                           contract_type_col = "contract_type",
                                           status_col         = "employment_status") {

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
    freq       = freq,
    status_col = status_col
  )
  # fire_events columns: personnel_id_col, ref_date, type_event

  # Join to contract panel to retrieve group_cols per exit.
  # govhr::add_contract_to_event() deduplicates to one row per
  # person x ref_date x keep_vars before joining, preventing row
  # duplication for multi-contract individuals.
  if (!is.null(group_cols) && length(group_cols) > 0) {
    fire_events <- govhr::add_contract_to_event(
      events      = fire_events,
      contracts   = panel_contract_dt,
      keep_vars   = group_cols
    )

    exit_counts <- fire_events[
      !is.na(get(group_cols[[1]])),
      .(n_exits = .N),
      by = c(ref_date_col, group_cols)
    ]
  } else {
    exit_counts <- fire_events[, .(n_exits = .N), by = ref_date_col]
  }

  # Each exit is dated to the START of the snapshot interval it happens in
  # (detect_personnel_event() flags someone as fired at t when they are gone
  # at t+1), so the exposed population is the active stock at t. The last
  # snapshot starts no interval and can never have an exit, so it is left
  # out -- including it would add a structural zero and bias the rate down
  # by (n - 1) / n.
  rate_dates <- if (length(panel_dates) >= 2L) panel_dates[-length(panel_dates)] else panel_dates

  # Compute active stock per snapshot per group (re-uses compute_current_stock)
  stock_list <- lapply(rate_dates, function(snap) {
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

  # Pooled rate: total exits / total exposed stock across all intervals (the
  # same pooling govhr::estimate_decrement_rates() uses). More stable than
  # averaging each interval's rate when groups are small.
  .pool <- function(n_ev, stock) {
    tot <- sum(stock, na.rm = TRUE)
    if (tot > 0) sum(n_ev, na.rm = TRUE) / tot else 0
  }
  if (!is.null(group_cols) && length(group_cols) > 0) {
    result <- rate_dt[, .(exit_rate = .pool(n_exits, current_stock)), by = group_cols]
  } else {
    result <- data.table::data.table(exit_rate = .pool(rate_dt$n_exits, rate_dt$current_stock))
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
#'   See \code{\link{simulate_exits}} for the full three-slot specification.
#'   Keys consumed here: \code{group_cols}, \code{policy_table} (with
#'   \code{exit_rate} and optionally \code{exit_multiplier} columns; exactly
#'   one row when \code{group_cols} is \code{NULL}), \code{defaults$exit_rate},
#'   \code{defaults$exit_multiplier},
#'   \code{defaults$exit_strategy}, \code{defaults$active_types} (default
#'   \code{c("permanent", "fixed-term", "short-term")} — the non-terminal
#'   \code{contract_type} values per the \code{govhr} harmonized dictionary).
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_type_col Character.  Default \code{"contract_type"}.
#'
#' @return data.table with at least \code{personnel_id_col} identifying
#'   personnel selected to exit this period.  Empty \code{data.table()} when
#'   there are no active contracts or the resolved rate produces zero exits.
#' @keywords internal
compute_status_quo_exits <- function(
    contract_dt,
    policy_params,
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type") {

  .defaults     <- policy_params$defaults %||% list()
  exit_strategy <- .defaults$exit_strategy %||% "random"
  active_types  <- .defaults$active_types  %||%
    c("permanent", "fixed-term", "short-term")
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

  if (is.null(group_cols) || length(group_cols) == 0L) {
    # Ungrouped path. resolve_policy_table() ignores policy_table when
    # group_cols is NULL, so an ungrouped estimate (the 1-row table
    # estimate_historical_exit_rates() returns for group_cols = NULL) must be
    # read here directly; defaults fill any column the table lacks.
    resolved_s <- resolve_policy_table(policy_params, active_dt[1L],
                                       c("exit_rate", "exit_multiplier"))
    pt <- policy_params$policy_table
    if (!is.null(pt) && nrow(pt) > 0L) {
      if (nrow(pt) > 1L)
        stop(
          "compute_status_quo_exits: policy_table has ", nrow(pt), " rows but ",
          "group_cols is NULL, so there is no way to tell which row applies. ",
          "Set group_cols, or pass a 1-row policy_table for an overall rate.",
          call. = FALSE
        )
      if ("exit_rate" %in% names(pt))       resolved_s$exit_rate       <- pt$exit_rate[[1L]]
      if ("exit_multiplier" %in% names(pt)) resolved_s$exit_multiplier <- pt$exit_multiplier[[1L]]
    }
    s_rate <- resolved_s$exit_rate %||% 0
    s_mult <- resolved_s$exit_multiplier %||% 1
    if (is.na(s_rate)) s_rate <- 0
    if (is.na(s_mult)) s_mult <- 1
    eff_rate <- s_rate * s_mult

    n_active <- data.table::uniqueN(active_dt[[personnel_id_col]])
    n_exit   <- min(round(n_active * eff_rate), n_active)
    if (n_exit <= 0L) return(data.table::data.table())

    pool_ids     <- unique(active_dt[[personnel_id_col]])
    selected_ids <- .select_personnel_subset(pool_ids, active_dt, n_exit,
                                              exit_strategy, personnel_id_col)
    return(data.table::data.table(personnel_id = selected_ids))
  }

  # Grouped path — scale-optimised for large HRMIS data.
  #
  # Key insight: resolve_policy_table() is O(N) when called on the full
  # active_dt (one row per contract), but the policy lookup only varies at
  # group level (G << N rows).  We therefore:
  #   1. Aggregate active_dt to one row per group, capturing n_active and
  #      the unique pool of personnel_ids as a list column  — single O(N) pass.
  #   2. Resolve rates on the G-row groups_dt — O(G) join instead of O(N).
  #   3. Compute n_exit per group on groups_dt — purely vectorised on G rows.
  #   4. Sample from the per-group pool with lapply — no .SD copy per group.
  #
  # For N = 1M, G = 200: step 2 is ~5,000x cheaper than the old approach.

  groups_dt <- active_dt[, .(
    n_active = data.table::uniqueN(get(personnel_id_col)),
    pool_ids = list(unique(get(personnel_id_col)))
  ), by = group_cols]

  # Resolve rates on the compact G-row table
  resolved_g <- resolve_policy_table(
    policy_params,
    groups_dt,
    c("exit_rate", "exit_multiplier")
  )
  groups_dt[, exit_rate := resolved_g$exit_rate %||% rep(0, .N)]
  groups_dt[is.na(exit_rate), exit_rate := 0]

  if (!is.null(resolved_g$exit_multiplier)) {
    groups_dt[, exit_rate := exit_rate *
                data.table::fifelse(is.na(resolved_g$exit_multiplier), 1,
                                    resolved_g$exit_multiplier)]
  }

  groups_dt[, n_exit := pmin(as.integer(round(n_active * exit_rate)), n_active)]

  # Sample from each group's pool — no .SD copy, no [, by =] overhead
  selected_ids <- unlist(
    mapply(
      function(pool, n_exit, grp_dt) {
        if (n_exit <= 0L || length(pool) == 0L) return(character(0))
        .select_personnel_subset(pool, active_dt, n_exit, exit_strategy, personnel_id_col)
      },
      groups_dt$pool_ids,
      groups_dt$n_exit,
      SIMPLIFY = FALSE
    ),
    use.names = FALSE
  )

  if (length(selected_ids) == 0L) return(data.table::data.table())

  result <- data.table::data.table(personnel_id = unique(selected_ids))
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
#'   Keys consumed: \code{defaults$exit_rate}, \code{defaults$exit_multiplier}
#'   (default \code{1}; multiplies \code{exit_rate}), \code{defaults$exit_strategy},
#'   \code{defaults$active_types} (default
#'   \code{c("permanent", "fixed-term", "short-term")} — the non-terminal
#'   \code{contract_type} values per the \code{govhr} harmonized dictionary).
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_type_col Character.  Default \code{"contract_type"}.
#'
#' @return data.table with \code{personnel_id_col} identifying personnel
#'   selected to exit.  Empty \code{data.table()} when there are no active
#'   contracts or the rate produces zero exits.
#' @keywords internal
compute_fixed_rate_exits <- function(
    contract_dt,
    policy_params,
    personnel_id_col  = "personnel_id",
    contract_type_col = "contract_type") {

  .defaults     <- policy_params$defaults %||% list()
  exit_rate     <- .defaults$exit_rate
  exit_strategy <- .defaults$exit_strategy %||% "random"
  active_types  <- .defaults$active_types  %||%
    c("permanent", "fixed-term", "short-term")

  if (is.null(exit_rate) || !is.numeric(exit_rate) || length(exit_rate) != 1L)
    stop(
      "defaults$exit_rate must be a numeric scalar when policy_table is NULL.",
      call. = FALSE
    )

  # Same lever as the status-quo path, so a multiplier is never silently
  # dropped when simulate_horizon() falls back to the flat rate.
  exit_multiplier <- .defaults$exit_multiplier %||% 1
  if (!is.numeric(exit_multiplier) || length(exit_multiplier) != 1L || is.na(exit_multiplier))
    stop("defaults$exit_multiplier must be a numeric scalar.", call. = FALSE)
  exit_rate <- exit_rate * exit_multiplier

  if (!data.table::is.data.table(contract_dt))
    contract_dt <- data.table::as.data.table(contract_dt)

  active_dt <- contract_dt[get(contract_type_col) %in% active_types]

  if (nrow(active_dt) == 0L) return(data.table::data.table())

  n_active <- data.table::uniqueN(active_dt[[personnel_id_col]])
  n_exit   <- min(round(n_active * exit_rate), n_active)

  if (n_exit <= 0L) return(data.table::data.table())

  pool_ids     <- unique(active_dt[[personnel_id_col]])
  selected_ids <- .select_personnel_subset(pool_ids, active_dt, n_exit,
                                            exit_strategy, personnel_id_col)

  result <- data.table::data.table(personnel_id = selected_ids)
  data.table::setnames(result, "personnel_id", personnel_id_col)
  result
}
