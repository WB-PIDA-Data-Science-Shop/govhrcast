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
  "n_exit",    # groups_dt: exit count column in compute_status_quo_exits grouped path
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

  # Join to contract panel to retrieve group_cols per exit.
  # govhr::add_contract_to_event() deduplicates to one row per
  # person x ref_date x keep_vars before joining, preventing row
  # duplication for multi-contract individuals.
  if (!is.null(group_cols) && length(group_cols) > 0) {
    fire_events <- govhr::add_contract_to_event(
      event_dt    = fire_events,
      contract_dt = panel_contract_dt,
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

  if (is.null(group_cols) || length(group_cols) == 0L) {
    # Scalar path — resolve rate from a 1-row table (no N-row join).
    resolved_s <- resolve_policy_table(policy_params, active_dt[1L],
                                       c("exit_rate", "exit_multiplier"))
    s_rate <- resolved_s$exit_rate %||% 0
    s_mult <- resolved_s$exit_multiplier %||% 1
    if (is.na(s_rate)) s_rate <- 0
    if (is.na(s_mult)) s_mult <- 1
    eff_rate <- s_rate * s_mult

    n_active <- data.table::uniqueN(active_dt[[personnel_id_col]])
    n_exit   <- min(round(n_active * eff_rate), n_active)
    if (n_exit <= 0L) return(data.table::data.table())

    pool_ids     <- unique(active_dt[[personnel_id_col]])
    selected_ids <- .select_exits(pool_ids, active_dt, n_exit,
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
        .select_exits(pool, active_dt, n_exit, exit_strategy, personnel_id_col)
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
