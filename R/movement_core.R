#' Core Promotions and Transfers Logic Functions
#'
#' @description
#' Functions for estimating empirical movement baselines from panel data,
#' computing time-in-grade, and calculating movement demand (promotions and
#' transfers) based on policy multipliers. All functions are pure (no state
#' modification).
#'
#' @import data.table
#' @name movement_core
#' @keywords internal
NULL

#' Compute Time-in-Grade for Active Personnel
#'
#' @description
#' Calculates how long each person has been in their current paygrade/position
#' by identifying the most recent transition into their current group state
#' using panel data. Uses contract-level start_date relative to the earliest
#' snapshot where the person appears in the current group.
#'
#' @param contract_dt data.table. Contract data (full panel, long format)
#' @param ref_date Date or character. Reference date for calculation
#' @param group_cols Character vector. Columns defining the "grade" state
#'   (e.g., c("est_id", "paygrade") or just c("paygrade"))
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param ref_date_col Character. Reference date column for panel data (default: "ref_date")
#' @param start_date_col Character. Contract start date column (default: "start_date")
#' @param end_date_col Character. Contract end date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#'
#' @return data.table with columns: personnel_id, time_in_grade (years)
#' @keywords internal
compute_time_in_grade <- function(contract_dt,
                                  ref_date,
                                  group_cols,
                                  personnel_id_col = "personnel_id",
                                  ref_date_col = "ref_date",
                                  start_date_col = "start_date",
                                  end_date_col = "end_date",
                                  contract_type_col = "contract_type_code") {

  ref_date <- validate_date_format(ref_date, "ref_date")

  # Get the current state (snapshot at or before ref_date)
  all_dates <- unique(contract_dt[[ref_date_col]])
  all_dates <- all_dates[!is.na(all_dates)]
  valid_dates <- sort(all_dates[all_dates <= ref_date])

  if (length(valid_dates) == 0) {
    stop("No panel snapshots found on or before ref_date: ", ref_date, call. = FALSE)
  }

  selected_ref_date <- max(valid_dates)
  current_snap <- contract_dt[get(ref_date_col) == selected_ref_date]

  # Filter active contracts only
  current_active <- current_snap[
    get(start_date_col) <= selected_ref_date &
      (is.na(get(end_date_col)) | get(end_date_col) >= selected_ref_date) &
      get(contract_type_col) != "inactive"
  ]

  if (nrow(current_active) == 0) {
    return(data.table::data.table(
      personnel_id = character(0),
      time_in_grade = numeric(0)
    ))
  }

  # Get the current group state for each active person (one row per person)
  # Concatenate group columns into a single state key
  current_state <- unique(current_active[, c(personnel_id_col, group_cols), with = FALSE])
  current_state[, .current_group_key := do.call(paste, c(.SD, sep = "||")),
                .SDcols = group_cols]

  # If we have panel data, look back through all available snapshots to find
  # the earliest date the person was already in their current state
  if (length(valid_dates) > 1) {
    # Build history: for each person+snapshot, what was their group state?
    historical <- contract_dt[get(ref_date_col) %in% valid_dates]
    historical_active <- historical[
      get(start_date_col) <= get(ref_date_col) &
        (is.na(get(end_date_col)) | get(end_date_col) >= get(ref_date_col)) &
        get(contract_type_col) != "inactive"
    ]

    # One record per person per snapshot (primary contract)
    hist_state <- unique(historical_active[, c(ref_date_col, personnel_id_col, group_cols),
                                           with = FALSE])
    hist_state[, .hist_group_key := do.call(paste, c(.SD, sep = "||")),
               .SDcols = group_cols]

    # Join to get current group key per person
    hist_state <- current_state[, c(personnel_id_col, ".current_group_key"), with = FALSE][
      hist_state,
      on = personnel_id_col,
      nomatch = NULL
    ]

    # Keep only snapshots where person was in the SAME state as current
    hist_in_current_state <- hist_state[.hist_group_key == .current_group_key]

    # Find the earliest snapshot in the current state per person
    earliest_in_state <- hist_in_current_state[,
      .(earliest_date = min(get(ref_date_col))),
      by = .(personnel_id = get(personnel_id_col))
    ]

    # Time in grade = ref_date minus the earliest date in current state
    tig_dt <- earliest_in_state[, .(
      personnel_id,
      time_in_grade = as.numeric(difftime(selected_ref_date, earliest_date, units = "days")) / 365.25
    )]

  } else {
    # Single snapshot: use contract start_date as fallback
    # Get primary contract per person for start_date
    primary <- get_primary_contract(
      contract_dt = current_active,
      personnel_id_col = personnel_id_col,
      start_date_col = start_date_col
    )

    tig_dt <- primary[, .(
      personnel_id = get(personnel_id_col),
      time_in_grade = as.numeric(difftime(selected_ref_date, get(start_date_col),
                                          units = "days")) / 365.25
    )]
  }

  # Ensure non-negative
  tig_dt[time_in_grade < 0, time_in_grade := 0]

  return(tig_dt)
}


#' Estimate Movement Baseline from Panel Data
#'
#' @description
#' Analyzes longitudinal panel data to compute empirical transition probabilities
#' for promotions and transfers. Compares consecutive snapshots (T0->T1, T1->T2,
#' etc.) and calculates the average probability P_ij = (sum of movements i->j) /
#' (sum of total population in state i at start of each period).
#'
#' States are defined by the concatenated values of \code{group_cols}. Only
#' actual transitions (\code{from_group != to_group}) are returned; stay rows
#' and rows with NA in any group_col are dropped.
#'
#' @param contract_dt data.table. Contract data in long (panel) format.
#'   Must contain ref_date_col for panel identification.
#' @param group_cols Character vector. Columns defining movement states
#'   (e.g., c("est_id", "paygrade") or c("paygrade"))
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param ref_date_col Character. Reference date column (default: "ref_date")
#' @param start_date_col Character. Contract start date column (default: "start_date")
#' @param end_date_col Character. Contract end date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#'
#' @return data.table with columns:
#'   \describe{
#'     \item{from_group}{Character. State at period start (concatenated group_cols)}
#'     \item{to_group}{Character. State at period end (concatenated group_cols or "NO_MOVEMENT")}
#'     \item{avg_prob}{Numeric. Average transition probability across all periods}
#'     \item{n_periods}{Integer. Number of periods used for estimation}
#'   }
#' @keywords internal
estimate_movement_baseline <- function(contract_dt,
                                       group_cols,
                                       personnel_id_col = "personnel_id",
                                       ref_date_col = "ref_date",
                                       start_date_col = "start_date",
                                       end_date_col = "end_date",
                                       contract_type_col = "contract_type_code") {

  # Validate inputs
  if (!data.table::is.data.table(contract_dt)) {
    stop("contract_dt must be a data.table", call. = FALSE)
  }
  if (is.null(group_cols) || length(group_cols) == 0) {
    stop("group_cols must be specified for movement baseline estimation", call. = FALSE)
  }

  missing_cols <- setdiff(c(ref_date_col, personnel_id_col, group_cols,
                             start_date_col, end_date_col, contract_type_col),
                          names(contract_dt))
  if (length(missing_cols) > 0) {
    stop("Columns not found in contract_dt: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  # Get sorted unique reference dates
  all_dates <- sort(unique(contract_dt[[ref_date_col]]))
  all_dates <- all_dates[!is.na(all_dates)]

  if (length(all_dates) < 2) {
    stop("At least 2 panel snapshots required to estimate movement baseline. ",
         "Found ", length(all_dates), " snapshot(s).", call. = FALSE)
  }

  # For each consecutive pair of snapshots, compute transition counts
  period_results <- vector("list", length(all_dates) - 1L)

  for (k in seq_len(length(all_dates) - 1L)) {
    t0_date <- all_dates[k]
    t1_date <- all_dates[k + 1L]

    # Get active persons and their group state at T0
    snap_t0 <- contract_dt[get(ref_date_col) == t0_date]
    active_t0 <- snap_t0[
      get(start_date_col) <= t0_date &
        (is.na(get(end_date_col)) | get(end_date_col) >= t0_date) &
        get(contract_type_col) != "inactive"
    ]

    # One row per person at T0 (primary contract determines group state)
    state_t0 <- unique(active_t0[, c(personnel_id_col, group_cols), with = FALSE])
    # Drop rows where any group_col is NA
    state_t0 <- stats::na.omit(state_t0, cols = group_cols)
    state_t0[, from_group := do.call(paste, c(.SD, sep = "||")), .SDcols = group_cols]
    data.table::setnames(state_t0, personnel_id_col, ".pid")

    # Get active persons and their group state at T1
    snap_t1 <- contract_dt[get(ref_date_col) == t1_date]
    active_t1 <- snap_t1[
      get(start_date_col) <= t1_date &
        (is.na(get(end_date_col)) | get(end_date_col) >= t1_date) &
        get(contract_type_col) != "inactive"
    ]

    state_t1 <- unique(active_t1[, c(personnel_id_col, group_cols), with = FALSE])
    # Drop rows where any group_col is NA
    state_t1 <- stats::na.omit(state_t1, cols = group_cols)
    state_t1[, to_group := do.call(paste, c(.SD, sep = "||")), .SDcols = group_cols]
    data.table::setnames(state_t1, personnel_id_col, ".pid")

    # Join T0 and T1 on person ID - only persons present at both periods
    transitions <- state_t0[state_t1[, .(.pid, to_group)], on = ".pid", nomatch = NULL]

    # Persons in T0 but not in T1 are exits (ignore for transition matrix)
    # Count movements: from_group -> to_group
    movement_counts <- transitions[, .(n_moves = .N), by = .(from_group, to_group)]

    # Total population in each from_group at T0
    pop_t0 <- state_t0[, .(n_pop = .N), by = from_group]

    # Merge to get denominator
    period_trans <- movement_counts[pop_t0, on = "from_group", nomatch = NA]
    period_trans[is.na(n_moves), n_moves := 0L]

    # Compute period-specific probability
    period_trans[, period_prob := n_moves / n_pop]
    period_trans[, period_key := k]

    period_results[[k]] <- period_trans
  }

  # Combine all periods
  all_periods <- data.table::rbindlist(period_results, fill = TRUE, use.names = TRUE)

  if (nrow(all_periods) == 0) {
    return(data.table::data.table(
      from_group = character(0),
      to_group   = character(0),
      avg_prob   = numeric(0),
      n_periods  = integer(0)
    ))
  }

  # Average probabilities across periods
  baseline_matrix <- all_periods[, .(
    avg_prob  = mean(period_prob, na.rm = TRUE),
    n_periods = data.table::uniqueN(period_key)
  ), by = .(from_group, to_group)]

  # Drop stay rows (from_group == to_group): we only want actual transitions
  baseline_matrix <- baseline_matrix[from_group != to_group]

  # Drop any rows where from_group or to_group encodes an NA value ("NA" string
  # or literal NA) — these arise when group_cols contains NAs in the data
  baseline_matrix <- baseline_matrix[
    !is.na(from_group) & !is.na(to_group) &
    from_group != "NA"  & to_group  != "NA"
  ]

  # Check for duplicate from_group/to_group (should not occur, but guard)
  if (anyDuplicated(baseline_matrix, by = c("from_group", "to_group"))) {
    baseline_matrix <- unique(baseline_matrix, by = c("from_group", "to_group"))
  }

  data.table::setkeyv(baseline_matrix, c("from_group", "to_group"))
  return(baseline_matrix)
}


#' Compute Movement Demand
#'
#' @description
#' Pure function that joins the current workforce stock with the baseline
#' transition matrix, applies policy multipliers, scrubs invalid targets
#' (not present in salary_scale), redistributes scrubbed probability to
#' "no movement", and calculates integer mover counts per from_group -> to_group
#' transition using stochastic rounding.
#'
#' @param contract_dt data.table. Current (single-snapshot) contract data
#' @param personnel_dt data.table. Current personnel data
#' @param baseline_matrix data.table. Output of \code{estimate_movement_baseline()}.
#'   Must have columns: from_group, to_group, avg_prob
#' @param policy_params List. Must contain:
#'   \describe{
#'     \item{group_cols}{Character vector. Columns used to define states}
#'     \item{promotion_multiplier}{Numeric. Scalar multiplier for promotion probs (default 1)}
#'     \item{transfer_multiplier}{Numeric. Scalar multiplier for transfer probs (default 1)}
#'     \item{promotion_strategy}{Character. "tenure" or "wage_based"}
#'     \item{transfer_strategy}{Character. "random", "tenure", or "reverse_tenure"}
#'   }
#' @param salary_scale_dt data.table. Valid destinations. Must contain group_cols columns.
#' @param ref_date Date or character. Reference date
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param start_date_col Character. Start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#' @param status_col Character. Status column (default: "status")
#'
#' @return data.table with columns:
#'   \describe{
#'     \item{from_group}{Character. Origin state}
#'     \item{to_group}{Character. Destination state (different from from_group)}
#'     \item{movement_type}{Character. "promotion" or "transfer"}
#'     \item{adj_prob}{Numeric. Adjusted probability after multipliers and scrubbing}
#'     \item{current_stock}{Integer. Population in from_group}
#'     \item{n_movers}{Integer. Number of people to move (stochastic rounded)}
#'   }
#' @keywords internal
compute_movement_demand <- function(contract_dt,
                                    personnel_dt,
                                    baseline_matrix,
                                    policy_params,
                                    salary_scale_dt,
                                    ref_date,
                                    personnel_id_col = "personnel_id",
                                    start_date_col = "start_date",
                                    end_date_col = "end_date",
                                    contract_type_col = "contract_type_code",
                                    status_col = "status") {

  ref_date <- validate_date_format(ref_date, "ref_date")

  group_cols          <- policy_params$group_cols
  promotion_mult      <- if (!is.null(policy_params$promotion_multiplier))
                           policy_params$promotion_multiplier else 1.0
  transfer_mult       <- if (!is.null(policy_params$transfer_multiplier))
                           policy_params$transfer_multiplier else 1.0

  # ------------------------------------------------------------------
  # 1. Compute current stock per from_group
  # ------------------------------------------------------------------
  active_contracts <- get_active_contracts(
    contract_dt = contract_dt,
    ref_date = ref_date,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col
  )

  # Deduplicate to one row per person (panel data has one row per snapshot).
  # We only need group_cols here, so take the unique person × group combination.
  person_group_unique <- unique(
    active_contracts[, c(personnel_id_col, group_cols), with = FALSE]
  )
  # Drop rows where any group_col is NA
  person_group_unique <- stats::na.omit(person_group_unique, cols = group_cols)

  active_personnel <- person_group_unique[
    personnel_dt[get(status_col) == "active", .(personnel_id = get(personnel_id_col))],
    on = c(personnel_id_col),
    nomatch = NULL
  ]

  if (nrow(active_personnel) == 0) {
    return(data.table::data.table(
      from_group     = character(0),
      to_group       = character(0),
      movement_type  = character(0),
      adj_prob       = numeric(0),
      current_stock  = integer(0),
      n_movers       = integer(0)
    ))
  }

  # Build group-state key per person (already one row per person after dedup above)
  person_states <- data.table::copy(active_personnel)
  person_states[, from_group := do.call(paste, c(.SD, sep = "||")), .SDcols = group_cols]

  stock_dt <- person_states[, .(current_stock = .N), by = from_group]

  # ------------------------------------------------------------------
  # 2. Build valid destination set from salary_scale
  # ------------------------------------------------------------------
  valid_dest_keys <- unique(salary_scale_dt[, group_cols, with = FALSE])
  valid_dest_keys[, to_group := do.call(paste, c(.SD, sep = "||")), .SDcols = group_cols]
  valid_to_groups <- valid_dest_keys$to_group

  # ------------------------------------------------------------------
  # 3. Join baseline matrix with stock, filter to from_groups in current data
  # ------------------------------------------------------------------
  demand_dt <- baseline_matrix[stock_dt, on = "from_group", nomatch = NULL]

  # Remove self-transitions (no movement entries) from movement demand
  demand_dt <- demand_dt[from_group != to_group]

  if (nrow(demand_dt) == 0) {
    return(data.table::data.table(
      from_group     = character(0),
      to_group       = character(0),
      movement_type  = character(0),
      adj_prob       = numeric(0),
      current_stock  = integer(0),
      n_movers       = integer(0)
    ))
  }

  # ------------------------------------------------------------------
  # 4. Classify as promotion vs transfer
  # A transition is a "promotion" if the to_group appears "higher" than
  # from_group. We determine this heuristically: if the to_group and
  # from_group share all BUT the last group_col component (i.e., same org,
  # different grade) it could be a promotion OR a transfer within org.
  # We classify: if the paygrade component changes -> promotion candidate;
  # if org changes (and paygrade same or absent) -> transfer.
  # For simplicity: any within-org grade change = promotion;
  # cross-org movement (first group col changes) = transfer.
  # ------------------------------------------------------------------
  if (length(group_cols) >= 2) {
    # Split from_group and to_group back into components
    demand_dt[, c(paste0(".from_", group_cols)) :=
                data.table::tstrsplit(from_group, split = "||", fixed = TRUE)]
    demand_dt[, c(paste0(".to_", group_cols)) :=
                data.table::tstrsplit(to_group, split = "||", fixed = TRUE)]

    # First component = org (est_id), last component = grade (paygrade)
    first_col    <- group_cols[1]
    last_col     <- group_cols[length(group_cols)]
    from_first   <- paste0(".from_", first_col)
    to_first     <- paste0(".to_", first_col)
    from_last    <- paste0(".from_", last_col)
    to_last      <- paste0(".to_", last_col)

    demand_dt[, movement_type := data.table::fifelse(
      get(from_first) == get(to_first),
      "promotion",    # same org, different state -> promotion
      "transfer"      # different org -> transfer
    )]

    # Clean up temporary columns
    demand_dt[, c(paste0(".from_", group_cols), paste0(".to_", group_cols)) := NULL]

  } else {
    # Single group_col: all movements are promotions (grade changes)
    demand_dt[, movement_type := "promotion"]
  }

  # ------------------------------------------------------------------
  # 5. Apply policy multipliers
  # ------------------------------------------------------------------
  demand_dt[movement_type == "promotion", adj_prob := avg_prob * promotion_mult]
  demand_dt[movement_type == "transfer",  adj_prob := avg_prob * transfer_mult]

  # ------------------------------------------------------------------
  # 6. NA scrubbing: remove to_group destinations not in salary_scale
  #    Redistribute that probability to "no movement" (i.e., just remove
  #    the row — the probability simply adds back to staying)
  # ------------------------------------------------------------------
  n_before_scrub <- nrow(demand_dt)
  demand_dt <- demand_dt[to_group %in% valid_to_groups]
  n_scrubbed <- n_before_scrub - nrow(demand_dt)
  if (n_scrubbed > 0) {
    message("Scrubbed ", n_scrubbed, " transition(s) where to_group not found in salary_scale.")
  }

  if (nrow(demand_dt) == 0) {
    return(data.table::data.table(
      from_group     = character(0),
      to_group       = character(0),
      movement_type  = character(0),
      adj_prob       = numeric(0),
      current_stock  = integer(0),
      n_movers       = integer(0)
    ))
  }

  # ------------------------------------------------------------------
  # 7. Cap probabilities: total outflow per from_group must be <= 1.0
  #    Scale back proportionally if exceeded
  # ------------------------------------------------------------------
  demand_dt[, total_outflow := sum(adj_prob), by = from_group]
  demand_dt[total_outflow > 1.0, adj_prob := adj_prob / total_outflow]
  demand_dt[, total_outflow := NULL]

  # ------------------------------------------------------------------
  # 8. Stochastic rounding: n_movers = floor(N) + Bernoulli(N - floor(N))
  # ------------------------------------------------------------------
  demand_dt[, expected_n := current_stock * adj_prob]
  demand_dt[, n_movers := {
    floor_n    <- floor(expected_n)
    fractional <- expected_n - floor_n
    as.integer(floor_n + stats::rbinom(n = .N, size = 1L, prob = fractional))
  }]

  # Remove zero-mover rows
  demand_dt <- demand_dt[n_movers > 0]

  # ------------------------------------------------------------------
  # 9. Return clean output
  # ------------------------------------------------------------------
  result <- demand_dt[, .(from_group, to_group, movement_type, adj_prob,
                          current_stock, n_movers)]
  data.table::setkeyv(result, c("from_group", "to_group"))
  return(result)
}


#' Compute Movement Summary Statistics
#'
#' @description
#' Aggregates summary statistics before and after promotions/transfers,
#' including historical rates from the baseline matrix and actual counts
#' from the simulation.
#'
#' @param movers_dt data.table. Output of \code{identify_movers()}
#' @param demand_dt data.table. Output of \code{compute_movement_demand()}
#' @param baseline_matrix data.table. Output of \code{estimate_movement_baseline()}
#' @param stock_before Integer. Total active headcount before movements
#' @param stock_after Integer. Total active headcount after movements
#'
#' @return data.table with summary statistics columns
#' @keywords internal
compute_movement_summary <- function(movers_dt,
                                     demand_dt,
                                     baseline_matrix,
                                     stock_before,
                                     stock_after) {

  # Actual movements by type
  if (!is.null(movers_dt) && nrow(movers_dt) > 0 && "movement_type" %in% names(movers_dt)) {
    n_promotions <- movers_dt[movement_type == "promotion", .N]
    n_transfers  <- movers_dt[movement_type == "transfer",  .N]
  } else {
    n_promotions <- 0L
    n_transfers  <- 0L
  }
  n_total_movers <- n_promotions + n_transfers

  # Historical (baseline) rates
  if (!is.null(baseline_matrix) && nrow(baseline_matrix) > 0) {
    hist_promotion_rate <- mean(
      baseline_matrix[from_group != to_group, avg_prob], na.rm = TRUE
    )
  } else {
    hist_promotion_rate <- NA_real_
  }

  # Demanded vs actual
  if (!is.null(demand_dt) && nrow(demand_dt) > 0) {
    n_promotions_demanded <- sum(demand_dt[movement_type == "promotion", n_movers],
                                 na.rm = TRUE)
    n_transfers_demanded  <- sum(demand_dt[movement_type == "transfer",  n_movers],
                                 na.rm = TRUE)
  } else {
    n_promotions_demanded <- 0L
    n_transfers_demanded  <- 0L
  }

  summary_tbl <- data.table::data.table(
    n_promotions           = as.integer(n_promotions),
    n_transfers            = as.integer(n_transfers),
    n_total_movers         = as.integer(n_total_movers),
    n_promotions_demanded  = as.integer(n_promotions_demanded),
    n_transfers_demanded   = as.integer(n_transfers_demanded),
    hist_avg_movement_rate = hist_promotion_rate,
    headcount_before       = as.integer(stock_before),
    headcount_after        = as.integer(stock_after)
  )

  return(summary_tbl)
}
