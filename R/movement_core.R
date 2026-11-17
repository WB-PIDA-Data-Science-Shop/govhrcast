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

# Suppress R CMD check NOTEs for data.table column names.
utils::globalVariables(c(
  "t0_date",       # estimate_historical_movement_rates: grouping column added by := then used in by=
  "movement_rate",  # compute_movement_demand: column used in := and by= expressions
  "n_dest",         # compute_fixed_rate_movements: bare column name in data.table
  "rate_per_dest"   # compute_fixed_rate_movements: bare column name in data.table
))

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
#' @param as_of_date Date or character. Reference date for calculation
#' @param group_cols Character vector. Columns defining the "grade" state
#'   (e.g., c("est_id", "paygrade") or just c("paygrade"))
#' @param column_spec_args Complete column specification from
#'   \code{define_column_spec()}.
#'
#' @return data.table with two columns: the caller's personnel ID column and
#'   \code{time_in_grade} (years)
#' @keywords internal
compute_time_in_grade <- function(contract_dt,
                                  as_of_date,
                                  group_cols,
                                  column_spec_args = define_column_spec()) {
  personnel_id_col  <- column_spec_args$personnel_id_col
  ref_date_col      <- column_spec_args$ref_date_col
  start_date_col    <- column_spec_args$start_date_col
  end_date_col      <- column_spec_args$end_date_col
  contract_type_col <- column_spec_args$contract_type_col

  as_of_date <- validate_date_format(as_of_date, "as_of_date")

  # Get the current state (snapshot at or before as_of_date)
  all_dates <- unique(contract_dt[[ref_date_col]])
  all_dates <- all_dates[!is.na(all_dates)]
  valid_dates <- sort(all_dates[all_dates <= as_of_date])

  if (length(valid_dates) == 0) {
    stop("No panel snapshots found on or before as_of_date: ", as_of_date, call. = FALSE)
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
    empty <- data.table::data.table(.id = character(0), time_in_grade = numeric(0))
    return(data.table::setnames(empty, ".id", personnel_id_col))
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
      by = c(personnel_id_col)
    ]

    # Time in grade = as_of_date minus the earliest date in current state
    tig_dt <- earliest_in_state[, time_in_grade := as.numeric(
      difftime(selected_ref_date, earliest_date, units = "days")
    ) / 365.25]
    tig_dt[, earliest_date := NULL]

  } else {
    # Single snapshot: use contract start_date as fallback
    # Get primary contract per person for start_date
    primary <- get_primary_contract(
      contract_dt = current_active,
      personnel_id_col = personnel_id_col,
      contract_id_col = column_spec_args$contract_id_col,
      start_date_col = start_date_col,
      salary_col = column_spec_args$salary_col
    )

    tig_dt <- primary[, c(personnel_id_col, start_date_col), with = FALSE]
    tig_dt[, time_in_grade := as.numeric(
      difftime(selected_ref_date, get(start_date_col), units = "days")
    ) / 365.25]
    tig_dt[, (start_date_col) := NULL]
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
#' etc.) and calculates the pooled probability P_ij = (sum of movements i->j) /
#' (sum of total population in state i at start of each period).  Every pair of
#' snapshots counts, including pairs with no move from i to j.
#'
#' States are defined by the concatenated values of \code{group_cols}. Only
#' actual transitions (\code{from_group != to_group}) are returned; stay rows
#' and rows with NA in any group_col are dropped.
#'
#' Someone with several contracts counts once, in the group of their main contract
#' on that date (see \code{\link{get_primary_contract}}), so the result does not
#' depend on the order of the rows.  Because the main contract is the newest, adding
#' a newer contract in another group counts as a move, even if the older one is kept.
#'
#' @param contract_dt data.table. Contract data in long (panel) format.
#'   Must contain ref_date_col for panel identification.
#' @param group_cols Character vector. Columns defining movement states
#'   (e.g., c("est_id", "paygrade") or c("paygrade"))
#' @param column_spec_args Column names, from \code{\link{define_column_spec}()}.  Change only
#'   the ones that differ from the \code{govhr} defaults, e.g.
#'   \code{define_column_spec(personnel_id_col = "emp_id")}.  Keys read
#'   here: \code{personnel_id_col}, \code{ref_date_col}, \code{start_date_col},
#'   \code{end_date_col}, \code{contract_type_col},
#'   \code{contract_id_col}, \code{salary_col}.
#'
#' @return data.table with columns:
#'   \describe{
#'     \item{from_group}{Character. State at period start (concatenated group_cols)}
#'     \item{to_group}{Character. State at period end (concatenated
#'       group_cols). Always different from \code{from_group} -- stay rows
#'       are dropped.}
#'     \item{movement_rate}{Numeric. Pooled transition probability over all
#'       snapshot pairs}
#'     \item{n_periods}{Integer. Number of snapshot pairs in which this
#'       transition was observed}
#'   }
#'   A zero-row table (not \code{NULL}) is returned when no transitions are
#'   observed.
#' @keywords internal
estimate_historical_movement_rates <- function(contract_dt,
                                               group_cols,
                                               column_spec_args = define_column_spec()) {

  # Validate inputs
  if (!data.table::is.data.table(contract_dt)) {
    stop("contract_dt must be a data.table", call. = FALSE)
  }
  if (is.null(group_cols) || length(group_cols) == 0) {
    stop("group_cols must be specified for movement baseline estimation", call. = FALSE)
  }

  # pull down the variables we need from the column spec 
  cols <- column_spec_args
  personnel_id_col <- cols$personnel_id_col
  ref_date_col <- cols$ref_date_col
  needed_cols <- unique(c(ref_date_col, personnel_id_col, group_cols,
                          cols$contract_id_col, cols$salary_col,
                          cols$start_date_col, cols$end_date_col, cols$contract_type_col))

  missing_cols <- setdiff(needed_cols, names(contract_dt))
  if (length(missing_cols) > 0) {
    stop("Columns not found in contract_dt: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  n_snapshots <- data.table::uniqueN(contract_dt[[ref_date_col]], na.rm = TRUE)
  if (n_snapshots < 2L) {
    stop("At least 2 panel snapshots required to estimate movement baseline. ",
         "Found ", n_snapshots, " snapshot(s).", call. = FALSE)
  }

  # Active staff on each snapshot date. (get() reads the column named by the spec.)
  active <- contract_dt[
    get(cols$start_date_col) <= get(ref_date_col) &
      (is.na(get(cols$end_date_col)) | get(cols$end_date_col) >= get(ref_date_col)) &
      get(cols$contract_type_col) != "inactive",
    needed_cols, with = FALSE
  ]

  # Each person counts in the group of their main contract on each date, the same
  # contract the projection moves. Blank groups go before the columns are combined:
  # paste() would turn a blank into the text "NA", and a later value into a move.
  main_contracts <- get_primary_contract(
    active,
    personnel_id_col = personnel_id_col,
    contract_id_col  = cols$contract_id_col,
    start_date_col   = cols$start_date_col,
    salary_col       = cols$salary_col,
    snapshot_col     = ref_date_col
  )
  states <- main_contracts[, c(personnel_id_col, ref_date_col, group_cols), with = FALSE]
  states <- stats::na.omit(states, cols = group_cols)

  # Fewer than two dates hold no moves, and govhr::compute_transition() stops on a
  # single record
  state_dates <- sort(unique(states[[ref_date_col]]))
  if (length(state_dates) < 2L) {
    no_moves <- data.table::data.table(from_group = character(0), to_group = character(0),
                                       movement_rate = numeric(0), n_periods = integer(0))
    return(data.table::setkeyv(no_moves, c("from_group", "to_group"))[])
  }

  # Moves between consecutive snapshots. govhr::compute_transition() reads the date
  # from a column called ref_date, so it gets a copy under that name.
  states[, .movement_group := do.call(paste, c(.SD, sep = "||")), .SDcols = group_cols]
  transition_input <- states[, c(personnel_id_col, ref_date_col, ".movement_group"), with = FALSE]
  data.table::setnames(transition_input, ref_date_col, "ref_date")
  moves <- govhr::compute_transition(transition_input, id_col = personnel_id_col,
                                     group_col = ".movement_group")

  # Everyone at risk of moving: the people in each group on every date but the
  # last, so a year with no move from a group still counts towards its rate
  exposed <- states[get(ref_date_col) %in% state_dates[-length(state_dates)],
                    .(n_exposed = .N), by = .(from_group = .movement_group)]

  # Pooled rate: all moves over everyone exposed, the same pooling as the exit and
  # hiring rates
  rates <- moves[, .(n_moved = sum(transitions), n_periods = data.table::uniqueN(get("ref_date"))),
                 by = .(from_group = from, to_group = to)]
  rates <- exposed[rates, on = "from_group"]
  rates[, movement_rate := n_moved / n_exposed]
  rates <- rates[from_group != "NA" & to_group != "NA",
                 .(from_group, to_group, movement_rate, n_periods)]

  data.table::setkeyv(rates, c("from_group", "to_group"))
  rates[]
}


#' Compute Movement Demand
#'
#' @description
#' Pure function that joins the current workforce stock with the baseline
#' transition matrix, scrubs destinations not present in
#' \code{salary_scale_dt} (those employees simply stay put), rescales each
#' origin group's rates so total outflow is at most 1, and calculates
#' integer mover counts per from_group -> to_group transition using
#' stochastic rounding. Rates are used as supplied -- there is no multiplier
#' applied here.
#'
#' @param contract_dt data.table. Current (single-snapshot) contract data
#' @param personnel_dt data.table. Current personnel data
#' @param baseline_matrix data.table. Output of
#'   \code{estimate_historical_movement_rates()} or a user-supplied table.
#'   Must have columns: from_group, to_group, movement_rate
#' @param policy_params List. Canonical three-slot movement policy. Only
#'   \code{group_cols} (character vector of columns defining states) is
#'   read here.
#' @param salary_scale_dt data.table. Valid destinations. Must contain group_cols columns.
#' @param as_of_date Date or character. Reference date
#' @param column_spec_args Complete column specification from
#'   \code{define_column_spec()}.
#'
#' @return data.table with columns:
#'   \describe{
#'     \item{from_group}{Character. Origin state}
#'     \item{to_group}{Character. Destination state (different from from_group)}
#'     \item{movement_rate}{Numeric. Transition probability (capped to respect outflow <= 1)}
#'     \item{current_stock}{Integer. Population in from_group}
#'     \item{n_movers}{Integer. Number of people to move (stochastic rounded)}
#'   }
#' @keywords internal
compute_movement_demand <- function(contract_dt,
                                    personnel_dt,
                                    baseline_matrix,
                                    policy_params,
                                    salary_scale_dt,
                                    as_of_date,
                                    column_spec_args = define_column_spec()) {
  personnel_id_col <- column_spec_args$personnel_id_col

  as_of_date <- validate_date_format(as_of_date, "as_of_date")

  group_cols <- policy_params$group_cols

  # ------------------------------------------------------------------
  # 1. Compute current stock per from_group
  # ------------------------------------------------------------------
  active_contracts <- .covered_by_policy(get_active_contracts(
    contract_dt = contract_dt,
    as_of_date = as_of_date,
    start_date_col = column_spec_args$start_date_col,
    end_date_col = column_spec_args$end_date_col,
    contract_type_col = column_spec_args$contract_type_col
  ), policy_params, column_spec_args$contract_type_col)

  # Deduplicate to one row per person (panel data has one row per snapshot).
  # We only need group_cols here, so take the unique person × group combination.
  person_group_unique <- unique(
    active_contracts[, c(personnel_id_col, group_cols), with = FALSE]
  )
  # Drop rows where any group_col is NA
  person_group_unique <- stats::na.omit(person_group_unique, cols = group_cols)

  active_personnel <- person_group_unique[
    personnel_dt[get(column_spec_args$status_col) == "active", personnel_id_col, with = FALSE],
    on = c(personnel_id_col),
    nomatch = NULL
  ]

  if (nrow(active_personnel) == 0) {
    return(data.table::data.table(
      from_group    = character(0),
      to_group      = character(0),
      movement_rate = numeric(0),
      current_stock = integer(0),
      n_movers      = integer(0)
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
      from_group    = character(0),
      to_group      = character(0),
      movement_rate = numeric(0),
      current_stock = integer(0),
      n_movers      = integer(0)
    ))
  }

  # ------------------------------------------------------------------
  # 4. Scrub destinations not in salary_scale_dt
  # ------------------------------------------------------------------
  n_before_scrub <- nrow(demand_dt)
  demand_dt <- demand_dt[to_group %in% valid_to_groups]
  n_scrubbed <- n_before_scrub - nrow(demand_dt)
  if (n_scrubbed > 0) {
    message("Scrubbed ", n_scrubbed, " transition(s) where to_group not found in salary_scale_dt.")
  }

  if (nrow(demand_dt) == 0) {
    return(data.table::data.table(
      from_group    = character(0),
      to_group      = character(0),
      movement_rate = numeric(0),
      current_stock = integer(0),
      n_movers      = integer(0)
    ))
  }

  # ------------------------------------------------------------------
  # 5. Cap probabilities: total outflow per from_group must be <= 1.0
  # ------------------------------------------------------------------
  demand_dt[, total_outflow := sum(movement_rate), by = from_group]
  demand_dt[total_outflow > 1.0, movement_rate := movement_rate / total_outflow]
  demand_dt[, total_outflow := NULL]

  # ------------------------------------------------------------------
  # 6. Stochastic rounding: n_movers = floor(N) + Bernoulli(N - floor(N))
  # ------------------------------------------------------------------
  demand_dt[, expected_n := current_stock * movement_rate]
  demand_dt[, n_movers := stochastic_round(expected_n)]

  # Remove zero-mover rows
  demand_dt <- demand_dt[n_movers > 0]

  # ------------------------------------------------------------------
  # 7. Return clean output
  # ------------------------------------------------------------------
  result <- demand_dt[, .(from_group, to_group, movement_rate, current_stock, n_movers)]
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
#' @param baseline_matrix data.table. Output of \code{estimate_historical_movement_rates()}
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

  n_movers <- if (!is.null(movers_dt) && nrow(movers_dt) > 0)
    nrow(movers_dt) else 0L

  n_movers_demanded <- if (!is.null(demand_dt) && nrow(demand_dt) > 0)
    sum(demand_dt$n_movers, na.rm = TRUE) else 0L

  hist_avg_rate <- if (!is.null(baseline_matrix) && nrow(baseline_matrix) > 0)
    mean(baseline_matrix$movement_rate, na.rm = TRUE) else NA_real_

  summary_tbl <- data.table::data.table(
    n_movers               = as.integer(n_movers),
    n_movers_demanded      = as.integer(n_movers_demanded),
    hist_avg_movement_rate = hist_avg_rate,
    headcount_before       = as.integer(stock_before),
    headcount_after        = as.integer(stock_after)
  )

  return(summary_tbl)
}


# =============================================================================
# compute_fixed_rate_movements() — flat rate path
# =============================================================================

#' Select Personnel for Movement — Flat Rate
#'
#' @description
#' Applies a single scalar \code{movement_rate} from
#' \code{policy_params$defaults}.
#' Called by \code{\link{simulate_promotions_transfers}} when
#' \code{policy_params$policy_table} is \code{NULL} and estimation from a
#' multi-snapshot panel fails or finds zero transitions.  Movers are distributed
#' uniformly across all valid destination groups in \code{salary_scale_dt}.
#'
#' @param contract_dt data.table.  Current (single-snapshot) contract data.
#' @param personnel_dt data.table.  Current personnel data.
#' @param salary_scale_dt data.table.  Pay table; defines valid destination groups.
#' @param policy_params List.  Canonical three-slot movement policy specification.
#'   Keys consumed: \code{group_cols}, \code{defaults$movement_rate}.
#' @param as_of_date Date.  Reference date.
#' @param column_spec_args Complete column specification from
#'   \code{define_column_spec()}.
#'
#' @return data.table with columns \code{from_group}, \code{to_group},
#'   \code{movement_rate}, \code{current_stock}, \code{n_movers}.
#'   Empty data.table when no active workers or rate produces zero movers.
#' @keywords internal
compute_fixed_rate_movements <- function(contract_dt,
                                         personnel_dt,
                                         salary_scale_dt,
                                         policy_params,
                                         as_of_date,
                                         column_spec_args = define_column_spec()) {
  personnel_id_col <- column_spec_args$personnel_id_col

  as_of_date      <- validate_date_format(as_of_date, "as_of_date")
  group_cols    <- policy_params$group_cols
  movement_rate <- policy_params$defaults$movement_rate

  if (is.null(movement_rate) || !is.numeric(movement_rate) || length(movement_rate) != 1L)
    stop("defaults$movement_rate must be a numeric scalar when policy_table is NULL.",
         call. = FALSE)

  if (!data.table::is.data.table(contract_dt))
    contract_dt <- data.table::as.data.table(contract_dt)

  # Active workforce
  active_contracts <- .covered_by_policy(get_active_contracts(
    contract_dt       = contract_dt,
    as_of_date          = as_of_date,
    start_date_col    = column_spec_args$start_date_col,
    end_date_col      = column_spec_args$end_date_col,
    contract_type_col = column_spec_args$contract_type_col
  ), policy_params, column_spec_args$contract_type_col)

  person_groups <- unique(active_contracts[, c(personnel_id_col, group_cols), with = FALSE])
  person_groups <- stats::na.omit(person_groups, cols = group_cols)
  person_groups <- person_groups[
    personnel_dt[get(column_spec_args$status_col) == "active", personnel_id_col, with = FALSE],
    on = personnel_id_col, nomatch = NULL
  ]

  if (nrow(person_groups) == 0L) return(data.table::data.table())

  person_groups[, from_group := do.call(paste, c(.SD, sep = "||")), .SDcols = group_cols]
  stock_dt <- person_groups[, .(current_stock = .N), by = from_group]

  # Valid destinations from salary_scale
  valid_dest <- unique(salary_scale_dt[, group_cols, with = FALSE])
  valid_dest[, to_group := do.call(paste, c(.SD, sep = "||")), .SDcols = group_cols]

  # Cross-join: each from_group can move to every to_group except self
  demand_dt <- data.table::CJ(
    from_group = stock_dt$from_group,
    to_group   = valid_dest$to_group,
    unique     = TRUE
  )
  demand_dt <- stock_dt[demand_dt, on = "from_group"]
  demand_dt <- demand_dt[from_group != to_group]

  if (nrow(demand_dt) == 0L) return(data.table::data.table())

  # Distribute movement_rate equally across destinations
  demand_dt[, n_dest         := .N, by = from_group]
  demand_dt[, rate_per_dest  := movement_rate / n_dest]
  demand_dt[, expected_n     := current_stock * rate_per_dest]
  demand_dt[, n_movers := stochastic_round(expected_n)]
  demand_dt <- demand_dt[n_movers > 0]

  if (nrow(demand_dt) == 0L) return(data.table::data.table())

  result <- demand_dt[, .(from_group, to_group,
                           movement_rate = rate_per_dest,
                           current_stock, n_movers)]
  data.table::setkeyv(result, c("from_group", "to_group"))
  result
}
