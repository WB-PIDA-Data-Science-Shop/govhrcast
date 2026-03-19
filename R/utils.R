#' Utility Helper Functions
#'
#' @description
#' Collection of utility functions used across all simulation modules.
#' Includes date calculations, tenure computation, and other common operations.
#'
#' @import data.table
#' @name utils
#' @keywords internal
NULL

# Suppress R CMD check NOTEs for data.table column names used in compute_tenure().
utils::globalVariables(c(
  ".eff_end",    # compute_tenure: effective end date temp col
  ".s",          # compute_tenure: segment start numeric
  ".e",          # compute_tenure: segment end numeric
  ".lag_max_e",  # compute_tenure: lagged max end for overlap detection
  ".contrib"     # compute_tenure: per-segment tenure contribution
))

#' Calculate Years Between Two Dates
#'
#' @description
#' Vectorized function to compute time elapsed in years between two date vectors.
#' Uses 365.25 days per year to account for leap years.
#'
#' @param start_date Date vector. Start dates
#' @param end_date Date vector. End dates
#'
#' @return Numeric vector of years elapsed
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' compute_years(as.Date("2000-01-01"), as.Date("2025-01-01"))
#' # Returns: 25
#' }
compute_years <- function(start_date, end_date) {
  days_diff <- as.numeric(difftime(end_date, start_date, units = "days"))
  years <- days_diff / 365.25
  
  return(years)
}


#' Calculate Age from Birth Date
#'
#' @description
#' Computes age in years for each personnel at a reference date.
#'
#' @param personnel_dt data.table. Personnel data
#' @param ref_date Date. Reference date for age calculation
#' @param birth_date_col Character. Name of birth date column (default: "birth_date")
#' @param personnel_id_col Character. Name of personnel ID column (default: "personnel_id")
#'
#' @return data.table with personnel_id and age columns
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' age_dt <- compute_age(
#'   personnel_dt = personnel_data,
#'   ref_date = as.Date("2025-01-01"),
#'   birth_date_col = "birth_date"
#' )
#' }
compute_age <- function(personnel_dt,
                        ref_date,
                        birth_date_col = "birth_date",
                        personnel_id_col = "personnel_id") {
  
  # Compute age directly without copying
  # Return only ID and age - no modification to input
  age_dt <- personnel_dt[, .(
    personnel_id = get(personnel_id_col),
    age = compute_years(start_date = get(birth_date_col), end_date = ref_date)
  )]
  
  return(age_dt)
}


#' Calculate Tenure from Contract History
#'
#' @description
#' Computes total years of service for each personnel as of a reference date
#' using a vectorised interval-union algorithm based on \code{cummax()}.
#' Overlapping and nested contracts are correctly de-duplicated; gaps between
#' contracts are excluded from the total.
#'
#' The algorithm sorts each person's contracts by start date, then propagates
#' the "furthest right endpoint seen so far" with \code{cummax()}.  Three
#' cases cover all interval relationships:
#' \itemize{
#'   \item \strong{Case 1} (new span): start > lag_cummax → contributes \code{end - start}
#'   \item \strong{Case 2} (extension): end > lag_cummax ≥ start → contributes \code{end - lag_cummax}
#'   \item \strong{Case 3} (nested): end ≤ lag_cummax → contributes 0
#' }
#' This is O(n log n) for the sort, O(n) for the sweep.
#'
#' @param contract_dt data.table. Contract data (may contain panel observations)
#' @param ref_date Date. Reference date for tenure calculation
#' @param personnel_id_col Character. Name of personnel ID column (default: "personnel_id")
#' @param contract_id_col Character. Name of contract ID column (default: "contract_id")
#' @param start_date_col Character. Name of start date column (default: "start_date")
#' @param end_date_col Character. Name of end date column (default: "end_date")
#' @param contract_type_col Character. Name of contract type column (default: "contract_type_code")
#'
#' @return data.table with personnel_id, tenure_days, and tenure_years columns
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' tenure_dt <- compute_tenure(
#'   contract_dt = contract_data,
#'   ref_date = as.Date("2025-01-01")
#' )
#' }
compute_tenure <- function(contract_dt,
                           ref_date,
                           personnel_id_col = "personnel_id",
                           contract_id_col = "contract_id",
                           start_date_col = "start_date",
                           end_date_col = "end_date",
                           contract_type_col = "contract_type_code") {

  # Alias ref_date to a name that cannot be shadowed by a column named 'ref_date'
  # in panel data.tables (data.table resolves column names before env variables).
  .ref_date_ <- ref_date

  # 1. Filter inactive types — subset returns a new object, no copy() needed
  dt <- contract_dt[!get(contract_type_col) %in% c("inactive", "pensioner")]

  # 2. Keep only contracts that started on or before ref_date
  dt <- dt[get(start_date_col) <= .ref_date_]

  # 3. Cap open-ended / future contracts at ref_date
  dt[, .eff_end := data.table::fifelse(
    is.na(get(end_date_col)) | get(end_date_col) > .ref_date_,
    .ref_date_,
    get(end_date_col)
  )]

  # 4. Deduplicate panel snapshots: one row per (contract_id, start_date)
  dt <- dt[dt[, .I[1L], by = c(contract_id_col, start_date_col)]$V1]

  # 5. Work on numeric days — use numeric (not integer) to avoid overflow when
  #    subtracting the sentinel fill value from real date integers.
  dt[, .s := as.numeric(get(start_date_col))]
  dt[, .e := as.numeric(.eff_end)]

  # Drop zero-length contracts (start == end contributes nothing)
  dt <- dt[.e > .s]

  if (nrow(dt) == 0L) {
    empty <- data.table::data.table(
      personnel_id = character(0),
      tenure_days  = numeric(0),
      tenure_years = numeric(0)
    )
    data.table::setnames(empty, "personnel_id", personnel_id_col)
    return(empty[, c(personnel_id_col, "tenure_days", "tenure_years"), with = FALSE])
  }

  # 6. Sort by person then start — O(n log n)
  data.table::setorderv(dt, c(personnel_id_col, ".s"))

  # 7. Lagged cummax of end-dates within each person.
  #    fill = -1e15 (a numeric constant far outside any real date range) ensures
  #    the first interval per person is always classified as a new span without
  #    triggering integer overflow.
  dt[, .lag_max_e := data.table::shift(cummax(.e), fill = -1e15),
     by = c(personnel_id_col)]

  # 8. Classify each interval and compute its contribution to the union
  #    >= in Case 1: adjacent intervals (end_prev == start_curr) are new spans,
  #    not extensions and not nested.
  dt[, .contrib := data.table::fcase(
    .s >= .lag_max_e,  .e - .s,          # Case 1: new span (or exact-boundary adjacent)
    .e >  .lag_max_e,  .e - .lag_max_e,  # Case 2: partial extension
    default = 0                           # Case 3: nested
  )]

  # 9. Sum contributions per person
  result <- dt[,
    .(tenure_days = sum(.contrib, na.rm = TRUE)),
    by = .(personnel_id_val = get(personnel_id_col))
  ]
  result[, tenure_years := tenure_days / 365.25]
  data.table::setnames(result, "personnel_id_val", personnel_id_col)

  result[, c(personnel_id_col, "tenure_days", "tenure_years"), with = FALSE]
}


#' Get Active Contracts at Reference Date
#'
#' @description
#' Filters contracts that are active at a given reference date.
#' A contract is active if it has started and has not ended before ref_date.
#'
#' @param contract_dt data.table. Contract data
#' @param ref_date Date. Reference date
#' @param start_date_col Character. Name of start date column (default: "start_date")
#' @param end_date_col Character. Name of end date column (default: "end_date")
#' @param contract_type_col Character. Name of contract type column (default: "contract_type_code")
#'
#' @return data.table of active contracts
#' @keywords internal
get_active_contracts <- function(contract_dt,
                                 ref_date,
                                 start_date_col = "start_date",
                                 end_date_col = "end_date",
                                 contract_type_col = "contract_type_code") {
  
  # Filter: started before/on ref_date AND (no end date OR ended after ref_date)
  # AND not inactive AND not pensioner (pensioners are not part of the active workforce)
  active <- contract_dt[
    get(start_date_col) <= ref_date &
    (is.na(get(end_date_col)) | get(end_date_col) >= ref_date) &
    !get(contract_type_col) %in% c("inactive", "pensioner")
  ]
  
  return(active)
}


#' Get Primary Contract for Each Personnel
#'
#' @description
#' For personnel with multiple active contracts, selects the primary contract
#' based on priority: (1) latest start_date, (2) highest salary, (3) lowest contract_id.
#'
#' @param contract_dt data.table. Contract data (should be pre-filtered for active contracts)
#' @param personnel_id_col Character. Name of personnel ID column (default: "personnel_id")
#' @param contract_id_col Character. Name of contract ID column (default: "contract_id")
#' @param start_date_col Character. Name of start date column (default: "start_date")
#' @param salary_col Character. Name of salary column for prioritization (default: "gross_salary_lcu")
#'
#' @return data.table with one row per personnel (their primary contract)
#' @keywords internal
get_primary_contract <- function(contract_dt,
                                 personnel_id_col = "personnel_id",
                                 contract_id_col = "contract_id",
                                 start_date_col = "start_date",
                                 salary_col = "gross_salary_lcu") {
  
  # Order by priority: start_date DESC, salary DESC, contract_id ASC
  data.table::setorderv(
    contract_dt,
    cols = c(start_date_col, salary_col, contract_id_col),
    order = c(-1, -1, 1)  # DESC, DESC, ASC
  )
  
  # Take first row per personnel (highest priority)
  primary <- contract_dt[, .SD[1], by = .(personnel_id = get(personnel_id_col))]
  
  return(primary)
}


#' Generate New Personnel IDs
#'
#' @description
#' Creates deterministic personnel IDs for new hires using a date + group + sequence
#' scheme.  Calling with the same inputs always produces the same IDs, making
#' period-over-period microdata joins and snapshot tests reproducible.
#' If \code{group_key} is supplied it is sanitised (non-alphanumeric chars → "_") and
#' embedded in the ID to prevent collisions when called multiple times on the same
#' \code{ref_date} (e.g. once per group in a loop).
#'
#' @param n Integer. Number of IDs to generate
#' @param ref_date Date. Reference date for ID stamping
#' @param prefix Character. Prefix for IDs (default "NEW")
#' @param group_key Character scalar. Optional group identifier to embed in ID
#'   (e.g. "HR" or "ministry_of_finance"). Non-alphanumeric characters are
#'   replaced with underscores.
#'
#' @return Character vector of new IDs
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' generate_new_ids(3, as.Date("2025-01-15"), prefix = "P", group_key = "HR")
#' # Returns: c("P_2025-01-15_HR_001", "P_2025-01-15_HR_002", "P_2025-01-15_HR_003")
#' }
generate_new_ids <- function(n, ref_date, prefix = "NEW", group_key = NULL) {
  date_str <- format(ref_date, "%Y%m%d")
  seq_str  <- formatC(seq_len(n), width = max(3L, nchar(as.character(n))), flag = "0")
  if (!is.null(group_key) && nchar(group_key) > 0L) {
    # Sanitise: replace any non-alphanumeric character with underscore, collapse runs
    safe_key <- gsub("[^[:alnum:]]+", "_", group_key)
    safe_key <- gsub("^_|_$", "", safe_key)  # strip leading/trailing underscores
    paste0(prefix, "_", date_str, "_", safe_key, "_", seq_str)
  } else {
    paste0(prefix, "_", date_str, "_", seq_str)
  }
}


#' Create Empty Event Data Table
#'
#' @description
#' Creates an empty data.table with standard event structure.
#' Used when no events occur in a simulation step.
#'
#' @param event_type Character. Type of event (e.g., "retirement", "hire")
#'
#' @return Empty data.table with standard event columns
#' @keywords internal
create_empty_events <- function(event_type = "event") {
  empty_events <- data.table::data.table(
    personnel_id = character(0),
    event_type = character(0),
    event_date = as.Date(character(0))
  )
  
  return(empty_events)
}


#' Select Nearest Reference Date
#'
#' @description
#' Finds the reference date in a vector that is closest to (but not after)
#' a specified target date. Used to select appropriate snapshot from panel data.
#'
#' @param x Date vector. Available reference dates to choose from
#' @param ref_date Date. Target reference date
#'
#' @return Date. Single date value (closest to ref_date without exceeding it)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' dates <- as.Date(c("2015-01-01", "2016-01-01", "2017-01-01"))
#' select_nearest_ref_date(dates, as.Date("2016-06-01"))
#' # Returns: 2016-01-01
#' }
select_nearest_ref_date <- function(x, ref_date) {
  unique_dates <- unique(x)
  # Remove NA values before comparison
  unique_dates <- unique_dates[!is.na(unique_dates)]
  valid_dates <- unique_dates[unique_dates <= ref_date]
  
  if (length(valid_dates) == 0) {
    stop("No dates found on or before ", ref_date, call. = FALSE)
  }
  
  max(valid_dates)
}


#' Format Date as YYYYMMDD String
#'
#' @description
#' Converts dates to compact string format for IDs and timestamps.
#'
#' @param date Date vector
#'
#' @return Character vector in YYYYMMDD format
#' @keywords internal
format_date_stamp <- function(date) {
  date_str <- gsub("-", "", as.character(date))
  
  return(date_str)
}


#' Expand a Rate Parameter to a Per-Period Vector
#'
#' @description
#' Validates and expands a scalar or length-\code{n} rate parameter (e.g.
#' \code{salary_growth_rate}, \code{pension_cola_rate}) into a numeric vector
#' of exactly length \code{n_periods}.  A scalar is recycled via
#' \code{rep()}; a vector of any other length triggers an error with a
#' informative message that names the offending parameter.
#'
#' @param x Numeric scalar or vector.
#' @param n_periods Positive integer.  Expected length of the output vector.
#' @param param_name Character scalar.  Name of the parameter (used in the
#'   error message).
#'
#' @return Numeric vector of length \code{n_periods}.
#' @keywords internal
expand_rate_vector <- function(x, n_periods, param_name) {
  if (length(x) == 1L) {
    rep(x, n_periods)
  } else if (length(x) == n_periods) {
    x
  } else {
    stop(
      param_name, " must be a scalar or a vector of length n_periods (",
      n_periods, ").",
      call. = FALSE
    )
  }
}


#' Iterate Consecutive Snapshot Pairs in a Panel data.table
#'
#' @description
#' Sets a data.table key on \code{date_col} (enabling O(log N) binary-search
#' subsetting rather than O(N) full-table scans), then calls a user-supplied
#' function \code{f(snap_a, snap_b, ...)} for every consecutive pair of
#' distinct dates in the panel.  Results are collected and returned as a
#' single \code{data.table} via \code{rbindlist}.
#'
#' This helper enforces the key-setting pattern for all callers that need to
#' walk a longitudinal panel snapshot by snapshot.  At scale (50 M rows, 15
#' annual snapshots) the difference between an unkeyed and a keyed scan is
#' roughly 5–10×.
#'
#' @param panel_dt data.table.  Panel data containing all snapshots.  The key
#'   is set/updated in-place on entry; pass \code{data.table::copy()} if the
#'   caller must preserve the original key.
#' @param date_col Character scalar.  Name of the date column that identifies
#'   snapshots (e.g. \code{"ref_date"}).  \code{NA} values are silently dropped
#'   before iteration.
#' @param f Function.  Called as \code{f(snap_a, snap_b, ...)} where
#'   \code{snap_a} and \code{snap_b} are the T0 and T1 subsets respectively.
#'   Must return a \code{data.table} or \code{NULL}; \code{NULL} rows are
#'   skipped.
#' @param ... Additional arguments forwarded to \code{f} unchanged.
#'
#' @return A single \code{data.table} produced by
#'   \code{rbindlist(results, fill = TRUE, use.names = TRUE)} over all
#'   non-\code{NULL} results.  Returns an empty \code{data.table()} when all
#'   calls return \code{NULL} or the panel has fewer than two distinct dates.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' panel <- data.table(
#'   ref_date     = as.Date(c("2015-01-01","2015-01-01","2016-01-01","2016-01-01")),
#'   personnel_id = c("P1", "P2", "P1", "P2"),
#'   paygrade     = c("G1", "G2", "G2", "G2")
#' )
#'
#' count_movers <- function(a, b) {
#'   data.table(n_persons_t0 = nrow(a), n_persons_t1 = nrow(b))
#' }
#'
#' roll_snapshot_pairs(panel, date_col = "ref_date", f = count_movers)
#' }
#'
#' @keywords internal
roll_snapshot_pairs <- function(panel_dt, date_col, f, ...) {

  if (!data.table::is.data.table(panel_dt))
    stop("panel_dt must be a data.table.", call. = FALSE)

  if (!is.character(date_col) || length(date_col) != 1L)
    stop("date_col must be a single character string.", call. = FALSE)

  if (!date_col %in% names(panel_dt))
    stop("date_col '", date_col, "' not found in panel_dt.", call. = FALSE)

  # Set key for binary-search subsetting — this is the core performance lever.
  # Only re-key if needed to avoid unnecessary copies of the index.
  cur_key <- data.table::key(panel_dt)
  if (is.null(cur_key) || cur_key[1L] != date_col)
    data.table::setkeyv(panel_dt, date_col)

  all_dates <- sort(unique(panel_dt[[date_col]]))
  all_dates  <- all_dates[!is.na(all_dates)]

  if (length(all_dates) < 2L) {
    return(data.table::data.table())
  }

  n_pairs  <- length(all_dates) - 1L
  results  <- vector("list", n_pairs)

  for (k in seq_len(n_pairs)) {
    snap_a <- panel_dt[.(all_dates[k])]
    snap_b <- panel_dt[.(all_dates[k + 1L])]
    res_k  <- f(snap_a, snap_b, ...)
    if (!is.null(res_k)) results[[k]] <- res_k
  }

  non_null <- Filter(Negate(is.null), results)
  if (length(non_null) == 0L) return(data.table::data.table())

  data.table::rbindlist(non_null, fill = TRUE, use.names = TRUE)
}
