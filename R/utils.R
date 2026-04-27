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
#' Vectorised computation of elapsed time in years between two \code{Date}
#' vectors.  Uses a 365.25-day year to account for leap years consistently
#' across all age and tenure calculations in the package.
#'
#' @param start_date Date vector.  Start dates.  Recycled to length of
#'   \code{end_date} by standard R rules.
#' @param end_date Date vector.  End dates.  Must be coercible to \code{Date}
#'   via \code{as.numeric(difftime(..., units = "days"))}.
#'
#' @return Numeric vector (length = \code{max(length(start_date), length(end_date))})
#'   of elapsed years.  Negative if \code{end_date < start_date}.
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
#' Computes age in fractional years at a reference date for every person in
#' \code{personnel_dt}.  Thin wrapper around \code{\link{compute_years}}
#' that extracts the birth date column and returns a tidy two-column
#' \code{data.table} ready for joining onto eligibility tables.
#'
#' @param personnel_dt data.table.  Personnel register.  Required columns:
#'   \code{personnel_id_col}, \code{birth_date_col}.
#' @param ref_date Date.  The reference date used as the \emph{end} date for
#'   the age interval.  Typically the simulation period close date.
#' @param birth_date_col Character.  Date of birth column in
#'   \code{personnel_dt}.  (default: \code{"birth_date"}).
#' @param personnel_id_col Character.  Unique personnel identifier column.
#'   (default: \code{"personnel_id"}).
#'
#' @return data.table (one row per person) with columns:
#'   \describe{
#'     \item{\code{personnel_id}}{Personnel identifier.}
#'     \item{\code{age}}{Numeric.  Age in fractional years at \code{ref_date}.}
#'   }
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
#' Returns the strictly active workforce subset of \code{contract_dt}:
#' contracts that have started, have not yet ended, and are not classified as
#' \code{"inactive"} or \code{"pensioner"}.
#'
#' Use this filter for headcount, attrition, hiring demand, and retirement
#' eligibility computations.  For wage bill computations use
#' \code{\link{get_salary_bearing_contracts}} instead, which retains
#' inactive-but-paid staff.
#'
#' @param contract_dt data.table.  Contract register.  Required columns:
#'   \code{start_date_col}, \code{end_date_col}, \code{contract_type_col}.
#' @param ref_date Date.  The snapshot date.  A contract is included if
#'   \code{start_date <= ref_date} and
#'   (\code{is.na(end_date)} or \code{end_date >= ref_date}).
#' @param start_date_col Character.  Contract start date column.
#'   (default: \code{"start_date"}).
#' @param end_date_col Character.  Contract end date column; \code{NA} for
#'   open-ended contracts.  (default: \code{"end_date"}).
#' @param contract_type_col Character.  Contract classification column.
#'   Contracts with type \code{"inactive"} or \code{"pensioner"} are
#'   excluded.  (default: \code{"contract_type_code"}).
#'
#' @return data.table.  Subset of \code{contract_dt} containing only active
#'   contracts at \code{ref_date}.  All original columns are preserved.
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


#' Get Salary-Bearing Contracts at Reference Date
#'
#' @description
#' Returns all contract rows where the government is paying a salary — that is,
#' any row that is \emph{not} a pensioner contract and has a non-\code{NA}
#' salary value.  This is the correct filter for wage bill computation.
#'
#' Unlike \code{get_active_contracts()}, which restricts to personnel who are
#' actively working, this function deliberately retains inactive-but-paid staff
#' (e.g. those on government-funded leave or training) because the government
#' is still paying their salary and they must be counted in the wage bill.
#'
#' @param contract_dt data.table.  Contract data.
#' @param salary_col Character.  Name of the salary column.
#'   Default \code{"gross_salary_lcu"}.
#' @param contract_type_col Character.  Name of the contract type column.
#'   Default \code{"contract_type_code"}.
#' @param pensioner_type Character scalar.  The contract type value that
#'   identifies pensioner rows.  Default \code{"pensioner"}.
#'
#' @return data.table of salary-bearing contract rows (a subset of
#'   \code{contract_dt}).
#' @keywords internal
get_salary_bearing_contracts <- function(contract_dt,
                                         salary_col          = "gross_salary_lcu",
                                         contract_type_col   = "contract_type_code",
                                         pensioner_type      = "pensioner") {
  contract_dt[
    get(contract_type_col) != pensioner_type &
    !is.na(get(salary_col))
  ]
}


#' Get Primary Contract for Each Personnel
#'
#' @description
#' Deduplicates \code{contract_dt} to one row per person by applying a
#' three-level priority rule: (1) latest \code{start_date}, (2) highest
#' salary, (3) lowest \code{contract_id} (tie-break for determinism).
#' Typically called on the output of \code{\link{get_active_contracts}} to
#' obtain a unique primary contract per eligible employee.
#'
#' @param contract_dt data.table.  Contract register, pre-filtered to the
#'   relevant subset (e.g. active contracts only).  Required columns:
#'   \code{personnel_id_col}, \code{contract_id_col}, \code{start_date_col},
#'   \code{salary_col}.
#' @param personnel_id_col Character.  Personnel identifier column.
#'   (default: \code{"personnel_id"}).
#' @param contract_id_col Character.  Contract identifier column used as the
#'   deterministic tie-break (lowest value wins).
#'   (default: \code{"contract_id"}).
#' @param start_date_col Character.  Contract start date column.
#'   (default: \code{"start_date"}).
#' @param salary_col Character.  Salary column for the second priority level
#'   (highest salary wins when start dates are tied).
#'   (default: \code{"gross_salary_lcu"}).
#'
#' @details
#' Sorts \code{contract_dt} in place (\code{data.table::setorderv}) before
#' taking \code{.SD[1]} per group.  The caller's table is reordered as a
#' side-effect; pass \code{data.table::copy(contract_dt)} if the original
#' row order must be preserved.
#'
#' @return data.table.  One row per unique \code{personnel_id_col} value,
#'   containing all original columns of the highest-priority contract.
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


#' Extract group_cols from a param spec
#'
#' @description
#' Returns the \code{group_cols} slot when \code{param_spec} is a three-slot
#' list, otherwise \code{NULL}. Used internally to collect all contract
#' columns that must be joined onto the working data before calling
#' \code{dispatch_param()}.
#'
#' @param param_spec A bare scalar, or a list with a \code{group_cols} slot.
#' @return Character vector or \code{NULL}.
#' @keywords internal
.param_group_cols <- function(param_spec) {
  if (is.list(param_spec) && !data.table::is.data.table(param_spec))
    param_spec$group_cols
  else
    NULL
}


#' Dispatch a Policy Parameter to a Per-Row Vector
#'
#' @description
#' Resolves a policy parameter specification into a numeric vector aligned
#' to the rows of \code{working_dt}. Accepts three input forms:
#' \itemize{
#'   \item A bare scalar (e.g. \code{60}) — repeated for every row.
#'   \item A three-slot list with \code{group_cols = NULL} — \code{default}
#'         is repeated for every row.
#'   \item A three-slot list with \code{group_cols} and \code{policy_table} — the
#'         param table is joined onto \code{working_dt} by \code{group_cols}
#'         and the \code{param_name} column is returned; unmatched rows are
#'         filled with \code{default}, or an error is raised if
#'         \code{default} is \code{NULL}.
#' }
#'
#' @param param_spec Scalar, or a list with slots:
#'   \describe{
#'     \item{default}{Scalar fallback value. Used directly when
#'       \code{group_cols} is \code{NULL}, or as fill for unmatched rows
#'       when \code{group_cols} is set.}
#'     \item{group_cols}{Character vector of column names in
#'       \code{working_dt} that define the grouping key. \code{NULL} for
#'       scalar dispatch.}
#'     \item{policy_table}{data.table with \code{group_cols} plus a column named
#'       \code{param_name} holding the per-group values. Required when
#'       \code{group_cols} is non-\code{NULL}.}
#'   }
#' @param working_dt data.table. The working data against which the parameter
#'   is resolved. Must contain all columns listed in \code{group_cols}.
#' @param param_name Character scalar. Name of the parameter being resolved;
#'   must match the value column in \code{dt}.
#'
#' @return Numeric (or logical/character) vector of length \code{nrow(working_dt)}.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(grade = c("A", "B", "A"), salary = c(5000, 4000, 3000))
#'
#' # Scalar dispatch
#' dispatch_param(60, dt, "min_age")
#' # [1] 60 60 60
#'
#' # Group-level dispatch
#' lookup <- data.table(grade = c("A", "B"), min_age = c(60, 55))
#' dispatch_param(
#'   list(default = NULL, group_cols = "grade", policy_table = lookup),
#'   dt, "min_age"
#' )
#' # [1] 60 55 60
#' }
#'
#' @keywords internal
dispatch_param <- function(param_spec, working_dt, param_name) {

  # --- Normalise bare scalar to canonical three-slot form -----------------
  if (!is.list(param_spec) || data.table::is.data.table(param_spec))
    param_spec <- list(default = param_spec, group_cols = NULL, policy_table = NULL)

  group_cols   <- param_spec$group_cols
  default      <- param_spec$default
  lookup_dt    <- param_spec$policy_table

  # --- Validate: policy_table without group_cols or group_cols without policy_table ---
  if (is.null(group_cols) && !is.null(lookup_dt))
    stop("dispatch_param: '", param_name,
         "' has policy_table but group_cols is NULL. ",
         "Specify group_cols or remove policy_table.", call. = FALSE)

  if (!is.null(group_cols) && is.null(lookup_dt))
    stop("dispatch_param: '", param_name,
         "' has group_cols but policy_table is NULL. ",
         "Provide a policy_table or set group_cols = NULL for scalar dispatch.",
         call. = FALSE)

  # --- Scalar path: no join needed ----------------------------------------
  if (is.null(group_cols))
    return(rep(default, nrow(working_dt)))

  # --- Group-level path ---------------------------------------------------

  # Validate group_cols exist in working_dt
  missing_cols <- setdiff(group_cols, names(working_dt))
  if (length(missing_cols) > 0L)
    stop("dispatch_param: '", param_name,
         "' group_cols not found in working_dt: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)

  # Validate value column exists in lookup_dt
  if (!param_name %in% names(lookup_dt))
    stop("dispatch_param: '", param_name,
         "' column not found in dt. ",
         "dt must contain a column named '", param_name, "'.", call. = FALSE)

  # Select only the columns we need — avoid dragging extra cols through join
  lookup_slim <- lookup_dt[, c(group_cols, param_name), with = FALSE]

  # Left-join: every row of working_dt gets the matched value (or NA)
  joined <- lookup_slim[working_dt, on = group_cols]
  vals   <- joined[[param_name]]

  # Fill unmatched rows
  unmatched <- is.na(vals)
  if (any(unmatched)) {
    if (is.null(default))
      stop("dispatch_param: '", param_name, "' has ",
           sum(unmatched), " unmatched group(s) and no default value. ",
           "Add a default or make the lookup dt exhaustive.", call. = FALSE)
    vals[unmatched] <- default
  }

  vals
}


#' Resolve All Policy Parameters to Per-Row Vectors
#'
#' @description
#' The unified group-level policy parameter resolver.  Performs a single left
#' join of \code{policy_params$policy_table} onto \code{working_dt} by
#' \code{policy_params$group_cols}, then fills every unmatched cell (and every
#' parameter not present in \code{policy_table}) from
#' \code{policy_params$defaults}.
#'
#' The result is a named list of vectors, each of length
#' \code{nrow(working_dt)}, ready to be assigned as columns on the working
#' data.table.  Parameters absent from both \code{defaults} and
#' \code{policy_table} are silently omitted.
#'
#' @param policy_params List.  Must contain at minimum a \code{defaults} named
#'   list of scalar fallback values.  Optionally:
#'   \describe{
#'     \item{group_cols}{Character vector of join-key column names present in
#'       \code{working_dt}.  \code{NULL} for scalar-only dispatch.}
#'     \item{policy_table}{data.table with \code{group_cols} plus any
#'       per-group parameter columns; or \code{NULL}.}
#'   }
#' @param working_dt data.table.  The working data to resolve parameters
#'   against.  Must contain all columns listed in \code{group_cols}.
#' @param param_names Character vector.  Names of parameters to resolve.
#'
#' @return Named list of vectors, each of length \code{nrow(working_dt)}.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(paygrade = c("A", "B", "A"), salary = c(5000, 4000, 3000))
#'
#' # Scalar path — no policy_table
#' policy_params <- list(
#'   group_cols   = NULL,
#'   policy_table = NULL,
#'   defaults     = list(min_age = 60, pension_type = "db")
#' )
#' resolve_policy_table(policy_params, dt, c("min_age", "pension_type"))
#' # $min_age: c(60, 60, 60)  $pension_type: c("db","db","db")
#'
#' # Group-level path
#' pt <- data.table(paygrade = c("A", "B"), min_age = c(60, 55))
#' policy_params <- list(
#'   group_cols   = "paygrade",
#'   policy_table = pt,
#'   defaults     = list(min_age = 58, pension_type = "db")
#' )
#' resolve_policy_table(policy_params, dt, c("min_age", "pension_type"))
#' # $min_age: c(60, 55, 60)  $pension_type: c("db","db","db")
#' }
#'
#' @keywords internal
resolve_policy_table <- function(policy_params, working_dt, param_names) {

  defaults     <- if (is.null(policy_params$defaults)) list()
                  else policy_params$defaults
  group_cols   <- policy_params$group_cols
  policy_table <- policy_params$policy_table

  n      <- nrow(working_dt)
  result <- vector("list", length(param_names))
  names(result) <- param_names

  # --- Initialise every requested param from defaults ----------------------
  for (p in param_names) {
    val <- defaults[[p]]
    if (!is.null(val)) result[[p]] <- rep(val, n)
  }

  # --- Group-level override via policy_table --------------------------------
  if (!is.null(policy_table) && !is.null(group_cols)) {

    missing_gc <- setdiff(group_cols, names(working_dt))
    if (length(missing_gc) > 0L)
      stop("resolve_policy_table: group_cols not found in working_dt: ",
           paste(missing_gc, collapse = ", "), call. = FALSE)

    # Coerce policy_table join keys to match working_dt types.
    # A type mismatch (e.g. factor vs character) silently produces all-NA
    # matches in data.table joins, so we coerce before the join.
    policy_table <- data.table::copy(policy_table)  # avoid modifying caller's table
    for (.gc in group_cols) {
      .wclass <- class(working_dt[[.gc]])[1L]
      .pclass <- class(policy_table[[.gc]])[1L]
      if (!identical(.wclass, .pclass)) {
        tryCatch(
          data.table::set(policy_table, j = .gc,
                          value = methods::as(policy_table[[.gc]], .wclass)),
          error = function(e)
            warning("resolve_policy_table: could not coerce policy_table$", .gc,
                    " from ", .pclass, " to ", .wclass,
                    ". Join may produce NA values.", call. = FALSE)
        )
      }
    }

    cols_to_get <- intersect(param_names, names(policy_table))
    if (length(cols_to_get) > 0L) {
      lookup_slim <- policy_table[, c(group_cols, cols_to_get), with = FALSE]
      joined      <- lookup_slim[working_dt, on = group_cols]
      for (p in cols_to_get) {
        vals      <- joined[[p]]
        unmatched <- is.na(vals)
        if (any(unmatched)) {
          fallback <- defaults[[p]]
          if (!is.null(fallback)) {
            vals[unmatched] <- fallback
          } else if (all(unmatched)) {
            stop("resolve_policy_table: '", p,
                 "' has unmatched groups and no default. ",
                 "Add a default or make policy_table exhaustive.",
                 call. = FALSE)
          }
          # partial match with no fallback: leave NAs in place
        }
        result[[p]] <- vals
      }
    }
  }

  # Drop NULLs (params not in defaults and not in policy_table)
  Filter(Negate(is.null), result)
}
