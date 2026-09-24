#' Exit Policy Constructor Helpers
#'
#' @description
#' Constructors for building the \code{exit_policy} argument to
#' \code{\link{simulate_horizon}} (and \code{policy_params} for
#' \code{\link{simulate_exits}}) without hand-writing the three-slot list.
#' "Exits" here means non-retirement attrition: resignations, dismissals and
#' contracts not renewed.  Each constructor checks its inputs as soon as it
#' is called.
#'
#' Three policies:
#' \itemize{
#'   \item \code{\link{design_exit_policy_flat}} -- one annual exit rate for
#'     everyone. Works with a single snapshot of data.
#'   \item \code{\link{design_exit_policy_status_quo}} -- the exit rates
#'     observed in your historical panel, estimated automatically by
#'     \code{simulate_horizon()}, optionally scaled up or down.
#'   \item \code{\link{design_exit_policy_adjustment}} -- your own rate
#'     table, usually \code{\link{estimate_historical_exit_rates}} output with
#'     some rates edited.
#' }
#'
#' Arguments shared by all three:
#' \itemize{
#'   \item \code{exit_strategy} -- who leaves first within a group:
#'     \code{"random"} (default), or the name of a numeric column in the
#'     contract data, in which case the lowest values leave first (e.g.
#'     \code{"gross_salary_lcu"} for lowest-paid first).  An unknown column
#'     name falls back to random.
#'   \item \code{active_types} -- which \code{contract_type} values can exit.
#'     Default \code{c("permanent", "fixed-term", "short-term")}, the
#'     harmonized \code{govhr} values.  These must match the values actually
#'     in your data -- if none match, nobody exits.
#' }
#'
#' Every constructor returns a \strong{plain list}, the same shape as a
#' hand-written \code{exit_policy}; anything not covered here (e.g.
#' \code{defaults$exited_type}) can be set by editing the list directly.
#'
#' @import data.table
#' @name exit_policy_helpers
#' @keywords internal
NULL


# Shared checks for the arguments every exit constructor takes.
#' @keywords internal
.validate_exit_common <- function(fn, exit_strategy, active_types) {
  if (!is.character(exit_strategy) || length(exit_strategy) != 1L || is.na(exit_strategy)) {
    stop(fn, ": exit_strategy must be \"random\" or the name of a numeric column ",
         "(a single character string).", call. = FALSE)
  }
  if (!is.character(active_types) || length(active_types) == 0L || anyNA(active_types)) {
    stop(fn, ": active_types must be a non-empty character vector of contract_type ",
         "values (e.g. \"permanent\").", call. = FALSE)
  }
  invisible(TRUE)
}


#' @keywords internal
.validate_share <- function(fn, x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1) {
    stop(fn, ": ", name, " must be a single number between 0 and 1.", call. = FALSE)
  }
  invisible(TRUE)
}


#' Flat-Rate Exit Policy
#'
#' @description
#' Builds an exit policy where the same share of active staff leaves each
#' period, everywhere: \code{round(active staff x exit_rate)} people exit.
#' Useful when you only have one snapshot of data (so no history to
#' estimate from), or to test a simple assumption such as "5% attrition a
#' year".
#'
#' @param exit_rate Number between 0 and 1.  Share of active staff who leave
#'   each period (e.g. \code{0.05} = 5%).
#' @param exit_strategy,active_types See \code{\link{exit_policy_helpers}}.
#'
#' @return A list with \code{group_cols = NULL}, \code{policy_table = NULL}
#'   and \code{defaults} -- directly usable as \code{simulate_horizon()}'s
#'   \code{exit_policy} or \code{simulate_exits()}'s \code{policy_params}.
#' @seealso \code{\link{design_exit_policy_status_quo}},
#'   \code{\link{design_exit_policy_adjustment}}
#' @examples
#' design_exit_policy_flat(exit_rate = 0.05)
#'
#' # Lowest-paid leave first
#' design_exit_policy_flat(exit_rate = 0.05, exit_strategy = "gross_salary_lcu")
#' @export
design_exit_policy_flat <- function(exit_rate,
                                    exit_strategy = "random",
                                    active_types  = c("permanent", "fixed-term", "short-term")) {
  fn <- "design_exit_policy_flat"
  .validate_share(fn, exit_rate, "exit_rate")
  .validate_exit_common(fn, exit_strategy, active_types)
  list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults = list(
      exit_rate     = exit_rate,
      exit_strategy = exit_strategy,
      active_types  = active_types
    )
  )
}


#' Status-Quo Exit Policy
#'
#' @description
#' Builds an exit policy that reproduces recently observed attrition.
#' \code{policy_table} is left \code{NULL}, which tells
#' \code{\link{simulate_horizon}} to estimate the historical exit rate per
#' group from the full panel before the first period (see
#' \code{\link{estimate_historical_exit_rates}} for exactly how).
#'
#' \strong{Use this with \code{simulate_horizon()} and panel data} (at least
#' two \code{ref_date} snapshots).  With single-snapshot data there is
#' nothing to estimate from: \code{simulate_horizon()} warns and models no
#' exits -- use \code{\link{design_exit_policy_flat}} instead.  Passed to
#' \code{\link{simulate_exits}} directly, this policy stops with an error,
#' since that function never estimates rates itself; estimate them with
#' \code{\link{estimate_historical_exit_rates}} and use
#' \code{\link{design_exit_policy_adjustment}} instead.
#'
#' @param group_cols Character vector or \code{NULL} (default).  Columns to
#'   estimate separate exit rates for (e.g. \code{"est_id"} for one rate per
#'   establishment).  \code{NULL} estimates one overall rate.  Finer groups
#'   capture more variation but rest on fewer people each.
#' @param exit_multiplier Number, 0 or more.  Scales every estimated rate --
#'   \code{1} (default) is the status quo, \code{1.2} means 20% more exits
#'   than history, \code{0.5} half as many.
#' @param exit_strategy,active_types See \code{\link{exit_policy_helpers}}.
#'
#' @return A list with \code{group_cols}, \code{policy_table = NULL} and
#'   \code{defaults} -- directly usable as \code{simulate_horizon()}'s
#'   \code{exit_policy}.
#' @seealso \code{\link{estimate_historical_exit_rates}},
#'   \code{\link{design_exit_policy_adjustment}}
#' @examples
#' design_exit_policy_status_quo()
#'
#' # Per-establishment rates, 20% above history
#' design_exit_policy_status_quo(group_cols = "est_id", exit_multiplier = 1.2)
#' @export
design_exit_policy_status_quo <- function(group_cols      = NULL,
                                          exit_multiplier = 1,
                                          exit_strategy   = "random",
                                          active_types    = c("permanent", "fixed-term", "short-term")) {
  fn <- "design_exit_policy_status_quo"
  if (!is.null(group_cols) && (!is.character(group_cols) || length(group_cols) == 0L)) {
    stop(fn, ": group_cols must be a character vector or NULL.", call. = FALSE)
  }
  if (!is.numeric(exit_multiplier) || length(exit_multiplier) != 1L ||
      is.na(exit_multiplier) || exit_multiplier < 0) {
    stop(fn, ": exit_multiplier must be a single number, 0 or more.", call. = FALSE)
  }
  .validate_exit_common(fn, exit_strategy, active_types)
  list(
    group_cols   = group_cols,
    policy_table = NULL,
    defaults = list(
      exit_multiplier = exit_multiplier,
      exit_strategy   = exit_strategy,
      active_types    = active_types
    )
  )
}


#' Exit Policy from Your Own Rate Table
#'
#' @description
#' Builds an exit policy from a table of exit rates you control -- usually
#' the output of \code{\link{estimate_historical_exit_rates}} after editing
#' some rates to represent a reform (e.g. halving exits in the health
#' sector).  Works with \code{\link{simulate_horizon}} and with
#' \code{\link{simulate_exits}} on a single snapshot, since nothing needs
#' estimating.
#'
#' The table is checked immediately.  It stops with an error if:
#' \itemize{
#'   \item the \code{exit_rate} column is missing, or the table is empty;
#'   \item a rate is missing (NA) or outside 0--1;
#'   \item a \code{group_cols} column is missing, or a group is listed twice;
#'   \item \code{group_cols = NULL} but the table has more than one row.
#' }
#'
#' @param policy_table data.table with an \code{exit_rate} column (share of
#'   the group's active staff who leave per period) plus the
#'   \code{group_cols} columns.  With \code{group_cols = NULL}, exactly one
#'   row: an overall rate.  May also have an \code{exit_multiplier} column to
#'   scale some groups' rates without editing \code{exit_rate} itself.  A
#'   copy is stored, so later edits to your table don't change the policy.
#' @param group_cols Character vector or \code{NULL} (default).  The columns
#'   that identify a group in \code{policy_table} (e.g. \code{"est_id"}).
#'   Must be the \code{group_cols} the table was estimated with.
#' @param unmatched_rate Number between 0 and 1.  Exit rate for groups in
#'   the data that have no row in \code{policy_table} (e.g. an establishment
#'   created after the history you estimated from).  Default \code{0}: they
#'   have no exits.
#' @param exit_strategy,active_types See \code{\link{exit_policy_helpers}}.
#'
#' @return A list with \code{group_cols}, \code{policy_table} and
#'   \code{defaults} -- directly usable as \code{simulate_horizon()}'s
#'   \code{exit_policy} or \code{simulate_exits()}'s \code{policy_params}.
#' @seealso \code{\link{estimate_historical_exit_rates}},
#'   \code{\link{design_exit_policy_status_quo}}
#' @examples
#' # One overall rate
#' design_exit_policy_adjustment(data.table::data.table(exit_rate = 0.04))
#'
#' # Per-establishment rates you set yourself; any other establishment gets 3%
#' rates <- data.table::data.table(est_id = c("E1", "E2"), exit_rate = c(0.02, 0.06))
#' design_exit_policy_adjustment(rates, group_cols = "est_id", unmatched_rate = 0.03)
#'
#' \donttest{
#' # Typical reform workflow: estimate, edit, adjust
#' panel_c <- bra_hrmis_contract[ref_date <= as.Date("2012-09-01")]
#' panel_p <- bra_hrmis_personnel[ref_date <= as.Date("2012-09-01")]
#' rates <- estimate_historical_exit_rates(panel_c, panel_p, group_cols = "est_id")
#' rates[, exit_rate := exit_rate * 0.5]   # halve exits everywhere
#' design_exit_policy_adjustment(rates, group_cols = "est_id")
#' }
#' @export
design_exit_policy_adjustment <- function(policy_table,
                                          group_cols     = NULL,
                                          unmatched_rate = 0,
                                          exit_strategy  = "random",
                                          active_types   = c("permanent", "fixed-term", "short-term")) {
  fn <- "design_exit_policy_adjustment"
  .validate_share(fn, unmatched_rate, "unmatched_rate")
  .validate_exit_common(fn, exit_strategy, active_types)

  if (!data.table::is.data.table(policy_table)) {
    stop(fn, ": policy_table must be a data.table (e.g. from ",
         "estimate_historical_exit_rates()).", call. = FALSE)
  }
  if (!"exit_rate" %in% names(policy_table)) {
    stop(fn, ": policy_table must contain an 'exit_rate' column.", call. = FALSE)
  }
  if (nrow(policy_table) == 0L) {
    stop(fn, ": policy_table has no rows. To switch exits off, pass ",
         "exit_policy = NULL instead.", call. = FALSE)
  }

  if (is.null(group_cols)) {
    if (nrow(policy_table) != 1L) {
      stop(fn, ": with group_cols = NULL, policy_table must have exactly one row ",
           "(an overall rate). It has ", nrow(policy_table), " -- set group_cols ",
           "to the columns that tell the rows apart.", call. = FALSE)
    }
  } else {
    if (!is.character(group_cols) || length(group_cols) == 0L) {
      stop(fn, ": group_cols must be a character vector or NULL.", call. = FALSE)
    }
    .missing <- setdiff(group_cols, names(policy_table))
    if (length(.missing) > 0L) {
      stop(fn, ": policy_table is missing group_cols column(s): ",
           paste(.missing, collapse = ", "), ".", call. = FALSE)
    }
    if (anyDuplicated(policy_table, by = group_cols)) {
      stop(fn, ": policy_table has more than one row for the same group (",
           paste(group_cols, collapse = ", "), "). Each group must appear once.",
           call. = FALSE)
    }
  }

  .rate <- policy_table$exit_rate
  if (!is.numeric(.rate) || anyNA(.rate) || any(.rate < 0 | .rate > 1)) {
    stop(fn, ": exit_rate must be numeric, with no NA, between 0 and 1.", call. = FALSE)
  }
  if ("exit_multiplier" %in% names(policy_table)) {
    .mult <- policy_table$exit_multiplier
    if (!is.numeric(.mult) || anyNA(.mult) || any(.mult < 0)) {
      stop(fn, ": exit_multiplier must be numeric, with no NA, 0 or more.", call. = FALSE)
    }
  }

  list(
    group_cols   = group_cols,
    policy_table = data.table::copy(policy_table),
    defaults = list(
      exit_rate     = unmatched_rate,
      exit_strategy = exit_strategy,
      active_types  = active_types
    )
  )
}
