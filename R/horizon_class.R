# R/horizon_class.R
# =============================================================================
# S3 class: `horizon`
# =============================================================================
# Returned by simulate_horizon() and generate_scenario_matrix().
# Structure:
#   $comparison  — data.table  one row per (scenario × period)
#   $metadata    — list        policy_args + call metadata
# =============================================================================

#' Constructor for the `horizon` S3 class
#'
#' @description
#' Wraps a simulation summary table and its generating metadata into a
#' lightweight S3 object.  End-users should not call this directly; it is
#' invoked by \code{\link{simulate_horizon}} and
#' \code{\link{generate_scenario_matrix}}.
#'
#' @param comparison data.table.  One row per (scenario × period).  Must
#'   contain at least \code{period_date} and \code{wage_bill_end}.
#' @param metadata Named list.  Policy arguments and call context used to
#'   generate the simulation (captured via \code{match.call()} in the calling
#'   function).
#'
#' @return An object of class \code{horizon}.
#' @keywords internal
new_horizon <- function(comparison, metadata = list()) {
  stopifnot(data.table::is.data.table(comparison))
  # summary_dt is kept as a backward-compatible alias so that existing code
  # using res$summary_dt continues to work without modification.
  structure(
    list(
      comparison = comparison,
      summary_dt = comparison,   # alias — same object
      metadata   = metadata
    ),
    class = "horizon"
  )
}


#' Validate a `horizon` object
#'
#' @param x Object to check.
#' @return Invisible \code{x} if valid; stops with informative error otherwise.
#' @keywords internal
validate_horizon <- function(x) {
  if (!inherits(x, "horizon")) {
    stop("Object is not of class 'horizon'.", call. = FALSE)
  }
  if (!is.list(x) || !all(c("comparison", "metadata") %in% names(x))) {
    stop("A 'horizon' object must contain '$comparison' and '$metadata'.",
         call. = FALSE)
  }
  if (!data.table::is.data.table(x$comparison)) {
    stop("'horizon$comparison' must be a data.table.", call. = FALSE)
  }
  required_cols <- c("period_date", "wage_bill_end")
  missing_cols  <- setdiff(required_cols, names(x$comparison))
  if (length(missing_cols) > 0L) {
    stop("'horizon$comparison' is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(x)
}


#' Test whether an object is a `horizon`
#'
#' @param x Any R object.
#' @return Logical scalar.
#' @export
is.horizon <- function(x) inherits(x, "horizon")


# =============================================================================
# print / format / summary methods
# =============================================================================

#' Print a `horizon` object
#'
#' @description
#' Concise console summary: scenario count, period range, and headline
#' figures for the first and last period.
#'
#' @param x A \code{horizon} object.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.horizon <- function(x, ...) {
  validate_horizon(x)
  dt <- x$comparison

  n_scenarios <- if ("scenario" %in% names(dt)) data.table::uniqueN(dt$scenario) else 1L
  n_periods   <- if ("scenario" %in% names(dt)) {
    data.table::uniqueN(dt[, .SD, .SDcols = "period_date"])
  } else nrow(dt)

  date_range <- range(dt$period_date, na.rm = TRUE)

  cat("<horizon>\n")
  cat(sprintf("  Scenarios : %d\n", n_scenarios))
  cat(sprintf("  Periods   : %d  (%s \u2013 %s)\n",
              n_periods,
              format(date_range[1], "%Y-%m-%d"),
              format(date_range[2], "%Y-%m-%d")))
  cat(sprintf("  Columns   : %d\n", ncol(dt)))

  if (n_scenarios <= 4L) {
    scen_col <- if ("scenario" %in% names(dt)) dt$scenario else rep("(single)", nrow(dt))
    cat("  Scenario labels:\n")
    for (s in unique(scen_col)) cat(sprintf("    - %s\n", s))
  }

  invisible(x)
}


#' One-line summary of a `horizon` object
#'
#' @description
#' Prints the final-period wage bill and headcount for every scenario.
#'
#' @param object A \code{horizon} object.
#' @param ... Ignored.
#' @return Invisibly returns a data.table with the summary.
#' @export
summary.horizon <- function(object, ...) {
  validate_horizon(object)
  dt <- object$comparison

  keep_cols <- intersect(
    c("scenario", "period_date", "n_headcount_end",
      "wage_bill_end", "pension_cost_total", "n_exits", "n_hires"),
    names(dt)
  )

  # Final period per scenario
  if ("scenario" %in% names(dt)) {
    out <- dt[dt[, .I[which.max(period_date)], by = "scenario"]$V1, ..keep_cols]
  } else {
    out <- dt[nrow(dt), ..keep_cols]
  }

  print(out)
  invisible(out)
}
