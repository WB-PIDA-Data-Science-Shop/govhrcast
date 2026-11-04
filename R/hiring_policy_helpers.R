#' Hiring Policy Constructor Helpers
#'
#' @description
#' Small constructors for building the \code{hiring_policy} argument to
#' \code{\link{simulate_hiring}}/\code{\link{simulate_horizon}} without
#' hand-writing the mode-specific list directly.  Each constructor validates
#' its own inputs immediately (at construction time) rather than deferring to
#' a runtime error deep inside the simulation.
#'
#' Unlike \code{\link{design_retirement_policy}}, hiring has no separate
#' "combine two independent things" step: \code{hiring_policy} varies along a
#' single axis (which \code{mode}), and each mode's required fields are just
#' that mode's fields, not a cross-product of two orthogonal choices.  So
#' there is one constructor per mode, each returning the complete policy list
#' directly -- closer to \code{\link{design_retirement_policy_age}} and its
#' siblings than to \code{eligibility_age()}/\code{pension_flat()}.
#'
#' The constructors:
#' \itemize{
#'   \item \code{\link{design_hiring_policy_flow}} -- replace a fraction of
#'     exits (e.g. \code{replacement_rate = 1} rehires everyone who left).
#'   \item \code{\link{design_hiring_policy_stock}} -- hire (or downsize)
#'     toward a target headcount.
#'   \item \code{\link{design_hiring_policy_status_quo}} -- reproduce
#'     recently observed hiring rates, calibrated from historical panel data.
#'   \item \code{\link{design_hiring_policy_adjustment}} -- your own hiring
#'     rates, usually the historical rates with some groups edited: status
#'     quo for most groups and a reform for others.
#' }
#' \code{"combined"} mode (flow + stock simultaneously) has no constructor
#' here -- it has no clean policy interpretation of its own and is expected
#' to see negligible real use; hand-write the list directly for that case.
#'
#' Every constructor returns a \strong{plain list} -- the same shape a
#' hand-written \code{hiring_policy} already has.  Nothing is hidden behind a
#' class: anything not reachable through these constructors remains reachable
#' by editing the returned list directly.
#'
#' @import data.table
#' @name hiring_policy_helpers
#' @keywords internal
NULL


#' @keywords internal
.validate_hiring_salary_scale <- function(salary_scale) {
  if (is.null(salary_scale)) return(invisible(TRUE))
  if (!data.table::is.data.table(salary_scale)) {
    stop("salary_scale must be a data.table or NULL.", call. = FALSE)
  }
  invisible(TRUE)
}


#' Flow-Based Hiring Policy
#'
#' @description
#' Builds a hiring policy that replaces a fraction of exits each period:
#' \code{demand = n_exits x replacement_rate}, where \code{n_exits} comes
#' from \code{simulate_retirement()}'s output for that period (supplied
#' automatically when called through \code{\link{simulate_scenario}}/
#' \code{\link{simulate_horizon}}).  See \code{\link{compute_flow_demand}}.
#'
#' @param replacement_rate Numeric scalar, or data.table with \code{group_cols}
#'   and a \code{replacement_rate} column for group-varying rates (e.g. IT
#'   replaces 100% of leavers, HR replaces 50%).  \code{1.0} replaces every
#'   exit; \code{0} hires no replacements. A data.table here requires
#'   \code{group_cols} to be set.
#' @param group_cols Character vector or \code{NULL} (default).  Columns
#'   defining hiring groups (e.g. \code{"est_id"}).  \code{NULL} for an
#'   overall (ungrouped) policy.
#' @param salary_scale data.table or \code{NULL} (default).  Salary scale
#'   for new hires, keyed on \code{group_cols} plus a numeric salary column.
#'   When \code{NULL} and called through \code{\link{simulate_horizon}},
#'   automatically derived from its \code{salary_scale_dt} argument -- this
#'   only happens inside \code{simulate_horizon()}; standalone
#'   \code{\link{simulate_hiring}} calls need it supplied directly.
#' @param unmatched_rate Number, 0 or more.  Replacement rate for groups that
#'   have leavers but no row in a \code{replacement_rate} table.  Default
#'   \code{0}: nobody is hired to replace them.
#'
#' @return A list with \code{mode = "flow"} plus the fields above -- directly
#'   usable as \code{simulate_hiring()}'s or \code{simulate_horizon()}'s
#'   \code{hiring_policy} argument.
#' @seealso \code{\link{simulate_hiring}}, \code{\link{compute_flow_demand}}
#' @examples
#' design_hiring_policy_flow(replacement_rate = 1.0)
#' design_hiring_policy_flow(
#'   replacement_rate = data.table::data.table(est_id = c("E1", "E2"), replacement_rate = c(1.0, 0.5)),
#'   group_cols = "est_id"
#' )
#' @export
design_hiring_policy_flow <- function(replacement_rate, group_cols = NULL, salary_scale = NULL,
                                      unmatched_rate = 0) {
  validate_positive_number(unmatched_rate, "unmatched_rate", allow_zero = TRUE)
  if (!is.null(group_cols) && !is.character(group_cols)) {
    stop("design_hiring_policy_flow: group_cols must be a character vector or NULL.", call. = FALSE)
  }
  if (data.table::is.data.table(replacement_rate)) {
    if (!"replacement_rate" %in% names(replacement_rate)) {
      stop(
        "design_hiring_policy_flow: replacement_rate data.table must contain ",
        "a 'replacement_rate' column.",
        call. = FALSE
      )
    }
    if (is.null(group_cols)) {
      stop(
        "design_hiring_policy_flow: group_cols must be specified when ",
        "replacement_rate is a data.table.",
        call. = FALSE
      )
    }
    .missing <- setdiff(group_cols, names(replacement_rate))
    if (length(.missing) > 0L) {
      stop(
        "design_hiring_policy_flow: replacement_rate is missing group_cols ",
        "column(s): ", paste(.missing, collapse = ", "), ".",
        call. = FALSE
      )
    }
  } else {
    validate_positive_number(replacement_rate, "replacement_rate", allow_zero = TRUE)
  }
  .validate_hiring_salary_scale(salary_scale)

  list(
    mode             = "flow",
    group_cols       = group_cols,
    replacement_rate = replacement_rate,
    unmatched_rate   = unmatched_rate,
    salary_scale     = salary_scale
  )
}


#' Stock-Based Hiring Policy
#'
#' @description
#' Builds a hiring policy that hires (or downsizes) toward a target
#' headcount: \code{demand = target_stock - current_stock} -- negative values
#' trigger downsizing, using \code{removal_strategy} to select who leaves.
#' See \code{\link{compute_stock_demand}} and
#' \code{\link{select_personnel_to_remove}}.
#'
#' @param stock_targets data.table.  Must contain a \code{target_stock}
#'   column, plus \code{group_cols} if grouped.  If \code{group_cols} is
#'   \code{NULL}, must have exactly one row (an overall target).
#' @param group_cols Character vector or \code{NULL} (default).  Columns
#'   defining hiring groups (e.g. \code{"est_id"}).
#' @param salary_scale data.table or \code{NULL} (default).  See
#'   \code{\link{design_hiring_policy_flow}} for the auto-derivation caveat.
#' @param removal_strategy Character.  \code{"last_hired_first"} (default,
#'   removes the most recently hired first) or \code{"random"}.  Only
#'   relevant when a group's target implies downsizing.
#'
#' @return A list with \code{mode = "stock"} plus the fields above -- directly
#'   usable as \code{simulate_hiring()}'s or \code{simulate_horizon()}'s
#'   \code{hiring_policy} argument.
#' @seealso \code{\link{simulate_hiring}}, \code{\link{compute_stock_demand}}
#' @examples
#' design_hiring_policy_stock(
#'   stock_targets = data.table::data.table(est_id = c("E1", "E2"), target_stock = c(100, 150)),
#'   group_cols = "est_id"
#' )
#' @export
design_hiring_policy_stock <- function(stock_targets,
                                       group_cols       = NULL,
                                       salary_scale      = NULL,
                                       removal_strategy  = "last_hired_first") {
  if (!is.null(group_cols) && !is.character(group_cols)) {
    stop("design_hiring_policy_stock: group_cols must be a character vector or NULL.", call. = FALSE)
  }
  if (!data.table::is.data.table(stock_targets)) {
    stop("design_hiring_policy_stock: stock_targets must be a data.table.", call. = FALSE)
  }
  if (!"target_stock" %in% names(stock_targets)) {
    stop("design_hiring_policy_stock: stock_targets must contain a 'target_stock' column.", call. = FALSE)
  }
  if (!is.null(group_cols)) {
    .missing <- setdiff(group_cols, names(stock_targets))
    if (length(.missing) > 0L) {
      stop(
        "design_hiring_policy_stock: stock_targets is missing group_cols ",
        "column(s): ", paste(.missing, collapse = ", "), ".",
        call. = FALSE
      )
    }
  } else if (nrow(stock_targets) != 1L) {
    stop(
      "design_hiring_policy_stock: when group_cols is NULL, stock_targets ",
      "must have exactly 1 row (an overall target).",
      call. = FALSE
    )
  }
  validate_choice(removal_strategy, c("last_hired_first", "random"), "removal_strategy")
  .validate_hiring_salary_scale(salary_scale)

  list(
    mode             = "stock",
    group_cols       = group_cols,
    stock_targets    = stock_targets,
    salary_scale     = salary_scale,
    removal_strategy = removal_strategy
  )
}


#' Status-Quo Hiring Policy
#'
#' @description
#' Builds a hiring policy that reproduces recently observed hiring behavior:
#' a group-level hiring rate calibrated from historical panel data, applied
#' to the current active stock.  See \code{\link{estimate_historical_hiring_rates}}
#' and \code{\link{compute_status_quo_hiring}}.
#'
#' \strong{Auto-estimation only happens inside \code{\link{simulate_horizon}}},
#' which estimates the rate table from the panel you pass it and stores it in
#' the policy as \code{squorate_dt}.  To set some groups' rates yourself, use
#' \code{\link{design_hiring_policy_adjustment}} instead.  Without
#' rates there is no status quo to reproduce, so \code{simulate_horizon()}
#' stops with an error if the estimate fails (e.g. a \code{group_cols} column
#' the panel does not have) or if the data are a single snapshot and you have
#' not set \code{squorate_dt} yourself.  To project without hiring, pass
#' \code{hiring_policy = NULL} instead.  If the resulting
#' policy is instead passed to \code{\link{simulate_hiring}} directly (a
#' single snapshot, no panel), there is no automatic fallback: you must
#' pre-populate \code{policy_params$squorate_dt} yourself (e.g. by calling
#' \code{\link{estimate_historical_hiring_rates}} ahead of time) before
#' calling \code{simulate_hiring()} standalone.
#'
#' \strong{Each group keeps its historical rate for the whole projection}, and
#' the rate applies to the group's current staff, including people hired
#' earlier in the projection.  A group that hired heavily in the past therefore
#' keeps growing at that pace: a group with a rate of 100\% doubles every year.
#' This is the meaning of "status quo", and it matters more the longer you
#' project, so check the rates from
#' \code{\link{estimate_historical_hiring_rates}} if you project beyond a few
#' years.
#'
#' @param rate_mult Numeric.  Scales the estimated historical rate (e.g.
#'   \code{1.5} hires 50% faster than history). Default \code{1} (no
#'   scaling).
#' @param group_cols Character vector or \code{NULL} (default).  Columns
#'   defining hiring groups (e.g. \code{"est_id"}) -- also the columns the
#'   historical rate is stratified by.
#' @param salary_scale data.table or \code{NULL} (default).  See
#'   \code{\link{design_hiring_policy_flow}} for the auto-derivation caveat.
#'
#' @return A list with \code{mode = "status_quo"} plus the fields above --
#'   directly usable as \code{simulate_hiring()}'s or
#'   \code{simulate_horizon()}'s \code{hiring_policy} argument.
#' @seealso \code{\link{simulate_hiring}},
#'   \code{\link{estimate_historical_hiring_rates}},
#'   \code{\link{compute_status_quo_hiring}}
#' @examples
#' design_hiring_policy_status_quo()
#' design_hiring_policy_status_quo(rate_mult = 1.2, group_cols = "est_id")
#' @export
design_hiring_policy_status_quo <- function(rate_mult    = 1,
                                            group_cols    = NULL,
                                            salary_scale  = NULL) {
  if (!is.null(group_cols) && !is.character(group_cols)) {
    stop("design_hiring_policy_status_quo: group_cols must be a character vector or NULL.", call. = FALSE)
  }
  validate_positive_number(rate_mult, "rate_mult", allow_zero = TRUE)
  .validate_hiring_salary_scale(salary_scale)

  list(
    mode         = "status_quo",
    group_cols   = group_cols,
    rate_mult    = rate_mult,
    salary_scale = salary_scale
  )
}


#' Hiring Policy from Your Own Rate Table
#'
#' @description
#' Builds a hiring policy from hiring rates you control, so that groups can be
#' treated differently: usually the historical rates from
#' \code{\link{estimate_historical_hiring_rates}} with some groups edited.
#' Groups you leave alone stay at their status-quo rate; others can hire
#' faster, slower or not at all.  Each year a group hires
#' \code{hiring_rate x its current staff}, exactly as under
#' \code{\link{design_hiring_policy_status_quo}}.
#'
#' Works with \code{\link{simulate_horizon}} and with
#' \code{\link{simulate_hiring}} on a single snapshot, since nothing needs
#' estimating.
#'
#' The table is checked immediately.  It stops with an error if:
#' \itemize{
#'   \item the \code{hiring_rate} column is missing, or the table is empty;
#'   \item a rate is missing (NA) or negative;
#'   \item a \code{group_cols} column is missing, or a group is listed twice;
#'   \item \code{group_cols = NULL} but the table has more than one row.
#' }
#'
#' @param policy_table data.table with a \code{hiring_rate} column (new hires
#'   per head of current staff, per year: \code{0.05} is 5\%, \code{0} is a
#'   hiring freeze) plus the \code{group_cols} columns.  Rates above 1 are
#'   allowed.  With \code{group_cols = NULL}, exactly one row: an overall rate.
#'   Other columns are ignored, and a copy is stored, so later edits to your
#'   table don't change the policy.
#' @param group_cols Character vector or \code{NULL} (default).  The columns
#'   that identify a group in \code{policy_table} (e.g. \code{"est_id"}).
#' @param unmatched_rate Number, 0 or more.  Hiring rate for groups in the data
#'   that have no row in \code{policy_table}.  Default \code{0}: they hire
#'   nobody.  So to change one group and keep the others at status quo, start
#'   from the full table of historical rates rather than a table of just the
#'   groups you are changing.
#' @param salary_scale data.table or \code{NULL} (default).  See
#'   \code{\link{design_hiring_policy_flow}} for the auto-derivation caveat.
#'
#' @return A list with \code{mode = "status_quo"} plus the rate table --
#'   directly usable as \code{simulate_hiring()}'s or
#'   \code{simulate_horizon()}'s \code{hiring_policy} argument.
#' @seealso \code{\link{estimate_historical_hiring_rates}},
#'   \code{\link{design_hiring_policy_status_quo}}
#' @examples
#' # Per-establishment rates you set yourself; any other establishment hires 2%
#' rates <- data.table::data.table(est_id = c("E1", "E2"), hiring_rate = c(0.05, 0))
#' design_hiring_policy_adjustment(rates, group_cols = "est_id", unmatched_rate = 0.02)
#'
#' \donttest{
#' # Typical reform workflow: estimate, edit, adjust
#' panel_c <- bra_hrmis_contract[ref_date <= as.Date("2012-09-01")]
#' panel_p <- bra_hrmis_personnel[ref_date <= as.Date("2012-09-01")]
#' rates <- estimate_historical_hiring_rates(panel_c, panel_p, group_cols = "est_id")
#' some_est <- rates$est_id[1]
#' rates[est_id == some_est, hiring_rate := 0]   # freeze hiring in one establishment
#' design_hiring_policy_adjustment(rates, group_cols = "est_id")
#' }
#' @export
design_hiring_policy_adjustment <- function(policy_table,
                                            group_cols     = NULL,
                                            unmatched_rate = 0,
                                            salary_scale   = NULL) {
  fn <- "design_hiring_policy_adjustment"
  validate_positive_number(unmatched_rate, "unmatched_rate", allow_zero = TRUE)
  .validate_hiring_salary_scale(salary_scale)

  if (!data.table::is.data.table(policy_table)) {
    stop(fn, ": policy_table must be a data.table (e.g. from ",
         "estimate_historical_hiring_rates()).", call. = FALSE)
  }
  if (!"hiring_rate" %in% names(policy_table)) {
    stop(fn, ": policy_table must contain a 'hiring_rate' column.", call. = FALSE)
  }
  if (nrow(policy_table) == 0L) {
    stop(fn, ": policy_table has no rows. To switch hiring off, pass ",
         "hiring_policy = NULL instead.", call. = FALSE)
  }
  .validate_policy_groups(fn, policy_table, group_cols)

  rate <- policy_table$hiring_rate
  if (!is.numeric(rate) || anyNA(rate) || any(rate < 0)) {
    stop(fn, ": hiring_rate must be numeric, with no NA, 0 or more.", call. = FALSE)
  }

  # Hiring from a rate table is what status-quo mode does once its rates are
  # known, so this is the same policy with the rates already filled in.
  list(
    mode           = "status_quo",
    group_cols     = group_cols,
    squorate_dt    = policy_table[, c(group_cols, "hiring_rate"), with = FALSE],
    unmatched_rate = unmatched_rate,
    salary_scale   = salary_scale
  )
}
