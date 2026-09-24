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
#' Three modes, three constructors:
#' \itemize{
#'   \item \code{\link{design_hiring_policy_flow}} -- replace a fraction of
#'     exits (e.g. \code{replacement_rate = 1} rehires everyone who left).
#'   \item \code{\link{design_hiring_policy_stock}} -- hire (or downsize)
#'     toward a target headcount.
#'   \item \code{\link{design_hiring_policy_status_quo}} -- reproduce
#'     recently observed hiring rates, calibrated from historical panel data.
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
design_hiring_policy_flow <- function(replacement_rate, group_cols = NULL, salary_scale = NULL) {
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
#' which injects the panel data and the pre-computed rate table
#' automatically -- neither is something you supply here. If the resulting
#' policy is instead passed to \code{\link{simulate_hiring}} directly (a
#' single snapshot, no panel), there is no automatic fallback: you must
#' pre-populate \code{policy_params$squorate_dt} yourself (e.g. by calling
#' \code{\link{estimate_historical_hiring_rates}} ahead of time) before
#' calling \code{simulate_hiring()} standalone.
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
