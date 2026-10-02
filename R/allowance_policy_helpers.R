#' Allowance Policy Constructor Helpers
#'
#' @description
#' Constructors for building the \code{allowance_policy} argument to
#' \code{\link{simulate_horizon}} (and \code{policy_params} for
#' \code{\link{simulate_allowances}}) without hand-writing the list.  Each
#' constructor checks its inputs as soon as it is called.
#'
#' An allowance policy gives every active contract an amount for each
#' allowance type, looked up by the contract's group (e.g. its
#' establishment).  The allowance type is always part of that lookup, so
#' you only name the contract-level grouping (e.g. \code{"est_id"}) and the
#' helpers add \code{"allowance_type"} for you.
#'
#' Two policies:
#' \itemize{
#'   \item \code{\link{design_allowance_policy_status_quo}} -- the amounts
#'     paid at a baseline snapshot of your data, estimated immediately.
#'   \item \code{\link{design_allowance_policy_adjustment}} -- your own
#'     amount table, usually \code{\link{estimate_baseline_allowance_scale}}
#'     output with some amounts edited, or rows added for a new allowance.
#' }
#'
#' Both take \code{total_multiplier}, which scales each contract's total
#' across all allowance types (e.g. \code{0.9} is a 10% cut to everyone's
#' allowances) without touching the per-type amounts.  Give one number to
#' scale everyone, or a table with a \code{total_allowance_multiplier}
#' column plus \code{multiplier_group_cols} to scale only some groups.
#'
#' \strong{Avoid double counting}: when allowances are modelled, the salary
#' column tracked by the rest of the simulation should be \emph{base} pay
#' (e.g. \code{base_salary_lcu}), not a gross figure that already includes
#' allowances -- see \code{\link{simulate_allowances}}.
#'
#' Inside \code{simulate_horizon()}, amounts grow each period by
#' \code{allowance_growth_rate}.
#'
#' @import data.table
#' @name allowance_policy_helpers
#' @keywords internal
NULL


#' Status-Quo Allowance Policy
#'
#' @description
#' Builds an allowance policy that pays what was paid at a baseline
#' snapshot: for each group and allowance type, the average amount across
#' \emph{all} contracts in the group, counting contracts that received none
#' of that allowance as 0 (see \code{\link{estimate_baseline_allowance_scale}}
#' for why).  Applied to the same snapshot, this reproduces the actual
#' allowance total for the group.
#'
#' Unlike the other status-quo helpers, the amounts are estimated right
#' away from the data you pass in -- \code{simulate_horizon()} does not
#' receive allowance data, so it cannot estimate them for you.
#'
#' @param panel_allowance_dt data.table.  Long-format allowance data, one row
#'   per contract x snapshot x allowance type (e.g.
#'   \code{bra_hrmis_allowance}).  May span many snapshots; only the one at
#'   \code{ref_date} is used.
#' @param panel_contract_dt data.table.  Contract data covering
#'   \code{ref_date}; supplies each contract's group.
#' @param group_cols Character vector.  Contract-level columns that define
#'   the groups amounts are averaged over (e.g. \code{"est_id"} or
#'   \code{c("est_id", "paygrade")}).  Finer groups follow real pay more
#'   closely but rest on fewer contracts each.
#' @param ref_date Date.  The baseline snapshot -- normally the same
#'   \code{ref_date} you pass to \code{simulate_horizon()}.  The nearest
#'   snapshot on or before it is used.
#' @param total_multiplier,multiplier_group_cols See
#'   \code{\link{allowance_policy_helpers}}.  Default \code{1}: no change.
#' @param allowance_type_col,amount_col,contract_id_col,ref_date_col
#'   Character.  Column names, if they differ from the defaults.
#'   \code{amount_col} must already be numeric.
#'
#' @return A list with \code{group_cols} (your columns plus
#'   \code{"allowance_type"}), \code{policy_table}, \code{defaults} and the
#'   multiplier fields -- directly usable as \code{simulate_horizon()}'s
#'   \code{allowance_policy}.  Groups absent from the baseline (e.g. a new
#'   establishment) get 0.
#' @seealso \code{\link{estimate_baseline_allowance_scale}},
#'   \code{\link{design_allowance_policy_adjustment}}
#' @examples
#' ap <- design_allowance_policy_status_quo(
#'   panel_allowance_dt = bra_hrmis_allowance,
#'   panel_contract_dt  = bra_hrmis_contract,
#'   group_cols         = "est_id",
#'   ref_date           = as.Date("2012-09-01")
#' )
#' head(ap$policy_table)
#'
#' # The same, with a 10% cut to everyone's total allowance
#' ap_cut <- design_allowance_policy_status_quo(
#'   bra_hrmis_allowance, bra_hrmis_contract, group_cols = "est_id",
#'   ref_date = as.Date("2012-09-01"), total_multiplier = 0.9
#' )
#' @export
design_allowance_policy_status_quo <- function(panel_allowance_dt,
                                               panel_contract_dt,
                                               group_cols,
                                               ref_date,
                                               total_multiplier      = 1,
                                               multiplier_group_cols = NULL,
                                               allowance_type_col    = "allowance_type",
                                               amount_col            = "allowance_lcu",
                                               contract_id_col       = "contract_id",
                                               ref_date_col          = "ref_date") {
  fn <- "design_allowance_policy_status_quo"
  if (is.null(group_cols) || !is.character(group_cols) || length(group_cols) == 0L) {
    stop(fn, ": group_cols must be a non-empty character vector (e.g. \"est_id\").",
         call. = FALSE)
  }

  scale_dt <- estimate_baseline_allowance_scale(
    panel_allowance_dt = panel_allowance_dt,
    panel_contract_dt  = panel_contract_dt,
    group_cols         = setdiff(group_cols, "allowance_type"),
    ref_date           = ref_date,
    allowance_type_col = allowance_type_col,
    amount_col         = amount_col,
    contract_id_col    = contract_id_col,
    ref_date_col       = ref_date_col
  )

  .build_allowance_policy(fn, scale_dt, group_cols, total_multiplier,
                          multiplier_group_cols, unmatched_amount = 0)
}


#' Allowance Policy from Your Own Amount Table
#'
#' @description
#' Builds an allowance policy from a table of amounts you control -- usually
#' \code{\link{estimate_baseline_allowance_scale}} output with some amounts
#' edited (e.g. a 10% rise in hazard pay), or with rows added for an
#' allowance that doesn't exist yet.  A table with only
#' \code{allowance_type} and \code{amount} gives every active contract the
#' same amount of each type.
#'
#' The table is checked immediately.  It stops with an error if:
#' \itemize{
#'   \item the \code{allowance_type} or \code{amount} column, or a
#'     \code{group_cols} column, is missing;
#'   \item the table is empty;
#'   \item an \code{allowance_type} is NA;
#'   \item a group and allowance type combination is listed twice;
#'   \item an amount is missing (NA) or negative.
#' }
#'
#' @param policy_table data.table with \code{allowance_type} and
#'   \code{amount} (per contract, per period) columns plus the
#'   \code{group_cols} columns.  A copy is stored, so later edits to your
#'   table don't change the policy.
#' @param group_cols Character vector or \code{NULL} (default).  The
#'   contract-level columns that identify a group in \code{policy_table}
#'   (e.g. \code{"est_id"}).  \code{"allowance_type"} is added
#'   automatically.  \code{NULL} means amounts depend on the allowance type
#'   only.
#' @param total_multiplier,multiplier_group_cols See
#'   \code{\link{allowance_policy_helpers}}.  Default \code{1}: no change.
#' @param unmatched_amount Number, 0 or more.  Amount, for each allowance
#'   type in the table, paid to contracts whose group has no row for that
#'   type (e.g. an establishment created after the baseline).  Default
#'   \code{0}.
#'
#' @return A list with \code{group_cols} (your columns plus
#'   \code{"allowance_type"}), \code{policy_table}, \code{defaults} and the
#'   multiplier fields -- directly usable as \code{simulate_horizon()}'s
#'   \code{allowance_policy} or \code{simulate_allowances()}'s
#'   \code{policy_params}.
#' @seealso \code{\link{estimate_baseline_allowance_scale}},
#'   \code{\link{design_allowance_policy_status_quo}}
#' @examples
#' # A new transport allowance of 200 for every active contract
#' design_allowance_policy_adjustment(
#'   data.table::data.table(allowance_type = "transport", amount = 200)
#' )
#'
#' # Reform workflow: baseline amounts, raise one type, cut totals in one establishment
#' scale_dt <- estimate_baseline_allowance_scale(
#'   bra_hrmis_allowance, bra_hrmis_contract,
#'   group_cols = "est_id", ref_date = as.Date("2012-09-01")
#' )
#' scale_dt[, amount := amount * 1.1]   # 10% rise across all types
#' cut <- data.table::data.table(est_id = scale_dt$est_id[1], total_allowance_multiplier = 0.8)
#' design_allowance_policy_adjustment(
#'   scale_dt, group_cols = "est_id",
#'   total_multiplier = cut, multiplier_group_cols = "est_id"
#' )
#' @export
design_allowance_policy_adjustment <- function(policy_table,
                                               group_cols            = NULL,
                                               total_multiplier      = 1,
                                               multiplier_group_cols = NULL,
                                               unmatched_amount      = 0) {
  .build_allowance_policy("design_allowance_policy_adjustment", policy_table, group_cols,
                          total_multiplier, multiplier_group_cols, unmatched_amount)
}


# Validates an allowance amount table plus the multiplier arguments and
# assembles the policy list resolve_allowance_amounts() consumes.
#' @keywords internal
.build_allowance_policy <- function(fn, policy_table, group_cols, total_multiplier,
                                    multiplier_group_cols, unmatched_amount) {

  # --- amount table ------------------------------------------------------
  if (!is.null(group_cols) && (!is.character(group_cols) || length(group_cols) == 0L)) {
    stop(fn, ": group_cols must be a character vector or NULL.", call. = FALSE)
  }
  # allowance_type is always a lookup key: amounts are per type.
  full_group_cols <- unique(c(setdiff(group_cols, "allowance_type"), "allowance_type"))

  if (!data.table::is.data.table(policy_table)) {
    stop(fn, ": policy_table must be a data.table (e.g. from ",
         "estimate_baseline_allowance_scale()).", call. = FALSE)
  }
  .missing <- setdiff(c(full_group_cols, "amount"), names(policy_table))
  if (length(.missing) > 0L) {
    stop(fn, ": policy_table is missing column(s): ", paste(.missing, collapse = ", "), ".",
         call. = FALSE)
  }
  if (nrow(policy_table) == 0L) {
    stop(fn, ": policy_table has no rows. To switch allowances off, pass ",
         "allowance_policy = NULL instead.", call. = FALSE)
  }
  if (anyNA(policy_table$allowance_type)) {
    stop(fn, ": allowance_type must not contain NA.", call. = FALSE)
  }
  if (anyDuplicated(policy_table, by = full_group_cols)) {
    stop(fn, ": policy_table has more than one row for the same group and ",
         "allowance_type (", paste(full_group_cols, collapse = ", "), "). ",
         "Each combination must appear once.", call. = FALSE)
  }
  .amt <- policy_table$amount
  if (!is.numeric(.amt) || anyNA(.amt) || any(.amt < 0)) {
    stop(fn, ": amount must be numeric, with no NA, 0 or more.", call. = FALSE)
  }
  if (!is.numeric(unmatched_amount) || length(unmatched_amount) != 1L ||
      is.na(unmatched_amount) || unmatched_amount < 0) {
    stop(fn, ": unmatched_amount must be a single number, 0 or more.", call. = FALSE)
  }

  # --- total multiplier --------------------------------------------------
  multiplier_default <- 1
  multiplier_table   <- NULL
  if (data.table::is.data.table(total_multiplier)) {
    if (is.null(multiplier_group_cols) || !is.character(multiplier_group_cols) ||
        length(multiplier_group_cols) == 0L) {
      stop(fn, ": multiplier_group_cols must be set when total_multiplier is a ",
           "data.table.", call. = FALSE)
    }
    if ("allowance_type" %in% multiplier_group_cols) {
      stop(fn, ": multiplier_group_cols must not include 'allowance_type' -- the ",
           "multiplier scales a contract's total across all types. Edit the ",
           "per-type amounts in policy_table instead.", call. = FALSE)
    }
    .missing_m <- setdiff(c(multiplier_group_cols, "total_allowance_multiplier"),
                          names(total_multiplier))
    if (length(.missing_m) > 0L) {
      stop(fn, ": total_multiplier table is missing column(s): ",
           paste(.missing_m, collapse = ", "), ".", call. = FALSE)
    }
    if (anyDuplicated(total_multiplier, by = multiplier_group_cols)) {
      stop(fn, ": total_multiplier table has more than one row for the same group.",
           call. = FALSE)
    }
    .m <- total_multiplier$total_allowance_multiplier
    if (!is.numeric(.m) || anyNA(.m) || any(.m < 0)) {
      stop(fn, ": total_allowance_multiplier must be numeric, with no NA, 0 or more.",
           call. = FALSE)
    }
    multiplier_table <- data.table::copy(total_multiplier)
  } else {
    if (!is.null(multiplier_group_cols)) {
      stop(fn, ": multiplier_group_cols only applies when total_multiplier is a ",
           "data.table; with a single number it scales everyone.", call. = FALSE)
    }
    if (!is.numeric(total_multiplier) || length(total_multiplier) != 1L ||
        is.na(total_multiplier) || total_multiplier < 0) {
      stop(fn, ": total_multiplier must be a single number (0 or more) or a ",
           "data.table.", call. = FALSE)
    }
    multiplier_default <- total_multiplier
  }

  list(
    group_cols              = full_group_cols,
    policy_table            = data.table::copy(policy_table),
    defaults                = list(amount = unmatched_amount),
    multiplier_group_cols   = multiplier_group_cols,
    multiplier_policy_table = multiplier_table,
    multiplier_default      = multiplier_default
  )
}
