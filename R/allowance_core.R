#' Estimate a Baseline Allowance Scale from a Single-Period Snapshot
#'
#' @description
#' Computes, for each combination of \code{group_cols} and
#' \code{allowance_type}, the **unconditional mean** allowance amount across
#' *all* contracts in the group -- including contracts that receive no
#' allowance of that type at all (an implicit zero). This is deliberate: a
#' deterministic per-contract assignment scale (see
#' \code{\link{resolve_allowance_amounts}}) that grants the group's mean to
#' *every* contract will only reproduce the group's real aggregate cost if
#' the mean already accounts for non-recipients. Do not "fix" this into a
#' conditional mean (averaging only over recipients) -- that would silently
#' inflate the modeled cost relative to history, since every contract would
#' then be assigned an amount that assumes 100% incidence.
#'
#' Unlike \code{\link{estimate_historical_movement_rates}} or
#' \code{\link{estimate_historical_exit_rates}}, this is a **single-snapshot**
#' calculation -- it does not compute anything over time. \code{panel_allowance_dt}/
#' \code{panel_contract_dt} may be full multi-period panels (as
#' \code{bra_hrmis_allowance}/\code{bra_hrmis_contract} are); this function
#' resolves \code{ref_date} to the nearest available snapshot on or before it
#' (via \code{select_nearest_ref_date()}) and subsets both tables to that
#' snapshot before computing anything -- the same helper and pattern
#' \code{simulate_retirement()}/\code{simulate_hiring()}/
#' \code{simulate_promotions_transfers()} use to select their own snapshot
#' from panel data.
#'
#' @param panel_allowance_dt data.table. Long-format allowance data, one row
#'   per \code{contract_id} x \code{ref_date} x \code{allowance_type}, with a
#'   numeric amount column. May span multiple \code{ref_date}s -- only the
#'   resolved snapshot is used. A contract absent from this table for a given
#'   \code{allowance_type} (at that snapshot) is treated as receiving 0 of
#'   that allowance -- the mean is not conditioned on the contract appearing
#'   here.
#' @param panel_contract_dt data.table. Contract data covering at least
#'   \code{ref_date}. Defines the full population of contracts in each group
#'   -- including contracts entirely absent from \code{panel_allowance_dt} --
#'   and supplies \code{group_cols}, since \code{panel_allowance_dt}
#'   typically carries no grouping columns of its own (e.g. \code{est_id}).
#' @param group_cols Character vector. Columns in \code{panel_contract_dt}
#'   that define assignment groups (e.g. \code{"est_id"} or
#'   \code{c("est_id", "paygrade")}).
#' @param ref_date Date. The baseline snapshot to build the scale from --
#'   typically the same \code{ref_date} passed to \code{\link{simulate_horizon}}.
#'   Resolved to the nearest available panel date on or before it; does not
#'   need to land exactly on a snapshot date.
#' @param allowance_type_col Character. Default \code{"allowance_type"}.
#' @param amount_col Character. Default \code{"allowance_lcu"}. Must already
#'   be numeric -- this function does not parse currency strings.
#' @param contract_id_col Character. Default \code{"contract_id"}.
#' @param ref_date_col Character. Default \code{"ref_date"}.
#'
#' @return data.table with columns \code{group_cols}, \code{allowance_type},
#'   \code{amount}. Usable directly as \code{policy_params$policy_table} in
#'   \code{\link{resolve_allowance_amounts}}.
#'
#' @examples
#' al <- data.table::data.table(
#'   contract_id    = c("C1", "C1", "C2", "C2"),
#'   ref_date       = as.Date("2020-01-01"),
#'   allowance_type = c("hazard", "transport", "hazard", "transport"),
#'   allowance_lcu  = c(100, 0, 0, 0)
#' )
#' ct <- data.table::data.table(
#'   contract_id = c("C1", "C2", "C3"),   # C3 has no allowance rows at all
#'   ref_date    = as.Date("2020-01-01"),
#'   est_id      = c("E1", "E1", "E1")
#' )
#'
#' estimate_baseline_allowance_scale(al, ct, group_cols = "est_id", ref_date = as.Date("2020-01-01"))
#' # hazard: (100 + 0 + 0) / 3 = 33.3 -- C3's implicit zero pulls the mean down,
#' # exactly as intended for a deterministic per-contract assignment scale.
#'
#' # With the bundled Alagoas data, as the baseline for simulate_horizon()'s
#' # allowance_policy:
#' scale_dt <- estimate_baseline_allowance_scale(
#'   panel_allowance_dt = bra_hrmis_allowance,
#'   panel_contract_dt  = bra_hrmis_contract,
#'   group_cols         = "est_id",
#'   ref_date           = as.Date("2012-09-01")
#' )
#' head(scale_dt)
#'
#' @export
estimate_baseline_allowance_scale <- function(panel_allowance_dt,
                                              panel_contract_dt,
                                              group_cols,
                                              ref_date,
                                              allowance_type_col = "allowance_type",
                                              amount_col         = "allowance_lcu",
                                              contract_id_col    = "contract_id",
                                              ref_date_col       = "ref_date") {

  validate_datatable(panel_allowance_dt, "panel_allowance_dt")
  validate_datatable(panel_contract_dt, "panel_contract_dt")
  if (is.null(group_cols) || length(group_cols) == 0L)
    stop("group_cols must be specified for allowance scale estimation.", call. = FALSE)

  ref_date <- validate_date_format(ref_date, "ref_date")

  missing_contract_cols <- setdiff(c(group_cols, contract_id_col, ref_date_col),
                                    names(panel_contract_dt))
  if (length(missing_contract_cols) > 0L)
    stop("estimate_baseline_allowance_scale: columns not found in panel_contract_dt: ",
         paste(missing_contract_cols, collapse = ", "), call. = FALSE)

  missing_allowance_cols <- setdiff(c(allowance_type_col, amount_col, contract_id_col, ref_date_col),
                                     names(panel_allowance_dt))
  if (length(missing_allowance_cols) > 0L)
    stop("estimate_baseline_allowance_scale: columns not found in panel_allowance_dt: ",
         paste(missing_allowance_cols, collapse = ", "), call. = FALSE)

  # Select the baseline snapshot -- inputs may be full multi-period panels.
  # select_nearest_ref_date() (R/utils.R) finds the nearest available date on
  # or before ref_date, the same helper simulate_retirement()/simulate_hiring()/
  # simulate_promotions_transfers() use for this exact purpose -- so ref_date
  # need not land exactly on a panel snapshot date. Resolving to a
  # differently-named variable also sidesteps the classic data.table pitfall
  # where a bare `ref_date` inside DT[...] would resolve to a same-named
  # ref_date COLUMN instead of this function's parameter.
  selected_ref_date <- select_nearest_ref_date(panel_contract_dt[[ref_date_col]], ref_date)
  panel_contract_dt  <- panel_contract_dt[get(ref_date_col) == selected_ref_date]
  panel_allowance_dt <- panel_allowance_dt[get(ref_date_col) == selected_ref_date]

  if (!is.numeric(panel_allowance_dt[[amount_col]]))
    stop("estimate_baseline_allowance_scale: '", amount_col,
         "' must be numeric. Clean currency-formatted strings before calling ",
         "(see data-raw/prep_allowance_module.R for the pattern used on the ",
         "bundled example data).", call. = FALSE)

  allowance_types <- unique(panel_allowance_dt[[allowance_type_col]])
  allowance_types <- allowance_types[!is.na(allowance_types)]
  if (length(allowance_types) == 0L)
    stop("estimate_baseline_allowance_scale: no allowance types found in panel_allowance_dt.",
         call. = FALSE)

  # Full universe: every contract in panel_contract_dt x every allowance_type.
  # This is what makes the mean "unconditional" -- a contract missing from
  # panel_allowance_dt entirely, or missing just one allowance_type, still
  # enters the denominator (with amount 0), not just the contracts that
  # happen to already have a row in panel_allowance_dt.
  contracts_slim <- unique(panel_contract_dt[, c(contract_id_col, 
                                                 ref_date_col, 
                                                 group_cols), 
                                             with = FALSE])

  universe <- contracts_slim[rep(seq_len(.N), length(allowance_types))]
  universe[, (allowance_type_col) := rep(allowance_types, each = nrow(contracts_slim))]

  allowance_slim <- panel_allowance_dt[, c(contract_id_col, ref_date_col, allowance_type_col, amount_col), with = FALSE]

  joined <- allowance_slim[universe, on = c(contract_id_col, ref_date_col, allowance_type_col)]
  joined[is.na(get(amount_col)), (amount_col) := 0]

  scale_dt <- joined[, list(amount = mean(get(amount_col), na.rm = TRUE)),
                      by = c(group_cols, allowance_type_col)]

  if (allowance_type_col != "allowance_type")
    data.table::setnames(scale_dt, allowance_type_col, "allowance_type")

  scale_dt
}


#' Resolve Per-Contract Allowance Amounts from a Policy
#'
#' @description
#' Builds a long working table -- one row per active contract x requested
#' \code{allowance_type} -- and resolves each row's allowance \code{amount}
#' using the package's standard group-level policy resolver
#' (\code{\link{dispatch_param}}). Because \code{policy_params$group_cols}
#' can itself include \code{"allowance_type"}, the same join-based resolver
#' every other module uses already supports per-type, per-group amounts --
#' no new dispatch logic is needed here.
#'
#' A separate \code{total_allowance_multiplier} (default \code{1.0}) is then
#' resolved at the *per-contract* level (\code{policy_params$multiplier_group_cols},
#' which must not include \code{"allowance_type"}) and applied to every
#' type's amount for that contract. This is what lets a policy change
#' "someone's total allowance" without touching the per-type scale.
#'
#' @param contract_dt data.table. Current-period contract snapshot. Rows
#'   with \code{contract_type_col} in \code{c("pensioner", "inactive")} are
#'   excluded before resolving amounts -- matching \code{.active_wage_bill()}'s
#'   own definition of "active" exactly. This is not optional/hardcoded out of
#'   convenience: \code{simulate_retirement()} does \strong{not} remove a
#'   retiree's row from \code{contract_dt} -- it relabels
#'   \code{contract_type} to \code{"pensioner"} in place, so a retiree's row
#'   is still present here and must be filtered by label, not by absence.
#' @param policy_params List. Canonical policy object:
#'   \describe{
#'     \item{group_cols}{Character vector or \code{NULL}. Columns (which may
#'       include \code{"allowance_type"}) used to look up \code{amount} in
#'       \code{policy_table}. \code{NULL} for a single scalar default
#'       amount.}
#'     \item{policy_table}{data.table with \code{group_cols} plus an
#'       \code{amount} column (e.g. the output of
#'       \code{\link{estimate_baseline_allowance_scale}}), or \code{NULL}.}
#'     \item{defaults}{List with at least \code{amount} (fallback for
#'       unmatched groups).}
#'     \item{multiplier_group_cols}{Character vector or \code{NULL}. Must not
#'       include \code{"allowance_type"}. \code{NULL} for a scalar
#'       multiplier.}
#'     \item{multiplier_policy_table}{data.table with
#'       \code{multiplier_group_cols} plus a \code{total_allowance_multiplier}
#'       column, or \code{NULL}.}
#'     \item{multiplier_default}{Numeric. Default \code{1.0}.}
#'   }
#' @param allowance_types Character vector. The allowance types to resolve
#'   an amount for.
#' @param contract_type_col Character. Default \code{"contract_type"}.
#' @param personnel_id_col Character. Default \code{"personnel_id"}.
#' @param contract_id_col Character. Default \code{"contract_id"}.
#'
#' @return data.table: one row per active contract x \code{allowance_type},
#'   with \code{contract_id}, \code{personnel_id}, any \code{group_cols}/
#'   \code{multiplier_group_cols} used for dispatch, \code{allowance_type},
#'   and the final resolved \code{amount} (per-type amount x multiplier).
#'
#' @keywords internal
resolve_allowance_amounts <- function(contract_dt,
                                      policy_params,
                                      allowance_types,
                                      contract_type_col = "contract_type",
                                      personnel_id_col  = "personnel_id",
                                      contract_id_col   = "contract_id") {

  validate_datatable(contract_dt, "contract_dt")
  if (is.null(allowance_types) || length(allowance_types) == 0L)
    stop("allowance_types must be a non-empty character vector.", call. = FALSE)
  if (!is.null(policy_params$multiplier_group_cols) &&
      "allowance_type" %in% policy_params$multiplier_group_cols)
    stop("resolve_allowance_amounts: multiplier_group_cols must not include ",
         "'allowance_type' -- the multiplier is resolved per contract, ",
         "not per type.", call. = FALSE)

  active_dt <- contract_dt[!get(contract_type_col) %in% c("pensioner", "inactive")]

  base_cols <- unique(c(contract_id_col, personnel_id_col,
                        policy_params$group_cols, policy_params$multiplier_group_cols))
  base_cols <- intersect(base_cols, names(active_dt))
  base_dt   <- unique(active_dt[, base_cols, with = FALSE])

  working_dt <- base_dt[rep(seq_len(.N), length(allowance_types))]
  working_dt[, allowance_type := rep(allowance_types, each = nrow(base_dt))]

  amount_spec <- list(
    default      = policy_params$defaults$amount %||% 0,
    group_cols   = policy_params$group_cols,
    policy_table = policy_params$policy_table
  )
  working_dt[, amount := dispatch_param(amount_spec, working_dt, "amount")]

  multiplier_spec <- list(
    default      = policy_params$multiplier_default %||% 1.0,
    group_cols   = policy_params$multiplier_group_cols,
    policy_table = policy_params$multiplier_policy_table
  )
  working_dt[, total_allowance_multiplier := dispatch_param(
    multiplier_spec, working_dt, "total_allowance_multiplier"
  )]

  working_dt[, amount := amount * total_allowance_multiplier]
  working_dt[, total_allowance_multiplier := NULL]

  working_dt
}
