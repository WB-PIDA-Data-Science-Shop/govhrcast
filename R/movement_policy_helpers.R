#' Movement Policy Constructor Helpers
#'
#' @description
#' Constructors for building the \code{movement_policy} argument to
#' \code{\link{simulate_horizon}} (and \code{policy_params} for
#' \code{\link{simulate_promotions_transfers}}) without hand-writing the
#' three-slot list.  Each constructor checks its inputs as soon as it is
#' called, so mistakes surface here rather than deep inside a simulation.
#'
#' Two policies, plus one table-building helper:
#' \itemize{
#'   \item \code{\link{design_movement_policy_status_quo}} -- reproduce the
#'     promotion/transfer rates observed in your historical panel, estimated
#'     automatically by \code{simulate_horizon()}.
#'   \item \code{\link{movement_policy_template}} -- estimate those same
#'     historical rates up front and hand them to you as an editable table.
#'   \item \code{\link{design_movement_policy_adjustment}} -- turn that
#'     (edited) table into a policy, e.g. to model doubling promotions into
#'     senior grades.
#' }
#' The usual reform workflow is template -> edit \code{movement_rate} ->
#' adjustment.  Because you edit the rates directly, any scaling you want
#' (across the board, or only for some grades or establishments) is just a
#' line of \code{data.table} code on the template.
#'
#' Every constructor returns a \strong{plain list} -- the same shape a
#' hand-written \code{movement_policy} already has.  Anything not reachable
#' through these constructors (e.g. a flat \code{defaults$movement_rate}
#' fallback, or \code{promotion_order_col}) remains reachable by editing the
#' returned list directly.  \code{defaults$active_types} is deliberately not
#' offered: the movement module does not apply it yet.
#'
#' @import data.table
#' @name movement_policy_helpers
#' @keywords internal
NULL


# Shared checks for the arguments every movement constructor takes.
#' @keywords internal
.validate_movement_common <- function(fn, group_cols, movement_type,
                                      movement_strategy, salary_update_rule) {
  if (is.null(group_cols) || !is.character(group_cols) || length(group_cols) == 0L) {
    stop(fn, ": group_cols must be a non-empty character vector (e.g. \"paygrade\").",
         call. = FALSE)
  }
  validate_choice(movement_type, c("promotion", "transfer"), "movement_type")
  validate_choice(movement_strategy,
                  c("tenure", "wage_based", "reverse_tenure", "random"),
                  "movement_strategy")
  validate_choice(salary_update_rule, c("scale", "keep"), "salary_update_rule")
  invisible(TRUE)
}


#' @keywords internal
.movement_split_cols <- function(group_cols) {
  list(from = paste0("from_", group_cols), to = paste0("to_", group_cols))
}


#' Editable Table of Historical Movement Rates
#'
#' @description
#' Estimates, from a multi-snapshot panel, the share of each group that moved
#' to each other group between consecutive snapshots (averaged over all
#' snapshot pairs), and returns it as a table you can edit and then pass to
#' \code{\link{design_movement_policy_adjustment}}.  The \code{movement_rate}
#' column starts out equal to the historical (status-quo) rate, so an
#' unedited template reproduces the status quo.
#'
#' Only moves that actually happened in the panel are listed.  To model a
#' move that never occurred historically, add a row (e.g. with
#' \code{data.table::rbindlist()}).  Staying in the same group is never a row:
#' whatever share of a group does not move simply stays.
#'
#' @param contract_dt data.table.  Contract data with at least two
#'   \code{ref_date_col} snapshots (a panel).  Only contracts whose type is
#'   not \code{"inactive"} are counted.
#' @param group_cols Character vector.  The columns that define a "group" a
#'   person can move between -- this decides what kind of movement you are
#'   modelling: \code{"paygrade"} for promotions, \code{"est_id"} for
#'   transfers, \code{c("est_id", "paygrade")} for both at once.  Must be the
#'   same \code{group_cols} you later pass to
#'   \code{\link{design_movement_policy_adjustment}}.
#' @param personnel_id_col,ref_date_col,start_date_col,end_date_col,contract_type_col
#'   Character.  Column names in \code{contract_dt}, if they differ from the
#'   package defaults.
#'
#' @return A data.table with one row per observed move and columns:
#'   \describe{
#'     \item{\code{from_<col>}, \code{to_<col>}}{One pair per entry of
#'       \code{group_cols} (e.g. \code{from_paygrade}, \code{to_paygrade}),
#'       as character.  For reading and filtering -- see Details.}
#'     \item{\code{from_group}, \code{to_group}}{The group keys the
#'       simulation actually uses: the \code{group_cols} values joined with
#'       \code{"||"} (e.g. \code{"SECRETARIA||Grade A"}).}
#'     \item{\code{n_periods}}{Number of snapshot pairs in which this move
#'       was observed -- a rough guide to how reliable the rate is.}
#'     \item{\code{status_quo_rate}}{The historical rate. Leave it
#'       unchanged, as a reference point for comparing scenarios.}
#'     \item{\code{movement_rate}}{The rate the simulation will use -- the
#'       column to edit. A share between 0 and 1 of the origin group moving
#'       per period.}
#'   }
#'   A zero-row table (with a message) when no moves were observed.
#'
#' @details
#' The \code{from_*}/\code{to_*} columns let you target edits without
#' handling the \code{"||"} keys, e.g.
#' \code{tpl[to_paygrade == "C", movement_rate := pmin(movement_rate * 2, 1)]}.
#' They hold the values as text, so compare against quoted values.  If you
#' change one of these columns, change \code{from_group}/\code{to_group} to
#' match -- \code{\link{design_movement_policy_adjustment}} checks that the
#' two agree and stops if they don't.
#'
#' @seealso \code{\link{design_movement_policy_adjustment}},
#'   \code{\link{design_movement_policy_status_quo}}
#' @examples
#' tpl <- movement_policy_template(bra_hrmis_contract, group_cols = "paygrade")
#' head(tpl)
#' @export
movement_policy_template <- function(contract_dt,
                                     group_cols,
                                     personnel_id_col  = "personnel_id",
                                     ref_date_col      = "ref_date",
                                     start_date_col    = "start_date",
                                     end_date_col      = "end_date",
                                     contract_type_col = "contract_type") {
  if (is.null(group_cols) || !is.character(group_cols) || length(group_cols) == 0L) {
    stop("movement_policy_template: group_cols must be a non-empty character vector.",
         call. = FALSE)
  }
  if (!data.table::is.data.table(contract_dt)) {
    contract_dt <- data.table::as.data.table(contract_dt)
  }

  .split <- .movement_split_cols(group_cols)
  .reserved <- c("from_group", "to_group", "status_quo_rate", "movement_rate", "n_periods")
  .clash <- intersect(c(.split$from, .split$to), .reserved)
  if (length(.clash) > 0L) {
    stop("movement_policy_template: group_cols produce column name(s) that clash with ",
         "the template's own columns: ", paste(.clash, collapse = ", "), ".",
         call. = FALSE)
  }

  # Pass a column subset (a new table), not contract_dt itself:
  # roll_snapshot_pairs() re-keys its input by reference, which would silently
  # re-sort the caller's panel.
  .needed  <- c(ref_date_col, personnel_id_col, group_cols,
                start_date_col, end_date_col, contract_type_col)
  .missing <- setdiff(.needed, names(contract_dt))
  if (length(.missing) > 0L) {
    stop("movement_policy_template: contract_dt is missing column(s): ",
         paste(.missing, collapse = ", "), ".", call. = FALSE)
  }

  est <- estimate_historical_movement_rates(
    contract_dt       = contract_dt[, .needed, with = FALSE],
    group_cols        = group_cols,
    personnel_id_col  = personnel_id_col,
    ref_date_col      = ref_date_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    contract_type_col = contract_type_col
  )

  out <- data.table::data.table(
    from_group      = as.character(est$from_group),
    to_group        = as.character(est$to_group),
    n_periods       = as.integer(est$n_periods),
    status_quo_rate = as.numeric(est$movement_rate),
    movement_rate   = as.numeric(est$movement_rate)
  )

  if (nrow(out) == 0L) {
    message("movement_policy_template: no transitions observed in the panel for ",
            "group_cols = ", paste(group_cols, collapse = ", "),
            "; returning an empty template. Add rows to model new movements.")
    for (col in c(.split$from, .split$to)) out[, (col) := character(0)]
  } else {
    out[, (.split$from) := data.table::tstrsplit(from_group, split = "||", fixed = TRUE)]
    out[, (.split$to)   := data.table::tstrsplit(to_group,   split = "||", fixed = TRUE)]
  }

  data.table::setcolorder(out, c(.split$from, .split$to, "from_group", "to_group",
                                 "n_periods", "status_quo_rate", "movement_rate"))
  data.table::setkeyv(out, c("from_group", "to_group"))
  out[]
}


#' Status-Quo Movement Policy
#'
#' @description
#' Builds a movement policy that reproduces recently observed promotion or
#' transfer behaviour.  \code{policy_table} is left \code{NULL}, which tells
#' \code{\link{simulate_horizon}} to estimate the historical move rates
#' between groups from the full panel before the first period (see
#' \code{\link{movement_policy_template}} for what is estimated).  If the
#' panel shows no moves at all, nobody moves in the projection.
#'
#' \strong{Use this with \code{simulate_horizon()} and panel data.}  Passed to
#' \code{\link{simulate_promotions_transfers}} directly, this policy is
#' rejected by input validation (it has neither a \code{policy_table} nor a
#' flat \code{movement_rate}).  With single-snapshot data, no rates can be
#' estimated and nobody moves.  In either case, build the rates with
#' \code{\link{movement_policy_template}} and use
#' \code{\link{design_movement_policy_adjustment}} instead.
#'
#' @param group_cols Character vector.  Columns defining the groups people
#'   move between: \code{"paygrade"} for promotions, \code{"est_id"} for
#'   transfers, or both.  Must exist in the contract data and in
#'   \code{salary_scale_dt}; moves into a group missing from
#'   \code{salary_scale_dt} are dropped.
#' @param movement_type \code{"promotion"} or \code{"transfer"} (required).
#'   Only a label: it decides whether \code{simulate_horizon()} reports these
#'   moves under \code{n_promotions}/\code{promotion_effect} or
#'   \code{n_transfers}/\code{transfer_effect}.  It does not change who moves.
#' @param movement_strategy Who is picked first when a group has more
#'   candidates than moves: \code{"tenure"} (default; longest in their
#'   current group), \code{"wage_based"} (lowest pay relative to the group's
#'   top earner), \code{"reverse_tenure"} (shortest overall tenure), or
#'   \code{"random"}.  Inside \code{simulate_horizon()}, \code{"tenure"}
#'   ranks by time since contract start, since each period sees only one
#'   snapshot.
#' @param salary_update_rule \code{"scale"} (default) gives movers the
#'   salary of their new group from \code{salary_scale_dt}; \code{"keep"}
#'   leaves their salary unchanged (common for lateral transfers).
#'
#' @return A list with \code{group_cols}, \code{policy_table = NULL} and
#'   \code{defaults} -- directly usable as \code{simulate_horizon()}'s
#'   \code{movement_policy} argument.
#' @seealso \code{\link{design_movement_policy_adjustment}},
#'   \code{\link{movement_policy_template}}, \code{\link{simulate_horizon}}
#' @examples
#' design_movement_policy_status_quo(group_cols = "paygrade", movement_type = "promotion")
#' design_movement_policy_status_quo(
#'   group_cols = "est_id", movement_type = "transfer",
#'   movement_strategy = "random", salary_update_rule = "keep"
#' )
#' @export
design_movement_policy_status_quo <- function(group_cols,
                                              movement_type,
                                              movement_strategy  = "tenure",
                                              salary_update_rule = "scale") {
  .validate_movement_common("design_movement_policy_status_quo", group_cols,
                            movement_type, movement_strategy, salary_update_rule)
  list(
    group_cols   = group_cols,
    policy_table = NULL,
    defaults = list(
      movement_strategy  = movement_strategy,
      salary_update_rule = salary_update_rule,
      movement_type      = movement_type
    )
  )
}


#' Movement Policy from Your Own Rate Table
#'
#' @description
#' Builds a movement policy from a table of move rates that you control --
#' typically the output of \code{\link{movement_policy_template}} after
#' editing \code{movement_rate} to represent a reform (e.g. doubling
#' promotions into senior grades, or freezing transfers out of one
#' establishment).  Works with \code{\link{simulate_horizon}} and with
#' \code{\link{simulate_promotions_transfers}} on a single snapshot, since
#' nothing needs estimating.
#'
#' The table is checked immediately.  It stops with an error if:
#' \itemize{
#'   \item \code{from_group}, \code{to_group} or \code{movement_rate} is
#'     missing;
#'   \item the table is empty;
#'   \item a rate is missing (NA) or outside 0--1;
#'   \item a key is NA or doesn't have one part per \code{group_cols} entry;
#'   \item a row moves a group into itself;
#'   \item a move is listed twice;
#'   \item the \code{from_*}/\code{to_*} columns no longer match the keys.
#' }
#' It warns if a group's rates add up to more than 1.  The simulation would
#' then scale that group's rates down proportionally so they total 1.
#'
#' @param policy_table data.table with \code{from_group}, \code{to_group}
#'   and \code{movement_rate} columns -- see
#'   \code{\link{movement_policy_template}}.  Extra columns (e.g.
#'   \code{status_quo_rate}) are kept and ignored by the simulation.  A copy
#'   is stored, so later edits to your table don't change the policy.
#' @param group_cols Character vector.  Must be the \code{group_cols} the
#'   table was built with.
#' @param movement_type,movement_strategy,salary_update_rule See
#'   \code{\link{design_movement_policy_status_quo}}.
#'
#' @return A list with \code{group_cols}, \code{policy_table} and
#'   \code{defaults} -- directly usable as \code{simulate_horizon()}'s
#'   \code{movement_policy} argument.
#' @seealso \code{\link{movement_policy_template}},
#'   \code{\link{design_movement_policy_status_quo}}
#' @examples
#' tpl <- movement_policy_template(bra_hrmis_contract, group_cols = "paygrade")
#'
#' # Reform: 50% more promotions out of every grade
#' tpl[, movement_rate := pmin(movement_rate * 1.5, 1)]
#'
#' design_movement_policy_adjustment(tpl, group_cols = "paygrade",
#'                                   movement_type = "promotion")
#' @export
design_movement_policy_adjustment <- function(policy_table,
                                              group_cols,
                                              movement_type,
                                              movement_strategy  = "tenure",
                                              salary_update_rule = "scale") {
  fn <- "design_movement_policy_adjustment"
  .validate_movement_common(fn, group_cols, movement_type,
                            movement_strategy, salary_update_rule)

  if (!data.table::is.data.table(policy_table)) {
    stop(fn, ": policy_table must be a data.table (e.g. from movement_policy_template()).",
         call. = FALSE)
  }
  .missing <- setdiff(c("from_group", "to_group", "movement_rate"), names(policy_table))
  if (length(.missing) > 0L) {
    stop(fn, ": policy_table is missing column(s): ", paste(.missing, collapse = ", "), ".",
         call. = FALSE)
  }
  if (nrow(policy_table) == 0L) {
    stop(fn, ": policy_table has no rows. To switch movement off, pass ",
         "movement_policy = NULL instead.", call. = FALSE)
  }

  pt <- data.table::copy(policy_table)

  # --- keys --------------------------------------------------------------
  if (anyNA(pt$from_group) || anyNA(pt$to_group)) {
    stop(fn, ": from_group and to_group must not contain NA.", call. = FALSE)
  }
  pt[, from_group := as.character(from_group)]
  pt[, to_group   := as.character(to_group)]

  .n_parts <- function(x) lengths(strsplit(x, "||", fixed = TRUE))
  .bad_keys <- unique(c(pt$from_group[.n_parts(pt$from_group) != length(group_cols)],
                        pt$to_group[.n_parts(pt$to_group)     != length(group_cols)]))
  if (length(.bad_keys) > 0L) {
    stop(fn, ": ", length(.bad_keys), " group key(s) do not have one part per ",
         "group_cols entry (", paste(group_cols, collapse = ", "), "), e.g. '",
         .bad_keys[1L], "'. Keys join group values with '||'.", call. = FALSE)
  }

  .self <- pt[from_group == to_group]
  if (nrow(.self) > 0L) {
    stop(fn, ": ", nrow(.self), " row(s) have from_group == to_group (e.g. '",
         .self$from_group[1L], "'). Only actual moves belong in the table -- ",
         "staying put is whatever is left over.", call. = FALSE)
  }
  if (anyDuplicated(pt, by = c("from_group", "to_group"))) {
    .dup <- pt[duplicated(pt, by = c("from_group", "to_group"))][1L]
    stop(fn, ": duplicate from_group/to_group pair(s), e.g. '", .dup$from_group,
         "' -> '", .dup$to_group, "'. Each move must appear once.", call. = FALSE)
  }

  # --- split columns, if present, must agree with the keys ---------------
  .split <- .movement_split_cols(group_cols)
  for (side in c("from", "to")) {
    .cols <- .split[[side]]
    .present <- .cols %in% names(pt)
    if (!any(.present)) next
    if (!all(.present)) {
      stop(fn, ": policy_table has some but not all of ",
           paste(.cols, collapse = ", "), ". Keep all of them or none.", call. = FALSE)
    }
    .rebuilt <- do.call(paste, c(lapply(.cols, function(c) as.character(pt[[c]])), sep = "||"))
    .key     <- pt[[paste0(side, "_group")]]
    .n_off   <- sum(.rebuilt != .key)
    if (.n_off > 0L) {
      stop(fn, ": ", .n_off, " row(s) where ", paste(.cols, collapse = "/"),
           " disagree with ", side, "_group. If you changed a group value, update ",
           side, "_group to match (or drop the ", side, "_* columns).", call. = FALSE)
    }
  }

  # --- rates -------------------------------------------------------------
  if (!is.numeric(pt$movement_rate) || anyNA(pt$movement_rate)) {
    stop(fn, ": movement_rate must be numeric with no NA values.", call. = FALSE)
  }
  if (any(pt$movement_rate < 0 | pt$movement_rate > 1)) {
    stop(fn, ": movement_rate must be between 0 and 1 (a share of the origin ",
         "group moving per period).", call. = FALSE)
  }
  .outflow <- pt[, .(total_outflow = sum(movement_rate)), by = from_group][total_outflow > 1]
  if (nrow(.outflow) > 0L) {
    warning(fn, ": ", nrow(.outflow), " from_group(s) have total outflow above 1 ",
            "(e.g. '", .outflow$from_group[1L], "' = ", round(.outflow$total_outflow[1L], 3),
            "). The simulation will scale those rates down proportionally so the ",
            "total is 1.", call. = FALSE)
  }

  list(
    group_cols   = group_cols,
    policy_table = pt,
    defaults = list(
      movement_strategy  = movement_strategy,
      salary_update_rule = salary_update_rule,
      movement_type      = movement_type
    )
  )
}
