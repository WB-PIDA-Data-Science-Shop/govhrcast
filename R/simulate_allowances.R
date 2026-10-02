#' Simulate Allowance Assignment for a Single Projection Period
#'
#' @description
#' Assigns allowances to active contracts for one simulation period from an
#' allowance policy, and appends the result to the accumulated
#' \code{allowance_dt} ledger. \code{policy_params = NULL} is a no-op:
#' \code{contract_dt}/\code{personnel_dt} are returned unchanged and
#' \code{allowance_dt} is returned as supplied (or an empty ledger on the
#' first call).
#'
#' Unlike retirement, exits, movements, and hiring, this module never
#' mutates \code{contract_dt}/\code{personnel_dt} -- allowances are tracked
#' entirely in the separate \code{allowance_dt} ledger, the same way
#' \code{pensioner_register} tracks pension costs outside the wage bill.
#'
#' @details
#' \strong{Avoid double-counting allowances already folded into a gross
#' salary figure.} In the standard dictionary from \code{govhr::dictionary} 
#' \code{bra_hrmis_contract}, \code{gross_salary_lcu} also contains 
#' \code{base_salary_lcu} plus whatever lump-sum allowances the source 
#' system had already applied (\code{gross_salary_lcu} is approximately 
#' \code{base_salary_lcu + allowance_lcu} for most rows in the bundled data). 
#' If you run this module and then add its \code{allowance_cost_total} 
#' on top of a wage bill that was computed from \code{gross_salary_lcu}, 
#' you will double-count: the old lump allowance is still sitting inside 
#' \code{gross_salary_lcu}, and this module's per-type amounts are added 
#' on top of it a second time. When allowance modeling is active, 
#' whatever salary figure you track alongside it (e.g. \code{salary_col} 
#' elsewhere in the pipeline) should represent \strong{base} compensation 
#' only -- \code{base_salary_lcu} in the bundled data, not \code{gross_salary_lcu}. 
#' This is a data-authoring responsibility, not something this function 
#' can safely detect or warn about on its own (a correlation-based 
#' heuristic would be fragile and would flag legitimate cases too) 
#' -- see the worked example below.
#'
#' @param contract_dt data.table. Current-period contract snapshot.
#' @param personnel_dt data.table. Current-period personnel snapshot.
#' @param allowance_dt data.table or \code{NULL}. Accumulated ledger from
#'   prior periods. \code{NULL} for the first period.
#' @param policy_params List or \code{NULL}. See
#'   \code{\link{resolve_allowance_amounts}} for the expected shape.
#'   \code{NULL} disables allowance modeling for this call.
#' @param ref_date Date. The reference date for this period -- matches
#'   \code{simulate_retirement()}/\code{simulate_exits()}/
#'   \code{simulate_promotions_transfers()}/\code{simulate_hiring()}, which
#'   all receive \code{simulate_scenario()}'s \code{period_date} under this
#'   same name. Stamped onto the ledger and summary as \code{period_date},
#'   matching \code{pensioner_register}'s column.
#' @param allowance_types Character vector or \code{NULL}. The allowance
#'   types to resolve. When \code{NULL} (the default), derived from
#'   \code{unique(policy_params$policy_table$allowance_type)} -- so it only
#'   needs to be supplied explicitly for a purely scalar policy with no
#'   \code{policy_table}.
#' @param contract_type_col Character. Default \code{"contract_type"}. Rows
#'   with this in \code{c("pensioner", "inactive")} are excluded -- see
#'   \code{\link{resolve_allowance_amounts}} for why this can't be inferred
#'   from row absence.
#' @param personnel_id_col Character. Default \code{"personnel_id"}.
#' @param contract_id_col Character. Default \code{"contract_id"}.
#'
#' @return Named list:
#'   \describe{
#'     \item{contract_dt}{Unchanged from input.}
#'     \item{personnel_dt}{Unchanged from input.}
#'     \item{allowance_dt}{Updated ledger including this period's rows.}
#'     \item{summary}{One-row data.table: \code{period_date},
#'       \code{n_contracts}, \code{allowance_cost_total}.}
#'   }
#'
#' @examples
#' \dontrun{
#'
#' library(data.table)
#' library(govhrcast)
#'
#' ## NOTE: deliberately not called `ref_date` -- bra_hrmis_contract/
#' ## bra_hrmis_allowance both have a column literally named "ref_date", and
#' ## a same-named local variable would silently shadow it inside `[...]`
#' ## filters below (e.g. `dt[ref_date == ref_date]` is a no-op that returns
#' ## every row, not just the intended snapshot).
#' baseline_date <- as.Date("2017-09-01")
#'
#' ## Step 1: build a baseline allowance scale from real HRMIS data. The
#' ## allowance panel and contract panel are both full multi-period panels --
#' ## estimate_baseline_allowance_scale() resolves ref_date to the nearest
#' ## available snapshot internally, so they don't need to be pre-filtered.
#' ## group_cols here is the CONTRACT-level grouping dimension only --
#' ## allowance_type is always added automatically, since it comes from
#' ## panel_allowance_dt, not panel_contract_dt.
#' allowance_scale_dt <- estimate_baseline_allowance_scale(
#'   panel_allowance_dt = bra_hrmis_allowance,
#'   panel_contract_dt  = bra_hrmis_contract,
#'   group_cols          = "est_id",
#'   ref_date            = baseline_date
#' )
#'
#' ## Step 2: a single-snapshot working population to assign allowances against.
#' contract_dt  <- bra_hrmis_contract[ref_date == baseline_date]
#' personnel_dt <- bra_hrmis_personnel[ref_date == baseline_date]
#'
#' ## IMPORTANT: use base_salary_lcu (not gross_salary_lcu) as the salary
#' ## figure tracked alongside allowance_cost_total elsewhere in the
#' ## pipeline -- gross_salary_lcu already includes the old lump allowance,
#' ## so adding this module's cost on top of it would double-count. See
#' ## Details above.
#'
#' policy_params <- list(
#'   group_cols   = c("est_id", "allowance_type"),
#'   policy_table = allowance_scale_dt,
#'   defaults     = list(amount = 0)
#' )
#'
#' result <- simulate_allowances(
#'   contract_dt   = contract_dt,
#'   personnel_dt  = personnel_dt,
#'   policy_params = policy_params,
#'   ref_date      = baseline_date
#' )
#'
#' result$summary          # n_contracts, allowance_cost_total for this period
#' head(result$allowance_dt)
#'
#' ## Step 3 (next period): pass the ledger forward to accumulate history --
#' ## contract_dt/personnel_dt would normally be this period's updated state
#' ## from the rest of the simulate_scenario() pipeline.
#' result2 <- simulate_allowances(
#'   contract_dt   = contract_dt,
#'   personnel_dt  = personnel_dt,
#'   allowance_dt  = result$allowance_dt,
#'   policy_params = policy_params,
#'   ref_date      = as.Date("2019-09-01")
#' )
#' }
#'
#' @export
simulate_allowances <- function(contract_dt,
                                personnel_dt,
                                allowance_dt = NULL,
                                policy_params,
                                ref_date,
                                allowance_types    = NULL,
                                contract_type_col  = "contract_type",
                                personnel_id_col   = "personnel_id",
                                contract_id_col    = "contract_id") {

  ref_date <- validate_date_format(ref_date, "ref_date")

  if (is.null(policy_params)) {
    empty_ledger <- allowance_dt %||% data.table::data.table(
      contract_id    = character(0),
      personnel_id   = character(0),
      allowance_type = character(0),
      amount         = numeric(0),
      period_date    = as.Date(character(0))
    )
    return(list(
      contract_dt  = contract_dt,
      personnel_dt = personnel_dt,
      allowance_dt = empty_ledger,
      summary      = data.table::data.table(
        period_date = ref_date, n_contracts = 0L, allowance_cost_total = 0
      )
    ))
  }

  check_allowance_inputs(
    contract_dt       = contract_dt,
    personnel_dt      = personnel_dt,
    policy_params     = policy_params,
    ref_date          = ref_date,
    contract_type_col = contract_type_col,
    personnel_id_col  = personnel_id_col,
    contract_id_col   = contract_id_col
  )

  if (is.null(allowance_types)) {
    if (is.null(policy_params$policy_table) ||
        !"allowance_type" %in% names(policy_params$policy_table))
      stop("simulate_allowances: allowance_types was not supplied and could not ",
           "be derived -- policy_params$policy_table must contain an ",
           "'allowance_type' column, or pass allowance_types explicitly.",
           call. = FALSE)
    allowance_types <- unique(policy_params$policy_table$allowance_type)
  }

  resolved <- resolve_allowance_amounts(
    contract_dt       = contract_dt,
    policy_params     = policy_params,
    allowance_types   = allowance_types,
    contract_type_col = contract_type_col,
    personnel_id_col  = personnel_id_col,
    contract_id_col   = contract_id_col
  )

  allowance_dt <- update_allowance_register(
    allowance_dt        = allowance_dt,
    resolved_amounts_dt = resolved,
    ref_date            = ref_date
  )

  summary_dt <- data.table::data.table(
    period_date          = ref_date,
    n_contracts          = data.table::uniqueN(resolved[[contract_id_col]]),
    allowance_cost_total = sum(resolved$amount, na.rm = TRUE)
  )

  list(
    contract_dt  = contract_dt,
    personnel_dt = personnel_dt,
    allowance_dt = allowance_dt,
    summary      = summary_dt
  )
}
