#' Simulate Non-Retirement Exit Module
#'
#' @description
#' Main user-facing function for simulating non-retirement attrition events
#' (voluntary resignation, dismissal, contract non-renewal).  Orchestrates the
#' exit workflow: rate application, state updates, and summary statistics.
#' Mirrors the structure of \code{simulate_retirement()} and
#' \code{simulate_hiring()}.
#'
#' @section Exit rate modelling — status quo and future upgrade path:
#' The current \code{"status_quo"} mode applies historically estimated
#' group-level exit rates, held constant across all projection periods.  This
#' is equivalent to assuming that the \emph{composition} of exits — by grade,
#' contract type, and tenure — is stationary.  For short-horizon projections
#' (1–5 years) with stable workforce compositions this is a defensible
#' assumption.
#'
#' A planned upgrade will replace group-level rates with a \strong{survival
#' model} (e.g. Weibull or Cox proportional hazard) estimated from the
#' individual-level panel data.  The survival model approach:
#' \itemize{
#'   \item conditions exit probability on individual characteristics
#'     (tenure, age, grade, salary) that change over the simulation horizon;
#'   \item naturally handles compositional change as the workforce ages and
#'     grade structures evolve;
#'   \item produces a per-person, per-period exit probability vector that
#'     replaces the current group rate lookup.
#' }
#' The simulation architecture is already compatible with this upgrade: the
#' survival model would be estimated once (outside the period loop) and its
#' \code{predict()} output passed as a pre-computed probability column on
#' \code{contract_dt}, replacing the rate-lookup in
#' \code{compute_status_quo_exits()}.  No changes to the orchestration layer
#' (\code{simulate_scenario()}) would be required.
#'
#' @import data.table
#'
#' @param contract_dt data.table.  Contract data in govhr harmonised format.
#' @param personnel_dt data.table.  Personnel data in govhr harmonised format.
#' @param policy_params List.  Exit policy specification in the canonical
#'   three-slot format:
#'   \describe{
#'     \item{\code{group_cols}}{Character vector or \code{NULL}.  Columns in
#'       \code{contract_dt} used as the join key for group-level rate lookup
#'       (e.g. \code{"est_id"}, \code{"paygrade"}).  \code{NULL} for
#'       scalar-only dispatch.}
#'     \item{\code{policy_table}}{data.table with \code{group_cols} plus an
#'       \code{exit_rate} column, and optionally an \code{exit_multiplier}
#'       column for per-group reform scenarios.  Pass the output of
#'       \code{\link{estimate_historical_exit_rates}} here for
#'       \code{mode = "status_quo"}.  \code{NULL} when
#'       \code{mode = "fixed_rate"}.}
#'     \item{\code{defaults}}{Named list of scalar fallback values:
#'       \describe{
#'         \item{\code{exit_rate}}{Numeric scalar.  Required when
#'           \code{policy_table = NULL} (flat rate applied to all active
#'           workers).  Also used as the fallback rate for groups absent from
#'           \code{policy_table}.}
#'         \item{\code{exit_strategy}}{Character.  \code{"random"} (default)
#'           or the name of a numeric column in \code{contract_dt} to rank
#'           by (ascending — lowest values exit first).}
#'         \item{\code{active_types}}{Character vector.  Contract type values
#'           treated as active and eligible for exit.  Default
#'           \code{"active"}.}
#'         \item{\code{exited_type}}{Character.  Value written to
#'           \code{contract_type_col} after exit.  Default
#'           \code{"inactive"}.}
#'       }
#'     }
#'   }
#' @param ref_date Date.  Reference date for this simulation period.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_id_col Character.  Default \code{"contract_id"}.
#' @param contract_type_col Character.  Default \code{"contract_type_code"}.
#' @param status_col Character.  Default \code{"status"}.
#' @param salary_col Character.  Default \code{"gross_salary_lcu"}.
#' @param end_date_col Character.  Default \code{"end_date"}.
#'
#' @return Named list:
#'   \describe{
#'     \item{summary}{One-row data.table with \code{n_exits},
#'       \code{exit_savings}.}
#'     \item{contract_dt}{Updated contract data.}
#'     \item{personnel_dt}{Updated personnel data.}
#'     \item{exits_dt}{data.table of exited personnel with salary at exit.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' # Fixed rate — 5% scalar attrition
#' exit_policy <- list(
#'   group_cols   = NULL,
#'   policy_table = NULL,
#'   defaults = list(
#'     exit_rate     = 0.05,
#'     exit_strategy = "random",
#'     active_types  = c("perm", "fterm", "temp"),
#'     exited_type   = "inactive"
#'   )
#' )
#'
#' # Status quo — historical rates from panel, with reform multiplier by group
#' rates_dt <- estimate_historical_exit_rates(
#'   panel_contract_dt  = panel_contract_dt,
#'   panel_personnel_dt = panel_personnel_dt,
#'   group_cols         = "paygrade"
#' )
#' rates_dt[, exit_multiplier := ifelse(paygrade %in% c("A","B"), 0.8, 1.0)]
#'
#' exit_policy_reform <- list(
#'   group_cols   = "paygrade",
#'   policy_table = rates_dt,
#'   defaults = list(
#'     exit_strategy = "random",
#'     active_types  = c("perm", "fterm", "temp"),
#'     exited_type   = "inactive"
#'   )
#' )
#'
#' result <- simulate_exits(
#'   contract_dt   = contract_dt,
#'   personnel_dt  = personnel_dt,
#'   policy_params = exit_policy_reform,
#'   ref_date      = as.Date("2025-01-01")
#' )
#' result$summary
#' }
#'
#' @export
simulate_exits <- function(contract_dt,
                            personnel_dt,
                            policy_params,
                            ref_date,
                            personnel_id_col  = "personnel_id",
                            contract_id_col   = "contract_id",
                            contract_type_col = "contract_type_code",
                            status_col        = "status",
                            salary_col        = "gross_salary_lcu",
                            end_date_col      = "end_date") {

  # ------------------------------------------------------------------
  # 0. Validate & copy
  # ------------------------------------------------------------------
  ref_date <- validate_date_format(ref_date, "ref_date")

  if (!data.table::is.data.table(contract_dt))
    contract_dt <- data.table::as.data.table(contract_dt)
  else
    contract_dt <- data.table::copy(contract_dt)

  if (!data.table::is.data.table(personnel_dt))
    personnel_dt <- data.table::as.data.table(personnel_dt)
  else
    personnel_dt <- data.table::copy(personnel_dt)

  exit_strategy <- policy_params$defaults$exit_strategy %||% "random"
  active_types  <- policy_params$defaults$active_types  %||% "active"
  exited_type   <- policy_params$defaults$exited_type   %||% "inactive"

  # ------------------------------------------------------------------
  # 1. Validate policy_params
  # ------------------------------------------------------------------
  has_policy_table <- !is.null(policy_params$policy_table)
  has_group_cols   <- !is.null(policy_params$group_cols) &&
                       length(policy_params$group_cols) > 0L
  has_exit_rate    <- !is.null(policy_params$defaults$exit_rate) &&
                       is.numeric(policy_params$defaults$exit_rate) &&
                       length(policy_params$defaults$exit_rate) == 1L

  if (has_group_cols && !has_policy_table)
    stop(
      "policy_params$group_cols is set but policy_table is NULL. ",
      "Did you forget to pass the output of estimate_historical_exit_rates() ",
      "as policy_table?",
      call. = FALSE
    )

  if (!has_policy_table && !has_exit_rate)
    stop(
      "policy_table is NULL and defaults$exit_rate is not set. ",
      "Supply either a policy_table (for group-level status quo rates) or ",
      "defaults$exit_rate (for a flat scalar rate).",
      call. = FALSE
    )

  # ------------------------------------------------------------------
  # 2. Identify exits
  # ------------------------------------------------------------------
  exits_dt <- if (!is.null(policy_params$policy_table)) {
    compute_status_quo_exits(
      contract_dt       = contract_dt,
      policy_params     = policy_params,
      personnel_id_col  = personnel_id_col,
      contract_type_col = contract_type_col
    )
  } else {
    compute_fixed_rate_exits(
      contract_dt       = contract_dt,
      policy_params     = policy_params,
      personnel_id_col  = personnel_id_col,
      contract_type_col = contract_type_col
    )
  }

  # Attach salary at exit for savings computation
  if (!is.null(exits_dt) && nrow(exits_dt) > 0L) {
    # Sum ALL active contract salaries per exiting person.
    # A person with two simultaneous contracts costs both salaries;
    salary_at_exit <- contract_dt[
      get(personnel_id_col) %in% exits_dt[[personnel_id_col]] &
        get(contract_type_col) %in% active_types,
      .(total_sal = sum(get(salary_col), na.rm = TRUE)),
      by = c(personnel_id_col)
    ]
    exits_dt <- salary_at_exit[exits_dt, on = personnel_id_col]
    data.table::setnames(exits_dt, "total_sal", salary_col)
  }

  n_exits      <- if (!is.null(exits_dt)) nrow(exits_dt) else 0L
  exit_savings <- compute_non_retirement_exit_effect(exits_dt, salary_col)

  # ------------------------------------------------------------------
  # 3. Update state
  # ------------------------------------------------------------------
  if (!is.null(exits_dt) && nrow(exits_dt) > 0L) {

    ### remember contract_dt and personnel_dt are updated in place
    ### this means that we do not need to make any assignments

    update_contracts_for_exits(
      contract_dt       = contract_dt,
      exits_dt          = exits_dt,
      ref_date          = ref_date,
      personnel_id_col  = personnel_id_col,
      contract_type_col = contract_type_col,
      end_date_col      = end_date_col,
      active_types      = active_types,
      exited_type       = exited_type
    )
    update_personnel_for_exits(
      personnel_dt     = personnel_dt,
      exits_dt         = exits_dt,
      personnel_id_col = personnel_id_col,
      status_col       = status_col
    )
  }

  # ------------------------------------------------------------------
  # 4. Summary
  # ------------------------------------------------------------------
  summary_tbl <- data.table::data.table(
    n_exits      = as.integer(n_exits),
    exit_savings = exit_savings
  )

  list(
    summary      = summary_tbl,
    contract_dt  = contract_dt,
    personnel_dt = personnel_dt,
    exits_dt     = if (!is.null(exits_dt)) exits_dt else data.table::data.table()
  )
}
