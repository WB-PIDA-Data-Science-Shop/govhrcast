#' Simulate Non-Retirement Exit Module
#'
#' @description
#' Main user-facing function for simulating non-retirement attrition events
#' (voluntary resignation, dismissal, contract non-renewal).  Orchestrates the
#' exit workflow: rate application, state updates, and summary statistics.
#' Mirrors the structure of \code{simulate_retirement()} and
#' \code{simulate_hiring()}.
#'
#' @import data.table
#'
#' @param contract_dt data.table.  Contract data in govhr harmonised format.
#' @param personnel_dt data.table.  Personnel data in govhr harmonised format.
#' @param policy_params List.  Exit policy parameters:
#'   \describe{
#'     \item{mode}{Character.  \code{"status_quo"} (default) — apply historical
#'       rates; \code{"fixed_rate"} — apply a user-supplied scalar rate.}
#'     \item{group_cols}{Character vector or \code{NULL}.  Grouping columns
#'       (e.g. \code{"est_id"}).}
#'     \item{exit_rates_dt}{data.table or \code{NULL}.  Pre-computed rates
#'       from \code{estimate_historical_exit_rates()}.  If \code{NULL} and
#'       \code{mode = "status_quo"}, an error is raised — rates must be
#'       estimated in the caller's prologue and supplied here.}
#'     \item{exit_strategy}{Character.  \code{"random"} (default) or a numeric
#'       column name in \code{contract_dt} (ascending — lowest exits first).}
#'     \item{exit_multiplier}{Numeric.  Scale the historical rate.  Default
#'       \code{1.0}.}
#'     \item{fixed_rate}{Numeric.  Required when \code{mode = "fixed_rate"};
#'       applied as a flat attrition rate regardless of history.}
#'     \item{active_types}{Character vector.  Contract type values treated as
#'       active and eligible for exit.  Default \code{"active"}.}
#'     \item{exited_type}{Character.  Value written to \code{contract_type_col}
#'       after exit.  Default \code{"inactive"}.}
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
#' exit_policy <- list(
#'   mode          = "fixed_rate",
#'   fixed_rate    = 0.05,
#'   exit_strategy = "random",
#'   active_types  = "permanent",
#'   exited_type   = "inactive"
#' )
#'
#' result <- simulate_exits(
#'   contract_dt  = contract_dt,
#'   personnel_dt = personnel_dt,
#'   policy_params = exit_policy,
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

  mode          <- policy_params$mode          %||% "status_quo"
  group_cols    <- policy_params$group_cols
  exit_strategy <- policy_params$exit_strategy %||% "random"
  multiplier    <- policy_params$exit_multiplier %||% 1.0
  active_types  <- policy_params$active_types  %||% "active"
  exited_type   <- policy_params$exited_type   %||% "inactive"

  # ------------------------------------------------------------------
  # 1. Build rates table (or use supplied one)
  # ------------------------------------------------------------------
  exit_rates_dt <- policy_params$exit_rates_dt

  if (mode == "fixed_rate") {
    fixed_rate <- policy_params$fixed_rate
    if (is.null(fixed_rate) || !is.numeric(fixed_rate) || length(fixed_rate) != 1L)
      stop("exit_policy$fixed_rate must be a numeric scalar when mode = 'fixed_rate'.",
           call. = FALSE)
    exit_rates_dt <- data.table::data.table(exit_rate = fixed_rate)

  } else if (mode == "status_quo") {
    if (is.null(exit_rates_dt)) {
      stop(
        "exit_policy$exit_rates_dt must be supplied when mode = 'status_quo'. ",
        "Call estimate_historical_exit_rates() in your simulation prologue and ",
        "store the result in exit_policy$exit_rates_dt.",
        call. = FALSE
      )
    }
  } else {
    stop("exit_policy$mode must be 'status_quo' or 'fixed_rate', got: '", mode, "'.",
         call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 2. Identify exits
  # ------------------------------------------------------------------
  exits_dt <- compute_status_quo_exits(
    contract_dt       = contract_dt,
    exit_rates_dt     = exit_rates_dt,
    group_cols        = group_cols,
    exit_strategy     = exit_strategy,
    exit_multiplier   = multiplier,
    personnel_id_col  = personnel_id_col,
    contract_type_col = contract_type_col,
    active_types      = active_types
  )

  # Attach salary at exit for savings computation
  if (!is.null(exits_dt) && nrow(exits_dt) > 0L) {
    # Sum ALL active contract salaries per exiting person.
    # A person with two simultaneous contracts costs both salaries;
    # max() would understate the saving when multiple contracts exist.
    salary_at_exit <- contract_dt[
      get(personnel_id_col) %in% exits_dt[[personnel_id_col]] &
        get(contract_type_col) %in% active_types,
      .(total_sal = sum(get(salary_col), na.rm = TRUE)),
      by = c(personnel_id_col)
    ]
    data.table::setnames(salary_at_exit, personnel_id_col, ".pid_exit_")
    data.table::setnames(salary_at_exit, ".pid_exit_", personnel_id_col)
    exits_dt <- salary_at_exit[exits_dt, on = personnel_id_col]
    data.table::setnames(exits_dt, "total_sal", salary_col)
  }

  n_exits      <- if (!is.null(exits_dt)) nrow(exits_dt) else 0L
  exit_savings <- compute_non_retirement_exit_effect(exits_dt, salary_col)

  # ------------------------------------------------------------------
  # 3. Update state
  # ------------------------------------------------------------------
  if (!is.null(exits_dt) && nrow(exits_dt) > 0L) {
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
