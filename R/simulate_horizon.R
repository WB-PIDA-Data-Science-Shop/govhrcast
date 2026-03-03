# ===========================================================================
# Effect-computation helpers
# ===========================================================================

#' Compute Exit Effect
#'
#' @description
#' Returns the total salary mass removed by retirements (or any other exits)
#' in one simulation period.  This is a pure function with no side-effects.
#'
#' @param retirees_dt data.table or \code{NULL}.  Output of
#'   \code{simulate_retirement()}.  Must contain the column named by
#'   \code{salary_col} if non-empty.
#' @param salary_col Character.  Name of the salary column.  Default:
#'   \code{"gross_salary_lcu"}.
#'
#' @return Numeric scalar \eqn{\ge 0}: sum of \code{salary_col} across all
#'   rows in \code{retirees_dt}, or \code{0} if \code{retirees_dt} is
#'   \code{NULL} or has zero rows.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' retirees <- data.table(personnel_id = "P1", gross_salary_lcu = 50000)
#' compute_exit_effect(retirees)  # 50000
#' compute_exit_effect(NULL)      # 0
#' }
#'
#' @export
compute_exit_effect <- function(retirees_dt,
                                salary_col = "gross_salary_lcu") {
  if (is.null(retirees_dt) || nrow(retirees_dt) == 0L) return(0)
  if (!salary_col %in% names(retirees_dt)) return(0)
  sum(retirees_dt[[salary_col]], na.rm = TRUE)
}


#' Compute Movement Effect
#'
#' @description
#' Computes the net salary change attributable to promotions and transfers in
#' one simulation period.  Requires \code{movers_dt} to carry a
#' \code{salary_before} column (pre-move salary, summed across all contracts
#' per person) and \code{movement_type} (\code{"promotion"} or
#' \code{"transfer"}).  Post-move salaries are read from \code{contract_dt_after}.
#'
#' @param movers_dt data.table or \code{NULL}.  Output of
#'   \code{simulate_promotions_transfers()$movers_dt}.  Must contain columns
#'   \code{salary_before}, \code{movement_type}, and the column named by
#'   \code{personnel_id_col}.
#' @param contract_dt_after data.table.  Contract snapshot \emph{after} the
#'   movement step — used to look up post-move salaries.
#' @param personnel_id_col Character.  Personnel ID column.  Default:
#'   \code{"personnel_id"}.
#' @param salary_col Character.  Salary column.  Default:
#'   \code{"gross_salary_lcu"}.
#'
#' @return Named list with two numeric scalars:
#'   \describe{
#'     \item{promotion}{Net salary change from promotions
#'       (post-move minus pre-move, summed across all promotion movers).}
#'     \item{transfer}{Net salary change from transfers.}
#'   }
#'   Both are \code{0} when \code{movers_dt} is \code{NULL} or empty.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' movers <- data.table(
#'   personnel_id  = c("P1", "P2"),
#'   movement_type = c("promotion", "transfer"),
#'   salary_before = c(10000, 12000)
#' )
#' after <- data.table(
#'   personnel_id     = c("P1", "P2"),
#'   gross_salary_lcu = c(12000, 11000)
#' )
#' compute_movement_effect(movers, after)
#' # list(promotion = 2000, transfer = -1000)
#' }
#'
#' @export
compute_movement_effect <- function(movers_dt,
                                    contract_dt_after,
                                    personnel_id_col = "personnel_id",
                                    salary_col       = "gross_salary_lcu") {
  zero <- list(promotion = 0, transfer = 0)
  if (is.null(movers_dt) || nrow(movers_dt) == 0L) return(zero)
  if (!"salary_before" %in% names(movers_dt))       return(zero)
  if (!"movement_type" %in% names(movers_dt))        return(zero)
  if (!salary_col %in% names(contract_dt_after))     return(zero)

  # Sum post-move salary per person (across all their contracts after move)
  post_salary_dt <- contract_dt_after[
    get(personnel_id_col) %in% movers_dt[[personnel_id_col]],
    .(salary_after = sum(get(salary_col), na.rm = TRUE)),
    by = c(personnel_id_col)
  ]

  # Join pre/post
  mv <- post_salary_dt[movers_dt, on = personnel_id_col]
  mv[is.na(salary_after), salary_after := 0]
  mv[, salary_diff := salary_after - salary_before]

  promotion_effect <- mv[movement_type == "promotion",
                          sum(salary_diff, na.rm = TRUE)]
  transfer_effect  <- mv[movement_type == "transfer",
                          sum(salary_diff, na.rm = TRUE)]

  # Protect against empty subsets returning numeric(0)
  if (length(promotion_effect) == 0L) promotion_effect <- 0
  if (length(transfer_effect)  == 0L) transfer_effect  <- 0

  list(promotion = promotion_effect, transfer = transfer_effect)
}


#' Compute Hiring Effect
#'
#' @description
#' Returns the total salary cost of new hire contracts created in one
#' simulation period.  This is a pure function with no side-effects.
#'
#' @param new_hire_contracts_dt data.table or \code{NULL}.  Contract-level
#'   new hire records returned as \code{new_hire_contracts_dt} by
#'   \code{simulate_hiring()}.  Must contain the column named by
#'   \code{salary_col} if non-empty.
#' @param salary_col Character.  Name of the salary column.  Default:
#'   \code{"gross_salary_lcu"}.
#'
#' @return Numeric scalar \eqn{\ge 0}: sum of \code{salary_col} across all
#'   rows in \code{new_hire_contracts_dt}, or \code{0} if \code{NULL} or empty.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' contracts <- data.table(contract_id = "C99", gross_salary_lcu = 40000)
#' compute_hiring_effect(contracts)  # 40000
#' compute_hiring_effect(NULL)       # 0
#' }
#'
#' @export
compute_hiring_effect <- function(new_hire_contracts_dt,
                                  salary_col = "gross_salary_lcu") {
  if (is.null(new_hire_contracts_dt) || nrow(new_hire_contracts_dt) == 0L) return(0)
  if (!salary_col %in% names(new_hire_contracts_dt)) return(0)
  sum(new_hire_contracts_dt[[salary_col]], na.rm = TRUE)
}


#' Compute Inflation Effect
#'
#' @description
#' Returns the additional payroll cost arising from applying a COLA
#' (cost-of-living adjustment) rate to the existing payroll.  This is a
#' pure function with no side-effects.
#'
#' @param pre_cola_wage_bill Numeric scalar.  Total payroll \emph{before} the
#'   COLA is applied.
#' @param growth_rate Numeric scalar.  COLA / salary growth rate
#'   (e.g. \code{0.03} for 3 percent).
#'
#' @return Numeric scalar: \code{pre_cola_wage_bill * growth_rate}.
#'   Zero when \code{growth_rate} is zero; negative when \code{growth_rate}
#'   is negative.
#'
#' @examples
#' compute_inflation_effect(1000000, 0.03)   # 30000
#' compute_inflation_effect(1000000, 0)      # 0
#' compute_inflation_effect(1000000, -0.02)  # -20000
#'
#' @export
compute_inflation_effect <- function(pre_cola_wage_bill, growth_rate) {
  pre_cola_wage_bill * growth_rate
}


# ===========================================================================
# simulate_scenario() — single-period orchestrator
# ===========================================================================

#' Single-Period Simulation
#'
#' @description
#' Runs all govhrcast simulation modules for \emph{one} period (year) and
#' returns a one-row wage-bill decomposition summary together with updated
#' state objects.  Operations are applied in this order:
#'
#' 1. **Snapshot A** — record \code{wage_bill_start} and \code{n_headcount_start}.
#' 2. **Retirement** — remove retirees; accumulate \code{pensioner_register}.
#' 3. **Movements** — apply promotions and transfers.
#' 4. **Hiring** — fill vacancies.
#' 5. **Aging** — increment \code{age} and \code{tenure_years}.
#' 6. **COLA** — scale salary table and contract salaries.
#' 7. **Snapshot B** — record \code{wage_bill_end} and \code{n_headcount_end}.
#'
#' @details
#' **Wage bill measurement**: \code{wage_bill_start} and \code{wage_bill_end}
#' are computed as \code{sum(salary_col)} over \emph{all} rows in
#' \code{contract_dt} with no filtering.  From period 2 onwards pensioner rows
#' are present in \code{contract_dt} (with
#' \code{contract_type_code = "pensioner"}) and their \code{salary_col} value
#' is included automatically.  The \code{pensioner_register} is therefore an
#' audit ledger tracking \code{pension_amount} (the pension formula output).
#'
#' @import data.table
#'
#' @param contract_dt data.table.  Single-snapshot contract microdata.
#' @param personnel_dt data.table.  Single-snapshot personnel microdata.
#' @param salary_scale_dt data.table.  Pay table used for movement and hiring
#'   salary assignment.  Updated in-place each period by \code{salary_growth_rate}.
#' @param period_date Date.  The reference date for this simulation period.
#'   Stored in \code{pensioner_register} for cohort auditing and used as the
#'   \code{period_date} column in the returned summary row.
#' @param pensioner_register data.table or \code{NULL}.  Accumulated pensioner
#'   ledger from prior periods.  Must contain columns \code{personnel_id},
#'   \code{pension_amount}, \code{period_date}.  Pass \code{NULL} (default)
#'   for the first period — an empty register is initialised internally.
#' @param retirement_policy List or \code{NULL}.  Passed to
#'   \code{simulate_retirement()}.  Pass \code{NULL} to skip retirement.
#' @param movement_policy List or \code{NULL}.  Passed to
#'   \code{simulate_promotions_transfers()}.  Pass \code{NULL} to skip.
#' @param hiring_policy List or \code{NULL}.  Passed to
#'   \code{simulate_hiring()}.  Pass \code{NULL} to skip.
#' @param salary_growth_rate Numeric scalar.  COLA rate for this period.
#'   Default \code{0}.
#' @param period_date Date.  Simulation date for this period — stored in
#'   \code{pensioner_register} for cohort auditing.  Default: \code{Sys.Date()}.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_id_col Character.  Default \code{"contract_id"}.
#' @param start_date_col Character.  Default \code{"start_date"}.
#' @param end_date_col Character.  Default \code{"end_date"}.
#' @param salary_col Character.  Default \code{"gross_salary_lcu"}.
#' @param contract_type_col Character.  Default \code{"contract_type_code"}.
#' @param status_col Character.  Default \code{"status"}.
#' @param age_col Character or \code{NULL}.  Age column to increment.
#'   Default \code{"age"}.
#' @param tenure_col Character or \code{NULL}.  Tenure column to increment.
#'   Default \code{"tenure_years"}.
#'
#' @return Named list:
#'   \describe{
#'     \item{summary}{One-row \code{data.table} with columns:
#'       \code{period_date}, \code{n_headcount_start}, \code{wage_bill_start},
#'       \code{n_exits}, \code{exit_savings}, \code{pension_cost_new},
#'       \code{pension_cost_total}, \code{n_promotions}, \code{n_transfers},
#'       \code{promotion_effect}, \code{transfer_effect}, \code{n_hires},
#'       \code{hiring_effect}, \code{inflation_effect},
#'       \code{n_headcount_end}, \code{wage_bill_end}.}
#'     \item{contract_dt}{Updated contract snapshot.}
#'     \item{personnel_dt}{Updated personnel snapshot.}
#'     \item{salary_scale_dt}{Updated pay table (post-COLA).}
#'     \item{pensioner_register}{Accumulated pensioner ledger including new
#'       retirees from this period.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' s <- list(
#'   contract_dt  = data.table(
#'     contract_id        = "C1",
#'     personnel_id       = "P1",
#'     est_id             = "E1",
#'     start_date         = as.Date("2010-01-01"),
#'     end_date           = as.Date(NA),
#'     contract_type_code = "permanent",
#'     gross_salary_lcu   = 50000
#'   ),
#'   personnel_dt = data.table(
#'     personnel_id = "P1",
#'     birth_date   = as.Date("1975-01-01"),
#'     status       = "active",
#'     age          = 45,
#'     tenure_years = 14
#'   )
#' )
#' result <- simulate_scenario(
#'   contract_dt     = s$contract_dt,
#'   personnel_dt    = s$personnel_dt,
#'   salary_scale_dt = data.table(est_id = "E1", gross_salary_lcu = 50000),
#'   year            = 2025L,
#'   period_date     = as.Date("2025-01-01"),
#'   salary_growth_rate = 0.03
#' )
#' result$summary
#' }
#'
#' @export
simulate_scenario <- function(contract_dt,
                               personnel_dt,
                               salary_scale_dt,
                               period_date,
                               pensioner_register  = NULL,
                               retirement_policy   = NULL,
                               movement_policy     = NULL,
                               hiring_policy       = NULL,
                               salary_growth_rate  = 0,
                               personnel_id_col    = "personnel_id",
                               contract_id_col     = "contract_id",
                               start_date_col      = "start_date",
                               end_date_col        = "end_date",
                               salary_col          = "gross_salary_lcu",
                               contract_type_col   = "contract_type_code",
                               status_col          = "status",
                               age_col             = "age",
                               tenure_col          = "tenure_years") {

  # ------------------------------------------------------------------
  # 0. Initialise / validate
  # ------------------------------------------------------------------
  period_date <- validate_date_format(period_date, "period_date")

  if (!data.table::is.data.table(contract_dt))
    contract_dt <- data.table::as.data.table(contract_dt)
  if (!data.table::is.data.table(personnel_dt))
    personnel_dt <- data.table::as.data.table(personnel_dt)

  if (!salary_col %in% names(contract_dt))
    stop("salary_col '", salary_col, "' not found in contract_dt.", call. = FALSE)

  # Initialise empty pensioner register on first call
  if (is.null(pensioner_register)) {
    pensioner_register <- data.table::data.table(
      personnel_id   = character(0),
      pension_amount = numeric(0),
      period_date    = as.Date(character(0))
    )
  }

  # Detect salary column in salary_scale_dt
  scale_salary_pat   <- "salary|wage|pay|compensation|allowance"
  scale_salary_cands <- grep(scale_salary_pat, names(salary_scale_dt),
                             value = TRUE, ignore.case = TRUE)
  if (length(scale_salary_cands) == 0L)
    stop("Could not detect a salary column in salary_scale_dt.", call. = FALSE)
  scale_salary_col <- if (salary_col %in% scale_salary_cands) salary_col
                      else scale_salary_cands[1L]

  # ------------------------------------------------------------------
  # SNAPSHOT A  (active employees only — pensioner rows excluded)
  # ------------------------------------------------------------------
  n_headcount_start <- nrow(contract_dt[get(contract_type_col) != "pensioner"])
  wage_bill_start   <- sum(
    contract_dt[get(contract_type_col) != "pensioner"][[salary_col]],
    na.rm = TRUE
  )

  # ------------------------------------------------------------------
  # STEP 1: RETIREMENT
  # ------------------------------------------------------------------
  n_exits          <- 0L
  exit_savings     <- 0
  pension_cost_new <- 0
  retirees_dt      <- NULL

  if (!is.null(retirement_policy)) {
    ret_result <- simulate_retirement(
      contract_dt       = contract_dt,
      personnel_dt      = personnel_dt,
      policy_params     = retirement_policy,
      ref_date          = period_date,
      personnel_id_col  = personnel_id_col,
      contract_id_col   = contract_id_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      salary_col        = salary_col,
      contract_type_col = contract_type_col,
      status_col        = status_col
    )
    retirees_dt  <- ret_result$retirees_dt
    contract_dt  <- ret_result$contract_dt
    personnel_dt <- ret_result$personnel_dt

    n_exits      <- if (!is.null(retirees_dt)) nrow(retirees_dt) else 0L
    exit_savings <- compute_exit_effect(retirees_dt, salary_col)

    if (!is.null(retirees_dt) && nrow(retirees_dt) > 0L &&
        "pension" %in% names(retirees_dt)) {
      pension_cost_new <- sum(retirees_dt$pension, na.rm = TRUE)

      # Append to pensioner register with period_date stamp
      new_reg <- data.table::data.table(
        personnel_id   = retirees_dt[[personnel_id_col]],
        pension_amount = retirees_dt$pension,
        period_date    = period_date
      )
      pensioner_register <- data.table::rbindlist(
        list(pensioner_register, new_reg),
        use.names = TRUE, fill = TRUE
      )
    }
  }

  pension_cost_total <- sum(pensioner_register$pension_amount, na.rm = TRUE)

  # ------------------------------------------------------------------
  # STEP 2: MOVEMENTS
  # ------------------------------------------------------------------
  n_promotions     <- 0L
  n_transfers      <- 0L
  promotion_effect <- 0
  transfer_effect  <- 0

  if (!is.null(movement_policy)) {
    if (is.null(movement_policy$salary_scale)) {
      movement_policy$salary_scale <- data.table::copy(salary_scale_dt)
    }

    mov_result <- simulate_promotions_transfers(
      contract_dt       = contract_dt,
      personnel_dt      = personnel_dt,
      policy_params     = movement_policy,
      ref_date          = period_date,
      personnel_id_col  = personnel_id_col,
      contract_id_col   = contract_id_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      salary_col        = salary_col,
      contract_type_col = contract_type_col,
      status_col        = status_col
    )

    movers_dt    <- mov_result$movers_dt
    contract_dt  <- mov_result$contract_dt
    personnel_dt <- mov_result$personnel_dt

    if (!is.null(mov_result$summary)) {
      n_prom <- mov_result$summary$n_promotions
      n_tran <- mov_result$summary$n_transfers
      n_promotions <- as.integer(if (!is.null(n_prom)) n_prom else 0L)
      n_transfers  <- as.integer(if (!is.null(n_tran)) n_tran else 0L)
    }

    mov_effects <- compute_movement_effect(
      movers_dt         = movers_dt,
      contract_dt_after = contract_dt,
      personnel_id_col  = personnel_id_col,
      salary_col        = salary_col
    )
    promotion_effect <- mov_effects$promotion
    transfer_effect  <- mov_effects$transfer
  }

  # ------------------------------------------------------------------
  # STEP 3: HIRING
  # ------------------------------------------------------------------
  n_hires       <- 0L
  hiring_effect <- 0

  if (!is.null(hiring_policy)) {
    if (is.null(hiring_policy$salary_scale)) {
      hiring_policy$salary_scale <- data.table::copy(salary_scale_dt)
    }

    hire_result <- simulate_hiring(
      contract_dt       = contract_dt,
      personnel_dt      = personnel_dt,
      policy_params     = hiring_policy,
      retirees_dt       = retirees_dt,
      ref_date          = period_date,
      personnel_id_col  = personnel_id_col,
      contract_id_col   = contract_id_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      salary_col        = salary_col,
      contract_type_col = contract_type_col,
      status_col        = status_col
    )

    contract_dt  <- hire_result$contract_dt
    personnel_dt <- hire_result$personnel_dt

    hiring_effect <- compute_hiring_effect(
      new_hire_contracts_dt = hire_result$new_hire_contracts_dt,
      salary_col            = salary_col
    )
    if (!is.null(hire_result$summary) &&
        "n_new_hires" %in% names(hire_result$summary)) {
      n_hires <- as.integer(hire_result$summary$n_new_hires)
    }
  }

  # ------------------------------------------------------------------
  # STEP 4: AGING
  # ------------------------------------------------------------------
  if (!is.null(age_col) && age_col %in% names(personnel_dt)) {
    personnel_dt[get(status_col) == "active",
                 (age_col) := get(age_col) + 1]
  }
  if (!is.null(tenure_col) && tenure_col %in% names(personnel_dt)) {
    personnel_dt[get(status_col) == "active",
                 (tenure_col) := get(tenure_col) + 1]
  }

  # ------------------------------------------------------------------
  # STEP 5: COLA
  # ------------------------------------------------------------------
  growth <- salary_growth_rate

  # COLA applies only to active (non-pensioner) salaries; pensioner rows are already 0
  pre_cola_wage_bill <- sum(
    contract_dt[get(contract_type_col) != "pensioner"][[salary_col]],
    na.rm = TRUE
  )
  inflation_effect   <- compute_inflation_effect(pre_cola_wage_bill, growth)

  if (growth != 0) {
    salary_scale_dt[, (scale_salary_col) := get(scale_salary_col) * (1 + growth)]
    contract_dt[get(contract_type_col) != "pensioner",
                (salary_col) := get(salary_col) * (1 + growth)]
  }

  # ------------------------------------------------------------------
  # SNAPSHOT B  (active employees only — pensioner rows excluded)
  # ------------------------------------------------------------------
  n_headcount_end <- nrow(contract_dt[get(contract_type_col) != "pensioner"])
  wage_bill_end   <- sum(
    contract_dt[get(contract_type_col) != "pensioner"][[salary_col]],
    na.rm = TRUE
  )

  # ------------------------------------------------------------------
  # Build summary row
  # ------------------------------------------------------------------
  summary_row <- data.table::data.table(
    period_date        = as.Date(period_date),
    n_headcount_start  = as.integer(n_headcount_start),
    wage_bill_start    = wage_bill_start,
    n_exits            = as.integer(n_exits),
    exit_savings       = exit_savings,
    pension_cost_new   = pension_cost_new,
    pension_cost_total = pension_cost_total,
    n_promotions       = as.integer(n_promotions),
    n_transfers        = as.integer(n_transfers),
    promotion_effect   = promotion_effect,
    transfer_effect    = transfer_effect,
    n_hires            = as.integer(n_hires),
    hiring_effect      = hiring_effect,
    inflation_effect   = inflation_effect,
    n_headcount_end    = as.integer(n_headcount_end),
    wage_bill_end      = wage_bill_end
  )

  list(
    summary            = summary_row,
    contract_dt        = contract_dt,
    personnel_dt       = personnel_dt,
    salary_scale_dt    = salary_scale_dt,
    pensioner_register = pensioner_register
  )
}


# ---------------------------------------------------------------------------
# Minimal NULL-coalescing helper (not exported)
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b


# ===========================================================================
# simulate_horizon() — multi-period loop
# ===========================================================================

#' Multi-Period Simulation Orchestrator
#'
#' @description
#' Runs the govhrcast simulation modules sequentially over \code{n_periods} years,
#' producing a wage bill decomposition table that attributes year-on-year changes to
#' four drivers: exits (retirements), internal movements (promotions/transfers),
#' new hiring, and inflation/COLA.
#'
#' @details
#' Within each period the operations are applied in this strict order:
#'
#' 1. **Retirement** — retirees are identified and removed; their pre-retirement
#'    salaries become \code{exit_savings}.
#' 2. **Movements** — promotions and transfers are applied; the net per-mover
#'    salary change is \code{promotion_effect} / \code{transfer_effect}.
#' 3. **Hiring** — vacancies are filled; new-hire contract salaries sum to
#'    \code{hiring_effect}.
#' 4. **Aging** — \code{age} and \code{tenure_years} incremented for all
#'    active personnel.
#' 5. **Inflation** — \code{salary_scale_dt} and all contract salaries scaled
#'    by \code{(1 + growth_rate)}.
#'
#' Wage bill is measured as \code{sum(salary_col)} over all rows in
#' \code{contract_dt} with no filtering.
#'
#' @import data.table
#'
#' @param contract_dt data.table. Initial contract microdata (single snapshot).
#' @param personnel_dt data.table. Initial personnel microdata.
#' @param salary_scale_dt data.table. Base pay table. Updated each period by
#'   \code{salary_growth_rate}.
#' @param n_periods Integer. Number of annual periods to simulate.
#' @param retirement_policy List or \code{NULL}. Policy parameters for
#'   \code{simulate_retirement()}. Pass \code{NULL} to skip.
#' @param movement_policy List or \code{NULL}. Policy parameters for
#'   \code{simulate_promotions_transfers()}. Pass \code{NULL} to skip.
#' @param hiring_policy List or \code{NULL}. Policy parameters for
#'   \code{simulate_hiring()}. Must include \code{mode} and \code{salary_scale}.
#'   Pass \code{NULL} to skip hiring in all periods.
#'   For \code{mode = "status_quo"}, also include \code{group_cols} and
#'   optionally \code{rate_mult} (default 1).  The full panel tables
#'   (\code{panel_contract_dt} and \code{panel_personnel_dt}) are injected
#'   automatically by \code{simulate_horizon()} from the \code{contract_dt} and
#'   \code{personnel_dt} arguments — you do \emph{not} need to set them manually.
#' @param salary_growth_rate Numeric scalar or vector of length \code{n_periods}.
#'   Annual COLA / inflation rate applied to salaries and the pay scale.
#'   Default \code{0} (no inflation).
#' @param base_year Integer. Calendar year label for period 0 (default:
#'   \code{as.integer(format(Sys.Date(), "\%Y"))}).
#' @param return_microdata Logical. If \code{TRUE}, include the final-period
#'   \code{contract_dt} and \code{personnel_dt} in the return value. Set
#'   \code{FALSE} (default) to conserve memory on long horizons.
#' @param ref_date Date. Simulation anchor date (default: \code{Sys.Date()}).
#'   Used to compute \code{period_date} for each period.
#' @param personnel_id_col Character. Personnel ID column (default:
#'   \code{"personnel_id"}).
#' @param contract_id_col Character. Contract ID column (default:
#'   \code{"contract_id"}).
#' @param start_date_col Character. Contract start date column (default:
#'   \code{"start_date"}).
#' @param end_date_col Character. Contract end date column (default:
#'   \code{"end_date"}).
#' @param salary_col Character. Salary column (default: \code{"gross_salary_lcu"}).
#' @param contract_type_col Character. Contract type column (default:
#'   \code{"contract_type_code"}).
#' @param status_col Character. Personnel status column (default:
#'   \code{"status"}).
#' @param age_col Character or \code{NULL}. Age column to increment each period.
#'   Default: \code{"age"}.
#' @param tenure_col Character or \code{NULL}. Tenure column to increment each
#'   period. Default: \code{"tenure_years"}.
#'
#' @return An object of class \code{horizon} with two primary components:
#'   \describe{
#'     \item{\code{$comparison}}{data.table. One row per simulated period with columns:
#'       \code{period_date}, \code{n_headcount_start}, \code{wage_bill_start},
#'       \code{n_exits}, \code{exit_savings}, \code{pension_cost_new},
#'       \code{pension_cost_total}, \code{n_promotions}, \code{n_transfers},
#'       \code{promotion_effect}, \code{transfer_effect}, \code{n_hires},
#'       \code{hiring_effect}, \code{inflation_effect},
#'       \code{n_headcount_end}, \code{wage_bill_end}, plus
#'       \code{exit_savings_pct_of_end_bill},
#'       \code{promotion_effect_pct_of_end_bill},
#'       \code{transfer_effect_pct_of_end_bill},
#'       \code{hiring_effect_pct_of_end_bill},
#'       \code{inflation_effect_pct_of_end_bill}.}
#'     \item{\code{$summary_dt}}{Same as \code{$comparison} (backward-compatible alias).}
#'     \item{\code{$metadata}}{Named list with \code{policy_args} capturing the
#'       retirement, movement, and hiring policy parameters used.}
#'     \item{\code{$contract_dt}}{data.table. Final-period contract snapshot.
#'       Only present when \code{return_microdata = TRUE}.}
#'     \item{\code{$personnel_dt}}{data.table. Final-period personnel snapshot.
#'       Only present when \code{return_microdata = TRUE}.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' contract_dt  <- data.table::copy(bra_hrmis_contract)
#' personnel_dt <- data.table::copy(bra_hrmis_personnel)
#' REF_DATE     <- as.Date("2016-09-01")
#'
#' ct <- contract_dt[ref_date == REF_DATE]
#' pt <- personnel_dt[ref_date == REF_DATE]
#'
#' salary_scale <- data.table(
#'   est_id           = unique(ct$est_id),
#'   gross_salary_lcu = 5000
#' )
#'
#' results <- simulate_horizon(
#'   contract_dt        = ct,
#'   personnel_dt       = pt,
#'   salary_scale_dt    = salary_scale,
#'   n_periods          = 5L,
#'   retirement_policy  = list(
#'     eligibility_type = "age_and_tenure",
#'     min_age          = 60,
#'     min_tenure       = 20,
#'     pension_type     = "db",
#'     pension_params   = list(accrual_rate = 0.02, ref_wage_col = "gross_salary_lcu",
#'                             max_years = 35, replacement_cap = 0.80)
#'   ),
#'   movement_policy = list(
#'     group_cols           = "est_id",
#'     salary_scale         = salary_scale,
#'     promotion_multiplier = 1.0,
#'     transfer_multiplier  = 1.0,
#'     promotion_strategy   = "tenure",
#'     transfer_strategy    = "random"
#'   ),
#'   hiring_policy = list(
#'     mode             = "flow",
#'     group_cols       = "est_id",
#'     replacement_rate = 1.0,
#'     salary_scale     = salary_scale
#'   ),
#'   salary_growth_rate = 0.03,
#'   ref_date           = REF_DATE,
#'   return_microdata   = FALSE
#' )
#'
#' results$summary_dt
#' }
#'
#' @export
simulate_horizon <- function(contract_dt,
                             personnel_dt,
                             salary_scale_dt,
                             n_periods,
                             retirement_policy  = NULL,
                             movement_policy    = NULL,
                             hiring_policy      = NULL,
                             salary_growth_rate = 0,
                             base_year          = as.integer(format(Sys.Date(), "%Y")),
                             return_microdata   = FALSE,
                             ref_date           = Sys.Date(),
                             personnel_id_col   = "personnel_id",
                             contract_id_col    = "contract_id",
                             start_date_col     = "start_date",
                             end_date_col       = "end_date",
                             salary_col         = "gross_salary_lcu",
                             contract_type_col  = "contract_type_code",
                             status_col         = "status",
                             age_col            = "age",
                             tenure_col         = "tenure_years") {

  # ====================================================================
  # 0. Validate and set up
  # ====================================================================
  n_periods <- as.integer(n_periods)
  if (is.na(n_periods) || n_periods < 1L) {
    stop("n_periods must be a positive integer.", call. = FALSE)
  }

  ref_date <- validate_date_format(ref_date, "ref_date")

  # Capture original value for metadata before expanding to per-period vector
  salary_growth_rate_input <- salary_growth_rate

  # Expand scalar growth rate to vector
  if (length(salary_growth_rate) == 1L) {
    salary_growth_rate <- rep(salary_growth_rate, n_periods)
  } else if (length(salary_growth_rate) != n_periods) {
    stop("salary_growth_rate must be a scalar or a vector of length n_periods (",
         n_periods, ").", call. = FALSE)
  }

  # Working copies
  if (!data.table::is.data.table(contract_dt)) {
    contract_dt <- data.table::as.data.table(contract_dt)
  } else {
    contract_dt <- data.table::copy(contract_dt)
  }
  if (!data.table::is.data.table(personnel_dt)) {
    personnel_dt <- data.table::as.data.table(personnel_dt)
  } else {
    personnel_dt <- data.table::copy(personnel_dt)
  }
  salary_scale_dt <- data.table::copy(salary_scale_dt)

  # For status_quo hiring, the panel must be captured BEFORE ref_date is stripped
  if (!is.null(hiring_policy) && identical(hiring_policy$mode, "status_quo")) {
    if (is.null(hiring_policy$panel_contract_dt))
      hiring_policy$panel_contract_dt  <- data.table::copy(contract_dt)
    if (is.null(hiring_policy$panel_personnel_dt))
      hiring_policy$panel_personnel_dt <- data.table::copy(personnel_dt)
  }

  # For movement, pre-compute the baseline from the full panel BEFORE stripping
  # ref_date.  simulate_horizon works on single-snapshot data in each period, so
  # without this cache simulate_promotions_transfers would never see a panel.
  if (!is.null(movement_policy) && is.null(movement_policy$baseline_matrix)) {
    ref_date_col_name <- "ref_date"   # standard column name used throughout
    has_panel <- ref_date_col_name %in% names(contract_dt) &&
                 data.table::uniqueN(contract_dt[[ref_date_col_name]]) >= 2L
    if (has_panel) {
      movement_policy$baseline_matrix <- estimate_movement_baseline(
        contract_dt       = contract_dt,
        group_cols        = movement_policy$group_cols,
        personnel_id_col  = personnel_id_col,
        ref_date_col      = ref_date_col_name,
        start_date_col    = start_date_col,
        end_date_col      = end_date_col,
        contract_type_col = contract_type_col
      )
    }
  }

  # If data contains a ref_date panel column, subset to the starting snapshot
  # before stripping so that sub-modules see a single-period snapshot
  start_ref <- ref_date   # capture before any column could shadow it
  if ("ref_date" %in% names(contract_dt)) {
    contract_dt <- contract_dt[get("ref_date") == start_ref]
    contract_dt[, ref_date := NULL]
  }
  if ("ref_date" %in% names(personnel_dt)) {
    personnel_dt <- personnel_dt[get("ref_date") == start_ref]
    personnel_dt[, ref_date := NULL]
  }

  if (!salary_col %in% names(contract_dt)) {
    stop("salary_col '", salary_col, "' not found in contract_dt.", call. = FALSE)
  }

  # Initialise pensioner register
  pensioner_register <- data.table::data.table(
    personnel_id   = character(0),
    pension_amount = numeric(0),
    period_date    = as.Date(character(0))
  )

  period_rows <- vector("list", n_periods)

  # ====================================================================
  # PERIOD LOOP
  # ====================================================================
  for (t in seq_len(n_periods)) {

    growth   <- salary_growth_rate[t]
    cur_date <- .add_years(ref_date, t - 1L)

    scenario_result <- simulate_scenario(
      contract_dt        = contract_dt,
      personnel_dt       = personnel_dt,
      salary_scale_dt    = salary_scale_dt,
      period_date        = cur_date,
      pensioner_register = pensioner_register,
      retirement_policy  = retirement_policy,
      movement_policy    = movement_policy,
      hiring_policy      = hiring_policy,
      salary_growth_rate = growth,
      personnel_id_col   = personnel_id_col,
      contract_id_col    = contract_id_col,
      start_date_col     = start_date_col,
      end_date_col       = end_date_col,
      salary_col         = salary_col,
      contract_type_col  = contract_type_col,
      status_col         = status_col,
      age_col            = age_col,
      tenure_col         = tenure_col
    )

    # Thread state forward
    contract_dt        <- scenario_result$contract_dt
    personnel_dt       <- scenario_result$personnel_dt
    salary_scale_dt    <- scenario_result$salary_scale_dt
    pensioner_register <- scenario_result$pensioner_register

    period_rows[[t]] <- scenario_result$summary
  }

  # ====================================================================
  # Assemble output
  # ====================================================================
  summary_dt <- data.table::rbindlist(period_rows, use.names = TRUE, fill = TRUE)

  # Add _pct_of_end_bill share columns for each effect driver
  effect_cols <- c("exit_savings", "promotion_effect", "transfer_effect",
                   "hiring_effect", "inflation_effect")
  for (ec in effect_cols) {
    pct_col <- paste0(ec, "_pct_of_end_bill")
    summary_dt[, (pct_col) := data.table::fifelse(
      wage_bill_end > 0,
      get(ec) / wage_bill_end,
      NA_real_
    )]
  }

  # Capture policy arguments for metadata
  policy_args <- list(
    retirement_policy  = retirement_policy,
    movement_policy    = movement_policy,
    hiring_policy      = hiring_policy,
    salary_growth_rate = salary_growth_rate_input,
    n_periods          = n_periods,
    ref_date           = ref_date
  )

  out <- new_horizon(
    comparison = summary_dt,
    metadata   = list(policy_args = policy_args)
  )

  if (return_microdata) {
    out$contract_dt  <- contract_dt
    out$personnel_dt <- personnel_dt
  }

  return(out)
}


# ---------------------------------------------------------------------------
# Helper: advance Date by integer years (no lubridate)
# ---------------------------------------------------------------------------
.add_years <- function(date, n) {
  if (n == 0L) return(date)
  seq(date, by = "year", length.out = n + 1L)[n + 1L]
}
