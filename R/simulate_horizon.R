# ===========================================================================
# Effect-computation helpers
# ===========================================================================

# Suppress R CMD check NOTEs for data.table column names used in
# [.data.table j/i expressions throughout this file.
utils::globalVariables(c(
  "salary",             # .active_wage_bill: named column in .[, .(salary = ...)]
  "pension_amount",     # simulate_scenario / simulate_horizon: pensioner register col
  "age_at_ret",         # simulate_horizon: temp col in age_lookup join
  "age_at_retirement",  # simulate_horizon / simulate_scenario: pensioner register col
  "i.age_at_ret",       # simulate_horizon: i-prefix col from data.table join
  ".already_eligible_"  # simulate_horizon: temp eligibility flag
))

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
  zero <- list(movement = 0)
  if (is.null(movers_dt) || nrow(movers_dt) == 0L) return(zero)
  if (!"salary_before" %in% names(movers_dt))       return(zero)
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

  movement_effect <- mv[, sum(salary_diff, na.rm = TRUE)]
  if (length(movement_effect) == 0L) movement_effect <- 0

  list(movement = movement_effect)
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
# simulate_scenario() -- single-period orchestrator
# ===========================================================================

# Internal helper: salary-bearing payroll total.
# Wage bill definition: sum of ALL salary-bearing contract rows per person,
# then sum across persons.  A person with two simultaneous active contracts
# contributes both salaries.  Inactive-but-paid staff (non-NA salary,
# non-pensioner) are included because the government is paying their salary.
# Pensioner rows are excluded — their cost is tracked in pensioner_register.
# Used three times in simulate_scenario() for wage_bill_start, pre_cola, and
# wage_bill_end snapshots.  No roxygen: internal only, not exported.
.active_wage_bill <- function(contract_dt,
                              contract_type_col,
                              salary_col,
                              personnel_id_col) {
  contract_dt[
    get(contract_type_col) != "pensioner" & !is.na(get(salary_col)),
    .(salary = sum(get(salary_col), na.rm = TRUE)),
    by = c(personnel_id_col)
  ][, sum(salary, na.rm = TRUE)]
}

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
#' @param exit_policy List or \code{NULL}.  Policy parameters for non-retirement
#'   attrition.  Passed to \code{compute_status_quo_exits()}.  Pass \code{NULL}
#'   to model zero attrition.
#' @param movement_policy List or \code{NULL}.  Passed to
#'   \code{simulate_promotions_transfers()}.  Pass \code{NULL} to skip.
#' @param hiring_policy List or \code{NULL}.  Passed to
#'   \code{simulate_hiring()}.  Pass \code{NULL} to skip.
#' @param salary_growth_rate Numeric scalar.  COLA rate for this period.
#'   Default \code{0}.
#' @param pension_cola_rate Numeric scalar.  Annual COLA rate applied to
#'   \code{pensioner_register$pension_amount} this period.  Defaults to
#'   \code{salary_growth_rate} when \code{NULL}.  Default \code{0}.
#' @param period_date Date.  Simulation date for this period -- stored in
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
#' @param period_fraction Numeric.  Fraction of a year each period represents
#'   (1 for annual, 1/12 for monthly, 1/365.25 for daily).  Used to increment
#'   \code{age_col} and \code{tenure_col} by the correct amount.  Default 1.
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
                               exit_policy         = NULL,
                               movement_policy     = NULL,
                               hiring_policy       = NULL,
                               salary_growth_rate  = 0,
                               pension_cola_rate   = 0,
                               personnel_id_col    = "personnel_id",
                               contract_id_col     = "contract_id",
                               start_date_col      = "start_date",
                               end_date_col        = "end_date",
                               salary_col          = "gross_salary_lcu",
                               contract_type_col   = "contract_type_code",
                               status_col          = "status",
                               age_col             = "age",
                               tenure_col          = "tenure_years",
                               period_fraction     = 1) {

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
      personnel_id               = character(0),
      pension_amount             = numeric(0),
      final_salary               = numeric(0),
      tenure_years_at_retirement = numeric(0),
      age_at_retirement          = numeric(0),
      period_date                = as.Date(character(0))
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
  n_headcount_start <- data.table::uniqueN(
    contract_dt[get(contract_type_col) != "pensioner"],
    by = personnel_id_col
  )
  wage_bill_start <- .active_wage_bill(
    contract_dt, contract_type_col, salary_col, personnel_id_col
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
      status_col        = status_col,
      age_col           = age_col,
      tenure_col        = tenure_col
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
        personnel_id               = retirees_dt[[personnel_id_col]],
        pension_amount             = retirees_dt$pension,
        final_salary               = retirees_dt[[salary_col]],
        tenure_years_at_retirement = if ("tenure_years" %in% names(retirees_dt))
                                       retirees_dt$tenure_years else NA_real_,
        age_at_retirement          = if ("age" %in% names(retirees_dt))
                                       retirees_dt$age else NA_real_,
        period_date                = period_date
      )
      pensioner_register <- data.table::rbindlist(
        list(pensioner_register, new_reg),
        use.names = TRUE, fill = TRUE
      )
    }
  }

  pension_cost_total <- sum(pensioner_register$pension_amount, na.rm = TRUE)

  # ------------------------------------------------------------------
  # STEP 2: NON-RETIREMENT EXITS (Phase 3)
  # ------------------------------------------------------------------
  n_non_ret_exits          <- 0L
  non_ret_exit_savings     <- 0

  if (!is.null(exit_policy)) {
    exit_result <- simulate_exits(
      contract_dt       = contract_dt,
      personnel_dt      = personnel_dt,
      policy_params     = exit_policy,
      ref_date          = period_date,
      personnel_id_col  = personnel_id_col,
      contract_id_col   = contract_id_col,
      contract_type_col = contract_type_col,
      status_col        = status_col,
      salary_col        = salary_col,
      end_date_col      = end_date_col
    )
    contract_dt          <- exit_result$contract_dt
    personnel_dt         <- exit_result$personnel_dt
    n_non_ret_exits      <- exit_result$summary$n_exits
    non_ret_exit_savings <- exit_result$summary$exit_savings
  }

  # ------------------------------------------------------------------
  # STEP 3: MOVEMENTS
  # ------------------------------------------------------------------
  n_promotions     <- 0L
  n_transfers      <- 0L
  promotion_effect <- 0
  transfer_effect  <- 0

  if (!is.null(movement_policy)) {
    mov_result <- simulate_promotions_transfers(
      contract_dt       = contract_dt,
      personnel_dt      = personnel_dt,
      salary_scale_dt   = data.table::copy(salary_scale_dt),
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
      n_movers_val <- mov_result$summary$n_movers
      n_promotions <- as.integer(if (!is.null(n_movers_val)) n_movers_val else 0L)
      n_transfers  <- 0L
    }

    mov_effects <- compute_movement_effect(
      movers_dt         = movers_dt,
      contract_dt_after = contract_dt,
      personnel_id_col  = personnel_id_col,
      salary_col        = salary_col
    )
    promotion_effect <- mov_effects$movement
    transfer_effect  <- 0
  }

  # ------------------------------------------------------------------
  # STEP 4: HIRING
  # ------------------------------------------------------------------
  n_hires       <- 0L
  hiring_effect <- 0

  if (!is.null(hiring_policy)) {
    if (is.null(hiring_policy$salary_scale)) {
      # Derive a hiring-compatible salary scale from salary_scale_dt by
      # aggregating to the hiring group_cols level.  This avoids injecting a
      # finer-grained scale (e.g. est_id × paygrade) into a hiring module that
      # joins only on est_id — which would cause a cartesian join error.
      hire_gcols <- hiring_policy$group_cols %||% character(0)
      valid_gcols <- intersect(hire_gcols, names(salary_scale_dt))
      if (length(valid_gcols) > 0L) {
        hiring_policy$salary_scale <- salary_scale_dt[,
          .(gross_salary_lcu = mean(get(scale_salary_col), na.rm = TRUE)),
          keyby = valid_gcols
        ]
      } else {
        hiring_policy$salary_scale <- data.table::copy(salary_scale_dt)
      }
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
  # STEP 5: AGING
  # ------------------------------------------------------------------
  if (!is.null(age_col) && age_col %in% names(personnel_dt)) {
    personnel_dt[get(status_col) == "active",
                 (age_col) := get(age_col) + period_fraction]
  }
  if (!is.null(tenure_col) && tenure_col %in% names(personnel_dt)) {
    personnel_dt[get(status_col) == "active",
                 (tenure_col) := get(tenure_col) + period_fraction]
  }

  # ------------------------------------------------------------------
  # STEP 6: COLA
  # ------------------------------------------------------------------
  growth <- salary_growth_rate

  # COLA applies only to active (non-pensioner) salaries; pensioner rows are already 0
  pre_cola_wage_bill <- .active_wage_bill(
    contract_dt, contract_type_col, salary_col, personnel_id_col
  )
  inflation_effect   <- compute_inflation_effect(pre_cola_wage_bill, growth)

  if (growth != 0) {
    salary_scale_dt[, (scale_salary_col) := get(scale_salary_col) * (1 + growth)]
    contract_dt[get(contract_type_col) != "pensioner",
                (salary_col) := get(salary_col) * (1 + growth)]
  }

  # Apply pension COLA to the register (separate rate from active salary COLA)
  if (pension_cola_rate != 0 && nrow(pensioner_register) > 0L) {
    pensioner_register[, pension_amount := pension_amount * (1 + pension_cola_rate)]
  }

  # ------------------------------------------------------------------
  # SNAPSHOT B  (active employees only — pensioner rows excluded)
  # ------------------------------------------------------------------
  n_headcount_end <- data.table::uniqueN(
    contract_dt[get(contract_type_col) != "pensioner"],
    by = personnel_id_col
  )
  wage_bill_end <- .active_wage_bill(
    contract_dt, contract_type_col, salary_col, personnel_id_col
  )

  # ------------------------------------------------------------------
  # Build summary row
  # ------------------------------------------------------------------
  summary_row <- data.table::data.table(
    period_date              = as.Date(period_date),
    n_headcount_start        = as.integer(n_headcount_start),
    wage_bill_start          = wage_bill_start,
    n_exits                  = as.integer(n_exits),
    exit_savings             = exit_savings,
    n_non_ret_exits          = as.integer(n_non_ret_exits),
    non_ret_exit_savings     = non_ret_exit_savings,
    pension_cost_new         = pension_cost_new,
    pension_cost_total       = pension_cost_total,
    n_promotions             = as.integer(n_promotions),
    n_transfers              = as.integer(n_transfers),
    promotion_effect         = promotion_effect,
    transfer_effect          = transfer_effect,
    n_hires                  = as.integer(n_hires),
    hiring_effect            = hiring_effect,
    inflation_effect         = inflation_effect,
    n_headcount_end          = as.integer(n_headcount_end),
    wage_bill_end            = wage_bill_end
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
#' @param exit_policy List or \code{NULL}. Policy parameters controlling
#'   non-retirement attrition (voluntary exits, contract non-renewals).  When
#'   non-\code{NULL}, must contain at minimum \code{mode} (one of
#'   \code{"fixed_rate"}, \code{"status_quo"}) and \code{exit_strategy}.
#'   Pass \code{NULL} (default) to model zero non-retirement attrition.
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
#' @param pension_cola_rate Numeric scalar or vector of length \code{n_periods}.
#'   Annual COLA rate applied to \code{pensioner_register$pension_amount} each
#'   period.  Defaults to \code{salary_growth_rate} (same rate for both groups).
#'   Accepts either a scalar or a length-\code{n_periods} vector.
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
#' @param period_unit Character. Stepping unit for each simulated period.
#'   One of \code{"year"} (default), \code{"month"}, or \code{"day"}.
#'   Controls both the calendar advance and the fraction by which
#'   \code{age_col} / \code{tenure_col} are incremented.  When
#'   \code{period_unit != "year"}, \code{salary_growth_rate} (an annual rate)
#'   is automatically converted to a per-period compound equivalent.
#' @param birth_date_col Character or \code{NULL}. Birth date column in
#'   \code{personnel_dt} used to auto-compute \code{age_col} at the start of
#'   the simulation (before the first period).  If the column is present,
#'   \code{age_col} is overwritten with the age in fractional years at
#'   \code{ref_date}.  Pass \code{NULL} to skip (user must supply a
#'   pre-computed \code{age_col} column).  Default: \code{"birth_date"}.
#' @param scenario_name Character scalar or \code{NULL}.  A human-readable
#'   label for this simulation run (e.g. \code{"Baseline"} or
#'   \code{"High-growth scenario"}).  When supplied, it is stamped into
#'   \code{$comparison} as both \code{scenario_id} (the value itself, used as
#'   a stable join key) and \code{scenario_label} (for display).  These
#'   columns are consumed by the Shiny app's \code{generate_scenario_matrix()}
#'   layer and by \code{is_baseline} toggle logic.  Default \code{NULL}
#'   (columns are omitted from the output).
#' @param is_baseline Logical scalar.  When \code{TRUE}, stamps an
#'   \code{is_baseline} column (\code{TRUE}) onto \code{$comparison} to flag
#'   this run as the reference scenario in multi-scenario comparisons.  Only
#'   meaningful when \code{scenario_name} is also supplied.  Default
#'   \code{FALSE}.
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
#'       \code{inflation_effect_pct_of_end_bill}.
#'       When \code{scenario_name} is supplied, also includes
#'       \code{scenario_id}, \code{scenario_label}, and (when
#'       \code{is_baseline = TRUE}) \code{is_baseline}.}
#'     \item{\code{$summary_dt}}{Same as \code{$comparison} (backward-compatible alias).}
#'     \item{\code{$metadata}}{Named list with \code{policy_args} capturing the
#'       retirement, movement, and hiring policy parameters used, plus
#'       \code{scenario_name} and \code{is_baseline} when supplied.}
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
                             exit_policy        = NULL,
                             movement_policy    = NULL,
                             hiring_policy      = NULL,
                             salary_growth_rate = 0,
                             pension_cola_rate  = salary_growth_rate,
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
                             age_col            = NULL,
                             tenure_col         = NULL,
                             period_unit        = "year",
                             birth_date_col     = "birth_date",
                             scenario_name      = NULL,
                             is_baseline        = FALSE) {

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

  # Resolve NULL age_col / tenure_col to standard column names.
  # NULL means: "compute from source data and store in the default column".
  # The rest of the function always receives a character scalar.
  if (is.null(age_col))    age_col    <- "age"
  if (is.null(tenure_col)) tenure_col <- "tenure_years"

  # Validate period_unit
  period_unit <- match.arg(period_unit, c("year", "month", "day"))

  # Fraction of a year each period represents; used for aging increment and
  # for converting annual salary_growth_rate to a per-period compound rate.
  period_fraction <- switch(period_unit,
                            year  = 1,
                            month = 1 / 12,
                            day   = 1 / 365.25)

  # Expand scalar/vector rate parameters to per-period vectors
  salary_growth_rate <- expand_rate_vector(salary_growth_rate, n_periods, "salary_growth_rate")
  pension_cola_rate  <- expand_rate_vector(pension_cola_rate,  n_periods, "pension_cola_rate")

  # Convert annual rates to per-period compound equivalents when period != year
  if (period_unit != "year") {
    salary_growth_rate <- (1 + salary_growth_rate)^period_fraction - 1
    pension_cola_rate  <- (1 + pension_cola_rate)^period_fraction  - 1
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
  if (!is.null(movement_policy) && is.null(movement_policy$policy_table)) {
    ref_date_col_name <- "ref_date"   # standard column name used throughout
    has_panel <- ref_date_col_name %in% names(contract_dt) &&
                 data.table::uniqueN(contract_dt[[ref_date_col_name]]) >= 2L
    if (has_panel) {
      movement_policy$policy_table <- estimate_movement_baseline(
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

  # Resolve pensioner type label before any filtering uses it.
  .pensioner_type_val_ <- if (!is.null(retirement_policy) &&
                               !is.null(retirement_policy$pensioner_type_value))
    retirement_policy$pensioner_type_value else "pensioner"

  # Retain only salary-bearing contracts in the starting snapshot.
  # Rule: keep a row if it is a pensioner contract (needed to seed the
  # pensioner register) OR if it has a non-NA salary (the government is paying,
  # regardless of contract_type — this correctly includes inactive staff on
  # government-funded leave or training).
  # Drop rows that are neither: these are truly separated staff whose contract
  # lingers in panel data with no salary, and must not inflate headcount or
  # the wage bill.
  .has_salary_ <- !is.na(contract_dt[[salary_col]])
  .is_pensioner_ <- contract_dt[[contract_type_col]] == .pensioner_type_val_
  if (any(!(.has_salary_ | .is_pensioner_))) {
    contract_dt <- contract_dt[.has_salary_ | .is_pensioner_]
  }

  if (!salary_col %in% names(contract_dt)) {
    stop("salary_col '", salary_col, "' not found in contract_dt.", call. = FALSE)
  }

  # Initialise pensioner register with expanded schema (Phase 2c Part B)
  pensioner_register <- data.table::data.table(
    personnel_id               = character(0),
    pension_amount             = numeric(0),   # COLA-adjusted each period
    final_salary               = numeric(0),   # salary at moment of retirement
    tenure_years_at_retirement = numeric(0),
    age_at_retirement          = numeric(0),
    period_date                = as.Date(character(0))
  )

  # Seed register from pre-existing retirees in the starting snapshot (Phase 2c Part A).
  # People with contract_type_col == pensioner_type_value were already retired at ref_date;
  # their pension costs should appear in period 1's pension_cost_total.

  existing_pensioners <- contract_dt[
    get(contract_type_col) == .pensioner_type_val_
  ]
  if (nrow(existing_pensioners) > 0L) {
    .ref_date_seed_ <- ref_date  # alias — avoid data.table column shadowing
    seed_reg <- existing_pensioners[, {
      max_sal <- if (.N > 0L) max(get(salary_col), na.rm = TRUE) else 0
      list(
        pension_amount             = max_sal,
        final_salary               = max_sal,
        tenure_years_at_retirement = NA_real_,
        age_at_retirement          = NA_real_,
        period_date                = .ref_date_seed_
      )
    }, by = c(personnel_id_col)]
    data.table::setnames(seed_reg, personnel_id_col, "personnel_id")

    # Enrich seed with age at retirement from birth_date in personnel_dt.
    # Tenure is not reliably available for pre-existing pensioners (their
    # contracts are already closed), so leave tenure_years_at_retirement as NA.
    if (!is.null(birth_date_col) && birth_date_col %in% names(personnel_dt)) {
      age_lookup <- personnel_dt[
        get(personnel_id_col) %in% seed_reg$personnel_id,
        c(personnel_id_col, birth_date_col), with = FALSE
      ]
      data.table::setnames(age_lookup, personnel_id_col, "personnel_id")
      .ref_seed_age_ <- ref_date
      age_lookup[, age_at_ret := as.numeric(
        difftime(.ref_seed_age_, get(birth_date_col), units = "days")
      ) / 365.25]
      seed_reg[age_lookup, age_at_retirement := i.age_at_ret,
               on = "personnel_id"]
    }

    pensioner_register <- data.table::rbindlist(
      list(pensioner_register, seed_reg),
      use.names = TRUE, fill = TRUE
    )
  }

  # ====================================================================
  # P2/P3. Auto-compute age and tenure from source columns (Phase 1b)
  # ====================================================================
  # Age: overwrite age_col using birth_date_col, if both are available.
  # This is forward-compatible with Phase 2b where the prologue will also
  # pre-compute tenure once and increment it per period rather than
  # recomputing inside identify_retirees().
  if (!is.null(birth_date_col) &&
      !is.null(age_col) &&
      birth_date_col %in% names(personnel_dt)) {
    .ref_date_p1b_ <- ref_date  # alias: data.table col 'ref_date' must not shadow this
    personnel_dt[, (age_col) := as.numeric(
      difftime(.ref_date_p1b_, get(birth_date_col), units = "days")
    ) / 365.25]
  }

  # Tenure: compute from contract history and inject into personnel_dt.
  # This replaces the per-period compute_tenure() call inside
  # identify_retirees() with a single upfront computation.
  if (!is.null(tenure_col)) {
    tenure_init <- compute_tenure(
      contract_dt       = contract_dt,
      ref_date          = ref_date,
      personnel_id_col  = personnel_id_col,
      contract_id_col   = contract_id_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      contract_type_col = contract_type_col
    )
    if (nrow(tenure_init) > 0L) {
      # Coerce tenure_col to numeric first to avoid integer truncation warning
      # when the column was initialised as integer (e.g. rep(10L, n)).
      if (is.integer(personnel_dt[[tenure_col]])) {
        personnel_dt[, (tenure_col) := as.numeric(get(tenure_col))]
      }
      personnel_dt[tenure_init, (tenure_col) := i.tenure_years,
                   on = c(personnel_id_col)]
    }
  }

  # ====================================================================
  # PRE-LOOP: Retire the already-eligible backlog (Phase 2c Part C)
  # ====================================================================
  # Problem: the starting snapshot may contain active employees who are
  # already past the retirement eligibility threshold at ref_date.  These
  # are people who *should* have retired before the simulation window —
  # either due to a data quality lag (HRMIS not updated) or because they
  # crossed the threshold in the partial year before ref_date.  If left
  # untreated they all fire in period 1, making n_exits in the first
  # period 2–5× higher than any subsequent period.
  #
  # Fix: apply the same eligibility check used inside identify_retirees()
  # against the already-computed age / tenure columns, retire the backlog
  # into the pensioner_register at period_date = ref_date, and mark their
  # contracts / personnel status accordingly.  Period 1 then only catches
  # the genuine marginal cohort.
  if (!is.null(retirement_policy) &&
      !is.null(age_col) && age_col %in% names(personnel_dt)) {

    .rp_defs_    <- retirement_policy$defaults %||% list()
    .min_age_    <- .rp_defs_$min_age    %||% Inf
    .min_tenure_ <- .rp_defs_$min_tenure %||% 0
    .elig_type_  <- .rp_defs_$eligibility_type %||% "age_only"

    # Backlog threshold = min_age + period_fraction.
    # People aged exactly [min_age, min_age + period_fraction) at ref_date
    # crossed the threshold during the 12 months (or 1 month, etc.) *before*
    # ref_date — they are the genuine period-1 marginal cohort and must NOT
    # be pre-retired here.  Only those already past (min_age + period_fraction)
    # are true backlog cases that should have retired in a prior period.
    .backlog_age_cutoff_    <- .min_age_ + period_fraction
    .backlog_tenure_cutoff_ <- .min_tenure_ + period_fraction

    # Build a small eligibility table using the pre-computed columns.
    .pre_elig_ <- personnel_dt[
      get(status_col) == "active",
      c(personnel_id_col, age_col,
        if (!is.null(tenure_col) && tenure_col %in% names(personnel_dt))
          tenure_col else NULL),
      with = FALSE
    ]

    # Apply the same switch logic as identify_retirees(), but using the
    # backlog cutoffs (min_age + period_fraction) not the raw thresholds.
    .pre_elig_[, .already_eligible_ := switch(
      .elig_type_,
      "age_only"      = get(age_col) >= .backlog_age_cutoff_,
      "tenure_only"   = if (!is.null(tenure_col) && tenure_col %in% names(.pre_elig_))
                          get(tenure_col) >= .backlog_tenure_cutoff_ else FALSE,
      "age_and_tenure"= get(age_col) >= .backlog_age_cutoff_ &
                        if (!is.null(tenure_col) && tenure_col %in% names(.pre_elig_))
                          get(tenure_col) >= .backlog_tenure_cutoff_ else FALSE,
      FALSE
    )]

    .backlog_ids_ <- .pre_elig_[.already_eligible_ == TRUE,
                                 get(personnel_id_col)]

    if (length(.backlog_ids_) > 0L) {

      # Get their primary active contract to derive salary and group info
      .backlog_contracts_ <- get_active_contracts(
        contract_dt       = contract_dt,
        ref_date          = ref_date,
        start_date_col    = start_date_col,
        end_date_col      = end_date_col,
        contract_type_col = contract_type_col
      )[get(personnel_id_col) %in% .backlog_ids_]

      if (nrow(.backlog_contracts_) > 0L) {
        .backlog_primary_ <- get_primary_contract(
          contract_dt      = .backlog_contracts_,
          personnel_id_col = personnel_id_col,
          contract_id_col  = contract_id_col,
          start_date_col   = start_date_col,
          salary_col       = salary_col
        )

        # Join age / tenure onto primary contracts
        .backlog_primary_[.pre_elig_[, c(personnel_id_col, age_col,
          if (!is.null(tenure_col) && tenure_col %in% names(.pre_elig_))
            tenure_col else NULL), with = FALSE],
          c("age", "tenure_years") :=
            list(get(paste0("i.", age_col)),
                 if (!is.null(tenure_col) && tenure_col %in% names(.pre_elig_))
                   get(paste0("i.", tenure_col)) else NA_real_),
          on = c(personnel_id_col)
        ]
        if (!"age" %in% names(.backlog_primary_))
          .backlog_primary_[, age := NA_real_]
        if (!"tenure_years" %in% names(.backlog_primary_))
          .backlog_primary_[, tenure_years := NA_real_]

        # Compute pension amounts — add param columns required by new compute_pension(dt)
        .bp_defs_ <- retirement_policy$defaults %||% list()
        data.table::set(.backlog_primary_, j = "pension_type",    value = .bp_defs_$pension_type    %||% "flat")
        data.table::set(.backlog_primary_, j = "flat_amount",     value = .bp_defs_$flat_amount     %||% 0)
        data.table::set(.backlog_primary_, j = "accrual_rate",    value = .bp_defs_$accrual_rate    %||% NA_real_)
        data.table::set(.backlog_primary_, j = "ref_wage_col",    value = .bp_defs_$ref_wage_col    %||% NA_character_)
        data.table::set(.backlog_primary_, j = "max_years",       value = .bp_defs_$max_years       %||% NA_real_)
        data.table::set(.backlog_primary_, j = "replacement_cap", value = .bp_defs_$replacement_cap %||% NA_real_)
        data.table::set(.backlog_primary_, j = "balance_col",     value = .bp_defs_$balance_col     %||% NA_character_)
        data.table::set(.backlog_primary_, j = "annuity_factor",  value = .bp_defs_$annuity_factor  %||% NA_real_)
        data.table::set(.backlog_primary_, j = "notional_rate",   value = .bp_defs_$notional_rate   %||% NA_real_)
        data.table::set(.backlog_primary_, j = "pension",         value = compute_pension(.backlog_primary_))

        # Append to pensioner register
        .ref_backlog_ <- ref_date
        .backlog_reg_ <- .backlog_primary_[, .(
          personnel_id               = get(personnel_id_col),
          pension_amount             = pension,
          final_salary               = get(salary_col),
          tenure_years_at_retirement = tenure_years,
          age_at_retirement          = age,
          period_date                = .ref_backlog_
        )]
        pensioner_register <- data.table::rbindlist(
          list(pensioner_register, .backlog_reg_),
          use.names = TRUE, fill = TRUE
        )
      }

      # Flip contracts: mark all active contracts for backlog ids as pensioner
      contract_dt[
        get(personnel_id_col) %in% .backlog_ids_ &
          !get(contract_type_col) %in% c(.pensioner_type_val_, "inactive"),
        c(contract_type_col, end_date_col, salary_col) :=
          list(.pensioner_type_val_, ref_date, 0)
      ]

      # Flip personnel status to inactive
      personnel_dt[
        get(personnel_id_col) %in% .backlog_ids_,
        (status_col) := "inactive"
      ]
    }
  }

  period_rows <- vector("list", n_periods)

  # ====================================================================
  # PERIOD LOOP
  # =====================================================================
  for (t in seq_len(n_periods)) {

    growth      <- salary_growth_rate[t]
    cola_rate   <- pension_cola_rate[t]
    cur_date    <- .advance_period(ref_date, t - 1L, unit = period_unit)

    scenario_result <- simulate_scenario(
      contract_dt        = contract_dt,
      personnel_dt       = personnel_dt,
      salary_scale_dt    = salary_scale_dt,
      period_date        = cur_date,
      pensioner_register = pensioner_register,
      retirement_policy  = retirement_policy,
      exit_policy        = exit_policy,
      movement_policy    = movement_policy,
      hiring_policy      = hiring_policy,
      salary_growth_rate = growth,
      pension_cola_rate  = cola_rate,
      personnel_id_col   = personnel_id_col,
      contract_id_col    = contract_id_col,
      start_date_col     = start_date_col,
      end_date_col       = end_date_col,
      salary_col         = salary_col,
      contract_type_col  = contract_type_col,
      status_col         = status_col,
      age_col            = age_col,
      tenure_col         = tenure_col,
      period_fraction    = period_fraction
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
  effect_cols <- c("exit_savings", "non_ret_exit_savings", "promotion_effect",
                   "transfer_effect",
                   "hiring_effect", "inflation_effect")
  for (ec in effect_cols) {
    pct_col <- paste0(ec, "_pct_of_end_bill")
    summary_dt[, (pct_col) := data.table::fifelse(
      wage_bill_end > 0,
      get(ec) / wage_bill_end,
      NA_real_
    )]
  }

  # ------------------------------------------------------------------
  # Stamp scenario identity columns when scenario_name is supplied.
  # scenario_id and scenario_label carry the same value by default;
  # generate_scenario_matrix() may overwrite scenario_id with a UUID later.
  # is_baseline is always stamped so that multi-scenario rbindlists have a
  # consistent column even when only the baseline called simulate_horizon()
  # with is_baseline = TRUE.
  # ------------------------------------------------------------------
  if (!is.null(scenario_name)) {
    if (!is.character(scenario_name) || length(scenario_name) != 1L)
      stop("scenario_name must be a single character string or NULL.",
           call. = FALSE)
    summary_dt[, scenario_id    := scenario_name]
    summary_dt[, scenario_label := scenario_name]
  }
  summary_dt[, is_baseline := isTRUE(is_baseline)]

  # Move identity columns to the front for readability
  id_cols   <- intersect(c("scenario_id", "scenario_label", "is_baseline"),
                         names(summary_dt))
  other_cols <- setdiff(names(summary_dt), id_cols)
  data.table::setcolorder(summary_dt, c(id_cols, other_cols))

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
    metadata   = list(
      policy_args   = policy_args,
      scenario_name = scenario_name,
      is_baseline   = isTRUE(is_baseline)
    )
  )

  # Pensioner register is always included: it is needed for pension cost audits
  # and is lightweight (one row per ever-retired person, not per period).
  out$pensioner_register <- pensioner_register

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

# ---------------------------------------------------------------------------
# Helper: advance Date by n units (year / month / day)
# ---------------------------------------------------------------------------
.advance_period <- function(date, n = 1L, unit = "year") {
  switch(
    unit,
    year  = seq(date, by = "year",  length.out = n + 1L)[n + 1L],
    month = seq(date, by = "month", length.out = n + 1L)[n + 1L],
    day   = date + as.integer(n),
    stop(".advance_period: unknown unit '", unit, "'", call. = FALSE)
  )
}
