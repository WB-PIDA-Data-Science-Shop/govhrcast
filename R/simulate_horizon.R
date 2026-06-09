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
#'   movement step -- used to look up post-move salaries.
#' @param personnel_id_col Character.  Personnel ID column.  Default:
#'   \code{"personnel_id"}.
#' @param salary_col Character.  Salary column.  Default:
#'   \code{"gross_salary_lcu"}.
#'
#' @return Named list with one numeric scalar:
#'   \describe{
#'     \item{movement}{Net salary change across all movers
#'       (post-move minus pre-move, summed regardless of movement type).}
#'   }
#'   Zero when \code{movers_dt} is \code{NULL} or empty.
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
#' # list(movement = 1000)
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
# Pensioner rows are excluded -- their cost is tracked in pensioner_register.
# Used three times in simulate_scenario() for wage_bill_start, pre_cola, and
# wage_bill_end snapshots.  No roxygen: internal only, not exported.
.active_wage_bill <- function(contract_dt,
                              contract_type_col,
                              salary_col,
                              personnel_id_col) {
  contract_dt[
    !get(contract_type_col) %in% c("pensioner", "inactive") & !is.na(get(salary_col)),
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
#' 1. **Snapshot A** -- record \code{wage_bill_start} and \code{n_headcount_start}.
#' 2. **Retirement** -- remove retirees; accumulate \code{pensioner_register}.
#' 3. **Movements** -- apply promotions and transfers.
#' 4. **Hiring** -- fill vacancies.
#' 5. **Aging** -- increment \code{age} and \code{tenure_years}.
#' 6. **COLA** -- scale salary table and contract salaries.
#' 7. **Snapshot B** -- record \code{wage_bill_end} and \code{n_headcount_end}.
#'
#' @details
#' **Wage bill measurement**: \code{wage_bill_start} and \code{wage_bill_end}
#' are computed via an internal helper that sums \code{salary_col} over active
#' contract rows only -- rows with \code{contract_type_code = "pensioner"} and
#' rows with a missing salary are excluded.  The \code{pensioner_register} is a
#' separate audit ledger that tracks \code{pension_amount} (the pension formula
#' output) and is never folded into the wage-bill totals.
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
#'   for the first period -- an empty register is initialised internally.
#' @param retirement_policy List or \code{NULL}.  Canonical 3-slot policy passed
#'   to \code{simulate_retirement()}: \code{group_cols}, \code{policy_table},
#'   \code{defaults} (keys: \code{eligibility_type}, \code{pension_type},
#'   \code{min_age}, \code{min_tenure}, \code{accrual_rate},
#'   \code{ref_wage_col}, \code{max_years}, \code{replacement_cap}).
#'   Pass \code{NULL} to skip retirement.
#' @param exit_policy List or \code{NULL}.  Canonical 3-slot policy passed to
#'   \code{simulate_exits()}: \code{group_cols}, \code{policy_table},
#'   \code{defaults} (keys: \code{exit_rate}, \code{exit_strategy},
#'   \code{active_types}, \code{exited_type}).
#'   Pass \code{NULL} to model zero non-retirement attrition.
#' @param movement_policy List or \code{NULL}.  Canonical 3-slot policy passed
#'   to \code{simulate_promotions_transfers()}: \code{group_cols},
#'   \code{policy_table}, \code{defaults} (keys: \code{movement_rate},
#'   \code{movement_strategy}, \code{active_types}, \code{salary_update_rule}).
#'   Pass \code{NULL} to skip movements.
#' @param hiring_policy List or \code{NULL}.  Passed to
#'   \code{simulate_hiring()}.  Pass \code{NULL} to skip.
#' @param retirement_hazard_model A calibrated \code{hazard_model} object or
#'   \code{NULL} (default).  Forwarded to \code{\link{simulate_retirement}}.
#'   When non-\code{NULL}, retirement take-up is governed by
#'   \code{\link{predict_hazard}} instead of the 100 percent take-up assumption.
#' @param exit_hazard_model A calibrated \code{hazard_model} object or
#'   \code{NULL} (default).  Forwarded to \code{\link{simulate_exits}}.
#'   Activates hazard-mode exit selection when
#'   \code{exit_policy$defaults$exit_strategy = "hazard"}.
#' @param salary_growth_rate Numeric scalar.  COLA rate for this period.
#'   Default \code{0}.
#' @param pension_cola_rate Numeric scalar.  Annual COLA rate applied to
#'   \code{pensioner_register$pension_amount} this period.  Defaults to
#'   \code{salary_growth_rate} when \code{NULL}.  Default \code{0}.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_id_col Character.  Default \code{"contract_id"}.
#' @param birth_date_col Character.  Column holding date of birth.  Required
#'   when a hazard model uses age as a covariate.  Default \code{"birth_date"}.
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
#' @param hire_date_col Character or \code{NULL}.  Forwarded to
#'   \code{simulate_hiring()}.  When supplied, historical hiring rates are
#'   estimated from this person-level hire-date column rather than from panel
#'   first-appearance detection.  Only relevant when
#'   \code{hiring_policy$mode = "status_quo"}.  Default \code{NULL}.
#'
#' @return Named list:
#'   \describe{
#'     \item{summary}{One-row \code{data.table} with columns:
#'       \code{period_date}, \code{n_headcount_start}, \code{wage_bill_start},
#'       \code{n_exits}, \code{exit_savings}, \code{n_non_ret_exits},
#'       \code{non_ret_exit_savings}, \code{pension_cost_new},
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
                               pensioner_register      = NULL,
                               retirement_policy       = NULL,
                               exit_policy             = NULL,
                               movement_policy         = NULL,
                               hiring_policy           = NULL,
                               retirement_hazard_model = NULL,
                               exit_hazard_model       = NULL,
                               salary_growth_rate  = 0,
                               pension_cola_rate   = 0,
                               personnel_id_col    = "personnel_id",
                               contract_id_col     = "contract_id",
                               birth_date_col      = "birth_date",
                               start_date_col      = "start_date",
                               end_date_col        = "end_date",
                               salary_col          = "gross_salary_lcu",
                               contract_type_col   = "contract_type_code",
                               status_col          = "status",
                               age_col             = "age",
                               tenure_col          = "tenure_years",
                               period_fraction     = 1,
                               hire_date_col       = NULL) {

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
  # SNAPSHOT A  (active employees only -- pensioner rows excluded)
  # ------------------------------------------------------------------
  n_headcount_start <- data.table::uniqueN(
    contract_dt[!get(contract_type_col) %in% c("pensioner", "inactive")],
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
      contract_dt             = contract_dt,
      personnel_dt            = personnel_dt,
      policy_params           = retirement_policy,
      ref_date                = period_date,
      retirement_hazard_model = retirement_hazard_model,
      personnel_id_col        = personnel_id_col,
      contract_id_col         = contract_id_col,
      birth_date_col          = birth_date_col,
      start_date_col          = start_date_col,
      end_date_col            = end_date_col,
      salary_col              = salary_col,
      contract_type_col       = contract_type_col,
      status_col              = status_col,
      age_col                 = age_col,
      tenure_col              = tenure_col
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
      exit_hazard_model = exit_hazard_model,
      personnel_id_col  = personnel_id_col,
      birth_date_col    = birth_date_col,
      start_date_col    = start_date_col,
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
      # finer-grained scale (e.g. est_id x paygrade) into a hiring module that
      # joins only on est_id -- which would cause a cartesian join error.
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
      status_col        = status_col,
      hire_date_col     = hire_date_col
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
  # SNAPSHOT B  (active employees only -- pensioner rows excluded)
  # ------------------------------------------------------------------
  n_headcount_end <- data.table::uniqueN(
    contract_dt[!get(contract_type_col) %in% c("pensioner", "inactive")],
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
# simulate_horizon() -- multi-period loop
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
#' 1. **Retirement** -- retirees are identified and removed; their pre-retirement
#'    salaries become \code{exit_savings}.
#' 2. **Movements** -- promotions and transfers are applied; the net per-mover
#'    salary change is \code{promotion_effect} / \code{transfer_effect}.
#' 3. **Hiring** -- vacancies are filled; new-hire contract salaries sum to
#'    \code{hiring_effect}.
#' 4. **Aging** -- \code{age} and \code{tenure_years} incremented for all
#'    active personnel.
#' 5. **Inflation** -- \code{salary_scale_dt} and all contract salaries scaled
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
#' @param retirement_policy List or \code{NULL}. Canonical 3-slot policy passed
#'   to \code{simulate_retirement()}:
#'   \describe{
#'     \item{\code{group_cols}}{Character vector or \code{NULL}. Columns that
#'       define policy groups (e.g. \code{c("paygrade", "occupation_isconame")}).}
#'     \item{\code{policy_table}}{data.table or \code{NULL}. Per-group overrides
#'       keyed on \code{group_cols}.  \code{NULL} for scalar-only dispatch.}
#'     \item{\code{defaults}}{Named list. Fallback values for all groups.
#'       Keys: \code{eligibility_type} (\code{"age_only"},
#'       \code{"tenure_only"}, \code{"age_and_tenure"}),
#'       \code{pension_type} (\code{"db"}, \code{"dc"}, \code{"flat"},
#'       \code{"hybrid"}), \code{min_age}, \code{min_tenure},
#'       \code{accrual_rate}, \code{ref_wage_col}, \code{max_years},
#'       \code{replacement_cap}.}
#'   }
#'   Pass \code{NULL} to skip retirement entirely.
#' @param exit_policy List or \code{NULL}. Canonical 3-slot policy passed to
#'   \code{simulate_exits()}:
#'   \describe{
#'     \item{\code{group_cols}}{Character vector or \code{NULL}.}
#'     \item{\code{policy_table}}{data.table or \code{NULL}. Per-group exit
#'       rates keyed on \code{group_cols}.}
#'     \item{\code{defaults}}{Named list. Keys: \code{exit_rate} (required
#'       when \code{policy_table = NULL}), \code{exit_strategy}
#'       (\code{"random"} or a numeric column name), \code{active_types},
#'       \code{exited_type}.}
#'   }
#'   Pass \code{NULL} to model zero non-retirement attrition.
#' @param movement_policy List or \code{NULL}. Canonical 3-slot policy passed
#'   to \code{simulate_promotions_transfers()}:
#'   \describe{
#'     \item{\code{group_cols}}{Character vector or \code{NULL}. State-defining
#'       columns (e.g. \code{"paygrade"}).}
#'     \item{\code{policy_table}}{data.table or \code{NULL}. Pre-computed
#'       transition matrix (output of \code{estimate_movement_baseline()}).}
#'     \item{\code{defaults}}{Named list. Keys: \code{movement_rate} (required
#'       when \code{policy_table = NULL}), \code{movement_strategy}
#'       (\code{"random"}, \code{"tenure"}, \code{"reverse_tenure"},
#'       \code{"wage_based"}), \code{active_types}, \code{salary_update_rule}.}
#'   }
#'   Pass \code{NULL} to skip movements.
#' @param hiring_policy List or \code{NULL}. Policy parameters passed to
#'   \code{simulate_hiring()}. Must include \code{mode} (one of
#'   \code{"flow"}, \code{"stock"}, \code{"combined"}, \code{"status_quo"})
#'   and \code{salary_scale} (a data.table keyed on \code{group_cols}).
#'   For \code{mode = "flow"}: also \code{group_cols}, \code{replacement_rate}.
#'   For \code{mode = "stock"}: also \code{group_cols}, \code{stock_targets}.
#'   For \code{mode = "status_quo"}: also \code{group_cols} and optionally
#'   \code{rate_mult} (default 1).  The panel tables
#'   (\code{panel_contract_dt} and \code{panel_personnel_dt}) are injected
#'   automatically -- you do \emph{not} need to supply them.
#'   Pass \code{NULL} to skip hiring in all periods.
#' @param retirement_hazard_options Named list controlling hazard-model
#'   retirement.  Three recognised slots:
#'   \describe{
#'     \item{\code{use_hazard_model}}{Logical.  \code{TRUE} to fit a GLM on
#'       \code{contract_dt} / \code{personnel_dt} history and use
#'       \code{\link{predict_hazard}} for take-up.  Default \code{FALSE}.}
#'     \item{\code{covariates}}{Character vector of covariate column names
#'       passed to \code{\link{fit_hazard_model}} when
#'       \code{use_hazard_model = TRUE}.  Default \code{NULL} (uses
#'       \code{age} and \code{tenure_years}).}
#'     \item{\code{custom_model}}{A pre-fitted \code{hazard_model} object
#'       returned by \code{\link{fit_hazard_model}} /
#'       \code{\link{select_hazard_threshold}}.  When supplied,
#'       \code{use_hazard_model} is ignored and this model is used directly.}
#'   }
#'   Default: \code{list(use_hazard_model = FALSE, covariates = NULL, custom_model = NULL)}.
#' @param exit_hazard_options Named list controlling hazard-model voluntary
#'   exits.  Same three slots as \code{retirement_hazard_options}:
#'   \code{use_hazard_model}, \code{covariates}, \code{custom_model}.
#'   Default: \code{list(use_hazard_model = FALSE, covariates = NULL, custom_model = NULL)}.
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
#' @param hire_date_col Character or \code{NULL}.  Name of a person-level column
#'   in \code{personnel_dt} that holds the true administrative hire date
#'   (e.g. \code{"first_employment_date"}).  When supplied and
#'   \code{hiring_policy$mode = "status_quo"}, the function estimates historical
#'   hiring rates from this column instead of panel first-appearance detection,
#'   avoiding left-censoring bias.  Passed unchanged to every call of
#'   \code{simulate_scenario()}.  Default \code{NULL}.
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
#'       \code{n_exits}, \code{exit_savings}, \code{n_non_ret_exits},
#'       \code{non_ret_exit_savings}, \code{pension_cost_new},
#'       \code{pension_cost_total}, \code{n_promotions}, \code{n_transfers},
#'       \code{promotion_effect}, \code{transfer_effect}, \code{n_hires},
#'       \code{hiring_effect}, \code{inflation_effect},
#'       \code{n_headcount_end}, \code{wage_bill_end}, plus
#'       \code{exit_savings_pct_of_end_bill},
#'       \code{non_ret_exit_savings_pct_of_end_bill},
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
#'     \item{\code{$pensioner_register}}{data.table. Accumulated ledger of all
#'       retirees across all simulated periods, with columns \code{personnel_id},
#'       \code{pension_amount}, and \code{period_date}.  Always present
#'       (zero-row table when no retirements occurred).}
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
#'     group_cols   = NULL,
#'     policy_table = NULL,
#'     defaults = list(
#'       eligibility_type = "age_and_tenure",
#'       pension_type     = "db",
#'       min_age          = 60,
#'       min_tenure       = 20,
#'       accrual_rate     = 0.02,
#'       ref_wage_col     = "gross_salary_lcu",
#'       max_years        = 35,
#'       replacement_cap  = 0.80
#'     )
#'   ),
#'   exit_policy = list(
#'     group_cols   = NULL,
#'     policy_table = NULL,
#'     defaults = list(
#'       exit_rate     = 0.05,
#'       exit_strategy = "random",
#'       active_types  = c("permanent", "fterm"),
#'       exited_type   = "inactive"
#'     )
#'   ),
#'   movement_policy = list(
#'     group_cols   = "est_id",
#'     policy_table = NULL,
#'     defaults = list(
#'       movement_rate      = 0.05,
#'       movement_strategy  = "tenure",
#'       active_types       = "permanent",
#'       salary_update_rule = "scale"
#'     )
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
                             retirement_policy  = list(
                               group_cols   = NULL,
                               policy_table = NULL,
                               defaults = list(
                                 eligibility_type = "age_and_tenure",
                                 pension_type     = "db",
                                 min_age          = 60,
                                 min_tenure       = 10,
                                 accrual_rate     = 0.02,
                                 ref_wage_col     = "gross_salary_lcu",
                                 max_years        = 35,
                                 replacement_cap  = 0.80
                               )
                             ),
                             exit_policy        = NULL,
                             movement_policy    = NULL,
                             hiring_policy      = NULL,
                             retirement_hazard_options = list(
                               use_hazard_model = FALSE,
                               covariates       = NULL,
                               custom_model     = NULL
                             ),
                             exit_hazard_options = list(
                               use_hazard_model = FALSE,
                               covariates       = NULL,
                               custom_model     = NULL
                             ),
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
                             is_baseline        = FALSE,
                             hire_date_col      = NULL) {

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

  # ------------------------------------------------------------------
  # Unpack hazard option lists into internal scalar variables.
  # ------------------------------------------------------------------
  .ret_haz_use_   <- isTRUE(retirement_hazard_options$use_hazard_model)
  .ret_haz_covs_  <- retirement_hazard_options$covariates
  retirement_hazard_model <- retirement_hazard_options$custom_model

  .exit_haz_use_  <- isTRUE(exit_hazard_options$use_hazard_model)
  .exit_haz_covs_ <- exit_hazard_options$covariates
  exit_hazard_model <- exit_hazard_options$custom_model

  ### check if we are status quo modelling
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
    ## check if we have a panel dataset based on reference date
    has_panel <- ref_date_col_name %in% names(contract_dt) &&
                 data.table::uniqueN(contract_dt[[ref_date_col_name]]) >= 2L
    ### if we have a panel compute the movement baseline rates for transfers and
    ### promotions
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

  # For exit, pre-estimate historical rates from the full panel BEFORE stripping
  # ref_date, so simulate_exits() receives a ready-made policy_table.
  # Skipped when .exit_haz_use_ = TRUE -- the hazard model replaces rate-based selection.
  if (!.exit_haz_use_ && !is.null(exit_policy) && is.null(exit_policy$policy_table)) {
    ref_date_col_name <- "ref_date"
    has_panel <- ref_date_col_name %in% names(contract_dt) &&
                 data.table::uniqueN(contract_dt[[ref_date_col_name]]) >= 2L
    if (has_panel) {
      exit_policy$policy_table <- tryCatch(
        estimate_historical_exit_rates(
          panel_contract_dt  = contract_dt,
          panel_personnel_dt = personnel_dt,
          group_cols         = exit_policy$group_cols,
          personnel_id_col   = personnel_id_col,
          ref_date_col       = ref_date_col_name,
          start_date_col     = start_date_col,
          end_date_col       = end_date_col,
          contract_type_col  = contract_type_col,
          status_col         = status_col
        ),
        error = function(e) {
          warning("simulate_horizon: exit rate estimation failed \u2014 ",
                  conditionMessage(e), ". Falling back to defaults$exit_rate.",
                  call. = FALSE)
          NULL
        }
      )
    } else if (is.null(exit_policy$defaults$exit_rate)) {
      warning("simulate_horizon: single-snapshot data and no exit_rate in ",
              "exit_policy$defaults. Non-retirement exits will be skipped.",
              call. = FALSE)
      exit_policy <- NULL
    }
  }

  # If data contains a ref_date panel column, subset to the starting snapshot
  # before stripping so that sub-modules see a single-period snapshot.
  # Capture the full panel now for optional hazard model fitting below.

  start_ref <- ref_date   # capture before any column could shadow it
  .ref_date_col_name_ <- "ref_date"
  .has_panel_ <- .ref_date_col_name_ %in% names(contract_dt) &&
                 data.table::uniqueN(contract_dt[[.ref_date_col_name_]]) >= 2L
  .full_panel_c_ <- if (.has_panel_) data.table::copy(contract_dt)  else NULL
  .full_panel_p_ <- if (.has_panel_) data.table::copy(personnel_dt) else NULL

  if ("ref_date" %in% names(contract_dt)) {
    contract_dt <- contract_dt[get("ref_date") == start_ref]
    contract_dt[, ref_date := NULL]
  }
  if ("ref_date" %in% names(personnel_dt)) {
    personnel_dt <- personnel_dt[get("ref_date") == start_ref]
    personnel_dt[, ref_date := NULL]
  }

  # ------------------------------------------------------------------
  # Auto-fit hazard models from the panel (once, before the loop).
  # These blocks run only when the user opts in via use_*_hazard = TRUE
  # and has not already supplied a pre-fitted model object.
  # The panel is captured above before ref_date stripping so the full
  # history is available for training.
  # ------------------------------------------------------------------

  # Retirement hazard: eligibility is always included as a covariate
  # (via retirement_policy passed to project_retirement_hazard), so the
  # model learns the jump in retirement probability at the eligibility
  # threshold.  The hard eligibility gate inside simulate_retirement()
  # remains as a safety net to prevent policy violations.
  if (.ret_haz_use_ && is.null(retirement_hazard_model)) {
    if (!is.null(.full_panel_c_) && !is.null(.full_panel_p_)) {
      .hz_ret_result_ <- tryCatch(
        project_retirement_hazard(
          panel_contract_dt  = .full_panel_c_,
          panel_personnel_dt = .full_panel_p_,
          sim_contract_dt    = contract_dt,
          sim_personnel_dt   = personnel_dt,
          use_hazard_model   = TRUE,
          retirement_policy  = retirement_policy,
          extra_covariates   = .ret_haz_covs_,
          ref_date           = start_ref
        ),
        error = function(e) {
          warning("simulate_horizon: retirement hazard fitting failed \u2014 ",
                  conditionMessage(e), ". Falling back to eligibility rule.",
                  call. = FALSE)
          NULL
        }
      )
      if (!is.null(.hz_ret_result_))
        retirement_hazard_model <- attr(.hz_ret_result_, "hazard_model")
    } else {
      warning("simulate_horizon: use_retirement_hazard = TRUE but no panel ",
              "data available for training. Falling back to eligibility rule.",
              call. = FALSE)
    }
  }

  # Exit hazard: fitted from the panel; exit_strategy is set to "hazard" on
  # exit_policy$defaults so simulate_exits() routes to predict_hazard().
  if (.exit_haz_use_ && is.null(exit_hazard_model)) {
    if (!is.null(.full_panel_c_) && !is.null(.full_panel_p_)) {
      .hz_exit_result_ <- tryCatch(
        project_exit_hazard(
          panel_contract_dt  = .full_panel_c_,
          panel_personnel_dt = .full_panel_p_,
          sim_contract_dt    = contract_dt,
          sim_personnel_dt   = personnel_dt,
          use_hazard_model   = TRUE,
          active_types       = exit_policy$defaults$active_types,
          extra_covariates   = .exit_haz_covs_,
          ref_date           = start_ref
        ),
        error = function(e) {
          warning("simulate_horizon: exit hazard fitting failed \u2014 ",
                  conditionMessage(e), ". Falling back to rate-based exits.",
                  call. = FALSE)
          NULL
        }
      )
      if (!is.null(.hz_exit_result_)) {
        exit_hazard_model <- attr(.hz_exit_result_, "hazard_model")
        # Signal simulate_exits() to use the hazard path
        if (!is.null(exit_policy))
          exit_policy$defaults$exit_strategy <- "hazard"
      }
    } else {
      warning("simulate_horizon: use_exit_hazard = TRUE but no panel data ",
              "available for training. Falling back to rate-based exits.",
              call. = FALSE)
    }
  }

  # Resolve pensioner type label before any filtering uses it.
  .pensioner_type_val_ <- if (!is.null(retirement_policy) &&
                               !is.null(retirement_policy$pensioner_type_value))
    retirement_policy$pensioner_type_value else "pensioner"

  # Retain only salary-bearing contracts in the starting snapshot.
  # Rule: keep a row if it is a pensioner contract (needed to seed the
  # pensioner register) OR if it has a non-NA salary (the government is paying,
  # regardless of contract_type -- this correctly includes inactive staff on
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

  # Initialise pensioner register with expanded schema 
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
    .ref_date_seed_ <- ref_date  # alias \u2014 avoid data.table column shadowing
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

  # Remove pre-existing pensioner contracts from the working snapshot now that
  # they are captured in the register.  Keeping them in contract_dt with
  # salary = 0 would silently inflate row counts and make contract_type
  # filtering harder in every downstream module.  Personnel rows are removed
  # by the same key so that headcount counts stay consistent.
  if (nrow(existing_pensioners) > 0L) {
    .pensioner_ids_ <- unique(existing_pensioners[[personnel_id_col]])
    contract_dt  <- contract_dt[!get(personnel_id_col) %in% .pensioner_ids_]
    personnel_dt <- personnel_dt[!get(personnel_id_col) %in% .pensioner_ids_]
  }

  # ====================================================================
  # P2/P3. Auto-compute age and tenure from source columns (Phase 1b)
  # ====================================================================
  # Age: overwrite age_col using birth_date_col, if both are available.
  # This is forward-compatible with Phase 2b where the prologue will also
  # pre-compute tenure once and increment it per period rather than
  # recomputing inside identify_eligibility().
  # Age: overwrite age_col using birth_date_col, if both are available.
  # Age: compute from birth_date_col only when age_col is not already
  # populated in personnel_dt (all NA or column absent).
  .age_missing_ <- is.null(age_col) ||
    !(age_col %in% names(personnel_dt)) ||
    all(is.na(personnel_dt[[age_col]]))

  if (.age_missing_ && !is.null(birth_date_col) &&
      !is.null(age_col) &&
      birth_date_col %in% names(personnel_dt)) {
    .ref_date_p1b_ <- ref_date
    personnel_dt[, (age_col) := as.numeric(
      difftime(.ref_date_p1b_, get(birth_date_col), units = "days")
    ) / 365.25]
  }

  # Tenure: compute from contract history only when tenure_col is not already
  # populated in personnel_dt (all NA or column absent).
  .tenure_missing_ <- is.null(tenure_col) ||
                      !(tenure_col %in% names(personnel_dt)) ||
                      all(is.na(personnel_dt[[tenure_col]]))

  if (!is.null(tenure_col) && .tenure_missing_) {    
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

  period_rows <- vector("list", n_periods)

  # compute historical hiring rates if status quo hiring is selected
  # so that simulate_hiring() under status quo mode recieves a 
  # pre-computed hiring rate
  if (!is.null(hiring_policy) && identical(hiring_policy$mode, "status_quo")){

    if (.has_panel_){

      hiring_policy$squorate_dt <- tryCatch(
        estimate_historical_hiring_rates(
          panel_contract_dt  = hiring_policy$panel_contract_dt,
          panel_personnel_dt = hiring_policy$panel_personnel_dt,
          group_cols         = hiring_policy$group_cols,
          hire_date_col      = hire_date_col,
          personnel_id_col   = personnel_id_col,
          start_date_col     = start_date_col,
          end_date_col       = end_date_col,
          contract_type_col  = contract_type_col,
          status_col         = status_col
        ),
        error = function(e) {
          warning("simulate_horizon: historical rate estimation failed ",
                  conditionMessage(e), ". No hiring will be simulated",
                  call. = FALSE)
          hiring_policy <- NULL
        }
      )
    }


  }

  # ====================================================================
  # PERIOD LOOP
  # =====================================================================
  for (t in seq_len(n_periods)) {

    growth      <- salary_growth_rate[t]
    cola_rate   <- pension_cola_rate[t]
    cur_date    <- .advance_period(ref_date, t - 1L, unit = period_unit)

    scenario_result <- simulate_scenario(
      contract_dt             = contract_dt,
      personnel_dt            = personnel_dt,
      salary_scale_dt         = salary_scale_dt,
      period_date             = cur_date,
      pensioner_register      = pensioner_register,
      retirement_policy       = retirement_policy,
      exit_policy             = exit_policy,
      movement_policy         = movement_policy,
      hiring_policy           = hiring_policy,
      retirement_hazard_model = retirement_hazard_model,
      exit_hazard_model       = exit_hazard_model,
      salary_growth_rate      = growth,
      pension_cola_rate       = cola_rate,
      personnel_id_col        = personnel_id_col,
      contract_id_col         = contract_id_col,
      birth_date_col          = birth_date_col,
      start_date_col     = start_date_col,
      end_date_col       = end_date_col,
      salary_col         = salary_col,
      contract_type_col  = contract_type_col,
      status_col         = status_col,
      age_col            = age_col,
      tenure_col         = tenure_col,
      period_fraction    = period_fraction,
      hire_date_col      = hire_date_col
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
