
#' Compute Exit Effect
#'
#' Computes the total salary removed from the wage bill due to employee exits
#' (e.g., retirements) by summing the values in the specified salary column.
#'
#' @param retirees_dt A data frame or \code{data.table} containing the employees
#'   who exited the workforce. If \code{NULL} or empty, the function returns
#'   \code{0}.
#' @param salary_col A character string giving the name of the salary variable
#'   in \code{retirees_dt}. Defaults to \code{"gross_salary_lcu"}.
#'
#' @return A numeric scalar equal to the sum of \code{salary_col} across all
#'   rows of \code{retirees_dt}. Returns \code{0} if \code{retirees_dt} is
#'   \code{NULL}, has zero rows, or if \code{salary_col} does not exist.
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
#' 
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
#' state objects.  \code{\link{simulate_horizon}} calls this once per year;
#' most users should use \code{simulate_horizon()} instead, which also
#' estimates status-quo policies from historical data.  Operations are
#' applied in this order:
#'
#' 1. **Snapshot A** -- record \code{wage_bill_start} and \code{n_headcount_start}.
#' 2. **Retirement** -- remove retirees; accumulate \code{pensioner_register}.
#' 3. **Other exits** -- resignations, dismissals, contracts not renewed.
#' 4. **Movements** -- apply promotions and transfers.
#' 5. **Hiring** -- fill vacancies.
#' 6. **Allowances** -- assign allowance amounts to active contracts.
#' 7. **Aging** -- increment \code{age_col} and \code{tenure_col}.
#' 8. **COLA** -- scale salary table, contract salaries and pensions.
#' 9. **Snapshot B** -- record \code{wage_bill_end} and \code{n_headcount_end}.
#'
#' @details
#' **Wage bill measurement**: \code{wage_bill_start} and \code{wage_bill_end}
#' are computed via an internal helper that sums \code{salary_col} over active
#' contract rows only -- rows with \code{contract_type = "pensioner"} and
#' rows with a missing salary are excluded.  The \code{pensioner_register} is a
#' separate audit ledger that tracks \code{pension_amount} (the pension formula
#' output) and is never folded into the wage-bill totals.
#'
#' @import data.table
#'
#' @param contract_dt data.table.  Single-snapshot contract microdata.
#' @param personnel_dt data.table.  Single-snapshot personnel microdata.
#' @param salary_scale_dt data.table.  Pay table used for movement and hiring
#'   salary assignment.  Not modified: the pay table grown by
#'   \code{salary_growth_rate} is returned as \code{$salary_scale_dt}.  None of
#'   the tables passed in (\code{contract_dt}, \code{personnel_dt},
#'   \code{salary_scale_dt}, \code{pensioner_register}) is changed; the updated
#'   versions are returned.
#' @param period_date Date.  The reference date for this simulation period.
#'   Stored in \code{pensioner_register} for cohort auditing and used as the
#'   \code{period_date} column in the returned summary row.
#' @param pensioner_register data.table or \code{NULL}.  Accumulated pensioner
#'   ledger from prior periods.  Must contain columns \code{personnel_id},
#'   \code{pension_amount}, \code{period_date}.  Pass \code{NULL} (default)
#'   for the first period -- an empty register is initialised internally.
#' @param retirement_policy,exit_policy,movement_policy,hiring_policy,allowance_policy
#'   Lists, or \code{NULL} to switch that module off (the default here for
#'   all five).  Build them with the same helper functions as
#'   \code{\link{simulate_horizon}} -- see the table in its Details:
#'   \code{design_retirement_policy_*()} (\code{\link{retirement_policy_helpers}}),
#'   \code{design_exit_policy_*()} (\code{\link{exit_policy_helpers}}),
#'   \code{design_movement_policy_*()} (\code{\link{movement_policy_helpers}}),
#'   \code{design_hiring_policy_*()} (\code{\link{hiring_policy_helpers}}) and
#'   \code{design_allowance_policy_*()} (\code{\link{allowance_policy_helpers}}).
#'
#'   \strong{This function never estimates historical rates.}  A status-quo
#'   policy must already have its rates filled in: pass a retirement,
#'   exit or movement policy whose \code{policy_table} holds estimated
#'   rates (e.g. from \code{\link{estimate_retirement_hazard_rates}},
#'   \code{\link{estimate_historical_exit_rates}} or
#'   \code{\link{movement_policy_template}}), or a status-quo hiring policy
#'   with \code{squorate_dt} filled in (from
#'   \code{\link{estimate_historical_hiring_rates}}).
#'   \code{simulate_horizon()} does all of this for you.  When allowances are modelled, \code{salary_col} should be base
#'   pay, not a gross figure that already includes allowances -- see
#'   \code{\link{simulate_allowances}}.
#' @param allowance_dt data.table or \code{NULL}.  Accumulated allowance
#'   ledger from prior periods within \emph{this same} \code{simulate_scenario()}
#'   chain.  \code{NULL} (default) starts a fresh ledger.  Unlike
#'   \code{pensioner_register}, \code{simulate_horizon()} does not accept this
#'   as an input -- it is purely internal there.
#' @param salary_growth_rate Numeric scalar.  COLA rate for this period.
#'   Default \code{0}.
#' @param pension_cola_rate Numeric scalar.  Annual COLA rate applied to
#'   \code{pensioner_register$pension_amount} this period.  Defaults to
#'   \code{salary_growth_rate} when \code{NULL}.  Default \code{0}.
#' @param allowance_growth_rate Numeric scalar.  COLA-style growth rate
#'   applied to \code{allowance_policy$policy_table}'s \code{amount} column
#'   this period (via \code{apply_salary_scale_adjustment()}), so next
#'   period's assignment reflects the grown scale.  Default \code{0}.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_id_col Character.  Default \code{"contract_id"}.
#' @param birth_date_col Character.  Column holding date of birth.
#'   Default \code{"birth_date"}.
#' @param start_date_col Character.  Default \code{"start_date"}.
#' @param end_date_col Character.  Default \code{"end_date"}.
#' @param salary_col Character.  Default \code{"gross_salary_lcu"}.
#' @param contract_type_col Character.  Default \code{"contract_type"}.
#' @param status_col Character.  Default \code{"employment_status"}.
#' @param age_col Character or \code{NULL}.  Age column to increment.
#'   Default \code{"age"}.
#' @param tenure_col Character or \code{NULL}.  Tenure (years of service)
#'   column to increment.  Default \code{"personnel_tenure"}, the \code{govhr}
#'   dictionary name.
#' @param period_fraction Numeric.  Fraction of a year each period represents
#'   (1 for annual, 1/12 for monthly, 1/365.25 for daily).  Used to increment
#'   \code{age_col} and \code{tenure_col} by the correct amount.  Default 1.
#' @param hire_date_col Character or \code{NULL}.  Forwarded to
#'   \code{simulate_hiring()}.  When supplied, historical hiring rates are
#'   estimated from this person-level hire-date column rather than from panel
#'   first-appearance detection.  Only relevant when
#'   \code{hiring_policy$mode = "status_quo"}.  Default \code{NULL}.
#' @param seed Integer or \code{NULL}.  When supplied, calls
#'   \code{set.seed(seed)} before any random selection this call performs
#'   (e.g. \code{exit_strategy}/\code{retire_strategy} \code{"random"}
#'   draws), making a standalone \code{simulate_scenario()} call
#'   reproducible.  Default \code{NULL} (no seeding).  When calling this
#'   function repeatedly from \code{\link{simulate_horizon}}'s own period
#'   loop, leave this \code{NULL} and pass \code{seed} to
#'   \code{simulate_horizon()} instead -- seeding once per period would
#'   create correlated draws across periods rather than a continuous RNG
#'   stream.  Your own random-number state is restored when the function
#'   returns, so seeding a run does not change the random draws in code you
#'   run afterwards.
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
#'       \code{n_headcount_end}, \code{wage_bill_end}, \code{allowance_cost_end}
#'       (\code{0} when \code{allowance_policy} is \code{NULL}).}
#'     \item{contract_dt}{Updated contract snapshot.}
#'     \item{personnel_dt}{Updated personnel snapshot.}
#'     \item{salary_scale_dt}{Updated pay table (post-COLA).}
#'     \item{pensioner_register}{Accumulated pensioner ledger including new
#'       retirees from this period.}
#'     \item{allowance_dt}{Accumulated allowance ledger including this
#'       period's rows, or unchanged/\code{NULL} when \code{allowance_policy}
#'       is \code{NULL}.}
#'     \item{allowance_policy}{\code{allowance_policy} as supplied, with
#'       \code{policy_table} grown by \code{allowance_growth_rate} -- pass
#'       this back in as next period's \code{allowance_policy} to carry the
#'       growth forward (\code{simulate_horizon()} does this automatically).}
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
#'     contract_type = "permanent",
#'     gross_salary_lcu   = 50000
#'   ),
#'   personnel_dt = data.table(
#'     personnel_id = "P1",
#'     birth_date   = as.Date("1975-01-01"),
#'     employment_status = "active",
#'     age          = 45,
#'     personnel_tenure = 14
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
                               allowance_policy        = NULL,
                               allowance_dt            = NULL,
                               salary_growth_rate  = 0,
                               pension_cola_rate   = 0,
                               allowance_growth_rate = 0,
                               personnel_id_col    = "personnel_id",
                               contract_id_col     = "contract_id",
                               birth_date_col      = "birth_date",
                               start_date_col      = "start_date",
                               end_date_col        = "end_date",
                               salary_col          = "gross_salary_lcu",
                               contract_type_col   = "contract_type",
                               status_col         = "employment_status",
                               age_col             = "age",
                               tenure_col          = "personnel_tenure",
                               period_fraction     = 1,
                               hire_date_col       = NULL,
                               seed                = NULL) {

  # ------------------------------------------------------------------
  # 0. Initialise / validate
  # ------------------------------------------------------------------
  # Seed this run only; the caller's random stream is restored on exit
  if (!is.null(seed)) {
    rng_state <- .save_rng_state()
    on.exit(.restore_rng_state(rng_state), add = TRUE)
    set.seed(seed)
  }

  period_date <- validate_date_format(period_date, "period_date")

  # Work on copies: aging, COLA and the pension COLA below update these tables
  # in place (data.table's :=), and the caller's own tables must come back
  # unchanged. setDT(copy(x)) gives a data.table copy of a data.table or a
  # data.frame alike.
  contract_dt     <- data.table::setDT(data.table::copy(contract_dt))
  personnel_dt    <- data.table::setDT(data.table::copy(personnel_dt))
  salary_scale_dt <- data.table::setDT(data.table::copy(salary_scale_dt))
  if (!is.null(pensioner_register))
    pensioner_register <- data.table::setDT(data.table::copy(pensioner_register))

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
  scale_salary_pat   <- "salary|wage|pay|compensation|gross|base"
  scale_salary_cands <- grep(scale_salary_pat, names(salary_scale_dt),
                             value = TRUE, ignore.case = TRUE)
  # Only keep numeric candidates to avoid matching non-salary columns (e.g. "paygrade")
  scale_salary_cands <- scale_salary_cands[
    vapply(salary_scale_dt[, scale_salary_cands, with = FALSE], is.numeric, logical(1))
  ]
  if (length(scale_salary_cands) == 0L)
    stop("Could not detect a numeric salary column in salary_scale_dt.", call. = FALSE)
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

    # simulate_promotions_transfers()'s movement mechanism is generic -- it
    # has no inherent concept of "promotion" vs "transfer", it just moves
    # people between whatever states movement_policy$group_cols defines
    # (paygrade change, est_id change, or both at once). Which real-world
    # concept that represents is a fact about how THIS policy is configured,
    # not something derivable from the data, so the caller states it via
    # movement_policy$defaults$movement_type. Defaults to "promotion" to
    # match this package's prior (hardcoded) behavior for existing callers.
    movement_type <- movement_policy$defaults$movement_type %||% "promotion"

    if (!is.null(mov_result$summary)) {
      n_movers_val <- mov_result$summary$n_movers
      n_movers     <- as.integer(if (!is.null(n_movers_val)) n_movers_val else 0L)
      if (identical(movement_type, "transfer")) {
        n_transfers <- n_movers
      } else {
        n_promotions <- n_movers
      }
    }

    mov_effects <- compute_movement_effect(
      movers_dt         = movers_dt,
      contract_dt_after = contract_dt,
      personnel_id_col  = personnel_id_col,
      salary_col        = salary_col
    )
    if (identical(movement_type, "transfer")) {
      transfer_effect <- mov_effects$movement
    } else {
      promotion_effect <- mov_effects$movement
    }
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
  # STEP 5: ALLOWANCES
  # ------------------------------------------------------------------
  # Runs after hiring/movements finalise group membership (est_id, paygrade,
  # etc.) for the period, and before aging/COLA. contract_dt/personnel_dt are
  # never mutated here -- allowance costs live entirely in the allowance_dt
  # ledger, the same way pensioner_register tracks pension costs outside the
  # wage bill.
  allowance_cost_end <- 0

  if (!is.null(allowance_policy)) {
    alw_result <- simulate_allowances(
      contract_dt   = contract_dt,
      personnel_dt  = personnel_dt,
      allowance_dt  = allowance_dt,
      policy_params = allowance_policy,
      ref_date      = period_date,
      contract_type_col = contract_type_col,
      personnel_id_col  = personnel_id_col,
      contract_id_col   = contract_id_col
    )
    allowance_dt       <- alw_result$allowance_dt
    allowance_cost_end <- alw_result$summary$allowance_cost_total
  }

  # ------------------------------------------------------------------
  # STEP 6: AGING
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
  # STEP 7: COLA
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

  # Grow the allowance scale itself (not individual ledger rows) so that next
  # period's simulate_allowances() call resolves against the COLA-adjusted
  # amounts. A contract whose group_cols don't change between periods will
  # see its allowance grow at exactly this rate -- re-resolving from a scale
  # grown by (1+g) each period is arithmetically identical to growing a
  # sticky per-contract value by (1+g), while a mover still correctly picks
  # up their new group's (already-grown) amount instead of a stale one.
  if (!is.null(allowance_policy) && !is.null(allowance_policy$policy_table) &&
      allowance_growth_rate != 0) {
    allowance_policy$policy_table <- apply_salary_scale_adjustment(
      allowance_policy$policy_table,
      adjustment = 1 + allowance_growth_rate,
      salary_col = "amount"
    )
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
    wage_bill_end            = wage_bill_end,
    allowance_cost_end       = allowance_cost_end
  )

  list(
    summary            = summary_row,
    contract_dt        = contract_dt,
    personnel_dt       = personnel_dt,
    salary_scale_dt    = salary_scale_dt,
    pensioner_register = pensioner_register,
    allowance_dt       = allowance_dt,
    allowance_policy   = allowance_policy
  )
}


# ---------------------------------------------------------------------------
# Minimal NULL-coalescing helper (not exported)
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b


# ===========================================================================
# simulate_horizon() -- multi-period loop
# ===========================================================================

#' Project a Public-Sector Workforce and Its Wage Bill Over Several Years
#'
#' @description
#' The main entry point of govhrcast.  Starting from a snapshot of HRMIS
#' microdata, \code{simulate_horizon()} moves the workforce forward one year
#' at a time for \code{n_periods} years.  Each year it applies five
#' policies -- retirement, other exits, promotions/transfers, hiring and
#' allowances -- then ages staff and applies salary inflation.  It returns
#' one row per year that breaks the change in the wage bill down into what
#' caused it: savings from people leaving, the cost of promotions and
#' transfers, the cost of new hires, and inflation.
#'
#' Each policy is a small list that you build with a helper function, such
#' as \code{\link{design_exit_policy_status_quo}}.  For every module there
#' is a \strong{status-quo} helper that reproduces recent history
#' (estimated from your data), plus helpers for \strong{reforms} that let
#' you set the rules or rates yourself.  The usual workflow is:
#' \enumerate{
#'   \item Run a status-quo baseline, with every module using its
#'     status-quo helper.
#'   \item Run one or more reform scenarios, changing only the policies the
#'     reform affects.
#'   \item Stack the \code{$comparison} tables with
#'     \code{data.table::rbindlist()} and compare them.
#' }
#'
#' @details
#' ## Choosing a policy for each module
#'
#' Pass \code{NULL} for any module to switch it off: nobody leaves, moves,
#' is hired or receives modelled allowances through that module.  Only
#' retirement is on by default.
#'
#' | Module | Argument | Status quo (from history) | Reform / your own assumptions |
#' |:-------|:---------|:--------------------------|:------------------------------|
#' | Retirement | \code{retirement_policy} | \code{\link{design_retirement_policy_status_quo}} (default) | \code{\link{design_retirement_policy_age}}, \code{\link{design_retirement_policy_tenure}}, \code{\link{design_retirement_policy_age_tenure}}, or \code{\link{design_retirement_policy}} for any \code{eligibility_*()} + \code{pension_*()} combination |
#' | Other exits | \code{exit_policy} | \code{\link{design_exit_policy_status_quo}} | \code{\link{design_exit_policy_flat}}, \code{\link{design_exit_policy_adjustment}} |
#' | Promotions / transfers | \code{movement_policy} | \code{\link{design_movement_policy_status_quo}} | \code{\link{design_movement_policy_adjustment}} (start from \code{\link{movement_policy_template}}) |
#' | Hiring | \code{hiring_policy} | \code{\link{design_hiring_policy_status_quo}} | \code{\link{design_hiring_policy_flow}}, \code{\link{design_hiring_policy_stock}} |
#' | Allowances | \code{allowance_policy} | \code{\link{design_allowance_policy_status_quo}} | \code{\link{design_allowance_policy_adjustment}} |
#'
#' Each helper returns a plain list, so you can inspect it or edit it before
#' passing it in.  Writing the list by hand also works; each module's own
#' help page (e.g. \code{\link{simulate_exits}}) describes its exact shape.
#'
#' ## Data requirements
#'
#' \code{contract_dt} and \code{personnel_dt} should be \strong{panels}:
#' several yearly snapshots stacked together, each row stamped with its
#' snapshot date in a \code{ref_date} column (the \code{govhr} harmonized
#' format, as in \code{\link{bra_hrmis_contract}}).  The projection starts
#' from the snapshot whose date equals \code{ref_date}.  The earlier
#' snapshots are the history that status-quo policies are estimated from.
#' At least two snapshots are needed for any status-quo policy, including
#' the default retirement policy -- without history there are no trends
#' to project.
#'
#' ## What happens before the first year
#'
#' \enumerate{
#'   \item Every status-quo policy is estimated from the full panel:
#'     retirement rates by single year of age
#'     (\code{\link{estimate_retirement_hazard_rates}}), exit rates
#'     (\code{\link{estimate_historical_exit_rates}}), movement rates
#'     between groups, and hiring rates
#'     (\code{\link{estimate_historical_hiring_rates}}).  If the history
#'     shows no retirements, or no moves, the status quo is that nobody
#'     retires or moves (a message says so).
#'   \item The data are cut to the starting snapshot at \code{ref_date}.
#'   \item Age is calculated from \code{birth_date} when no age column is
#'     filled in, and tenure is calculated from contract history when no
#'     tenure column is filled in.
#' }
#'
#' ## What happens each year
#'
#' \enumerate{
#'   \item \strong{Retirement} -- eligible staff retire; their pensions are
#'     recorded in \code{$pensioner_register}.
#'   \item \strong{Other exits} -- resignations, dismissals and contracts
#'     not renewed.
#'   \item \strong{Promotions / transfers} -- staff move between groups
#'     (e.g. paygrades or establishments).
#'   \item \strong{Hiring} -- new contracts are created.
#'   \item \strong{Allowances} -- allowance amounts are assigned to every
#'     active contract.
#'   \item \strong{Aging} -- age and tenure increase by one year.
#'   \item \strong{Inflation} -- salaries, the pay scale, pensions and
#'     allowance amounts grow by their growth rates.
#' }
#' The order matters: people who retire or leave are not available to be
#' promoted, and hiring sees the vacancies created earlier in the year.
#'
#' ## How the wage bill is measured
#'
#' \code{wage_bill_start}/\code{wage_bill_end} are the sum of
#' \code{salary_col} over contracts that are not \code{"pensioner"} or
#' \code{"inactive"} and have a salary.  Pension costs are \emph{never}
#' included -- they are tracked separately in \code{$pensioner_register}.
#' Modelled allowances are reported separately as
#' \code{allowance_cost_start}/\code{allowance_cost_end}, and
#' \code{total_wage_bill_start}/\code{total_wage_bill_end} is salaries plus
#' allowances.
#'
#' ## Comparing scenarios
#'
#' Give every run a \code{scenario_name} (and flag the baseline with
#' \code{is_baseline = TRUE}) in \code{output_control_args}, then stack the
#' runs' \code{$comparison} tables with \code{data.table::rbindlist()}.
#' Pass the same \code{seed} to every run so that differences come from the
#' policies, not from random selection.
#'
#' @import data.table
#'
#' @param contract_dt data.table.  Contract microdata in the \code{govhr}
#'   harmonized format: one row per contract per snapshot, normally a panel
#'   of several \code{ref_date} snapshots (see \emph{Data requirements}).
#' @param personnel_dt data.table.  Personnel microdata in the \code{govhr}
#'   harmonized format: one row per person per snapshot, covering the same
#'   snapshots as \code{contract_dt}.
#' @param salary_scale_dt data.table.  Pay table: one row per group, the
#'   group columns plus a numeric salary column (normally named the same as
#'   \code{salary_col}).  Used to set the pay of new hires, and of movers
#'   when a movement policy uses \code{salary_update_rule = "scale"}.  Must
#'   include every group column used by \code{movement_policy} and
#'   \code{hiring_policy}.  Grows each year by \code{salary_growth_rate}.
#' @param n_periods Integer.  Number of years to project.
#' @param retirement_policy List or \code{NULL}.  Who retires each year and
#'   what pension they receive.  Build it with a
#'   \code{design_retirement_policy_*()} helper -- see the table in
#'   \emph{Details} and \code{\link{retirement_policy_helpers}}.
#'
#'   The default is the status quo:
#'   \code{design_retirement_policy_status_quo(pension = pension_db(accrual_rate = 0.02, ref_wage_col = "gross_salary_lcu", max_years = 35, replacement_cap = 0.80))}.
#'   Staff retire at the rates observed in the panel for their age, with a
#'   defined-benefit pension of 2% of \code{gross_salary_lcu} per year of
#'   service, counting at most 35 years and paying at most 80% of salary.
#'   There is no mandatory retirement age -- pass your own policy with
#'   \code{mandatory_age} to add one, or to base pensions on a different
#'   salary column.  Needs at least two snapshots.
#'
#'   \code{NULL} means nobody retires.
#' @param exit_policy List or \code{NULL}.  Non-retirement exits:
#'   resignations, dismissals and contracts not renewed.  Build it with
#'   \code{\link{design_exit_policy_status_quo}} (historical rates,
#'   optionally scaled), \code{\link{design_exit_policy_flat}} (one rate for
#'   everyone) or \code{\link{design_exit_policy_adjustment}} (your own
#'   rates, e.g. edited \code{\link{estimate_historical_exit_rates}}
#'   output).  See \code{\link{exit_policy_helpers}}.  Default \code{NULL}:
#'   nobody leaves other than through retirement.
#' @param movement_policy List or \code{NULL}.  Promotions and transfers:
#'   staff moving between the groups defined by the policy's
#'   \code{group_cols} (e.g. \code{"paygrade"} for promotions,
#'   \code{"est_id"} for transfers).  Build it with
#'   \code{\link{design_movement_policy_status_quo}} (historical move rates)
#'   or \code{\link{design_movement_policy_adjustment}} (your own rates,
#'   starting from \code{\link{movement_policy_template}}).  See
#'   \code{\link{movement_policy_helpers}}.
#'
#'   Both helpers require a \code{movement_type} of \code{"promotion"} or
#'   \code{"transfer"}, which decides whether the moves are reported under
#'   \code{n_promotions}/\code{promotion_effect} or
#'   \code{n_transfers}/\code{transfer_effect}.  One policy models one kind of
#'   movement per run.  Default \code{NULL}: nobody moves.
#' @param hiring_policy List or \code{NULL}.  How many people are hired each
#'   year, and into which groups.  Build it with
#'   \code{\link{design_hiring_policy_status_quo}} (historical hiring rates),
#'   \code{\link{design_hiring_policy_flow}} (replace a share of those who
#'   left) or \code{\link{design_hiring_policy_stock}} (hire or downsize
#'   towards target headcounts).  See \code{\link{hiring_policy_helpers}}.
#'   New hires are paid from \code{salary_scale_dt} unless the policy
#'   supplies its own \code{salary_scale}.  Default \code{NULL}: nobody is
#'   hired.
#' @param allowance_policy List or \code{NULL}.  Allowances paid on top of
#'   salaries, per allowance type.  Build it with
#'   \code{\link{design_allowance_policy_status_quo}} (amounts paid at the
#'   starting snapshot) or \code{\link{design_allowance_policy_adjustment}}
#'   (your own amounts, e.g. edited
#'   \code{\link{estimate_baseline_allowance_scale}} output).  See
#'   \code{\link{allowance_policy_helpers}}.  Default \code{NULL}: allowances
#'   are not modelled.
#'
#'   \strong{When allowances are modelled, \code{salary_col} should be base
#'   pay} (e.g. \code{"base_salary_lcu"}), not a gross salary that already
#'   includes allowances -- otherwise allowances are counted twice.  See
#'   \code{\link{simulate_allowances}}.  The allowance ledger is returned in
#'   \code{$allowance_dt} but is not accepted as an input; to chain several
#'   runs, combine their ledgers with \code{data.table::rbindlist()}.
#' @param salary_growth_rate Number, or one number per year (a vector of
#'   length \code{n_periods}).  Annual salary inflation applied to salaries
#'   and to \code{salary_scale_dt}, e.g. \code{0.03} for 3%.  Default
#'   \code{0}.
#' @param pension_cola_rate Number, or one per year.  Annual increase applied
#'   to pensions in \code{$pensioner_register}.  Defaults to
#'   \code{salary_growth_rate}.
#' @param allowance_growth_rate Number, or one per year.  Annual increase in
#'   allowance amounts.  Default \code{0}.  Staff who move to a new group pick
#'   up that group's current (already grown) amount.
#' @param base_year Integer.  Calendar year label for the starting point.
#'   Default: the current year.
#' @param ref_date Date.  The snapshot the projection starts from; each year
#'   advances it by one year.  With panel data it must equal one of the
#'   \code{ref_date} values in \code{contract_dt} exactly -- normally the
#'   latest one.  The default, \code{Sys.Date()}, only makes sense for
#'   single-snapshot data without a \code{ref_date} column.
#' @param column_spec_args Named list.  Column names, if your data use
#'   different names from the \code{govhr} harmonized defaults.  Give only
#'   the ones you need to change, e.g. \code{list(salary_col =
#'   "base_salary_lcu")}.  Recognized keys and defaults:
#'   \describe{
#'     \item{\code{personnel_id_col}}{\code{"personnel_id"}}
#'     \item{\code{contract_id_col}}{\code{"contract_id"}}
#'     \item{\code{start_date_col}}{\code{"start_date"}}
#'     \item{\code{end_date_col}}{\code{"end_date"}}
#'     \item{\code{salary_col}}{\code{"gross_salary_lcu"}.  The salary the
#'       wage bill is measured in.}
#'     \item{\code{contract_type_col}}{\code{"contract_type"}}
#'     \item{\code{status_col}}{\code{"employment_status"}}
#'     \item{\code{age_col}}{\code{NULL}, meaning \code{"age"}.  Age in years;
#'       calculated from \code{birth_date_col} when the column is missing or
#'       empty, then increased by 1 each year.}
#'     \item{\code{tenure_col}}{\code{NULL}, meaning \code{"personnel_tenure"}
#'       (the \code{govhr} dictionary name).  Years of service in the public
#'       sector; calculated from contract history when the column is missing
#'       or empty, then increased by 1 each year.  Custom retirement rules
#'       and pension formulas can refer to it as \code{tenure_years}, whatever
#'       this column is called.}
#'     \item{\code{birth_date_col}}{\code{"birth_date"}.  Used to calculate
#'       age.  \code{NULL} to skip, in which case \code{age_col} must already
#'       be filled in.}
#'     \item{\code{hire_date_col}}{\code{NULL}.  Optional column in
#'       \code{personnel_dt} with each person's original hire date (e.g.
#'       \code{"first_employment_date"}).  When given, status-quo hiring
#'       rates are estimated from it instead of from when people first
#'       appear in the panel.}
#'   }
#' @param output_control_args Named list.  Labelling and output size.  Give
#'   only the ones you need to change.  Recognized keys and defaults:
#'   \describe{
#'     \item{\code{scenario_name}}{\code{NULL}.  A label for this run, e.g.
#'       \code{"Status quo"}.  When given, \code{$comparison} gets
#'       \code{scenario_id} and \code{scenario_label} columns, so runs can
#'       be stacked and told apart.}
#'     \item{\code{is_baseline}}{\code{FALSE}.  \code{TRUE} adds an
#'       \code{is_baseline} column to flag this run as the reference
#'       scenario.  Only used together with \code{scenario_name}.}
#'     \item{\code{return_microdata}}{\code{FALSE}.  \code{TRUE} also returns
#'       the final year's \code{contract_dt}, \code{personnel_dt} and the
#'       allowance ledger.  These can be large.}
#'   }
#' @param seed Integer or \code{NULL}.  Makes random selections (e.g. who
#'   leaves when a strategy is \code{"random"}) reproducible.  Use the same
#'   seed across scenarios you want to compare.  Your own random-number
#'   state is restored when the function returns, so seeding a run does not
#'   change the random draws in code you run afterwards.  Default \code{NULL}.
#'
#' @return An object of class \code{horizon} with these components:
#'   \describe{
#'     \item{\code{$comparison}}{data.table, one row per projected year.
#'       Headcount and wage bill at the start and end of the year
#'       (\code{n_headcount_start}, \code{wage_bill_start},
#'       \code{n_headcount_end}, \code{wage_bill_end}); what changed them:
#'       retirements (\code{n_exits}, \code{exit_savings}), other exits
#'       (\code{n_non_ret_exits}, \code{non_ret_exit_savings}), promotions
#'       and transfers (\code{n_promotions}, \code{promotion_effect},
#'       \code{n_transfers}, \code{transfer_effect}), hiring
#'       (\code{n_hires}, \code{hiring_effect}) and inflation
#'       (\code{inflation_effect}); each effect as a share of the end-of-year
#'       wage bill (\code{*_pct_of_end_bill}); pension costs
#'       (\code{pension_cost_new}, \code{pension_cost_total}); allowances
#'       (\code{allowance_cost_start}, \code{allowance_cost_end}, both
#'       \code{0} when allowances are not modelled); and
#'       \code{total_wage_bill_start}/\code{total_wage_bill_end} (salaries
#'       plus allowances).  Also \code{period_date}, plus
#'       \code{scenario_id}, \code{scenario_label} and \code{is_baseline}
#'       when a \code{scenario_name} is given.}
#'     \item{\code{$summary_dt}}{The same table as \code{$comparison} (kept
#'       for backward compatibility).}
#'     \item{\code{$metadata}}{List recording the policies used, plus
#'       \code{scenario_name} and \code{is_baseline} when given.}
#'     \item{\code{$pensioner_register}}{data.table, one row per retiree per
#'       year: \code{personnel_id}, \code{pension_amount},
#'       \code{period_date} and details at retirement.  Always present
#'       (zero rows when nobody retired).  To chain several runs, combine
#'       their registers with \code{data.table::rbindlist()}.  This is
#'       person-level data: handle it like the input microdata before
#'       saving or sharing the result.}
#'     \item{\code{$contract_dt}, \code{$personnel_dt}}{Every projected
#'       year's microdata, stacked (one snapshot per year, stamped in
#'       \code{ref_date}).  Only when \code{return_microdata = TRUE}.  Also
#'       person-level data.}
#'     \item{\code{$allowance_dt}}{Allowance ledger: one row per contract x
#'       allowance type x year.  Only when \code{return_microdata = TRUE}
#'       and allowances are modelled.}
#'   }
#'
#' @seealso \code{\link{simulate_scenario}} to run a single year;
#'   \code{\link{print.horizon}} and \code{\link{summary.horizon}}.
#'
#' @examples
#' library(data.table)
#'
#' # History: yearly snapshots up to 2012; project 3 years from 2012.
#' # Note: don't name this variable `start_date` or `ref_date` -- those are
#' # column names in the data, and data.table would read the column instead.
#' sim_start <- as.Date("2012-09-01")
#' hist_c <- bra_hrmis_contract[ref_date <= sim_start]
#' hist_p <- bra_hrmis_personnel[ref_date <= sim_start]
#'
#' # Pay table: average base salary per establishment at the start
#' salary_scale <- hist_c[
#'   ref_date == sim_start & !is.na(base_salary_lcu),
#'   .(base_salary_lcu = mean(base_salary_lcu)),
#'   by = est_id
#' ]
#'
#' pension <- pension_db(accrual_rate = 0.02, ref_wage_col = "base_salary_lcu",
#'                       max_years = 35, replacement_cap = 0.80)
#'
#' \donttest{
#' # 1. Status quo: every module reproduces recent history
#' status_quo <- simulate_horizon(
#'   contract_dt       = hist_c,
#'   personnel_dt      = hist_p,
#'   salary_scale_dt   = salary_scale,
#'   n_periods         = 3L,
#'   ref_date          = sim_start,
#'   retirement_policy = design_retirement_policy_status_quo(
#'     pension = pension, mandatory_age = 75
#'   ),
#'   exit_policy       = design_exit_policy_status_quo(group_cols = "est_id"),
#'   movement_policy   = design_movement_policy_status_quo(
#'     group_cols = "est_id", movement_type = "transfer",
#'     salary_update_rule = "keep"
#'   ),
#'   hiring_policy     = design_hiring_policy_status_quo(group_cols = "est_id"),
#'   allowance_policy  = design_allowance_policy_status_quo(
#'     bra_hrmis_allowance, hist_c, group_cols = "est_id", ref_date = sim_start
#'   ),
#'   salary_growth_rate  = 0.03,
#'   column_spec_args    = list(salary_col = "base_salary_lcu"),
#'   output_control_args = list(scenario_name = "Status quo", is_baseline = TRUE),
#'   seed = 1L
#' )
#' status_quo$comparison
#'
#' # 2. Reform: retire at 65 with 10+ years' service, and replace only half
#' #    of the people who leave. Exits stay at their historical rates.
#' reform <- simulate_horizon(
#'   contract_dt       = hist_c,
#'   personnel_dt      = hist_p,
#'   salary_scale_dt   = salary_scale,
#'   n_periods         = 3L,
#'   ref_date          = sim_start,
#'   retirement_policy = design_retirement_policy_age_tenure(
#'     min_age = 65, min_tenure = 10, pension = pension
#'   ),
#'   exit_policy       = design_exit_policy_status_quo(group_cols = "est_id"),
#'   hiring_policy     = design_hiring_policy_flow(replacement_rate = 0.5,
#'                                                group_cols = "est_id"),
#'   salary_growth_rate  = 0.03,
#'   column_spec_args    = list(salary_col = "base_salary_lcu"),
#'   output_control_args = list(scenario_name = "Retire at 65, replace half"),
#'   seed = 1L
#' )
#'
#' # 3. Compare the two side by side
#' rbindlist(list(status_quo$comparison, reform$comparison), fill = TRUE)[
#'   , .(scenario_label, period_date, n_headcount_end, wage_bill_end)
#' ]
#' }
#'
#' @export
simulate_horizon <- function(contract_dt,
                             personnel_dt,
                             salary_scale_dt,
                             n_periods,
                             retirement_policy = design_retirement_policy_status_quo(pension = pension_db(accrual_rate = 0.02, 
                                                                                                          ref_wage_col = "gross_salary_lcu",
                                                                                                          max_years = 35, 
                                                                                                          replacement_cap = 0.80)),
                             exit_policy        = NULL,
                             movement_policy    = NULL,
                             hiring_policy      = NULL,
                             allowance_policy   = NULL,
                             salary_growth_rate = 0,
                             pension_cola_rate  = salary_growth_rate,
                             allowance_growth_rate = 0,
                             base_year          = as.integer(format(Sys.Date(), "%Y")),
                             ref_date           = Sys.Date(),
                             column_spec_args   = list(),
                             output_control_args = list(),
                             seed               = NULL) {

  # ====================================================================
  # 0. Validate and set up
  # ====================================================================

  # column_spec_args / output_control_args: caller supplies only the keys
  # they want to override (utils::modifyList() layers them onto the full
  # defaults below), so e.g. column_spec_args = list(age_col = "age_yrs")
  # is sufficient -- every other column name keeps its default.
  .default_column_spec <- list(
    personnel_id_col  = "personnel_id",
    contract_id_col   = "contract_id",
    start_date_col    = "start_date",
    end_date_col      = "end_date",
    salary_col        = "gross_salary_lcu",
    contract_type_col = "contract_type",
    status_col        = "employment_status",
    age_col           = NULL,
    tenure_col        = NULL,
    birth_date_col    = "birth_date",
    hire_date_col     = NULL
  )
  column_spec_args <- utils::modifyList(.default_column_spec, column_spec_args)
  personnel_id_col  <- column_spec_args$personnel_id_col
  contract_id_col   <- column_spec_args$contract_id_col
  start_date_col    <- column_spec_args$start_date_col
  end_date_col      <- column_spec_args$end_date_col
  salary_col        <- column_spec_args$salary_col
  contract_type_col <- column_spec_args$contract_type_col
  status_col        <- column_spec_args$status_col
  age_col           <- column_spec_args$age_col
  tenure_col        <- column_spec_args$tenure_col
  birth_date_col    <- column_spec_args$birth_date_col
  hire_date_col     <- column_spec_args$hire_date_col

  .default_output_control <- list(
    scenario_name    = NULL,
    is_baseline      = FALSE,
    return_microdata = FALSE
  )
  output_control_args <- utils::modifyList(.default_output_control, output_control_args)
  scenario_name    <- output_control_args$scenario_name
  is_baseline      <- output_control_args$is_baseline
  return_microdata <- output_control_args$return_microdata

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
  if (is.null(tenure_col)) tenure_col <- "personnel_tenure"

  # Periods are always annual -- period_fraction exists only because
  # simulate_scenario() takes it as a direct parameter (kept flexible there
  # for manual chaining); simulate_horizon() always passes 1.
  period_fraction <- 1

  # Expand scalar/vector rate parameters to per-period vectors
  salary_growth_rate    <- expand_rate_vector(salary_growth_rate, n_periods, "salary_growth_rate")
  pension_cola_rate     <- expand_rate_vector(pension_cola_rate,  n_periods, "pension_cola_rate")
  allowance_growth_rate <- expand_rate_vector(allowance_growth_rate, n_periods, "allowance_growth_rate")

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
      movement_policy$policy_table <- estimate_historical_movement_rates(
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
  if (!is.null(exit_policy) && is.null(exit_policy$policy_table)) {
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

  # For retirement, pre-estimate hazard rates from the full panel BEFORE
  # stripping ref_date, mirroring the exit_policy pattern immediately above.
  # Unlike exit_policy, a single-snapshot panel is a hard error here (not a
  # warning/fallback): a hazard rate needs multiple periods of exposure to be
  # stable at the per-age level the way a single-snapshot exit_rate cannot be
  # estimated at all any other way, so there is no sensible default to fall
  # back to.
  if (!is.null(retirement_policy) &&
      identical(retirement_policy$defaults$eligibility_type, "hazard_rate") &&
      is.null(retirement_policy$policy_table)) {

    ref_date_col_name <- "ref_date"
    has_panel <- ref_date_col_name %in% names(contract_dt) &&
                 data.table::uniqueN(contract_dt[[ref_date_col_name]]) >= 2L

    if (!has_panel) {
      stop(
        "simulate_horizon: eligibility_type = 'hazard_rate' requires panel ",
        "data (>= 2 ref_date snapshots) to estimate hazard rates from -- a ",
        "single snapshot cannot estimate a stable per-age retirement rate.",
        call. = FALSE
      )
    }

    # Hazard rates are bucketed by age at each snapshot, but age_col is only
    # auto-computed from birth_date_col further down (Phase 1b), after the
    # panel has been cut to the starting snapshot. When the panel carries
    # birth dates but no populated age column, derive age at EACH snapshot's
    # own ref_date (same formula as Phase 1b) on a copy used only here.
    .hazard_panel_ <- personnel_dt
    .panel_age_missing_ <- !(age_col %in% names(personnel_dt)) ||
                           all(is.na(personnel_dt[[age_col]]))
    if (.panel_age_missing_ && !is.null(birth_date_col) &&
        birth_date_col %in% names(personnel_dt)) {
      .hazard_panel_ <- data.table::copy(personnel_dt)
      .hazard_panel_[, (age_col) := as.numeric(
        difftime(get(ref_date_col_name), get(birth_date_col), units = "days")
      ) / 365.25]
    }

    retirement_policy$policy_table <- estimate_retirement_hazard_rates(
      panel_personnel_dt = .hazard_panel_,
      group_cols          = retirement_policy$group_cols,
      age_col             = age_col,
      personnel_id_col    = personnel_id_col,
      ref_date_col         = ref_date_col_name,
      status_col           = status_col,
      retire_status         = retirement_policy$pensioner_type_value %||% "pensioner"
    )
    if (nrow(retirement_policy$policy_table) == 0L) {
      message(
        "simulate_horizon: no retirements observed in the panel, so status-quo ",
        "retirement will produce none",
        if (!is.null(retirement_policy$defaults$min_age))
          paste0(" (apart from the mandatory age of ", retirement_policy$defaults$min_age, ")"),
        "."
      )
    }
  }

  # If data contains a ref_date panel column, subset to the starting snapshot
  # before stripping so that sub-modules see a single-period snapshot.

  start_ref <- ref_date   # capture before any column could shadow it
  .ref_date_col_name_ <- "ref_date"
  .has_panel_ <- .ref_date_col_name_ %in% names(contract_dt) &&
                 data.table::uniqueN(contract_dt[[.ref_date_col_name_]]) >= 2L
  .full_panel_c_ <- if (.has_panel_) data.table::copy(contract_dt)  else NULL
  .full_panel_p_ <- if (.has_panel_) data.table::copy(personnel_dt) else NULL

  # Status-quo hiring: estimate the historical hiring rates from the full
  # panel now, before it is cut to the starting snapshot below. A panel the
  # user put in the policy (panel_contract_dt / panel_personnel_dt) takes
  # precedence. Either way the panels are then dropped from the policy, so
  # the policy -- which is returned in $metadata -- holds only the small rate
  # table, never person-level data.
  if (!is.null(hiring_policy) && identical(hiring_policy$mode, "status_quo")) {
    if (.has_panel_) {
      hiring_policy$squorate_dt <- tryCatch(
        estimate_historical_hiring_rates(
          panel_contract_dt  = hiring_policy$panel_contract_dt  %||% contract_dt,
          panel_personnel_dt = hiring_policy$panel_personnel_dt %||% personnel_dt,
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
    hiring_policy$panel_contract_dt  <- NULL
    hiring_policy$panel_personnel_dt <- NULL
  }

  if ("ref_date" %in% names(contract_dt)) {
    contract_dt <- contract_dt[get("ref_date") == start_ref]
    # contract_dt[, ref_date := NULL]
  }
  if ("ref_date" %in% names(personnel_dt)) {
    personnel_dt <- personnel_dt[get("ref_date") == start_ref]
    # personnel_dt[, ref_date := NULL]
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
  # %in%, not == : a real NA contract_type value (which occurs in bundled and
  # real HRMIS data) makes `== .pensioner_type_val_` evaluate to NA rather
  # than FALSE, which can make the any() check below NA instead of TRUE/FALSE
  # depending on the rest of the snapshot -- %in% treats NA as non-matching,
  # which is the correct "not a pensioner contract" reading.
  .is_pensioner_ <- contract_dt[[contract_type_col]] %in% .pensioner_type_val_
  if (any(!(.has_salary_ | .is_pensioner_))) {
    contract_dt <- contract_dt[.has_salary_ | .is_pensioner_]
  }

  if (!salary_col %in% names(contract_dt)) {
    stop("salary_col '", salary_col, "' not found in contract_dt.", call. = FALSE)
  }

  # Initialise the allowance ledger. Unlike pensioner_register, this is never
  # accepted as a simulate_horizon() input -- it accumulates purely within
  # this call and is returned in the output, matching the documented
  # constraint that a register is never accepted back in (chaining across
  # separate simulate_horizon() calls is the user's responsibility via
  # rbindlist(), same as for pensioner_register).
  allowance_dt <- NULL

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

  # ====================================================================
  # PERIOD LOOP
  # =====================================================================
  # before the loop, pre-allocate lists to hold the contract and personnel snapshots for each period
  # these will be used to return the final microdata if return_microdata = TRUE

  contract_panel_list <- vector("list", n_periods)
  personnel_panel_list <- vector("list", n_periods)

  # Real baseline allowance cost, before period 1 runs -- analogous to how
  # wage_bill_start reflects the already-existing salaries in the input data,
  # not zero. Used only as the fill value for period 1's allowance_cost_start
  # below; the allowance_dt this produces is discarded (period 1's real
  # ledger entry comes from the period loop's own step 5).
  period0_allowance_cost <- 0
  if (!is.null(allowance_policy)) {
    alw0 <- simulate_allowances(
      contract_dt   = contract_dt,
      personnel_dt  = personnel_dt,
      allowance_dt  = NULL,
      policy_params = allowance_policy,
      ref_date      = ref_date,
      contract_type_col = contract_type_col,
      personnel_id_col  = personnel_id_col,
      contract_id_col   = contract_id_col
    )
    period0_allowance_cost <- alw0$summary$allowance_cost_total
  }

  # Seed once, here, before the period loop -- not per-period. The RNG
  # stream should continue naturally across periods so each period draws
  # fresh values; re-seeding every period would create correlated/repeated
  # patterns across periods. The per-period simulate_scenario() calls below
  # deliberately do not pass seed (leaving it at simulate_scenario()'s own
  # default NULL) so they don't re-seed. The caller's own random stream is
  # restored when simulate_horizon() exits.
  if (!is.null(seed)) {
    rng_state <- .save_rng_state()
    on.exit(.restore_rng_state(rng_state), add = TRUE)
    set.seed(seed)
  }

  for (t in seq_len(n_periods)) {

    growth        <- salary_growth_rate[t]
    cola_rate     <- pension_cola_rate[t]
    allow_growth  <- allowance_growth_rate[t]
    cur_date      <- .advance_period(ref_date, t - 1L, unit = "year")

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
      allowance_policy        = allowance_policy,
      allowance_dt            = allowance_dt,
      salary_growth_rate      = growth,
      pension_cola_rate       = cola_rate,
      allowance_growth_rate   = allow_growth,
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
    allowance_dt        <- scenario_result$allowance_dt
    allowance_policy     <- scenario_result$allowance_policy

    # Accumulate period snapshots for panel output
    if (return_microdata) {
      ct_snap <- data.table::copy(contract_dt)
      ct_snap[, ref_date := cur_date]
      contract_panel_list[[t]] <- ct_snap

      pt_snap <- data.table::copy(personnel_dt)
      pt_snap[, ref_date := cur_date]
      personnel_panel_list[[t]] <- pt_snap
    }

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

  # Additive allowance/total-wage-bill columns. allowance_cost_start is last
  # period's allowance_cost_end for periods 2+ (nothing touches allowance_dt
  # between periods except this same pipeline, so this is exactly what was
  # actually last assigned -- NOT the same as independently re-resolving
  # against the already-COLA-grown scale, which would double-count next
  # period's own growth before its assignment step has even run). Period 1
  # has no prior simulated period, so it falls back to period0_allowance_cost
  # -- the real baseline resolved against the original starting population,
  # mirroring how wage_bill_start reflects real pre-existing salaries rather
  # than zero. wage_bill_start/end and .active_wage_bill() are untouched, so
  # total_wage_bill is purely additive on top of them.
  summary_dt[, allowance_cost_start := data.table::shift(
    allowance_cost_end, n = 1L, fill = period0_allowance_cost
  )]
  summary_dt[, total_wage_bill_start := wage_bill_start + allowance_cost_start]
  summary_dt[, total_wage_bill_end   := wage_bill_end   + allowance_cost_end]

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
    allowance_policy   = allowance_policy,
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
  out$contract_dt  <- data.table::rbindlist(
    contract_panel_list,  use.names = TRUE, fill = TRUE
  )
  out$personnel_dt <- data.table::rbindlist(
    personnel_panel_list, use.names = TRUE, fill = TRUE
  )
  # allowance_dt is already the full accumulated ledger (built incrementally
  # each period by update_allowance_register()), not a list of snapshots --
  # no rbindlist needed. NULL when allowance_policy was never used. Gated
  # behind return_microdata (unlike pensioner_register) since it's one row
  # per contract x allowance_type x period and can get large.
  out$allowance_dt <- allowance_dt
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
