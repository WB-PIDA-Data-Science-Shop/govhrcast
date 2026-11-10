# ===========================================================================
# simulate_scenario() -- single-period orchestrator
# ===========================================================================

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
#' 6. **Aging** -- increment \code{age_col} and \code{tenure_col}.
#' 7. **COLA** -- grow the salary table, contract salaries, a pay table passed
#'    to the hiring policy (by \code{salary_growth_rate}) and the allowance
#'    amounts (by \code{allowance_growth_rate}).
#' 8. **Allowances** -- assign this year's grown allowance amounts to the
#'    contracts, after movements and hiring have settled who is in which group.
#' 9. **Pension COLA** -- grow the pensions on the register by
#'    \code{pension_cola_rate}.
#' 10. **Snapshot B** -- record \code{wage_bill_end} and \code{n_headcount_end}.
#'
#' @details
#' **Wage bill measurement**: \code{wage_bill_start} and \code{wage_bill_end}
#' are computed via an internal helper that sums \code{salary_col} over the
#' contract rows on the payroll -- rows with \code{contract_type} of
#' \code{"pensioner"} or \code{"inactive"} and rows with a missing salary are
#' excluded, while a blank contract type counts as staff.  Unlike
#' \code{simulate_horizon()}, this function does not look at
#' \code{employment_status} to find pensioners, so give their rows the contract
#' type \code{"pensioner"} when you call it directly.  The \code{pensioner_register} is a
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
#' @param period_date Date.  The date the period's rules run at (ages, tenure,
#'   end dates), and the \code{period_date} column of the returned summary row.
#'   \code{\link{simulate_horizon}()} passes the start of each year, then dates
#'   the year's results at its end.
#' @param pensioner_register data.table or \code{NULL}.  The pensions carried
#'   from earlier periods: one row per pension contract, with the person and
#'   contract ID columns (under the names \code{column_spec_args} gives) and
#'   \code{pension_amount}, this year's pension.  Pass \code{NULL} (default) for
#'   the first period -- an empty register is initialised internally.
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
#' @param allowance_growth_rate Numeric scalar.  Growth rate applied to the
#'   amounts in \code{allowance_policy} this period, before they are assigned,
#'   so allowances grow in the same year as pay.  Default \code{0}.
#' @param column_spec_args Column names, from \code{\link{define_column_spec}()}.  Change only
#'   the ones that differ from the \code{govhr} defaults, e.g.
#'   \code{define_column_spec(salary_col = "base_salary_lcu")}.  See
#'   \code{\link{define_column_spec}} for the keys and defaults.  Here,
#'   \code{age_col} and \code{tenure_col} are the columns aged each period;
#'   \code{NULL} leaves that column as it is.
#' @param period_fraction Numeric.  Fraction of a year each period represents
#'   (1 for annual, 1/12 for monthly, 1/365.25 for daily).  Used to increment
#'   the age and tenure columns by the correct amount.  Default 1.
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
#'       \code{hiring_effect}, \code{n_downsized}, \code{downsizing_savings},
#'       \code{inflation_effect},
#'       \code{n_headcount_end}, \code{wage_bill_end}, \code{allowance_cost_end}
#'       (\code{0} when \code{allowance_policy} is \code{NULL}).}
#'     \item{contract_dt}{Updated contract snapshot.}
#'     \item{personnel_dt}{Updated personnel snapshot.}
#'     \item{salary_scale_dt}{Updated pay table (post-COLA).}
#'     \item{pensioner_register}{The pensions after this period: every earlier
#'       pension grown by \code{pension_cola_rate}, plus this period's retirees,
#'       one row each on their main contract.}
#'     \item{allowance_dt}{Accumulated allowance ledger including this
#'       period's rows, or unchanged/\code{NULL} when \code{allowance_policy}
#'       is \code{NULL}.}
#'     \item{allowance_policy}{\code{allowance_policy} as supplied, with
#'       \code{policy_table} grown by \code{allowance_growth_rate} -- pass
#'       this back in as next period's \code{allowance_policy} to carry the
#'       growth forward (\code{simulate_horizon()} does this automatically).}
#'     \item{hiring_policy}{\code{hiring_policy} as supplied, with any
#'       \code{salary_scale} you passed in it grown by
#'       \code{salary_growth_rate} -- pass this back in as next period's
#'       \code{hiring_policy} so new hires keep up with pay
#'       (\code{simulate_horizon()} does this automatically).}
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
                               column_spec_args    = define_column_spec(),
                               period_fraction     = 1,
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

  .check_column_spec(column_spec_args)
  .check_spec_columns(contract_dt, personnel_dt, column_spec_args, required = list(
    contract_dt = c("personnel_id_col", "contract_type_col", "salary_col")
  ))
  personnel_id_col  <- column_spec_args$personnel_id_col
  salary_col        <- column_spec_args$salary_col
  contract_type_col <- column_spec_args$contract_type_col
  status_col        <- column_spec_args$status_col
  age_col           <- column_spec_args$age_col
  tenure_col        <- column_spec_args$tenure_col

  # Work on copies: aging, COLA and the pension COLA below update these tables
  # in place (data.table's :=), and the caller's own tables must come back
  # unchanged. setDT(copy(x)) gives a data.table copy of a data.table or a
  # data.frame alike.
  contract_dt     <- data.table::setDT(data.table::copy(contract_dt))
  personnel_dt    <- data.table::setDT(data.table::copy(personnel_dt))
  salary_scale_dt <- data.table::setDT(data.table::copy(salary_scale_dt))
  if (!is.null(pensioner_register))
    pensioner_register <- data.table::setDT(data.table::copy(pensioner_register))

  # Initialise empty pensioner register on first call
  if (is.null(pensioner_register)) {
    pensioner_register <- .empty_pensioner_register(personnel_id_col, column_spec_args$contract_id_col)
  }

  .check_pay_column(salary_scale_dt, salary_col, "salary_scale_dt")

  # ------------------------------------------------------------------
  # SNAPSHOT A  (active employees only -- pensioner rows excluded)
  # ------------------------------------------------------------------
  n_headcount_start <- data.table::uniqueN(
    contract_dt[.on_payroll(get(contract_type_col))],
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
      as_of_date                = period_date,
      column_spec_args        = column_spec_args
    )
    retirees_dt  <- ret_result$retirees_dt
    contract_dt  <- ret_result$contract_dt
    personnel_dt <- ret_result$personnel_dt

    n_exits      <- if (!is.null(retirees_dt)) nrow(retirees_dt) else 0L
    exit_savings <- compute_exit_effect(retirees_dt, salary_col)

    if (!is.null(retirees_dt) && nrow(retirees_dt) > 0L &&
        "pension" %in% names(retirees_dt)) {
      pension_cost_new <- sum(retirees_dt$pension, na.rm = TRUE)

      # Add them to the register: one row per retiree, on their main contract,
      # which carries the pension in the returned tables
      new_reg <- retirees_dt[, c(personnel_id_col, column_spec_args$contract_id_col, "pension"), with = FALSE]
      data.table::setnames(new_reg, "pension", "pension_amount")
      new_reg[, pension_amount := as.numeric(pension_amount)]
      pensioner_register <- data.table::rbindlist(list(pensioner_register, new_reg), use.names = TRUE)
    }
  }

  # ------------------------------------------------------------------
  # STEP 2: NON-RETIREMENT EXITS (Phase 3)
  # ------------------------------------------------------------------
  n_non_ret_exits          <- 0L
  non_ret_exit_savings     <- 0
  exits_dt                 <- NULL

  if (!is.null(exit_policy)) {
    exit_result <- simulate_exits(
      contract_dt      = contract_dt,
      personnel_dt     = personnel_dt,
      policy_params    = exit_policy,
      as_of_date         = period_date,
      column_spec_args = column_spec_args
    )
    contract_dt          <- exit_result$contract_dt
    personnel_dt         <- exit_result$personnel_dt
    n_non_ret_exits      <- exit_result$summary$n_exits
    non_ret_exit_savings <- exit_result$summary$exit_savings
    exits_dt             <- exit_result$exits_dt
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
      as_of_date          = period_date,
      column_spec_args  = column_spec_args
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
  n_hires            <- 0L
  hiring_effect      <- 0
  n_downsized        <- 0L
  downsizing_savings <- 0

  if (!is.null(hiring_policy)) {
    hiring_this_year <- hiring_policy
    if (is.null(hiring_policy$salary_scale)) {
      # Derive a hiring-compatible salary scale from salary_scale_dt by
      # aggregating to the hiring group_cols level.  This avoids injecting a
      # finer-grained scale (e.g. est_id x paygrade) into a hiring module that
      # joins only on est_id -- which would cause a cartesian join error.
      # With no hiring groups this is one row: the mean of the pay table.
      # Used for this year only: next year's comes from the grown salary_scale_dt.
      hire_groups <- intersect(hiring_policy$group_cols, names(salary_scale_dt))
      hiring_this_year$salary_scale <- salary_scale_dt[,
        lapply(.SD, mean, na.rm = TRUE),
        keyby = hire_groups,
        .SDcols = salary_col
      ]
    }

    hire_result <- simulate_hiring(
      contract_dt       = contract_dt,
      personnel_dt      = personnel_dt,
      policy_params     = hiring_this_year,
      leavers_dt        = .leavers_this_period(retirees_dt, exits_dt, contract_dt,
                                               hiring_policy$group_cols, column_spec_args),
      as_of_date          = period_date,
      column_spec_args  = column_spec_args
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
    n_downsized        <- hire_result$summary$n_downsized
    downsizing_savings <- hire_result$summary$downsizing_savings
  }

  # ------------------------------------------------------------------
  # STEP 5: AGING
  # ------------------------------------------------------------------
  # Everyone still here grows older, pensioners included, as in real data; only
  # staff gain tenure
  if (!is.null(age_col) && age_col %in% names(personnel_dt)) {
    personnel_dt[get(status_col) %in% c("active", "pensioner"),
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

  # COLA applies only to pay on the payroll; pension rows are already 0
  pre_cola_wage_bill <- .active_wage_bill(
    contract_dt, contract_type_col, salary_col, personnel_id_col
  )
  inflation_effect   <- compute_inflation_effect(pre_cola_wage_bill, growth)

  if (growth != 0) {
    salary_scale_dt[, (salary_col) := get(salary_col) * (1 + growth)]
    contract_dt[.on_payroll(get(contract_type_col)),
                (salary_col) := get(salary_col) * (1 + growth)]

    # A pay table the user passed to the hiring policy grows the same way. This
    # works on a copy, so the user's own table is never changed.
    if (!is.null(hiring_policy$salary_scale)) {
      hiring_policy$salary_scale <- data.table::copy(hiring_policy$salary_scale)[
        , (salary_col) := get(salary_col) * (1 + growth)
      ]
    }
  }

  # Grow the allowance amounts themselves (not individual ledger rows), including
  # the amount for unmatched groups, so this year's allowances below use them,
  # the same year pay grows. A contract that stays in its group sees its allowance
  # grow at exactly this rate, while a mover picks up the new group's grown amount.
  if (!is.null(allowance_policy) && allowance_growth_rate != 0) {
    if (!is.null(allowance_policy$policy_table)) {
      allowance_policy$policy_table <- apply_salary_scale_adjustment(
        allowance_policy$policy_table,
        adjustment = 1 + allowance_growth_rate,
        salary_col = "amount"
      )
    }
    allowance_policy$defaults$amount <- allowance_policy$defaults$amount * (1 + allowance_growth_rate)
  }

  # ------------------------------------------------------------------
  # STEP 7: ALLOWANCES
  # ------------------------------------------------------------------
  # Runs after hiring/movements finalise group membership (est_id, paygrade,
  # etc.) for the period. contract_dt/personnel_dt are never mutated here --
  # allowance costs live entirely in the allowance_dt ledger, the same way
  # pensioner_register tracks pension costs outside the wage bill.
  allowance_cost_end <- 0

  if (!is.null(allowance_policy)) {
    alw_result <- simulate_allowances(
      contract_dt   = contract_dt,
      personnel_dt  = personnel_dt,
      allowance_dt  = allowance_dt,
      policy_params = allowance_policy,
      as_of_date      = period_date,
      column_spec_args = column_spec_args
    )
    allowance_dt       <- alw_result$allowance_dt
    allowance_cost_end <- alw_result$summary$allowance_cost_total
  }

  # Apply pension COLA to the register (separate rate from active salary COLA)
  if (pension_cola_rate != 0 && nrow(pensioner_register) > 0L) {
    pensioner_register[, pension_amount := pension_amount * (1 + pension_cola_rate)]
  }

  # This year's cost of everyone on the register, after this year's growth
  pension_cost_total <- sum(pensioner_register$pension_amount, na.rm = TRUE)

  # ------------------------------------------------------------------
  # SNAPSHOT B  (active employees only -- pensioner rows excluded)
  # ------------------------------------------------------------------
  n_headcount_end <- data.table::uniqueN(
    contract_dt[.on_payroll(get(contract_type_col))],
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
    n_downsized              = as.integer(n_downsized),
    downsizing_savings       = downsizing_savings,
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
    allowance_policy   = allowance_policy,
    hiring_policy      = hiring_policy
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
#' | Hiring | \code{hiring_policy} | \code{\link{design_hiring_policy_status_quo}} | \code{\link{design_hiring_policy_adjustment}}, \code{\link{design_hiring_policy_flow}}, \code{\link{design_hiring_policy_stock}} |
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
#' snapshot date in a \code{ref_date} column, or the column named by
#' \code{ref_date_col} (the \code{govhr} harmonized format, as in
#' \code{\link{bra_hrmis_contract}}).  The projection starts
#' from the latest snapshot, or from the one whose date equals \code{baseline_date}
#' if you give it.  The earlier
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
#'   \item The new hire profile is estimated from the people hired during the
#'     panel (\code{\link{estimate_new_hire_profile}}).
#'   \item The data are cut to the starting snapshot.  A \code{baseline_date} that is
#'     not one of the snapshots stops with an error.
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
#'   \item \strong{Hiring} -- new staff join.  They start with tenure 0, the
#'     median entry age and most common contract type among recent hires in their
#'     group, and a gender drawn from the group's shares among recent hires, so
#'     from then on they age, retire and exit like everyone else (see
#'     \code{\link{estimate_new_hire_profile}}).
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
#'
#' ## Who counts as what at the start
#'
#' \itemize{
#'   \item \strong{Pensioner:} a contract row is a pension if its
#'     \code{contract_type} is \code{"pensioner"} or the person's
#'     \code{employment_status} is \code{"pensioner"}.  The amount in
#'     \code{salary_col} is the pension, and several rows add up.  A pensioner
#'     with no amount stays on the register with a blank, and you get a message
#'     giving the count.  Every other row is staff, so someone with a pension
#'     and a working contract is on the register and on the payroll.
#'   \item \strong{Blank \code{contract_type}:} this is missing data.  The row
#'     counts as staff if the person's status is \code{"active"}, and is off the
#'     payroll otherwise.
#'   \item \strong{Contract end dates:} staff contracts are renewed past their
#'     end date, so everyone paid at the start stays until retirement, an exit
#'     or downsizing removes them.  End dates are blank in the returned
#'     microdata until then.
#' }
#'
#' People who retire get the status \code{"pensioner"}; people who exit or are let
#' go by downsizing get the status and contract type \code{"inactive"}.
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
#'   group columns plus a numeric column with the same name as
#'   \code{salary_col}.  Used to set the pay of new hires, and of movers
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
#'   \code{design_retirement_policy_status_quo(pension = pension_db(accrual_rate = 0.02, max_years = 35, replacement_cap = 0.80))}.
#'   Staff retire at the rates observed in the panel for their age, and ages
#'   above the oldest age with a rate take that age's rate.  They get a
#'   defined-benefit pension of 2% of \code{salary_col} (on all their contracts) per year of
#'   service, counting at most 35 years and paying at most 80% of salary.
#'   There is no mandatory retirement age -- pass your own policy with
#'   \code{mandatory_age} to add one.  Needs at least two snapshots.
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
#'   \code{\link{design_hiring_policy_adjustment}} (your own rates, e.g. edited
#'   \code{\link{estimate_historical_hiring_rates}} output, to treat groups
#'   differently),
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
#' @param baseline_date Date or \code{NULL}.  The snapshot the projection starts from;
#'   each year advances it by one year.  The default, \code{NULL}, uses the latest
#'   snapshot that both \code{contract_dt} and \code{personnel_dt} hold.  A date you
#'   give must match one of the snapshots in \code{contract_dt} exactly.  If your
#'   data have no snapshot date column, you must give the date they describe.  This is the
#'   date only: \code{is_baseline} in \code{\link{define_output_control}()} is a
#'   separate choice, which marks a run as the scenario others are compared with.
#' @param column_spec_args Which columns hold what, from
#'   \code{\link{define_column_spec}()}.  The default reads the \code{govhr}
#'   harmonized names, so harmonized data need nothing here.  Change only the
#'   ones that differ, e.g.
#'   \code{define_column_spec(salary_col = "base_salary_lcu")}.
#' @param output_control_args How to label this run and what to return, from
#'   \code{\link{define_output_control}()}.  The default leaves the run unlabelled
#'   and returns no person-level microdata.  Change only what differs, e.g.
#'   \code{define_output_control(scenario_name = "Status quo")}.
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
#'       (\code{n_hires}, \code{hiring_effect}), downsizing
#'       (\code{n_downsized}, \code{downsizing_savings}, a positive number
#'       when pay is saved) and inflation
#'       (\code{inflation_effect}); each effect as a share of the end-of-year
#'       wage bill (\code{*_pct_of_end_bill}); pension costs
#'       (\code{pension_cost_new}, what the year's new retirees are awarded, and
#'       \code{pension_cost_total}, the whole register after the year's pension
#'       growth); allowances
#'       (\code{allowance_cost_start}, \code{allowance_cost_end}, both
#'       \code{0} when allowances are not modelled); and
#'       \code{total_wage_bill_start}/\code{total_wage_bill_end} (salaries
#'       plus allowances).  Also \code{period_date}, the date of the snapshot
#'       the year leaves behind: \code{baseline_date} plus one year for year 1,
#'       and so on (each year's rules run as of its start).  Also
#'       \code{is_baseline}, plus \code{scenario_id} and \code{scenario_label}
#'       when a \code{scenario_name} is given.}
#'     \item{\code{$summary_dt}}{The same table as \code{$comparison} (kept
#'       for backward compatibility).}
#'     \item{\code{$metadata}}{List recording what produced the run, so results
#'       can be traced back and compared.  \code{$policy_args} holds the five
#'       policies with their status-quo rates filled in, as they stood before
#'       the first year (the yearly growth of a hiring pay table or of
#'       allowance amounts is not recorded), the three growth rates as you
#'       passed them, \code{n_periods} and \code{baseline_date}.  The hiring
#'       policy includes the new hire profile, which can describe few people in
#'       small groups.  Also \code{scenario_name} and \code{is_baseline}.}
#'     \item{\code{$pensioner_register}}{data.table, one row per pension
#'       contract per simulated year: the person ID, contract ID and date
#'       columns (under your names for them) and \code{pension_amount}, that
#'       year's pension.  People retired before \code{baseline_date} are on it
#'       from year 1; a new retiree from the year they retire, on their main
#'       contract.  Each year's total is that year's \code{pension_cost_total}.
#'       Always present (zero rows when nobody is retired).  Nobody leaves the
#'       register: deaths are not modelled, so long projections overstate
#'       pension costs.  To chain several runs, combine their registers with
#'       \code{data.table::rbindlist()}.  This is person-level data: handle it
#'       like the input microdata before saving or sharing the result.}
#'     \item{\code{$contract_dt}, \code{$personnel_dt}}{Your data continued
#'       through the projection, so govhr and govhrapp read history and
#'       projection alike.  Up to \code{baseline_date} they are your own rows,
#'       unchanged.  Each simulated year is then one snapshot, dated in
#'       \code{ref_date_col} as in \code{$comparison}, showing what govhr data
#'       would: staff; every pensioner, with that year's pension in
#'       \code{salary_col} (people retired before the baseline as in your data,
#'       new retirees with \code{contract_type} \code{"pensioner"}); and nobody
#'       who has left, since government data rarely record an exit and govhr
#'       detects one as a person who is gone the next year.  Your columns, in
#'       your order, plus \code{age_col} and \code{tenure_col} when the
#'       simulation fills them in and your data lack them.  Pensioners grow
#'       older each year; only staff gain tenure.  Only when
#'       \code{return_microdata = TRUE}.  Also person-level data.}
#'     \item{\code{$allowance_dt}}{Allowance ledger: one row per contract x
#'       allowance type x year, dated like the snapshots.  Only when
#'       \code{return_microdata = TRUE} and allowances are modelled.}
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
#' pension <- pension_db(accrual_rate = 0.02, max_years = 35, replacement_cap = 0.80)
#'
#' \donttest{
#' # 1. Status quo: every module reproduces recent history
#' status_quo <- simulate_horizon(
#'   contract_dt       = hist_c,
#'   personnel_dt      = hist_p,
#'   salary_scale_dt   = salary_scale,
#'   n_periods         = 3L,
#'   baseline_date          = sim_start,
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
#'     bra_hrmis_allowance, hist_c, group_cols = "est_id", baseline_date = sim_start
#'   ),
#'   salary_growth_rate  = 0.03,
#'   column_spec_args    = define_column_spec(salary_col = "base_salary_lcu"),
#'   output_control_args = define_output_control(scenario_name = "Status quo",
#'                                               is_baseline = TRUE),
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
#'   baseline_date          = sim_start,
#'   retirement_policy = design_retirement_policy_age_tenure(
#'     min_age = 65, min_tenure = 10, pension = pension
#'   ),
#'   exit_policy       = design_exit_policy_status_quo(group_cols = "est_id"),
#'   hiring_policy     = design_hiring_policy_flow(replacement_rate = 0.5,
#'                                                group_cols = "est_id"),
#'   salary_growth_rate  = 0.03,
#'   column_spec_args    = define_column_spec(salary_col = "base_salary_lcu"),
#'   output_control_args = define_output_control(
#'     scenario_name = "Retire at 65, replace half"
#'   ),
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
                                                                                                          max_years = 35,
                                                                                                          replacement_cap = 0.80)),
                             exit_policy        = NULL,
                             movement_policy    = NULL,
                             hiring_policy      = NULL,
                             allowance_policy   = NULL,
                             salary_growth_rate = 0,
                             pension_cola_rate  = salary_growth_rate,
                             allowance_growth_rate = 0,
                             baseline_date           = NULL,
                             column_spec_args   = define_column_spec(),
                             output_control_args = define_output_control(),
                             seed               = NULL) {

  # ====================================================================
  # 0. Validate and set up
  # ====================================================================

  # output_control_args comes from define_output_control()
  .check_output_control(output_control_args)
  scenario_name    <- output_control_args$scenario_name
  is_baseline      <- output_control_args$is_baseline
  return_microdata <- output_control_args$return_microdata

  # Everything the period loop needs: the starting tables cut to baseline_date,
  # the policies with their status-quo rates, the resolved column spec and
  # dates, and one rate per year
  inputs <- prepare_horizon_inputs(
    contract_dt           = contract_dt,
    personnel_dt          = personnel_dt,
    salary_scale_dt       = salary_scale_dt,
    n_periods             = n_periods,
    retirement_policy     = retirement_policy,
    exit_policy           = exit_policy,
    movement_policy       = movement_policy,
    hiring_policy         = hiring_policy,
    allowance_policy      = allowance_policy,
    salary_growth_rate    = salary_growth_rate,
    pension_cola_rate     = pension_cola_rate,
    allowance_growth_rate = allowance_growth_rate,
    baseline_date         = baseline_date,
    column_spec_args      = column_spec_args
  )
  ref_date_col <- inputs$column_spec_args$ref_date_col

  # The returned tables continue your own rows up to the baseline, unchanged
  # (ADR 0017), so they are taken here, before anything below replaces them
  if (return_microdata) {
    history_contracts <- .history_up_to(contract_dt, inputs$baseline_date, ref_date_col)
    history_personnel <- .history_up_to(personnel_dt, inputs$baseline_date, ref_date_col)
  }

  # The state each year hands to the next starts from the prepared inputs
  contract_dt        <- inputs$contract_dt
  personnel_dt       <- inputs$personnel_dt
  salary_scale_dt    <- inputs$salary_scale_dt
  pensioner_register <- inputs$pensioner_register
  hiring_policy      <- inputs$hiring_policy
  # The allowance ledger is built up year by year and returned. Like the
  # pensioner register it is never taken as an input: to chain runs, combine
  # their ledgers with rbindlist().
  allowance_dt       <- NULL

  # Keep the allowance policy as supplied for $metadata: the loop grows its
  # amounts each year
  allowance_policy_input <- allowance_policy

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

  # ====================================================================
  # PERIOD LOOP
  # ====================================================================
  # One summary row and one register snapshot per year, and each year's tables
  # when return_microdata = TRUE
  period_rows          <- vector("list", inputs$n_periods)
  register_panel_list  <- vector("list", inputs$n_periods)
  contract_panel_list  <- vector("list", inputs$n_periods)
  personnel_panel_list <- vector("list", inputs$n_periods)
  # Each year's rules run as of its start; what it leaves behind is the snapshot
  # at its end, a year later. Year 1 is never dated at the baseline itself.
  year_starts    <- seq(inputs$baseline_date, by = "year", length.out = inputs$n_periods + 1L)
  as_of_dates    <- year_starts[-length(year_starts)]
  snapshot_dates <- year_starts[-1L]

  for (t in seq_len(inputs$n_periods)) {

    growth        <- inputs$salary_growth_rate[t]
    cola_rate     <- inputs$pension_cola_rate[t]
    allow_growth  <- inputs$allowance_growth_rate[t]

    scenario_result <- simulate_scenario(
      contract_dt             = contract_dt,
      personnel_dt            = personnel_dt,
      salary_scale_dt         = salary_scale_dt,
      period_date             = as_of_dates[t],
      pensioner_register      = pensioner_register,
      retirement_policy       = inputs$retirement_policy,
      exit_policy             = inputs$exit_policy,
      movement_policy         = inputs$movement_policy,
      hiring_policy           = hiring_policy,
      allowance_policy        = allowance_policy,
      allowance_dt            = allowance_dt,
      salary_growth_rate      = growth,
      pension_cola_rate       = cola_rate,
      allowance_growth_rate   = allow_growth,
      column_spec_args        = inputs$column_spec_args,
      # Years are always whole: period_fraction is there for callers who chain
      # simulate_scenario() themselves
      period_fraction         = 1
    )

    # Thread state forward
    contract_dt          <- scenario_result$contract_dt
    personnel_dt         <- scenario_result$personnel_dt
    salary_scale_dt      <- scenario_result$salary_scale_dt
    pensioner_register   <- scenario_result$pensioner_register
    allowance_dt         <- scenario_result$allowance_dt
    allowance_policy     <- scenario_result$allowance_policy
    hiring_policy        <- scenario_result$hiring_policy

    # The year's snapshot: its summary row, this year's pensions, and the tables
    # as a govhr snapshot would show them (ADR 0017)
    period_rows[[t]] <- scenario_result$summary[, period_date := snapshot_dates[t]]
    register_panel_list[[t]] <- data.table::setcolorder(
      data.table::copy(pensioner_register)[, (ref_date_col) := snapshot_dates[t]],
      c(inputs$column_spec_args$personnel_id_col, inputs$column_spec_args$contract_id_col, ref_date_col))
    if (return_microdata) {
      contract_panel_list[[t]] <- snapshot_contracts(
        contract_dt, personnel_dt, pensioner_register, inputs$pension_contracts,
        snapshot_dates[t], inputs$column_spec_args)
      personnel_panel_list[[t]] <- snapshot_personnel(
        personnel_dt, inputs$pension_people, t, snapshot_dates[t], inputs$column_spec_args)
    }
  }

  # ====================================================================
  # Assemble output
  # ====================================================================
  summary_dt <- data.table::rbindlist(period_rows, use.names = TRUE, fill = TRUE)

  # Add _pct_of_end_bill share columns for each effect driver
  effect_cols <- c("exit_savings", "non_ret_exit_savings", "promotion_effect",
                   "transfer_effect",
                   "hiring_effect", "downsizing_savings", "inflation_effect")
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
    allowance_cost_end, n = 1L, fill = inputs$period0_allowance_cost
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
    summary_dt[, scenario_id    := scenario_name]
    summary_dt[, scenario_label := scenario_name]
  }
  summary_dt[, is_baseline := isTRUE(is_baseline)]

  # Move identity columns to the front for readability
  id_cols   <- intersect(c("scenario_id", "scenario_label", "is_baseline"),
                         names(summary_dt))
  other_cols <- setdiff(names(summary_dt), id_cols)
  data.table::setcolorder(summary_dt, c(id_cols, other_cols))

  # The policies and rates the run used: policies with their status-quo rates
  # filled in, before any year's growth; rates as supplied
  policy_args <- list(
    retirement_policy     = inputs$retirement_policy,
    exit_policy           = inputs$exit_policy,
    movement_policy       = inputs$movement_policy,
    hiring_policy         = inputs$hiring_policy,
    allowance_policy      = allowance_policy_input,
    salary_growth_rate    = salary_growth_rate,
    pension_cola_rate     = pension_cola_rate,
    allowance_growth_rate = allowance_growth_rate,
    n_periods             = inputs$n_periods,
    baseline_date         = inputs$baseline_date
  )

  out <- new_horizon(
    comparison = summary_dt,
    metadata   = list(
      policy_args   = policy_args,
      scenario_name = scenario_name,
      is_baseline   = isTRUE(is_baseline)
    )
  )

  # The pensioner register is always included: each year's pension per pension
  # contract, for pension cost audits. It is small: pensioners x years.
  out$pensioner_register <- data.table::rbindlist(register_panel_list, use.names = TRUE)

  if (return_microdata) {
    # Your rows up to the baseline, then one govhr snapshot per simulated year
    out$contract_dt  <- .continue_table(history_contracts, contract_panel_list)
    out$personnel_dt <- .continue_table(history_personnel, personnel_panel_list,
                                        also_keep = c(inputs$column_spec_args$age_col,
                                                      inputs$column_spec_args$tenure_col))
    # The allowance ledger, dated like the snapshots: each year's rows at its end.
    # NULL when there is no allowance policy. Only with return_microdata: one row
    # per contract x allowance type x year can get large.
    if (!is.null(allowance_dt)) {
      allowance_dt[, period_date := snapshot_dates[match(period_date, as_of_dates)]]
    }
    out$allowance_dt <- allowance_dt
  }

  out
}
