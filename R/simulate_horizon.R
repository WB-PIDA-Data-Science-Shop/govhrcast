
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

# Internal helper: payroll total.
# Wage bill definition: sum of the pay on every contract row on the payroll (see
# .on_payroll()), per person, then across persons.  A person with two contracts
# contributes both salaries.  Pensions are excluded: their cost is tracked in
# pensioner_register.
# Used three times in simulate_scenario() for wage_bill_start, pre_cola, and
# wage_bill_end snapshots.  No roxygen: internal only, not exported.
.active_wage_bill <- function(contract_dt,
                              contract_type_col,
                              salary_col,
                              personnel_id_col) {
  contract_dt[
    .on_payroll(get(contract_type_col)) & !is.na(get(salary_col)),
    .(salary = sum(get(salary_col), na.rm = TRUE)),
    by = c(personnel_id_col)
  ][, sum(salary, na.rm = TRUE)]
}

# Internal helper: everyone who left this period, for flow hiring to replace. One
# row per person, in the group of their main contract. Retirees bring their group
# with them, because retiring has already zeroed the pay that helps pick the main
# contract. An exit leaves the contract's pay and dates as they were for picking.
.leavers_this_period <- function(retirees_dt, exits_dt, contract_dt, group_cols, column_spec_args) {
  personnel_id_col <- column_spec_args$personnel_id_col
  keep <- c(personnel_id_col, group_cols)

  retired <- if (!is.null(retirees_dt) && nrow(retirees_dt) > 0L) retirees_dt[, keep, with = FALSE]
  exited <- if (!is.null(exits_dt) && nrow(exits_dt) > 0L) {
    get_primary_contract(
      contract_dt      = contract_dt[get(personnel_id_col) %in% exits_dt[[personnel_id_col]]],
      personnel_id_col = personnel_id_col,
      contract_id_col  = column_spec_args$contract_id_col,
      start_date_col   = column_spec_args$start_date_col,
      salary_col       = column_spec_args$salary_col
    )[, keep, with = FALSE]
  }
  data.table::rbindlist(list(retired, exited), use.names = TRUE)
}

# Internal helper: tells the user once, per module, which groups on the payroll
# have no row in a rate table, so a frozen group is not a silent one.
.report_groups_without_rate <- function(module, consequence, rate_table, group_cols,
                                        contract_dt, contract_type_col) {
  group_cols <- setdiff(group_cols, "age_yr_int")
  if (is.null(rate_table) || length(group_cols) == 0L) return(invisible(NULL))

  group_key <- function(dt) do.call(paste, c(dt[, group_cols, with = FALSE], sep = "||"))
  staffed <- stats::na.omit(
    unique(contract_dt[.on_payroll(get(contract_type_col)), group_cols, with = FALSE])
  )
  rated <- if ("from_group" %in% names(rate_table)) rate_table$from_group else group_key(rate_table)
  missing_groups <- setdiff(group_key(staffed), rated)

  if (length(missing_groups) > 0L) {
    shown <- paste(utils::head(missing_groups, 5L), collapse = ", ")
    if (length(missing_groups) > 5L) shown <- paste0(shown, ", ...")
    message("simulate_horizon: ", module, ": ", length(missing_groups),
            " group(s) on the payroll have no rate in the table (", shown, "). ", consequence)
  }
  invisible(NULL)
}

# Internal helper: the pensioner register before anyone is on it, keyed by the
# caller's person ID column.
.empty_pensioner_register <- function(personnel_id_col) {
  register <- data.table::data.table(
    .id                        = character(0),
    pension_amount             = numeric(0),   # COLA-adjusted each period
    final_salary               = numeric(0),   # salary at moment of retirement
    tenure_years_at_retirement = numeric(0),
    age_at_retirement          = numeric(0),
    period_date                = as.Date(character(0))
  )
  data.table::setnames(register, ".id", personnel_id_col)
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
#' @param period_date Date.  The reference date for this simulation period.
#'   Stored in \code{pensioner_register} for cohort auditing and used as the
#'   \code{period_date} column in the returned summary row.
#' @param pensioner_register data.table or \code{NULL}.  Accumulated pensioner
#'   ledger from prior periods.  Must contain the person ID column (under the
#'   name \code{personnel_id_col} gives), \code{pension_amount} and
#'   \code{period_date}.  Pass \code{NULL} (default) for the first period --
#'   an empty register is initialised internally.
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
#' @param column_spec_args Named list.  Column names, if your data use
#'   different names from the \code{govhr} defaults: give only the ones you
#'   need to change, e.g. \code{list(salary_col = "base_salary_lcu")}.  See
#'   \code{\link{simulate_horizon}} for the keys and defaults.  Here,
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
#'     \item{pensioner_register}{Accumulated pensioner ledger including new
#'       retirees from this period.}
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
                               column_spec_args    = list(),
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

  column_spec_args <- .column_spec(column_spec_args)
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
    pensioner_register <- .empty_pensioner_register(personnel_id_col)
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
      ref_date                = period_date,
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

      # Append to pensioner register with period_date stamp
      new_reg <- data.table::data.table(
        .id                        = retirees_dt[[personnel_id_col]],
        pension_amount             = retirees_dt$pension,
        final_salary               = retirees_dt[[salary_col]],
        tenure_years_at_retirement = if ("tenure_years" %in% names(retirees_dt))
                                       retirees_dt$tenure_years else NA_real_,
        age_at_retirement          = if ("age" %in% names(retirees_dt))
                                       retirees_dt$age else NA_real_,
        period_date                = period_date
      )
      data.table::setnames(new_reg, ".id", personnel_id_col)
      pensioner_register <- data.table::rbindlist(
        list(pensioner_register, new_reg),
        use.names = TRUE, fill = TRUE
      )
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
      ref_date         = period_date,
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
      ref_date          = period_date,
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
      ref_date          = period_date,
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
      ref_date      = period_date,
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
#'   \item The new hire profile is estimated from the people hired during the
#'     panel (\code{\link{estimate_new_hire_profile}}).
#'   \item The data are cut to the starting snapshot at \code{ref_date}.  A
#'     \code{ref_date} that is not one of the snapshots stops with an error.
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
#'     \item{\code{ref_date_col}}{\code{"ref_date"}.  The snapshot date.  Also
#'       the column that stamps each year in the returned microdata.}
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
#'     \item{\code{gender_col}}{\code{"gender"}.  Used to give new hires a
#'       gender.  If your data have no such column, new hires have none.}
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
#'       plus allowances).  Also \code{period_date}, plus
#'       \code{scenario_id}, \code{scenario_label} and \code{is_baseline}
#'       when a \code{scenario_name} is given.}
#'     \item{\code{$summary_dt}}{The same table as \code{$comparison} (kept
#'       for backward compatibility).}
#'     \item{\code{$metadata}}{List recording the policies used (including the
#'       new hire profile, which can describe few people in small groups), plus
#'       \code{scenario_name} and \code{is_baseline} when given.}
#'     \item{\code{$pensioner_register}}{data.table, one row per retiree per
#'       year: the person ID (under your name for that column),
#'       \code{pension_amount}, \code{period_date} and details at retirement.  Always present
#'       (zero rows when nobody retired).  Nobody leaves the register: deaths are
#'       not modelled, so long projections overstate pension costs.  To chain several runs, combine
#'       their registers with \code{data.table::rbindlist()}.  This is
#'       person-level data: handle it like the input microdata before
#'       saving or sharing the result.}
#'     \item{\code{$contract_dt}, \code{$personnel_dt}}{Every projected
#'       year's microdata, stacked (one snapshot per year, stamped in
#'       \code{ref_date_col}).  Only when \code{return_microdata = TRUE}.  Also
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
#' pension <- pension_db(accrual_rate = 0.02, max_years = 35, replacement_cap = 0.80)
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

  # column_spec_args / output_control_args: the caller supplies only the keys
  # they want to override, e.g. column_spec_args = list(age_col = "age_yrs");
  # every other name keeps its govhr default. Unknown keys, malformed values,
  # two keys naming one column, and mapped columns missing from the data all
  # stop the run here, before any work starts.
  column_spec_args <- .column_spec(column_spec_args)
  .check_spec_columns(contract_dt, personnel_dt, column_spec_args)

  # Resolve NULL age_col / tenure_col to standard column names.
  # NULL means: "compute from source data and store in the default column".
  # Everything below receives a character scalar.
  if (is.null(column_spec_args$age_col))    column_spec_args$age_col    <- "age"
  if (is.null(column_spec_args$tenure_col)) column_spec_args$tenure_col <- "personnel_tenure"

  # The columns this function reads itself; the modules take the whole spec
  personnel_id_col  <- column_spec_args$personnel_id_col
  ref_date_col      <- column_spec_args$ref_date_col
  salary_col        <- column_spec_args$salary_col
  contract_type_col <- column_spec_args$contract_type_col
  age_col           <- column_spec_args$age_col
  tenure_col        <- column_spec_args$tenure_col
  birth_date_col    <- column_spec_args$birth_date_col

  output_control_args <- .output_control(output_control_args)
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

  # A panel has at least two snapshots; status-quo policies are estimated from it
  has_panel <- ref_date_col %in% names(contract_dt) &&
               data.table::uniqueN(contract_dt[[ref_date_col]]) >= 2L

  # The projection starts from a snapshot in the data. Between snapshots there is
  # nobody to project.
  if (ref_date_col %in% names(contract_dt) && !ref_date %in% contract_dt[[ref_date_col]]) {
    snapshots <- sort(unique(contract_dt[[ref_date_col]]))
    stop(
      "simulate_horizon: ref_date (", format(ref_date), ") is not a snapshot in contract_dt. ",
      "Pass one of its dates, normally the latest. It has ", length(snapshots),
      " snapshot(s) from ", format(min(snapshots)), " to ", format(max(snapshots)), ".",
      call. = FALSE
    )
  }

  # For movement, pre-compute the baseline from the full panel BEFORE stripping
  # ref_date.  simulate_horizon works on single-snapshot data in each period, so
  # without this cache simulate_promotions_transfers would never see a panel.
  if (!is.null(movement_policy) && is.null(movement_policy$policy_table)) {
    ### if we have a panel compute the movement baseline rates for transfers and
    ### promotions
    if (has_panel) {
      movement_policy$policy_table <- estimate_historical_movement_rates(
        contract_dt       = contract_dt,
        group_cols        = movement_policy$group_cols,
        column_spec_args  = column_spec_args
      )
    }
  }

  # For exit, pre-estimate historical rates from the full panel BEFORE stripping
  # ref_date, so simulate_exits() receives a ready-made policy_table. A flat rate
  # (defaults$exit_rate with no groups) is the user's own: nothing to estimate.
  # Without rates there is no status quo to reproduce, so this stops rather than
  # projecting on something else, as hiring and retirement do.
  .exit_is_flat_ <- is.null(exit_policy$group_cols) && !is.null(exit_policy$defaults$exit_rate)
  if (!is.null(exit_policy) && is.null(exit_policy$policy_table) && !.exit_is_flat_) {
    if (!has_panel) {
      stop(
        "simulate_horizon: status-quo exits need a panel with at least two ref_date ",
        "snapshots to estimate exit rates from, or your own rate from ",
        "design_exit_policy_flat() or design_exit_policy_adjustment().",
        call. = FALSE
      )
    }
    exit_policy$policy_table <- tryCatch(
      estimate_historical_exit_rates(
        panel_contract_dt  = contract_dt,
        panel_personnel_dt = personnel_dt,
        group_cols         = exit_policy$group_cols,
        column_spec_args   = column_spec_args
      ),
      error = function(e) stop(
        "simulate_horizon: could not estimate status-quo exit rates from the panel: ",
        conditionMessage(e), ". Check exit_policy$group_cols, or set the rates yourself ",
        "with design_exit_policy_adjustment().",
        call. = FALSE
      )
    )
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
        difftime(get(ref_date_col), get(birth_date_col), units = "days")
      ) / 365.25]
    }

    retirement_policy$policy_table <- estimate_retirement_hazard_rates(
      panel_personnel_dt = .hazard_panel_,
      group_cols          = retirement_policy$group_cols,
      column_spec_args    = column_spec_args
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

  # Status-quo hiring: unless the policy already holds a rate table (the
  # user's own, from design_hiring_policy_adjustment()), estimate the historical
  # hiring rates from the full panel now, before it is cut to the starting
  # snapshot below. A panel the user put in the policy (panel_contract_dt /
  # panel_personnel_dt) takes precedence. Either way the panels are then dropped
  # from the policy, so the policy -- which is returned in $metadata -- holds
  # only the small rate table, never person-level data.
  # Without rates there is no status quo to reproduce, so a failed estimate
  # or a missing panel stops the run with a clear error rather than
  # projecting zero hiring (pass hiring_policy = NULL to run without hiring).
  if (!is.null(hiring_policy) && identical(hiring_policy$mode, "status_quo")) {
    if (is.null(hiring_policy$squorate_dt)) {
      if (!has_panel) {
        stop(
          "simulate_horizon: status-quo hiring needs a panel with at least two ",
          "ref_date snapshots to estimate hiring rates from, or your own rates ",
          "from design_hiring_policy_adjustment().",
          call. = FALSE
        )
      }
      hiring_policy$squorate_dt <- tryCatch(
        estimate_historical_hiring_rates(
          panel_contract_dt  = hiring_policy$panel_contract_dt  %||% contract_dt,
          panel_personnel_dt = hiring_policy$panel_personnel_dt %||% personnel_dt,
          group_cols         = hiring_policy$group_cols,
          column_spec_args   = column_spec_args
        ),
        error = function(e) stop(
          "simulate_horizon: could not estimate status-quo hiring rates from the ",
          "panel: ", conditionMessage(e), ". Check hiring_policy$group_cols, or ",
          "set the rates yourself with design_hiring_policy_adjustment().",
          call. = FALSE
        )
      )
    }
    hiring_policy$panel_contract_dt  <- NULL
    hiring_policy$panel_personnel_dt <- NULL
  }

  # What new hires start with, from the people hired during the panel, unless the
  # policy brings its own.
  if (!is.null(hiring_policy)) {
    profile_from_panel <- function(group_cols) {
      if (!has_panel) return(NULL)
      estimate_new_hire_profile(contract_dt, personnel_dt, group_cols, column_spec_args)
    }
    if (is.null(hiring_policy$new_hire_profile)) {
      hiring_policy$new_hire_profile <- profile_from_panel(hiring_policy$group_cols)
    }
    if (is.null(hiring_policy$new_hire_profile_overall)) {
      hiring_policy$new_hire_profile_overall <- profile_from_panel(NULL)
    }
    if (is.null(hiring_policy$new_hire_profile) || nrow(hiring_policy$new_hire_profile) == 0L) {
      message(
        "simulate_horizon: no recent hires in the data to base new hires on, so they start with a ",
        "blank age and gender, and will not retire on age. Set hiring_policy$new_hire_profile ",
        "to say what they look like: see ?estimate_new_hire_profile."
      )
    }
  }

  if (ref_date_col %in% names(contract_dt)) {
    contract_dt <- contract_dt[get(ref_date_col) == start_ref]
  }
  if (ref_date_col %in% names(personnel_dt)) {
    personnel_dt <- personnel_dt[get(ref_date_col) == start_ref]
  }

  # Pension rows: the contract type says so, or the person's status does. Every
  # other row is staff, so someone with a pension and a working contract goes on
  # the register and stays on the payroll. They are set aside here, and leave the
  # working tables below once they are on the register.
  .pensioner_ids_ <- personnel_dt[get(column_spec_args$status_col) %in% "pensioner"][[personnel_id_col]]
  .is_pension_row_ <- contract_dt[[contract_type_col]] %in% "pensioner" |
    contract_dt[[personnel_id_col]] %in% .pensioner_ids_
  existing_pensioners <- contract_dt[.is_pension_row_]

  # Staff rows with no pay are people the government no longer pays, whose contract
  # lingers in the data: they must not inflate headcount.
  contract_dt <- contract_dt[!.is_pension_row_ & !is.na(get(salary_col))]

  # A blank contract type is missing data: staff if the person's status is active,
  # off the payroll otherwise.
  .active_ids_ <- personnel_dt[get(column_spec_args$status_col) %in% "active"][[personnel_id_col]]
  contract_dt[is.na(get(contract_type_col)) & !get(personnel_id_col) %in% .active_ids_,
              (contract_type_col) := "inactive"]

  # Contracts are renewed past their end date in the projection (decision D3):
  # everyone paid today stays until a module says otherwise.
  contract_dt[.on_payroll(get(contract_type_col)),
              (column_spec_args$end_date_col) := as.Date(NA)]

  # Say which groups have no rate, once per run, rather than freezing them silently
  .report_no_rate <- function(module, consequence, rate_table, group_cols) {
    .report_groups_without_rate(module, consequence, rate_table, group_cols, contract_dt, contract_type_col)
  }
  .report_no_rate("exits", "Nobody exits from them.", exit_policy$policy_table, exit_policy$group_cols)
  .report_no_rate("movement", "Nobody moves out of them.", movement_policy$policy_table, movement_policy$group_cols)
  .report_no_rate("retirement", "Nobody retires from them before any mandatory age.",
                  retirement_policy$policy_table, retirement_policy$group_cols)
  .report_no_rate("hiring", "They use the policy's unmatched_rate, 0 unless you set it.",
                  hiring_policy$squorate_dt, hiring_policy$group_cols)
  .report_no_rate("allowances", "They get the policy's default amount.",
                  allowance_policy$policy_table, setdiff(allowance_policy$group_cols, "allowance_type"))

  # Initialise the allowance ledger. Unlike pensioner_register, this is never
  # accepted as a simulate_horizon() input -- it accumulates purely within
  # this call and is returned in the output, matching the documented
  # constraint that a register is never accepted back in (chaining across
  # separate simulate_horizon() calls is the user's responsibility via
  # rbindlist(), same as for pensioner_register).
  allowance_dt <- NULL

  # Initialise pensioner register with expanded schema
  pensioner_register <- .empty_pensioner_register(personnel_id_col)

  # Seed the register from people already retired at ref_date, so their pensions
  # appear in period 1's pension_cost_total. Pensions sit in salary_col; several
  # rows add up, and a pensioner with no pay at all has a blank amount.
  if (nrow(existing_pensioners) > 0L) {
    .ref_date_seed_ <- ref_date  # alias \u2014 avoid data.table column shadowing
    seed_reg <- existing_pensioners[, {
      pension <- .total_pay(get(salary_col))
      list(
        pension_amount             = pension,
        final_salary               = pension,
        tenure_years_at_retirement = NA_real_,
        age_at_retirement          = NA_real_,
        period_date                = .ref_date_seed_
      )
    }, by = c(personnel_id_col)]

    # Enrich seed with age at retirement from birth_date in personnel_dt.
    # Tenure is not reliably available for pre-existing pensioners (their
    # contracts are already closed), so leave tenure_years_at_retirement as NA.
    if (!is.null(birth_date_col) && birth_date_col %in% names(personnel_dt)) {
      age_lookup <- personnel_dt[
        get(personnel_id_col) %in% seed_reg[[personnel_id_col]],
        c(personnel_id_col, birth_date_col), with = FALSE
      ]
      .ref_seed_age_ <- ref_date
      age_lookup[, age_at_ret := as.numeric(
        difftime(.ref_seed_age_, get(birth_date_col), units = "days")
      ) / 365.25]
      seed_reg[age_lookup, age_at_retirement := i.age_at_ret,
               on = personnel_id_col]
    }

    pensioner_register <- data.table::rbindlist(
      list(pensioner_register, seed_reg),
      use.names = TRUE, fill = TRUE
    )

    .n_no_pension_ <- sum(is.na(seed_reg$pension_amount))
    if (.n_no_pension_ > 0L) {
      message("simulate_horizon: ", .n_no_pension_, " existing pensioner(s) have no value in '",
              salary_col, "'. They are on the pensioner register with a blank amount, and ",
              "add nothing to the pension cost.")
    }

    # People who only hold a pension leave the working tables too; someone who
    # also has a working contract stays.
    personnel_dt <- personnel_dt[
      !get(personnel_id_col) %in% setdiff(existing_pensioners[[personnel_id_col]],
                                          contract_dt[[personnel_id_col]])
    ]
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
      contract_id_col   = column_spec_args$contract_id_col,
      start_date_col    = column_spec_args$start_date_col,
      end_date_col      = column_spec_args$end_date_col,
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
      column_spec_args = column_spec_args
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

  # Keep the hiring policy as supplied for $metadata: the loop below grows the
  # pay table inside it each year.
  hiring_policy_input <- hiring_policy

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
      column_spec_args        = column_spec_args,
      period_fraction         = period_fraction
    )

    # Thread state forward
    contract_dt        <- scenario_result$contract_dt
    personnel_dt       <- scenario_result$personnel_dt
    salary_scale_dt    <- scenario_result$salary_scale_dt
    pensioner_register <- scenario_result$pensioner_register
    allowance_dt        <- scenario_result$allowance_dt
    allowance_policy     <- scenario_result$allowance_policy
    hiring_policy        <- scenario_result$hiring_policy

    # Accumulate period snapshots for panel output
    if (return_microdata) {
      ct_snap <- data.table::copy(contract_dt)
      ct_snap[, (ref_date_col) := cur_date]
      contract_panel_list[[t]] <- ct_snap

      pt_snap <- data.table::copy(personnel_dt)
      pt_snap[, (ref_date_col) := cur_date]
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
    hiring_policy      = hiring_policy_input,
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
