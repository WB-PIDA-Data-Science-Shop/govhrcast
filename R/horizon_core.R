# Core computations for simulate_scenario() / simulate_horizon(): the pieces of
# the wage-bill decomposition.

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

# ===========================================================================
# Status-quo rates, estimated once from the full panel before simulate_horizon()
# cuts it to the baseline date. Each takes a policy and returns it, with the
# rates filled in or unchanged.
# ===========================================================================

#' Fill In Status-Quo Exit Rates Before a Projection
#'
#' @description
#' \code{simulate_horizon()} calls this once, before it cuts the data to the
#' baseline date. A status-quo exit policy has no rates of its own; this
#' estimates them from the history in the panel, with
#' \code{\link{estimate_historical_exit_rates}()}, and puts them in
#' \code{exit_policy$policy_table}.
#'
#' Nothing is estimated when there is no exit policy, when it already holds a
#' rate table (your own), or when it is one flat rate with no groups (from
#' \code{\link{design_exit_policy_flat}()}).
#'
#' @param exit_policy The exit policy list, or \code{NULL} for no exits.
#' @param contract_dt,personnel_dt data.table. The full panel: every snapshot,
#'   not only the baseline date.
#' @param has_panel Logical. \code{TRUE} when the data hold at least two
#'   snapshots. Rates can only be estimated from a panel.
#' @param column_spec_args Column names, from \code{\link{define_column_spec}()}.
#'
#' @return \code{exit_policy}, with \code{policy_table} filled in if it was
#'   estimated, unchanged otherwise.  Stops with an error when rates are needed
#'   but there is no panel, or the estimate fails.
#'
#' @keywords internal
precompute_exit_rates <- function(exit_policy, contract_dt, personnel_dt,
                                  has_panel, column_spec_args) {
  # Nothing to estimate: no exits, the user's own rates, or one flat rate
  if (is.null(exit_policy) || !is.null(exit_policy$policy_table)) return(exit_policy)
  if (is.null(exit_policy$group_cols) && !is.null(exit_policy$defaults$exit_rate)) {
    return(exit_policy)
  }

  # Without rates there is no status quo to reproduce, so stop rather than
  # project on something else
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
  exit_policy
}

#' Fill In Status-Quo Movement Rates Before a Projection
#'
#' @description
#' \code{simulate_horizon()} calls this once, before it cuts the data to the
#' baseline date. Each period then sees one snapshot only, so without this
#' \code{\link{simulate_promotions_transfers}()} would never see the history it
#' estimates promotion and transfer rates from. The rates come from
#' \code{\link{estimate_historical_movement_rates}()} and go in
#' \code{movement_policy$policy_table}.
#'
#' Nothing is estimated when there is no movement policy or it already holds a
#' rate table (your own). Unlike exits, retirement and hiring, a missing panel
#' does not stop the run: the policy is returned unchanged, and
#' \code{simulate_promotions_transfers()} later reports that nobody moves.
#'
#' @inheritParams precompute_exit_rates
#' @param movement_policy The movement policy list, or \code{NULL} for no
#'   promotions or transfers.
#'
#' @return \code{movement_policy}, with \code{policy_table} filled in if it was
#'   estimated, unchanged otherwise.
#'
#' @keywords internal
precompute_movement_rates <- function(movement_policy, contract_dt,
                                      has_panel, column_spec_args) {
  # Nothing to estimate: no movement, or the user's own rates
  if (is.null(movement_policy) || !is.null(movement_policy$policy_table)) {
    return(movement_policy)
  }
  # No panel: leave it to simulate_promotions_transfers(), which moves nobody
  if (!has_panel) return(movement_policy)

  movement_policy$policy_table <- estimate_historical_movement_rates(
    contract_dt      = contract_dt,
    group_cols       = movement_policy$group_cols,
    column_spec_args = column_spec_args
  )
  movement_policy
}

#' Fill In Status-Quo Retirement Rates Before a Projection
#'
#' @description
#' \code{simulate_horizon()} calls this once, before it cuts the data to the
#' baseline date. Retirement with \code{eligibility_type = "hazard_rate"} needs
#' the share of people who retire at each age; this estimates those rates from
#' the panel, with \code{\link{estimate_retirement_hazard_rates}()}, and puts
#' them in \code{retirement_policy$policy_table}.
#'
#' Nothing is estimated for other eligibility types (age or tenure rules need no
#' rates), or when the policy already holds a rate table (your own).
#'
#' Rates are grouped by age at each snapshot. If the panel has no ages but has
#' birth dates, the age at each snapshot is worked out on a copy used only
#' here.
#'
#' @inheritParams precompute_exit_rates
#' @param retirement_policy The retirement policy list, or \code{NULL} for no
#'   retirements.
#' @param personnel_dt data.table. The full personnel panel: every snapshot,
#'   not only the baseline date.
#'
#' @return \code{retirement_policy}, with \code{policy_table} filled in if it was
#'   estimated, unchanged otherwise. Stops with an error when rates are needed
#'   but there is no panel.
#'
#' @keywords internal
precompute_retirement_rates <- function(retirement_policy, personnel_dt,
                                        has_panel, column_spec_args) {
  # Only hazard-rate retirement estimates anything, and only without the user's rates
  if (is.null(retirement_policy) ||
      !identical(retirement_policy$defaults$eligibility_type, "hazard_rate") ||
      !is.null(retirement_policy$policy_table)) {
    return(retirement_policy)
  }

  # A single snapshot cannot give a stable rate per age, and there is no
  # sensible default to fall back on
  if (!has_panel) {
    stop(
      "simulate_horizon: eligibility_type = 'hazard_rate' requires panel ",
      "data (>= 2 ref_date snapshots) to estimate hazard rates from -- a ",
      "single snapshot cannot estimate a stable per-age retirement rate.",
      call. = FALSE
    )
  }

  # Age at each snapshot's own date, when the panel has birth dates but no ages.
  # simulate_horizon() fills the age column later, after the panel is cut.
  age_col        <- column_spec_args$age_col
  birth_date_col <- column_spec_args$birth_date_col
  ref_date_col   <- column_spec_args$ref_date_col
  panel <- personnel_dt
  age_missing <- !(age_col %in% names(personnel_dt)) || all(is.na(personnel_dt[[age_col]]))
  if (age_missing && !is.null(birth_date_col) && birth_date_col %in% names(personnel_dt)) {
    panel <- data.table::copy(personnel_dt)
    panel[, (age_col) := as.numeric(
      difftime(get(ref_date_col), get(birth_date_col), units = "days")
    ) / 365.25]
  }

  retirement_policy$policy_table <- estimate_retirement_hazard_rates(
    panel_personnel_dt = panel,
    group_cols         = retirement_policy$group_cols,
    column_spec_args   = column_spec_args
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
  retirement_policy
}

#' Fill In Status-Quo Hiring Rates Before a Projection
#'
#' @description
#' \code{simulate_horizon()} calls this once, before it cuts the data to the
#' baseline date. Status-quo hiring (\code{mode = "status_quo"}) needs the
#' hiring rates seen in the panel; this estimates them with
#' \code{\link{estimate_historical_hiring_rates}()} and puts them in
#' \code{hiring_policy$squorate_dt}. A panel stored in the policy
#' (\code{panel_contract_dt} / \code{panel_personnel_dt}) is used instead of
#' the data, if there is one.
#'
#' Nothing is estimated for the other modes, which bring their own numbers, or
#' when the policy already holds a rate table (your own, from
#' \code{\link{design_hiring_policy_adjustment}()}).
#'
#' Either way, any panel stored in the policy is then dropped. The policy is
#' returned in the result's \code{$metadata}, so it should hold the small rate
#' table only, never person-level data.
#'
#' @inheritParams precompute_exit_rates
#' @param hiring_policy The hiring policy list, or \code{NULL} for no hiring.
#'
#' @return \code{hiring_policy}, with \code{squorate_dt} filled in if it was
#'   estimated and any stored panel removed. Stops with an error when rates are
#'   needed but there is no panel, or the estimate fails: pass
#'   \code{hiring_policy = NULL} to run without hiring.
#'
#' @keywords internal
precompute_hiring_rates <- function(hiring_policy, contract_dt, personnel_dt,
                                    has_panel, column_spec_args) {
  # Only status-quo hiring estimates rates; the other modes bring their own
  if (is.null(hiring_policy) || !identical(hiring_policy$mode, "status_quo")) {
    return(hiring_policy)
  }

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

  # The policy goes into $metadata: keep the rate table, not person-level data
  hiring_policy$panel_contract_dt  <- NULL
  hiring_policy$panel_personnel_dt <- NULL
  hiring_policy
}

#' Fill In What New Hires Look Like Before a Projection
#'
#' @description
#' \code{simulate_horizon()} calls this once, before it cuts the data to the
#' baseline date. New hires need an age, a gender and a contract type. Unless
#' the hiring policy brings its own, these come from the people hired during the
#' panel, with \code{\link{estimate_new_hire_profile}()}: one profile per hiring
#' group (\code{new_hire_profile}) and one for all hires together
#' (\code{new_hire_profile_overall}), used for groups with no recent hires.
#'
#' This runs for every hiring mode, not only status quo. Without a panel there
#' are no recent hires to learn from: new hires then start with a blank age and
#' gender, and a message says so.
#'
#' @inheritParams precompute_hiring_rates
#'
#' @return \code{hiring_policy}, with \code{new_hire_profile} and
#'   \code{new_hire_profile_overall} filled in where they were missing.
#'
#' @keywords internal
precompute_new_hire_profile <- function(hiring_policy, contract_dt, personnel_dt,
                                        has_panel, column_spec_args) {
  # Nothing to estimate: no hiring, or the user's own profiles
  if (is.null(hiring_policy)) return(hiring_policy)

  # The profile is estimated from the panel, if there is one, and the policy
  # brings no profile of its own. Otherwise it is left alone.
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
  hiring_policy
}

#' Prepare What simulate_horizon() Needs Before Its First Year
#'
#' @description
#' Everything \code{\link{simulate_horizon}()} does before its period loop: it
#' checks the inputs, fills in the status-quo rates from the full panel, cuts
#' the data to the starting snapshot, sets existing pensioners aside onto the
#' pensioner register, fills in age and tenure where they are missing, and
#' measures what allowances cost at the start.  It returns only what the loop
#' and the result need.
#'
#' @inheritParams simulate_horizon
#'
#' @return A named list:
#'   \describe{
#'     \item{contract_dt, personnel_dt}{The starting tables: the snapshot at
#'       \code{baseline_date}, with people who only hold a pension set aside,
#'       contracts renewed past their end date, and age and tenure filled in.}
#'     \item{salary_scale_dt}{A copy of the pay table.}
#'     \item{pensioner_register}{The register, holding the people already
#'       retired at \code{baseline_date}.}
#'     \item{retirement_policy, exit_policy, movement_policy, hiring_policy}{The
#'       policies, with status-quo rates and the new hire profile filled in.}
#'     \item{column_spec_args}{The column spec, with a \code{NULL} \code{age_col}
#'       or \code{tenure_col} replaced by \code{"age"} or
#'       \code{"personnel_tenure"}.}
#'     \item{baseline_date}{The starting date as a \code{Date}: the one passed
#'       in, or the latest snapshot.}
#'     \item{n_periods}{The number of years, as an integer.}
#'     \item{salary_growth_rate, pension_cola_rate, allowance_growth_rate}{One
#'       rate per year.}
#'     \item{period0_allowance_cost}{What allowances cost at the start; period
#'       1's \code{allowance_cost_start}.  Zero without an allowance policy.}
#'   }
#'
#' @keywords internal
prepare_horizon_inputs <- function(contract_dt,
                                   personnel_dt,
                                   salary_scale_dt,
                                   n_periods,
                                   retirement_policy,
                                   exit_policy,
                                   movement_policy,
                                   hiring_policy,
                                   allowance_policy,
                                   salary_growth_rate,
                                   pension_cola_rate,
                                   allowance_growth_rate,
                                   baseline_date,
                                   column_spec_args) {

  # ====================================================================
  # 0. Validate and set up
  # ====================================================================

  # column_spec_args comes from define_column_spec(). A malformed spec, or a
  # mapped column missing from the data, stops the run here, before any work
  # starts.
  .check_column_spec(column_spec_args)
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

  n_periods <- as.integer(n_periods)
  if (is.na(n_periods) || n_periods < 1L) {
    stop("n_periods must be a positive integer.", call. = FALSE)
  }

  # The projection starts from the date the caller names, or else the latest snapshot
  if (is.null(baseline_date)) baseline_date <- .latest_snapshot(contract_dt, personnel_dt, ref_date_col)
  baseline_date <- validate_date_format(baseline_date, "baseline_date")

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
  if (ref_date_col %in% names(contract_dt) && !baseline_date %in% contract_dt[[ref_date_col]]) {
    snapshots <- sort(unique(contract_dt[[ref_date_col]]))
    stop(
      "simulate_horizon: baseline_date (", format(baseline_date), ") is not a snapshot in contract_dt. ",
      "Pass one of its dates, normally the latest. It has ", length(snapshots),
      " snapshot(s) from ", format(min(snapshots)), " to ", format(max(snapshots)), ".",
      call. = FALSE
    )
  }

  # Status-quo rates and the new-hire profile come from the full panel, so they
  # are filled in now, before the panel is cut to the baseline date below. Each
  # returns its policy, filled in or unchanged.
  movement_policy   <- precompute_movement_rates(movement_policy, 
                                                 contract_dt, 
                                                 has_panel, 
                                                 column_spec_args)
  exit_policy       <- precompute_exit_rates(exit_policy, 
                                             contract_dt, 
                                             personnel_dt, 
                                             has_panel, 
                                             column_spec_args)
  retirement_policy <- precompute_retirement_rates(retirement_policy, 
                                                   personnel_dt, 
                                                   has_panel, 
                                                   column_spec_args)
  hiring_policy     <- precompute_hiring_rates(hiring_policy, 
                                               contract_dt, 
                                               personnel_dt, 
                                               has_panel, 
                                               column_spec_args)
  hiring_policy     <- precompute_new_hire_profile(hiring_policy, 
                                                   contract_dt, 
                                                   personnel_dt, 
                                                   has_panel, 
                                                   column_spec_args)

  # If data contains a ref_date panel column, subset to the starting snapshot
  # before stripping so that sub-modules see a single-period snapshot.
  if (ref_date_col %in% names(contract_dt)) {
    contract_dt <- contract_dt[get(ref_date_col) == baseline_date]
  }
  if (ref_date_col %in% names(personnel_dt)) {
    personnel_dt <- personnel_dt[get(ref_date_col) == baseline_date]
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

  # Initialise pensioner register with expanded schema
  pensioner_register <- .empty_pensioner_register(personnel_id_col)

  # Seed the register from people already retired at baseline_date, so their pensions
  # appear in period 1's pension_cost_total. Pensions sit in salary_col; several
  # rows add up, and a pensioner with no pay at all has a blank amount.
  if (nrow(existing_pensioners) > 0L) {
    seed_reg <- existing_pensioners[, {
      pension <- .total_pay(get(salary_col))
      list(
        pension_amount             = pension,
        final_salary               = pension,
        tenure_years_at_retirement = NA_real_,
        age_at_retirement          = NA_real_,
        period_date                = baseline_date
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
      age_lookup[, age_at_ret := as.numeric(
        difftime(baseline_date, get(birth_date_col), units = "days")
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
  # Age and tenure at baseline_date, where the data do not hold them
  # ====================================================================
  # age_col and tenure_col are always named by now (see the top), so only
  # birth_date_col can be missing.

  # Age: from birth_date_col, only when age_col is absent or entirely blank
  .age_missing_ <- !(age_col %in% names(personnel_dt)) ||
    all(is.na(personnel_dt[[age_col]]))

  if (.age_missing_ && !is.null(birth_date_col) &&
      birth_date_col %in% names(personnel_dt)) {
    personnel_dt[, (age_col) := as.numeric(
      difftime(baseline_date, get(birth_date_col), units = "days")
    ) / 365.25]
  }

  # Tenure: from contract history, only when tenure_col is absent or entirely blank
  .tenure_missing_ <- !(tenure_col %in% names(personnel_dt)) ||
                      all(is.na(personnel_dt[[tenure_col]]))

  if (.tenure_missing_) {
    tenure_init <- compute_tenure(
      contract_dt       = contract_dt,
      as_of_date          = baseline_date,
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

  # Real baseline allowance cost, before period 1 runs -- analogous to how
  # wage_bill_start reflects the already-existing salaries in the input data,
  # not zero. Used only as the fill value for period 1's allowance_cost_start
  # below; the allowance_dt this produces is discarded (period 1's real
  # ledger entry comes from the allowance step in the loop).
  period0_allowance_cost <- 0
  if (!is.null(allowance_policy)) {
    alw0 <- simulate_allowances(
      contract_dt   = contract_dt,
      personnel_dt  = personnel_dt,
      allowance_dt  = NULL,
      policy_params = allowance_policy,
      as_of_date      = baseline_date,
      column_spec_args = column_spec_args
    )
    period0_allowance_cost <- alw0$summary$allowance_cost_total
  }

  list(
    contract_dt            = contract_dt,
    personnel_dt           = personnel_dt,
    salary_scale_dt        = salary_scale_dt,
    pensioner_register     = pensioner_register,
    retirement_policy      = retirement_policy,
    exit_policy            = exit_policy,
    movement_policy        = movement_policy,
    hiring_policy          = hiring_policy,
    column_spec_args       = column_spec_args,
    baseline_date          = baseline_date,
    n_periods              = n_periods,
    salary_growth_rate     = salary_growth_rate,
    pension_cola_rate      = pension_cola_rate,
    allowance_growth_rate  = allowance_growth_rate,
    period0_allowance_cost = period0_allowance_cost
  )
}
