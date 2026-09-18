#' Simulate Non-Retirement Exits for a Single Projection Period
#'
#' Simulates voluntary resignations, dismissals, and contract non-renewals
#' for one projection period by applying group-level or scalar attrition
#' rates to the active workforce, updating the contract and personnel
#' registers, and returning a summary of exit outcomes.
#'
#' @description
#' `simulate_exits()` is the non-retirement attrition module used throughout
#' **govhrcast**. During a single simulation period it:
#'
#' 1. Validates the supplied workforce data and exit policy.
#' 2. Identifies the active workforce eligible for non-retirement attrition.
#' 3. Resolves per-group exit rates from the policy specification.
#' 4. Selects employees to exit using the chosen exit strategy.
#' 5. Records the salary savings attributable to exits.
#' 6. Removes exiting employees from the active workforce.
#' 7. Returns updated workforce tables together with a summary of exit
#'    outcomes.
#'
#' This function simulates a **single projection period**. Multi-period
#' workforce projections should instead use [simulate_horizon()], which
#' repeatedly calls `simulate_exits()` over successive simulation periods,
#' always after [simulate_retirement()] has already removed retirees from
#' the active pool.
#'
#' @details
#'
#' ## Exit workflow
#'
#' Non-retirement exit simulation proceeds through the following stages:
#'
#' 1. Validate all inputs.
#' 2. Identify the active workforce by filtering on `active_types` within
#'    `contract_type_col`. This filter must match actual values in
#'    `contract_type_col` — passing `employment_status` values such as
#'    `"active"` will silently produce zero exits.
#' 3. Resolve exit rates from `policy_params` — either a scalar rate applied
#'    uniformly or group-specific rates joined from `policy_table`.
#' 4. Compute the number of exits per group as
#'    `round(n_active * exit_rate)`.
#' 5. Select which employees exit using the chosen `exit_strategy`.
#' 6. Attach pre-exit salaries to compute savings.
#' 7. Update contract and personnel registers in place.
#' 8. Return updated data and summary statistics.
#'
#' Input data are **never modified in place**. Both `contract_dt` and
#' `personnel_dt` are copied internally before any processing occurs.
#'
#' ## Policy specification
#'
#' Exit behavior is controlled entirely through `policy_params`. Two
#' dispatch paths are available depending on whether `policy_table` is
#' supplied:
#'
#' * **Status quo mode** (`policy_table` supplied): group-level exit rates
#'   estimated from historical panel data are applied to each group. Rates
#'   can additionally be scaled by an `exit_multiplier` column in
#'   `policy_table` to model reform scenarios without altering the
#'   underlying historical estimates.
#' * **Fixed rate mode** (`policy_table = NULL`): a single scalar
#'   `exit_rate` from `defaults` is applied uniformly across the entire
#'   active workforce.
#'
#' When `simulate_horizon()` is called with `policy_table = NULL` and
#' panel data are supplied, historical exit rates are estimated
#' automatically from the panel via [estimate_historical_exit_rates()]
#' before the period loop begins.
#'
#' ## Exit strategy
#'
#' The `exit_strategy` parameter in `defaults` controls which employees
#' are selected to exit once the number of exits per group has been
#' determined. Two options are supported:
#'
#' * `"random"` — employees are drawn uniformly at random from the
#'   eligible pool (default).
#' * Any numeric column name present in `contract_dt` — employees are
#'   ranked in ascending order of that column and the lowest-ranked exit
#'   first. For example, `"gross_salary_lcu"` exits the lowest-paid
#'   first; `"age"` exits the youngest first; `"personnel_tenure"` exits
#'   the least tenured first. An unrecognised column name silently falls
#'   back to `"random"`.
#'
#' ## Interaction with retirement
#'
#' Within each simulation period, [simulate_retirement()] runs before
#' `simulate_exits()`. The contract and personnel tables passed to
#' `simulate_exits()` therefore reflect a workforce from which retirees
#' have already been removed. Exit rates are applied to this post-retirement
#' active pool.
#'
#' @param policy_params A named list describing the exit policy.
#'
#' The list contains three elements:
#'
#' **group_cols**
#'
#' Character vector identifying the variables that define policy groups.
#' Examples include `"est_id"`, `"paygrade"`, or `c("est_id", "paygrade")`.
#'
#' Use `NULL` when a single scalar exit rate applies to the entire
#' workforce.
#'
#' **policy_table**
#'
#' A `data.table` containing group-specific exit rates, keyed on
#' `group_cols`. Must contain an `exit_rate` column. May optionally
#' contain an `exit_multiplier` column to scale historical rates for
#' reform scenarios without modifying the underlying estimates.
#'
#' Pass the output of [estimate_historical_exit_rates()] here for
#' status quo projections.
#'
#' Use `NULL` to apply a flat scalar rate to the entire workforce.
#'
#' **defaults**
#'
#' Named list containing default exit policy parameters. The following
#' fields are recognised:
#'
#' | Parameter | Description |
#' |:----------|:------------|
#' | `exit_rate` | Scalar attrition rate applied when `policy_table = NULL`, or as the fallback rate for groups absent from `policy_table` |
#' | `exit_strategy` | `"random"` or a numeric column name in `contract_dt` to rank employees for exit selection |
#' | `active_types` | Contract type values in `contract_type_col` treated as eligible for exit. Must match actual `contract_type` values, not `employment_status` values. Default `c("permanent", "fixed-term", "short-term")` — the non-terminal `contract_type` values per the `govhr` harmonized dictionary |
#' | `exited_type` | Value written to `contract_type_col` after exit. Default `"inactive"` |
#'
#' @param contract_dt A `data.table` (or object coercible to a
#' `data.table`) containing the active workforce contract register in
#' **govhr** harmonized format.
#'
#' At a minimum, this table must contain the columns identified by
#' `personnel_id_col`, `contract_id_col`, `start_date_col`,
#' `end_date_col`, `contract_type_col`, and `salary_col`.
#'
#' This table should be a single-period snapshot. When called through
#' [simulate_horizon()], retirees have already been removed before this
#' function is called.
#'
#' @param personnel_dt A `data.table` (or object coercible to a
#' `data.table`) containing the personnel register in **govhr**
#' harmonized format.
#'
#' Must contain at least the columns identified by `personnel_id_col`
#' and `status_col`.
#'
#' @param ref_date A `Date` giving the simulation date for the current
#' projection period. Used to stamp exit dates onto closed contracts.
#'
#' @param personnel_id_col Character scalar giving the unique personnel
#' identifier shared by both `contract_dt` and `personnel_dt`.
#' Default is `"personnel_id"`.
#'
#' @param birth_date_col Character scalar giving the column containing each
#' employee's date of birth. Default is `"birth_date"`.
#'
#' @param start_date_col Character scalar identifying the contract start
#' date column. Default is `"start_date"`.
#'
#' @param contract_id_col Character scalar identifying unique contracts.
#' Default is `"contract_id"`.
#'
#' @param contract_type_col Character scalar identifying the contract type
#' variable. Only contract types listed in `policy_params$defaults$active_types`
#' are eligible for exit selection. Exited contracts are reclassified to the
#' value in `policy_params$defaults$exited_type` (default `"inactive"`).
#' Default is `"contract_type"`.
#'
#' @param status_col Character scalar identifying the employment status
#' variable in `personnel_dt`. Personnel records for exiting employees are
#' updated to `"inactive"`. Default is `"employment_status"`.
#'
#' @param salary_col Character scalar identifying the primary salary column.
#' Used to compute salary savings attributable to exits.
#' Default is `"gross_salary_lcu"`.
#'
#' @param end_date_col Character scalar identifying the contract end date
#' column. Active contracts contain `NA` here. Exiting employees have this
#' value replaced with `ref_date`. Default is `"end_date"`.
#'
#' @return
#' A named list with four elements:
#'
#' * **summary**
#'
#'   A one-row `data.table` summarizing exit outcomes for the current
#'   simulation period, containing:
#'
#'   | Column | Description |
#'   |:-------|:------------|
#'   | `n_exits` | Number of employees who exited |
#'   | `exit_savings` | Total salary savings from exits |
#'
#' * **contract_dt**
#'
#'   The updated contract register after exits have been processed.
#'   Exited contracts have `contract_type_col` set to `exited_type` and
#'   `end_date_col` set to `ref_date`.
#'
#' * **personnel_dt**
#'
#'   The updated personnel register. Employees who exit during the current
#'   simulation period are marked as inactive.
#'
#' * **exits_dt**
#'
#'   A `data.table` containing one row for every employee who exits during
#'   the current simulation period, including their salary at the time of
#'   exit. Returns an empty `data.table` when no exits occur.
#'
#' @examples
#' \dontrun{
#'
#' library(data.table)
#' library(govhrcast)
#'
#' contract_dt  <- copy(bra_hrmis_contract)
#' personnel_dt <- copy(bra_hrmis_personnel)
#' ref_date     <- as.Date("2014-01-01")
#'
#' ############################################################
#' ## Example 1: Flat scalar attrition rate
#' ############################################################
#'
#' exit_policy <- list(
#'   group_cols   = NULL,
#'   policy_table = NULL,
#'   defaults = list(
#'     exit_rate     = 0.05,
#'     exit_strategy = "random",
#'     active_types  = c("permanent", "short-term", "fixed-term"),
#'     exited_type   = "inactive"
#'   )
#' )
#'
#' results <- simulate_exits(
#'   contract_dt   = contract_dt,
#'   personnel_dt  = personnel_dt,
#'   policy_params = exit_policy,
#'   ref_date      = ref_date
#' )
#'
#' results$summary
#' head(results$exits_dt)
#'
#' ############################################################
#' ## Example 2: Group-level historical rates with a reform
#' ##            multiplier and tenure-based exit selection
#' ############################################################
#'
#' rates_dt <- estimate_historical_exit_rates(
#'   panel_contract_dt  = contract_dt,
#'   panel_personnel_dt = personnel_dt,
#'   group_cols         = "est_id"
#' )
#'
#' # Reduce exits by 50% for one establishment
#' rates_dt[, exit_multiplier := ifelse(
#'   est_id == "SECRETARIA DE ESTADO DA SAUDE", 0.5, 1.0
#' )]
#'
#' exit_policy <- list(
#'   group_cols   = "est_id",
#'   policy_table = rates_dt,
#'   defaults = list(
#'     exit_rate     = mean(rates_dt$exit_rate, na.rm = TRUE),
#'     exit_strategy = "personnel_tenure",
#'     active_types  = c("permanent", "short-term", "fixed-term"),
#'     exited_type   = "inactive"
#'   )
#' )
#'
#' results <- simulate_exits(
#'   contract_dt   = contract_dt,
#'   personnel_dt  = personnel_dt,
#'   policy_params = exit_policy,
#'   ref_date      = ref_date
#' )
#'
#' results$summary
#'
#' }
#'
#' @seealso
#'
#' [simulate_horizon()] for multi-period workforce simulations.
#'
#' [estimate_historical_exit_rates()] for estimating group-level attrition
#' rates from panel data to supply as `policy_table`.
#'
#' [simulate_retirement()] for the retirement module, which runs before
#' this function within each simulation period.
#'
#' [simulate_hiring()] for the hiring module, which runs after this
#' function within each simulation period.
#'
#' @family exit simulation
#'
#' @export

simulate_exits <- function(contract_dt,
                           personnel_dt,
                           policy_params,
                           ref_date,
                           personnel_id_col  = "personnel_id",
                           birth_date_col    = "birth_date",
                           start_date_col    = "start_date",
                           contract_id_col   = "contract_id",
                           contract_type_col = "contract_type",
                           status_col         = "employment_status",
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
  active_types  <- policy_params$defaults$active_types  %||%
    c("permanent", "fixed-term", "short-term")
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
