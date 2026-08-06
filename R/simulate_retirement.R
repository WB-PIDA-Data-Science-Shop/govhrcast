# Suppress R CMD check NOTEs for data.table bare column names.
utils::globalVariables(c(
  "eligible" # simulate_retirement: injected flag column used bare in [eligible == 1L]
))

#' Simulate Retirement Events for a Single Projection Period
#'
#' Simulates retirements for one projection period by identifying retirement-
#' eligible employees, computing pension entitlements, updating the active
#' workforce, and returning both the updated workforce and a complete retiree
#' ledger.
#'
#' @description
#' `simulate_retirement()` is the retirement module used throughout
#' **govhrcast**. During a single simulation period it:
#'
#' 1. Validates the supplied workforce data and retirement policy.
#' 2. Selects the appropriate workforce snapshot when panel data are supplied.
#' 3. Identifies employees who satisfy the retirement eligibility rules.
#' 4. Computes pension benefits for each retiree.
#' 5. Removes retirees from the active workforce.
#' 6. Returns updated workforce tables together with a summary of retirement
#'    outcomes.
#'
#' The function assumes that **all employees who become eligible retire during
#' the current simulation period**. Retirement probabilities or delayed
#' retirement behavior are not currently modeled.
#'
#' This function simulates a **single projection period**. Multi-period
#' workforce projections should instead use
#' [simulate_horizon()], which repeatedly calls
#' `simulate_retirement()` over successive simulation periods.
#'
#' @details
#'
#' ## Retirement workflow
#'
#' Retirement simulation proceeds through the following stages:
#'
#' 1. Validate all inputs.
#' 2. Select the workforce snapshot nearest (but not later than) `ref_date`
#'    when historical panel data are supplied.
#' 3. Determine retirement eligibility using the policy specified in
#'    `policy_params`.
#' 4. Assemble a retiree dataset containing the variables required for pension
#'    calculations.
#' 5. Resolve policy parameters (including any group-specific overrides).
#' 6. Compute pension entitlements.
#' 7. Update the contract and personnel registers.
#' 8. Produce summary statistics describing retirement activity.
#'
#' Input data are **never modified in place**. Both `contract_dt` and
#' `personnel_dt` are copied internally before any processing occurs.
#'
#' ## Policy specification
#'
#' Retirement behavior is controlled entirely through `policy_params`.
#' The policy is intentionally separated from the workforce data so that
#' alternative retirement systems can be evaluated without changing the
#' underlying administrative records.
#'
#' Policies may be:
#'
#' * Uniform across the workforce.
#' * Different by pay grade.
#' * Different by ministry.
#' * Different by establishment.
#' * Different by any combination of grouping variables.
#'
#' Group-specific rules are implemented through `policy_table`, while
#' `defaults` provide fallback values for employees whose group does not appear
#' in the policy table.
#'
#' ## Pension calculation
#'
#' Five pension formulas are currently supported:
#'
#' * `"db"` — Defined Benefit pension
#' * `"dc"` — Defined Contribution pension
#' * `"rate"` — Fixed percentage of reference salary
#' * `"flat"` — Fixed pension amount
#' * `"hybrid"` — Combined Defined Benefit and Defined Contribution pension
#'
#' The appropriate pension formula is selected automatically for each retiree
#' using the value of `pension_type`.
#'
#' ## Group-specific policies
#'
#' When `group_cols` is supplied, retirement parameters are resolved
#' separately for each employee group by joining `policy_table`
#' onto the retiree dataset.
#'
#' Any parameter omitted from `policy_table`
#' automatically falls back to the value supplied in `defaults`.
#'
#' @param policy_params A named list describing the retirement policy.
#'
#' The list contains three elements:
#'
#' **group_cols**
#'
#' Character vector identifying the variables that define policy groups.
#' Examples include `"paygrade"`, `"est_id"`,
#' or `c("paygrade","est_id")`.
#'
#' Use `NULL` when a single retirement policy applies to the entire workforce.
#'
#' **policy_table**
#'
#' A `data.table` containing group-specific policy overrides.
#'
#' Each row represents one policy group.
#' Columns may contain any retirement eligibility or pension parameter.
#'
#' Parameters omitted from a row automatically inherit the corresponding value
#' from `defaults`.
#'
#' Use `NULL` when all employees share the same retirement policy.
#'
#' **defaults**
#'
#' Named list containing the default retirement policy.
#'
#' The following fields are recognized:
#'
#' | Parameter | Description |
#' |:----------|:------------|
#' | `eligibility_type` | `"age_only"`, `"tenure_only"` or `"age_and_tenure"` |
#' | `min_age` | Minimum retirement age |
#' | `min_tenure` | Minimum years of service |
#' | `active_types` | Contract types considered active |
#' | `pension_type` | `"db"`, `"dc"`, `"rate"`, `"flat"` or `"hybrid"` |
#' | `ref_wage_col` | Salary column used as the pension reference wage |
#' | `accrual_rate` | Annual DB accrual rate |
#' | `max_years` | Maximum years counted toward DB accrual |
#' | `replacement_cap` | Maximum pension replacement rate |
#' | `balance_col` | Defined Contribution account balance column |
#' | `annuity_factor` | DC annuity conversion factor |
#' | `notional_rate` | Optional DC interest rate |
#' | `flat_amount` | Flat pension amount |
#' | `pension_rate` | Pension as a proportion of the reference wage |
#' 
#' @param contract_dt A `data.table` (or object coercible to a
#' `data.table`) containing the active workforce contract register in
#' **govhr** harmonized format.
#'
#' At a minimum, this table must contain the columns identified by
#' `personnel_id_col`, `contract_id_col`, `start_date_col`,
#' `end_date_col`, `contract_type_col`, and `salary_col`.
#'
#' The table may represent either:
#'
#' * a single workforce snapshot, or
#' * a longitudinal panel containing multiple `ref_date` values.
#'
#' When multiple snapshots are present, the snapshot nearest (but not later
#' than) `ref_date` is selected automatically.
#'
#' @param personnel_dt A `data.table` (or object coercible to a
#' `data.table`) containing the personnel register in **govhr**
#' harmonized format.
#'
#' The table must contain at least the personnel identifier and date of
#' birth columns specified by `personnel_id_col` and `birth_date_col`.
#'
#' If pre-computed age and tenure variables are supplied, they are used
#' directly to avoid unnecessary recalculation.
#'
#' @param ref_date A `Date` giving the simulation date for the current
#' projection period.
#'
#' This date is used to:
#'
#' * compute retirement eligibility,
#' * calculate employee age,
#' * calculate years of service,
#' * select the appropriate workforce snapshot, and
#' * stamp retirement dates onto closed contracts.
#'
#' @param ref_date_col Character scalar giving the name of the reference
#' date column used in workforce panel datasets.
#'
#' If this column is present and contains multiple unique dates,
#' `simulate_retirement()` automatically selects the snapshot nearest
#' (but not later than) `ref_date`.
#'
#' Ignored when only a single workforce snapshot is supplied.
#'
#' Default is `"ref_date"`.
#'
#' @param personnel_id_col Character scalar giving the unique personnel
#' identifier shared by both `contract_dt` and `personnel_dt`.
#'
#' Default is `"personnel_id"`.
#'
#' @param birth_date_col Character scalar giving the column containing each
#' employee's date of birth.
#'
#' Used to calculate age whenever `age_col` is unavailable or contains
#' missing values.
#'
#' Default is `"birth_date"`.
#'
#' @param contract_id_col Character scalar identifying unique contracts.
#'
#' Used internally when constructing employment histories and resolving
#' duplicate contract records.
#'
#' Default is `"contract_id"`.
#'
#' @param start_date_col Character scalar identifying the contract start
#' date column.
#'
#' Used when computing employee tenure.
#'
#' Default is `"start_date"`.
#'
#' @param end_date_col Character scalar identifying the contract end date
#' column.
#'
#' Active contracts should contain `NA` in this column.
#'
#' Retiring employees have this value replaced with `ref_date`.
#'
#' Default is `"end_date"`.
#'
#' @param salary_col Character scalar identifying the primary salary column.
#'
#' This column is used when preparing retiree records and when updating
#' contracts after retirement.
#'
#' Depending on the selected pension formula, pension calculations may
#' instead use the column specified by `policy_params$defaults$ref_wage_col`.
#'
#' Default is `"gross_salary_lcu"`.
#'
#' @param contract_type_col Character scalar identifying the contract type
#' variable.
#'
#' Only contract types listed in
#' `policy_params$defaults$active_types`
#' are considered eligible for retirement.
#'
#' Contracts belonging to retiring employees are reclassified as
#' `"pensioner"` before being returned.
#'
#' Default is `"contract_type"`.
#'
#' @param status_col Character scalar identifying the employment status
#' variable in `personnel_dt`.
#'
#' Personnel records corresponding to retiring employees are updated to
#' `"inactive"`.
#'
#' Default is `"employment_status"`.
#'
#' @param age_col Character scalar giving the name of a pre-computed age
#' variable.
#'
#' When this column exists and contains non-missing values, age is read
#' directly instead of being calculated from `birth_date_col`.
#'
#' Default is `"age"`.
#'
#' @param tenure_col Character scalar giving the name of a pre-computed
#' tenure variable measured in years.
#'
#' When available, this variable is used directly instead of reconstructing
#' tenure from contract histories.
#'
#' Default is `"tenure_years"`.
#' @return
#' A named list with four elements:
#'
#' * **summary**
#'
#'   A one-row `data.table` summarizing retirement outcomes for the
#'   current simulation period.
#'
#'   The table contains aggregate retirement statistics produced by
#'   [compute_retirement_summary()], such as the number of retirees,
#'   aggregate pension liabilities, and summary measures of retiree
#'   characteristics.
#'
#' * **contract_dt**
#'
#'   The updated contract register after retirement has been processed.
#'
#'   Contracts belonging to retiring employees are updated in place to
#'   reflect retirement (for example, contract status, end date and salary,
#'   where applicable).
#'
#' * **personnel_dt**
#'
#'   The updated personnel register after retirement has been processed.
#'
#'   Employees who retire during the current simulation period are marked
#'   as inactive.
#'
#' * **retirees_dt**
#'
#'   A `data.table` containing one row for every employee retiring during
#'   the current simulation period.
#'
#'   This table includes the variables required for pension calculation,
#'   the resolved retirement policy parameters applied to each retiree,
#'   and the computed pension benefit.
#'
#'   If no employees are eligible for retirement, this is returned as a
#'   zero-row `data.table`.
#'
#' @examples
#' \dontrun{
#'
#' library(data.table)
#' library(govhrcast)
#'
#' contract_dt  <- copy(bra_hrmis_contract)
#' personnel_dt <- copy(bra_hrmis_personnel)
#'
#' ref_date <- as.Date("2014-01-01")
#'
#' ############################################################
#' ## Example 1: Uniform age-based retirement with a
#' ##            rate-based pension
#' ############################################################
#'
#' policy_params <- list(
#'   group_cols = NULL,
#'   policy_table = NULL,
#'   defaults = list(
#'     eligibility_type = "age_only",
#'     min_age = 60,
#'     active_types = c(
#'       "permanent",
#'       "short-term",
#'       "fixed-term"
#'     ),
#'     pension_type = "rate",
#'     pension_rate = 0.15,
#'     ref_wage_col = "gross_salary_lcu"
#'   )
#' )
#'
#' results <- simulate_retirement(
#'   contract_dt = contract_dt,
#'   personnel_dt = personnel_dt,
#'   ref_date = ref_date,
#'   policy_params = policy_params
#' )
#'
#' results$summary
#' head(results$retirees_dt)
#'
#'
#' ############################################################
#' ## Example 2: Paygrade-specific defined benefit pension
#' ############################################################
#'
#' accrual_tbl <- data.table(
#'   paygrade = c("A", "B", "C", "D"),
#'   accrual_rate = c(0.018, 0.020, 0.022, 0.025)
#' )
#'
#' policy_params <- list(
#'   group_cols = "paygrade",
#'   policy_table = accrual_tbl,
#'   defaults = list(
#'     eligibility_type = "age_and_tenure",
#'     min_age = 60,
#'     min_tenure = 10,
#'     active_types = c("permanent", "fixed-term"),
#'     pension_type = "db",
#'     accrual_rate = 0.02,
#'     ref_wage_col = "gross_salary_lcu",
#'     max_years = 35,
#'     replacement_cap = 0.80
#'   )
#' )
#'
#' results <- simulate_retirement(
#'   contract_dt = contract_dt,
#'   personnel_dt = personnel_dt,
#'   ref_date = ref_date,
#'   policy_params = policy_params
#' )
#'
#' results$summary
#' head(results$retirees_dt)
#'
#' }
#'
#' @seealso
#'
#' [simulate_horizon()] for multi-period workforce simulations.
#'
#' [identify_eligibility()] for retirement eligibility determination.
#'
#' [compute_pension()] for pension benefit calculations.
#'
#' [resolve_policy_table()] for applying group-specific policy parameters.
#'
#' [compute_retirement_summary()] for retirement summary statistics.
#'
#' @family retirement simulation
#'
#' @export

simulate_retirement <- function(contract_dt,
                                personnel_dt,
                                ref_date,
                                policy_params = list(
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
                                ref_date_col = "ref_date",
                                personnel_id_col = "personnel_id",
                                birth_date_col = "birth_date",
                                contract_id_col = "contract_id",
                                start_date_col = "start_date",
                                end_date_col = "end_date",
                                salary_col = "gross_salary_lcu",
                                contract_type_col = "contract_type",
                                status_col         = "employment_status",
                                age_col    = "age",
                                tenure_col = "tenure_years") {
  
  # ========================================
  # 1. Input Validation
  check_retirement_inputs(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date,
    personnel_id_col = personnel_id_col,
    birth_date_col = birth_date_col,
    contract_id_col = contract_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col,
    status_col = status_col
  )
  
  # Convert to data.table and create working copies
  # We copy here to avoid modifying the user's input data
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
  
  # ========================================
  # 2. Select Nearest Reference Date
  # ========================================
  # Find the reference date in the data closest to (but not after) the specified ref_date
  if (ref_date_col %in% names(contract_dt)) {
    selected_ref_date <- select_nearest_ref_date(contract_dt[[ref_date_col]], ref_date)
    
    # Subset both datasets to the selected reference date
    contract_dt <- contract_dt[get(ref_date_col) == selected_ref_date]
    if (ref_date_col %in% names(personnel_dt)) {
      personnel_dt <- personnel_dt[get(ref_date_col) == selected_ref_date]
    }
  }
  
  # ========================================
  # 3. Identify eligible retirees
  # ========================================
  eligibility_dt <- identify_eligibility(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date,  # Use user's ref_date for age/tenure calculation
    personnel_id_col = personnel_id_col,
    birth_date_col = birth_date_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col,
    age_col    = age_col,
    tenure_col = tenure_col
  )
  
  # ========================================
  # 4. Prepare Retiree Data
  # ========================================
  retirees_dt <- prepare_retiree_data(
    eligibility_dt    = eligibility_dt,
    contract_dt       = contract_dt,
    personnel_dt      = personnel_dt,
    ref_date          = ref_date,  # Use user's ref_date
    personnel_id_col  = personnel_id_col,
    birth_date_col    = birth_date_col,
    contract_id_col   = contract_id_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    salary_col        = salary_col,
    contract_type_col = contract_type_col
  )
  
  # Handle case of no retirees
  if (nrow(retirees_dt) == 0) {
    summary_tbl <- data.table::data.table(
      n_retired = 0L,
      total_pension = 0,
      avg_pension = NA_real_,
      avg_age = NA_real_,
      avg_tenure = NA_real_
    )
    
    return(list(
      summary = summary_tbl,
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      retirees_dt = data.table::data.table()
    ))
  }
  
  # ========================================
  # 5. Compute Pensions
  # ========================================
  # Resolve all policy params (eligibility + pension) to per-row columns on
  # retirees_dt via a single policy_table join + defaults fill.
  .all_pension_params <- c(
    "pension_type", "accrual_rate", "ref_wage_col",
    "max_years", "replacement_cap",
    "balance_col", "annuity_factor", "notional_rate",
    "flat_amount", "pension_rate", "pension_formula"
  )

  .resolved_pension <- resolve_policy_table(
    policy_params,
    retirees_dt,
    .all_pension_params
  )
  for (.p in names(.resolved_pension))
    data.table::set(retirees_dt, j = .p, value = .resolved_pension[[.p]])

  data.table::set(retirees_dt, j = "pension", value = compute_pension(retirees_dt))
  
  # ========================================
  # 6. Update State
  # ========================================
  
  # Update contracts (modifies contract_dt in place)
  update_contracts_for_retirees(
    contract_dt = contract_dt,
    retirees_dt = retirees_dt,
    ref_date = ref_date,  # Use user's ref_date
    personnel_id_col = personnel_id_col,
    contract_id_col = contract_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    salary_col = salary_col,
    contract_type_col = contract_type_col
  )
  
  # Update personnel (modifies personnel_dt in place)
  update_personnel_for_retirees(
    personnel_dt = personnel_dt,
    contract_dt = contract_dt,
    personnel_id_col = personnel_id_col,
    contract_type_col = contract_type_col,
    status_col = status_col
  )
  
  # ========================================
  # 7. Compute Summary Statistics
  # ========================================
  summary_tbl <- compute_retirement_summary(
    retirees_dt = retirees_dt,
    contract_dt = contract_dt
  )
  
  # ========================================
  # 8. Return Results
  # ========================================
  return(list(
    summary = summary_tbl,
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    retirees_dt = retirees_dt
  ))
}

