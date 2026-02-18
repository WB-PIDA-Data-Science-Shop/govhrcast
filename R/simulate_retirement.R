#' Simulate Retirement Module
#'
#' @description
#' Main user-facing function for simulating retirement events. Orchestrates
#' the entire retirement workflow: eligibility determination, pension calculation,
#' and state updates.
#'
#' @import data.table
#'
#' @param contract_dt data.table. Contract data in govhr harmonized format
#' @param personnel_dt data.table. Personnel data in govhr harmonized format
#' @param policy_params List. Retirement policy parameters including:
#'   \describe{
#'     \item{eligibility_type}{Character. One of: "age_only", "tenure_only", "age_and_tenure"}
#'     \item{min_age}{Numeric. Minimum retirement age (required for age-based eligibility)}
#'     \item{min_tenure}{Numeric. Minimum years of service (required for tenure-based eligibility)}
#'     \item{pension_type}{Character. One of: "db", "dc", "flat", "hybrid"}
#'     \item{pension_params}{List. Parameters specific to the chosen pension type}
#'   }
#' @param ref_date Date. Reference date for retirement simulation
#'
#' @return List containing:
#'   \describe{
#'     \item{summary}{data.table with retirement summary statistics}
#'     \item{contract_dt}{Updated contract data with retirees marked}
#'     \item{personnel_dt}{Updated personnel data with retirees marked inactive}
#'     \item{retirees_dt}{data.table of retirees with pension amounts}
#'   }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(govhr)
#'
#' # Load example data
#' contract_dt <- as.data.table(govhr::bra_hrmis_contract)
#' personnel_dt <- as.data.table(govhr::bra_hrmis_personnel)
#'
#' # Define retirement policy
#' policy_params <- list(
#'   eligibility_type = "age_and_tenure",
#'   min_age = 60,
#'   min_tenure = 20,
#'   pension_type = "db",
#'   pension_params = list(
#'     accrual_rate = 0.02,
#'     ref_wage_col = "gross_salary_lcu",
#'     max_years = 35,
#'     replacement_cap = 0.80
#'   )
#' )
#'
#' # Run simulation
#' results <- simulate_retirement(
#'   contract_dt = contract_dt,
#'   personnel_dt = personnel_dt,
#'   policy_params = policy_params,
#'   ref_date = as.Date("2025-01-01")
#' )
#'
#' # View results
#' results$summary
#' results$retirees_dt
#' }
#'
#' @export
simulate_retirement <- function(contract_dt,
                               personnel_dt,
                               policy_params,
                               ref_date) {
  
  # ========================================
  # 1. Input Validation
  # ========================================
  check_retirement_inputs(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # Convert to data.table if not already
  if (!data.table::is.data.table(contract_dt)) {
    contract_dt <- data.table::as.data.table(contract_dt)
  }
  if (!data.table::is.data.table(personnel_dt)) {
    personnel_dt <- data.table::as.data.table(personnel_dt)
  }
  
  # ========================================
  # 2. Identify Retirees
  # ========================================
  eligibility_dt <- identify_retirees(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date
  )
  
  # ========================================
  # 3. Prepare Retiree Data
  # ========================================
  retirees_dt <- prepare_retiree_data(
    eligibility_dt = eligibility_dt,
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    ref_date = ref_date
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
  # 4. Compute Pensions
  # ========================================
  retirees_dt[, pension := compute_pension(
    retirees_dt = .SD,
    policy_type = policy_params$pension_type,
    params = policy_params$pension_params
  )]
  
  # ========================================
  # 5. Update State
  # ========================================
  
  # Update contracts
  updated_contract_dt <- update_contracts_for_retirees(
    contract_dt = contract_dt,
    retirees_dt = retirees_dt,
    ref_date = ref_date,
    personnel_id_col = "personnel_id",
    contract_id_col = "contract_id",
    start_date_col = "start_date",
    end_date_col = "end_date",
    salary_col = "gross_salary_lcu",
    contract_type_col = "contract_type_code"
  )
  
  # Update personnel
  updated_personnel_dt <- update_personnel_for_retirees(
    personnel_dt = personnel_dt,
    contract_dt = updated_contract_dt,
    personnel_id_col = "personnel_id",
    contract_type_col = "contract_type_code",
    status_col = "status"
  )
  
  # ========================================
  # 6. Compute Summary Statistics
  # ========================================
  summary_tbl <- compute_retirement_summary(
    retirees_dt = retirees_dt,
    contract_dt = contract_dt
  )
  
  # ========================================
  # 7. Return Results
  # ========================================
  return(list(
    summary = summary_tbl,
    contract_dt = updated_contract_dt,
    personnel_dt = updated_personnel_dt,
    retirees_dt = retirees_dt
  ))
}
