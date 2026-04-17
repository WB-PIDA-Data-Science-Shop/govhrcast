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
#' @param policy_params List. Retirement policy parameters in unified format:
#'   \describe{
#'     \item{defaults}{Named list of scalar fallbacks.  Common keys:
#'       \code{eligibility_type} (\code{"age_only"}, \code{"tenure_only"},
#'       \code{"age_and_tenure"}), \code{min_age}, \code{min_tenure},
#'       \code{pension_type} (\code{"db"}, \code{"dc"}, \code{"flat"},
#'       \code{"hybrid"}), \code{accrual_rate}, \code{ref_wage_col},
#'       \code{max_years}, \code{replacement_cap}, \code{balance_col},
#'       \code{annuity_factor}, \code{flat_amount}.}
#'     \item{group_cols}{Character vector of contract columns to join on, or
#'       \code{NULL}.}
#'     \item{policy_table}{data.table with \code{group_cols} plus any
#'       per-group parameter columns; or \code{NULL}.}
#'   }
#'   When \code{policy_params} is omitted, a sensible DB
#'   \code{age_and_tenure} default is used (min_age = 60, min_tenure = 10,
#'   accrual_rate = 0.02, max_years = 35, replacement_cap = 0.80).
#' @param ref_date Date. Reference date for retirement simulation
#' @param ref_date_col Character. Name of reference date column in panel data (default: "ref_date")
#' @param personnel_id_col Character. Name of personnel ID column (default: "personnel_id")
#' @param birth_date_col Character. Name of birth date column (default: "birth_date")
#' @param contract_id_col Character. Name of contract ID column (default: "contract_id")
#' @param start_date_col Character. Name of start date column (default: "start_date")
#' @param end_date_col Character. Name of end date column (default: "end_date")
#' @param salary_col Character. Name of salary column (default: "gross_salary_lcu")
#' @param contract_type_col Character. Name of contract type column (default: "contract_type_code")
#' @param status_col Character. Name of status column (default: "status")
#' @param age_col Character. Name of the age column used for eligibility
#'   evaluation (default: \code{"age"}).
#' @param tenure_col Character. Name of the tenure-in-years column used for
#'   tenure-based eligibility (default: \code{"tenure_years"}).
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
#' library(govhrcast)
#'
#' # Load example data
#' contract_dt  <- data.table::copy(bra_hrmis_contract)
#' personnel_dt <- data.table::copy(bra_hrmis_personnel)
#' ref_date     <- as.Date("2014-01-01")
#'
#' # Scalar policy (no group differentiation)
#' policy_params <- list(
#'   group_cols   = NULL,
#'   policy_table = NULL,
#'   defaults = list(
#'     eligibility_type = "age_and_tenure",
#'     pension_type     = "db",
#'     min_age          = 60,
#'     min_tenure       = 20,
#'     accrual_rate     = 0.02,
#'     ref_wage_col     = "gross_salary_lcu",
#'     max_years        = 35,
#'     replacement_cap  = 0.80
#'   )
#' )
#'
#' # Group-level policy: different accrual_rate per paygrade
#' accrual_tbl <- data.table::data.table(
#'   paygrade     = c("D",   "E"),
#'   accrual_rate = c(0.025, 0.03)
#' )
#' policy_grouped <- list(
#'   group_cols   = "paygrade",
#'   policy_table = accrual_tbl,
#'   defaults = list(
#'     eligibility_type = "age_and_tenure",
#'     pension_type     = "db",
#'     min_age          = 60,
#'     min_tenure       = 20,
#'     accrual_rate     = 0.02,
#'     ref_wage_col     = "gross_salary_lcu",
#'     max_years        = 35,
#'     replacement_cap  = 0.80
#'   )
#' )
#'
#' # Run simulation
#' results <- simulate_retirement(
#'   contract_dt  = contract_dt,
#'   personnel_dt = personnel_dt,
#'   ref_date     = ref_date,
#'   policy_params = policy_params
#' )
#'
#' results$summary
#' results$retirees_dt
#' }
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
                                contract_type_col = "contract_type_code",
                                status_col = "status",
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
  # 3. Identify Retirees
  # ========================================
  eligibility_dt <- identify_retirees(
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
    "flat_amount"
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

