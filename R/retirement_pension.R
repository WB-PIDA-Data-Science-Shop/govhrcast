#' Pension Calculation Functions
#'
#' @description
#' Functions for computing pension amounts under different pension systems:
#' defined-benefit (DB), defined-contribution (DC), flat, and hybrid.
#'
#' @section Missing Salary Data:
#' If salary values are missing (NA) in the reference wage column, pension
#' calculation will return NA. Future implementations may include imputation
#' strategies such as:
#' \itemize{
#'   \item Average salary by paygrade + seniority + occupation_native
#'   \item Median salary within the same establishment
#'   \item Regression-based imputation using observable characteristics
#' }
#'
#' @import data.table
#' @name retirement_pension
#' @keywords internal
NULL

#' Compute Pension Amounts
#'
#' @description
#' Main dispatcher function that routes pension calculation to the appropriate
#' method based on policy type. Uses switch() for clean routing.
#'
#' @param retirees_dt data.table. Retiree data with salary and tenure information
#' @param policy_type Character. One of: "db", "dc", "flat", "hybrid"
#' @param params List. Policy-specific parameters for pension calculation
#'
#' @return Numeric vector of pension amounts
#' @keywords internal
compute_pension <- function(retirees_dt, policy_type, params) {
  
  pension <- switch(
    policy_type,
    "db" = compute_db_pension(retirees_dt, params),
    "dc" = compute_dc_pension(retirees_dt, params),
    "flat" = compute_flat_pension(retirees_dt, params),
    "hybrid" = compute_hybrid_pension(retirees_dt, params),
    stop("Unknown pension policy type: ", policy_type, call. = FALSE)
  )
  
  return(pension)
}


#' Compute Defined-Benefit (DB) Pension
#'
#' @description
#' Calculates pension based on accrual rate, years of service, and reference wage.
#' Formula: pension = min(accrual_rate * years * ref_wage, replacement_cap * ref_wage)
#'
#' @param dt data.table. Retiree data
#' @param params List with parameters:
#'   \itemize{
#'     \item accrual_rate: Annual accrual rate (e.g., 0.02 for 2% per year)
#'     \item ref_wage_col: Column name for reference wage
#'     \item max_years: Maximum years counted for pension (optional, default: no cap)
#'     \item replacement_cap: Maximum replacement rate (optional, default: 1.0)
#'   }
#'
#' @return Numeric vector of pension amounts
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   accrual_rate = 0.02,
#'   ref_wage_col = "gross_salary_lcu",
#'   max_years = 35,
#'   replacement_cap = 0.80
#' )
#' pensions <- compute_db_pension(retirees_dt, params)
#' }
compute_db_pension <- function(dt, params) {
  
  # Validate required parameters
  required <- c("accrual_rate", "ref_wage_col")
  missing <- setdiff(required, names(params))
  if (length(missing) > 0) {
    stop("Missing DB pension parameters: ", paste(missing, collapse = ", "), 
         call. = FALSE)
  }
  
  # Extract reference wage
  ref_wage <- dt[[params$ref_wage_col]]
  
  # Apply service cap if specified
  if (!is.null(params$max_years)) {
    years <- pmin(dt$tenure_years, params$max_years)
  } else {
    years <- dt$tenure_years
  }
  
  # Calculate gross pension
  gross_pension <- params$accrual_rate * years * ref_wage
  
  # Apply replacement cap if specified
  if (!is.null(params$replacement_cap)) {
    max_allowed <- params$replacement_cap * ref_wage
    pension <- pmin(gross_pension, max_allowed)
  } else {
    pension <- gross_pension
  }
  
  return(pension)
}


#' Compute Defined-Contribution (DC) Pension
#'
#' @description
#' Calculates pension by converting accumulated balance to annuity.
#' Supports both standard DC and notional DC (NDC) systems.
#' Formula: pension = balance / annuity_factor
#'
#' @param dt data.table. Retiree data
#' @param params List with parameters:
#'   \itemize{
#'     \item balance_col: Column name for account balance
#'     \item annuity_factor: Factor for converting balance to annual pension
#'     \item type: "DC" or "NDC" (optional, for notional DC adjustments)
#'     \item notional_rate: Interest rate for NDC (required if type = "NDC")
#'   }
#'
#' @return Numeric vector of pension amounts
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Standard DC
#' params <- list(
#'   balance_col = "account_balance",
#'   annuity_factor = 18
#' )
#' 
#' # Notional DC
#' params <- list(
#'   balance_col = "notional_balance",
#'   annuity_factor = 18,
#'   type = "NDC",
#'   notional_rate = 0.015
#' )
#' }
compute_dc_pension <- function(dt, params) {
  
  # Validate required parameters
  required <- c("balance_col", "annuity_factor")
  missing <- setdiff(required, names(params))
  if (length(missing) > 0) {
    stop("Missing DC pension parameters: ", paste(missing, collapse = ", "), 
         call. = FALSE)
  }
  
  # Extract balance
  balance <- dt[[params$balance_col]]
  
  # Apply notional interest if NDC
  if (!is.null(params$type) && params$type == "NDC") {
    if (is.null(params$notional_rate)) {
      stop("notional_rate required for NDC pension type", call. = FALSE)
    }
    balance <- balance * (1 + params$notional_rate)
  }
  
  # Convert to annual pension
  pension <- balance / params$annuity_factor
  
  return(pension)
}


#' Compute Flat Pension
#'
#' @description
#' Assigns a uniform flat pension amount to all retirees.
#'
#' @param dt data.table. Retiree data
#' @param params List with parameter:
#'   \itemize{
#'     \item flat_amount: Fixed pension amount for all retirees
#'   }
#'
#' @return Numeric vector of pension amounts (all identical)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' params <- list(flat_amount = 15000)
#' pensions <- compute_flat_pension(retirees_dt, params)
#' }
compute_flat_pension <- function(dt, params) {
  
  # Validate required parameter
  if (is.null(params$flat_amount)) {
    stop("flat_amount is required for flat pension type", call. = FALSE)
  }
  
  # Assign flat amount to all retirees
  pension <- rep(params$flat_amount, nrow(dt))
  
  return(pension)
}


#' Compute Hybrid Pension
#'
#' @description
#' Combines DB and DC pension components into a single pension.
#' Formula: pension = DB_part + DC_part
#'
#' @param dt data.table. Retiree data
#' @param params List with parameters:
#'   \itemize{
#'     \item db_params: List of parameters for DB component
#'     \item dc_params: List of parameters for DC component
#'   }
#'
#' @return Numeric vector of pension amounts
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   db_params = list(
#'     accrual_rate = 0.015,
#'     ref_wage_col = "gross_salary_lcu",
#'     max_years = 30
#'   ),
#'   dc_params = list(
#'     balance_col = "dc_balance",
#'     annuity_factor = 20
#'   )
#' )
#' pensions <- compute_hybrid_pension(retirees_dt, params)
#' }
compute_hybrid_pension <- function(dt, params) {
  
  # Validate required parameters
  if (is.null(params$db_params) || is.null(params$dc_params)) {
    stop("Both db_params and dc_params are required for hybrid pension type", 
         call. = FALSE)
  }
  
  # Compute DB component
  db_part <- compute_db_pension(dt, params$db_params)
  
  # Compute DC component
  dc_part <- compute_dc_pension(dt, params$dc_params)
  
  # Combine
  pension <- db_part + dc_part
  
  return(pension)
}
