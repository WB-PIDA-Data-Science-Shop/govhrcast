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
#' method based on the \code{pension_type} column in \code{retirees_dt}.
#' Iterates over each unique pension type present and accumulates results,
#' enabling mixed pension types within a single retiree cohort.
#'
#' All pension parameters (\code{accrual_rate}, \code{ref_wage_col},
#' \code{max_years}, \code{replacement_cap}, \code{balance_col},
#' \code{annuity_factor}, \code{flat_amount}, etc.) are expected to be
#' columns on \code{retirees_dt} — resolved upstream by
#' \code{resolve_policy_table()}.
#'
#' @param retirees_dt data.table. Retiree data including a \code{pension_type}
#'   column and all relevant pension parameter columns.
#'
#' @return Numeric vector of pension amounts (length \code{nrow(retirees_dt)}).
#' @keywords internal
compute_pension <- function(retirees_dt) {

  result <- numeric(nrow(retirees_dt))

  for (ptype in unique(retirees_dt$pension_type)) {
    idx    <- which(retirees_dt$pension_type == ptype)
    dt_sub <- retirees_dt[idx]
    result[idx] <- switch(
      ptype,
      "db"     = compute_db_pension(dt_sub),
      "dc"     = compute_dc_pension(dt_sub),
      "flat"   = compute_flat_pension(dt_sub),
      "hybrid" = compute_hybrid_pension(dt_sub),
      stop("Unknown pension policy type: ", ptype, call. = FALSE)
    )
  }

  result
}


#' Compute Defined-Benefit (DB) Pension
#'
#' @description
#' Calculates pension based on accrual rate, years of service, and reference
#' wage.  All parameters are read from columns on \code{dt}:
#' \itemize{
#'   \item \code{accrual_rate}: Annual accrual rate (e.g. 0.02).
#'   \item \code{ref_wage_col}: Character column whose value names the wage
#'     column to use (e.g. \code{"gross_salary_lcu"}).
#'   \item \code{max_years}: Service cap (optional; \code{NA} = no cap).
#'   \item \code{replacement_cap}: Max replacement rate (optional; \code{NA} = no cap).
#' }
#' Formula: \code{pension = min(accrual_rate * years * wage, replacement_cap * wage)}
#'
#' @param dt data.table. Retiree subset for DB pension calculation.
#'   Must have columns: \code{accrual_rate}, \code{ref_wage_col},
#'   \code{tenure_years}.  Optional: \code{max_years}, \code{replacement_cap}.
#'
#' @return Numeric vector of pension amounts.
#' @keywords internal
compute_db_pension <- function(dt) {

  if (nrow(dt) == 0L) return(numeric(0))

  # ref_wage_col is a character column naming the actual wage column
  wage_col <- dt$ref_wage_col[1L]
  if (is.null(wage_col) || is.na(wage_col))
    stop("compute_db_pension: 'ref_wage_col' column is missing or NA", call. = FALSE)
  if (!wage_col %in% names(dt))
    stop("compute_db_pension: wage column '", wage_col, "' not found in retirees_dt",
         call. = FALSE)

  if (!"accrual_rate" %in% names(dt))
    stop("compute_db_pension: 'accrual_rate' column not found in retirees_dt",
         call. = FALSE)

  ref_wage <- dt[[wage_col]]

  # Apply service cap if present (NA = no cap)
  if ("max_years" %in% names(dt) && !all(is.na(dt$max_years))) {
    years <- pmin(dt$tenure_years, dt$max_years, na.rm = FALSE)
  } else {
    years <- dt$tenure_years
  }

  gross_pension <- dt$accrual_rate * years * ref_wage

  # Apply replacement cap if present (NA = no cap)
  if ("replacement_cap" %in% names(dt) && !all(is.na(dt$replacement_cap))) {
    max_allowed <- dt$replacement_cap * ref_wage
    pension     <- pmin(gross_pension, max_allowed)
  } else {
    pension <- gross_pension
  }

  # Pension cannot be negative (e.g. from negative accrual_rate or wage)
  pmax(pension, 0)
}


#' Compute Defined-Contribution (DC) Pension
#'
#' @description
#' Calculates pension by converting accumulated balance to an annuity.
#' Supports both standard DC and notional DC (NDC) systems.
#' Formula: \code{pension = balance / annuity_factor}
#'
#' All parameters are read from columns on \code{dt}:
#' \itemize{
#'   \item \code{balance_col}: Character column naming the account balance column.
#'   \item \code{annuity_factor}: Conversion divisor.
#'   \item \code{notional_rate}: Optional NDC interest rate (column or scalar).
#' }
#'
#' @param dt data.table. Retiree subset for DC pension calculation.
#'
#' @return Numeric vector of pension amounts.
#' @keywords internal
compute_dc_pension <- function(dt) {

  if (nrow(dt) == 0L) return(numeric(0))

  # balance_col is a character column naming the actual balance column
  bal_col <- dt$balance_col[1L]
  if (is.null(bal_col) || is.na(bal_col))
    stop("compute_dc_pension: 'balance_col' column is missing or NA", call. = FALSE)
  if (!bal_col %in% names(dt))
    stop("compute_dc_pension: balance column '", bal_col, "' not found in retirees_dt",
         call. = FALSE)

  if (!"annuity_factor" %in% names(dt))
    stop("compute_dc_pension: 'annuity_factor' column not found in retirees_dt",
         call. = FALSE)

  balance <- dt[[bal_col]]

  # Apply notional interest if NDC (notional_rate present and non-NA)
  if ("notional_rate" %in% names(dt) && !all(is.na(dt$notional_rate))) {
    balance <- balance * (1 + dt$notional_rate)
  }

  # Pension cannot be negative (e.g. from zero or negative balance / annuity_factor)
  pmax(balance / dt$annuity_factor, 0)
}


#' Compute Flat Pension
#'
#' @description
#' Assigns a uniform flat pension amount to all retirees.
#' Reads \code{flat_amount} from the \code{dt$flat_amount} column (per-row
#' values resolved upstream by \code{resolve_policy_table()}).
#'
#' @param dt data.table. Retiree subset.  Must have column \code{flat_amount}.
#'
#' @return Numeric vector of pension amounts.
#' @keywords internal
compute_flat_pension <- function(dt) {

  if (nrow(dt) == 0L) return(numeric(0))

  if (!"flat_amount" %in% names(dt) || all(is.na(dt$flat_amount)))
    stop("compute_flat_pension: 'flat_amount' column is missing or all NA", call. = FALSE)

  dt$flat_amount
}


#' Compute Hybrid Pension
#'
#' @description
#' Combines DB and DC pension components into a single pension.
#' Formula: \code{pension = DB_part + DC_part}.
#'
#' Expects \code{dt} to have all columns required by both
#' \code{compute_db_pension()} and \code{compute_dc_pension()}.
#'
#' @param dt data.table. Retiree subset with all DB + DC parameter columns.
#'
#' @return Numeric vector of pension amounts.
#' @keywords internal
compute_hybrid_pension <- function(dt) {
  compute_db_pension(dt) + compute_dc_pension(dt)
}
