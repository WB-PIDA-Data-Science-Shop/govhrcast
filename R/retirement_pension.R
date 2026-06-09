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
#' Dispatcher that routes each row in \code{retirees_dt} to the appropriate
#' pension formula (\code{"db"}, \code{"dc"}, \code{"flat"}, or
#' \code{"hybrid"}) based on the per-row \code{pension_type} column.  Iterates
#' over each unique type present in the cohort, computes pensions for that
#' subset, and accumulates results into a single numeric vector aligned with
#' \code{retirees_dt}.
#'
#' @param retirees_dt data.table.  Enriched retiree table produced by the
#'   pension-parameter resolution stage of \code{\link{simulate_retirement}}.
#'   Required columns (all stamped as per-row by
#'   \code{\link{resolve_policy_table}}):
#'   \describe{
#'     \item{\code{pension_type}}{Character.  One of \code{"db"},
#'       \code{"dc"}, \code{"flat"}, \code{"hybrid"}.}
#'     \item{\code{accrual_rate}}{Numeric. DB accrual rate (required for DB
#'       and hybrid).}
#'     \item{\code{ref_wage_col}}{Character column-pointer naming the wage
#'       column (required for DB and hybrid).}
#'     \item{\code{tenure_years}}{Numeric. Years of service (DB and hybrid).}
#'     \item{\code{max_years}}{Numeric. Service cap; \code{NA} = uncapped
#'       (DB and hybrid).}
#'     \item{\code{replacement_cap}}{Numeric. Replacement rate ceiling;
#'       \code{NA} = uncapped (DB and hybrid).}
#'     \item{\code{balance_col}}{Character column-pointer naming the DC
#'       account balance (DC and hybrid).}
#'     \item{\code{annuity_factor}}{Numeric. DC annuity divisor (DC and
#'       hybrid).}
#'     \item{\code{flat_amount}}{Numeric. Uniform pension amount (flat only).}
#'   }
#'
#' @return Numeric vector of length \code{nrow(retirees_dt)} giving the
#'   periodic pension amount for each retiree.  Order matches the row order
#'   of \code{retirees_dt}.
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
      "rate"   = compute_rate_pension(dt_sub),
      stop("Unknown pension policy type: ", ptype, call. = FALSE)
    )
  }

  result
}


#' Compute Defined-Benefit (DB) Pension
#'
#' @description
#' Computes periodic pension entitlement under a final-salary defined-benefit
#' scheme.  All parameters are read from per-row columns on \code{dt},
#' pre-resolved by \code{\link{resolve_policy_table}}.
#'
#' @param dt data.table.  Retiree subset (\code{pension_type == "db"}).
#'   Required columns: \code{accrual_rate} (numeric), \code{ref_wage_col}
#'   (character column-pointer), \code{tenure_years} (numeric).  Optional:
#'   \code{max_years} (numeric or \code{NA}), \code{replacement_cap}
#'   (numeric or \code{NA}).
#'
#' @details
#' \strong{Formula:}
#' \deqn{P_i = \min(\alpha_i \cdot \min(s_i, S^{\max}) \cdot w_i,\; c_i \cdot w_i)}
#' where:
#' \itemize{
#'   \item \eqn{\alpha_i} = \code{accrual_rate} (e.g. 0.02 = 2\% per year),
#'   \item \eqn{s_i} = \code{tenure_years},
#'   \item \eqn{S^{\max}} = \code{max_years} (\code{NA} = uncapped),
#'   \item \eqn{w_i} = wage from the column named by \code{ref_wage_col},
#'   \item \eqn{c_i} = \code{replacement_cap} (\code{NA} = uncapped).
#' }
#' \code{pmax(pension, 0)} is applied to prevent negative pensions from
#' non-positive accrual rates or wages.
#'
#' @return Numeric vector of DB pension amounts (length \code{nrow(dt)}).
#'   Returns \code{numeric(0)} when \code{nrow(dt) == 0L}.
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
#' Converts an accumulated DC account balance to a periodic annuity.  Supports
#' notional DC (NDC) systems by optionally applying a pre-retirement interest
#' credit to the balance.  All parameters are read from per-row columns on
#' \code{dt}, pre-resolved by \code{\link{resolve_policy_table}}.
#'
#' @param dt data.table.  Retiree subset (\code{pension_type == "dc"}).
#'   Required columns: \code{balance_col} (character column-pointer),
#'   \code{annuity_factor} (numeric).  Optional: \code{notional_rate}
#'   (numeric; NDC interest credit applied to balance before annuitisation).
#'
#' @details
#' \strong{Formula:}
#' \deqn{P_i = \frac{B_i \cdot (1 + r_i)}{A_i}}
#' where \eqn{B_i} = balance from the column named by \code{balance_col},
#' \eqn{r_i} = \code{notional_rate} (0 when absent), and \eqn{A_i} =
#' \code{annuity_factor}.  \code{pmax(pension, 0)} prevents negative pensions
#' from negative balances.
#'
#' @return Numeric vector of DC pension amounts (length \code{nrow(dt)}).
#'   Returns \code{numeric(0)} when \code{nrow(dt) == 0L}.
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
#' Returns the per-row \code{flat_amount} column unchanged.  No computation is
#' performed beyond a column existence check.  Used for universal social pension
#' systems where every retiree receives the same nominal transfer.
#'
#' @param dt data.table.  Retiree subset (\code{pension_type == "flat"}).
#'   Required column: \code{flat_amount} (numeric, non-NA).
#'
#' @return Numeric vector (\code{dt$flat_amount}).  Returns \code{numeric(0)}
#'   when \code{nrow(dt) == 0L}.
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
#' Adds a DB pillar and a DC pillar to produce a hybrid pension entitlement.
#' Delegates entirely to \code{\link{compute_db_pension}} and
#' \code{\link{compute_dc_pension}}; \code{dt} must satisfy the column
#' requirements of both sub-functions.
#'
#' \strong{Formula:} \eqn{P_i = P^{\text{DB}}_i + P^{\text{DC}}_i}
#'
#' @param dt data.table.  Retiree subset (\code{pension_type == "hybrid"}).
#'   Must contain all columns required by \code{\link{compute_db_pension}} and
#'   \code{\link{compute_dc_pension}}.
#'
#' @return Numeric vector of hybrid pension amounts (length \code{nrow(dt)}).
#'   Returns \code{numeric(0)} when \code{nrow(dt) == 0L}.
#' @keywords internal
compute_hybrid_pension <- function(dt) {
  compute_db_pension(dt) + compute_dc_pension(dt)
}




#' Compute Rate Pension
#'
#' @description
#' Computes a pension as a fixed proportion of the retiree's reference wage.
#' Intended for schemes where the periodic pension is a known percentage of
#' final salary (e.g. Botswana's 15\% defined-contribution scheme).
#'
#' @param dt data.table. Retiree subset (\code{pension_type == "rate"}).
#'   Required columns: \code{pension_rate} (numeric, e.g. \code{0.15}),
#'   \code{ref_wage_col} (character column-pointer naming the wage column).
#'
#' @details
#' \strong{Formula:}
#' \deqn{P_i = r_i \cdot w_i}
#' where \eqn{r_i} = \code{pension_rate} and \eqn{w_i} = wage from the
#' column named by \code{ref_wage_col}. \code{pmax(pension, 0)} prevents
#' negative pensions from non-positive rates or wages.
#'
#' @return Numeric vector of pension amounts (length \code{nrow(dt)}).
#'   Returns \code{numeric(0)} when \code{nrow(dt) == 0L}.
#' @keywords internal
compute_rate_pension <- function(dt) {
  if (nrow(dt) == 0L) return(numeric(0))

  wage_col <- dt$ref_wage_col[1L]
  if (is.null(wage_col) || is.na(wage_col))
    stop("compute_rate_pension: 'ref_wage_col' column is missing or NA", call. = FALSE)
  if (!wage_col %in% names(dt))
    stop("compute_rate_pension: wage column '", wage_col, "' not found in retirees_dt",
         call. = FALSE)
  if (!"pension_rate" %in% names(dt))
    stop("compute_rate_pension: 'pension_rate' column not found in retirees_dt",
         call. = FALSE)

  pmax(dt$pension_rate * dt[[wage_col]], 0)
}
