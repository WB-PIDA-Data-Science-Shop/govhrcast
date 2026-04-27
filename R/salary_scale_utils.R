# Suppress R CMD check NOTEs for data.table column names.
utils::globalVariables(c(
  ".multiplier",    # apply_salary_scale_adjustment: temp join column
  "i..multiplier"   # apply_salary_scale_adjustment: i-prefix from data.table join
))

#' Salary Scale Utilities
#'
#' @description
#' Utility functions for adjusting salary scale tables.  These are neutral
#' helpers -- users label the adjustment concept (COLA, regrading, compression
#' policy) in their own code and documentation.
#'
#' @import data.table
#' @name salary_scale_utils
NULL


#' Apply a Multiplier Adjustment to a Salary Scale
#'
#' @description
#' Multiplies all or selected salary entries in a salary scale table by a
#' scalar or named numeric vector.  The result is a consistent salary scale
#' that can be passed into \code{hiring_policy$salary_scale} or
#' \code{movement_policy$salary_scale}, or used directly before calling
#' \code{simulate_horizon()}.
#'
#' Behaviour:
#' \itemize{
#'   \item Scalar \code{adjustment} (e.g. \code{1.05}) -- every entry is
#'     multiplied by the same factor.
#'   \item Named numeric vector -- matched against
#'     \code{salary_scale_dt[[key_col]]}; unmatched entries receive multiplier
#'     \code{1.0} (unchanged) and a warning is issued.
#'   \item Zero or negative multipliers raise an error.
#'   \item Multipliers outside the range \code{(0.9, 1.2)} trigger a warning
#'     (likely user error -- the limits are not enforced).
#' }
#'
#' @param salary_scale_dt data.table.  The salary scale to adjust.  Must
#'   contain \code{salary_col}.  Modified \strong{in a copy} -- the caller's
#'   object is not changed.
#' @param adjustment Numeric.  Scalar multiplier or named numeric vector where
#'   \code{names(adjustment)} match values in
#'   \code{salary_scale_dt[[key_col]]}.
#' @param salary_col Character.  Name of the salary column to update.
#'   Default \code{"gross_salary_lcu"}.
#' @param key_col Character or \code{NULL}.  Column whose values are matched
#'   against \code{names(adjustment)} when \code{adjustment} is a named
#'   vector.  Ignored for scalar adjustments.  Default \code{NULL}.
#'
#' @return A copy of \code{salary_scale_dt} with \code{salary_col} updated.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' scale <- data.table(
#'   grade            = c("G1", "G2", "G3"),
#'   gross_salary_lcu = c(40000, 55000, 70000)
#' )
#'
#' # Scalar: 5% COLA for all grades
#' apply_salary_scale_adjustment(scale, adjustment = 1.05)
#'
#' # Named vector: grade-specific regrading
#' apply_salary_scale_adjustment(
#'   scale,
#'   adjustment = c(G1 = 1.0, G2 = 1.08, G3 = 1.15),
#'   key_col    = "grade"
#' )
#' }
#'
#' @export
apply_salary_scale_adjustment <- function(salary_scale_dt,
                                          adjustment,
                                          salary_col = "gross_salary_lcu",
                                          key_col    = NULL) {

  if (!data.table::is.data.table(salary_scale_dt))
    stop("salary_scale_dt must be a data.table.", call. = FALSE)

  if (!salary_col %in% names(salary_scale_dt))
    stop("salary_col '", salary_col, "' not found in salary_scale_dt.",
         call. = FALSE)

  if (!is.numeric(adjustment))
    stop("adjustment must be numeric (scalar or named vector).", call. = FALSE)

  # Validate: no zero or negative values
  if (any(adjustment <= 0, na.rm = TRUE))
    stop("All adjustment values must be > 0.", call. = FALSE)

  # Warn on extreme values
  if (any(adjustment < 0.9 | adjustment > 1.2, na.rm = TRUE))
    warning(
      "Some adjustment values are outside the range (0.9, 1.2). ",
      "Verify these are intentional -- large multipliers can produce ",
      "unrealistic salary scales.",
      call. = FALSE
    )

  # Work on a copy -- don't modify the caller's object
  out <- data.table::copy(salary_scale_dt)

  if (length(adjustment) == 1L) {
    # Scalar adjustment: apply uniformly
    out[, (salary_col) := get(salary_col) * adjustment]
    return(out)
  }

  # Named vector adjustment
  if (is.null(key_col))
    stop(
      "key_col must be specified when adjustment is a named vector.",
      call. = FALSE
    )
  if (!key_col %in% names(out))
    stop("key_col '", key_col, "' not found in salary_scale_dt.", call. = FALSE)

  adj_dt <- data.table::data.table(
    .key_val_   = names(adjustment),
    .multiplier = as.numeric(adjustment)
  )
  data.table::setnames(adj_dt, ".key_val_", key_col)

  # Warn about unmatched keys
  scale_keys <- unique(out[[key_col]])
  adj_keys   <- names(adjustment)
  unmatched  <- setdiff(scale_keys, adj_keys)
  if (length(unmatched) > 0L) {
    warning(
      "The following key_col values in salary_scale_dt are not in names(adjustment) ",
      "and will be left unchanged: ",
      paste(unmatched, collapse = ", "),
      call. = FALSE
    )
  }

  unknown_keys <- setdiff(adj_keys, scale_keys)
  if (length(unknown_keys) > 0L) {
    warning(
      "The following names(adjustment) do not match any value in key_col '",
      key_col, "' and will be ignored: ",
      paste(unknown_keys, collapse = ", "),
      call. = FALSE
    )
  }

  # Join multipliers, default unmatched to 1.0
  out[adj_dt, .multiplier := i..multiplier, on = key_col]
  out[is.na(.multiplier), .multiplier := 1.0]
  out[, (salary_col) := get(salary_col) * .multiplier]
  out[, .multiplier := NULL]

  out
}
