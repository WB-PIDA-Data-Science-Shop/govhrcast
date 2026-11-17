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
#'   \item Scalar \code{adjustment} (e.g. \code{1.05}, \strong{unnamed}) --
#'     every entry is multiplied by the same factor.
#'   \item Named numeric vector, \strong{of any length including one}
#'     (e.g. \code{c(hazard = 1.20)} to adjust a single key) -- matched
#'     against \code{salary_scale_dt[[key_col]]}; unmatched entries receive
#'     multiplier \code{1.0} (unchanged) and a warning is issued. Dispatch is
#'     based on whether \code{adjustment} is named, not its length -- a
#'     single named value is not treated as a scalar.
#'   \item Zero or negative multipliers raise an error.
#'   \item Multipliers outside the range \code{(0.9, 1.2)} trigger a warning
#'     (likely user error -- the limits are not enforced).
#' }
#'
#' @param salary_scale_dt data.table.  The salary scale to adjust.  Must
#'   contain \code{salary_col}.  Modified \strong{in a copy} -- the caller's
#'   object is not changed.
#' @param adjustment Numeric.  An \strong{unnamed} single value applies to
#'   every row uniformly.  A \strong{named} vector -- even a single named
#'   value, e.g. \code{c(hazard = 1.20)} -- is matched by name against
#'   \code{salary_scale_dt[[key_col]]}; only matching rows are adjusted.
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

  if (is.null(names(adjustment))) {
    # True scalar (unnamed, any length -- but validated to length 1 below):
    # apply uniformly. Checking names() rather than length() matters because
    # a *named* single-element vector (e.g. c(G2 = 1.5)) must still be routed
    # to the key_col-matching path below, not treated as a uniform scalar.
    if (length(adjustment) != 1L)
      stop("adjustment must be a single unnamed scalar, or a named vector.",
           call. = FALSE)
    out[, (salary_col) := get(salary_col) * adjustment]
    return(out)
  }

  # Named vector adjustment (one or more elements)
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


#' Build a Pay Table From the Data
#'
#' @description
#' Summarises the pay of the staff on the payroll at \code{baseline_date} into
#' one value per group (for example, the mean pay of each pay grade). The result
#' is the pay table \code{\link{simulate_horizon}()} takes as
#' \code{salary_scale_dt}: hires and people promoted into a group are paid that
#' group's value.
#'
#' Only staff count. Rows whose contract type is \code{"pensioner"} or
#' \code{"inactive"} are left out, because pensions and lapsed contracts sit in
#' the same pay column and would distort the table. A row with a blank contract
#' type counts as staff (blank is treated as missing data, as everywhere in the
#' package); if your pensioners have a blank contract type, drop their rows
#' before calling this.
#'
#' @param contract_dt data.table. Contract-level data, with the pay column, the
#'   snapshot date column, the contract type column and \code{group_cols}. It can
#'   hold many snapshots; only \code{baseline_date}'s rows are used.
#' @param baseline_date Date, or a \code{"YYYY-MM-DD"} string. The snapshot to
#'   take pay from, normally the one the projection starts from.
#' @param group_cols Character vector. The columns that define a group (e.g.
#'   \code{"paygrade"}, or \code{c("est_id", "paygrade")}): one row is returned
#'   per combination. \code{NULL} gives a single value over all staff.
#' @param fn A function, or its name as a string (\code{mean}, \code{"median"},
#'   \code{stats::quantile}, or a function of your own). It receives the pay of
#'   one group and must return a single number. Default \code{mean}.
#' @param ... Further arguments passed on to \code{fn}, for example
#'   \code{na.rm = TRUE} so that blank pay is ignored rather than making the
#'   group's value blank, or \code{probs = 0.75} for \code{stats::quantile}.
#' @param column_spec_args Column names, from \code{\link{define_column_spec}()}.
#'   Uses \code{salary_col} (the pay summarised, and the name of the result's pay
#'   column), \code{ref_date_col} and \code{contract_type_col}. Because it comes
#'   after \code{...}, it must be named in the call.
#'
#' @return A data.table with \code{group_cols} and one pay column named after
#'   \code{salary_col}, one row per group.
#'
#' @examples
#' library(data.table)
#' # Mean pay of each pay grade at the 2017 snapshot, ignoring blank pay
#' prepare_salary_scale(bra_hrmis_contract, "2017-09-01", "paygrade",
#'                      mean, na.rm = TRUE)
#'
#' # The 75th percentile instead
#' prepare_salary_scale(bra_hrmis_contract, "2017-09-01", "paygrade",
#'                      stats::quantile, probs = 0.75, na.rm = TRUE, names = FALSE)
#'
#' @seealso \code{\link{apply_salary_scale_adjustment}} to scale the table up or
#'   down afterwards.
#' @export
prepare_salary_scale <- function(contract_dt,
                                 baseline_date,
                                 group_cols,
                                 fn = mean,
                                 ...,
                                 column_spec_args = define_column_spec()) {
  # 1. Check the inputs
  .check_column_spec(column_spec_args)
  cols <- column_spec_args
  validate_datatable(contract_dt, "contract_dt")
  .check_spec_columns(contract_dt, NULL, cols, required = list(
    contract_dt = c("ref_date_col", "salary_col", "contract_type_col")))
  validate_columns_exist(contract_dt, group_cols, "contract_dt")
  baseline_date <- validate_date_format(baseline_date, "baseline_date")
  salary_col    <- cols$salary_col

  # fn may be given as a function (mean) or as its name ("mean")
  fn <- match.fun(fn)

  # 2. Staff on the payroll at the baseline date. Pensions and inactive
  #    contracts sit in the same pay column and are left out.
  staff <- contract_dt[get(cols$ref_date_col) == baseline_date &
                         .on_payroll(get(cols$contract_type_col))]
  if (nrow(staff) == 0L) {
    stop("prepare_salary_scale: nobody on the payroll at ", format(baseline_date),
         " in contract_dt.", call. = FALSE)
  }

  # 3. One value per group. Anything after fn in the call (na.rm = TRUE, ...)
  #    is passed on to it. as.numeric(): median() of whole numbers returns an
  #    integer for some groups and a double for others, and a column needs one type.
  scale_dt <- staff[, as.numeric(fn(get(salary_col), ...)), by = group_cols]

  # A function that returns several values (range(), say) gives a group several rows
  if (any(scale_dt[, .N, by = group_cols]$N > 1L)) {
    stop("prepare_salary_scale: fn must return one value per group.", call. = FALSE)
  }

  data.table::setnames(scale_dt, "V1", salary_col)
  scale_dt[]
}