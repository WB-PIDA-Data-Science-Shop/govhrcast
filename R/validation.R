#' Validation Helper Functions
#'
#' @description
#' Collection of validation functions used across all simulation modules.
#' Each function validates one specific input type and stops with
#' informative error messages on failure.
#'
#' @import data.table
#' @name validation
#' @keywords internal
NULL

#' Validate Data Table Input
#'
#' @param dt Object to validate
#' @param varname Character. Variable name for error messages
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_datatable <- function(dt, varname) {
  if (!data.table::is.data.table(dt)) {
    stop(varname, " must be a data.table", call. = FALSE)
  }

  if (nrow(dt) == 0) {
    stop(varname, " cannot be empty", call. = FALSE)
  }

  return(invisible(TRUE))
}

#' Validate Column Exists in Data Table
#'
#' @param dt data.table to check
#' @param colname Character. Column name to validate
#' @param varname Character. Variable name for error messages
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_column_exists <- function(dt, colname, varname) {
  if (!colname %in% names(dt)) {
    stop(
      "Column '",
      colname,
      "' not found in ",
      varname,
      call. = FALSE
    )
  }

  return(invisible(TRUE))
}

#' Validate Multiple Columns Exist
#'
#' @param dt data.table to check
#' @param colnames Character vector. Column names to validate
#' @param varname Character. Variable name for error messages
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_columns_exist <- function(dt, colnames, varname) {
  missing_cols <- setdiff(colnames, names(dt))

  if (length(missing_cols) > 0) {
    stop(
      "Columns not found in ",
      varname,
      ": ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  return(invisible(TRUE))
}

#' Validate Date Format
#'
#' @param date Object to validate
#' @param varname Character. Variable name for error messages
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_date_format <- function(date, varname) {
  # Accept both Date objects and character strings
  if (is.character(date)) {
    tryCatch(
      {
        date <- as.Date(date)
      },
      error = function(e) {
        stop(
          varname,
          " must be a valid date string (e.g., '2024-01-01') or Date object. ",
          "Error: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  if (!inherits(date, "Date")) {
    stop(
      varname,
      " must be a Date object or date string (e.g., '2024-01-01')",
      call. = FALSE
    )
  }

  if (length(date) != 1) {
    stop(varname, " must be a single Date value", call. = FALSE)
  }

  if (is.na(date)) {
    stop(varname, " cannot be NA", call. = FALSE)
  }

  return(date)
}

#' Validate a Policy Parameter Specification
#'
#' @description
#' Accepts either a bare positive scalar or a three-slot param spec list
#' (\code{default}, \code{group_cols}, \code{policy_table}) as produced by
#' \code{dispatch_param()}.  Raises an informative error if the structure is
#' invalid.
#'
#' @param param_spec Numeric scalar or named list with slots \code{default},
#'   \code{group_cols}, and \code{policy_table}.  A bare scalar is interpreted
#'   as a group-free constant rate.
#' @param param_name Character scalar.  Used in error messages and as the
#'   expected value column name in \code{policy_table}.
#'
#' @return Invisible \code{TRUE}.  Stops with an informative error on the
#'   first violation found.
#'
#' @section Error Conditions:
#' \describe{
#'   \item{Non-numeric scalar}{\code{param_spec} is not a list and is not a
#'     numeric scalar of length 1.}
#'   \item{Non-positive scalar}{\code{param_spec <= 0} (bare scalar path).}
#'   \item{Invalid default}{\code{param_spec$default} is non-numeric, length
#'     != 1, NA, or <= 0 (when not \code{NULL}).}
#'   \item{Mismatched group spec}{\code{group_cols} supplied without
#'     \code{policy_table}, or vice versa.}
#'   \item{Bad policy_table}{\code{policy_table} is not a data.table, has 0
#'     rows, is missing one or more \code{group_cols} columns, or does not
#'     contain a column named \code{param_name}.}
#' }
#'
#' @keywords internal
validate_param_spec <- function(param_spec, param_name) {
  # --- Bare scalar path ---------------------------------------------------
  if (!is.list(param_spec) || data.table::is.data.table(param_spec)) {
    if (
      !is.numeric(param_spec) || length(param_spec) != 1L || is.na(param_spec)
    ) {
      stop(
        param_name,
        " must be a single non-NA numeric scalar or a param ",
        "spec list (list(default, group_cols, policy_table))",
        call. = FALSE
      )
    }
    if (param_spec <= 0) {
      stop(param_name, " must be > 0", call. = FALSE)
    }
    return(invisible(TRUE))
  }

  # --- Three-slot list path -----------------------------------------------
  group_cols <- param_spec$group_cols
  default <- param_spec$default
  policy_table <- param_spec$policy_table

  # default: must be positive scalar or NULL
  if (!is.null(default)) {
    if (!is.numeric(default) || length(default) != 1L || is.na(default)) {
      stop(
        param_name,
        "$default must be a single non-NA numeric scalar or NULL",
        call. = FALSE
      )
    }
    if (default <= 0) {
      stop(param_name, "$default must be > 0", call. = FALSE)
    }
  }

  # group_cols / policy_table must both be set or both be NULL
  if (is.null(group_cols) && !is.null(policy_table)) {
    stop(
      param_name,
      ": policy_table supplied but group_cols is NULL. ",
      "Set group_cols to the column(s) to join on.",
      call. = FALSE
    )
  }

  if (!is.null(group_cols) && is.null(policy_table)) {
    stop(
      param_name,
      ": group_cols supplied but policy_table is NULL. ",
      "Provide a policy_table or remove group_cols.",
      call. = FALSE
    )
  }

  # If group-level, validate the policy_table
  if (!is.null(group_cols)) {
    if (!data.table::is.data.table(policy_table)) {
      stop(param_name, "$policy_table must be a data.table", call. = FALSE)
    }

    missing_gcols <- setdiff(group_cols, names(policy_table))
    if (length(missing_gcols) > 0L) {
      stop(
        param_name,
        "$policy_table is missing group_cols column(s): ",
        paste(missing_gcols, collapse = ", "),
        call. = FALSE
      )
    }

    if (!param_name %in% names(policy_table)) {
      stop(
        param_name,
        "$policy_table must contain a column named '",
        param_name,
        "'",
        call. = FALSE
      )
    }

    if (nrow(policy_table) == 0L) {
      stop(param_name, "$policy_table has 0 rows", call. = FALSE)
    }
  }

  invisible(TRUE)
}


#' Validate a Positive Number
#'
#' @param num Numeric to validate
#' @param varname Character. Variable name for error messages
#' @param allow_zero Logical. Allow zero? (default FALSE)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_positive_number <- function(num, varname, allow_zero = FALSE) {
  if (!is.numeric(num)) {
    stop(varname, " must be numeric", call. = FALSE)
  }

  if (length(num) != 1) {
    stop(varname, " must be a single numeric value", call. = FALSE)
  }

  if (is.na(num)) {
    stop(varname, " cannot be NA", call. = FALSE)
  }

  if (allow_zero) {
    if (num < 0) {
      stop(varname, " must be >= 0", call. = FALSE)
    }
  } else {
    if (num <= 0) {
      stop(varname, " must be > 0", call. = FALSE)
    }
  }

  return(invisible(TRUE))
}

#' Validate Number in Range
#'
#' @param num Numeric to validate
#' @param varname Character. Variable name for error messages
#' @param min Numeric. Minimum value (inclusive)
#' @param max Numeric. Maximum value (inclusive)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_number_range <- function(num, varname, min, max) {
  if (!is.numeric(num)) {
    stop(varname, " must be numeric", call. = FALSE)
  }

  if (length(num) != 1) {
    stop(varname, " must be a single numeric value", call. = FALSE)
  }

  if (is.na(num)) {
    stop(varname, " cannot be NA", call. = FALSE)
  }

  if (num < min || num > max) {
    stop(
      varname,
      " must be between ",
      min,
      " and ",
      max,
      call. = FALSE
    )
  }

  return(invisible(TRUE))
}

#' Validate Character String
#'
#' @param str Object to validate
#' @param varname Character. Variable name for error messages
#' @param allow_na Logical. Allow NA? (default FALSE)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_character_string <- function(str, varname, allow_na = FALSE) {
  if (!is.character(str) || length(str) != 1) {
    stop(varname, " must be a single character string", call. = FALSE)
  }

  if (!allow_na && is.na(str)) {
    stop(varname, " cannot be NA", call. = FALSE)
  }

  return(invisible(TRUE))
}

#' Validate Choice from Valid Options
#'
#' @param choice Character. Value to validate
#' @param valid_choices Character vector. Valid options
#' @param varname Character. Variable name for error messages
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_choice <- function(choice, valid_choices, varname) {
  if (!is.character(choice) || length(choice) != 1) {
    stop(varname, " must be a single character string", call. = FALSE)
  }

  if (!choice %in% valid_choices) {
    stop(
      "Invalid ",
      varname,
      ": '",
      choice,
      "'. ",
      "Valid options: ",
      paste(valid_choices, collapse = ", "),
      call. = FALSE
    )
  }

  return(invisible(TRUE))
}

#' Validate Required Parameters in List
#'
#' @param params List. Parameters to validate
#' @param required_params Character vector. Required parameter names
#' @param context Character. Context for error messages
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_required_params <- function(params, required_params, context) {
  if (!is.list(params)) {
    stop("policy_params must be a list", call. = FALSE)
  }

  missing_params <- setdiff(required_params, names(params))

  if (length(missing_params) > 0) {
    stop(
      "Missing required parameters for ",
      context,
      ": ",
      paste(missing_params, collapse = ", "),
      call. = FALSE
    )
  }

  return(invisible(TRUE))
}

#' Validate a Unified Policy Parameters Object
#'
#' @description
#' Validates the three-slot structure shared by all govhrcast policy objects:
#' a \code{defaults} named list, an optional \code{group_cols} character
#' vector, and an optional \code{policy_table} data.table.  Called at the
#' top of every \code{simulate_*()} entry point before any computation begins.
#'
#' @param policy_params List.  Must contain at minimum a \code{defaults}
#'   named list.  \code{group_cols} and \code{policy_table} are optional but
#'   must be supplied together.
#'
#' @return Invisible \code{TRUE}.  Stops with an informative error on the
#'   first violation found.
#'
#' @section Error Conditions:
#' \describe{
#'   \item{Not a list}{\code{policy_params} is not an R list.}
#'   \item{Missing defaults}{\code{policy_params$defaults} is \code{NULL} or
#'     is not a list.}
#'   \item{group_cols without policy_table}{\code{group_cols} is non-\code{NULL}
#'     but \code{policy_table} is \code{NULL}.}
#'   \item{policy_table without group_cols}{\code{policy_table} is
#'     non-\code{NULL} but \code{group_cols} is \code{NULL}.}
#'   \item{Bad policy_table}{\code{policy_table} is not a data.table, has 0
#'     rows, or is missing one or more of the \code{group_cols} columns.}
#' }
#'
#' @keywords internal
validate_policy_table <- function(policy_params) {
  if (!is.list(policy_params)) {
    stop("policy_params must be a list", call. = FALSE)
  }

  if (is.null(policy_params$defaults) || !is.list(policy_params$defaults)) {
    stop("policy_params must contain a 'defaults' list", call. = FALSE)
  }

  gc <- policy_params$group_cols
  pt <- policy_params$policy_table

  if (!is.null(gc) && is.null(pt)) {
    stop(
      "policy_params: group_cols supplied but policy_table is NULL. ",
      "Provide a policy_table or set group_cols = NULL.",
      call. = FALSE
    )
  }

  if (is.null(gc) && !is.null(pt)) {
    stop(
      "policy_params: policy_table supplied but group_cols is NULL. ",
      "Provide group_cols or set policy_table = NULL.",
      call. = FALSE
    )
  }

  if (!is.null(pt)) {
    if (!data.table::is.data.table(pt)) {
      stop("policy_params$policy_table must be a data.table", call. = FALSE)
    }

    if (nrow(pt) == 0L) {
      stop("policy_params$policy_table has 0 rows", call. = FALSE)
    }

    missing_gc <- setdiff(gc, names(pt))
    if (length(missing_gc) > 0L) {
      stop(
        "policy_params$policy_table is missing group_cols column(s): ",
        paste(missing_gc, collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}


#' Check Retirement Module Inputs
#'
#' @description
#' Gate-keeper called at the start of \code{\link{simulate_retirement}}.
#' Validates all inputs — data tables, column names, policy structure, and
#' eligibility/pension parameter consistency — before any computation begins.
#' Stops on the first violation with an error message that names the
#' offending argument.
#'
#' @param contract_dt data.table.  Non-empty contract snapshot.
#' @param personnel_dt data.table.  Non-empty personnel snapshot.
#' @param policy_params List.  Three-slot policy object.  Validated by
#'   \code{\link{validate_policy_table}}.  The \code{defaults} sub-list must
#'   contain:
#'   \describe{
#'     \item{\code{eligibility_type}}{One of \code{"age_only"},
#'       \code{"tenure_only"}, \code{"age_and_tenure"}.}
#'     \item{\code{pension_type}}{One of \code{"db"}, \code{"dc"},
#'       \code{"flat"}, \code{"hybrid"}.}
#'     \item{\code{min_age}}{Required when \code{eligibility_type} involves
#'       age.  May be omitted if \code{policy_table} carries a \code{min_age}
#'       column.}
#'     \item{\code{min_tenure}}{Required when \code{eligibility_type} involves
#'       tenure.  May be omitted if \code{policy_table} carries a
#'       \code{min_tenure} column.}
#'   }
#' @param ref_date Date or character convertible to Date.  Reference date for
#'   the simulation period.
#' @param column_spec_args Complete column specification from
#'   \code{.column_spec()}. Keys read: \code{personnel_id_col} (must exist in
#'   both tables), \code{contract_id_col}, \code{start_date_col},
#'   \code{end_date_col}, \code{contract_type_col} (in \code{contract_dt}),
#'   \code{status_col} (in \code{personnel_dt}) and \code{birth_date_col}
#'   (in \code{personnel_dt} when eligibility involves age, unless
#'   \code{NULL}, meaning age is pre-computed).
#'
#' @return Invisible \code{TRUE}.  Stops with an informative error on the
#'   first violation found.
#'
#' @section Error Conditions:
#' \describe{
#'   \item{Empty tables}{\code{contract_dt} or \code{personnel_dt} has 0 rows.}
#'   \item{Invalid ref_date}{\code{ref_date} cannot be parsed as a Date, is NA,
#'     or is not length 1.}
#'   \item{Invalid policy structure}{\code{validate_policy_table()} fails — see
#'     that function for sub-conditions.}
#'   \item{Missing required defaults}{\code{eligibility_type} or
#'     \code{pension_type} absent from \code{policy_params$defaults}.}
#'   \item{Invalid eligibility_type}{Value not in \code{c("age_only",
#'     "tenure_only", "age_and_tenure")}, either in defaults or in
#'     \code{policy_table}.}
#'   \item{Invalid pension_type}{Value not in \code{c("db", "dc", "flat",
#'     "hybrid")}, either in defaults or in \code{policy_table}.}
#'   \item{Missing min_age}{\code{eligibility_type} requires age but
#'     \code{min_age} is absent from both \code{defaults} and
#'     \code{policy_table}.}
#'   \item{Missing birth_date_col}{The column named by \code{birth_date_col}
#'     is absent from \code{personnel_dt} (when \code{birth_date_col} is not
#'     \code{NULL}).}
#'   \item{Missing min_tenure}{\code{eligibility_type} requires tenure but
#'     \code{min_tenure} is absent from both \code{defaults} and
#'     \code{policy_table}.}
#'   \item{Missing contract columns}{One or more of \code{contract_id_col},
#'     \code{personnel_id_col}, \code{start_date_col}, \code{end_date_col},
#'     \code{contract_type_col} absent from \code{contract_dt}.}
#'   \item{Missing personnel columns}{\code{personnel_id_col} or
#'     \code{status_col} absent from \code{personnel_dt}.}
#' }
#'
#' @keywords internal
check_retirement_inputs <- function(contract_dt,
                                    personnel_dt,
                                    policy_params,
                                    ref_date,
                                    column_spec_args = .column_spec()) {
  birth_date_col <- column_spec_args$birth_date_col

  # Validate data tables
  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")

  # Validate ref_date
  validate_date_format(ref_date, "ref_date")

  # Validate top-level structure
  if (!is.list(policy_params)) {
    stop("policy_params must be a list", call. = FALSE)
  }
  # A hazard_rate policy with no rate table (NULL, or 0 rows because the
  # panel showed no retirements) is valid: every group's rate resolves to 0
  # in compute_status_quo_retirements(), so only a mandatory age (min_age)
  # forces anyone out. Validate its structure as a scalar policy instead.
  .pt <- policy_params$policy_table
  if (identical(policy_params$defaults$eligibility_type, "hazard_rate") &&
      (is.null(.pt) || (data.table::is.data.table(.pt) && nrow(.pt) == 0L))) {
    validate_policy_table(utils::modifyList(policy_params,
                                            list(group_cols = NULL, policy_table = NULL)))
  } else {
    validate_policy_table(policy_params)
  }

  .defaults <- policy_params$defaults

  # Check required top-level params in defaults
  required_top <- c("eligibility_type", "pension_type")
  validate_required_params(.defaults, required_top, "retirement policy")

  # Validate eligibility_type value(s)
  valid_eligibility <- c(
    "age_only", "tenure_only", "age_and_tenure", "custom", "hazard_rate"
  )
  # Check defaults
  validate_choice(
    .defaults$eligibility_type,
    valid_eligibility,
    "eligibility_type"
  )
  # Also check any values in policy_table if present
  if (
    !is.null(policy_params$policy_table) &&
      "eligibility_type" %in% names(policy_params$policy_table)
  ) {
    bad <- setdiff(
      unique(policy_params$policy_table$eligibility_type),
      valid_eligibility
    )
    if (length(bad) > 0L) {
      stop(
        "Invalid eligibility_type in policy_table: '",
        paste(bad, collapse = "', '"),
        "'. Valid options: ",
        paste(valid_eligibility, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Validate pension_type value(s)
  valid_pension <- c("db", "dc", "flat", "hybrid", "rate", "custom")
  validate_choice(.defaults$pension_type, valid_pension, "pension_type")
  if (
    !is.null(policy_params$policy_table) &&
      "pension_type" %in% names(policy_params$policy_table)
  ) {
    bad <- setdiff(
      unique(policy_params$policy_table$pension_type),
      valid_pension
    )
    if (length(bad) > 0L) {
      stop(
        "Invalid pension_type in policy_table: '",
        paste(bad, collapse = "', '"),
        "'. Valid options: ",
        paste(valid_pension, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Validate every pension_formula the policy supplies -- in defaults and/or
  # a policy_table column -- against the formula allow-list, fail-fast,
  # before any computation starts.
  .formulas <- .defaults$pension_formula
  if (
    !is.null(policy_params$policy_table) &&
      "pension_formula" %in% names(policy_params$policy_table)
  ) {
    .formulas <- c(.formulas, policy_params$policy_table$pension_formula)
  }
  for (.formula in unique(.formulas[!is.na(.formulas)])) {
    validate_pension_formula(.formula)
  }

  # Validate hazard_rate-specific requirements. min_age/min_tenure-required
  # checks below are keyed off eligibility_type %in% c("age_only",
  # "tenure_only", "age_and_tenure") and so already naturally exclude
  # "hazard_rate" -- min_age is optional (mandatory-ceiling semantics) and
  # min_tenure is unused by this type.
  if (identical(.defaults$eligibility_type, "hazard_rate")) {
    if (!is.null(policy_params$policy_table) &&
        !"hazard_rate" %in% names(policy_params$policy_table)) {
      stop(
        "policy_table must contain a 'hazard_rate' column when ",
        "eligibility_type = 'hazard_rate'.",
        call. = FALSE
      )
    }
    if (is.null(policy_params$group_cols) ||
        !("age_yr_int" %in% policy_params$group_cols)) {
      stop(
        "eligibility_type = 'hazard_rate' requires group_cols to include ",
        "'age_yr_int'.",
        call. = FALSE
      )
    }
  }

  # Validate eligibility parameters based on effective type
  .etype_varies <- !is.null(policy_params$policy_table) &&
    "eligibility_type" %in% names(policy_params$policy_table)
  .needs_age <- .etype_varies ||
    .defaults$eligibility_type %in% c("age_only", "age_and_tenure")
  .needs_tenure <- .etype_varies ||
    .defaults$eligibility_type %in% c("tenure_only", "age_and_tenure")

  if (.needs_age) {
    # min_age must be present in defaults or policy_table
    .age_in_pt <- !is.null(policy_params$policy_table) &&
      "min_age" %in% names(policy_params$policy_table)
    if (is.null(.defaults$min_age) && !.age_in_pt) {
      stop(
        "min_age is required for eligibility_type '",
        .defaults$eligibility_type,
        "'",
        call. = FALSE
      )
    }
    # birth_date_col is optional: NULL means age was pre-computed by the caller
    # (e.g. simulate_horizon Phase 1b) and is already present in personnel_dt.
    if (!is.null(birth_date_col)) {
      validate_column_exists(personnel_dt, birth_date_col, "personnel_dt")
    }
  }

  if (.needs_tenure) {
    .tenure_in_pt <- !is.null(policy_params$policy_table) &&
      "min_tenure" %in% names(policy_params$policy_table)
    if (is.null(.defaults$min_tenure) && !.tenure_in_pt) {
      stop(
        "min_tenure is required for eligibility_type '",
        .defaults$eligibility_type,
        "'",
        call. = FALSE
      )
    }
  }

  # Validate eligibility_rule when eligibility_type = "custom" is actually
  # in play. Unlike .needs_age/.needs_tenure above, this deliberately checks
  # whether "custom" is among the *actual values used* (defaults and/or the
  # policy_table column) rather than firing on .etype_varies alone -- a
  # policy_table that only ever contains "age_only"/"tenure_only" values
  # shouldn't be forced to also supply an eligibility_rule it will never use.
  .etype_values <- .defaults$eligibility_type
  if (
    !is.null(policy_params$policy_table) &&
      "eligibility_type" %in% names(policy_params$policy_table)
  ) {
    .etype_values <- c(
      .etype_values,
      unique(policy_params$policy_table$eligibility_type)
    )
  }
  .needs_rule <- "custom" %in% .etype_values

  if (.needs_rule) {
    .rule_in_pt <- !is.null(policy_params$policy_table) &&
      "eligibility_rule" %in% names(policy_params$policy_table)
    if (is.null(.defaults$eligibility_rule) && !.rule_in_pt) {
      stop(
        "eligibility_rule is required when eligibility_type includes ",
        "'custom'",
        call. = FALSE
      )
    }

    # Validate every unique rule string that could actually apply -- from
    # defaults and/or every distinct value present in the policy_table
    # column -- against the real available columns, fail-fast, before any
    # computation starts.
    .rules_to_check <- character(0)
    if (!is.null(.defaults$eligibility_rule)) {
      .rules_to_check <- c(.rules_to_check, .defaults$eligibility_rule)
    }
    if (.rule_in_pt) {
      .rules_to_check <- c(
        .rules_to_check,
        unique(policy_params$policy_table$eligibility_rule)
      )
    }

    .available_cols <- c(
      names(contract_dt),
      names(personnel_dt),
      "age",
      "tenure_years"
    )
    for (.rule in unique(.rules_to_check)) {
      validate_eligibility_rule(.rule, available_cols = .available_cols)
    }
  }

  # The columns retirement reads must exist under the caller's names
  .check_spec_columns(contract_dt, personnel_dt, column_spec_args, required = list(
    contract_dt  = c("contract_id_col", "personnel_id_col", "start_date_col",
                     "end_date_col", "contract_type_col"),
    personnel_dt = c("personnel_id_col", "status_col")
  ))

  return(invisible(TRUE))
}


#' Check Hiring Module Inputs
#'
#' @description
#' Validates inputs for simulate_hiring function. Checks data tables,
#' column existence, policy parameters, and hiring-specific requirements.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param policy_params List. Policy parameters
#' @param ref_date Date. Reference date
#' @param column_spec_args Complete column specification from
#'   \code{.column_spec()}.
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
check_hiring_inputs <- function(contract_dt,
                                personnel_dt,
                                policy_params,
                                ref_date,
                                column_spec_args = .column_spec()) {
  salary_col <- column_spec_args$salary_col

  # Validate data tables
  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")

  # Validate ref_date
  validate_date_format(ref_date, "ref_date")

  # Validate policy_params is a list
  if (!is.list(policy_params)) {
    stop("policy_params must be a list", call. = FALSE)
  }

  # Validate mode
  if (is.null(policy_params$mode)) {
    stop("policy_params must contain 'mode'", call. = FALSE)
  }

  valid_modes <- c("flow", "stock", "combined", "status_quo")
  validate_choice(policy_params$mode, valid_modes, "mode")

  # Validate mode-specific parameters
  if (policy_params$mode %in% c("flow", "combined")) {
    if (is.null(policy_params$replacement_rate)) {
      stop(
        "replacement_rate is required for mode '",
        policy_params$mode,
        "'",
        call. = FALSE
      )
    }

    # If replacement_rate is data.table, validate structure
    if (data.table::is.data.table(policy_params$replacement_rate)) {
      if (!"replacement_rate" %in% names(policy_params$replacement_rate)) {
        stop(
          "replacement_rate data.table must contain 'replacement_rate' column",
          call. = FALSE
        )
      }

      # Check group_cols present
      if (
        is.null(policy_params$group_cols) ||
          length(policy_params$group_cols) == 0
      ) {
        stop(
          "group_cols must be specified when replacement_rate is a data.table",
          call. = FALSE
        )
      }

      validate_columns_exist(
        policy_params$replacement_rate,
        policy_params$group_cols,
        "replacement_rate"
      )
    } else {
      # Scalar replacement_rate — 0 is valid (hiring policy active but zero flow)
      validate_positive_number(
        policy_params$replacement_rate,
        "replacement_rate",
        allow_zero = TRUE
      )
    }
  }

  if (policy_params$mode %in% c("stock", "combined")) {
    if (is.null(policy_params$stock_targets)) {
      stop(
        "stock_targets is required for mode '",
        policy_params$mode,
        "'",
        call. = FALSE
      )
    }

    if (!data.table::is.data.table(policy_params$stock_targets)) {
      stop("stock_targets must be a data.table", call. = FALSE)
    }

    if (!"target_stock" %in% names(policy_params$stock_targets)) {
      stop("stock_targets must contain 'target_stock' column", call. = FALSE)
    }

    # Validate group_cols if specified
    if (
      !is.null(policy_params$group_cols) && length(policy_params$group_cols) > 0
    ) {
      validate_columns_exist(
        policy_params$stock_targets,
        policy_params$group_cols,
        "stock_targets"
      )
    }
  }

  # Status-quo mode applies historical hiring rates, so the rate table must
  # already be there (simulate_horizon() estimates it from the panel).
  if (policy_params$mode == "status_quo") {
    .rates <- policy_params$squorate_dt
    if (!data.table::is.data.table(.rates) || !"hiring_rate" %in% names(.rates)) {
      stop(
        "squorate_dt (a data.table with a 'hiring_rate' column) is required for ",
        "mode 'status_quo'. simulate_horizon() estimates it from the panel; when ",
        "calling simulate_hiring() directly, create it with ",
        "estimate_historical_hiring_rates().",
        call. = FALSE
      )
    }
  }

  # Validate salary_scale if provided
  if (!is.null(policy_params$salary_scale)) {
    if (!data.table::is.data.table(policy_params$salary_scale)) {
      stop("salary_scale must be a data.table", call. = FALSE)
    }

    # Check for the configured salary column (not always literally
    # "gross_salary_lcu" -- e.g. simulate_allowances() explicitly recommends
    # a base-salary column like "base_salary_lcu" when allowances are also
    # being modeled, to avoid double-counting).
    if (!salary_col %in% names(policy_params$salary_scale)) {
      warning(
        "salary_scale does not contain '", salary_col, "' column. ",
        "Ensure it contains the appropriate salary column.",
        call. = FALSE
      )
    }
  }

  # Validate group_cols exist in contract_dt if specified
  if (
    !is.null(policy_params$group_cols) && length(policy_params$group_cols) > 0
  ) {
    validate_columns_exist(contract_dt, policy_params$group_cols, "contract_dt")
  }

  # The columns this module reads
  .check_spec_columns(contract_dt, personnel_dt, column_spec_args, required = list(
    contract_dt  = c("personnel_id_col", "contract_id_col", "start_date_col",
                     "end_date_col", "salary_col", "contract_type_col"),
    personnel_dt = c("personnel_id_col", "status_col")
  ))

  # For flow mode, validate retirement eligibility parameters if retirees_dt not provided
  # (these are needed if compute_flow_demand needs to calculate retirements internally)
  if (policy_params$mode == "flow") {
    # Birth date may be needed for retirement calculation
    if (!is.null(policy_params$eligibility_type)) {
      if (policy_params$eligibility_type %in% c("age_only", "age_and_tenure")) {
        .check_spec_columns(contract_dt, personnel_dt, column_spec_args,
                            required = list(personnel_dt = "birth_date_col"))
      }
    }
  }

  return(invisible(TRUE))
}


#' Check Promotions and Transfers Module Inputs
#'
#' @description
#' Validates all inputs for simulate_promotions_transfers. Checks data tables,
#' required columns, and policy parameter structure.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param policy_params List. Policy parameters
#' @param ref_date Date or character. Reference date
#' @param column_spec_args Complete column specification from
#'   \code{.column_spec()}.
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
check_movement_inputs <- function(
  contract_dt,
  personnel_dt,
  salary_scale_dt,
  policy_params,
  ref_date,
  column_spec_args = .column_spec()
) {
  # Validate data tables
  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")

  # Validate ref_date
  validate_date_format(ref_date, "ref_date")

  # Validate salary_scale_dt
  if (is.null(salary_scale_dt)) {
    stop(
      "salary_scale_dt is required (data.table keyed on group_cols with a salary column)",
      call. = FALSE
    )
  }
  if (!data.table::is.data.table(salary_scale_dt)) {
    stop("salary_scale_dt must be a data.table", call. = FALSE)
  }

  # Validate policy_params structure
  if (!is.list(policy_params)) {
    stop("policy_params must be a list", call. = FALSE)
  }

  # group_cols: NULL permitted (flat-rate path), but if supplied must be character
  if (
    !is.null(policy_params$group_cols) &&
      !is.character(policy_params$group_cols)
  ) {
    stop(
      "policy_params$group_cols must be a character vector or NULL",
      call. = FALSE
    )
  }

  # Flat-rate path: policy_table NULL → defaults$movement_rate required
  has_policy_table <- !is.null(policy_params$policy_table)
  has_movement_rate <- !is.null(policy_params$defaults$movement_rate)

  if (!has_policy_table && !has_movement_rate) {
    stop(
      paste0(
        "policy_params must include either 'policy_table' (data.table) or ",
        "'defaults$movement_rate' (numeric)"
      ),
      call. = FALSE
    )
  }

  if (has_movement_rate) {
    validate_positive_number(
      policy_params$defaults$movement_rate,
      "defaults$movement_rate",
      allow_zero = TRUE
    )
  }

  # Optional: validate movement_strategy if present
  if (!is.null(policy_params$defaults$movement_strategy)) {
    validate_choice(
      policy_params$defaults$movement_strategy,
      c("random", "tenure", "reverse_tenure", "wage_based"),
      "defaults$movement_strategy"
    )
  }

  # Optional: validate movement_type if present -- tells simulate_horizon()
  # whether to report this policy's mover count under n_promotions or
  # n_transfers (the movement mechanism itself is generic and has no way to
  # tell these apart from group_cols alone).
  if (!is.null(policy_params$defaults$movement_type)) {
    validate_choice(
      policy_params$defaults$movement_type,
      c("promotion", "transfer"),
      "defaults$movement_type"
    )
  }

  # salary_scale_dt must contain group_cols (if non-NULL group_cols supplied)
  if (!is.null(policy_params$group_cols)) {
    validate_columns_exist(
      salary_scale_dt,
      policy_params$group_cols,
      "salary_scale_dt"
    )
  }

  # salary_scale_dt must contain a salary column
  salary_pattern <- "salary|wage|compensation|remuneration"
  salary_candidates <- grep(
    salary_pattern,
    names(salary_scale_dt),
    value = TRUE,
    ignore.case = TRUE
  )
  if (length(salary_candidates) == 0) {
    stop(
      "salary_scale_dt must contain a salary column matching 'salary|wage|compensation|remuneration'",
      call. = FALSE
    )
  }

  # Validate group_cols exist in contract_dt
  if (!is.null(policy_params$group_cols)) {
    validate_columns_exist(contract_dt, policy_params$group_cols, "contract_dt")
  }

  # The columns this module reads
  .check_spec_columns(contract_dt, personnel_dt, column_spec_args, required = list(
    contract_dt  = c("personnel_id_col", "contract_id_col", "start_date_col",
                     "end_date_col", "salary_col", "contract_type_col"),
    personnel_dt = c("personnel_id_col", "status_col")
  ))

  return(invisible(TRUE))
}


#' Validate Inputs for Allowance Simulation
#'
#' @description
#' Composite validator for \code{\link{simulate_allowances}}, following the
#' same pattern as \code{\link{check_retirement_inputs}} /
#' \code{\link{check_hiring_inputs}} / \code{\link{check_movement_inputs}}.
#' Not called when \code{policy_params} is \code{NULL} -- that short-circuits
#' to a no-op before validation.
#'
#' @param contract_dt data.table. Current-period contract snapshot.
#' @param personnel_dt data.table. Current-period personnel snapshot.
#' @param policy_params List. See \code{\link{resolve_allowance_amounts}} for
#'   the expected shape.
#' @param ref_date Date. The reference date for this period -- matches
#'   \code{check_retirement_inputs()}'s own parameter name.
#' @param column_spec_args Complete column specification from
#'   \code{.column_spec()}.
#'
#' @return Invisible \code{TRUE} if valid; stops with an informative error
#'   otherwise.
#' @keywords internal
check_allowance_inputs <- function(contract_dt,
                                   personnel_dt,
                                   policy_params,
                                   ref_date,
                                   column_spec_args = .column_spec()) {

  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")
  validate_date_format(ref_date, "ref_date")
  .check_spec_columns(contract_dt, personnel_dt, column_spec_args, required = list(
    contract_dt = c("contract_id_col", "personnel_id_col", "contract_type_col")
  ))

  validate_policy_table(policy_params)

  if (is.null(policy_params$defaults$amount)) {
    stop("check_allowance_inputs: policy_params$defaults must contain 'amount' ",
         "(the fallback allowance amount for unmatched groups).", call. = FALSE)
  }

  if (!is.null(policy_params$group_cols) &&
      "allowance_type" %in% policy_params$group_cols &&
      !"allowance_type" %in% names(policy_params$policy_table)) {
    stop("check_allowance_inputs: policy_params$group_cols includes ",
         "'allowance_type' but policy_table has no 'allowance_type' column.",
         call. = FALSE)
  }

  if (!is.null(policy_params$policy_table) &&
      !"amount" %in% names(policy_params$policy_table)) {
    stop("check_allowance_inputs: policy_table must contain an 'amount' column.",
         call. = FALSE)
  }

  # Multiplier slot: same NULL/NULL consistency rule as the main 3-slot pattern
  mgc <- policy_params$multiplier_group_cols
  mpt <- policy_params$multiplier_policy_table
  if (!is.null(mgc) && is.null(mpt)) {
    stop("check_allowance_inputs: multiplier_group_cols supplied but ",
         "multiplier_policy_table is NULL.", call. = FALSE)
  }
  if (!is.null(mgc) && "allowance_type" %in% mgc) {
    stop("check_allowance_inputs: multiplier_group_cols must not include ",
         "'allowance_type' -- the multiplier is resolved per contract, ",
         "not per type.", call. = FALSE)
  }
  if (!is.null(mpt) && !"total_allowance_multiplier" %in% names(mpt)) {
    stop("check_allowance_inputs: multiplier_policy_table must contain a ",
         "'total_allowance_multiplier' column.", call. = FALSE)
  }

  invisible(TRUE)
}


## column-name specification and output options (ADR 0012)

# Default column names: the govhr harmonized names. The caller's names are
# used as given all the way down; nothing is renamed. For the optional keys,
# NULL means: age_col / tenure_col -- do not read a precomputed column, derive
# the values (from birth dates / contract histories); birth_date_col /
# hire_date_col -- not available.
.COLUMN_SPEC_DEFAULTS <- list(
  personnel_id_col  = "personnel_id",
  contract_id_col   = "contract_id",
  ref_date_col      = "ref_date",
  start_date_col    = "start_date",
  end_date_col      = "end_date",
  salary_col        = "gross_salary_lcu",
  contract_type_col = "contract_type",
  status_col        = "employment_status",
  age_col           = "age",
  tenure_col        = "personnel_tenure",
  birth_date_col    = "birth_date",
  hire_date_col     = NULL
)
.COLUMN_SPEC_OPTIONAL <- c("age_col", "tenure_col", "birth_date_col", "hire_date_col")

.OUTPUT_CONTROL_DEFAULTS <- list(
  scenario_name    = NULL,
  is_baseline      = FALSE,
  return_microdata = FALSE
)

# Stop if `args` has keys outside `valid`, listing the valid keys.
.reject_unknown_keys <- function(args, valid, arg_name) {
  unknown <- setdiff(names(args), valid)
  if (length(unknown) > 0L) {
    stop(
      arg_name, ": unknown key(s) ", paste0("'", unknown, "'", collapse = ", "),
      ". Valid keys: ", paste(valid, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Build and Validate the Column-Name Specification
#'
#' Fills the caller's \code{column_spec_args} with the govhr defaults and
#' checks it: only known keys; each value a single column name (\code{NULL}
#' allowed for the optional keys); no two keys naming the same column.
#'
#' Every exported function that reads caller columns builds its spec here,
#' and internal functions receive the complete spec (or, for small helpers,
#' just the column names they read).
#'
#' @param column_spec_args Named list supplied by the caller (may be empty).
#' @return The complete specification: a named list with every key in
#'   \code{.COLUMN_SPEC_DEFAULTS}.
#' @noRd
.column_spec <- function(column_spec_args = list()) {
  if (is.null(column_spec_args)) column_spec_args <- list()
  if (!is.list(column_spec_args)) {
    stop("column_spec_args must be a named list, e.g. list(salary_col = \"base_salary_lcu\").",
         call. = FALSE)
  }
  .reject_unknown_keys(column_spec_args, names(.COLUMN_SPEC_DEFAULTS), "column_spec_args")

  # Each supplied value: one non-missing string, or NULL where optional
  for (key in names(column_spec_args)) {
    value <- column_spec_args[[key]]
    if (is.null(value) && key %in% .COLUMN_SPEC_OPTIONAL) next
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop(key, " must be a single column name (a string)", call. = FALSE)
    }
  }

  # modifyList() drops NULL entries, so assign key by key to keep explicit NULLs
  spec <- .COLUMN_SPEC_DEFAULTS
  for (key in names(column_spec_args)) spec[key] <- list(column_spec_args[[key]])

  # Two arguments must not name the same column
  named <- unlist(Filter(Negate(is.null), spec))
  dup <- named[duplicated(named) | duplicated(named, fromLast = TRUE)]
  if (length(dup) > 0L) {
    col <- dup[[1L]]
    stop(
      paste(names(dup)[dup == col], collapse = " and "),
      " both name column '", col, "'. Each argument must name a different column.",
      call. = FALSE
    )
  }
  spec
}

#' Check That the Specified Columns Exist in the Data
#'
#' Each exported function checks only the keys it reads, so one spec can be
#' reused across calls; keys set to \code{NULL} are skipped.
#'
#' @param contract_dt,personnel_dt The caller's tables.
#' @param spec Complete specification from \code{.column_spec()}.
#' @param required Named list: for each table, the spec keys whose columns
#'   it must contain. The default is what \code{simulate_horizon()} needs.
#' @return \code{invisible(TRUE)}; stops naming the argument and table of the
#'   first missing column.
#' @noRd
.check_spec_columns <- function(contract_dt, personnel_dt, spec,
                                required = list(
                                  contract_dt  = c("personnel_id_col", "contract_id_col",
                                                   "start_date_col", "end_date_col",
                                                   "salary_col", "contract_type_col"),
                                  personnel_dt = c("personnel_id_col", "status_col",
                                                   "hire_date_col")
                                )) {
  tables <- list(contract_dt = contract_dt, personnel_dt = personnel_dt)
  for (tbl in names(required)) {
    for (key in required[[tbl]]) {
      if (is.null(spec[[key]])) next
      if (!spec[[key]] %in% names(tables[[tbl]])) {
        stop(key, " = '", spec[[key]], "': column not found in ", tbl, ".",
             call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

#' Build and Validate the Output Options
#'
#' @param output_control_args Named list supplied by the caller (may be empty).
#' @return The complete options: \code{scenario_name}, \code{is_baseline},
#'   \code{return_microdata}.
#' @noRd
.output_control <- function(output_control_args) {
  if (is.null(output_control_args)) output_control_args <- list()
  if (!is.list(output_control_args)) {
    stop("output_control_args must be a named list.", call. = FALSE)
  }
  .reject_unknown_keys(output_control_args, names(.OUTPUT_CONTROL_DEFAULTS),
                       "output_control_args")
  out <- .OUTPUT_CONTROL_DEFAULTS
  for (key in names(output_control_args)) out[key] <- list(output_control_args[[key]])

  # scenario_name labels the run; the switches are single TRUE/FALSE values
  if (!is.null(out$scenario_name) &&
      (!is.character(out$scenario_name) || length(out$scenario_name) != 1L ||
       is.na(out$scenario_name))) {
    stop("scenario_name must be a single character string or NULL.", call. = FALSE)
  }
  for (key in c("is_baseline", "return_microdata")) {
    if (!is.logical(out[[key]]) || length(out[[key]]) != 1L || is.na(out[[key]])) {
      stop(key, " must be TRUE or FALSE.", call. = FALSE)
    }
  }
  out
}


## custom retirement eligibility validation

.ALLOWED_RULE_CALLS <- c(
  "(",
  "&",
  "|",
  "!",
  "&&",
  "||",
  ">",
  ">=",
  "<",
  "<=",
  "==",
  "!=",
  "%in%",
  "c",
  "is.na"
)


.ALLOWED_FORMULA_CALLS <- c(
  "(",
  "+",
  "-",
  "*",
  "/",
  "^",
  "c",
  "is.na",
  "pmin",
  "pmax",
  "round",
  "abs"
)

#' Recursively Validate That an Expression Only Uses Safe Calls
#'
#' @description
#' Walks a parsed R expression (as produced by \code{\link{str2lang}}) and
#' throws an error if it contains any function/operator call not present in
#' \code{allowed_calls}. This guards user-supplied eligibility rules and
#' pension formulas, which would otherwise be able to call arbitrary R code
#' (e.g. \code{system()}) when evaluated later via \code{eval()}. It is run
#' both when a policy is validated and immediately before evaluation.
#'
#' @param expr A parsed R language object (call, name, or atomic value), as
#'   returned by \code{str2lang()}.
#' @param allowed_calls Character vector of function/operator names that may
#'   be called (\code{.ALLOWED_RULE_CALLS} or \code{.ALLOWED_FORMULA_CALLS}).
#' @param label Character scalar naming the user input being checked
#'   (\code{"eligibility_rule"} or \code{"pension_formula"}); used as the
#'   prefix of every error message.
#'
#' @details
#' The check is recursive: for a call, it validates the call's own
#' function/operator name and then recurses into each argument. Names (bare
#' column references) and atomic values (numbers, strings, logicals) are
#' always considered safe and terminate the recursion. A call whose head is
#' not a plain name -- \code{pkg::fn(...)}, \code{(function() ...)()} -- is
#' rejected outright.
#'
#' @return \code{TRUE} if \code{expr} is a safe name/atomic value. Otherwise
#'   called for its side effect of throwing an error; the return value is not
#'   meaningful for call expressions.
#'
#' @keywords internal
.check_safe_calls <- function(expr,
                              allowed_calls = .ALLOWED_RULE_CALLS,
                              label = "eligibility_rule") {
  if (is.call(expr)) {
    # Only a plain function name can be called: pkg::fn() and anonymous
    # functions have a call as their head and are rejected here.
    if (!is.name(expr[[1]])) {
      stop(
        label, ": only plain function names can be called; '",
        paste(deparse(expr[[1]]), collapse = " "), "' is not allowed.",
        call. = FALSE
      )
    }

    # The function itself must be on the allow-list.
    func_name <- as.character(expr[[1]])
    if (!func_name %in% allowed_calls) {
      stop(
        label, ": disallowed function/operator '",
        func_name,
        "'. Allowed: ",
        paste(allowed_calls, collapse = ", "),
        call. = FALSE
      )
    }

    # Recursively check each argument of the call.
    for (arg in as.list(expr[-1])) {
      .check_safe_calls(arg, allowed_calls, label)
    }
  } else if (is.name(expr) || is.atomic(expr)) {
    # Bare column references and literal values are always safe.
    return(TRUE)
  } else {
    stop(
      "Invalid expression in ", label, ": ",
      deparse(expr),
      ". Only the allowed operators and functions can be used.",
      call. = FALSE
    )
  }
}

#' Validate a User-Supplied Eligibility Rule String
#'
#' @description
#' Parses an \code{eligibility_rule} string (for example,
#' \code{"age >= 60 & tenure_years >= 5"}), checks that it only uses
#' allow-listed operators/functions via \code{\link{.check_safe_calls}}, and
#' optionally verifies that every referenced column exists in
#' \code{available_cols}.
#'
#' @param rule Character scalar. Rule expression string.
#' @param available_cols Character vector or \code{NULL}. If supplied, all
#'   symbols in \code{rule} must be members of this set.
#'
#' @return Invisibly returns the parsed expression.
#'
#' @details
#' This function is primarily called for validation side effects. It errors on
#' parse failures, disallowed calls, or references to unavailable columns.
#'
#' @keywords internal
validate_eligibility_rule <- function(rule, available_cols = NULL) {
  validate_character_string(rule, "eligibility_rule")

  # Parse the rule string into an expression; report parse failures clearly.
  expr <- tryCatch(str2lang(rule), error = function(e) {
    stop(
      "eligibility_rule could not be parsed as an R expression: ",
      conditionMessage(e),
      call. = FALSE
    )
  })

  # Reject any expression that uses functions/operators outside the allow-list.
  .check_safe_calls(expr)

  # Optionally verify that every column referenced in the rule is available.
  if (!is.null(available_cols)) {
    used_cols <- all.vars(expr)
    missing_cols <- setdiff(used_cols, available_cols)
    if (length(missing_cols) > 0L) {
      stop(
        "eligibility_rule references column(s) not found: ",
        paste(missing_cols, collapse = ", "),
        ". Available columns: ",
        paste(available_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(expr)
}


#' Validate a User-Supplied Pension Formula String
#' 
#' @description
#' Parses a \code{pension_formula} string (for example, \code{"salary * 0.02 * tenure_years"}), 
#' checks that it only uses allow-listed operators/functions via \code{\link{.check_safe_calls}}, 
#' and optionally verifies that every referenced column exists in \code{available_cols}.
#' 
#' @param formula Character scalar. Formula expression string.
#' @param available_cols Character vector or \code{NULL}. If supplied, all
#'   symbols in \code{formula} must be members of this set.
#'
#' @return Invisibly returns the parsed expression.
#'
#' @details
#' This function is primarily called for validation side effects. It errors on
#' parse failures, disallowed calls, or references to unavailable columns.
#'
#' @keywords internal
validate_pension_formula <- function(formula, available_cols = NULL) {
  validate_character_string(formula, "pension_formula")

  # Parse the formula string into an expression; report parse failures clearly.
  expr <- tryCatch(str2lang(formula), error = function(e) {
    stop(
      "pension_formula could not be parsed as an R expression: ",
      conditionMessage(e),
      call. = FALSE
    )
  })

  # Reject any expression that uses functions/operators outside the allow-list.
  .check_safe_calls(expr, allowed_calls = .ALLOWED_FORMULA_CALLS,
                    label = "pension_formula")

  # Optionally verify that every column referenced in the formula is available.
  if (!is.null(available_cols)) {
    used_cols <- all.vars(expr)
    missing_cols <- setdiff(used_cols, available_cols)
    if (length(missing_cols) > 0L) {
      stop(
        "pension_formula references column(s) not found: ",
        paste(missing_cols, collapse = ", "),
        ". Available columns: ",
        paste(available_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(expr)
}