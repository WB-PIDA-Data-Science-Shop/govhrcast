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
      "Column '", colname, "' not found in ", varname,
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
      "Columns not found in ", varname, ": ",
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
    tryCatch({
      date <- as.Date(date)
    }, error = function(e) {
      stop(varname, " must be a valid date string (e.g., '2024-01-01') or Date object. ",
           "Error: ", e$message, call. = FALSE)
    })
  }
  
  if (!inherits(date, "Date")) {
    stop(varname, " must be a Date object or date string (e.g., '2024-01-01')", call. = FALSE)
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
    if (!is.numeric(param_spec) || length(param_spec) != 1L || is.na(param_spec))
      stop(param_name, " must be a single non-NA numeric scalar or a param ",
           "spec list (list(default, group_cols, policy_table))", call. = FALSE)
    if (param_spec <= 0)
      stop(param_name, " must be > 0", call. = FALSE)
    return(invisible(TRUE))
  }

  # --- Three-slot list path -----------------------------------------------
  group_cols   <- param_spec$group_cols
  default      <- param_spec$default
  policy_table <- param_spec$policy_table

  # default: must be positive scalar or NULL
  if (!is.null(default)) {
    if (!is.numeric(default) || length(default) != 1L || is.na(default))
      stop(param_name, "$default must be a single non-NA numeric scalar or NULL",
           call. = FALSE)
    if (default <= 0)
      stop(param_name, "$default must be > 0", call. = FALSE)
  }

  # group_cols / policy_table must both be set or both be NULL
  if (is.null(group_cols) && !is.null(policy_table))
    stop(param_name, ": policy_table supplied but group_cols is NULL. ",
         "Set group_cols to the column(s) to join on.", call. = FALSE)

  if (!is.null(group_cols) && is.null(policy_table))
    stop(param_name, ": group_cols supplied but policy_table is NULL. ",
         "Provide a policy_table or remove group_cols.", call. = FALSE)

  # If group-level, validate the policy_table
  if (!is.null(group_cols)) {
    if (!data.table::is.data.table(policy_table))
      stop(param_name, "$policy_table must be a data.table", call. = FALSE)

    missing_gcols <- setdiff(group_cols, names(policy_table))
    if (length(missing_gcols) > 0L)
      stop(param_name, "$policy_table is missing group_cols column(s): ",
           paste(missing_gcols, collapse = ", "), call. = FALSE)

    if (!param_name %in% names(policy_table))
      stop(param_name, "$policy_table must contain a column named '",
           param_name, "'", call. = FALSE)

    if (nrow(policy_table) == 0L)
      stop(param_name, "$policy_table has 0 rows", call. = FALSE)
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
      varname, " must be between ", min, " and ", max,
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
      "Invalid ", varname, ": '", choice, "'. ",
      "Valid options: ", paste(valid_choices, collapse = ", "),
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
      "Missing required parameters for ", context, ": ",
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

  if (!is.list(policy_params))
    stop("policy_params must be a list", call. = FALSE)

  if (is.null(policy_params$defaults) || !is.list(policy_params$defaults))
    stop("policy_params must contain a 'defaults' list", call. = FALSE)

  gc <- policy_params$group_cols
  pt <- policy_params$policy_table

  if (!is.null(gc) && is.null(pt))
    stop("policy_params: group_cols supplied but policy_table is NULL. ",
         "Provide a policy_table or set group_cols = NULL.", call. = FALSE)

  if (is.null(gc) && !is.null(pt))
    stop("policy_params: policy_table supplied but group_cols is NULL. ",
         "Provide group_cols or set policy_table = NULL.", call. = FALSE)

  if (!is.null(pt)) {
    if (!data.table::is.data.table(pt))
      stop("policy_params$policy_table must be a data.table", call. = FALSE)

    if (nrow(pt) == 0L)
      stop("policy_params$policy_table has 0 rows", call. = FALSE)

    missing_gc <- setdiff(gc, names(pt))
    if (length(missing_gc) > 0L)
      stop("policy_params$policy_table is missing group_cols column(s): ",
           paste(missing_gc, collapse = ", "), call. = FALSE)
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
#' @param personnel_id_col Character.  Must exist in both \code{contract_dt}
#'   and \code{personnel_dt}.  Default \code{"personnel_id"}.
#' @param birth_date_col Character or \code{NULL}.  Must exist in
#'   \code{personnel_dt} when \code{eligibility_type} involves age.  Pass
#'   \code{NULL} when age is pre-computed by the caller.  Default
#'   \code{"birth_date"}.
#' @param contract_id_col Character.  Default \code{"contract_id"}.
#' @param start_date_col Character.  Default \code{"start_date"}.
#' @param end_date_col Character.  Default \code{"end_date"}.
#' @param contract_type_col Character.  Default \code{"contract_type_code"}.
#' @param status_col Character.  Default \code{"status"}.
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
                                    personnel_id_col = "personnel_id",
                                    birth_date_col = "birth_date",
                                    contract_id_col = "contract_id",
                                    start_date_col = "start_date",
                                    end_date_col = "end_date",
                                    contract_type_col = "contract_type_code",
                                    status_col = "status") {
  
  # Validate data tables
  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")

  # Validate ref_date
  validate_date_format(ref_date, "ref_date")

  # Validate top-level structure
  if (!is.list(policy_params)) {
    stop("policy_params must be a list", call. = FALSE)
  }
  validate_policy_table(policy_params)

  .defaults <- policy_params$defaults

  # Check required top-level params in defaults
  required_top <- c("eligibility_type", "pension_type")
  validate_required_params(.defaults, required_top, "retirement policy")

  # Validate eligibility_type value(s)
  valid_eligibility <- c("age_only", "tenure_only", "age_and_tenure")
  # Check defaults
  validate_choice(.defaults$eligibility_type, valid_eligibility, "eligibility_type")
  # Also check any values in policy_table if present
  if (!is.null(policy_params$policy_table) &&
      "eligibility_type" %in% names(policy_params$policy_table)) {
    bad <- setdiff(unique(policy_params$policy_table$eligibility_type), valid_eligibility)
    if (length(bad) > 0L)
      stop("Invalid eligibility_type in policy_table: '",
           paste(bad, collapse = "', '"),
           "'. Valid options: ", paste(valid_eligibility, collapse = ", "),
           call. = FALSE)
  }

  # Validate pension_type value(s)
  valid_pension <- c("db", "dc", "flat", "hybrid")
  validate_choice(.defaults$pension_type, valid_pension, "pension_type")
  if (!is.null(policy_params$policy_table) &&
      "pension_type" %in% names(policy_params$policy_table)) {
    bad <- setdiff(unique(policy_params$policy_table$pension_type), valid_pension)
    if (length(bad) > 0L)
      stop("Invalid pension_type in policy_table: '",
           paste(bad, collapse = "', '"),
           "'. Valid options: ", paste(valid_pension, collapse = ", "),
           call. = FALSE)
  }

  # Validate eligibility parameters based on effective type
  .etype_varies <- !is.null(policy_params$policy_table) &&
                   "eligibility_type" %in% names(policy_params$policy_table)
  .needs_age    <- .etype_varies ||
                   .defaults$eligibility_type %in% c("age_only", "age_and_tenure")
  .needs_tenure <- .etype_varies ||
                   .defaults$eligibility_type %in% c("tenure_only", "age_and_tenure")

  if (.needs_age) {
    # min_age must be present in defaults or policy_table
    .age_in_pt <- !is.null(policy_params$policy_table) &&
                   "min_age" %in% names(policy_params$policy_table)
    if (is.null(.defaults$min_age) && !.age_in_pt)
      stop("min_age is required for eligibility_type '",
           .defaults$eligibility_type, "'", call. = FALSE)
    # birth_date_col is optional: NULL means age was pre-computed by the caller
    # (e.g. simulate_horizon Phase 1b) and is already present in personnel_dt.
    if (!is.null(birth_date_col))
      validate_column_exists(personnel_dt, birth_date_col, "personnel_dt")
  }

  if (.needs_tenure) {
    .tenure_in_pt <- !is.null(policy_params$policy_table) &&
                      "min_tenure" %in% names(policy_params$policy_table)
    if (is.null(.defaults$min_tenure) && !.tenure_in_pt)
      stop("min_tenure is required for eligibility_type '",
           .defaults$eligibility_type, "'", call. = FALSE)
  }

  # Check required contract_dt columns
  required_contract_cols <- c(contract_id_col, personnel_id_col, start_date_col,
                              end_date_col, contract_type_col)
  validate_columns_exist(contract_dt, required_contract_cols, "contract_dt")

  # Check required personnel_dt columns
  required_personnel_cols <- c(personnel_id_col, status_col)
  validate_columns_exist(personnel_dt, required_personnel_cols, "personnel_dt")

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
#' @param personnel_id_col Character. Personnel ID column name
#' @param birth_date_col Character. Birth date column name
#' @param contract_id_col Character. Contract ID column name
#' @param start_date_col Character. Start date column name
#' @param end_date_col Character. End date column name
#' @param contract_type_col Character. Contract type column name
#' @param status_col Character. Status column name
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
check_hiring_inputs <- function(contract_dt,
                                personnel_dt,
                                policy_params,
                                ref_date,
                                personnel_id_col = "personnel_id",
                                birth_date_col = "birth_date",
                                contract_id_col = "contract_id",
                                start_date_col = "start_date",
                                end_date_col = "end_date",
                                contract_type_col = "contract_type_code",
                                status_col = "status") {
  
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
      stop("replacement_rate is required for mode '", policy_params$mode, "'", 
           call. = FALSE)
    }
    
    # If replacement_rate is data.table, validate structure
    if (data.table::is.data.table(policy_params$replacement_rate)) {
      if (!"replacement_rate" %in% names(policy_params$replacement_rate)) {
        stop("replacement_rate data.table must contain 'replacement_rate' column", 
             call. = FALSE)
      }
      
      # Check group_cols present
      if (is.null(policy_params$group_cols) || length(policy_params$group_cols) == 0) {
        stop("group_cols must be specified when replacement_rate is a data.table", 
             call. = FALSE)
      }
      
      validate_columns_exist(
        policy_params$replacement_rate,
        policy_params$group_cols,
        "replacement_rate"
      )
    } else {
      # Scalar replacement_rate — 0 is valid (hiring policy active but zero flow)
      validate_positive_number(policy_params$replacement_rate, "replacement_rate",
                               allow_zero = TRUE)
    }
  }
  
  if (policy_params$mode %in% c("stock", "combined")) {
    if (is.null(policy_params$stock_targets)) {
      stop("stock_targets is required for mode '", policy_params$mode, "'", 
           call. = FALSE)
    }
    
    if (!data.table::is.data.table(policy_params$stock_targets)) {
      stop("stock_targets must be a data.table", call. = FALSE)
    }
    
    if (!"target_stock" %in% names(policy_params$stock_targets)) {
      stop("stock_targets must contain 'target_stock' column", call. = FALSE)
    }
    
    # Validate group_cols if specified
    if (!is.null(policy_params$group_cols) && length(policy_params$group_cols) > 0) {
      validate_columns_exist(
        policy_params$stock_targets,
        policy_params$group_cols,
        "stock_targets"
      )
    }
  }
  
  # Validate salary_scale if provided
  if (!is.null(policy_params$salary_scale)) {
    if (!data.table::is.data.table(policy_params$salary_scale)) {
      stop("salary_scale must be a data.table", call. = FALSE)
    }
    
    # Check for salary column (default name)
    if (!"gross_salary_lcu" %in% names(policy_params$salary_scale)) {
      warning("salary_scale does not contain 'gross_salary_lcu' column. ",
              "Ensure it contains the appropriate salary column.", 
              call. = FALSE)
    }
  }
  
  # Validate group_cols exist in contract_dt if specified
  if (!is.null(policy_params$group_cols) && length(policy_params$group_cols) > 0) {
    validate_columns_exist(contract_dt, policy_params$group_cols, "contract_dt")
  }
  
  # Check required contract_dt columns
  required_contract_cols <- c(contract_id_col, personnel_id_col, start_date_col,
                              end_date_col, contract_type_col)
  validate_columns_exist(contract_dt, required_contract_cols, "contract_dt")
  
  # Check required personnel_dt columns
  required_personnel_cols <- c(personnel_id_col, status_col)
  validate_columns_exist(personnel_dt, required_personnel_cols, "personnel_dt")
  
  # For flow mode, validate retirement eligibility parameters if retirees_dt not provided
  # (these are needed if compute_flow_demand needs to calculate retirements internally)
  if (policy_params$mode == "flow") {
    # Birth date may be needed for retirement calculation
    if (!is.null(policy_params$eligibility_type)) {
      if (policy_params$eligibility_type %in% c("age_only", "age_and_tenure")) {
        validate_column_exists(personnel_dt, birth_date_col, "personnel_dt")
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
#' @param personnel_id_col Character. Personnel ID column name
#' @param start_date_col Character. Start date column name
#' @param end_date_col Character. End date column name
#' @param contract_type_col Character. Contract type column name
#' @param status_col Character. Status column name
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
check_movement_inputs <- function(contract_dt,
                                  personnel_dt,
                                  salary_scale_dt,
                                  policy_params,
                                  ref_date,
                                  personnel_id_col  = "personnel_id",
                                  start_date_col    = "start_date",
                                  end_date_col      = "end_date",
                                  contract_type_col = "contract_type_code",
                                  status_col        = "status") {

  # Validate data tables
  validate_datatable(contract_dt,  "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")

  # Validate ref_date
  validate_date_format(ref_date, "ref_date")

  # Validate salary_scale_dt
  if (is.null(salary_scale_dt)) {
    stop("salary_scale_dt is required (data.table keyed on group_cols with a salary column)",
         call. = FALSE)
  }
  if (!data.table::is.data.table(salary_scale_dt)) {
    stop("salary_scale_dt must be a data.table", call. = FALSE)
  }

  # Validate policy_params structure
  if (!is.list(policy_params)) {
    stop("policy_params must be a list", call. = FALSE)
  }

  # group_cols: NULL permitted (flat-rate path), but if supplied must be character
  if (!is.null(policy_params$group_cols) && !is.character(policy_params$group_cols)) {
    stop("policy_params$group_cols must be a character vector or NULL", call. = FALSE)
  }

  # Flat-rate path: policy_table NULL → defaults$movement_rate required
  has_policy_table <- !is.null(policy_params$policy_table)
  has_movement_rate <- !is.null(policy_params$defaults$movement_rate)

  if (!has_policy_table && !has_movement_rate) {
    stop(paste0(
      "policy_params must include either 'policy_table' (data.table) or ",
      "'defaults$movement_rate' (numeric)"
    ), call. = FALSE)
  }

  if (has_movement_rate) {
    validate_positive_number(policy_params$defaults$movement_rate,
                             "defaults$movement_rate", allow_zero = TRUE)
  }

  # Optional: validate movement_strategy if present
  if (!is.null(policy_params$defaults$movement_strategy)) {
    validate_choice(policy_params$defaults$movement_strategy,
                    c("random", "tenure", "reverse_tenure", "wage_based"),
                    "defaults$movement_strategy")
  }

  # salary_scale_dt must contain group_cols (if non-NULL group_cols supplied)
  if (!is.null(policy_params$group_cols)) {
    validate_columns_exist(salary_scale_dt, policy_params$group_cols, "salary_scale_dt")
  }

  # salary_scale_dt must contain a salary column
  salary_pattern    <- "salary|wage|compensation|remuneration"
  salary_candidates <- grep(salary_pattern, names(salary_scale_dt),
                            value = TRUE, ignore.case = TRUE)
  if (length(salary_candidates) == 0) {
    stop("salary_scale_dt must contain a salary column matching 'salary|wage|compensation|remuneration'",
         call. = FALSE)
  }

  # Validate group_cols exist in contract_dt
  if (!is.null(policy_params$group_cols)) {
    validate_columns_exist(contract_dt, policy_params$group_cols, "contract_dt")
  }

  # Required contract_dt columns
  required_contract_cols <- c(personnel_id_col, start_date_col,
                              end_date_col, contract_type_col)
  validate_columns_exist(contract_dt, required_contract_cols, "contract_dt")

  # Required personnel_dt columns
  required_personnel_cols <- c(personnel_id_col, status_col)
  validate_columns_exist(personnel_dt, required_personnel_cols, "personnel_dt")

  return(invisible(TRUE))
}
