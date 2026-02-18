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
  if (!inherits(date, "Date")) {
    stop(varname, " must be a Date object", call. = FALSE)
  }
  
  if (length(date) != 1) {
    stop(varname, " must be a single Date value", call. = FALSE)
  }
  
  if (is.na(date)) {
    stop(varname, " cannot be NA", call. = FALSE)
  }
  
  return(invisible(TRUE))
}

#' Validate Positive Number
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

#' Check Retirement Module Inputs
#'
#' @description
#' Validates all inputs for retirement simulation, including data tables,
#' policy parameters, and column specifications.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param policy_params List. Policy parameters
#' @param ref_date Date. Reference date
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
check_retirement_inputs <- function(contract_dt, 
                                    personnel_dt, 
                                    policy_params,
                                    ref_date) {
  
  # Validate data tables
  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")
  
  # Validate ref_date
  validate_date_format(ref_date, "ref_date")
  
  # Validate policy_params structure
  if (!is.list(policy_params)) {
    stop("policy_params must be a list", call. = FALSE)
  }
  
  # Check for required top-level parameters
  required_top <- c("eligibility_type", "pension_type")
  validate_required_params(policy_params, required_top, "retirement policy")
  
  # Validate eligibility_type
  valid_eligibility <- c("age_only", "tenure_only", "age_and_tenure")
  validate_choice(
    policy_params$eligibility_type, 
    valid_eligibility, 
    "eligibility_type"
  )
  
  # Validate pension_type
  valid_pension <- c("db", "dc", "flat", "hybrid")
  validate_choice(
    policy_params$pension_type,
    valid_pension,
    "pension_type"
  )
  
  # Validate eligibility parameters based on type
  if (policy_params$eligibility_type %in% c("age_only", "age_and_tenure")) {
    if (is.null(policy_params$min_age)) {
      stop("min_age is required for eligibility_type '", 
           policy_params$eligibility_type, "'", call. = FALSE)
    }
    validate_positive_number(policy_params$min_age, "min_age")
    validate_column_exists(personnel_dt, "birth_date", "personnel_dt")
  }
  
  if (policy_params$eligibility_type %in% c("tenure_only", "age_and_tenure")) {
    if (is.null(policy_params$min_tenure)) {
      stop("min_tenure is required for eligibility_type '", 
           policy_params$eligibility_type, "'", call. = FALSE)
    }
    validate_positive_number(policy_params$min_tenure, "min_tenure")
  }
  
  # Validate pension parameters exist
  if (is.null(policy_params$pension_params)) {
    stop("pension_params must be specified in policy_params", call. = FALSE)
  }
  
  # Check required contract_dt columns
  required_contract_cols <- c("contract_id", "personnel_id", "start_date", 
                              "end_date", "contract_type_code")
  validate_columns_exist(contract_dt, required_contract_cols, "contract_dt")
  
  # Check required personnel_dt columns
  required_personnel_cols <- c("personnel_id", "status")
  validate_columns_exist(personnel_dt, required_personnel_cols, "personnel_dt")
  
  return(invisible(TRUE))
}
