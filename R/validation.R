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
    validate_column_exists(personnel_dt, birth_date_col, "personnel_dt")
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

  # Validate policy_params
  if (!is.list(policy_params)) {
    stop("policy_params must be a list", call. = FALSE)
  }

  # Required: group_cols
  if (is.null(policy_params$group_cols) || length(policy_params$group_cols) == 0) {
    stop("policy_params must contain 'group_cols' (character vector of state-defining columns)",
         call. = FALSE)
  }
  if (!is.character(policy_params$group_cols)) {
    stop("policy_params$group_cols must be a character vector", call. = FALSE)
  }

  # Required: salary_scale
  if (is.null(policy_params$salary_scale)) {
    stop("policy_params must contain 'salary_scale' (data.table keyed on group_cols)",
         call. = FALSE)
  }
  if (!data.table::is.data.table(policy_params$salary_scale)) {
    stop("policy_params$salary_scale must be a data.table", call. = FALSE)
  }

  # salary_scale must contain group_cols
  validate_columns_exist(policy_params$salary_scale, policy_params$group_cols, "salary_scale")

  # salary_scale must contain a salary column
  # Match salary/wage/compensation as whole words or substrings, but avoid
  # false positives like 'paygrade' (which contains 'pay' but is not a salary col)
  salary_pattern    <- "salary|wage|compensation|remuneration"
  salary_candidates <- grep(salary_pattern, names(policy_params$salary_scale),
                            value = TRUE, ignore.case = TRUE)
  if (length(salary_candidates) == 0) {
    stop("salary_scale must contain a salary column matching 'salary|wage|compensation|remuneration'",
         call. = FALSE)
  }

  # Optional: validate promotion_multiplier if present
  if (!is.null(policy_params$promotion_multiplier)) {
    validate_positive_number(policy_params$promotion_multiplier,
                             "promotion_multiplier", allow_zero = TRUE)
  }

  # Optional: validate transfer_multiplier if present
  if (!is.null(policy_params$transfer_multiplier)) {
    validate_positive_number(policy_params$transfer_multiplier,
                             "transfer_multiplier", allow_zero = TRUE)
  }

  # Optional: validate promotion_strategy if present
  if (!is.null(policy_params$promotion_strategy)) {
    validate_choice(policy_params$promotion_strategy,
                    c("tenure", "wage_based"),
                    "promotion_strategy")
  }

  # Optional: validate transfer_strategy if present
  if (!is.null(policy_params$transfer_strategy)) {
    validate_choice(policy_params$transfer_strategy,
                    c("random", "tenure", "reverse_tenure"),
                    "transfer_strategy")
  }

  # Validate group_cols exist in contract_dt
  validate_columns_exist(contract_dt, policy_params$group_cols, "contract_dt")

  # Required contract_dt columns
  required_contract_cols <- c(personnel_id_col, start_date_col,
                              end_date_col, contract_type_col)
  validate_columns_exist(contract_dt, required_contract_cols, "contract_dt")

  # Required personnel_dt columns
  required_personnel_cols <- c(personnel_id_col, status_col)
  validate_columns_exist(personnel_dt, required_personnel_cols, "personnel_dt")

  return(invisible(TRUE))
}
