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


#' Check the Inputs for Selecting Retirees
#'
#' @description
#' Called first by [select_retirees()]. Stops, with a message that says what to
#' fix, when the data or the "who retires" part of a retirement policy cannot
#' work. The pension part is checked separately, by
#' `check_retirement_pension_inputs()`.
#'
#' It checks, in order:
#' 1. The data: both tables are non-empty data.tables with the columns
#'    retirement reads, and `as_of_date` is a date.
#' 2. The policy's `type` (`"status_quo"`, `"eligibility"` or `"mandatory"`)
#'    and its `active_types`.
#' 3. The rates, when the type uses them: keyed by age or tenure, with a rate
#'    table (an empty one is fine: the history showed no retirements).
#' 4. The rule, when the type uses it: its kind, the thresholds that kind
#'    needs, and a custom rule's safety.
#' 5. The columns the policy looks people up by: every group column on one of
#'    the tables, and the age and tenure columns on `personnel_dt` when the
#'    policy uses age or tenure (they are read, not computed).
#'
#' @inheritParams select_retirees
#'
#' @return `TRUE`, invisibly, when everything is usable.
#'
#' @keywords internal
check_retirement_selection_inputs <- function(contract_dt,
                                              personnel_dt,
                                              policy_params,
                                              as_of_date,
                                              column_spec_args = define_column_spec()) {
  cols <- column_spec_args

  # 1. The data and the date
  .check_column_spec(cols)
  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")
  validate_date_format(as_of_date, "as_of_date")
  .check_spec_columns(contract_dt, personnel_dt, cols, required = list(
    contract_dt  = c("contract_id_col", "personnel_id_col", "start_date_col",
                     "end_date_col", "contract_type_col"),
    personnel_dt = c("personnel_id_col", "status_col")
  ))

  # 2. The kind of policy, and the contract types it covers
  type <- if (is.list(policy_params)) policy_params$type
  if (is.null(type)) {
    stop("The retirement policy has no 'type'. Build it with ",
         "design_retirement_policy_status_quo(), design_retirement_policy_eligibility() ",
         "or design_retirement_policy_mandatory().", call. = FALSE)
  }
  validate_choice(type, c("status_quo", "eligibility", "mandatory"), "retirement policy type")
  active_types <- policy_params$active_types
  if (!is.null(active_types) &&
      (!is.character(active_types) || length(active_types) == 0L || anyNA(active_types))) {
    stop("active_types must be contract type values, e.g. \"permanent\", or NULL.", call. = FALSE)
  }

  # Only the parts this type uses are checked; a part it does not use is ignored
  rates <- if (type %in% c("status_quo", "mandatory"))  policy_params$rates
  rule  <- if (type %in% c("eligibility", "mandatory")) policy_params$rule
  kind  <- rule$defaults$kind %||% "none"

  # 3. The rates
  if (type != "eligibility") {
    if (!any(c("age_yr_int", "tenure_yr_int") %in% rates$group_cols)) {
      stop("The retirement rates must vary by age or tenure: build them with ",
           "retirement_rates_status_quo().", call. = FALSE)
    }
    if (is.null(rates$policy_table)) {
      stop("The retirement policy has no rates. Estimate them with ",
           "estimate_retirement_hazard_rates(), or run through simulate_horizon(), ",
           "which estimates them from your panel data.", call. = FALSE)
    }
    missing_cols <- setdiff(c(rates$group_cols, "hazard_rate"), names(rates$policy_table))
    if (!data.table::is.data.table(rates$policy_table) || length(missing_cols) > 0L) {
      stop("The retirement rate table must be a data.table with columns ",
           paste(c(rates$group_cols, "hazard_rate"), collapse = ", "), ".", call. = FALSE)
    }
  }

  # 4. The rule
  rule_vars <- character(0)
  if (type != "status_quo") {
    validate_policy_table(rule)
    validate_choice(kind, c("age", "tenure", "age_tenure", "custom"), "retirement rule kind")
    needs <- c(if (kind %in% c("age", "age_tenure"))    "min_age",
               if (kind %in% c("tenure", "age_tenure")) "min_tenure")
    for (p in needs) {
      if (is.null(rule$defaults[[p]]) && !p %in% names(rule$policy_table)) {
        stop("The '", kind, "' retirement rule needs ", p, ".", call. = FALSE)
      }
    }
    # A custom rule: parsed and checked against the allow-list of safe calls
    if (kind == "custom") {
      rule_vars <- all.vars(validate_eligibility_rule(rule$defaults$custom_rule))
    }
  }

  # 5. The columns the policy looks people up by. Group columns (and a
  #    retire_strategy column) can be on either table. Age and tenure are read
  #    from personnel_dt, so they must be there when the policy uses them.
  strategy   <- rates$defaults$retire_strategy %||% "random"
  group_cols <- setdiff(c(rates$group_cols, rule$group_cols, strategy),
                        c("age_yr_int", "tenure_yr_int", "random"))
  not_found  <- setdiff(group_cols, c(names(personnel_dt), names(contract_dt)))
  if (length(not_found) > 0L) {
    stop("The retirement policy uses column(s) that neither personnel_dt nor ",
         "contract_dt has: ", paste(not_found, collapse = ", "), ".", call. = FALSE)
  }

  uses_age    <- "age_yr_int" %in% rates$group_cols || kind %in% c("age", "age_tenure") ||
                 "age" %in% rule_vars
  uses_tenure <- "tenure_yr_int" %in% rates$group_cols || kind %in% c("tenure", "age_tenure") ||
                 "tenure_years" %in% rule_vars
  for (need in c(if (uses_age) "age_col", if (uses_tenure) "tenure_col")) {
    if (!isTRUE(cols[[need]] %in% names(personnel_dt))) {
      stop("The retirement policy uses ", sub("_col$", "", need), ", but personnel_dt has ",
           "no '", cols[[need]], "' column. Add one, or run through simulate_horizon(), ",
           "which works it out.", call. = FALSE)
    }
  }

  invisible(TRUE)
}


#' Check the Pension Part of a Retirement Policy
#'
#' @description
#' Called first by [simulate_retirement()], before anyone is picked to retire,
#' so a pension that cannot be paid stops the run straight away. Who retires is
#' checked separately, by [check_retirement_selection_inputs()].
#'
#' It checks that the policy has a pension; that the pension has the
#' three-slot shape (`group_cols`, `policy_table`, `defaults`); that every
#' `pension_type` it uses, in `defaults` or in the table, is one of `"db"`,
#' `"dc"`, `"flat"`, `"hybrid"`, `"rate"` or `"custom"`; and that every custom
#' `pension_formula` uses only allowed arithmetic.
#'
#' @param policy_params List. The retirement policy.
#'
#' @return `TRUE`, invisibly, when the pension is usable.
#'
#' @keywords internal
check_retirement_pension_inputs <- function(policy_params) {
  pension <- if (is.list(policy_params)) policy_params$pension
  if (is.null(pension)) {
    stop("The retirement policy has no pension. Give one from a pension_*() function, ",
         "e.g. pension_db(0.02), to design_retirement_policy_*().", call. = FALSE)
  }
  validate_policy_table(pension)

  # Every pension type in use: the default and any set by group in the table
  validate_choice(pension$defaults$pension_type,
                  c("db", "dc", "flat", "hybrid", "rate", "custom"), "pension_type")
  bad <- setdiff(pension$policy_table$pension_type,
                 c("db", "dc", "flat", "hybrid", "rate", "custom"))
  if (length(bad) > 0L) {
    stop("Unknown pension_type in the pension table: ", paste(bad, collapse = ", "), ".",
         call. = FALSE)
  }

  # Every custom formula, checked against the allow-list of safe arithmetic
  formulas <- unique(c(pension$defaults$pension_formula, pension$policy_table$pension_formula))
  for (f in formulas[!is.na(formulas)]) validate_pension_formula(f)

  invisible(TRUE)
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
#' @param as_of_date Date. Reference date
#' @param column_spec_args Complete column specification from
#'   \code{define_column_spec()}.
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
check_hiring_inputs <- function(contract_dt,
                                personnel_dt,
                                policy_params,
                                as_of_date,
                                column_spec_args = define_column_spec()) {
  salary_col <- column_spec_args$salary_col

  # Validate data tables
  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")

  # Validate as_of_date
  validate_date_format(as_of_date, "as_of_date")

  # Validate policy_params is a list
  if (!is.list(policy_params)) {
    stop("policy_params must be a list", call. = FALSE)
  }

  # Validate mode
  if (is.null(policy_params$mode)) {
    stop("policy_params must contain 'mode'", call. = FALSE)
  }

  valid_modes <- c("flow", "stock", "status_quo")
  validate_choice(policy_params$mode, valid_modes, "mode")

  # Validate mode-specific parameters
  if (policy_params$mode == "flow") {
    if (is.null(policy_params$replacement_rate)) {
      stop("replacement_rate is required for mode 'flow'", call. = FALSE)
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

  if (policy_params$mode == "stock") {
    if (is.null(policy_params$stock_targets)) {
      stop("stock_targets is required for mode 'stock'", call. = FALSE)
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

  if (!is.null(policy_params$salary_scale)) {
    .check_pay_column(policy_params$salary_scale, salary_col, "salary_scale")
  }

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
#' @param as_of_date Date or character. Reference date
#' @param column_spec_args Complete column specification from
#'   \code{define_column_spec()}.
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
check_movement_inputs <- function(
  contract_dt,
  personnel_dt,
  salary_scale_dt,
  policy_params,
  as_of_date,
  column_spec_args = define_column_spec()
) {
  # Validate data tables
  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")

  # Validate as_of_date
  validate_date_format(as_of_date, "as_of_date")

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

  .check_pay_column(salary_scale_dt, column_spec_args$salary_col, "salary_scale_dt")

  return(invisible(TRUE))
}


#' Validate Inputs for Allowance Simulation
#'
#' @description
#' Composite validator for \code{\link{simulate_allowances}}, following the
#' same pattern as \code{\link{check_retirement_selection_inputs}} /
#' \code{\link{check_hiring_inputs}} / \code{\link{check_movement_inputs}}.
#' Not called when \code{policy_params} is \code{NULL} -- that short-circuits
#' to a no-op before validation.
#'
#' @param contract_dt data.table. Current-period contract snapshot.
#' @param personnel_dt data.table. Current-period personnel snapshot.
#' @param policy_params List. See \code{\link{resolve_allowance_amounts}} for
#'   the expected shape.
#' @param as_of_date Date. The reference date for this period -- matches
#'   \code{check_retirement_selection_inputs()}'s own parameter name.
#' @param column_spec_args Complete column specification from
#'   \code{define_column_spec()}.
#'
#' @return Invisible \code{TRUE} if valid; stops with an informative error
#'   otherwise.
#' @keywords internal
check_allowance_inputs <- function(contract_dt,
                                   personnel_dt,
                                   policy_params,
                                   as_of_date,
                                   column_spec_args = define_column_spec()) {

  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")
  validate_date_format(as_of_date, "as_of_date")
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


## column-name specification and output options (ADR 0012, 0013, 0014)

# The columns that may be set to NULL in define_column_spec().
.COLUMN_SPEC_OPTIONAL <- c("age_col", "tenure_col", "birth_date_col", "hire_date_col", "gender_col")

#' Say Which Column Holds What in Your Data
#'
#' Which column in your data holds each thing the simulation reads? Build the
#' answer here and pass it as `column_spec_args` to any **govhrcast** function.
#'
#' Data harmonized with **govhr** already use the default names, so
#' `define_column_spec()` with no arguments is all it needs, and it is the
#' default everywhere. If your data differ, name only the columns that differ.
#' Your names are used all the way through: nothing is renamed, so the tables
#' that come back keep them.
#'
#' @param personnel_id_col String. The person ID, in both the contract and the
#'   personnel table. Default `"personnel_id"`.
#' @param contract_id_col String. The contract ID, in the contract table.
#'   Default `"contract_id"`.
#' @param ref_date_col String. The snapshot date. It also stamps each year in
#'   the returned microdata. Default `"ref_date"`.
#' @param start_date_col,end_date_col Strings. When a contract starts and ends.
#'   Defaults `"start_date"` and `"end_date"`.
#' @param salary_col String. The salary the wage bill is measured in. A pay table
#'   such as `salary_scale_dt` must have a column of the same name. Default
#'   `"gross_salary_lcu"`.
#' @param contract_type_col String. The contract type, which says who is on the
#'   payroll and who holds a pension. Default `"contract_type"`.
#' @param status_col String. The person's employment status, in the personnel
#'   table. Default `"employment_status"`.
#' @param age_col String or `NULL`. Age in years, in the personnel table.
#'   `simulate_horizon()` calculates it from `birth_date_col` when the column is
#'   missing or empty, then adds 1 each year. Default `"age"`. See Details for
#'   what `NULL` does.
#' @param tenure_col String or `NULL`. Years of service in the public sector, in
#'   the personnel table. `simulate_horizon()` calculates it from contract history
#'   when the column is missing or empty, then adds 1 each year. Custom retirement
#'   rules and pension formulas can call it `tenure_years`, whatever this column
#'   is called. Default `"personnel_tenure"`. See Details for what `NULL` does.
#' @param birth_date_col String or `NULL`. Date of birth, used to calculate age.
#'   Use `NULL` if you have none, and then `age_col` must already be filled in.
#'   Default `"birth_date"`.
#' @param hire_date_col String or `NULL`. Each person's original hire date, in the
#'   personnel table (for example `"first_employment_date"`). When given,
#'   status-quo hiring rates are estimated from it. With the default `NULL`, they
#'   are estimated from when people first appear in the panel.
#' @param gender_col String or `NULL`. Used to give new hires a gender. If your
#'   data have no such column, new hires have none. Default `"gender"`.
#'
#' @return A named list with one element per argument, in the order above. Pass
#'   it as `column_spec_args`. The same list works for every function: columns a
#'   function does not use are ignored.
#'
#' @details
#' `NULL` for `age_col` or `tenure_col` means the default name, `"age"` or
#' `"personnel_tenure"`, everywhere: `simulate_horizon()` fills that column in
#' when it is missing or empty, and the retirement functions read it.
#' `simulate_scenario()` called on its own with `NULL` leaves the value as it is
#' and does not add a year.
#'
#' This function checks the names only, not your data: it cannot tell whether a
#' column exists. Each function checks the columns it reads when it runs.
#'
#' It stops with an error when an argument is misspelled, when a name is not one
#' non-empty string, when `NULL` is given for a column that cannot be left out,
#' or when two arguments name the same column. Other functions stop with an error
#' if `column_spec_args` did not come from here, for example a plain `list()`.
#'
#' @seealso [simulate_horizon()], which takes this as `column_spec_args`.
#'
#' @examples
#' # Harmonized data: nothing to name
#' define_column_spec()$salary_col
#'
#' # Name only what differs; everything else keeps its default
#' spec <- define_column_spec(salary_col = "base_salary_lcu")
#' spec$salary_col
#' spec$personnel_id_col
#'
#' # A table whose person ID has another name. The template has one row for each
#' # pair of establishments that people moved between.
#' contract <- data.table::copy(bra_hrmis_contract)
#' data.table::setnames(contract, "personnel_id", "pid")
#' template <- movement_policy_template(
#'   contract,
#'   group_cols = "est_id",
#'   column_spec_args = define_column_spec(personnel_id_col = "pid")
#' )
#' names(template)
#'
#' @export
define_column_spec <- function(personnel_id_col  = "personnel_id",
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
                               hire_date_col     = NULL,
                               gender_col        = "gender") {
  spec <- mget(names(formals()))   # all 13 arguments, as a named list
  .check_column_spec(spec)
  spec
}

#' Check a Column Specification
#'
#' Run by [define_column_spec()] and again by each exported function, so a plain
#' list or a spec edited by hand cannot reach the modules.
#'
#' @param spec The object passed as `column_spec_args`.
#' @return `invisible(TRUE)`; stops if `spec` does not have exactly
#'   [define_column_spec()]'s arguments, if a name is not one non-empty string
#'   (`NULL` is allowed for `.COLUMN_SPEC_OPTIONAL`), or if two arguments name the
#'   same column.
#' @noRd
.check_column_spec <- function(spec) {
  if (!identical(names(spec), names(formals(define_column_spec)))) {
    stop("column_spec_args must come from define_column_spec(), e.g. ",
         "define_column_spec(salary_col = \"base_salary_lcu\").", call. = FALSE)
  }

  for (key in names(spec)) {
    value <- spec[[key]]
    if (is.null(value) && key %in% .COLUMN_SPEC_OPTIONAL) next
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop(key, " must be a single column name (a string)", call. = FALSE)
    }
  }

  named <- unlist(spec)
  dup <- named[duplicated(named) | duplicated(named, fromLast = TRUE)]
  if (length(dup) > 0L) {
    col <- dup[[1L]]
    stop(
      paste(names(dup)[dup == col], collapse = " and "),
      " both name column '", col, "'. Each argument must name a different column.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Say How to Label a Run and What It Returns
#'
#' How should this run be labelled, and should it return its person-level data?
#' Build the answer here and pass it as `output_control_args` to
#' [simulate_horizon()].
#'
#' With no arguments the run has no label and returns only its summary tables, and
#' that is the default in [simulate_horizon()]. Name only what you want to change.
#'
#' @param scenario_name String or `NULL`. A label for this run, for example
#'   `"Status quo"`. When given, `$comparison` gets `scenario_id` and
#'   `scenario_label` columns, so several runs can be stacked and told apart.
#'   Default `NULL`.
#' @param is_baseline `TRUE` or `FALSE`. Whether this run is the one the others are
#'   compared with. `$comparison` always has an `is_baseline` column holding this
#'   value. It says nothing about dates: where the projection starts is
#'   `baseline_date` in [simulate_horizon()]. Default `FALSE`.
#' @param return_microdata `TRUE` or `FALSE`. `TRUE` also returns `$contract_dt`
#'   and `$personnel_dt`, with every projected year stacked, and `$allowance_dt`
#'   when allowances are modelled. These can be large, and they are person-level
#'   data, so handle them like the input microdata before saving or sharing a
#'   result. Default `FALSE`.
#'
#' @return A named list with one element per argument. Pass it as
#'   `output_control_args`.
#'
#' @details
#' It stops with an error when an argument is misspelled, when `scenario_name` is
#' not one string or `NULL`, or when a switch is not a single `TRUE` or `FALSE`.
#' [simulate_horizon()] stops with an error if `output_control_args` did not come
#' from here, for example a plain `list()`.
#'
#' @seealso [simulate_horizon()], which takes this as `output_control_args`;
#'   [define_column_spec()], which says which columns hold what.
#'
#' @examples
#' # The default: no label and no person-level data
#' define_output_control()$return_microdata
#'
#' # Name only what you want to change
#' reform <- define_output_control(scenario_name = "Retire at 65")
#' reform$scenario_name
#' reform$is_baseline
#'
#' @export
define_output_control <- function(scenario_name    = NULL,
                                  is_baseline      = FALSE,
                                  return_microdata = FALSE) {
  output_control <- mget(names(formals()))   # all 3 arguments, as a named list
  .check_output_control(output_control)
  output_control
}

#' Check the Output Options
#'
#' Run by [define_output_control()] and again by [simulate_horizon()], so a plain
#' list or options edited by hand cannot reach the run.
#'
#' @param output_control The object passed as `output_control_args`.
#' @return `invisible(TRUE)`; stops if it does not have exactly
#'   [define_output_control()]'s arguments, if `scenario_name` is not one string or
#'   `NULL`, or if a switch is not a single `TRUE` or `FALSE`.
#' @noRd
.check_output_control <- function(output_control) {
  if (!identical(names(output_control), names(formals(define_output_control)))) {
    stop("output_control_args must come from define_output_control(), e.g. ",
         "define_output_control(return_microdata = TRUE).", call. = FALSE)
  }

  # scenario_name labels the run; the switches are single TRUE/FALSE values
  scenario_name <- output_control$scenario_name
  if (!is.null(scenario_name) &&
      (!is.character(scenario_name) || length(scenario_name) != 1L || is.na(scenario_name))) {
    stop("scenario_name must be a single character string or NULL.", call. = FALSE)
  }
  for (key in c("is_baseline", "return_microdata")) {
    value <- output_control[[key]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(key, " must be TRUE or FALSE.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Check That the Specified Columns Exist in the Data
#'
#' Each exported function checks only the keys it reads, so one spec can be
#' reused across calls; keys set to \code{NULL} are skipped.
#'
#' @param contract_dt,personnel_dt The caller's tables.
#' @param spec Complete specification from \code{define_column_spec()}.
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

#' Check That a Pay Table Holds the Pay Column
#'
#' A pay table gives the pay for each group in a column with the same name as
#' \code{salary_col}. There is one salary column, for the table and the data.
#'
#' @param pay_table data.table.  The pay table.
#' @param salary_col Character.  The pay column.
#' @param table_name Character.  The table's name, for the error message.
#' @return \code{invisible(TRUE)}; stops if the column is missing or not numeric.
#' @noRd
.check_pay_column <- function(pay_table, salary_col, table_name) {
  if (!salary_col %in% names(pay_table)) {
    stop(table_name, " must have a column named '", salary_col,
         "', the same as salary_col, holding the pay for each group. ",
         "It has: ", paste(names(pay_table), collapse = ", "), ".", call. = FALSE)
  }
  if (!is.numeric(pay_table[[salary_col]])) {
    stop(table_name, ": column '", salary_col, "' must be numeric.", call. = FALSE)
  }
  invisible(TRUE)
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
