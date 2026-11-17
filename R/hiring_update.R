#' State Update Functions for Hiring
#'
#' @description
#' Functions for updating contract_dt and personnel_dt after hiring/downsizing events.
#' Handles new personnel generation, contract creation, salary assignment, and
#' personnel removal.
#'
#' @import data.table
#' @name hiring_update
#' @keywords internal
NULL

#' Generate New Personnel Records
#'
#' @description
#' Creates new personnel records for new hires. Generates unique personnel IDs
#' and sets status to "active". Their age, tenure and gender are set afterwards,
#' from the policy's new hire profile.
#'
#' @param n Integer. Number of new personnel to generate
#' @param as_of_date Date. Reference date for ID generation
#' @param group_vals Named list. Values for grouping columns (e.g., list(department = "HR", grade = "G5"))
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param status_col Character. Status column (default: "employment_status")
#'
#' @return data.table with new personnel records
#' @keywords internal
generate_new_personnel <- function(n,
                                   as_of_date,
                                   group_vals = NULL,
                                   personnel_id_col = "personnel_id",
                                   status_col         = "employment_status") {

  if (n <= 0) {
    return(data.table::data.table())
  }

  # Generate unique personnel IDs — deterministic per (date, group, position)
  group_key <- if (!is.null(group_vals) && length(group_vals) > 0L) {
    paste(unlist(group_vals), collapse = "_")
  } else {
    NULL
  }
  new_ids <- generate_new_ids(n = n, as_of_date = as_of_date, prefix = "P", group_key = group_key)

  # Create base personnel records
  new_personnel <- data.table::data.table(
    personnel_id = new_ids,
    status = "active"
  )

  # Rename columns to match user's schema
  data.table::setnames(new_personnel,
                       old = c("personnel_id", "status"),
                       new = c(personnel_id_col, status_col))

  # Add grouping columns if specified
  if (!is.null(group_vals) && length(group_vals) > 0) {
    for (col_name in names(group_vals)) {
      new_personnel[, (col_name) := group_vals[[col_name]]]
    }
  }

  return(new_personnel)
}


#' Generate New Contract Records
#'
#' @description
#' Creates new contract records for new hires. Generates unique contract IDs
#' and sets start_date to as_of_date. The type is "permanent" until set from the
#' policy's new hire profile.
#'
#' @param personnel_ids Character vector. Personnel IDs for new hires
#' @param as_of_date Date. Contract start date
#' @param group_vals Named list. Values for grouping columns
#' @param column_spec_args Complete column specification from
#'   \code{define_column_spec()}.
#'
#' @return data.table with new contract records
#' @keywords internal
generate_new_contracts <- function(personnel_ids,
                                   as_of_date,
                                   group_vals = NULL,
                                   column_spec_args = define_column_spec()) {

  n <- length(personnel_ids)

  if (n <= 0) {
    return(data.table::data.table())
  }

  # Generate unique contract IDs — deterministic per (date, group, position)
  group_key <- if (!is.null(group_vals) && length(group_vals) > 0L) {
    paste(unlist(group_vals), collapse = "_")
  } else {
    NULL
  }
  contract_ids <- generate_new_ids(n = n, as_of_date = as_of_date, prefix = "C", group_key = group_key)

  # Create base contract records
  new_contracts <- data.table::data.table(
    contract_id = contract_ids,
    personnel_id = personnel_ids,
    start_date = as_of_date,
    end_date = as.Date(NA),
    contract_type = "permanent"
  )

  # Rename columns to match user's schema
  data.table::setnames(new_contracts,
                       old = c("contract_id", "personnel_id", "start_date",
                               "end_date", "contract_type"),
                       new = c(column_spec_args$contract_id_col,
                               column_spec_args$personnel_id_col,
                               column_spec_args$start_date_col,
                               column_spec_args$end_date_col,
                               column_spec_args$contract_type_col))

  # Add grouping columns if specified
  if (!is.null(group_vals) && length(group_vals) > 0) {
    for (col_name in names(group_vals)) {
      new_contracts[, (col_name) := group_vals[[col_name]]]
    }
  }

  return(new_contracts)
}


#' Assign Compensation to New Hires
#'
#' @description
#' Merges salary scale onto new hire contracts, so new hires are paid in
#' \code{salary_col}. Automatically detects join columns if not specified.
#' Issues warning (not error) if some contracts cannot be matched to salary scale.
#'
#' @param new_contracts_dt data.table. New hire contracts
#' @param salary_scale_dt data.table. Salary scale with join columns + salary column
#' @param join_cols Character vector. Columns to join on. If NULL (default), uses
#'   the columns the two tables share, other than \code{salary_col}. With none, a
#'   one-row scale gives everyone the same pay.
#' @param salary_col Character. The pay column, in both the scale and the
#'   contracts. Default \code{"gross_salary_lcu"}.
#'
#' @return Updated new_contracts_dt with salary column
#' @keywords internal
assign_compensation <- function(new_contracts_dt,
                                salary_scale_dt,
                                join_cols = NULL,
                                salary_col = "gross_salary_lcu") {

  if (nrow(new_contracts_dt) == 0) {
    return(new_contracts_dt)
  }

  .check_pay_column(salary_scale_dt, salary_col, "salary_scale")

  # Auto-detect join columns if not specified
  if (is.null(join_cols)) {
    # Use intersection of column names, excluding the salary column
    common_cols <- intersect(names(new_contracts_dt), names(salary_scale_dt))
    join_cols <- setdiff(common_cols, salary_col)

    if (length(join_cols) == 0) {
      # No join key — only valid if salary_scale_dt is a single row (flat/universal
      # salary). In that case broadcast the single value directly and return early.
      if (nrow(salary_scale_dt) == 1L) {
        new_contracts_dt[, (salary_col) := salary_scale_dt[[salary_col]]]
        return(new_contracts_dt)
      }
      stop("No common columns found for joining: salary_scale has several rows (",
           nrow(salary_scale_dt), "), so new hires cannot all get one pay. Give it one ",
           "row, or set group_cols to the columns that tell its rows apart.", call. = FALSE)
    }
  }

  # Validate join columns exist in both tables
  missing_in_contracts <- setdiff(join_cols, names(new_contracts_dt))
  missing_in_scale <- setdiff(join_cols, names(salary_scale_dt))

  if (length(missing_in_contracts) > 0) {
    stop("Join columns not found in new_contracts_dt: ",
         paste(missing_in_contracts, collapse = ", "), call. = FALSE)
  }

  if (length(missing_in_scale) > 0) {
    stop("Join columns not found in salary_scale_dt: ",
         paste(missing_in_scale, collapse = ", "), call. = FALSE)
  }

  # Check for duplicate keys in salary_scale_dt to avoid cartesian join
  if (anyDuplicated(salary_scale_dt, by = join_cols)) {
    dup_keys <- salary_scale_dt[duplicated(salary_scale_dt, by = join_cols),
                                join_cols, with = FALSE]
    stop("Duplicate keys found in salary_scale_dt. This would cause a cartesian join.\n",
         "Duplicated join key values:\n",
         paste(utils::capture.output(print(head(unique(dup_keys), 10))), collapse = "\n"),
         "\nPlease ensure salary_scale_dt has unique keys for: ",
         paste(join_cols, collapse = ", "), call. = FALSE)
  }

  # Store original row count
  n_original <- nrow(new_contracts_dt)

  # Perform join
  # Select only join_cols and salary_col from salary_scale_dt
  scale_subset <- unique(salary_scale_dt[, c(join_cols, salary_col), with = FALSE])

  # Join salary onto contracts
  new_contracts_dt <- scale_subset[new_contracts_dt, on = join_cols]

  # Check for unmatched rows and issue warning (not error)
  if (any(is.na(new_contracts_dt[[salary_col]]))) {
    n_unmatched <- sum(is.na(new_contracts_dt[[salary_col]]))
    unmatched <- new_contracts_dt[is.na(get(salary_col))]
    unmatched_vals <- unique(unmatched[, join_cols, with = FALSE])

    warning("WARNING: ", n_unmatched, " out of ", n_original,
            " contracts could not be matched to salary_scale_dt.\n",
            "These contracts will have NA salary values.\n",
            "Unmatched join key combinations (showing up to 10):\n",
            paste(utils::capture.output(print(head(unmatched_vals, 10))), collapse = "\n"),
            call. = FALSE, immediate. = TRUE)
  }

  return(new_contracts_dt)
}


#' Select Personnel to Remove for Downsizing
#'
#' @description
#' Identifies personnel to remove when net_change is negative. Supports
#' different removal strategies.
#'
#' @param contract_dt data.table. Current contract data
#' @param personnel_dt data.table. Current personnel data
#' @param n_remove Integer. Number of personnel to remove (positive number)
#' @param strategy Character. Removal strategy (default: "last_hired_first")
#'   Options: "last_hired_first", "random"
#' @param group_vals Named list. Values for grouping columns to filter on
#' @param as_of_date Date. Reference date
#' @param column_spec_args Complete column specification from
#'   \code{define_column_spec()}.
#'
#' @return Character vector of personnel_ids to remove
#' @keywords internal
select_personnel_to_remove <- function(contract_dt,
                                       personnel_dt,
                                       n_remove,
                                       strategy = "last_hired_first",
                                       group_vals = NULL,
                                       as_of_date,
                                       column_spec_args = define_column_spec()) {
  personnel_id_col <- column_spec_args$personnel_id_col
  start_date_col   <- column_spec_args$start_date_col

  if (n_remove <= 0) {
    return(character(0))
  }

  # Get active personnel
  active_contracts <- get_active_contracts(
    contract_dt = contract_dt,
    as_of_date = as_of_date,
    start_date_col = start_date_col,
    end_date_col = column_spec_args$end_date_col,
    contract_type_col = column_spec_args$contract_type_col
  )

  active_personnel <- active_contracts[
    personnel_dt[get(column_spec_args$status_col) == "active"],
    on = personnel_id_col,
    nomatch = NULL
  ]

  # Filter by group if specified
  if (!is.null(group_vals) && length(group_vals) > 0) {
    for (col_name in names(group_vals)) {
      active_personnel <- active_personnel[get(col_name) == group_vals[[col_name]]]
    }
  }

  if (nrow(active_personnel) == 0) {
    warning("No active personnel found to remove", call. = FALSE)
    return(character(0))
  }

  # Get primary contract per personnel for start_date
  primary_contracts <- get_primary_contract(
    contract_dt = active_personnel,
    personnel_id_col = personnel_id_col,
    contract_id_col = column_spec_args$contract_id_col,
    start_date_col = start_date_col,
    salary_col = column_spec_args$salary_col
  )

  # Apply removal strategy
  personnel_to_remove <- switch(
    strategy,

    "last_hired_first" = {
      # Order by start_date descending (most recent first)
      data.table::setorderv(primary_contracts, start_date_col, order = -1)
      head(primary_contracts[[personnel_id_col]], n_remove)
    },

    "random" = {
      # Random selection
      if (nrow(primary_contracts) <= n_remove) {
        primary_contracts[[personnel_id_col]]
      } else {
        sample(primary_contracts[[personnel_id_col]], size = n_remove)
      }
    },

    stop("Unknown removal strategy: ", strategy,
         ". Supported: 'last_hired_first', 'random'", call. = FALSE)
  )

  return(personnel_to_remove)
}


# Draws the profile of n new hires in a group, from the group's rows in the policy's
# profile, or the overall profile if the group has none. NULL if there is neither.
.draw_new_hire_profile <- function(n, group_vals, policy_params) {
  profile <- policy_params$new_hire_profile
  if (!is.null(profile) && length(group_vals) > 0L) {
    profile <- profile[data.table::as.data.table(group_vals), on = names(group_vals), nomatch = NULL]
  }
  if (is.null(profile) || nrow(profile) == 0L) profile <- policy_params$new_hire_profile_overall
  if (is.null(profile) || nrow(profile) == 0L) return(NULL)
  profile[sample.int(nrow(profile), n, replace = TRUE, prob = profile$share)]
}


# Gives new hires the state everyone else has: entry age and birth date, tenure 0,
# gender, hire date and contract type. Personal details go only in columns the
# personnel table has. Changes both tables in place.
.set_new_hire_state <- function(new_personnel, new_contracts, group_vals, policy_params, as_of_date,
                                personnel_dt, column_spec_args) {
  drawn <- .draw_new_hire_profile(nrow(new_personnel), group_vals, policy_params)
  contract_type_col <- column_spec_args$contract_type_col
  gender_col        <- column_spec_args$gender_col

  set_detail <- function(col, value) {
    if (!is.null(col) && col %in% names(personnel_dt)) {
      data.table::set(new_personnel, j = col, value = value)
    }
  }
  entry_age <- drawn$entry_age %||% NA_real_
  set_detail(column_spec_args$age_col, entry_age)
  set_detail(column_spec_args$birth_date_col, as_of_date - round(entry_age * 365.25))
  set_detail(column_spec_args$tenure_col, 0)
  set_detail(column_spec_args$hire_date_col, as_of_date)
  if (!is.null(gender_col) && gender_col %in% names(drawn)) set_detail(gender_col, drawn[[gender_col]])

  if (contract_type_col %in% names(drawn)) {
    data.table::set(new_contracts, j = contract_type_col,
                    value = data.table::fcoalesce(as.character(drawn[[contract_type_col]]), "permanent"))
  }
  invisible(NULL)
}


# Lets people go: their contracts on the payroll become "inactive" and end today,
# and their status becomes "inactive". A pension row they hold is left alone.
# Changes both tables in place, and returns the contracts as they were before,
# so the caller can report what was saved.
.let_people_go <- function(contract_dt, personnel_dt, ids, as_of_date, column_spec_args) {
  personnel_id_col  <- column_spec_args$personnel_id_col
  contract_type_col <- column_spec_args$contract_type_col

  on_payroll <- contract_dt[[personnel_id_col]] %in% ids &
    .on_payroll(contract_dt[[contract_type_col]])
  let_go <- contract_dt[on_payroll]
  contract_dt[on_payroll, c(column_spec_args$end_date_col, contract_type_col) := list(as_of_date, "inactive")]
  personnel_dt[get(personnel_id_col) %in% ids, (column_spec_args$status_col) := "inactive"]
  let_go
}


#' Update State with Hiring/Downsizing Adjustments
#'
#' @description
#' Main state update function. Handles both hiring (positive net_change) and
#' downsizing (negative net_change). Returns modified contract_dt and personnel_dt.
#' This is the ONLY function that modifies state in the hiring module.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param adjustment_dt data.table. Adjustment targets with group_cols + net_change column
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item salary_scale: data.table with salary information
#'     \item removal_strategy: Character for downsizing (default: "last_hired_first")
#'     \item group_cols: Character vector of grouping columns (can be NULL)
#'     \item new_hire_profile, new_hire_profile_overall: Optional. What new hires
#'       start with: see \code{\link{estimate_new_hire_profile}}.  Without a
#'       profile, hires are \code{"permanent"} with a blank age and gender.
#'   }
#' @param as_of_date Date. Reference date for adjustments
#' @param column_spec_args Complete column specification from
#'   \code{define_column_spec()}.
#'
#' @return List containing:
#'   \itemize{
#'     \item contract_dt: Updated contract data
#'     \item personnel_dt: Updated personnel data
#'     \item new_personnel_dt: New personnel added (for reporting)
#'     \item new_contracts_dt: New contracts added (for reporting)
#'     \item terminated_contracts_dt: Contracts terminated in downsizing (salary reflects cost saved)
#'   }
#' @section Data Integrity:
#'   Modifies \code{contract_dt} and \code{personnel_dt} **in place** via
#'   data.table reference semantics.  Pass \code{data.table::copy()} on
#'   each if the originals must be preserved.
#' @keywords internal
update_state_with_adjustment <- function(contract_dt,
                                         personnel_dt,
                                         adjustment_dt,
                                         policy_params,
                                         as_of_date,
                                         column_spec_args = define_column_spec()) {
  personnel_id_col  <- column_spec_args$personnel_id_col
  status_col        <- column_spec_args$status_col
  salary_col        <- column_spec_args$salary_col

  group_cols <- policy_params$group_cols
  salary_scale <- policy_params$salary_scale
  removal_strategy <- if (!is.null(policy_params$removal_strategy)) {
    policy_params$removal_strategy
  } else {
    "last_hired_first"
  }

  # Initialize collectors for new hires and terminations
  all_new_personnel <- list()
  all_new_contracts <- list()
  all_terminated_contracts <- list()

  # Handle each adjustment group
  if (!is.null(group_cols) && length(group_cols) > 0) {
    # Process each grouping cell
    for (i in seq_len(nrow(adjustment_dt))) {
      row <- adjustment_dt[i]
      net_change <- row$net_change

      # Extract group values
      group_vals <- as.list(row[, group_cols, with = FALSE])

      if (net_change > 0) {
        # HIRING
        # Generate new personnel
        new_personnel <- generate_new_personnel(
          n = net_change,
          as_of_date = as_of_date,
          group_vals = group_vals,
          personnel_id_col = personnel_id_col,
          status_col = status_col
        )

        # Generate new contracts
        new_contracts <- generate_new_contracts(
          personnel_ids = new_personnel[[personnel_id_col]],
          as_of_date = as_of_date,
          group_vals = group_vals,
          column_spec_args = column_spec_args
        )

        # Pay them from the pay table, for their group
        new_contracts <- assign_compensation(
          new_contracts_dt = new_contracts,
          salary_scale_dt = salary_scale,
          join_cols = group_cols,
          salary_col = salary_col
        )

        .set_new_hire_state(new_personnel, new_contracts, group_vals, policy_params, as_of_date,
                            personnel_dt, column_spec_args)

        # Append new records to data.tables
        personnel_dt <- data.table::rbindlist(
          list(personnel_dt, new_personnel),
          fill = TRUE,
          use.names = TRUE
        )

        contract_dt <- data.table::rbindlist(
          list(contract_dt, new_contracts),
          fill = TRUE,
          use.names = TRUE
        )

        # Collect for summary
        all_new_personnel[[i]] <- new_personnel
        all_new_contracts[[i]] <- new_contracts

      } else if (net_change < 0) {
        # DOWNSIZING
        n_remove <- abs(net_change)

        # Select personnel to remove
        personnel_to_remove <- select_personnel_to_remove(
          contract_dt = contract_dt,
          personnel_dt = personnel_dt,
          n_remove = n_remove,
          strategy = removal_strategy,
          group_vals = group_vals,
          as_of_date = as_of_date,
          column_spec_args = column_spec_args
        )

        all_terminated_contracts[[i]] <- .let_people_go(
          contract_dt, personnel_dt, personnel_to_remove, as_of_date, column_spec_args
        )
      }
      # net_change == 0: no action
    }
  } else {
    # Overall adjustment (no grouping)
    if (nrow(adjustment_dt) != 1) {
      stop("For overall adjustment (no group_cols), adjustment_dt must have exactly 1 row",
           call. = FALSE)
    }

    net_change <- adjustment_dt$net_change[1]

    if (net_change > 0) {
      # HIRING
      new_personnel <- generate_new_personnel(
        n = net_change,
        as_of_date = as_of_date,
        group_vals = NULL,
        personnel_id_col = personnel_id_col,
        status_col = status_col
      )

      new_contracts <- generate_new_contracts(
        personnel_ids = new_personnel[[personnel_id_col]],
        as_of_date = as_of_date,
        group_vals = NULL,
        column_spec_args = column_spec_args
      )

      # No groups, so the pay table needs one row (or columns it shares with
      # the new contracts)
      if (!is.null(salary_scale) && nrow(salary_scale) > 0) {
        new_contracts <- assign_compensation(
          new_contracts_dt = new_contracts,
          salary_scale_dt = salary_scale,
          salary_col = salary_col
        )
      }

      .set_new_hire_state(new_personnel, new_contracts, NULL, policy_params, as_of_date,
                          personnel_dt, column_spec_args)

      # Append to data
      personnel_dt <- data.table::rbindlist(
        list(personnel_dt, new_personnel),
        fill = TRUE,
        use.names = TRUE
      )

      contract_dt <- data.table::rbindlist(
        list(contract_dt, new_contracts),
        fill = TRUE,
        use.names = TRUE
      )

      all_new_personnel[[1]] <- new_personnel
      all_new_contracts[[1]] <- new_contracts

    } else if (net_change < 0) {
      # DOWNSIZING
      n_remove <- abs(net_change)

      personnel_to_remove <- select_personnel_to_remove(
        contract_dt = contract_dt,
        personnel_dt = personnel_dt,
        n_remove = n_remove,
        strategy = removal_strategy,
        group_vals = NULL,
        as_of_date = as_of_date,
        column_spec_args = column_spec_args
      )

      all_terminated_contracts[[1]] <- .let_people_go(
        contract_dt, personnel_dt, personnel_to_remove, as_of_date, column_spec_args
      )
    }
  }

  # Combine all new personnel and contracts for summary
  new_personnel_all <- if (length(all_new_personnel) > 0) {
    data.table::rbindlist(all_new_personnel, fill = TRUE, use.names = TRUE)
  } else {
    data.table::data.table()
  }

  new_contracts_all <- if (length(all_new_contracts) > 0) {
    data.table::rbindlist(all_new_contracts, fill = TRUE, use.names = TRUE)
  } else {
    data.table::data.table()
  }

  terminated_contracts_all <- if (length(all_terminated_contracts) > 0) {
    data.table::rbindlist(all_terminated_contracts, fill = TRUE, use.names = TRUE)
  } else {
    data.table::data.table()
  }

  return(list(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    new_personnel_dt = new_personnel_all,
    new_contracts_dt = new_contracts_all,
    terminated_contracts_dt = terminated_contracts_all
  ))
}
