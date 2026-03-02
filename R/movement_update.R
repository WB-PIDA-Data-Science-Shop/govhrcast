#' State Update Functions for Promotions and Transfers
#'
#' @description
#' Functions for selecting individuals to promote/transfer using strategy-based
#' ranking engines, and for updating contract_dt and personnel_dt with new
#' group assignments and salaries. Only \code{update_state_with_movement}
#' modifies state.
#'
#' @import data.table
#' @name movement_update
#' @keywords internal
NULL


#' Stochastic Round a Single Number
#'
#' @description
#' Rounds a fractional number stochastically: always at least floor(x),
#' with probability (x - floor(x)) of rounding up by one.
#' Exported internally so compute_movement_demand can use it too.
#'
#' @param x Numeric scalar.
#' @return Integer scalar.
#' @keywords internal
stochastic_round <- function(x) {
  floor_x <- floor(x)
  as.integer(floor_x + stats::rbinom(1L, 1L, x - floor_x))
}


#' Identify Movers from Current Workforce
#'
#' @description
#' Selects individual personnel to promote or transfer based on the
#' computed movement demand table. For each \code{from_group -> to_group}
#' pair, selects exactly \code{n_movers} individuals using the specified
#' strategy (tenure, wage_based, random, reverse_tenure).
#'
#' Requires pre-computed tenure / time-in-grade columns to be available in
#' the merged contract+personnel view, or will compute on-the-fly from
#' the provided contract_dt.
#'
#' @param contract_dt data.table. Current (single-snapshot) contract data
#' @param personnel_dt data.table. Current personnel data
#' @param demand_dt data.table. Output of \code{compute_movement_demand()}.
#'   Must have columns: from_group, to_group, movement_type, n_movers
#' @param policy_params List. Must contain:
#'   \describe{
#'     \item{group_cols}{Character vector defining state columns}
#'     \item{promotion_strategy}{Character. "tenure" or "wage_based"}
#'     \item{transfer_strategy}{Character. "random", "tenure", or "reverse_tenure"}
#'   }
#' @param ref_date Date or character. Reference date
#' @param baseline_matrix data.table. Full baseline matrix (used to compute
#'   max_salary per grade for wage_based strategy). Can be NULL.
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param start_date_col Character. Contract start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#' @param salary_col Character. Salary column (default: "gross_salary_lcu")
#' @param status_col Character. Status column (default: "status")
#' @param ref_date_col Character. Reference date column for panel data (default: "ref_date")
#'
#' @return data.table with columns:
#'   \describe{
#'     \item{personnel_id}{Character. Selected mover ID}
#'     \item{from_group}{Character. Origin state key}
#'     \item{to_group}{Character. Destination state key}
#'     \item{movement_type}{Character. "promotion" or "transfer"}
#'   }
#' @keywords internal
identify_movers <- function(contract_dt,
                             personnel_dt,
                             demand_dt,
                             policy_params,
                             ref_date,
                             baseline_matrix = NULL,
                             personnel_id_col = "personnel_id",
                             start_date_col = "start_date",
                             end_date_col = "end_date",
                             contract_type_col = "contract_type_code",
                             salary_col = "gross_salary_lcu",
                             status_col = "status",
                             ref_date_col = "ref_date") {

  ref_date <- validate_date_format(ref_date, "ref_date")

  if (is.null(demand_dt) || nrow(demand_dt) == 0) {
    return(data.table::data.table(
      personnel_id  = character(0),
      from_group    = character(0),
      to_group      = character(0),
      movement_type = character(0)
    ))
  }

  group_cols         <- policy_params$group_cols
  promotion_strategy <- if (!is.null(policy_params$promotion_strategy))
                          policy_params$promotion_strategy else "tenure"
  transfer_strategy  <- if (!is.null(policy_params$transfer_strategy))
                          policy_params$transfer_strategy else "random"

  # ------------------------------------------------------------------
  # Build active workforce view with all needed ranking columns
  # ------------------------------------------------------------------
  active_contracts <- get_active_contracts(
    contract_dt = contract_dt,
    ref_date = ref_date,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col
  )

  # One row per active person (primary contract)
  workforce <- active_contracts[
    personnel_dt[get(status_col) == "active"],
    on = personnel_id_col,
    nomatch = NULL
  ]

  workforce <- get_primary_contract(
    contract_dt = workforce,
    personnel_id_col = personnel_id_col,
    start_date_col = start_date_col,
    salary_col = if (salary_col %in% names(workforce)) salary_col else "gross_salary_lcu"
  )

  # Add group key
  workforce[, .from_group := do.call(paste, c(.SD, sep = "||")), .SDcols = group_cols]

  # ------------------------------------------------------------------
  # Compute tenure (total) for tenure-based strategies
  # ------------------------------------------------------------------
  needs_tenure <- (promotion_strategy == "tenure") ||
                  (transfer_strategy %in% c("tenure", "reverse_tenure"))

  if (needs_tenure) {
    tenure_dt <- compute_tenure(
      contract_dt = contract_dt,
      ref_date = ref_date,
      personnel_id_col = personnel_id_col,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      contract_type_col = contract_type_col
    )
    data.table::setnames(tenure_dt, "personnel_id", ".pid_tenure")
    workforce <- tenure_dt[workforce, on = c(.pid_tenure = personnel_id_col)]
    data.table::setnames(workforce, ".pid_tenure", personnel_id_col)
  }

  # ------------------------------------------------------------------
  # Compute time-in-grade for tenure-based PROMOTION strategy
  # ------------------------------------------------------------------
  needs_tig <- (promotion_strategy == "tenure")
  if (needs_tig && ref_date_col %in% names(contract_dt)) {
    tig_dt <- compute_time_in_grade(
      contract_dt = contract_dt,
      ref_date = ref_date,
      group_cols = group_cols,
      personnel_id_col = personnel_id_col,
      ref_date_col = ref_date_col,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      contract_type_col = contract_type_col
    )
    data.table::setnames(tig_dt, "personnel_id", ".pid_tig")
    workforce <- tig_dt[workforce, on = c(.pid_tig = personnel_id_col)]
    data.table::setnames(workforce, ".pid_tig", personnel_id_col)
  } else if (needs_tig) {
    # Fall back to start_date if no panel data
    workforce[, time_in_grade := as.numeric(
      difftime(ref_date, get(start_date_col), units = "days")
    ) / 365.25]
  }

  # ------------------------------------------------------------------
  # Compute max_salary per from_group for wage_based promotion
  # ------------------------------------------------------------------
  if (promotion_strategy == "wage_based" && salary_col %in% names(workforce)) {
    max_salary_dt <- workforce[, .(max_salary = max(get(salary_col), na.rm = TRUE)),
                               by = .(.from_group)]
    workforce <- max_salary_dt[workforce, on = ".from_group"]
    workforce[, salary_ratio := get(salary_col) / max_salary]
    workforce[is.na(salary_ratio) | !is.finite(salary_ratio), salary_ratio := 0]
  }

  # ------------------------------------------------------------------
  # Select movers for each from_group -> to_group transition
  # ------------------------------------------------------------------
  # Guard: a person can only be selected ONCE across all transitions
  already_selected <- character(0)

  mover_list <- vector("list", nrow(demand_dt))

  for (i in seq_len(nrow(demand_dt))) {
    row          <- demand_dt[i]
    fg           <- row$from_group
    tg           <- row$to_group
    mtype        <- row$movement_type
    n_select     <- row$n_movers

    # Eligible pool: in the from_group and not yet selected
    pool <- workforce[.from_group == fg & !get(personnel_id_col) %in% already_selected]

    if (nrow(pool) == 0 || n_select <= 0L) {
      next
    }

    # Cap to available pool
    n_select <- min(n_select, nrow(pool))

    # Select using strategy
    selected_ids <- if (mtype == "promotion") {
      switch(
        promotion_strategy,

        "tenure" = {
          # Rank by time_in_grade descending
          if ("time_in_grade" %in% names(pool)) {
            data.table::setorderv(pool, "time_in_grade", order = -1L)
          } else {
            data.table::setorderv(pool, start_date_col, order = 1L)
          }
          head(pool[[personnel_id_col]], n_select)
        },

        "wage_based" = {
          # Rank by salary_ratio ascending (lowest paid relative to max gets promoted)
          if ("salary_ratio" %in% names(pool)) {
            data.table::setorderv(pool, "salary_ratio", order = 1L)
          }
          head(pool[[personnel_id_col]], n_select)
        },

        # Default: tenure
        {
          if ("time_in_grade" %in% names(pool)) {
            data.table::setorderv(pool, "time_in_grade", order = -1L)
          }
          head(pool[[personnel_id_col]], n_select)
        }
      )
    } else {
      # Transfer strategies
      switch(
        transfer_strategy,

        "random" = {
          sample(pool[[personnel_id_col]], size = n_select, replace = FALSE)
        },

        "tenure" = {
          if ("tenure_years" %in% names(pool)) {
            data.table::setorderv(pool, "tenure_years", order = -1L)
          }
          head(pool[[personnel_id_col]], n_select)
        },

        "reverse_tenure" = {
          if ("tenure_years" %in% names(pool)) {
            data.table::setorderv(pool, "tenure_years", order = 1L)
          }
          head(pool[[personnel_id_col]], n_select)
        },

        # Default: random
        sample(pool[[personnel_id_col]], size = n_select, replace = FALSE)
      )
    }

    if (length(selected_ids) == 0) next

    mover_list[[i]] <- data.table::data.table(
      personnel_id  = selected_ids,
      from_group    = fg,
      to_group      = tg,
      movement_type = mtype
    )

    already_selected <- c(already_selected, selected_ids)
  }

  movers_dt <- data.table::rbindlist(
    mover_list[!vapply(mover_list, is.null, logical(1L))],
    fill = TRUE, use.names = TRUE
  )

  if (nrow(movers_dt) == 0) {
    movers_dt <- data.table::data.table(
      personnel_id  = character(0),
      from_group    = character(0),
      to_group      = character(0),
      movement_type = character(0)
    )
  }

  return(movers_dt)
}


#' Update State with Movement (Promotions and Transfers)
#'
#' @description
#' The ONLY function that modifies state in the promotions/transfers module.
#' Updates \code{contract_dt} to reflect new group assignments (org_id and/or
#' paygrade_id) and re-assigns compensation from the salary scale for all
#' selected movers. Returns modified copies.
#'
#' @param contract_dt data.table. Current contract data (will be copied)
#' @param personnel_dt data.table. Current personnel data (will be copied)
#' @param movers_dt data.table. Output of \code{identify_movers()}.
#'   Must have columns: personnel_id, from_group, to_group, movement_type
#' @param policy_params List. Must contain:
#'   \describe{
#'     \item{group_cols}{Character vector defining state columns}
#'     \item{salary_scale}{data.table. Salary scale keyed on group_cols}
#'     \item{salary_update_rule}{Character. How to set salary after a move:
#'       \code{"scale"} (default) assigns the destination salary from
#'       \code{salary_scale}; \code{"keep"} retains the mover's current salary.}
#'   }
#' @param ref_date Date or character. Reference date
#' @param personnel_id_col Character. Personnel ID column (default: "personnel_id")
#' @param salary_col Character. Salary column (default: "gross_salary_lcu")
#' @param start_date_col Character. Contract start date column (default: "start_date")
#' @param end_date_col Character. End date column (default: "end_date")
#' @param contract_type_col Character. Contract type column (default: "contract_type_code")
#'
#' @return List containing:
#'   \describe{
#'     \item{contract_dt}{Updated contract data}
#'     \item{personnel_dt}{Updated personnel data (unchanged in current spec)}
#'     \item{movers_dt}{The input movers_dt (passed through for reporting)}
#'   }
#' @keywords internal
update_state_with_movement <- function(contract_dt,
                                       personnel_dt,
                                       movers_dt,
                                       policy_params,
                                       ref_date,
                                       personnel_id_col = "personnel_id",
                                       salary_col = "gross_salary_lcu",
                                       start_date_col = "start_date",
                                       end_date_col = "end_date",
                                       contract_type_col = "contract_type_code") {

  ref_date <- validate_date_format(ref_date, "ref_date")

  # Guard: no movers
  if (is.null(movers_dt) || nrow(movers_dt) == 0) {
    return(list(
      contract_dt  = contract_dt,
      personnel_dt = personnel_dt,
      movers_dt    = movers_dt
    ))
  }

  # ------------------------------------------------------------------
  # Capture pre-move salary per mover (summed across all contracts per person)
  # Must happen BEFORE any group or salary updates are applied.
  # ------------------------------------------------------------------
  mover_ids <- movers_dt[[personnel_id_col]]
  pre_salary_dt <- contract_dt[
    get(personnel_id_col) %in% mover_ids,
    .(salary_before = sum(get(salary_col), na.rm = TRUE)),
    by = c(personnel_id_col)
  ]
  movers_dt <- data.table::copy(movers_dt)
  movers_dt <- pre_salary_dt[movers_dt, on = personnel_id_col]
  # If no salary column in contract_dt, default to 0
  movers_dt[is.na(salary_before), salary_before := 0]

  group_cols         <- policy_params$group_cols
  salary_scale       <- policy_params$salary_scale
  salary_update_rule <- if (!is.null(policy_params$salary_update_rule))
                          policy_params$salary_update_rule else "scale"

  if (!salary_update_rule %in% c("scale", "keep")) {
    stop('policy_params$salary_update_rule must be "scale" or "keep"', call. = FALSE)
  }

  # ------------------------------------------------------------------
  # Validate salary_scale (only needed when rule = "scale")
  # ------------------------------------------------------------------
  if (salary_update_rule == "scale" &&
      (is.null(salary_scale) || !data.table::is.data.table(salary_scale))) {
    stop("policy_params$salary_scale must be a data.table", call. = FALSE)
  }

  # Detect salary column in salary_scale (only needed when rule = "scale")
  scale_salary_col <- NULL
  if (salary_update_rule == "scale") {
    salary_pattern    <- "salary|wage|pay|compensation|allowance"
    salary_candidates <- grep(salary_pattern, names(salary_scale),
                              value = TRUE, ignore.case = TRUE)
    if (length(salary_candidates) == 0) {
      stop("Could not detect salary column in salary_scale. ",
           "Ensure it contains a column matching 'salary|wage|pay|compensation|allowance'.",
           call. = FALSE)
    }
    scale_salary_col <- if ("gross_salary_lcu" %in% salary_candidates) "gross_salary_lcu"
                        else salary_candidates[1]

    if (anyDuplicated(salary_scale, by = group_cols)) {
      stop("Duplicate keys found in salary_scale for group_cols: ",
           paste(group_cols, collapse = ", "),
           ". Ensure salary_scale has unique rows per group.", call. = FALSE)
    }
  }

  # ------------------------------------------------------------------
  # Split to_group key back into group_col values
  # ------------------------------------------------------------------
  movers_dt <- data.table::copy(movers_dt)
  movers_dt[, c(paste0(".new_", group_cols)) :=
              data.table::tstrsplit(to_group, split = "||", fixed = TRUE)]

  # ------------------------------------------------------------------
  # Update group columns in contract_dt for each mover
  # ------------------------------------------------------------------
  # Use a join approach: set new group values in one pass
  # Build a lookup: personnel_id -> new group values
  update_lookup <- movers_dt[, c("personnel_id", paste0(".new_", group_cols)),
                              with = FALSE]
  data.table::setnames(update_lookup,
                       paste0(".new_", group_cols),
                       group_cols)

  # Check for duplicate key detection
  if (anyDuplicated(update_lookup, by = "personnel_id")) {
    warning("Some personnel appear in multiple movement transitions. ",
            "Only the last transition will be applied.", call. = FALSE)
    update_lookup <- unique(update_lookup, by = "personnel_id", fromLast = TRUE)
  }

  # Perform update on contract_dt by reference
  # Find rows in contract_dt belonging to movers
  mover_pids <- update_lookup$personnel_id

  for (gc in group_cols) {
    new_vals <- update_lookup[[gc]]
    names(new_vals) <- update_lookup$personnel_id

    contract_dt[get(personnel_id_col) %in% mover_pids,
                (gc) := new_vals[get(personnel_id_col)]]
  }

  # ------------------------------------------------------------------
  # Re-assign salary based on new group (skipped when rule = "keep")
  # ------------------------------------------------------------------
  if (salary_update_rule == "scale") {
    # Build salary lookup: group_col values -> salary
    scale_subset <- unique(salary_scale[, c(group_cols, scale_salary_col), with = FALSE])

    # Add group key to scale_subset
    scale_subset[, .group_key := do.call(paste, c(.SD, sep = "||")), .SDcols = group_cols]

    # Build a personnel_id -> new salary lookup via to_group key
    salary_lookup <- movers_dt[, .(personnel_id, .group_key = to_group)]
    salary_lookup <- scale_subset[, .(.group_key, .new_salary = get(scale_salary_col))][
      salary_lookup, on = ".group_key"
    ]
    salary_lookup <- salary_lookup[!is.na(.new_salary)]

    if (nrow(salary_lookup) > 0) {
      new_salary_vec <- salary_lookup$.new_salary
      names(new_salary_vec) <- salary_lookup$personnel_id

      # Ensure salary column exists in contract_dt
      if (!salary_col %in% names(contract_dt)) {
        contract_dt[, (salary_col) := NA_real_]
      }

      contract_dt[get(personnel_id_col) %in% names(new_salary_vec),
                  (salary_col) := new_salary_vec[get(personnel_id_col)]]
    }

    # Warn if any movers didn't get a salary assigned
    n_unmatched <- length(setdiff(mover_pids, salary_lookup$personnel_id))
    if (n_unmatched > 0) {
      warning(n_unmatched, " mover(s) could not be matched to salary_scale. ",
              "Their salary was not updated.", call. = FALSE, immediate. = TRUE)
    }
  }
  # salary_update_rule == "keep": salaries left unchanged in contract_dt

  # Clean up temporary columns from movers_dt
  movers_dt[, c(paste0(".new_", group_cols)) := NULL]

  return(list(
    contract_dt  = contract_dt,
    personnel_dt = personnel_dt,
    movers_dt    = movers_dt
  ))
}
