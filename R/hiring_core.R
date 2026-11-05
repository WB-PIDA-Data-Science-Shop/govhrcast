#' Core Hiring Logic Functions
#'
#' @description
#' Functions for computing current workforce stock, estimating hiring demand
#' based on different policy modes (flow, stock, status quo), and calculating
#' hiring-related statistics.
#'
#' @import data.table
#' @name hiring_core
#' @keywords internal
NULL

#' Compute Current Workforce Stock
#'
#' @description
#' Aggregates active personnel counts at a reference date, optionally by
#' grouping variables (e.g., department, grade, etc.). Returns total headcount
#' if no grouping variables are specified.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param ref_date Date. Reference date for stock calculation
#' @param group_cols Character vector. Columns to group by (default: NULL for overall count)
#' @param column_spec_args Complete column specification from
#'   \code{.column_spec()}.
#'
#' @return data.table with group_cols (if specified) and current_stock column
#' @keywords internal
compute_current_stock <- function(contract_dt,
                                  personnel_dt,
                                  ref_date,
                                  group_cols = NULL,
                                  column_spec_args = .column_spec()) {
  personnel_id_col <- column_spec_args$personnel_id_col

  # Get active contracts at ref_date
  active_contracts <- get_active_contracts(
    contract_dt = contract_dt,
    ref_date = ref_date,
    start_date_col = column_spec_args$start_date_col,
    end_date_col = column_spec_args$end_date_col,
    contract_type_col = column_spec_args$contract_type_col
  )

  # Merge with personnel to get active personnel only.
  # NOTE: both contract_dt and personnel_dt must already be single-snapshot data.
  # The orchestrators (simulate_hiring, simulate_retirement) pre-filter to the
  # nearest ref_date before calling this function. If calling directly with raw
  # panel data, subset to a single snapshot first.
  #
  # personnel_dt is expected to be unique at the personnel_id level. If it is not,
  # the join below will produce a cartesian product. Check upfront and error clearly.
  if (anyDuplicated(personnel_dt, by = personnel_id_col)) {
    stop(
      "personnel_dt has duplicate rows for the same ", personnel_id_col, ". ",
      "Ensure personnel_dt is unique at the ", personnel_id_col,
      " level before calling compute_current_stock().",
      call. = FALSE
    )
  }

  active_personnel <- active_contracts[
    personnel_dt[get(column_spec_args$status_col) == "active"],
    on = personnel_id_col,
    nomatch = NULL
  ]

  # If group_cols specified, ensure they exist in active_personnel
  if (!is.null(group_cols) && length(group_cols) > 0) {
    missing_cols <- setdiff(group_cols, names(active_personnel))
    if (length(missing_cols) > 0) {
      stop("Grouping columns not found in data: ", paste(missing_cols, collapse = ", "),
           call. = FALSE)
    }

    # Aggregate by group_cols - count unique personnel
    stock_dt <- active_personnel[,
      .(current_stock = data.table::uniqueN(get(personnel_id_col))),
      by = group_cols
    ]
  } else {
    # Overall count - no grouping
    stock_dt <- data.table::data.table(
      current_stock = data.table::uniqueN(active_personnel[[personnel_id_col]])
    )
  }

  return(stock_dt)
}


#' Compute Flow-Based Hiring Demand
#'
#' @description
#' Calculates hiring needs from the people who left this period: retirees and
#' other exits.  Demand = leavers x replacement_rate, so a replacement rate of 1
#' holds headcount.  Replacement rate can be scalar or data.table matched on
#' group_cols.  A period when nobody left needs no hires.
#'
#' @param contract_dt data.table. Current contract data
#' @param personnel_dt data.table. Current personnel data
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item replacement_rate: Scalar or data.table with group_cols + replacement_rate
#'     \item group_cols: Character vector of grouping columns (can be NULL)
#'     \item defaults: Only when \code{leavers_dt} is \code{NULL}: the retirement
#'       rule to work out who leaves, e.g.
#'       \code{list(eligibility_type = "age_only", min_age = 60)}.
#'   }
#' @param leavers_dt data.table or \code{NULL}. The people who left, with the
#'   person ID column, and the group columns if they have them (otherwise these
#'   are read from \code{contract_dt}).  An empty table means nobody left.
#'   \code{NULL} means this is not known, so retirees are worked out from the rule
#'   in \code{policy_params$defaults}.  \code{simulate_scenario()} always passes
#'   a table.
#' @param ref_date Date. Reference date for calculations
#' @param column_spec_args Complete column specification from
#'   \code{.column_spec()}.
#'
#' @return data.table with group_cols (if specified) and total_hires column
#' @keywords internal
compute_flow_demand <- function(contract_dt,
                                personnel_dt,
                                policy_params,
                                leavers_dt = NULL,
                                ref_date,
                                column_spec_args = .column_spec()) {
  personnel_id_col <- column_spec_args$personnel_id_col

  group_cols <- policy_params$group_cols
  replacement_rate <- policy_params$replacement_rate

  if (is.null(leavers_dt)) {
    if (is.null(policy_params$defaults$eligibility_type)) {
      stop(
        "Flow hiring needs to know who left: pass leavers_dt, or set a retirement ",
        "rule in the policy's defaults, e.g. defaults$eligibility_type = \"age_only\" ",
        "and defaults$min_age = 60.",
        call. = FALSE
      )
    }
    leavers_dt <- identify_eligibility(
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      policy_params = policy_params,
      ref_date = ref_date,
      column_spec_args = column_spec_args
    )[retire == 1]
  }

  # Nobody left, so there is nobody to replace
  if (nrow(leavers_dt) == 0L) {
    return(data.table::data.table(total_hires = numeric(0)))
  }

  # Each leaver is replaced in the group of their main contract
  exits_dt <- leavers_dt
  if (!is.null(group_cols) && length(group_cols) > 0) {
    if (length(setdiff(group_cols, names(exits_dt))) > 0) {
      exits_dt <- get_primary_contract(
        contract_dt      = contract_dt[get(personnel_id_col) %in% exits_dt[[personnel_id_col]]],
        personnel_id_col = personnel_id_col,
        contract_id_col  = column_spec_args$contract_id_col,
        start_date_col   = column_spec_args$start_date_col,
        salary_col       = column_spec_args$salary_col
      )[, c(personnel_id_col, group_cols), with = FALSE]
    }
  }

  # Compute exiting stock by group (or overall)
  if (!is.null(group_cols) && length(group_cols) > 0) {
    exiting_stock <- exits_dt[,
      .(exit_count = .N),
      by = group_cols
    ]
  } else {
    exiting_stock <- data.table::data.table(
      exit_count = nrow(exits_dt)
    )
  }

  # Apply replacement rate
  if (is.data.table(replacement_rate)) {
    # Merge replacement rates by group_cols
    if (is.null(group_cols) || length(group_cols) == 0) {
      stop("replacement_rate is a data.table but no group_cols specified", call. = FALSE)
    }

    # A group with leavers but no row in the table uses unmatched_rate
    unmatched_rate <- as.numeric(policy_params$unmatched_rate %||% 0)
    demand_dt <- replacement_rate[exiting_stock, on = group_cols]
    demand_dt[, total_hires := exit_count *
                data.table::fcoalesce(as.numeric(replacement_rate), unmatched_rate)]
  } else {
    # Scalar replacement rate
    demand_dt <- data.table::copy(exiting_stock)
    demand_dt[, total_hires := exit_count * replacement_rate]
  }

  # Whole people, never negative
  demand_dt[, total_hires := stochastic_round(pmax(0, total_hires))]

  # Return with group_cols and total_hires
  if (!is.null(group_cols) && length(group_cols) > 0) {
    result <- demand_dt[, c(group_cols, "total_hires"), with = FALSE]
  } else {
    result <- demand_dt[, .(total_hires)]
  }

  return(result)
}


#' Compute Stock-Based Hiring Demand
#'
#' @description
#' Calculates hiring needs to achieve target workforce levels.
#' Demand = target_stock - current_stock. Allows negative values (downsizing).
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item stock_targets: data.table with group_cols + target_stock
#'     \item group_cols: Character vector of grouping columns (can be NULL)
#'   }
#' @param ref_date Date. Reference date
#' @param column_spec_args Complete column specification from
#'   \code{.column_spec()}.
#'
#' @return data.table with group_cols (if specified) and total_hires column
#' @keywords internal
compute_stock_demand <- function(contract_dt,
                                 personnel_dt,
                                 policy_params,
                                 ref_date,
                                 column_spec_args = .column_spec()) {

  group_cols <- policy_params$group_cols
  stock_targets <- policy_params$stock_targets

  # Validate stock_targets
  if (!is.data.table(stock_targets)) {
    stop("policy_params$stock_targets must be a data.table", call. = FALSE)
  }

  if (!"target_stock" %in% names(stock_targets)) {
    stop("stock_targets must contain 'target_stock' column", call. = FALSE)
  }

  # Compute current stock
  current_stock_dt <- compute_current_stock(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    ref_date = ref_date,
    group_cols = group_cols,
    column_spec_args = column_spec_args
  )

  # Merge targets with current stock, keeping groups found in only one of them:
  # a group with no target stays as it is, a target for a group with no staff yet
  # is hired up to.
  if (!is.null(group_cols) && length(group_cols) > 0) {
    demand_dt <- merge(stock_targets, current_stock_dt, by = group_cols, all = TRUE)
    demand_dt[is.na(target_stock), target_stock := current_stock]
    demand_dt[is.na(current_stock), current_stock := 0L]
  } else {
    # Overall demand (no grouping)
    if (nrow(stock_targets) != 1) {
      stop("For overall demand (no group_cols), stock_targets must have exactly 1 row",
           call. = FALSE)
    }
    demand_dt <- data.table::copy(stock_targets)
    demand_dt[, current_stock := current_stock_dt$current_stock]
  }

  # Compute demand (allow negative values for downsizing)
  demand_dt[, total_hires := target_stock - current_stock]

  # Return with group_cols and total_hires
  if (!is.null(group_cols) && length(group_cols) > 0) {
    result <- demand_dt[, c(group_cols, "total_hires"), with = FALSE]
  } else {
    result <- demand_dt[, .(total_hires)]
  }

  return(result)
}


#' Estimate Historical Hiring Rates from Panel Data
#'
#' @description
#' Estimates per-group hiring rates (hires per head of starting stock, per interval) from historical panel
#' data.  Two methods are supported, selected by \code{hire_date_col} in
#' \code{column_spec_args}:
#'
#' \describe{
#'   \item{Panel first-appearance (default, \code{hire_date_col = NULL})}{
#'     Uses \code{govhr::detect_movement()} to detect the first time a
#'     person appears in the panel.  Left-censored at the panel start date —
#'     pre-existing staff present at the first snapshot are not counted as
#'     hires.  Each pair of consecutive snapshots is one interval; someone
#'     absent at its start and active at its end is a hire in that interval.
#'     The rate is \strong{pooled} across intervals:
#'     \code{hiring_rate = total hires / total active staff at the start of
#'     each interval}.  E.g. 5 hires on a starting stock of 10, then 0 hires
#'     on 15, gives \code{5 / 25}.  Dividing by the \emph{starting} stock
#'     (not the stock after hiring) matches how the simulation applies the
#'     rate: rate x the stock at the start of each period.  Hires are counted
#'     in the group they join; a group with no staff at the start of an
#'     interval has no base, so its hires in that interval are not counted.}
#'   \item{Administrative hire date (\code{hire_date_col} supplied)}{
#'     Uses a person-level hire-date column (e.g. \code{"first_employment_date"})
#'     read directly from \code{panel_personnel_dt}.  A hire belongs to the
#'     interval their hire date falls in, and is counted in the group they are in
#'     at the first snapshot after it.  The rate is pooled in the same way, over
#'     the same starting stock.}
#' }
#'
#' @param panel_contract_dt data.table. Full panel of contract data (all ref_dates).
#' @param panel_personnel_dt data.table. Full panel of personnel data (all ref_dates).
#' @param group_cols Character vector. Columns to group by (e.g. \code{"est_id"}).
#'   Pass \code{NULL} for overall (ungrouped) rates.
#' @param column_spec_args Named list.  Column names, if your data use
#'   different names from the \code{govhr} defaults: give only the ones you
#'   need to change, e.g. \code{list(personnel_id_col = "emp_id")}.  Keys read
#'   here: \code{personnel_id_col}, \code{ref_date_col}, \code{start_date_col},
#'   \code{end_date_col}, \code{contract_type_col}, \code{status_col}, and
#'   \code{hire_date_col}: a person-level column in \code{panel_personnel_dt}
#'   holding the true administrative hire date (e.g.
#'   \code{"first_employment_date"}).  When given, hires are counted directly
#'   from that column instead of from when people first appear in the panel.
#'   See \code{\link{simulate_horizon}} for the defaults.
#'
#' @return data.table with \code{group_cols} (if specified) and \code{hiring_rate} column.
#' @export
estimate_historical_hiring_rates <- function(panel_contract_dt,
                                             panel_personnel_dt,
                                             group_cols,
                                             column_spec_args = list()) {

  cols <- .column_spec(column_spec_args)
  .check_spec_columns(panel_contract_dt, panel_personnel_dt, cols, required = list(
    contract_dt  = c("personnel_id_col", "ref_date_col", "start_date_col",
                     "end_date_col", "contract_type_col"),
    personnel_dt = c("personnel_id_col", "ref_date_col", "status_col")
  ))
  personnel_id_col <- cols$personnel_id_col
  ref_date_col     <- cols$ref_date_col

  panel_dates <- sort(unique(panel_personnel_dt[[ref_date_col]]))

  # Who was hired, each dated to the snapshot that ends the interval they were
  # hired in. The two sources differ only there: the rate below is the same.
  hire_events <- .hire_events(panel_personnel_dt, cols)

  # Join to contract panel to retrieve group_cols per hire.
  # govhr::add_contract_to_event() deduplicates to one row per
  # person x ref_date x keep_vars before joining, preventing row
  # duplication for multi-contract individuals.
  if (!is.null(group_cols) && length(group_cols) > 0) {
    hire_events <- .govhr_add_contract_to_event(
      hire_events, panel_contract_dt, group_cols, personnel_id_col, ref_date_col
    )

    hire_counts <- hire_events[
      !is.na(get(group_cols[[1]])),   # drop hires without a matching group
      .(n_hires = .N),
      by = c(ref_date_col, group_cols)
    ]
  } else {
    hire_counts <- hire_events[, .(n_hires = .N), by = ref_date_col]
  }

  # Each hire is dated to the END of the snapshot interval it happens in
  # (detect_movement() flags a hire at t when the person was absent
  # at t-1). The exposed base is the active stock at the START of that
  # interval (t-1) -- matching how compute_status_quo_hiring() applies the
  # rate (rate x stock at the start of a period) and how exit rates are
  # estimated. Using the stock at t instead would count the new hires in
  # their own denominator. The first snapshot ends no interval and can
  # never have a hire, so it contributes no events -- only its stock, as
  # the base for the first interval.
  if (length(panel_dates) >= 2L) {
    interval_start <- panel_dates[-length(panel_dates)]
    interval_end   <- panel_dates[-1L]
  } else {
    interval_start <- panel_dates
    interval_end   <- panel_dates
  }

  # Active stock per group at each interval start, stamped with the
  # interval's END date so it joins to the hires dated at that end.
  stock_list <- lapply(seq_along(interval_start), function(i) {
    snap <- interval_start[i]

    s <- compute_current_stock(
      contract_dt       = .panel_snapshot(panel_contract_dt, ref_date_col, snap),
      personnel_dt      = .panel_snapshot(panel_personnel_dt, ref_date_col, snap),
      ref_date          = snap,
      group_cols        = group_cols,
      column_spec_args  = cols
    )
    s[, (ref_date_col) := interval_end[i]]
    s
  })
  stock_dt <- data.table::rbindlist(stock_list, fill = TRUE)

  # Merge hire counts with start-of-interval stock
  join_keys <- if (!is.null(group_cols) && length(group_cols) > 0) {
    c(ref_date_col, group_cols)
  } else {
    ref_date_col
  }

  # Left join from the stock side: hires into a group that had no staff at
  # the start of the interval have no base to form a rate from and drop out.
  rate_dt <- hire_counts[stock_dt, on = join_keys]
  rate_dt[is.na(n_hires), n_hires := 0L]

  # Pooled rate: total hires / total start-of-interval stock across all
  # intervals (same pooling as estimate_historical_exit_rates() and
  # govhr::estimate_decrement_rates()).
  .pool <- function(n_ev, stock) {
    tot <- sum(stock, na.rm = TRUE)
    if (tot > 0) sum(n_ev, na.rm = TRUE) / tot else 0
  }
  if (!is.null(group_cols) && length(group_cols) > 0) {
    result <- rate_dt[, .(hiring_rate = .pool(n_hires, current_stock)), by = group_cols]
  } else {
    result <- data.table::data.table(hiring_rate = .pool(rate_dt$n_hires, rate_dt$current_stock))
  }

  result
}


# The hires in a panel, one row per hire, dated to the first snapshot that shows
# them after they were hired. From the hire date column if the spec names one, or
# else from when people first appear in the panel.
.hire_events <- function(panel_personnel_dt, cols) {
  personnel_id_col <- cols$personnel_id_col
  ref_date_col     <- cols$ref_date_col
  hire_date_col    <- cols$hire_date_col

  if (is.null(hire_date_col)) {
    return(.govhr_detect_movement(
      panel_personnel_dt, personnel_id_col, ref_date_col, cols$status_col
    )[hire %in% TRUE])
  }

  if (!hire_date_col %in% names(panel_personnel_dt)) {
    stop("hire_date_col '", hire_date_col, "' not found in panel_personnel_dt.", call. = FALSE)
  }
  panel_dates <- panel_personnel_dt[[ref_date_col]]
  events <- panel_personnel_dt[
    get(hire_date_col) > min(panel_dates, na.rm = TRUE) &
      get(hire_date_col) <= max(panel_dates, na.rm = TRUE) &
      get(ref_date_col) >= get(hire_date_col),
    c(personnel_id_col, ref_date_col), with = FALSE
  ]
  events[events[, .I[which.min(get(ref_date_col))], by = personnel_id_col]$V1]
}


#' Estimate the Profile of New Hires from Panel Data
#'
#' @description
#' Works out what a new hire looks like, from the people hired during the panel,
#' so that simulated hires start with the same kind of state as everyone else.
#' \code{\link{simulate_horizon}} does this for you, once for each hiring group and
#' once overall for groups with no hires in the panel.  Call it yourself to look at
#' the profile, or to edit it and pass it back as \code{hiring_policy$new_hire_profile}.
#'
#' Hires are found the same way as in \code{\link{estimate_historical_hiring_rates}}.
#' Entry age is the median of their ages at the first snapshot that shows them, and
#' contract type is the most common among them (ties go to the first alphabetically).
#'
#' @param panel_contract_dt data.table. Full panel of contract data (all snapshots).
#' @param panel_personnel_dt data.table. Full panel of personnel data (all snapshots).
#' @param group_cols Character vector or \code{NULL}. The hiring groups, e.g.
#'   \code{"est_id"}.  \code{NULL} gives one profile for all hires.
#' @param column_spec_args Named list.  Column names, if your data use different
#'   names from the \code{govhr} defaults: give only the ones you need to change.
#'   Keys read here: \code{personnel_id_col}, \code{ref_date_col}, \code{status_col},
#'   \code{hire_date_col}, \code{contract_type_col}, \code{age_col},
#'   \code{birth_date_col} and \code{gender_col}.  Age comes from \code{age_col}
#'   where it is filled in, and from \code{birth_date_col} otherwise.
#'
#' @return data.table, one row per group and gender: \code{group_cols}, the gender
#'   column (if your data have one), \code{share} (the share of that gender among
#'   the group's hires; a blank gender is left out), \code{entry_age} and the
#'   contract type column.  Zero rows when the panel shows no hires.  Each row
#'   describes few people when groups are small, so handle it like the microdata.
#'
#' @examples
#' start <- as.Date("2012-09-01")
#' estimate_new_hire_profile(
#'   bra_hrmis_contract[bra_hrmis_contract$ref_date <= start, ],
#'   bra_hrmis_personnel[bra_hrmis_personnel$ref_date <= start, ]
#' )
#' @export
estimate_new_hire_profile <- function(panel_contract_dt,
                                      panel_personnel_dt,
                                      group_cols = NULL,
                                      column_spec_args = list()) {
  cols <- .column_spec(column_spec_args)
  .check_spec_columns(panel_contract_dt, panel_personnel_dt, cols, required = list(
    contract_dt  = c("personnel_id_col", "ref_date_col", "contract_type_col"),
    personnel_dt = c("personnel_id_col", "ref_date_col", "status_col")
  ))
  personnel_id_col  <- cols$personnel_id_col
  ref_date_col      <- cols$ref_date_col
  contract_type_col <- cols$contract_type_col
  gender_col        <- cols$gender_col

  # Each hire's group and contract type at that snapshot, and their personal details
  hires <- .govhr_add_contract_to_event(
    .hire_events(panel_personnel_dt, cols), panel_contract_dt,
    c(group_cols, contract_type_col), personnel_id_col, ref_date_col
  )
  hires <- unique(hires, by = c(personnel_id_col, ref_date_col))
  if (length(group_cols) > 0L) hires <- stats::na.omit(hires, cols = group_cols)

  details <- intersect(c(cols$age_col, cols$birth_date_col, gender_col), names(panel_personnel_dt))
  hires <- panel_personnel_dt[, c(personnel_id_col, ref_date_col, details), with = FALSE][
    hires, on = c(personnel_id_col, ref_date_col)
  ]

  has_gender <- !is.null(gender_col) && gender_col %in% details
  if (nrow(hires) == 0L) {
    empty <- hires[0L, c(group_cols, if (has_gender) gender_col, contract_type_col), with = FALSE]
    return(empty[, `:=`(share = numeric(0), entry_age = numeric(0))][])
  }

  age_from <- function(col, values) {
    if (!is.null(col) && col %in% details) values(hires[[col]]) else rep(NA_real_, nrow(hires))
  }
  hires[, .entry_age := data.table::fcoalesce(
    age_from(cols$age_col, as.numeric),
    age_from(cols$birth_date_col, function(birth) {
      as.numeric(difftime(hires[[ref_date_col]], birth, units = "days")) / 365.25
    })
  )]

  by_group <- hires[, .(
    entry_age = stats::median(.entry_age, na.rm = TRUE),
    .type = .most_common_staff_type(get(contract_type_col))
  ), by = group_cols]
  data.table::setnames(by_group, ".type", contract_type_col)

  shares <- if (has_gender) hires[!is.na(get(gender_col)), .(share = as.numeric(.N)), by = c(group_cols, gender_col)]
  if (is.null(shares) || nrow(shares) == 0L) return(by_group[, share := 1][])

  shares[, share := share / sum(share), by = group_cols]
  profile <- if (length(group_cols) > 0L) {
    shares[by_group, on = group_cols][is.na(share), share := 1]
  } else {
    cbind(shares, by_group)
  }
  data.table::setorderv(profile, c(group_cols, gender_col))[]
}

# The most common contract type among staff, or blank if there is none.
.most_common_staff_type <- function(contract_type) {
  staff <- contract_type[!is.na(contract_type) & .on_payroll(contract_type)]
  if (length(staff) == 0L) return(NA_character_)
  counts <- table(staff)
  names(counts)[which.max(counts)]
}


#' Compute Status-Quo Hiring Demand
#'
#' @description
#' Replicates the historical hiring rate observed in the panel to generate
#' period-specific hiring demand.  The rate table is estimated once by
#' \code{estimate_historical_hiring_rates()} (\code{simulate_horizon()} does
#' this before the period loop and stores it as \code{policy_params$squorate_dt}),
#' then scaled by \code{policy_params$rate_mult} (default 1), and applied to
#' the current active stock.
#'
#' @param contract_dt data.table. Current-period contract snapshot (no ref_date column).
#' @param personnel_dt data.table. Current-period personnel snapshot (no ref_date column).
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item squorate_dt: data.table of historical hiring rates
#'       (\code{group_cols} plus \code{hiring_rate}), from
#'       \code{estimate_historical_hiring_rates()}.
#'     \item group_cols: Character vector or NULL.
#'     \item rate_mult: Numeric scalar (default 1). Scales the estimated historical rate.
#'     \item unmatched_rate: Numeric scalar (default 0). Rate for groups with no
#'       row in \code{squorate_dt}.
#'     \item salary_scale: data.table used for salary assignment of new hires.
#'   }
#' @param ref_date Date. Reference date for current-period stock calculation.
#' @param column_spec_args Complete column specification from
#'   \code{.column_spec()}.
#'
#' @return data.table with \code{group_cols} (if specified) and \code{total_hires} column.
#' @keywords internal
compute_status_quo_hiring <- function(contract_dt,
                                      personnel_dt,
                                      policy_params,
                                      ref_date,
                                      column_spec_args = .column_spec()) {

  group_cols     <- policy_params$group_cols
  rate_mult      <- policy_params$rate_mult %||% 1
  unmatched_rate <- as.numeric(policy_params$unmatched_rate %||% 0)
  rates          <- policy_params$squorate_dt

  # Current active stock in this period
  stock <- compute_current_stock(
    contract_dt       = contract_dt,
    personnel_dt      = personnel_dt,
    ref_date          = ref_date,
    group_cols        = group_cols,
    column_spec_args  = column_spec_args
  )

  # Merge rates onto stock
  if (!is.null(group_cols) && length(group_cols) > 0) {
    demand <- rates[stock, on = group_cols]
  } else {
    demand <- cbind(stock, rates)
  }

  # A group with no row in the rate table
  demand[, hiring_rate := data.table::fcoalesce(as.numeric(hiring_rate), unmatched_rate)]

  # Hires this period: current stock x historical rate x rate_mult. The
  # policy's rate table is only read, never changed, so every period applies
  # rate_mult to the same historical rates.
  demand[, total_hires := stochastic_round(pmax(0, current_stock * hiring_rate * rate_mult))]

  if (!is.null(group_cols) && length(group_cols) > 0) {
    demand[, c(group_cols, "total_hires"), with = FALSE]
  } else {
    demand[, .(total_hires)]
  }
}


#' Estimate Hiring Demand
#'
#' @description
#' Wrapper function that routes to appropriate demand calculation based on
#' policy mode: \code{"flow"}, \code{"stock"} or \code{"status_quo"}. Pure
#' function — no state modification.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param policy_params List. Must contain:
#'   \itemize{
#'     \item mode: \code{"flow"}, \code{"stock"} or \code{"status_quo"}
#'     \item Other parameters depending on mode (see specific functions)
#'   }
#' @param leavers_dt data.table or \code{NULL}. The people who left this period,
#'   for flow hiring: see \code{\link{compute_flow_demand}}.
#' @param ref_date Date. Reference date
#' @param column_spec_args Complete column specification from
#'   \code{.column_spec()}.
#'
#' @return data.table with group_cols (if specified) and total_hires column
#' @keywords internal
estimate_hiring_demand <- function(contract_dt,
                                   personnel_dt,
                                   policy_params,
                                   leavers_dt = NULL,
                                   ref_date,
                                   column_spec_args = .column_spec()) {

  mode <- policy_params$mode

  demand_dt <- switch(
    mode,

    "flow" = compute_flow_demand(
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      policy_params = policy_params,
      leavers_dt = leavers_dt,
      ref_date = ref_date,
      column_spec_args = column_spec_args
    ),

    "stock" = compute_stock_demand(
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      policy_params = policy_params,
      ref_date = ref_date,
      column_spec_args = column_spec_args
    ),

    "status_quo" = compute_status_quo_hiring(
      contract_dt       = contract_dt,
      personnel_dt      = personnel_dt,
      policy_params     = policy_params,
      ref_date          = ref_date,
      column_spec_args  = column_spec_args
    ),

    stop("Unknown hiring mode: ", mode, ". Must be 'flow', 'stock' or 'status_quo'",
         call. = FALSE)
  )

  return(demand_dt)
}


#' Compute Hiring Summary Statistics
#'
#' @description
#' Aggregates key statistics about hiring including new hires count,
#' net headcount change, total headcount, and total salary cost impact.
#' Uses data.table for efficient computation.
#'
#' For hiring scenarios, `total_new_salary_cost` is the annual salary bill added
#' (sum of new hire salaries). For downsizing scenarios it is the annual salary
#' bill removed (sum of terminated worker salaries, reported as a negative number
#' to indicate savings). Mixed scenarios sum both.
#'
#' @param adjustment_dt data.table. Adjustment data with net_change column
#' @param new_hires_dt data.table. New hire personnel data (optional)
#' @param new_contracts_dt data.table. New hire contract data with salary (optional)
#' @param terminated_contracts_dt data.table. Terminated contract data with salary (optional)
#' @param total_headcount Integer. Current total headcount after adjustment
#' @param salary_col Character. Pay column (default: "gross_salary_lcu")
#' @param personnel_id_col Character. Person ID column, to count the people let
#'   go (default: "personnel_id")
#'
#' @return data.table with summary statistics, including \code{n_downsized} (people
#'   let go) and \code{downsizing_savings} (their pay, as a positive number)
#' @keywords internal
compute_hiring_summary <- function(adjustment_dt,
                                   new_hires_dt = NULL,
                                   new_contracts_dt = NULL,
                                   terminated_contracts_dt = NULL,
                                   total_headcount = NA_integer_,
                                   salary_col = "gross_salary_lcu",
                                   personnel_id_col = "personnel_id") {

  # Calculate statistics
  n_new_hires <- if (!is.null(new_hires_dt)) nrow(new_hires_dt) else 0L
  net_change <- if (nrow(adjustment_dt) > 0) sum(adjustment_dt$net_change, na.rm = TRUE) else 0L

  # New hire salary cost (positive)
  hire_cost <- if (!is.null(new_contracts_dt) && nrow(new_contracts_dt) > 0 &&
                   salary_col %in% names(new_contracts_dt)) {
    sum(new_contracts_dt[[salary_col]], na.rm = TRUE)
  } else {
    0
  }

  # Terminated salary savings (reported as negative — cost removed from payroll)
  downsize_cost <- if (!is.null(terminated_contracts_dt) && nrow(terminated_contracts_dt) > 0 &&
                        salary_col %in% names(terminated_contracts_dt)) {
    -sum(terminated_contracts_dt[[salary_col]], na.rm = TRUE)
  } else {
    0
  }

  # Combined: positive = net new cost, negative = net savings
  total_salary_impact <- hire_cost + downsize_cost
  new_salary_cost <- if (hire_cost == 0 && downsize_cost == 0) NA_real_ else total_salary_impact

  n_downsized <- if (!is.null(terminated_contracts_dt) && nrow(terminated_contracts_dt) > 0) {
    data.table::uniqueN(terminated_contracts_dt[[personnel_id_col]])
  } else {
    0L
  }

  summary_tbl <- data.table::data.table(
    n_new_hires = as.integer(n_new_hires),
    net_headcount_change = as.integer(net_change),
    total_headcount = as.integer(total_headcount),
    total_new_salary_cost = new_salary_cost,
    n_downsized = as.integer(n_downsized),
    downsizing_savings = -downsize_cost
  )

  return(summary_tbl)
}
