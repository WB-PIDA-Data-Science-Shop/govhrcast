#' Simulate Promotions and Transfers Module
#'
#' @description
#' Main user-facing function for simulating internal labor movements.
#' Orchestrates the full workflow:
#' \enumerate{
#'   \item Input validation
#'   \item Estimate empirical movement baseline from full panel data
#'   \item Select snapshot for simulation (nearest ref_date)
#'   \item Compute movement demand (policy-adjusted probabilities -> integer counts)
#'   \item Identify individual movers using selection strategies
#'   \item Update state (group assignments and salaries)
#'   \item Compute summary statistics
#' }
#'
#' @import data.table
#'
#' @param contract_dt data.table. Contract data in govhr harmonized format.
#'   For baseline estimation the full panel (all ref_dates) is used.
#'   A single-period subset is also accepted (transitions matrix will be empty).
#' @param personnel_dt data.table. Personnel data in govhr harmonized format.
#' @param salary_scale_dt data.table. Salary scale keyed on group_cols.
#'   Must include the salary column. Required.
#' @param policy_params List. Canonical 3-slot movement policy:
#'   \describe{
#'     \item{group_cols}{Character vector. State-defining columns, e.g.,
#'       \code{c("est_id", "paygrade")}. Or \code{NULL} for flat-rate path.}
#'     \item{policy_table}{data.table or \code{NULL}. Pre-computed transition
#'       probability matrix (output of \code{estimate_movement_baseline()}).
#'       When non-NULL, the panel-baseline step is skipped.}
#'     \item{defaults}{Named list. Must include:
#'       \describe{
#'         \item{movement_rate}{Numeric. Flat exit rate used when
#'           \code{policy_table} is \code{NULL}.}
#'         \item{movement_strategy}{Character. Candidate selection:
#'           \code{"random"}, \code{"tenure"}, \code{"reverse_tenure"}, or
#'           \code{"wage_based"}. Default: \code{"tenure"}.}
#'         \item{active_types}{Character vector. Contract type codes treated as
#'           active (default: \code{"perm"}).}
#'         \item{salary_update_rule}{Character. \code{"scale"} (destination
#'           salary from \code{salary_scale_dt}) or \code{"keep"} (retain
#'           pre-move salary). Default: \code{"scale"}.}
#'       }
#'     }
#'   }
#' @param ref_date Date or character. Reference date for the simulation snapshot.
#' @param ref_date_col Character. Column name holding panel snapshot dates
#'   (default: \code{"ref_date"}).
#' @param personnel_id_col Character. Personnel ID column (default: \code{"personnel_id"}).
#' @param contract_id_col Character. Contract ID column (default: \code{"contract_id"}).
#' @param start_date_col Character. Contract start date column (default: \code{"start_date"}).
#' @param end_date_col Character. Contract end date column (default: \code{"end_date"}).
#' @param salary_col Character. Salary column (default: \code{"gross_salary_lcu"}).
#' @param contract_type_col Character. Contract type column (default: \code{"contract_type_code"}).
#' @param status_col Character. Personnel status column (default: \code{"status"}).
#'
#' @return Named list:
#'   \describe{
#'     \item{summary}{data.table. High-level statistics: n_movers,
#'       historical rates, headcount before/after.}
#'     \item{contract_dt}{data.table. Updated contracts with new group assignments
#'       and salaries for movers. This is the simulation-period snapshot only.}
#'     \item{personnel_dt}{data.table. Personnel data (unchanged by this module).}
#'     \item{movers_dt}{data.table. Records for each mover: personnel_id,
#'       from_group, to_group.}
#'     \item{baseline_matrix}{data.table. Estimated transition probabilities
#'       from the full panel data.}
#'     \item{demand_dt}{data.table. Policy-adjusted movement demand by transition.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' contract_dt  <- copy(bra_hrmis_contract)
#' personnel_dt <- copy(bra_hrmis_personnel)
#'
#' salary_scale_dt <- data.table(
#'   est_id           = unique(contract_dt$est_id),
#'   gross_salary_lcu = 5000
#' )
#'
#' policy_params <- list(
#'   group_cols   = c("est_id"),
#'   policy_table = NULL,
#'   defaults = list(
#'     movement_rate     = 0.05,
#'     movement_strategy = "tenure",
#'     active_types      = "perm",
#'     salary_update_rule = "scale"
#'   )
#' )
#'
#' results <- simulate_promotions_transfers(
#'   contract_dt     = contract_dt,
#'   personnel_dt    = personnel_dt,
#'   salary_scale_dt = salary_scale_dt,
#'   policy_params   = policy_params,
#'   ref_date        = "2016-09-01"
#' )
#'
#' results$summary
#' results$movers_dt
#' results$baseline_matrix
#' }
#'
#' @export
simulate_promotions_transfers <- function(contract_dt,
                                          personnel_dt,
                                          salary_scale_dt,
                                          policy_params,
                                          ref_date,
                                          ref_date_col       = "ref_date",
                                          personnel_id_col   = "personnel_id",
                                          contract_id_col    = "contract_id",
                                          start_date_col     = "start_date",
                                          end_date_col       = "end_date",
                                          salary_col         = "gross_salary_lcu",
                                          contract_type_col  = "contract_type_code",
                                          status_col         = "status") {

  # ======================================================================
  # 1. Input Validation
  # ======================================================================
  check_movement_inputs(
    contract_dt       = contract_dt,
    personnel_dt      = personnel_dt,
    salary_scale_dt   = salary_scale_dt,
    policy_params     = policy_params,
    ref_date          = ref_date,
    personnel_id_col  = personnel_id_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    contract_type_col = contract_type_col,
    status_col        = status_col
  )

  # Convert ref_date early (accepts string or Date)
  ref_date <- validate_date_format(ref_date, "ref_date")

  # ======================================================================
  # 2. Copy inputs once (copy-once-modify-by-reference pattern)
  # ======================================================================
  if (!data.table::is.data.table(contract_dt)) {
    contract_dt <- data.table::as.data.table(contract_dt)
  } else {
    contract_dt <- data.table::copy(contract_dt)
  }

  if (!data.table::is.data.table(personnel_dt)) {
    personnel_dt <- data.table::as.data.table(personnel_dt)
  } else {
    personnel_dt <- data.table::copy(personnel_dt)
  }

  group_cols <- policy_params$group_cols

  # ======================================================================
  # 3. Extract or estimate baseline matrix
  # ======================================================================
  # policy_table = non-NULL  → use it directly as the transition baseline
  # policy_table = NULL      → try to estimate from panel (if >= 2 snapshots)
  #                            and store as informational output; demand is
  #                            still computed via compute_fixed_rate_movements()
  #                            (see step 6)
  n_snapshots <- if (ref_date_col %in% names(contract_dt))
    data.table::uniqueN(contract_dt[[ref_date_col]]) else 1L

  if (!is.null(policy_params$policy_table) &&
      data.table::is.data.table(policy_params$policy_table) &&
      nrow(policy_params$policy_table) > 0L) {
    baseline_matrix  <- policy_params$policy_table
    estimated_baseline <- baseline_matrix
  } else if (n_snapshots >= 2L && !is.null(group_cols)) {
    estimated_baseline <- tryCatch(
      estimate_movement_baseline(
        contract_dt       = contract_dt,
        group_cols        = group_cols,
        personnel_id_col  = personnel_id_col,
        ref_date_col      = ref_date_col,
        start_date_col    = start_date_col,
        end_date_col      = end_date_col,
        contract_type_col = contract_type_col
      ),
      error = function(e) NULL
    )
    baseline_matrix <- NULL   # demand still uses flat-rate
  } else {
    estimated_baseline <- NULL
    baseline_matrix    <- NULL
  }

  # ======================================================================
  # 4. Select simulation snapshot (nearest ref_date not after target)
  # ======================================================================
  selected_ref_date <- ref_date
  if (ref_date_col %in% names(contract_dt)) {
    selected_ref_date <- select_nearest_ref_date(contract_dt[[ref_date_col]], ref_date)
    snap_contract_dt  <- contract_dt[get(ref_date_col) == selected_ref_date]

    if (ref_date_col %in% names(personnel_dt)) {
      snap_personnel_dt <- personnel_dt[get(ref_date_col) == selected_ref_date]
    } else {
      snap_personnel_dt <- personnel_dt
    }
  } else {
    snap_contract_dt  <- contract_dt
    snap_personnel_dt <- personnel_dt
  }

  # ======================================================================
  # 4b. Guard: single snapshot with no pre-computed policy_table
  # ======================================================================
  # When only one period of data is available and the caller has not
  # supplied a policy_table, there is nothing to estimate or apply.
  if (n_snapshots < 2L && is.null(policy_params$policy_table)) {
    message("No movement baseline available: contract_dt contains only one ",
            "snapshot and policy_table is NULL. Returning 0 movers.")
    empty_movers <- data.table::data.table(
      personnel_id = character(0),
      from_group   = character(0),
      to_group     = character(0)
    )
    empty_demand <- data.table::data.table(
      from_group    = character(0),
      to_group      = character(0),
      movement_rate = numeric(0),
      current_stock = integer(0),
      n_movers      = integer(0)
    )
    empty_summary <- data.table::data.table(
      n_movers              = 0L,
      n_movers_demanded     = 0L,
      hist_avg_movement_rate = NA_real_,
      headcount_before      = 0L,
      headcount_after       = 0L
    )
    return(list(
      summary         = empty_summary,
      contract_dt     = snap_contract_dt,
      personnel_dt    = snap_personnel_dt,
      movers_dt       = empty_movers,
      baseline_matrix = data.table::data.table(),
      demand_dt       = empty_demand
    ))
  }

  # ======================================================================
  # 5. Compute headcount before
  # ======================================================================
  active_before <- get_active_contracts(
    contract_dt = snap_contract_dt,
    ref_date = selected_ref_date,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col
  )
  active_before <- active_before[
    snap_personnel_dt[get(status_col) == "active"],
    on = personnel_id_col, nomatch = NULL
  ]
  stock_before <- data.table::uniqueN(active_before[[personnel_id_col]])

  # ======================================================================
  # 6. Compute Movement Demand
  # ======================================================================
  # When a pre-computed baseline is available use compute_movement_demand()
  # (matrix × stock = expected movers per transition).
  # When policy_table = NULL use compute_fixed_rate_movements() which applies
  # defaults$movement_rate as a flat scalar across the active workforce.
  if (!is.null(baseline_matrix)) {
    demand_dt <- compute_movement_demand(
      contract_dt       = snap_contract_dt,
      personnel_dt      = snap_personnel_dt,
      baseline_matrix   = baseline_matrix,
      policy_params     = policy_params,
      salary_scale_dt   = salary_scale_dt,
      ref_date          = selected_ref_date,
      personnel_id_col  = personnel_id_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      contract_type_col = contract_type_col,
      status_col        = status_col
    )
  } else {
    baseline_matrix <- data.table::data.table()   # keep downstream code clean
    demand_dt <- compute_fixed_rate_movements(
      contract_dt       = snap_contract_dt,
      personnel_dt      = snap_personnel_dt,
      salary_scale_dt   = salary_scale_dt,
      policy_params     = policy_params,
      ref_date          = selected_ref_date,
      personnel_id_col  = personnel_id_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      contract_type_col = contract_type_col,
      status_col        = status_col
    )
  }

  # ======================================================================
  # 7. Identify Movers (selection engine)
  # ======================================================================
  if (is.null(demand_dt) || nrow(demand_dt) == 0 || sum(demand_dt$n_movers) == 0) {
    empty_movers <- data.table::data.table(
      personnel_id = character(0),
      from_group   = character(0),
      to_group     = character(0)
    )

    summary_tbl <- compute_movement_summary(
      movers_dt       = empty_movers,
      demand_dt       = demand_dt,
      baseline_matrix = baseline_matrix,
      stock_before    = stock_before,
      stock_after     = stock_before
    )

    return(list(
      summary         = summary_tbl,
      contract_dt     = snap_contract_dt,
      personnel_dt    = snap_personnel_dt,
      movers_dt       = empty_movers,
      baseline_matrix = if (!is.null(estimated_baseline)) estimated_baseline else baseline_matrix,
      demand_dt       = demand_dt
    ))
  }

  movers_dt <- identify_movers(
    contract_dt       = snap_contract_dt,
    personnel_dt      = snap_personnel_dt,
    demand_dt         = demand_dt,
    policy_params     = policy_params,
    ref_date          = selected_ref_date,
    baseline_matrix   = baseline_matrix,
    personnel_id_col  = personnel_id_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    contract_type_col = contract_type_col,
    salary_col        = salary_col,
    status_col        = status_col,
    ref_date_col      = ref_date_col
  )

  # ======================================================================
  # 8. Update State (ONLY state-modifying step)
  # ======================================================================
  update_result <- update_state_with_movement(
    contract_dt       = snap_contract_dt,
    personnel_dt      = snap_personnel_dt,
    movers_dt         = movers_dt,
    policy_params     = policy_params,
    salary_scale_dt   = salary_scale_dt,
    ref_date          = selected_ref_date,
    personnel_id_col  = personnel_id_col,
    salary_col        = salary_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    contract_type_col = contract_type_col
  )

  snap_contract_dt  <- update_result$contract_dt
  snap_personnel_dt <- update_result$personnel_dt
  movers_dt         <- update_result$movers_dt

  # ======================================================================
  # 10. Compute Summary Statistics
  # ======================================================================
  # Headcount after (unchanged since movements don't add/remove people)
  stock_after <- stock_before

  summary_tbl <- compute_movement_summary(
    movers_dt       = movers_dt,
    demand_dt       = demand_dt,
    baseline_matrix = baseline_matrix,
    stock_before    = stock_before,
    stock_after     = stock_after
  )

  # ======================================================================
  # 11. Return Results
  # ======================================================================
  return(list(
    summary         = summary_tbl,
    contract_dt     = snap_contract_dt,
    personnel_dt    = snap_personnel_dt,
    movers_dt       = movers_dt,
    baseline_matrix = if (!is.null(estimated_baseline)) estimated_baseline else baseline_matrix,
    demand_dt       = demand_dt
  ))
}
