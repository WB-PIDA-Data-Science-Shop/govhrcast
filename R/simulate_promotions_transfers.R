#' Simulate Promotions and Transfers Module
#'
#' @description
#' Main user-facing function for simulating internal labor movements (promotions
#' and transfers). Orchestrates the full workflow:
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
#' @param policy_params List. Movement policy parameters:
#'   \describe{
#'     \item{group_cols}{Character vector. State-defining columns, e.g.,
#'       \code{c("est_id", "paygrade")}. Required.}
#'     \item{salary_scale}{data.table. Salary scale keyed on group_cols.
#'       Must include a salary column. Required.}
#'     \item{promotion_multiplier}{Numeric scalar. Multiplier applied to all
#'       promotion probabilities (default: 1.0 = status quo).}
#'     \item{transfer_multiplier}{Numeric scalar. Multiplier applied to all
#'       transfer probabilities (default: 1.0).}
#'     \item{promotion_strategy}{Character. How to rank promotion candidates:
#'       \code{"tenure"} (longest time-in-grade first) or
#'       \code{"wage_based"} (lowest paid relative to grade max first).
#'       Default: \code{"tenure"}.}
#'     \item{transfer_strategy}{Character. How to rank transfer candidates:
#'       \code{"random"}, \code{"tenure"} (longest total tenure first), or
#'       \code{"reverse_tenure"} (shortest total tenure first, LIFO).
#'       Default: \code{"random"}.}
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
#'     \item{summary}{data.table. High-level statistics: promotions, transfers,
#'       historical rates, headcount before/after.}
#'     \item{contract_dt}{data.table. Updated contracts with new group assignments
#'       and salaries for movers. This is the simulation-period snapshot only.}
#'     \item{personnel_dt}{data.table. Personnel data (unchanged by this module).}
#'     \item{movers_dt}{data.table. Records for each mover: personnel_id,
#'       from_group, to_group, movement_type.}
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
#' policy_params <- list(
#'   group_cols          = c("est_id"),
#'   salary_scale        = data.table(
#'     est_id           = unique(contract_dt$est_id),
#'     gross_salary_lcu = 5000
#'   ),
#'   promotion_multiplier = 1.0,
#'   transfer_multiplier  = 1.0,
#'   promotion_strategy   = "tenure",
#'   transfer_strategy    = "random"
#' )
#'
#' results <- simulate_promotions_transfers(
#'   contract_dt  = contract_dt,
#'   personnel_dt = personnel_dt,
#'   policy_params = policy_params,
#'   ref_date     = "2016-09-01"
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
  # 3. Estimate Movement Baseline (uses FULL panel data)
  # ======================================================================
  baseline_matrix <- NULL
  has_panel <- ref_date_col %in% names(contract_dt) &&
               data.table::uniqueN(contract_dt[[ref_date_col]]) >= 2L

  if (has_panel) {
    baseline_matrix <- estimate_movement_baseline(
      contract_dt       = contract_dt,
      group_cols        = group_cols,
      personnel_id_col  = personnel_id_col,
      ref_date_col      = ref_date_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      contract_type_col = contract_type_col
    )
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
  # 6. Handle no baseline (single snapshot or no movements)
  # ======================================================================
  if (is.null(baseline_matrix) || nrow(baseline_matrix) == 0) {
    message("No movement baseline available (need >= 2 panel snapshots). ",
            "Returning input data unchanged.")

    empty_movers <- data.table::data.table(
      personnel_id  = character(0),
      from_group    = character(0),
      to_group      = character(0),
      movement_type = character(0)
    )

    empty_demand <- data.table::data.table(
      from_group    = character(0),
      to_group      = character(0),
      movement_type = character(0),
      adj_prob      = numeric(0),
      current_stock = integer(0),
      n_movers      = integer(0)
    )

    summary_tbl <- compute_movement_summary(
      movers_dt       = empty_movers,
      demand_dt       = empty_demand,
      baseline_matrix = NULL,
      stock_before    = stock_before,
      stock_after     = stock_before
    )

    return(list(
      summary        = summary_tbl,
      contract_dt    = snap_contract_dt,
      personnel_dt   = snap_personnel_dt,
      movers_dt      = empty_movers,
      baseline_matrix = data.table::data.table(),
      demand_dt      = empty_demand
    ))
  }

  # ======================================================================
  # 7. Compute Policy-Adjusted Movement Demand (pure function)
  # ======================================================================
  demand_dt <- compute_movement_demand(
    contract_dt       = snap_contract_dt,
    personnel_dt      = snap_personnel_dt,
    baseline_matrix   = baseline_matrix,
    policy_params     = policy_params,
    salary_scale_dt   = policy_params$salary_scale,
    ref_date          = selected_ref_date,
    personnel_id_col  = personnel_id_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    contract_type_col = contract_type_col,
    status_col        = status_col
  )

  # ======================================================================
  # 8. Identify Movers (selection engine)
  # ======================================================================
  if (is.null(demand_dt) || nrow(demand_dt) == 0 || sum(demand_dt$n_movers) == 0) {
    empty_movers <- data.table::data.table(
      personnel_id  = character(0),
      from_group    = character(0),
      to_group      = character(0),
      movement_type = character(0)
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
      baseline_matrix = baseline_matrix,
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
  # 9. Update State (ONLY state-modifying step)
  # ======================================================================
  update_result <- update_state_with_movement(
    contract_dt       = snap_contract_dt,
    personnel_dt      = snap_personnel_dt,
    movers_dt         = movers_dt,
    policy_params     = policy_params,
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
    baseline_matrix = baseline_matrix,
    demand_dt       = demand_dt
  ))
}
