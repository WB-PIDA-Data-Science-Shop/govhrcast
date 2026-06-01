#' Generate a Scenario Matrix Across a Grid of Policy Parameters
#'
#' @description
#' High-performance batch wrapper that runs \code{\link{simulate_horizon}} across
#' every combination of policy levers supplied in \code{param_grid}.  Each
#' combination is a scenario; results are stacked into a single long-flat
#' \code{data.table} optimised for real-time filtering and plotting in Shiny.
#'
#' @details
#' **Grid mapping** \cr
#' \code{param_grid} is a named list where each element is a vector of values to
#' try for that lever.  All combinations are expanded with
#' \code{data.table::CJ()}.  Lever names are mapped to their target parameter
#' using the following rules (checked in order):
#' \enumerate{
#'   \item Exact match to an argument of \code{simulate_horizon()} (e.g.
#'     \code{n_periods}, \code{salary_growth_rate}).
#'   \item Prefix \code{"retirement_"} → bare key injected into
#'     \code{retirement_policy$defaults} if the key exists there, otherwise
#'     into the top level of \code{retirement_policy}
#'     (e.g. \code{retirement_min_age} →
#'     \code{retirement_policy$defaults$min_age}).
#'   \item Prefix \code{"exit_"} → injected into \code{exit_policy} by the
#'     same depth rule (e.g. \code{exit_rate} →
#'     \code{exit_policy$defaults$exit_rate}).
#'   \item Prefix \code{"movement_"} → injected into \code{movement_policy}
#'     by the same depth rule.
#'   \item Prefix \code{"hiring_"} → injected into \code{hiring_policy} by
#'     the same depth rule.
#' }
#' Scalar levers (\code{min_age}, \code{exit_rate}, \code{movement_rate},
#' \code{replacement_rate}, etc.) live under \code{$defaults} in the canonical
#' 3-slot policy structure and are injected there automatically.  Structural
#' keys (\code{group_cols}, \code{policy_table}, \code{mode}) live at the top
#' level and are injected there.  The base policy objects passed to
#' \code{generate_scenario_matrix()} may already contain a \code{policy_table}
#' defining group-level parameter variation; the grid then varies scalar
#' defaults on top of that fixed group structure.
#'
#' **Execution** \cr
#' The first scenario is always run as a synchronous smoke test before launching
#' the full batch.  If \code{future.apply} is installed and a
#' \code{\link[future:plan]{future::plan}} has been set (anything other than
#' \code{sequential}), the remaining scenarios run in parallel; otherwise they
#' fall back to a sequential \code{lapply}.  Each worker receives
#' \emph{deep copies} of the base data and calls
#' \code{data.table::setDTthreads(1)} to avoid thread collisions.
#'
#' **Baseline flag** \cr
#' A scenario is flagged \code{is_baseline = TRUE} when all numeric multipliers
#' in \code{param_grid} are at their default (1.0) value and all growth rates are
#' at 0, as determined by the first scenario in the grid (unless overridden by
#' \code{baseline_scenario_id}).
#'
#' @import data.table
#'
#' @param contract_dt data.table.  Initial contract microdata (single snapshot).
#' @param personnel_dt data.table.  Initial personnel microdata (single snapshot).
#' @param salary_scale_dt data.table.  Base pay table.  Copied per scenario.
#' @param param_grid Named list.  Each element is a vector of values for one
#'   policy lever.  Must contain at least one element.  See Details for naming
#'   conventions.
#' @param n_periods Integer.  Number of annual periods per scenario.  Can also
#'   be included in \code{param_grid} to vary it across scenarios.
#' @param retirement_policy List or \code{NULL}.  Base canonical 3-slot
#'   retirement policy (\code{group_cols}, \code{policy_table}, \code{defaults}).
#'   Per-scenario scalar overrides are injected from \code{param_grid} entries
#'   with prefix \code{"retirement_"} into \code{$defaults}.
#'   Pass \code{NULL} to skip retirement in all scenarios.
#' @param exit_policy List or \code{NULL}.  Base canonical 3-slot exit policy.
#'   Per-scenario scalar overrides from \code{param_grid} entries with prefix
#'   \code{"exit_"} are injected into \code{$defaults}.
#'   Pass \code{NULL} to skip non-retirement attrition.
#' @param movement_policy List or \code{NULL}.  Base canonical 3-slot movement
#'   policy.  Per-scenario scalar overrides from \code{param_grid} entries with
#'   prefix \code{"movement_"} are injected into \code{$defaults}.
#'   Pass \code{NULL} to skip.
#' @param hiring_policy List or \code{NULL}.  Base hiring policy.  Per-scenario
#'   scalar overrides from \code{param_grid} entries with prefix \code{"hiring_"}
#'   are injected (into \code{$defaults} if present, else top-level).
#'   Pass \code{NULL} to skip.
#' @param salary_growth_rate Numeric scalar.  Base COLA rate.  Overridden per
#'   scenario if \code{"salary_growth_rate"} appears in \code{param_grid}.
#' @param base_year Integer.  Calendar year label for period 1.
#' @param ref_date Date.  Simulation anchor date.
#' @param baseline_scenario_id Integer or \code{NULL}.  Row index of the grid
#'   combination to flag as the baseline.  If \code{NULL} (default), the first
#'   scenario (\code{scenario_id = 1}) is used.
#' @param personnel_id_col Character.  Default \code{"personnel_id"}.
#' @param contract_id_col Character.  Default \code{"contract_id"}.
#' @param start_date_col Character.  Default \code{"start_date"}.
#' @param end_date_col Character.  Default \code{"end_date"}.
#' @param salary_col Character.  Default \code{"gross_salary_lcu"}.
#' @param contract_type_col Character.  Default \code{"contract_type_code"}.
#' @param status_col Character.  Default \code{"status"}.
#' @param age_col Character or \code{NULL}.  Name of the age column in
#'   \code{personnel_dt}.  When \code{NULL} (default), the value
#'   \code{"age"} is used and the column is (re-)computed from
#'   \code{birth_date_col} inside \code{simulate_horizon()} if available.
#' @param tenure_col Character.  Default \code{"tenure_years"}.
#'
#' @return \code{data.table} with one row per \strong{scenario × period},
#'   keyed by \code{(scenario_id, year)}:
#'   \describe{
#'     \item{scenario_id}{Integer.  Unique scenario identifier (row index of
#'       expanded grid).}
#'     \item{scenario_label}{Character.  Human-readable description of the
#'       lever values for this scenario.}
#'     \item{is_baseline}{Logical.  \code{TRUE} for the designated baseline.}
#'     \item{\emph{<lever_name>}}{One column per lever in \code{param_grid},
#'       recording the exact value used in that scenario.}
#'     \item{year}{Integer period label (\code{base_year + t - 1}).}
#'     \item{n_headcount_start, n_headcount_end}{Headcount at start/end of period.}
#'     \item{wage_bill_start, wage_bill_end}{Total payroll at start/end of period
#'       (\code{sum(salary_col)} over all contract rows, no filtering).}
#'     \item{n_exits, exit_savings}{Retirement count and salary mass removed.}
#'     \item{pension_cost_new, pension_cost_total}{New-period and cumulative
#'       pension commitments.}
#'     \item{n_promotions, n_transfers, promotion_effect, transfer_effect}{
#'       Movement counts and net salary effects.}
#'     \item{n_hires, hiring_effect}{New hire count and total new-hire salary.}
#'     \item{inflation_effect}{Payroll increment from COLA.}
#'     \item{exit_savings_pct_of_end_bill, promotion_effect_pct_of_end_bill,
#'       transfer_effect_pct_of_end_bill, hiring_effect_pct_of_end_bill,
#'       inflation_effect_pct_of_end_bill}{Each effect as a share of
#'       \code{wage_bill_end}.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#'
#' ct <- bra_hrmis_contract[ref_date == as.Date("2016-09-01")]
#' pt <- bra_hrmis_personnel[ref_date == as.Date("2016-09-01")]
#' ss <- data.table(est_id = unique(ct$est_id), gross_salary_lcu = 5000)
#'
#' # Grid: vary salary growth, retirement age, and exit rate across scenarios
#' grid <- list(
#'   salary_growth_rate = c(0.02, 0.05, 0.10),
#'   retirement_min_age = c(55, 60),       # → retirement_policy$defaults$min_age
#'   exit_exit_rate     = c(0.03, 0.05)   # → exit_policy$defaults$exit_rate
#' )
#'
#' results <- generate_scenario_matrix(
#'   contract_dt      = ct,
#'   personnel_dt     = pt,
#'   salary_scale_dt  = ss,
#'   param_grid       = grid,
#'   n_periods        = 5L,
#'   retirement_policy = list(
#'     group_cols   = NULL,
#'     policy_table = NULL,
#'     defaults = list(
#'       eligibility_type = "age_only",
#'       pension_type     = "flat",
#'       min_age          = 60,
#'       flat_amount      = 1000
#'     )
#'   ),
#'   exit_policy = list(
#'     group_cols   = NULL,
#'     policy_table = NULL,
#'     defaults = list(
#'       exit_rate     = 0.04,
#'       exit_strategy = "random",
#'       active_types  = c("permanent", "fterm"),
#'       exited_type   = "inactive"
#'     )
#'   ),
#'   movement_policy = list(
#'     group_cols   = "est_id",
#'     policy_table = NULL,
#'     defaults = list(
#'       movement_rate      = 0.05,
#'       movement_strategy  = "tenure",
#'       active_types       = "permanent",
#'       salary_update_rule = "scale"
#'     )
#'   ),
#'   hiring_policy = list(
#'     mode             = "flow",
#'     group_cols       = "est_id",
#'     replacement_rate = 1.0,
#'     salary_scale     = ss
#'   ),
#'   salary_growth_rate = 0.03,
#'   ref_date           = as.Date("2016-09-01")
#' )
#'
#' # Side-by-side wage bill trajectory
#' # ggplot(results[scenario_id %in% c(1, 5)],
#' #   aes(x = year, y = wage_bill_end, color = scenario_label)) +
#' #   geom_line()
#' }
#'
#' @export
generate_scenario_matrix <- function(contract_dt,
                                     personnel_dt,
                                     salary_scale_dt,
                                     param_grid,
                                     n_periods          = 5L,
                                     retirement_policy  = NULL,
                                     exit_policy        = NULL,
                                     movement_policy    = NULL,
                                     hiring_policy      = NULL,
                                     salary_growth_rate = 0,
                                     base_year          = as.integer(format(Sys.Date(), "%Y")),
                                     ref_date           = Sys.Date(),
                                     baseline_scenario_id = NULL,
                                     personnel_id_col   = "personnel_id",
                                     contract_id_col    = "contract_id",
                                     start_date_col     = "start_date",
                                     end_date_col       = "end_date",
                                     salary_col         = "gross_salary_lcu",
                                     contract_type_col  = "contract_type_code",
                                     status_col         = "status",
                                     age_col            = NULL,
                                     tenure_col         = NULL) {

  # ====================================================================
  # 0. Input validation
  # ====================================================================
  if (!is.list(param_grid) || length(param_grid) == 0L) {
    stop("param_grid must be a non-empty named list.", call. = FALSE)
  }
  lever_names <- names(param_grid)
  if (is.null(lever_names) || any(lever_names == "")) {
    stop("All elements of param_grid must be named.", call. = FALSE)
  }

  n_periods <- as.integer(n_periods)
  if (is.na(n_periods) || n_periods < 1L) {
    stop("n_periods must be a positive integer.", call. = FALSE)
  }

  ref_date <- validate_date_format(ref_date, "ref_date")

  # ====================================================================
  # 1. Expand the grid
  # ====================================================================
  grid_dt <- do.call(data.table::CJ, c(param_grid, list(sorted = FALSE)))
  n_scenarios <- nrow(grid_dt)

  # ====================================================================
  # 2. Build scenario labels and IDs
  # ====================================================================
  label_parts <- lapply(seq_len(n_scenarios), function(i) {
    row <- grid_dt[i]
    parts <- vapply(lever_names, function(nm) {
      val <- row[[nm]]
      # Shorten lever name: strip module prefix for readability
      short <- gsub("^(retirement|movement|hiring)_", "", nm)
      short <- gsub("_", " ", short)
      paste0(short, ": ", val)
    }, character(1L))
    paste(parts, collapse = " | ")
  })
  grid_dt[, scenario_id    := seq_len(n_scenarios)]
  grid_dt[, scenario_label := unlist(label_parts)]

  # Baseline flag
  baseline_id <- if (!is.null(baseline_scenario_id)) {
    as.integer(baseline_scenario_id)
  } else {
    1L
  }
  grid_dt[, is_baseline := scenario_id == baseline_id]

  # ====================================================================
  # 3. Build per-scenario simulate_horizon argument constructor
  # ====================================================================
  # Known orchestrator-level scalar levers (subset of simulate_horizon formals)
  horizon_scalars <- c("n_periods", "salary_growth_rate", "base_year")

  # Inject a lever value into a policy list at the correct depth:
  #   - if the key exists in pol$defaults → inject there (scalar default lever)
  #   - otherwise → inject at the top level (structural key: group_cols, mode…)
  .inject <- function(pol, key, val) {
    if (!is.null(pol$defaults) && key %in% names(pol$defaults)) {
      pol$defaults[[key]] <- val
    } else {
      pol[[key]] <- val
    }
    pol
  }

  .build_args <- function(scenario_row) {
    # Start from base values
    ret_pol  <- retirement_policy
    exit_pol <- exit_policy
    mov_pol  <- movement_policy
    hire_pol <- hiring_policy
    n_per    <- n_periods
    sgr      <- salary_growth_rate
    by       <- base_year

    for (nm in lever_names) {
      val <- scenario_row[[nm]]

      if (nm == "n_periods") {
        n_per <- as.integer(val)
      } else if (nm == "salary_growth_rate") {
        sgr <- as.numeric(val)
      } else if (nm == "base_year") {
        by <- as.integer(val)
      } else if (startsWith(nm, "retirement_")) {
        key <- sub("^retirement_", "", nm)
        if (!is.null(ret_pol))  ret_pol  <- .inject(ret_pol,  key, val)
      } else if (startsWith(nm, "exit_")) {
        key <- sub("^exit_", "", nm)
        if (!is.null(exit_pol)) exit_pol <- .inject(exit_pol, key, val)
      } else if (startsWith(nm, "movement_")) {
        key <- sub("^movement_", "", nm)
        if (!is.null(mov_pol))  mov_pol  <- .inject(mov_pol,  key, val)
      } else if (startsWith(nm, "hiring_")) {
        key <- sub("^hiring_", "", nm)
        if (!is.null(hire_pol)) hire_pol <- .inject(hire_pol, key, val)
      }
      # Unknown lever names are silently ignored (forward-compatible)
    }

    list(
      n_periods          = n_per,
      salary_growth_rate = sgr,
      base_year          = by,
      retirement_policy  = ret_pol,
      exit_policy        = exit_pol,
      movement_policy    = mov_pol,
      hiring_policy      = hire_pol
    )
  }

  # ====================================================================
  # 4. Worker function — runs one scenario, returns summary_dt with metadata
  # ====================================================================
  .run_one <- function(i) {
    data.table::setDTthreads(1L)   # prevent thread collisions in workers

    row <- grid_dt[i]
    args <- .build_args(row)

    result <- tryCatch(
      simulate_horizon(
        contract_dt        = data.table::copy(contract_dt),
        personnel_dt       = data.table::copy(personnel_dt),
        salary_scale_dt    = data.table::copy(salary_scale_dt),
        n_periods          = args$n_periods,
        retirement_policy  = args$retirement_policy,
        exit_policy        = args$exit_policy,
        movement_policy    = args$movement_policy,
        hiring_policy      = args$hiring_policy,
        salary_growth_rate = args$salary_growth_rate,
        base_year          = args$base_year,
        return_microdata   = FALSE,       # never keep microdata in batch
        ref_date           = ref_date,
        personnel_id_col   = personnel_id_col,
        contract_id_col    = contract_id_col,
        start_date_col     = start_date_col,
        end_date_col       = end_date_col,
        salary_col         = salary_col,
        contract_type_col  = contract_type_col,
        status_col         = status_col,
        age_col            = age_col,
        tenure_col         = tenure_col
      ),
      error = function(e) {
        warning("Scenario ", i, " failed: ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )

    if (is.null(result)) return(NULL)

    # Attach metadata columns from the grid row
    out <- result$summary_dt
    meta_cols <- c("scenario_id", "scenario_label", "is_baseline", lever_names)
    for (col in meta_cols) {
      out[, (col) := row[[col]]]
    }

    # Reorder: metadata first, then time-series
    all_cols   <- names(out)
    ts_cols    <- setdiff(all_cols, meta_cols)
    data.table::setcolorder(out, c(meta_cols, ts_cols))
    out
  }

  # ====================================================================
  # 5. Smoke test: run scenario 1 synchronously
  # ====================================================================
  message("Running smoke test (scenario 1 of ", n_scenarios, ")...")
  smoke <- .run_one(1L)
  if (is.null(smoke)) {
    stop("Smoke test failed on scenario 1. Aborting batch.", call. = FALSE)
  }
  message("Smoke test passed.")

  if (n_scenarios == 1L) {
    data.table::setkeyv(smoke, c("scenario_id", "period_date"))
    attr(smoke, "param_grid") <- param_grid
    return(smoke)
  }

  # ====================================================================
  # 6. Batch execution: parallel if future.apply available + plan set,
  #    otherwise sequential lapply
  # ====================================================================
  remaining <- seq(2L, n_scenarios)

  is_dev <- requireNamespace("pkgload", quietly = TRUE) &&
    pkgload::is_dev_package("govhrcast")

  use_parallel <- !is_dev &&
    requireNamespace("future.apply", quietly = TRUE) &&
    !inherits(future::plan(), "SequentialFuture")

  if (use_parallel) {
    message("Running ", length(remaining), " remaining scenarios in parallel...")
    rest_results <- future.apply::future_lapply(
      remaining,
      .run_one,
      future.seed = TRUE
    )
  } else {
    message("Running ", length(remaining), " remaining scenarios sequentially...")
    rest_results <- lapply(remaining, .run_one)
  }

  # ====================================================================
  # 7. Combine all results
  # ====================================================================
  all_results <- c(list(smoke), rest_results)
  all_results <- all_results[!vapply(all_results, is.null, logical(1L))]

  n_failed <- n_scenarios - length(all_results)
  if (n_failed > 0L) {
    warning(n_failed, " scenario(s) failed and were excluded from results.",
            call. = FALSE)
  }

  out_dt <- data.table::rbindlist(all_results, use.names = TRUE, fill = TRUE)
  data.table::setkeyv(out_dt, c("scenario_id", "period_date"))
  attr(out_dt, "param_grid") <- param_grid
  out_dt
}
