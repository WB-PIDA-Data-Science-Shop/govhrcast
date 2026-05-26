#' Simulate Retirement Events for One Period
#'
#' @description
#' Core module for simulating public sector retirement in a single period.
#' Orchestrates a five-stage logic gate pipeline:
#' \enumerate{
#'   \item \strong{Input validation} — structural checks on \code{contract_dt},
#'     \code{personnel_dt}, and \code{policy_params} via
#'     \code{\link{check_retirement_inputs}}.
#'   \item \strong{Panel snapshot selection} — if \code{contract_dt} is a
#'     longitudinal panel, the snapshot closest to (but not exceeding)
#'     \code{ref_date} is selected via \code{\link{select_nearest_ref_date}}.
#'   \item \strong{Eligibility gate} — \code{\link{identify_eligibility}} applies
#'     vectorised per-row eligibility rules (age, tenure, or both), resolved
#'     through \code{\link{resolve_policy_table}} to handle group-level
#'     policy variation.
#'   \item \strong{Pension calculation} — pension parameters are resolved to
#'     per-row columns via a single \code{\link{resolve_policy_table}} call,
#'     then \code{\link{compute_pension}} dispatches to the appropriate formula
#'     (DB, DC, flat, or hybrid) per retiree.
#'   \item \strong{State update} — \code{\link{update_contracts_for_retirees}}
#'     and \code{\link{update_personnel_for_retirees}} mutate the working copies
#'     of \code{contract_dt} and \code{personnel_dt} in place.
#' }
#'
#' @import data.table
#'
#' @param contract_dt data.table. Active workforce contracts in
#'   \pkg{govhr}-harmonised format.  Required columns: \code{personnel_id_col},
#'   \code{contract_id_col}, \code{start_date_col}, \code{end_date_col},
#'   \code{contract_type_col}, \code{salary_col}.  May be a multi-year panel
#'   with a \code{ref_date_col} snapshot column — the nearest snapshot to
#'   \code{ref_date} is selected automatically.
#' @param personnel_dt data.table. Personnel register in \pkg{govhr}-harmonised
#'   format.  Required columns: \code{personnel_id_col}, \code{birth_date_col}.
#' @param policy_params List. Retirement policy specification in the unified
#'   three-slot format:
#'   \describe{
#'     \item{\code{defaults}}{Named list of scalar fallback values applied to
#'       every employee not matched by \code{policy_table}.  Recognised keys:
#'       \describe{
#'         \item{\code{eligibility_type}}{Character scalar. Eligibility rule:
#'           \code{"age_only"}, \code{"tenure_only"}, or
#'           \code{"age_and_tenure"}.}
#'         \item{\code{min_age}}{Numeric scalar. Minimum age threshold in years
#'           (required when \code{eligibility_type} involves age).}
#'         \item{\code{min_tenure}}{Numeric scalar. Minimum years of service
#'           (required when \code{eligibility_type} involves tenure).}
#'         \item{\code{pension_type}}{Character scalar. Pension formula:
#'           \code{"db"} (defined-benefit), \code{"dc"} (defined-contribution),
#'           \code{"flat"}, or \code{"hybrid"} (DB + DC).}
#'         \item{\code{accrual_rate}}{Numeric scalar. Annual DB accrual rate
#'           (e.g. \code{0.02} = 2\% of final salary per year of service).}
#'         \item{\code{ref_wage_col}}{Character scalar. Column pointer — names
#'           the salary column in \code{contract_dt} to use as the DB wage base
#'           (default: \code{"gross_salary_lcu"}).}
#'         \item{\code{max_years}}{Numeric scalar. Service cap for DB accrual.
#'           \code{NA} = no cap.}
#'         \item{\code{replacement_cap}}{Numeric scalar. Maximum pension as a
#'           fraction of final salary (e.g. \code{0.80} = 80\%).  \code{NA} =
#'           no cap.}
#'         \item{\code{balance_col}}{Character scalar. Column pointer — names
#'           the DC account balance column in \code{contract_dt}.}
#'         \item{\code{annuity_factor}}{Numeric scalar. DC annuity divisor
#'           (e.g. \code{15} = 15-year payout horizon).}
#'         \item{\code{flat_amount}}{Numeric scalar. Fixed periodic pension for
#'           flat-rate systems.}
#'       }
#'     }
#'     \item{\code{group_cols}}{Character vector. Columns in \code{contract_dt}
#'       used as the join key for group-level policy variation (e.g.
#'       \code{c("paygrade", "occupation_isconame")}).  \code{NULL} for
#'       scalar-only dispatch.}
#'     \item{\code{policy_table}}{data.table. Lookup table with \code{group_cols}
#'       plus any per-group parameter columns that override \code{defaults}.
#'       \code{NULL} when group-level variation is not required.}
#'   }
#'   When omitted, a standard DB \code{age_and_tenure} policy is used
#'   (\code{min_age = 60}, \code{min_tenure = 10}, \code{accrual_rate = 0.02},
#'   \code{max_years = 35}, \code{replacement_cap = 0.80}).
#' @param ref_date Date. The simulation period date.  Used as the reference for
#'   age and tenure computation and as the closing date stamped on retired
#'   contracts.  Note: if \code{contract_dt} is a panel, panel snapshot
#'   selection uses \code{ref_date} as an upper bound, but age and tenure are
#'   always computed against this exact date.
#' @param retirement_hazard_model A calibrated \code{hazard_model} object
#'   returned by \code{\link{fit_hazard_model}} and
#'   \code{\link{select_hazard_threshold}}, or \code{NULL} (default).
#'   When supplied, \code{\link{predict_hazard}} is called on the
#'   current-period snapshot and its \code{event = 1} predictions are
#'   intersected with the eligibility pool before any state updates occur.
#'   Ineligible persons predicted by the model are silently dropped.
#'   When \code{NULL}, all eligible persons retire (100\% take-up).
#' @param ref_date_col Character. Column in \code{contract_dt} (and optionally
#'   \code{personnel_dt}) identifying the panel snapshot date.
#'   (default: \code{"ref_date"}).  Ignored if \code{contract_dt} is a
#'   single cross-section.
#' @param personnel_id_col Character. Unique personnel identifier present in
#'   both \code{contract_dt} and \code{personnel_dt}.
#'   (default: \code{"personnel_id"}).
#' @param birth_date_col Character. Date column in \code{personnel_dt}
#'   containing each person's date of birth.  Used by \code{\link{compute_age}}
#'   when a pre-computed age column is absent.
#'   (default: \code{"birth_date"}).
#' @param contract_id_col Character. Unique contract identifier in
#'   \code{contract_dt}.  Used to deduplicate panel snapshots during tenure
#'   computation.  (default: \code{"contract_id"}).
#' @param start_date_col Character. Contract start date column in
#'   \code{contract_dt}.  (default: \code{"start_date"}).
#' @param end_date_col Character. Contract end date column in \code{contract_dt}.
#'   Open-ended contracts have \code{NA} here.
#'   (default: \code{"end_date"}).
#' @param salary_col Character. Gross salary column in \code{contract_dt}.
#'   Used as the pension wage base when \code{ref_wage_col} is not overridden
#'   in \code{policy_params}.  (default: \code{"gross_salary_lcu"}).
#' @param contract_type_col Character. Contract classification column in
#'   \code{contract_dt}.  Active contracts are any type not in
#'   \code{c("inactive", "pensioner")}.  Retired contracts are reclassified
#'   to \code{"pensioner"} by \code{\link{update_contracts_for_retirees}}.
#'   (default: \code{"contract_type_code"}).
#' @param status_col Character. Employment status column in
#'   \code{personnel_dt}.  Set to \code{"inactive"} for retiring personnel.
#'   (default: \code{"status"}).
#' @param age_col Character. Column in \code{personnel_dt} containing
#'   pre-computed age in years.  When present, \code{\link{compute_age}} is
#'   skipped for efficiency.  (default: \code{"age"}).
#' @param tenure_col Character. Column in \code{personnel_dt} containing
#'   pre-computed tenure in years.  When present,
#'   \code{\link{compute_tenure}} is skipped for efficiency.
#'   (default: \code{"tenure_years"}).
#'
#' @details
#' \strong{Order of operations:}
#' \enumerate{
#'   \item \code{\link{check_retirement_inputs}} — validates all inputs before
#'     any computation.
#'   \item Deep copies of \code{contract_dt} and \code{personnel_dt} are made
#'     via \code{data.table::copy()} to protect the caller's objects.
#'   \item If \code{ref_date_col} exists in \code{contract_dt}, the panel is
#'     filtered to the nearest snapshot using
#'     \code{\link{select_nearest_ref_date}}.
#'   \item \code{\link{identify_eligibility}} builds a per-person eligibility
#'     table.  Group-level thresholds are resolved via a single
#'     \code{\link{resolve_policy_table}} join.
#'   \item \code{\link{prepare_retiree_data}} enriches the eligible pool with
#'     salary, fills any \code{NA} age/tenure values, and selects the primary
#'     contract per retiree.
#'   \item All pension parameters are stamped onto \code{retirees_dt} as columns
#'     using \code{data.table::set()} (avoids shallow-copy warnings from
#'     \code{:=} inside a loop).  \code{\link{compute_pension}} then dispatches
#'     each row to the correct formula.
#'   \item \code{\link{update_contracts_for_retirees}} closes active contracts
#'     and zeros salaries in place.  \code{\link{update_personnel_for_retirees}}
#'     sets \code{status_col} to \code{"inactive"}.
#' }
#'
#' @section Retirement eligibility vs. retirement choice:
#' When \code{retirement_hazard_model} is \code{NULL} (default), this function
#' equates eligibility with retirement (100\% take-up): every person who meets
#' the policy thresholds retires.
#'
#' When a calibrated \code{\link{hazard_model}} object is supplied via
#' \code{retirement_hazard_model}, \code{\link{predict_hazard}} is called on
#' the current-period snapshot to obtain a probabilistic retirement prediction
#' for each active person.  Only persons for whom \code{event = 1} \emph{and}
#' who are policy-eligible are retired.  The eligibility gate is always
#' enforced as a post-filter on the hazard predictions: model-predicted
#' retirements for ineligible persons are silently dropped.
#'
#' @section Data Integrity:
#' \code{contract_dt} and \code{personnel_dt} are deep-copied at entry
#' (\code{data.table::copy()}).  The caller's original objects are never
#' modified.  The returned \code{contract_dt} and \code{personnel_dt} are the
#' mutated copies.
#'
#' @return Named list with four elements:
#'   \describe{
#'     \item{\code{summary}}{\code{data.table} (1 row) with aggregate retirement
#'       statistics for the period: \code{n_retired} (integer),
#'       \code{total_pension} (numeric), \code{avg_pension} (numeric),
#'       \code{avg_age} (numeric), \code{avg_tenure} (numeric).}
#'     \item{\code{contract_dt}}{\code{data.table}.  Updated contract register.
#'       Retiring contracts have \code{contract_type_col} set to
#'       \code{"pensioner"}, \code{end_date_col} set to \code{ref_date}, and
#'       \code{salary_col} set to \code{0}.}
#'     \item{\code{personnel_dt}}{\code{data.table}.  Updated personnel register.
#'       Retiring personnel have \code{status_col} set to \code{"inactive"}.}
#'     \item{\code{retirees_dt}}{\code{data.table}.  One row per retiree with
#'       all resolved pension parameter columns (\code{pension_type},
#'       \code{accrual_rate}, \code{ref_wage_col}, etc.) plus a \code{pension}
#'       column containing the computed periodic pension amount.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(govhrcast)
#'
#' # Load example data
#' contract_dt  <- data.table::copy(bra_hrmis_contract)
#' personnel_dt <- data.table::copy(bra_hrmis_personnel)
#' ref_date     <- as.Date("2014-01-01")
#'
#' # Scalar policy (no group differentiation)
#' policy_params <- list(
#'   group_cols   = NULL,
#'   policy_table = NULL,
#'   defaults = list(
#'     eligibility_type = "age_and_tenure",
#'     pension_type     = "db",
#'     min_age          = 60,
#'     min_tenure       = 20,
#'     accrual_rate     = 0.02,
#'     ref_wage_col     = "gross_salary_lcu",
#'     max_years        = 35,
#'     replacement_cap  = 0.80
#'   )
#' )
#'
#' # Group-level policy: different accrual_rate per paygrade
#' accrual_tbl <- data.table::data.table(
#'   paygrade     = c("D",   "E"),
#'   accrual_rate = c(0.025, 0.03)
#' )
#' policy_grouped <- list(
#'   group_cols   = "paygrade",
#'   policy_table = accrual_tbl,
#'   defaults = list(
#'     eligibility_type = "age_and_tenure",
#'     pension_type     = "db",
#'     min_age          = 60,
#'     min_tenure       = 20,
#'     accrual_rate     = 0.02,
#'     ref_wage_col     = "gross_salary_lcu",
#'     max_years        = 35,
#'     replacement_cap  = 0.80
#'   )
#' )
#'
#' # Run simulation
#' results <- simulate_retirement(
#'   contract_dt  = contract_dt,
#'   personnel_dt = personnel_dt,
#'   ref_date     = ref_date,
#'   policy_params = policy_params
#' )
#'
#' results$summary
#' results$retirees_dt
#' }
#'
#' @export
simulate_retirement <- function(contract_dt,
                                personnel_dt,
                                ref_date,
                                policy_params = list(
                                  group_cols   = NULL,
                                  policy_table = NULL,
                                  defaults = list(
                                    eligibility_type = "age_and_tenure",
                                    pension_type     = "db",
                                    min_age          = 60,
                                    min_tenure       = 10,
                                    accrual_rate     = 0.02,
                                    ref_wage_col     = "gross_salary_lcu",
                                    max_years        = 35,
                                    replacement_cap  = 0.80
                                  )
                                ),
                                retirement_hazard_model = NULL,
                                ref_date_col = "ref_date",
                                personnel_id_col = "personnel_id",
                                birth_date_col = "birth_date",
                                contract_id_col = "contract_id",
                                start_date_col = "start_date",
                                end_date_col = "end_date",
                                salary_col = "gross_salary_lcu",
                                contract_type_col = "contract_type_code",
                                status_col = "status",
                                age_col    = "age",
                                tenure_col = "tenure_years") {
  
  # ========================================
  # 1. Input Validation
  check_retirement_inputs(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date,
    personnel_id_col = personnel_id_col,
    birth_date_col = birth_date_col,
    contract_id_col = contract_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col,
    status_col = status_col
  )
  
  # Convert to data.table and create working copies
  # We copy here to avoid modifying the user's input data
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
  
  # ========================================
  # 2. Select Nearest Reference Date
  # ========================================
  # Find the reference date in the data closest to (but not after) the specified ref_date
  if (ref_date_col %in% names(contract_dt)) {
    selected_ref_date <- select_nearest_ref_date(contract_dt[[ref_date_col]], ref_date)
    
    # Subset both datasets to the selected reference date
    contract_dt <- contract_dt[get(ref_date_col) == selected_ref_date]
    if (ref_date_col %in% names(personnel_dt)) {
      personnel_dt <- personnel_dt[get(ref_date_col) == selected_ref_date]
    }
  }
  
  # ========================================
  # 3. Identify eligible retirees
  # ========================================
  eligibility_dt <- identify_eligibility(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    policy_params = policy_params,
    ref_date = ref_date,  # Use user's ref_date for age/tenure calculation
    personnel_id_col = personnel_id_col,
    birth_date_col = birth_date_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col,
    age_col    = age_col,
    tenure_col = tenure_col
  )
  
  # ========================================
  # 3b. Hazard filter (optional)
  # ========================================
  # When a calibrated hazard model is supplied, predict event probabilities for
  # all active persons this period and retain only model-predicted retirees
  # (event = 1) who are also policy-eligible.  Ineligible predictions are
  # silently dropped — the eligibility gate is always the binding constraint.
  if (!is.null(retirement_hazard_model)) {
    # If `eligible` is a covariate in the fitted model, inject it onto
    # personnel_dt so predict_hazard() can use it during scoring.
    # eligible = 1 for persons identified as policy-eligible this period;
    # 0 for everyone else (including non-active staff excluded by eligibility_dt).
    .model_covs_ <- all.vars(stats::formula(retirement_hazard_model$model))[-1L]
    if ("eligible" %in% .model_covs_) {
      .elig_flag_ <- eligibility_dt[,
        c(personnel_id_col, "retire"), with = FALSE
      ]
      data.table::setnames(.elig_flag_, "retire", "eligible")
      personnel_dt[, eligible := 0L]
      personnel_dt[.elig_flag_[eligible == 1L],
                   eligible := 1L,
                   on = personnel_id_col]
    }

    hazard_preds <- predict_hazard(
      hazard_model      = retirement_hazard_model,
      contract_dt       = contract_dt,
      personnel_dt      = personnel_dt,
      personnel_id_col  = personnel_id_col,
      birth_date_col    = birth_date_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      contract_type_col = contract_type_col,
      ref_date          = ref_date
    )

    # Clean up temporary column
    if ("eligible" %in% .model_covs_ && "eligible" %in% names(personnel_dt))
      personnel_dt[, eligible := NULL]

    # Persons the model says will retire this period
    hazard_retirers <- hazard_preds[event == 1L, get(personnel_id_col)]
    # Intersect with eligible pool — eligibility gate is always the binding constraint
    eligibility_dt <- eligibility_dt[
      get(personnel_id_col) %in% hazard_retirers
    ]
  }

  # ========================================
  # 4. Prepare Retiree Data
  # ========================================
  retirees_dt <- prepare_retiree_data(
    eligibility_dt    = eligibility_dt,
    contract_dt       = contract_dt,
    personnel_dt      = personnel_dt,
    ref_date          = ref_date,  # Use user's ref_date
    personnel_id_col  = personnel_id_col,
    birth_date_col    = birth_date_col,
    contract_id_col   = contract_id_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    salary_col        = salary_col,
    contract_type_col = contract_type_col
  )
  
  # Handle case of no retirees
  if (nrow(retirees_dt) == 0) {
    summary_tbl <- data.table::data.table(
      n_retired = 0L,
      total_pension = 0,
      avg_pension = NA_real_,
      avg_age = NA_real_,
      avg_tenure = NA_real_
    )
    
    return(list(
      summary = summary_tbl,
      contract_dt = contract_dt,
      personnel_dt = personnel_dt,
      retirees_dt = data.table::data.table()
    ))
  }
  
  # ========================================
  # 5. Compute Pensions
  # ========================================
  # Resolve all policy params (eligibility + pension) to per-row columns on
  # retirees_dt via a single policy_table join + defaults fill.
  .all_pension_params <- c(
    "pension_type", "accrual_rate", "ref_wage_col",
    "max_years", "replacement_cap",
    "balance_col", "annuity_factor", "notional_rate",
    "flat_amount"
  )
  .resolved_pension <- resolve_policy_table(
    policy_params,
    retirees_dt,
    .all_pension_params
  )
  for (.p in names(.resolved_pension))
    data.table::set(retirees_dt, j = .p, value = .resolved_pension[[.p]])

  data.table::set(retirees_dt, j = "pension", value = compute_pension(retirees_dt))
  
  # ========================================
  # 6. Update State
  # ========================================
  
  # Update contracts (modifies contract_dt in place)
  update_contracts_for_retirees(
    contract_dt = contract_dt,
    retirees_dt = retirees_dt,
    ref_date = ref_date,  # Use user's ref_date
    personnel_id_col = personnel_id_col,
    contract_id_col = contract_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    salary_col = salary_col,
    contract_type_col = contract_type_col
  )
  
  # Update personnel (modifies personnel_dt in place)
  update_personnel_for_retirees(
    personnel_dt = personnel_dt,
    contract_dt = contract_dt,
    personnel_id_col = personnel_id_col,
    contract_type_col = contract_type_col,
    status_col = status_col
  )
  
  # ========================================
  # 7. Compute Summary Statistics
  # ========================================
  summary_tbl <- compute_retirement_summary(
    retirees_dt = retirees_dt,
    contract_dt = contract_dt
  )
  
  # ========================================
  # 8. Return Results
  # ========================================
  return(list(
    summary = summary_tbl,
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    retirees_dt = retirees_dt
  ))
}

