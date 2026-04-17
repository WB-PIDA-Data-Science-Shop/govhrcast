#' Core Retirement Logic Functions
#'
#' @description
#' Functions for identifying retirement-eligible personnel and computing
#' retirement eligibility based on age and/or tenure.
#'
#' @import data.table
#' @name retirement_core
#' @keywords internal
NULL

# Suppress R CMD check NOTEs for data.table column names used in identify_retirees().
utils::globalVariables(c(
  "eligibility_type",  # per-row eligibility type column (resolved via resolve_policy_table)
  "min_age",           # per-row minimum age threshold
  "min_tenure",        # per-row minimum tenure threshold
  "retire",            # eligibility flag computed by fcase
  "age",               # age in years
  "tenure_years",      # tenure in years
  "tenure_days"        # tenure in days (intermediate)
))

#' Identify Personnel Eligible for Retirement
#'
#' @description
#' Determines which personnel are eligible for retirement based on age and/or
#' tenure criteria specified in policy parameters. Supports both scalar and
#' group-level eligibility parameters via \code{resolve_policy_table()};
#' uses \code{fcase()} for vectorised per-row eligibility dispatch.
#'
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data with birth dates
#' @param policy_params List. Policy parameters using the unified format:
#'   \describe{
#'     \item{defaults}{Named list of scalar fallback values.  Must contain
#'       \code{eligibility_type} (one of \code{"age_only"},
#'       \code{"tenure_only"}, \code{"age_and_tenure"}), \code{min_age}
#'       (required for age-based eligibility), and \code{min_tenure}
#'       (required for tenure-based eligibility).}
#'     \item{group_cols}{Character vector of contract columns to join on, or
#'       \code{NULL} for scalar-only dispatch.}
#'     \item{policy_table}{data.table with \code{group_cols} plus any
#'       per-group parameter columns (e.g. \code{eligibility_type},
#'       \code{min_age}, \code{min_tenure}).  \code{NULL} when not needed.}
#'   }
#' @param ref_date Date. Reference date for eligibility calculation
#' @param personnel_id_col Character. Name of personnel ID column (default: "personnel_id")
#' @param birth_date_col Character. Name of birth date column (default: "birth_date")
#' @param start_date_col Character. Name of start date column (default: "start_date")
#' @param end_date_col Character. Name of end date column (default: "end_date")
#' @param contract_type_col Character. Name of contract type column (default: "contract_type_code")
#' @param age_col Character or NULL. Column in personnel_dt containing pre-computed
#'   age (in years). When present, \code{compute_age()} is skipped. Default \code{"age"}.
#' @param tenure_col Character or NULL. Column in personnel_dt containing pre-computed
#'   tenure (in years). When present, \code{compute_tenure()} is skipped.
#'   Default \code{"tenure_years"}.
#'
#' @return data.table with columns: personnel_id, retire (0/1), age, tenure_years
#'
#' @section Retirement eligibility vs. retirement choice:
#' This function equates \emph{eligibility} with \emph{retirement} — all
#' personnel who meet the policy thresholds are assumed to retire
#' (100\% take-up).  In practice, not all eligible personnel choose to retire
#' immediately; take-up depends on individual incentives, pension wealth, and
#' labour market conditions.  A future extension will add an optional second
#' stage where a retirement choice model (e.g. probit on age, tenure, salary
#' relative to expected pension) is applied to the eligible pool to produce a
#' probabilistic retirement indicator.  This would be passed as an optional
#' \code{choice_model} argument and default to \code{NULL} (100\% take-up).
#'
#' @keywords internal
identify_retirees <- function(contract_dt,
                              personnel_dt,
                              policy_params,
                              ref_date,
                              personnel_id_col = "personnel_id",
                              contract_id_col = "contract_id",
                              birth_date_col = "birth_date",
                              start_date_col = "start_date",
                              end_date_col = "end_date",
                              contract_type_col = "contract_type_code",
                              age_col   = "age",
                              tenure_col = "tenure_years") {
  
  # Determine the effective scalar eligibility_type from defaults.
  # Used to decide which metrics to compute before the per-row resolution.
  .defaults        <- if (is.null(policy_params$defaults)) list() else policy_params$defaults
  .default_etype   <- .defaults$eligibility_type
  if (is.null(.default_etype) || length(.default_etype) == 0L)
    .default_etype <- "age_only"   # safe default for non-retirement callers

  # If policy_table has an eligibility_type column, types can vary per group
  # so we may need both age and tenure regardless of the scalar default.
  .etype_varies <- !is.null(policy_params$policy_table) &&
                   "eligibility_type" %in% names(policy_params$policy_table)

  .needs_age    <- .etype_varies || .default_etype %in% c("age_only",    "age_and_tenure")
  .needs_tenure <- .etype_varies || .default_etype %in% c("tenure_only", "age_and_tenure")

  # Restrict candidate pool to personnel who have at least one active contract.
  # People with only inactive/pensioner contracts must not be identified as
  # retirement candidates — they are already out of the workforce.
  # Personnel who are filtered out here still appear in the result with retire = 0.
  all_pid    <- unique(personnel_dt[[personnel_id_col]])
  active_pid <- unique(get_active_contracts(
    contract_dt       = contract_dt,
    ref_date          = ref_date,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    contract_type_col = contract_type_col
  )[[personnel_id_col]])
  personnel_dt <- personnel_dt[get(personnel_id_col) %in% active_pid]

  # Compute age if needed — prefer pre-computed column on personnel_dt.
  if (.needs_age) {
    if (!is.null(age_col) && age_col %in% names(personnel_dt)) {
      age_dt <- personnel_dt[, c(personnel_id_col, age_col), with = FALSE]
      data.table::setnames(age_dt, c(personnel_id_col, age_col), c(personnel_id_col, "age"))
    } else {
      age_dt <- compute_age(
        personnel_dt = personnel_dt,
        ref_date = ref_date,
        birth_date_col = birth_date_col,
        personnel_id_col = personnel_id_col
      )
    }
  } else {
    age_dt <- data.table::data.table(
      personnel_id = unique(personnel_dt[[personnel_id_col]]),
      age = NA_real_
    )
  }

  # Compute tenure if needed — prefer pre-computed column on personnel_dt.
  if (.needs_tenure) {
    if (!is.null(tenure_col) && tenure_col %in% names(personnel_dt)) {
      tenure_dt <- personnel_dt[, c(personnel_id_col, tenure_col), with = FALSE]
      data.table::setnames(tenure_dt, c(personnel_id_col, tenure_col),
                           c(personnel_id_col, "tenure_years"))
      tenure_dt[, tenure_days := tenure_years * 365.25]
    } else {
      tenure_dt <- compute_tenure(
        contract_dt = contract_dt,
        ref_date = ref_date,
        personnel_id_col = personnel_id_col,
        start_date_col = start_date_col,
        end_date_col = end_date_col,
        contract_type_col = contract_type_col
      )
    }
  } else {
    tenure_dt <- data.table::data.table(
      personnel_id = unique(contract_dt[[personnel_id_col]]),
      tenure_years = NA_real_,
      tenure_days = NA_real_
    )
  }
  
  # Merge age and tenure using data.table full join
  # Need full join to preserve all personnel from both age_dt and tenure_dt
  data.table::setnames(age_dt, "personnel_id", personnel_id_col)
  data.table::setnames(tenure_dt, "personnel_id", personnel_id_col)
  
  # Get all unique personnel_ids
  all_ids <- unique(c(age_dt[[personnel_id_col]], tenure_dt[[personnel_id_col]]))
  
  # Create base table with all IDs
  eligibility_dt <- data.table::data.table(id = all_ids)
  data.table::setnames(eligibility_dt, "id", personnel_id_col)
  
  # Left join age and tenure
  eligibility_dt <- age_dt[eligibility_dt, on = personnel_id_col]
  eligibility_dt <- tenure_dt[eligibility_dt, on = personnel_id_col]
  
  # If any group_cols are specified, enrich eligibility_dt with the
  # corresponding contract columns from each person's primary contract.
  # This single join serves all params (eligibility_type, min_age, min_tenure).
  .group_cols <- policy_params$group_cols
  if (!is.null(.group_cols) && length(.group_cols) > 0L) {
    .primary_groups <- get_primary_contract(
      contract_dt = get_active_contracts(
        contract_dt       = contract_dt,
        ref_date          = ref_date,
        start_date_col    = start_date_col,
        end_date_col      = end_date_col,
        contract_type_col = contract_type_col
      ),
      personnel_id_col = personnel_id_col,
      contract_id_col  = contract_id_col,
      start_date_col   = start_date_col
    )[, c(personnel_id_col, .group_cols), with = FALSE]

    eligibility_dt[
      .primary_groups,
      (.group_cols) := mget(paste0("i.", .group_cols)),
      on = personnel_id_col
    ]
  }

  # Resolve eligibility params to per-row vectors in a single call.
  .elig_resolved <- resolve_policy_table(
    policy_params,
    eligibility_dt,
    c("eligibility_type", "min_age", "min_tenure")
  )

  # Assign resolved params as columns — always add all three to avoid
  # 'object not found' in fcase() when a param is absent from defaults.
  eligibility_dt[, eligibility_type := .elig_resolved$eligibility_type %||%
                   rep("age_only", nrow(eligibility_dt))]
  eligibility_dt[, min_age    := .elig_resolved$min_age    %||%
                   rep(NA_real_, nrow(eligibility_dt))]
  eligibility_dt[, min_tenure := .elig_resolved$min_tenure %||%
                   rep(NA_real_, nrow(eligibility_dt))]

  # Validate that mandatory thresholds are present for the eligibility type —
  # but only when the caller explicitly supplied an eligibility_type in
  # policy_params$defaults.  When identify_retirees() is called from a non-
  # retirement context (e.g. hiring demand) that provides no eligibility_type,
  # the safe default "age_only" is substituted above purely to keep fcase()
  # from erroring; in that case there is no misconfiguration to report.
  .etype_explicit <- !is.null(.defaults$eligibility_type) &&
                     length(.defaults$eligibility_type) > 0L
  if (.etype_explicit) {
    .bad_age <- eligibility_dt[
      eligibility_type %in% c("age_only", "age_and_tenure") & is.na(min_age)
    ]
    .bad_ten <- eligibility_dt[
      eligibility_type %in% c("tenure_only", "age_and_tenure") & is.na(min_tenure)
    ]
    if (nrow(.bad_age) > 0L)
      stop("identify_retirees: ", nrow(.bad_age),
           " rows have eligibility_type requiring 'min_age' but min_age is NA. ",
           "Check policy_params$defaults$min_age or policy_table.",
           call. = FALSE)
    if (nrow(.bad_ten) > 0L)
      stop("identify_retirees: ", nrow(.bad_ten),
           " rows have eligibility_type requiring 'min_tenure' but min_tenure is NA. ",
           "Check policy_params$defaults$min_tenure or policy_table.",
           call. = FALSE)
  }

  # Vectorised eligibility check via fcase — handles mixed per-row types.
  # NA comparisons (e.g. age >= min_age when age is NA) yield NA which fcase
  # treats as unmatched → falls to default = 0L (not eligible).
  eligibility_dt[, retire := data.table::fcase(
    eligibility_type == "age_only",
      as.integer(!is.na(age) & !is.na(min_age) & age >= min_age),
    eligibility_type == "tenure_only",
      as.integer(!is.na(tenure_years) & !is.na(min_tenure) & tenure_years >= min_tenure),
    eligibility_type == "age_and_tenure",
      as.integer(!is.na(age) & !is.na(tenure_years) &
                 !is.na(min_age) & !is.na(min_tenure) &
                 age >= min_age & tenure_years >= min_tenure),
    default = 0L
  )]
  
  # Handle NA values (set to 0 - not eligible)
  eligibility_dt[is.na(retire), retire := 0L]

  # Return with standardized column names
  result <- eligibility_dt[, c(personnel_id_col, "retire", "age", "tenure_years"), with = FALSE]
  data.table::setnames(result, personnel_id_col, "personnel_id")

  # Add back any personnel who were filtered out (no active contract) with retire = 0.
  # This preserves the contract — "all personnel appear in the result" — expected by
  # callers and tests that check retire == 0 for personnel with NA start_date, etc.
  inactive_pids <- setdiff(all_pid, active_pid)
  if (length(inactive_pids) > 0L) {
    inactive_rows <- data.table::data.table(
      personnel_id = inactive_pids,
      retire       = 0L,
      age          = NA_real_,
      tenure_years = NA_real_
    )
    result <- data.table::rbindlist(list(result, inactive_rows),
                                    use.names = TRUE, fill = TRUE)
  }

  return(result)
}


#' Compute Retirement Summary Statistics
#'
#' @description
#' Aggregates key statistics about retirees including count, total pension cost,
#' average age, and average tenure. Uses data.table for efficient computation
#' on large datasets.
#'
#' @param retirees_dt data.table. Retiree data with pension amounts
#' @param contract_dt data.table. Original contract data (for wage bill comparison)
#'
#' @return data.table with summary statistics
#' @keywords internal
compute_retirement_summary <- function(retirees_dt, contract_dt = NULL) {
  
  # Handle case of no retirees
  if (nrow(retirees_dt) == 0) {
    summary_tbl <- data.table::data.table(
      n_retired = 0L,
      total_pension = 0,
      avg_pension = NA_real_,
      avg_age = NA_real_,
      avg_tenure = NA_real_
    )
    return(summary_tbl)
  }
  
  # Compute summary statistics using data.table for efficiency
  summary_tbl <- retirees_dt[, .(
    n_retired = .N,
    total_pension = sum(pension, na.rm = TRUE),
    avg_pension = mean(pension, na.rm = TRUE),
    avg_age = mean(age, na.rm = TRUE),
    avg_tenure = mean(tenure_years, na.rm = TRUE)
  )]
  
  return(summary_tbl)
}


#' Prepare Retiree Data for Pension Calculation
#'
#' @description
#' Enriches retiree eligibility data with salary and contract information
#' needed for pension calculations. Merges age/tenure with contract data.
#'
#' @param eligibility_dt data.table. Output from identify_retirees()
#' @param contract_dt data.table. Contract data
#' @param personnel_dt data.table. Personnel data
#' @param ref_date Date. Reference date
#' @param personnel_id_col Character. Name of personnel ID column (default: "personnel_id")
#' @param birth_date_col Character. Name of birth date column (default: "birth_date")
#' @param contract_id_col Character. Name of contract ID column (default: "contract_id")
#' @param start_date_col Character. Name of start date column (default: "start_date")
#' @param end_date_col Character. Name of end date column (default: "end_date")
#' @param salary_col Character. Name of salary column (default: "gross_salary_lcu")
#' @param contract_type_col Character. Name of contract type column (default: "contract_type_code")
#'
#' @return data.table with retiree information ready for pension calculation
#' @keywords internal
prepare_retiree_data <- function(eligibility_dt,
                                 contract_dt,
                                 personnel_dt,
                                 ref_date,
                                 personnel_id_col = "personnel_id",
                                 birth_date_col = "birth_date",
                                 contract_id_col = "contract_id",
                                 start_date_col = "start_date",
                                 end_date_col = "end_date",
                                 salary_col = "gross_salary_lcu",
                                 contract_type_col = "contract_type_code") {
  
  # Filter to eligible retirees only
  retirees_only <- eligibility_dt[retire == 1]
  
  # If no retirees, return empty
  if (nrow(retirees_only) == 0) {
    return(data.table::data.table())
  }
  
  # Always compute both age and tenure here, regardless of eligibility_type.
  # identify_retirees() only computes whichever field the eligibility rule needs,
  # leaving the other as NA. Pension formulas (e.g. DB) may need both, so we
  # fill any missing values now from the source data.
  if (anyNA(retirees_only$age)) {
    age_dt <- compute_age(
      personnel_dt     = personnel_dt,
      ref_date         = ref_date,
      birth_date_col   = birth_date_col,
      personnel_id_col = personnel_id_col
    )
    retirees_only[age_dt, age := i.age, on = "personnel_id"]
  }
  
  if (anyNA(retirees_only$tenure_years)) {
    tenure_dt <- compute_tenure(
      contract_dt       = contract_dt,
      ref_date          = ref_date,
      personnel_id_col  = personnel_id_col,
      contract_id_col   = contract_id_col,
      start_date_col    = start_date_col,
      end_date_col      = end_date_col,
      contract_type_col = contract_type_col
    )
    retirees_only[tenure_dt, tenure_years := i.tenure_years, on = "personnel_id"]
  }
  
  # Get active contracts for these retirees
  active_contracts <- get_active_contracts(
    contract_dt = contract_dt,
    ref_date = ref_date,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col
  )
  
  # Filter to retirees only
  retiree_contracts <- active_contracts[get(personnel_id_col) %in% retirees_only$personnel_id]
  
  # Get primary contract for each retiree
  primary_contracts <- get_primary_contract(
    contract_dt = retiree_contracts,
    personnel_id_col = personnel_id_col,
    contract_id_col = contract_id_col,
    start_date_col = start_date_col,
    salary_col = salary_col
  )
  
  # Merge with eligibility data (age, tenure) using data.table join
  retirees_dt <- primary_contracts[retirees_only[, .(personnel_id, age, tenure_years)], 
                                   on = personnel_id_col]
  
  return(retirees_dt)
}
