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

# Suppress R CMD check NOTEs for data.table column names used in identify_eligibility().
utils::globalVariables(c(
  "eligibility_type", # per-row eligibility type column (resolved via resolve_policy_table)
  "min_age", # per-row minimum age threshold
  "min_tenure", # per-row minimum tenure threshold
  "retire", # eligibility flag computed by fcase
  "age", # age in years
  "tenure_years", # tenure in years
  "tenure_days" # tenure in days (intermediate)
))


#' Determine Which Eligibility Metrics Must Be Computed
#'
#' @description
#' Inspects retirement policy parameters and determines whether age and/or
#' tenure must be computed for this run. It also extracts all column names
#' referenced by any custom eligibility rules, so caller code can enrich the
#' working table only with fields that are actually needed.
#'
#' @param policy_params List. Unified policy specification containing
#'   \code{defaults} and optionally \code{policy_table}.
#'
#' @return Named list with elements \code{default_etype}, \code{etype_varies},
#'   \code{rule_all_cols}, \code{needs_age}, and \code{needs_tenure}.
#'
#' @keywords internal
.determine_required_metrics <- function(policy_params, 
                                        age_col = "age", 
                                        tenure_col = "tenure_years") {
  
  .defaults <- if (is.null(policy_params$defaults)) {
    list()
  } else {
    policy_params$defaults
  }
  .default_etype <- .defaults$eligibility_type
  if (is.null(.default_etype) || length(.default_etype) == 0L) {
    .default_etype <- "age_only"
  }

  .etype_varies <- !is.null(policy_params$policy_table) &&
    "eligibility_type" %in% names(policy_params$policy_table)

  .rule_candidates <- character(0)
  if (!is.null(.defaults$eligibility_rule)) {
    .rule_candidates <- c(.rule_candidates, .defaults$eligibility_rule)
  }
  if (
    !is.null(policy_params$policy_table) &&
      "eligibility_rule" %in% names(policy_params$policy_table)
  ) {
    .rule_candidates <- c(
      .rule_candidates,
      unique(policy_params$policy_table$eligibility_rule)
    )
  }
  .rule_candidates <- unique(.rule_candidates)

  .rule_all_cols <- if (length(.rule_candidates) > 0L) {
    unique(unlist(lapply(.rule_candidates, function(r) all.vars(str2lang(r)))))
  } else {
    character(0)
  }

  list(
    default_etype = .default_etype,
    etype_varies = .etype_varies,
    rule_all_cols = .rule_all_cols,
    needs_age = .etype_varies ||
      .default_etype %in% c("age_only", "age_and_tenure") ||
      any(c("age", age_col) %in% .rule_all_cols),
    needs_tenure = .etype_varies ||
      .default_etype %in% c("tenure_only", "age_and_tenure") ||
      any(c("tenure_years", tenure_col) %in% .rule_all_cols))
  
}

#' Filter Personnel to Active Retirement Candidates
#'
#' @description
#' Restricts the candidate pool to personnel who hold an active, non-pensioner
#' contract at \code{ref_date}. Returns both the filtered table and identifier
#' vectors used later to restore inactive personnel with \code{retire = 0}.
#'
#' @param contract_dt data.table. Contract register.
#' @param personnel_dt data.table. Personnel register.
#' @param ref_date Date. Reference date for active-contract filtering.
#' @param personnel_id_col Character. Personnel identifier column name.
#' @param start_date_col Character. Contract start date column name.
#' @param end_date_col Character. Contract end date column name.
#' @param contract_type_col Character. Contract type column name.
#' @param status_col Character. Personnel status column name.
#'
#' @return Named list with \code{personnel_dt}, \code{all_pid}, and
#'   \code{active_pid}.
#'
#' @keywords internal
.filter_active_candidate_pool <- function(contract_dt,
                                          personnel_dt,
                                          ref_date,
                                          personnel_id_col,
                                          start_date_col,
                                          end_date_col,
                                          contract_type_col,
                                          status_col) {
  
  all_pid <- unique(personnel_dt[[personnel_id_col]])
  active_pid <- unique(get_active_contracts(
    contract_dt = contract_dt,
    ref_date = ref_date,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col
  )[[personnel_id_col]])

  filtered_dt <- personnel_dt[get(personnel_id_col) %in% active_pid]
  if (!is.null(status_col) && status_col %in% names(filtered_dt)) {
    filtered_dt <- filtered_dt[!get(status_col) == "pensioner", ]
  }

  list(personnel_dt = filtered_dt, all_pid = all_pid, active_pid = active_pid)
}

#' Enrich a Working Table with Required Columns
#'
#' @description
#' Adds requested columns to a working \code{data.table} by joining from
#' \code{contract_dt} and/or \code{personnel_dt}. Existing columns are left
#' untouched. The function modifies \code{working_dt} by reference.
#'
#' @param working_dt data.table. Table to enrich in-place.
#' @param needed_cols Character vector. Columns to add when absent.
#' @param contract_dt data.table. Contract source table.
#' @param personnel_dt data.table. Personnel source table.
#' @param ref_date Date. Reference date for active contract extraction.
#' @param personnel_id_col Character. Personnel identifier column name.
#' @param contract_id_col Character. Contract identifier column name.
#' @param start_date_col Character. Contract start date column name.
#' @param end_date_col Character. Contract end date column name.
#' @param contract_type_col Character. Contract type column name.
#'
#' @return Invisibly returns \code{working_dt} after in-place modification.
#'
#' @keywords internal
.enrich_working_table <- function(working_dt,
                                  needed_cols,
                                  contract_dt,
                                  personnel_dt,
                                  ref_date,
                                  personnel_id_col,
                                  contract_id_col,
                                  start_date_col,
                                  end_date_col,
                                  contract_type_col){
  # Add only missing requested columns.
  needed_cols <- setdiff(unique(needed_cols), names(working_dt))
  if (length(needed_cols) == 0L) {
    return(invisible(working_dt))
  }

  from_contract <- intersect(needed_cols, names(contract_dt))
  from_personnel <- intersect(needed_cols, names(personnel_dt))

  # Contract-level columns are sourced from the primary active contract.
  if (length(from_contract) > 0L) {
    primary <- get_primary_contract(
      contract_dt = get_active_contracts(
        contract_dt = contract_dt,
        ref_date = ref_date,
        start_date_col = start_date_col,
        end_date_col = end_date_col,
        contract_type_col = contract_type_col
      ),
      personnel_id_col = personnel_id_col,
      contract_id_col = contract_id_col,
      start_date_col = start_date_col
    )[, c(personnel_id_col, from_contract), with = FALSE]

    working_dt[
      primary,
      (from_contract) := mget(paste0("i.", from_contract)),
      on = personnel_id_col
    ]
  }

  # Personnel-level columns are sourced directly from personnel_dt.
  if (length(from_personnel) > 0L) {
    working_dt[
      personnel_dt[, c(personnel_id_col, from_personnel), with = FALSE],
      (from_personnel) := mget(paste0("i.", from_personnel)),
      on = personnel_id_col
    ]
  }

  invisible(working_dt)
}


#' Evaluate Row-Level Expression Strings
#'
#' @description
#' Evaluates expression strings stored in \code{expr_col} for rows selected by
#' \code{mask}, then writes results into \code{result_col}. For efficiency,
#' rows are grouped by unique expression text and each expression is parsed once
#' per unique value.
#'
#' @param dt data.table. Input table.
#' @param mask Logical vector. Rows where evaluation should occur.
#' @param expr_col Character. Column holding expression strings.
#' @param result_col Character. Destination column for evaluated results.
#'
#' @return Invisibly returns the modified \code{dt}.
#'
#' @keywords internal
.eval_per_row_expression <- function(dt, mask, expr_col, result_col, init_value = NA) {
  # Initialize output; rows outside mask stay NA.
  dt[, (result_col) := init_value]
  if (!any(mask)) {
    return(invisible(dt))
  }

  unique_exprs <- unique(dt[[expr_col]][mask]) ## set the unique user input expressions
  ## parse each unique expression 
  for (e in unique_exprs) {
    parsed <- str2lang(e) 
    cols <- all.vars(parsed) ## get the column names within the expression
    subrows <- mask & dt[[expr_col]] == e ## get the set of row each expression applies to

    # Evaluate in the row context of required columns only.
    dt[subrows, (result_col) := eval(parsed, .SD), .SDcols = cols]
  }

  invisible(dt)
}

#' Identify Personnel Eligible for Retirement
#'
#' @description
#' Determines which personnel are eligible for retirement based on age and/or
#' tenure criteria specified in \code{policy_params}.  Supports scalar and
#' group-level policy variation via \code{\link{resolve_policy_table}}, and
#' uses \code{data.table::fcase()} for fully vectorised per-row eligibility
#' dispatch across mixed \code{eligibility_type} values.
#'
#' @param contract_dt data.table.  Active workforce contracts in govhr format.
#'   Required columns: \code{personnel_id_col}, \code{contract_id_col},
#'   \code{start_date_col}, \code{end_date_col}, \code{contract_type_col}.
#'   When \code{group_cols} are non-null, must also include those columns.
#' @param personnel_dt data.table.  Personnel register.  Required columns:
#'   \code{personnel_id_col}, \code{birth_date_col}.  Optional:
#'   \code{age_col}, \code{tenure_col}.
#' @param policy_params List.  Unified policy specification.  See
#'   \code{\link{simulate_retirement}} for the full three-slot specification.
#'   Eligibility-relevant keys in \code{defaults}:
#'   \itemize{
#'     \item \code{eligibility_type}: Character scalar.  One of
#'       \code{"age_only"}, \code{"tenure_only"}, \code{"age_and_tenure"}.
#'     \item \code{min_age}: Numeric scalar.  Required when
#'       \code{eligibility_type} is \code{"age_only"} or
#'       \code{"age_and_tenure"}.
#'     \item \code{min_tenure}: Numeric scalar.  Required when
#'       \code{eligibility_type} is \code{"tenure_only"} or
#'       \code{"age_and_tenure"}.
#'   }
#' @param ref_date Date.  Reference date for age and tenure computation.
#' @param personnel_id_col Character.  Unique personnel identifier present in
#'   both \code{contract_dt} and \code{personnel_dt}.
#'   (default: \code{"personnel_id"}).
#' @param birth_date_col Character.  Date of birth column in
#'   \code{personnel_dt}.  (default: \code{"birth_date"}).
#' @param contract_id_col Character.  Contract identifier in
#'   \code{contract_dt}.  Used by \code{\link{compute_tenure}} to deduplicate
#'   overlapping spells.  (default: \code{"contract_id"}).
#' @param start_date_col Character.  Contract start date column.
#'   (default: \code{"start_date"}).
#' @param end_date_col Character.  Contract end date column; \code{NA} for
#'   open-ended contracts.  (default: \code{"end_date"}).
#' @param contract_type_col Character.  Contract classification column.  Only
#'   contracts with type not in \code{c("inactive", "pensioner")} are treated
#'   as active candidates.  (default: \code{"contract_type"}).
#' @param age_col Character.  Column in \code{personnel_dt} with pre-computed
#'   age in years.  When the column is present, \code{\link{compute_age}} is
#'   skipped entirely.  (default: \code{"age"}).
#' @param tenure_col Character.  Column in \code{personnel_dt} with
#'   pre-computed tenure in years.  When present,
#'   \code{\link{compute_tenure}} is skipped.  (default: \code{"tenure_years"}).
#'
#' @details
#' \strong{Computation strategy:}
#' \enumerate{
#'   \item Only active contracts (\code{\link{get_active_contracts}}) are
#'     candidates.  Personnel with only inactive/pensioner contracts receive
#'     \code{retire = 0L} immediately.
#'   \item Age and tenure are computed only for the metric(s) required by the
#'     effective \code{eligibility_type}.  If \code{policy_table} contains an
#'     \code{eligibility_type} column (i.e. the type varies by group), both
#'     metrics are always computed defensively.
#'   \item Group-level parameters are resolved onto \code{eligibility_dt} via
#'     \code{\link{resolve_policy_table}}, a single join that stamps
#'     \code{eligibility_type}, \code{min_age}, and \code{min_tenure} as
#'     per-row columns.
#'   \item \code{data.table::fcase()} performs the eligibility dispatch in one
#'     vectorised pass.  \code{NA} comparisons evaluate to \code{NA} and fall
#'     through to \code{default = 0L} (not eligible).
#'   \item NA validation (missing required thresholds) fires only when the
#'     caller explicitly supplied \code{eligibility_type} in
#'     \code{policy_params$defaults}.  This prevents false positives when
#'     \code{identify_eligibility()} is invoked from non-retirement contexts
#'     (e.g. hiring demand estimation) that omit eligibility configuration.
#' }
#'
#' @return data.table (one row per unique \code{personnel_id}) with columns:
#'   \describe{
#'     \item{\code{personnel_id}}{Personnel identifier.  All IDs from
#'       \code{personnel_dt} are present — inactive personnel are not dropped.}
#'     \item{\code{retire}}{Integer (0/1).  1 = eligible and assumed to retire;
#'       0 = not eligible, already inactive, or a threshold was \code{NA}.}
#'     \item{\code{age}}{Numeric.  Age in years at \code{ref_date}.
#'       \code{NA} for personnel inactive at \code{ref_date} or when age
#'       computation was skipped (tenure-only eligibility).}
#'     \item{\code{tenure_years}}{Numeric.  Cumulative service in years at
#'       \code{ref_date}.  \code{NA} analogously to \code{age}.}
#'   }
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
#'

identify_eligibility <- function(contract_dt,
                                 personnel_dt,
                                 policy_params,
                                 ref_date,
                                 personnel_id_col  = "personnel_id",
                                 contract_id_col   = "contract_id",
                                 birth_date_col    = "birth_date",
                                 start_date_col    = "start_date",
                                 end_date_col      = "end_date",
                                 contract_type_col = "contract_type",
                                 age_col           = "age",
                                 tenure_col        = "tenure_years",
                                 status_col        = "employment_status") {
  
  .defaults <- if (is.null(policy_params$defaults)) {
    list()
  } else {
    policy_params$defaults
  }

  .metrics <- .determine_required_metrics(policy_params, age_col, tenure_col)

  # Restrict candidate pool to personnel with an active, non-pensioner
  # contract. Personnel filtered out here still appear in the final result
  # with retire = 0.
  .pool <- .filter_active_candidate_pool(
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    ref_date = ref_date,
    personnel_id_col = personnel_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col,
    status_col = status_col
  )
  personnel_dt <- .pool$personnel_dt

  # Compute age if needed — prefer pre-computed column on personnel_dt.
  # The goal is to ensure at the end of the day we have an column named "age"
  if (.metrics$needs_age) {
    if (!is.null(age_col) && age_col %in% names(personnel_dt)) {
      age_dt <- personnel_dt[, c(personnel_id_col, age_col), with = FALSE]
      data.table::setnames(
        age_dt,
        c(personnel_id_col, age_col),
        c(personnel_id_col, "age")
      )
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
  # but also make sure at the end whatever the case, tenure_years and tenure_days
  # are the names of the tenure column
  if (.metrics$needs_tenure) {
    if (!is.null(tenure_col) && tenure_col %in% names(personnel_dt)) {
      tenure_dt <- personnel_dt[, c(personnel_id_col, tenure_col), with = FALSE]
      data.table::setnames(
        tenure_dt,
        c(personnel_id_col, tenure_col),
        c(personnel_id_col, "tenure_years")
      )
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

  data.table::setnames(age_dt, "personnel_id", personnel_id_col)
  data.table::setnames(tenure_dt, "personnel_id", personnel_id_col)

  all_ids <- unique(c(
    age_dt[[personnel_id_col]],
    tenure_dt[[personnel_id_col]]
  ))
  eligibility_dt <- data.table::data.table(id = all_ids)
  data.table::setnames(eligibility_dt, "id", personnel_id_col)
  eligibility_dt <- age_dt[eligibility_dt, on = personnel_id_col]
  eligibility_dt <- tenure_dt[eligibility_dt, on = personnel_id_col]

  # One join call covers both .group_cols (parameter grouping) and any
  # columns a custom eligibility_rule references -- .enrich_working_table()
  # skips whatever's already present, so there's no risk of double-joining
  # age/tenure_years even though they're technically in .rule_all_cols too.
  .needed_cols <- union(policy_params$group_cols, .metrics$rule_all_cols)
  .enrich_working_table(
    working_dt = eligibility_dt,
    needed_cols = .needed_cols,
    contract_dt = contract_dt,
    personnel_dt = personnel_dt,
    ref_date = ref_date,
    personnel_id_col = personnel_id_col,
    contract_id_col = contract_id_col,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col
  )

  # Resolve eligibility params to per-row vectors in a single call.
  .elig_resolved <- resolve_policy_table(
    policy_params,
    eligibility_dt,
    c("eligibility_type", "min_age", "min_tenure", "eligibility_rule")
  )
  eligibility_dt[,
    eligibility_type := .elig_resolved$eligibility_type %||%
      rep("age_only", nrow(eligibility_dt))
  ]
  eligibility_dt[,
    min_age := .elig_resolved$min_age %||%
      rep(NA_real_, nrow(eligibility_dt))
  ]
  eligibility_dt[,
    min_tenure := .elig_resolved$min_tenure %||%
      rep(NA_real_, nrow(eligibility_dt))
  ]
  eligibility_dt[,
    eligibility_rule := .elig_resolved$eligibility_rule %||%
      rep(NA_character_, nrow(eligibility_dt))
  ]

  # Validate that mandatory thresholds are present — only when the caller
  # explicitly supplied an eligibility_type (see original comment: this
  # keeps non-retirement callers, e.g. hiring demand, from erroring on a
  # substituted default that was never really "configured").
  .etype_explicit <- !is.null(.defaults$eligibility_type) &&
    length(.defaults$eligibility_type) > 0L
  if (.etype_explicit) {
    .bad_age <- eligibility_dt[
      eligibility_type %in% c("age_only", "age_and_tenure") & is.na(min_age)
    ]
    .bad_ten <- eligibility_dt[
      eligibility_type %in%
        c("tenure_only", "age_and_tenure") &
        is.na(min_tenure)
    ]
    if (nrow(.bad_age) > 0L) {
      stop(
        "identify_eligibility: ",
        nrow(.bad_age),
        " rows have eligibility_type requiring 'min_age' but min_age is NA. ",
        "Check policy_params$defaults$min_age or policy_table.",
        call. = FALSE
      )
    }
    if (nrow(.bad_ten) > 0L) {
      stop(
        "identify_eligibility: ",
        nrow(.bad_ten),
        " rows have eligibility_type requiring 'min_tenure' but min_tenure is NA. ",
        "Check policy_params$defaults$min_tenure or policy_table.",
        call. = FALSE
      )
    }
  }

  # Custom rules: grouped-by-unique-string evaluation, reusing the same
  # helper compute_custom_pension() will use later.
  .eval_per_row_expression(
    dt = eligibility_dt,
    mask = eligibility_dt$eligibility_type == "custom",
    expr_col = "eligibility_rule",
    result_col = "custom_eligible"
  )

  eligibility_dt[,
    retire := data.table::fcase(
      eligibility_type == "age_only",
      as.integer(!is.na(age) & !is.na(min_age) & age >= min_age),
      eligibility_type == "tenure_only",
      as.integer(!is.na(tenure_years) & !is.na(min_tenure) & tenure_years >= min_tenure),
      eligibility_type == "age_and_tenure",
      as.integer(!is.na(age) & !is.na(tenure_years) & 
        !is.na(min_age) & !is.na(min_tenure) & age >= min_age & tenure_years >= min_tenure),
      eligibility_type == "custom",
      as.integer(!is.na(custom_eligible) & custom_eligible),                                                                               
      default = 0L
    )
  ]
  eligibility_dt[is.na(retire), retire := 0L]

  result <- eligibility_dt[,
    c(personnel_id_col, "retire", "age", "tenure_years"),
    with = FALSE
  ]
  data.table::setnames(result, personnel_id_col, "personnel_id")

  inactive_pids <- setdiff(.pool$all_pid, .pool$active_pid)
  if (length(inactive_pids) > 0L) {
    inactive_rows <- data.table::data.table(
      personnel_id = inactive_pids,
      retire = 0L,
      age = NA_real_,
      tenure_years = NA_real_
    )
    result <- data.table::rbindlist(
      list(result, inactive_rows),
      use.names = TRUE,
      fill = TRUE
    )
  }

  return(result)
}


#' Compute Retirement Summary Statistics
#'
#' @description
#' Aggregates per-period retirement statistics from the enriched retiree
#' table produced by \code{\link{simulate_retirement}}.  Returns a consistent
#' 1-row \code{data.table} even when there are no retirees, so downstream
#' horizon accumulators can \code{rbindlist()} safely across all periods.
#'
#' @param retirees_dt data.table.  One row per retiree as returned by the
#'   pension calculation stage of \code{\link{simulate_retirement}}.  Must
#'   contain columns: \code{pension} (numeric), \code{age} (numeric),
#'   \code{tenure_years} (numeric).  When \code{nrow(retirees_dt) == 0} the
#'   function returns a zero-filled result without inspecting columns.
#' @param contract_dt data.table.  Unused; kept for API compatibility.  May
#'   be \code{NULL}.
#'
#' @return data.table (1 row) with five columns:
#'   \describe{
#'     \item{\code{n_retired}}{Integer.  Count of retirees in the period.
#'       \code{0L} when there are no retirees.}
#'     \item{\code{total_pension}}{Numeric.  Sum of all individual periodic
#'       pension amounts in \code{retirees_dt$pension}.  \code{0} when empty.}
#'     \item{\code{avg_pension}}{Numeric.  Mean pension across retirees.
#'       \code{NA_real_} when there are no retirees.}
#'     \item{\code{avg_age}}{Numeric.  Mean age at retirement.
#'       \code{NA_real_} when there are no retirees.}
#'     \item{\code{avg_tenure}}{Numeric.  Mean years of service at retirement.
#'       \code{NA_real_} when there are no retirees.}
#'   }
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
#' Enriches the eligibility table from \code{\link{identify_eligibility}} with
#' salary and contract information required for pension formula evaluation.
#' Fills any \code{NA} age or tenure values left by the eligibility stage
#' (e.g. when \code{eligibility_type = "tenure_only"} left \code{age} as
#' \code{NA}) and selects one primary contract per retiree via
#' \code{\link{get_primary_contract}}.
#'
#' @param eligibility_dt data.table.  Output of \code{\link{identify_eligibility}}
#'   with columns \code{personnel_id}, \code{retire}, \code{age},
#'   \code{tenure_years}.  Only rows where \code{retire == 1} are processed;
#'   the rest are ignored.
#' @param contract_dt data.table.  Full contract register (not pre-filtered).
#'   Active contracts are extracted internally via
#'   \code{\link{get_active_contracts}}.
#' @param personnel_dt data.table.  Personnel register.  Used to compute
#'   missing age values when \code{anyNA(eligibility_dt$age)} is \code{TRUE}.
#' @param ref_date Date.  Reference date passed to age/tenure computations
#'   when filling \code{NA} values.
#' @param personnel_id_col Character.  Personnel identifier column.
#'   (default: \code{"personnel_id"}).
#' @param birth_date_col Character.  Date of birth column in
#'   \code{personnel_dt}.  (default: \code{"birth_date"}).
#' @param contract_id_col Character.  Contract identifier column.
#'   (default: \code{"contract_id"}).
#' @param start_date_col Character.  Contract start date column.
#'   (default: \code{"start_date"}).
#' @param end_date_col Character.  Contract end date column.
#'   (default: \code{"end_date"}).
#' @param salary_col Character.  Gross salary column used as the pension wage
#'   base unless overridden by \code{policy_params$defaults$ref_wage_col}.
#'   (default: \code{"gross_salary_lcu"}).
#' @param contract_type_col Character.  Contract classification column.
#'   (default: \code{"contract_type"}).
#'
#' @details
#' \strong{Fill logic for missing metrics:}
#' \itemize{
#'   \item If \code{eligibility_type = "tenure_only"}, \code{age} was not
#'     computed upstream.  \code{anyNA(age)} triggers a call to
#'     \code{\link{compute_age}} to fill the gaps — needed by DB pension
#'     formulas that use age.
#'   \item Analogously, \code{anyNA(tenure_years)} triggers
#'     \code{\link{compute_tenure}}.
#'   \item Only missing values are filled; pre-computed values are untouched.
#' }
#'
#' @return data.table.  One row per retiree with all primary contract columns
#'   (including \code{salary_col}) plus \code{age} and \code{tenure_years}.
#'   Returns an empty \code{data.table()} (zero rows, no columns) when
#'   \code{eligibility_dt} contains no retirees.
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
                                 contract_type_col = "contract_type"){
  
  # Filter to eligible retirees only
  retirees_only <- eligibility_dt[retire == 1]

  # If no retirees, return empty
  if (nrow(retirees_only) == 0) {
    return(data.table::data.table())
  }

  # Always compute both age and tenure here, regardless of eligibility_type.
  # identify_eligibility() only computes whichever field the eligibility rule needs,
  # leaving the other as NA. Pension formulas (e.g. DB) may need both, so we
  # fill any missing values now from the source data.
  if (anyNA(retirees_only$age)) {
    age_dt <- compute_age(
      personnel_dt = personnel_dt,
      ref_date = ref_date,
      birth_date_col = birth_date_col,
      personnel_id_col = personnel_id_col
    )
    retirees_only[age_dt, age := i.age, on = "personnel_id"]
  }

  if (anyNA(retirees_only$tenure_years)) {
    tenure_dt <- compute_tenure(
      contract_dt = contract_dt,
      ref_date = ref_date,
      personnel_id_col = personnel_id_col,
      contract_id_col = contract_id_col,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      contract_type_col = contract_type_col
    )
    retirees_only[
      tenure_dt,
      tenure_years := i.tenure_years,
      on = "personnel_id"
    ]
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
  retiree_contracts <- active_contracts[
    get(personnel_id_col) %in% retirees_only$personnel_id
  ]

  # Get primary contract for each retiree
  primary_contracts <- get_primary_contract(
    contract_dt = retiree_contracts,
    personnel_id_col = personnel_id_col,
    contract_id_col = contract_id_col,
    start_date_col = start_date_col,
    salary_col = salary_col
  )

  # Merge with eligibility data (age, tenure) using data.table join
  retirees_dt <- primary_contracts[
    retirees_only[, .(personnel_id, age, tenure_years)],
    on = personnel_id_col
  ]

  return(retirees_dt)
}
