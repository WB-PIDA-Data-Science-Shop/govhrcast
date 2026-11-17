#' Core Retirement Logic Functions
#'
#' @description
#' Who retires: [select_retirees()] and the three kinds of policy it runs
#' (status quo, eligibility rule, mandatory rule on top of the status quo);
#' estimating the status-quo rates from panel data; and the retirees' data the
#' pension calculation needs.
#'
#' @import data.table
#' @name retirement_core
#' @keywords internal
NULL

# Suppress R CMD check NOTEs for data.table column names used in this file.
utils::globalVariables(c(
  "retire", # always 1 on select_retirees() output, for prepare_retiree_data()
  "age", # age in years
  "tenure_years", # tenure in years
  "age_yr_int", # whole-year age, a key of the status-quo rate tables
  "hazard_rate", # per-group retirement hazard rate resolved onto working tables
  "n_retire", # per-group retirement count computed in compute_status_quo_retirements
  "n_active", # per-group active headcount computed in compute_status_quo_retirements
  "retire_reason", # why each retiree retires: "rule" or "status_quo"
  # estimate_replacement_rates(): its working table and output
  "id", "date", "x.date", "pension_date", "last_date", "periods_apart",
  "last_salary", "first_pension", "replacement_rate"
))


#' Select Who Retires This Year
#'
#' @description
#' Picks the people who retire at `as_of_date` under a retirement policy, without
#' calculating their pensions. [simulate_retirement()] calls it and then pays the
#' pensions.
#'
#' Who can retire: everyone holding an active contract who does not already draw
#' a pension. A blank contract type or status counts as staff. A policy with
#' `active_types` (e.g. `"permanent"`) narrows this to people holding a contract
#' of those types.
#'
#' Who does retire depends on the policy's `type`:
#' * `"status_quo"`: in each group of the rate table (e.g. each age), the share
#'   that retired in the past, drawn at random or by `retire_strategy`.
#' * `"eligibility"`: everyone who meets the rule (e.g. aged 60 or over).
#' * `"mandatory"`: everyone who meets the rule, then the status quo among the
#'   rest.
#'
#' Build the policy with [design_retirement_policy_status_quo()],
#' [design_retirement_policy_eligibility()] or
#' [design_retirement_policy_mandatory()].
#'
#' @param contract_dt data.table. Contracts at `as_of_date`.
#' @param personnel_dt data.table. People at `as_of_date`, with an age column
#'   and a tenure column (named in `column_spec_args`) when the policy uses age
#'   or tenure. [simulate_horizon()] fills both in for you.
#' @param policy_params List. A retirement policy; its `pension` part is not used
#'   here, so the policy you pass to [simulate_retirement()] works as it is.
#' @param as_of_date Date. The date at which people retire.
#' @param column_spec_args Column names, from [define_column_spec()].
#'
#' @return A data.table with one row per retiree: the person ID, `retire` (always
#'   1), `age`, `tenure_years` and `retire_reason` (`"rule"` or `"status_quo"`).
#'   No rows when nobody retires.
#'
#' @export
select_retirees <- function(contract_dt,
                            personnel_dt,
                            policy_params,
                            as_of_date,
                            column_spec_args = define_column_spec()) {
  # A NULL age_col / tenure_col means the default name, as in simulate_horizon()
  cols   <- .fill_age_tenure_col_names(column_spec_args)

  # Stop early, with a message that says what to fix, if the policy or the data
  # cannot work (e.g. a rule with no age, or no age column in personnel_dt)
  check_retirement_selection_inputs(contract_dt, personnel_dt, policy_params,
                                    as_of_date, cols)
  id_col <- cols$personnel_id_col

  # 1. The contracts that count: started, not ended, and not already a pension
  #    or inactive. This and the pool below are subsets, so new tables: the
  #    caller's data is never changed.
  active <- get_active_contracts(contract_dt, as_of_date,
                                 cols$start_date_col, cols$end_date_col,
                                 cols$contract_type_col)

  # 2. A policy for some contract types only (e.g. "permanent") keeps those
  #    types. A blank type is missing data, so it counts as staff and stays.
  if (!is.null(policy_params$active_types)) {
    active <- active[get(cols$contract_type_col) %in% c(policy_params$active_types, NA)]
  }

  # 3. The pool: one row per person who holds one of those contracts and does
  #    not already draw a pension. %in% keeps a blank status (== would drop it).
  pool <- personnel_dt[
    get(id_col) %in% active[[id_col]] & !get(cols$status_col) %in% "pensioner"
  ]

  # 4. Add what the policy looks people up by: whole-year age and tenure, and
  #    any group column held on contracts (e.g. occupation), taken from each
  #    person's main contract. `active` is passed so it is not worked out again.
  pool <- .add_policy_columns(pool, active, policy_params, cols)

  # 5. Who retires, by the kind of policy. Each returns a subset of the pool's
  #    rows (ID, age, tenure_years, retire_reason), so an empty pool or nobody
  #    retiring gives 0 rows of the same shape, with no special case needed.
  retirees <- 
    switch(policy_params$type,
           status_quo  = compute_status_quo_retirements(pool, policy_params$rates, cols),
           eligibility = compute_eligibility_retirements(pool, policy_params$rule, cols),
           mandatory   = compute_mandatory_retirements(pool, policy_params$rates,
                                                      policy_params$rule, cols)
  )

  # 6. The shape prepare_retiree_data() expects: ID, retire = 1, age,
  #    tenure_years (plus retire_reason, which it ignores). Both are set by
  #    reference, so nothing is copied.
  retirees[, retire := 1L]
  data.table::setcolorder(retirees, c(id_col, "retire"))
  retirees
}


#' Add the Columns a Retirement Policy Looks People Up By
#'
#' @description
#' The pool comes from `personnel_dt`, one row per person. The policy's rate
#' table and rule look people up by columns the pool may not have yet, or has
#' under the data's own names. This adds them:
#' * `age` and `tenure_years`: the age and tenure columns under fixed names, so
#'   a rule can always say `age >= min_age`, whatever the data call them. They
#'   are read, not computed: [simulate_horizon()] sets them once at the start
#'   and adds a year each year.
#' * `age_yr_int` and `tenure_yr_int`: the same in whole years, which the rate
#'   tables are keyed by.
#' * Group columns kept on contracts rather than on people (e.g. `occupation`,
#'   `est_id`), and the `retire_strategy` column when it is one of those. They
#'   are taken from each person's main contract ([get_primary_contract()], the
#'   same contract [prepare_retiree_data()] takes pay from), so someone with two
#'   contracts still gets one value.
#'
#' An age or tenure column the data do not have is left blank; the input check
#' in [select_retirees()] stops first if the policy needs it.
#'
#' @param pool data.table. The people who can retire, one row per person, from
#'   `personnel_dt`. The age and tenure columns are added to it in place.
#' @param active data.table. Their active contracts, from [select_retirees()].
#' @param policy_params List. The retirement policy; its `rates` and `rule`
#'   say which group columns are needed.
#' @param column_spec_args Column names, from [define_column_spec()].
#'
#' @return The pool with the added columns. Use the returned table: when
#'   contract columns are added it is a new table, not `pool`.
#'
#' @keywords internal
.add_policy_columns <- function(pool, active, policy_params, column_spec_args) {
  cols   <- column_spec_args
  id_col <- cols$personnel_id_col

  # 1. Age and tenure under fixed names, and in whole years. A column the data
  #    do not have gives NA (isTRUE() also covers a column name set to NULL).
  .age    <- if (isTRUE(cols$age_col    %in% names(pool))) pool[[cols$age_col]]    else NA_real_
  .tenure <- if (isTRUE(cols$tenure_col %in% names(pool))) pool[[cols$tenure_col]] else NA_real_
  pool[, `:=`(age           = .age,
              tenure_years  = .tenure,
              age_yr_int    = as.integer(.age),
              tenure_yr_int = as.integer(.tenure))]

  # 2. The columns the rates, the rule and the draw use that the pool does not
  #    have yet: these live on contracts
  strategy      <- policy_params$rates$defaults$retire_strategy %||% "random"
  wanted        <- unique(c(policy_params$rates$group_cols, policy_params$rule$group_cols,
                            setdiff(strategy, "random")))
  from_contract <- setdiff(wanted, names(pool))
  if (length(from_contract) == 0L) return(pool)

  # 3. Take them from each person's main contract: one row per person, so the
  #    join keeps one row per person in the pool
  main <- get_primary_contract(active, id_col, cols$contract_id_col,
                               cols$start_date_col, cols$salary_col)
  main[, c(id_col, from_contract), with = FALSE][pool, on = id_col]
}


#' Retire Everyone Who Meets a Rule
#'
#' @description
#' Everyone in the pool who meets the retirement rule retires, with certainty.
#' [select_retirees()] calls it for an eligibility policy, and
#' [compute_mandatory_retirements()] calls it as the first step of a mandatory
#' one.
#'
#' The rule is one of four kinds, set in `rule$defaults$kind`:
#' * `"age"`: `age >= min_age`
#' * `"tenure"`: `tenure_years >= min_tenure`
#' * `"age_tenure"`: both
#' * `"custom"`: the user's own expression in `rule$defaults$custom_rule`, e.g.
#'   `"age >= min_age | tenure_years >= min_tenure"`. It is checked against an
#'   allow-list of safe calls before it runs, and can use only the pool's
#'   columns, `min_age` and `min_tenure`.
#'
#' `min_age` and `min_tenure` can differ by group: a row of `rule$policy_table`
#' (keyed by `rule$group_cols`, e.g. occupation) sets them for that group, and
#' `rule$defaults` sets them for everyone else. A person whose age or tenure is
#' missing cannot be tested, and does not retire.
#'
#' @param pool data.table. The people who can retire, one row per person, with
#'   `age`, `tenure_years` and the rule's group columns (see [select_retirees()]).
#'   Not changed.
#' @param rule List. The rule part of a retirement policy, from a
#'   `retirement_rule_*()` constructor.
#' @param column_spec_args Column names, from [define_column_spec()].
#'
#' @return A data.table with one row per retiree: the person ID, `age`,
#'   `tenure_years` and `retire_reason = "rule"`. No rows when nobody meets the
#'   rule.
#'
#' @keywords internal
compute_eligibility_retirements <- function(pool, rule, column_spec_args) {
  id_col <- column_spec_args$personnel_id_col

  # 1. Each person's thresholds: their group's row in the rule table (e.g. 70
  #    for teachers), or the rule's default for everyone else. Only the
  #    thresholds the rule sets are looked up; a custom rule may set none.
  params     <- intersect(c("min_age", "min_tenure"),
                          c(names(rule$defaults), names(rule$policy_table)))
  thresholds <- resolve_policy_table(rule, pool, params)

  # 2. The rule as an R expression over the columns age, tenure_years, min_age
  #    and min_tenure. A custom rule is the user's own text: parsed and checked
  #    against the allow-list of safe calls, and its column names against the
  #    pool, here where it runs.
  rule_expr <- switch(rule$defaults$kind,
    age        = quote(age >= min_age),
    tenure     = quote(tenure_years >= min_tenure),
    age_tenure = quote(age >= min_age & tenure_years >= min_tenure),
    custom     = validate_eligibility_rule(rule$defaults$custom_rule,
                                           c(names(pool), params))
  )

  # 3. Test everyone at once. The rule sees only the pool's columns and the
  #    thresholds (enclos = baseenv() keeps it from reaching the caller's
  #    variables), and nothing is added to the pool.
  meets <- eval(rule_expr, c(pool, thresholds), enclos = baseenv())

  # 4. which() drops a missing result (e.g. no age): a person the rule cannot
  #    be tested on does not retire.
  retirees <- pool[which(meets), c(id_col, "age", "tenure_years"), with = FALSE]
  retirees[, retire_reason := "rule"]
  retirees
}


#' Retire by a Mandatory Rule, Then by the Status Quo
#'
#' @description
#' A mandatory rule on top of the status quo. Everyone who meets the rule (e.g.
#' everyone aged 75 or over) retires with certainty; the rest of the pool then
#' retire at the status-quo rates, as in [compute_status_quo_retirements()].
#'
#' The people retired by the rule leave the pool before the rates are applied,
#' so they are not counted a second time in their group's draw: a 76-year-old
#' retired by the rule does not also use up one of the age-76 retirements.
#'
#' @param pool data.table. The people who can retire, one row per person (see
#'   [select_retirees()]). Not changed.
#' @param rates List. The rates part of a retirement policy, from
#'   [retirement_rates_status_quo()].
#' @param rule List. The rule part of a retirement policy, from a
#'   `retirement_rule_*()` constructor.
#' @param column_spec_args Column names, from [define_column_spec()].
#'
#' @return A data.table with one row per retiree: the person ID, `age`,
#'   `tenure_years` and `retire_reason` (`"rule"` or `"status_quo"`).
#'
#' @keywords internal
compute_mandatory_retirements <- function(pool, rates, rule, column_spec_args) {
  id_col <- column_spec_args$personnel_id_col

  # 1. Everyone who meets the rule retires for certain
  forced <- compute_eligibility_retirements(pool, rule, column_spec_args)

  # 2. The forced retirees leave the pool before the rates are applied, so they
  #    are not counted a second time in their group's draw
  rest <- compute_status_quo_retirements(pool[!get(id_col) %in% forced[[id_col]]],
                                         rates, column_spec_args)

  rbind(forced, rest)
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
#' @param expr_col Character. Column holding expression strings
#'   (\code{"eligibility_rule"} or \code{"pension_formula"}); also used to
#'   label errors.
#' @param result_col Character. Destination column for evaluated results.
#' @param allowed_calls Character vector. The allow-list every expression is
#'   checked against (via \code{.check_safe_calls()}) immediately before it is
#'   evaluated: \code{.ALLOWED_RULE_CALLS} (the default) for eligibility
#'   rules, \code{.ALLOWED_FORMULA_CALLS} for pension formulas. Checking here,
#'   at the single point of evaluation, means no caller can skip the sandbox.
#'
#' @return Invisibly returns the modified \code{dt}.
#'
#' @keywords internal
.eval_per_row_expression <- function(dt, mask, expr_col, result_col,
                                     allowed_calls = .ALLOWED_RULE_CALLS,
                                     init_value = NA) {
  # Initialize output; rows outside mask stay NA.
  dt[, (result_col) := init_value]
  if (!any(mask)) {
    return(invisible(dt))
  }

  unique_exprs <- unique(dt[[expr_col]][mask]) ## set the unique user input expressions
  ## parse each unique expression
  for (e in unique_exprs) {
    parsed <- str2lang(e)

    # Sandbox: refuse to evaluate anything outside the allow-list, whichever
    # path the expression arrived by.
    .check_safe_calls(parsed, allowed_calls, label = expr_col)

    cols <- all.vars(parsed) ## get the column names within the expression
    subrows <- mask & dt[[expr_col]] == e ## get the set of row each expression applies to

    # Evaluate in the row context of required columns only.
    dt[subrows, (result_col) := eval(parsed, .SD), .SDcols = cols]
  }

  invisible(dt)
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
#' Enriches the retirees from \code{\link{select_retirees}} with
#' salary and contract information required for pension formula evaluation.
#' Fills any \code{NA} age or tenure values (e.g. when the data hold no tenure
#' column, so \code{select_retirees()} left \code{tenure_years} blank) from the
#' source data, and selects one primary contract per retiree via
#' \code{\link{get_primary_contract}}.  Their \code{salary_col} is then replaced
#' by their pay on all their active contracts added up, because a retiree leaves
#' every job they hold.  Their other columns come from the main contract.
#'
#' @param eligibility_dt data.table.  Output of \code{\link{select_retirees}}
#'   with columns \code{personnel_id_col}, \code{retire}, \code{age},
#'   \code{tenure_years} (and \code{retire_reason}, which is kept).  Only rows where \code{retire == 1} are processed;
#'   the rest are ignored.
#' @param contract_dt data.table.  Full contract register (not pre-filtered).
#'   Active contracts are extracted internally via
#'   \code{\link{get_active_contracts}}.
#' @param personnel_dt data.table.  Personnel register.  Used to compute
#'   missing age values when \code{anyNA(eligibility_dt$age)} is \code{TRUE}.
#' @param as_of_date Date.  Reference date passed to age/tenure computations
#'   when filling \code{NA} values.
#' @param column_spec_args Complete column specification
#'   (\code{define_column_spec()}), in the caller's column names. Keys read:
#'   \code{personnel_id_col}, \code{birth_date_col}, \code{contract_id_col},
#'   \code{start_date_col}, \code{end_date_col}, \code{contract_type_col},
#'   and \code{salary_col} (picks the primary contract; the pension wage base).
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
#' @return data.table.  One row per retiree with all primary contract columns,
#'   \code{salary_col} holding their pay on all contracts added up (blank only if
#'   every contract is blank), plus \code{age} and \code{tenure_years}.
#'   Returns an empty \code{data.table()} (zero rows, no columns) when
#'   \code{eligibility_dt} contains no retirees.
#' @keywords internal
prepare_retiree_data <- function(eligibility_dt,
                                 contract_dt,
                                 personnel_dt,
                                 as_of_date,
                                 column_spec_args = define_column_spec()) {
  personnel_id_col  <- column_spec_args$personnel_id_col
  birth_date_col    <- column_spec_args$birth_date_col
  contract_id_col   <- column_spec_args$contract_id_col
  start_date_col    <- column_spec_args$start_date_col
  end_date_col      <- column_spec_args$end_date_col
  salary_col        <- column_spec_args$salary_col
  contract_type_col <- column_spec_args$contract_type_col

  # Filter to eligible retirees only
  retirees_only <- eligibility_dt[retire == 1]

  # If no retirees, return empty
  if (nrow(retirees_only) == 0) {
    return(data.table::data.table())
  }

  # select_retirees() reads age and tenure from the data and leaves them NA when
  # the data hold none. Pension formulas (e.g. DB) may need both, so fill any
  # missing values now from the source data.
  if (anyNA(retirees_only$age)) {
    age_dt <- compute_age(
      personnel_dt = personnel_dt,
      as_of_date = as_of_date,
      birth_date_col = birth_date_col,
      personnel_id_col = personnel_id_col
    )
    retirees_only[age_dt, age := i.age, on = personnel_id_col]
  }

  if (anyNA(retirees_only$tenure_years)) {
    tenure_dt <- compute_tenure(
      contract_dt = contract_dt,
      as_of_date = as_of_date,
      personnel_id_col = personnel_id_col,
      contract_id_col = contract_id_col,
      start_date_col = start_date_col,
      end_date_col = end_date_col,
      contract_type_col = contract_type_col
    )
    retirees_only[
      tenure_dt,
      tenure_years := i.tenure_years,
      on = personnel_id_col
    ]
  }

  # Get active contracts for these retirees
  active_contracts <- get_active_contracts(
    contract_dt = contract_dt,
    as_of_date = as_of_date,
    start_date_col = start_date_col,
    end_date_col = end_date_col,
    contract_type_col = contract_type_col
  )

  # Filter to retirees only
  retiree_contracts <- active_contracts[
    get(personnel_id_col) %in% retirees_only[[personnel_id_col]]
  ]

  # Get primary contract for each retiree
  primary_contracts <- get_primary_contract(
    contract_dt = retiree_contracts,
    personnel_id_col = personnel_id_col,
    contract_id_col = contract_id_col,
    start_date_col = start_date_col,
    salary_col = salary_col
  )

  # The person's pay is what they earn on all their contracts: a retiree with two
  # jobs leaves both, so both count towards the pension and the savings.
  total_pay <- retiree_contracts[, .(pay = .total_pay(get(salary_col))), by = personnel_id_col]
  primary_contracts[, (salary_col) := total_pay[.SD, on = personnel_id_col, pay]]

  # Add age, tenure and, from select_retirees(), why each person retires
  keep <- intersect(c(personnel_id_col, "age", "tenure_years", "retire_reason"),
                    names(retirees_only))
  retirees_dt <- primary_contracts[retirees_only[, keep, with = FALSE], on = personnel_id_col]

  return(retirees_dt)
}


# =============================================================================
# Status-quo rates — estimate_retirement_hazard_rates()
# =============================================================================

#' Estimate Retirement Rates from Panel Data
#'
#' @description
#' The share of people who retired between one snapshot and the next, by
#' whole-year age, whole-year tenure, or both, and by any other `group_cols`.
#' Everyone active at a snapshot counts towards their group's total; those who
#' are pensioners at the next snapshot count as retiring. The rates are pooled
#' over every pair of snapshots, weighted by how many people each pair had. The
#' counting is done by `govhr::estimate_decrement_rates()`.
#'
#' The result is the rate table of [retirement_rates_status_quo()]:
#' [simulate_horizon()] calls this function for you (through
#' `precompute_retirement_rates()`) when a status-quo or mandatory policy has no
#' rates of its own.
#'
#' @param panel_personnel_dt data.table. The personnel panel: every snapshot,
#'   with the person ID, snapshot date and status columns, and every column in
#'   `group_cols`. `age_yr_int` and `tenure_yr_int` are worked out from the age
#'   and tenure columns when the panel does not hold them. A group column kept
#'   on contracts (e.g. `est_id`) must already be on the panel;
#'   [simulate_horizon()] adds it for you.
#' @param group_cols Character. What the rates vary by: `"age_yr_int"`,
#'   `"tenure_yr_int"` or both, plus any other columns (e.g. `"est_id"`). The
#'   `group_cols` of [retirement_rates_status_quo()].
#' @param retire_status Character. The status that marks a retiree. Default
#'   `"pensioner"`.
#' @param smooth Logical. `TRUE` smooths the rates along age (or tenure) with
#'   `govhr::smooth_decrement_rates()` before they are returned, which helps
#'   when groups are small. Default `FALSE`: the rates as observed.
#' @param column_spec_args Column names, from [define_column_spec()]. Keys read:
#'   `personnel_id_col`, `ref_date_col`, `status_col`, and `age_col` /
#'   `tenure_col` when `age_yr_int` / `tenure_yr_int` must be worked out
#'   (`NULL` means the default name, `"age"` / `"personnel_tenure"`).
#'
#' @return A data.table with the `group_cols` and `hazard_rate`: one row per
#'   group seen in the panel.
#'
#' @seealso [retirement_rates_status_quo()] for using the rates,
#'   [compute_status_quo_retirements()] for how they are applied.
#'
#' @export
estimate_retirement_hazard_rates <- function(panel_personnel_dt,
                                             group_cols         = NULL,
                                             retire_status      = "pensioner",
                                             smooth             = FALSE,
                                             column_spec_args   = define_column_spec()) {
  fn <- "estimate_retirement_hazard_rates"
  if (!any(c("age_yr_int", "tenure_yr_int") %in% group_cols)) {
    stop(fn, ": group_cols must include 'age_yr_int', 'tenure_yr_int' or both: ",
         "retirement rates vary by age, tenure, or both.", call. = FALSE)
  }
  .check_column_spec(column_spec_args)
  # A NULL age_col / tenure_col means the default name, as in simulate_horizon()
  cols <- .fill_age_tenure_col_names(column_spec_args)
  .check_spec_columns(NULL, panel_personnel_dt, cols, required = list(
    personnel_dt = c("personnel_id_col", "ref_date_col", "status_col")
  ))

  # 1. Whole-year age and tenure, from the age and tenure columns, where the
  #    panel does not hold them yet. On a copy: the caller's panel is not changed.
  panel <- data.table::setDT(data.table::copy(panel_personnel_dt))
  for (key in intersect(c("age_yr_int", "tenure_yr_int"), group_cols)) {
    if (key %in% names(panel)) next
    source_key <- if (key == "age_yr_int") "age_col" else "tenure_col"
    .check_spec_columns(NULL, panel, cols, required = list(personnel_dt = source_key))
    panel[, (key) := as.integer(get(cols[[source_key]]))]
  }

  # 2. The rates run along age when they use age, along tenure otherwise; govhr
  #    calls that the duration, and groups by it as well as by the other keys
  duration_col <- if ("age_yr_int" %in% group_cols) "age_yr_int" else "tenure_yr_int"
  other_cols   <- setdiff(group_cols, duration_col)

  decrements <- govhr::estimate_decrement_rates(
    personnel        = panel,
    duration_col     = duration_col,
    status_col       = cols$status_col,
    personnel_id_col = cols$personnel_id_col,
    ref_date_col     = cols$ref_date_col,
    group_cols       = other_cols
  )

  # 3. Optional smoothing. It must see every outcome (retired, left, stayed)
  #    for each duration, so it runs before the retirements are picked out.
  if (isTRUE(smooth)) {
    decrements <- govhr::smooth_decrement_rates(
      decrements   = decrements,
      duration_col = duration_col,
      status_col   = cols$status_col,
      group_cols   = other_cols
    )
  }

  # 4. Keep the retirements: their rate is the hazard rate
  result <- decrements[get(cols$status_col) == retire_status]
  data.table::setnames(result, "decrement_rate", "hazard_rate")
  result[, c(group_cols, "hazard_rate"), with = FALSE]
}


#' Give Groups Older Than the History the Oldest Rate
#'
#' @description
#' Status-quo rates come from the history, so an age only has a rate if someone
#' that age was seen retiring. Without this step, everyone older than that would
#' get a rate of 0 and never retire. Here, groups past the oldest age with a rate
#' above 0 take that rate instead: if 71 is the oldest age seen retiring, a
#' 75-year-old retires at the age-71 rate.
#'
#' This is not a mandatory age: the rate carries on, so only that share retire
#' each year. Groups inside the history's range with no rate keep the default.
#'
#' The rates run along `duration_col`: age, or tenure when the rates are by
#' tenure only. The oldest rate is found separately for each combination of the
#' other keys, e.g. for each tenure year when the rates are by age and tenure.
#'
#' @param groups_dt data.table. One row per group, with the `group_cols` and a
#'   `hazard_rate` column. Changed in place.
#' @param rate_table data.table. The rate table, with the `group_cols` and
#'   `hazard_rate`. `NULL` leaves `groups_dt` as it is.
#' @param group_cols Character. The keys of the rate table.
#' @param duration_col Character. The key the rates run along:
#'   `"age_yr_int"` (default) or `"tenure_yr_int"`.
#'
#' @return `groups_dt`, invisibly, with `hazard_rate` filled in past the oldest
#'   rate.
#'
#' @keywords internal
.apply_oldest_age_rate <- function(groups_dt, rate_table, group_cols,
                                   duration_col = "age_yr_int") {
  if (is.null(rate_table)) return(invisible(groups_dt))
  rated <- rate_table[hazard_rate > 0]
  if (nrow(rated) == 0L) return(invisible(groups_dt))

  # The last duration with a rate above 0, and that rate, per combination of
  # the other keys
  other_cols <- setdiff(group_cols, duration_col)
  last <- rated[rated[, .I[which.max(get(duration_col))], by = other_cols]$V1,
                c(other_cols, duration_col, "hazard_rate"), with = FALSE]
  data.table::setnames(last, c(duration_col, "hazard_rate"), c(".last_duration", ".last_rate"))

  # Groups past that duration take that rate
  if (length(other_cols) > 0L) {
    groups_dt[last, c(".last_duration", ".last_rate") := list(i..last_duration, i..last_rate),
              on = other_cols]
  } else {
    groups_dt[, c(".last_duration", ".last_rate") := list(last$.last_duration, last$.last_rate)]
  }
  groups_dt[get(duration_col) > .last_duration, hazard_rate := .last_rate]
  groups_dt[, c(".last_duration", ".last_rate") := NULL]
  invisible(groups_dt)
}


#' Retire at the Status-Quo Rates
#'
#' @description
#' In each group of the rate table (e.g. each age, or each age and tenure year),
#' the share of people that retired in the past retire now. [select_retirees()]
#' calls it for a status-quo policy, and [compute_mandatory_retirements()] calls
#' it after the rule.
#'
#' For each group:
#' 1. Count the people in it.
#' 2. Look up its rate in `rates$policy_table`. A group with no row gets the
#'    default rate (0); a group older than the history gets the oldest rate (see
#'    [.apply_oldest_age_rate()]).
#' 3. Multiply people by rate, and round up or down at random in proportion to
#'    the fraction (see [stochastic_round()]): 40 people at 0.0625 is 2.5, so 2
#'    or 3 retire, each half the time.
#' 4. Pick that many members with [.select_personnel_subset()]: at random, or
#'    lowest first on the column named in `rates$defaults$retire_strategy`.
#'
#' The draw is random, so set a seed (e.g. `simulate_horizon(seed = )`) for
#' results you can reproduce.
#'
#' @param pool data.table. The people who can retire, one row per person, with
#'   `age`, `tenure_years` and the rate table's keys (see [select_retirees()]).
#'   Not changed.
#' @param rates List. The rates part of a retirement policy, from
#'   [retirement_rates_status_quo()]: `group_cols` (always `age_yr_int` or
#'   `tenure_yr_int`, plus any others), `policy_table` (a `hazard_rate` per
#'   group) and `defaults` (`hazard_rate`, `retire_strategy`).
#' @param column_spec_args Column names, from [define_column_spec()].
#'
#' @return A data.table with one row per retiree: the person ID, `age`,
#'   `tenure_years` and `retire_reason = "status_quo"`. No rows when nobody is
#'   drawn.
#'
#' @keywords internal
compute_status_quo_retirements <- function(pool, rates, column_spec_args) {
  id_col     <- column_spec_args$personnel_id_col
  group_cols <- rates$group_cols
  strategy   <- rates$defaults$retire_strategy %||% "random"

  # 1. How many people are in each group (e.g. 40 people aged 58)
  groups <- pool[, .(n_active = .N), by = group_cols]

  # 2. Each group's rate: its row in the rate table, or the default (0) for a
  #    group the history has no rate for. Groups older than the history take the
  #    oldest rate (see .apply_oldest_age_rate()).
  groups[, hazard_rate := resolve_policy_table(rates, groups, "hazard_rate")$hazard_rate]
  .apply_oldest_age_rate(groups, rates$policy_table, group_cols,
                         duration_col = if ("age_yr_int" %in% group_cols) "age_yr_int"
                                        else "tenure_yr_int")

  # 3. How many retire in each group: people x rate, rounded up or down at
  #    random in proportion to the fraction (40 x 0.0625 = 2.5 gives 2 or 3,
  #    each half the time), so the expected number is right
  groups[, n_retire := stochastic_round(n_active * hazard_rate)]

  # 4. Who retires: in each group with someone to retire, pick n_retire of its
  #    members, at random or lowest first on the retire_strategy column. Only
  #    the ID and that column are passed to the picker, to keep each group small.
  #    When no group has anyone to retire (an empty pool, or every rate 0),
  #    picked stays NULL: data.table would otherwise still run the picker once
  #    on the empty table, with no n_retire to give it.
  sd_cols <- unique(c(id_col, setdiff(strategy, "random")))
  to_pick <- groups[n_retire > 0L]
  picked  <- if (nrow(to_pick) > 0L) {
    pool[to_pick, on = group_cols, nomatch = NULL][,
      .(id = .select_personnel_subset(get(id_col), .SD, n_retire[1L], strategy, id_col)),
      by = group_cols, .SDcols = sd_cols]
  }

  # 5. The retirees' rows from the pool
  retirees <- pool[get(id_col) %in% picked$id, c(id_col, "age", "tenure_years"), with = FALSE]
  retirees[, retire_reason := "status_quo"]
  retirees
}

#' Estimate Each Retiree's Pension as a Share of Their Last Salary
#'
#' @description
#' For everyone the panel shows retiring, compares their first pension with
#' their last salary. This is the replacement rate a status-quo pension can be
#' built from: what retirement has paid in the past, as a share of pay.
#'
#' A retirement is a person who is a pensioner at some snapshot and active at
#' an earlier one:
#' * **first pension**: their pay at the first snapshot where they are a
#'   pensioner;
#' * **last salary**: their pay at the latest snapshot before that where they
#'   are active, however many periods earlier (a pension can start some
#'   periods after the last salary).
#'
#' Pay is the total on all their contracts at that snapshot, as
#' [prepare_retiree_data()] counts it. People seen only as pensioners (retired
#' before the panel starts) have no salary to compare, and are left out.
#'
#' `periods_apart` says how many snapshots separate the two. Over a long gap,
#' pay growth makes the rate look higher, and a very long gap may be someone
#' who left and later drew a pension rather than a retirement; the step that
#' turns these into rates by group can leave such cases out.
#'
#' This function belongs with the data measures of the **govhr** package and is
#' meant to move there.
#'
#' @param contract_dt data.table. The contract panel: every snapshot.
#' @param personnel_dt data.table. The personnel panel: every snapshot, with
#'   each person's status.
#' @param group_cols Character or `NULL`. Columns to describe each retiree by,
#'   as they stood at their **last active snapshot** (the job they retired
#'   from, since pension rows say little about it). A column on
#'   `personnel_dt` (e.g. `"gender"`) comes from the person's row; a column on
#'   `contract_dt` only (e.g. `"est_id"`, `"occupation"`) comes from their main
#'   contract, the one [prepare_retiree_data()] uses, so someone with two
#'   contracts is counted in one group. Values are returned as they are: to
#'   group by whole years of age, cut the age column afterwards. Default
#'   `NULL`: no groups.
#' @param column_spec_args Column names, from [define_column_spec()]. Keys read:
#'   `personnel_id_col`, `ref_date_col`, `status_col`, `salary_col`; with a
#'   contract column in `group_cols`, also `contract_id_col` and
#'   `start_date_col` (to find the main contract).
#'
#' @return A data.table with one row per retirement: the person ID, the
#'   `group_cols`, `last_date`, `pension_date`, `periods_apart`, `last_salary`,
#'   `first_pension` and `replacement_rate` (`first_pension / last_salary`;
#'   blank when either pay is missing or 0).
#'
#' @examples
#' \dontrun{
#' rates <- estimate_replacement_rates(bra_hrmis_contract, bra_hrmis_personnel,
#'                                     group_cols = "est_id")
#' rates[periods_apart == 1L, .(median_rate = median(replacement_rate, na.rm = TRUE)),
#'       by = est_id]
#' }
#'
#' @export
estimate_replacement_rates <- function(contract_dt,
                                       personnel_dt,
                                       group_cols = NULL,
                                       column_spec_args = define_column_spec()) {
  .check_column_spec(column_spec_args)
  cols <- column_spec_args
  validate_datatable(contract_dt, "contract_dt")
  validate_datatable(personnel_dt, "personnel_dt")
  .check_spec_columns(contract_dt, personnel_dt, cols, required = list(
    contract_dt  = c("personnel_id_col", "ref_date_col", "salary_col"),
    personnel_dt = c("personnel_id_col", "ref_date_col", "status_col")
  ))

  # Each person's status at each snapshot, under short fixed names: only the
  # three columns needed, so the copy stays small
  status <- personnel_dt[, .(id     = get(cols$personnel_id_col),
                             date   = get(cols$ref_date_col),
                             status = get(cols$status_col))]

  # 1. Each pensioner's first pension date
  retirees <- status[status %in% "pensioner", .(pension_date = min(date)), by = id]

  # 2. Their last active date before it. A rolling join (roll = TRUE) takes,
  #    for each pensioner, the latest active snapshot on or before the pension
  #    date, however far back. It cannot be the pension date itself: the person
  #    is a pensioner then.
  active <- status[status %in% "active"]
  retirees[, last_date := active[retirees, on = .(id, date = pension_date), roll = TRUE, x.date]]
  retirees <- retirees[!is.na(last_date)]  # seen only as pensioners: no salary to compare

  snapshots <- sort(unique(status$date))
  retirees[, periods_apart := match(pension_date, snapshots) - match(last_date, snapshots)]

  # 3. The groups they retired from, as at their last active snapshot: a
  #    personnel column from their own row, a contract column from their main
  #    contract (so someone with two contracts is counted once)
  people_cols   <- intersect(group_cols, names(personnel_dt))
  contract_cols <- setdiff(group_cols, people_cols)
  if (length(people_cols) > 0L) {
    retirees[personnel_dt, (people_cols) := mget(paste0("i.", people_cols)),
             on = c(id = cols$personnel_id_col, last_date = cols$ref_date_col)]
  }
  if (length(contract_cols) > 0L) {
    validate_columns_exist(contract_dt, contract_cols, "contract_dt or personnel_dt")
    at_last <- contract_dt[.(retirees$id, retirees$last_date),
                           on = c(cols$personnel_id_col, cols$ref_date_col), nomatch = NULL]
    main <- get_primary_contract(at_last, cols$personnel_id_col, cols$contract_id_col,
                                 cols$start_date_col, cols$salary_col)
    retirees[main, (contract_cols) := mget(paste0("i.", contract_cols)),
             on = c(id = cols$personnel_id_col)]
  }

  # 4. Pay on all contracts, added up only for these people at these dates
  pay_on <- function(dates) {
    contract_dt[.(retirees$id, dates), on = c(cols$personnel_id_col, cols$ref_date_col),
                sum(get(cols$salary_col), na.rm = TRUE), by = .EACHI]$V1
  }
  retirees[, `:=`(last_salary = pay_on(last_date), first_pension = pay_on(pension_date))]

  # 5. The pension as a share of the last salary. A 0 means no pay was found,
  #    so the rate is left blank rather than 0 or infinite.
  retirees[last_salary > 0 & first_pension > 0, replacement_rate := first_pension / last_salary]

  data.table::setnames(retirees, "id", cols$personnel_id_col)
  data.table::setcolorder(retirees, c(cols$personnel_id_col, group_cols, "last_date", "pension_date"))
  retirees[]
}
