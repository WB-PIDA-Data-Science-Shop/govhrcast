#' Retirement Policy Constructor Helpers
#'
#' @description
#' Small, composable constructors for building the `retirement_policy` argument
#' to [simulate_retirement()], [select_retirees()] and [simulate_horizon()]
#' without hand-writing the list. Each checks its own inputs when you call it,
#' so a mistake stops where you made it rather than deep inside a simulation.
#'
#' A retirement policy has three kinds, one constructor each:
#' * [design_retirement_policy_status_quo()]: people retire at the rates seen in
#'   the history.
#' * [design_retirement_policy_eligibility()]: everyone who meets a rule retires.
#' * [design_retirement_policy_mandatory()]: everyone who meets a rule retires,
#'   and the rest retire at the historical rates.
#'
#' They are built from three parts:
#' 1. **Who retires**: a rule from a `retirement_rule_*()` function (by age,
#'    tenure, both, or your own expression), and/or rates from
#'    [retirement_rates_status_quo()].
#' 2. **What they are paid**: a pension from a `pension_*()` function.
#' 3. Optionally, `active_types`: the contract types the policy covers.
#'
#' Every constructor returns a plain list, so you can also edit what it returns,
#' or write the list yourself.
#'
#' @import data.table
#' @name retirement_policy_helpers
#' @keywords internal
NULL


# =============================================================================
# Who retires: rules and rates
# =============================================================================

#' Assemble a Retirement Rule
#'
#' @description
#' Shared by the `retirement_rule_*()` functions: checks the group columns and
#' the rule table, and returns the rule in the three-slot shape
#' (`group_cols`, `policy_table`, `defaults`). Thresholds left `NULL` are dropped
#' from `defaults`, so a rule only carries the ones it sets.
#'
#' @param fn Character. The calling function's name, for error messages.
#' @param defaults List. `kind` plus the thresholds the rule sets.
#' @param group_cols Character or `NULL`. Columns the rule table is keyed by.
#' @param policy_table data.table or `NULL`. Thresholds per group.
#'
#' @return The rule list.
#'
#' @keywords internal
.build_retirement_rule <- function(fn, defaults, group_cols, policy_table) {
  if (!is.null(group_cols) && !is.character(group_cols)) {
    stop(fn, ": group_cols must be a character vector of column names, or NULL.", call. = FALSE)
  }
  if (!is.null(policy_table)) {
    if (!data.table::is.data.table(policy_table)) {
      stop(fn, ": policy_table must be a data.table.", call. = FALSE)
    }
    missing_cols <- setdiff(group_cols, names(policy_table))
    if (length(missing_cols) > 0L) {
      stop(fn, ": policy_table is missing the group column(s) ",
           paste(missing_cols, collapse = ", "), ".", call. = FALSE)
    }
  }
  list(group_cols   = group_cols,
       policy_table = policy_table,
       defaults     = Filter(Negate(is.null), defaults))
}


#' Retirement Rule: By Age
#'
#' @description
#' Everyone aged `min_age` or over retires. To set a different age for some
#' groups (e.g. teachers at 70, everyone else at 75), give a `policy_table` with
#' the group column and a `min_age` column; `min_age` here applies to every
#' group the table does not list.
#'
#' Use the rule in [design_retirement_policy_eligibility()] (everyone who meets
#' it retires) or [design_retirement_policy_mandatory()] (the same, on top of
#' the historical rates).
#'
#' @param min_age Numeric. The retirement age, for everyone or for every group
#'   not in `policy_table`.
#' @param group_cols Character or `NULL`. Columns `policy_table` is keyed by,
#'   e.g. `"occupation"`. A column held on contracts is taken from each person's
#'   main contract.
#' @param policy_table data.table or `NULL`. One row per group, with the
#'   `group_cols` and `min_age`.
#'
#' @return A rule list, to pass as `rule` to a `design_retirement_policy_*()`
#'   function.
#' @seealso [retirement_rule_tenure()], [retirement_rule_age_tenure()],
#'   [retirement_rule_custom()]
#' @examples
#' retirement_rule_age(60)
#' retirement_rule_age(75, group_cols = "occupation",
#'                     policy_table = data.table::data.table(occupation = "teacher", min_age = 70))
#' @export
retirement_rule_age <- function(min_age, group_cols = NULL, policy_table = NULL) {
  validate_positive_number(min_age, "min_age")
  .build_retirement_rule("retirement_rule_age", list(kind = "age", min_age = min_age),
                         group_cols, policy_table)
}


#' Retirement Rule: By Tenure
#'
#' @description
#' Everyone with `min_tenure` years of service or more retires. A `policy_table`
#' with the group column and a `min_tenure` column sets a different number of
#' years for some groups. See [retirement_rule_age()] for how the parts fit.
#'
#' @param min_tenure Numeric. Years of service needed, for everyone or for every
#'   group not in `policy_table`.
#' @inheritParams retirement_rule_age
#' @param policy_table data.table or `NULL`. One row per group, with the
#'   `group_cols` and `min_tenure`.
#'
#' @return A rule list, to pass as `rule` to a `design_retirement_policy_*()`
#'   function.
#' @examples
#' retirement_rule_tenure(35)
#' @export
retirement_rule_tenure <- function(min_tenure, group_cols = NULL, policy_table = NULL) {
  validate_positive_number(min_tenure, "min_tenure", allow_zero = TRUE)
  .build_retirement_rule("retirement_rule_tenure", list(kind = "tenure", min_tenure = min_tenure),
                         group_cols, policy_table)
}


#' Retirement Rule: By Age and Tenure
#'
#' @description
#' Everyone aged `min_age` or over **and** with `min_tenure` years of service or
#' more retires. A `policy_table` can set either threshold by group. For "age
#' or tenure", use [retirement_rule_custom()].
#'
#' @inheritParams retirement_rule_age
#' @inheritParams retirement_rule_tenure
#' @param policy_table data.table or `NULL`. One row per group, with the
#'   `group_cols` and `min_age` and/or `min_tenure`.
#'
#' @return A rule list, to pass as `rule` to a `design_retirement_policy_*()`
#'   function.
#' @examples
#' retirement_rule_age_tenure(min_age = 60, min_tenure = 10)
#' @export
retirement_rule_age_tenure <- function(min_age, min_tenure, group_cols = NULL, policy_table = NULL) {
  validate_positive_number(min_age, "min_age")
  validate_positive_number(min_tenure, "min_tenure", allow_zero = TRUE)
  .build_retirement_rule("retirement_rule_age_tenure",
                         list(kind = "age_tenure", min_age = min_age, min_tenure = min_tenure),
                         group_cols, policy_table)
}


#' Retirement Rule: Your Own Expression
#'
#' @description
#' Everyone for whom `rule` is true retires. Write it with `age`,
#' `tenure_years`, any column of the personnel data, and `min_age` and
#' `min_tenure` when you set them here. Only comparisons and logic are allowed
#' (`>=`, `<`, `==`, `&`, `|`, `!`, `%in%`, `is.na()`, ...); anything else is
#' refused when you call this function.
#'
#' Setting `min_age` or `min_tenure` here, with a `policy_table`, lets one rule
#' use a different threshold per group. For example
#' `"age >= min_age | tenure_years >= min_tenure"`, with a table of thresholds by
#' occupation, retires each occupation at its own age or its own years of
#' service, whichever comes first.
#'
#' @param rule Character. The expression, e.g. `"age >= 65 | tenure_years >= 35"`.
#' @param min_age,min_tenure Numeric or `NULL`. Thresholds the rule can refer
#'   to, for everyone or for every group not in `policy_table`.
#' @inheritParams retirement_rule_age
#' @param policy_table data.table or `NULL`. One row per group, with the
#'   `group_cols` and `min_age` and/or `min_tenure`.
#'
#' @return A rule list, to pass as `rule` to a `design_retirement_policy_*()`
#'   function.
#' @examples
#' retirement_rule_custom("age >= 65 | tenure_years >= 35")
#' retirement_rule_custom("age >= min_age | tenure_years >= min_tenure",
#'                        min_age = 65, min_tenure = 35)
#' @export
retirement_rule_custom <- function(rule, min_age = NULL, min_tenure = NULL,
                                   group_cols = NULL, policy_table = NULL) {
  validate_eligibility_rule(rule)
  if (!is.null(min_age))    validate_positive_number(min_age, "min_age")
  if (!is.null(min_tenure)) validate_positive_number(min_tenure, "min_tenure", allow_zero = TRUE)
  .build_retirement_rule("retirement_rule_custom",
                         list(kind = "custom", custom_rule = rule,
                              min_age = min_age, min_tenure = min_tenure),
                         group_cols, policy_table)
}


#' Retirement Rates: As in the History
#'
#' @description
#' The share of people who retired in the past, by age, by tenure, or by both,
#' to apply in the projection. [simulate_horizon()] estimates the rates from
#' your panel data unless you give your own `hazard_table`.
#'
#' Ages and years of service are whole years. A group with no rate in the table
#' retires nobody, except that groups older than the oldest age seen retiring
#' take that age's rate.
#'
#' Use the rates in [design_retirement_policy_status_quo()] or
#' [design_retirement_policy_mandatory()].
#'
#' @param by Character. What the rates vary by: `"age"` (default), `"tenure"`
#'   or `"age_tenure"`.
#' @param group_cols Character or `NULL`. Further columns the rates vary by,
#'   e.g. `"est_id"` for a rate per establishment.
#' @param hazard_table data.table or `NULL`. Your own rates: one row per group,
#'   with `age_yr_int` and/or `tenure_yr_int` (as `by` says), the `group_cols`,
#'   and `hazard_rate` (the share who retire in a year, 0 to 1). `NULL`
#'   (default) means [simulate_horizon()] estimates them.
#' @param retire_strategy Character. Who retires within a group once the number
#'   is known: `"random"` (default), or a column name to retire the lowest
#'   values first.
#'
#' @return A rates list, to pass as `rates` to
#'   [design_retirement_policy_status_quo()] or
#'   [design_retirement_policy_mandatory()].
#' @examples
#' retirement_rates_status_quo()
#' retirement_rates_status_quo(by = "age_tenure", group_cols = "est_id")
#' @export
retirement_rates_status_quo <- function(by = "age", group_cols = NULL,
                                        hazard_table = NULL, retire_strategy = "random") {
  fn <- "retirement_rates_status_quo"
  validate_choice(by, c("age", "tenure", "age_tenure"), "by")
  if (!is.null(group_cols) && !is.character(group_cols)) {
    stop(fn, ": group_cols must be a character vector of column names, or NULL.", call. = FALSE)
  }
  if (!is.character(retire_strategy) || length(retire_strategy) != 1L) {
    stop(fn, ": retire_strategy must be \"random\" or one column name.", call. = FALSE)
  }

  # The keys the rates are looked up by: whole-year age and/or tenure, then the
  # caller's own columns
  keys <- unique(c(if (by %in% c("age", "age_tenure"))    "age_yr_int",
                   if (by %in% c("tenure", "age_tenure")) "tenure_yr_int",
                   group_cols))

  if (!is.null(hazard_table)) {
    if (!data.table::is.data.table(hazard_table)) {
      stop(fn, ": hazard_table must be a data.table.", call. = FALSE)
    }
    missing_cols <- setdiff(c(keys, "hazard_rate"), names(hazard_table))
    if (length(missing_cols) > 0L) {
      stop(fn, ": hazard_table is missing column(s) ",
           paste(missing_cols, collapse = ", "), ".", call. = FALSE)
    }
  }

  # hazard_rate = 0 is the rate for a group the table has no row for
  list(group_cols   = keys,
       policy_table = hazard_table,
       defaults     = list(hazard_rate = 0, retire_strategy = retire_strategy))
}


# =============================================================================
# Pension constructors
# =============================================================================

#' Defined-Benefit (DB) Pension
#'
#' @description
#' Builds the pension half of a retirement policy for a final-salary
#' defined-benefit formula.  See \code{\link{compute_db_pension}} for the
#' exact formula (accrual rate x capped service x pay, itself
#' optionally capped as a replacement rate).  Pay is the run's \code{salary_col}.
#'
#' @param accrual_rate Numeric.  Annual accrual rate (e.g. \code{0.02} = 2%
#'   of wage per year of service).
#' @param max_years Numeric or \code{NA} (default).  Service cap in years;
#'   \code{NA} = uncapped.
#' @param replacement_cap Numeric or \code{NA} (default).  Maximum
#'   replacement rate as a fraction of wage; \code{NA} = uncapped.
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{pension} argument to a \code{design_retirement_policy_*()}
#'   function, or as \code{\link{pension_hybrid}}'s \code{db} argument.
#' @seealso \code{\link{design_retirement_policy_eligibility}}, \code{\link{compute_db_pension}}
#' @examples
#' pension_db(accrual_rate = 0.02, max_years = 35, replacement_cap = 0.80)
#' @export
pension_db <- function(accrual_rate, max_years = NA, replacement_cap = NA) {
  validate_positive_number(accrual_rate, "accrual_rate", allow_zero = TRUE)
  if (!is.na(max_years))       validate_positive_number(max_years, "max_years", allow_zero = TRUE)
  if (!is.na(replacement_cap)) validate_positive_number(replacement_cap, "replacement_cap", allow_zero = TRUE)

  list(defaults = list(
    pension_type    = "db",
    accrual_rate    = accrual_rate,
    max_years       = max_years,
    replacement_cap = replacement_cap
  ))
}


#' Defined-Contribution (DC) Pension
#'
#' @description
#' Builds the pension half of a retirement policy that converts an
#' accumulated account balance to a periodic annuity.  Supports notional DC
#' (NDC) systems via an optional pre-retirement interest credit.  See
#' \code{\link{compute_dc_pension}} for the exact formula.
#'
#' @param balance_col Character.  Name of the account-balance column (e.g.
#'   \code{"dc_balance"}) -- a column-name pointer, not checked for existence
#'   here.
#' @param annuity_factor Numeric.  Divisor converting the balance to a
#'   periodic annuity (e.g. \code{12} for a monthly annuity from an annual
#'   balance).
#' @param notional_rate Numeric or \code{NA} (default).  NDC interest credit
#'   applied to the balance before annuitisation; \code{NA} = none.
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{pension} argument to a \code{design_retirement_policy_*()}
#'   function, or as \code{\link{pension_hybrid}}'s \code{dc} argument.
#' @seealso \code{\link{design_retirement_policy_eligibility}}, \code{\link{compute_dc_pension}}
#' @examples
#' pension_dc(balance_col = "dc_balance", annuity_factor = 12)
#' @export
pension_dc <- function(balance_col, annuity_factor, notional_rate = NA) {
  validate_character_string(balance_col, "balance_col")
  validate_positive_number(annuity_factor, "annuity_factor")
  if (!is.na(notional_rate)) validate_positive_number(notional_rate, "notional_rate", allow_zero = TRUE)

  list(defaults = list(
    pension_type   = "dc",
    balance_col    = balance_col,
    annuity_factor = annuity_factor,
    notional_rate  = notional_rate
  ))
}


#' Flat Pension
#'
#' @description
#' Builds the pension half of a retirement policy that pays every retiree the
#' same nominal amount -- e.g. a universal social pension.  See
#' \code{\link{compute_flat_pension}}.
#'
#' @param flat_amount Numeric.  Uniform pension amount paid to every retiree.
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{pension} argument to a \code{design_retirement_policy_*()}
#'   function.
#' @seealso \code{\link{design_retirement_policy_eligibility}}, \code{\link{compute_flat_pension}}
#' @examples
#' pension_flat(flat_amount = 500)
#' @export
pension_flat <- function(flat_amount) {
  validate_positive_number(flat_amount, "flat_amount", allow_zero = TRUE)
  list(defaults = list(pension_type = "flat", flat_amount = flat_amount))
}


#' Rate-of-Wage Pension
#'
#' @description
#' Builds the pension half of a retirement policy that pays a fixed
#' proportion of the retiree's pay, the run's \code{salary_col} (e.g. a scheme
#' paying a flat 15% of final salary). See \code{\link{compute_rate_pension}}.
#'
#' @param pension_rate Numeric.  Proportion of pay paid (e.g. \code{0.15}).
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{pension} argument to a \code{design_retirement_policy_*()}
#'   function.
#' @seealso \code{\link{design_retirement_policy_eligibility}}, \code{\link{compute_rate_pension}}
#' @examples
#' pension_rate(pension_rate = 0.15)
#' @export
pension_rate <- function(pension_rate) {
  validate_positive_number(pension_rate, "pension_rate", allow_zero = TRUE)
  list(defaults = list(
    pension_type = "rate",
    pension_rate = pension_rate
  ))
}


#' Hybrid (DB + DC) Pension
#'
#' @description
#' Builds the pension half of a retirement policy that pays the sum of a
#' defined-benefit pillar and a defined-contribution pillar.  Delegates
#' entirely to its two arguments -- see \code{\link{compute_hybrid_pension}}.
#'
#' @param db List.  Output of \code{\link{pension_db}}.
#' @param dc List.  Output of \code{\link{pension_dc}}.
#'
#' @return A list with a single \code{defaults} element (the union of
#'   \code{db}'s and \code{dc}'s fields, with \code{pension_type} set to
#'   \code{"hybrid"}) -- pass this as the \code{pension} argument to
#'   a \code{design_retirement_policy_*()} function.
#' @seealso \code{\link{design_retirement_policy_eligibility}}, \code{\link{compute_hybrid_pension}}
#' @examples
#' pension_hybrid(
#'   db = pension_db(accrual_rate = 0.015),
#'   dc = pension_dc(balance_col = "dc_balance", annuity_factor = 12)
#' )
#' @export
pension_hybrid <- function(db, dc) {
  if (!is.list(db) || is.null(db$defaults) || !identical(db$defaults$pension_type, "db")) {
    stop("pension_hybrid: 'db' must be the result of pension_db().", call. = FALSE)
  }
  if (!is.list(dc) || is.null(dc$defaults) || !identical(dc$defaults$pension_type, "dc")) {
    stop("pension_hybrid: 'dc' must be the result of pension_dc().", call. = FALSE)
  }
  merged <- utils::modifyList(db$defaults, dc$defaults)
  merged$pension_type <- "hybrid"
  list(defaults = merged)
}


#' Custom-Formula Pension
#'
#' @description
#' Builds the pension half of a retirement policy from a user-defined,
#' arithmetic-only formula string, evaluated safely against a restricted set
#' of operators -- see \code{\link{validate_pension_formula}} for the full
#' allow-list.  The formula may reference any column present on the retiree
#' data at simulation time (e.g. \code{"base_salary_lcu"}).  Years of service
#' are always available as \code{tenure_years}, whatever the tenure column in
#' your data is called.
#'
#' @param pension_formula Character scalar.  An arithmetic R expression
#'   string, e.g. \code{"pmin(base_salary_lcu * 0.02 * tenure_years,
#'   base_salary_lcu * 0.8)"}.  Validated immediately (parsed and checked
#'   against the safety allow-list) -- errors here, not buried at simulation
#'   time, if the formula is malformed or uses a disallowed function.
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{pension} argument to a \code{design_retirement_policy_*()}
#'   function.
#' @seealso \code{\link{design_retirement_policy_eligibility}}, \code{\link{validate_pension_formula}}
#' @examples
#' pension_custom("pmin(base_salary_lcu * 0.02 * tenure_years, base_salary_lcu * 0.8)")
#' @export
pension_custom <- function(pension_formula) {
  validate_pension_formula(pension_formula)
  list(defaults = list(pension_type = "custom", pension_formula = pension_formula))
}



pension_status_quo <- function(replacement_rates, group_cols = NULL){

  policy_dt <- 
    replacement_rates[!is.na(replacement_rate), 
                      .(pension_rate = stats::median(replacement_rate)), 
                      by = group_cols]

  list(group_cols   = group_cols,
       policy_table = policy_dt,
       defaults     = list(pension_type = "rate",
                           pension_rate = stats::median(policy_dt$replacement_rate)))

}

# =============================================================================
# Policies: one per kind
# =============================================================================

#' Assemble a Retirement Policy
#'
#' @description
#' Shared by the three `design_retirement_policy_*()` functions: checks that
#' each part came from the right constructor, and returns the policy list. A
#' part a kind does not use (the rule of a status-quo policy, say) is left out.
#'
#' @param type Character. `"status_quo"`, `"eligibility"` or `"mandatory"`.
#' @param pension List. From a `pension_*()` function.
#' @param rates List or `NULL`. From [retirement_rates_status_quo()].
#' @param rule List or `NULL`. From a `retirement_rule_*()` function.
#' @param active_types Character or `NULL`. Contract types the policy covers.
#'
#' @return The retirement policy list.
#'
#' @keywords internal
.build_retirement_policy <- function(type, pension, rates = NULL, rule = NULL,
                                     active_types = NULL) {
  fn <- paste0("design_retirement_policy_", type)
  if (!is.null(rule) && is.null(rule$defaults$kind)) {
    stop(fn, ": 'rule' must come from a retirement_rule_*() function.", call. = FALSE)
  }
  if (!is.null(rates) && !any(c("age_yr_int", "tenure_yr_int") %in% rates$group_cols)) {
    stop(fn, ": 'rates' must come from retirement_rates_status_quo().", call. = FALSE)
  }
  if (is.null(pension$defaults$pension_type)) {
    stop(fn, ": 'pension' must come from a pension_*() function.", call. = FALSE)
  }
  if (!is.null(active_types) &&
      (!is.character(active_types) || length(active_types) == 0L || anyNA(active_types))) {
    stop(fn, ": active_types must be contract type values, e.g. \"permanent\", or NULL.",
         call. = FALSE)
  }
  Filter(Negate(is.null), list(type = type, rates = rates, rule = rule,
                               pension = pension, active_types = active_types))
}


#' Retirement Policy: As in the History
#'
#' @description
#' People retire at the rates seen in your panel data: in each age (or tenure
#' year, or both), the share that retired in the past retire each year of the
#' projection. Nobody is forced to retire at any age. This is the default in
#' [simulate_horizon()], and the baseline to compare reforms against.
#'
#' @param pension List. What retirees are paid, from a `pension_*()` function,
#'   e.g. [pension_db()].
#' @param rates List. The rates, from [retirement_rates_status_quo()]. The
#'   default is rates by age, estimated from your panel by [simulate_horizon()].
#' @param active_types Character or `NULL`. The contract types the policy
#'   covers, e.g. `"permanent"`: only people holding a contract of these types
#'   can retire (a blank type counts as staff). `NULL` (default) covers everyone
#'   on the payroll.
#'
#' @return A retirement policy list, for [simulate_horizon()],
#'   [simulate_retirement()] or [select_retirees()].
#' @examples
#' design_retirement_policy_status_quo(pension_db(0.02, max_years = 35, replacement_cap = 0.80))
#' design_retirement_policy_status_quo(pension_flat(1000),
#'                                     rates = retirement_rates_status_quo(by = "age_tenure"))
#' @export
design_retirement_policy_status_quo <- function(pension,
                                                rates = retirement_rates_status_quo(),
                                                active_types = NULL) {
  .build_retirement_policy("status_quo", pension, rates = rates, active_types = active_types)
}


#' Retirement Policy: Everyone Who Meets a Rule
#'
#' @description
#' Everyone who meets the rule retires, every year: e.g. everyone aged 60 or
#' over. Use it to see what a retirement age, or a years-of-service
#' requirement, would do.
#'
#' @param rule List. Who retires, from a `retirement_rule_*()` function, e.g.
#'   [retirement_rule_age()].
#' @inheritParams design_retirement_policy_status_quo
#'
#' @return A retirement policy list, for [simulate_horizon()],
#'   [simulate_retirement()] or [select_retirees()].
#' @examples
#' design_retirement_policy_eligibility(retirement_rule_age(60), pension_flat(1000))
#' design_retirement_policy_eligibility(retirement_rule_age_tenure(60, 10),
#'                                      pension_db(0.02), active_types = "permanent")
#' @export
design_retirement_policy_eligibility <- function(rule, pension, active_types = NULL) {
  .build_retirement_policy("eligibility", pension, rule = rule, active_types = active_types)
}


#' Retirement Policy: A Mandatory Rule on Top of the History
#'
#' @description
#' Everyone who meets the rule retires (e.g. everyone aged 75 or over), and the
#' rest retire at the rates seen in your panel data, as in
#' [design_retirement_policy_status_quo()]. Use it to see what imposing a
#' compulsory retirement age would change against the status quo.
#'
#' @inheritParams design_retirement_policy_eligibility
#' @inheritParams design_retirement_policy_status_quo
#'
#' @return A retirement policy list, for [simulate_horizon()],
#'   [simulate_retirement()] or [select_retirees()].
#' @examples
#' design_retirement_policy_mandatory(retirement_rule_age(75), pension_db(0.02))
#' @export
design_retirement_policy_mandatory <- function(rule, pension,
                                               rates = retirement_rates_status_quo(),
                                               active_types = NULL) {
  .build_retirement_policy("mandatory", pension, rates = rates, rule = rule,
                           active_types = active_types)
}
