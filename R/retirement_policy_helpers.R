#' Retirement Policy Constructor Helpers
#'
#' @description
#' Small, composable constructors for building the \code{retirement_policy}
#' argument to \code{\link{simulate_retirement}}/\code{\link{simulate_horizon}}
#' without hand-writing the full three-slot list.  Each constructor validates
#' its own inputs immediately (at construction time) rather than deferring to
#' a runtime error deep inside the simulation.
#'
#' Three layers, composed together:
#' \enumerate{
#'   \item \strong{Eligibility constructors} (\code{eligibility_*()}) --
#'     which employees are eligible to retire.
#'   \item \strong{Pension constructors} (\code{pension_*()}) -- how much
#'     they receive.
#'   \item \code{\link{design_retirement_policy}} combines one of each into
#'     the final policy list, or use a \code{design_retirement_policy_*()}
#'     preset to skip building the eligibility piece by hand.
#' }
#'
#' Every constructor here returns a \strong{plain list} -- the same shape a
#' hand-written \code{retirement_policy} already has.  Nothing is hidden
#' behind a class: any \code{retirement_policy} configuration not reachable
#' through these constructors (e.g. the eligibility or pension \emph{type}
#' itself varying by group, rather than just its parameters) remains fully
#' reachable by passing a hand-built \code{policy_table} to
#' \code{\link{design_retirement_policy}}, or by editing the returned list
#' directly.
#'
#' @import data.table
#' @name retirement_policy_helpers
#' @keywords internal
NULL


# =============================================================================
# Eligibility constructors
# =============================================================================

#' Age-Threshold Retirement Eligibility
#'
#' @description
#' Builds the eligibility half of a retirement policy: everyone at or above
#' \code{min_age} is eligible to retire (100% take-up, no partial
#' probability). Combine with a pension constructor (e.g.
#' \code{\link{pension_flat}}) via \code{\link{design_retirement_policy}}, or
#' use \code{\link{design_retirement_policy_age}} to skip this step entirely.
#'
#' @param min_age Numeric. Minimum retirement age.
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{eligibility} argument to \code{\link{design_retirement_policy}}.
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{identify_eligibility}}
#' @examples
#' eligibility_age(min_age = 60)
#' @export
eligibility_age <- function(min_age) {
  validate_positive_number(min_age, "min_age")
  list(defaults = list(eligibility_type = "age_only", min_age = min_age))
}


#' Tenure-Threshold Retirement Eligibility
#'
#' @description
#' Builds the eligibility half of a retirement policy: everyone with at least
#' \code{min_tenure} years of service is eligible to retire.  Combine with a
#' pension constructor via \code{\link{design_retirement_policy}}, or use
#' \code{\link{design_retirement_policy_tenure}} to skip this step.
#'
#' @param min_tenure Numeric. Minimum years of service.
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{eligibility} argument to \code{\link{design_retirement_policy}}.
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{identify_eligibility}}
#' @examples
#' eligibility_tenure(min_tenure = 30)
#' @export
eligibility_tenure <- function(min_tenure) {
  validate_positive_number(min_tenure, "min_tenure", allow_zero = TRUE)
  list(defaults = list(eligibility_type = "tenure_only", min_tenure = min_tenure))
}


#' Combined Age-and-Tenure Retirement Eligibility
#'
#' @description
#' Builds the eligibility half of a retirement policy: eligible only once
#' \emph{both} \code{min_age} and \code{min_tenure} are met.  Combine with a
#' pension constructor via \code{\link{design_retirement_policy}}, or use
#' \code{\link{design_retirement_policy_age_tenure}} to skip this step.
#'
#' @param min_age Numeric. Minimum retirement age.
#' @param min_tenure Numeric. Minimum years of service.
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{eligibility} argument to \code{\link{design_retirement_policy}}.
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{identify_eligibility}}
#' @examples
#' eligibility_age_tenure(min_age = 60, min_tenure = 10)
#' @export
eligibility_age_tenure <- function(min_age, min_tenure) {
  validate_positive_number(min_age, "min_age")
  validate_positive_number(min_tenure, "min_tenure", allow_zero = TRUE)
  list(defaults = list(
    eligibility_type = "age_and_tenure",
    min_age          = min_age,
    min_tenure       = min_tenure
  ))
}


#' Status-Quo (Hazard-Rate) Retirement Eligibility
#'
#' @description
#' Builds the eligibility half of a retirement policy driven by a group-level
#' retirement \emph{rate} (typically per integer age) rather than a fixed
#' threshold -- the "status quo" mechanism, since the rate is calibrated to
#' reproduce recently observed retirement behavior instead of imposing a
#' rule. Combine with a pension constructor via
#' \code{\link{design_retirement_policy}}, or use
#' \code{\link{design_retirement_policy_status_quo}} to skip this step.
#'
#' @param hazard_table data.table or \code{NULL} (default).  A table with
#'   \code{group_cols} and a \code{hazard_rate} column, as produced by
#'   \code{\link{estimate_retirement_hazard_rates}}.
#'
#'   \code{NULL} defers estimation to \code{\link{simulate_horizon}}, which
#'   automatically calls \code{\link{estimate_retirement_hazard_rates}} on
#'   the historical panel before the first period.  \strong{This
#'   auto-estimation only happens inside \code{simulate_horizon()}}, which
#'   has access to the full multi-snapshot panel.  If the resulting policy is
#'   instead passed to \code{\link{simulate_retirement}} directly (a single
#'   snapshot, no panel), \code{NULL} silently resolves to a hazard rate of
#'   \code{0} for every group -- no one retires via the rate mechanism that
#'   period, only \code{mandatory_age} (if set) still forces retirement. This
#'   is quiet, not an error, so supply your own \code{hazard_table} (e.g. via
#'   \code{\link{estimate_retirement_hazard_rates}} called ahead of time) if
#'   you plan to call \code{simulate_retirement()} standalone.
#'
#'   Supplying a table in either calling context also lets you model a
#'   shocked or hand-built rate curve instead of the historical one.
#' @param mandatory_age Numeric or \code{NULL} (default).  A hard ceiling:
#'   everyone at or above this age retires with certainty, resolved
#'   \emph{before} the hazard-rate draw so they aren't double-counted against
#'   their group's rate-driven quota.  \code{NULL} means no ceiling.
#' @param group_cols Character vector or \code{NULL} (default).  Additional
#'   columns to stratify by beyond age (e.g. \code{"est_id"}) -- hazard rates
#'   are \emph{always} estimated/applied per integer age regardless of what
#'   you pass here, that bucketing is handled internally and you never need
#'   to name it yourself.  Whatever you supply is combined with it
#'   automatically.
#' @param retire_strategy Character.  \code{"random"} (default), or the name
#'   of a numeric column to rank ascending (lowest values selected first)
#'   within each group's quota.
#'
#' @return A list with \code{group_cols}, \code{policy_table}, and
#'   \code{defaults} elements -- pass this as the \code{eligibility} argument
#'   to \code{\link{design_retirement_policy}}.
#' @seealso \code{\link{design_retirement_policy}},
#'   \code{\link{estimate_retirement_hazard_rates}},
#'   \code{\link{compute_status_quo_retirements}}
#' @examples
#' eligibility_status_quo(mandatory_age = 75)
#' eligibility_status_quo(mandatory_age = 75, group_cols = "est_id")
#' @export
eligibility_status_quo <- function(hazard_table      = NULL,
                                   mandatory_age      = NULL,
                                   group_cols         = NULL,
                                   retire_strategy    = "random") {
  if (!is.null(group_cols) && !is.character(group_cols)) {
    stop("eligibility_status_quo: group_cols must be a character vector or NULL.", call. = FALSE)
  }
  # age_yr_int is never something the caller names -- hazard rates are
  # always bucketed by integer age internally (estimate_retirement_hazard_rates()/
  # compute_status_quo_retirements() both derive it from age). Whatever the
  # caller passes in group_cols is combined with it automatically.
  group_cols <- unique(c("age_yr_int", group_cols))

  if (!is.null(hazard_table)) {
    if (!data.table::is.data.table(hazard_table)) {
      stop("eligibility_status_quo: hazard_table must be a data.table.", call. = FALSE)
    }
    .missing_cols <- setdiff(c(group_cols, "hazard_rate"), names(hazard_table))
    if (length(.missing_cols) > 0L) {
      stop(
        "eligibility_status_quo: hazard_table is missing column(s): ",
        paste(.missing_cols, collapse = ", "), ".",
        call. = FALSE
      )
    }
  }
  if (!is.null(mandatory_age)) {
    validate_positive_number(mandatory_age, "mandatory_age")
  }
  if (!is.character(retire_strategy) || length(retire_strategy) != 1L) {
    stop("eligibility_status_quo: retire_strategy must be a single character string.", call. = FALSE)
  }

  list(
    group_cols   = group_cols,
    policy_table = hazard_table,
    defaults = list(
      eligibility_type = "hazard_rate",
      min_age          = mandatory_age,
      retire_strategy  = retire_strategy
    )
  )
}


#' Custom-Rule Retirement Eligibility
#'
#' @description
#' Builds the eligibility half of a retirement policy from a user-defined
#' rule string, evaluated safely against a restricted set of operators
#' (comparisons, boolean logic, \code{\%in\%}, \code{is.na}) -- see
#' \code{\link{validate_eligibility_rule}} for the full allow-list.  The rule
#' may reference any column present on the workforce data at simulation time
#' (e.g. \code{"paygrade"}).  \code{age} and \code{tenure_years} (years of
#' service) are always available, whatever your age and tenure columns are
#' called -- e.g. tenure supplied as \code{personnel_tenure} can be written
#' either as \code{tenure_years} or as \code{personnel_tenure}.  Combine with
#' a pension constructor via \code{\link{design_retirement_policy}}.
#'
#' @param rule Character scalar.  An R expression string evaluating to
#'   \code{TRUE}/\code{FALSE} per person, e.g.
#'   \code{"age >= 62 | (tenure_years >= 30 & age >= 55)"}.  Validated
#'   immediately (parsed and checked against the safety allow-list) -- errors
#'   here, not buried at simulation time, if the rule is malformed or uses a
#'   disallowed function.
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{eligibility} argument to \code{\link{design_retirement_policy}}.
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{validate_eligibility_rule}}
#' @examples
#' eligibility_custom("age >= 62 | (tenure_years >= 30 & age >= 55)")
#' @export
eligibility_custom <- function(rule) {
  validate_eligibility_rule(rule)
  list(defaults = list(eligibility_type = "custom", eligibility_rule = rule))
}


# =============================================================================
# Pension constructors
# =============================================================================

#' Defined-Benefit (DB) Pension
#'
#' @description
#' Builds the pension half of a retirement policy for a final-salary
#' defined-benefit formula.  See \code{\link{compute_db_pension}} for the
#' exact formula (accrual rate x capped service x reference wage, itself
#' optionally capped as a replacement rate).
#'
#' @param accrual_rate Numeric.  Annual accrual rate (e.g. \code{0.02} = 2%
#'   of wage per year of service).
#' @param ref_wage_col Character.  Name of the salary column to use as the
#'   pension reference wage (e.g. \code{"base_salary_lcu"}) -- this is a
#'   column-name pointer, not checked for existence here since it can only be
#'   verified once real workforce data is supplied at simulation time.
#' @param max_years Numeric or \code{NA} (default).  Service cap in years;
#'   \code{NA} = uncapped.
#' @param replacement_cap Numeric or \code{NA} (default).  Maximum
#'   replacement rate as a fraction of wage; \code{NA} = uncapped.
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{pension} argument to \code{\link{design_retirement_policy}} (or a
#'   preset), or as \code{\link{pension_hybrid}}'s \code{db} argument.
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{compute_db_pension}}
#' @examples
#' pension_db(accrual_rate = 0.02, ref_wage_col = "base_salary_lcu",
#'            max_years = 35, replacement_cap = 0.80)
#' @export
pension_db <- function(accrual_rate, ref_wage_col, max_years = NA, replacement_cap = NA) {
  validate_positive_number(accrual_rate, "accrual_rate", allow_zero = TRUE)
  validate_character_string(ref_wage_col, "ref_wage_col")
  if (!is.na(max_years))       validate_positive_number(max_years, "max_years", allow_zero = TRUE)
  if (!is.na(replacement_cap)) validate_positive_number(replacement_cap, "replacement_cap", allow_zero = TRUE)

  list(defaults = list(
    pension_type    = "db",
    accrual_rate    = accrual_rate,
    ref_wage_col    = ref_wage_col,
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
#'   \code{pension} argument to \code{\link{design_retirement_policy}} (or a
#'   preset), or as \code{\link{pension_hybrid}}'s \code{dc} argument.
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{compute_dc_pension}}
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
#'   \code{pension} argument to \code{\link{design_retirement_policy}} (or a
#'   preset).
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{compute_flat_pension}}
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
#' proportion of the retiree's reference wage (e.g. a scheme paying a flat
#' 15% of final salary). See \code{\link{compute_rate_pension}}.
#'
#' @param pension_rate Numeric.  Proportion of reference wage paid (e.g.
#'   \code{0.15}).
#' @param ref_wage_col Character.  Name of the salary column to use as the
#'   reference wage -- a column-name pointer, not checked for existence here.
#'
#' @return A list with a single \code{defaults} element -- pass this as the
#'   \code{pension} argument to \code{\link{design_retirement_policy}} (or a
#'   preset).
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{compute_rate_pension}}
#' @examples
#' pension_rate(pension_rate = 0.15, ref_wage_col = "base_salary_lcu")
#' @export
pension_rate <- function(pension_rate, ref_wage_col) {
  validate_positive_number(pension_rate, "pension_rate", allow_zero = TRUE)
  validate_character_string(ref_wage_col, "ref_wage_col")
  list(defaults = list(
    pension_type = "rate",
    pension_rate = pension_rate,
    ref_wage_col = ref_wage_col
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
#'   \code{\link{design_retirement_policy}} (or a preset).
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{compute_hybrid_pension}}
#' @examples
#' pension_hybrid(
#'   db = pension_db(accrual_rate = 0.015, ref_wage_col = "base_salary_lcu"),
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
#'   \code{pension} argument to \code{\link{design_retirement_policy}} (or a
#'   preset).
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{validate_pension_formula}}
#' @examples
#' pension_custom("pmin(base_salary_lcu * 0.02 * tenure_years, base_salary_lcu * 0.8)")
#' @export
pension_custom <- function(pension_formula) {
  validate_pension_formula(pension_formula)
  list(defaults = list(pension_type = "custom", pension_formula = pension_formula))
}


# =============================================================================
# Combiner
# =============================================================================

#' Combine an Eligibility and Pension Spec into a Retirement Policy
#'
#' @description
#' Combines the output of an \code{eligibility_*()} constructor and a
#' \code{pension_*()} constructor into the canonical three-slot
#' \code{retirement_policy} list consumed by \code{\link{simulate_retirement}}
#' and \code{\link{simulate_horizon}}.  This is a plain list combiner: the
#' result is identical in shape to a hand-written \code{retirement_policy},
#' so anything not reachable through these constructors remains reachable by
#' editing the returned list directly (e.g. \code{policy$policy_table <- ...}).
#'
#' @param eligibility List.  Output of an \code{eligibility_*()} constructor:
#'   \code{\link{eligibility_age}}, \code{\link{eligibility_tenure}},
#'   \code{\link{eligibility_age_tenure}}, \code{\link{eligibility_status_quo}},
#'   or \code{\link{eligibility_custom}}.
#' @param pension List.  Output of a \code{pension_*()} constructor:
#'   \code{\link{pension_db}}, \code{\link{pension_dc}},
#'   \code{\link{pension_flat}}, \code{\link{pension_rate}},
#'   \code{\link{pension_hybrid}}, or \code{\link{pension_custom}}.
#' @param group_cols Character vector or \code{NULL} (default).  Columns
#'   defining policy groups for group-varying parameters (e.g.
#'   \code{"grade"}) -- see \code{\link{resolve_policy_table}}. If
#'   \code{eligibility} already set \code{group_cols} (only
#'   \code{\link{eligibility_status_quo}} does this), passing a different,
#'   non-\code{NULL} value here raises an error rather than silently picking
#'   one.
#' @param policy_table data.table or \code{NULL} (default).  Group-level
#'   overrides matching \code{group_cols}; only columns present here override
#'   the corresponding \code{defaults} value -- any column absent falls back
#'   to its constructor default.  Same conflict rule as \code{group_cols}
#'   applies when \code{eligibility} already set one.
#'
#' @return A list with \code{group_cols}, \code{policy_table}, and
#'   \code{defaults} elements -- directly usable as \code{simulate_retirement()}'s
#'   or \code{simulate_horizon()}'s \code{retirement_policy} argument.
#' @seealso \code{\link{simulate_retirement}}, \code{\link{simulate_horizon}}
#' @examples
#' design_retirement_policy(
#'   eligibility = eligibility_age(min_age = 60),
#'   pension     = pension_rate(pension_rate = 0.15, ref_wage_col = "base_salary_lcu")
#' )
#'
#' # group-varying accrual_rate by grade
#' design_retirement_policy(
#'   eligibility  = eligibility_age(min_age = 60),
#'   pension      = pension_db(accrual_rate = 0.02, ref_wage_col = "base_salary_lcu"),
#'   group_cols   = "grade",
#'   policy_table = data.table::data.table(grade = c("A", "B"), accrual_rate = c(0.02, 0.03))
#' )
#' @export
design_retirement_policy <- function(eligibility, pension, group_cols = NULL, policy_table = NULL) {
  if (!is.list(eligibility) || is.null(eligibility$defaults)) {
    stop(
      "design_retirement_policy: 'eligibility' must be the output of an ",
      "eligibility_*() constructor.",
      call. = FALSE
    )
  }
  if (!is.list(pension) || is.null(pension$defaults)) {
    stop(
      "design_retirement_policy: 'pension' must be the output of a ",
      "pension_*() constructor.",
      call. = FALSE
    )
  }

  .elig_group_cols   <- eligibility$group_cols
  .elig_policy_table <- eligibility$policy_table

  if (!is.null(.elig_group_cols) && !is.null(group_cols) &&
      !identical(.elig_group_cols, group_cols)) {
    stop(
      "design_retirement_policy: 'eligibility' already specifies group_cols (",
      paste(.elig_group_cols, collapse = ", "), ") -- passing a different ",
      "group_cols here is ambiguous. Omit one, or make them match.",
      call. = FALSE
    )
  }
  if (!is.null(.elig_policy_table) && !is.null(policy_table)) {
    stop(
      "design_retirement_policy: 'eligibility' already specifies a ",
      "policy_table (e.g. via eligibility_status_quo()'s hazard_table) -- ",
      "passing another policy_table here is ambiguous. Omit one.",
      call. = FALSE
    )
  }

  list(
    group_cols   = .elig_group_cols   %||% group_cols,
    policy_table = .elig_policy_table %||% policy_table,
    defaults     = utils::modifyList(eligibility$defaults, pension$defaults)
  )
}


# =============================================================================
# Presets
# =============================================================================

#' Age-Threshold Retirement Policy (Preset)
#'
#' @description
#' Shortcut for \code{design_retirement_policy(eligibility_age(min_age), pension)}.
#'
#' @param min_age Numeric. Minimum retirement age.
#' @param pension List. Output of a \code{pension_*()} constructor -- see
#'   \code{\link{design_retirement_policy}}.
#' @param group_cols,policy_table Optional group-varying overrides, forwarded
#'   to \code{\link{design_retirement_policy}} -- e.g. a grade-varying
#'   \code{min_age}. See there for details.
#'
#' @return Same shape as \code{\link{design_retirement_policy}}.
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{eligibility_age}}
#' @examples
#' design_retirement_policy_age(min_age = 60, pension = pension_flat(500))
#' @export
design_retirement_policy_age <- function(min_age, pension, group_cols = NULL, policy_table = NULL) {
  design_retirement_policy(eligibility_age(min_age), pension,
                           group_cols = group_cols, policy_table = policy_table)
}


#' Tenure-Threshold Retirement Policy (Preset)
#'
#' @description
#' Shortcut for \code{design_retirement_policy(eligibility_tenure(min_tenure), pension)}.
#'
#' @param min_tenure Numeric. Minimum years of service.
#' @param pension List. Output of a \code{pension_*()} constructor -- see
#'   \code{\link{design_retirement_policy}}.
#' @param group_cols,policy_table Optional group-varying overrides, forwarded
#'   to \code{\link{design_retirement_policy}} -- e.g. a grade-varying
#'   \code{min_tenure}. See there for details.
#'
#' @return Same shape as \code{\link{design_retirement_policy}}.
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{eligibility_tenure}}
#' @examples
#' design_retirement_policy_tenure(min_tenure = 30, pension = pension_flat(500))
#' @export
design_retirement_policy_tenure <- function(min_tenure, pension, group_cols = NULL, policy_table = NULL) {
  design_retirement_policy(eligibility_tenure(min_tenure), pension,
                           group_cols = group_cols, policy_table = policy_table)
}


#' Combined Age-and-Tenure Retirement Policy (Preset)
#'
#' @description
#' Shortcut for
#' \code{design_retirement_policy(eligibility_age_tenure(min_age, min_tenure), pension)}.
#'
#' @param min_age Numeric. Minimum retirement age.
#' @param min_tenure Numeric. Minimum years of service.
#' @param pension List. Output of a \code{pension_*()} constructor -- see
#'   \code{\link{design_retirement_policy}}.
#' @param group_cols,policy_table Optional group-varying overrides, forwarded
#'   to \code{\link{design_retirement_policy}}. See there for details.
#'
#' @return Same shape as \code{\link{design_retirement_policy}}.
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{eligibility_age_tenure}}
#' @examples
#' design_retirement_policy_age_tenure(
#'   min_age = 60, min_tenure = 10,
#'   pension = pension_db(accrual_rate = 0.02, ref_wage_col = "base_salary_lcu")
#' )
#' @export
design_retirement_policy_age_tenure <- function(min_age, min_tenure, pension,
                                                group_cols = NULL, policy_table = NULL) {
  design_retirement_policy(eligibility_age_tenure(min_age, min_tenure), pension,
                           group_cols = group_cols, policy_table = policy_table)
}


#' Status-Quo (Hazard-Rate) Retirement Policy (Preset)
#'
#' @description
#' Shortcut for
#' \code{design_retirement_policy(eligibility_status_quo(...), pension)}. See
#' \code{\link{eligibility_status_quo}} for the auto-estimation caveat
#' (only fires inside \code{\link{simulate_horizon}}, not standalone
#' \code{\link{simulate_retirement}}).
#'
#' @param pension List. Output of a \code{pension_*()} constructor -- see
#'   \code{\link{design_retirement_policy}}.
#' @param hazard_table,mandatory_age,group_cols,retire_strategy See
#'   \code{\link{eligibility_status_quo}}.
#'
#' @return Same shape as \code{\link{design_retirement_policy}}.
#' @seealso \code{\link{design_retirement_policy}}, \code{\link{eligibility_status_quo}}
#' @examples
#' design_retirement_policy_status_quo(
#'   mandatory_age = 75,
#'   pension       = pension_rate(pension_rate = 0.15, ref_wage_col = "base_salary_lcu")
#' )
#' @export
design_retirement_policy_status_quo <- function(pension,
                                                hazard_table  = NULL,
                                                mandatory_age  = NULL,
                                                group_cols     = NULL,
                                                retire_strategy = "random") {
  design_retirement_policy(
    eligibility_status_quo(
      hazard_table    = hazard_table,
      mandatory_age    = mandatory_age,
      group_cols       = group_cols,
      retire_strategy  = retire_strategy
    ),
    pension
  )
}
