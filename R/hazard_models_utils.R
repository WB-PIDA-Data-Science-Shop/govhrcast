# =============================================================================
# Discrete-time hazard model utilities
#
# Functions for fitting, calibrating, and applying binomial GLM hazard models
# to simulate retirement and non-retirement exit events in govhrcast.
#
# Exported:
#   build_retirement_hazard_data() — construct a person-period regression panel for retirements
#   build_exit_hazard_data()       — construct a person-period regression panel for non-retirement exits
#   fit_hazard_model()             — estimate a binomial GLM on that panel
#   select_hazard_threshold()      — find the optimal probability cut-off
#   predict_hazard()               — score a single-period snapshot
# =============================================================================

# Internal helper: coerce character columns to factor, leave others unchanged.
.chr_to_factor <- function(x) if (is.character(x)) factor(x) else x


# -----------------------------------------------------------------------------
# .dedup_to_primary()
#
# Deduplicate a contract data.table to one row per (person [x snapshot]).
# Works for both stacked panels (by_cols = c(pid, ref_date)) and single-period
# snapshots (by_cols = pid only).
#
# Sort tiebreaker priority: latest start_date > highest salary > lowest
# contract_id — identical to get_primary_contract() convention.
#
# @param dt              data.table subset to relevant columns already.
# @param by_cols         Character vector: grouping key, e.g.
#                        c("personnel_id", "ref_date") or "personnel_id".
# @param start_date_col  Character. Tiebreaker 1 (descending).
# @param salary_col      Character. Tiebreaker 2 (descending).
# @param contract_id_col Character. Tiebreaker 3 (ascending).
# -----------------------------------------------------------------------------
.dedup_to_primary <- function(dt,
                               by_cols,
                               start_date_col   = "start_date",
                               salary_col       = "gross_salary_lcu",
                               contract_id_col  = "contract_id") {
  sort_cols  <- intersect(c(start_date_col, salary_col, contract_id_col),
                          names(dt))
  sort_order <- c(-1L, -1L, 1L)[seq_along(sort_cols)]
  data.table::setorderv(dt,
                        cols  = c(by_cols, sort_cols),
                        order = c(rep(1L, length(by_cols)), sort_order))
  dt[, .SD[1L], by = by_cols]
}


# -----------------------------------------------------------------------------
# .attach_age_tenure()
#
# Attach age and tenure_years columns to a regression data.table, joining from
# a pre-built covariate panel (or computing on-the-fly for the snapshot path).
#
# Panel path  (ref_date_col is non-NULL):
#   Builds age_panel and tenure_panel keyed on (personnel_id, ref_date) then
#   joins onto reg_dt.
#
# Snapshot path (ref_date_col is NULL):
#   age and tenure_years are keyed on personnel_id only; ref_date scalar must
#   be supplied when columns are not pre-computed.
#
# @param reg_dt           data.table to join onto (modified by reference).
# @param personnel_dt     Personnel panel / snapshot.
# @param contract_dt      Contract panel / snapshot (needed for tenure slow path).
# @param personnel_id_col Character.
# @param ref_date_col     Character or NULL.  NULL triggers snapshot path.
# @param age_col          Name of pre-computed age column on personnel_dt, or NULL.
# @param tenure_col       Name of pre-computed tenure column on personnel_dt, or NULL.
# @param birth_date_col   Character. Used only on slow age path.
# @param start_date_col   Character. Passed to compute_tenure_panel().
# @param end_date_col     Character. Passed to compute_tenure_panel().
# @param contract_type_col Character. Passed to compute_tenure_panel().
# @param contract_id_col  Character. Passed to compute_tenure_panel().
# @param ref_date         Date scalar. Required on snapshot slow paths.
#
# @return reg_dt with age and tenure_years columns joined in.
# -----------------------------------------------------------------------------
.attach_age_tenure <- function(reg_dt,
                                personnel_dt,
                                contract_dt,
                                personnel_id_col   = "personnel_id",
                                ref_date_col       = "ref_date",
                                age_col            = "age",
                                tenure_col         = "tenure_years",
                                birth_date_col     = "birth_date",
                                start_date_col     = "start_date",
                                end_date_col       = "end_date",
                                contract_type_col  = "contract_type_code",
                                contract_id_col    = "contract_id",
                                ref_date           = NULL) {

  is_panel  <- !is.null(ref_date_col)
  join_cols <- if (is_panel) c(personnel_id_col, ref_date_col)
               else personnel_id_col

  # --- Age -----------------------------------------------------------------
  has_age <- !is.null(age_col) && age_col %in% names(personnel_dt)

  if (has_age) {
    age_dt <- personnel_dt[, .SD,
                .SDcols = c(personnel_id_col,
                            if (is_panel) ref_date_col else NULL,
                            age_col)]
    if (age_col != "age") data.table::setnames(age_dt, age_col, "age")
  } else {
    if (!is_panel && is.null(ref_date))
      stop("'age' is not pre-computed on personnel_dt and ref_date = NULL. ",
           "Supply ref_date or a personnel_dt with an age column.",
           call. = FALSE)
    if (!is_panel && !birth_date_col %in% names(personnel_dt))
      stop("personnel_dt is missing required column: '", birth_date_col, "'.",
           call. = FALSE)
    if (is_panel && !birth_date_col %in% names(personnel_dt))
      stop("personnel_dt is missing required column: '", birth_date_col, "'.",
           call. = FALSE)

    if (is_panel) {
      age_dt <- personnel_dt[, c(
        list(.pid   = get(personnel_id_col),
             .refdt = get(ref_date_col)),
        list(age = compute_years(get(birth_date_col), get(ref_date_col)))
      )]
      data.table::setnames(age_dt,
                           c(".pid", ".refdt"),
                           c(personnel_id_col, ref_date_col))
    } else {
      age_dt <- personnel_dt[, .(
        .pid = get(personnel_id_col),
        age  = compute_years(get(birth_date_col), ref_date)
      )]
      data.table::setnames(age_dt, ".pid", personnel_id_col)
    }
  }

  # --- Tenure --------------------------------------------------------------
  has_tenure <- !is.null(tenure_col) && tenure_col %in% names(personnel_dt)

  if (has_tenure) {
    tenure_dt <- personnel_dt[, .SD,
                   .SDcols = c(personnel_id_col,
                               if (is_panel) ref_date_col else NULL,
                               tenure_col)]
    if (tenure_col != "tenure_years")
      data.table::setnames(tenure_dt, tenure_col, "tenure_years")
  } else {
    if (!is_panel && is.null(ref_date))
      stop("'tenure_years' is not pre-computed on personnel_dt and ref_date = NULL. ",
           "Supply ref_date or a personnel_dt with a tenure column.",
           call. = FALSE)

    if (is_panel) {
      tenure_dt <- compute_tenure_panel(
        contract_dt       = contract_dt,
        personnel_id_col  = personnel_id_col,
        ref_date_col      = ref_date_col,
        contract_id_col   = contract_id_col,
        start_date_col    = start_date_col,
        end_date_col      = end_date_col,
        contract_type_col = contract_type_col
      )
    } else {
      if (is.null(ref_date))
        stop("'tenure_years' is not pre-computed on personnel_dt and ref_date = NULL. ",
             "Supply ref_date or a personnel_dt with a tenure column.",
             call. = FALSE)
      contract_with_ref <- data.table::copy(contract_dt)
      contract_with_ref[, .tmp_ref := ref_date]
      tenure_dt <- compute_tenure_panel(
        contract_dt       = contract_with_ref,
        personnel_id_col  = personnel_id_col,
        ref_date_col      = ".tmp_ref",
        contract_id_col   = contract_id_col,
        start_date_col    = start_date_col,
        end_date_col      = end_date_col,
        contract_type_col = contract_type_col
      )[, c(personnel_id_col, "tenure_years"), with = FALSE]
    }
  }

  # --- Join both onto reg_dt -----------------------------------------------
  cov_dt <- age_dt[tenure_dt, on = join_cols]
  cov_dt[reg_dt, on = join_cols]
}


# =============================================================================
# build_retirement_hazard_data
# =============================================================================

#' Build a Person-Period Regression Dataset for Retirement Hazard Modelling
#'
#' @description
#' Constructs a person-period (discrete-time survival) dataset suitable for
#' fitting a retirement hazard model with \code{\link{fit_hazard_model}}.
#'
#' Each row represents one person observed in one panel snapshot while they
#' are still at risk (active, not yet retired).  The binary outcome column
#' (\code{"retired"} by default) equals \code{1} on the first snapshot where
#' the person's contract type is \code{"pensioner"}, and \code{0} on all
#' prior snapshots.  All rows after the first \code{1} are dropped — once the
#' event has occurred the spell is complete.  Persons who never become
#' pensioners are included in full as censored observations (\code{retired = 0}
#' throughout).
#'
#' @details
#' **Algorithm** \cr
#' \enumerate{
#'   \item Deduplicate to the primary contract per person per snapshot using
#'     \code{\link{get_primary_contract}}, so multi-contract individuals
#'     contribute exactly one row per snapshot.
#'   \item Flag \code{retired = 1} where \code{contract_type_col == "pensioner"}.
#'   \item Sort by \code{personnel_id_col}, \code{ref_date_col}.
#'   \item For each person, retain rows up to and including the first
#'     \code{retired = 1}; discard all subsequent rows.
#'   \item Attach age (\code{\link{compute_age}}), tenure
#'     (\code{\link{compute_tenure}}), any \code{extra_covariates}, and
#'     optionally an \code{eligible} flag (\code{\link{identify_eligibility}})
#'     — all measured at each snapshot's \code{ref_date}.
#' }
#'
#' **Censored observations** \cr
#' A person who leaves the panel without ever becoming a pensioner is treated
#' as censored: their rows are retained with \code{retired = 0} throughout.
#' This is correct for discrete-time hazard estimation — the model sees that
#' the event did not occur during their observed spell.
#'
#' **Retirement policy** \cr
#' If \code{retirement_policy} is supplied, an \code{eligible} flag (0/1) is
#' computed at each snapshot using \code{\link{identify_eligibility}} and
#' included as a covariate.  This captures the sharp discontinuity in
#' retirement probability at the eligibility threshold and is the most
#' informative single predictor for most civil service systems.
#'
#' @param panel_contract_dt data.table.  Full historical contract panel — all
#'   snapshots stacked, with a \code{ref_date_col} column.
#' @param panel_personnel_dt data.table.  Full historical personnel panel —
#'   all snapshots stacked, with a \code{ref_date_col} column.  Used for age
#'   computation via \code{birth_date_col}.
#' @param retirement_policy Optional named list.  If supplied, an
#'   \code{eligible} column (0/1) is added using
#'   \code{\link{identify_eligibility}} at each snapshot.  Follows the canonical
#'   3-slot structure: \code{group_cols}, \code{policy_table}, \code{defaults}.
#' @param age_col Character scalar or \code{NULL}.  Name of a pre-computed age
#'   column already present on \code{panel_personnel_dt}.  When the column
#'   exists, age is obtained via a direct join rather than recomputed with
#'   \code{\link{compute_age}} at every snapshot.  Default: \code{"age"}.
#'   Set to \code{NULL} to force recomputation.
#' @param tenure_col Character scalar or \code{NULL}.  Name of a pre-computed
#'   tenure column already present on \code{panel_personnel_dt}.  When the
#'   column exists, tenure is obtained via a direct join rather than recomputed
#'   with \code{\link{compute_tenure}} at every snapshot.  Default:
#'   \code{"tenure_years"}.  Set to \code{NULL} to force recomputation.
#' @param extra_covariates Character vector.  Additional columns to carry
#'   forward from the primary contract at each snapshot (e.g.
#'   \code{c("paygrade", "gross_salary_lcu")}).  All must be present in
#'   \code{panel_contract_dt}.  Default: \code{NULL}.
#' @param outcome_col Character scalar.  Name of the binary outcome column in
#'   the returned dataset.  Default: \code{"retired"}.
#' @param personnel_id_col Character.  Default: \code{"personnel_id"}.
#' @param ref_date_col Character.  Default: \code{"ref_date"}.
#' @param birth_date_col Character.  Default: \code{"birth_date"}.
#' @param start_date_col Character.  Default: \code{"start_date"}.
#' @param end_date_col Character.  Default: \code{"end_date"}.
#' @param contract_type_col Character.  Default: \code{"contract_type_code"}.
#' @param contract_id_col Character.  Default: \code{"contract_id"}.
#'
#' @return A \code{data.table} with one row per person per at-risk snapshot.
#'   Always contains:
#'   \describe{
#'     \item{\code{personnel_id_col}}{Person identifier.}
#'     \item{\code{ref_date_col}}{Snapshot date.}
#'     \item{\code{outcome_col}}{Binary (0/1).  1 = first pensioner snapshot.}
#'     \item{\code{age}}{Age in years at the snapshot.}
#'     \item{\code{tenure_years}}{Cumulative service years at the snapshot.}
#'   }
#'   Plus any requested \code{extra_covariates} and, if
#'   \code{retirement_policy} is supplied, \code{eligible} (0/1).
#'
#' @seealso \code{\link{fit_hazard_model}}, \code{\link{identify_eligibility}}
#'
#' @examples
#' \dontrun{
#' reg_dt <- build_retirement_hazard_data(
#'   panel_contract_dt  = my_panel_contracts,
#'   panel_personnel_dt = my_panel_personnel,
#'   extra_covariates   = c("paygrade", "gross_salary_lcu")
#' )
#' hm <- fit_hazard_model(reg_dt, outcome_col = "retired",
#'                        covariates = c("eligible", "age", "tenure_years",
#'                                       "paygrade", "gross_salary_lcu"))
#' }
#'
#' @export
build_retirement_hazard_data <- function(panel_contract_dt,
                                         panel_personnel_dt,
                                         retirement_policy  = NULL,
                                         extra_covariates   = NULL,
                                         age_col            = "age",
                                         tenure_col         = "tenure_years",
                                         outcome_col        = "retired",
                                         personnel_id_col   = "personnel_id",
                                         ref_date_col       = "ref_date",
                                         birth_date_col     = "birth_date",
                                         start_date_col     = "start_date",
                                         end_date_col       = "end_date",
                                         contract_type_col  = "contract_type_code",
                                         contract_id_col    = "contract_id",
                                         salary_col         = "gross_salary_lcu") {

  # ------------------------------------------------------------------
  # 1. Coerce and validate
  # ------------------------------------------------------------------
  if (!data.table::is.data.table(panel_contract_dt))
    panel_contract_dt <- data.table::as.data.table(panel_contract_dt)
  if (!data.table::is.data.table(panel_personnel_dt))
    panel_personnel_dt <- data.table::as.data.table(panel_personnel_dt)

  for (col in c(ref_date_col, personnel_id_col, contract_type_col)) {
    if (!col %in% names(panel_contract_dt))
      stop("panel_contract_dt is missing required column: '", col, "'.",
           call. = FALSE)
  }
  for (col in c(ref_date_col, personnel_id_col)) {
    if (!col %in% names(panel_personnel_dt))
      stop("panel_personnel_dt is missing required column: '", col, "'.",
           call. = FALSE)
  }

  if (!is.null(extra_covariates)) {
    missing_extra <- setdiff(extra_covariates, names(panel_contract_dt))
    if (length(missing_extra) > 0L)
      stop("extra_covariates not found in panel_contract_dt: ",
           paste(missing_extra, collapse = ", "), call. = FALSE)
  }

  panel_dates <- sort(unique(panel_personnel_dt[[ref_date_col]]))
  if (length(panel_dates) < 2L)
    stop("panel_personnel_dt must contain at least 2 distinct snapshot dates.",
         call. = FALSE)

  # ------------------------------------------------------------------
  # 2. Deduplicate to primary contract per person per snapshot.
  # ------------------------------------------------------------------
  keep_cols  <- unique(c(personnel_id_col, ref_date_col, contract_type_col,
                         start_date_col, end_date_col, contract_id_col,
                         salary_col, extra_covariates))
  keep_cols  <- intersect(keep_cols, names(panel_contract_dt))
  primary_dt <- .dedup_to_primary(
    dt               = panel_contract_dt[, .SD, .SDcols = keep_cols],
    by_cols          = c(personnel_id_col, ref_date_col),
    start_date_col   = start_date_col,
    salary_col       = salary_col,
    contract_id_col  = contract_id_col
  )

  # ------------------------------------------------------------------
  # 3. Flag outcome, sort, truncate spells at first retirement
  # ------------------------------------------------------------------
  primary_dt[, (outcome_col) := as.integer(
    get(contract_type_col) == "pensioner"
  )]

  data.table::setorderv(primary_dt, c(personnel_id_col, ref_date_col))

  # For each person: keep rows up to and including the first retired = 1.
  # Persons who never retire keep all their rows (censored).
  primary_dt[,
    .first_retire_date := {
      ret_dates <- get(ref_date_col)[get(outcome_col) == 1L]
      if (length(ret_dates)) min(ret_dates) else as.Date(NA)
    },
    by = c(personnel_id_col)
  ]

  reg_dt <- primary_dt[
    is.na(.first_retire_date) | get(ref_date_col) <= .first_retire_date
  ]
  reg_dt[, .first_retire_date := NULL]

  # ------------------------------------------------------------------
  # 4. Attach age and tenure at each snapshot.
  # ------------------------------------------------------------------
  reg_dt <- .attach_age_tenure(
    reg_dt            = reg_dt,
    personnel_dt      = panel_personnel_dt,
    contract_dt       = panel_contract_dt,
    personnel_id_col  = personnel_id_col,
    ref_date_col      = ref_date_col,
    age_col           = age_col,
    tenure_col        = tenure_col,
    birth_date_col    = birth_date_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    contract_type_col = contract_type_col,
    contract_id_col   = contract_id_col
  )

  # ------------------------------------------------------------------
  # 5. Attach eligibility flag per snapshot (optional).
  #    identify_eligibility() accepts a scalar ref_date so a per-snapshot
  #    loop is unavoidable; age_col/tenure_col are forwarded so it
  #    skips recomputing whichever are already on the personnel slice.
  # ------------------------------------------------------------------
  if (!is.null(retirement_policy)) {
    elig_list <- lapply(panel_dates, function(snap) {
      ct_bare <- panel_contract_dt[get(ref_date_col) == snap, !ref_date_col, with = FALSE]
      pt_bare <- panel_personnel_dt[get(ref_date_col) == snap, !ref_date_col, with = FALSE]
      elig_dt <- identify_eligibility(
        contract_dt       = ct_bare,
        personnel_dt      = pt_bare,
        policy_params     = retirement_policy,
        ref_date          = snap,
        personnel_id_col  = personnel_id_col,
        contract_id_col   = contract_id_col,
        birth_date_col    = birth_date_col,
        start_date_col    = start_date_col,
        end_date_col      = end_date_col,
        contract_type_col = contract_type_col,
        age_col           = age_col,
        tenure_col        = tenure_col
      )[, c("personnel_id", "retire"), with = FALSE]
      data.table::setnames(elig_dt, c("personnel_id", "retire"),
                           c(personnel_id_col, "eligible"))
      elig_dt[, (ref_date_col) := snap]
      elig_dt
    })
    elig_panel <- data.table::rbindlist(elig_list, use.names = TRUE)
    reg_dt <- elig_panel[reg_dt, on = c(personnel_id_col, ref_date_col)]
  }

  # ------------------------------------------------------------------
  # 6. Canonical column order
  # ------------------------------------------------------------------
  front_cols <- c(personnel_id_col, ref_date_col, outcome_col)
  data.table::setcolorder(
    reg_dt,
    c(front_cols, setdiff(names(reg_dt), front_cols))
  )

  reg_dt
}


# =============================================================================
# build_exit_hazard_data
# =============================================================================

#' Build a Person-Period Regression Dataset for Non-Retirement Exit Hazard Modelling
#'
#' @description
#' Constructs a person-period (discrete-time survival) dataset for fitting a
#' non-retirement exit hazard model with \code{\link{fit_hazard_model}}.
#'
#' Each row represents one person observed at one panel snapshot while still
#' active.  The binary outcome (\code{"exited"} by default) equals \code{1}
#' at snapshot \eqn{t} when the person is active at \eqn{t} but does
#' \emph{not} appear as an active or pensioner contract at \eqn{t+1}.
#' Persons who never exit are included in full as censored observations
#' (\code{exited = 0} throughout).  Retirements (persons who transition to
#' \code{"pensioner"}) are excluded from the at-risk pool entirely — they
#' are not non-retirement exits.
#'
#' @details
#' **Algorithm** \cr
#' \enumerate{
#'   \item Deduplicate to the primary contract per person per snapshot
#'     (vectorised sort + \code{.SD[1L]} by person \eqn{\times} snapshot).
#'   \item Use \code{govhr::detect_personnel_event(event_type = "fire")} to
#'     identify all persons who disappear from \code{panel_personnel_dt}
#'     between consecutive snapshots.  \code{freq} is inferred automatically
#'     from the median gap between panel dates (\code{NULL}) or overridden
#'     explicitly.
#'   \item Anti-join against persons who became \code{"pensioner"} at the
#'     same snapshot — retirements are disappearances but are not exits.
#'   \item Truncate: drop all rows after the first \code{exited = 1}.
#'   \item Attach \code{age} and \code{tenure_years} vectorised across all
#'     snapshots (fast-path join when pre-computed; inline computation otherwise).
#'   \item Carry forward any \code{extra_covariates} from the primary contract.
#' }
#'
#' **Frequency inference** \cr
#' When \code{freq = NULL} (the default), the panel cadence is inferred from
#' the median gap between consecutive \code{ref_date} values:
#' \eqn{\geq 360} days \eqn{\to} \code{"year"};
#' \eqn{\geq 85} \eqn{\to} \code{"quarter"};
#' \eqn{\geq 27} \eqn{\to} \code{"month"};
#' otherwise \code{"day"}.  Pass an explicit string (any value accepted by
#' \code{seq.Date(by = ...)}) to override.
#'
#' @param panel_contract_dt data.table.  Full historical contract panel — all
#'   snapshots stacked, with a \code{ref_date_col} column.
#' @param panel_personnel_dt data.table.  Full historical personnel panel —
#'   all snapshots stacked, with a \code{ref_date_col} column.
#' @param active_types Character vector or \code{NULL}.  Contract type codes
#'   that constitute an active (at-risk) worker.  \code{NULL} (default) uses
#'   all types except \code{"inactive"} and \code{"pensioner"}, mirroring the
#'   \code{\link{get_active_contracts}} convention.  Supply an explicit vector
#'   (e.g. \code{c("perm", "temp")}) to restrict to specific types.
#' @param freq Character scalar or \code{NULL}.  Frequency string passed to
#'   \code{govhr::detect_personnel_event()} — must match the panel cadence
#'   (e.g. \code{"year"}, \code{"quarter"}, \code{"month"}).
#'   \code{NULL} (default) auto-infers from the median gap between snapshots.
#' @param extra_covariates Character vector or \code{NULL}.  Additional columns
#'   to carry forward from the primary contract at each snapshot.  All must
#'   exist in \code{panel_contract_dt}.  Default: \code{NULL}.
#' @param age_col Character scalar or \code{NULL}.  Pre-computed age column on
#'   \code{panel_personnel_dt}.  Default: \code{"age"}.
#' @param tenure_col Character scalar or \code{NULL}.  Pre-computed tenure
#'   column on \code{panel_personnel_dt}.  Default: \code{"tenure_years"}.
#' @param outcome_col Character scalar.  Name of the binary outcome column.
#'   Default: \code{"exited"}.
#' @param personnel_id_col Character.  Default: \code{"personnel_id"}.
#' @param ref_date_col Character.  Default: \code{"ref_date"}.
#' @param birth_date_col Character.  Default: \code{"birth_date"}.
#' @param start_date_col Character.  Default: \code{"start_date"}.
#' @param end_date_col Character.  Default: \code{"end_date"}.
#' @param contract_type_col Character.  Default: \code{"contract_type_code"}.
#' @param contract_id_col Character.  Default: \code{"contract_id"}.
#' @param salary_col Character.  Salary column used as a tiebreaker in primary
#'   contract selection.  Default: \code{"gross_salary_lcu"}.
#'
#' @return A \code{data.table} with one row per person per at-risk snapshot.
#'   Always contains \code{personnel_id_col}, \code{ref_date_col},
#'   \code{outcome_col} (0/1), \code{age}, and \code{tenure_years}.
#'   Plus any requested \code{extra_covariates}.
#'
#' @seealso \code{\link{build_retirement_hazard_data}},
#'   \code{\link{fit_hazard_model}}
#'
#' @export
build_exit_hazard_data <- function(panel_contract_dt,
                                   panel_personnel_dt,
                                   active_types       = NULL,
                                   freq               = NULL,
                                   extra_covariates   = NULL,
                                   age_col            = "age",
                                   tenure_col         = "tenure_years",
                                   outcome_col        = "exited",
                                   personnel_id_col   = "personnel_id",
                                   ref_date_col       = "ref_date",
                                   birth_date_col     = "birth_date",
                                   start_date_col     = "start_date",
                                   end_date_col       = "end_date",
                                   contract_type_col  = "contract_type_code",
                                   contract_id_col    = "contract_id",
                                   salary_col         = "gross_salary_lcu") {

  # ------------------------------------------------------------------
  # 1. Coerce and validate
  # ------------------------------------------------------------------
  if (!data.table::is.data.table(panel_contract_dt))
    panel_contract_dt <- data.table::as.data.table(panel_contract_dt)
  if (!data.table::is.data.table(panel_personnel_dt))
    panel_personnel_dt <- data.table::as.data.table(panel_personnel_dt)

  for (col in c(ref_date_col, personnel_id_col, contract_type_col)) {
    if (!col %in% names(panel_contract_dt))
      stop("panel_contract_dt is missing required column: '", col, "'.",
           call. = FALSE)
  }
  for (col in c(ref_date_col, personnel_id_col)) {
    if (!col %in% names(panel_personnel_dt))
      stop("panel_personnel_dt is missing required column: '", col, "'.",
           call. = FALSE)
  }

  if (!is.null(extra_covariates)) {
    missing_extra <- setdiff(extra_covariates, names(panel_contract_dt))
    if (length(missing_extra) > 0L)
      stop("extra_covariates not found in panel_contract_dt: ",
           paste(missing_extra, collapse = ", "), call. = FALSE)
  }

  panel_dates <- sort(unique(panel_personnel_dt[[ref_date_col]]))
  if (length(panel_dates) < 2L)
    stop("panel_personnel_dt must contain at least 2 distinct snapshot dates.",
         call. = FALSE)

  # ------------------------------------------------------------------
  # 2. Deduplicate to primary contract per person per snapshot.
  # ------------------------------------------------------------------
  keep_cols  <- unique(c(personnel_id_col, ref_date_col, contract_type_col,
                         start_date_col, end_date_col, contract_id_col,
                         salary_col, extra_covariates))
  keep_cols  <- intersect(keep_cols, names(panel_contract_dt))
  primary_dt <- .dedup_to_primary(
    dt               = panel_contract_dt[, .SD, .SDcols = keep_cols],
    by_cols          = c(personnel_id_col, ref_date_col),
    start_date_col   = start_date_col,
    salary_col       = salary_col,
    contract_id_col  = contract_id_col
  )

  # ------------------------------------------------------------------
  # 3. Detect exit events via govhr::detect_personnel_event().
  #    "fire" events = persons who disappear between consecutive snapshots.
  #    Infer freq from the median inter-snapshot gap when not supplied.
  # ------------------------------------------------------------------
  if (is.null(freq)) {
    .gaps  <- as.integer(diff(panel_dates))
    .med   <- stats::median(.gaps)
    freq   <- dplyr::case_when(
      .med >= 360 ~ "year",
      .med >=  85 ~ "quarter",
      .med >=  27 ~ "month",
      TRUE        ~ "day"
    )
  }

  fire_events <- govhr::detect_personnel_event(
    data       = panel_personnel_dt,
    id_col     = personnel_id_col,
    event_type = "fire",
    start_date = format(min(panel_dates)),
    end_date   = format(max(panel_dates)),
    freq       = freq
  )
  # fire_events columns: personnel_id_col, ref_date (date the exit was detected)

  # Exclude retirements: any person who appears as "pensioner" at ANY snapshot
  # is a retiree, not a non-retirement exit.  Anti-join on personnel_id only
  # because the fire event date (last active snapshot) won't match the pensioner
  # snapshot date (one period later), so a date-keyed anti-join would miss them.
  ever_pensioner <- unique(
    primary_dt[get(contract_type_col) == "pensioner",
               c(personnel_id_col), with = FALSE]
  )
  fire_events <- fire_events[!ever_pensioner, on = personnel_id_col]

  # Build person-period panel: everyone active at each snapshot, with exited flag.
  # When active_types is NULL (default), include all non-inactive, non-pensioner
  # contracts — mirroring get_active_contracts() convention.
  if (is.null(active_types)) {
    active_pool <- primary_dt[
      !get(contract_type_col) %in% "pensioner",
      c(personnel_id_col, ref_date_col), with = FALSE
    ]
  } else {
    active_pool <- primary_dt[
      get(contract_type_col) %in% active_types,
      c(personnel_id_col, ref_date_col), with = FALSE
    ]
  }
  active_pool[, (outcome_col) := 0L]
  fire_events[, (outcome_col) := 1L]

  # Update outcome on the active_pool rows that match a fire event
  active_pool[fire_events, (outcome_col) := 1L,
              on = c(personnel_id_col, ref_date_col)]

  reg_dt <- active_pool
  data.table::setorderv(reg_dt, c(personnel_id_col, ref_date_col))

  # Truncate: drop rows after the first exited = 1 per person
  reg_dt[,
    .first_exit_date := {
      ex_dates <- get(ref_date_col)[get(outcome_col) == 1L]
      if (length(ex_dates)) min(ex_dates) else as.Date(NA)
    },
    by = c(personnel_id_col)
  ]
  reg_dt <- reg_dt[
    is.na(.first_exit_date) | get(ref_date_col) <= .first_exit_date
  ]
  reg_dt[, .first_exit_date := NULL]

  # ------------------------------------------------------------------
  # 4. Attach extra_covariates from primary_dt
  # ------------------------------------------------------------------
  if (!is.null(extra_covariates) && length(extra_covariates) > 0L) {
    cov_dt <- primary_dt[,
      c(personnel_id_col, ref_date_col, extra_covariates), with = FALSE
    ]
    reg_dt <- cov_dt[reg_dt, on = c(personnel_id_col, ref_date_col)]
  }

  # ------------------------------------------------------------------
  # 5. Attach age and tenure.
  # ------------------------------------------------------------------
  reg_dt <- .attach_age_tenure(
    reg_dt            = reg_dt,
    personnel_dt      = panel_personnel_dt,
    contract_dt       = panel_contract_dt,
    personnel_id_col  = personnel_id_col,
    ref_date_col      = ref_date_col,
    age_col           = age_col,
    tenure_col        = tenure_col,
    birth_date_col    = birth_date_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    contract_type_col = contract_type_col,
    contract_id_col   = contract_id_col
  )

  # ------------------------------------------------------------------
  # 6. Canonical column order
  # ------------------------------------------------------------------
  front_cols <- c(personnel_id_col, ref_date_col, outcome_col)
  data.table::setcolorder(
    reg_dt,
    c(front_cols, setdiff(names(reg_dt), front_cols))
  )

  reg_dt
}


# =============================================================================
# fit_hazard_model
# =============================================================================

#' Fit a Discrete-Time Hazard Model
#'
#' @description
#' Estimates a binomial GLM for discrete-time survival (hazard) analysis.
#' Each row in \code{reg_dt} is one person-period observation; the outcome
#' column equals \code{1} in the period the event occurs and \code{0} in all
#' earlier periods.  The returned \code{hazard_model} object is consumed by
#' \code{\link{select_hazard_threshold}} (to calibrate a decision threshold)
#' and \code{\link{predict_hazard}} (to score a simulated workforce).
#'
#' @details
#' **Link function** \cr
#' The complementary log-log link (\code{binomial(link = "cloglog")}) is the
#' theoretically preferred choice for discrete-time survival data because it
#' models the log cumulative hazard and is consistent with an underlying
#' continuous-time proportional-hazards process.
#' \code{binomial(link = "logit")} is a valid and more widely familiar
#' alternative when odds-ratio interpretability is preferred.
#'
#' **Complete-case fitting** \cr
#' Only rows that are complete across \code{outcome_col} and all
#' \code{covariates} are used for fitting.  If no complete cases remain an
#' error is raised.  Character covariates are coerced to factors automatically.
#'
#' **Metadata access** \cr
#' The returned object is intentionally slim — only \code{model},
#' \code{outcome_col}, and \code{threshold} are stored at the top level.
#' All other metadata is accessible through the embedded \code{glm} object:
#' \itemize{
#'   \item Covariates: \code{all.vars(stats::formula(hm$model))[-1]}
#'   \item Family / link: \code{stats::family(hm$model)$family} /
#'     \code{stats::family(hm$model)$link}
#'   \item Observations used: \code{length(hm$model$y)}
#'   \item Events: \code{sum(hm$model$y)}
#' }
#'
#' **Returned threshold** \cr
#' \code{$threshold} is initialised to \code{NA_real_}.  Call
#' \code{\link{select_hazard_threshold}} to populate it before passing the
#' object to \code{\link{predict_hazard}}.
#'
#' @param reg_dt data.frame or data.table.  Person-period regression dataset.
#'   Must contain \code{outcome_col} and all columns in \code{covariates}.
#' @param outcome_col Character scalar.  Name of the binary (0/1) outcome
#'   column.
#' @param covariates Character vector.  Predictor column names.
#' @param family A \code{\link[stats]{family}} object passed to
#'   \code{\link[stats]{glm}}.  Default: \code{binomial(link = "cloglog")}.
#'
#' @return A named list of class \code{"hazard_model"}:
#'   \describe{
#'     \item{\code{model}}{The fitted \code{glm} object.  All model metadata
#'       (formula, family, link, fitted values, residuals) is accessible here.}
#'     \item{\code{outcome_col}}{Character.  Stored explicitly to avoid
#'       formula parsing at prediction time.}
#'     \item{\code{threshold}}{\code{NA_real_} until
#'       \code{\link{select_hazard_threshold}} is called.}
#'   }
#'
#' @seealso \code{\link{select_hazard_threshold}}, \code{\link{predict_hazard}}
#'
#' @examples
#' \dontrun{
#' reg_dt <- data.frame(
#'   retired    = c(0L, 0L, 1L, 0L, 1L, 0L),
#'   age        = c(55, 58, 62, 50, 61, 45),
#'   tenure_yrs = c(20, 25, 30, 15, 28, 10)
#' )
#' hm <- fit_hazard_model(
#'   reg_dt      = reg_dt,
#'   outcome_col = "retired",
#'   covariates  = c("age", "tenure_yrs")
#' )
#' sum(hm$model$y)       # number of events
#' hm$threshold          # NA_real_ until select_hazard_threshold() is called
#' }
#'
#' @export
fit_hazard_model <- function(reg_dt,
                             outcome_col,
                             covariates,
                             family = binomial(link = "cloglog")) {

  # ------------------------------------------------------------------
  # 1. Input validation
  # ------------------------------------------------------------------
  if (!is.data.frame(reg_dt))
    stop("reg_dt must be a data.frame or data.table.", call. = FALSE)

  if (!is.character(outcome_col) || length(outcome_col) != 1L)
    stop("outcome_col must be a single character string.", call. = FALSE)

  if (!outcome_col %in% names(reg_dt))
    stop("outcome_col '", outcome_col, "' not found in reg_dt.", call. = FALSE)

  if (!is.character(covariates) || length(covariates) == 0L)
    stop("covariates must be a non-empty character vector.", call. = FALSE)

  missing_covs <- setdiff(covariates, names(reg_dt))
  if (length(missing_covs) > 0L)
    stop("Covariates not found in reg_dt: ",
         paste(missing_covs, collapse = ", "), call. = FALSE)

  # Outcome must be binary 0/1 (NAs allowed at this stage)
  y        <- reg_dt[[outcome_col]]
  bad_vals <- y[!is.na(y) & !y %in% c(0L, 1L, 0, 1)]
  if (length(bad_vals) > 0L)
    stop("outcome_col '", outcome_col,
         "' must contain only 0, 1, or NA.", call. = FALSE)

  if (all(is.na(y)))
    stop("outcome_col '", outcome_col, "' is entirely NA.", call. = FALSE)

  # ------------------------------------------------------------------
  # 2. Select relevant columns, keep complete cases
  # ------------------------------------------------------------------
  model_cols <- c(outcome_col, covariates)
  model_dt   <- data.table::as.data.table(reg_dt)[, .SD, .SDcols = model_cols]
  model_dt   <- stats::na.omit(model_dt)

  if (nrow(model_dt) == 0L)
    stop("No complete cases remain after removing rows with missing values.",
         call. = FALSE)

  # ------------------------------------------------------------------
  # 3. Coerce character covariates to factor
  # ------------------------------------------------------------------
  model_dt[, (covariates) := lapply(.SD, .chr_to_factor), .SDcols = covariates]

  # ------------------------------------------------------------------
  # 4. Fit GLM
  # ------------------------------------------------------------------
  eqn <- stats::as.formula(
    paste0(outcome_col, " ~ ", paste(covariates, collapse = " + "))
  )
  model_obj <- stats::glm(eqn, family = family, data = model_dt)

  # ------------------------------------------------------------------
  # 5. Return slim hazard_model object
  # ------------------------------------------------------------------
  structure(
    list(
      model       = model_obj,
      outcome_col = outcome_col,
      threshold   = NA_real_
    ),
    class = "hazard_model"
  )
}


# =============================================================================
# select_hazard_threshold
# =============================================================================

#' Select Optimal Probability Threshold for a Hazard Model
#'
#' @description
#' Scores \code{reg_dt} using a fitted \code{\link{fit_hazard_model}} object,
#' sweeps 99 candidate thresholds from 0.01 to 0.99, and selects the cut-off
#' that maximises a classification criterion.  The selected threshold is
#' written into \code{hazard_model$threshold} and used by
#' \code{\link{predict_hazard}} to convert predicted probabilities into binary
#' event/no-event decisions.
#'
#' @details
#' **Threshold selection methods** \cr
#' \describe{
#'   \item{\code{"youden"}}{Youden's J statistic:
#'     \deqn{J = \text{sensitivity} + \text{specificity} - 1}
#'     Maximises the joint true-positive and true-negative rates.  Appropriate
#'     when over-predicting and under-predicting events are equally costly
#'     (the default for most workforce projection use cases).}
#'   \item{\code{"f1"}}{F1 score: harmonic mean of precision and recall.
#'     Appropriate when the positive class (events) is rare and false negatives
#'     are more costly than false positives.}
#' }
#'
#' **Tie-breaking** \cr
#' When multiple thresholds achieve the same maximum criterion value the
#' \emph{lowest} threshold is selected — the more conservative choice that
#' flags more potential events.
#'
#' **Validation data** \cr
#' \code{reg_dt} is typically the same training data passed to
#' \code{\link{fit_hazard_model}}, but an independent held-out validation set
#' can be passed for out-of-sample calibration.
#'
#' **Covariates** \cr
#' Predictor names are recovered from the model formula via
#' \code{all.vars(stats::formula(hazard_model$model))[-1]} so there is no
#' need to store them separately in the \code{hazard_model} object.
#'
#' @param hazard_model A \code{hazard_model} object returned by
#'   \code{\link{fit_hazard_model}}.
#' @param reg_dt data.frame or data.table.  Scoring dataset.  Must contain
#'   \code{hazard_model$outcome_col} and all predictor columns from the fitted
#'   model formula.
#' @param method Character scalar.  One of \code{"youden"} (default) or
#'   \code{"f1"}.
#'
#' @return The input \code{hazard_model} with three fields added or updated:
#'   \describe{
#'     \item{\code{threshold}}{Numeric.  The selected optimal threshold.}
#'     \item{\code{threshold_method}}{Character.  The method used.}
#'     \item{\code{threshold_diagnostics}}{data.table with 99 rows (one per
#'       candidate threshold) and columns: \code{threshold}, \code{youden},
#'       \code{f1}, \code{sensitivity}, \code{specificity}, \code{precision},
#'       \code{recall}.}
#'   }
#'
#' @seealso \code{\link{fit_hazard_model}}, \code{\link{predict_hazard}}
#'
#' @examples
#' \dontrun{
#' hm <- fit_hazard_model(reg_dt, outcome_col = "retired",
#'                        covariates = c("age", "tenure_yrs"))
#' hm <- select_hazard_threshold(hm, reg_dt, method = "youden")
#' hm$threshold  # e.g. 0.08
#' }
#'
#' @export
select_hazard_threshold <- function(hazard_model,
                                    reg_dt,
                                    method = "youden") {

  # ------------------------------------------------------------------
  # 1. Input validation
  # ------------------------------------------------------------------
  if (!inherits(hazard_model, "hazard_model"))
    stop("hazard_model must be an object returned by fit_hazard_model().",
         call. = FALSE)

  if (!is.data.frame(reg_dt))
    stop("reg_dt must be a data.frame or data.table.", call. = FALSE)

  method <- match.arg(method, c("youden", "f1"))

  outcome_col <- hazard_model$outcome_col
  covariates  <- all.vars(stats::formula(hazard_model$model))[-1L]

  missing_cols <- setdiff(c(outcome_col, covariates), names(reg_dt))
  if (length(missing_cols) > 0L)
    stop("Columns not found in reg_dt: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)

  y <- reg_dt[[outcome_col]]
  if (all(is.na(y)))
    stop("outcome_col '", outcome_col, "' is entirely NA in reg_dt.",
         call. = FALSE)

  # ------------------------------------------------------------------
  # 2. Prepare scoring data — complete cases, coerce characters to factors
  # ------------------------------------------------------------------
  score_cols <- c(outcome_col, covariates)
  score_dt   <- data.table::as.data.table(reg_dt)[, .SD, .SDcols = score_cols]
  score_dt   <- stats::na.omit(score_dt)
  score_dt[, (covariates) := lapply(.SD, .chr_to_factor), .SDcols = covariates]

  probs  <- stats::predict(hazard_model$model, newdata = score_dt,
                           type = "response")
  actual <- as.integer(score_dt[[outcome_col]])

  # ------------------------------------------------------------------
  # 3. Sweep candidates and compute metrics (fully vectorised)
  #
  # outer() builds an n x 99 logical matrix in one call:
  #   predicted_mat[i, j] = TRUE iff probs[i] >= candidates[j]
  # colSums() then gives TP/TN/FP/FN for all thresholds simultaneously,
  # avoiding any R-level loop.
  # ------------------------------------------------------------------
  candidates    <- seq(0.01, 0.99, by = 0.01)
  predicted_mat <- outer(probs, candidates, ">=")   # n x 99 logical matrix

  pos <- actual == 1L
  neg <- actual == 0L

  tp <- colSums( predicted_mat &  pos, na.rm = TRUE)
  tn <- colSums(!predicted_mat &  neg, na.rm = TRUE)
  fp <- colSums( predicted_mat &  neg, na.rm = TRUE)
  fn <- colSums(!predicted_mat &  pos, na.rm = TRUE)

  sens <- ifelse(tp + fn > 0L, tp / (tp + fn), 0)
  spec <- ifelse(tn + fp > 0L, tn / (tn + fp), 0)
  prec <- ifelse(tp + fp > 0L, tp / (tp + fp), 0)
  f1_v <- ifelse(prec + sens > 0, 2 * prec * sens / (prec + sens), 0)

  diag_dt <- data.table::data.table(
    threshold   = candidates,
    youden      = sens + spec - 1,
    f1          = f1_v,
    sensitivity = sens,
    specificity = spec,
    precision   = prec,
    recall      = sens
  )

  # ------------------------------------------------------------------
  # 4. Select optimal threshold (first maximum = lowest threshold wins)
  # ------------------------------------------------------------------
  criterion_col <- if (method == "youden") "youden" else "f1"
  best_thr      <- diag_dt[which.max(get(criterion_col)), threshold]

  # ------------------------------------------------------------------
  # 5. Update hazard_model and return
  # ------------------------------------------------------------------
  hazard_model$threshold             <- best_thr
  hazard_model$threshold_method      <- method
  hazard_model$threshold_diagnostics <- diag_dt

  hazard_model
}


# =============================================================================
# predict_hazard
# =============================================================================

#' Score a Workforce Snapshot with a Fitted Hazard Model
#'
#' @description
#' Applies a fitted \code{\link{fit_hazard_model}} object to a single workforce
#' snapshot, returning a predicted event probability and a binary event
#' indicator for each active person.
#'
#' This is the scoring counterpart to \code{\link{build_retirement_hazard_data}}
#' and \code{\link{build_exit_hazard_data}}: it performs the same covariate
#' preparation steps (deduplication to primary contract, age/tenure
#' computation) on a \emph{single-period} snapshot rather than a historical
#' stacked panel, then calls \code{\link[stats]{predict.glm}} to generate
#' probabilities.
#'
#' @details
#' **Covariate preparation** \cr
#' The function recovers the predictor names from the fitted model formula via
#' \code{all.vars(stats::formula(hazard_model$model))[-1]}.  It then selects
#' those columns from the deduped primary contract (one row per person), after
#' attaching age and tenure if they appear among the required predictors.
#' Character columns are coerced to the same factor levels used at fit time
#' via \code{stats::predict} with \code{newdata}; unseen levels produce
#' \code{NA} probabilities, which are mapped to \code{event = 0L} (no
#' event predicted).
#'
#' **Threshold requirement** \cr
#' \code{hazard_model$threshold} must not be \code{NA}.  Call
#' \code{\link{select_hazard_threshold}} before \code{predict_hazard()}.
#'
#' **Return value** \cr
#' One row per active person in the snapshot.  Persons whose primary contract
#' type is \code{"inactive"} or \code{"pensioner"} are excluded from the
#' at-risk pool and do not appear in the output.
#'
#' @param hazard_model A \code{hazard_model} object with a calibrated
#'   \code{threshold} (i.e. after calling
#'   \code{\link{select_hazard_threshold}}).
#' @param contract_dt data.table.  Single-period contract snapshot.  Must not
#'   contain a \code{ref_date_col} column (or, if it does, all rows should
#'   belong to the same snapshot date — no stacking).
#' @param personnel_dt data.table.  Single-period personnel snapshot.  Used
#'   for age computation if \code{age_col} is required by the model and not
#'   already present.
#' @param age_col Character scalar or \code{NULL}.  Pre-computed age column on
#'   \code{personnel_dt}.  Passed through to the covariate preparation step.
#'   Default: \code{"age"}.
#' @param tenure_col Character scalar or \code{NULL}.  Pre-computed tenure
#'   column on \code{personnel_dt}.  Default: \code{"tenure_years"}.
#' @param personnel_id_col Character.  Default: \code{"personnel_id"}.
#' @param birth_date_col Character.  Default: \code{"birth_date"}.
#' @param start_date_col Character.  Default: \code{"start_date"}.
#' @param end_date_col Character.  Default: \code{"end_date"}.
#' @param contract_type_col Character.  Default: \code{"contract_type_code"}.
#' @param contract_id_col Character.  Default: \code{"contract_id"}.
#' @param salary_col Character.  Tiebreaker column for primary contract
#'   selection.  Default: \code{"gross_salary_lcu"}.
#' @param ref_date Date scalar or \code{NULL}.  The snapshot date, used as
#'   \code{ref_date} when computing tenure and age on-the-fly (slow path).
#'   Required when neither \code{age_col} nor \code{tenure_col} is
#'   pre-computed and the model includes those predictors.  Default:
#'   \code{NULL}.
#'
#' @return A \code{data.table} with one row per active person:
#'   \describe{
#'     \item{\code{personnel_id_col}}{Person identifier.}
#'     \item{\code{prob}}{Numeric.  Predicted event probability from the GLM.
#'       \code{NA} for persons with unseen covariate levels.}
#'     \item{\code{event}}{Integer 0/1.  \code{1} iff
#'       \code{prob >= hazard_model$threshold}; \code{0} otherwise (including
#'       when \code{prob} is \code{NA}).}
#'   }
#'
#' @seealso \code{\link{fit_hazard_model}}, \code{\link{select_hazard_threshold}}
#'
#' @examples
#' \dontrun{
#' hm <- fit_hazard_model(reg_dt, "retired", c("age", "tenure_years"))
#' hm <- select_hazard_threshold(hm, reg_dt)
#'
#' preds <- predict_hazard(
#'   hazard_model   = hm,
#'   contract_dt    = current_contracts,
#'   personnel_dt   = current_personnel,
#'   ref_date       = as.Date("2025-01-01")
#' )
#' sum(preds$event)  # expected number of events this period
#' }
#'
#' @export
predict_hazard <- function(hazard_model,
                           contract_dt,
                           personnel_dt,
                           age_col           = "age",
                           tenure_col        = "tenure_years",
                           personnel_id_col  = "personnel_id",
                           birth_date_col    = "birth_date",
                           start_date_col    = "start_date",
                           end_date_col      = "end_date",
                           contract_type_col = "contract_type_code",
                           contract_id_col   = "contract_id",
                           salary_col        = "gross_salary_lcu",
                           ref_date          = NULL) {

  # ------------------------------------------------------------------
  # 1. Validate inputs
  # ------------------------------------------------------------------
  if (!inherits(hazard_model, "hazard_model"))
    stop("hazard_model must be an object returned by fit_hazard_model().",
         call. = FALSE)

  if (is.na(hazard_model$threshold))
    stop("hazard_model$threshold is NA. ",
         "Call select_hazard_threshold() before predict_hazard().",
         call. = FALSE)

  if (!data.table::is.data.table(contract_dt))
    contract_dt <- data.table::as.data.table(contract_dt)
  if (!data.table::is.data.table(personnel_dt))
    personnel_dt <- data.table::as.data.table(personnel_dt)

  for (col in c(personnel_id_col, contract_type_col)) {
    if (!col %in% names(contract_dt))
      stop("contract_dt is missing required column: '", col, "'.",
           call. = FALSE)
  }
  if (!personnel_id_col %in% names(personnel_dt))
    stop("personnel_dt is missing required column: '",
         personnel_id_col, "'.", call. = FALSE)

  # ------------------------------------------------------------------
  # 2. Recover covariates from fitted model formula
  # ------------------------------------------------------------------
  covariates <- all.vars(stats::formula(hazard_model$model))[-1L]

  # ------------------------------------------------------------------
  # 3. Deduplicate to primary contract per person (snapshot — no ref_date).
  # ------------------------------------------------------------------
  keep_cols  <- unique(c(personnel_id_col, contract_type_col,
                         start_date_col, end_date_col, contract_id_col,
                         salary_col, covariates))
  keep_cols  <- intersect(keep_cols, names(contract_dt))
  primary_dt <- .dedup_to_primary(
    dt               = contract_dt[
                         !get(contract_type_col) %in% c("inactive", "pensioner"),
                         .SD, .SDcols = keep_cols
                       ],
    by_cols          = personnel_id_col,
    start_date_col   = start_date_col,
    salary_col       = salary_col,
    contract_id_col  = contract_id_col
  )

  # ------------------------------------------------------------------
  # 4. Attach age and tenure (snapshot path — ref_date_col = NULL).
  # ------------------------------------------------------------------
  primary_dt <- .attach_age_tenure(
    reg_dt            = primary_dt,
    personnel_dt      = personnel_dt,
    contract_dt       = contract_dt,
    personnel_id_col  = personnel_id_col,
    ref_date_col      = NULL,
    age_col           = age_col,
    tenure_col        = tenure_col,
    birth_date_col    = birth_date_col,
    start_date_col    = start_date_col,
    end_date_col      = end_date_col,
    contract_type_col = contract_type_col,
    contract_id_col   = contract_id_col,
    ref_date          = ref_date
  )

  # ------------------------------------------------------------------
  # 5. Select only covariate columns + id for scoring
  # ------------------------------------------------------------------
  missing_covs <- setdiff(covariates, names(primary_dt))
  if (length(missing_covs) > 0L)
    stop("Required model covariates not found in prepared scoring data: ",
         paste(missing_covs, collapse = ", "), call. = FALSE)

  score_dt <- primary_dt[, c(personnel_id_col, covariates), with = FALSE]

  # ------------------------------------------------------------------
  # 6. Score: predict probabilities; apply threshold to get event flag.
  #    NA probabilities (unseen factor levels) → event = 0L.
  # ------------------------------------------------------------------
  probs <- tryCatch(
    stats::predict(hazard_model$model,
                   newdata = score_dt,
                   type    = "response"),
    warning = function(w) {
      suppressWarnings(
        stats::predict(hazard_model$model,
                       newdata = score_dt,
                       type    = "response")
      )
    }
  )

  out <- data.table::data.table(
    pid   = score_dt[[personnel_id_col]],
    prob  = as.numeric(probs),
    event = as.integer(!is.na(probs) & probs >= hazard_model$threshold)
  )
  data.table::setnames(out, "pid", personnel_id_col)

  # Canonical column order
  data.table::setcolorder(out, c(personnel_id_col, "prob", "event"))

  out
}
