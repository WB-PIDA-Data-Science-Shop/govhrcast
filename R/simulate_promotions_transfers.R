#' Simulate Promotions and Transfers for a Single Projection Period
#'
#' Simulates internal workforce movements — promotions, transfers, or both —
#' for one projection period by estimating or applying a transition matrix,
#' selecting movers using a ranking strategy, updating group assignments and
#' salaries, and returning updated workforce tables together with a summary
#' of movement outcomes.
#'
#' @description
#' `simulate_promotions_transfers()` is the internal movement module used
#' throughout **govhrcast**. During a single simulation period it:
#'
#' 1. Validates the supplied workforce data and movement policy.
#' 2. Estimates an empirical transition matrix from panel data when no
#'    `policy_table` is supplied, or uses the supplied matrix directly.
#' 3. Selects the appropriate workforce snapshot when panel data are supplied.
#' 4. Computes movement demand — the expected number of movers per
#'    `from_group → to_group` transition using stochastic rounding.
#' 5. Selects individual employees to move using the specified ranking
#'    strategy.
#' 6. Updates group assignments and optionally re-assigns salaries from the
#'    pay scale.
#' 7. Returns updated workforce tables together with a summary of movement
#'    outcomes.
#'
#' This function simulates a **single projection period**. Multi-period
#' workforce projections should instead use [simulate_horizon()], which
#' repeatedly calls `simulate_promotions_transfers()` over successive
#' simulation periods.
#'
#' @details
#'
#' ## Movement workflow
#'
#' Promotions and transfers simulation proceeds through the following stages:
#'
#' 1. Validate all inputs.
#' 2. Estimate or load the transition baseline (see **Transition baseline**
#'    below).
#' 3. Select the workforce snapshot nearest (but not later than) `ref_date`
#'    when panel data are supplied.
#' 4. Compute movement demand via [compute_movement_demand()] when a
#'    `policy_table` is supplied, or via [compute_fixed_rate_movements()]
#'    when it is not. Demand is expressed as integer mover counts per
#'    `from_group → to_group` pair using stochastic rounding.
#' 5. Select individual movers via [identify_movers()] using the specified
#'    `movement_strategy`.
#' 6. Update state via [update_state_with_movement()] — the only
#'    state-modifying step.
#' 7. Return updated data and summary statistics.
#'
#' Input data are **never modified in place**. Both `contract_dt` and
#' `personnel_dt` are copied internally before any processing occurs.
#'
#' ## Transition baseline
#'
#' The transition baseline is a `from_group → to_group` matrix of movement
#' rates. Two paths are available:
#'
#' **Empirical estimation** (`policy_table = NULL`, panel data supplied):
#' [estimate_movement_baseline()] processes consecutive snapshot pairs
#' (T0→T1, T1→T2, …), counts observed transitions per pair, and averages
#' across all pairs to produce `avg_prob` per `from_group → to_group` cell.
#' Only actual transitions (`from_group ≠ to_group`) are retained. Groups
#' with NA in any `group_col` are dropped. This is the natural baseline for
#' status quo projections.
#'
#' **User-supplied matrix** (`policy_table` provided): the supplied
#' `data.table` is used directly as the transition baseline, bypassing
#' estimation entirely. Use this path for reform scenarios where you want
#' to impose specific transition probabilities — for example doubling
#' promotion rates into senior grades — without being constrained by
#' historical patterns.
#'
#' **Flat scalar rate** (`policy_table = NULL`, single snapshot): when only
#' one snapshot is available and no `policy_table` is supplied, the function
#' falls back to [compute_fixed_rate_movements()], which applies
#' `defaults$movement_rate` uniformly across the active workforce and
#' distributes movers equally across all valid destinations in
#' `salary_scale_dt`.
#'
#' ## Demand calculation
#'
#' When a transition baseline is available, movement demand per
#' `from_group → to_group` pair is computed as:
#'
#' ```
#' expected_n = current_stock × movement_rate
#' n_movers   = floor(expected_n) + Bernoulli(expected_n - floor(expected_n))
#' ```
#'
#' Total outflow from any `from_group` is capped at 1.0 — if the sum of
#' transition probabilities out of a group exceeds 1, all rates are
#' rescaled proportionally before demand is computed. Destinations not
#' present in `salary_scale_dt` are scrubbed and a message is emitted
#' for each scrubbed transition.
#'
#' ## Mover selection strategies
#'
#' Once demand is established, [identify_movers()] selects individuals
#' from each `from_group` pool. Each employee can be selected at most once
#' across all transitions in a period. Four strategies are supported via
#' `defaults$movement_strategy`:
#'
#' * `"tenure"` — employees with the longest time-in-grade are selected
#'   first. Time-in-grade is computed from panel history when available,
#'   falling back to contract start date for single-snapshot data. This
#'   models seniority-based promotion systems.
#' * `"wage_based"` — employees are ranked by their salary as a proportion
#'   of the maximum salary in their group (`salary / max_salary`). Those
#'   with the lowest ratio — furthest from the group ceiling — are selected
#'   first. This models systems where those most underpaid relative to
#'   their grade ceiling are next in line for promotion.
#' * `"reverse_tenure"` — employees with the shortest overall tenure are
#'   selected first. Useful for modelling transfer policies that rotate
#'   newer staff across establishments.
#' * `"random"` (default) — employees are drawn uniformly at random from
#'   the eligible pool. Any unrecognised strategy value also falls back
#'   to random.
#'
#' The `promotion_order_col` override in `policy_params` takes precedence
#' over `movement_strategy` when supplied: employees are ranked in
#' descending order of that column and the highest-ranked are selected
#' first.
#'
#' ## Salary update after movement
#'
#' Two salary update rules are available via `defaults$salary_update_rule`:
#'
#' * `"scale"` (default) — the mover's salary is replaced with the value
#'   from `salary_scale_dt` for their new `to_group`. The salary column
#'   detected in `salary_scale_dt` must match `salary_col`.
#' * `"keep"` — the mover retains their current salary. Use this when
#'   salary adjustments are handled separately, for example through a
#'   merit increment applied after movement.
#'
#' ## Defining promotions vs transfers
#'
#' The distinction between promotions and transfers is determined entirely
#' by the choice of `group_cols`, not by an internal flag:
#'
#' * `group_cols = "paygrade"` — movements are vertical promotions between
#'   grades within the same establishment.
#' * `group_cols = "est_id"` — movements are lateral transfers between
#'   establishments within the same grade.
#' * `group_cols = c("est_id", "paygrade")` — movements can be either or
#'   both simultaneously, with the empirical baseline capturing the actual
#'   mix observed in the panel.
#'
#' ## Interaction with other modules
#'
#' Within each simulation period, [simulate_retirement()] and
#' [simulate_exits()] run before `simulate_promotions_transfers()`.
#' The workforce passed to this function therefore reflects a post-attrition
#' active pool. Movement demand is computed against this pool. Headcount
#' is unchanged by this module — movements reassign workers between groups
#' but do not add or remove them.
#'
#' @param policy_params A named list describing the movement policy.
#'
#' The list contains three elements:
#'
#' **group_cols**
#'
#' Character vector identifying the variables that define movement states.
#' Examples include `"paygrade"` (promotions only), `"est_id"` (transfers
#' only), or `c("est_id", "paygrade")` (both). The same columns must be
#' present in both the workforce data and `salary_scale_dt`.
#'
#' **policy_table**
#'
#' A `data.table` containing the transition baseline. Must have columns
#' `from_group`, `to_group`, and `movement_rate`, where group keys are
#' the `||`-concatenated values of `group_cols` (e.g.
#' `"SECRETARIA||Grade A"`).
#'
#' Pass the output of [estimate_movement_baseline()] for status quo
#' projections, or supply a custom matrix for reform scenarios.
#'
#' Use `NULL` to trigger automatic estimation from panel data (requires
#' ≥2 snapshots) or flat scalar rate application (single snapshot).
#'
#' **defaults**
#'
#' Named list containing default movement policy parameters. The following
#' fields are recognised:
#'
#' | Parameter | Description |
#' |:----------|:------------|
#' | `movement_rate` | Scalar attrition rate used when `policy_table = NULL` and only one snapshot is available |
#' | `movement_strategy` | Mover selection rule: `"tenure"`, `"wage_based"`, `"reverse_tenure"`, or `"random"` |
#' | `active_types` | Contract type values treated as eligible for movement. Must match actual `contract_type` values (e.g. `"permanent"`) |
#' | `salary_update_rule` | `"scale"` (assign destination salary from `salary_scale_dt`) or `"keep"` (retain current salary) |
#'
#' @param contract_dt A `data.table` (or object coercible to a
#' `data.table`) containing the contract register in **govhr** harmonized
#' format.
#'
#' May be a single-period snapshot or a longitudinal panel containing
#' multiple `ref_date` values. When multiple snapshots are present, the
#' snapshot nearest (but not later than) `ref_date` is used for the
#' simulation, while all snapshots are used for baseline estimation and
#' time-in-grade calculation.
#'
#' Must contain at least the columns identified by `personnel_id_col`,
#' `contract_id_col`, `start_date_col`, `end_date_col`,
#' `contract_type_col`, and `salary_col`.
#'
#' @param personnel_dt A `data.table` (or object coercible to a
#' `data.table`) containing the personnel register in **govhr** harmonized
#' format.
#'
#' Must contain at least the columns identified by `personnel_id_col` and
#' `status_col`.
#'
#' @param salary_scale_dt A `data.table` keyed on `group_cols` containing
#' the reference salary for each group. Used both to validate movement
#' destinations (transitions whose `to_group` is absent from
#' `salary_scale_dt` are scrubbed) and to assign salaries to movers when
#' `salary_update_rule = "scale"`.
#'
#' Must have exactly one row per group — duplicate keys will cause an
#' error. Must contain a numeric salary column matching `salary_col`.
#'
#' @param ref_date A `Date` giving the simulation date for the current
#' projection period. Used to select the workforce snapshot, compute
#' time-in-grade, and compute overall tenure.
#'
#' @param ref_date_col Character scalar giving the name of the reference
#' date column used in workforce panel datasets. Used to identify and
#' iterate over snapshot pairs during baseline estimation and time-in-grade
#' calculation. Default is `"ref_date"`.
#'
#' @param personnel_id_col Character scalar giving the unique personnel
#' identifier shared by both `contract_dt` and `personnel_dt`.
#' Default is `"personnel_id"`.
#'
#' @param contract_id_col Character scalar identifying unique contracts.
#' Default is `"contract_id"`.
#'
#' @param start_date_col Character scalar identifying the contract start
#' date column. Used to compute tenure and as a fallback for time-in-grade
#' when panel history is unavailable. Default is `"start_date"`.
#'
#' @param end_date_col Character scalar identifying the contract end date
#' column. Active contracts contain `NA` here. Default is `"end_date"`.
#'
#' @param salary_col Character scalar identifying the primary salary column
#' in `contract_dt`. Used to capture pre-move salaries for reporting and
#' to assign post-move salaries when `salary_update_rule = "scale"`.
#' Default is `"gross_salary_lcu"`.
#'
#' @param contract_type_col Character scalar identifying the contract type
#' variable. Only contract types listed in `policy_params$defaults$active_types`
#' are eligible for movement. Default is `"contract_type"`.
#'
#' @param status_col Character scalar identifying the employment status
#' variable in `personnel_dt`. Only personnel with status `"active"` are
#' included in the eligible pool. Default is `"employment_status"`.
#'
#' @return
#' A named list with six elements:
#'
#' * **summary**
#'
#'   A one-row `data.table` summarizing movement outcomes for the current
#'   simulation period:
#'
#'   | Column | Description |
#'   |:-------|:------------|
#'   | `n_movers` | Number of employees who actually moved |
#'   | `n_movers_demanded` | Total movers demanded by the transition matrix |
#'   | `hist_avg_movement_rate` | Mean movement rate across all `from → to` pairs in the baseline matrix |
#'   | `headcount_before` | Active headcount before movements |
#'   | `headcount_after` | Active headcount after movements (equal to `headcount_before` — movements do not change headcount) |
#'
#' * **contract_dt**
#'
#'   The updated contract register. Movers have their `group_cols` columns
#'   updated to reflect their new group assignment. When
#'   `salary_update_rule = "scale"`, their salary column is also updated
#'   to the destination group salary from `salary_scale_dt`.
#'
#' * **personnel_dt**
#'
#'   The personnel register, returned unchanged. Movement does not alter
#'   personnel-level status fields in the current implementation.
#'
#' * **movers_dt**
#'
#'   A `data.table` containing one row per employee who moved during the
#'   current period, with columns:
#'
#'   | Column | Description |
#'   |:-------|:------------|
#'   | `personnel_id` | Employee identifier |
#'   | `from_group` | Origin group key (`\|\|`-concatenated `group_cols` values) |
#'   | `to_group` | Destination group key |
#'   | `salary_before` | Salary at the time of movement, before any update |
#'
#'   Returns an empty `data.table` when no movements occur.
#'
#' * **baseline_matrix**
#'
#'   The transition baseline used for the simulation — either the estimated
#'   matrix from [estimate_movement_baseline()] or the user-supplied
#'   `policy_table`. Empty `data.table` when the flat scalar rate path
#'   was used.
#'
#' * **demand_dt**
#'
#'   A `data.table` showing computed movement demand per
#'   `from_group → to_group` pair, with columns `from_group`, `to_group`,
#'   `movement_rate`, `current_stock`, and `n_movers`.
#'
#' @examples
#' \dontrun{
#'
#' library(data.table)
#' library(govhrcast)
#'
#' contract_dt  <- copy(bra_hrmis_contract)
#' personnel_dt <- copy(bra_hrmis_personnel)
#' ref_date     <- as.Date("2017-09-01")
#'
#' salary_scale_dt <- contract_dt[
#'   ref_date == "2017-09-01",
#'   .(gross_salary_cpi = mean(gross_salary_cpi, na.rm = TRUE)),
#'   by = .(est_id, paygrade)
#' ]
#'
#' ############################################################
#' ## Example 1: Status quo — empirical baseline estimated
#' ##            from panel data
#' ##
#' ## policy_table = NULL with >= 2 snapshots triggers automatic
#' ## estimation of the transition matrix from consecutive pairs.
#' ## This is the natural baseline for status quo projections.
#' ############################################################
#'
#' movement_policy <- list(
#'   group_cols   = c("est_id", "paygrade"),
#'   policy_table = NULL,
#'   defaults = list(
#'     movement_rate      = 0,
#'     movement_strategy  = "tenure",
#'     active_types       = "permanent",
#'     salary_update_rule = "scale"
#'   )
#' )
#'
#' results <- simulate_promotions_transfers(
#'   contract_dt     = contract_dt,
#'   personnel_dt    = personnel_dt,
#'   salary_scale_dt = salary_scale_dt,
#'   policy_params   = movement_policy,
#'   ref_date        = ref_date,
#'   salary_col      = "gross_salary_cpi"
#' )
#'
#' results$summary
#' head(results$baseline_matrix)
#' head(results$movers_dt)
#'
#' ############################################################
#' ## Example 2: Reform scenario — user-supplied transition
#' ##            matrix doubling promotion rates into senior grades
#' ##
#' ## Estimate the historical baseline first, then scale up
#' ## promotion probabilities for a targeted reform scenario.
#' ############################################################
#'
#' baseline <- estimate_movement_baseline(
#'   contract_dt       = contract_dt,
#'   group_cols        = c("est_id", "paygrade"),
#'   personnel_id_col  = "personnel_id",
#'   ref_date_col      = "ref_date",
#'   start_date_col    = "start_date",
#'   end_date_col      = "end_date",
#'   contract_type_col = "contract_type"
#' )
#'
#' # Double movement rates into the two most senior paygrades
#' reform_matrix <- data.table::copy(baseline)
#' reform_matrix[
#'   grepl("Grade C|Grade D", to_group),
#'   movement_rate := pmin(movement_rate * 2, 1.0)
#' ]
#'
#' movement_policy_reform <- list(
#'   group_cols   = c("est_id", "paygrade"),
#'   policy_table = reform_matrix,
#'   defaults = list(
#'     movement_rate      = 0,
#'     movement_strategy  = "tenure",
#'     active_types       = "permanent",
#'     salary_update_rule = "scale"
#'   )
#' )
#'
#' results_reform <- simulate_promotions_transfers(
#'   contract_dt     = contract_dt,
#'   personnel_dt    = personnel_dt,
#'   salary_scale_dt = salary_scale_dt,
#'   policy_params   = movement_policy_reform,
#'   ref_date        = ref_date,
#'   salary_col      = "gross_salary_cpi"
#' )
#'
#' results_reform$summary
#'
#' ############################################################
#' ## Example 3: Single snapshot — flat scalar movement rate
#' ##
#' ## When only one snapshot is available and policy_table = NULL,
#' ## the function falls back to a flat rate applied uniformly
#' ## across the active workforce, with movers distributed equally
#' ## across all valid destinations in salary_scale_dt.
#' ############################################################
#'
#' snap_contract_dt <- contract_dt[ref_date == as.Date("2017-09-01")]
#' snap_personnel_dt <- personnel_dt[ref_date == as.Date("2017-09-01")]
#'
#' movement_policy_flat <- list(
#'   group_cols   = c("est_id", "paygrade"),
#'   policy_table = NULL,
#'   defaults = list(
#'     movement_rate      = 0.05,
#'     movement_strategy  = "random",
#'     active_types       = "permanent",
#'     salary_update_rule = "scale"
#'   )
#' )
#'
#' results_flat <- simulate_promotions_transfers(
#'   contract_dt     = snap_contract_dt,
#'   personnel_dt    = snap_personnel_dt,
#'   salary_scale_dt = salary_scale_dt,
#'   policy_params   = movement_policy_flat,
#'   ref_date        = ref_date,
#'   salary_col      = "gross_salary_cpi"
#' )
#'
#' results_flat$summary
#'
#' ############################################################
#' ## Example 4: Paygrade-only promotions, tenure-based selection
#' ##
#' ## group_cols = "paygrade" restricts movements to vertical
#' ## promotions within grades regardless of establishment.
#' ## Employees with the longest time-in-grade are promoted first.
#' ############################################################
#'
#' salary_scale_grade <- contract_dt[
#'   ref_date == "2017-09-01",
#'   .(gross_salary_cpi = mean(gross_salary_cpi, na.rm = TRUE)),
#'   by = "paygrade"
#' ]
#'
#' movement_policy_promotions <- list(
#'   group_cols   = "paygrade",
#'   policy_table = NULL,
#'   defaults = list(
#'     movement_rate      = 0,
#'     movement_strategy  = "tenure",
#'     active_types       = "permanent",
#'     salary_update_rule = "scale"
#'   )
#' )
#'
#' results_promotions <- simulate_promotions_transfers(
#'   contract_dt     = contract_dt,
#'   personnel_dt    = personnel_dt,
#'   salary_scale_dt = salary_scale_grade,
#'   policy_params   = movement_policy_promotions,
#'   ref_date        = ref_date,
#'   salary_col      = "gross_salary_cpi"
#' )
#'
#' results_promotions$summary
#'
#' ############################################################
#' ## Example 5: Establishment-only transfers, random selection
#' ##
#' ## group_cols = "est_id" restricts movements to lateral
#' ## transfers between establishments with no grade change.
#' ## Employees are selected uniformly at random.
#' ############################################################
#'
#' salary_scale_est <- contract_dt[
#'   ref_date == "2017-09-01",
#'   .(gross_salary_cpi = mean(gross_salary_cpi, na.rm = TRUE)),
#'   by = "est_id"
#' ]
#'
#' movement_policy_transfers <- list(
#'   group_cols   = "est_id",
#'   policy_table = NULL,
#'   defaults = list(
#'     movement_rate      = 0,
#'     movement_strategy  = "random",
#'     active_types       = "permanent",
#'     salary_update_rule = "keep"   # salary unchanged on lateral transfer
#'   )
#' )
#'
#' results_transfers <- simulate_promotions_transfers(
#'   contract_dt     = contract_dt,
#'   personnel_dt    = personnel_dt,
#'   salary_scale_dt = salary_scale_est,
#'   policy_params   = movement_policy_transfers,
#'   ref_date        = ref_date,
#'   salary_col      = "gross_salary_cpi"
#' )
#'
#' results_transfers$summary
#' head(results_transfers$movers_dt)
#'
#' ############################################################
#' ## Example 6: salary_update_rule = "keep"
#' ##
#' ## Movers change group assignment but retain their current
#' ## salary. Useful when salary adjustments are handled
#' ## separately through a merit increment rather than a step
#' ## change at the point of promotion.
#' ############################################################
#'
#' movement_policy_keep <- list(
#'   group_cols   = c("est_id", "paygrade"),
#'   policy_table = NULL,
#'   defaults = list(
#'     movement_rate      = 0,
#'     movement_strategy  = "wage_based",
#'     active_types       = "permanent",
#'     salary_update_rule = "keep"
#'   )
#' )
#'
#' results_keep <- simulate_promotions_transfers(
#'   contract_dt     = contract_dt,
#'   personnel_dt    = personnel_dt,
#'   salary_scale_dt = salary_scale_dt,
#'   policy_params   = movement_policy_keep,
#'   ref_date        = ref_date,
#'   salary_col      = "gross_salary_cpi"
#' )
#'
#' # Confirm salaries unchanged: salary_before should equal
#' # the salary in the updated contract_dt for all movers
#' mover_ids <- results_keep$movers_dt$personnel_id
#' updated_salaries <- results_keep$contract_dt[
#'   personnel_id %in% mover_ids,
#'   .(personnel_id, gross_salary_cpi)
#' ]
#' check <- results_keep$movers_dt[updated_salaries, on = "personnel_id"]
#' all(check$salary_before == check$gross_salary_cpi)  # should be TRUE
#'
#' }
#'
#' @seealso
#'
#' [simulate_horizon()] for multi-period workforce simulations.
#'
#' [estimate_movement_baseline()] for estimating empirical transition
#' probabilities from panel data to supply as `policy_table`.
#'
#' [compute_movement_demand()] for the demand calculation when a transition
#' baseline is available.
#'
#' [compute_fixed_rate_movements()] for the flat scalar rate fallback when
#' only a single snapshot is available.
#'
#' [identify_movers()] for the individual mover selection engine.
#'
#' [update_state_with_movement()] for the state update step.
#'
#' [simulate_retirement()] and [simulate_exits()] for the modules that run
#' before this function within each simulation period.
#'
#' @family movement simulation
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
                                          contract_type_col  = "contract_type",
                                          status_col         = "employment_status") {

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
