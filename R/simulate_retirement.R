#' Simulate Retirement for One Year
#'
#' @description
#' Retires people at `as_of_date` under a retirement policy, pays their
#' pensions, and takes them off the payroll. It:
#'
#' 1. Checks the pension part of the policy.
#' 2. When the data hold several snapshots, keeps the one nearest `as_of_date`
#'    (but not after it).
#' 3. Picks who retires, with [select_retirees()].
#' 4. Works out each retiree's pension, with [compute_pension()].
#' 5. Closes the retirees' contracts and marks them as pensioners.
#' 6. Summarises the year's retirements.
#'
#' It covers one year. For a projection over several years, use
#' [simulate_horizon()], which calls this function each year. Your input
#' tables are never changed: the function works on copies.
#'
#' @param contract_dt data.table (or data.frame). The contracts, in the **govhr**
#'   format: one snapshot, or a panel with several `ref_date` values.
#' @param personnel_dt data.table (or data.frame). The people, with an age
#'   column and a tenure column when the policy uses age or tenure
#'   ([simulate_horizon()] works both out for you).
#' @param as_of_date Date. The date people retire on. Retiring contracts end on
#'   it.
#' @param policy_params List. The retirement policy, from
#'   [design_retirement_policy_status_quo()],
#'   [design_retirement_policy_eligibility()] or
#'   [design_retirement_policy_mandatory()]. It says who retires (see
#'   [select_retirees()]) and what they are paid (a `pension_*()` function, e.g.
#'   [pension_db()]; the pension can differ by group, see below).
#'
#'   **A pension that differs by group.** The pension part is a list with
#'   `group_cols`, `policy_table` and `defaults`. To pay, say, a different
#'   accrual rate by paygrade, set `group_cols = "paygrade"` and give a
#'   `policy_table` with `paygrade` and `accrual_rate`; retirees whose paygrade
#'   is not in the table get the `defaults`. The parameters each pension type
#'   reads:
#'
#'   | Parameter | Pension types | What it is |
#'   |:----------|:--------------|:-----------|
#'   | `pension_type` | all | `"db"`, `"dc"`, `"rate"`, `"flat"`, `"hybrid"` or `"custom"` |
#'   | `accrual_rate` | db, hybrid | Share of pay earned per year of service |
#'   | `max_years` | db, hybrid | Most years of service counted |
#'   | `replacement_cap` | db, hybrid | Highest pension as a share of pay |
#'   | `balance_col` | dc, hybrid | Column holding each person's account balance |
#'   | `annuity_factor` | dc, hybrid | Balance divided by this gives the yearly pension |
#'   | `notional_rate` | dc, hybrid | Optional yearly interest on the balance |
#'   | `flat_amount` | flat | The same pension for everyone |
#'   | `pension_rate` | rate | Pension as a share of pay |
#'   | `pension_formula` | custom | Your own arithmetic, e.g. `"salary * 0.02 * tenure_years"` |
#' @param column_spec_args What your columns are called, from
#'   [define_column_spec()]. Change only the names that differ from the **govhr**
#'   defaults, e.g. `define_column_spec(personnel_id_col = "pid", age_col = "idade")`.
#'   The returned tables keep your names. Keys used here (default in brackets):
#'   * `personnel_id_col` (`"personnel_id"`), `contract_id_col` (`"contract_id"`).
#'   * `ref_date_col` (`"ref_date"`): the snapshot date of a panel.
#'   * `start_date_col`, `end_date_col` (`"start_date"`, `"end_date"`): which
#'     contracts are active; retiring contracts end on `as_of_date`.
#'   * `salary_col` (`"gross_salary_lcu"`): pay. Picks each retiree's main
#'     contract, is what pay-based pensions are worked out from, and is set to
#'     0 on the contracts that close.
#'   * `contract_type_col` (`"contract_type"`): closing contracts become
#'     `"pensioner"`; `active_types` in the policy reads it.
#'   * `status_col` (`"employment_status"`): set to `"pensioner"` for retirees.
#'   * `age_col`, `tenure_col` (`"age"`, `"personnel_tenure"`): age and years of
#'     service, read when the policy uses them.
#'
#' @return A list:
#'   * `summary`: one row, from [compute_retirement_summary()]: how many
#'     retired, total and average pension, average age and tenure.
#'   * `contract_dt`: the contracts, with the retirees' contracts closed.
#'   * `personnel_dt`: the people, with retirees marked `"pensioner"`.
#'   * `retirees_dt`: one row per retiree, with their main contract's columns,
#'     `salary_col` (their pay on all their contracts added up: what the pension
#'     is worked out from, and what leaves the wage bill), `age`,
#'     `tenure_years`, the pension parameters applied to them, and `pension`.
#'     No rows when nobody retires.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' contract_dt  <- copy(bra_hrmis_contract)
#' personnel_dt <- copy(bra_hrmis_personnel)
#' personnel_dt[, age := as.numeric(as.Date("2016-09-01") - birth_date) / 365.25]
#'
#' # Everyone aged 60 or over retires, on 15% of their pay
#' policy <- design_retirement_policy_eligibility(retirement_rule_age(60),
#'                                                pension_rate(0.15))
#' result <- simulate_retirement(contract_dt, personnel_dt, as.Date("2016-09-01"), policy)
#' result$summary
#'
#' # A defined-benefit pension with a higher accrual rate for paygrade D
#' pension <- pension_db(accrual_rate = 0.02, max_years = 35, replacement_cap = 0.80)
#' pension$group_cols   <- "paygrade"
#' pension$policy_table <- data.table(paygrade = "D", accrual_rate = 0.025)
#' policy <- design_retirement_policy_eligibility(retirement_rule_age(60), pension)
#' simulate_retirement(contract_dt, personnel_dt, as.Date("2016-09-01"), policy)$summary
#' }
#'
#' @seealso [select_retirees()] for who retires, [compute_pension()] for the
#'   pension formulas, [simulate_horizon()] for a projection over several years.
#'
#' @family retirement simulation
#'
#' @export
simulate_retirement <- function(contract_dt,
                                personnel_dt,
                                as_of_date,
                                policy_params = design_retirement_policy_eligibility(
                                  retirement_rule_age_tenure(min_age = 60, min_tenure = 10),
                                  pension_db(accrual_rate = 0.02, max_years = 35,
                                             replacement_cap = 0.80)
                                ),
                                column_spec_args = define_column_spec()) {
  # 1. Check the column names and the pension part of the policy. Who retires
  #    is checked by select_retirees() below, before anyone is picked.
  .check_column_spec(column_spec_args)
  check_retirement_pension_inputs(policy_params)
  cols             <- column_spec_args
  personnel_id_col <- cols$personnel_id_col
  ref_date_col     <- cols$ref_date_col

  # Work on copies: the updates below change contract_dt and personnel_dt in
  # place, and the caller's tables must not change
  contract_dt  <- data.table::setDT(data.table::copy(contract_dt))
  personnel_dt <- data.table::setDT(data.table::copy(personnel_dt))

  # 2. With a panel, keep the snapshot nearest as_of_date (but not after it)
  if (ref_date_col %in% names(contract_dt)) {
    selected_ref_date <- select_nearest_ref_date(contract_dt[[ref_date_col]], as_of_date)
    contract_dt <- contract_dt[get(ref_date_col) == selected_ref_date]
    if (ref_date_col %in% names(personnel_dt)) {
      personnel_dt <- personnel_dt[get(ref_date_col) == selected_ref_date]
    }
  }

  # 3. Who retires, then their pay and main contract: the columns the pension
  #    and the updates below need
  retirees_dt <- select_retirees(contract_dt, personnel_dt, policy_params, as_of_date, cols)
  retirees_dt <- prepare_retiree_data(retirees_dt, contract_dt, personnel_dt, as_of_date, cols)

  if (nrow(retirees_dt) == 0L) {
    return(list(
      summary      = data.table::data.table(n_retired = 0L, total_pension = 0,
                                            avg_pension = NA_real_, avg_age = NA_real_,
                                            avg_tenure = NA_real_),
      contract_dt  = contract_dt,
      personnel_dt = personnel_dt,
      retirees_dt  = data.table::data.table()
    ))
  }

  # 4. Each retiree's pension parameters, from their group's row of the pension
  #    table or the pension's defaults, then their pension
  pension_params <- c("pension_type", "accrual_rate", "max_years", "replacement_cap",
                      "balance_col", "annuity_factor", "notional_rate",
                      "flat_amount", "pension_rate", "pension_formula")
  resolved <- resolve_policy_table(policy_params$pension, retirees_dt, pension_params)
  for (p in names(resolved)) data.table::set(retirees_dt, j = p, value = resolved[[p]])
  data.table::set(retirees_dt, j = "pension", value = compute_pension(retirees_dt, cols$salary_col))

  # 5. Close the retirees' contracts and mark them as pensioners (in place)
  update_contracts_for_retirees(contract_dt = contract_dt, retirees_dt = retirees_dt,
                                as_of_date = as_of_date, column_spec_args = cols)
  update_personnel_for_retirees(personnel_dt = personnel_dt, retirees_dt = retirees_dt,
                                personnel_id_col = personnel_id_col, status_col = cols$status_col)

  # 6. The year's summary
  list(
    summary      = compute_retirement_summary(retirees_dt = retirees_dt, contract_dt = contract_dt),
    contract_dt  = contract_dt,
    personnel_dt = personnel_dt,
    retirees_dt  = retirees_dt
  )
}
