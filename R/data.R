#' Brazil (Alagoas) Harmonized HRMIS Contract Dataset
#'
#' A longitudinal contract-level dataset containing harmonized Human Resource
#' Management Information System (HRMIS) records for public sector employees in
#' the Brazilian state of Alagoas. Each observation represents a contract for a
#' personnel member at a given reference date, allowing employment history,
#' remuneration, occupations, contract characteristics, and tenure to be
#' analysed over time.
#'
#' @format A data.table with 16,360 observations and 21 variables:
#' \describe{
#'   \item{contract_id}{Unique contract identifier.}
#'   \item{personnel_id}{Unique personnel identifier.}
#'   \item{est_id}{Government establishment employing the personnel member.}
#'   \item{ref_date}{Reference (snapshot) date of the observation.}
#'   \item{base_salary_lcu}{Base salary in local currency units (Brazilian Real).}
#'   \item{allowance_lcu}{Total allowances paid in local currency units.}
#'   \item{gross_salary_lcu}{Gross salary in local currency units.}
#'   \item{net_salary_lcu}{Net salary in local currency units.}
#'   \item{whours}{Contracted weekly working hours.}
#'   \item{start_date}{Contract start date.}
#'   \item{end_date}{Contract end date. Missing values indicate contracts that
#'   were active at the reference date.}
#'   \item{paygrade}{Pay grade associated with the contract.}
#'   \item{seniority}{Seniority level or step within the pay structure.}
#'   \item{occupation_native}{Occupation title in Portuguese.}
#'   \item{occupation_english}{Occupation title translated into English.}
#'   \item{occupation_iscocode}{International Standard Classification of
#'   Occupations (ISCO-08) occupation code.}
#'   \item{occupation_isconame}{ISCO-08 occupation title.}
#'   \item{contract_type_native}{Original contract type recorded in Portuguese.}
#'   \item{contract_type}{Harmonized contract type.}
#'   \item{contract_tenure}{Length of service under the contract, in years, as
#'   of the reference date.}
#' }
#'
#' @details
#' The dataset has been harmonized using the \pkg{govhr} data model to provide
#' consistent variable names and coding across HRMIS systems. Multiple
#' observations may exist for the same contract because contracts are observed
#' repeatedly at different reference dates, forming a longitudinal panel.
#'
#' Salary variables are expressed in nominal local currency units (LCU).
#'
#' @source Government of the State of Alagoas, Brazil. Harmonized for the
#' \pkg{govhr} package.
#'
#' @examples
#' data(bra_hrmis_contract)
#'
#' head(bra_hrmis_contract)
"bra_hrmis_contract"


#' Brazil (Alagoas) Harmonized HRMIS Personnel Dataset
#'
#' A longitudinal personnel-level dataset containing harmonized Human Resource
#' Management Information System (HRMIS) records for public sector employees in
#' the Brazilian state of Alagoas. Each observation represents a personnel
#' member at a given reference date, providing demographic characteristics,
#' employment status, education, and public service tenure over time.
#'
#' @format A data.table with 15,612 observations and 12 variables:
#' \describe{
#'   \item{personnel_id}{Unique personnel identifier.}
#'   \item{ref_date}{Reference (snapshot) date of the observation.}
#'   \item{birth_date}{Date of birth.}
#'   \item{age}{Age, in years, at the reference date.}
#'   \item{gender}{Gender recorded in the HRMIS.}
#'   \item{educat7}{Educational attainment using the World Bank's
#'   seven-level Global Labor Database (GLD) classification:
#'   \code{"No education"},
#'   \code{"Primary incomplete"},
#'   \code{"Primary complete"},
#'   \code{"Secondary incomplete"},
#'   \code{"Secondary complete"},
#'   \code{"Post-secondary but not university"}, and
#'   \code{"University complete or incomplete"}.}
#'   \item{tribe}{Ethnic or tribal affiliation, where available.}
#'   \item{race}{Race or broad ethnic classification, where available.}
#'   \item{employment_status}{Employment status. One of
#'   \code{"active"}, \code{"inactive"}, or \code{"pensioner"}.}
#'   \item{personnel_tenure}{Total public service tenure, in years, as of the
#'   reference date.}
#'   \item{service_type}{Type of public service. One of
#'   \code{"civilian"} or \code{"military"}.}
#'   \item{first_employment_date}{Date the personnel member first entered
#'   public service.}
#' }
#'
#' @details
#' The dataset has been harmonized using the \pkg{govhr} data model to provide
#' consistent variable names and coding across HRMIS systems. Multiple
#' observations may exist for the same personnel member because personnel are
#' observed repeatedly at different reference dates, forming a longitudinal
#' panel.
#'
#' Educational attainment follows the World Bank's Global Labor Database (GLD)
#' seven-category education classification to facilitate cross-country
#' comparability.
#'
#' @source Government of the State of Alagoas, Brazil. Harmonized for the
#' \pkg{govhr} package.
#'
#' @examples
#' data(bra_hrmis_personnel)
#'
#' head(bra_hrmis_personnel)
"bra_hrmis_personnel"



#' Brazil (Alagoas) Harmonized HRMIS Allowance Dataset
#'
#' A longitudinal allowance-level dataset containing harmonized Human Resource
#' Management Information System (HRMIS) records for public sector employees in
#' the Brazilian state of Alagoas. Each observation represents a single
#' allowance type associated with a contract at a given reference date,
#' allowing detailed analysis of remuneration components over time.
#'
#' @format A data.table with 60,430 observations and 5 variables:
#' \describe{
#'   \item{personnel_id}{Unique personnel identifier.}
#'   \item{contract_id}{Unique contract identifier.}
#'   \item{ref_date}{Reference (snapshot) date of the observation.}
#'   \item{allowance_type}{Allowance type recorded in the HRMIS.}
#'   \item{allowance_lcu}{Allowance amount in local currency units (Brazilian
#'   Real) for the corresponding allowance type.}
#' }
#'
#' @details
#' The dataset has been harmonized using the \pkg{govhr} data model to provide
#' consistent variable names across HRMIS systems. Multiple observations may
#' exist for the same contract and reference date because each allowance type
#' is stored as a separate record.
#'
#' Allowance amounts are expressed in nominal local currency units (LCU).
#' Total allowances for a contract can be obtained by summing
#' \code{allowance_lcu} across allowance types within each
#' \code{contract_id} and \code{ref_date}.
#'
#' @source Government of the State of Alagoas, Brazil. Harmonized for the
#' \pkg{govhr} package.
#'
#' @examples
#' data(bra_hrmis_allowance)
#'
#' head(bra_hrmis_allowance)
"bra_hrmis_allowance"


#' Status-Quo Projection of the Alagoas Data, 2017 to 2022
#'
#' The result of a five-year \code{\link{simulate_horizon}()} run on
#' \code{\link{bra_hrmis_contract}} and \code{\link{bra_hrmis_personnel}}, from
#' the 2017-09-01 snapshot, with the microdata kept. It is saved so that a
#' dashboard, an example or a test can start from a finished baseline without
#' running the model.
#'
#' The policies are the status quo, with rates estimated from the panel up to
#' 2017:
#' \itemize{
#'   \item \strong{Retirement:} the historical retirement rate at each age, with a
#'     defined-benefit pension of 2\% of pay per year of service, at most 35
#'     years and at most 80\% of pay.
#'   \item \strong{Other exits:} the historical exit rate of each establishment
#'     x pay grade group.
#'   \item \strong{Promotions:} the historical rates of moving between
#'     establishment x pay grade groups.
#'   \item \strong{Hiring:} the historical hiring rate of each establishment x
#'     pay grade group. Hires and people promoted into a group are paid the
#'     group's mean pay at 2017 (pensions left out).
#'   \item \strong{Pay growth:} none, so amounts are in 2017 money.
#' }
#' The payroll goes from 928 to 964 people over the five years. Each group
#' keeps its own rates, but the groups that hire fastest (mostly temporary posts
#' with no pay grade) also grow fastest, so the payroll-wide hiring rate creeps
#' up, from 7.7\% in the first year to 12\% in the fifth. That is what
#' continuing the past trend means for this data, not a forecast.
#'
#' @format A \code{horizon} object (a list; see \code{\link{simulate_horizon}})
#'   with:
#' \describe{
#'   \item{comparison}{data.table, one row per projected year (5 rows, 36
#'     columns): headcount, wage bill and its decomposition into exit savings,
#'     promotion, hiring and inflation effects, and pension costs. Labelled with
#'     \code{scenario_label = "Status quo"} and \code{is_baseline = TRUE}.}
#'   \item{summary_dt}{The same table as \code{comparison}.}
#'   \item{metadata}{List: the scenario name, \code{is_baseline}, and in
#'     \code{policy_args} the policies (with their estimated rates), the growth
#'     rates, \code{n_periods} and \code{baseline_date}.}
#'   \item{pensioner_register}{data.table, one row per pensioner (768 rows):
#'     the 609 already retired at 2017 (\code{retire_reason} blank) and those
#'     who retire in the projection (\code{"status_quo"}), with
#'     \code{pension_amount}, \code{final_salary}, age and tenure at retirement
#'     and \code{period_date}.}
#'   \item{contract_dt, personnel_dt}{data.tables in the same columns as
#'     \code{bra_hrmis_contract} and \code{bra_hrmis_personnel}, one snapshot per
#'     \code{ref_date} from 2007 to 2022. Snapshots before 2017-09-01 are the
#'     input data unchanged; 2017-09-01 is the starting point; 2018 to 2022 are
#'     projected (the data's own 2018 snapshot is replaced).}
#' }
#'
#' @details
#' Built by \code{data-raw/bra_status_quo_horizon.R} with a fixed seed. It is a
#' snapshot of what the model returned when it was built: rebuild it whenever
#' the model changes. \code{print()} and \code{summary()} use the
#' \code{horizon} methods, so govhrcast must be loaded.
#'
#' @source Projected by govhrcast from the Alagoas data in
#'   \code{\link{bra_hrmis_contract}} and \code{\link{bra_hrmis_personnel}}.
#'
#' @examples
#' bra_status_quo_horizon
#'
#' # Year-by-year headcount and wage bill
#' bra_status_quo_horizon$comparison[, .(period_end_date, n_headcount_end, wage_bill_end)]
"bra_status_quo_horizon"