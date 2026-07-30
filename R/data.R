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