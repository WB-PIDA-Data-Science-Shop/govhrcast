#' Brazil HRMIS Contract Data
#'
#' @description
#' Example contract-level HRMIS data from Alagoas state, Brazil.
#' This is a subset of the harmonized data from the govhr package,
#' formatted as a data.table for use in govhrcast simulations.
#'
#' @format A data.table with 8,885 rows and 23 columns:
#' \describe{
#'   \item{ref_date}{Date. Reference date for the observation}
#'   \item{personnel_id}{Character. Unique personnel identifier}
#'   \item{contract_id}{Character. Contract identifier}
#'   \item{est_id}{Character. Establishment/organization identifier}
#'   \item{start_date}{Date. Contract start date}
#'   \item{end_date}{Date. Contract end date (NA if ongoing)}
#'   \item{contract_type_code}{Character. Type of contract (perm, fterm, temp, etc.)}
#'   \item{base_salary_lcu}{Numeric. Base salary in local currency units}
#'   \item{allowance_lcu}{Numeric. Allowances in local currency units}
#'   \item{gross_salary_lcu}{Numeric. Gross salary in local currency units}
#'   \item{net_salary_lcu}{Numeric. Net salary in local currency units}
#'   \item{whours}{Numeric. Working hours}
#'   \item{...}{Additional contract attributes}
#' }
#'
#' @source Derived from \code{govhr::bra_hrmis_contract}
"bra_hrmis_contract"

#' Brazil HRMIS Personnel Data
#'
#' @description
#' Example personnel-level HRMIS data from Alagoas state, Brazil.
#' This version includes simulated birth_date values to enable testing
#' age-based retirement policies.
#'
#' @format A data.table with 8,672 rows and 9 columns:
#' \describe{
#'   \item{ref_date}{Date. Reference date for the observation}
#'   \item{personnel_id}{Character. Unique personnel identifier}
#'   \item{birth_date}{Date. Simulated birth date (for testing purposes)}
#'   \item{gender}{Character. Gender}
#'   \item{educat7}{Character. Education level (7 categories)}
#'   \item{tribe}{Character. Tribe/ethnicity}
#'   \item{race}{Character. Race}
#'   \item{status}{Character. Employment status (active/inactive)}
#'   \item{country_code}{Character. Country code}
#' }
#'
#' @note The birth_date values are simulated based on contract start dates
#'   and typical public sector hiring ages (22-45). They are intended for
#'   testing and demonstration purposes only.
#'
#' @source Derived from \code{govhr::bra_hrmis_personnel} with simulated birth_date
"bra_hrmis_personnel"
