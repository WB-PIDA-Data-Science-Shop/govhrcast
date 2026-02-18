## Script to prepare example HRMIS data for govhrcast package
## This creates bra_hrmis_contract and bra_hrmis_personnel with simulated birth_date

library(data.table)
library(govhr)

# Load original data from govhr package
bra_hrmis_contract <- as.data.table(govhr::bra_hrmis_contract)
bra_hrmis_personnel <- as.data.table(govhr::bra_hrmis_personnel)

# ========================================
# Simulate realistic birth_date values
# ========================================

# IMPORTANT: Personnel data is panel data with multiple observations per person
# We need ONE birth_date per unique personnel_id

# Get unique personnel_id from personnel_dt
unique_personnel <- unique(bra_hrmis_personnel[, .(personnel_id, status)])

# Assumptions:
# - Most public sector employees are between ages 25-65
# - Use earliest start_date in contract_dt as proxy for hire date
# - Assume hired between ages 22-45 (typical entry ages)
# - Reference date: use max ref_date from contract_dt

ref_date <- max(bra_hrmis_contract$ref_date, na.rm = TRUE)

# Get earliest start_date per personnel as proxy for hire date
hire_dates <- bra_hrmis_contract[!is.na(start_date), 
                                  .(hire_date = min(start_date)), 
                                  by = personnel_id]

# Merge with unique personnel
personnel_with_hire <- merge(
  unique_personnel,
  hire_dates,
  by = "personnel_id",
  all.x = TRUE
)

# Simulate age at hire (normal distribution, mean=30, sd=8, bounded [22,45])
set.seed(20260218)  # For reproducibility
personnel_with_hire[!is.na(hire_date), 
                    age_at_hire := pmax(22, pmin(45, rnorm(.N, mean = 30, sd = 8)))]

# Calculate birth_date = hire_date - age_at_hire (in years)
personnel_with_hire[!is.na(age_at_hire), 
                    birth_date := as.Date(hire_date - round(age_at_hire * 365.25))]

# For personnel without hire dates, simulate based on current status
# Assume ages 25-65 for active, 60-75 for inactive
personnel_with_hire[is.na(birth_date) & status == "active",
                    birth_date := as.Date(ref_date - round(runif(.N, 25, 65) * 365.25))]

personnel_with_hire[is.na(birth_date) & status == "inactive",
                    birth_date := as.Date(ref_date - round(runif(.N, 60, 75) * 365.25))]

# Now add birth_date to original personnel_dt using update join
# Set keys for efficient join
setkey(bra_hrmis_personnel, personnel_id)
setkey(personnel_with_hire, personnel_id)

# Update join: add birth_date column from personnel_with_hire
bra_hrmis_personnel[, birth_date := NULL]  # Remove old column first
bra_hrmis_personnel[personnel_with_hire, birth_date := i.birth_date]

# Reorder columns
setcolorder(bra_hrmis_personnel, 
            c("ref_date", "personnel_id", "birth_date", "gender", "educat7", 
              "tribe", "race", "status", "country_code"))

# ========================================
# Save package data
# ========================================

usethis::use_data(bra_hrmis_contract, overwrite = TRUE)
usethis::use_data(bra_hrmis_personnel, overwrite = TRUE)

# ========================================
# Document the data
# ========================================

# Create documentation file if it doesn't exist
doc_file <- "R/data.R"
if (!file.exists(doc_file)) {
  cat('#\' Brazil HRMIS Contract Data
#\'
#\' @description
#\' Example contract-level HRMIS data from Alagoas state, Brazil.
#\' This is a subset of the harmonized data from the govhr package,
#\' formatted as a data.table for use in govhrcast simulations.
#\'
#\' @format A data.table with 8,885 rows and 23 columns:
#\' \\describe{
#\'   \\item{ref_date}{Date. Reference date for the observation}
#\'   \\item{personnel_id}{Character. Unique personnel identifier}
#\'   \\item{contract_id}{Character. Contract identifier}
#\'   \\item{est_id}{Character. Establishment/organization identifier}
#\'   \\item{start_date}{Date. Contract start date}
#\'   \\item{end_date}{Date. Contract end date (NA if ongoing)}
#\'   \\item{contract_type_code}{Character. Type of contract (perm, fterm, temp, etc.)}
#\'   \\item{base_salary_lcu}{Numeric. Base salary in local currency units}
#\'   \\item{allowance_lcu}{Numeric. Allowances in local currency units}
#\'   \\item{gross_salary_lcu}{Numeric. Gross salary in local currency units}
#\'   \\item{net_salary_lcu}{Numeric. Net salary in local currency units}
#\'   \\item{whours}{Numeric. Working hours}
#\'   \\item{...}{Additional contract attributes}
#\' }
#\'
#\' @source Derived from \\code{govhr::bra_hrmis_contract}
"bra_hrmis_contract"

#\' Brazil HRMIS Personnel Data
#\'
#\' @description
#\' Example personnel-level HRMIS data from Alagoas state, Brazil.
#\' This version includes simulated birth_date values to enable testing
#\' age-based retirement policies.
#\'
#\' @format A data.table with 8,672 rows and 9 columns:
#\' \\describe{
#\'   \\item{ref_date}{Date. Reference date for the observation}
#\'   \\item{personnel_id}{Character. Unique personnel identifier}
#\'   \\item{birth_date}{Date. Simulated birth date (for testing purposes)}
#\'   \\item{gender}{Character. Gender}
#\'   \\item{educat7}{Character. Education level (7 categories)}
#\'   \\item{tribe}{Character. Tribe/ethnicity}
#\'   \\item{race}{Character. Race}
#\'   \\item{status}{Character. Employment status (active/inactive)}
#\'   \\item{country_code}{Character. Country code}
#\' }
#\'
#\' @note The birth_date values are simulated based on contract start dates
#\'   and typical public sector hiring ages (22-45). They are intended for
#\'   testing and demonstration purposes only.
#\'
#\' @source Derived from \\code{govhr::bra_hrmis_personnel} with simulated birth_date
"bra_hrmis_personnel"
', file = doc_file)
}

message("✓ Package data created: bra_hrmis_contract, bra_hrmis_personnel")
message("✓ Birth dates simulated for ", sum(!is.na(bra_hrmis_personnel$birth_date)), 
        " out of ", nrow(bra_hrmis_personnel), " personnel")
