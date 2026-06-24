#######################################################################
###### PREPARE A SUPPLEMENTARY ALLOWANCE FOR SIMULATION MODELLING #####
#######################################################################

### prepare the allowance categories
library(data.table)

set.seed(123)

allowance_types <- c(
  "transport",
  "meal",
  "management",
  "hazard",
  "performance",
  "overtime",
  "child"
)

# Keep only records with positive allowances
dt <- bra_hrmis_contract[
  !is.na(allowance_lcu) &
    allowance_lcu > 0,
  .(contract_id, ref_date, allowance_lcu)
]

# Number of allowance categories per contract
dt[, n_allowances := sample(1:5, .N, replace = TRUE)]

# Expand rows
dt <- dt[rep(seq_len(.N), n_allowances)]

# Random weights
dt[,
  weight := rgamma(.N, shape = 1)
]

# Normalize weights within contract
dt[,  weight := weight / sum(weight), by = .(contract_id, ref_date)]

# Allocate allowance
dt[, allowance_lcu := allowance_lcu * weight]

# Allocate allowance_type
type_dt <- data.table(allowance_type = allowance_types, 
                      allowance_num = 1:length(allowance_types))

dt[, allowance_num := sample(1:length(allowance_types), .N, replace = FALSE), by = c("contract_id", "ref_date")]

dt <- type_dt[dt, on = c("allowance_num")][, c("contract_id", "ref_date", "allowance_type", "allowance_lcu")]

bra_hrmis_allowances <- dt

dt <- NULL 

usethis::use_data(bra_hrmis_allowances, overwrite = TRUE)



# ## quick check sum allowances in the new table by contract_id and ref_date match
# # Sum allowances in the new table by contract_id and ref_date
# allowance_check <- bra_hrmis_allowances[, 
#   .(allowance_lcu_sim = sum(allowance_lcu)), 
#   by = .(contract_id, ref_date)
# ]

# # Pull the original allowances from the source table
# original <- bra_hrmis_contract[
#   !is.na(allowance_lcu) & allowance_lcu > 0,
#   .(contract_id, ref_date, allowance_lcu_orig = allowance_lcu)
# ]

# # Join and compare
# check <- original[allowance_check, on = .(contract_id, ref_date)]

# # Flag mismatches (using a tolerance for floating point)
# check[, match := abs(allowance_lcu_orig - allowance_lcu_sim) < 1e-9]

# # Summary
# cat("Total contracts checked:", nrow(check), "\n")
# cat("Matching:", sum(check$match), "\n")
# cat("Mismatching:", sum(!check$match), "\n")

# # Inspect any mismatches
# check[match == FALSE]
