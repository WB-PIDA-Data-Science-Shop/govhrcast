library(data.table)
library(govhrcast)

ct <- bra_hrmis_contract[ref_date == as.Date("2016-09-01")]

# Overall headcount
compute_current_stock(ct, group_cols = NULL,
                      personnel_id_col   = "personnel_id",
                      contract_type_col  = "contract_type_code",
                      active_types       = c("permanent", "temporary"),
                      ref_date           = as.Date("2016-09-01"))
# n_active: 531 (unique people, not contracts)

# Per establishment
compute_current_stock(ct, group_cols = "est_id",
                      personnel_id_col   = "personnel_id",
                      contract_type_col  = "contract_type_code",
                      active_types       = c("permanent", "temporary"),
                      ref_date           = as.Date("2016-09-01"))
#                          est_id  n_active
# SECRETARIA DA EDUCACAO...          212
# SECRETARIA DA SAUDE...              89
# ...